package Test::Smoke::Database::Parsing;

# Copyright 200x A.Barbet alian@cpan.org  All rights reserved.
# $Date: 2003/08/02 12:38:09 $
# $Log: Parsing.pm,v $
# Revision 1.2  2003/08/02 12:38:09  alian
# Remove unused package
#
# Revision 1.1  2003/07/30 22:08:02  alian
# Code from Database.pm
#

use strict;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK);
use DBI;
use Data::Dumper;
use Carp qw(cluck);
use File::Basename;
require Exporter;

@ISA = qw(Exporter);
@EXPORT = qw();
$VERSION = ('$Revision: 1.2 $ ' =~ /(\d+\.\d+)/)[0];

#------------------------------------------------------------------------------
# parse_import
#------------------------------------------------------------------------------
sub parse_import {
  my $self = shift;
  my ($nb,$nbo,%k) = (0,0);
  print scalar(localtime),": Parse reports\n"
    if ($self->{opts}->{verbose});
  # Select list of knows id
  my $st = $self->{DBH}->prepare('select distinct id from builds');
  $st->execute;
  while (my ($id)= $st->fetchrow_array) { $k{$id}=1; }
  $st->finish;

  # Read a .rpt file
  foreach (glob($self->{opts}->{dir}."/*.rpt*")) {
    $nb++;
    # skip backup file or already defined report
    next if (/~$/ or ( /(\d+)\.rpt/ && $k{$1}));
    my $ref = parse_rpt($_);
    if (!defined($ref)) { 
      warn "Can't read/parse $_\n" if ($self->{opts}->{debug});
    }
    elsif (!ref($ref)) {
      if ($ref == -1) {
	my @l = parse_hm_brand_rpt($_);
	foreach (@l) {
	  next if (!$_->{id} or $k{$_->{id}});
	  print STDERR "Add a H.M. Brand report\n"
	    if ($self->{opts}->{debug});
	  $self->add_to_db($_) && $nbo++;
	  $k{$_->{id}}=1;
	}
      } elsif ($ref == -2) {
	warn "\tSeems to be a DEAD report, will be unlink\n"
	  if ($self->{opts}->{verbose});
	unlink $_;
      } elsif ($ref == -3) {
	warn "\tSeems to be a Alian report with too more rows, will be unlink"
	  if ($self->{opts}->{verbose});
	unlink $_;
      } else {
	warn "\tWhat's this ? $_";
      }
	
    }
    else {
      # Add it to database
      print STDERR "Add report $_\n" if ($self->{opts}->{debug});
      $self->add_to_db($ref) && $nbo++;
    }
  }
  print scalar(localtime),": $nbo reports imported from $nb files\n" 
    if ($self->{opts}->{verbose});
  return $nbo;
}

#------------------------------------------------------------------------------
# parse_hm_brand_rpt
#------------------------------------------------------------------------------
sub parse_hm_brand_rpt {
  my $file = shift;
  return if (!$file);
  if (!-r $file) { warn "Can't found $file"; return; }
  my (@lr,%last,$header);
  open(FILE,$file) or die "Can't read $file:$!\n";
  my @content = <FILE>;
  close(FILE);
  my $ok=0;
  # Rebuild report wrapped by mail to 72c
  my $cont; my $re = 0;
  foreach my $l (@content) {
    chomp($l);
    if ($l=~/\=$/) { chop($l); $re=1; }
    if ($re) { $cont.=$l; $re=0; }
    else { $cont.=$l."\n"; }
  }
  my $origI = 0;
  my $nbI = 0;
  foreach my $l (split(/\n/, $cont)) {
    $l.="\n";
    my $i = $origI;
    foreach my $a (@lr) {
      if (!$a or !ref($a)) { delete $lr[$i]; next;}
      $i++;
    }
    $i = $origI;
    if ($l=~/MULTIPART_MIXED_/) { $origI = $#lr+1; $ok=0; }
    $ok = 1 if ($l=~/^ HP-UX/ && !$ok);
    if (!$ok) { $header.= $l; next;} # skip header
    if ($ok<5) {$l=~s/\s+/ /g; }
    if ($ok ==1) { # os
      foreach (split(/ /,$l)) { push(@lr, +{ os => $_ }) if ($_); } $ok++;}
    elsif ($ok == 2) { # osver
      foreach (split(/ /,$l)) { $lr[$i++]->{osver} = $_ if ($_); } $ok++;}
    elsif ($ok == 3) { # cc
      foreach (split(/ /,$l)) { $lr[$i++]->{cc} = $_ if ($_); } $ok++;}
    elsif ($ok == 4) { # no smoke
      foreach (split(/ /,$l)) { $lr[$i++]->{smoke} = $_ if ($_ && /^\d*$/); }
      $ok++; $nbI = $i-$origI;
    } elsif ($ok == 5) { $ok++; next; } # line of -
    # line of speed result
    elsif ($ok >5 && (($l=~/^\d/) or ($l=~/^ \d/))) {
     for my $i (0..$nbI) { delete $lr[$origI+$i]; }
    }
    # line of result
    elsif ($ok >5 && ($l=~/^O/ || $l=~/^F/ || $l=~/^m/) && $l ne "Failures:\n") {
      chomp($l);
      my @l;
      $i=0;
      while ($i < $nbI) {
	((length($l)>=9*$i) ? push(@l,substr($l,9*$i,9)) : push(@l,' '));
	$i++;
      }
      $i=$origI;
      my $conf = (length($l)>9*$nbI ? substr($l,9*$nbI) : " ");
      next if ($conf!~/^-/ and $conf!~/^\s*$/);
      foreach (@l) {
	if (!(/^[ \?\-\.]+$/)) { # really a result
	  $lr[$i]->{build}{$conf} = $_ if ($_!~m!^\s*$!);
	}
	$i++;
      }
      $ok++;
    }
    # errors
    elsif ($ok > 6) {
      my ($r,%ln)=(0);
      foreach my $a (@lr) {
#	print "Dump:",Data::Dumper->Dump([ $a ]),"\n";
#	print $a,"\n";
	next if (!$a->{os} && !$a->{osver});
	if ($a->{os} =~ /cygwin/i) {
	  $ln{$i++} = $a->{os}." ".substr($a->{osver},0,3);
	} elsif ($a->{os} =~ /aix/i) {
	  $ln{$i++} = $a->{os}." ".substr($a->{osver},0,3).' '.$a->{cc};
	} else { $ln{$i++} = $a->{os}." ".$a->{osver}; }
      }
      foreach my $n (keys %ln) {# print $ln{$n},"\n";
	if ($l=~/^$ln{$n}/i) {
	  $lr[$n]->{failure}.=$l if ($lr[$n]);
	  $last{$n}=1;
	  $r=1; #last;
	}
      }
      if (!$r) {
	if ($l=~/^[ \t]+/ && %last) {
	  foreach (keys %last) { 
	    if ($lr[$_]) { $lr[$_]->{failure}.=$l; $lr[$_]->{nbte}++;  }
	  }
	} else { undef %last; }
      }
    }
  }

  $ok=-1;
  foreach my $r (@lr) {
    $ok++;
    if (!ref($r) or !$r->{smoke}) { delete $lr[$ok]; next; }
    $r->{osver} = $1 if ($r->{osver}=~/^(.*)-\d/);
    # Try to guess cc version
    my $name = $r->{os}.' '.$r->{osver};
    if (!$r->{ccver} && $header=~m/$name[^ ]*  \s*([^\n]*)\n/i) {
      my $v = $1;
      if ($v=~/^([^\n]*?\d)\s+(.*)/) {
	$r->{ccver} = $1;
	$lr[$ok+1]->{ccver}=$2
	  if ($lr[$ok+1]->{os} && $lr[$ok+1]->{os} eq $r->{os});
      } else { $r->{ccver} = $v; }
    }
    # Set others values
    $r->{file} = $file;
    $r->{nbc} = scalar keys %{$r->{build}};
    $r->{nbco} = 0;
    $r->{nbte} = 0 if (!$r->{nbte});
    $r->{id} = $r->{smoke}.$ok;
    if ($r->{ccver} && $r->{ccver}=~/^(.*?)\s+32-bit$/) {
      $r->{ccver} = $1; 
    } elsif (!$r->{ccver}) { $r->{ccver}= ' '; }
    $r->{cc} = ' ' if (!$r->{cc});
    $r->{osver} = ' ' if (!$r->{osver});
    $r->{archi}= ' ';
    $r->{matrix} = [
		    'PERLIO = stdio',
		    'PERLIO = perlio',
		    'PERLIO = stdio  -DDEBUGGING',
		    'PERLIO = perlio -DDEBUGGING'
		   ];
  }
  return @lr;
}
 
#------------------------------------------------------------------------------
# parse_rpt
#------------------------------------------------------------------------------
sub parse_rpt {
  my $file = shift;
  my ($nbr,$fail,$col,$content)=(0);
  return if (!$file);
  if (!-r $file) { warn "Can't found $file"; return; }
  open(FILE,$file) or die "Can't read $file:$!\n";
  my %h = ( file => $file );
  my @content = <FILE>;
  close(FILE);
  my $r = 0;
  # Rebuild report wrapped by mail to 72c
  my $cont;
  foreach my $l (@content) {
    chomp($l);
    $l=~s/=3D/=/g;
    if ($l=~/=$/) { chop($l); $r=1; }
    if ($r) { $cont.=$l; $r=0; }
    else { $cont.=$l."\n"; }
  }
  return undef if (!$cont);
  my $irix = 0;
  my $re = qr/(?:\w|-|\?) /;
  foreach my $l (split(/\n/, $cont)) {
    $content.=$l;
    chomp($l);
    $nbr++ if ($l=~/^>/);
    if ($l=~/^From:/ && $l=~/Brand/) { $col=-1; }
    elsif ($l=~/^From:/ && $l=~/Alian/) { $col=-3; }
    elsif ($l=~/^Return-Path: <h.m.brand\@hccnet.nl>/) { $col=-1; }
    # A reply
    elsif ($l=~/^Subject: Re:/) { return -2; }
    # A report without info about os
    elsif (($l=~/Automated smoke report for patch (\d+) on  - $/) or
	   ($l=~/Automated smoke report for patch (\d*) on  -  \(\)$/)) {
      return -2;
    }
    # A normal report with os and osver
    elsif (($l=~/Automated smoke report for patch (\d+) on (.*) - (.*)$/) or
	   ($l=~/Automated smoke report for .* patch (\d+) on (.*) - (.*)$/)) {
      ($h{smoke},$h{os}, $h{osver}) = ($1,$2,$3);
      if (!$h{os} and !$h{osver}) {
#	print "\tNo os and osver defined in report\n"
#	  if ($self->{opts}->{verbose});
	return undef;
      }
    }
    elsif ($l=~/Automated smoke report for patch (\d*) on (.*)$/) {
      ($h{smoke},$h{os}, $h{osver}) = ($1,$2,"??");
      if ($l=~/(irix\d*)$/) { $irix = 1; $h{os}=$1;}
    }
    elsif ($l=~/Automated smoke report for patch (\d*)$/) {
      ($h{smoke}) = ($1);
    }
    elsif ($irix==1) {
      $irix=0;
      $h{osver} = $1 if ($l=~/^ - (.*)$/);
    }
    elsif ($l=~/on (.*) using (.*) version (.*)$/) {
      ($h{os}, $h{cc},$h{ccver},$h{osver}) = ($1,$2,$3,"??");
    }
    elsif ($l=~/using (.*) version (.*)$/) {
      ($h{cc}, $h{ccver}) = ($1,$2);
    }
    # A line of result
    elsif (($l=~/^($re{3,}(?:\w|-|\?)) +(-.+)$/)
	    || ($l=~/^($re{3,}(?:\w|-|\?))$/)) {
      next if (!$1 or $1 eq '? ? ? ?' or $1 eq 't t t t');
      my $c = $2; 
      $c=' ' if (!$c);
      $h{"build"}{$c} = $1;
    }
    # Matrix
    elsif (!$fail && $l=~/^[\| ]*\+-+ (.*)$/ && $1!~/^-*$/) {
      push(@{$h{matrix}}, $1) if ($1 ne 'Configuration');
    }
    # Failures
    elsif ($fail) {
      $h{"failure"}.=$l."\n" if ($l);
      $h{nbte}++ if ($l=~/\.\.\./); 
    }
    elsif ($l=~/Failures(.*):/) { $fail=1; }
  }
#    if ($h{os}=~/irix/i) { print Data::Dumper->Dump( [ \%h]); }
#  print $h{failure},"\n";
  # Valid report have os and build
  if ($h{build} && $h{os}) {
    # ccver
    if (!$h{ccver}) { $h{ccver}="??"; }
    else {
      $h{ccver}=~s/\(prerelease\)//g;
      $h{ccver}=~s/\(release\)//g;
    }
    # Number of failed test
    $h{nbte} = 0 if (!$h{nbte});
    # cc
    if (!$h{cc}) { $h{cc}="??"; }
    elsif ($h{cc}=~m!/([^/]*)$!) { $h{cc}=$1 }
    # Number of configure run
    $h{nbc} = scalar keys %{$h{build}};
    $h{nbco} = 0;
    # Try to set the archi
    if ($h{osver}=~m!^(.*)\((.*)/.*\)! or $h{osver}=~m!^(.*)\((.*)\)!) {
      $h{osver} = $1;
      $h{archi} = $2;
      if ($h{archi}=~m!^([^-]*)-!) { $h{archi} = $1; }
      $h{archi} = "i386" if ($h{archi}=~/86$/);
    } else {$h{archi}= '??';}
    if ($h{os}=~/^irix/ && $h{osver}=~/^(.*) (IP\d*)/) {
      $h{osver}=$1; $h{archi}=$2;
    }
    @{$h{matrix}}=reverse @{$h{matrix}} if ($h{matrix});
#    $h{report}= $content;
    $h{id}=$1 if (($file=~/(\d+)\.rpt/ or
		  $file=~/(\d+)\.normal\.rpt/));
    return \%h
  }
  # More than 8 lines beginning with '>', seems to be a reply
  if ($nbr>8) {
    warn "$file seems to be a reply\n";
    return -2;
  }
#  elsif ($col && (!$h{os} || !$h{) { warn "$file have no build or os\n"; }
  return ($col ? $col : undef);
}

__END__

#------------------------------------------------------------------------------
# POD DOC
#------------------------------------------------------------------------------


=head1 NAME

Test::Smoke::Database::Parsing - Routine for parsing Test::Smoke reports

=head1 SYNOPSIS

  $ admin_smokedb --create --suck --import --update_archi
  $ lynx http://localhost/cgi-bin/smokedb.cgi

=head1 SEE ALSO

L<Test::Smoke::Database>

=head1 METHODS

=over 4

=item B<parse_import> I<Test::Smoke::Database object>

As his name say, this method will parse and import fetched report found
in $self->{opts}->{dir} and put them in database. Return the number of
reports added to database on succes, 0 else.

=item B<parse_rpt> I<file>

This method is call by parse_import.
Parse I<file> and return values parsed in a reference of hash.
Else return -1 for a H.M. Brand report (then B<parse_hm_brand_rpt> will be 
called), -2 for a bad report, ie a report without os/osver (this report will be
deleted), -3 for an Alian multi-col report (deleted too).
Return undef if no file or if file doesn't exist;

=item B<parse_hm_brand_rpt> I<file>

Do a specific parsing for H.M Brand report I<file>. (his report is multi-col).
Return a list of reference of report to use with add_db.
Return undef if no file or if file doesn't exist;

=back

=head1 VERSION

$Revision: 1.2 $

=head1 AUTHOR

Alain BARBET

=cut

1;
