package Test::Smoke::Database;

# module Test::Smoke::Database - Add / parse /display perl reports smoke database
# Copyright 2003 A.Barbet alian@alianwebserver.com.  All rights reserved.
# $Date: 2003/01/05 21:45:55 $
# $Log: Database.pm,v $
# Revision 1.4  2003/01/05 21:45:55  alian
# Fix for parsing hm. brand reports with 5.6, fix test with 5.6
#
# Revision 1.3  2003/01/05 01:15:55  alian
# - Add a special parser for HM Brand's reports
# - Remove --rename option
# - Rewrite code for better daily use with no --clear option
# - Add tests for report parsing
# - Update POD
#

use Carp;
use strict;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK);
use DBI;
use CGI qw/:standard/;
use News::NNTPClient;
use Data::Dumper;
use Carp qw(cluck);

require Exporter;

@ISA = qw(Exporter);
@EXPORT = qw(prompt);
$VERSION = ('$Revision: 1.4 $ ' =~ /(\d+\.\d+)/)[0];

my $limite = 18013;
#$limite = 0;

#------------------------------------------------------------------------------
# new
#------------------------------------------------------------------------------
sub new   {
  my $class = shift;
  my $self = {};
  bless $self, $class;
  $self->{opts} = shift || return undef;
  my $driver = "DBI:mysql:database=".$self->{opts}->{database}.
    ";host=localhost;port=3306";
  if (!$self->{opts}->{no_dbconnect}) {
    $self->{DBH} = DBI->connect($driver,
				$self->{opts}->{user},
				$self->{opts}->{password})
      || die "Can't connect to Mysql:$driver:$!\n";
  }
  $limite = $self->{opts}->{limit} if ($self->{opts}->{limit});
  $limite = 0 if ($limite eq 'All');
  return $self;
}

sub DESTROY { $_[0]->{DBH}->disconnect if ($_[0]->{DBH}); }

#------------------------------------------------------------------------------
# header
#------------------------------------------------------------------------------
sub header_html {
  return <<EOF;
 <div class=menubar><table width="100%"><tr><td class=links>&nbsp;
   <a class=o href="$ENV{SCRIPT_NAME}">Home</a> &nbsp;|&nbsp;
   <a class=m href="$ENV{SCRIPT_NAME}?filter=1">Filter</a> &nbsp;|&nbsp;
   <a class=m href="$ENV{SCRIPT_NAME}?last=1">Last report</a> &nbsp;|&nbsp;
   <a class=m href="$ENV{SCRIPT_NAME}?last=1;want_smoke=1">Last smoke</a> &nbsp;|&nbsp;
   <a class=m href="$ENV{SCRIPT_NAME}?failure=1">
  Last failures</a> &nbsp;|&nbsp;
   <a class=m href="FAQ.html">FAQ</a> &nbsp;|&nbsp;
   <a class=m href="http://qa.perl.org">About</a> &nbsp;|&nbsp;
   <a class=m href="mailto:alian\@cpan.org">Author</a> &nbsp;|&nbsp;
</td><td align=right></td></tr></table>
</div>
<h1>Perl-current smoke results</h1>
EOF

}

#------------------------------------------------------------------------------
# rundb
#------------------------------------------------------------------------------
sub rundb {
  my ($self,$cmd) = @_;
  foreach (split(/;/, $cmd)) {
    $_=~s/\n//g;
    next if (!$_ or $_ eq ';');
    print "mysql <-\t$_\n" if ($self->{opts}->{verbose});
    $self->{DBH}->do($_) || print "Error $_: $DBI::errstr!\n";
  }
}

#------------------------------------------------------------------------------
# rename_rpt
#------------------------------------------------------------------------------
sub rename_rpt {
  my $self = shift;
  foreach my $f (glob($self->{opts}->{dir}."/*.rpt")) {
    my $e=`grep "for patch" $f`;
    if ($e=~/for patch (\d+)/ or $e=~/for .* patch (\d*)/) {
      if (-e "$f.$1") { unlink($f); }
      else {
	print "Rename $f $1\n" if ($self->{opts}->{verbose});
	`mv $f $f.$1`;
      }
    }
  }

#  unlink("<$self->{opts}->{dir}/*.rpt>");
#  unlink("<$self->{opts}->{dir}/*.>");
}

#------------------------------------------------------------------------------
# suck_ng
#------------------------------------------------------------------------------
sub suck_ng {
  my $self = shift;
  # Find last id on dir
  my $max=0;
  my @l = glob($self->{opts}->{dir}."/*");
  foreach (@l) { $max=$1 if (/\/(\d*)\.rpt/ && $1 > $max); }
  print "NNTP max id is $max ($#l files in $self->{opts}->{dir})\n"
    if ($self->{opts}->{verbose});

  # Connect on ng
  print "Connect on $self->{opts}->{nntp_server}\n" 
    if ($self->{opts}->{verbose});
  my $c = new News::NNTPClient($self->{opts}->{nntp_server});

  # Fetch last - first
  my ($first, $last) = ($c->group("perl.daily-build.reports"));
  #print "Max:$max first:$first last:$last\n";
  if ($max) {
    if ($max == $last) {
      print "No new report on perl.daily-build.reports\n"
	if ($self->{opts}->{verbose});
      return;
    }
    else { $first = $max; }
  }

  while( $first <= $last) {
    open(F,">$self->{opts}->{dir}/$first.rpt") 
      or die "Can't create $self->{opts}->{dir}/$first.rpt:$!\n";
    my @buf = $c->article($first);
    my ($ok,$isreport,$buf)=(0,1);
    foreach (@buf) {
      if (/In-Reply-To/) { $isreport=0; last;}
      next if (/From:/);
      print F $_;
    }
    close(F);
    if (!$isreport) { unlink("$first.rpt"); }
    $first++;
  }
  $self->rename_rpt();
}

#------------------------------------------------------------------------------
# filter
#------------------------------------------------------------------------------
sub filter {
  my $d = shift;
  my %t =
    (
     'os' => '1 - Os',
     'osver' =>'2 - Version OS',
     'archi' =>'3 - Architecture',
     'cc'=>'4 - Compiler',
     'ccver'=>'5 - Compiler version',
     'smoke'=>'6 - Only this smoke',
     'last_smoke'=>'7 - Nothing before this smoke'
    );
  my $bi = h2("Filter").start_form({-method=>'GET'})."<table border=1><tr>";
  $bi.= hidden({-name=>'last',-value=>1}) if (param('last'));
  $bi.= hidden({-name=>'failure',-value=>1}) if (param('failure'));
  foreach my $o (sort { $t{$a} cmp $t{$b} } keys %t) {
    $bi.='<tr><td>'.$t{$o}.'</td><td>'.
      "<select name=\"".$o."_fil\"><option value=\"All\">All</option>";
    my $r = $o;
#    print STDERR $r,"\n";
    $r = 'smoke' if ($o eq 'last_smoke');
    my @l = @{$d->distinct($r)};
    @l = reverse @l if ($o eq 'smoke');
    my $v = param($o) || param($o.'_fil') || cookie($o) || undef;
    $v = $limite if (!$v and $o eq 'last_smoke');
    foreach my $name (@l) {
      my $sname = (($o eq 'ccver') ? substr($name,0,10) : $name);
      $sname = substr($sname,0,15) if ($o eq 'cc');
      if (($v and $v eq $name) or (!$v and $name eq 'Last') or
	 ($o eq 'last_smoke' and $name eq $limite)) {
	$bi.="<option selected value=\"$name\">$sname</option>\n";
      } else {
	$bi.="<option value=\"$name\">$sname</option>\n";
      }
    }
    $bi.="</select></td></tr>";
  }
  $bi.= Tr(td(),td(submit))."</table>".end_form;
  return $bi;
}

#------------------------------------------------------------------------------
# display
#------------------------------------------------------------------------------
sub display {
  my ($self,$os,$osver,$ar,$cc,$ccver,$smoke)=@_;
  my ($i,$summary,$details,$failure,$class)=(0);
  # Walk on each smoke
  $summary = h2("Summary - ".$self->nb." reports available after smoke $limite.").
    "<div class=\"box\"><table border=1>".
    Tr(th("Os"),th("Os version"),th("Archi"),th("Compiler"),th("Version compiler"),
       th("Last smoke"),th("Configuration<br>tested|pass"),
       th("Tests fails"))."\n";
  my $ref = $self->read_all;
  my ($lasta,$lastosv,$lastcc,$lastccv,$lastar,$oss,$osvv,$ccc,$ccvv,$arr)=
    (" "," "," "," "," ");
  my (@los,@ls,@lccver,@lcc);
  # By os
  if ($os and $os ne 'All') { push(@los,lc($os)); }
  else { @los = sort keys %$ref}
  foreach my $os (@los) {
    # By os version
    my @losver;
    if ($osver and $osver ne 'All') { push(@losver,$osver); }
    else { @losver = sort keys %{$$ref{$os}}; }
    $lastosv = " ";
    foreach my $osver (@losver) {
      # By arch
      my @lar;
      if ($ar and $ar ne 'All') { push(@lar,$ar); }
      else { @lar = sort keys %{$$ref{$os}{$osver}}; }
      $lastar= " ";
      foreach my $ar (@lar) {
	# By cc
	undef @lcc;
	if ($cc and $cc ne 'All') { push(@lcc,$cc); }
	else { @lcc = sort keys %{$$ref{$os}{$osver}{$ar}}; }
	$lastcc=" ";
	foreach my $cc (@lcc) {
	  # By ccver
	  undef @lccver;
	  if ($ccver and $ccver ne 'All') { push(@lccver,$ccver); }
	  else { @lccver = sort keys %{$$ref{$os}{$osver}{$ar}{$cc}}; }
	  $lastccv=" ";
	  foreach my $ccver (@lccver) {
	    # By smoke
	    undef @ls;
	    if ($smoke && $smoke eq 'All') {
	      @ls = reverse sort keys %{$$ref{$os}{$osver}{$ar}{$cc}{$ccver}}; 
	    }
	    elsif (!$smoke or $smoke eq 'Last') { 
	      # On prend le dernier smoke
	      @ls = reverse sort keys %{$$ref{$os}{$osver}{$ar}{$cc}{$ccver}};
	      @ls = shift @ls;
	    }
	    else { @ls =($smoke);  }

	  foreach my $smoke (sort @ls) {
	    next if (!$$ref{$os}{$osver}{$ar}{$cc}{$ccver}{$smoke});
	    my ($nbt,$nbc,$nbto)=
	      ($$ref{$os}{$osver}{$ar}{$cc}{$ccver}{$smoke}{nbte},
	       $$ref{$os}{$osver}{$ar}{$cc}{$ccver}{$smoke}{nbc},
	       $$ref{$os}{$osver}{$ar}{$cc}{$ccver}{$smoke}{nbco});
	    my $id = $$ref{$os}{$osver}{$ar}{$cc}{$ccver}{$smoke}{id};
	    # debut des tableaux erreurs et details
	    my $de = "<a name=\"$id\"></a> <table width=\"80%\">".
	      Tr(th({-colspan=>2},"$os $osver $ar $cc $ccver smoke patch $smoke"));
	    # Liste des tests echoues
	    if ($$ref{$os}{$osver}{$ar}{$cc}{$ccver}{$smoke}{failure}) {
	      (my $f = $$ref{$os}{$osver}{$ar}{$cc}{$ccver}{$smoke}{failure})
		=~s/\n/<br>/g;
	      $nbt = ($f =~ tr/FAILED//);
	      if (param('failure')) {$failure.=$de.Tr(td($f))."</table><br>"; }
	    }
	
	    # Liste des configs testees
	    if (ref($$ref{$os}{$osver}{$ar}{$cc}{$ccver}{$smoke}{build})) {
	      my $r2 = 1;
	      my $dets;
	      foreach my $config (sort keys %{$$ref{$os}{$osver}{$ar}{$cc}{$ccver}{$smoke}{build}}) {
		$dets.= "<tr>".td($config);
		my $r = 1;
		foreach my $v (split(/ /,$$ref{$os}{$osver}{$ar}{$cc}{$ccver}{$smoke}{build}{$config})) {
		  my $u = $ENV{SCRIPT_NAME}."?failure=1&smoke=$smoke";
		  $u.=$self->compl_url if ($self->compl_url);
		  $u.="#$id" if ($id);
		  if (($v ne 'O') and ($v ne '?')) {
		    $v = a({-href=>$u},$v); $r=0; $r2=0;
		  }
		  $dets.=td({width=>5},$v);
		}
		$dets.="</tr>";
		$nbto+=$r;
		$nbc++;
	      }
	      $details.=$de.$dets."</table><br>"
		if (!param('want_smoke') or !$r2);
	    }
	    # Sommaire
	    if ($lasta ne $os) { $oss = $os; $lasta = $os; $class=($i++)%2;}
	    else { $oss=" "; }
	    if ($lastcc ne $cc) { $ccc = $cc; $lastcc = $cc; }
	    else { $ccc=" "; }
	    if ($lastccv ne $ccver) { $ccvv = $ccver; $lastccv = $ccvv; }
	    else { $ccvv=" "; }
	    if ($lastosv ne $osver) { $osvv = $osver; $lastosv = $osver; }
	    else { $osvv=" "; }
	    if ($lastar ne $ar) { $arr = $ar; $lastar = $ar; }
	    else { $arr=" "; }
	    if ($nbt) {
	      my $u = $ENV{SCRIPT_NAME}."?failure=1&smoke=$smoke";
	      $u.=$self->compl_url if ($self->compl_url);
	      $u.="#$id" if ($id);
	      $nbt=a({-href=>$u,-class=>'red'},$nbt);
	      $nbt = td({-align=>"center", -class=>'red'},$nbt);
	    }
	    else { $nbt=td({-align=>"center"},0); }
	    my $u = $ENV{SCRIPT_NAME}."?last=1";
	    $u.= $self->compl_url if ($self->compl_url);
	    $u.="#$id" if ($id);
	    $nbto=font({-color=>"red"},$nbto) if ($nbc!=$nbto);
	    $summary.=Tr({-class=>"mod".$class},
			 td($oss),td($osvv),td($arr),td($ccc),td($ccvv),
			 td({-align=>"center"},a({-href=>$u}, $smoke)),
			 td(table({-border=>1}, Tr(td({-width=>"50"},$nbc),
						   td({-width=>"50"},$nbto)))),
			 $nbt."\n");
	  }
	  }
	}
      }
    }
  }
  $summary.="</table></div>";
  return (\$summary,\$details,\$failure);
}

#------------------------------------------------------------------------------
# compl_url
#------------------------------------------------------------------------------
sub compl_url {
  my $self = shift;
  my $buf;
  foreach ('os','osver','archi','cc','ccver','smoke') {
    $buf.="&$_=".param($_) if (param($_));
  }
  return $buf;
}

#------------------------------------------------------------------------------
# parse_import
#------------------------------------------------------------------------------
sub parse_import {
  my $self = shift;
  my ($nb,$nbo,%k) = (0,0);

  # Select list of knows id
  my $st = $self->{DBH}->prepare('select distinct id from builds');
  $st->execute;
  while (my ($id)= $st->fetchrow_array) { $k{$id}=1; }
  $st->finish;

  # Read a .rpt file
  foreach (glob($self->{opts}->{dir}."/*.rpt*")) {
    $nb++;
    # skip backup file or already defined report
    next if (/~$/ or ( /(\d*)\.rpt/ && $k{$1}));
    my $ref = $self->parse_rpt($_);
    if (!defined($ref)) { 
      print STDERR "Can't read/parse $_\n" 
	if ($self->{opts}->{verbose});
    }
    elsif (!ref($ref)) {
      if ($ref == -1) {
	my @l = $self->parse_hm_brand_rpt($_);
	foreach (@l) {
	  next if (!$_->{id} or $k{$_->{id}});
	  print STDERR "Add a H.M. Brand report\n"
	    if ($self->{opts}->{verbose});
	  $self->add_to_db($_) && $nbo++;
	  $k{$_->{id}}=1;
	}
      } elsif ($ref == -2) {
	print "\tSeems to be a DEAD report, will be unlink\n"
	  if ($self->{opts}->{verbose});
	unlink $_;
      } elsif ($ref == -3) {
	print "\tSeems to be a Alian report with too more rows, will be unlink\n"
	  if ($self->{opts}->{verbose});
	unlink $_;
      }
    }
    else {
      # Add it to database
      print STDERR "Add report $_\n" if ($self->{opts}->{verbose});
      $self->add_to_db($ref) && $nbo++;
    }
  }
  print "$nbo reports imported from $nb files\n" if ($self->{opts}->{verbose});
}

#------------------------------------------------------------------------------
# parse_hm_brand_rpt
#------------------------------------------------------------------------------
sub parse_hm_brand_rpt {
  my ($self,$file)=@_;
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
    elsif ($ok >5 && ($l=~/^O/ || $l=~/^F/ || $l=~/^m/)) {
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
	$lr[$i]->{build}{$conf} = $_ if ($_!~m!^\s*$!);
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
	if ($a->{os} eq "cygwin") {
	  $ln{$i++} = $a->{os}." ".substr($a->{osver},0,3);
	} else { $ln{$i++} = $a->{os}." ".$a->{osver}; }
      }
      foreach my $n (keys %ln) {
	if ($l=~/^$ln{$n}/i) {
	  $lr[$n]->{failure}.=$l if ($lr[$n]);
	  $last{$n}=1;
	  $r=1; #last;
	}
      }
      if (!$r) {
	if ($l=~/^\s+/ && %last) {
	  foreach (keys %last) { $lr[$_]->{failure}.=$l if ($lr[$_]);  }
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
    $r->{nbte} = ( $r->{failure} ? ($r->{failure}=~tr/FAILED//) : 0);
    $r->{nbc} = scalar keys %{$r->{build}};
    $r->{nbco} = 0;
    $r->{id} = $r->{smoke}.$ok;
  }
  return @lr;
}
 
#------------------------------------------------------------------------------
# parse_rpt
#------------------------------------------------------------------------------
sub parse_rpt {
  my ($self,$file)=@_;
  my ($nbr,$fail,%h,$col,$content)=(0);
  return if (!$file);
  if (!-r $file) { warn "Can't found $file"; return; }
  open(FILE,$file) or die "Can't read $file:$!\n";
  my @content = <FILE>;
  close(FILE);
  my $r = 0;
  # Rebuild report wrapped by mail to 72c
  my $cont;
  foreach my $l (@content) {
    chomp($l);
    if ($l=~/=$/) { chop($l); $r=1; }
    if ($r) { $cont.=$l; $r=0; }
    else { $cont.=$l."\n"; }
  }
  return undef if (!$cont);
  foreach my $l (split(/\n/, $cont)) {
    $content.=$l;
    chomp($l);
    $nbr++ if ($l=~/^>/);
    if ($l=~/^From:/ && $l=~/Brand/) { $col=-1; }
    elsif ($l=~/^From:/ && $l=~/Alian/) { $col=-3; }
    elsif ($l=~/^Return-Path: <h.m.brand\@hccnet.nl>/) { $col=-1; }
    # A report without info about os
    elsif ($l=~/Automated smoke report for patch (\d+) on  - $/) { return -2; }
    # A report without info about os
    elsif ($l=~/Automated smoke report for patch (\d*) on  -  \(\)$/) { return -2; }
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
    }
    elsif ($l=~/Automated smoke report for patch (\d*)$/) {
      ($h{smoke}) = ($1);
    }
    elsif ($l=~/on (.*) using (.*) version (.*)$/) {
      ($h{os}, $h{cc},$h{ccver},$h{osver}) = ($1,$2,$3,"??");
    }
    elsif ($l=~/using (.*) version (.*)$/) {
      ($h{cc}, $h{ccver}) = ($1,$2);
    }
    elsif ($l=~/^([\w?-] [\w?-] [\w?-] [\w?-]) ?\|? ?(.*)$/) {
      my $c = $2; 
      $c=' ' if (!$c);
      $h{"build"}{$c} = $1;
    }
    elsif ($l=~/Failures(.*):/) { $fail=1; }
    elsif ($fail) { $h{"failure"}.=$l."\n"; }
  }

#  print $h{failure},"\n";
  if ($h{build} && $h{os}) {
    $h{ccver}="??" if (!$h{ccver});
    $h{nbte} = ( $h{failure} ? ($h{failure}=~tr/FAILED//) : 0);
    $h{nbc} = scalar keys %{$h{build}};
    $h{nbco} = 0;
#    $h{report}= $content;
    $h{id}=$1 if ($file=~/(\d+)\.rpt/ or $file=~/(\d+)\.normal\.rpt/);
    return \%h
  }
  # More than 8 lines beginning with '>', seems to be a reply
  if ($nbr>8) {
    print "\t Seems to be a reply\n" if ($self->{opts}->{verbose}); 
    return -2;
  }
  elsif ($self->{opts}->{verbose} && !$col) { print "No build or os\n"; }
  return ($col ? $col : undef);
}

#------------------------------------------------------------------------------
# read_all
#------------------------------------------------------------------------------
sub read_all {
  my $self = shift;
  my (%h2,@lid,$req,%f,$a);
# if (param('smoke') or ($limite and (param('last') or param('failure')))) {
# if (param('smoke') or $limite) {
  $req = "select id from builds ";
  if (param('smoke')) { $a.="smoke =".param('smoke'); }
  else { $a.="smoke >=$limite"; }
  foreach my $o ('cc','ccver','os','osver','archi') {
    my $v = param($o) || param($o.'_fil') || cookie($o) || undef;
    next if (!$v or $v eq 'All');
    $a.=" and " if ($a);
    $a.="$o='$v' ";
  }
  $req.="where $a" if ($a);
  print STDERR $req,"\n";
  my $st3 = $self->{DBH}->prepare($req);
  $st3->execute or print STDERR "On $req";
  while (my @l = $st3->fetchrow_array) {
    push(@lid, shift @l);
  }
  $st3->finish();
  #  }

  if (param('failure')) {
    $req = "select idbuild,failure from data";
    if (@lid) { $req.=" where idbuild in (".join(",",@lid).")"; }
    my $st2 = $self->{DBH}->prepare($req);
    $st2->execute or print STDERR "On $req";
    print STDERR $req,"\n";
    while (my @l = $st2->fetchrow_array) {
      my $id = shift @l;
      $f{$id} = shift @l;
    }
    $st2->finish;
  }

  if (param('last')) {
    $req = "select idbuild,configure,result from configure";
    if (@lid) { $req.=" where idbuild in (".join(",",@lid).")"; }
    print STDERR $req,"\n";
    my $st2 = $self->{DBH}->prepare($req);
#    print $req,"<br>";
    $st2->execute or print STDERR "On $req";
    while (my @l = $st2->fetchrow_array) {
      my $id = shift @l;
      push(@{$h2{$id}}, \@l);
    }
    $st2->finish();
  }

  $req = <<EOF;
select id,os,osver,archi,cc,ccver,date,smoke,nbc,nbco,nbte
from builds
EOF
  if (@lid) { $req.=" where id in (".join(",",@lid).")"; }
    print STDERR $req,"\n";
  my $st = $self->{DBH}->prepare($req);
  $st->execute or print STDERR $req,"<br>";
  my %h;
  while (my ($id,$os,$osver,$archi,$cc,$ccver,$date,$smoke,$nbc,$nbco,$nbte)=
	 $st->fetchrow_array) {
    $os=lc($os);
    $h{$os}{$osver}{$archi}{$cc}{$ccver}{$smoke}{date}=$date;
    $h{$os}{$osver}{$archi}{$cc}{$ccver}{$smoke}{id} = $id;
    $h{$os}{$osver}{$archi}{$cc}{$ccver}{$smoke}{nbc} = $nbc;
    $h{$os}{$osver}{$archi}{$cc}{$ccver}{$smoke}{nbco} = $nbco;
    $h{$os}{$osver}{$archi}{$cc}{$ccver}{$smoke}{nbte} = $nbte;
    $h{$os}{$osver}{$archi}{$cc}{$ccver}{$smoke}{failure} =$f{$id} 
      if ($f{$id});
    foreach (@{$h2{$id}}) {
      $h{$os}{$osver}{$archi}{$cc}{$ccver}{$smoke}{build}{$_->[0]}
	= $_->[1] if ($_);
    }
  }
  $st->finish;
  undef %h2; undef @lid;
  return \%h;
}

#------------------------------------------------------------------------------
# distinct
#------------------------------------------------------------------------------
sub distinct {
  my ($self, $col)=@_;
  my @res;
  my $req = "select distinct $col from builds where smoke>=$limite 
             order by $col";
  my $st = $self->{DBH}->prepare($req);
  print STDERR $req,"\n";
  $st->execute or confess($req) && return undef;
  while (my @l =$st->fetchrow_array) { push(@res,join('-',@l)); }
  $st->finish;
  if ($col eq 'smoke') { unshift(@res, "Last"); }
  return \@res;
}

#------------------------------------------------------------------------------
# nb
#------------------------------------------------------------------------------
sub nb {
  my $self = shift;
  my $req = "select count(*) from builds where smoke >= $limite";
  print STDERR $req,"\n";
  my $st = $self->{DBH}->prepare($req);
  $st->execute or return undef;
  my ($nb) =$st->fetchrow_array;
  $st->finish;
  return $nb;
}

#------------------------------------------------------------------------------
# add_to_db
#------------------------------------------------------------------------------
sub add_to_db {
  my ($self, $ref)=@_;
  print STDERR Data::Dumper->Dump([$ref]) if ($self->{opts}->{verbose});
  return if (!$ref->{os});
  my ($nbco,$vf)=(0,0);
  my ($cc,$ccf,$f,$r) = ($ref->{cc},$ref->{ccver} || ' ',
			 $ref->{failure},$ref->{report});
  foreach ($cc,$ccf,$f,$r) { s/'/\\'/g if ($_); }
  foreach my $c (keys %{$$ref{build}}) {
    $vf=1;
    foreach (split(/ /,$$ref{build}{$c})) {
      $vf=0 if ($_ ne 'O' and $_ ne '?');
    }
    $nbco+=$vf;
  }
  # Ajout des infos sur le host
  my $req = "INSERT INTO builds(";
  $req.= 'id,' if ($ref->{id});
  $req.= "os,osver,cc,ccver,date,smoke,nbc,nbco,nbte) VALUES (";
  $req.= "$ref->{id}," if ($ref->{id});
  $req.= <<EOF;
'$ref->{os}', '$ref->{osver}','$cc','$ccf', NOW(),
        $ref->{smoke}, $ref->{nbc}, $nbco, $ref->{nbte})
EOF
#  print $req,"\n";
  my $st = $self->{DBH}->prepare($req);
  if (!$st->execute) {
    cluck($DBI::err." on $req");
    return;
  }
  # id du test
  my $id =  $st->{'mysql_insertid'};

  # Ajout des details des erreurs
  $r = ' ' if (!$r);
  $f = ' ' if (!$f);
  $req = <<EOF;
INSERT INTO data(idbuild,failure)
VALUES ($id, '$f')
EOF
    $self->{DBH}->do($req) or print STDERR "On $req\n";

  # Ajout des options du configure
  foreach my $config (keys %{$$ref{build}}) {
    my $co = $config; $co=~s/'/\\'/g;
    my $v = $$ref{build}{$config};
    $v=~s/'/\\'/g;
    $req = <<EOF;
INSERT INTO configure (idbuild,configure,result)
VALUES ($id,'$co','$v')
EOF
 #   print $req,"\n";
    $self->{DBH}->do($req) or print STDERR "On $req\n";
  }
  return ($DBI::errstr ? 0 : 1);
}
#------------------------------------------------------------------------------
# POD DOC
#------------------------------------------------------------------------------


=head1 NAME

Test::Smoke::Database - Add / parse /display perl reports smoke database

=head1 SYNOPSIS

  $ admin_smokedb --create --suck --import --update_archi
  $ lynx http://localhost/cgi-bin/smokedb.cgi
 

=head1 DESCRIPTION

This module help to build an application that parses smoke-reports for
perl-current and puts the results in a database. This allows for a simple
overview of the build status on as wide a variety of supported platforms 
(operating system/architecture) as possible.

This distribution come with 2 perl scripts:

=over

=item admin_smokedb

Fetch / Import smoke report in a mysql database. See L<admin_smokedb>

=item smokedb.cgi

A www interface to browse this smoke database

=back

=head1 SEE ALSO

L<admin_smokedb>, L<Test::Smoke::Database::FAQ>, L<Test::Smoke>,
L<http://www.alianwebserver.com/perl/smoke/smoke_db.cgi>

=head1 METHODS

=over 4

=item B<new> I<hash reference>

Construct a new Test::Smoke::Database object and return it. This call too
connect method of DBD::Mysql and store dbh in $self->{DBH} except if 
key I<no_dbconnect> is found in I<hash reference>. Disconnect method is
auto called with DESTROY call.

=item B<rundb> I<SQL request>

This will do like $dbh->do, but several request can be put in SQL request,
separated by ';'

=back

=head2 Method for cgi

=over 4

=item B<nb>

Return the number of reports found after limit.

=item B<header_html>

Return the HTML menubar that will be displayed in the CGI

=item B<filter>

Return the HTML filter screen.

=item B<display>

Return the main HTML screen with summary

=item B<distinct> I<col of mysql table 'smoke'>

=back

=head2 Actions for admin_smokedb

See L<admin_smokedb>

=over 4

=item B<parse_import>

As his name say, this method will parse and import fetched report found
in $self->{opts}->{dir} and put them in database.

=item B<suck_ng>

Fetch new report from perl.daily-build.reports

=back

=head2 Private methods

=over 4

=item B<read_all>

=item B<compl_url>

=item B<parse_rpt> I<file>

This method is call by parse_import.
Parse I<file> and return values parsed in a reference of hash.
Else return -1 for a H.M. Brand report (then B<parse_hm_brand_rpt> will be 
called), -2 for a bad report, ie a report without os/osver (this report will be
deleted), -3 for an Alian multi-col report (deleted too).

=item B<parse_hm_brand_rpt> I<file>

Do a specific parsing for H.M Brand report I<file>. (his report is multi-col).
Return a list of reference of report to use with add_db.

=item B<rename_rpt>

Rename fetched report to add no of smoke in name of file.
For all reports found, this will append at end of name the number of smoke.
After that all *. and *.rpt file will be deleted. This method is auto. called
after B<fetch> method.

=back

=head1 VERSION

$Revision: 1.4 $

=head1 AUTHOR

Alain BARBET with some help from Abe Timmerman

=cut

1;
