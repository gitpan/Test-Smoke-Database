package Test::Smoke::Database::DB;

# Test::Smoke::Database::DB
# Copyright 2003 A.Barbet alian@alianwebserver.com.  All rights reserved.
# $Date: 2003/08/08 14:27:59 $
# $Log: DB.pm,v $
# Revision 1.3  2003/08/08 14:27:59  alian
# Update POD documentation
#
# Revision 1.2  2003/08/07 18:01:44  alian
# Update read_all to speed up requests
#
# Revision 1.1  2003/08/06 18:50:41  alian
# New interfaces with DB.pm & Display.pm
#

use Carp;
use strict;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK);
use DBI;
use Data::Dumper;
use Carp qw(cluck);
use File::Basename;
require Exporter;

@ISA = qw(Exporter);
@EXPORT = qw();
$VERSION = ('$Revision: 1.3 $ ' =~ /(\d+\.\d+)/)[0];
use vars qw/$debug $verbose $limit/;
#$limite = 0;

#------------------------------------------------------------------------------
# new
#------------------------------------------------------------------------------
sub new   {
  my $class = shift;
  my $self = {};
  my $indexer = shift;
  bless $self, $class;
  $self->{DBH} = $indexer->{DBH};
  $self->{CGI} = $indexer->{opts}->{cgi};
  $debug = ($indexer->{opts}->{debug} ? 1 : 0);
  $verbose = ($indexer->{opts}->{verbose} ? 1 : 0);
  $limit = $indexer->{opts}->{limit};
  return $self;
}

#------------------------------------------------------------------------------
# DESTROY
#------------------------------------------------------------------------------
sub DESTROY {
  $_[0]->{DBH}->disconnect if ($_[0]->{DBH});
  print scalar(localtime),": Finished\n" if $verbose;
}

#------------------------------------------------------------------------------
# rundb
#------------------------------------------------------------------------------
sub rundb(\%\%) {
  my ($self,$cmd,$nochomp) = @_;
  my $ret = 1;
  foreach (split(/;/, $cmd)) {
    $_=~s/\n//g if (!$nochomp);
    next if (!$_ or $_ eq ';');
    print "mysql <-\t$_\n" if ($debug);
    if (!$self->{DBH}->do($_)) {
      print STDERR "Error $_: $DBI::errstr!\n";
      $ret = 0;
    }
  }
  return $ret;
}

#------------------------------------------------------------------------------
# read_all
#------------------------------------------------------------------------------
sub read_all(\%) {
  my $self = shift;
  my $cgi = $self->{CGI};
  return {} if (!$self->{DBH});
  my ($req,%h2);

  # $a is SQL restriction on database
  my $a;
  if ($cgi->param('smoke')) { $a.="smoke =".$cgi->param('smoke'); }
  else { $a.="smoke >=$limit"; }
  foreach my $o ('cc','ccver','os','osver','archi') {
    my $v = $cgi->param($o) || $cgi->param($o.'_fil') 
      || $cgi->cookie($o) || undef;
    next if (!$v or $v eq 'All');
    $a.=" and " if ($a);
    $a.="$o='$v' ";
  }

  # Select id of build for failure & details
  my $list_id;
  if ($cgi->param('failure') || ($cgi->param('last'))) {
    my $req = "select id from builds ";
    $req.="where $a" if ($a);
    my $ref_lid = $self->{DBH}->selectcol_arrayref($req) ||
      print "On $req: $DBI::errstr\n";
    $list_id = join("," , @$ref_lid);
  }

  # Failure
  my $ref_failure;
  if ($cgi->param('failure')) {
    $req = "select idbuild,failure from data";
    if ($list_id) { $req.=" where idbuild in (".$list_id.")"; }
    $ref_failure = $self->{DBH}->selectall_hashref($req, 'idbuild') ||
      print "On $req: $DBI::errstr\n";
  }

  # Detailed results
  if ($cgi->param('last')) {
    $req = "select idbuild,configure,result from configure ";
    if ($list_id) { $req.=" where idbuild in (".$list_id.")"; }
    my $ref_result = $self->{DBH}->selectall_arrayref($req) ||
      print "On $req: $DBI::errstr\n";
    foreach my $ra (@$ref_result) {
      $h2{$ra->[0]}{$ra->[1]} = $ra->[2];
    }
  }

  # Each times, read config
  $req = <<EOF;
select id,os,osver,archi,cc,ccver,date,smoke,nbc,nbco,
       nbcm,nbcf,nbcc,nbte,matrix
from builds
EOF
   $req.="where $a" if ($a);
  my $st = $self->{DBH}->prepare($req);
  $st->execute || print STDERR $req,"<br>";
  my %h;
  while (my ($id,$os,$osver,$archi,$cc,$ccver,$date,$smoke,$nbc,$nbco,
	     $nbcm,$nbcf,$nbcc,$nbte,$matrix)=
	 $st->fetchrow_array) {
    $os=lc($os);
    $h{$os}{$osver}{$archi}{$cc}{$ccver}{$smoke}{date}=$date;
    $h{$os}{$osver}{$archi}{$cc}{$ccver}{$smoke}{id} = $id;
    $h{$os}{$osver}{$archi}{$cc}{$ccver}{$smoke}{nbc} = $nbc;
    $h{$os}{$osver}{$archi}{$cc}{$ccver}{$smoke}{nbco} = $nbco;
    $h{$os}{$osver}{$archi}{$cc}{$ccver}{$smoke}{nbcf} = $nbcf;
    $h{$os}{$osver}{$archi}{$cc}{$ccver}{$smoke}{nbcc} = $nbcc;
    $h{$os}{$osver}{$archi}{$cc}{$ccver}{$smoke}{nbcm} = $nbcm;
    $h{$os}{$osver}{$archi}{$cc}{$ccver}{$smoke}{nbte} = $nbte;
    $h{$os}{$osver}{$archi}{$cc}{$ccver}{$smoke}{matrix} = $matrix;
    $h{$os}{$osver}{$archi}{$cc}{$ccver}{$smoke}{nbtt} =
      $nbcf + $nbcm + $nbco + $nbcc;
    $h{$os}{$osver}{$archi}{$cc}{$ccver}{$smoke}{failure} =
      $ref_failure->{$id}{failure} if ($ref_failure->{$id});
    $h{$os}{$osver}{$archi}{$cc}{$ccver}{$smoke}{build} = $h2{$id}
      if ($h2{$id});
  }
  $st->finish;
  return \%h;
}

#------------------------------------------------------------------------------
# distinct
#------------------------------------------------------------------------------
sub distinct(\%$) {
  my ($self, $col)=@_;
  my $req = "select distinct $col from builds where smoke>=$limit 
             order by $col";
  return $self->{DBH}->selectcol_arrayref($req) || undef;
}

#------------------------------------------------------------------------------
# nb
#------------------------------------------------------------------------------
sub nb(\%) {
  my $self = shift;
  my $req = "select count(*) from builds where smoke >= $limit";
  return $self->one_shot($req);
}

#------------------------------------------------------------------------------
# last50
#------------------------------------------------------------------------------
sub last50(\%) {
  my $self = shift;
  my $req = 'select max(smoke)-50 from builds';
  return $self->one_shot($req);
}

#------------------------------------------------------------------------------
# one_shot
#------------------------------------------------------------------------------
sub one_shot(\%$) {
  my ($self, $req) = @_;
  return if (!$self->{DBH});
  my $row_ary = $self->{DBH}->selectrow_arrayref($req) || return undef;
  print STDERR $req,"\n", Data::Dumper->Dump([$row_ary]) if $debug;
  return $row_ary->[0] || undef;
}

#------------------------------------------------------------------------------
# add_to_db
#------------------------------------------------------------------------------
sub add_to_db(\%\%) {
  my ($self, $ref)=@_;
  return if (!ref($ref) || ref($ref) ne 'HASH' || !$ref->{os});
  my ($nbco, $nbcf, $nbcm, $nbcc)=(0,0,0,0);
  my ($cc,$ccf,$f,$r) = ($ref->{cc}||' ',$ref->{ccver} || ' ',
			 $ref->{failure},$ref->{report});
  foreach ($cc,$ccf,$f,$r) { s/'/\\'/g if ($_); }
  $ref->{osver}=~s/[\s]+$//g;
  # Count make test ok / build fail in make / configure fail / make test fail
  foreach my $c (keys %{$$ref{build}}) {
    foreach (split(/ /,$$ref{build}{$c})) {
      if ($_ eq 'O') { $nbco++; }
      elsif ($_ eq 'F') { $nbcf++; }
      elsif ($_ eq 'm') { $nbcm++; }
      elsif ($_ eq 'c') { $nbcc++; }
    }
  }
  my $pass = 1;
  $pass = 0 if ($ref->{failure});
  printf( "\t =>%25s %s %5d (%s)\n",
	  $ref->{os}." ".$ref->{osver}, ($pass ? "PASS" : "FAIL"),
	  $ref->{smoke}, basename($ref->{file})) if $verbose;
  # Ajout des infos sur le host
  my $v2 = ($ref->{matrix} ? join("|", @{$ref->{matrix}}) : '');
  my $req = "INSERT INTO builds(";
  $req.= 'id,' if ($ref->{id});
  $req.= "os,osver,cc,ccver,date,smoke,nbc,nbco,nbcf,nbcm,nbcc,nbte,archi,matrix) ".
    "VALUES (";
  $req.= "$ref->{id}," if ($ref->{id});
  $req.= <<EOF;

'$ref->{os}',
'$ref->{osver}',
'$cc',
'$ccf',
NOW(),
$ref->{smoke},
$ref->{nbc},
$nbco,
$nbcf,
$nbcm,
$nbcc,
$ref->{nbte},
'$ref->{archi}',
'$v2')
EOF

  print $req,"\n" if $debug;
  my $st = $self->{DBH}->prepare($req);
  if (!$st->execute) {
    print STDERR "SQL: $req\n", Data::Dumper->Dump([$ref]);
    cluck($DBI::errstr);
    return;
  }
  # id du test
  my $id =  $st->{'mysql_insertid'};
  $ref->{id}=$id;
  print STDERR Data::Dumper->Dump([$ref]) if $debug;

  # Ajout des details des erreurs
  $r = ' ' if (!$r);
  $f = ' ' if (!$f);
  $req = <<EOF;
INSERT INTO data(idbuild,failure)
VALUES ($id, '$f')
EOF
    $self->rundb($req,1) || print STDERR "On $req\n";

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
    $self->rundb($req,1) or print STDERR "On $req\n";
  }
  return ($DBI::errstr ? 0 : 1);
}

__END__

#------------------------------------------------------------------------------
# POD DOC
#------------------------------------------------------------------------------


=head1 NAME

Test::Smoke::Database::DB - Interface for smoke database

=head1 SYNOPSIS

  my $a = new Test::Smoke::Database;
  $a->db->rundb("SQL request");

=head1 DESCRIPTION

This module give all mysql method for manipulate smoke database

=head1 SEE ALSO

L<admin_smokedb>, L<Test::Smoke::Database>,
L<http://www.alianwebserver.com/perl/smoke/smoke_db.cgi>

=head1 METHODS

=over 4

=item B<new> I<hash reference>

Construct a new Test::Smoke::Database object and return it. This call too
connect method of DBD::Mysql and store dbh in $self->{DBH} except if 
key I<no_dbconnect> is found in I<hash reference>. Disconnect method is
auto called with DESTROY call if needed.

=item B<rundb> I<SQL request>

This will do like $dbh->do, but several request can be put in SQL request,
separated by ';'. Return 1 on sucess, 0 if one of request failed. If failed,
reason is printed on STDERR.

=back

=head2 Private methods

=over 4

=item B<read_all>

=back

=head1 VERSION

$Revision: 1.3 $

=head1 AUTHOR

Alain BARBET with some help from Abe Timmerman

=cut

1;
