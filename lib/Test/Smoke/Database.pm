package Test::Smoke::Database;

# Test::Smoke::Database - Add / parse /display perl reports smoke database
# Copyright 2003 A.Barbet alian@alianwebserver.com.  All rights reserved.
# $Date: 2003/08/02 12:39:05 $
# $Log: Database.pm,v $
# Revision 1.9  2003/08/02 12:39:05  alian
# Use dbi method like selectrow_array
#
# Revision 1.8  2003/07/30 22:07:34  alian
# - Move away parsing code in Parsing.pm
# - Update POD documentation
#
# Revision 1.7  2003/07/19 18:12:16  alian
# Use a debug flag and a verbose one. Fix output
#
# Revision 1.6  2003/02/16 18:47:04  alian
# - Update summary table:add number of configure failed, number of make failed.
# - Add legend after summary table
# - Add parsing/display of matrice,as Test::Smoke 1.16_15+ can report more than
# 4 columns
# - Correct a bug that add a 'Failure:' in HM Brand Report
#
# Revision 1.5  2003/02/10 00:58:05  alian
# - Add feature of graph
# - Correct Irix report parsing (no os version)
# - Correct number of failed test
# - Read archi from 1.16 report
# - Update parsing of error of HM Brand report
# - Update display for cgi
#
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
use Test::Smoke::Database::Graph;
use Test::Smoke::Database::Parsing;
use Carp qw(cluck);
use File::Basename;
require Exporter;

@ISA = qw(Exporter);
@EXPORT = qw();
$VERSION = ('$Revision: 1.9 $ ' =~ /(\d+\.\d+)/)[0];

my $limite = 18600;
#$limite = 0;

#------------------------------------------------------------------------------
# new
#------------------------------------------------------------------------------
sub new($$)   {
  my $class = shift;
  my $self = {};
  bless $self, $class;
  $self->{opts} = shift || {};
  my $driver = "DBI:mysql:database=".$self->{opts}->{database}.
    ";host=localhost;port=3306";
  if (!$self->{opts}->{no_dbconnect}) {
    $self->{DBH} = DBI->connect($driver,
				$self->{opts}->{user},
				$self->{opts}->{password} || undef)
      || die "Can't connect to Mysql:$driver:$!\n";
  }
  $limite = $self->{opts}->{limit} if (defined($self->{opts}->{limit}));
  $limite = 0 if ($limite eq 'All');
  print scalar(localtime),": New run\n" if ($self->{opts}->{verbose});
  return $self;
}

sub DESTROY {
  $_[0]->{DBH}->disconnect if ($_[0]->{DBH});
  print scalar(localtime),": Finished\n" if ($_[0]->{opts}->{verbose});
}

#------------------------------------------------------------------------------
# header
#------------------------------------------------------------------------------
sub header_html {
  my $self = shift;
  my $u = $self->{opts}->{url_base} || $ENV{BASE} || '/perl/smoke';
  if (!$ENV{SCRIPT_NAME}) {
    $ENV{SCRIPT_NAME} = $ENV{CGI_BASE} || '/cgi-bin';
    $ENV{SCRIPT_NAME}.='/smoke_db.cgi';
  }
  my $buf = start_html
    (-style=>{'src'=>"$u/smokedb.css"}, -title=>"perl-current smoke results");
  $buf.= <<EOF;
 <div class=menubar><table width="100%"><tr><td class=links>&nbsp;
   <a class=m href="$ENV{SCRIPT_NAME}">Home</a> &nbsp;|&nbsp;
   <a class=m href="$ENV{SCRIPT_NAME}?filter=1">Filter</a> &nbsp;|&nbsp;
   <a class=m href="$ENV{SCRIPT_NAME}?last=1">Last report</a> &nbsp;|&nbsp;
   <a class=m href="$ENV{SCRIPT_NAME}?last=1;want_smoke=1">Last smoke</a> &nbsp;|&nbsp;
   <a class=m href="$ENV{SCRIPT_NAME}?failure=1">
  Last failures</a> &nbsp;|&nbsp;
   <a class=m href="$u/FAQ.html">FAQ</a> &nbsp;|&nbsp;
   <a class=m href="$u/0.html">Stats</a> &nbsp;|&nbsp;
   <a class=m href="http://qa.perl.org">About</a> &nbsp;|&nbsp;
   <a class=m href="mailto:alian\@cpan.org">Author</a> &nbsp;|&nbsp;
</td><td align=right></td></tr></table>
</div>
<h1>Perl-current smoke results</h1>
EOF
  return $buf;

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
    print "mysql <-\t$_\n" if ($self->{opts}->{debug});
    if (!$self->{DBH}->do($_)) {
      print STDERR "Error $_: $DBI::errstr!\n";
      $ret = 0;
    }
  }
  return $ret;
}

#------------------------------------------------------------------------------
# build_graph
#------------------------------------------------------------------------------
sub build_graph(\%) {
  my $self = shift;
  print scalar(localtime),": Create graph\n"
    if ($self->{opts}->{verbose});
  eval("use GD::Graph::mixed");
  if ($@) {
    print scalar(localtime),
      ": You don't seem to have GD::Graph, aborted graph\n"
	if ($self->{opts}->{verbose});
    return;
  }
  my $c = new CGI;
  # Last 50 smoke
  my $st = $self->{DBH}->prepare('select max(smoke)-50 from builds');
  $st->execute;
  my ($li) = $st->fetchrow_array;
  $st->finish;
  # Begin, perl-5.9, last 50 smoke
  my %limit = (0 =>'Since smoke 11613', 
	       17500=>'Perl 5.9', 
	       $li=>'Last 50 smoke');
  my %limit2 = %limit;
  $limit2{cpan}= 'CPAN modules';
  $limit2{"last50"}=$limit2{$li};
  delete $limit2{$li};
  foreach my $mt (keys %limit) {
    my $mtx = $mt;
    $mtx = "last50" if ($mt == $li);
    my $graph = new Test::Smoke::Database::Graph($self->{DBH}, $self,$mt, $mtx);
    $graph->percent_configure();
    $graph->percent_configure_all();
    $graph->configure_per_os();
    $graph->smoke_per_os();
    $graph->configure_per_smoke();
    $graph->os_by_smoke();
    $graph->success_by_os();
    $graph->create_html($mtx, \%limit2, $c);
  }

  my $graph = new Test::Smoke::Database::Graph($self->{DBH}, $self,undef, "cpan");
  $graph->stats_cpan();
  $graph->create_html("cpan", \%limit2, $c);
}


#------------------------------------------------------------------------------
# rename_rpt
#------------------------------------------------------------------------------
sub rename_rpt {
  my $self = shift;
  my $nb = 0;
  print scalar(localtime),": Rename report with his nntp id\n"
    if ($self->{opts}->{verbose});
  foreach my $f (glob($self->{opts}->{dir}."/*.rpt")) {
    my $e=`grep 'for [ 1234567890.]*patch' $f`;
    if ($e=~/for [\d\.]* ?patch (\d+)/) {
      if (-e "$f.$1") { unlink($f); }
      else {
	print "Rename $f $1\n" if ($self->{opts}->{debug});
	`mv $f $f.$1`;
	$nb++;
      }
    }
  }
  return $nb;
}

#------------------------------------------------------------------------------
# suck_ng
#------------------------------------------------------------------------------
sub suck_ng {
  my $self = shift;
  print scalar(localtime),": Suck newsgroup on $self->{opts}->{nntp_server}\n"
    if ($self->{opts}->{verbose});
  # Find last id on dir
  my $max=0;
  my @l = glob($self->{opts}->{dir}."/*");
  foreach (@l) { $max=$1 if (/\/(\d*)\.rpt/ && $1 > $max); }
  print "NNTP max id is $max ($#l files in $self->{opts}->{dir})\n"
    if ($self->{opts}->{debug});

  # Connect on ng
  my $c = new News::NNTPClient($self->{opts}->{nntp_server});
  return undef if (!$c->ok);

  # Fetch last - first
  my ($first, $last) = ($c->group("perl.daily-build.reports"));
  #print "Max:$max first:$first last:$last\n";
  if ($max) {
    if ($max == $last) {
      print scalar(localtime),": No new report on perl.daily-build.reports\n"
	if ($self->{opts}->{verbose});
      $self->rename_rpt();
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
  my ($i,$summary,$details,$failure,$class,$resume)=(0);
  my ($lastsmoke, $lastsuccessful)=(0,0,0);
  # Walk on each smoke
  $summary = "
<table class=box width=\"90%\"><tr><td>
<table border=\"1\" width=\"100%\" class=\"box2\">".
  Tr(th("Os"), th("Os version"), th("Archi"), th("Compiler"), 
     th("Version compiler"), th("Last smoke"), th(a({-href=>'#legend'},"(1)")),
     th(table({-align=>"left",-border=>0, -width=>'100%'},
	      Tr(td({-width=>"15"},a({-href=>'#legend'},"(2)")),
		 td({-width=>"15"},a({-href=>'#legend'},"(3)")),
		 td({-width=>"15"},a({-href=>'#legend'},"(4)")),
		 td({-width=>"15"},a({-href=>'#legend'},"(5)")),
		 td({-width=>"15"},a({-href=>'#legend'},"(6)")),
		))), th("(7)"))."\n";
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
	    $lastsmoke = $smoke if ($smoke >$lastsmoke);
	    my ($nbt,$nbc,$nbto,$nbcf,$nbcm,$nbcc,$nbtt,$matrix)=
	      ($$ref{$os}{$osver}{$ar}{$cc}{$ccver}{$smoke}{nbte},
	       $$ref{$os}{$osver}{$ar}{$cc}{$ccver}{$smoke}{nbc},
	       $$ref{$os}{$osver}{$ar}{$cc}{$ccver}{$smoke}{nbco},
	       $$ref{$os}{$osver}{$ar}{$cc}{$ccver}{$smoke}{nbcf},
	       $$ref{$os}{$osver}{$ar}{$cc}{$ccver}{$smoke}{nbcm},
	       $$ref{$os}{$osver}{$ar}{$cc}{$ccver}{$smoke}{nbcc},
	       $$ref{$os}{$osver}{$ar}{$cc}{$ccver}{$smoke}{nbtt},
	       $$ref{$os}{$osver}{$ar}{$cc}{$ccver}{$smoke}{matrix}
	      );
	    my $id = $$ref{$os}{$osver}{$ar}{$cc}{$ccver}{$smoke}{id};
	    # debut des tableaux erreurs et details
	    my $de = "\n<a name=\"$id\"></a> <table width=\"80%\" class=\"box\">".
	      Tr(th({-colspan=>5},"$os $osver $ar $cc $ccver smoke patch $smoke"));
	    # Matrice
	    my $matrixe;
	    my $y=0;
	    my @ltmp = split(/\|/, $matrix);
	    foreach (@ltmp) {
	      $matrixe.="<tr><td align=right>$_</td>".("<td>_</td>"x$y++).
		("<td>|</td>"x($#ltmp-$y+2))."</tr>";
	    }
	    # Liste des tests echoues
	    if ($$ref{$os}{$osver}{$ar}{$cc}{$ccver}{$smoke}{failure}) {
	      my $f = $$ref{$os}{$osver}{$ar}{$cc}{$ccver}{$smoke}{failure};
	      if (param('failure') && $nbt) {
		$failure.=$de.Tr(td(pre($f)))."</table><br>"; }
	    }
	    # Liste des configs testees
	    if (ref($$ref{$os}{$osver}{$ar}{$cc}{$ccver}{$smoke}{build})) {
	      my $r2 = 1;
	      my ($dets);
	      foreach my $config (sort keys %{$$ref{$os}{$osver}{$ar}{$cc}{$ccver}{$smoke}{build}}) {
		$dets.= "<tr>".td($config);
		my $co="<table border=0><tr>";
		my $r = 1; my $classe=" ";
		foreach my $v (split(/ /,$$ref{$os}{$osver}{$ar}{$cc}{$ccver}{$smoke}{build}{$config})) {
		  my $u = $ENV{SCRIPT_NAME}."?failure=1&smoke=$smoke";
		  $u.=$self->compl_url if ($self->compl_url);
		  $u.="#$id" if ($id);
		  if ($v eq 'F') {
		    $v= a({-href=>$u},$v); $r=0; $r2=0;
		  } elsif ($v eq 'm' or $v eq 'c') {
		    $classe="red";
		  }
		  $dets.=td({-class=>$classe,-width=>3},$v);
		}
		$dets.="</tr>";
		$nbto+=$r;
		$nbc++;
	      }
	      $details.=$de.$dets.$matrixe."</table><br>"
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
	    my $u = $ENV{SCRIPT_NAME}."?last=1&smoke=$smoke";
	    $u.= $self->compl_url if ($self->compl_url);
	    $u.="#$id" if ($id);
	    my $ss="makeOk";
	    if ($nbcc) { $ss='confFail';}
	    elsif ($nbcm) { $ss='makeFail';}
	    elsif ($nbcf) { $ss='makeTestFail';}
	    $summary.=Tr({-class=>"mod".$class},
			 td({-class=>"os"},$oss),
			 td({-class=>"osver"},$osvv),
			 td({-class=>"archi"},$arr),
			 td({-class=>"cc"},$ccc),
			 td({-class=>"ccver"},$ccvv),
			 td({-class=>"smoke"},a({-href=>$u}, $smoke)),
			 td({-class=>"configure"},table(Tr(td($nbc)))),
			 td(table
			    ({-class=>$ss,-align=>"left",
			      -border=>0, -width=>'100%'},
			     Tr(td({-width=>"15"},$nbtt),
				td({-width=>"15"},$nbto),
				td({-width=>"15"},$nbcc),
				td({-width=>"15"},$nbcm),
				td({-width=>"15"},$nbcf),
			       ))),
			 $nbt."\n");
	    $lastsuccessful = $smoke if ($nbto == $nbtt && ($smoke>$lastsuccessful));
	  }
	  }
	}
      }
    }
  }
  $summary.=<<EOF;
</table></td></tr></table>
<div class=box>
<a name="legend">
<h2>Legend</h2>
<ol>
  <li>Number of configure run</li>
  <li>Number of make test run</li>
  <li>Number of make test ok</li>
  <li class="confFail">Number of failed configure</li>
  <li class="makeFail">Number of failed make</li>
  <li class="makeTestFail">Number of failed make test</li>
  <li>Number of failed test</li>
</ol>
</div>
EOF
   $lastsuccessful = "Never" if ! $lastsuccessful;
  $resume = table({ border=>1, class=>"box2" },
		  Tr(th("Smoke available"),
		     th("Since smoke"),
		     th("Last, "),
		     th("Last successfull")),
		  Tr(td($self->nb), td($limite),
		     td($lastsmoke),td($lastsuccessful)));
  $summary = $resume.$summary;
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
  Test::Smoke::Database::Parsing::parse_import(@_);
}

#------------------------------------------------------------------------------
# read_all
#------------------------------------------------------------------------------
sub read_all {
  my $self = shift;
  return {} if (!$self->{DBH});
  my ($req,$a,%h2);
  my ($ref_result, $ref_failure);
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
  my $ref_lid = $self->{DBH}->selectall_hashref($req, 'id') ||
      print "On $req: $DBI::errstr\n";
#  print STDERR $req,$DBI::errstr;
  my $list_id = join("," , keys %$ref_lid);

  if (param('failure')) {
    $req = "select idbuild,failure from data";
    if ($list_id) { $req.=" where idbuild in (".$list_id.")"; }
    $ref_failure = $self->{DBH}->selectall_hashref($req, 'idbuild') ||
      print "On $req: $DBI::errstr\n";
  }

  if (param('last')) {
    $req = "select idbuild,configure,result from configure";
    if ($list_id) { $req.=" where idbuild in (".$list_id.")"; }
    $ref_result = $self->{DBH}->selectall_arrayref($req) ||
      print "On $req: $DBI::errstr\n";
#    print STDERR "SQL: $req\n", Data::Dumper->Dump([$ref_result]);
    foreach my $ra (@$ref_result) {
      $h2{$ra->[0]}{$ra->[1]} = $ra->[2];
    }
  }

  $req = <<EOF;
select id,os,osver,archi,cc,ccver,date,smoke,nbc,nbco,nbcm,nbcf,nbcc,nbte,matrix
from builds
EOF
  if ($list_id) { $req.=" where id in (".$list_id.")"; }
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
sub distinct {
  my ($self, $col)=@_;
  my @res;
  my $req = "select distinct $col from builds where smoke>=$limite 
             order by $col";
  my $st = $self->{DBH}->prepare($req);
  print STDERR $req,"\n" if ($self->{opts}->{debug});
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
  print STDERR $req,"\n" if ($self->{opts}->{debug});
  return if (!$self->{DBH});
  my @row_ary = $self->{DBH}->selectrow_array($req) || return undef;
  return $row_ary[0];
}

#------------------------------------------------------------------------------
# add_to_db
#------------------------------------------------------------------------------
sub add_to_db {
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
	  $ref->{smoke}, basename($ref->{file}))
    if ($self->{opts}->{verbose});
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

  print $req,"\n" if ($self->{opts}->{debug});
  my $st = $self->{DBH}->prepare($req);
  if (!$st->execute) {
    print STDERR "SQL: $req\n", Data::Dumper->Dump([$ref]);
    cluck($DBI::errstr);
    return;
  }
  # id du test
  my $id =  $st->{'mysql_insertid'};
  $ref->{id}=$id;
  print STDERR Data::Dumper->Dump([$ref]) if ($self->{opts}->{debug});

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
auto called with DESTROY call if needed.

=item B<rundb> I<SQL request>

This will do like $dbh->do, but several request can be put in SQL request,
separated by ';'. Return 1 on sucess, 0 if one of request failed. If failed,
reason is printed on STDERR.

=back

=head2 Method for cgi

=over 4

=item B<nb>

Return the number of reports found after limit. Return undef if SQL request
fail.

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

Wrapper. See L<Test::Smoke::Database::Parsing>

=item B<suck_ng>

Fetch new report from perl.daily-build.reports

=back

=head2 Private methods

=over 4

=item B<read_all>

=item B<compl_url>

=item B<rename_rpt>

Rename fetched report to add no of smoke in name of file.
For all reports found, this will append at end of name the number of smoke.
After that all *. and *.rpt file will be deleted. This method is auto. called
after B<fetch> method.

=back

=head1 VERSION

$Revision: 1.9 $

=head1 AUTHOR

Alain BARBET with some help from Abe Timmerman

=cut

1;
