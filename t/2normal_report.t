# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl 2.t'


use Test::More;
use Test::Smoke::Database;
use Data::Dumper;
use File::Basename qw(basename dirname);

use strict;

chdir(dirname($0));
my ($rep,$tt);

if ($ARGV[0]) {
  foreach (glob("rpt/*.normal.rpt.*")) { $$rep{$_}=1; }
  $tt=1;
} else {
  # Count how many test
  my $d = "cat result_normal";
  $rep = eval `$d`;
  $tt = 0;
  foreach my $f (keys %$rep) {
    $tt+= $#{$$rep{$f}}+1;
  }
}

plan tests => $tt;
my $t = new Test::Smoke::Database({no_dbconnect => 1, verbose => 1});
my %res;
foreach my $f (keys %$rep) {
  my @lr = $t->parse_rpt($f);
  if ($ARGV[0]) { $res{$f}=\@lr; next; }
  else {
    my $nb=0;
    foreach (@lr) { 
       ok(eq_hash($_, $$rep{$f}->[$nb])) 
          or diag("Find ". Data::Dumper->Dump([ $_  ]).
                  " and want ".Data::Dumper->Dump([ $$rep{$f}->[$nb] ]));
       $nb++;
    }
  }
}
print Data::Dumper->Dump([ \%res], ['rep']),"\n" if ($ARGV[0]);
