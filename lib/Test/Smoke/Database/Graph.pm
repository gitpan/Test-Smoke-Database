package Test::Smoke::Database::Graph;

# module Test::Smoke::Database - Create graph about smoke database
# Copyright 2003 A.Barbet alian@alianwebserver.com.  All rights reserved.
# $Date: 2003/02/10 00:59:25 $
# $Log: Graph.pm,v $
# Revision 1.1  2003/02/10 00:59:25  alian
# New files
#
#

use strict;
use GD::Graph::mixed;
use GD::Graph::colour;
use GD::Graph::Data;
use Data::Dumper;
use POSIX;
use CGI;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK);
require Exporter;

@ISA = qw(Exporter);
@EXPORT = qw(prompt);
$VERSION = ('$Revision: 1.1 $ ' =~ /(\d+\.\d+)/)[0];

my $debug = 0;
my $font = '/usr/X11R6/share/enlightenment/themes/Blue_OS/ttfonts/arial.ttf';

#------------------------------------------------------------------------------
# new
#------------------------------------------------------------------------------
sub new   {
  my $class = shift;
  my $self = {};
  bless $self, $class;
  $self->{DBH} = shift;
  $self->{LIMIT} = shift || 0;
  if (!-e $self->{LIMIT}) { mkdir $self->{LIMIT},0755 or return undef };
  return $self;
}

#------------------------------------------------------------------------------
# percent_configure
#------------------------------------------------------------------------------
sub percent_configure {
  my $self = shift;
  my $request = "select smoke,os,(sum(nbco)/sum(nbc))*100 from builds ";
  $request.="where smoke > $self->{LIMIT} " if ($self->{LIMIT});
  $request.="group by smoke,os order by smoke";
  my (%l,%tt);
  my $st = $self->{DBH}->prepare($request);
  $st->execute or print STDERR $request,"<br>";
  while (my @l = $st->fetchrow_array) { $l{lc($l[1])}{$l[0]}=$l[2] if ($l[2]);}
  $st->finish;
  my @l1;
  foreach my $os (keys %l) {
    my (@l,@l2,$tt);
    foreach (sort keys %{$l{$os}}) {
      push(@l,$_);
      push(@l2,$l{$os}{$_});
      $tt+=$l{$os}{$_};
    }
    next if $#l2 < 2;
    $tt{$os}=sprintf("%2d", $tt/($#l2+1));
    my @la=(\@l, \@l2);
    my $my_graph = GD::Graph::area->new(800,350);
    $my_graph->set_legend("","% of successful make test");
    $my_graph->set( 
		   title           => '% of successful make test for '
		                      .$os. ' by smoke',
		   y_max_value     => 100,
		   y_tick_number   => 10,
		   x_label_skip    => ($#l2)/ 10,
 		   legend_spacing => 40,
		   axis_space => 20,
		   t_margin => 40,
		   b_margin => 10,
		   box_axis => 0,
		   dclrs       => [ qw/dpurple/ ],
		   transparent     => 0,
		  )
      or warn $my_graph->error;
    go($my_graph, \@la, "$self->{LIMIT}/9_os_".$os);
  }
}
#------------------------------------------------------------------------------
# percent_configure_all
#------------------------------------------------------------------------------
sub percent_configure_all {
  my $self = shift;
  my $request = "select smoke,(sum(nbco)/sum(nbc))*100 from builds ";
  $request.="where smoke > $self->{LIMIT} " if ($self->{LIMIT});
  $request.="group by smoke order by smoke";
  my $ref = $self->fetch_array($request);
  my $my_graph = GD::Graph::area->new(800,350);
  $my_graph->set_legend("","% of successful make test");
  $my_graph->set( 
		 title           => '% of successful make test',
		 y_max_value     => 100,
		 y_tick_number   => 10,
		 x_label_skip    => (scalar @{@{$ref}[0]})/8,,
		 legend_spacing => 40,
		 axis_space => 20,
		 t_margin => 40,
		 b_margin => 10,
		 box_axis => 0,
		 dclrs       => [ qw/black/ ],
		 transparent     => 0,
		)
    or warn $my_graph->error;
  go($my_graph, $ref, "$self->{LIMIT}/90_os");
}

#------------------------------------------------------------------------------
# configure_per_smoke
#------------------------------------------------------------------------------
sub configure_per_smoke {
  my $self = shift;
  my $req ="select smoke,sum(nbc),sum(nbco) from builds ";
  $req.="where smoke > $self->{LIMIT} " if ($self->{LIMIT});
  $req.="group by smoke order by smoke";
  my $ref = $self->fetch_array($req);
  my $my_graph = GD::Graph::mixed->new(800,300);
  $my_graph->set_legend("make test run","make test pass all tests");
  $my_graph->set(
		 y_label         => 'make test run',
		 title           => 'make test run/pass all tests by smoke',
		 y_max_value     => 250,
		 y_tick_number   => 10,
		 x_label_skip    => (scalar @{@{$ref}[0]})/8,
		 types => [qw(lines area )],
		 shadowclr       => 'dred',
		 transparent     => 0,
		 legend_spacing => 30,
		 dclrs       => [ qw/red dblue/ ],
		 axis_space => 20,
		 t_margin => 50,
		 b_margin => 20,
		 box_axis => 0,

		)
    or warn $my_graph->error;
  go($my_graph, $ref, "$self->{LIMIT}/7_conftested");
}

#------------------------------------------------------------------------------
# configure_per_os
#------------------------------------------------------------------------------
sub configure_per_os {
  my $self = shift;
  my $req = "select os,sum(nbc) from builds ";
  $req.="where smoke > $self->{LIMIT} " if ($self->{LIMIT});
  $req.="group by os order by 2";
  my $ref = $self->fetch_array($req,10);
  my @a = @{$$ref[1]};
  my $my = (floor($a[$#a] / 50)+1)*50;
  my $my_graph = GD::Graph::bars->new(800,300);
  $my_graph->set_legend("","os tested");
  $my_graph->set(
		 title           => 'Number of configure run by os',
		 y_max_value     => $my,
		 y_tick_number   => 5,
		 show_values => 1,
		 x_label_skip    => 1,
		 y_label_position => 0,
		 axis_space      => 20,
		 shadowclr       => 'dred',
		 shadow_depth    => 4,
		 transparent     => 0,
		 bar_spacing => 10,
		 legend_spacing => 40,
		 t_margin => 35,
		 box_axis => 0,
		)
    or warn $my_graph->error;
  go($my_graph, $ref, "$self->{LIMIT}/4_nb_configure");
}

#------------------------------------------------------------------------------
# smoke_per_os
#------------------------------------------------------------------------------
sub smoke_per_os {
  my $self = shift;
  my $req = "select os,count(id) from builds ";
  $req.="where smoke > $self->{LIMIT} " if ($self->{LIMIT});
  $req.="group by os order by 2";
  my $ref = $self->fetch_array($req,2);
  my @a = @{$$ref[1]};
  my $my = (floor($a[$#a] / 50)+1)*50;
  my $my_graph = GD::Graph::bars->new(800,300);
  $my_graph->set_legend("","os tested");
  $my_graph->set(
		 title           => 'Number of smoke run by os',
		 y_max_value     => $my,
		 y_tick_number   => 10,
		 show_values => 1,
		 x_label_skip    => 1,
		 y_label_position => 0,
		 axis_space => 20,
		 shadowclr       => 'dred',
		 shadow_depth    => 4,
		 transparent     => 0,
		 bar_spacing => 10,
		 legend_spacing => 40,
 		 t_margin => 35,
		 box_axis => 0
		)
    or warn $my_graph->error;
  go($my_graph, $ref, "$self->{LIMIT}/3_nb_smoke");
}

#------------------------------------------------------------------------------
# os_by_smoke
#------------------------------------------------------------------------------
sub os_by_smoke {
  my $self = shift;
  my $req = "select smoke,count(distinct os) from builds ";
  $req.="where smoke > $self->{LIMIT} " if ($self->{LIMIT});
  $req.="group by smoke order by smoke";
  my $ref = $self->fetch_array($req);
  my $my_graph = GD::Graph::area->new(800,300);
  $my_graph->set_legend("","os tested");
  $my_graph->set(
		 title           => 'Number of os by smoke',
		 y_max_value     => 10,
		 y_tick_number   => 10,
		 x_label_skip    => scalar @{@{$ref}[0]}/10,,
		 y_label_position => 0,
		 axis_space => 20,
		 # shadows
		 shadowclr       => 'dred',
		 shadow_depth    => 4,
		 transparent     => 0,
		 bar_spacing => 10,
		 legend_spacing => 40,
 		 t_margin => 35,
		 box_axis => 0
		)
    or warn $my_graph->error;
  go($my_graph, $ref, "$self->{LIMIT}/6_nb_os_by_smoke");
}

#------------------------------------------------------------------------------
# success_by_os
#------------------------------------------------------------------------------
sub success_by_os {
  my $self = shift;
  my $req = "select os,(sum(nbco)/sum(nbc))*100 from builds ";
  $req.="where smoke > $self->{LIMIT} " if ($self->{LIMIT});
  $req.="group by os order by 2";
  my $ref = $self->fetch_array($req, 15);
  my $my_graph = GD::Graph::bars->new(800,300);
  $my_graph->set_legend("","os tested");
  $my_graph->set(
		 title           => 'Average % of successful make test by os',
		 y_max_value     => 100,
		 y_tick_number   => 10,
		 show_values => 1,
		 x_label_skip    => 1,
		 y_label_position => 0,
		 axis_space => 20,
		 # shadows
		 shadowclr       => 'dred',
		 shadow_depth    => 4,
		 transparent     => 0,
		 bar_spacing => 10,
		 legend_spacing => 40,
 		 t_margin => 35,
		 box_axis => 0
		)
    or warn $my_graph->error;
  go($my_graph, $ref, "$self->{LIMIT}/5_configure_by_os");
}

#------------------------------------------------------------------------------
# go
#------------------------------------------------------------------------------
sub go {
  my ($my_graph, $data, $filename)=@_;
  my $ok = 0;
  print STDERR $filename,"=>\n",Data::Dumper->Dump( $data) if ($debug);
  foreach my $ref ($$data[1]) {
    foreach my $ref2 (@$ref) {
      $ok=1 if ($ref2 != 0);
    }
  }
  return if (!$ok);
  $data = GD::Graph::Data->new($data) or die GD::Graph::Data->error;
  $my_graph->set_x_axis_font($font,12 );
  $my_graph->set_y_axis_font($font,9 );
  $my_graph->set_title_font($font,14);
  $my_graph->set_text_clr("black");
  $my_graph->plot($data) or die $my_graph->error;
  print STDERR "Create $filename.png\n" if ($debug);
  save_chart($my_graph, $filename);
}

#------------------------------------------------------------------------------
# save_chart
#------------------------------------------------------------------------------
sub save_chart {
  my $chart = shift or die "Need a chart!";
  my $name = shift or die "Need a name!";
  local(*OUT);
  my $ext = $chart->export_format;
  open(OUT, ">$name.$ext") or 
    die "Cannot open $name.$ext for write: $!";
  binmode OUT;
  print OUT $chart->gd->$ext();
  close OUT;
}

#------------------------------------------------------------------------------
# fetch_array
#------------------------------------------------------------------------------
sub fetch_array {
  my ($self,$request, $limit)=@_;
  my (@tab,@tab2);
  print STDERR "SQL request =>$request\n" if ($debug);
  my $st = $self->{DBH}->prepare($request);
  $st->execute or print STDERR $request,"<br>";
  while (my @l = $st->fetchrow_array) {
    next if (($limit && $l[1] < $limit) or (!$l[1] or !$l[0]));
    my $i = 0;
    foreach (@l) { push( @{$tab[$i++]}, $_);  }
  }
  $st->finish;

  print STDERR Data::Dumper->Dump([ \@tab ]) if ($debug);
  return \@tab;
}

#------------------------------------------------------------------------------
# POD DOC
#------------------------------------------------------------------------------


=head1 NAME

Test::Smoke::Database::Graph - Method for build chart on BleadPerl

=head1 SYNOPSIS

  $ admin_smokedb --create --suck --import --update_archi
  $ lynx http://localhost/cgi-bin/smokedb.cgi

=head1 DESCRIPTION

This module help to build an application that parses smoke-reports for
perl-current and puts the results in a database. This allows for a simple
overview of the build status on as wide a variety of supported platforms 
(operating system/architecture) as possible.

=head1 SEE ALSO

L<admin_smokedb>, L<Test::Smoke::Database::FAQ>, L<Test::Smoke>,
L<http://www.alianwebserver.com/perl/smoke/smoke_db.cgi>

=head1 METHODS

=over 4

=item B<new> I<DBH>, I<LIMIT>

Construct a new Test::Smoke::Database::Graph object and return it.

=back

=head1 VERSION

$Revision: 1.1 $

=head1 AUTHOR

Alain BARBET

=cut

1;
