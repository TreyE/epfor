-module(epfor).

-export([link_packet_port/2, link_packet_port/3, call_packet_port/2, link_packet_port_pid/2]).

link_packet_port_pid(CmdSpec, Options) -> epfor_packet_port:start_link(CmdSpec, OtherArgs).

link_packet_port(NameSpec, CmdSpec) -> link_packet_port(NameSpec, CmdSpec, []).
link_packet_port(NameSpec, CmdSpec, OtherArgs) -> epfor_packet_port:start_link(NameSpec, CmdSpec, OtherArgs).

call_packet_port(NameSpec, Cmd) -> epfor_packet_port:send_command(NameSpec, Cmd).
