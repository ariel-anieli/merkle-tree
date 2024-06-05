# Merkle tree assigment

## Usage
```erlang
1> c(merkle_computer).
merkle_computer.erl:19:27: Warning: variable 'Arg' shadowed in 'fun'
%   19|     lists:foldl(fun(Func, Arg) -> Func(Arg) end, Arg, Funcs).
%     |                           ^

{ok,merkle_computer}
2> merkle_computer:compute_root("input.txt").
#{file => "input.txt",
  root =>
      "afd5c12556531f079b7fd00d8cb9afdf20b5c9d30bc06b055323f61d20ad1447",
  duration_ms => 113}
```

## Build machine
```shell
$ lscpu
Architecture:             x86_64
  CPU op-mode(s):         32-bit, 64-bit
  Address sizes:          39 bits physical, 48 bits virtual
  Byte Order:             Little Endian
CPU(s):                   8
  On-line CPU(s) list:    0-7
Vendor ID:                GenuineIntel
  Model name:             Intel(R) Core(TM) i7-8665U CPU @ 1.90GHz
    CPU family:           6
    Model:                142
    Thread(s) per core:   2
    Core(s) per socket:   4
    Socket(s):            1
    Stepping:             12
    CPU(s) scaling MHz:   14%
    CPU max MHz:          4800.0000
    CPU min MHz:          400.0000
    BogoMIPS:             4201.88
    Flags:                fpu vme de pse tsc msr pae mce cx8 apic sep mtrr pge mca cmov pat pse36 clflush dts acpi mm
                          x fxsr sse sse2 ss ht tm pbe syscall nx pdpe1gb rdtscp lm constant_tsc art arch_perfmon peb
                          s bts rep_good nopl xtopology nonstop_tsc cpuid aperfmperf pni pclmulqdq dtes64 monitor ds_
                          cpl vmx smx est tm2 ssse3 sdbg fma cx16 xtpr pdcm pcid sse4_1 sse4_2 x2apic movbe popcnt ts
                          c_deadline_timer aes xsave avx f16c rdrand lahf_lm abm 3dnowprefetch cpuid_fault epb ssbd i
                          brs ibpb stibp ibrs_enhanced tpr_shadow flexpriority ept vpid ept_ad fsgsbase tsc_adjust bm
                          i1 avx2 smep bmi2 erms invpcid mpx rdseed adx smap clflushopt intel_pt xsaveopt xsavec xget
                          bv1 xsaves dtherm ida arat pln pts hwp hwp_notify hwp_act_window hwp_epp vnmi md_clear flus
                          h_l1d arch_capabilities
Virtualization features:  
  Virtualization:         VT-x
Caches (sum of all):      
  L1d:                    128 KiB (4 instances)
  L1i:                    128 KiB (4 instances)
  L2:                     1 MiB (4 instances)
  L3:                     8 MiB (1 instance)
NUMA:                     
  NUMA node(s):           1
  NUMA node0 CPU(s):      0-7
Vulnerabilities:          
  Gather data sampling:   Mitigation; Microcode
  Itlb multihit:          KVM: Mitigation: VMX disabled
  L1tf:                   Not affected
  Mds:                    Not affected
  Meltdown:               Not affected
  Mmio stale data:        Mitigation; Clear CPU buffers; SMT vulnerable
  Reg file data sampling: Not affected
  Retbleed:               Mitigation; Enhanced IBRS
  Spec rstack overflow:   Not affected
  Spec store bypass:      Mitigation; Speculative Store Bypass disabled via prctl
  Spectre v1:             Mitigation; usercopy/swapgs barriers and __user pointer sanitization
  Spectre v2:             Mitigation; Enhanced / Automatic IBRS; IBPB conditional; RSB filling; PBRSB-eIBRS SW sequen
                          ce; BHI SW loop, KVM SW loop
  Srbds:                  Mitigation; Microcode
  Tsx async abort:        Mitigation; TSX disabled
  
$ grep Mem /proc/meminfo
MemTotal:       16184208 kB
MemFree:        10980472 kB
MemAvailable:   14429832 kB

$ grep -P 'Mem|Swap' /proc/meminfo 
MemTotal:       16184208 kB
MemFree:        10979716 kB
MemAvailable:   14429148 kB
SwapCached:            0 kB
SwapTotal:       4194300 kB
SwapFree:        4194300 kB

$ rebar3 --version 
rebar 3.23.0 on Erlang/OTP 26 Erts 14.2.5

$ sha256sum input.txt 
ef128dc2cf6422cfa0b2a35e8da6197849d07ef8661882c36b9d4a432a4a722f  input.txt
```
