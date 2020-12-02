{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.PhoneNumberCountryCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.PhoneNumberCountryCode where

import Network.AWS.Prelude

data PhoneNumberCountryCode
  = PNCCAD
  | PNCCAE
  | PNCCAF
  | PNCCAG
  | PNCCAI
  | PNCCAL
  | PNCCAM
  | PNCCAN
  | PNCCAO
  | PNCCAQ
  | PNCCAR
  | PNCCAS
  | PNCCAT
  | PNCCAU
  | PNCCAW
  | PNCCAZ
  | PNCCBA
  | PNCCBB
  | PNCCBD
  | PNCCBE
  | PNCCBF
  | PNCCBG
  | PNCCBH
  | PNCCBI
  | PNCCBJ
  | PNCCBL
  | PNCCBM
  | PNCCBN
  | PNCCBO
  | PNCCBR
  | PNCCBS
  | PNCCBT
  | PNCCBW
  | PNCCBY
  | PNCCBZ
  | PNCCCA
  | PNCCCC
  | PNCCCD
  | PNCCCF
  | PNCCCG
  | PNCCCH
  | PNCCCI
  | PNCCCK
  | PNCCCL
  | PNCCCM
  | PNCCCN
  | PNCCCO
  | PNCCCR
  | PNCCCU
  | PNCCCV
  | PNCCCW
  | PNCCCX
  | PNCCCY
  | PNCCCZ
  | PNCCDE
  | PNCCDJ
  | PNCCDK
  | PNCCDM
  | PNCCDO
  | PNCCDZ
  | PNCCEC
  | PNCCEE
  | PNCCEG
  | PNCCEH
  | PNCCER
  | PNCCES
  | PNCCET
  | PNCCFI
  | PNCCFJ
  | PNCCFK
  | PNCCFM
  | PNCCFO
  | PNCCFR
  | PNCCGA
  | PNCCGB
  | PNCCGD
  | PNCCGE
  | PNCCGG
  | PNCCGH
  | PNCCGI
  | PNCCGL
  | PNCCGM
  | PNCCGN
  | PNCCGQ
  | PNCCGR
  | PNCCGT'
  | PNCCGU
  | PNCCGW
  | PNCCGY
  | PNCCHK
  | PNCCHN
  | PNCCHR
  | PNCCHT
  | PNCCHU
  | PNCCIE
  | PNCCIL
  | PNCCIM
  | PNCCIN
  | PNCCIO
  | PNCCIQ
  | PNCCIR
  | PNCCIS
  | PNCCIT
  | PNCCId
  | PNCCJE
  | PNCCJM
  | PNCCJO
  | PNCCJP
  | PNCCKE
  | PNCCKG
  | PNCCKH
  | PNCCKI
  | PNCCKM
  | PNCCKN
  | PNCCKP
  | PNCCKR
  | PNCCKW
  | PNCCKY
  | PNCCKZ
  | PNCCLA
  | PNCCLB
  | PNCCLC
  | PNCCLI
  | PNCCLK
  | PNCCLR
  | PNCCLS
  | PNCCLT'
  | PNCCLU
  | PNCCLV
  | PNCCLY
  | PNCCMA
  | PNCCMC
  | PNCCMD
  | PNCCME
  | PNCCMF
  | PNCCMG
  | PNCCMH
  | PNCCMK
  | PNCCML
  | PNCCMM
  | PNCCMN
  | PNCCMO
  | PNCCMP
  | PNCCMR
  | PNCCMS
  | PNCCMT
  | PNCCMU
  | PNCCMV
  | PNCCMW
  | PNCCMX
  | PNCCMY
  | PNCCMZ
  | PNCCNA
  | PNCCNC
  | PNCCNE
  | PNCCNG
  | PNCCNI
  | PNCCNL
  | PNCCNO
  | PNCCNP
  | PNCCNR
  | PNCCNU
  | PNCCNZ
  | PNCCOM
  | PNCCPA
  | PNCCPE
  | PNCCPF
  | PNCCPG
  | PNCCPH
  | PNCCPK
  | PNCCPL
  | PNCCPM
  | PNCCPN
  | PNCCPR
  | PNCCPT
  | PNCCPW
  | PNCCPY
  | PNCCQA
  | PNCCRE
  | PNCCRO
  | PNCCRS
  | PNCCRU
  | PNCCRW
  | PNCCSA
  | PNCCSB
  | PNCCSC
  | PNCCSD
  | PNCCSE
  | PNCCSG
  | PNCCSH
  | PNCCSI
  | PNCCSJ
  | PNCCSK
  | PNCCSL
  | PNCCSM
  | PNCCSN
  | PNCCSO
  | PNCCSR
  | PNCCST
  | PNCCSV
  | PNCCSX
  | PNCCSY
  | PNCCSZ
  | PNCCTC
  | PNCCTD
  | PNCCTG
  | PNCCTH
  | PNCCTJ
  | PNCCTK
  | PNCCTL
  | PNCCTM
  | PNCCTN
  | PNCCTO
  | PNCCTR
  | PNCCTT
  | PNCCTV
  | PNCCTW
  | PNCCTZ
  | PNCCUA
  | PNCCUG
  | PNCCUS
  | PNCCUY
  | PNCCUZ
  | PNCCVA
  | PNCCVC
  | PNCCVE
  | PNCCVG
  | PNCCVI
  | PNCCVN
  | PNCCVU
  | PNCCWF
  | PNCCWS
  | PNCCYE
  | PNCCYT
  | PNCCZA
  | PNCCZM
  | PNCCZW
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText PhoneNumberCountryCode where
  parser =
    takeLowerText >>= \case
      "ad" -> pure PNCCAD
      "ae" -> pure PNCCAE
      "af" -> pure PNCCAF
      "ag" -> pure PNCCAG
      "ai" -> pure PNCCAI
      "al" -> pure PNCCAL
      "am" -> pure PNCCAM
      "an" -> pure PNCCAN
      "ao" -> pure PNCCAO
      "aq" -> pure PNCCAQ
      "ar" -> pure PNCCAR
      "as" -> pure PNCCAS
      "at" -> pure PNCCAT
      "au" -> pure PNCCAU
      "aw" -> pure PNCCAW
      "az" -> pure PNCCAZ
      "ba" -> pure PNCCBA
      "bb" -> pure PNCCBB
      "bd" -> pure PNCCBD
      "be" -> pure PNCCBE
      "bf" -> pure PNCCBF
      "bg" -> pure PNCCBG
      "bh" -> pure PNCCBH
      "bi" -> pure PNCCBI
      "bj" -> pure PNCCBJ
      "bl" -> pure PNCCBL
      "bm" -> pure PNCCBM
      "bn" -> pure PNCCBN
      "bo" -> pure PNCCBO
      "br" -> pure PNCCBR
      "bs" -> pure PNCCBS
      "bt" -> pure PNCCBT
      "bw" -> pure PNCCBW
      "by" -> pure PNCCBY
      "bz" -> pure PNCCBZ
      "ca" -> pure PNCCCA
      "cc" -> pure PNCCCC
      "cd" -> pure PNCCCD
      "cf" -> pure PNCCCF
      "cg" -> pure PNCCCG
      "ch" -> pure PNCCCH
      "ci" -> pure PNCCCI
      "ck" -> pure PNCCCK
      "cl" -> pure PNCCCL
      "cm" -> pure PNCCCM
      "cn" -> pure PNCCCN
      "co" -> pure PNCCCO
      "cr" -> pure PNCCCR
      "cu" -> pure PNCCCU
      "cv" -> pure PNCCCV
      "cw" -> pure PNCCCW
      "cx" -> pure PNCCCX
      "cy" -> pure PNCCCY
      "cz" -> pure PNCCCZ
      "de" -> pure PNCCDE
      "dj" -> pure PNCCDJ
      "dk" -> pure PNCCDK
      "dm" -> pure PNCCDM
      "do" -> pure PNCCDO
      "dz" -> pure PNCCDZ
      "ec" -> pure PNCCEC
      "ee" -> pure PNCCEE
      "eg" -> pure PNCCEG
      "eh" -> pure PNCCEH
      "er" -> pure PNCCER
      "es" -> pure PNCCES
      "et" -> pure PNCCET
      "fi" -> pure PNCCFI
      "fj" -> pure PNCCFJ
      "fk" -> pure PNCCFK
      "fm" -> pure PNCCFM
      "fo" -> pure PNCCFO
      "fr" -> pure PNCCFR
      "ga" -> pure PNCCGA
      "gb" -> pure PNCCGB
      "gd" -> pure PNCCGD
      "ge" -> pure PNCCGE
      "gg" -> pure PNCCGG
      "gh" -> pure PNCCGH
      "gi" -> pure PNCCGI
      "gl" -> pure PNCCGL
      "gm" -> pure PNCCGM
      "gn" -> pure PNCCGN
      "gq" -> pure PNCCGQ
      "gr" -> pure PNCCGR
      "gt" -> pure PNCCGT'
      "gu" -> pure PNCCGU
      "gw" -> pure PNCCGW
      "gy" -> pure PNCCGY
      "hk" -> pure PNCCHK
      "hn" -> pure PNCCHN
      "hr" -> pure PNCCHR
      "ht" -> pure PNCCHT
      "hu" -> pure PNCCHU
      "ie" -> pure PNCCIE
      "il" -> pure PNCCIL
      "im" -> pure PNCCIM
      "in" -> pure PNCCIN
      "io" -> pure PNCCIO
      "iq" -> pure PNCCIQ
      "ir" -> pure PNCCIR
      "is" -> pure PNCCIS
      "it" -> pure PNCCIT
      "id" -> pure PNCCId
      "je" -> pure PNCCJE
      "jm" -> pure PNCCJM
      "jo" -> pure PNCCJO
      "jp" -> pure PNCCJP
      "ke" -> pure PNCCKE
      "kg" -> pure PNCCKG
      "kh" -> pure PNCCKH
      "ki" -> pure PNCCKI
      "km" -> pure PNCCKM
      "kn" -> pure PNCCKN
      "kp" -> pure PNCCKP
      "kr" -> pure PNCCKR
      "kw" -> pure PNCCKW
      "ky" -> pure PNCCKY
      "kz" -> pure PNCCKZ
      "la" -> pure PNCCLA
      "lb" -> pure PNCCLB
      "lc" -> pure PNCCLC
      "li" -> pure PNCCLI
      "lk" -> pure PNCCLK
      "lr" -> pure PNCCLR
      "ls" -> pure PNCCLS
      "lt" -> pure PNCCLT'
      "lu" -> pure PNCCLU
      "lv" -> pure PNCCLV
      "ly" -> pure PNCCLY
      "ma" -> pure PNCCMA
      "mc" -> pure PNCCMC
      "md" -> pure PNCCMD
      "me" -> pure PNCCME
      "mf" -> pure PNCCMF
      "mg" -> pure PNCCMG
      "mh" -> pure PNCCMH
      "mk" -> pure PNCCMK
      "ml" -> pure PNCCML
      "mm" -> pure PNCCMM
      "mn" -> pure PNCCMN
      "mo" -> pure PNCCMO
      "mp" -> pure PNCCMP
      "mr" -> pure PNCCMR
      "ms" -> pure PNCCMS
      "mt" -> pure PNCCMT
      "mu" -> pure PNCCMU
      "mv" -> pure PNCCMV
      "mw" -> pure PNCCMW
      "mx" -> pure PNCCMX
      "my" -> pure PNCCMY
      "mz" -> pure PNCCMZ
      "na" -> pure PNCCNA
      "nc" -> pure PNCCNC
      "ne" -> pure PNCCNE
      "ng" -> pure PNCCNG
      "ni" -> pure PNCCNI
      "nl" -> pure PNCCNL
      "no" -> pure PNCCNO
      "np" -> pure PNCCNP
      "nr" -> pure PNCCNR
      "nu" -> pure PNCCNU
      "nz" -> pure PNCCNZ
      "om" -> pure PNCCOM
      "pa" -> pure PNCCPA
      "pe" -> pure PNCCPE
      "pf" -> pure PNCCPF
      "pg" -> pure PNCCPG
      "ph" -> pure PNCCPH
      "pk" -> pure PNCCPK
      "pl" -> pure PNCCPL
      "pm" -> pure PNCCPM
      "pn" -> pure PNCCPN
      "pr" -> pure PNCCPR
      "pt" -> pure PNCCPT
      "pw" -> pure PNCCPW
      "py" -> pure PNCCPY
      "qa" -> pure PNCCQA
      "re" -> pure PNCCRE
      "ro" -> pure PNCCRO
      "rs" -> pure PNCCRS
      "ru" -> pure PNCCRU
      "rw" -> pure PNCCRW
      "sa" -> pure PNCCSA
      "sb" -> pure PNCCSB
      "sc" -> pure PNCCSC
      "sd" -> pure PNCCSD
      "se" -> pure PNCCSE
      "sg" -> pure PNCCSG
      "sh" -> pure PNCCSH
      "si" -> pure PNCCSI
      "sj" -> pure PNCCSJ
      "sk" -> pure PNCCSK
      "sl" -> pure PNCCSL
      "sm" -> pure PNCCSM
      "sn" -> pure PNCCSN
      "so" -> pure PNCCSO
      "sr" -> pure PNCCSR
      "st" -> pure PNCCST
      "sv" -> pure PNCCSV
      "sx" -> pure PNCCSX
      "sy" -> pure PNCCSY
      "sz" -> pure PNCCSZ
      "tc" -> pure PNCCTC
      "td" -> pure PNCCTD
      "tg" -> pure PNCCTG
      "th" -> pure PNCCTH
      "tj" -> pure PNCCTJ
      "tk" -> pure PNCCTK
      "tl" -> pure PNCCTL
      "tm" -> pure PNCCTM
      "tn" -> pure PNCCTN
      "to" -> pure PNCCTO
      "tr" -> pure PNCCTR
      "tt" -> pure PNCCTT
      "tv" -> pure PNCCTV
      "tw" -> pure PNCCTW
      "tz" -> pure PNCCTZ
      "ua" -> pure PNCCUA
      "ug" -> pure PNCCUG
      "us" -> pure PNCCUS
      "uy" -> pure PNCCUY
      "uz" -> pure PNCCUZ
      "va" -> pure PNCCVA
      "vc" -> pure PNCCVC
      "ve" -> pure PNCCVE
      "vg" -> pure PNCCVG
      "vi" -> pure PNCCVI
      "vn" -> pure PNCCVN
      "vu" -> pure PNCCVU
      "wf" -> pure PNCCWF
      "ws" -> pure PNCCWS
      "ye" -> pure PNCCYE
      "yt" -> pure PNCCYT
      "za" -> pure PNCCZA
      "zm" -> pure PNCCZM
      "zw" -> pure PNCCZW
      e ->
        fromTextError $
          "Failure parsing PhoneNumberCountryCode from value: '" <> e
            <> "'. Accepted values: ad, ae, af, ag, ai, al, am, an, ao, aq, ar, as, at, au, aw, az, ba, bb, bd, be, bf, bg, bh, bi, bj, bl, bm, bn, bo, br, bs, bt, bw, by, bz, ca, cc, cd, cf, cg, ch, ci, ck, cl, cm, cn, co, cr, cu, cv, cw, cx, cy, cz, de, dj, dk, dm, do, dz, ec, ee, eg, eh, er, es, et, fi, fj, fk, fm, fo, fr, ga, gb, gd, ge, gg, gh, gi, gl, gm, gn, gq, gr, gt, gu, gw, gy, hk, hn, hr, ht, hu, ie, il, im, in, io, iq, ir, is, it, id, je, jm, jo, jp, ke, kg, kh, ki, km, kn, kp, kr, kw, ky, kz, la, lb, lc, li, lk, lr, ls, lt, lu, lv, ly, ma, mc, md, me, mf, mg, mh, mk, ml, mm, mn, mo, mp, mr, ms, mt, mu, mv, mw, mx, my, mz, na, nc, ne, ng, ni, nl, no, np, nr, nu, nz, om, pa, pe, pf, pg, ph, pk, pl, pm, pn, pr, pt, pw, py, qa, re, ro, rs, ru, rw, sa, sb, sc, sd, se, sg, sh, si, sj, sk, sl, sm, sn, so, sr, st, sv, sx, sy, sz, tc, td, tg, th, tj, tk, tl, tm, tn, to, tr, tt, tv, tw, tz, ua, ug, us, uy, uz, va, vc, ve, vg, vi, vn, vu, wf, ws, ye, yt, za, zm, zw"

instance ToText PhoneNumberCountryCode where
  toText = \case
    PNCCAD -> "AD"
    PNCCAE -> "AE"
    PNCCAF -> "AF"
    PNCCAG -> "AG"
    PNCCAI -> "AI"
    PNCCAL -> "AL"
    PNCCAM -> "AM"
    PNCCAN -> "AN"
    PNCCAO -> "AO"
    PNCCAQ -> "AQ"
    PNCCAR -> "AR"
    PNCCAS -> "AS"
    PNCCAT -> "AT"
    PNCCAU -> "AU"
    PNCCAW -> "AW"
    PNCCAZ -> "AZ"
    PNCCBA -> "BA"
    PNCCBB -> "BB"
    PNCCBD -> "BD"
    PNCCBE -> "BE"
    PNCCBF -> "BF"
    PNCCBG -> "BG"
    PNCCBH -> "BH"
    PNCCBI -> "BI"
    PNCCBJ -> "BJ"
    PNCCBL -> "BL"
    PNCCBM -> "BM"
    PNCCBN -> "BN"
    PNCCBO -> "BO"
    PNCCBR -> "BR"
    PNCCBS -> "BS"
    PNCCBT -> "BT"
    PNCCBW -> "BW"
    PNCCBY -> "BY"
    PNCCBZ -> "BZ"
    PNCCCA -> "CA"
    PNCCCC -> "CC"
    PNCCCD -> "CD"
    PNCCCF -> "CF"
    PNCCCG -> "CG"
    PNCCCH -> "CH"
    PNCCCI -> "CI"
    PNCCCK -> "CK"
    PNCCCL -> "CL"
    PNCCCM -> "CM"
    PNCCCN -> "CN"
    PNCCCO -> "CO"
    PNCCCR -> "CR"
    PNCCCU -> "CU"
    PNCCCV -> "CV"
    PNCCCW -> "CW"
    PNCCCX -> "CX"
    PNCCCY -> "CY"
    PNCCCZ -> "CZ"
    PNCCDE -> "DE"
    PNCCDJ -> "DJ"
    PNCCDK -> "DK"
    PNCCDM -> "DM"
    PNCCDO -> "DO"
    PNCCDZ -> "DZ"
    PNCCEC -> "EC"
    PNCCEE -> "EE"
    PNCCEG -> "EG"
    PNCCEH -> "EH"
    PNCCER -> "ER"
    PNCCES -> "ES"
    PNCCET -> "ET"
    PNCCFI -> "FI"
    PNCCFJ -> "FJ"
    PNCCFK -> "FK"
    PNCCFM -> "FM"
    PNCCFO -> "FO"
    PNCCFR -> "FR"
    PNCCGA -> "GA"
    PNCCGB -> "GB"
    PNCCGD -> "GD"
    PNCCGE -> "GE"
    PNCCGG -> "GG"
    PNCCGH -> "GH"
    PNCCGI -> "GI"
    PNCCGL -> "GL"
    PNCCGM -> "GM"
    PNCCGN -> "GN"
    PNCCGQ -> "GQ"
    PNCCGR -> "GR"
    PNCCGT' -> "GT"
    PNCCGU -> "GU"
    PNCCGW -> "GW"
    PNCCGY -> "GY"
    PNCCHK -> "HK"
    PNCCHN -> "HN"
    PNCCHR -> "HR"
    PNCCHT -> "HT"
    PNCCHU -> "HU"
    PNCCIE -> "IE"
    PNCCIL -> "IL"
    PNCCIM -> "IM"
    PNCCIN -> "IN"
    PNCCIO -> "IO"
    PNCCIQ -> "IQ"
    PNCCIR -> "IR"
    PNCCIS -> "IS"
    PNCCIT -> "IT"
    PNCCId -> "ID"
    PNCCJE -> "JE"
    PNCCJM -> "JM"
    PNCCJO -> "JO"
    PNCCJP -> "JP"
    PNCCKE -> "KE"
    PNCCKG -> "KG"
    PNCCKH -> "KH"
    PNCCKI -> "KI"
    PNCCKM -> "KM"
    PNCCKN -> "KN"
    PNCCKP -> "KP"
    PNCCKR -> "KR"
    PNCCKW -> "KW"
    PNCCKY -> "KY"
    PNCCKZ -> "KZ"
    PNCCLA -> "LA"
    PNCCLB -> "LB"
    PNCCLC -> "LC"
    PNCCLI -> "LI"
    PNCCLK -> "LK"
    PNCCLR -> "LR"
    PNCCLS -> "LS"
    PNCCLT' -> "LT"
    PNCCLU -> "LU"
    PNCCLV -> "LV"
    PNCCLY -> "LY"
    PNCCMA -> "MA"
    PNCCMC -> "MC"
    PNCCMD -> "MD"
    PNCCME -> "ME"
    PNCCMF -> "MF"
    PNCCMG -> "MG"
    PNCCMH -> "MH"
    PNCCMK -> "MK"
    PNCCML -> "ML"
    PNCCMM -> "MM"
    PNCCMN -> "MN"
    PNCCMO -> "MO"
    PNCCMP -> "MP"
    PNCCMR -> "MR"
    PNCCMS -> "MS"
    PNCCMT -> "MT"
    PNCCMU -> "MU"
    PNCCMV -> "MV"
    PNCCMW -> "MW"
    PNCCMX -> "MX"
    PNCCMY -> "MY"
    PNCCMZ -> "MZ"
    PNCCNA -> "NA"
    PNCCNC -> "NC"
    PNCCNE -> "NE"
    PNCCNG -> "NG"
    PNCCNI -> "NI"
    PNCCNL -> "NL"
    PNCCNO -> "NO"
    PNCCNP -> "NP"
    PNCCNR -> "NR"
    PNCCNU -> "NU"
    PNCCNZ -> "NZ"
    PNCCOM -> "OM"
    PNCCPA -> "PA"
    PNCCPE -> "PE"
    PNCCPF -> "PF"
    PNCCPG -> "PG"
    PNCCPH -> "PH"
    PNCCPK -> "PK"
    PNCCPL -> "PL"
    PNCCPM -> "PM"
    PNCCPN -> "PN"
    PNCCPR -> "PR"
    PNCCPT -> "PT"
    PNCCPW -> "PW"
    PNCCPY -> "PY"
    PNCCQA -> "QA"
    PNCCRE -> "RE"
    PNCCRO -> "RO"
    PNCCRS -> "RS"
    PNCCRU -> "RU"
    PNCCRW -> "RW"
    PNCCSA -> "SA"
    PNCCSB -> "SB"
    PNCCSC -> "SC"
    PNCCSD -> "SD"
    PNCCSE -> "SE"
    PNCCSG -> "SG"
    PNCCSH -> "SH"
    PNCCSI -> "SI"
    PNCCSJ -> "SJ"
    PNCCSK -> "SK"
    PNCCSL -> "SL"
    PNCCSM -> "SM"
    PNCCSN -> "SN"
    PNCCSO -> "SO"
    PNCCSR -> "SR"
    PNCCST -> "ST"
    PNCCSV -> "SV"
    PNCCSX -> "SX"
    PNCCSY -> "SY"
    PNCCSZ -> "SZ"
    PNCCTC -> "TC"
    PNCCTD -> "TD"
    PNCCTG -> "TG"
    PNCCTH -> "TH"
    PNCCTJ -> "TJ"
    PNCCTK -> "TK"
    PNCCTL -> "TL"
    PNCCTM -> "TM"
    PNCCTN -> "TN"
    PNCCTO -> "TO"
    PNCCTR -> "TR"
    PNCCTT -> "TT"
    PNCCTV -> "TV"
    PNCCTW -> "TW"
    PNCCTZ -> "TZ"
    PNCCUA -> "UA"
    PNCCUG -> "UG"
    PNCCUS -> "US"
    PNCCUY -> "UY"
    PNCCUZ -> "UZ"
    PNCCVA -> "VA"
    PNCCVC -> "VC"
    PNCCVE -> "VE"
    PNCCVG -> "VG"
    PNCCVI -> "VI"
    PNCCVN -> "VN"
    PNCCVU -> "VU"
    PNCCWF -> "WF"
    PNCCWS -> "WS"
    PNCCYE -> "YE"
    PNCCYT -> "YT"
    PNCCZA -> "ZA"
    PNCCZM -> "ZM"
    PNCCZW -> "ZW"

instance Hashable PhoneNumberCountryCode

instance NFData PhoneNumberCountryCode

instance ToByteString PhoneNumberCountryCode

instance ToQuery PhoneNumberCountryCode

instance ToHeader PhoneNumberCountryCode

instance ToJSON PhoneNumberCountryCode where
  toJSON = toJSONText

instance FromJSON PhoneNumberCountryCode where
  parseJSON = parseJSONText "PhoneNumberCountryCode"
