{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.CountryCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53Domains.Types.CountryCode where

import Network.AWS.Prelude

data CountryCode
  = AD
  | AE
  | AF
  | AG
  | AI
  | AL
  | AM
  | AN
  | AO
  | AQ
  | AR
  | AS
  | AT
  | AU
  | AW
  | AZ
  | BA
  | BB
  | BD
  | BE
  | BF
  | BG
  | BH
  | BI
  | BJ
  | BL
  | BM
  | BN
  | BO
  | BR
  | BS
  | BT
  | BW
  | BY
  | BZ
  | CA
  | CC
  | CD
  | CF
  | CG
  | CH
  | CI
  | CK
  | CL
  | CM
  | CN
  | CO
  | CR
  | CU
  | CV
  | CX
  | CY
  | CZ
  | DE
  | DJ
  | DK
  | DM
  | DO
  | DZ
  | EC
  | EE
  | EG
  | ER
  | ES
  | ET
  | FI
  | FJ
  | FK
  | FM
  | FO
  | FR
  | GA
  | GB
  | GD
  | GE
  | GH
  | GI
  | GL
  | GM
  | GN
  | GQ
  | GR
  | GT'
  | GU
  | GW
  | GY
  | HK
  | HN
  | HR
  | HT
  | HU
  | IE
  | IL
  | IM
  | IN
  | IQ
  | IR
  | IS
  | IT
  | Id
  | JM
  | JO
  | JP
  | KE
  | KG
  | KH
  | KI
  | KM
  | KN
  | KP
  | KR
  | KW
  | KY
  | KZ
  | LA
  | LB
  | LC
  | LI
  | LK
  | LR
  | LS
  | LT'
  | LU
  | LV
  | LY
  | MA
  | MC
  | MD
  | ME
  | MF
  | MG
  | MH
  | MK
  | ML
  | MM
  | MN
  | MO
  | MP
  | MR
  | MS
  | MT
  | MU
  | MV
  | MW
  | MX
  | MY
  | MZ
  | NA
  | NC
  | NE
  | NG
  | NI
  | NL
  | NO
  | NP
  | NR
  | NU
  | NZ
  | OM
  | PA
  | PE
  | PF
  | PG
  | PH
  | PK
  | PL
  | PM
  | PN
  | PR
  | PT
  | PW
  | PY
  | QA
  | RO
  | RS
  | RU
  | RW
  | SA
  | SB
  | SC
  | SD
  | SE
  | SG
  | SH
  | SI
  | SK
  | SL
  | SM
  | SN
  | SO
  | SR
  | ST
  | SV
  | SY
  | SZ
  | TC
  | TD
  | TG
  | TH
  | TJ
  | TK
  | TL
  | TM
  | TN
  | TO
  | TR
  | TT
  | TV
  | TW
  | TZ
  | UA
  | UG
  | US
  | UY
  | UZ
  | VA
  | VC
  | VE
  | VG
  | VI
  | VN
  | VU
  | WF
  | WS
  | YE
  | YT
  | ZA
  | ZM
  | ZW
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

instance FromText CountryCode where
  parser =
    takeLowerText >>= \case
      "ad" -> pure AD
      "ae" -> pure AE
      "af" -> pure AF
      "ag" -> pure AG
      "ai" -> pure AI
      "al" -> pure AL
      "am" -> pure AM
      "an" -> pure AN
      "ao" -> pure AO
      "aq" -> pure AQ
      "ar" -> pure AR
      "as" -> pure AS
      "at" -> pure AT
      "au" -> pure AU
      "aw" -> pure AW
      "az" -> pure AZ
      "ba" -> pure BA
      "bb" -> pure BB
      "bd" -> pure BD
      "be" -> pure BE
      "bf" -> pure BF
      "bg" -> pure BG
      "bh" -> pure BH
      "bi" -> pure BI
      "bj" -> pure BJ
      "bl" -> pure BL
      "bm" -> pure BM
      "bn" -> pure BN
      "bo" -> pure BO
      "br" -> pure BR
      "bs" -> pure BS
      "bt" -> pure BT
      "bw" -> pure BW
      "by" -> pure BY
      "bz" -> pure BZ
      "ca" -> pure CA
      "cc" -> pure CC
      "cd" -> pure CD
      "cf" -> pure CF
      "cg" -> pure CG
      "ch" -> pure CH
      "ci" -> pure CI
      "ck" -> pure CK
      "cl" -> pure CL
      "cm" -> pure CM
      "cn" -> pure CN
      "co" -> pure CO
      "cr" -> pure CR
      "cu" -> pure CU
      "cv" -> pure CV
      "cx" -> pure CX
      "cy" -> pure CY
      "cz" -> pure CZ
      "de" -> pure DE
      "dj" -> pure DJ
      "dk" -> pure DK
      "dm" -> pure DM
      "do" -> pure DO
      "dz" -> pure DZ
      "ec" -> pure EC
      "ee" -> pure EE
      "eg" -> pure EG
      "er" -> pure ER
      "es" -> pure ES
      "et" -> pure ET
      "fi" -> pure FI
      "fj" -> pure FJ
      "fk" -> pure FK
      "fm" -> pure FM
      "fo" -> pure FO
      "fr" -> pure FR
      "ga" -> pure GA
      "gb" -> pure GB
      "gd" -> pure GD
      "ge" -> pure GE
      "gh" -> pure GH
      "gi" -> pure GI
      "gl" -> pure GL
      "gm" -> pure GM
      "gn" -> pure GN
      "gq" -> pure GQ
      "gr" -> pure GR
      "gt" -> pure GT'
      "gu" -> pure GU
      "gw" -> pure GW
      "gy" -> pure GY
      "hk" -> pure HK
      "hn" -> pure HN
      "hr" -> pure HR
      "ht" -> pure HT
      "hu" -> pure HU
      "ie" -> pure IE
      "il" -> pure IL
      "im" -> pure IM
      "in" -> pure IN
      "iq" -> pure IQ
      "ir" -> pure IR
      "is" -> pure IS
      "it" -> pure IT
      "id" -> pure Id
      "jm" -> pure JM
      "jo" -> pure JO
      "jp" -> pure JP
      "ke" -> pure KE
      "kg" -> pure KG
      "kh" -> pure KH
      "ki" -> pure KI
      "km" -> pure KM
      "kn" -> pure KN
      "kp" -> pure KP
      "kr" -> pure KR
      "kw" -> pure KW
      "ky" -> pure KY
      "kz" -> pure KZ
      "la" -> pure LA
      "lb" -> pure LB
      "lc" -> pure LC
      "li" -> pure LI
      "lk" -> pure LK
      "lr" -> pure LR
      "ls" -> pure LS
      "lt" -> pure LT'
      "lu" -> pure LU
      "lv" -> pure LV
      "ly" -> pure LY
      "ma" -> pure MA
      "mc" -> pure MC
      "md" -> pure MD
      "me" -> pure ME
      "mf" -> pure MF
      "mg" -> pure MG
      "mh" -> pure MH
      "mk" -> pure MK
      "ml" -> pure ML
      "mm" -> pure MM
      "mn" -> pure MN
      "mo" -> pure MO
      "mp" -> pure MP
      "mr" -> pure MR
      "ms" -> pure MS
      "mt" -> pure MT
      "mu" -> pure MU
      "mv" -> pure MV
      "mw" -> pure MW
      "mx" -> pure MX
      "my" -> pure MY
      "mz" -> pure MZ
      "na" -> pure NA
      "nc" -> pure NC
      "ne" -> pure NE
      "ng" -> pure NG
      "ni" -> pure NI
      "nl" -> pure NL
      "no" -> pure NO
      "np" -> pure NP
      "nr" -> pure NR
      "nu" -> pure NU
      "nz" -> pure NZ
      "om" -> pure OM
      "pa" -> pure PA
      "pe" -> pure PE
      "pf" -> pure PF
      "pg" -> pure PG
      "ph" -> pure PH
      "pk" -> pure PK
      "pl" -> pure PL
      "pm" -> pure PM
      "pn" -> pure PN
      "pr" -> pure PR
      "pt" -> pure PT
      "pw" -> pure PW
      "py" -> pure PY
      "qa" -> pure QA
      "ro" -> pure RO
      "rs" -> pure RS
      "ru" -> pure RU
      "rw" -> pure RW
      "sa" -> pure SA
      "sb" -> pure SB
      "sc" -> pure SC
      "sd" -> pure SD
      "se" -> pure SE
      "sg" -> pure SG
      "sh" -> pure SH
      "si" -> pure SI
      "sk" -> pure SK
      "sl" -> pure SL
      "sm" -> pure SM
      "sn" -> pure SN
      "so" -> pure SO
      "sr" -> pure SR
      "st" -> pure ST
      "sv" -> pure SV
      "sy" -> pure SY
      "sz" -> pure SZ
      "tc" -> pure TC
      "td" -> pure TD
      "tg" -> pure TG
      "th" -> pure TH
      "tj" -> pure TJ
      "tk" -> pure TK
      "tl" -> pure TL
      "tm" -> pure TM
      "tn" -> pure TN
      "to" -> pure TO
      "tr" -> pure TR
      "tt" -> pure TT
      "tv" -> pure TV
      "tw" -> pure TW
      "tz" -> pure TZ
      "ua" -> pure UA
      "ug" -> pure UG
      "us" -> pure US
      "uy" -> pure UY
      "uz" -> pure UZ
      "va" -> pure VA
      "vc" -> pure VC
      "ve" -> pure VE
      "vg" -> pure VG
      "vi" -> pure VI
      "vn" -> pure VN
      "vu" -> pure VU
      "wf" -> pure WF
      "ws" -> pure WS
      "ye" -> pure YE
      "yt" -> pure YT
      "za" -> pure ZA
      "zm" -> pure ZM
      "zw" -> pure ZW
      e ->
        fromTextError $
          "Failure parsing CountryCode from value: '" <> e
            <> "'. Accepted values: ad, ae, af, ag, ai, al, am, an, ao, aq, ar, as, at, au, aw, az, ba, bb, bd, be, bf, bg, bh, bi, bj, bl, bm, bn, bo, br, bs, bt, bw, by, bz, ca, cc, cd, cf, cg, ch, ci, ck, cl, cm, cn, co, cr, cu, cv, cx, cy, cz, de, dj, dk, dm, do, dz, ec, ee, eg, er, es, et, fi, fj, fk, fm, fo, fr, ga, gb, gd, ge, gh, gi, gl, gm, gn, gq, gr, gt, gu, gw, gy, hk, hn, hr, ht, hu, ie, il, im, in, iq, ir, is, it, id, jm, jo, jp, ke, kg, kh, ki, km, kn, kp, kr, kw, ky, kz, la, lb, lc, li, lk, lr, ls, lt, lu, lv, ly, ma, mc, md, me, mf, mg, mh, mk, ml, mm, mn, mo, mp, mr, ms, mt, mu, mv, mw, mx, my, mz, na, nc, ne, ng, ni, nl, no, np, nr, nu, nz, om, pa, pe, pf, pg, ph, pk, pl, pm, pn, pr, pt, pw, py, qa, ro, rs, ru, rw, sa, sb, sc, sd, se, sg, sh, si, sk, sl, sm, sn, so, sr, st, sv, sy, sz, tc, td, tg, th, tj, tk, tl, tm, tn, to, tr, tt, tv, tw, tz, ua, ug, us, uy, uz, va, vc, ve, vg, vi, vn, vu, wf, ws, ye, yt, za, zm, zw"

instance ToText CountryCode where
  toText = \case
    AD -> "AD"
    AE -> "AE"
    AF -> "AF"
    AG -> "AG"
    AI -> "AI"
    AL -> "AL"
    AM -> "AM"
    AN -> "AN"
    AO -> "AO"
    AQ -> "AQ"
    AR -> "AR"
    AS -> "AS"
    AT -> "AT"
    AU -> "AU"
    AW -> "AW"
    AZ -> "AZ"
    BA -> "BA"
    BB -> "BB"
    BD -> "BD"
    BE -> "BE"
    BF -> "BF"
    BG -> "BG"
    BH -> "BH"
    BI -> "BI"
    BJ -> "BJ"
    BL -> "BL"
    BM -> "BM"
    BN -> "BN"
    BO -> "BO"
    BR -> "BR"
    BS -> "BS"
    BT -> "BT"
    BW -> "BW"
    BY -> "BY"
    BZ -> "BZ"
    CA -> "CA"
    CC -> "CC"
    CD -> "CD"
    CF -> "CF"
    CG -> "CG"
    CH -> "CH"
    CI -> "CI"
    CK -> "CK"
    CL -> "CL"
    CM -> "CM"
    CN -> "CN"
    CO -> "CO"
    CR -> "CR"
    CU -> "CU"
    CV -> "CV"
    CX -> "CX"
    CY -> "CY"
    CZ -> "CZ"
    DE -> "DE"
    DJ -> "DJ"
    DK -> "DK"
    DM -> "DM"
    DO -> "DO"
    DZ -> "DZ"
    EC -> "EC"
    EE -> "EE"
    EG -> "EG"
    ER -> "ER"
    ES -> "ES"
    ET -> "ET"
    FI -> "FI"
    FJ -> "FJ"
    FK -> "FK"
    FM -> "FM"
    FO -> "FO"
    FR -> "FR"
    GA -> "GA"
    GB -> "GB"
    GD -> "GD"
    GE -> "GE"
    GH -> "GH"
    GI -> "GI"
    GL -> "GL"
    GM -> "GM"
    GN -> "GN"
    GQ -> "GQ"
    GR -> "GR"
    GT' -> "GT"
    GU -> "GU"
    GW -> "GW"
    GY -> "GY"
    HK -> "HK"
    HN -> "HN"
    HR -> "HR"
    HT -> "HT"
    HU -> "HU"
    IE -> "IE"
    IL -> "IL"
    IM -> "IM"
    IN -> "IN"
    IQ -> "IQ"
    IR -> "IR"
    IS -> "IS"
    IT -> "IT"
    Id -> "ID"
    JM -> "JM"
    JO -> "JO"
    JP -> "JP"
    KE -> "KE"
    KG -> "KG"
    KH -> "KH"
    KI -> "KI"
    KM -> "KM"
    KN -> "KN"
    KP -> "KP"
    KR -> "KR"
    KW -> "KW"
    KY -> "KY"
    KZ -> "KZ"
    LA -> "LA"
    LB -> "LB"
    LC -> "LC"
    LI -> "LI"
    LK -> "LK"
    LR -> "LR"
    LS -> "LS"
    LT' -> "LT"
    LU -> "LU"
    LV -> "LV"
    LY -> "LY"
    MA -> "MA"
    MC -> "MC"
    MD -> "MD"
    ME -> "ME"
    MF -> "MF"
    MG -> "MG"
    MH -> "MH"
    MK -> "MK"
    ML -> "ML"
    MM -> "MM"
    MN -> "MN"
    MO -> "MO"
    MP -> "MP"
    MR -> "MR"
    MS -> "MS"
    MT -> "MT"
    MU -> "MU"
    MV -> "MV"
    MW -> "MW"
    MX -> "MX"
    MY -> "MY"
    MZ -> "MZ"
    NA -> "NA"
    NC -> "NC"
    NE -> "NE"
    NG -> "NG"
    NI -> "NI"
    NL -> "NL"
    NO -> "NO"
    NP -> "NP"
    NR -> "NR"
    NU -> "NU"
    NZ -> "NZ"
    OM -> "OM"
    PA -> "PA"
    PE -> "PE"
    PF -> "PF"
    PG -> "PG"
    PH -> "PH"
    PK -> "PK"
    PL -> "PL"
    PM -> "PM"
    PN -> "PN"
    PR -> "PR"
    PT -> "PT"
    PW -> "PW"
    PY -> "PY"
    QA -> "QA"
    RO -> "RO"
    RS -> "RS"
    RU -> "RU"
    RW -> "RW"
    SA -> "SA"
    SB -> "SB"
    SC -> "SC"
    SD -> "SD"
    SE -> "SE"
    SG -> "SG"
    SH -> "SH"
    SI -> "SI"
    SK -> "SK"
    SL -> "SL"
    SM -> "SM"
    SN -> "SN"
    SO -> "SO"
    SR -> "SR"
    ST -> "ST"
    SV -> "SV"
    SY -> "SY"
    SZ -> "SZ"
    TC -> "TC"
    TD -> "TD"
    TG -> "TG"
    TH -> "TH"
    TJ -> "TJ"
    TK -> "TK"
    TL -> "TL"
    TM -> "TM"
    TN -> "TN"
    TO -> "TO"
    TR -> "TR"
    TT -> "TT"
    TV -> "TV"
    TW -> "TW"
    TZ -> "TZ"
    UA -> "UA"
    UG -> "UG"
    US -> "US"
    UY -> "UY"
    UZ -> "UZ"
    VA -> "VA"
    VC -> "VC"
    VE -> "VE"
    VG -> "VG"
    VI -> "VI"
    VN -> "VN"
    VU -> "VU"
    WF -> "WF"
    WS -> "WS"
    YE -> "YE"
    YT -> "YT"
    ZA -> "ZA"
    ZM -> "ZM"
    ZW -> "ZW"

instance Hashable CountryCode

instance NFData CountryCode

instance ToByteString CountryCode

instance ToQuery CountryCode

instance ToHeader CountryCode

instance ToJSON CountryCode where
  toJSON = toJSONText

instance FromJSON CountryCode where
  parseJSON = parseJSONText "CountryCode"
