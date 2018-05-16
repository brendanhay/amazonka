{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WAFRegional.Types.Sum where

import Network.AWS.Prelude

data ChangeAction
  = Delete
  | Insert
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ChangeAction where
    parser = takeLowerText >>= \case
        "delete" -> pure Delete
        "insert" -> pure Insert
        e -> fromTextError $ "Failure parsing ChangeAction from value: '" <> e
           <> "'. Accepted values: delete, insert"

instance ToText ChangeAction where
    toText = \case
        Delete -> "DELETE"
        Insert -> "INSERT"

instance Hashable     ChangeAction
instance NFData       ChangeAction
instance ToByteString ChangeAction
instance ToQuery      ChangeAction
instance ToHeader     ChangeAction

instance ToJSON ChangeAction where
    toJSON = toJSONText

data ChangeTokenStatus
  = Insync
  | Pending
  | Provisioned
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ChangeTokenStatus where
    parser = takeLowerText >>= \case
        "insync" -> pure Insync
        "pending" -> pure Pending
        "provisioned" -> pure Provisioned
        e -> fromTextError $ "Failure parsing ChangeTokenStatus from value: '" <> e
           <> "'. Accepted values: insync, pending, provisioned"

instance ToText ChangeTokenStatus where
    toText = \case
        Insync -> "INSYNC"
        Pending -> "PENDING"
        Provisioned -> "PROVISIONED"

instance Hashable     ChangeTokenStatus
instance NFData       ChangeTokenStatus
instance ToByteString ChangeTokenStatus
instance ToQuery      ChangeTokenStatus
instance ToHeader     ChangeTokenStatus

instance FromJSON ChangeTokenStatus where
    parseJSON = parseJSONText "ChangeTokenStatus"

data ComparisonOperator
  = EQ'
  | GE
  | GT'
  | LE
  | LT'
  | NE
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ComparisonOperator where
    parser = takeLowerText >>= \case
        "eq" -> pure EQ'
        "ge" -> pure GE
        "gt" -> pure GT'
        "le" -> pure LE
        "lt" -> pure LT'
        "ne" -> pure NE
        e -> fromTextError $ "Failure parsing ComparisonOperator from value: '" <> e
           <> "'. Accepted values: eq, ge, gt, le, lt, ne"

instance ToText ComparisonOperator where
    toText = \case
        EQ' -> "EQ"
        GE -> "GE"
        GT' -> "GT"
        LE -> "LE"
        LT' -> "LT"
        NE -> "NE"

instance Hashable     ComparisonOperator
instance NFData       ComparisonOperator
instance ToByteString ComparisonOperator
instance ToQuery      ComparisonOperator
instance ToHeader     ComparisonOperator

instance ToJSON ComparisonOperator where
    toJSON = toJSONText

instance FromJSON ComparisonOperator where
    parseJSON = parseJSONText "ComparisonOperator"

data GeoMatchConstraintType =
  Country
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText GeoMatchConstraintType where
    parser = takeLowerText >>= \case
        "country" -> pure Country
        e -> fromTextError $ "Failure parsing GeoMatchConstraintType from value: '" <> e
           <> "'. Accepted values: country"

instance ToText GeoMatchConstraintType where
    toText = \case
        Country -> "Country"

instance Hashable     GeoMatchConstraintType
instance NFData       GeoMatchConstraintType
instance ToByteString GeoMatchConstraintType
instance ToQuery      GeoMatchConstraintType
instance ToHeader     GeoMatchConstraintType

instance ToJSON GeoMatchConstraintType where
    toJSON = toJSONText

instance FromJSON GeoMatchConstraintType where
    parseJSON = parseJSONText "GeoMatchConstraintType"

data GeoMatchConstraintValue
  = GMCVAD
  | GMCVAE
  | GMCVAF
  | GMCVAG
  | GMCVAI
  | GMCVAL
  | GMCVAM
  | GMCVAO
  | GMCVAQ
  | GMCVAR
  | GMCVAS
  | GMCVAT
  | GMCVAU
  | GMCVAW
  | GMCVAX
  | GMCVAZ
  | GMCVBA
  | GMCVBB
  | GMCVBD
  | GMCVBE
  | GMCVBF
  | GMCVBG
  | GMCVBH
  | GMCVBI
  | GMCVBJ
  | GMCVBL
  | GMCVBM
  | GMCVBN
  | GMCVBO
  | GMCVBQ
  | GMCVBR
  | GMCVBS
  | GMCVBT
  | GMCVBV
  | GMCVBW
  | GMCVBY
  | GMCVBZ
  | GMCVCA
  | GMCVCC
  | GMCVCD
  | GMCVCF
  | GMCVCG
  | GMCVCH
  | GMCVCI
  | GMCVCK
  | GMCVCL
  | GMCVCM
  | GMCVCN
  | GMCVCO
  | GMCVCR
  | GMCVCU
  | GMCVCV
  | GMCVCW
  | GMCVCX
  | GMCVCY
  | GMCVCZ
  | GMCVDE
  | GMCVDJ
  | GMCVDK
  | GMCVDM
  | GMCVDO
  | GMCVDZ
  | GMCVEC
  | GMCVEE
  | GMCVEG
  | GMCVEH
  | GMCVER
  | GMCVES
  | GMCVET
  | GMCVFI
  | GMCVFJ
  | GMCVFK
  | GMCVFM
  | GMCVFO
  | GMCVFR
  | GMCVGA
  | GMCVGB
  | GMCVGD
  | GMCVGE
  | GMCVGF
  | GMCVGG
  | GMCVGH
  | GMCVGI
  | GMCVGL
  | GMCVGM
  | GMCVGN
  | GMCVGP
  | GMCVGQ
  | GMCVGR
  | GMCVGS
  | GMCVGT'
  | GMCVGU
  | GMCVGW
  | GMCVGY
  | GMCVHK
  | GMCVHM
  | GMCVHN
  | GMCVHR
  | GMCVHT
  | GMCVHU
  | GMCVIE
  | GMCVIL
  | GMCVIM
  | GMCVIN
  | GMCVIO
  | GMCVIQ
  | GMCVIR
  | GMCVIS
  | GMCVIT
  | GMCVId
  | GMCVJE
  | GMCVJM
  | GMCVJO
  | GMCVJP
  | GMCVKE
  | GMCVKG
  | GMCVKH
  | GMCVKI
  | GMCVKM
  | GMCVKN
  | GMCVKP
  | GMCVKR
  | GMCVKW
  | GMCVKY
  | GMCVKZ
  | GMCVLA
  | GMCVLB
  | GMCVLC
  | GMCVLI
  | GMCVLK
  | GMCVLR
  | GMCVLS
  | GMCVLT'
  | GMCVLU
  | GMCVLV
  | GMCVLY
  | GMCVMA
  | GMCVMC
  | GMCVMD
  | GMCVME
  | GMCVMF
  | GMCVMG
  | GMCVMH
  | GMCVMK
  | GMCVML
  | GMCVMM
  | GMCVMN
  | GMCVMO
  | GMCVMP
  | GMCVMQ
  | GMCVMR
  | GMCVMS
  | GMCVMT
  | GMCVMU
  | GMCVMV
  | GMCVMW
  | GMCVMX
  | GMCVMY
  | GMCVMZ
  | GMCVNA
  | GMCVNC
  | GMCVNE
  | GMCVNF
  | GMCVNG
  | GMCVNI
  | GMCVNL
  | GMCVNO
  | GMCVNP
  | GMCVNR
  | GMCVNU
  | GMCVNZ
  | GMCVOM
  | GMCVPA
  | GMCVPE
  | GMCVPF
  | GMCVPG
  | GMCVPH
  | GMCVPK
  | GMCVPL
  | GMCVPM
  | GMCVPN
  | GMCVPR
  | GMCVPS
  | GMCVPT
  | GMCVPW
  | GMCVPY
  | GMCVQA
  | GMCVRE
  | GMCVRO
  | GMCVRS
  | GMCVRU
  | GMCVRW
  | GMCVSA
  | GMCVSB
  | GMCVSC
  | GMCVSD
  | GMCVSE
  | GMCVSG
  | GMCVSH
  | GMCVSI
  | GMCVSJ
  | GMCVSK
  | GMCVSL
  | GMCVSM
  | GMCVSN
  | GMCVSO
  | GMCVSR
  | GMCVSS
  | GMCVST
  | GMCVSV
  | GMCVSX
  | GMCVSY
  | GMCVSZ
  | GMCVTC
  | GMCVTD
  | GMCVTF
  | GMCVTG
  | GMCVTH
  | GMCVTJ
  | GMCVTK
  | GMCVTL
  | GMCVTM
  | GMCVTN
  | GMCVTO
  | GMCVTR
  | GMCVTT
  | GMCVTV
  | GMCVTW
  | GMCVTZ
  | GMCVUA
  | GMCVUG
  | GMCVUM
  | GMCVUS
  | GMCVUY
  | GMCVUZ
  | GMCVVA
  | GMCVVC
  | GMCVVE
  | GMCVVG
  | GMCVVI
  | GMCVVN
  | GMCVVU
  | GMCVWF
  | GMCVWS
  | GMCVYE
  | GMCVYT
  | GMCVZA
  | GMCVZM
  | GMCVZW
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText GeoMatchConstraintValue where
    parser = takeLowerText >>= \case
        "ad" -> pure GMCVAD
        "ae" -> pure GMCVAE
        "af" -> pure GMCVAF
        "ag" -> pure GMCVAG
        "ai" -> pure GMCVAI
        "al" -> pure GMCVAL
        "am" -> pure GMCVAM
        "ao" -> pure GMCVAO
        "aq" -> pure GMCVAQ
        "ar" -> pure GMCVAR
        "as" -> pure GMCVAS
        "at" -> pure GMCVAT
        "au" -> pure GMCVAU
        "aw" -> pure GMCVAW
        "ax" -> pure GMCVAX
        "az" -> pure GMCVAZ
        "ba" -> pure GMCVBA
        "bb" -> pure GMCVBB
        "bd" -> pure GMCVBD
        "be" -> pure GMCVBE
        "bf" -> pure GMCVBF
        "bg" -> pure GMCVBG
        "bh" -> pure GMCVBH
        "bi" -> pure GMCVBI
        "bj" -> pure GMCVBJ
        "bl" -> pure GMCVBL
        "bm" -> pure GMCVBM
        "bn" -> pure GMCVBN
        "bo" -> pure GMCVBO
        "bq" -> pure GMCVBQ
        "br" -> pure GMCVBR
        "bs" -> pure GMCVBS
        "bt" -> pure GMCVBT
        "bv" -> pure GMCVBV
        "bw" -> pure GMCVBW
        "by" -> pure GMCVBY
        "bz" -> pure GMCVBZ
        "ca" -> pure GMCVCA
        "cc" -> pure GMCVCC
        "cd" -> pure GMCVCD
        "cf" -> pure GMCVCF
        "cg" -> pure GMCVCG
        "ch" -> pure GMCVCH
        "ci" -> pure GMCVCI
        "ck" -> pure GMCVCK
        "cl" -> pure GMCVCL
        "cm" -> pure GMCVCM
        "cn" -> pure GMCVCN
        "co" -> pure GMCVCO
        "cr" -> pure GMCVCR
        "cu" -> pure GMCVCU
        "cv" -> pure GMCVCV
        "cw" -> pure GMCVCW
        "cx" -> pure GMCVCX
        "cy" -> pure GMCVCY
        "cz" -> pure GMCVCZ
        "de" -> pure GMCVDE
        "dj" -> pure GMCVDJ
        "dk" -> pure GMCVDK
        "dm" -> pure GMCVDM
        "do" -> pure GMCVDO
        "dz" -> pure GMCVDZ
        "ec" -> pure GMCVEC
        "ee" -> pure GMCVEE
        "eg" -> pure GMCVEG
        "eh" -> pure GMCVEH
        "er" -> pure GMCVER
        "es" -> pure GMCVES
        "et" -> pure GMCVET
        "fi" -> pure GMCVFI
        "fj" -> pure GMCVFJ
        "fk" -> pure GMCVFK
        "fm" -> pure GMCVFM
        "fo" -> pure GMCVFO
        "fr" -> pure GMCVFR
        "ga" -> pure GMCVGA
        "gb" -> pure GMCVGB
        "gd" -> pure GMCVGD
        "ge" -> pure GMCVGE
        "gf" -> pure GMCVGF
        "gg" -> pure GMCVGG
        "gh" -> pure GMCVGH
        "gi" -> pure GMCVGI
        "gl" -> pure GMCVGL
        "gm" -> pure GMCVGM
        "gn" -> pure GMCVGN
        "gp" -> pure GMCVGP
        "gq" -> pure GMCVGQ
        "gr" -> pure GMCVGR
        "gs" -> pure GMCVGS
        "gt" -> pure GMCVGT'
        "gu" -> pure GMCVGU
        "gw" -> pure GMCVGW
        "gy" -> pure GMCVGY
        "hk" -> pure GMCVHK
        "hm" -> pure GMCVHM
        "hn" -> pure GMCVHN
        "hr" -> pure GMCVHR
        "ht" -> pure GMCVHT
        "hu" -> pure GMCVHU
        "ie" -> pure GMCVIE
        "il" -> pure GMCVIL
        "im" -> pure GMCVIM
        "in" -> pure GMCVIN
        "io" -> pure GMCVIO
        "iq" -> pure GMCVIQ
        "ir" -> pure GMCVIR
        "is" -> pure GMCVIS
        "it" -> pure GMCVIT
        "id" -> pure GMCVId
        "je" -> pure GMCVJE
        "jm" -> pure GMCVJM
        "jo" -> pure GMCVJO
        "jp" -> pure GMCVJP
        "ke" -> pure GMCVKE
        "kg" -> pure GMCVKG
        "kh" -> pure GMCVKH
        "ki" -> pure GMCVKI
        "km" -> pure GMCVKM
        "kn" -> pure GMCVKN
        "kp" -> pure GMCVKP
        "kr" -> pure GMCVKR
        "kw" -> pure GMCVKW
        "ky" -> pure GMCVKY
        "kz" -> pure GMCVKZ
        "la" -> pure GMCVLA
        "lb" -> pure GMCVLB
        "lc" -> pure GMCVLC
        "li" -> pure GMCVLI
        "lk" -> pure GMCVLK
        "lr" -> pure GMCVLR
        "ls" -> pure GMCVLS
        "lt" -> pure GMCVLT'
        "lu" -> pure GMCVLU
        "lv" -> pure GMCVLV
        "ly" -> pure GMCVLY
        "ma" -> pure GMCVMA
        "mc" -> pure GMCVMC
        "md" -> pure GMCVMD
        "me" -> pure GMCVME
        "mf" -> pure GMCVMF
        "mg" -> pure GMCVMG
        "mh" -> pure GMCVMH
        "mk" -> pure GMCVMK
        "ml" -> pure GMCVML
        "mm" -> pure GMCVMM
        "mn" -> pure GMCVMN
        "mo" -> pure GMCVMO
        "mp" -> pure GMCVMP
        "mq" -> pure GMCVMQ
        "mr" -> pure GMCVMR
        "ms" -> pure GMCVMS
        "mt" -> pure GMCVMT
        "mu" -> pure GMCVMU
        "mv" -> pure GMCVMV
        "mw" -> pure GMCVMW
        "mx" -> pure GMCVMX
        "my" -> pure GMCVMY
        "mz" -> pure GMCVMZ
        "na" -> pure GMCVNA
        "nc" -> pure GMCVNC
        "ne" -> pure GMCVNE
        "nf" -> pure GMCVNF
        "ng" -> pure GMCVNG
        "ni" -> pure GMCVNI
        "nl" -> pure GMCVNL
        "no" -> pure GMCVNO
        "np" -> pure GMCVNP
        "nr" -> pure GMCVNR
        "nu" -> pure GMCVNU
        "nz" -> pure GMCVNZ
        "om" -> pure GMCVOM
        "pa" -> pure GMCVPA
        "pe" -> pure GMCVPE
        "pf" -> pure GMCVPF
        "pg" -> pure GMCVPG
        "ph" -> pure GMCVPH
        "pk" -> pure GMCVPK
        "pl" -> pure GMCVPL
        "pm" -> pure GMCVPM
        "pn" -> pure GMCVPN
        "pr" -> pure GMCVPR
        "ps" -> pure GMCVPS
        "pt" -> pure GMCVPT
        "pw" -> pure GMCVPW
        "py" -> pure GMCVPY
        "qa" -> pure GMCVQA
        "re" -> pure GMCVRE
        "ro" -> pure GMCVRO
        "rs" -> pure GMCVRS
        "ru" -> pure GMCVRU
        "rw" -> pure GMCVRW
        "sa" -> pure GMCVSA
        "sb" -> pure GMCVSB
        "sc" -> pure GMCVSC
        "sd" -> pure GMCVSD
        "se" -> pure GMCVSE
        "sg" -> pure GMCVSG
        "sh" -> pure GMCVSH
        "si" -> pure GMCVSI
        "sj" -> pure GMCVSJ
        "sk" -> pure GMCVSK
        "sl" -> pure GMCVSL
        "sm" -> pure GMCVSM
        "sn" -> pure GMCVSN
        "so" -> pure GMCVSO
        "sr" -> pure GMCVSR
        "ss" -> pure GMCVSS
        "st" -> pure GMCVST
        "sv" -> pure GMCVSV
        "sx" -> pure GMCVSX
        "sy" -> pure GMCVSY
        "sz" -> pure GMCVSZ
        "tc" -> pure GMCVTC
        "td" -> pure GMCVTD
        "tf" -> pure GMCVTF
        "tg" -> pure GMCVTG
        "th" -> pure GMCVTH
        "tj" -> pure GMCVTJ
        "tk" -> pure GMCVTK
        "tl" -> pure GMCVTL
        "tm" -> pure GMCVTM
        "tn" -> pure GMCVTN
        "to" -> pure GMCVTO
        "tr" -> pure GMCVTR
        "tt" -> pure GMCVTT
        "tv" -> pure GMCVTV
        "tw" -> pure GMCVTW
        "tz" -> pure GMCVTZ
        "ua" -> pure GMCVUA
        "ug" -> pure GMCVUG
        "um" -> pure GMCVUM
        "us" -> pure GMCVUS
        "uy" -> pure GMCVUY
        "uz" -> pure GMCVUZ
        "va" -> pure GMCVVA
        "vc" -> pure GMCVVC
        "ve" -> pure GMCVVE
        "vg" -> pure GMCVVG
        "vi" -> pure GMCVVI
        "vn" -> pure GMCVVN
        "vu" -> pure GMCVVU
        "wf" -> pure GMCVWF
        "ws" -> pure GMCVWS
        "ye" -> pure GMCVYE
        "yt" -> pure GMCVYT
        "za" -> pure GMCVZA
        "zm" -> pure GMCVZM
        "zw" -> pure GMCVZW
        e -> fromTextError $ "Failure parsing GeoMatchConstraintValue from value: '" <> e
           <> "'. Accepted values: ad, ae, af, ag, ai, al, am, ao, aq, ar, as, at, au, aw, ax, az, ba, bb, bd, be, bf, bg, bh, bi, bj, bl, bm, bn, bo, bq, br, bs, bt, bv, bw, by, bz, ca, cc, cd, cf, cg, ch, ci, ck, cl, cm, cn, co, cr, cu, cv, cw, cx, cy, cz, de, dj, dk, dm, do, dz, ec, ee, eg, eh, er, es, et, fi, fj, fk, fm, fo, fr, ga, gb, gd, ge, gf, gg, gh, gi, gl, gm, gn, gp, gq, gr, gs, gt, gu, gw, gy, hk, hm, hn, hr, ht, hu, ie, il, im, in, io, iq, ir, is, it, id, je, jm, jo, jp, ke, kg, kh, ki, km, kn, kp, kr, kw, ky, kz, la, lb, lc, li, lk, lr, ls, lt, lu, lv, ly, ma, mc, md, me, mf, mg, mh, mk, ml, mm, mn, mo, mp, mq, mr, ms, mt, mu, mv, mw, mx, my, mz, na, nc, ne, nf, ng, ni, nl, no, np, nr, nu, nz, om, pa, pe, pf, pg, ph, pk, pl, pm, pn, pr, ps, pt, pw, py, qa, re, ro, rs, ru, rw, sa, sb, sc, sd, se, sg, sh, si, sj, sk, sl, sm, sn, so, sr, ss, st, sv, sx, sy, sz, tc, td, tf, tg, th, tj, tk, tl, tm, tn, to, tr, tt, tv, tw, tz, ua, ug, um, us, uy, uz, va, vc, ve, vg, vi, vn, vu, wf, ws, ye, yt, za, zm, zw"

instance ToText GeoMatchConstraintValue where
    toText = \case
        GMCVAD -> "AD"
        GMCVAE -> "AE"
        GMCVAF -> "AF"
        GMCVAG -> "AG"
        GMCVAI -> "AI"
        GMCVAL -> "AL"
        GMCVAM -> "AM"
        GMCVAO -> "AO"
        GMCVAQ -> "AQ"
        GMCVAR -> "AR"
        GMCVAS -> "AS"
        GMCVAT -> "AT"
        GMCVAU -> "AU"
        GMCVAW -> "AW"
        GMCVAX -> "AX"
        GMCVAZ -> "AZ"
        GMCVBA -> "BA"
        GMCVBB -> "BB"
        GMCVBD -> "BD"
        GMCVBE -> "BE"
        GMCVBF -> "BF"
        GMCVBG -> "BG"
        GMCVBH -> "BH"
        GMCVBI -> "BI"
        GMCVBJ -> "BJ"
        GMCVBL -> "BL"
        GMCVBM -> "BM"
        GMCVBN -> "BN"
        GMCVBO -> "BO"
        GMCVBQ -> "BQ"
        GMCVBR -> "BR"
        GMCVBS -> "BS"
        GMCVBT -> "BT"
        GMCVBV -> "BV"
        GMCVBW -> "BW"
        GMCVBY -> "BY"
        GMCVBZ -> "BZ"
        GMCVCA -> "CA"
        GMCVCC -> "CC"
        GMCVCD -> "CD"
        GMCVCF -> "CF"
        GMCVCG -> "CG"
        GMCVCH -> "CH"
        GMCVCI -> "CI"
        GMCVCK -> "CK"
        GMCVCL -> "CL"
        GMCVCM -> "CM"
        GMCVCN -> "CN"
        GMCVCO -> "CO"
        GMCVCR -> "CR"
        GMCVCU -> "CU"
        GMCVCV -> "CV"
        GMCVCW -> "CW"
        GMCVCX -> "CX"
        GMCVCY -> "CY"
        GMCVCZ -> "CZ"
        GMCVDE -> "DE"
        GMCVDJ -> "DJ"
        GMCVDK -> "DK"
        GMCVDM -> "DM"
        GMCVDO -> "DO"
        GMCVDZ -> "DZ"
        GMCVEC -> "EC"
        GMCVEE -> "EE"
        GMCVEG -> "EG"
        GMCVEH -> "EH"
        GMCVER -> "ER"
        GMCVES -> "ES"
        GMCVET -> "ET"
        GMCVFI -> "FI"
        GMCVFJ -> "FJ"
        GMCVFK -> "FK"
        GMCVFM -> "FM"
        GMCVFO -> "FO"
        GMCVFR -> "FR"
        GMCVGA -> "GA"
        GMCVGB -> "GB"
        GMCVGD -> "GD"
        GMCVGE -> "GE"
        GMCVGF -> "GF"
        GMCVGG -> "GG"
        GMCVGH -> "GH"
        GMCVGI -> "GI"
        GMCVGL -> "GL"
        GMCVGM -> "GM"
        GMCVGN -> "GN"
        GMCVGP -> "GP"
        GMCVGQ -> "GQ"
        GMCVGR -> "GR"
        GMCVGS -> "GS"
        GMCVGT' -> "GT"
        GMCVGU -> "GU"
        GMCVGW -> "GW"
        GMCVGY -> "GY"
        GMCVHK -> "HK"
        GMCVHM -> "HM"
        GMCVHN -> "HN"
        GMCVHR -> "HR"
        GMCVHT -> "HT"
        GMCVHU -> "HU"
        GMCVIE -> "IE"
        GMCVIL -> "IL"
        GMCVIM -> "IM"
        GMCVIN -> "IN"
        GMCVIO -> "IO"
        GMCVIQ -> "IQ"
        GMCVIR -> "IR"
        GMCVIS -> "IS"
        GMCVIT -> "IT"
        GMCVId -> "ID"
        GMCVJE -> "JE"
        GMCVJM -> "JM"
        GMCVJO -> "JO"
        GMCVJP -> "JP"
        GMCVKE -> "KE"
        GMCVKG -> "KG"
        GMCVKH -> "KH"
        GMCVKI -> "KI"
        GMCVKM -> "KM"
        GMCVKN -> "KN"
        GMCVKP -> "KP"
        GMCVKR -> "KR"
        GMCVKW -> "KW"
        GMCVKY -> "KY"
        GMCVKZ -> "KZ"
        GMCVLA -> "LA"
        GMCVLB -> "LB"
        GMCVLC -> "LC"
        GMCVLI -> "LI"
        GMCVLK -> "LK"
        GMCVLR -> "LR"
        GMCVLS -> "LS"
        GMCVLT' -> "LT"
        GMCVLU -> "LU"
        GMCVLV -> "LV"
        GMCVLY -> "LY"
        GMCVMA -> "MA"
        GMCVMC -> "MC"
        GMCVMD -> "MD"
        GMCVME -> "ME"
        GMCVMF -> "MF"
        GMCVMG -> "MG"
        GMCVMH -> "MH"
        GMCVMK -> "MK"
        GMCVML -> "ML"
        GMCVMM -> "MM"
        GMCVMN -> "MN"
        GMCVMO -> "MO"
        GMCVMP -> "MP"
        GMCVMQ -> "MQ"
        GMCVMR -> "MR"
        GMCVMS -> "MS"
        GMCVMT -> "MT"
        GMCVMU -> "MU"
        GMCVMV -> "MV"
        GMCVMW -> "MW"
        GMCVMX -> "MX"
        GMCVMY -> "MY"
        GMCVMZ -> "MZ"
        GMCVNA -> "NA"
        GMCVNC -> "NC"
        GMCVNE -> "NE"
        GMCVNF -> "NF"
        GMCVNG -> "NG"
        GMCVNI -> "NI"
        GMCVNL -> "NL"
        GMCVNO -> "NO"
        GMCVNP -> "NP"
        GMCVNR -> "NR"
        GMCVNU -> "NU"
        GMCVNZ -> "NZ"
        GMCVOM -> "OM"
        GMCVPA -> "PA"
        GMCVPE -> "PE"
        GMCVPF -> "PF"
        GMCVPG -> "PG"
        GMCVPH -> "PH"
        GMCVPK -> "PK"
        GMCVPL -> "PL"
        GMCVPM -> "PM"
        GMCVPN -> "PN"
        GMCVPR -> "PR"
        GMCVPS -> "PS"
        GMCVPT -> "PT"
        GMCVPW -> "PW"
        GMCVPY -> "PY"
        GMCVQA -> "QA"
        GMCVRE -> "RE"
        GMCVRO -> "RO"
        GMCVRS -> "RS"
        GMCVRU -> "RU"
        GMCVRW -> "RW"
        GMCVSA -> "SA"
        GMCVSB -> "SB"
        GMCVSC -> "SC"
        GMCVSD -> "SD"
        GMCVSE -> "SE"
        GMCVSG -> "SG"
        GMCVSH -> "SH"
        GMCVSI -> "SI"
        GMCVSJ -> "SJ"
        GMCVSK -> "SK"
        GMCVSL -> "SL"
        GMCVSM -> "SM"
        GMCVSN -> "SN"
        GMCVSO -> "SO"
        GMCVSR -> "SR"
        GMCVSS -> "SS"
        GMCVST -> "ST"
        GMCVSV -> "SV"
        GMCVSX -> "SX"
        GMCVSY -> "SY"
        GMCVSZ -> "SZ"
        GMCVTC -> "TC"
        GMCVTD -> "TD"
        GMCVTF -> "TF"
        GMCVTG -> "TG"
        GMCVTH -> "TH"
        GMCVTJ -> "TJ"
        GMCVTK -> "TK"
        GMCVTL -> "TL"
        GMCVTM -> "TM"
        GMCVTN -> "TN"
        GMCVTO -> "TO"
        GMCVTR -> "TR"
        GMCVTT -> "TT"
        GMCVTV -> "TV"
        GMCVTW -> "TW"
        GMCVTZ -> "TZ"
        GMCVUA -> "UA"
        GMCVUG -> "UG"
        GMCVUM -> "UM"
        GMCVUS -> "US"
        GMCVUY -> "UY"
        GMCVUZ -> "UZ"
        GMCVVA -> "VA"
        GMCVVC -> "VC"
        GMCVVE -> "VE"
        GMCVVG -> "VG"
        GMCVVI -> "VI"
        GMCVVN -> "VN"
        GMCVVU -> "VU"
        GMCVWF -> "WF"
        GMCVWS -> "WS"
        GMCVYE -> "YE"
        GMCVYT -> "YT"
        GMCVZA -> "ZA"
        GMCVZM -> "ZM"
        GMCVZW -> "ZW"

instance Hashable     GeoMatchConstraintValue
instance NFData       GeoMatchConstraintValue
instance ToByteString GeoMatchConstraintValue
instance ToQuery      GeoMatchConstraintValue
instance ToHeader     GeoMatchConstraintValue

instance ToJSON GeoMatchConstraintValue where
    toJSON = toJSONText

instance FromJSON GeoMatchConstraintValue where
    parseJSON = parseJSONText "GeoMatchConstraintValue"

data IPSetDescriptorType
  = IPV4
  | IPV6
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText IPSetDescriptorType where
    parser = takeLowerText >>= \case
        "ipv4" -> pure IPV4
        "ipv6" -> pure IPV6
        e -> fromTextError $ "Failure parsing IPSetDescriptorType from value: '" <> e
           <> "'. Accepted values: ipv4, ipv6"

instance ToText IPSetDescriptorType where
    toText = \case
        IPV4 -> "IPV4"
        IPV6 -> "IPV6"

instance Hashable     IPSetDescriptorType
instance NFData       IPSetDescriptorType
instance ToByteString IPSetDescriptorType
instance ToQuery      IPSetDescriptorType
instance ToHeader     IPSetDescriptorType

instance ToJSON IPSetDescriptorType where
    toJSON = toJSONText

instance FromJSON IPSetDescriptorType where
    parseJSON = parseJSONText "IPSetDescriptorType"

data MatchFieldType
  = Body
  | Header
  | Method
  | QueryString
  | URI
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText MatchFieldType where
    parser = takeLowerText >>= \case
        "body" -> pure Body
        "header" -> pure Header
        "method" -> pure Method
        "query_string" -> pure QueryString
        "uri" -> pure URI
        e -> fromTextError $ "Failure parsing MatchFieldType from value: '" <> e
           <> "'. Accepted values: body, header, method, query_string, uri"

instance ToText MatchFieldType where
    toText = \case
        Body -> "BODY"
        Header -> "HEADER"
        Method -> "METHOD"
        QueryString -> "QUERY_STRING"
        URI -> "URI"

instance Hashable     MatchFieldType
instance NFData       MatchFieldType
instance ToByteString MatchFieldType
instance ToQuery      MatchFieldType
instance ToHeader     MatchFieldType

instance ToJSON MatchFieldType where
    toJSON = toJSONText

instance FromJSON MatchFieldType where
    parseJSON = parseJSONText "MatchFieldType"

data PositionalConstraint
  = Contains
  | ContainsWord
  | EndsWith
  | Exactly
  | StartsWith
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PositionalConstraint where
    parser = takeLowerText >>= \case
        "contains" -> pure Contains
        "contains_word" -> pure ContainsWord
        "ends_with" -> pure EndsWith
        "exactly" -> pure Exactly
        "starts_with" -> pure StartsWith
        e -> fromTextError $ "Failure parsing PositionalConstraint from value: '" <> e
           <> "'. Accepted values: contains, contains_word, ends_with, exactly, starts_with"

instance ToText PositionalConstraint where
    toText = \case
        Contains -> "CONTAINS"
        ContainsWord -> "CONTAINS_WORD"
        EndsWith -> "ENDS_WITH"
        Exactly -> "EXACTLY"
        StartsWith -> "STARTS_WITH"

instance Hashable     PositionalConstraint
instance NFData       PositionalConstraint
instance ToByteString PositionalConstraint
instance ToQuery      PositionalConstraint
instance ToHeader     PositionalConstraint

instance ToJSON PositionalConstraint where
    toJSON = toJSONText

instance FromJSON PositionalConstraint where
    parseJSON = parseJSONText "PositionalConstraint"

data PredicateType
  = ByteMatch
  | GeoMatch
  | IPMatch
  | RegexMatch
  | SizeConstraint
  | SqlInjectionMatch
  | XSSMatch
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PredicateType where
    parser = takeLowerText >>= \case
        "bytematch" -> pure ByteMatch
        "geomatch" -> pure GeoMatch
        "ipmatch" -> pure IPMatch
        "regexmatch" -> pure RegexMatch
        "sizeconstraint" -> pure SizeConstraint
        "sqlinjectionmatch" -> pure SqlInjectionMatch
        "xssmatch" -> pure XSSMatch
        e -> fromTextError $ "Failure parsing PredicateType from value: '" <> e
           <> "'. Accepted values: bytematch, geomatch, ipmatch, regexmatch, sizeconstraint, sqlinjectionmatch, xssmatch"

instance ToText PredicateType where
    toText = \case
        ByteMatch -> "ByteMatch"
        GeoMatch -> "GeoMatch"
        IPMatch -> "IPMatch"
        RegexMatch -> "RegexMatch"
        SizeConstraint -> "SizeConstraint"
        SqlInjectionMatch -> "SqlInjectionMatch"
        XSSMatch -> "XssMatch"

instance Hashable     PredicateType
instance NFData       PredicateType
instance ToByteString PredicateType
instance ToQuery      PredicateType
instance ToHeader     PredicateType

instance ToJSON PredicateType where
    toJSON = toJSONText

instance FromJSON PredicateType where
    parseJSON = parseJSONText "PredicateType"

data RateKey =
  IP
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText RateKey where
    parser = takeLowerText >>= \case
        "ip" -> pure IP
        e -> fromTextError $ "Failure parsing RateKey from value: '" <> e
           <> "'. Accepted values: ip"

instance ToText RateKey where
    toText = \case
        IP -> "IP"

instance Hashable     RateKey
instance NFData       RateKey
instance ToByteString RateKey
instance ToQuery      RateKey
instance ToHeader     RateKey

instance ToJSON RateKey where
    toJSON = toJSONText

instance FromJSON RateKey where
    parseJSON = parseJSONText "RateKey"

data TextTransformation
  = CmdLine
  | CompressWhiteSpace
  | HTMLEntityDecode
  | Lowercase
  | None
  | URLDecode
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText TextTransformation where
    parser = takeLowerText >>= \case
        "cmd_line" -> pure CmdLine
        "compress_white_space" -> pure CompressWhiteSpace
        "html_entity_decode" -> pure HTMLEntityDecode
        "lowercase" -> pure Lowercase
        "none" -> pure None
        "url_decode" -> pure URLDecode
        e -> fromTextError $ "Failure parsing TextTransformation from value: '" <> e
           <> "'. Accepted values: cmd_line, compress_white_space, html_entity_decode, lowercase, none, url_decode"

instance ToText TextTransformation where
    toText = \case
        CmdLine -> "CMD_LINE"
        CompressWhiteSpace -> "COMPRESS_WHITE_SPACE"
        HTMLEntityDecode -> "HTML_ENTITY_DECODE"
        Lowercase -> "LOWERCASE"
        None -> "NONE"
        URLDecode -> "URL_DECODE"

instance Hashable     TextTransformation
instance NFData       TextTransformation
instance ToByteString TextTransformation
instance ToQuery      TextTransformation
instance ToHeader     TextTransformation

instance ToJSON TextTransformation where
    toJSON = toJSONText

instance FromJSON TextTransformation where
    parseJSON = parseJSONText "TextTransformation"

data WafActionType
  = Allow
  | Block
  | Count
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText WafActionType where
    parser = takeLowerText >>= \case
        "allow" -> pure Allow
        "block" -> pure Block
        "count" -> pure Count
        e -> fromTextError $ "Failure parsing WafActionType from value: '" <> e
           <> "'. Accepted values: allow, block, count"

instance ToText WafActionType where
    toText = \case
        Allow -> "ALLOW"
        Block -> "BLOCK"
        Count -> "COUNT"

instance Hashable     WafActionType
instance NFData       WafActionType
instance ToByteString WafActionType
instance ToQuery      WafActionType
instance ToHeader     WafActionType

instance ToJSON WafActionType where
    toJSON = toJSONText

instance FromJSON WafActionType where
    parseJSON = parseJSONText "WafActionType"

data WafOverrideActionType
  = WOATCount
  | WOATNone
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText WafOverrideActionType where
    parser = takeLowerText >>= \case
        "count" -> pure WOATCount
        "none" -> pure WOATNone
        e -> fromTextError $ "Failure parsing WafOverrideActionType from value: '" <> e
           <> "'. Accepted values: count, none"

instance ToText WafOverrideActionType where
    toText = \case
        WOATCount -> "COUNT"
        WOATNone -> "NONE"

instance Hashable     WafOverrideActionType
instance NFData       WafOverrideActionType
instance ToByteString WafOverrideActionType
instance ToQuery      WafOverrideActionType
instance ToHeader     WafOverrideActionType

instance ToJSON WafOverrideActionType where
    toJSON = toJSONText

instance FromJSON WafOverrideActionType where
    parseJSON = parseJSONText "WafOverrideActionType"

data WafRuleType
  = Group
  | RateBased
  | Regular
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText WafRuleType where
    parser = takeLowerText >>= \case
        "group" -> pure Group
        "rate_based" -> pure RateBased
        "regular" -> pure Regular
        e -> fromTextError $ "Failure parsing WafRuleType from value: '" <> e
           <> "'. Accepted values: group, rate_based, regular"

instance ToText WafRuleType where
    toText = \case
        Group -> "GROUP"
        RateBased -> "RATE_BASED"
        Regular -> "REGULAR"

instance Hashable     WafRuleType
instance NFData       WafRuleType
instance ToByteString WafRuleType
instance ToQuery      WafRuleType
instance ToHeader     WafRuleType

instance ToJSON WafRuleType where
    toJSON = toJSONText

instance FromJSON WafRuleType where
    parseJSON = parseJSONText "WafRuleType"
