{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.Sum
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53Domains.Types.Sum where

import           Network.AWS.Prelude

data ContactType
    = Association
    | Company
    | Person
    | PublicBody
    | Reseller
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ContactType where
    parser = takeLowerText >>= \case
        "association" -> pure Association
        "company" -> pure Company
        "person" -> pure Person
        "public_body" -> pure PublicBody
        "reseller" -> pure Reseller
        e -> fromTextError $ "Failure parsing ContactType from value: '" <> e
           <> "'. Accepted values: association, company, person, public_body, reseller"

instance ToText ContactType where
    toText = \case
        Association -> "association"
        Company -> "company"
        Person -> "person"
        PublicBody -> "public_body"
        Reseller -> "reseller"

instance Hashable     ContactType
instance ToByteString ContactType
instance ToQuery      ContactType
instance ToHeader     ContactType

instance ToJSON ContactType where
    toJSON = toJSONText

instance FromJSON ContactType where
    parseJSON = parseJSONText "ContactType"

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
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText CountryCode where
    parser = takeLowerText >>= \case
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
        e -> fromTextError $ "Failure parsing CountryCode from value: '" <> e
           <> "'. Accepted values: ad, ae, af, ag, ai, al, am, an, ao, aq, ar, as, at, au, aw, az, ba, bb, bd, be, bf, bg, bh, bi, bj, bl, bm, bn, bo, br, bs, bt, bw, by, bz, ca, cc, cd, cf, cg, ch, ci, ck, cl, cm, cn, co, cr, cu, cv, cx, cy, cz, de, dj, dk, dm, do, dz, ec, ee, eg, er, es, et, fi, fj, fk, fm, fo, fr, ga, gb, gd, ge, gh, gi, gl, gm, gn, gq, gr, gt, gu, gw, gy, hk, hn, hr, ht, hu, ie, il, im, in, iq, ir, is, it, id, jm, jo, jp, ke, kg, kh, ki, km, kn, kp, kr, kw, ky, kz, la, lb, lc, li, lk, lr, ls, lt, lu, lv, ly, ma, mc, md, me, mf, mg, mh, mk, ml, mm, mn, mo, mp, mr, ms, mt, mu, mv, mw, mx, my, mz, na, nc, ne, ng, ni, nl, no, np, nr, nu, nz, om, pa, pe, pf, pg, ph, pk, pl, pm, pn, pr, pt, pw, py, qa, ro, rs, ru, rw, sa, sb, sc, sd, se, sg, sh, si, sk, sl, sm, sn, so, sr, st, sv, sy, sz, tc, td, tg, th, tj, tk, tl, tm, tn, to, tr, tt, tv, tw, tz, ua, ug, us, uy, uz, va, vc, ve, vg, vi, vn, vu, wf, ws, ye, yt, za, zm, zw"

instance ToText CountryCode where
    toText = \case
        AD -> "ad"
        AE -> "ae"
        AF -> "af"
        AG -> "ag"
        AI -> "ai"
        AL -> "al"
        AM -> "am"
        AN -> "an"
        AO -> "ao"
        AQ -> "aq"
        AR -> "ar"
        AS -> "as"
        AT -> "at"
        AU -> "au"
        AW -> "aw"
        AZ -> "az"
        BA -> "ba"
        BB -> "bb"
        BD -> "bd"
        BE -> "be"
        BF -> "bf"
        BG -> "bg"
        BH -> "bh"
        BI -> "bi"
        BJ -> "bj"
        BL -> "bl"
        BM -> "bm"
        BN -> "bn"
        BO -> "bo"
        BR -> "br"
        BS -> "bs"
        BT -> "bt"
        BW -> "bw"
        BY -> "by"
        BZ -> "bz"
        CA -> "ca"
        CC -> "cc"
        CD -> "cd"
        CF -> "cf"
        CG -> "cg"
        CH -> "ch"
        CI -> "ci"
        CK -> "ck"
        CL -> "cl"
        CM -> "cm"
        CN -> "cn"
        CO -> "co"
        CR -> "cr"
        CU -> "cu"
        CV -> "cv"
        CX -> "cx"
        CY -> "cy"
        CZ -> "cz"
        DE -> "de"
        DJ -> "dj"
        DK -> "dk"
        DM -> "dm"
        DO -> "do"
        DZ -> "dz"
        EC -> "ec"
        EE -> "ee"
        EG -> "eg"
        ER -> "er"
        ES -> "es"
        ET -> "et"
        FI -> "fi"
        FJ -> "fj"
        FK -> "fk"
        FM -> "fm"
        FO -> "fo"
        FR -> "fr"
        GA -> "ga"
        GB -> "gb"
        GD -> "gd"
        GE -> "ge"
        GH -> "gh"
        GI -> "gi"
        GL -> "gl"
        GM -> "gm"
        GN -> "gn"
        GQ -> "gq"
        GR -> "gr"
        GT' -> "gt"
        GU -> "gu"
        GW -> "gw"
        GY -> "gy"
        HK -> "hk"
        HN -> "hn"
        HR -> "hr"
        HT -> "ht"
        HU -> "hu"
        IE -> "ie"
        IL -> "il"
        IM -> "im"
        IN -> "in"
        IQ -> "iq"
        IR -> "ir"
        IS -> "is"
        IT -> "it"
        Id -> "id"
        JM -> "jm"
        JO -> "jo"
        JP -> "jp"
        KE -> "ke"
        KG -> "kg"
        KH -> "kh"
        KI -> "ki"
        KM -> "km"
        KN -> "kn"
        KP -> "kp"
        KR -> "kr"
        KW -> "kw"
        KY -> "ky"
        KZ -> "kz"
        LA -> "la"
        LB -> "lb"
        LC -> "lc"
        LI -> "li"
        LK -> "lk"
        LR -> "lr"
        LS -> "ls"
        LT' -> "lt"
        LU -> "lu"
        LV -> "lv"
        LY -> "ly"
        MA -> "ma"
        MC -> "mc"
        MD -> "md"
        ME -> "me"
        MF -> "mf"
        MG -> "mg"
        MH -> "mh"
        MK -> "mk"
        ML -> "ml"
        MM -> "mm"
        MN -> "mn"
        MO -> "mo"
        MP -> "mp"
        MR -> "mr"
        MS -> "ms"
        MT -> "mt"
        MU -> "mu"
        MV -> "mv"
        MW -> "mw"
        MX -> "mx"
        MY -> "my"
        MZ -> "mz"
        NA -> "na"
        NC -> "nc"
        NE -> "ne"
        NG -> "ng"
        NI -> "ni"
        NL -> "nl"
        NO -> "no"
        NP -> "np"
        NR -> "nr"
        NU -> "nu"
        NZ -> "nz"
        OM -> "om"
        PA -> "pa"
        PE -> "pe"
        PF -> "pf"
        PG -> "pg"
        PH -> "ph"
        PK -> "pk"
        PL -> "pl"
        PM -> "pm"
        PN -> "pn"
        PR -> "pr"
        PT -> "pt"
        PW -> "pw"
        PY -> "py"
        QA -> "qa"
        RO -> "ro"
        RS -> "rs"
        RU -> "ru"
        RW -> "rw"
        SA -> "sa"
        SB -> "sb"
        SC -> "sc"
        SD -> "sd"
        SE -> "se"
        SG -> "sg"
        SH -> "sh"
        SI -> "si"
        SK -> "sk"
        SL -> "sl"
        SM -> "sm"
        SN -> "sn"
        SO -> "so"
        SR -> "sr"
        ST -> "st"
        SV -> "sv"
        SY -> "sy"
        SZ -> "sz"
        TC -> "tc"
        TD -> "td"
        TG -> "tg"
        TH -> "th"
        TJ -> "tj"
        TK -> "tk"
        TL -> "tl"
        TM -> "tm"
        TN -> "tn"
        TO -> "to"
        TR -> "tr"
        TT -> "tt"
        TV -> "tv"
        TW -> "tw"
        TZ -> "tz"
        UA -> "ua"
        UG -> "ug"
        US -> "us"
        UY -> "uy"
        UZ -> "uz"
        VA -> "va"
        VC -> "vc"
        VE -> "ve"
        VG -> "vg"
        VI -> "vi"
        VN -> "vn"
        VU -> "vu"
        WF -> "wf"
        WS -> "ws"
        YE -> "ye"
        YT -> "yt"
        ZA -> "za"
        ZM -> "zm"
        ZW -> "zw"

instance Hashable     CountryCode
instance ToByteString CountryCode
instance ToQuery      CountryCode
instance ToHeader     CountryCode

instance ToJSON CountryCode where
    toJSON = toJSONText

instance FromJSON CountryCode where
    parseJSON = parseJSONText "CountryCode"

data DomainAvailability
    = Available
    | AvailablePreorder
    | AvailableReserved
    | DontKnow
    | Reserved
    | Unavailable
    | UnavailablePremium
    | UnavailableRestricted
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText DomainAvailability where
    parser = takeLowerText >>= \case
        "available" -> pure Available
        "available_preorder" -> pure AvailablePreorder
        "available_reserved" -> pure AvailableReserved
        "dont_know" -> pure DontKnow
        "reserved" -> pure Reserved
        "unavailable" -> pure Unavailable
        "unavailable_premium" -> pure UnavailablePremium
        "unavailable_restricted" -> pure UnavailableRestricted
        e -> fromTextError $ "Failure parsing DomainAvailability from value: '" <> e
           <> "'. Accepted values: available, available_preorder, available_reserved, dont_know, reserved, unavailable, unavailable_premium, unavailable_restricted"

instance ToText DomainAvailability where
    toText = \case
        Available -> "available"
        AvailablePreorder -> "available_preorder"
        AvailableReserved -> "available_reserved"
        DontKnow -> "dont_know"
        Reserved -> "reserved"
        Unavailable -> "unavailable"
        UnavailablePremium -> "unavailable_premium"
        UnavailableRestricted -> "unavailable_restricted"

instance Hashable     DomainAvailability
instance ToByteString DomainAvailability
instance ToQuery      DomainAvailability
instance ToHeader     DomainAvailability

instance FromJSON DomainAvailability where
    parseJSON = parseJSONText "DomainAvailability"

data ExtraParamName
    = AuIdNumber
    | AuIdType
    | BirthCity
    | BirthCountry
    | BirthDateInYyyyMmDd
    | BirthDepartment
    | BrandNumber
    | CaLegalType
    | DocumentNumber
    | DunsNumber
    | EsIdentification
    | EsIdentificationType
    | EsLegalForm
    | FiBusinessNumber
    | FiIdNumber
    | ItPin
    | RuPassportData
    | SeIdNumber
    | SgIdNumber
    | VatNumber
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText ExtraParamName where
    parser = takeLowerText >>= \case
        "au_id_number" -> pure AuIdNumber
        "au_id_type" -> pure AuIdType
        "birth_city" -> pure BirthCity
        "birth_country" -> pure BirthCountry
        "birth_date_in_yyyy_mm_dd" -> pure BirthDateInYyyyMmDd
        "birth_department" -> pure BirthDepartment
        "brand_number" -> pure BrandNumber
        "ca_legal_type" -> pure CaLegalType
        "document_number" -> pure DocumentNumber
        "duns_number" -> pure DunsNumber
        "es_identification" -> pure EsIdentification
        "es_identification_type" -> pure EsIdentificationType
        "es_legal_form" -> pure EsLegalForm
        "fi_business_number" -> pure FiBusinessNumber
        "fi_id_number" -> pure FiIdNumber
        "it_pin" -> pure ItPin
        "ru_passport_data" -> pure RuPassportData
        "se_id_number" -> pure SeIdNumber
        "sg_id_number" -> pure SgIdNumber
        "vat_number" -> pure VatNumber
        e -> fromTextError $ "Failure parsing ExtraParamName from value: '" <> e
           <> "'. Accepted values: au_id_number, au_id_type, birth_city, birth_country, birth_date_in_yyyy_mm_dd, birth_department, brand_number, ca_legal_type, document_number, duns_number, es_identification, es_identification_type, es_legal_form, fi_business_number, fi_id_number, it_pin, ru_passport_data, se_id_number, sg_id_number, vat_number"

instance ToText ExtraParamName where
    toText = \case
        AuIdNumber -> "au_id_number"
        AuIdType -> "au_id_type"
        BirthCity -> "birth_city"
        BirthCountry -> "birth_country"
        BirthDateInYyyyMmDd -> "birth_date_in_yyyy_mm_dd"
        BirthDepartment -> "birth_department"
        BrandNumber -> "brand_number"
        CaLegalType -> "ca_legal_type"
        DocumentNumber -> "document_number"
        DunsNumber -> "duns_number"
        EsIdentification -> "es_identification"
        EsIdentificationType -> "es_identification_type"
        EsLegalForm -> "es_legal_form"
        FiBusinessNumber -> "fi_business_number"
        FiIdNumber -> "fi_id_number"
        ItPin -> "it_pin"
        RuPassportData -> "ru_passport_data"
        SeIdNumber -> "se_id_number"
        SgIdNumber -> "sg_id_number"
        VatNumber -> "vat_number"

instance Hashable     ExtraParamName
instance ToByteString ExtraParamName
instance ToQuery      ExtraParamName
instance ToHeader     ExtraParamName

instance ToJSON ExtraParamName where
    toJSON = toJSONText

instance FromJSON ExtraParamName where
    parseJSON = parseJSONText "ExtraParamName"

data OperationStatus
    = Error'
    | Failed
    | InProgress
    | Submitted
    | Successful
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText OperationStatus where
    parser = takeLowerText >>= \case
        "error" -> pure Error'
        "failed" -> pure Failed
        "in_progress" -> pure InProgress
        "submitted" -> pure Submitted
        "successful" -> pure Successful
        e -> fromTextError $ "Failure parsing OperationStatus from value: '" <> e
           <> "'. Accepted values: error, failed, in_progress, submitted, successful"

instance ToText OperationStatus where
    toText = \case
        Error' -> "error"
        Failed -> "failed"
        InProgress -> "in_progress"
        Submitted -> "submitted"
        Successful -> "successful"

instance Hashable     OperationStatus
instance ToByteString OperationStatus
instance ToQuery      OperationStatus
instance ToHeader     OperationStatus

instance FromJSON OperationStatus where
    parseJSON = parseJSONText "OperationStatus"

data OperationType
    = ChangePrivacyProtection
    | DeleteDomain
    | DomainLock
    | RegisterDomain
    | TransferInDomain
    | UpdateDomainContact
    | UpdateNameserver
    deriving (Eq,Ord,Read,Show,Enum,Data,Typeable,Generic)

instance FromText OperationType where
    parser = takeLowerText >>= \case
        "change_privacy_protection" -> pure ChangePrivacyProtection
        "delete_domain" -> pure DeleteDomain
        "domain_lock" -> pure DomainLock
        "register_domain" -> pure RegisterDomain
        "transfer_in_domain" -> pure TransferInDomain
        "update_domain_contact" -> pure UpdateDomainContact
        "update_nameserver" -> pure UpdateNameserver
        e -> fromTextError $ "Failure parsing OperationType from value: '" <> e
           <> "'. Accepted values: change_privacy_protection, delete_domain, domain_lock, register_domain, transfer_in_domain, update_domain_contact, update_nameserver"

instance ToText OperationType where
    toText = \case
        ChangePrivacyProtection -> "change_privacy_protection"
        DeleteDomain -> "delete_domain"
        DomainLock -> "domain_lock"
        RegisterDomain -> "register_domain"
        TransferInDomain -> "transfer_in_domain"
        UpdateDomainContact -> "update_domain_contact"
        UpdateNameserver -> "update_nameserver"

instance Hashable     OperationType
instance ToByteString OperationType
instance ToQuery      OperationType
instance ToHeader     OperationType

instance FromJSON OperationType where
    parseJSON = parseJSONText "OperationType"
