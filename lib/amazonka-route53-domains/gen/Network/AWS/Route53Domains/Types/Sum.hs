{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53Domains.Types.Sum where

import Network.AWS.Prelude

data ContactType
  = Association
  | Company
  | Person
  | PublicBody
  | Reseller
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
        Association -> "ASSOCIATION"
        Company -> "COMPANY"
        Person -> "PERSON"
        PublicBody -> "PUBLIC_BODY"
        Reseller -> "RESELLER"

instance Hashable     ContactType
instance NFData       ContactType
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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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

instance Hashable     CountryCode
instance NFData       CountryCode
instance ToByteString CountryCode
instance ToQuery      CountryCode
instance ToHeader     CountryCode

instance ToJSON CountryCode where
    toJSON = toJSONText

instance FromJSON CountryCode where
    parseJSON = parseJSONText "CountryCode"

data DomainAvailability
  = DAAvailable
  | DAAvailablePreorder
  | DAAvailableReserved
  | DADontKnow
  | DAReserved
  | DAUnavailable
  | DAUnavailablePremium
  | DAUnavailableRestricted
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DomainAvailability where
    parser = takeLowerText >>= \case
        "available" -> pure DAAvailable
        "available_preorder" -> pure DAAvailablePreorder
        "available_reserved" -> pure DAAvailableReserved
        "dont_know" -> pure DADontKnow
        "reserved" -> pure DAReserved
        "unavailable" -> pure DAUnavailable
        "unavailable_premium" -> pure DAUnavailablePremium
        "unavailable_restricted" -> pure DAUnavailableRestricted
        e -> fromTextError $ "Failure parsing DomainAvailability from value: '" <> e
           <> "'. Accepted values: available, available_preorder, available_reserved, dont_know, reserved, unavailable, unavailable_premium, unavailable_restricted"

instance ToText DomainAvailability where
    toText = \case
        DAAvailable -> "AVAILABLE"
        DAAvailablePreorder -> "AVAILABLE_PREORDER"
        DAAvailableReserved -> "AVAILABLE_RESERVED"
        DADontKnow -> "DONT_KNOW"
        DAReserved -> "RESERVED"
        DAUnavailable -> "UNAVAILABLE"
        DAUnavailablePremium -> "UNAVAILABLE_PREMIUM"
        DAUnavailableRestricted -> "UNAVAILABLE_RESTRICTED"

instance Hashable     DomainAvailability
instance NFData       DomainAvailability
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
  | CaBusinessEntityType
  | CaLegalType
  | DocumentNumber
  | DunsNumber
  | EsIdentification
  | EsIdentificationType
  | EsLegalForm
  | FiBusinessNumber
  | FiIdNumber
  | FiNationality
  | FiOrganizationType
  | ItPin
  | ItRegistrantEntityType
  | RuPassportData
  | SeIdNumber
  | SgIdNumber
  | UkCompanyNumber
  | UkContactType
  | VatNumber
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ExtraParamName where
    parser = takeLowerText >>= \case
        "au_id_number" -> pure AuIdNumber
        "au_id_type" -> pure AuIdType
        "birth_city" -> pure BirthCity
        "birth_country" -> pure BirthCountry
        "birth_date_in_yyyy_mm_dd" -> pure BirthDateInYyyyMmDd
        "birth_department" -> pure BirthDepartment
        "brand_number" -> pure BrandNumber
        "ca_business_entity_type" -> pure CaBusinessEntityType
        "ca_legal_type" -> pure CaLegalType
        "document_number" -> pure DocumentNumber
        "duns_number" -> pure DunsNumber
        "es_identification" -> pure EsIdentification
        "es_identification_type" -> pure EsIdentificationType
        "es_legal_form" -> pure EsLegalForm
        "fi_business_number" -> pure FiBusinessNumber
        "fi_id_number" -> pure FiIdNumber
        "fi_nationality" -> pure FiNationality
        "fi_organization_type" -> pure FiOrganizationType
        "it_pin" -> pure ItPin
        "it_registrant_entity_type" -> pure ItRegistrantEntityType
        "ru_passport_data" -> pure RuPassportData
        "se_id_number" -> pure SeIdNumber
        "sg_id_number" -> pure SgIdNumber
        "uk_company_number" -> pure UkCompanyNumber
        "uk_contact_type" -> pure UkContactType
        "vat_number" -> pure VatNumber
        e -> fromTextError $ "Failure parsing ExtraParamName from value: '" <> e
           <> "'. Accepted values: au_id_number, au_id_type, birth_city, birth_country, birth_date_in_yyyy_mm_dd, birth_department, brand_number, ca_business_entity_type, ca_legal_type, document_number, duns_number, es_identification, es_identification_type, es_legal_form, fi_business_number, fi_id_number, fi_nationality, fi_organization_type, it_pin, it_registrant_entity_type, ru_passport_data, se_id_number, sg_id_number, uk_company_number, uk_contact_type, vat_number"

instance ToText ExtraParamName where
    toText = \case
        AuIdNumber -> "AU_ID_NUMBER"
        AuIdType -> "AU_ID_TYPE"
        BirthCity -> "BIRTH_CITY"
        BirthCountry -> "BIRTH_COUNTRY"
        BirthDateInYyyyMmDd -> "BIRTH_DATE_IN_YYYY_MM_DD"
        BirthDepartment -> "BIRTH_DEPARTMENT"
        BrandNumber -> "BRAND_NUMBER"
        CaBusinessEntityType -> "CA_BUSINESS_ENTITY_TYPE"
        CaLegalType -> "CA_LEGAL_TYPE"
        DocumentNumber -> "DOCUMENT_NUMBER"
        DunsNumber -> "DUNS_NUMBER"
        EsIdentification -> "ES_IDENTIFICATION"
        EsIdentificationType -> "ES_IDENTIFICATION_TYPE"
        EsLegalForm -> "ES_LEGAL_FORM"
        FiBusinessNumber -> "FI_BUSINESS_NUMBER"
        FiIdNumber -> "FI_ID_NUMBER"
        FiNationality -> "FI_NATIONALITY"
        FiOrganizationType -> "FI_ORGANIZATION_TYPE"
        ItPin -> "IT_PIN"
        ItRegistrantEntityType -> "IT_REGISTRANT_ENTITY_TYPE"
        RuPassportData -> "RU_PASSPORT_DATA"
        SeIdNumber -> "SE_ID_NUMBER"
        SgIdNumber -> "SG_ID_NUMBER"
        UkCompanyNumber -> "UK_COMPANY_NUMBER"
        UkContactType -> "UK_CONTACT_TYPE"
        VatNumber -> "VAT_NUMBER"

instance Hashable     ExtraParamName
instance NFData       ExtraParamName
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
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


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
        Error' -> "ERROR"
        Failed -> "FAILED"
        InProgress -> "IN_PROGRESS"
        Submitted -> "SUBMITTED"
        Successful -> "SUCCESSFUL"

instance Hashable     OperationStatus
instance NFData       OperationStatus
instance ToByteString OperationStatus
instance ToQuery      OperationStatus
instance ToHeader     OperationStatus

instance FromJSON OperationStatus where
    parseJSON = parseJSONText "OperationStatus"

data OperationType
  = AddDNSsec
  | ChangeDomainOwner
  | ChangePrivacyProtection
  | DeleteDomain
  | DisableAutorenew
  | DomainLock
  | EnableAutorenew
  | ExpireDomain
  | PushDomain
  | RegisterDomain
  | RemoveDNSsec
  | RenewDomain
  | TransferInDomain
  | TransferOutDomain
  | UpdateDomainContact
  | UpdateNameserver
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText OperationType where
    parser = takeLowerText >>= \case
        "add_dnssec" -> pure AddDNSsec
        "change_domain_owner" -> pure ChangeDomainOwner
        "change_privacy_protection" -> pure ChangePrivacyProtection
        "delete_domain" -> pure DeleteDomain
        "disable_autorenew" -> pure DisableAutorenew
        "domain_lock" -> pure DomainLock
        "enable_autorenew" -> pure EnableAutorenew
        "expire_domain" -> pure ExpireDomain
        "push_domain" -> pure PushDomain
        "register_domain" -> pure RegisterDomain
        "remove_dnssec" -> pure RemoveDNSsec
        "renew_domain" -> pure RenewDomain
        "transfer_in_domain" -> pure TransferInDomain
        "transfer_out_domain" -> pure TransferOutDomain
        "update_domain_contact" -> pure UpdateDomainContact
        "update_nameserver" -> pure UpdateNameserver
        e -> fromTextError $ "Failure parsing OperationType from value: '" <> e
           <> "'. Accepted values: add_dnssec, change_domain_owner, change_privacy_protection, delete_domain, disable_autorenew, domain_lock, enable_autorenew, expire_domain, push_domain, register_domain, remove_dnssec, renew_domain, transfer_in_domain, transfer_out_domain, update_domain_contact, update_nameserver"

instance ToText OperationType where
    toText = \case
        AddDNSsec -> "ADD_DNSSEC"
        ChangeDomainOwner -> "CHANGE_DOMAIN_OWNER"
        ChangePrivacyProtection -> "CHANGE_PRIVACY_PROTECTION"
        DeleteDomain -> "DELETE_DOMAIN"
        DisableAutorenew -> "DISABLE_AUTORENEW"
        DomainLock -> "DOMAIN_LOCK"
        EnableAutorenew -> "ENABLE_AUTORENEW"
        ExpireDomain -> "EXPIRE_DOMAIN"
        PushDomain -> "PUSH_DOMAIN"
        RegisterDomain -> "REGISTER_DOMAIN"
        RemoveDNSsec -> "REMOVE_DNSSEC"
        RenewDomain -> "RENEW_DOMAIN"
        TransferInDomain -> "TRANSFER_IN_DOMAIN"
        TransferOutDomain -> "TRANSFER_OUT_DOMAIN"
        UpdateDomainContact -> "UPDATE_DOMAIN_CONTACT"
        UpdateNameserver -> "UPDATE_NAMESERVER"

instance Hashable     OperationType
instance NFData       OperationType
instance ToByteString OperationType
instance ToQuery      OperationType
instance ToHeader     OperationType

instance FromJSON OperationType where
    parseJSON = parseJSONText "OperationType"

data ReachabilityStatus
  = Done
  | Expired
  | Pending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ReachabilityStatus where
    parser = takeLowerText >>= \case
        "done" -> pure Done
        "expired" -> pure Expired
        "pending" -> pure Pending
        e -> fromTextError $ "Failure parsing ReachabilityStatus from value: '" <> e
           <> "'. Accepted values: done, expired, pending"

instance ToText ReachabilityStatus where
    toText = \case
        Done -> "DONE"
        Expired -> "EXPIRED"
        Pending -> "PENDING"

instance Hashable     ReachabilityStatus
instance NFData       ReachabilityStatus
instance ToByteString ReachabilityStatus
instance ToQuery      ReachabilityStatus
instance ToHeader     ReachabilityStatus

instance FromJSON ReachabilityStatus where
    parseJSON = parseJSONText "ReachabilityStatus"

-- | Whether the domain name can be transferred to Amazon Route 53.
--
--
-- Valid values:
--
--     * TRANSFERABLE    * The domain name can be transferred to Amazon Route 53.
--
--     * UNTRANSFERRABLE    * The domain name can't be transferred to Amazon Route 53.
--
--     * DONT_KNOW    * Reserved for future use.
--
--
--
data Transferable
  = DontKnow
  | Transferable
  | Untransferable
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Transferable where
    parser = takeLowerText >>= \case
        "dont_know" -> pure DontKnow
        "transferable" -> pure Transferable
        "untransferable" -> pure Untransferable
        e -> fromTextError $ "Failure parsing Transferable from value: '" <> e
           <> "'. Accepted values: dont_know, transferable, untransferable"

instance ToText Transferable where
    toText = \case
        DontKnow -> "DONT_KNOW"
        Transferable -> "TRANSFERABLE"
        Untransferable -> "UNTRANSFERABLE"

instance Hashable     Transferable
instance NFData       Transferable
instance ToByteString Transferable
instance ToQuery      Transferable
instance ToHeader     Transferable

instance FromJSON Transferable where
    parseJSON = parseJSONText "Transferable"
