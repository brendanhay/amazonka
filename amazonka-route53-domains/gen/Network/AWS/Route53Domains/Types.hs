{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Route53Domains.Types
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Network.AWS.Route53Domains.Types
    (
    -- * Service
      Route53Domains

    -- * Errors
    , _InvalidInput
    , _OperationLimitExceeded
    , _DomainLimitExceeded
    , _UnsupportedTLD
    , _TLDRulesViolation
    , _DuplicateRequest

    -- * ContactType
    , ContactType (..)

    -- * CountryCode
    , CountryCode (..)

    -- * DomainAvailability
    , DomainAvailability (..)

    -- * ExtraParamName
    , ExtraParamName (..)

    -- * OperationStatus
    , OperationStatus (..)

    -- * OperationType
    , OperationType (..)

    -- * ContactDetail
    , ContactDetail
    , contactDetail
    , cdOrganizationName
    , cdEmail
    , cdFax
    , cdState
    , cdLastName
    , cdExtraParams
    , cdZipCode
    , cdAddressLine1
    , cdCity
    , cdPhoneNumber
    , cdAddressLine2
    , cdFirstName
    , cdCountryCode
    , cdContactType

    -- * DomainSummary
    , DomainSummary
    , domainSummary
    , dsExpiry
    , dsTransferLock
    , dsAutoRenew
    , dsDomainName

    -- * ExtraParam
    , ExtraParam
    , extraParam
    , epName
    , epValue

    -- * Nameserver
    , Nameserver
    , nameserver
    , namGlueIPs
    , namName

    -- * OperationSummary
    , OperationSummary
    , operationSummary
    , osOperationId
    , osStatus
    , osType
    , osSubmittedDate

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Sign.V4

-- | Version @2014-05-15@ of the Amazon Route 53 Domains SDK.
data Route53Domains

instance AWSService Route53Domains where
    type Sg Route53Domains = V4
    service = const svc
      where
        svc =
            Service
            { _svcAbbrev = "Route53Domains"
            , _svcPrefix = "route53domains"
            , _svcVersion = "2014-05-15"
            , _svcEndpoint = defaultEndpoint svc
            , _svcTimeout = 80000000
            , _svcStatus = statusSuccess
            , _svcError = parseJSONError
            , _svcRetry = retry
            }
        retry =
            Exponential
            { _retryBase = 5.0e-2
            , _retryGrowth = 2
            , _retryAttempts = 5
            , _retryCheck = check
            }
        check e
          | has (hasCode "ThrottlingException" . hasStatus 400) e =
              Just "throttling_exception"
          | has (hasCode "Throttling" . hasStatus 400) e = Just "throttling"
          | has (hasStatus 503) e = Just "service_unavailable"
          | has (hasStatus 500) e = Just "general_server_error"
          | has (hasStatus 509) e = Just "limit_exceeded"
          | otherwise = Nothing

-- | The requested item is not acceptable. For example, for an OperationId it
-- may refer to the ID of an operation that is already completed. For a
-- domain name, it may not be a valid domain name or belong to the
-- requester account.
_InvalidInput :: AWSError a => Getting (First ServiceError) a ServiceError
_InvalidInput = _ServiceError . hasStatus 400 . hasCode "InvalidInput"

-- | The number of operations or jobs running exceeded the allowed threshold
-- for the account.
_OperationLimitExceeded :: AWSError a => Getting (First ServiceError) a ServiceError
_OperationLimitExceeded =
    _ServiceError . hasStatus 400 . hasCode "OperationLimitExceeded"

-- | The number of domains has exceeded the allowed threshold for the
-- account.
_DomainLimitExceeded :: AWSError a => Getting (First ServiceError) a ServiceError
_DomainLimitExceeded =
    _ServiceError . hasStatus 400 . hasCode "DomainLimitExceeded"

-- | Amazon Route 53 does not support this top-level domain.
_UnsupportedTLD :: AWSError a => Getting (First ServiceError) a ServiceError
_UnsupportedTLD = _ServiceError . hasStatus 400 . hasCode "UnsupportedTLD"

-- | The top-level domain does not support this operation.
_TLDRulesViolation :: AWSError a => Getting (First ServiceError) a ServiceError
_TLDRulesViolation =
    _ServiceError . hasStatus 400 . hasCode "TLDRulesViolation"

-- | The request is already in progress for the domain.
_DuplicateRequest :: AWSError a => Getting (First ServiceError) a ServiceError
_DuplicateRequest = _ServiceError . hasStatus 400 . hasCode "DuplicateRequest"

data ContactType
    = Person
    | Company
    | Reseller
    | Association
    | PublicBody
    deriving (Eq,Ord,Read,Show,Enum,Generic)

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

instance Hashable ContactType
instance ToQuery ContactType
instance ToHeader ContactType

instance ToJSON ContactType where
    toJSON = toJSONText

instance FromJSON ContactType where
    parseJSON = parseJSONText "ContactType"

data CountryCode
    = NR
    | CG
    | TH
    | WS
    | IM
    | FJ
    | AU
    | UA
    | PL
    | MY
    | AE
    | SG
    | HT
    | BN
    | PG
    | JM
    | MR
    | VA
    | UZ
    | AN
    | RU
    | GH
    | BE
    | SL
    | CL
    | TC
    | NI
    | ZM
    | LK
    | PW
    | LV
    | UG
    | AS
    | GE
    | HR
    | BH
    | SA
    | EG
    | GU
    | KY
    | CA
    | TN
    | KI
    | OM
    | JP
    | MO
    | CV
    | MH
    | FK
    | ZW
    | AT
    | RO
    | PM
    | MX
    | SV
    | AD
    | GB
    | BO
    | HU
    | GR
    | CF
    | KN
    | NC
    | IL
    | LA
    | TT
    | CK
    | TD
    | ME
    | IQ
    | MU
    | BR
    | AI
    | BB
    | SK
    | NU
    | KH
    | TO
    | NE
    | ZA
    | MN
    | FM
    | AR
    | DO
    | PK
    | BY
    | GD
    | BI
    | GA
    | BL
    | SE
    | EC
    | GQ
    | TZ
    | NP
    | KM
    | TJ
    | LB
    | CU
    | MK
    | LR
    | AW
    | DJ
    | PN
    | ES
    | DZ
    | US
    | AG
    | DE
    | MT
    | PA
    | VG
    | BS
    | SZ
    | RS
    | GN
    | KR
    | NO
    | CZ
    | MD
    | PT
    | MA
    | AM
    | UY
    | BF
    | SO
    | YE
    | KW
    | NZ
    | CO
    | KG
    | FR
    | IE
    | GT'
    | TK
    | NA
    | LT'
    | LC
    | IN
    | FI
    | LS
    | DK
    | MZ
    | VI
    | ST
    | ER
    | AF
    | BM
    | SD
    | CD
    | TV
    | CI
    | NL
    | CY
    | PR
    | MG
    | IS
    | MW
    | SY
    | GM
    | SI
    | YT
    | SN
    | BG
    | CN
    | ID
    | LI
    | IT
    | LY
    | PE
    | JO
    | MP
    | BW
    | VC
    | AL
    | RW
    | VN
    | BZ
    | BJ
    | SC
    | EE
    | QA
    | GW
    | CC
    | TL
    | MM
    | AQ
    | PH
    | CX
    | MF
    | IR
    | AZ
    | MV
    | VE
    | HK
    | GL
    | BA
    | SH
    | VU
    | KP
    | TW
    | CH
    | TG
    | KE
    | MC
    | PF
    | MS
    | BT
    | HN
    | AO
    | GI
    | SM
    | BD
    | GY
    | TR
    | CM
    | TM
    | NG
    | CR
    | PY
    | ML
    | FO
    | WF
    | LU
    | DM
    | SR
    | ET
    | SB
    | KZ
    deriving (Eq,Ord,Read,Show,Enum,Generic)

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
        "id" -> pure ID
        "ie" -> pure IE
        "il" -> pure IL
        "im" -> pure IM
        "in" -> pure IN
        "iq" -> pure IQ
        "ir" -> pure IR
        "is" -> pure IS
        "it" -> pure IT
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
           <> "'. Accepted values: ad, ae, af, ag, ai, al, am, an, ao, aq, ar, as, at, au, aw, az, ba, bb, bd, be, bf, bg, bh, bi, bj, bl, bm, bn, bo, br, bs, bt, bw, by, bz, ca, cc, cd, cf, cg, ch, ci, ck, cl, cm, cn, co, cr, cu, cv, cx, cy, cz, de, dj, dk, dm, do, dz, ec, ee, eg, er, es, et, fi, fj, fk, fm, fo, fr, ga, gb, gd, ge, gh, gi, gl, gm, gn, gq, gr, gt, gu, gw, gy, hk, hn, hr, ht, hu, id, ie, il, im, in, iq, ir, is, it, jm, jo, jp, ke, kg, kh, ki, km, kn, kp, kr, kw, ky, kz, la, lb, lc, li, lk, lr, ls, lt, lu, lv, ly, ma, mc, md, me, mf, mg, mh, mk, ml, mm, mn, mo, mp, mr, ms, mt, mu, mv, mw, mx, my, mz, na, nc, ne, ng, ni, nl, no, np, nr, nu, nz, om, pa, pe, pf, pg, ph, pk, pl, pm, pn, pr, pt, pw, py, qa, ro, rs, ru, rw, sa, sb, sc, sd, se, sg, sh, si, sk, sl, sm, sn, so, sr, st, sv, sy, sz, tc, td, tg, th, tj, tk, tl, tm, tn, to, tr, tt, tv, tw, tz, ua, ug, us, uy, uz, va, vc, ve, vg, vi, vn, vu, wf, ws, ye, yt, za, zm, zw"

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
        ID -> "id"
        IE -> "ie"
        IL -> "il"
        IM -> "im"
        IN -> "in"
        IQ -> "iq"
        IR -> "ir"
        IS -> "is"
        IT -> "it"
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

instance Hashable CountryCode
instance ToQuery CountryCode
instance ToHeader CountryCode

instance ToJSON CountryCode where
    toJSON = toJSONText

instance FromJSON CountryCode where
    parseJSON = parseJSONText "CountryCode"

data DomainAvailability
    = DontKnow
    | UnavailableRestricted
    | AvailableReserved
    | AvailablePreorder
    | Reserved
    | Unavailable
    | UnavailablePremium
    | Available
    deriving (Eq,Ord,Read,Show,Enum,Generic)

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

instance Hashable DomainAvailability
instance ToQuery DomainAvailability
instance ToHeader DomainAvailability

instance FromJSON DomainAvailability where
    parseJSON = parseJSONText "DomainAvailability"

data ExtraParamName
    = DocumentNumber
    | ESIdentificationType
    | RUPassportData
    | SGIDNumber
    | FIBusinessNumber
    | AUIDNumber
    | ESLegalForm
    | BirthDateINYyyyMMDD
    | CALegalType
    | AUIDType
    | BirthDepartment
    | ESIdentification
    | DunsNumber
    | BirthCity
    | ITPin
    | BirthCountry
    | VatNumber
    | BrandNumber
    | SEIDNumber
    | FIIDNumber
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText ExtraParamName where
    parser = takeLowerText >>= \case
        "au_id_number" -> pure AUIDNumber
        "au_id_type" -> pure AUIDType
        "birth_city" -> pure BirthCity
        "birth_country" -> pure BirthCountry
        "birth_date_in_yyyy_mm_dd" -> pure BirthDateINYyyyMMDD
        "birth_department" -> pure BirthDepartment
        "brand_number" -> pure BrandNumber
        "ca_legal_type" -> pure CALegalType
        "document_number" -> pure DocumentNumber
        "duns_number" -> pure DunsNumber
        "es_identification" -> pure ESIdentification
        "es_identification_type" -> pure ESIdentificationType
        "es_legal_form" -> pure ESLegalForm
        "fi_business_number" -> pure FIBusinessNumber
        "fi_id_number" -> pure FIIDNumber
        "it_pin" -> pure ITPin
        "ru_passport_data" -> pure RUPassportData
        "se_id_number" -> pure SEIDNumber
        "sg_id_number" -> pure SGIDNumber
        "vat_number" -> pure VatNumber
        e -> fromTextError $ "Failure parsing ExtraParamName from value: '" <> e
           <> "'. Accepted values: au_id_number, au_id_type, birth_city, birth_country, birth_date_in_yyyy_mm_dd, birth_department, brand_number, ca_legal_type, document_number, duns_number, es_identification, es_identification_type, es_legal_form, fi_business_number, fi_id_number, it_pin, ru_passport_data, se_id_number, sg_id_number, vat_number"

instance ToText ExtraParamName where
    toText = \case
        AUIDNumber -> "au_id_number"
        AUIDType -> "au_id_type"
        BirthCity -> "birth_city"
        BirthCountry -> "birth_country"
        BirthDateINYyyyMMDD -> "birth_date_in_yyyy_mm_dd"
        BirthDepartment -> "birth_department"
        BrandNumber -> "brand_number"
        CALegalType -> "ca_legal_type"
        DocumentNumber -> "document_number"
        DunsNumber -> "duns_number"
        ESIdentification -> "es_identification"
        ESIdentificationType -> "es_identification_type"
        ESLegalForm -> "es_legal_form"
        FIBusinessNumber -> "fi_business_number"
        FIIDNumber -> "fi_id_number"
        ITPin -> "it_pin"
        RUPassportData -> "ru_passport_data"
        SEIDNumber -> "se_id_number"
        SGIDNumber -> "sg_id_number"
        VatNumber -> "vat_number"

instance Hashable ExtraParamName
instance ToQuery ExtraParamName
instance ToHeader ExtraParamName

instance ToJSON ExtraParamName where
    toJSON = toJSONText

instance FromJSON ExtraParamName where
    parseJSON = parseJSONText "ExtraParamName"

data OperationStatus
    = Error'
    | Successful
    | INProgress
    | Failed
    | Submitted
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText OperationStatus where
    parser = takeLowerText >>= \case
        "error" -> pure Error'
        "failed" -> pure Failed
        "in_progress" -> pure INProgress
        "submitted" -> pure Submitted
        "successful" -> pure Successful
        e -> fromTextError $ "Failure parsing OperationStatus from value: '" <> e
           <> "'. Accepted values: error, failed, in_progress, submitted, successful"

instance ToText OperationStatus where
    toText = \case
        Error' -> "error"
        Failed -> "failed"
        INProgress -> "in_progress"
        Submitted -> "submitted"
        Successful -> "successful"

instance Hashable OperationStatus
instance ToQuery OperationStatus
instance ToHeader OperationStatus

instance FromJSON OperationStatus where
    parseJSON = parseJSONText "OperationStatus"

data OperationType
    = TransferINDomain
    | ChangePrivacyProtection
    | UpdateDomainContact
    | RegisterDomain
    | UpdateNameserver
    | DomainLock
    | DeleteDomain
    deriving (Eq,Ord,Read,Show,Enum,Generic)

instance FromText OperationType where
    parser = takeLowerText >>= \case
        "change_privacy_protection" -> pure ChangePrivacyProtection
        "delete_domain" -> pure DeleteDomain
        "domain_lock" -> pure DomainLock
        "register_domain" -> pure RegisterDomain
        "transfer_in_domain" -> pure TransferINDomain
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
        TransferINDomain -> "transfer_in_domain"
        UpdateDomainContact -> "update_domain_contact"
        UpdateNameserver -> "update_nameserver"

instance Hashable OperationType
instance ToQuery OperationType
instance ToHeader OperationType

instance FromJSON OperationType where
    parseJSON = parseJSONText "OperationType"

-- | ContactDetail includes the following elements.
--
-- /See:/ 'contactDetail' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdOrganizationName'
--
-- * 'cdEmail'
--
-- * 'cdFax'
--
-- * 'cdState'
--
-- * 'cdLastName'
--
-- * 'cdExtraParams'
--
-- * 'cdZipCode'
--
-- * 'cdAddressLine1'
--
-- * 'cdCity'
--
-- * 'cdPhoneNumber'
--
-- * 'cdAddressLine2'
--
-- * 'cdFirstName'
--
-- * 'cdCountryCode'
--
-- * 'cdContactType'
data ContactDetail = ContactDetail'
    { _cdOrganizationName :: !(Maybe Text)
    , _cdEmail            :: !(Maybe Text)
    , _cdFax              :: !(Maybe Text)
    , _cdState            :: !(Maybe Text)
    , _cdLastName         :: !(Maybe Text)
    , _cdExtraParams      :: !(Maybe [ExtraParam])
    , _cdZipCode          :: !(Maybe Text)
    , _cdAddressLine1     :: !(Maybe Text)
    , _cdCity             :: !(Maybe Text)
    , _cdPhoneNumber      :: !(Maybe Text)
    , _cdAddressLine2     :: !(Maybe Text)
    , _cdFirstName        :: !(Maybe Text)
    , _cdCountryCode      :: !(Maybe CountryCode)
    , _cdContactType      :: !(Maybe ContactType)
    } deriving (Eq,Read,Show)

-- | 'ContactDetail' smart constructor.
contactDetail :: ContactDetail
contactDetail =
    ContactDetail'
    { _cdOrganizationName = Nothing
    , _cdEmail = Nothing
    , _cdFax = Nothing
    , _cdState = Nothing
    , _cdLastName = Nothing
    , _cdExtraParams = Nothing
    , _cdZipCode = Nothing
    , _cdAddressLine1 = Nothing
    , _cdCity = Nothing
    , _cdPhoneNumber = Nothing
    , _cdAddressLine2 = Nothing
    , _cdFirstName = Nothing
    , _cdCountryCode = Nothing
    , _cdContactType = Nothing
    }

-- | Name of the organization for contact types other than @PERSON@.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 255 characters. Contact type must not be @PERSON@.
--
-- Parents: @RegistrantContact@, @AdminContact@, @TechContact@
--
-- Required: No
cdOrganizationName :: Lens' ContactDetail (Maybe Text)
cdOrganizationName = lens _cdOrganizationName (\ s a -> s{_cdOrganizationName = a});

-- | Email address of the contact.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 254 characters.
--
-- Parents: @RegistrantContact@, @AdminContact@, @TechContact@
--
-- Required: Yes
cdEmail :: Lens' ContactDetail (Maybe Text)
cdEmail = lens _cdEmail (\ s a -> s{_cdEmail = a});

-- | Fax number of the contact.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Phone number must be specified in the format \"+[country
-- dialing code].[number including any area code]\". For example, a US
-- phone number might appear as @\"+1.1234567890\"@.
--
-- Parents: @RegistrantContact@, @AdminContact@, @TechContact@
--
-- Required: No
cdFax :: Lens' ContactDetail (Maybe Text)
cdFax = lens _cdFax (\ s a -> s{_cdFax = a});

-- | The state or province of the contact\'s city.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 255 characters.
--
-- Parents: @RegistrantContact@, @AdminContact@, @TechContact@
--
-- Required: No
cdState :: Lens' ContactDetail (Maybe Text)
cdState = lens _cdState (\ s a -> s{_cdState = a});

-- | Last name of contact.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 255 characters.
--
-- Parents: @RegistrantContact@, @AdminContact@, @TechContact@
--
-- Required: Yes
cdLastName :: Lens' ContactDetail (Maybe Text)
cdLastName = lens _cdLastName (\ s a -> s{_cdLastName = a});

-- | A list of name-value pairs for parameters required by certain top-level
-- domains.
--
-- Type: Complex
--
-- Default: None
--
-- Parents: @RegistrantContact@, @AdminContact@, @TechContact@
--
-- Children: @Name@, @Value@
--
-- Required: No
cdExtraParams :: Lens' ContactDetail [ExtraParam]
cdExtraParams = lens _cdExtraParams (\ s a -> s{_cdExtraParams = a}) . _Default;

-- | The zip or postal code of the contact\'s address.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 255 characters.
--
-- Parents: @RegistrantContact@, @AdminContact@, @TechContact@
--
-- Required: No
cdZipCode :: Lens' ContactDetail (Maybe Text)
cdZipCode = lens _cdZipCode (\ s a -> s{_cdZipCode = a});

-- | First line of the contact\'s address.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 255 characters.
--
-- Parents: @RegistrantContact@, @AdminContact@, @TechContact@
--
-- Required: Yes
cdAddressLine1 :: Lens' ContactDetail (Maybe Text)
cdAddressLine1 = lens _cdAddressLine1 (\ s a -> s{_cdAddressLine1 = a});

-- | The city of the contact\'s address.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 255 characters.
--
-- Parents: @RegistrantContact@, @AdminContact@, @TechContact@
--
-- Required: Yes
cdCity :: Lens' ContactDetail (Maybe Text)
cdCity = lens _cdCity (\ s a -> s{_cdCity = a});

-- | The phone number of the contact.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Phone number must be specified in the format \"+[country
-- dialing code].[number including any area code>]\". For example, a US
-- phone number might appear as @\"+1.1234567890\"@.
--
-- Parents: @RegistrantContact@, @AdminContact@, @TechContact@
--
-- Required: Yes
cdPhoneNumber :: Lens' ContactDetail (Maybe Text)
cdPhoneNumber = lens _cdPhoneNumber (\ s a -> s{_cdPhoneNumber = a});

-- | Second line of contact\'s address, if any.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 255 characters.
--
-- Parents: @RegistrantContact@, @AdminContact@, @TechContact@
--
-- Required: No
cdAddressLine2 :: Lens' ContactDetail (Maybe Text)
cdAddressLine2 = lens _cdAddressLine2 (\ s a -> s{_cdAddressLine2 = a});

-- | First name of contact.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 255 characters.
--
-- Parents: @RegistrantContact@, @AdminContact@, @TechContact@
--
-- Required: Yes
cdFirstName :: Lens' ContactDetail (Maybe Text)
cdFirstName = lens _cdFirstName (\ s a -> s{_cdFirstName = a});

-- | Code for the country of the contact\'s address.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 255 characters.
--
-- Parents: @RegistrantContact@, @AdminContact@, @TechContact@
--
-- Required: Yes
cdCountryCode :: Lens' ContactDetail (Maybe CountryCode)
cdCountryCode = lens _cdCountryCode (\ s a -> s{_cdCountryCode = a});

-- | Indicates whether the contact is a person, company, association, or
-- public organization. If you choose an option other than @PERSON@, you
-- must enter an organization name, and you can\'t enable privacy
-- protection for the contact.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 255 characters.
--
-- Valid values: @PERSON@ | @COMPANY@ | @ASSOCIATION@ | @PUBLIC_BODY@
--
-- Parents: @RegistrantContact@, @AdminContact@, @TechContact@
--
-- Required: Yes
cdContactType :: Lens' ContactDetail (Maybe ContactType)
cdContactType = lens _cdContactType (\ s a -> s{_cdContactType = a});

instance FromJSON ContactDetail where
        parseJSON
          = withObject "ContactDetail"
              (\ x ->
                 ContactDetail' <$>
                   (x .:? "OrganizationName") <*> (x .:? "Email") <*>
                     (x .:? "Fax")
                     <*> (x .:? "State")
                     <*> (x .:? "LastName")
                     <*> (x .:? "ExtraParams" .!= mempty)
                     <*> (x .:? "ZipCode")
                     <*> (x .:? "AddressLine1")
                     <*> (x .:? "City")
                     <*> (x .:? "PhoneNumber")
                     <*> (x .:? "AddressLine2")
                     <*> (x .:? "FirstName")
                     <*> (x .:? "CountryCode")
                     <*> (x .:? "ContactType"))

instance ToJSON ContactDetail where
        toJSON ContactDetail'{..}
          = object
              ["OrganizationName" .= _cdOrganizationName,
               "Email" .= _cdEmail, "Fax" .= _cdFax,
               "State" .= _cdState, "LastName" .= _cdLastName,
               "ExtraParams" .= _cdExtraParams,
               "ZipCode" .= _cdZipCode,
               "AddressLine1" .= _cdAddressLine1, "City" .= _cdCity,
               "PhoneNumber" .= _cdPhoneNumber,
               "AddressLine2" .= _cdAddressLine2,
               "FirstName" .= _cdFirstName,
               "CountryCode" .= _cdCountryCode,
               "ContactType" .= _cdContactType]

-- | /See:/ 'domainSummary' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsExpiry'
--
-- * 'dsTransferLock'
--
-- * 'dsAutoRenew'
--
-- * 'dsDomainName'
data DomainSummary = DomainSummary'
    { _dsExpiry       :: !(Maybe POSIX)
    , _dsTransferLock :: !(Maybe Bool)
    , _dsAutoRenew    :: !(Maybe Bool)
    , _dsDomainName   :: !Text
    } deriving (Eq,Read,Show)

-- | 'DomainSummary' smart constructor.
domainSummary :: Text -> DomainSummary
domainSummary pDomainName =
    DomainSummary'
    { _dsExpiry = Nothing
    , _dsTransferLock = Nothing
    , _dsAutoRenew = Nothing
    , _dsDomainName = pDomainName
    }

-- | Expiration date of the domain in Coordinated Universal Time (UTC).
--
-- Type: Long
dsExpiry :: Lens' DomainSummary (Maybe UTCTime)
dsExpiry = lens _dsExpiry (\ s a -> s{_dsExpiry = a}) . mapping _Time;

-- | Indicates whether a domain is locked from unauthorized transfer to
-- another party.
--
-- Type: Boolean
--
-- Valid values: @True@ | @False@
dsTransferLock :: Lens' DomainSummary (Maybe Bool)
dsTransferLock = lens _dsTransferLock (\ s a -> s{_dsTransferLock = a});

-- | Indicates whether the domain is automatically renewed upon expiration.
--
-- Type: Boolean
--
-- Valid values: @True@ | @False@
dsAutoRenew :: Lens' DomainSummary (Maybe Bool)
dsAutoRenew = lens _dsAutoRenew (\ s a -> s{_dsAutoRenew = a});

-- | The name of a domain.
--
-- Type: String
dsDomainName :: Lens' DomainSummary Text
dsDomainName = lens _dsDomainName (\ s a -> s{_dsDomainName = a});

instance FromJSON DomainSummary where
        parseJSON
          = withObject "DomainSummary"
              (\ x ->
                 DomainSummary' <$>
                   (x .:? "Expiry") <*> (x .:? "TransferLock") <*>
                     (x .:? "AutoRenew")
                     <*> (x .: "DomainName"))

-- | ExtraParam includes the following elements.
--
-- /See:/ 'extraParam' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'epName'
--
-- * 'epValue'
data ExtraParam = ExtraParam'
    { _epName  :: !ExtraParamName
    , _epValue :: !Text
    } deriving (Eq,Read,Show)

-- | 'ExtraParam' smart constructor.
extraParam :: ExtraParamName -> Text -> ExtraParam
extraParam pName pValue =
    ExtraParam'
    { _epName = pName
    , _epValue = pValue
    }

-- | Name of the additional parameter required by the top-level domain.
--
-- Type: String
--
-- Default: None
--
-- Valid values: @DUNS_NUMBER@ | @BRAND_NUMBER@ | @BIRTH_DEPARTMENT@ |
-- @BIRTH_DATE_IN_YYYY_MM_DD@ | @BIRTH_COUNTRY@ | @BIRTH_CITY@ |
-- @DOCUMENT_NUMBER@ | @AU_ID_NUMBER@ | @AU_ID_TYPE@ | @CA_LEGAL_TYPE@ |
-- @ES_IDENTIFICATION@ | @ES_IDENTIFICATION_TYPE@ | @ES_LEGAL_FORM@ |
-- @FI_BUSINESS_NUMBER@ | @FI_ID_NUMBER@ | @IT_PIN@ | @RU_PASSPORT_DATA@ |
-- @SE_ID_NUMBER@ | @SG_ID_NUMBER@ | @VAT_NUMBER@
--
-- Parent: @ExtraParams@
--
-- Required: Yes
epName :: Lens' ExtraParam ExtraParamName
epName = lens _epName (\ s a -> s{_epName = a});

-- | Values corresponding to the additional parameter names required by some
-- top-level domains.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 2048 characters.
--
-- Parent: @ExtraParams@
--
-- Required: Yes
epValue :: Lens' ExtraParam Text
epValue = lens _epValue (\ s a -> s{_epValue = a});

instance FromJSON ExtraParam where
        parseJSON
          = withObject "ExtraParam"
              (\ x ->
                 ExtraParam' <$> (x .: "Name") <*> (x .: "Value"))

instance ToJSON ExtraParam where
        toJSON ExtraParam'{..}
          = object ["Name" .= _epName, "Value" .= _epValue]

-- | Nameserver includes the following elements.
--
-- /See:/ 'nameserver' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'namGlueIPs'
--
-- * 'namName'
data Nameserver = Nameserver'
    { _namGlueIPs :: !(Maybe [Text])
    , _namName    :: !Text
    } deriving (Eq,Read,Show)

-- | 'Nameserver' smart constructor.
nameserver :: Text -> Nameserver
nameserver pName =
    Nameserver'
    { _namGlueIPs = Nothing
    , _namName = pName
    }

-- | Glue IP address of a name server entry. Glue IP addresses are required
-- only when the name of the name server is a subdomain of the domain. For
-- example, if your domain is example.com and the name server for the
-- domain is ns.example.com, you need to specify the IP address for
-- ns.example.com.
--
-- Type: List of IP addresses.
--
-- Constraints: The list can contain only one IPv4 and one IPv6 address.
--
-- Parent: @Nameservers@
namGlueIPs :: Lens' Nameserver [Text]
namGlueIPs = lens _namGlueIPs (\ s a -> s{_namGlueIPs = a}) . _Default;

-- | The fully qualified host name of the name server.
--
-- Type: String
--
-- Constraint: Maximum 255 characterss
--
-- Parent: @Nameservers@
namName :: Lens' Nameserver Text
namName = lens _namName (\ s a -> s{_namName = a});

instance FromJSON Nameserver where
        parseJSON
          = withObject "Nameserver"
              (\ x ->
                 Nameserver' <$>
                   (x .:? "GlueIps" .!= mempty) <*> (x .: "Name"))

instance ToJSON Nameserver where
        toJSON Nameserver'{..}
          = object
              ["GlueIps" .= _namGlueIPs, "Name" .= _namName]

-- | OperationSummary includes the following elements.
--
-- /See:/ 'operationSummary' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'osOperationId'
--
-- * 'osStatus'
--
-- * 'osType'
--
-- * 'osSubmittedDate'
data OperationSummary = OperationSummary'
    { _osOperationId   :: !Text
    , _osStatus        :: !OperationStatus
    , _osType          :: !OperationType
    , _osSubmittedDate :: !POSIX
    } deriving (Eq,Read,Show)

-- | 'OperationSummary' smart constructor.
operationSummary :: Text -> OperationStatus -> OperationType -> UTCTime -> OperationSummary
operationSummary pOperationId pStatus pType pSubmittedDate =
    OperationSummary'
    { _osOperationId = pOperationId
    , _osStatus = pStatus
    , _osType = pType
    , _osSubmittedDate = _Time # pSubmittedDate
    }

-- | Identifier returned to track the requested action.
--
-- Type: String
osOperationId :: Lens' OperationSummary Text
osOperationId = lens _osOperationId (\ s a -> s{_osOperationId = a});

-- | The current status of the requested operation in the system.
--
-- Type: String
osStatus :: Lens' OperationSummary OperationStatus
osStatus = lens _osStatus (\ s a -> s{_osStatus = a});

-- | Type of the action requested.
--
-- Type: String
--
-- Valid values: @REGISTER_DOMAIN@ | @DELETE_DOMAIN@ | @TRANSFER_IN_DOMAIN@
-- | @UPDATE_DOMAIN_CONTACT@ | @UPDATE_NAMESERVER@ |
-- @CHANGE_PRIVACY_PROTECTION@ | @DOMAIN_LOCK@
osType :: Lens' OperationSummary OperationType
osType = lens _osType (\ s a -> s{_osType = a});

-- | The date when the request was submitted.
osSubmittedDate :: Lens' OperationSummary UTCTime
osSubmittedDate = lens _osSubmittedDate (\ s a -> s{_osSubmittedDate = a}) . _Time;

instance FromJSON OperationSummary where
        parseJSON
          = withObject "OperationSummary"
              (\ x ->
                 OperationSummary' <$>
                   (x .: "OperationId") <*> (x .: "Status") <*>
                     (x .: "Type")
                     <*> (x .: "SubmittedDate"))

-- | Each tag includes the following elements.
--
-- /See:/ 'tag' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tagValue'
--
-- * 'tagKey'
data Tag = Tag'
    { _tagValue :: !(Maybe Text)
    , _tagKey   :: !(Maybe Text)
    } deriving (Eq,Read,Show)

-- | 'Tag' smart constructor.
tag :: Tag
tag =
    Tag'
    { _tagValue = Nothing
    , _tagKey = Nothing
    }

-- | The value of a tag.
--
-- Type: String
--
-- Default: None
--
-- Valid values: A-Z, a-z, 0-9, space, \".:\/=+\\-\@\"
--
-- Constraints: Each value can be 0-256 characters long.
--
-- Required: Yes
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a});

-- | The key (name) of a tag.
--
-- Type: String
--
-- Default: None
--
-- Valid values: A-Z, a-z, 0-9, space, \".:\/=+\\-\@\"
--
-- Constraints: Each key can be 1-128 characters long.
--
-- Required: Yes
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\ s a -> s{_tagKey = a});

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .:? "Value") <*> (x .:? "Key"))

instance ToJSON Tag where
        toJSON Tag'{..}
          = object ["Value" .= _tagValue, "Key" .= _tagKey]
