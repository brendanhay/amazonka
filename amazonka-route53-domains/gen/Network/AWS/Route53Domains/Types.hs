{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53Domains.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
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
    -- ** Error
    , JSONError

    -- * DomainSummary
    , DomainSummary
    , domainSummary
    , dsAutoRenew
    , dsDomainName
    , dsExpiry
    , dsTransferLock

    -- * ExtraParamName
    , ExtraParamName (..)

    -- * Nameserver
    , Nameserver
    , nameserver
    , nGlueIps
    , nName

    -- * OperationStatus
    , OperationStatus (..)

    -- * DomainAvailability
    , DomainAvailability (..)

    -- * OperationType
    , OperationType (..)

    -- * CountryCode
    , CountryCode (..)

    -- * ExtraParam
    , ExtraParam
    , extraParam
    , epName
    , epValue

    -- * ContactType
    , ContactType (..)

    -- * ContactDetail
    , ContactDetail
    , contactDetail
    , cdAddressLine1
    , cdAddressLine2
    , cdCity
    , cdContactType
    , cdCountryCode
    , cdEmail
    , cdExtraParams
    , cdFax
    , cdFirstName
    , cdLastName
    , cdOrganizationName
    , cdPhoneNumber
    , cdState
    , cdZipCode

    -- * OperationSummary
    , OperationSummary
    , operationSummary
    , osOperationId
    , osStatus
    , osSubmittedDate
    , osType
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing
import qualified GHC.Exts

-- | Version @2014-05-15@ of the Amazon Route 53 Domains service.
data Route53Domains

instance AWSService Route53Domains where
    type Sg Route53Domains = V4
    type Er Route53Domains = JSONError

    service = service'
      where
        service' :: Service Route53Domains
        service' = Service
            { _svcAbbrev       = "Route53Domains"
            , _svcPrefix       = "route53domains"
            , _svcVersion      = "2014-05-15"
            , _svcTargetPrefix = Just "Route53Domains_v20140515"
            , _svcJSONVersion  = Just "1.1"
            , _svcHandle       = handle
            , _svcRetry        = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError JSONError)
        handle = jsonError statusSuccess service'

        retry :: Retry Route53Domains
        retry = Exponential
            { _retryBase     = 0.05
            , _retryGrowth   = 2
            , _retryAttempts = 5
            , _retryCheck    = check
            }

        check :: Status
              -> JSONError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e)
            | s == 500  = True -- General Server Error
            | s == 509  = True -- Limit Exceeded
            | s == 503  = True -- Service Unavailable
            | otherwise = False

data DomainSummary = DomainSummary
    { _dsAutoRenew    :: Maybe Bool
    , _dsDomainName   :: Text
    , _dsExpiry       :: Maybe POSIX
    , _dsTransferLock :: Maybe Bool
    } deriving (Eq, Ord, Read, Show)

-- | 'DomainSummary' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsAutoRenew' @::@ 'Maybe' 'Bool'
--
-- * 'dsDomainName' @::@ 'Text'
--
-- * 'dsExpiry' @::@ 'Maybe' 'UTCTime'
--
-- * 'dsTransferLock' @::@ 'Maybe' 'Bool'
--
domainSummary :: Text -- ^ 'dsDomainName'
              -> DomainSummary
domainSummary p1 = DomainSummary
    { _dsDomainName   = p1
    , _dsAutoRenew    = Nothing
    , _dsTransferLock = Nothing
    , _dsExpiry       = Nothing
    }

-- | Indicates whether the domain is automatically renewed upon expiration.
--
-- Type: Boolean
--
-- Valid values: 'True' | 'False'
dsAutoRenew :: Lens' DomainSummary (Maybe Bool)
dsAutoRenew = lens _dsAutoRenew (\s a -> s { _dsAutoRenew = a })

-- | The name of a domain.
--
-- Type: String
dsDomainName :: Lens' DomainSummary Text
dsDomainName = lens _dsDomainName (\s a -> s { _dsDomainName = a })

-- | Expiration date of the domain in Coordinated Universal Time (UTC).
--
-- Type: Long
dsExpiry :: Lens' DomainSummary (Maybe UTCTime)
dsExpiry = lens _dsExpiry (\s a -> s { _dsExpiry = a }) . mapping _Time

-- | Indicates whether a domain is locked from unauthorized transfer to another
-- party.
--
-- Type: Boolean
--
-- Valid values: 'True' | 'False'
dsTransferLock :: Lens' DomainSummary (Maybe Bool)
dsTransferLock = lens _dsTransferLock (\s a -> s { _dsTransferLock = a })

instance FromJSON DomainSummary where
    parseJSON = withObject "DomainSummary" $ \o -> DomainSummary
        <$> o .:? "AutoRenew"
        <*> o .:  "DomainName"
        <*> o .:? "Expiry"
        <*> o .:? "TransferLock"

instance ToJSON DomainSummary where
    toJSON DomainSummary{..} = object
        [ "DomainName"   .= _dsDomainName
        , "AutoRenew"    .= _dsAutoRenew
        , "TransferLock" .= _dsTransferLock
        , "Expiry"       .= _dsExpiry
        ]

data ExtraParamName
    = AuIdNumber          -- ^ AU_ID_NUMBER
    | AuIdType            -- ^ AU_ID_TYPE
    | BirthCity           -- ^ BIRTH_CITY
    | BirthCountry        -- ^ BIRTH_COUNTRY
    | BirthDateInYyyyMmDd -- ^ BIRTH_DATE_IN_YYYY_MM_DD
    | BirthDepartment     -- ^ BIRTH_DEPARTMENT
    | BrandNumber         -- ^ BRAND_NUMBER
    | CaLegalType         -- ^ CA_LEGAL_TYPE
    | DocumentNumber      -- ^ DOCUMENT_NUMBER
    | DunsNumber          -- ^ DUNS_NUMBER
    | FiBusinessNumber    -- ^ FI_BUSINESS_NUMBER
    | FiIdNumber          -- ^ FI_ID_NUMBER
    | ItPin               -- ^ IT_PIN
    | RuPassportData      -- ^ RU_PASSPORT_DATA
    | SeIdNumber          -- ^ SE_ID_NUMBER
    | SgIdNumber          -- ^ SG_ID_NUMBER
    | VatNumber           -- ^ VAT_NUMBER
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable ExtraParamName

instance FromText ExtraParamName where
    parser = takeLowerText >>= \case
        "au_id_number"             -> pure AuIdNumber
        "au_id_type"               -> pure AuIdType
        "birth_city"               -> pure BirthCity
        "birth_country"            -> pure BirthCountry
        "birth_date_in_yyyy_mm_dd" -> pure BirthDateInYyyyMmDd
        "birth_department"         -> pure BirthDepartment
        "brand_number"             -> pure BrandNumber
        "ca_legal_type"            -> pure CaLegalType
        "document_number"          -> pure DocumentNumber
        "duns_number"              -> pure DunsNumber
        "fi_business_number"       -> pure FiBusinessNumber
        "fi_id_number"             -> pure FiIdNumber
        "it_pin"                   -> pure ItPin
        "ru_passport_data"         -> pure RuPassportData
        "se_id_number"             -> pure SeIdNumber
        "sg_id_number"             -> pure SgIdNumber
        "vat_number"               -> pure VatNumber
        e                          -> fail $
            "Failure parsing ExtraParamName from " ++ show e

instance ToText ExtraParamName where
    toText = \case
        AuIdNumber          -> "AU_ID_NUMBER"
        AuIdType            -> "AU_ID_TYPE"
        BirthCity           -> "BIRTH_CITY"
        BirthCountry        -> "BIRTH_COUNTRY"
        BirthDateInYyyyMmDd -> "BIRTH_DATE_IN_YYYY_MM_DD"
        BirthDepartment     -> "BIRTH_DEPARTMENT"
        BrandNumber         -> "BRAND_NUMBER"
        CaLegalType         -> "CA_LEGAL_TYPE"
        DocumentNumber      -> "DOCUMENT_NUMBER"
        DunsNumber          -> "DUNS_NUMBER"
        FiBusinessNumber    -> "FI_BUSINESS_NUMBER"
        FiIdNumber          -> "FI_ID_NUMBER"
        ItPin               -> "IT_PIN"
        RuPassportData      -> "RU_PASSPORT_DATA"
        SeIdNumber          -> "SE_ID_NUMBER"
        SgIdNumber          -> "SG_ID_NUMBER"
        VatNumber           -> "VAT_NUMBER"

instance ToByteString ExtraParamName
instance ToHeader     ExtraParamName
instance ToQuery      ExtraParamName

instance FromJSON ExtraParamName where
    parseJSON = parseJSONText "ExtraParamName"

instance ToJSON ExtraParamName where
    toJSON = toJSONText

data Nameserver = Nameserver
    { _nGlueIps :: List "GlueIps" Text
    , _nName    :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'Nameserver' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'nGlueIps' @::@ ['Text']
--
-- * 'nName' @::@ 'Text'
--
nameserver :: Text -- ^ 'nName'
           -> Nameserver
nameserver p1 = Nameserver
    { _nName    = p1
    , _nGlueIps = mempty
    }

-- | Glue IP address of a name server entry. Glue IP addresses are required only
-- when the name of the name server is a subdomain of the domain. For example,
-- if your domain is example.com and the name server for the domain is
-- ns.example.com, you need to specify the IP address for ns.example.com.
--
-- Type: List of IP addresses.
--
-- Constraints: The list can contain only one IPv4 and one IPv6 address.
--
-- Parent: 'Nameservers'
nGlueIps :: Lens' Nameserver [Text]
nGlueIps = lens _nGlueIps (\s a -> s { _nGlueIps = a }) . _List

-- | The fully qualified host name of the name server.
--
-- Type: String
--
-- Constraint: Maximum 255 characterss
--
-- Parent: 'Nameservers'
nName :: Lens' Nameserver Text
nName = lens _nName (\s a -> s { _nName = a })

instance FromJSON Nameserver where
    parseJSON = withObject "Nameserver" $ \o -> Nameserver
        <$> o .:? "GlueIps" .!= mempty
        <*> o .:  "Name"

instance ToJSON Nameserver where
    toJSON Nameserver{..} = object
        [ "Name"    .= _nName
        , "GlueIps" .= _nGlueIps
        ]

data OperationStatus
    = Error      -- ^ ERROR
    | Failed     -- ^ FAILED
    | InProgress -- ^ IN_PROGRESS
    | Submitted  -- ^ SUBMITTED
    | Successful -- ^ SUCCESSFUL
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable OperationStatus

instance FromText OperationStatus where
    parser = takeLowerText >>= \case
        "error"       -> pure Error
        "failed"      -> pure Failed
        "in_progress" -> pure InProgress
        "submitted"   -> pure Submitted
        "successful"  -> pure Successful
        e             -> fail $
            "Failure parsing OperationStatus from " ++ show e

instance ToText OperationStatus where
    toText = \case
        Error      -> "ERROR"
        Failed     -> "FAILED"
        InProgress -> "IN_PROGRESS"
        Submitted  -> "SUBMITTED"
        Successful -> "SUCCESSFUL"

instance ToByteString OperationStatus
instance ToHeader     OperationStatus
instance ToQuery      OperationStatus

instance FromJSON OperationStatus where
    parseJSON = parseJSONText "OperationStatus"

instance ToJSON OperationStatus where
    toJSON = toJSONText

data DomainAvailability
    = Available             -- ^ AVAILABLE
    | AvailablePreorder     -- ^ AVAILABLE_PREORDER
    | AvailableReserved     -- ^ AVAILABLE_RESERVED
    | Reserved              -- ^ RESERVED
    | Unavailable           -- ^ UNAVAILABLE
    | UnavailablePremium    -- ^ UNAVAILABLE_PREMIUM
    | UnavailableRestricted -- ^ UNAVAILABLE_RESTRICTED
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable DomainAvailability

instance FromText DomainAvailability where
    parser = takeLowerText >>= \case
        "available"              -> pure Available
        "available_preorder"     -> pure AvailablePreorder
        "available_reserved"     -> pure AvailableReserved
        "reserved"               -> pure Reserved
        "unavailable"            -> pure Unavailable
        "unavailable_premium"    -> pure UnavailablePremium
        "unavailable_restricted" -> pure UnavailableRestricted
        e                        -> fail $
            "Failure parsing DomainAvailability from " ++ show e

instance ToText DomainAvailability where
    toText = \case
        Available             -> "AVAILABLE"
        AvailablePreorder     -> "AVAILABLE_PREORDER"
        AvailableReserved     -> "AVAILABLE_RESERVED"
        Reserved              -> "RESERVED"
        Unavailable           -> "UNAVAILABLE"
        UnavailablePremium    -> "UNAVAILABLE_PREMIUM"
        UnavailableRestricted -> "UNAVAILABLE_RESTRICTED"

instance ToByteString DomainAvailability
instance ToHeader     DomainAvailability
instance ToQuery      DomainAvailability

instance FromJSON DomainAvailability where
    parseJSON = parseJSONText "DomainAvailability"

instance ToJSON DomainAvailability where
    toJSON = toJSONText

data OperationType
    = OTChangePrivacyProtection -- ^ CHANGE_PRIVACY_PROTECTION
    | OTDeleteDomain            -- ^ DELETE_DOMAIN
    | OTDomainLock              -- ^ DOMAIN_LOCK
    | OTRegisterDomain          -- ^ REGISTER_DOMAIN
    | OTTransferInDomain        -- ^ TRANSFER_IN_DOMAIN
    | OTUpdateDomainContact     -- ^ UPDATE_DOMAIN_CONTACT
    | OTUpdateNameserver        -- ^ UPDATE_NAMESERVER
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable OperationType

instance FromText OperationType where
    parser = takeLowerText >>= \case
        "change_privacy_protection" -> pure OTChangePrivacyProtection
        "delete_domain"             -> pure OTDeleteDomain
        "domain_lock"               -> pure OTDomainLock
        "register_domain"           -> pure OTRegisterDomain
        "transfer_in_domain"        -> pure OTTransferInDomain
        "update_domain_contact"     -> pure OTUpdateDomainContact
        "update_nameserver"         -> pure OTUpdateNameserver
        e                           -> fail $
            "Failure parsing OperationType from " ++ show e

instance ToText OperationType where
    toText = \case
        OTChangePrivacyProtection -> "CHANGE_PRIVACY_PROTECTION"
        OTDeleteDomain            -> "DELETE_DOMAIN"
        OTDomainLock              -> "DOMAIN_LOCK"
        OTRegisterDomain          -> "REGISTER_DOMAIN"
        OTTransferInDomain        -> "TRANSFER_IN_DOMAIN"
        OTUpdateDomainContact     -> "UPDATE_DOMAIN_CONTACT"
        OTUpdateNameserver        -> "UPDATE_NAMESERVER"

instance ToByteString OperationType
instance ToHeader     OperationType
instance ToQuery      OperationType

instance FromJSON OperationType where
    parseJSON = parseJSONText "OperationType"

instance ToJSON OperationType where
    toJSON = toJSONText

data CountryCode
    = Ad  -- ^ AD
    | Ae  -- ^ AE
    | Af  -- ^ AF
    | Ag  -- ^ AG
    | Ai  -- ^ AI
    | Al  -- ^ AL
    | Am  -- ^ AM
    | An  -- ^ AN
    | Ao  -- ^ AO
    | Aq  -- ^ AQ
    | Ar  -- ^ AR
    | As  -- ^ AS
    | At  -- ^ AT
    | Au  -- ^ AU
    | Aw  -- ^ AW
    | Az  -- ^ AZ
    | Ba  -- ^ BA
    | Bb  -- ^ BB
    | Bd  -- ^ BD
    | Be  -- ^ BE
    | Bf  -- ^ BF
    | Bg  -- ^ BG
    | Bh  -- ^ BH
    | Bi  -- ^ BI
    | Bj  -- ^ BJ
    | Bl  -- ^ BL
    | Bm  -- ^ BM
    | Bn  -- ^ BN
    | Bo  -- ^ BO
    | Br  -- ^ BR
    | Bs  -- ^ BS
    | Bt  -- ^ BT
    | Bw  -- ^ BW
    | By  -- ^ BY
    | Bz  -- ^ BZ
    | Ca  -- ^ CA
    | Cc  -- ^ CC
    | Cd  -- ^ CD
    | Cf  -- ^ CF
    | Cg  -- ^ CG
    | Ch  -- ^ CH
    | Ci  -- ^ CI
    | Ck  -- ^ CK
    | Cl  -- ^ CL
    | Cm  -- ^ CM
    | Cn  -- ^ CN
    | Co  -- ^ CO
    | Cr  -- ^ CR
    | Cu  -- ^ CU
    | Cv  -- ^ CV
    | Cx  -- ^ CX
    | Cy  -- ^ CY
    | Cz  -- ^ CZ
    | De  -- ^ DE
    | Dj  -- ^ DJ
    | Dk  -- ^ DK
    | Dm  -- ^ DM
    | Do' -- ^ DO
    | Dz  -- ^ DZ
    | Ec  -- ^ EC
    | Ee  -- ^ EE
    | Eg  -- ^ EG
    | Er  -- ^ ER
    | Es  -- ^ ES
    | Et  -- ^ ET
    | Fi  -- ^ FI
    | Fj  -- ^ FJ
    | Fk  -- ^ FK
    | Fm  -- ^ FM
    | Fo  -- ^ FO
    | Fr  -- ^ FR
    | Ga  -- ^ GA
    | Gb  -- ^ GB
    | Gd  -- ^ GD
    | Ge  -- ^ GE
    | Gh  -- ^ GH
    | Gi  -- ^ GI
    | Gl  -- ^ GL
    | Gm  -- ^ GM
    | Gn  -- ^ GN
    | Gq  -- ^ GQ
    | Gr  -- ^ GR
    | Gt  -- ^ GT
    | Gu  -- ^ GU
    | Gw  -- ^ GW
    | Gy  -- ^ GY
    | Hk  -- ^ HK
    | Hn  -- ^ HN
    | Hr  -- ^ HR
    | Ht  -- ^ HT
    | Hu  -- ^ HU
    | Id  -- ^ ID
    | Ie  -- ^ IE
    | Il  -- ^ IL
    | Im  -- ^ IM
    | In' -- ^ IN
    | Iq  -- ^ IQ
    | Ir  -- ^ IR
    | Is  -- ^ IS
    | It  -- ^ IT
    | Jm  -- ^ JM
    | Jo  -- ^ JO
    | Jp  -- ^ JP
    | Ke  -- ^ KE
    | Kg  -- ^ KG
    | Kh  -- ^ KH
    | Ki  -- ^ KI
    | Km  -- ^ KM
    | Kn  -- ^ KN
    | Kp  -- ^ KP
    | Kr  -- ^ KR
    | Kw  -- ^ KW
    | Ky  -- ^ KY
    | Kz  -- ^ KZ
    | La  -- ^ LA
    | Lb  -- ^ LB
    | Lc  -- ^ LC
    | Li  -- ^ LI
    | Lk  -- ^ LK
    | Lr  -- ^ LR
    | Ls  -- ^ LS
    | Lt  -- ^ LT
    | Lu  -- ^ LU
    | Lv  -- ^ LV
    | Ly  -- ^ LY
    | Ma  -- ^ MA
    | Mc  -- ^ MC
    | Md  -- ^ MD
    | Me  -- ^ ME
    | Mf  -- ^ MF
    | Mg  -- ^ MG
    | Mh  -- ^ MH
    | Mk  -- ^ MK
    | Ml  -- ^ ML
    | Mm  -- ^ MM
    | Mn  -- ^ MN
    | Mo  -- ^ MO
    | Mp  -- ^ MP
    | Mr  -- ^ MR
    | Ms  -- ^ MS
    | Mt  -- ^ MT
    | Mu  -- ^ MU
    | Mv  -- ^ MV
    | Mw  -- ^ MW
    | Mx  -- ^ MX
    | My  -- ^ MY
    | Mz  -- ^ MZ
    | Na  -- ^ NA
    | Nc  -- ^ NC
    | Ne  -- ^ NE
    | Ng  -- ^ NG
    | Ni  -- ^ NI
    | Nl  -- ^ NL
    | No  -- ^ NO
    | Np  -- ^ NP
    | Nr  -- ^ NR
    | Nu  -- ^ NU
    | Nz  -- ^ NZ
    | Om  -- ^ OM
    | Pa  -- ^ PA
    | Pe  -- ^ PE
    | Pf  -- ^ PF
    | Pg  -- ^ PG
    | Ph  -- ^ PH
    | Pk  -- ^ PK
    | Pl  -- ^ PL
    | Pm  -- ^ PM
    | Pn  -- ^ PN
    | Pr  -- ^ PR
    | Pt  -- ^ PT
    | Pw  -- ^ PW
    | Py  -- ^ PY
    | Qa  -- ^ QA
    | Ro  -- ^ RO
    | Rs  -- ^ RS
    | Ru  -- ^ RU
    | Rw  -- ^ RW
    | Sa  -- ^ SA
    | Sb  -- ^ SB
    | Sc  -- ^ SC
    | Sd  -- ^ SD
    | Se  -- ^ SE
    | Sg  -- ^ SG
    | Sh  -- ^ SH
    | Si  -- ^ SI
    | Sk  -- ^ SK
    | Sl  -- ^ SL
    | Sm  -- ^ SM
    | Sn  -- ^ SN
    | So  -- ^ SO
    | Sr  -- ^ SR
    | St  -- ^ ST
    | Sv  -- ^ SV
    | Sy  -- ^ SY
    | Sz  -- ^ SZ
    | Tc  -- ^ TC
    | Td  -- ^ TD
    | Tg  -- ^ TG
    | Th  -- ^ TH
    | Tj  -- ^ TJ
    | Tk  -- ^ TK
    | Tl  -- ^ TL
    | Tm  -- ^ TM
    | Tn  -- ^ TN
    | To  -- ^ TO
    | Tr  -- ^ TR
    | Tt  -- ^ TT
    | Tv  -- ^ TV
    | Tw  -- ^ TW
    | Tz  -- ^ TZ
    | Ua  -- ^ UA
    | Ug  -- ^ UG
    | Us  -- ^ US
    | Uy  -- ^ UY
    | Uz  -- ^ UZ
    | Va  -- ^ VA
    | Vc  -- ^ VC
    | Ve  -- ^ VE
    | Vg  -- ^ VG
    | Vi  -- ^ VI
    | Vn  -- ^ VN
    | Vu  -- ^ VU
    | Wf  -- ^ WF
    | Ws  -- ^ WS
    | Ye  -- ^ YE
    | Yt  -- ^ YT
    | Za  -- ^ ZA
    | Zm  -- ^ ZM
    | Zw  -- ^ ZW
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable CountryCode

instance FromText CountryCode where
    parser = takeLowerText >>= \case
        "ad" -> pure Ad
        "ae" -> pure Ae
        "af" -> pure Af
        "ag" -> pure Ag
        "ai" -> pure Ai
        "al" -> pure Al
        "am" -> pure Am
        "an" -> pure An
        "ao" -> pure Ao
        "aq" -> pure Aq
        "ar" -> pure Ar
        "as" -> pure As
        "at" -> pure At
        "au" -> pure Au
        "aw" -> pure Aw
        "az" -> pure Az
        "ba" -> pure Ba
        "bb" -> pure Bb
        "bd" -> pure Bd
        "be" -> pure Be
        "bf" -> pure Bf
        "bg" -> pure Bg
        "bh" -> pure Bh
        "bi" -> pure Bi
        "bj" -> pure Bj
        "bl" -> pure Bl
        "bm" -> pure Bm
        "bn" -> pure Bn
        "bo" -> pure Bo
        "br" -> pure Br
        "bs" -> pure Bs
        "bt" -> pure Bt
        "bw" -> pure Bw
        "by" -> pure By
        "bz" -> pure Bz
        "ca" -> pure Ca
        "cc" -> pure Cc
        "cd" -> pure Cd
        "cf" -> pure Cf
        "cg" -> pure Cg
        "ch" -> pure Ch
        "ci" -> pure Ci
        "ck" -> pure Ck
        "cl" -> pure Cl
        "cm" -> pure Cm
        "cn" -> pure Cn
        "co" -> pure Co
        "cr" -> pure Cr
        "cu" -> pure Cu
        "cv" -> pure Cv
        "cx" -> pure Cx
        "cy" -> pure Cy
        "cz" -> pure Cz
        "de" -> pure De
        "dj" -> pure Dj
        "dk" -> pure Dk
        "dm" -> pure Dm
        "do" -> pure Do'
        "dz" -> pure Dz
        "ec" -> pure Ec
        "ee" -> pure Ee
        "eg" -> pure Eg
        "er" -> pure Er
        "es" -> pure Es
        "et" -> pure Et
        "fi" -> pure Fi
        "fj" -> pure Fj
        "fk" -> pure Fk
        "fm" -> pure Fm
        "fo" -> pure Fo
        "fr" -> pure Fr
        "ga" -> pure Ga
        "gb" -> pure Gb
        "gd" -> pure Gd
        "ge" -> pure Ge
        "gh" -> pure Gh
        "gi" -> pure Gi
        "gl" -> pure Gl
        "gm" -> pure Gm
        "gn" -> pure Gn
        "gq" -> pure Gq
        "gr" -> pure Gr
        "gt" -> pure Gt
        "gu" -> pure Gu
        "gw" -> pure Gw
        "gy" -> pure Gy
        "hk" -> pure Hk
        "hn" -> pure Hn
        "hr" -> pure Hr
        "ht" -> pure Ht
        "hu" -> pure Hu
        "id" -> pure Id
        "ie" -> pure Ie
        "il" -> pure Il
        "im" -> pure Im
        "in" -> pure In'
        "iq" -> pure Iq
        "ir" -> pure Ir
        "is" -> pure Is
        "it" -> pure It
        "jm" -> pure Jm
        "jo" -> pure Jo
        "jp" -> pure Jp
        "ke" -> pure Ke
        "kg" -> pure Kg
        "kh" -> pure Kh
        "ki" -> pure Ki
        "km" -> pure Km
        "kn" -> pure Kn
        "kp" -> pure Kp
        "kr" -> pure Kr
        "kw" -> pure Kw
        "ky" -> pure Ky
        "kz" -> pure Kz
        "la" -> pure La
        "lb" -> pure Lb
        "lc" -> pure Lc
        "li" -> pure Li
        "lk" -> pure Lk
        "lr" -> pure Lr
        "ls" -> pure Ls
        "lt" -> pure Lt
        "lu" -> pure Lu
        "lv" -> pure Lv
        "ly" -> pure Ly
        "ma" -> pure Ma
        "mc" -> pure Mc
        "md" -> pure Md
        "me" -> pure Me
        "mf" -> pure Mf
        "mg" -> pure Mg
        "mh" -> pure Mh
        "mk" -> pure Mk
        "ml" -> pure Ml
        "mm" -> pure Mm
        "mn" -> pure Mn
        "mo" -> pure Mo
        "mp" -> pure Mp
        "mr" -> pure Mr
        "ms" -> pure Ms
        "mt" -> pure Mt
        "mu" -> pure Mu
        "mv" -> pure Mv
        "mw" -> pure Mw
        "mx" -> pure Mx
        "my" -> pure My
        "mz" -> pure Mz
        "na" -> pure Na
        "nc" -> pure Nc
        "ne" -> pure Ne
        "ng" -> pure Ng
        "ni" -> pure Ni
        "nl" -> pure Nl
        "no" -> pure No
        "np" -> pure Np
        "nr" -> pure Nr
        "nu" -> pure Nu
        "nz" -> pure Nz
        "om" -> pure Om
        "pa" -> pure Pa
        "pe" -> pure Pe
        "pf" -> pure Pf
        "pg" -> pure Pg
        "ph" -> pure Ph
        "pk" -> pure Pk
        "pl" -> pure Pl
        "pm" -> pure Pm
        "pn" -> pure Pn
        "pr" -> pure Pr
        "pt" -> pure Pt
        "pw" -> pure Pw
        "py" -> pure Py
        "qa" -> pure Qa
        "ro" -> pure Ro
        "rs" -> pure Rs
        "ru" -> pure Ru
        "rw" -> pure Rw
        "sa" -> pure Sa
        "sb" -> pure Sb
        "sc" -> pure Sc
        "sd" -> pure Sd
        "se" -> pure Se
        "sg" -> pure Sg
        "sh" -> pure Sh
        "si" -> pure Si
        "sk" -> pure Sk
        "sl" -> pure Sl
        "sm" -> pure Sm
        "sn" -> pure Sn
        "so" -> pure So
        "sr" -> pure Sr
        "st" -> pure St
        "sv" -> pure Sv
        "sy" -> pure Sy
        "sz" -> pure Sz
        "tc" -> pure Tc
        "td" -> pure Td
        "tg" -> pure Tg
        "th" -> pure Th
        "tj" -> pure Tj
        "tk" -> pure Tk
        "tl" -> pure Tl
        "tm" -> pure Tm
        "tn" -> pure Tn
        "to" -> pure To
        "tr" -> pure Tr
        "tt" -> pure Tt
        "tv" -> pure Tv
        "tw" -> pure Tw
        "tz" -> pure Tz
        "ua" -> pure Ua
        "ug" -> pure Ug
        "us" -> pure Us
        "uy" -> pure Uy
        "uz" -> pure Uz
        "va" -> pure Va
        "vc" -> pure Vc
        "ve" -> pure Ve
        "vg" -> pure Vg
        "vi" -> pure Vi
        "vn" -> pure Vn
        "vu" -> pure Vu
        "wf" -> pure Wf
        "ws" -> pure Ws
        "ye" -> pure Ye
        "yt" -> pure Yt
        "za" -> pure Za
        "zm" -> pure Zm
        "zw" -> pure Zw
        e    -> fail $
            "Failure parsing CountryCode from " ++ show e

instance ToText CountryCode where
    toText = \case
        Ad  -> "AD"
        Ae  -> "AE"
        Af  -> "AF"
        Ag  -> "AG"
        Ai  -> "AI"
        Al  -> "AL"
        Am  -> "AM"
        An  -> "AN"
        Ao  -> "AO"
        Aq  -> "AQ"
        Ar  -> "AR"
        As  -> "AS"
        At  -> "AT"
        Au  -> "AU"
        Aw  -> "AW"
        Az  -> "AZ"
        Ba  -> "BA"
        Bb  -> "BB"
        Bd  -> "BD"
        Be  -> "BE"
        Bf  -> "BF"
        Bg  -> "BG"
        Bh  -> "BH"
        Bi  -> "BI"
        Bj  -> "BJ"
        Bl  -> "BL"
        Bm  -> "BM"
        Bn  -> "BN"
        Bo  -> "BO"
        Br  -> "BR"
        Bs  -> "BS"
        Bt  -> "BT"
        Bw  -> "BW"
        By  -> "BY"
        Bz  -> "BZ"
        Ca  -> "CA"
        Cc  -> "CC"
        Cd  -> "CD"
        Cf  -> "CF"
        Cg  -> "CG"
        Ch  -> "CH"
        Ci  -> "CI"
        Ck  -> "CK"
        Cl  -> "CL"
        Cm  -> "CM"
        Cn  -> "CN"
        Co  -> "CO"
        Cr  -> "CR"
        Cu  -> "CU"
        Cv  -> "CV"
        Cx  -> "CX"
        Cy  -> "CY"
        Cz  -> "CZ"
        De  -> "DE"
        Dj  -> "DJ"
        Dk  -> "DK"
        Dm  -> "DM"
        Do' -> "DO"
        Dz  -> "DZ"
        Ec  -> "EC"
        Ee  -> "EE"
        Eg  -> "EG"
        Er  -> "ER"
        Es  -> "ES"
        Et  -> "ET"
        Fi  -> "FI"
        Fj  -> "FJ"
        Fk  -> "FK"
        Fm  -> "FM"
        Fo  -> "FO"
        Fr  -> "FR"
        Ga  -> "GA"
        Gb  -> "GB"
        Gd  -> "GD"
        Ge  -> "GE"
        Gh  -> "GH"
        Gi  -> "GI"
        Gl  -> "GL"
        Gm  -> "GM"
        Gn  -> "GN"
        Gq  -> "GQ"
        Gr  -> "GR"
        Gt  -> "GT"
        Gu  -> "GU"
        Gw  -> "GW"
        Gy  -> "GY"
        Hk  -> "HK"
        Hn  -> "HN"
        Hr  -> "HR"
        Ht  -> "HT"
        Hu  -> "HU"
        Id  -> "ID"
        Ie  -> "IE"
        Il  -> "IL"
        Im  -> "IM"
        In' -> "IN"
        Iq  -> "IQ"
        Ir  -> "IR"
        Is  -> "IS"
        It  -> "IT"
        Jm  -> "JM"
        Jo  -> "JO"
        Jp  -> "JP"
        Ke  -> "KE"
        Kg  -> "KG"
        Kh  -> "KH"
        Ki  -> "KI"
        Km  -> "KM"
        Kn  -> "KN"
        Kp  -> "KP"
        Kr  -> "KR"
        Kw  -> "KW"
        Ky  -> "KY"
        Kz  -> "KZ"
        La  -> "LA"
        Lb  -> "LB"
        Lc  -> "LC"
        Li  -> "LI"
        Lk  -> "LK"
        Lr  -> "LR"
        Ls  -> "LS"
        Lt  -> "LT"
        Lu  -> "LU"
        Lv  -> "LV"
        Ly  -> "LY"
        Ma  -> "MA"
        Mc  -> "MC"
        Md  -> "MD"
        Me  -> "ME"
        Mf  -> "MF"
        Mg  -> "MG"
        Mh  -> "MH"
        Mk  -> "MK"
        Ml  -> "ML"
        Mm  -> "MM"
        Mn  -> "MN"
        Mo  -> "MO"
        Mp  -> "MP"
        Mr  -> "MR"
        Ms  -> "MS"
        Mt  -> "MT"
        Mu  -> "MU"
        Mv  -> "MV"
        Mw  -> "MW"
        Mx  -> "MX"
        My  -> "MY"
        Mz  -> "MZ"
        Na  -> "NA"
        Nc  -> "NC"
        Ne  -> "NE"
        Ng  -> "NG"
        Ni  -> "NI"
        Nl  -> "NL"
        No  -> "NO"
        Np  -> "NP"
        Nr  -> "NR"
        Nu  -> "NU"
        Nz  -> "NZ"
        Om  -> "OM"
        Pa  -> "PA"
        Pe  -> "PE"
        Pf  -> "PF"
        Pg  -> "PG"
        Ph  -> "PH"
        Pk  -> "PK"
        Pl  -> "PL"
        Pm  -> "PM"
        Pn  -> "PN"
        Pr  -> "PR"
        Pt  -> "PT"
        Pw  -> "PW"
        Py  -> "PY"
        Qa  -> "QA"
        Ro  -> "RO"
        Rs  -> "RS"
        Ru  -> "RU"
        Rw  -> "RW"
        Sa  -> "SA"
        Sb  -> "SB"
        Sc  -> "SC"
        Sd  -> "SD"
        Se  -> "SE"
        Sg  -> "SG"
        Sh  -> "SH"
        Si  -> "SI"
        Sk  -> "SK"
        Sl  -> "SL"
        Sm  -> "SM"
        Sn  -> "SN"
        So  -> "SO"
        Sr  -> "SR"
        St  -> "ST"
        Sv  -> "SV"
        Sy  -> "SY"
        Sz  -> "SZ"
        Tc  -> "TC"
        Td  -> "TD"
        Tg  -> "TG"
        Th  -> "TH"
        Tj  -> "TJ"
        Tk  -> "TK"
        Tl  -> "TL"
        Tm  -> "TM"
        Tn  -> "TN"
        To  -> "TO"
        Tr  -> "TR"
        Tt  -> "TT"
        Tv  -> "TV"
        Tw  -> "TW"
        Tz  -> "TZ"
        Ua  -> "UA"
        Ug  -> "UG"
        Us  -> "US"
        Uy  -> "UY"
        Uz  -> "UZ"
        Va  -> "VA"
        Vc  -> "VC"
        Ve  -> "VE"
        Vg  -> "VG"
        Vi  -> "VI"
        Vn  -> "VN"
        Vu  -> "VU"
        Wf  -> "WF"
        Ws  -> "WS"
        Ye  -> "YE"
        Yt  -> "YT"
        Za  -> "ZA"
        Zm  -> "ZM"
        Zw  -> "ZW"

instance ToByteString CountryCode
instance ToHeader     CountryCode
instance ToQuery      CountryCode

instance FromJSON CountryCode where
    parseJSON = parseJSONText "CountryCode"

instance ToJSON CountryCode where
    toJSON = toJSONText

data ExtraParam = ExtraParam
    { _epName  :: ExtraParamName
    , _epValue :: Text
    } deriving (Eq, Read, Show)

-- | 'ExtraParam' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'epName' @::@ 'ExtraParamName'
--
-- * 'epValue' @::@ 'Text'
--
extraParam :: ExtraParamName -- ^ 'epName'
           -> Text -- ^ 'epValue'
           -> ExtraParam
extraParam p1 p2 = ExtraParam
    { _epName  = p1
    , _epValue = p2
    }

-- | Name of the additional parameter required by the top-level domain.
--
-- Type: String
--
-- Default: None
--
-- Valid values: 'DUNS_NUMBER' | 'BRAND_NUMBER' | 'BIRTH_DEPARTMENT' | 'BIRTH_DATE_IN_YYYY_MM_DD' | 'BIRTH_COUNTRY' | 'BIRTH_CITY' | 'DOCUMENT_NUMBER' | 'AU_ID_NUMBER' | 'AU_ID_TYPE' | 'CA_LEGAL_TYPE' | 'FI_BUSINESS_NUMBER' | 'FI_ID_NUMBER' | 'IT_PIN' | 'RU_PASSPORT_DATA'
-- | 'SE_ID_NUMBER' | 'SG_ID_NUMBER' | 'VAT_NUMBER'
--
-- Parent: 'ExtraParams'
--
-- Required: Yes
epName :: Lens' ExtraParam ExtraParamName
epName = lens _epName (\s a -> s { _epName = a })

-- | Values corresponding to the additional parameter names required by some
-- top-level domains.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 2048 characters.
--
-- Parent: 'ExtraParams'
--
-- Required: Yes
epValue :: Lens' ExtraParam Text
epValue = lens _epValue (\s a -> s { _epValue = a })

instance FromJSON ExtraParam where
    parseJSON = withObject "ExtraParam" $ \o -> ExtraParam
        <$> o .:  "Name"
        <*> o .:  "Value"

instance ToJSON ExtraParam where
    toJSON ExtraParam{..} = object
        [ "Name"  .= _epName
        , "Value" .= _epValue
        ]

data ContactType
    = CTAssociation -- ^ ASSOCIATION
    | CTCompany     -- ^ COMPANY
    | CTPerson      -- ^ PERSON
    | CTPublicBody  -- ^ PUBLIC_BODY
    | CTReseller    -- ^ RESELLER
      deriving (Eq, Ord, Read, Show, Generic, Enum)

instance Hashable ContactType

instance FromText ContactType where
    parser = takeLowerText >>= \case
        "association" -> pure CTAssociation
        "company"     -> pure CTCompany
        "person"      -> pure CTPerson
        "public_body" -> pure CTPublicBody
        "reseller"    -> pure CTReseller
        e             -> fail $
            "Failure parsing ContactType from " ++ show e

instance ToText ContactType where
    toText = \case
        CTAssociation -> "ASSOCIATION"
        CTCompany     -> "COMPANY"
        CTPerson      -> "PERSON"
        CTPublicBody  -> "PUBLIC_BODY"
        CTReseller    -> "RESELLER"

instance ToByteString ContactType
instance ToHeader     ContactType
instance ToQuery      ContactType

instance FromJSON ContactType where
    parseJSON = parseJSONText "ContactType"

instance ToJSON ContactType where
    toJSON = toJSONText

data ContactDetail = ContactDetail
    { _cdAddressLine1     :: Maybe Text
    , _cdAddressLine2     :: Maybe Text
    , _cdCity             :: Maybe Text
    , _cdContactType      :: Maybe ContactType
    , _cdCountryCode      :: Maybe CountryCode
    , _cdEmail            :: Maybe Text
    , _cdExtraParams      :: List "ExtraParams" ExtraParam
    , _cdFax              :: Maybe Text
    , _cdFirstName        :: Maybe Text
    , _cdLastName         :: Maybe Text
    , _cdOrganizationName :: Maybe Text
    , _cdPhoneNumber      :: Maybe Text
    , _cdState            :: Maybe Text
    , _cdZipCode          :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'ContactDetail' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdAddressLine1' @::@ 'Maybe' 'Text'
--
-- * 'cdAddressLine2' @::@ 'Maybe' 'Text'
--
-- * 'cdCity' @::@ 'Maybe' 'Text'
--
-- * 'cdContactType' @::@ 'Maybe' 'ContactType'
--
-- * 'cdCountryCode' @::@ 'Maybe' 'CountryCode'
--
-- * 'cdEmail' @::@ 'Maybe' 'Text'
--
-- * 'cdExtraParams' @::@ ['ExtraParam']
--
-- * 'cdFax' @::@ 'Maybe' 'Text'
--
-- * 'cdFirstName' @::@ 'Maybe' 'Text'
--
-- * 'cdLastName' @::@ 'Maybe' 'Text'
--
-- * 'cdOrganizationName' @::@ 'Maybe' 'Text'
--
-- * 'cdPhoneNumber' @::@ 'Maybe' 'Text'
--
-- * 'cdState' @::@ 'Maybe' 'Text'
--
-- * 'cdZipCode' @::@ 'Maybe' 'Text'
--
contactDetail :: ContactDetail
contactDetail = ContactDetail
    { _cdFirstName        = Nothing
    , _cdLastName         = Nothing
    , _cdContactType      = Nothing
    , _cdOrganizationName = Nothing
    , _cdAddressLine1     = Nothing
    , _cdAddressLine2     = Nothing
    , _cdCity             = Nothing
    , _cdState            = Nothing
    , _cdCountryCode      = Nothing
    , _cdZipCode          = Nothing
    , _cdPhoneNumber      = Nothing
    , _cdEmail            = Nothing
    , _cdFax              = Nothing
    , _cdExtraParams      = mempty
    }

-- | First line of the contact's address.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 255 characters.
--
-- Parents: 'RegistrantContact', 'AdminContact', 'TechContact'
--
-- Required: Yes
cdAddressLine1 :: Lens' ContactDetail (Maybe Text)
cdAddressLine1 = lens _cdAddressLine1 (\s a -> s { _cdAddressLine1 = a })

-- | Second line of contact's address, if any.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 255 characters.
--
-- Parents: 'RegistrantContact', 'AdminContact', 'TechContact'
--
-- Required: No
cdAddressLine2 :: Lens' ContactDetail (Maybe Text)
cdAddressLine2 = lens _cdAddressLine2 (\s a -> s { _cdAddressLine2 = a })

-- | The city of the contact's address.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 255 characters.
--
-- Parents: 'RegistrantContact', 'AdminContact', 'TechContact'
--
-- Required: Yes
cdCity :: Lens' ContactDetail (Maybe Text)
cdCity = lens _cdCity (\s a -> s { _cdCity = a })

-- | Indicates whether the contact is a person, company, association, or public
-- organization. If you choose an option other than 'PERSON', you must enter an
-- organization name, and you can't enable privacy protection for the contact.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 255 characters.
--
-- Valid values: 'PERSON' | 'COMPANY' | 'ASSOCIATION' | 'PUBLIC_BODY'
--
-- Parents: 'RegistrantContact', 'AdminContact', 'TechContact'
--
-- Required: Yes
cdContactType :: Lens' ContactDetail (Maybe ContactType)
cdContactType = lens _cdContactType (\s a -> s { _cdContactType = a })

-- | Code for the country of the contact's address.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 255 characters.
--
-- Parents: 'RegistrantContact', 'AdminContact', 'TechContact'
--
-- Required: Yes
cdCountryCode :: Lens' ContactDetail (Maybe CountryCode)
cdCountryCode = lens _cdCountryCode (\s a -> s { _cdCountryCode = a })

-- | Email address of the contact.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 254 characters.
--
-- Parents: 'RegistrantContact', 'AdminContact', 'TechContact'
--
-- Required: Yes
cdEmail :: Lens' ContactDetail (Maybe Text)
cdEmail = lens _cdEmail (\s a -> s { _cdEmail = a })

-- | A list of name-value pairs for parameters required by certain top-level
-- domains.
--
-- Type: Complex
--
-- Default: None
--
-- Parents: 'RegistrantContact', 'AdminContact', 'TechContact'
--
-- Children: 'Name', 'Value'
--
-- Required: No
cdExtraParams :: Lens' ContactDetail [ExtraParam]
cdExtraParams = lens _cdExtraParams (\s a -> s { _cdExtraParams = a }) . _List

-- | Fax number of the contact.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Phone number must be specified in the format "+[country dialing
-- code].[number including any area code]". For example, a US phone number might
-- appear as '"+1.1234567890"'.
--
-- Parents: 'RegistrantContact', 'AdminContact', 'TechContact'
--
-- Required: No
cdFax :: Lens' ContactDetail (Maybe Text)
cdFax = lens _cdFax (\s a -> s { _cdFax = a })

-- | First name of contact.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 255 characters.
--
-- Parents: 'RegistrantContact', 'AdminContact', 'TechContact'
--
-- Required: Yes
cdFirstName :: Lens' ContactDetail (Maybe Text)
cdFirstName = lens _cdFirstName (\s a -> s { _cdFirstName = a })

-- | Last name of contact.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 255 characters.
--
-- Parents: 'RegistrantContact', 'AdminContact', 'TechContact'
--
-- Required: Yes
cdLastName :: Lens' ContactDetail (Maybe Text)
cdLastName = lens _cdLastName (\s a -> s { _cdLastName = a })

-- | Name of the organization for contact types other than 'PERSON'.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 255 characters. Contact type must not be 'PERSON'.
--
-- Parents: 'RegistrantContact', 'AdminContact', 'TechContact'
--
-- Required: No
cdOrganizationName :: Lens' ContactDetail (Maybe Text)
cdOrganizationName =
    lens _cdOrganizationName (\s a -> s { _cdOrganizationName = a })

-- | The phone number of the contact.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Phone number must be specified in the format "+[country dialing
-- code].[number including any area code>]". For example, a US phone number
-- might appear as '"+1.1234567890"'.
--
-- Parents: 'RegistrantContact', 'AdminContact', 'TechContact'
--
-- Required: Yes
cdPhoneNumber :: Lens' ContactDetail (Maybe Text)
cdPhoneNumber = lens _cdPhoneNumber (\s a -> s { _cdPhoneNumber = a })

-- | The state or province of the contact's city.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 255 characters.
--
-- Parents: 'RegistrantContact', 'AdminContact', 'TechContact'
--
-- Required: No
cdState :: Lens' ContactDetail (Maybe Text)
cdState = lens _cdState (\s a -> s { _cdState = a })

-- | The zip or postal code of the contact's address.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 255 characters.
--
-- Parents: 'RegistrantContact', 'AdminContact', 'TechContact'
--
-- Required: No
cdZipCode :: Lens' ContactDetail (Maybe Text)
cdZipCode = lens _cdZipCode (\s a -> s { _cdZipCode = a })

instance FromJSON ContactDetail where
    parseJSON = withObject "ContactDetail" $ \o -> ContactDetail
        <$> o .:? "AddressLine1"
        <*> o .:? "AddressLine2"
        <*> o .:? "City"
        <*> o .:? "ContactType"
        <*> o .:? "CountryCode"
        <*> o .:? "Email"
        <*> o .:? "ExtraParams" .!= mempty
        <*> o .:? "Fax"
        <*> o .:? "FirstName"
        <*> o .:? "LastName"
        <*> o .:? "OrganizationName"
        <*> o .:? "PhoneNumber"
        <*> o .:? "State"
        <*> o .:? "ZipCode"

instance ToJSON ContactDetail where
    toJSON ContactDetail{..} = object
        [ "FirstName"        .= _cdFirstName
        , "LastName"         .= _cdLastName
        , "ContactType"      .= _cdContactType
        , "OrganizationName" .= _cdOrganizationName
        , "AddressLine1"     .= _cdAddressLine1
        , "AddressLine2"     .= _cdAddressLine2
        , "City"             .= _cdCity
        , "State"            .= _cdState
        , "CountryCode"      .= _cdCountryCode
        , "ZipCode"          .= _cdZipCode
        , "PhoneNumber"      .= _cdPhoneNumber
        , "Email"            .= _cdEmail
        , "Fax"              .= _cdFax
        , "ExtraParams"      .= _cdExtraParams
        ]

data OperationSummary = OperationSummary
    { _osOperationId   :: Text
    , _osStatus        :: OperationStatus
    , _osSubmittedDate :: POSIX
    , _osType          :: OperationType
    } deriving (Eq, Read, Show)

-- | 'OperationSummary' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'osOperationId' @::@ 'Text'
--
-- * 'osStatus' @::@ 'OperationStatus'
--
-- * 'osSubmittedDate' @::@ 'UTCTime'
--
-- * 'osType' @::@ 'OperationType'
--
operationSummary :: Text -- ^ 'osOperationId'
                 -> OperationStatus -- ^ 'osStatus'
                 -> OperationType -- ^ 'osType'
                 -> UTCTime -- ^ 'osSubmittedDate'
                 -> OperationSummary
operationSummary p1 p2 p3 p4 = OperationSummary
    { _osOperationId   = p1
    , _osStatus        = p2
    , _osType          = p3
    , _osSubmittedDate = withIso _Time (const id) p4
    }

-- | Identifier returned to track the requested action.
--
-- Type: String
osOperationId :: Lens' OperationSummary Text
osOperationId = lens _osOperationId (\s a -> s { _osOperationId = a })

-- | The current status of the requested operation in the system.
--
-- Type: String
osStatus :: Lens' OperationSummary OperationStatus
osStatus = lens _osStatus (\s a -> s { _osStatus = a })

-- | The date when the request was submitted.
osSubmittedDate :: Lens' OperationSummary UTCTime
osSubmittedDate = lens _osSubmittedDate (\s a -> s { _osSubmittedDate = a }) . _Time

-- | Type of the action requested.
--
-- Type: String
--
-- Valid values: 'REGISTER_DOMAIN' | 'DELETE_DOMAIN' | 'TRANSFER_IN_DOMAIN' | 'UPDATE_DOMAIN_CONTACT' | 'UPDATE_NAMESERVER' | 'CHANGE_PRIVACY_PROTECTION' | 'DOMAIN_LOCK'
osType :: Lens' OperationSummary OperationType
osType = lens _osType (\s a -> s { _osType = a })

instance FromJSON OperationSummary where
    parseJSON = withObject "OperationSummary" $ \o -> OperationSummary
        <$> o .:  "OperationId"
        <*> o .:  "Status"
        <*> o .:  "SubmittedDate"
        <*> o .:  "Type"

instance ToJSON OperationSummary where
    toJSON OperationSummary{..} = object
        [ "OperationId"   .= _osOperationId
        , "Status"        .= _osStatus
        , "Type"          .= _osType
        , "SubmittedDate" .= _osSubmittedDate
        ]
