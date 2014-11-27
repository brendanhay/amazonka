{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

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

import Data.Char (isUpper)
import Network.AWS.Error
import Network.AWS.Prelude
import Network.AWS.Signing.V4
import qualified GHC.Exts

-- | Version @2014-05-15@ of the Amazon Route 53 Domains service.
data Route53Domains

instance AWSService Route53Domains where
    type Sg Route53Domains = V4
    type Er Route53Domains = JSONError

    service = Service
        { _svcAbbrev       = "Route53Domains"
        , _svcPrefix       = "route53domains"
        , _svcVersion      = "2014-05-15"
        , _svcTargetPrefix = Just "Route53Domains_v20140515"
        , _svcJSONVersion  = Just "1.1"
        }

    handle = jsonError statusSuccess

data DomainSummary = DomainSummary
    { _dsAutoRenew    :: Maybe Bool
    , _dsDomainName   :: Text
    , _dsExpiry       :: Maybe ISO8601
    , _dsTransferLock :: Maybe Bool
    } deriving (Eq, Ord, Show)

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
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ExtraParamName

instance FromText ExtraParamName where
    parser = takeText >>= \case
        "AU_ID_NUMBER"             -> pure AuIdNumber
        "AU_ID_TYPE"               -> pure AuIdType
        "BIRTH_CITY"               -> pure BirthCity
        "BIRTH_COUNTRY"            -> pure BirthCountry
        "BIRTH_DATE_IN_YYYY_MM_DD" -> pure BirthDateInYyyyMmDd
        "BIRTH_DEPARTMENT"         -> pure BirthDepartment
        "BRAND_NUMBER"             -> pure BrandNumber
        "CA_LEGAL_TYPE"            -> pure CaLegalType
        "DOCUMENT_NUMBER"          -> pure DocumentNumber
        "DUNS_NUMBER"              -> pure DunsNumber
        "FI_BUSINESS_NUMBER"       -> pure FiBusinessNumber
        "FI_ID_NUMBER"             -> pure FiIdNumber
        "IT_PIN"                   -> pure ItPin
        "RU_PASSPORT_DATA"         -> pure RuPassportData
        "SE_ID_NUMBER"             -> pure SeIdNumber
        "SG_ID_NUMBER"             -> pure SgIdNumber
        "VAT_NUMBER"               -> pure VatNumber
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
    } deriving (Eq, Ord, Show)

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
        <$> o .:  "GlueIps"
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
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable OperationStatus

instance FromText OperationStatus where
    parser = takeText >>= \case
        "ERROR"       -> pure Error
        "FAILED"      -> pure Failed
        "IN_PROGRESS" -> pure InProgress
        "SUBMITTED"   -> pure Submitted
        "SUCCESSFUL"  -> pure Successful
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
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable DomainAvailability

instance FromText DomainAvailability where
    parser = takeText >>= \case
        "AVAILABLE"              -> pure Available
        "AVAILABLE_PREORDER"     -> pure AvailablePreorder
        "AVAILABLE_RESERVED"     -> pure AvailableReserved
        "RESERVED"               -> pure Reserved
        "UNAVAILABLE"            -> pure Unavailable
        "UNAVAILABLE_PREMIUM"    -> pure UnavailablePremium
        "UNAVAILABLE_RESTRICTED" -> pure UnavailableRestricted
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
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable OperationType

instance FromText OperationType where
    parser = takeText >>= \case
        "CHANGE_PRIVACY_PROTECTION" -> pure OTChangePrivacyProtection
        "DELETE_DOMAIN"             -> pure OTDeleteDomain
        "DOMAIN_LOCK"               -> pure OTDomainLock
        "REGISTER_DOMAIN"           -> pure OTRegisterDomain
        "TRANSFER_IN_DOMAIN"        -> pure OTTransferInDomain
        "UPDATE_DOMAIN_CONTACT"     -> pure OTUpdateDomainContact
        "UPDATE_NAMESERVER"         -> pure OTUpdateNameserver
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
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable CountryCode

instance FromText CountryCode where
    parser = takeText >>= \case
        "AD" -> pure Ad
        "AE" -> pure Ae
        "AF" -> pure Af
        "AG" -> pure Ag
        "AI" -> pure Ai
        "AL" -> pure Al
        "AM" -> pure Am
        "AN" -> pure An
        "AO" -> pure Ao
        "AQ" -> pure Aq
        "AR" -> pure Ar
        "AS" -> pure As
        "AT" -> pure At
        "AU" -> pure Au
        "AW" -> pure Aw
        "AZ" -> pure Az
        "BA" -> pure Ba
        "BB" -> pure Bb
        "BD" -> pure Bd
        "BE" -> pure Be
        "BF" -> pure Bf
        "BG" -> pure Bg
        "BH" -> pure Bh
        "BI" -> pure Bi
        "BJ" -> pure Bj
        "BL" -> pure Bl
        "BM" -> pure Bm
        "BN" -> pure Bn
        "BO" -> pure Bo
        "BR" -> pure Br
        "BS" -> pure Bs
        "BT" -> pure Bt
        "BW" -> pure Bw
        "BY" -> pure By
        "BZ" -> pure Bz
        "CA" -> pure Ca
        "CC" -> pure Cc
        "CD" -> pure Cd
        "CF" -> pure Cf
        "CG" -> pure Cg
        "CH" -> pure Ch
        "CI" -> pure Ci
        "CK" -> pure Ck
        "CL" -> pure Cl
        "CM" -> pure Cm
        "CN" -> pure Cn
        "CO" -> pure Co
        "CR" -> pure Cr
        "CU" -> pure Cu
        "CV" -> pure Cv
        "CX" -> pure Cx
        "CY" -> pure Cy
        "CZ" -> pure Cz
        "DE" -> pure De
        "DJ" -> pure Dj
        "DK" -> pure Dk
        "DM" -> pure Dm
        "DO" -> pure Do'
        "DZ" -> pure Dz
        "EC" -> pure Ec
        "EE" -> pure Ee
        "EG" -> pure Eg
        "ER" -> pure Er
        "ES" -> pure Es
        "ET" -> pure Et
        "FI" -> pure Fi
        "FJ" -> pure Fj
        "FK" -> pure Fk
        "FM" -> pure Fm
        "FO" -> pure Fo
        "FR" -> pure Fr
        "GA" -> pure Ga
        "GB" -> pure Gb
        "GD" -> pure Gd
        "GE" -> pure Ge
        "GH" -> pure Gh
        "GI" -> pure Gi
        "GL" -> pure Gl
        "GM" -> pure Gm
        "GN" -> pure Gn
        "GQ" -> pure Gq
        "GR" -> pure Gr
        "GT" -> pure Gt
        "GU" -> pure Gu
        "GW" -> pure Gw
        "GY" -> pure Gy
        "HK" -> pure Hk
        "HN" -> pure Hn
        "HR" -> pure Hr
        "HT" -> pure Ht
        "HU" -> pure Hu
        "ID" -> pure Id
        "IE" -> pure Ie
        "IL" -> pure Il
        "IM" -> pure Im
        "IN" -> pure In'
        "IQ" -> pure Iq
        "IR" -> pure Ir
        "IS" -> pure Is
        "IT" -> pure It
        "JM" -> pure Jm
        "JO" -> pure Jo
        "JP" -> pure Jp
        "KE" -> pure Ke
        "KG" -> pure Kg
        "KH" -> pure Kh
        "KI" -> pure Ki
        "KM" -> pure Km
        "KN" -> pure Kn
        "KP" -> pure Kp
        "KR" -> pure Kr
        "KW" -> pure Kw
        "KY" -> pure Ky
        "KZ" -> pure Kz
        "LA" -> pure La
        "LB" -> pure Lb
        "LC" -> pure Lc
        "LI" -> pure Li
        "LK" -> pure Lk
        "LR" -> pure Lr
        "LS" -> pure Ls
        "LT" -> pure Lt
        "LU" -> pure Lu
        "LV" -> pure Lv
        "LY" -> pure Ly
        "MA" -> pure Ma
        "MC" -> pure Mc
        "MD" -> pure Md
        "ME" -> pure Me
        "MF" -> pure Mf
        "MG" -> pure Mg
        "MH" -> pure Mh
        "MK" -> pure Mk
        "ML" -> pure Ml
        "MM" -> pure Mm
        "MN" -> pure Mn
        "MO" -> pure Mo
        "MP" -> pure Mp
        "MR" -> pure Mr
        "MS" -> pure Ms
        "MT" -> pure Mt
        "MU" -> pure Mu
        "MV" -> pure Mv
        "MW" -> pure Mw
        "MX" -> pure Mx
        "MY" -> pure My
        "MZ" -> pure Mz
        "NA" -> pure Na
        "NC" -> pure Nc
        "NE" -> pure Ne
        "NG" -> pure Ng
        "NI" -> pure Ni
        "NL" -> pure Nl
        "NO" -> pure No
        "NP" -> pure Np
        "NR" -> pure Nr
        "NU" -> pure Nu
        "NZ" -> pure Nz
        "OM" -> pure Om
        "PA" -> pure Pa
        "PE" -> pure Pe
        "PF" -> pure Pf
        "PG" -> pure Pg
        "PH" -> pure Ph
        "PK" -> pure Pk
        "PL" -> pure Pl
        "PM" -> pure Pm
        "PN" -> pure Pn
        "PR" -> pure Pr
        "PT" -> pure Pt
        "PW" -> pure Pw
        "PY" -> pure Py
        "QA" -> pure Qa
        "RO" -> pure Ro
        "RS" -> pure Rs
        "RU" -> pure Ru
        "RW" -> pure Rw
        "SA" -> pure Sa
        "SB" -> pure Sb
        "SC" -> pure Sc
        "SD" -> pure Sd
        "SE" -> pure Se
        "SG" -> pure Sg
        "SH" -> pure Sh
        "SI" -> pure Si
        "SK" -> pure Sk
        "SL" -> pure Sl
        "SM" -> pure Sm
        "SN" -> pure Sn
        "SO" -> pure So
        "SR" -> pure Sr
        "ST" -> pure St
        "SV" -> pure Sv
        "SY" -> pure Sy
        "SZ" -> pure Sz
        "TC" -> pure Tc
        "TD" -> pure Td
        "TG" -> pure Tg
        "TH" -> pure Th
        "TJ" -> pure Tj
        "TK" -> pure Tk
        "TL" -> pure Tl
        "TM" -> pure Tm
        "TN" -> pure Tn
        "TO" -> pure To
        "TR" -> pure Tr
        "TT" -> pure Tt
        "TV" -> pure Tv
        "TW" -> pure Tw
        "TZ" -> pure Tz
        "UA" -> pure Ua
        "UG" -> pure Ug
        "US" -> pure Us
        "UY" -> pure Uy
        "UZ" -> pure Uz
        "VA" -> pure Va
        "VC" -> pure Vc
        "VE" -> pure Ve
        "VG" -> pure Vg
        "VI" -> pure Vi
        "VN" -> pure Vn
        "VU" -> pure Vu
        "WF" -> pure Wf
        "WS" -> pure Ws
        "YE" -> pure Ye
        "YT" -> pure Yt
        "ZA" -> pure Za
        "ZM" -> pure Zm
        "ZW" -> pure Zw
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
    } deriving (Eq, Show)

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
      deriving (Eq, Ord, Show, Generic, Enum)

instance Hashable ContactType

instance FromText ContactType where
    parser = takeText >>= \case
        "ASSOCIATION" -> pure CTAssociation
        "COMPANY"     -> pure CTCompany
        "PERSON"      -> pure CTPerson
        "PUBLIC_BODY" -> pure CTPublicBody
        "RESELLER"    -> pure CTReseller
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
    } deriving (Eq, Show)

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
        <*> o .:  "ExtraParams"
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
    , _osSubmittedDate :: ISO8601
    , _osType          :: OperationType
    } deriving (Eq, Show)

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
