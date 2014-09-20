{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
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

-- | Pending.
module Network.AWS.Route53Domains.Types
    (
    -- * Service
      Route53Domains
    -- ** Errors
    , Route53DomainsError (..)
    , _DomainLimitExceeded
    , _DuplicateRequest
    , _InvalidInput
    , _OperationLimitExceeded
    , _Route53DomainsClient
    , _Route53DomainsSerializer
    , _Route53DomainsService
    , _TLDRulesViolation
    , _UnsupportedTLD

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
    , cdFirstName
    , cdLastName
    , cdContactType
    , cdOrganizationName
    , cdAddressLine1
    , cdAddressLine2
    , cdCity
    , cdState
    , cdCountryCode
    , cdZipCode
    , cdPhoneNumber
    , cdEmail
    , cdFax
    , cdExtraParams

    -- * DomainSummary
    , DomainSummary
    , domainSummary
    , dsDomainName
    , dsAutoRenew
    , dsTransferLock
    , dsExpiry

    -- * ExtraParam
    , ExtraParam
    , extraParam
    , epName
    , epValue

    -- * Nameserver
    , Nameserver
    , nameserver
    , nName
    , nGlueIps

    -- * OperationSummary
    , OperationSummary
    , operationSummary
    , osOperationId
    , osStatus
    , osType
    , osSubmittedDate
    ) where

import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2014-05-15@) of the
-- @Amazon Route 53 Domains@ service.
data Route53Domains deriving (Typeable)

instance AWSService Route53Domains where
    type Sg Route53Domains = V4
    type Er Route53Domains = Route53DomainsError

    service = Service
        { _svcEndpoint = Regional
        , _svcPrefix   = "route53domains"
        , _svcVersion  = "2014-05-15"
        , _svcTarget   = Nothing
        }

-- | A sum type representing possible errors returned by the 'Route53Domains' service.
--
-- These typically include 'HTTPException's thrown by the underlying HTTP
-- mechanisms, serialisation errors, and typed errors as specified by the
-- service description where applicable.
data Route53DomainsError
      -- | The number of domains has exceeded the allowed threshold for the
      -- account.
    = DomainLimitExceeded
        { _dleMessage :: Maybe Text
        }
      -- | The request is already in progress for the domain.
    | DuplicateRequest
        { _drMessage :: Maybe Text
        }
      -- | The requested item is not acceptable. For example, for an
      -- OperationId it may refer to the ID of an operation that is
      -- already completed. For a domain name, it may not be a valid
      -- domain name or belong to the requester account.
    | InvalidInput
        { _iiMessage :: Maybe Text
        }
      -- | The number of operations or jobs running exceeded the allowed
      -- threshold for the account.
    | OperationLimitExceeded
        { _oleMessage :: Maybe Text
        }
    | Route53DomainsClient HttpException
    | Route53DomainsSerializer String
    | Route53DomainsService String
      -- | The top-level domain does not support this operation.
    | TLDRulesViolation
        { _tldrvMessage :: Maybe Text
        }
      -- | Amazon Route 53 does not support this top-level domain.
    | UnsupportedTLD
        { _utldMessage :: Maybe Text
        }
      deriving (Show, Typeable, Generic)

instance AWSError Route53DomainsError where
    awsError = const "Route53DomainsError"

instance AWSServiceError Route53DomainsError where
    serviceError    = Route53DomainsService
    clientError     = Route53DomainsClient
    serializerError = Route53DomainsSerializer

instance Exception Route53DomainsError

-- | The number of domains has exceeded the allowed threshold for the account.
--
-- See: 'DomainLimitExceeded'
_DomainLimitExceeded :: Prism' Route53DomainsError (Maybe Text)
_DomainLimitExceeded = prism
    DomainLimitExceeded
    (\case
        DomainLimitExceeded p1 -> Right p1
        x -> Left x)

-- | The request is already in progress for the domain.
--
-- See: 'DuplicateRequest'
_DuplicateRequest :: Prism' Route53DomainsError (Maybe Text)
_DuplicateRequest = prism
    DuplicateRequest
    (\case
        DuplicateRequest p1 -> Right p1
        x -> Left x)

-- | The requested item is not acceptable. For example, for an OperationId it
-- may refer to the ID of an operation that is already completed. For a domain
-- name, it may not be a valid domain name or belong to the requester account.
--
-- See: 'InvalidInput'
_InvalidInput :: Prism' Route53DomainsError (Maybe Text)
_InvalidInput = prism
    InvalidInput
    (\case
        InvalidInput p1 -> Right p1
        x -> Left x)

-- | The number of operations or jobs running exceeded the allowed threshold for
-- the account.
--
-- See: 'OperationLimitExceeded'
_OperationLimitExceeded :: Prism' Route53DomainsError (Maybe Text)
_OperationLimitExceeded = prism
    OperationLimitExceeded
    (\case
        OperationLimitExceeded p1 -> Right p1
        x -> Left x)

-- | See: 'Route53DomainsClient'
_Route53DomainsClient :: Prism' Route53DomainsError HttpException
_Route53DomainsClient = prism
    Route53DomainsClient
    (\case
        Route53DomainsClient p1 -> Right p1
        x -> Left x)

-- | See: 'Route53DomainsSerializer'
_Route53DomainsSerializer :: Prism' Route53DomainsError String
_Route53DomainsSerializer = prism
    Route53DomainsSerializer
    (\case
        Route53DomainsSerializer p1 -> Right p1
        x -> Left x)

-- | See: 'Route53DomainsService'
_Route53DomainsService :: Prism' Route53DomainsError String
_Route53DomainsService = prism
    Route53DomainsService
    (\case
        Route53DomainsService p1 -> Right p1
        x -> Left x)

-- | The top-level domain does not support this operation.
--
-- See: 'TLDRulesViolation'
_TLDRulesViolation :: Prism' Route53DomainsError (Maybe Text)
_TLDRulesViolation = prism
    TLDRulesViolation
    (\case
        TLDRulesViolation p1 -> Right p1
        x -> Left x)

-- | Amazon Route 53 does not support this top-level domain.
--
-- See: 'UnsupportedTLD'
_UnsupportedTLD :: Prism' Route53DomainsError (Maybe Text)
_UnsupportedTLD = prism
    UnsupportedTLD
    (\case
        UnsupportedTLD p1 -> Right p1
        x -> Left x)

data ContactType
    = ContactTypeAssociation -- ^ ASSOCIATION
    | ContactTypeCompany -- ^ COMPANY
    | ContactTypePerson -- ^ PERSON
    | ContactTypePublicBody -- ^ PUBLIC_BODY
    | ContactTypeReseller -- ^ RESELLER
      deriving (Eq, Show, Generic)

instance Hashable ContactType

instance FromText ContactType where
    parser = match "ASSOCIATION" ContactTypeAssociation
         <|> match "COMPANY" ContactTypeCompany
         <|> match "PERSON" ContactTypePerson
         <|> match "PUBLIC_BODY" ContactTypePublicBody
         <|> match "RESELLER" ContactTypeReseller

instance ToText ContactType where
    toText ContactTypeAssociation = "ASSOCIATION"
    toText ContactTypeCompany = "COMPANY"
    toText ContactTypePerson = "PERSON"
    toText ContactTypePublicBody = "PUBLIC_BODY"
    toText ContactTypeReseller = "RESELLER"

instance ToByteString ContactType where
    toBS ContactTypeAssociation = "ASSOCIATION"
    toBS ContactTypeCompany = "COMPANY"
    toBS ContactTypePerson = "PERSON"
    toBS ContactTypePublicBody = "PUBLIC_BODY"
    toBS ContactTypeReseller = "RESELLER"

instance ToHeader ContactType where
    toHeader k = toHeader k . toBS

instance ToQuery ContactType where
    toQuery = toQuery . toBS

instance FromJSON ContactType

instance ToJSON ContactType

data CountryCode
    = CountryCodeAd -- ^ AD
    | CountryCodeAe -- ^ AE
    | CountryCodeAf -- ^ AF
    | CountryCodeAg -- ^ AG
    | CountryCodeAi -- ^ AI
    | CountryCodeAl -- ^ AL
    | CountryCodeAm -- ^ AM
    | CountryCodeAn -- ^ AN
    | CountryCodeAo -- ^ AO
    | CountryCodeAq -- ^ AQ
    | CountryCodeAr -- ^ AR
    | CountryCodeAs -- ^ AS
    | CountryCodeAt -- ^ AT
    | CountryCodeAu -- ^ AU
    | CountryCodeAw -- ^ AW
    | CountryCodeAz -- ^ AZ
    | CountryCodeBa -- ^ BA
    | CountryCodeBb -- ^ BB
    | CountryCodeBd -- ^ BD
    | CountryCodeBe -- ^ BE
    | CountryCodeBf -- ^ BF
    | CountryCodeBg -- ^ BG
    | CountryCodeBh -- ^ BH
    | CountryCodeBi -- ^ BI
    | CountryCodeBj -- ^ BJ
    | CountryCodeBl -- ^ BL
    | CountryCodeBm -- ^ BM
    | CountryCodeBn -- ^ BN
    | CountryCodeBo -- ^ BO
    | CountryCodeBr -- ^ BR
    | CountryCodeBs -- ^ BS
    | CountryCodeBt -- ^ BT
    | CountryCodeBw -- ^ BW
    | CountryCodeBy -- ^ BY
    | CountryCodeBz -- ^ BZ
    | CountryCodeCa -- ^ CA
    | CountryCodeCc -- ^ CC
    | CountryCodeCd -- ^ CD
    | CountryCodeCf -- ^ CF
    | CountryCodeCg -- ^ CG
    | CountryCodeCh -- ^ CH
    | CountryCodeCi -- ^ CI
    | CountryCodeCk -- ^ CK
    | CountryCodeCl -- ^ CL
    | CountryCodeCm -- ^ CM
    | CountryCodeCn -- ^ CN
    | CountryCodeCo -- ^ CO
    | CountryCodeCr -- ^ CR
    | CountryCodeCu -- ^ CU
    | CountryCodeCv -- ^ CV
    | CountryCodeCx -- ^ CX
    | CountryCodeCy -- ^ CY
    | CountryCodeCz -- ^ CZ
    | CountryCodeDe -- ^ DE
    | CountryCodeDj -- ^ DJ
    | CountryCodeDk -- ^ DK
    | CountryCodeDm -- ^ DM
    | CountryCodeDo -- ^ DO
    | CountryCodeDz -- ^ DZ
    | CountryCodeEc -- ^ EC
    | CountryCodeEe -- ^ EE
    | CountryCodeEg -- ^ EG
    | CountryCodeEr -- ^ ER
    | CountryCodeEs -- ^ ES
    | CountryCodeEt -- ^ ET
    | CountryCodeFi -- ^ FI
    | CountryCodeFj -- ^ FJ
    | CountryCodeFk -- ^ FK
    | CountryCodeFm -- ^ FM
    | CountryCodeFo -- ^ FO
    | CountryCodeFr -- ^ FR
    | CountryCodeGa -- ^ GA
    | CountryCodeGb -- ^ GB
    | CountryCodeGd -- ^ GD
    | CountryCodeGe -- ^ GE
    | CountryCodeGh -- ^ GH
    | CountryCodeGi -- ^ GI
    | CountryCodeGl -- ^ GL
    | CountryCodeGm -- ^ GM
    | CountryCodeGn -- ^ GN
    | CountryCodeGq -- ^ GQ
    | CountryCodeGr -- ^ GR
    | CountryCodeGt -- ^ GT
    | CountryCodeGu -- ^ GU
    | CountryCodeGw -- ^ GW
    | CountryCodeGy -- ^ GY
    | CountryCodeHk -- ^ HK
    | CountryCodeHn -- ^ HN
    | CountryCodeHr -- ^ HR
    | CountryCodeHt -- ^ HT
    | CountryCodeHu -- ^ HU
    | CountryCodeId -- ^ ID
    | CountryCodeIe -- ^ IE
    | CountryCodeIl -- ^ IL
    | CountryCodeIm -- ^ IM
    | CountryCodeIn -- ^ IN
    | CountryCodeIq -- ^ IQ
    | CountryCodeIr -- ^ IR
    | CountryCodeIs -- ^ IS
    | CountryCodeIt -- ^ IT
    | CountryCodeJm -- ^ JM
    | CountryCodeJo -- ^ JO
    | CountryCodeJp -- ^ JP
    | CountryCodeKe -- ^ KE
    | CountryCodeKg -- ^ KG
    | CountryCodeKh -- ^ KH
    | CountryCodeKi -- ^ KI
    | CountryCodeKm -- ^ KM
    | CountryCodeKn -- ^ KN
    | CountryCodeKp -- ^ KP
    | CountryCodeKr -- ^ KR
    | CountryCodeKw -- ^ KW
    | CountryCodeKy -- ^ KY
    | CountryCodeKz -- ^ KZ
    | CountryCodeLa -- ^ LA
    | CountryCodeLb -- ^ LB
    | CountryCodeLc -- ^ LC
    | CountryCodeLi -- ^ LI
    | CountryCodeLk -- ^ LK
    | CountryCodeLr -- ^ LR
    | CountryCodeLs -- ^ LS
    | CountryCodeLt -- ^ LT
    | CountryCodeLu -- ^ LU
    | CountryCodeLv -- ^ LV
    | CountryCodeLy -- ^ LY
    | CountryCodeMa -- ^ MA
    | CountryCodeMc -- ^ MC
    | CountryCodeMd -- ^ MD
    | CountryCodeMe -- ^ ME
    | CountryCodeMf -- ^ MF
    | CountryCodeMg -- ^ MG
    | CountryCodeMh -- ^ MH
    | CountryCodeMk -- ^ MK
    | CountryCodeMl -- ^ ML
    | CountryCodeMm -- ^ MM
    | CountryCodeMn -- ^ MN
    | CountryCodeMo -- ^ MO
    | CountryCodeMp -- ^ MP
    | CountryCodeMr -- ^ MR
    | CountryCodeMs -- ^ MS
    | CountryCodeMt -- ^ MT
    | CountryCodeMu -- ^ MU
    | CountryCodeMv -- ^ MV
    | CountryCodeMw -- ^ MW
    | CountryCodeMx -- ^ MX
    | CountryCodeMy -- ^ MY
    | CountryCodeMz -- ^ MZ
    | CountryCodeNa -- ^ NA
    | CountryCodeNc -- ^ NC
    | CountryCodeNe -- ^ NE
    | CountryCodeNg -- ^ NG
    | CountryCodeNi -- ^ NI
    | CountryCodeNl -- ^ NL
    | CountryCodeNo -- ^ NO
    | CountryCodeNp -- ^ NP
    | CountryCodeNr -- ^ NR
    | CountryCodeNu -- ^ NU
    | CountryCodeNz -- ^ NZ
    | CountryCodeOm -- ^ OM
    | CountryCodePa -- ^ PA
    | CountryCodePe -- ^ PE
    | CountryCodePf -- ^ PF
    | CountryCodePg -- ^ PG
    | CountryCodePh -- ^ PH
    | CountryCodePk -- ^ PK
    | CountryCodePl -- ^ PL
    | CountryCodePm -- ^ PM
    | CountryCodePn -- ^ PN
    | CountryCodePr -- ^ PR
    | CountryCodePt -- ^ PT
    | CountryCodePw -- ^ PW
    | CountryCodePy -- ^ PY
    | CountryCodeQa -- ^ QA
    | CountryCodeRo -- ^ RO
    | CountryCodeRs -- ^ RS
    | CountryCodeRu -- ^ RU
    | CountryCodeRw -- ^ RW
    | CountryCodeSa -- ^ SA
    | CountryCodeSb -- ^ SB
    | CountryCodeSc -- ^ SC
    | CountryCodeSd -- ^ SD
    | CountryCodeSe -- ^ SE
    | CountryCodeSg -- ^ SG
    | CountryCodeSh -- ^ SH
    | CountryCodeSi -- ^ SI
    | CountryCodeSk -- ^ SK
    | CountryCodeSl -- ^ SL
    | CountryCodeSm -- ^ SM
    | CountryCodeSn -- ^ SN
    | CountryCodeSo -- ^ SO
    | CountryCodeSr -- ^ SR
    | CountryCodeSt -- ^ ST
    | CountryCodeSv -- ^ SV
    | CountryCodeSy -- ^ SY
    | CountryCodeSz -- ^ SZ
    | CountryCodeTc -- ^ TC
    | CountryCodeTd -- ^ TD
    | CountryCodeTg -- ^ TG
    | CountryCodeTh -- ^ TH
    | CountryCodeTj -- ^ TJ
    | CountryCodeTk -- ^ TK
    | CountryCodeTl -- ^ TL
    | CountryCodeTm -- ^ TM
    | CountryCodeTn -- ^ TN
    | CountryCodeTo -- ^ TO
    | CountryCodeTr -- ^ TR
    | CountryCodeTt -- ^ TT
    | CountryCodeTv -- ^ TV
    | CountryCodeTw -- ^ TW
    | CountryCodeTz -- ^ TZ
    | CountryCodeUa -- ^ UA
    | CountryCodeUg -- ^ UG
    | CountryCodeUs -- ^ US
    | CountryCodeUy -- ^ UY
    | CountryCodeUz -- ^ UZ
    | CountryCodeVa -- ^ VA
    | CountryCodeVc -- ^ VC
    | CountryCodeVe -- ^ VE
    | CountryCodeVg -- ^ VG
    | CountryCodeVi -- ^ VI
    | CountryCodeVn -- ^ VN
    | CountryCodeVu -- ^ VU
    | CountryCodeWf -- ^ WF
    | CountryCodeWs -- ^ WS
    | CountryCodeYe -- ^ YE
    | CountryCodeYt -- ^ YT
    | CountryCodeZa -- ^ ZA
    | CountryCodeZm -- ^ ZM
    | CountryCodeZw -- ^ ZW
      deriving (Eq, Show, Generic)

instance Hashable CountryCode

instance FromText CountryCode where
    parser = match "AD" CountryCodeAd
         <|> match "AE" CountryCodeAe
         <|> match "AF" CountryCodeAf
         <|> match "AG" CountryCodeAg
         <|> match "AI" CountryCodeAi
         <|> match "AL" CountryCodeAl
         <|> match "AM" CountryCodeAm
         <|> match "AN" CountryCodeAn
         <|> match "AO" CountryCodeAo
         <|> match "AQ" CountryCodeAq
         <|> match "AR" CountryCodeAr
         <|> match "AS" CountryCodeAs
         <|> match "AT" CountryCodeAt
         <|> match "AU" CountryCodeAu
         <|> match "AW" CountryCodeAw
         <|> match "AZ" CountryCodeAz
         <|> match "BA" CountryCodeBa
         <|> match "BB" CountryCodeBb
         <|> match "BD" CountryCodeBd
         <|> match "BE" CountryCodeBe
         <|> match "BF" CountryCodeBf
         <|> match "BG" CountryCodeBg
         <|> match "BH" CountryCodeBh
         <|> match "BI" CountryCodeBi
         <|> match "BJ" CountryCodeBj
         <|> match "BL" CountryCodeBl
         <|> match "BM" CountryCodeBm
         <|> match "BN" CountryCodeBn
         <|> match "BO" CountryCodeBo
         <|> match "BR" CountryCodeBr
         <|> match "BS" CountryCodeBs
         <|> match "BT" CountryCodeBt
         <|> match "BW" CountryCodeBw
         <|> match "BY" CountryCodeBy
         <|> match "BZ" CountryCodeBz
         <|> match "CA" CountryCodeCa
         <|> match "CC" CountryCodeCc
         <|> match "CD" CountryCodeCd
         <|> match "CF" CountryCodeCf
         <|> match "CG" CountryCodeCg
         <|> match "CH" CountryCodeCh
         <|> match "CI" CountryCodeCi
         <|> match "CK" CountryCodeCk
         <|> match "CL" CountryCodeCl
         <|> match "CM" CountryCodeCm
         <|> match "CN" CountryCodeCn
         <|> match "CO" CountryCodeCo
         <|> match "CR" CountryCodeCr
         <|> match "CU" CountryCodeCu
         <|> match "CV" CountryCodeCv
         <|> match "CX" CountryCodeCx
         <|> match "CY" CountryCodeCy
         <|> match "CZ" CountryCodeCz
         <|> match "DE" CountryCodeDe
         <|> match "DJ" CountryCodeDj
         <|> match "DK" CountryCodeDk
         <|> match "DM" CountryCodeDm
         <|> match "DO" CountryCodeDo
         <|> match "DZ" CountryCodeDz
         <|> match "EC" CountryCodeEc
         <|> match "EE" CountryCodeEe
         <|> match "EG" CountryCodeEg
         <|> match "ER" CountryCodeEr
         <|> match "ES" CountryCodeEs
         <|> match "ET" CountryCodeEt
         <|> match "FI" CountryCodeFi
         <|> match "FJ" CountryCodeFj
         <|> match "FK" CountryCodeFk
         <|> match "FM" CountryCodeFm
         <|> match "FO" CountryCodeFo
         <|> match "FR" CountryCodeFr
         <|> match "GA" CountryCodeGa
         <|> match "GB" CountryCodeGb
         <|> match "GD" CountryCodeGd
         <|> match "GE" CountryCodeGe
         <|> match "GH" CountryCodeGh
         <|> match "GI" CountryCodeGi
         <|> match "GL" CountryCodeGl
         <|> match "GM" CountryCodeGm
         <|> match "GN" CountryCodeGn
         <|> match "GQ" CountryCodeGq
         <|> match "GR" CountryCodeGr
         <|> match "GT" CountryCodeGt
         <|> match "GU" CountryCodeGu
         <|> match "GW" CountryCodeGw
         <|> match "GY" CountryCodeGy
         <|> match "HK" CountryCodeHk
         <|> match "HN" CountryCodeHn
         <|> match "HR" CountryCodeHr
         <|> match "HT" CountryCodeHt
         <|> match "HU" CountryCodeHu
         <|> match "ID" CountryCodeId
         <|> match "IE" CountryCodeIe
         <|> match "IL" CountryCodeIl
         <|> match "IM" CountryCodeIm
         <|> match "IN" CountryCodeIn
         <|> match "IQ" CountryCodeIq
         <|> match "IR" CountryCodeIr
         <|> match "IS" CountryCodeIs
         <|> match "IT" CountryCodeIt
         <|> match "JM" CountryCodeJm
         <|> match "JO" CountryCodeJo
         <|> match "JP" CountryCodeJp
         <|> match "KE" CountryCodeKe
         <|> match "KG" CountryCodeKg
         <|> match "KH" CountryCodeKh
         <|> match "KI" CountryCodeKi
         <|> match "KM" CountryCodeKm
         <|> match "KN" CountryCodeKn
         <|> match "KP" CountryCodeKp
         <|> match "KR" CountryCodeKr
         <|> match "KW" CountryCodeKw
         <|> match "KY" CountryCodeKy
         <|> match "KZ" CountryCodeKz
         <|> match "LA" CountryCodeLa
         <|> match "LB" CountryCodeLb
         <|> match "LC" CountryCodeLc
         <|> match "LI" CountryCodeLi
         <|> match "LK" CountryCodeLk
         <|> match "LR" CountryCodeLr
         <|> match "LS" CountryCodeLs
         <|> match "LT" CountryCodeLt
         <|> match "LU" CountryCodeLu
         <|> match "LV" CountryCodeLv
         <|> match "LY" CountryCodeLy
         <|> match "MA" CountryCodeMa
         <|> match "MC" CountryCodeMc
         <|> match "MD" CountryCodeMd
         <|> match "ME" CountryCodeMe
         <|> match "MF" CountryCodeMf
         <|> match "MG" CountryCodeMg
         <|> match "MH" CountryCodeMh
         <|> match "MK" CountryCodeMk
         <|> match "ML" CountryCodeMl
         <|> match "MM" CountryCodeMm
         <|> match "MN" CountryCodeMn
         <|> match "MO" CountryCodeMo
         <|> match "MP" CountryCodeMp
         <|> match "MR" CountryCodeMr
         <|> match "MS" CountryCodeMs
         <|> match "MT" CountryCodeMt
         <|> match "MU" CountryCodeMu
         <|> match "MV" CountryCodeMv
         <|> match "MW" CountryCodeMw
         <|> match "MX" CountryCodeMx
         <|> match "MY" CountryCodeMy
         <|> match "MZ" CountryCodeMz
         <|> match "NA" CountryCodeNa
         <|> match "NC" CountryCodeNc
         <|> match "NE" CountryCodeNe
         <|> match "NG" CountryCodeNg
         <|> match "NI" CountryCodeNi
         <|> match "NL" CountryCodeNl
         <|> match "NO" CountryCodeNo
         <|> match "NP" CountryCodeNp
         <|> match "NR" CountryCodeNr
         <|> match "NU" CountryCodeNu
         <|> match "NZ" CountryCodeNz
         <|> match "OM" CountryCodeOm
         <|> match "PA" CountryCodePa
         <|> match "PE" CountryCodePe
         <|> match "PF" CountryCodePf
         <|> match "PG" CountryCodePg
         <|> match "PH" CountryCodePh
         <|> match "PK" CountryCodePk
         <|> match "PL" CountryCodePl
         <|> match "PM" CountryCodePm
         <|> match "PN" CountryCodePn
         <|> match "PR" CountryCodePr
         <|> match "PT" CountryCodePt
         <|> match "PW" CountryCodePw
         <|> match "PY" CountryCodePy
         <|> match "QA" CountryCodeQa
         <|> match "RO" CountryCodeRo
         <|> match "RS" CountryCodeRs
         <|> match "RU" CountryCodeRu
         <|> match "RW" CountryCodeRw
         <|> match "SA" CountryCodeSa
         <|> match "SB" CountryCodeSb
         <|> match "SC" CountryCodeSc
         <|> match "SD" CountryCodeSd
         <|> match "SE" CountryCodeSe
         <|> match "SG" CountryCodeSg
         <|> match "SH" CountryCodeSh
         <|> match "SI" CountryCodeSi
         <|> match "SK" CountryCodeSk
         <|> match "SL" CountryCodeSl
         <|> match "SM" CountryCodeSm
         <|> match "SN" CountryCodeSn
         <|> match "SO" CountryCodeSo
         <|> match "SR" CountryCodeSr
         <|> match "ST" CountryCodeSt
         <|> match "SV" CountryCodeSv
         <|> match "SY" CountryCodeSy
         <|> match "SZ" CountryCodeSz
         <|> match "TC" CountryCodeTc
         <|> match "TD" CountryCodeTd
         <|> match "TG" CountryCodeTg
         <|> match "TH" CountryCodeTh
         <|> match "TJ" CountryCodeTj
         <|> match "TK" CountryCodeTk
         <|> match "TL" CountryCodeTl
         <|> match "TM" CountryCodeTm
         <|> match "TN" CountryCodeTn
         <|> match "TO" CountryCodeTo
         <|> match "TR" CountryCodeTr
         <|> match "TT" CountryCodeTt
         <|> match "TV" CountryCodeTv
         <|> match "TW" CountryCodeTw
         <|> match "TZ" CountryCodeTz
         <|> match "UA" CountryCodeUa
         <|> match "UG" CountryCodeUg
         <|> match "US" CountryCodeUs
         <|> match "UY" CountryCodeUy
         <|> match "UZ" CountryCodeUz
         <|> match "VA" CountryCodeVa
         <|> match "VC" CountryCodeVc
         <|> match "VE" CountryCodeVe
         <|> match "VG" CountryCodeVg
         <|> match "VI" CountryCodeVi
         <|> match "VN" CountryCodeVn
         <|> match "VU" CountryCodeVu
         <|> match "WF" CountryCodeWf
         <|> match "WS" CountryCodeWs
         <|> match "YE" CountryCodeYe
         <|> match "YT" CountryCodeYt
         <|> match "ZA" CountryCodeZa
         <|> match "ZM" CountryCodeZm
         <|> match "ZW" CountryCodeZw

instance ToText CountryCode where
    toText CountryCodeAd = "AD"
    toText CountryCodeAe = "AE"
    toText CountryCodeAf = "AF"
    toText CountryCodeAg = "AG"
    toText CountryCodeAi = "AI"
    toText CountryCodeAl = "AL"
    toText CountryCodeAm = "AM"
    toText CountryCodeAn = "AN"
    toText CountryCodeAo = "AO"
    toText CountryCodeAq = "AQ"
    toText CountryCodeAr = "AR"
    toText CountryCodeAs = "AS"
    toText CountryCodeAt = "AT"
    toText CountryCodeAu = "AU"
    toText CountryCodeAw = "AW"
    toText CountryCodeAz = "AZ"
    toText CountryCodeBa = "BA"
    toText CountryCodeBb = "BB"
    toText CountryCodeBd = "BD"
    toText CountryCodeBe = "BE"
    toText CountryCodeBf = "BF"
    toText CountryCodeBg = "BG"
    toText CountryCodeBh = "BH"
    toText CountryCodeBi = "BI"
    toText CountryCodeBj = "BJ"
    toText CountryCodeBl = "BL"
    toText CountryCodeBm = "BM"
    toText CountryCodeBn = "BN"
    toText CountryCodeBo = "BO"
    toText CountryCodeBr = "BR"
    toText CountryCodeBs = "BS"
    toText CountryCodeBt = "BT"
    toText CountryCodeBw = "BW"
    toText CountryCodeBy = "BY"
    toText CountryCodeBz = "BZ"
    toText CountryCodeCa = "CA"
    toText CountryCodeCc = "CC"
    toText CountryCodeCd = "CD"
    toText CountryCodeCf = "CF"
    toText CountryCodeCg = "CG"
    toText CountryCodeCh = "CH"
    toText CountryCodeCi = "CI"
    toText CountryCodeCk = "CK"
    toText CountryCodeCl = "CL"
    toText CountryCodeCm = "CM"
    toText CountryCodeCn = "CN"
    toText CountryCodeCo = "CO"
    toText CountryCodeCr = "CR"
    toText CountryCodeCu = "CU"
    toText CountryCodeCv = "CV"
    toText CountryCodeCx = "CX"
    toText CountryCodeCy = "CY"
    toText CountryCodeCz = "CZ"
    toText CountryCodeDe = "DE"
    toText CountryCodeDj = "DJ"
    toText CountryCodeDk = "DK"
    toText CountryCodeDm = "DM"
    toText CountryCodeDo = "DO"
    toText CountryCodeDz = "DZ"
    toText CountryCodeEc = "EC"
    toText CountryCodeEe = "EE"
    toText CountryCodeEg = "EG"
    toText CountryCodeEr = "ER"
    toText CountryCodeEs = "ES"
    toText CountryCodeEt = "ET"
    toText CountryCodeFi = "FI"
    toText CountryCodeFj = "FJ"
    toText CountryCodeFk = "FK"
    toText CountryCodeFm = "FM"
    toText CountryCodeFo = "FO"
    toText CountryCodeFr = "FR"
    toText CountryCodeGa = "GA"
    toText CountryCodeGb = "GB"
    toText CountryCodeGd = "GD"
    toText CountryCodeGe = "GE"
    toText CountryCodeGh = "GH"
    toText CountryCodeGi = "GI"
    toText CountryCodeGl = "GL"
    toText CountryCodeGm = "GM"
    toText CountryCodeGn = "GN"
    toText CountryCodeGq = "GQ"
    toText CountryCodeGr = "GR"
    toText CountryCodeGt = "GT"
    toText CountryCodeGu = "GU"
    toText CountryCodeGw = "GW"
    toText CountryCodeGy = "GY"
    toText CountryCodeHk = "HK"
    toText CountryCodeHn = "HN"
    toText CountryCodeHr = "HR"
    toText CountryCodeHt = "HT"
    toText CountryCodeHu = "HU"
    toText CountryCodeId = "ID"
    toText CountryCodeIe = "IE"
    toText CountryCodeIl = "IL"
    toText CountryCodeIm = "IM"
    toText CountryCodeIn = "IN"
    toText CountryCodeIq = "IQ"
    toText CountryCodeIr = "IR"
    toText CountryCodeIs = "IS"
    toText CountryCodeIt = "IT"
    toText CountryCodeJm = "JM"
    toText CountryCodeJo = "JO"
    toText CountryCodeJp = "JP"
    toText CountryCodeKe = "KE"
    toText CountryCodeKg = "KG"
    toText CountryCodeKh = "KH"
    toText CountryCodeKi = "KI"
    toText CountryCodeKm = "KM"
    toText CountryCodeKn = "KN"
    toText CountryCodeKp = "KP"
    toText CountryCodeKr = "KR"
    toText CountryCodeKw = "KW"
    toText CountryCodeKy = "KY"
    toText CountryCodeKz = "KZ"
    toText CountryCodeLa = "LA"
    toText CountryCodeLb = "LB"
    toText CountryCodeLc = "LC"
    toText CountryCodeLi = "LI"
    toText CountryCodeLk = "LK"
    toText CountryCodeLr = "LR"
    toText CountryCodeLs = "LS"
    toText CountryCodeLt = "LT"
    toText CountryCodeLu = "LU"
    toText CountryCodeLv = "LV"
    toText CountryCodeLy = "LY"
    toText CountryCodeMa = "MA"
    toText CountryCodeMc = "MC"
    toText CountryCodeMd = "MD"
    toText CountryCodeMe = "ME"
    toText CountryCodeMf = "MF"
    toText CountryCodeMg = "MG"
    toText CountryCodeMh = "MH"
    toText CountryCodeMk = "MK"
    toText CountryCodeMl = "ML"
    toText CountryCodeMm = "MM"
    toText CountryCodeMn = "MN"
    toText CountryCodeMo = "MO"
    toText CountryCodeMp = "MP"
    toText CountryCodeMr = "MR"
    toText CountryCodeMs = "MS"
    toText CountryCodeMt = "MT"
    toText CountryCodeMu = "MU"
    toText CountryCodeMv = "MV"
    toText CountryCodeMw = "MW"
    toText CountryCodeMx = "MX"
    toText CountryCodeMy = "MY"
    toText CountryCodeMz = "MZ"
    toText CountryCodeNa = "NA"
    toText CountryCodeNc = "NC"
    toText CountryCodeNe = "NE"
    toText CountryCodeNg = "NG"
    toText CountryCodeNi = "NI"
    toText CountryCodeNl = "NL"
    toText CountryCodeNo = "NO"
    toText CountryCodeNp = "NP"
    toText CountryCodeNr = "NR"
    toText CountryCodeNu = "NU"
    toText CountryCodeNz = "NZ"
    toText CountryCodeOm = "OM"
    toText CountryCodePa = "PA"
    toText CountryCodePe = "PE"
    toText CountryCodePf = "PF"
    toText CountryCodePg = "PG"
    toText CountryCodePh = "PH"
    toText CountryCodePk = "PK"
    toText CountryCodePl = "PL"
    toText CountryCodePm = "PM"
    toText CountryCodePn = "PN"
    toText CountryCodePr = "PR"
    toText CountryCodePt = "PT"
    toText CountryCodePw = "PW"
    toText CountryCodePy = "PY"
    toText CountryCodeQa = "QA"
    toText CountryCodeRo = "RO"
    toText CountryCodeRs = "RS"
    toText CountryCodeRu = "RU"
    toText CountryCodeRw = "RW"
    toText CountryCodeSa = "SA"
    toText CountryCodeSb = "SB"
    toText CountryCodeSc = "SC"
    toText CountryCodeSd = "SD"
    toText CountryCodeSe = "SE"
    toText CountryCodeSg = "SG"
    toText CountryCodeSh = "SH"
    toText CountryCodeSi = "SI"
    toText CountryCodeSk = "SK"
    toText CountryCodeSl = "SL"
    toText CountryCodeSm = "SM"
    toText CountryCodeSn = "SN"
    toText CountryCodeSo = "SO"
    toText CountryCodeSr = "SR"
    toText CountryCodeSt = "ST"
    toText CountryCodeSv = "SV"
    toText CountryCodeSy = "SY"
    toText CountryCodeSz = "SZ"
    toText CountryCodeTc = "TC"
    toText CountryCodeTd = "TD"
    toText CountryCodeTg = "TG"
    toText CountryCodeTh = "TH"
    toText CountryCodeTj = "TJ"
    toText CountryCodeTk = "TK"
    toText CountryCodeTl = "TL"
    toText CountryCodeTm = "TM"
    toText CountryCodeTn = "TN"
    toText CountryCodeTo = "TO"
    toText CountryCodeTr = "TR"
    toText CountryCodeTt = "TT"
    toText CountryCodeTv = "TV"
    toText CountryCodeTw = "TW"
    toText CountryCodeTz = "TZ"
    toText CountryCodeUa = "UA"
    toText CountryCodeUg = "UG"
    toText CountryCodeUs = "US"
    toText CountryCodeUy = "UY"
    toText CountryCodeUz = "UZ"
    toText CountryCodeVa = "VA"
    toText CountryCodeVc = "VC"
    toText CountryCodeVe = "VE"
    toText CountryCodeVg = "VG"
    toText CountryCodeVi = "VI"
    toText CountryCodeVn = "VN"
    toText CountryCodeVu = "VU"
    toText CountryCodeWf = "WF"
    toText CountryCodeWs = "WS"
    toText CountryCodeYe = "YE"
    toText CountryCodeYt = "YT"
    toText CountryCodeZa = "ZA"
    toText CountryCodeZm = "ZM"
    toText CountryCodeZw = "ZW"

instance ToByteString CountryCode where
    toBS CountryCodeAd = "AD"
    toBS CountryCodeAe = "AE"
    toBS CountryCodeAf = "AF"
    toBS CountryCodeAg = "AG"
    toBS CountryCodeAi = "AI"
    toBS CountryCodeAl = "AL"
    toBS CountryCodeAm = "AM"
    toBS CountryCodeAn = "AN"
    toBS CountryCodeAo = "AO"
    toBS CountryCodeAq = "AQ"
    toBS CountryCodeAr = "AR"
    toBS CountryCodeAs = "AS"
    toBS CountryCodeAt = "AT"
    toBS CountryCodeAu = "AU"
    toBS CountryCodeAw = "AW"
    toBS CountryCodeAz = "AZ"
    toBS CountryCodeBa = "BA"
    toBS CountryCodeBb = "BB"
    toBS CountryCodeBd = "BD"
    toBS CountryCodeBe = "BE"
    toBS CountryCodeBf = "BF"
    toBS CountryCodeBg = "BG"
    toBS CountryCodeBh = "BH"
    toBS CountryCodeBi = "BI"
    toBS CountryCodeBj = "BJ"
    toBS CountryCodeBl = "BL"
    toBS CountryCodeBm = "BM"
    toBS CountryCodeBn = "BN"
    toBS CountryCodeBo = "BO"
    toBS CountryCodeBr = "BR"
    toBS CountryCodeBs = "BS"
    toBS CountryCodeBt = "BT"
    toBS CountryCodeBw = "BW"
    toBS CountryCodeBy = "BY"
    toBS CountryCodeBz = "BZ"
    toBS CountryCodeCa = "CA"
    toBS CountryCodeCc = "CC"
    toBS CountryCodeCd = "CD"
    toBS CountryCodeCf = "CF"
    toBS CountryCodeCg = "CG"
    toBS CountryCodeCh = "CH"
    toBS CountryCodeCi = "CI"
    toBS CountryCodeCk = "CK"
    toBS CountryCodeCl = "CL"
    toBS CountryCodeCm = "CM"
    toBS CountryCodeCn = "CN"
    toBS CountryCodeCo = "CO"
    toBS CountryCodeCr = "CR"
    toBS CountryCodeCu = "CU"
    toBS CountryCodeCv = "CV"
    toBS CountryCodeCx = "CX"
    toBS CountryCodeCy = "CY"
    toBS CountryCodeCz = "CZ"
    toBS CountryCodeDe = "DE"
    toBS CountryCodeDj = "DJ"
    toBS CountryCodeDk = "DK"
    toBS CountryCodeDm = "DM"
    toBS CountryCodeDo = "DO"
    toBS CountryCodeDz = "DZ"
    toBS CountryCodeEc = "EC"
    toBS CountryCodeEe = "EE"
    toBS CountryCodeEg = "EG"
    toBS CountryCodeEr = "ER"
    toBS CountryCodeEs = "ES"
    toBS CountryCodeEt = "ET"
    toBS CountryCodeFi = "FI"
    toBS CountryCodeFj = "FJ"
    toBS CountryCodeFk = "FK"
    toBS CountryCodeFm = "FM"
    toBS CountryCodeFo = "FO"
    toBS CountryCodeFr = "FR"
    toBS CountryCodeGa = "GA"
    toBS CountryCodeGb = "GB"
    toBS CountryCodeGd = "GD"
    toBS CountryCodeGe = "GE"
    toBS CountryCodeGh = "GH"
    toBS CountryCodeGi = "GI"
    toBS CountryCodeGl = "GL"
    toBS CountryCodeGm = "GM"
    toBS CountryCodeGn = "GN"
    toBS CountryCodeGq = "GQ"
    toBS CountryCodeGr = "GR"
    toBS CountryCodeGt = "GT"
    toBS CountryCodeGu = "GU"
    toBS CountryCodeGw = "GW"
    toBS CountryCodeGy = "GY"
    toBS CountryCodeHk = "HK"
    toBS CountryCodeHn = "HN"
    toBS CountryCodeHr = "HR"
    toBS CountryCodeHt = "HT"
    toBS CountryCodeHu = "HU"
    toBS CountryCodeId = "ID"
    toBS CountryCodeIe = "IE"
    toBS CountryCodeIl = "IL"
    toBS CountryCodeIm = "IM"
    toBS CountryCodeIn = "IN"
    toBS CountryCodeIq = "IQ"
    toBS CountryCodeIr = "IR"
    toBS CountryCodeIs = "IS"
    toBS CountryCodeIt = "IT"
    toBS CountryCodeJm = "JM"
    toBS CountryCodeJo = "JO"
    toBS CountryCodeJp = "JP"
    toBS CountryCodeKe = "KE"
    toBS CountryCodeKg = "KG"
    toBS CountryCodeKh = "KH"
    toBS CountryCodeKi = "KI"
    toBS CountryCodeKm = "KM"
    toBS CountryCodeKn = "KN"
    toBS CountryCodeKp = "KP"
    toBS CountryCodeKr = "KR"
    toBS CountryCodeKw = "KW"
    toBS CountryCodeKy = "KY"
    toBS CountryCodeKz = "KZ"
    toBS CountryCodeLa = "LA"
    toBS CountryCodeLb = "LB"
    toBS CountryCodeLc = "LC"
    toBS CountryCodeLi = "LI"
    toBS CountryCodeLk = "LK"
    toBS CountryCodeLr = "LR"
    toBS CountryCodeLs = "LS"
    toBS CountryCodeLt = "LT"
    toBS CountryCodeLu = "LU"
    toBS CountryCodeLv = "LV"
    toBS CountryCodeLy = "LY"
    toBS CountryCodeMa = "MA"
    toBS CountryCodeMc = "MC"
    toBS CountryCodeMd = "MD"
    toBS CountryCodeMe = "ME"
    toBS CountryCodeMf = "MF"
    toBS CountryCodeMg = "MG"
    toBS CountryCodeMh = "MH"
    toBS CountryCodeMk = "MK"
    toBS CountryCodeMl = "ML"
    toBS CountryCodeMm = "MM"
    toBS CountryCodeMn = "MN"
    toBS CountryCodeMo = "MO"
    toBS CountryCodeMp = "MP"
    toBS CountryCodeMr = "MR"
    toBS CountryCodeMs = "MS"
    toBS CountryCodeMt = "MT"
    toBS CountryCodeMu = "MU"
    toBS CountryCodeMv = "MV"
    toBS CountryCodeMw = "MW"
    toBS CountryCodeMx = "MX"
    toBS CountryCodeMy = "MY"
    toBS CountryCodeMz = "MZ"
    toBS CountryCodeNa = "NA"
    toBS CountryCodeNc = "NC"
    toBS CountryCodeNe = "NE"
    toBS CountryCodeNg = "NG"
    toBS CountryCodeNi = "NI"
    toBS CountryCodeNl = "NL"
    toBS CountryCodeNo = "NO"
    toBS CountryCodeNp = "NP"
    toBS CountryCodeNr = "NR"
    toBS CountryCodeNu = "NU"
    toBS CountryCodeNz = "NZ"
    toBS CountryCodeOm = "OM"
    toBS CountryCodePa = "PA"
    toBS CountryCodePe = "PE"
    toBS CountryCodePf = "PF"
    toBS CountryCodePg = "PG"
    toBS CountryCodePh = "PH"
    toBS CountryCodePk = "PK"
    toBS CountryCodePl = "PL"
    toBS CountryCodePm = "PM"
    toBS CountryCodePn = "PN"
    toBS CountryCodePr = "PR"
    toBS CountryCodePt = "PT"
    toBS CountryCodePw = "PW"
    toBS CountryCodePy = "PY"
    toBS CountryCodeQa = "QA"
    toBS CountryCodeRo = "RO"
    toBS CountryCodeRs = "RS"
    toBS CountryCodeRu = "RU"
    toBS CountryCodeRw = "RW"
    toBS CountryCodeSa = "SA"
    toBS CountryCodeSb = "SB"
    toBS CountryCodeSc = "SC"
    toBS CountryCodeSd = "SD"
    toBS CountryCodeSe = "SE"
    toBS CountryCodeSg = "SG"
    toBS CountryCodeSh = "SH"
    toBS CountryCodeSi = "SI"
    toBS CountryCodeSk = "SK"
    toBS CountryCodeSl = "SL"
    toBS CountryCodeSm = "SM"
    toBS CountryCodeSn = "SN"
    toBS CountryCodeSo = "SO"
    toBS CountryCodeSr = "SR"
    toBS CountryCodeSt = "ST"
    toBS CountryCodeSv = "SV"
    toBS CountryCodeSy = "SY"
    toBS CountryCodeSz = "SZ"
    toBS CountryCodeTc = "TC"
    toBS CountryCodeTd = "TD"
    toBS CountryCodeTg = "TG"
    toBS CountryCodeTh = "TH"
    toBS CountryCodeTj = "TJ"
    toBS CountryCodeTk = "TK"
    toBS CountryCodeTl = "TL"
    toBS CountryCodeTm = "TM"
    toBS CountryCodeTn = "TN"
    toBS CountryCodeTo = "TO"
    toBS CountryCodeTr = "TR"
    toBS CountryCodeTt = "TT"
    toBS CountryCodeTv = "TV"
    toBS CountryCodeTw = "TW"
    toBS CountryCodeTz = "TZ"
    toBS CountryCodeUa = "UA"
    toBS CountryCodeUg = "UG"
    toBS CountryCodeUs = "US"
    toBS CountryCodeUy = "UY"
    toBS CountryCodeUz = "UZ"
    toBS CountryCodeVa = "VA"
    toBS CountryCodeVc = "VC"
    toBS CountryCodeVe = "VE"
    toBS CountryCodeVg = "VG"
    toBS CountryCodeVi = "VI"
    toBS CountryCodeVn = "VN"
    toBS CountryCodeVu = "VU"
    toBS CountryCodeWf = "WF"
    toBS CountryCodeWs = "WS"
    toBS CountryCodeYe = "YE"
    toBS CountryCodeYt = "YT"
    toBS CountryCodeZa = "ZA"
    toBS CountryCodeZm = "ZM"
    toBS CountryCodeZw = "ZW"

instance ToHeader CountryCode where
    toHeader k = toHeader k . toBS

instance ToQuery CountryCode where
    toQuery = toQuery . toBS

instance FromJSON CountryCode

instance ToJSON CountryCode

data DomainAvailability
    = DomainAvailabilityAvailable -- ^ AVAILABLE
    | DomainAvailabilityAvailablePreorder -- ^ AVAILABLE_PREORDER
    | DomainAvailabilityAvailableReserved -- ^ AVAILABLE_RESERVED
    | DomainAvailabilityReserved -- ^ RESERVED
    | DomainAvailabilityUnavailable -- ^ UNAVAILABLE
    | DomainAvailabilityUnavailablePremium -- ^ UNAVAILABLE_PREMIUM
    | DomainAvailabilityUnavailableRestricted -- ^ UNAVAILABLE_RESTRICTED
      deriving (Eq, Show, Generic)

instance Hashable DomainAvailability

instance FromText DomainAvailability where
    parser = match "AVAILABLE" DomainAvailabilityAvailable
         <|> match "AVAILABLE_PREORDER" DomainAvailabilityAvailablePreorder
         <|> match "AVAILABLE_RESERVED" DomainAvailabilityAvailableReserved
         <|> match "RESERVED" DomainAvailabilityReserved
         <|> match "UNAVAILABLE" DomainAvailabilityUnavailable
         <|> match "UNAVAILABLE_PREMIUM" DomainAvailabilityUnavailablePremium
         <|> match "UNAVAILABLE_RESTRICTED" DomainAvailabilityUnavailableRestricted

instance ToText DomainAvailability where
    toText DomainAvailabilityAvailable = "AVAILABLE"
    toText DomainAvailabilityAvailablePreorder = "AVAILABLE_PREORDER"
    toText DomainAvailabilityAvailableReserved = "AVAILABLE_RESERVED"
    toText DomainAvailabilityReserved = "RESERVED"
    toText DomainAvailabilityUnavailable = "UNAVAILABLE"
    toText DomainAvailabilityUnavailablePremium = "UNAVAILABLE_PREMIUM"
    toText DomainAvailabilityUnavailableRestricted = "UNAVAILABLE_RESTRICTED"

instance ToByteString DomainAvailability where
    toBS DomainAvailabilityAvailable = "AVAILABLE"
    toBS DomainAvailabilityAvailablePreorder = "AVAILABLE_PREORDER"
    toBS DomainAvailabilityAvailableReserved = "AVAILABLE_RESERVED"
    toBS DomainAvailabilityReserved = "RESERVED"
    toBS DomainAvailabilityUnavailable = "UNAVAILABLE"
    toBS DomainAvailabilityUnavailablePremium = "UNAVAILABLE_PREMIUM"
    toBS DomainAvailabilityUnavailableRestricted = "UNAVAILABLE_RESTRICTED"

instance ToHeader DomainAvailability where
    toHeader k = toHeader k . toBS

instance ToQuery DomainAvailability where
    toQuery = toQuery . toBS

instance FromJSON DomainAvailability

data ExtraParamName
    = ExtraParamNameAuIdNumber -- ^ AU_ID_NUMBER
    | ExtraParamNameAuIdType -- ^ AU_ID_TYPE
    | ExtraParamNameBirthCity -- ^ BIRTH_CITY
    | ExtraParamNameBirthCountry -- ^ BIRTH_COUNTRY
    | ExtraParamNameBirthDateInYyyyMmDd -- ^ BIRTH_DATE_IN_YYYY_MM_DD
    | ExtraParamNameBirthDepartment -- ^ BIRTH_DEPARTMENT
    | ExtraParamNameBrandNumber -- ^ BRAND_NUMBER
    | ExtraParamNameCaLegalType -- ^ CA_LEGAL_TYPE
    | ExtraParamNameDocumentNumber -- ^ DOCUMENT_NUMBER
    | ExtraParamNameDunsNumber -- ^ DUNS_NUMBER
    | ExtraParamNameFiBusinessNumber -- ^ FI_BUSINESS_NUMBER
    | ExtraParamNameFiIdNumber -- ^ FI_ID_NUMBER
    | ExtraParamNameItPin -- ^ IT_PIN
    | ExtraParamNameRuPassportData -- ^ RU_PASSPORT_DATA
    | ExtraParamNameSeIdNumber -- ^ SE_ID_NUMBER
    | ExtraParamNameSgIdNumber -- ^ SG_ID_NUMBER
    | ExtraParamNameVatNumber -- ^ VAT_NUMBER
      deriving (Eq, Show, Generic)

instance Hashable ExtraParamName

instance FromText ExtraParamName where
    parser = match "AU_ID_NUMBER" ExtraParamNameAuIdNumber
         <|> match "AU_ID_TYPE" ExtraParamNameAuIdType
         <|> match "BIRTH_CITY" ExtraParamNameBirthCity
         <|> match "BIRTH_COUNTRY" ExtraParamNameBirthCountry
         <|> match "BIRTH_DATE_IN_YYYY_MM_DD" ExtraParamNameBirthDateInYyyyMmDd
         <|> match "BIRTH_DEPARTMENT" ExtraParamNameBirthDepartment
         <|> match "BRAND_NUMBER" ExtraParamNameBrandNumber
         <|> match "CA_LEGAL_TYPE" ExtraParamNameCaLegalType
         <|> match "DOCUMENT_NUMBER" ExtraParamNameDocumentNumber
         <|> match "DUNS_NUMBER" ExtraParamNameDunsNumber
         <|> match "FI_BUSINESS_NUMBER" ExtraParamNameFiBusinessNumber
         <|> match "FI_ID_NUMBER" ExtraParamNameFiIdNumber
         <|> match "IT_PIN" ExtraParamNameItPin
         <|> match "RU_PASSPORT_DATA" ExtraParamNameRuPassportData
         <|> match "SE_ID_NUMBER" ExtraParamNameSeIdNumber
         <|> match "SG_ID_NUMBER" ExtraParamNameSgIdNumber
         <|> match "VAT_NUMBER" ExtraParamNameVatNumber

instance ToText ExtraParamName where
    toText ExtraParamNameAuIdNumber = "AU_ID_NUMBER"
    toText ExtraParamNameAuIdType = "AU_ID_TYPE"
    toText ExtraParamNameBirthCity = "BIRTH_CITY"
    toText ExtraParamNameBirthCountry = "BIRTH_COUNTRY"
    toText ExtraParamNameBirthDateInYyyyMmDd = "BIRTH_DATE_IN_YYYY_MM_DD"
    toText ExtraParamNameBirthDepartment = "BIRTH_DEPARTMENT"
    toText ExtraParamNameBrandNumber = "BRAND_NUMBER"
    toText ExtraParamNameCaLegalType = "CA_LEGAL_TYPE"
    toText ExtraParamNameDocumentNumber = "DOCUMENT_NUMBER"
    toText ExtraParamNameDunsNumber = "DUNS_NUMBER"
    toText ExtraParamNameFiBusinessNumber = "FI_BUSINESS_NUMBER"
    toText ExtraParamNameFiIdNumber = "FI_ID_NUMBER"
    toText ExtraParamNameItPin = "IT_PIN"
    toText ExtraParamNameRuPassportData = "RU_PASSPORT_DATA"
    toText ExtraParamNameSeIdNumber = "SE_ID_NUMBER"
    toText ExtraParamNameSgIdNumber = "SG_ID_NUMBER"
    toText ExtraParamNameVatNumber = "VAT_NUMBER"

instance ToByteString ExtraParamName where
    toBS ExtraParamNameAuIdNumber = "AU_ID_NUMBER"
    toBS ExtraParamNameAuIdType = "AU_ID_TYPE"
    toBS ExtraParamNameBirthCity = "BIRTH_CITY"
    toBS ExtraParamNameBirthCountry = "BIRTH_COUNTRY"
    toBS ExtraParamNameBirthDateInYyyyMmDd = "BIRTH_DATE_IN_YYYY_MM_DD"
    toBS ExtraParamNameBirthDepartment = "BIRTH_DEPARTMENT"
    toBS ExtraParamNameBrandNumber = "BRAND_NUMBER"
    toBS ExtraParamNameCaLegalType = "CA_LEGAL_TYPE"
    toBS ExtraParamNameDocumentNumber = "DOCUMENT_NUMBER"
    toBS ExtraParamNameDunsNumber = "DUNS_NUMBER"
    toBS ExtraParamNameFiBusinessNumber = "FI_BUSINESS_NUMBER"
    toBS ExtraParamNameFiIdNumber = "FI_ID_NUMBER"
    toBS ExtraParamNameItPin = "IT_PIN"
    toBS ExtraParamNameRuPassportData = "RU_PASSPORT_DATA"
    toBS ExtraParamNameSeIdNumber = "SE_ID_NUMBER"
    toBS ExtraParamNameSgIdNumber = "SG_ID_NUMBER"
    toBS ExtraParamNameVatNumber = "VAT_NUMBER"

instance ToHeader ExtraParamName where
    toHeader k = toHeader k . toBS

instance ToQuery ExtraParamName where
    toQuery = toQuery . toBS

instance FromJSON ExtraParamName

instance ToJSON ExtraParamName

data OperationStatus
    = OperationStatusError -- ^ ERROR
    | OperationStatusFailed -- ^ FAILED
    | OperationStatusInProgress -- ^ IN_PROGRESS
    | OperationStatusSubmitted -- ^ SUBMITTED
    | OperationStatusSuccessful -- ^ SUCCESSFUL
      deriving (Eq, Show, Generic)

instance Hashable OperationStatus

instance FromText OperationStatus where
    parser = match "ERROR" OperationStatusError
         <|> match "FAILED" OperationStatusFailed
         <|> match "IN_PROGRESS" OperationStatusInProgress
         <|> match "SUBMITTED" OperationStatusSubmitted
         <|> match "SUCCESSFUL" OperationStatusSuccessful

instance ToText OperationStatus where
    toText OperationStatusError = "ERROR"
    toText OperationStatusFailed = "FAILED"
    toText OperationStatusInProgress = "IN_PROGRESS"
    toText OperationStatusSubmitted = "SUBMITTED"
    toText OperationStatusSuccessful = "SUCCESSFUL"

instance ToByteString OperationStatus where
    toBS OperationStatusError = "ERROR"
    toBS OperationStatusFailed = "FAILED"
    toBS OperationStatusInProgress = "IN_PROGRESS"
    toBS OperationStatusSubmitted = "SUBMITTED"
    toBS OperationStatusSuccessful = "SUCCESSFUL"

instance ToHeader OperationStatus where
    toHeader k = toHeader k . toBS

instance ToQuery OperationStatus where
    toQuery = toQuery . toBS

instance FromJSON OperationStatus

instance ToJSON OperationStatus

data OperationType
    = OperationTypeChangePrivacyProtection -- ^ CHANGE_PRIVACY_PROTECTION
    | OperationTypeDeleteDomain -- ^ DELETE_DOMAIN
    | OperationTypeDomainLock -- ^ DOMAIN_LOCK
    | OperationTypeRegisterDomain -- ^ REGISTER_DOMAIN
    | OperationTypeTransferInDomain -- ^ TRANSFER_IN_DOMAIN
    | OperationTypeUpdateDomainContact -- ^ UPDATE_DOMAIN_CONTACT
    | OperationTypeUpdateNameserver -- ^ UPDATE_NAMESERVER
      deriving (Eq, Show, Generic)

instance Hashable OperationType

instance FromText OperationType where
    parser = match "CHANGE_PRIVACY_PROTECTION" OperationTypeChangePrivacyProtection
         <|> match "DELETE_DOMAIN" OperationTypeDeleteDomain
         <|> match "DOMAIN_LOCK" OperationTypeDomainLock
         <|> match "REGISTER_DOMAIN" OperationTypeRegisterDomain
         <|> match "TRANSFER_IN_DOMAIN" OperationTypeTransferInDomain
         <|> match "UPDATE_DOMAIN_CONTACT" OperationTypeUpdateDomainContact
         <|> match "UPDATE_NAMESERVER" OperationTypeUpdateNameserver

instance ToText OperationType where
    toText OperationTypeChangePrivacyProtection = "CHANGE_PRIVACY_PROTECTION"
    toText OperationTypeDeleteDomain = "DELETE_DOMAIN"
    toText OperationTypeDomainLock = "DOMAIN_LOCK"
    toText OperationTypeRegisterDomain = "REGISTER_DOMAIN"
    toText OperationTypeTransferInDomain = "TRANSFER_IN_DOMAIN"
    toText OperationTypeUpdateDomainContact = "UPDATE_DOMAIN_CONTACT"
    toText OperationTypeUpdateNameserver = "UPDATE_NAMESERVER"

instance ToByteString OperationType where
    toBS OperationTypeChangePrivacyProtection = "CHANGE_PRIVACY_PROTECTION"
    toBS OperationTypeDeleteDomain = "DELETE_DOMAIN"
    toBS OperationTypeDomainLock = "DOMAIN_LOCK"
    toBS OperationTypeRegisterDomain = "REGISTER_DOMAIN"
    toBS OperationTypeTransferInDomain = "TRANSFER_IN_DOMAIN"
    toBS OperationTypeUpdateDomainContact = "UPDATE_DOMAIN_CONTACT"
    toBS OperationTypeUpdateNameserver = "UPDATE_NAMESERVER"

instance ToHeader OperationType where
    toHeader k = toHeader k . toBS

instance ToQuery OperationType where
    toQuery = toQuery . toBS

instance FromJSON OperationType

instance ToJSON OperationType

-- | Provides details about the domain administrative contact. Type: Complex
-- Children: FirstName, MiddleName, LastName, ContactType, OrganizationName,
-- AddressLine1, AddressLine2, City, State, CountryCode, ZipCode, PhoneNumber,
-- Email, Fax, ExtraParams.
data ContactDetail = ContactDetail
    { _cdFirstName :: Maybe Text
    , _cdLastName :: Maybe Text
    , _cdContactType :: Maybe ContactType
    , _cdOrganizationName :: Maybe Text
    , _cdAddressLine1 :: Maybe Text
    , _cdAddressLine2 :: Maybe Text
    , _cdCity :: Maybe Text
    , _cdState :: Maybe Text
    , _cdCountryCode :: Maybe CountryCode
    , _cdZipCode :: Maybe Text
    , _cdPhoneNumber :: Maybe Text
    , _cdEmail :: Maybe Text
    , _cdFax :: Maybe Text
    , _cdExtraParams :: [ExtraParam]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ContactDetail' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @FirstName ::@ @Maybe Text@
--
-- * @LastName ::@ @Maybe Text@
--
-- * @ContactType ::@ @Maybe ContactType@
--
-- * @OrganizationName ::@ @Maybe Text@
--
-- * @AddressLine1 ::@ @Maybe Text@
--
-- * @AddressLine2 ::@ @Maybe Text@
--
-- * @City ::@ @Maybe Text@
--
-- * @State ::@ @Maybe Text@
--
-- * @CountryCode ::@ @Maybe CountryCode@
--
-- * @ZipCode ::@ @Maybe Text@
--
-- * @PhoneNumber ::@ @Maybe Text@
--
-- * @Email ::@ @Maybe Text@
--
-- * @Fax ::@ @Maybe Text@
--
-- * @ExtraParams ::@ @[ExtraParam]@
--
contactDetail :: ContactDetail
contactDetail = ContactDetail
    { _cdFirstName = Nothing
    , _cdLastName = Nothing
    , _cdContactType = Nothing
    , _cdOrganizationName = Nothing
    , _cdAddressLine1 = Nothing
    , _cdAddressLine2 = Nothing
    , _cdCity = Nothing
    , _cdState = Nothing
    , _cdCountryCode = Nothing
    , _cdZipCode = Nothing
    , _cdPhoneNumber = Nothing
    , _cdEmail = Nothing
    , _cdFax = Nothing
    , _cdExtraParams = mempty
    }

-- | First name of contact. Type: String Default: None Constraints: Maximum 255
-- characters. Parents: RegistrantContact, AdminContact, TechContact Required:
-- Yes.
cdFirstName :: Lens' ContactDetail (Maybe Text)
cdFirstName = lens _cdFirstName (\s a -> s { _cdFirstName = a })

-- | Last name of contact. Type: String Default: None Constraints: Maximum 255
-- characters. Parents: RegistrantContact, AdminContact, TechContact Required:
-- Yes.
cdLastName :: Lens' ContactDetail (Maybe Text)
cdLastName = lens _cdLastName (\s a -> s { _cdLastName = a })

-- | Indicates whether the contact is a person, company, association, or public
-- organization. If you choose an option other than PERSON, you must enter an
-- organization name, and you can't enable privacy protection for the contact.
-- Type: String Default: None Constraints: Maximum 255 characters. Valid
-- values: PERSON | COMPANY | ASSOCIATION | PUBLIC_BODY Parents:
-- RegistrantContact, AdminContact, TechContact Required: Yes.
cdContactType :: Lens' ContactDetail (Maybe ContactType)
cdContactType = lens _cdContactType (\s a -> s { _cdContactType = a })

-- | Name of the organization for contact types other than PERSON. Type: String
-- Default: None Constraints: Maximum 255 characters. Contact type must not be
-- PERSON. Parents: RegistrantContact, AdminContact, TechContact Required: No.
cdOrganizationName :: Lens' ContactDetail (Maybe Text)
cdOrganizationName =
    lens _cdOrganizationName (\s a -> s { _cdOrganizationName = a })

-- | First line of the contact's address. Type: String Default: None
-- Constraints: Maximum 255 characters. Parents: RegistrantContact,
-- AdminContact, TechContact Required: Yes.
cdAddressLine1 :: Lens' ContactDetail (Maybe Text)
cdAddressLine1 = lens _cdAddressLine1 (\s a -> s { _cdAddressLine1 = a })

-- | Second line of contact's address, if any. Type: String Default: None
-- Constraints: Maximum 255 characters. Parents: RegistrantContact,
-- AdminContact, TechContact Required: No.
cdAddressLine2 :: Lens' ContactDetail (Maybe Text)
cdAddressLine2 = lens _cdAddressLine2 (\s a -> s { _cdAddressLine2 = a })

-- | The city of the contact's address. Type: String Default: None Constraints:
-- Maximum 255 characters. Parents: RegistrantContact, AdminContact,
-- TechContact Required: Yes.
cdCity :: Lens' ContactDetail (Maybe Text)
cdCity = lens _cdCity (\s a -> s { _cdCity = a })

-- | The state or province of the contact's city. Type: String Default: None
-- Constraints: Maximum 255 characters. Parents: RegistrantContact,
-- AdminContact, TechContact Required: No.
cdState :: Lens' ContactDetail (Maybe Text)
cdState = lens _cdState (\s a -> s { _cdState = a })

-- | Code for the country of the contact's address. Type: String Default: None
-- Constraints: Maximum 255 characters. Parents: RegistrantContact,
-- AdminContact, TechContact Required: Yes.
cdCountryCode :: Lens' ContactDetail (Maybe CountryCode)
cdCountryCode = lens _cdCountryCode (\s a -> s { _cdCountryCode = a })

-- | The zip or postal code of the contact's address. Type: String Default: None
-- Constraints: Maximum 255 characters. Parents: RegistrantContact,
-- AdminContact, TechContact Required: No.
cdZipCode :: Lens' ContactDetail (Maybe Text)
cdZipCode = lens _cdZipCode (\s a -> s { _cdZipCode = a })

-- | The phone number of the contact. Type: String Default: None Constraints:
-- Phone number must be specified in the format "+[country dialing
-- code].[number including any area code&gt;]". For example, a US phone number
-- might appear as "+1.1234567890". Parents: RegistrantContact, AdminContact,
-- TechContact Required: Yes.
cdPhoneNumber :: Lens' ContactDetail (Maybe Text)
cdPhoneNumber = lens _cdPhoneNumber (\s a -> s { _cdPhoneNumber = a })

-- | Email address of the contact. Type: String Default: None Constraints:
-- Maximum 254 characters. Parents: RegistrantContact, AdminContact,
-- TechContact Required: Yes.
cdEmail :: Lens' ContactDetail (Maybe Text)
cdEmail = lens _cdEmail (\s a -> s { _cdEmail = a })

-- | Fax number of the contact. Type: String Default: None Constraints: Phone
-- number must be specified in the format "+[country dialing code].[number
-- including any area code]". For example, a US phone number might appear as
-- "+1.1234567890". Parents: RegistrantContact, AdminContact, TechContact
-- Required: No.
cdFax :: Lens' ContactDetail (Maybe Text)
cdFax = lens _cdFax (\s a -> s { _cdFax = a })

-- | A list of name-value pairs for parameters required by certain top-level
-- domains. Type: Complex Default: None Parents: RegistrantContact,
-- AdminContact, TechContact Children: Name, Value Required: No.
cdExtraParams :: Lens' ContactDetail [ExtraParam]
cdExtraParams = lens _cdExtraParams (\s a -> s { _cdExtraParams = a })

instance FromJSON ContactDetail

instance ToJSON ContactDetail

data DomainSummary = DomainSummary
    { _dsDomainName :: Text
    , _dsAutoRenew :: Maybe Bool
    , _dsTransferLock :: Maybe Bool
    , _dsExpiry :: Maybe ISO8601
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'DomainSummary' data type.
--
-- 'DomainSummary' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DomainName ::@ @Text@
--
-- * @AutoRenew ::@ @Maybe Bool@
--
-- * @TransferLock ::@ @Maybe Bool@
--
-- * @Expiry ::@ @Maybe ISO8601@
--
domainSummary :: Text -- ^ 'dsDomainName'
              -> DomainSummary
domainSummary p1 = DomainSummary
    { _dsDomainName = p1
    , _dsAutoRenew = Nothing
    , _dsTransferLock = Nothing
    , _dsExpiry = Nothing
    }

-- | The name of a domain. Type: String.
dsDomainName :: Lens' DomainSummary Text
dsDomainName = lens _dsDomainName (\s a -> s { _dsDomainName = a })

-- | Indicates whether the domain is automatically renewed upon expiration.
-- Type: Boolean Valid values: True | False.
dsAutoRenew :: Lens' DomainSummary (Maybe Bool)
dsAutoRenew = lens _dsAutoRenew (\s a -> s { _dsAutoRenew = a })

-- | Indicates whether a domain is locked from unauthorized transfer to another
-- party. Type: Boolean Valid values: True | False.
dsTransferLock :: Lens' DomainSummary (Maybe Bool)
dsTransferLock = lens _dsTransferLock (\s a -> s { _dsTransferLock = a })

-- | Expiration date of the domain in Coordinated Universal Time (UTC). Type:
-- Long.
dsExpiry :: Lens' DomainSummary (Maybe ISO8601)
dsExpiry = lens _dsExpiry (\s a -> s { _dsExpiry = a })

instance FromJSON DomainSummary

-- | ExtraParam includes the following elements.
data ExtraParam = ExtraParam
    { _epName :: ExtraParamName
    , _epValue :: Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'ExtraParam' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @ExtraParamName@
--
-- * @Value ::@ @Text@
--
extraParam :: ExtraParamName -- ^ 'epName'
           -> Text -- ^ 'epValue'
           -> ExtraParam
extraParam p1 p2 = ExtraParam
    { _epName = p1
    , _epValue = p2
    }

-- | Name of the additional parameter required by the top-level domain. Type:
-- String Default: None Valid values: DUNS_NUMBER | BRAND_NUMBER |
-- BIRTH_DEPARTMENT | BIRTH_DATE_IN_YYYY_MM_DD | BIRTH_COUNTRY | BIRTH_CITY |
-- DOCUMENT_NUMBER | AU_ID_NUMBER | AU_ID_TYPE | CA_LEGAL_TYPE |
-- FI_BUSINESS_NUMBER | FI_ID_NUMBER | IT_PIN | RU_PASSPORT_DATA |
-- SE_ID_NUMBER | SG_ID_NUMBER | VAT_NUMBER Parent: ExtraParams Required: Yes.
epName :: Lens' ExtraParam ExtraParamName
epName = lens _epName (\s a -> s { _epName = a })

-- | Values corresponding to the additional parameter names required by some
-- top-level domains. Type: String Default: None Constraints: Maximum 2048
-- characters. Parent: ExtraParams Required: Yes.
epValue :: Lens' ExtraParam Text
epValue = lens _epValue (\s a -> s { _epValue = a })

instance FromJSON ExtraParam

instance ToJSON ExtraParam

-- | Nameserver includes the following elements.
data Nameserver = Nameserver
    { _nName :: Text
    , _nGlueIps :: [Text]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'Nameserver' data type to populate a request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Text@
--
-- * @GlueIps ::@ @[Text]@
--
nameserver :: Text -- ^ 'nName'
           -> Nameserver
nameserver p1 = Nameserver
    { _nName = p1
    , _nGlueIps = mempty
    }

-- | The fully qualified host name of the name server. Type: String Constraint:
-- Maximum 255 characterss Parent: Nameservers.
nName :: Lens' Nameserver Text
nName = lens _nName (\s a -> s { _nName = a })

-- | Glue IP address of a name server entry. Glue IP addresses are required only
-- when the name of the name server is a subdomain of the domain. For example,
-- if your domain is example.com and the name server for the domain is
-- ns.example.com, you need to specify the IP address for ns.example.com.
-- Type: List of IP addresses. Constraints: The list can contain only one IPv4
-- and one IPv6 address. Parent: Nameservers.
nGlueIps :: Lens' Nameserver [Text]
nGlueIps = lens _nGlueIps (\s a -> s { _nGlueIps = a })

instance FromJSON Nameserver

instance ToJSON Nameserver

-- | OperationSummary includes the following elements.
data OperationSummary = OperationSummary
    { _osOperationId :: Text
    , _osStatus :: OperationStatus
    , _osType :: OperationType
    , _osSubmittedDate :: ISO8601
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required fields to construct
-- a valid 'OperationSummary' data type.
--
-- 'OperationSummary' is exclusively used in responses and this constructor
-- is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @OperationId ::@ @Text@
--
-- * @Status ::@ @OperationStatus@
--
-- * @Type ::@ @OperationType@
--
-- * @SubmittedDate ::@ @ISO8601@
--
operationSummary :: Text -- ^ 'osOperationId'
                 -> OperationStatus -- ^ 'osStatus'
                 -> OperationType -- ^ 'osType'
                 -> ISO8601 -- ^ 'osSubmittedDate'
                 -> OperationSummary
operationSummary p1 p2 p3 p4 = OperationSummary
    { _osOperationId = p1
    , _osStatus = p2
    , _osType = p3
    , _osSubmittedDate = p4
    }

-- | Identifier returned to track the requested action. Type: String.
osOperationId :: Lens' OperationSummary Text
osOperationId = lens _osOperationId (\s a -> s { _osOperationId = a })

-- | The current status of the requested operation in the system. Type: String.
osStatus :: Lens' OperationSummary OperationStatus
osStatus = lens _osStatus (\s a -> s { _osStatus = a })

-- | Type of the action requested. Type: String Valid values: REGISTER_DOMAIN |
-- DELETE_DOMAIN | TRANSFER_IN_DOMAIN | UPDATE_DOMAIN_CONTACT |
-- UPDATE_NAMESERVER | CHANGE_PRIVACY_PROTECTION | DOMAIN_LOCK.
osType :: Lens' OperationSummary OperationType
osType = lens _osType (\s a -> s { _osType = a })

-- | The date when the request was submitted.
osSubmittedDate :: Lens' OperationSummary ISO8601
osSubmittedDate = lens _osSubmittedDate (\s a -> s { _osSubmittedDate = a })

instance FromJSON OperationSummary
