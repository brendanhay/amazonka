{-# LANGUAGE DeriveDataTypeable          #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53Domains.V2014_05_15.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Pending.
module Network.AWS.Route53Domains.V2014_05_15.Types where

import Control.Lens.TH (makeLenses)
import Network.AWS.Prelude
import Network.AWS.Signing.V4

-- | Supported version (@2014-05-15@) of the
-- @Amazon Route 53 Domains@ service.
data Route53Domains deriving (Typeable)

instance AWSService Route53Domains where
    type Sg Route53Domains = V4
    data Er Route53Domains
        = DomainLimitExceeded
            { _dleMessage :: Maybe Text
            }
        | DuplicateRequest
            { _drMessage :: Maybe Text
            }
        | InvalidInput
            { _iiMessage :: Maybe Text
            }
        | OperationLimitExceeded
            { _oleMessage :: Maybe Text
            }
        | Route53DomainsClient HttpException
        | Route53DomainsSerializer String
        | Route53DomainsService String
        | TLDRulesViolation
            { _tldrvMessage :: Maybe Text
            }
        | UnsupportedTLD
            { _utldMessage :: Maybe Text
            }

    service = Service'
        { _svcEndpoint = Regional
        , _svcPrefix   = "route53domains"
        , _svcVersion  = "2014-05-15"
        , _svcTarget   = Nothing
        }

deriving instance Show    (Er Route53Domains)
deriving instance Generic (Er Route53Domains)

instance AWSError (Er Route53Domains) where
    awsError = const "Route53DomainsError"

instance AWSServiceError (Er Route53Domains) where
    serviceError    = Route53DomainsService
    clientError     = Route53DomainsClient
    serializerError = Route53DomainsSerializer

instance Exception (Er Route53Domains)

-- | Indicates whether the contact is a person, company, association, or public
-- organization. If you choose an option other than PERSON, you must enter an
-- organization name, and you can't enable privacy protection for the contact.
-- Type: String Default: None Constraints: Maximum 255 characters. Valid
-- values: PERSON | COMPANY | ASSOCIATION | PUBLIC_BODY Parents:
-- RegistrantContact, AdminContact, TechContact Required: Yes.
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

instance ToByteString ContactType

instance FromJSON ContactType

instance ToJSON ContactType

-- | Code for the country of the contact's address. Type: String Default: None
-- Constraints: Maximum 255 characters. Parents: RegistrantContact,
-- AdminContact, TechContact Required: Yes.
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

instance ToByteString CountryCode

instance FromJSON CountryCode

instance ToJSON CountryCode

-- | Whether the domain name is available for registering. You can only register
-- domains designated as AVAILABLE. Type: String Valid values: AVAILABLE – The
-- domain name is available. AVAILABLE_RESERVED – The domain name is reserved
-- under specific conditions. AVAILABLE_PREORDER – The domain name is
-- available and can be preordered. UNAVAILABLE – The domain name is not
-- available. UNAVAILABLE_PREMIUM – The domain name is not available.
-- UNAVAILABLE_RESTRICTED – The domain name is forbidden. RESERVED – The
-- domain name has been reserved for another person or organization.
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

instance ToByteString DomainAvailability

instance FromJSON DomainAvailability

-- | Name of the additional parameter required by the top-level domain. Type:
-- String Default: None Valid values: DUNS_NUMBER | BRAND_NUMBER |
-- BIRTH_DEPARTMENT | BIRTH_DATE_IN_YYYY_MM_DD | BIRTH_COUNTRY | BIRTH_CITY |
-- DOCUMENT_NUMBER | AU_ID_NUMBER | AU_ID_TYPE | CA_LEGAL_TYPE |
-- FI_BUSINESS_NUMBER | FI_ID_NUMBER | IT_PIN | RU_PASSPORT_DATA |
-- SE_ID_NUMBER | SG_ID_NUMBER | VAT_NUMBER Parent: ExtraParams Required: Yes.
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

instance ToByteString ExtraParamName

instance FromJSON ExtraParamName

instance ToJSON ExtraParamName

-- | The current status of the requested operation in the system. Type: String.
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

instance ToByteString OperationStatus

instance FromJSON OperationStatus

instance ToJSON OperationStatus

-- | Type of the action requested. Type: String Valid values: REGISTER_DOMAIN |
-- DELETE_DOMAIN | TRANSFER_IN_DOMAIN | UPDATE_DOMAIN_CONTACT |
-- UPDATE_NAMESERVER | CHANGE_PRIVACY_PROTECTION | DOMAIN_LOCK.
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

instance ToByteString OperationType

instance FromJSON OperationType

instance ToJSON OperationType

-- | Provides details about the domain registrant. Type: Complex Children:
-- FirstName, MiddleName, LastName, ContactType, OrganizationName,
-- AddressLine1, AddressLine2, City, State, CountryCode, ZipCode, PhoneNumber,
-- Email, Fax, ExtraParams.
data ContactDetail = ContactDetail
    { _cdOrganizationName :: Maybe Text
      -- ^ Name of the organization for contact types other than PERSON.
      -- Type: String Default: None Constraints: Maximum 255 characters.
      -- Contact type must not be PERSON. Parents: RegistrantContact,
      -- AdminContact, TechContact Required: No.
    , _cdEmail :: Maybe Text
      -- ^ Email address of the contact. Type: String Default: None
      -- Constraints: Maximum 254 characters. Parents: RegistrantContact,
      -- AdminContact, TechContact Required: Yes.
    , _cdState :: Maybe Text
      -- ^ The state or province of the contact's city. Type: String
      -- Default: None Constraints: Maximum 255 characters. Parents:
      -- RegistrantContact, AdminContact, TechContact Required: No.
    , _cdFax :: Maybe Text
      -- ^ Fax number of the contact. Type: String Default: None
      -- Constraints: Phone number must be specified in the format
      -- "+[country dialing code].[number including any area code]". For
      -- example, a US phone number might appear as "+1.1234567890".
      -- Parents: RegistrantContact, AdminContact, TechContact Required:
      -- No.
    , _cdLastName :: Maybe Text
      -- ^ Last name of contact. Type: String Default: None Constraints:
      -- Maximum 255 characters. Parents: RegistrantContact, AdminContact,
      -- TechContact Required: Yes.
    , _cdExtraParams :: [ExtraParam]
      -- ^ A list of name-value pairs for parameters required by certain
      -- top-level domains. Type: Complex Default: None Parents:
      -- RegistrantContact, AdminContact, TechContact Children: Name,
      -- Value Required: No.
    , _cdZipCode :: Maybe Text
      -- ^ The zip or postal code of the contact's address. Type: String
      -- Default: None Constraints: Maximum 255 characters. Parents:
      -- RegistrantContact, AdminContact, TechContact Required: No.
    , _cdAddressLine1 :: Maybe Text
      -- ^ First line of the contact's address. Type: String Default: None
      -- Constraints: Maximum 255 characters. Parents: RegistrantContact,
      -- AdminContact, TechContact Required: Yes.
    , _cdCity :: Maybe Text
      -- ^ The city of the contact's address. Type: String Default: None
      -- Constraints: Maximum 255 characters. Parents: RegistrantContact,
      -- AdminContact, TechContact Required: Yes.
    , _cdPhoneNumber :: Maybe Text
      -- ^ The phone number of the contact. Type: String Default: None
      -- Constraints: Phone number must be specified in the format
      -- "+[country dialing code].[number including any area code&gt;]".
      -- For example, a US phone number might appear as "+1.1234567890".
      -- Parents: RegistrantContact, AdminContact, TechContact Required:
      -- Yes.
    , _cdAddressLine2 :: Maybe Text
      -- ^ Second line of contact's address, if any. Type: String Default:
      -- None Constraints: Maximum 255 characters. Parents:
      -- RegistrantContact, AdminContact, TechContact Required: No.
    , _cdFirstName :: Maybe Text
      -- ^ First name of contact. Type: String Default: None Constraints:
      -- Maximum 255 characters. Parents: RegistrantContact, AdminContact,
      -- TechContact Required: Yes.
    , _cdCountryCode :: Maybe CountryCode
      -- ^ Code for the country of the contact's address. Type: String
      -- Default: None Constraints: Maximum 255 characters. Parents:
      -- RegistrantContact, AdminContact, TechContact Required: Yes.
    , _cdContactType :: Maybe ContactType
      -- ^ Indicates whether the contact is a person, company, association,
      -- or public organization. If you choose an option other than
      -- PERSON, you must enter an organization name, and you can't enable
      -- privacy protection for the contact. Type: String Default: None
      -- Constraints: Maximum 255 characters. Valid values: PERSON |
      -- COMPANY | ASSOCIATION | PUBLIC_BODY Parents: RegistrantContact,
      -- AdminContact, TechContact Required: Yes.
    } deriving (Show, Generic)

instance FromJSON ContactDetail

instance ToJSON ContactDetail

data DomainSummary = DomainSummary
    { _ddyExpiry :: Maybe ISO8601
      -- ^ Expiration date of the domain in Coordinated Universal Time
      -- (UTC). Type: Long.
    , _ddyTransferLock :: Maybe Bool
      -- ^ Indicates whether a domain is locked from unauthorized transfer
      -- to another party. Type: Boolean Valid values: True | False.
    , _ddyAutoRenew :: Maybe Bool
      -- ^ Indicates whether the domain is automatically renewed upon
      -- expiration. Type: Boolean Valid values: True | False.
    , _ddyDomainName :: Text
      -- ^ The name of a domain. Type: String.
    } deriving (Show, Generic)

instance FromJSON DomainSummary

-- | ExtraParam includes the following elements.
data ExtraParam = ExtraParam
    { _epValue :: Text
      -- ^ Values corresponding to the additional parameter names required
      -- by some top-level domains. Type: String Default: None
      -- Constraints: Maximum 2048 characters. Parent: ExtraParams
      -- Required: Yes.
    , _epName :: ExtraParamName
      -- ^ Name of the additional parameter required by the top-level
      -- domain. Type: String Default: None Valid values: DUNS_NUMBER |
      -- BRAND_NUMBER | BIRTH_DEPARTMENT | BIRTH_DATE_IN_YYYY_MM_DD |
      -- BIRTH_COUNTRY | BIRTH_CITY | DOCUMENT_NUMBER | AU_ID_NUMBER |
      -- AU_ID_TYPE | CA_LEGAL_TYPE | FI_BUSINESS_NUMBER | FI_ID_NUMBER |
      -- IT_PIN | RU_PASSPORT_DATA | SE_ID_NUMBER | SG_ID_NUMBER |
      -- VAT_NUMBER Parent: ExtraParams Required: Yes.
    } deriving (Show, Generic)

instance FromJSON ExtraParam

instance ToJSON ExtraParam

-- | Nameserver includes the following elements.
data Nameserver = Nameserver
    { _nName :: Text
      -- ^ The fully qualified host name of the name server. Type: String
      -- Constraint: Maximum 255 characterss Parent: Nameservers.
    , _nGlueIps :: [Text]
      -- ^ Glue IP address of a name server entry. Glue IP addresses are
      -- required only when the name of the name server is a subdomain of
      -- the domain. For example, if your domain is example.com and the
      -- name server for the domain is ns.example.com, you need to specify
      -- the IP address for ns.example.com. Type: List of IP addresses.
      -- Constraints: The list can contain only one IPv4 and one IPv6
      -- address. Parent: Nameservers.
    } deriving (Show, Generic)

instance FromJSON Nameserver

instance ToJSON Nameserver

-- | OperationSummary includes the following elements.
data OperationSummary = OperationSummary
    { _osStatus :: OperationStatus
      -- ^ The current status of the requested operation in the system.
      -- Type: String.
    , _osSubmittedDate :: ISO8601
      -- ^ The date when the request was submitted.
    , _osOperationId :: Text
      -- ^ Identifier returned to track the requested action. Type: String.
    , _osType :: OperationType
      -- ^ Type of the action requested. Type: String Valid values:
      -- REGISTER_DOMAIN | DELETE_DOMAIN | TRANSFER_IN_DOMAIN |
      -- UPDATE_DOMAIN_CONTACT | UPDATE_NAMESERVER |
      -- CHANGE_PRIVACY_PROTECTION | DOMAIN_LOCK.
    } deriving (Show, Generic)

instance FromJSON OperationSummary

-- Newtypes

-- Products
makeLenses ''ContactDetail
makeLenses ''DomainSummary
makeLenses ''ExtraParam
makeLenses ''Nameserver
makeLenses ''OperationSummary
