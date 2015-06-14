{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

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
    -- ** Errors
    , JSONError

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

    -- * ContactType
    , ContactType (..)

    -- * CountryCode
    , CountryCode (..)

    -- * DomainAvailability
    , DomainAvailability (..)

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

    -- * ExtraParamName
    , ExtraParamName (..)

    -- * Nameserver
    , Nameserver
    , nameserver
    , namGlueIPs
    , namName

    -- * OperationStatus
    , OperationStatus (..)

    -- * OperationSummary
    , OperationSummary
    , operationSummary
    , osOperationId
    , osStatus
    , osType
    , osSubmittedDate

    -- * OperationType
    , OperationType (..)

    -- * Tag
    , Tag
    , tag
    , tagValue
    , tagKey
    ) where

import Network.AWS.Prelude
import Network.AWS.Sign.V4

-- | Version @2014-05-15@ of the Amazon Route 53 Domains SDK.
data Route53Domains

instance AWSService Route53Domains where
    type Sg Route53Domains = V4
    type Er Route53Domains = JSONError

    service = service'
      where
        service' :: Service Route53Domains
        service' = Service
            { _svcAbbrev  = "Route53Domains"
            , _svcPrefix  = "route53domains"
            , _svcVersion = "2014-05-15"
            , _svcHandle  = handle
            , _svcRetry   = retry
            }

        handle :: Status
               -> Maybe (LazyByteString -> ServiceError JSONError)
        handle = jsonError statusSuccess service'

        retry :: Retry Route53Domains
        retry = undefined

        check :: Status
              -> JSONError
              -> Bool
        check (statusCode -> s) (awsErrorCode -> e) = undefined

-- | /See:/ 'contactDetail' smart constructor.
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
data ContactDetail = ContactDetail'{_cdOrganizationName :: Maybe Text, _cdEmail :: Maybe Text, _cdFax :: Maybe Text, _cdState :: Maybe Text, _cdLastName :: Maybe Text, _cdExtraParams :: [ExtraParam], _cdZipCode :: Maybe Text, _cdAddressLine1 :: Maybe Text, _cdCity :: Maybe Text, _cdPhoneNumber :: Maybe Text, _cdAddressLine2 :: Maybe Text, _cdFirstName :: Maybe Text, _cdCountryCode :: Maybe CountryCode, _cdContactType :: Maybe ContactType} deriving (Eq, Read, Show)

-- | 'ContactDetail' smart constructor.
contactDetail :: ContactDetail
contactDetail = ContactDetail'{_cdOrganizationName = Nothing, _cdEmail = Nothing, _cdFax = Nothing, _cdState = Nothing, _cdLastName = Nothing, _cdExtraParams = mempty, _cdZipCode = Nothing, _cdAddressLine1 = Nothing, _cdCity = Nothing, _cdPhoneNumber = Nothing, _cdAddressLine2 = Nothing, _cdFirstName = Nothing, _cdCountryCode = Nothing, _cdContactType = Nothing};

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
cdExtraParams = lens _cdExtraParams (\ s a -> s{_cdExtraParams = a});

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
                   x .:? "OrganizationName" <*> x .:? "Email" <*>
                     x .:? "Fax"
                     <*> x .:? "State"
                     <*> x .:? "LastName"
                     <*> x .:? "ExtraParams" .!= mempty
                     <*> x .:? "ZipCode"
                     <*> x .:? "AddressLine1"
                     <*> x .:? "City"
                     <*> x .:? "PhoneNumber"
                     <*> x .:? "AddressLine2"
                     <*> x .:? "FirstName"
                     <*> x .:? "CountryCode"
                     <*> x .:? "ContactType")

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

data ContactType = Person | Company | Reseller | Association | PublicBody deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText ContactType where
    parser = takeLowerText >>= \case
        "ASSOCIATION" -> pure Association
        "COMPANY" -> pure Company
        "PERSON" -> pure Person
        "PUBLIC_BODY" -> pure PublicBody
        "RESELLER" -> pure Reseller
        e -> fail ("Failure parsing ContactType from " ++ show e)

instance ToText ContactType where
    toText = \case
        Association -> "ASSOCIATION"
        Company -> "COMPANY"
        Person -> "PERSON"
        PublicBody -> "PUBLIC_BODY"
        Reseller -> "RESELLER"

instance Hashable ContactType
instance ToQuery ContactType
instance ToHeader ContactType

instance ToJSON ContactType where
    toJSON = toJSONText

instance FromJSON ContactType where
    parseJSON = parseJSONText "ContactType"

data CountryCode = NR | CG | TH | WS | IM | FJ | AU | UA | PL | MY | AE | SG | HT | BN | PG | JM | MR | VA | UZ | AN | RU | GH | BE | SL | CL | TC | NI | ZM | LK | PW | LV | UG | AS | GE | HR | BH | SA | EG | GU | KY | CA | TN | KI | OM | JP | MO | CV | MH | FK | ZW | AT | RO | PM | MX | SV | AD | GB | BO | HU | GR | CF | KN | NC | IL | LA | TT | CK | TD | ME | IQ | MU | BR | AI | BB | SK | NU | KH | TO | NE | ZA | MN | FM | AR | DO | PK | BY | GD | BI | GA | BL | SE | EC | GQ | TZ | NP | KM | TJ | LB | CU | MK | LR | AW | DJ | PN | ES | DZ | US | AG | DE | MT | PA | VG | BS | SZ | RS | GN | KR | NO | CZ | MD | PT | MA | AM | UY | BF | SO | YE | KW | NZ | CO | KG | FR | IE | GT' | TK | NA | LT' | LC | IN | FI | LS | DK | MZ | VI | ST | ER | AF | BM | SD | CD | TV | CI | NL | CY | PR | MG | IS | MW | SY | GM | SI | YT | SN | BG | CN | ID | LI | IT | LY | PE | JO | MP | BW | VC | AL | RW | VN | BZ | BJ | SC | EE | QA | GW | CC | TL | MM | AQ | PH | CX | MF | IR | AZ | MV | VE | HK | GL | BA | SH | VU | KP | TW | CH | TG | KE | MC | PF | MS | BT | HN | AO | GI | SM | BD | GY | TR | CM | TM | NG | CR | PY | ML | FO | WF | LU | DM | SR | ET | SB | KZ deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText CountryCode where
    parser = takeLowerText >>= \case
        "AD" -> pure AD
        "AE" -> pure AE
        "AF" -> pure AF
        "AG" -> pure AG
        "AI" -> pure AI
        "AL" -> pure AL
        "AM" -> pure AM
        "AN" -> pure AN
        "AO" -> pure AO
        "AQ" -> pure AQ
        "AR" -> pure AR
        "AS" -> pure AS
        "AT" -> pure AT
        "AU" -> pure AU
        "AW" -> pure AW
        "AZ" -> pure AZ
        "BA" -> pure BA
        "BB" -> pure BB
        "BD" -> pure BD
        "BE" -> pure BE
        "BF" -> pure BF
        "BG" -> pure BG
        "BH" -> pure BH
        "BI" -> pure BI
        "BJ" -> pure BJ
        "BL" -> pure BL
        "BM" -> pure BM
        "BN" -> pure BN
        "BO" -> pure BO
        "BR" -> pure BR
        "BS" -> pure BS
        "BT" -> pure BT
        "BW" -> pure BW
        "BY" -> pure BY
        "BZ" -> pure BZ
        "CA" -> pure CA
        "CC" -> pure CC
        "CD" -> pure CD
        "CF" -> pure CF
        "CG" -> pure CG
        "CH" -> pure CH
        "CI" -> pure CI
        "CK" -> pure CK
        "CL" -> pure CL
        "CM" -> pure CM
        "CN" -> pure CN
        "CO" -> pure CO
        "CR" -> pure CR
        "CU" -> pure CU
        "CV" -> pure CV
        "CX" -> pure CX
        "CY" -> pure CY
        "CZ" -> pure CZ
        "DE" -> pure DE
        "DJ" -> pure DJ
        "DK" -> pure DK
        "DM" -> pure DM
        "DO" -> pure DO
        "DZ" -> pure DZ
        "EC" -> pure EC
        "EE" -> pure EE
        "EG" -> pure EG
        "ER" -> pure ER
        "ES" -> pure ES
        "ET" -> pure ET
        "FI" -> pure FI
        "FJ" -> pure FJ
        "FK" -> pure FK
        "FM" -> pure FM
        "FO" -> pure FO
        "FR" -> pure FR
        "GA" -> pure GA
        "GB" -> pure GB
        "GD" -> pure GD
        "GE" -> pure GE
        "GH" -> pure GH
        "GI" -> pure GI
        "GL" -> pure GL
        "GM" -> pure GM
        "GN" -> pure GN
        "GQ" -> pure GQ
        "GR" -> pure GR
        "GT" -> pure GT'
        "GU" -> pure GU
        "GW" -> pure GW
        "GY" -> pure GY
        "HK" -> pure HK
        "HN" -> pure HN
        "HR" -> pure HR
        "HT" -> pure HT
        "HU" -> pure HU
        "ID" -> pure ID
        "IE" -> pure IE
        "IL" -> pure IL
        "IM" -> pure IM
        "IN" -> pure IN
        "IQ" -> pure IQ
        "IR" -> pure IR
        "IS" -> pure IS
        "IT" -> pure IT
        "JM" -> pure JM
        "JO" -> pure JO
        "JP" -> pure JP
        "KE" -> pure KE
        "KG" -> pure KG
        "KH" -> pure KH
        "KI" -> pure KI
        "KM" -> pure KM
        "KN" -> pure KN
        "KP" -> pure KP
        "KR" -> pure KR
        "KW" -> pure KW
        "KY" -> pure KY
        "KZ" -> pure KZ
        "LA" -> pure LA
        "LB" -> pure LB
        "LC" -> pure LC
        "LI" -> pure LI
        "LK" -> pure LK
        "LR" -> pure LR
        "LS" -> pure LS
        "LT" -> pure LT'
        "LU" -> pure LU
        "LV" -> pure LV
        "LY" -> pure LY
        "MA" -> pure MA
        "MC" -> pure MC
        "MD" -> pure MD
        "ME" -> pure ME
        "MF" -> pure MF
        "MG" -> pure MG
        "MH" -> pure MH
        "MK" -> pure MK
        "ML" -> pure ML
        "MM" -> pure MM
        "MN" -> pure MN
        "MO" -> pure MO
        "MP" -> pure MP
        "MR" -> pure MR
        "MS" -> pure MS
        "MT" -> pure MT
        "MU" -> pure MU
        "MV" -> pure MV
        "MW" -> pure MW
        "MX" -> pure MX
        "MY" -> pure MY
        "MZ" -> pure MZ
        "NA" -> pure NA
        "NC" -> pure NC
        "NE" -> pure NE
        "NG" -> pure NG
        "NI" -> pure NI
        "NL" -> pure NL
        "NO" -> pure NO
        "NP" -> pure NP
        "NR" -> pure NR
        "NU" -> pure NU
        "NZ" -> pure NZ
        "OM" -> pure OM
        "PA" -> pure PA
        "PE" -> pure PE
        "PF" -> pure PF
        "PG" -> pure PG
        "PH" -> pure PH
        "PK" -> pure PK
        "PL" -> pure PL
        "PM" -> pure PM
        "PN" -> pure PN
        "PR" -> pure PR
        "PT" -> pure PT
        "PW" -> pure PW
        "PY" -> pure PY
        "QA" -> pure QA
        "RO" -> pure RO
        "RS" -> pure RS
        "RU" -> pure RU
        "RW" -> pure RW
        "SA" -> pure SA
        "SB" -> pure SB
        "SC" -> pure SC
        "SD" -> pure SD
        "SE" -> pure SE
        "SG" -> pure SG
        "SH" -> pure SH
        "SI" -> pure SI
        "SK" -> pure SK
        "SL" -> pure SL
        "SM" -> pure SM
        "SN" -> pure SN
        "SO" -> pure SO
        "SR" -> pure SR
        "ST" -> pure ST
        "SV" -> pure SV
        "SY" -> pure SY
        "SZ" -> pure SZ
        "TC" -> pure TC
        "TD" -> pure TD
        "TG" -> pure TG
        "TH" -> pure TH
        "TJ" -> pure TJ
        "TK" -> pure TK
        "TL" -> pure TL
        "TM" -> pure TM
        "TN" -> pure TN
        "TO" -> pure TO
        "TR" -> pure TR
        "TT" -> pure TT
        "TV" -> pure TV
        "TW" -> pure TW
        "TZ" -> pure TZ
        "UA" -> pure UA
        "UG" -> pure UG
        "US" -> pure US
        "UY" -> pure UY
        "UZ" -> pure UZ
        "VA" -> pure VA
        "VC" -> pure VC
        "VE" -> pure VE
        "VG" -> pure VG
        "VI" -> pure VI
        "VN" -> pure VN
        "VU" -> pure VU
        "WF" -> pure WF
        "WS" -> pure WS
        "YE" -> pure YE
        "YT" -> pure YT
        "ZA" -> pure ZA
        "ZM" -> pure ZM
        "ZW" -> pure ZW
        e -> fail ("Failure parsing CountryCode from " ++ show e)

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
        ID -> "ID"
        IE -> "IE"
        IL -> "IL"
        IM -> "IM"
        IN -> "IN"
        IQ -> "IQ"
        IR -> "IR"
        IS -> "IS"
        IT -> "IT"
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
instance ToQuery CountryCode
instance ToHeader CountryCode

instance ToJSON CountryCode where
    toJSON = toJSONText

instance FromJSON CountryCode where
    parseJSON = parseJSONText "CountryCode"

data DomainAvailability = DontKnow | UnavailableRestricted | AvailableReserved | AvailablePreorder | Reserved | Unavailable | UnavailablePremium | Available deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText DomainAvailability where
    parser = takeLowerText >>= \case
        "AVAILABLE" -> pure Available
        "AVAILABLE_PREORDER" -> pure AvailablePreorder
        "AVAILABLE_RESERVED" -> pure AvailableReserved
        "DONT_KNOW" -> pure DontKnow
        "RESERVED" -> pure Reserved
        "UNAVAILABLE" -> pure Unavailable
        "UNAVAILABLE_PREMIUM" -> pure UnavailablePremium
        "UNAVAILABLE_RESTRICTED" -> pure UnavailableRestricted
        e -> fail ("Failure parsing DomainAvailability from " ++ show e)

instance ToText DomainAvailability where
    toText = \case
        Available -> "AVAILABLE"
        AvailablePreorder -> "AVAILABLE_PREORDER"
        AvailableReserved -> "AVAILABLE_RESERVED"
        DontKnow -> "DONT_KNOW"
        Reserved -> "RESERVED"
        Unavailable -> "UNAVAILABLE"
        UnavailablePremium -> "UNAVAILABLE_PREMIUM"
        UnavailableRestricted -> "UNAVAILABLE_RESTRICTED"

instance Hashable DomainAvailability
instance ToQuery DomainAvailability
instance ToHeader DomainAvailability

instance FromJSON DomainAvailability where
    parseJSON = parseJSONText "DomainAvailability"

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
data DomainSummary = DomainSummary'{_dsExpiry :: Maybe POSIX, _dsTransferLock :: Maybe Bool, _dsAutoRenew :: Maybe Bool, _dsDomainName :: Text} deriving (Eq, Read, Show)

-- | 'DomainSummary' smart constructor.
domainSummary :: Text -> DomainSummary
domainSummary pDomainName = DomainSummary'{_dsExpiry = Nothing, _dsTransferLock = Nothing, _dsAutoRenew = Nothing, _dsDomainName = pDomainName};

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
                   x .:? "Expiry" <*> x .:? "TransferLock" <*>
                     x .:? "AutoRenew"
                     <*> x .: "DomainName")

-- | /See:/ 'extraParam' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'epName'
--
-- * 'epValue'
data ExtraParam = ExtraParam'{_epName :: ExtraParamName, _epValue :: Text} deriving (Eq, Read, Show)

-- | 'ExtraParam' smart constructor.
extraParam :: ExtraParamName -> Text -> ExtraParam
extraParam pName pValue = ExtraParam'{_epName = pName, _epValue = pValue};

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
              (\ x -> ExtraParam' <$> x .: "Name" <*> x .: "Value")

instance ToJSON ExtraParam where
        toJSON ExtraParam'{..}
          = object ["Name" .= _epName, "Value" .= _epValue]

data ExtraParamName = DocumentNumber | ESIdentificationType | RUPassportData | SGIDNumber | FIBusinessNumber | AUIDNumber | ESLegalForm | BirthDateINYyyyMMDD | CALegalType | AUIDType | BirthDepartment | ESIdentification | DunsNumber | BirthCity | ITPin | BirthCountry | VatNumber | BrandNumber | SEIDNumber | FIIDNumber deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText ExtraParamName where
    parser = takeLowerText >>= \case
        "AU_ID_NUMBER" -> pure AUIDNumber
        "AU_ID_TYPE" -> pure AUIDType
        "BIRTH_CITY" -> pure BirthCity
        "BIRTH_COUNTRY" -> pure BirthCountry
        "BIRTH_DATE_IN_YYYY_MM_DD" -> pure BirthDateINYyyyMMDD
        "BIRTH_DEPARTMENT" -> pure BirthDepartment
        "BRAND_NUMBER" -> pure BrandNumber
        "CA_LEGAL_TYPE" -> pure CALegalType
        "DOCUMENT_NUMBER" -> pure DocumentNumber
        "DUNS_NUMBER" -> pure DunsNumber
        "ES_IDENTIFICATION" -> pure ESIdentification
        "ES_IDENTIFICATION_TYPE" -> pure ESIdentificationType
        "ES_LEGAL_FORM" -> pure ESLegalForm
        "FI_BUSINESS_NUMBER" -> pure FIBusinessNumber
        "FI_ID_NUMBER" -> pure FIIDNumber
        "IT_PIN" -> pure ITPin
        "RU_PASSPORT_DATA" -> pure RUPassportData
        "SE_ID_NUMBER" -> pure SEIDNumber
        "SG_ID_NUMBER" -> pure SGIDNumber
        "VAT_NUMBER" -> pure VatNumber
        e -> fail ("Failure parsing ExtraParamName from " ++ show e)

instance ToText ExtraParamName where
    toText = \case
        AUIDNumber -> "AU_ID_NUMBER"
        AUIDType -> "AU_ID_TYPE"
        BirthCity -> "BIRTH_CITY"
        BirthCountry -> "BIRTH_COUNTRY"
        BirthDateINYyyyMMDD -> "BIRTH_DATE_IN_YYYY_MM_DD"
        BirthDepartment -> "BIRTH_DEPARTMENT"
        BrandNumber -> "BRAND_NUMBER"
        CALegalType -> "CA_LEGAL_TYPE"
        DocumentNumber -> "DOCUMENT_NUMBER"
        DunsNumber -> "DUNS_NUMBER"
        ESIdentification -> "ES_IDENTIFICATION"
        ESIdentificationType -> "ES_IDENTIFICATION_TYPE"
        ESLegalForm -> "ES_LEGAL_FORM"
        FIBusinessNumber -> "FI_BUSINESS_NUMBER"
        FIIDNumber -> "FI_ID_NUMBER"
        ITPin -> "IT_PIN"
        RUPassportData -> "RU_PASSPORT_DATA"
        SEIDNumber -> "SE_ID_NUMBER"
        SGIDNumber -> "SG_ID_NUMBER"
        VatNumber -> "VAT_NUMBER"

instance Hashable ExtraParamName
instance ToQuery ExtraParamName
instance ToHeader ExtraParamName

instance ToJSON ExtraParamName where
    toJSON = toJSONText

instance FromJSON ExtraParamName where
    parseJSON = parseJSONText "ExtraParamName"

-- | /See:/ 'nameserver' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'namGlueIPs'
--
-- * 'namName'
data Nameserver = Nameserver'{_namGlueIPs :: [Text], _namName :: Text} deriving (Eq, Read, Show)

-- | 'Nameserver' smart constructor.
nameserver :: Text -> Nameserver
nameserver pName = Nameserver'{_namGlueIPs = mempty, _namName = pName};

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
namGlueIPs = lens _namGlueIPs (\ s a -> s{_namGlueIPs = a});

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
                   x .:? "GlueIps" .!= mempty <*> x .: "Name")

instance ToJSON Nameserver where
        toJSON Nameserver'{..}
          = object
              ["GlueIps" .= _namGlueIPs, "Name" .= _namName]

data OperationStatus = Error | Successful | INProgress | Failed | Submitted deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText OperationStatus where
    parser = takeLowerText >>= \case
        "ERROR" -> pure Error
        "FAILED" -> pure Failed
        "IN_PROGRESS" -> pure INProgress
        "SUBMITTED" -> pure Submitted
        "SUCCESSFUL" -> pure Successful
        e -> fail ("Failure parsing OperationStatus from " ++ show e)

instance ToText OperationStatus where
    toText = \case
        Error -> "ERROR"
        Failed -> "FAILED"
        INProgress -> "IN_PROGRESS"
        Submitted -> "SUBMITTED"
        Successful -> "SUCCESSFUL"

instance Hashable OperationStatus
instance ToQuery OperationStatus
instance ToHeader OperationStatus

instance FromJSON OperationStatus where
    parseJSON = parseJSONText "OperationStatus"

-- | /See:/ 'operationSummary' smart constructor.
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
data OperationSummary = OperationSummary'{_osOperationId :: Text, _osStatus :: OperationStatus, _osType :: OperationType, _osSubmittedDate :: POSIX} deriving (Eq, Read, Show)

-- | 'OperationSummary' smart constructor.
operationSummary :: Text -> OperationStatus -> OperationType -> UTCTime -> OperationSummary
operationSummary pOperationId pStatus pType' pSubmittedDate = OperationSummary'{_osOperationId = pOperationId, _osStatus = pStatus, _osType = pType', _osSubmittedDate = _Time # pSubmittedDate};

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
                   x .: "OperationId" <*> x .: "Status" <*> x .: "Type"
                     <*> x .: "SubmittedDate")

data OperationType = TransferINDomain | ChangePrivacyProtection | UpdateDomainContact | RegisterDomain | UpdateNameserver | DomainLock | DeleteDomain deriving (Eq, Ord, Read, Show, Enum, Generic)

instance FromText OperationType where
    parser = takeLowerText >>= \case
        "CHANGE_PRIVACY_PROTECTION" -> pure ChangePrivacyProtection
        "DELETE_DOMAIN" -> pure DeleteDomain
        "DOMAIN_LOCK" -> pure DomainLock
        "REGISTER_DOMAIN" -> pure RegisterDomain
        "TRANSFER_IN_DOMAIN" -> pure TransferINDomain
        "UPDATE_DOMAIN_CONTACT" -> pure UpdateDomainContact
        "UPDATE_NAMESERVER" -> pure UpdateNameserver
        e -> fail ("Failure parsing OperationType from " ++ show e)

instance ToText OperationType where
    toText = \case
        ChangePrivacyProtection -> "CHANGE_PRIVACY_PROTECTION"
        DeleteDomain -> "DELETE_DOMAIN"
        DomainLock -> "DOMAIN_LOCK"
        RegisterDomain -> "REGISTER_DOMAIN"
        TransferINDomain -> "TRANSFER_IN_DOMAIN"
        UpdateDomainContact -> "UPDATE_DOMAIN_CONTACT"
        UpdateNameserver -> "UPDATE_NAMESERVER"

instance Hashable OperationType
instance ToQuery OperationType
instance ToHeader OperationType

instance FromJSON OperationType where
    parseJSON = parseJSONText "OperationType"

-- | /See:/ 'tag' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tagValue'
--
-- * 'tagKey'
data Tag = Tag'{_tagValue :: Maybe Text, _tagKey :: Maybe Text} deriving (Eq, Read, Show)

-- | 'Tag' smart constructor.
tag :: Tag
tag = Tag'{_tagValue = Nothing, _tagKey = Nothing};

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
              (\ x -> Tag' <$> x .:? "Value" <*> x .:? "Key")

instance ToJSON Tag where
        toJSON Tag'{..}
          = object ["Value" .= _tagValue, "Key" .= _tagKey]
