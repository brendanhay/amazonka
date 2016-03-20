{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53Domains.Types.Product where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Route53Domains.Types.Sum

-- | ContactDetail includes the following elements.
--
-- /See:/ 'contactDetail' smart constructor.
data ContactDetail = ContactDetail'
    { _cdOrganizationName :: !(Maybe Text)
    , _cdEmail            :: !(Maybe Text)
    , _cdState            :: !(Maybe Text)
    , _cdFax              :: !(Maybe Text)
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ContactDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdOrganizationName'
--
-- * 'cdEmail'
--
-- * 'cdState'
--
-- * 'cdFax'
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
contactDetail
    :: ContactDetail
contactDetail =
    ContactDetail'
    { _cdOrganizationName = Nothing
    , _cdEmail = Nothing
    , _cdState = Nothing
    , _cdFax = Nothing
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
cdOrganizationName = lens _cdOrganizationName (\ s a -> s{_cdOrganizationName = a});

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
cdEmail = lens _cdEmail (\ s a -> s{_cdEmail = a});

-- | The state or province of the contact\'s city.
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
cdState = lens _cdState (\ s a -> s{_cdState = a});

-- | Fax number of the contact.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Phone number must be specified in the format \"+[country
-- dialing code].[number including any area code]\". For example, a US
-- phone number might appear as '\"+1.1234567890\"'.
--
-- Parents: 'RegistrantContact', 'AdminContact', 'TechContact'
--
-- Required: No
cdFax :: Lens' ContactDetail (Maybe Text)
cdFax = lens _cdFax (\ s a -> s{_cdFax = a});

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
cdLastName = lens _cdLastName (\ s a -> s{_cdLastName = a});

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
cdExtraParams = lens _cdExtraParams (\ s a -> s{_cdExtraParams = a}) . _Default . _Coerce;

-- | The zip or postal code of the contact\'s address.
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
cdZipCode = lens _cdZipCode (\ s a -> s{_cdZipCode = a});

-- | First line of the contact\'s address.
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
cdAddressLine1 = lens _cdAddressLine1 (\ s a -> s{_cdAddressLine1 = a});

-- | The city of the contact\'s address.
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
cdCity = lens _cdCity (\ s a -> s{_cdCity = a});

-- | The phone number of the contact.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Phone number must be specified in the format \"+[country
-- dialing code].[number including any area code>]\". For example, a US
-- phone number might appear as '\"+1.1234567890\"'.
--
-- Parents: 'RegistrantContact', 'AdminContact', 'TechContact'
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
-- Parents: 'RegistrantContact', 'AdminContact', 'TechContact'
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
-- Parents: 'RegistrantContact', 'AdminContact', 'TechContact'
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
-- Parents: 'RegistrantContact', 'AdminContact', 'TechContact'
--
-- Required: Yes
cdCountryCode :: Lens' ContactDetail (Maybe CountryCode)
cdCountryCode = lens _cdCountryCode (\ s a -> s{_cdCountryCode = a});

-- | Indicates whether the contact is a person, company, association, or
-- public organization. If you choose an option other than 'PERSON', you
-- must enter an organization name, and you can\'t enable privacy
-- protection for the contact.
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
cdContactType = lens _cdContactType (\ s a -> s{_cdContactType = a});

instance FromJSON ContactDetail where
        parseJSON
          = withObject "ContactDetail"
              (\ x ->
                 ContactDetail' <$>
                   (x .:? "OrganizationName") <*> (x .:? "Email") <*>
                     (x .:? "State")
                     <*> (x .:? "Fax")
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

instance Hashable ContactDetail

instance ToJSON ContactDetail where
        toJSON ContactDetail'{..}
          = object
              (catMaybes
                 [("OrganizationName" .=) <$> _cdOrganizationName,
                  ("Email" .=) <$> _cdEmail, ("State" .=) <$> _cdState,
                  ("Fax" .=) <$> _cdFax,
                  ("LastName" .=) <$> _cdLastName,
                  ("ExtraParams" .=) <$> _cdExtraParams,
                  ("ZipCode" .=) <$> _cdZipCode,
                  ("AddressLine1" .=) <$> _cdAddressLine1,
                  ("City" .=) <$> _cdCity,
                  ("PhoneNumber" .=) <$> _cdPhoneNumber,
                  ("AddressLine2" .=) <$> _cdAddressLine2,
                  ("FirstName" .=) <$> _cdFirstName,
                  ("CountryCode" .=) <$> _cdCountryCode,
                  ("ContactType" .=) <$> _cdContactType])

-- | /See:/ 'domainSummary' smart constructor.
data DomainSummary = DomainSummary'
    { _dsExpiry       :: !(Maybe POSIX)
    , _dsTransferLock :: !(Maybe Bool)
    , _dsAutoRenew    :: !(Maybe Bool)
    , _dsDomainName   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DomainSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsExpiry'
--
-- * 'dsTransferLock'
--
-- * 'dsAutoRenew'
--
-- * 'dsDomainName'
domainSummary
    :: Text -- ^ 'dsDomainName'
    -> DomainSummary
domainSummary pDomainName_ =
    DomainSummary'
    { _dsExpiry = Nothing
    , _dsTransferLock = Nothing
    , _dsAutoRenew = Nothing
    , _dsDomainName = pDomainName_
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
-- Valid values: 'True' | 'False'
dsTransferLock :: Lens' DomainSummary (Maybe Bool)
dsTransferLock = lens _dsTransferLock (\ s a -> s{_dsTransferLock = a});

-- | Indicates whether the domain is automatically renewed upon expiration.
--
-- Type: Boolean
--
-- Valid values: 'True' | 'False'
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

instance Hashable DomainSummary

-- | ExtraParam includes the following elements.
--
-- /See:/ 'extraParam' smart constructor.
data ExtraParam = ExtraParam'
    { _epName  :: !ExtraParamName
    , _epValue :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ExtraParam' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'epName'
--
-- * 'epValue'
extraParam
    :: ExtraParamName -- ^ 'epName'
    -> Text -- ^ 'epValue'
    -> ExtraParam
extraParam pName_ pValue_ =
    ExtraParam'
    { _epName = pName_
    , _epValue = pValue_
    }

-- | Name of the additional parameter required by the top-level domain.
--
-- Type: String
--
-- Default: None
--
-- Valid values: 'DUNS_NUMBER' | 'BRAND_NUMBER' | 'BIRTH_DEPARTMENT' |
-- 'BIRTH_DATE_IN_YYYY_MM_DD' | 'BIRTH_COUNTRY' | 'BIRTH_CITY' |
-- 'DOCUMENT_NUMBER' | 'AU_ID_NUMBER' | 'AU_ID_TYPE' | 'CA_LEGAL_TYPE' |
-- 'ES_IDENTIFICATION' | 'ES_IDENTIFICATION_TYPE' | 'ES_LEGAL_FORM' |
-- 'FI_BUSINESS_NUMBER' | 'FI_ID_NUMBER' | 'IT_PIN' | 'RU_PASSPORT_DATA' |
-- 'SE_ID_NUMBER' | 'SG_ID_NUMBER' | 'VAT_NUMBER'
--
-- Parent: 'ExtraParams'
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
-- Parent: 'ExtraParams'
--
-- Required: Yes
epValue :: Lens' ExtraParam Text
epValue = lens _epValue (\ s a -> s{_epValue = a});

instance FromJSON ExtraParam where
        parseJSON
          = withObject "ExtraParam"
              (\ x ->
                 ExtraParam' <$> (x .: "Name") <*> (x .: "Value"))

instance Hashable ExtraParam

instance ToJSON ExtraParam where
        toJSON ExtraParam'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _epName),
                  Just ("Value" .= _epValue)])

-- | Nameserver includes the following elements.
--
-- /See:/ 'nameserver' smart constructor.
data Nameserver = Nameserver'
    { _nGlueIPs :: !(Maybe [Text])
    , _nName    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Nameserver' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nGlueIPs'
--
-- * 'nName'
nameserver
    :: Text -- ^ 'nName'
    -> Nameserver
nameserver pName_ =
    Nameserver'
    { _nGlueIPs = Nothing
    , _nName = pName_
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
-- Parent: 'Nameservers'
nGlueIPs :: Lens' Nameserver [Text]
nGlueIPs = lens _nGlueIPs (\ s a -> s{_nGlueIPs = a}) . _Default . _Coerce;

-- | The fully qualified host name of the name server.
--
-- Type: String
--
-- Constraint: Maximum 255 characterss
--
-- Parent: 'Nameservers'
nName :: Lens' Nameserver Text
nName = lens _nName (\ s a -> s{_nName = a});

instance FromJSON Nameserver where
        parseJSON
          = withObject "Nameserver"
              (\ x ->
                 Nameserver' <$>
                   (x .:? "GlueIps" .!= mempty) <*> (x .: "Name"))

instance Hashable Nameserver

instance ToJSON Nameserver where
        toJSON Nameserver'{..}
          = object
              (catMaybes
                 [("GlueIps" .=) <$> _nGlueIPs,
                  Just ("Name" .= _nName)])

-- | OperationSummary includes the following elements.
--
-- /See:/ 'operationSummary' smart constructor.
data OperationSummary = OperationSummary'
    { _osOperationId   :: !Text
    , _osStatus        :: !OperationStatus
    , _osType          :: !OperationType
    , _osSubmittedDate :: !POSIX
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'OperationSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osOperationId'
--
-- * 'osStatus'
--
-- * 'osType'
--
-- * 'osSubmittedDate'
operationSummary
    :: Text -- ^ 'osOperationId'
    -> OperationStatus -- ^ 'osStatus'
    -> OperationType -- ^ 'osType'
    -> UTCTime -- ^ 'osSubmittedDate'
    -> OperationSummary
operationSummary pOperationId_ pStatus_ pType_ pSubmittedDate_ =
    OperationSummary'
    { _osOperationId = pOperationId_
    , _osStatus = pStatus_
    , _osType = pType_
    , _osSubmittedDate = _Time # pSubmittedDate_
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
-- Valid values: 'REGISTER_DOMAIN' | 'DELETE_DOMAIN' | 'TRANSFER_IN_DOMAIN'
-- | 'UPDATE_DOMAIN_CONTACT' | 'UPDATE_NAMESERVER' |
-- 'CHANGE_PRIVACY_PROTECTION' | 'DOMAIN_LOCK'
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

instance Hashable OperationSummary

-- | Each tag includes the following elements.
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
    { _tagValue :: !(Maybe Text)
    , _tagKey   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue'
--
-- * 'tagKey'
tag
    :: Tag
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
-- Valid values: A-Z, a-z, 0-9, space, \".:\/=+\\-\'\"
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
-- Valid values: A-Z, a-z, 0-9, space, \".:\/=+\\-\'\"
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

instance Hashable Tag

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [("Value" .=) <$> _tagValue, ("Key" .=) <$> _tagKey])
