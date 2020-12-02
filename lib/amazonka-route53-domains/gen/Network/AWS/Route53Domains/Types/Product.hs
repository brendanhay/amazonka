{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Route53Domains.Types.Product where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Route53Domains.Types.Sum

-- | Information for one billing record.
--
--
--
-- /See:/ 'billingRecord' smart constructor.
data BillingRecord = BillingRecord'
  { _brOperation  :: !(Maybe OperationType)
  , _brInvoiceId  :: !(Maybe Text)
  , _brDomainName :: !(Maybe Text)
  , _brBillDate   :: !(Maybe POSIX)
  , _brPrice      :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BillingRecord' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'brOperation' - The operation that you were charged for.
--
-- * 'brInvoiceId' - The ID of the invoice that is associated with the billing record.
--
-- * 'brDomainName' - The name of the domain that the billing record applies to. If the domain name contains characters other than a-z, 0-9, and - (hyphen), such as an internationalized domain name, then this value is in Punycode. For more information, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DomainNameFormat.html DNS Domain Name Format> in the /Amazon Route 53 Developer Guidezzz/ .
--
-- * 'brBillDate' - The date that the operation was billed, in Unix format.
--
-- * 'brPrice' - The price that you were charged for the operation, in US dollars. Example value: 12.0
billingRecord
    :: BillingRecord
billingRecord =
  BillingRecord'
    { _brOperation = Nothing
    , _brInvoiceId = Nothing
    , _brDomainName = Nothing
    , _brBillDate = Nothing
    , _brPrice = Nothing
    }


-- | The operation that you were charged for.
brOperation :: Lens' BillingRecord (Maybe OperationType)
brOperation = lens _brOperation (\ s a -> s{_brOperation = a})

-- | The ID of the invoice that is associated with the billing record.
brInvoiceId :: Lens' BillingRecord (Maybe Text)
brInvoiceId = lens _brInvoiceId (\ s a -> s{_brInvoiceId = a})

-- | The name of the domain that the billing record applies to. If the domain name contains characters other than a-z, 0-9, and - (hyphen), such as an internationalized domain name, then this value is in Punycode. For more information, see <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DomainNameFormat.html DNS Domain Name Format> in the /Amazon Route 53 Developer Guidezzz/ .
brDomainName :: Lens' BillingRecord (Maybe Text)
brDomainName = lens _brDomainName (\ s a -> s{_brDomainName = a})

-- | The date that the operation was billed, in Unix format.
brBillDate :: Lens' BillingRecord (Maybe UTCTime)
brBillDate = lens _brBillDate (\ s a -> s{_brBillDate = a}) . mapping _Time

-- | The price that you were charged for the operation, in US dollars. Example value: 12.0
brPrice :: Lens' BillingRecord (Maybe Double)
brPrice = lens _brPrice (\ s a -> s{_brPrice = a})

instance FromJSON BillingRecord where
        parseJSON
          = withObject "BillingRecord"
              (\ x ->
                 BillingRecord' <$>
                   (x .:? "Operation") <*> (x .:? "InvoiceId") <*>
                     (x .:? "DomainName")
                     <*> (x .:? "BillDate")
                     <*> (x .:? "Price"))

instance Hashable BillingRecord where

instance NFData BillingRecord where

-- | ContactDetail includes the following elements.
--
--
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
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ContactDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdOrganizationName' - Name of the organization for contact types other than @PERSON@ .
--
-- * 'cdEmail' - Email address of the contact.
--
-- * 'cdState' - The state or province of the contact's city.
--
-- * 'cdFax' - Fax number of the contact. Constraints: Phone number must be specified in the format "+[country dialing code].[number including any area code]". For example, a US phone number might appear as @"+1.1234567890"@ .
--
-- * 'cdLastName' - Last name of contact.
--
-- * 'cdExtraParams' - A list of name-value pairs for parameters required by certain top-level domains.
--
-- * 'cdZipCode' - The zip or postal code of the contact's address.
--
-- * 'cdAddressLine1' - First line of the contact's address.
--
-- * 'cdCity' - The city of the contact's address.
--
-- * 'cdPhoneNumber' - The phone number of the contact. Constraints: Phone number must be specified in the format "+[country dialing code].[number including any area code>]". For example, a US phone number might appear as @"+1.1234567890"@ .
--
-- * 'cdAddressLine2' - Second line of contact's address, if any.
--
-- * 'cdFirstName' - First name of contact.
--
-- * 'cdCountryCode' - Code for the country of the contact's address.
--
-- * 'cdContactType' - Indicates whether the contact is a person, company, association, or public organization. If you choose an option other than @PERSON@ , you must enter an organization name, and you can't enable privacy protection for the contact.
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


-- | Name of the organization for contact types other than @PERSON@ .
cdOrganizationName :: Lens' ContactDetail (Maybe Text)
cdOrganizationName = lens _cdOrganizationName (\ s a -> s{_cdOrganizationName = a})

-- | Email address of the contact.
cdEmail :: Lens' ContactDetail (Maybe Text)
cdEmail = lens _cdEmail (\ s a -> s{_cdEmail = a})

-- | The state or province of the contact's city.
cdState :: Lens' ContactDetail (Maybe Text)
cdState = lens _cdState (\ s a -> s{_cdState = a})

-- | Fax number of the contact. Constraints: Phone number must be specified in the format "+[country dialing code].[number including any area code]". For example, a US phone number might appear as @"+1.1234567890"@ .
cdFax :: Lens' ContactDetail (Maybe Text)
cdFax = lens _cdFax (\ s a -> s{_cdFax = a})

-- | Last name of contact.
cdLastName :: Lens' ContactDetail (Maybe Text)
cdLastName = lens _cdLastName (\ s a -> s{_cdLastName = a})

-- | A list of name-value pairs for parameters required by certain top-level domains.
cdExtraParams :: Lens' ContactDetail [ExtraParam]
cdExtraParams = lens _cdExtraParams (\ s a -> s{_cdExtraParams = a}) . _Default . _Coerce

-- | The zip or postal code of the contact's address.
cdZipCode :: Lens' ContactDetail (Maybe Text)
cdZipCode = lens _cdZipCode (\ s a -> s{_cdZipCode = a})

-- | First line of the contact's address.
cdAddressLine1 :: Lens' ContactDetail (Maybe Text)
cdAddressLine1 = lens _cdAddressLine1 (\ s a -> s{_cdAddressLine1 = a})

-- | The city of the contact's address.
cdCity :: Lens' ContactDetail (Maybe Text)
cdCity = lens _cdCity (\ s a -> s{_cdCity = a})

-- | The phone number of the contact. Constraints: Phone number must be specified in the format "+[country dialing code].[number including any area code>]". For example, a US phone number might appear as @"+1.1234567890"@ .
cdPhoneNumber :: Lens' ContactDetail (Maybe Text)
cdPhoneNumber = lens _cdPhoneNumber (\ s a -> s{_cdPhoneNumber = a})

-- | Second line of contact's address, if any.
cdAddressLine2 :: Lens' ContactDetail (Maybe Text)
cdAddressLine2 = lens _cdAddressLine2 (\ s a -> s{_cdAddressLine2 = a})

-- | First name of contact.
cdFirstName :: Lens' ContactDetail (Maybe Text)
cdFirstName = lens _cdFirstName (\ s a -> s{_cdFirstName = a})

-- | Code for the country of the contact's address.
cdCountryCode :: Lens' ContactDetail (Maybe CountryCode)
cdCountryCode = lens _cdCountryCode (\ s a -> s{_cdCountryCode = a})

-- | Indicates whether the contact is a person, company, association, or public organization. If you choose an option other than @PERSON@ , you must enter an organization name, and you can't enable privacy protection for the contact.
cdContactType :: Lens' ContactDetail (Maybe ContactType)
cdContactType = lens _cdContactType (\ s a -> s{_cdContactType = a})

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

instance Hashable ContactDetail where

instance NFData ContactDetail where

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

-- | Information about one suggested domain name.
--
--
--
-- /See:/ 'domainSuggestion' smart constructor.
data DomainSuggestion = DomainSuggestion'
  { _dAvailability :: !(Maybe Text)
  , _dDomainName   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DomainSuggestion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dAvailability' - Whether the domain name is available for registering. Valid values:     * AVAILABLE    * The domain name is available.     * AVAILABLE_RESERVED    * The domain name is reserved under specific conditions.     * AVAILABLE_PREORDER    * The domain name is available and can be preordered.     * DONT_KNOW    * The TLD registry didn't reply with a definitive answer about whether the domain name is available. Amazon Route 53 can return this response for a variety of reasons, for example, the registry is performing maintenance. Try again later.     * PENDING    * The TLD registry didn't return a response in the expected amount of time. When the response is delayed, it usually takes just a few extra seconds. You can resubmit the request immediately.     * RESERVED    * The domain name has been reserved for another person or organization.     * UNAVAILABLE    * The domain name is not available.     * UNAVAILABLE_PREMIUM    * The domain name is not available.     * UNAVAILABLE_RESTRICTED    * The domain name is forbidden.
--
-- * 'dDomainName' - A suggested domain name.
domainSuggestion
    :: DomainSuggestion
domainSuggestion =
  DomainSuggestion' {_dAvailability = Nothing, _dDomainName = Nothing}


-- | Whether the domain name is available for registering. Valid values:     * AVAILABLE    * The domain name is available.     * AVAILABLE_RESERVED    * The domain name is reserved under specific conditions.     * AVAILABLE_PREORDER    * The domain name is available and can be preordered.     * DONT_KNOW    * The TLD registry didn't reply with a definitive answer about whether the domain name is available. Amazon Route 53 can return this response for a variety of reasons, for example, the registry is performing maintenance. Try again later.     * PENDING    * The TLD registry didn't return a response in the expected amount of time. When the response is delayed, it usually takes just a few extra seconds. You can resubmit the request immediately.     * RESERVED    * The domain name has been reserved for another person or organization.     * UNAVAILABLE    * The domain name is not available.     * UNAVAILABLE_PREMIUM    * The domain name is not available.     * UNAVAILABLE_RESTRICTED    * The domain name is forbidden.
dAvailability :: Lens' DomainSuggestion (Maybe Text)
dAvailability = lens _dAvailability (\ s a -> s{_dAvailability = a})

-- | A suggested domain name.
dDomainName :: Lens' DomainSuggestion (Maybe Text)
dDomainName = lens _dDomainName (\ s a -> s{_dDomainName = a})

instance FromJSON DomainSuggestion where
        parseJSON
          = withObject "DomainSuggestion"
              (\ x ->
                 DomainSuggestion' <$>
                   (x .:? "Availability") <*> (x .:? "DomainName"))

instance Hashable DomainSuggestion where

instance NFData DomainSuggestion where

-- | Summary information about one domain.
--
--
--
-- /See:/ 'domainSummary' smart constructor.
data DomainSummary = DomainSummary'
  { _dsExpiry       :: !(Maybe POSIX)
  , _dsTransferLock :: !(Maybe Bool)
  , _dsAutoRenew    :: !(Maybe Bool)
  , _dsDomainName   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DomainSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsExpiry' - Expiration date of the domain in Coordinated Universal Time (UTC).
--
-- * 'dsTransferLock' - Indicates whether a domain is locked from unauthorized transfer to another party.
--
-- * 'dsAutoRenew' - Indicates whether the domain is automatically renewed upon expiration.
--
-- * 'dsDomainName' - The name of the domain that the summary information applies to.
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
dsExpiry :: Lens' DomainSummary (Maybe UTCTime)
dsExpiry = lens _dsExpiry (\ s a -> s{_dsExpiry = a}) . mapping _Time

-- | Indicates whether a domain is locked from unauthorized transfer to another party.
dsTransferLock :: Lens' DomainSummary (Maybe Bool)
dsTransferLock = lens _dsTransferLock (\ s a -> s{_dsTransferLock = a})

-- | Indicates whether the domain is automatically renewed upon expiration.
dsAutoRenew :: Lens' DomainSummary (Maybe Bool)
dsAutoRenew = lens _dsAutoRenew (\ s a -> s{_dsAutoRenew = a})

-- | The name of the domain that the summary information applies to.
dsDomainName :: Lens' DomainSummary Text
dsDomainName = lens _dsDomainName (\ s a -> s{_dsDomainName = a})

instance FromJSON DomainSummary where
        parseJSON
          = withObject "DomainSummary"
              (\ x ->
                 DomainSummary' <$>
                   (x .:? "Expiry") <*> (x .:? "TransferLock") <*>
                     (x .:? "AutoRenew")
                     <*> (x .: "DomainName"))

instance Hashable DomainSummary where

instance NFData DomainSummary where

-- | A complex type that contains information about whether the specified domain can be transferred to Amazon Route 53.
--
--
--
-- /See:/ 'domainTransferability' smart constructor.
newtype DomainTransferability = DomainTransferability'
  { _dtTransferable :: Maybe Transferable
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DomainTransferability' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtTransferable' - Undocumented member.
domainTransferability
    :: DomainTransferability
domainTransferability = DomainTransferability' {_dtTransferable = Nothing}


-- | Undocumented member.
dtTransferable :: Lens' DomainTransferability (Maybe Transferable)
dtTransferable = lens _dtTransferable (\ s a -> s{_dtTransferable = a})

instance FromJSON DomainTransferability where
        parseJSON
          = withObject "DomainTransferability"
              (\ x ->
                 DomainTransferability' <$> (x .:? "Transferable"))

instance Hashable DomainTransferability where

instance NFData DomainTransferability where

-- | ExtraParam includes the following elements.
--
--
--
-- /See:/ 'extraParam' smart constructor.
data ExtraParam = ExtraParam'
  { _epName  :: !ExtraParamName
  , _epValue :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ExtraParam' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'epName' - Name of the additional parameter required by the top-level domain. Here are the top-level domains that require additional parameters and which parameters they require:     * __.com.au and .net.au:__ @AU_ID_NUMBER@ and @AU_ID_TYPE@      * __.ca:__ @BRAND_NUMBER@ , @CA_LEGAL_TYPE@ , and @CA_BUSINESS_ENTITY_TYPE@      * __.es:__ @ES_IDENTIFICATION@ , @ES_IDENTIFICATION_TYPE@ , and @ES_LEGAL_FORM@      * __.fi:__ @BIRTH_DATE_IN_YYYY_MM_DD@ , @FI_BUSINESS_NUMBER@ , @FI_ID_NUMBER@ , @FI_NATIONALITY@ , and @FI_ORGANIZATION_TYPE@      * __.fr:__ @BRAND_NUMBER@ , @BIRTH_DEPARTMENT@ , @BIRTH_DATE_IN_YYYY_MM_DD@ , @BIRTH_COUNTRY@ , and @BIRTH_CITY@      * __.it:__ @BIRTH_COUNTRY@ , @IT_PIN@ , and @IT_REGISTRANT_ENTITY_TYPE@      * __.ru:__ @BIRTH_DATE_IN_YYYY_MM_DD@ and @RU_PASSPORT_DATA@      * __.se:__ @BIRTH_COUNTRY@ and @SE_ID_NUMBER@      * __.sg:__ @SG_ID_NUMBER@      * __.co.uk, .me.uk, and .org.uk:__ @UK_CONTACT_TYPE@ and @UK_COMPANY_NUMBER@  In addition, many TLDs require @VAT_NUMBER@ .
--
-- * 'epValue' - Values corresponding to the additional parameter names required by some top-level domains.
extraParam
    :: ExtraParamName -- ^ 'epName'
    -> Text -- ^ 'epValue'
    -> ExtraParam
extraParam pName_ pValue_ = ExtraParam' {_epName = pName_, _epValue = pValue_}


-- | Name of the additional parameter required by the top-level domain. Here are the top-level domains that require additional parameters and which parameters they require:     * __.com.au and .net.au:__ @AU_ID_NUMBER@ and @AU_ID_TYPE@      * __.ca:__ @BRAND_NUMBER@ , @CA_LEGAL_TYPE@ , and @CA_BUSINESS_ENTITY_TYPE@      * __.es:__ @ES_IDENTIFICATION@ , @ES_IDENTIFICATION_TYPE@ , and @ES_LEGAL_FORM@      * __.fi:__ @BIRTH_DATE_IN_YYYY_MM_DD@ , @FI_BUSINESS_NUMBER@ , @FI_ID_NUMBER@ , @FI_NATIONALITY@ , and @FI_ORGANIZATION_TYPE@      * __.fr:__ @BRAND_NUMBER@ , @BIRTH_DEPARTMENT@ , @BIRTH_DATE_IN_YYYY_MM_DD@ , @BIRTH_COUNTRY@ , and @BIRTH_CITY@      * __.it:__ @BIRTH_COUNTRY@ , @IT_PIN@ , and @IT_REGISTRANT_ENTITY_TYPE@      * __.ru:__ @BIRTH_DATE_IN_YYYY_MM_DD@ and @RU_PASSPORT_DATA@      * __.se:__ @BIRTH_COUNTRY@ and @SE_ID_NUMBER@      * __.sg:__ @SG_ID_NUMBER@      * __.co.uk, .me.uk, and .org.uk:__ @UK_CONTACT_TYPE@ and @UK_COMPANY_NUMBER@  In addition, many TLDs require @VAT_NUMBER@ .
epName :: Lens' ExtraParam ExtraParamName
epName = lens _epName (\ s a -> s{_epName = a})

-- | Values corresponding to the additional parameter names required by some top-level domains.
epValue :: Lens' ExtraParam Text
epValue = lens _epValue (\ s a -> s{_epValue = a})

instance FromJSON ExtraParam where
        parseJSON
          = withObject "ExtraParam"
              (\ x ->
                 ExtraParam' <$> (x .: "Name") <*> (x .: "Value"))

instance Hashable ExtraParam where

instance NFData ExtraParam where

instance ToJSON ExtraParam where
        toJSON ExtraParam'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _epName),
                  Just ("Value" .= _epValue)])

-- | Nameserver includes the following elements.
--
--
--
-- /See:/ 'nameserver' smart constructor.
data Nameserver = Nameserver'
  { _nGlueIPs :: !(Maybe [Text])
  , _nName    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Nameserver' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nGlueIPs' - Glue IP address of a name server entry. Glue IP addresses are required only when the name of the name server is a subdomain of the domain. For example, if your domain is example.com and the name server for the domain is ns.example.com, you need to specify the IP address for ns.example.com. Constraints: The list can contain only one IPv4 and one IPv6 address.
--
-- * 'nName' - The fully qualified host name of the name server. Constraint: Maximum 255 characters
nameserver
    :: Text -- ^ 'nName'
    -> Nameserver
nameserver pName_ = Nameserver' {_nGlueIPs = Nothing, _nName = pName_}


-- | Glue IP address of a name server entry. Glue IP addresses are required only when the name of the name server is a subdomain of the domain. For example, if your domain is example.com and the name server for the domain is ns.example.com, you need to specify the IP address for ns.example.com. Constraints: The list can contain only one IPv4 and one IPv6 address.
nGlueIPs :: Lens' Nameserver [Text]
nGlueIPs = lens _nGlueIPs (\ s a -> s{_nGlueIPs = a}) . _Default . _Coerce

-- | The fully qualified host name of the name server. Constraint: Maximum 255 characters
nName :: Lens' Nameserver Text
nName = lens _nName (\ s a -> s{_nName = a})

instance FromJSON Nameserver where
        parseJSON
          = withObject "Nameserver"
              (\ x ->
                 Nameserver' <$>
                   (x .:? "GlueIps" .!= mempty) <*> (x .: "Name"))

instance Hashable Nameserver where

instance NFData Nameserver where

instance ToJSON Nameserver where
        toJSON Nameserver'{..}
          = object
              (catMaybes
                 [("GlueIps" .=) <$> _nGlueIPs,
                  Just ("Name" .= _nName)])

-- | OperationSummary includes the following elements.
--
--
--
-- /See:/ 'operationSummary' smart constructor.
data OperationSummary = OperationSummary'
  { _osOperationId   :: !Text
  , _osStatus        :: !OperationStatus
  , _osType          :: !OperationType
  , _osSubmittedDate :: !POSIX
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'OperationSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'osOperationId' - Identifier returned to track the requested action.
--
-- * 'osStatus' - The current status of the requested operation in the system.
--
-- * 'osType' - Type of the action requested.
--
-- * 'osSubmittedDate' - The date when the request was submitted.
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
osOperationId :: Lens' OperationSummary Text
osOperationId = lens _osOperationId (\ s a -> s{_osOperationId = a})

-- | The current status of the requested operation in the system.
osStatus :: Lens' OperationSummary OperationStatus
osStatus = lens _osStatus (\ s a -> s{_osStatus = a})

-- | Type of the action requested.
osType :: Lens' OperationSummary OperationType
osType = lens _osType (\ s a -> s{_osType = a})

-- | The date when the request was submitted.
osSubmittedDate :: Lens' OperationSummary UTCTime
osSubmittedDate = lens _osSubmittedDate (\ s a -> s{_osSubmittedDate = a}) . _Time

instance FromJSON OperationSummary where
        parseJSON
          = withObject "OperationSummary"
              (\ x ->
                 OperationSummary' <$>
                   (x .: "OperationId") <*> (x .: "Status") <*>
                     (x .: "Type")
                     <*> (x .: "SubmittedDate"))

instance Hashable OperationSummary where

instance NFData OperationSummary where

-- | Each tag includes the following elements.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagValue :: !(Maybe Text)
  , _tagKey   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue' - The value of a tag. Valid values: A-Z, a-z, 0-9, space, ".:/=+\-@" Constraints: Each value can be 0-256 characters long.
--
-- * 'tagKey' - The key (name) of a tag. Valid values: A-Z, a-z, 0-9, space, ".:/=+\-@" Constraints: Each key can be 1-128 characters long.
tag
    :: Tag
tag = Tag' {_tagValue = Nothing, _tagKey = Nothing}


-- | The value of a tag. Valid values: A-Z, a-z, 0-9, space, ".:/=+\-@" Constraints: Each value can be 0-256 characters long.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

-- | The key (name) of a tag. Valid values: A-Z, a-z, 0-9, space, ".:/=+\-@" Constraints: Each key can be 1-128 characters long.
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .:? "Value") <*> (x .:? "Key"))

instance Hashable Tag where

instance NFData Tag where

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [("Value" .=) <$> _tagValue, ("Key" .=) <$> _tagKey])
