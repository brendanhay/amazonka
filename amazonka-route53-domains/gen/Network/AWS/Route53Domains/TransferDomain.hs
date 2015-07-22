{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.TransferDomain
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation transfers a domain from another registrar to Amazon Route
-- 53. When the transfer is complete, the domain is registered with the AWS
-- registrar partner, Gandi.
--
-- For transfer requirements, a detailed procedure, and information about
-- viewing the status of a domain transfer, see
-- <http://docs.aws.amazon.com/Route53/latest/DeveloperGuide/domain-transfer-to-route-53.html Transferring Registration for a Domain to Amazon Route 53>
-- in the Amazon Route 53 Developer Guide.
--
-- If the registrar for your domain is also the DNS service provider for
-- the domain, we highly recommend that you consider transferring your DNS
-- service to Amazon Route 53 or to another DNS service provider before you
-- transfer your registration. Some registrars provide free DNS service
-- when you purchase a domain registration. When you transfer the
-- registration, the previous registrar will not renew your domain
-- registration and could end your DNS service at any time.
--
-- Caution! If the registrar for your domain is also the DNS service
-- provider for the domain and you don\'t transfer DNS service to another
-- provider, your website, email, and the web applications associated with
-- the domain might become unavailable.
--
-- If the transfer is successful, this method returns an operation ID that
-- you can use to track the progress and completion of the action. If the
-- transfer doesn\'t complete successfully, the domain registrant will be
-- notified by email.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/api-TransferDomain.html>
module Network.AWS.Route53Domains.TransferDomain
    (
    -- * Request
      TransferDomain
    -- ** Request constructor
    , transferDomain
    -- ** Request lenses
    , tdrqPrivacyProtectTechContact
    , tdrqPrivacyProtectRegistrantContact
    , tdrqAutoRenew
    , tdrqPrivacyProtectAdminContact
    , tdrqIdNLangCode
    , tdrqAuthCode
    , tdrqNameservers
    , tdrqDomainName
    , tdrqDurationInYears
    , tdrqAdminContact
    , tdrqRegistrantContact
    , tdrqTechContact

    -- * Response
    , TransferDomainResponse
    -- ** Response constructor
    , transferDomainResponse
    -- ** Response lenses
    , tdrsStatus
    , tdrsOperationId
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53Domains.Types

-- | The TransferDomain request includes the following elements.
--
-- /See:/ 'transferDomain' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tdrqPrivacyProtectTechContact'
--
-- * 'tdrqPrivacyProtectRegistrantContact'
--
-- * 'tdrqAutoRenew'
--
-- * 'tdrqPrivacyProtectAdminContact'
--
-- * 'tdrqIdNLangCode'
--
-- * 'tdrqAuthCode'
--
-- * 'tdrqNameservers'
--
-- * 'tdrqDomainName'
--
-- * 'tdrqDurationInYears'
--
-- * 'tdrqAdminContact'
--
-- * 'tdrqRegistrantContact'
--
-- * 'tdrqTechContact'
data TransferDomain = TransferDomain'
    { _tdrqPrivacyProtectTechContact       :: !(Maybe Bool)
    , _tdrqPrivacyProtectRegistrantContact :: !(Maybe Bool)
    , _tdrqAutoRenew                       :: !(Maybe Bool)
    , _tdrqPrivacyProtectAdminContact      :: !(Maybe Bool)
    , _tdrqIdNLangCode                     :: !(Maybe Text)
    , _tdrqAuthCode                        :: !(Maybe (Sensitive Text))
    , _tdrqNameservers                     :: !(Maybe [Nameserver])
    , _tdrqDomainName                      :: !Text
    , _tdrqDurationInYears                 :: !Nat
    , _tdrqAdminContact                    :: !(Sensitive ContactDetail)
    , _tdrqRegistrantContact               :: !(Sensitive ContactDetail)
    , _tdrqTechContact                     :: !(Sensitive ContactDetail)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'TransferDomain' smart constructor.
transferDomain :: Text -> Natural -> ContactDetail -> ContactDetail -> ContactDetail -> TransferDomain
transferDomain pDomainName_ pDurationInYears_ pAdminContact_ pRegistrantContact_ pTechContact_ =
    TransferDomain'
    { _tdrqPrivacyProtectTechContact = Nothing
    , _tdrqPrivacyProtectRegistrantContact = Nothing
    , _tdrqAutoRenew = Nothing
    , _tdrqPrivacyProtectAdminContact = Nothing
    , _tdrqIdNLangCode = Nothing
    , _tdrqAuthCode = Nothing
    , _tdrqNameservers = Nothing
    , _tdrqDomainName = pDomainName_
    , _tdrqDurationInYears = _Nat # pDurationInYears_
    , _tdrqAdminContact = _Sensitive # pAdminContact_
    , _tdrqRegistrantContact = _Sensitive # pRegistrantContact_
    , _tdrqTechContact = _Sensitive # pTechContact_
    }

-- | Whether you want to conceal contact information from WHOIS queries. If
-- you specify true, WHOIS (\"who is\") queries will return contact
-- information for our registrar partner, Gandi, instead of the contact
-- information that you enter.
--
-- Type: Boolean
--
-- Default: @true@
--
-- Valid values: @true@ | @false@
--
-- Required: No
tdrqPrivacyProtectTechContact :: Lens' TransferDomain (Maybe Bool)
tdrqPrivacyProtectTechContact = lens _tdrqPrivacyProtectTechContact (\ s a -> s{_tdrqPrivacyProtectTechContact = a});

-- | Whether you want to conceal contact information from WHOIS queries. If
-- you specify true, WHOIS (\"who is\") queries will return contact
-- information for our registrar partner, Gandi, instead of the contact
-- information that you enter.
--
-- Type: Boolean
--
-- Default: @true@
--
-- Valid values: @true@ | @false@
--
-- Required: No
tdrqPrivacyProtectRegistrantContact :: Lens' TransferDomain (Maybe Bool)
tdrqPrivacyProtectRegistrantContact = lens _tdrqPrivacyProtectRegistrantContact (\ s a -> s{_tdrqPrivacyProtectRegistrantContact = a});

-- | Indicates whether the domain will be automatically renewed (true) or not
-- (false). Autorenewal only takes effect after the account is charged.
--
-- Type: Boolean
--
-- Valid values: @true@ | @false@
--
-- Default: true
--
-- Required: No
tdrqAutoRenew :: Lens' TransferDomain (Maybe Bool)
tdrqAutoRenew = lens _tdrqAutoRenew (\ s a -> s{_tdrqAutoRenew = a});

-- | Whether you want to conceal contact information from WHOIS queries. If
-- you specify true, WHOIS (\"who is\") queries will return contact
-- information for our registrar partner, Gandi, instead of the contact
-- information that you enter.
--
-- Type: Boolean
--
-- Default: @true@
--
-- Valid values: @true@ | @false@
--
-- Required: No
tdrqPrivacyProtectAdminContact :: Lens' TransferDomain (Maybe Bool)
tdrqPrivacyProtectAdminContact = lens _tdrqPrivacyProtectAdminContact (\ s a -> s{_tdrqPrivacyProtectAdminContact = a});

-- | Reserved for future use.
tdrqIdNLangCode :: Lens' TransferDomain (Maybe Text)
tdrqIdNLangCode = lens _tdrqIdNLangCode (\ s a -> s{_tdrqIdNLangCode = a});

-- | The authorization code for the domain. You get this value from the
-- current registrar.
--
-- Type: String
--
-- Required: Yes
tdrqAuthCode :: Lens' TransferDomain (Maybe Text)
tdrqAuthCode = lens _tdrqAuthCode (\ s a -> s{_tdrqAuthCode = a}) . mapping _Sensitive;

-- | Contains details for the host and glue IP addresses.
--
-- Type: Complex
--
-- Children: @GlueIps@, @Name@
--
-- Required: No
tdrqNameservers :: Lens' TransferDomain [Nameserver]
tdrqNameservers = lens _tdrqNameservers (\ s a -> s{_tdrqNameservers = a}) . _Default;

-- | The name of a domain.
--
-- Type: String
--
-- Default: None
--
-- Constraints: The domain name can contain only the letters a through z,
-- the numbers 0 through 9, and hyphen (-). Internationalized Domain Names
-- are not supported.
--
-- Required: Yes
tdrqDomainName :: Lens' TransferDomain Text
tdrqDomainName = lens _tdrqDomainName (\ s a -> s{_tdrqDomainName = a});

-- | The number of years the domain will be registered. Domains are
-- registered for a minimum of one year. The maximum period depends on the
-- top-level domain.
--
-- Type: Integer
--
-- Default: 1
--
-- Valid values: Integer from 1 to 10
--
-- Required: Yes
tdrqDurationInYears :: Lens' TransferDomain Natural
tdrqDurationInYears = lens _tdrqDurationInYears (\ s a -> s{_tdrqDurationInYears = a}) . _Nat;

-- | Provides detailed contact information.
--
-- Type: Complex
--
-- Children: @FirstName@, @MiddleName@, @LastName@, @ContactType@,
-- @OrganizationName@, @AddressLine1@, @AddressLine2@, @City@, @State@,
-- @CountryCode@, @ZipCode@, @PhoneNumber@, @Email@, @Fax@, @ExtraParams@
--
-- Required: Yes
tdrqAdminContact :: Lens' TransferDomain ContactDetail
tdrqAdminContact = lens _tdrqAdminContact (\ s a -> s{_tdrqAdminContact = a}) . _Sensitive;

-- | Provides detailed contact information.
--
-- Type: Complex
--
-- Children: @FirstName@, @MiddleName@, @LastName@, @ContactType@,
-- @OrganizationName@, @AddressLine1@, @AddressLine2@, @City@, @State@,
-- @CountryCode@, @ZipCode@, @PhoneNumber@, @Email@, @Fax@, @ExtraParams@
--
-- Required: Yes
tdrqRegistrantContact :: Lens' TransferDomain ContactDetail
tdrqRegistrantContact = lens _tdrqRegistrantContact (\ s a -> s{_tdrqRegistrantContact = a}) . _Sensitive;

-- | Provides detailed contact information.
--
-- Type: Complex
--
-- Children: @FirstName@, @MiddleName@, @LastName@, @ContactType@,
-- @OrganizationName@, @AddressLine1@, @AddressLine2@, @City@, @State@,
-- @CountryCode@, @ZipCode@, @PhoneNumber@, @Email@, @Fax@, @ExtraParams@
--
-- Required: Yes
tdrqTechContact :: Lens' TransferDomain ContactDetail
tdrqTechContact = lens _tdrqTechContact (\ s a -> s{_tdrqTechContact = a}) . _Sensitive;

instance AWSRequest TransferDomain where
        type Sv TransferDomain = Route53Domains
        type Rs TransferDomain = TransferDomainResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 TransferDomainResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "OperationId"))

instance ToHeaders TransferDomain where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53Domains_v20140515.TransferDomain" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON TransferDomain where
        toJSON TransferDomain'{..}
          = object
              ["PrivacyProtectTechContact" .=
                 _tdrqPrivacyProtectTechContact,
               "PrivacyProtectRegistrantContact" .=
                 _tdrqPrivacyProtectRegistrantContact,
               "AutoRenew" .= _tdrqAutoRenew,
               "PrivacyProtectAdminContact" .=
                 _tdrqPrivacyProtectAdminContact,
               "IdnLangCode" .= _tdrqIdNLangCode,
               "AuthCode" .= _tdrqAuthCode,
               "Nameservers" .= _tdrqNameservers,
               "DomainName" .= _tdrqDomainName,
               "DurationInYears" .= _tdrqDurationInYears,
               "AdminContact" .= _tdrqAdminContact,
               "RegistrantContact" .= _tdrqRegistrantContact,
               "TechContact" .= _tdrqTechContact]

instance ToPath TransferDomain where
        toPath = const "/"

instance ToQuery TransferDomain where
        toQuery = const mempty

-- | The TranserDomain response includes the following element.
--
-- /See:/ 'transferDomainResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tdrsStatus'
--
-- * 'tdrsOperationId'
data TransferDomainResponse = TransferDomainResponse'
    { _tdrsStatus      :: !Int
    , _tdrsOperationId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'TransferDomainResponse' smart constructor.
transferDomainResponse :: Int -> Text -> TransferDomainResponse
transferDomainResponse pStatus_ pOperationId_ =
    TransferDomainResponse'
    { _tdrsStatus = pStatus_
    , _tdrsOperationId = pOperationId_
    }

-- | FIXME: Undocumented member.
tdrsStatus :: Lens' TransferDomainResponse Int
tdrsStatus = lens _tdrsStatus (\ s a -> s{_tdrsStatus = a});

-- | Identifier for tracking the progress of the request. To use this ID to
-- query the operation status, use GetOperationDetail.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 255 characters.
tdrsOperationId :: Lens' TransferDomainResponse Text
tdrsOperationId = lens _tdrsOperationId (\ s a -> s{_tdrsOperationId = a});
