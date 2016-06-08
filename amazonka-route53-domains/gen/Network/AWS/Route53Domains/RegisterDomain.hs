{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.RegisterDomain
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation registers a domain. Domains are registered by the AWS registrar partner, Gandi. For some top-level domains (TLDs), this operation requires extra parameters.
--
-- When you register a domain, Amazon Route 53 does the following:
--
-- -   Creates a Amazon Route 53 hosted zone that has the same name as the domain. Amazon Route 53 assigns four name servers to your hosted zone and automatically updates your domain registration with the names of these name servers.
-- -   Enables autorenew, so your domain registration will renew automatically each year. We\'ll notify you in advance of the renewal date so you can choose whether to renew the registration.
-- -   Optionally enables privacy protection, so WHOIS queries return contact information for our registrar partner, Gandi, instead of the information you entered for registrant, admin, and tech contacts.
-- -   If registration is successful, returns an operation ID that you can use to track the progress and completion of the action. If the request is not completed successfully, the domain registrant is notified by email.
-- -   Charges your AWS account an amount based on the top-level domain. For more information, see <http://aws.amazon.com/route53/pricing/ Amazon Route 53 Pricing>.
module Network.AWS.Route53Domains.RegisterDomain
    (
    -- * Creating a Request
      registerDomain
    , RegisterDomain
    -- * Request Lenses
    , rdPrivacyProtectTechContact
    , rdPrivacyProtectRegistrantContact
    , rdAutoRenew
    , rdPrivacyProtectAdminContact
    , rdIdNLangCode
    , rdDomainName
    , rdDurationInYears
    , rdAdminContact
    , rdRegistrantContact
    , rdTechContact

    -- * Destructuring the Response
    , registerDomainResponse
    , RegisterDomainResponse
    -- * Response Lenses
    , rdrsResponseStatus
    , rdrsOperationId
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53Domains.Types
import           Network.AWS.Route53Domains.Types.Product

-- | The RegisterDomain request includes the following elements.
--
-- /See:/ 'registerDomain' smart constructor.
data RegisterDomain = RegisterDomain'
    { _rdPrivacyProtectTechContact       :: !(Maybe Bool)
    , _rdPrivacyProtectRegistrantContact :: !(Maybe Bool)
    , _rdAutoRenew                       :: !(Maybe Bool)
    , _rdPrivacyProtectAdminContact      :: !(Maybe Bool)
    , _rdIdNLangCode                     :: !(Maybe Text)
    , _rdDomainName                      :: !Text
    , _rdDurationInYears                 :: !Nat
    , _rdAdminContact                    :: !(Sensitive ContactDetail)
    , _rdRegistrantContact               :: !(Sensitive ContactDetail)
    , _rdTechContact                     :: !(Sensitive ContactDetail)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RegisterDomain' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdPrivacyProtectTechContact'
--
-- * 'rdPrivacyProtectRegistrantContact'
--
-- * 'rdAutoRenew'
--
-- * 'rdPrivacyProtectAdminContact'
--
-- * 'rdIdNLangCode'
--
-- * 'rdDomainName'
--
-- * 'rdDurationInYears'
--
-- * 'rdAdminContact'
--
-- * 'rdRegistrantContact'
--
-- * 'rdTechContact'
registerDomain
    :: Text -- ^ 'rdDomainName'
    -> Natural -- ^ 'rdDurationInYears'
    -> ContactDetail -- ^ 'rdAdminContact'
    -> ContactDetail -- ^ 'rdRegistrantContact'
    -> ContactDetail -- ^ 'rdTechContact'
    -> RegisterDomain
registerDomain pDomainName_ pDurationInYears_ pAdminContact_ pRegistrantContact_ pTechContact_ =
    RegisterDomain'
    { _rdPrivacyProtectTechContact = Nothing
    , _rdPrivacyProtectRegistrantContact = Nothing
    , _rdAutoRenew = Nothing
    , _rdPrivacyProtectAdminContact = Nothing
    , _rdIdNLangCode = Nothing
    , _rdDomainName = pDomainName_
    , _rdDurationInYears = _Nat # pDurationInYears_
    , _rdAdminContact = _Sensitive # pAdminContact_
    , _rdRegistrantContact = _Sensitive # pRegistrantContact_
    , _rdTechContact = _Sensitive # pTechContact_
    }

-- | Whether you want to conceal contact information from WHOIS queries. If you specify true, WHOIS (\"who is\") queries will return contact information for our registrar partner, Gandi, instead of the contact information that you enter.
--
-- Type: Boolean
--
-- Default: 'true'
--
-- Valid values: 'true' | 'false'
--
-- Required: No
rdPrivacyProtectTechContact :: Lens' RegisterDomain (Maybe Bool)
rdPrivacyProtectTechContact = lens _rdPrivacyProtectTechContact (\ s a -> s{_rdPrivacyProtectTechContact = a});

-- | Whether you want to conceal contact information from WHOIS queries. If you specify true, WHOIS (\"who is\") queries will return contact information for our registrar partner, Gandi, instead of the contact information that you enter.
--
-- Type: Boolean
--
-- Default: 'true'
--
-- Valid values: 'true' | 'false'
--
-- Required: No
rdPrivacyProtectRegistrantContact :: Lens' RegisterDomain (Maybe Bool)
rdPrivacyProtectRegistrantContact = lens _rdPrivacyProtectRegistrantContact (\ s a -> s{_rdPrivacyProtectRegistrantContact = a});

-- | Indicates whether the domain will be automatically renewed ('true') or not ('false'). Autorenewal only takes effect after the account is charged.
--
-- Type: Boolean
--
-- Valid values: 'true' | 'false'
--
-- Default: 'true'
--
-- Required: No
rdAutoRenew :: Lens' RegisterDomain (Maybe Bool)
rdAutoRenew = lens _rdAutoRenew (\ s a -> s{_rdAutoRenew = a});

-- | Whether you want to conceal contact information from WHOIS queries. If you specify true, WHOIS (\"who is\") queries will return contact information for our registrar partner, Gandi, instead of the contact information that you enter.
--
-- Type: Boolean
--
-- Default: 'true'
--
-- Valid values: 'true' | 'false'
--
-- Required: No
rdPrivacyProtectAdminContact :: Lens' RegisterDomain (Maybe Bool)
rdPrivacyProtectAdminContact = lens _rdPrivacyProtectAdminContact (\ s a -> s{_rdPrivacyProtectAdminContact = a});

-- | Reserved for future use.
rdIdNLangCode :: Lens' RegisterDomain (Maybe Text)
rdIdNLangCode = lens _rdIdNLangCode (\ s a -> s{_rdIdNLangCode = a});

-- | The name of a domain.
--
-- Type: String
--
-- Default: None
--
-- Constraints: The domain name can contain only the letters a through z, the numbers 0 through 9, and hyphen (-). Internationalized Domain Names are not supported.
--
-- Required: Yes
rdDomainName :: Lens' RegisterDomain Text
rdDomainName = lens _rdDomainName (\ s a -> s{_rdDomainName = a});

-- | The number of years the domain will be registered. Domains are registered for a minimum of one year. The maximum period depends on the top-level domain.
--
-- Type: Integer
--
-- Default: 1
--
-- Valid values: Integer from 1 to 10
--
-- Required: Yes
rdDurationInYears :: Lens' RegisterDomain Natural
rdDurationInYears = lens _rdDurationInYears (\ s a -> s{_rdDurationInYears = a}) . _Nat;

-- | Provides detailed contact information.
--
-- Type: Complex
--
-- Children: 'FirstName', 'MiddleName', 'LastName', 'ContactType', 'OrganizationName', 'AddressLine1', 'AddressLine2', 'City', 'State', 'CountryCode', 'ZipCode', 'PhoneNumber', 'Email', 'Fax', 'ExtraParams'
--
-- Required: Yes
rdAdminContact :: Lens' RegisterDomain ContactDetail
rdAdminContact = lens _rdAdminContact (\ s a -> s{_rdAdminContact = a}) . _Sensitive;

-- | Provides detailed contact information.
--
-- Type: Complex
--
-- Children: 'FirstName', 'MiddleName', 'LastName', 'ContactType', 'OrganizationName', 'AddressLine1', 'AddressLine2', 'City', 'State', 'CountryCode', 'ZipCode', 'PhoneNumber', 'Email', 'Fax', 'ExtraParams'
--
-- Required: Yes
rdRegistrantContact :: Lens' RegisterDomain ContactDetail
rdRegistrantContact = lens _rdRegistrantContact (\ s a -> s{_rdRegistrantContact = a}) . _Sensitive;

-- | Provides detailed contact information.
--
-- Type: Complex
--
-- Children: 'FirstName', 'MiddleName', 'LastName', 'ContactType', 'OrganizationName', 'AddressLine1', 'AddressLine2', 'City', 'State', 'CountryCode', 'ZipCode', 'PhoneNumber', 'Email', 'Fax', 'ExtraParams'
--
-- Required: Yes
rdTechContact :: Lens' RegisterDomain ContactDetail
rdTechContact = lens _rdTechContact (\ s a -> s{_rdTechContact = a}) . _Sensitive;

instance AWSRequest RegisterDomain where
        type Rs RegisterDomain = RegisterDomainResponse
        request = postJSON route53Domains
        response
          = receiveJSON
              (\ s h x ->
                 RegisterDomainResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "OperationId"))

instance Hashable RegisterDomain

instance NFData RegisterDomain

instance ToHeaders RegisterDomain where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53Domains_v20140515.RegisterDomain" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RegisterDomain where
        toJSON RegisterDomain'{..}
          = object
              (catMaybes
                 [("PrivacyProtectTechContact" .=) <$>
                    _rdPrivacyProtectTechContact,
                  ("PrivacyProtectRegistrantContact" .=) <$>
                    _rdPrivacyProtectRegistrantContact,
                  ("AutoRenew" .=) <$> _rdAutoRenew,
                  ("PrivacyProtectAdminContact" .=) <$>
                    _rdPrivacyProtectAdminContact,
                  ("IdnLangCode" .=) <$> _rdIdNLangCode,
                  Just ("DomainName" .= _rdDomainName),
                  Just ("DurationInYears" .= _rdDurationInYears),
                  Just ("AdminContact" .= _rdAdminContact),
                  Just ("RegistrantContact" .= _rdRegistrantContact),
                  Just ("TechContact" .= _rdTechContact)])

instance ToPath RegisterDomain where
        toPath = const "/"

instance ToQuery RegisterDomain where
        toQuery = const mempty

-- | The RegisterDomain response includes the following element.
--
-- /See:/ 'registerDomainResponse' smart constructor.
data RegisterDomainResponse = RegisterDomainResponse'
    { _rdrsResponseStatus :: !Int
    , _rdrsOperationId    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RegisterDomainResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdrsResponseStatus'
--
-- * 'rdrsOperationId'
registerDomainResponse
    :: Int -- ^ 'rdrsResponseStatus'
    -> Text -- ^ 'rdrsOperationId'
    -> RegisterDomainResponse
registerDomainResponse pResponseStatus_ pOperationId_ =
    RegisterDomainResponse'
    { _rdrsResponseStatus = pResponseStatus_
    , _rdrsOperationId = pOperationId_
    }

-- | The response status code.
rdrsResponseStatus :: Lens' RegisterDomainResponse Int
rdrsResponseStatus = lens _rdrsResponseStatus (\ s a -> s{_rdrsResponseStatus = a});

-- | Identifier for tracking the progress of the request. To use this ID to query the operation status, use GetOperationDetail.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 255 characters.
rdrsOperationId :: Lens' RegisterDomainResponse Text
rdrsOperationId = lens _rdrsOperationId (\ s a -> s{_rdrsOperationId = a});

instance NFData RegisterDomainResponse
