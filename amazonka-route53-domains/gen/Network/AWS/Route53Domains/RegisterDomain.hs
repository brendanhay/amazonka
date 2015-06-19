{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Route53Domains.RegisterDomain
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

-- | This operation registers a domain. Domains are registered by the AWS
-- registrar partner, Gandi. For some top-level domains (TLDs), this
-- operation requires extra parameters.
--
-- When you register a domain, Amazon Route 53 does the following:
--
-- -   Creates a Amazon Route 53 hosted zone that has the same name as the
--     domain. Amazon Route 53 assigns four name servers to your hosted
--     zone and automatically updates your domain registration with the
--     names of these name servers.
-- -   Enables autorenew, so your domain registration will renew
--     automatically each year. We\'ll notify you in advance of the renewal
--     date so you can choose whether to renew the registration.
-- -   Optionally enables privacy protection, so WHOIS queries return
--     contact information for our registrar partner, Gandi, instead of the
--     information you entered for registrant, admin, and tech contacts.
-- -   If registration is successful, returns an operation ID that you can
--     use to track the progress and completion of the action. If the
--     request is not completed successfully, the domain registrant is
--     notified by email.
-- -   Charges your AWS account an amount based on the top-level domain.
--     For more information, see
--     <http://aws.amazon.com/route53/pricing/ Amazon Route 53 Pricing>.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/api-RegisterDomain.html>
module Network.AWS.Route53Domains.RegisterDomain
    (
    -- * Request
      RegisterDomain
    -- ** Request constructor
    , registerDomain
    -- ** Request lenses
    , rdPrivacyProtectTechContact
    , rdPrivacyProtectRegistrantContact
    , rdAutoRenew
    , rdPrivacyProtectAdminContact
    , rdIDNLangCode
    , rdDomainName
    , rdDurationInYears
    , rdAdminContact
    , rdRegistrantContact
    , rdTechContact

    -- * Response
    , RegisterDomainResponse
    -- ** Response constructor
    , registerDomainResponse
    -- ** Response lenses
    , rdrOperationId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53Domains.Types

-- | /See:/ 'registerDomain' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdPrivacyProtectTechContact'
--
-- * 'rdPrivacyProtectRegistrantContact'
--
-- * 'rdAutoRenew'
--
-- * 'rdPrivacyProtectAdminContact'
--
-- * 'rdIDNLangCode'
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
data RegisterDomain = RegisterDomain'{_rdPrivacyProtectTechContact :: Maybe Bool, _rdPrivacyProtectRegistrantContact :: Maybe Bool, _rdAutoRenew :: Maybe Bool, _rdPrivacyProtectAdminContact :: Maybe Bool, _rdIDNLangCode :: Maybe Text, _rdDomainName :: Text, _rdDurationInYears :: Nat, _rdAdminContact :: Sensitive ContactDetail, _rdRegistrantContact :: Sensitive ContactDetail, _rdTechContact :: Sensitive ContactDetail} deriving (Eq, Read, Show)

-- | 'RegisterDomain' smart constructor.
registerDomain :: Text -> Natural -> ContactDetail -> ContactDetail -> ContactDetail -> RegisterDomain
registerDomain pDomainName pDurationInYears pAdminContact pRegistrantContact pTechContact = RegisterDomain'{_rdPrivacyProtectTechContact = Nothing, _rdPrivacyProtectRegistrantContact = Nothing, _rdAutoRenew = Nothing, _rdPrivacyProtectAdminContact = Nothing, _rdIDNLangCode = Nothing, _rdDomainName = pDomainName, _rdDurationInYears = _Nat # pDurationInYears, _rdAdminContact = _Sensitive # pAdminContact, _rdRegistrantContact = _Sensitive # pRegistrantContact, _rdTechContact = _Sensitive # pTechContact};

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
rdPrivacyProtectTechContact :: Lens' RegisterDomain (Maybe Bool)
rdPrivacyProtectTechContact = lens _rdPrivacyProtectTechContact (\ s a -> s{_rdPrivacyProtectTechContact = a});

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
rdPrivacyProtectRegistrantContact :: Lens' RegisterDomain (Maybe Bool)
rdPrivacyProtectRegistrantContact = lens _rdPrivacyProtectRegistrantContact (\ s a -> s{_rdPrivacyProtectRegistrantContact = a});

-- | Indicates whether the domain will be automatically renewed (@true@) or
-- not (@false@). Autorenewal only takes effect after the account is
-- charged.
--
-- Type: Boolean
--
-- Valid values: @true@ | @false@
--
-- Default: @true@
--
-- Required: No
rdAutoRenew :: Lens' RegisterDomain (Maybe Bool)
rdAutoRenew = lens _rdAutoRenew (\ s a -> s{_rdAutoRenew = a});

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
rdPrivacyProtectAdminContact :: Lens' RegisterDomain (Maybe Bool)
rdPrivacyProtectAdminContact = lens _rdPrivacyProtectAdminContact (\ s a -> s{_rdPrivacyProtectAdminContact = a});

-- | Reserved for future use.
rdIDNLangCode :: Lens' RegisterDomain (Maybe Text)
rdIDNLangCode = lens _rdIDNLangCode (\ s a -> s{_rdIDNLangCode = a});

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
rdDomainName :: Lens' RegisterDomain Text
rdDomainName = lens _rdDomainName (\ s a -> s{_rdDomainName = a});

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
rdDurationInYears :: Lens' RegisterDomain Natural
rdDurationInYears = lens _rdDurationInYears (\ s a -> s{_rdDurationInYears = a}) . _Nat;

-- | Provides detailed contact information.
--
-- Type: Complex
--
-- Children: @FirstName@, @MiddleName@, @LastName@, @ContactType@,
-- @OrganizationName@, @AddressLine1@, @AddressLine2@, @City@, @State@,
-- @CountryCode@, @ZipCode@, @PhoneNumber@, @Email@, @Fax@, @ExtraParams@
--
-- Required: Yes
rdAdminContact :: Lens' RegisterDomain ContactDetail
rdAdminContact = lens _rdAdminContact (\ s a -> s{_rdAdminContact = a}) . _Sensitive;

-- | Provides detailed contact information.
--
-- Type: Complex
--
-- Children: @FirstName@, @MiddleName@, @LastName@, @ContactType@,
-- @OrganizationName@, @AddressLine1@, @AddressLine2@, @City@, @State@,
-- @CountryCode@, @ZipCode@, @PhoneNumber@, @Email@, @Fax@, @ExtraParams@
--
-- Required: Yes
rdRegistrantContact :: Lens' RegisterDomain ContactDetail
rdRegistrantContact = lens _rdRegistrantContact (\ s a -> s{_rdRegistrantContact = a}) . _Sensitive;

-- | Provides detailed contact information.
--
-- Type: Complex
--
-- Children: @FirstName@, @MiddleName@, @LastName@, @ContactType@,
-- @OrganizationName@, @AddressLine1@, @AddressLine2@, @City@, @State@,
-- @CountryCode@, @ZipCode@, @PhoneNumber@, @Email@, @Fax@, @ExtraParams@
--
-- Required: Yes
rdTechContact :: Lens' RegisterDomain ContactDetail
rdTechContact = lens _rdTechContact (\ s a -> s{_rdTechContact = a}) . _Sensitive;

instance AWSRequest RegisterDomain where
        type Sv RegisterDomain = Route53Domains
        type Rs RegisterDomain = RegisterDomainResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 RegisterDomainResponse' <$> (x .:> "OperationId"))

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
              ["PrivacyProtectTechContact" .=
                 _rdPrivacyProtectTechContact,
               "PrivacyProtectRegistrantContact" .=
                 _rdPrivacyProtectRegistrantContact,
               "AutoRenew" .= _rdAutoRenew,
               "PrivacyProtectAdminContact" .=
                 _rdPrivacyProtectAdminContact,
               "IdnLangCode" .= _rdIDNLangCode,
               "DomainName" .= _rdDomainName,
               "DurationInYears" .= _rdDurationInYears,
               "AdminContact" .= _rdAdminContact,
               "RegistrantContact" .= _rdRegistrantContact,
               "TechContact" .= _rdTechContact]

instance ToPath RegisterDomain where
        toPath = const "/"

instance ToQuery RegisterDomain where
        toQuery = const mempty

-- | /See:/ 'registerDomainResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdrOperationId'
newtype RegisterDomainResponse = RegisterDomainResponse'{_rdrOperationId :: Text} deriving (Eq, Read, Show)

-- | 'RegisterDomainResponse' smart constructor.
registerDomainResponse :: Text -> RegisterDomainResponse
registerDomainResponse pOperationId = RegisterDomainResponse'{_rdrOperationId = pOperationId};

-- | Identifier for tracking the progress of the request. To use this ID to
-- query the operation status, use GetOperationDetail.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 255 characters.
rdrOperationId :: Lens' RegisterDomainResponse Text
rdrOperationId = lens _rdrOperationId (\ s a -> s{_rdrOperationId = a});
