{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.RegisterDomain
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation registers a domain. Domains are registered by the AWS
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
    , rdrqPrivacyProtectTechContact
    , rdrqPrivacyProtectRegistrantContact
    , rdrqAutoRenew
    , rdrqPrivacyProtectAdminContact
    , rdrqIdNLangCode
    , rdrqDomainName
    , rdrqDurationInYears
    , rdrqAdminContact
    , rdrqRegistrantContact
    , rdrqTechContact

    -- * Response
    , RegisterDomainResponse
    -- ** Response constructor
    , registerDomainResponse
    -- ** Response lenses
    , rdrsStatus
    , rdrsOperationId
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53Domains.Types

-- | The RegisterDomain request includes the following elements.
--
-- /See:/ 'registerDomain' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdrqPrivacyProtectTechContact'
--
-- * 'rdrqPrivacyProtectRegistrantContact'
--
-- * 'rdrqAutoRenew'
--
-- * 'rdrqPrivacyProtectAdminContact'
--
-- * 'rdrqIdNLangCode'
--
-- * 'rdrqDomainName'
--
-- * 'rdrqDurationInYears'
--
-- * 'rdrqAdminContact'
--
-- * 'rdrqRegistrantContact'
--
-- * 'rdrqTechContact'
data RegisterDomain = RegisterDomain'
    { _rdrqPrivacyProtectTechContact       :: !(Maybe Bool)
    , _rdrqPrivacyProtectRegistrantContact :: !(Maybe Bool)
    , _rdrqAutoRenew                       :: !(Maybe Bool)
    , _rdrqPrivacyProtectAdminContact      :: !(Maybe Bool)
    , _rdrqIdNLangCode                     :: !(Maybe Text)
    , _rdrqDomainName                      :: !Text
    , _rdrqDurationInYears                 :: !Nat
    , _rdrqAdminContact                    :: !(Sensitive ContactDetail)
    , _rdrqRegistrantContact               :: !(Sensitive ContactDetail)
    , _rdrqTechContact                     :: !(Sensitive ContactDetail)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RegisterDomain' smart constructor.
registerDomain :: Text -> Natural -> ContactDetail -> ContactDetail -> ContactDetail -> RegisterDomain
registerDomain pDomainName pDurationInYears pAdminContact pRegistrantContact pTechContact =
    RegisterDomain'
    { _rdrqPrivacyProtectTechContact = Nothing
    , _rdrqPrivacyProtectRegistrantContact = Nothing
    , _rdrqAutoRenew = Nothing
    , _rdrqPrivacyProtectAdminContact = Nothing
    , _rdrqIdNLangCode = Nothing
    , _rdrqDomainName = pDomainName
    , _rdrqDurationInYears = _Nat # pDurationInYears
    , _rdrqAdminContact = _Sensitive # pAdminContact
    , _rdrqRegistrantContact = _Sensitive # pRegistrantContact
    , _rdrqTechContact = _Sensitive # pTechContact
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
rdrqPrivacyProtectTechContact :: Lens' RegisterDomain (Maybe Bool)
rdrqPrivacyProtectTechContact = lens _rdrqPrivacyProtectTechContact (\ s a -> s{_rdrqPrivacyProtectTechContact = a});

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
rdrqPrivacyProtectRegistrantContact :: Lens' RegisterDomain (Maybe Bool)
rdrqPrivacyProtectRegistrantContact = lens _rdrqPrivacyProtectRegistrantContact (\ s a -> s{_rdrqPrivacyProtectRegistrantContact = a});

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
rdrqAutoRenew :: Lens' RegisterDomain (Maybe Bool)
rdrqAutoRenew = lens _rdrqAutoRenew (\ s a -> s{_rdrqAutoRenew = a});

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
rdrqPrivacyProtectAdminContact :: Lens' RegisterDomain (Maybe Bool)
rdrqPrivacyProtectAdminContact = lens _rdrqPrivacyProtectAdminContact (\ s a -> s{_rdrqPrivacyProtectAdminContact = a});

-- | Reserved for future use.
rdrqIdNLangCode :: Lens' RegisterDomain (Maybe Text)
rdrqIdNLangCode = lens _rdrqIdNLangCode (\ s a -> s{_rdrqIdNLangCode = a});

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
rdrqDomainName :: Lens' RegisterDomain Text
rdrqDomainName = lens _rdrqDomainName (\ s a -> s{_rdrqDomainName = a});

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
rdrqDurationInYears :: Lens' RegisterDomain Natural
rdrqDurationInYears = lens _rdrqDurationInYears (\ s a -> s{_rdrqDurationInYears = a}) . _Nat;

-- | Provides detailed contact information.
--
-- Type: Complex
--
-- Children: @FirstName@, @MiddleName@, @LastName@, @ContactType@,
-- @OrganizationName@, @AddressLine1@, @AddressLine2@, @City@, @State@,
-- @CountryCode@, @ZipCode@, @PhoneNumber@, @Email@, @Fax@, @ExtraParams@
--
-- Required: Yes
rdrqAdminContact :: Lens' RegisterDomain ContactDetail
rdrqAdminContact = lens _rdrqAdminContact (\ s a -> s{_rdrqAdminContact = a}) . _Sensitive;

-- | Provides detailed contact information.
--
-- Type: Complex
--
-- Children: @FirstName@, @MiddleName@, @LastName@, @ContactType@,
-- @OrganizationName@, @AddressLine1@, @AddressLine2@, @City@, @State@,
-- @CountryCode@, @ZipCode@, @PhoneNumber@, @Email@, @Fax@, @ExtraParams@
--
-- Required: Yes
rdrqRegistrantContact :: Lens' RegisterDomain ContactDetail
rdrqRegistrantContact = lens _rdrqRegistrantContact (\ s a -> s{_rdrqRegistrantContact = a}) . _Sensitive;

-- | Provides detailed contact information.
--
-- Type: Complex
--
-- Children: @FirstName@, @MiddleName@, @LastName@, @ContactType@,
-- @OrganizationName@, @AddressLine1@, @AddressLine2@, @City@, @State@,
-- @CountryCode@, @ZipCode@, @PhoneNumber@, @Email@, @Fax@, @ExtraParams@
--
-- Required: Yes
rdrqTechContact :: Lens' RegisterDomain ContactDetail
rdrqTechContact = lens _rdrqTechContact (\ s a -> s{_rdrqTechContact = a}) . _Sensitive;

instance AWSRequest RegisterDomain where
        type Sv RegisterDomain = Route53Domains
        type Rs RegisterDomain = RegisterDomainResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 RegisterDomainResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "OperationId"))

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
                 _rdrqPrivacyProtectTechContact,
               "PrivacyProtectRegistrantContact" .=
                 _rdrqPrivacyProtectRegistrantContact,
               "AutoRenew" .= _rdrqAutoRenew,
               "PrivacyProtectAdminContact" .=
                 _rdrqPrivacyProtectAdminContact,
               "IdnLangCode" .= _rdrqIdNLangCode,
               "DomainName" .= _rdrqDomainName,
               "DurationInYears" .= _rdrqDurationInYears,
               "AdminContact" .= _rdrqAdminContact,
               "RegistrantContact" .= _rdrqRegistrantContact,
               "TechContact" .= _rdrqTechContact]

instance ToPath RegisterDomain where
        toPath = const "/"

instance ToQuery RegisterDomain where
        toQuery = const mempty

-- | The RegisterDomain response includes the following element.
--
-- /See:/ 'registerDomainResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdrsStatus'
--
-- * 'rdrsOperationId'
data RegisterDomainResponse = RegisterDomainResponse'
    { _rdrsStatus      :: !Int
    , _rdrsOperationId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RegisterDomainResponse' smart constructor.
registerDomainResponse :: Int -> Text -> RegisterDomainResponse
registerDomainResponse pStatus pOperationId =
    RegisterDomainResponse'
    { _rdrsStatus = pStatus
    , _rdrsOperationId = pOperationId
    }

-- | FIXME: Undocumented member.
rdrsStatus :: Lens' RegisterDomainResponse Int
rdrsStatus = lens _rdrsStatus (\ s a -> s{_rdrsStatus = a});

-- | Identifier for tracking the progress of the request. To use this ID to
-- query the operation status, use GetOperationDetail.
--
-- Type: String
--
-- Default: None
--
-- Constraints: Maximum 255 characters.
rdrsOperationId :: Lens' RegisterDomainResponse Text
rdrsOperationId = lens _rdrsOperationId (\ s a -> s{_rdrsOperationId = a});
