{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.Route53Domains.RegisterDomain
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation registers a domain. Domains are registered by the AWS
-- registrar partner, Gandi. For some top-level domains (TLDs), this operation
-- requires extra parameters. When you register a domain, Amazon Route 53 does
-- the following: Creates a Amazon Route 53 hosted zone that has the same name
-- as the domain. Amazon Route 53 assigns four name servers to your hosted
-- zone and automatically updates your domain registration with the names of
-- these name servers. Enables autorenew, so your domain registration will
-- renew automatically each year. We'll notify you in advance of the renewal
-- date so you can choose whether to renew the registration. Optionally
-- enables privacy protection, so WHOIS queries return contact information for
-- our registrar partner, Gandi, instead of the information you entered for
-- registrant, admin, and tech contacts. If registration is successful,
-- returns an operation ID that you can use to track the progress and
-- completion of the action. If the request is not completed successfully, the
-- domain registrant is notified by email. Charges your AWS account an amount
-- based on the top-level domain. For more information, see Amazon Route 53
-- Pricing.
module Network.AWS.Route53Domains.RegisterDomain
    (
    -- * Request
      RegisterDomain
    -- ** Request constructor
    , registerDomain
    -- ** Request lenses
    , rdAdminContact
    , rdAutoRenew
    , rdDomainName
    , rdDurationInYears
    , rdIdnLangCode
    , rdPrivacyProtectAdminContact
    , rdPrivacyProtectRegistrantContact
    , rdPrivacyProtectTechContact
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
import Network.AWS.Route53Domains.Types
import qualified GHC.Exts

data RegisterDomain = RegisterDomain
    { _rdAdminContact                    :: ContactDetail
    , _rdAutoRenew                       :: Maybe Bool
    , _rdDomainName                      :: Text
    , _rdDurationInYears                 :: Nat
    , _rdIdnLangCode                     :: Maybe Text
    , _rdPrivacyProtectAdminContact      :: Maybe Bool
    , _rdPrivacyProtectRegistrantContact :: Maybe Bool
    , _rdPrivacyProtectTechContact       :: Maybe Bool
    , _rdRegistrantContact               :: ContactDetail
    , _rdTechContact                     :: ContactDetail
    } deriving (Eq, Show, Generic)

-- | 'RegisterDomain' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdAdminContact' @::@ 'ContactDetail'
--
-- * 'rdAutoRenew' @::@ 'Maybe' 'Bool'
--
-- * 'rdDomainName' @::@ 'Text'
--
-- * 'rdDurationInYears' @::@ 'Natural'
--
-- * 'rdIdnLangCode' @::@ 'Maybe' 'Text'
--
-- * 'rdPrivacyProtectAdminContact' @::@ 'Maybe' 'Bool'
--
-- * 'rdPrivacyProtectRegistrantContact' @::@ 'Maybe' 'Bool'
--
-- * 'rdPrivacyProtectTechContact' @::@ 'Maybe' 'Bool'
--
-- * 'rdRegistrantContact' @::@ 'ContactDetail'
--
-- * 'rdTechContact' @::@ 'ContactDetail'
--
registerDomain :: Text -- ^ 'rdDomainName'
               -> Natural -- ^ 'rdDurationInYears'
               -> ContactDetail -- ^ 'rdAdminContact'
               -> ContactDetail -- ^ 'rdRegistrantContact'
               -> ContactDetail -- ^ 'rdTechContact'
               -> RegisterDomain
registerDomain p1 p2 p3 p4 p5 = RegisterDomain
    { _rdDomainName                      = p1
    , _rdDurationInYears                 = withIso _Nat (const id) p2
    , _rdAdminContact                    = p3
    , _rdRegistrantContact               = p4
    , _rdTechContact                     = p5
    , _rdIdnLangCode                     = Nothing
    , _rdAutoRenew                       = Nothing
    , _rdPrivacyProtectAdminContact      = Nothing
    , _rdPrivacyProtectRegistrantContact = Nothing
    , _rdPrivacyProtectTechContact       = Nothing
    }

-- | Provides detailed contact information. Type: Complex Children: FirstName,
-- MiddleName, LastName, ContactType, OrganizationName, AddressLine1,
-- AddressLine2, City, State, CountryCode, ZipCode, PhoneNumber, Email, Fax,
-- ExtraParams Required: Yes.
rdAdminContact :: Lens' RegisterDomain ContactDetail
rdAdminContact = lens _rdAdminContact (\s a -> s { _rdAdminContact = a })

-- | Indicates whether the domain will be automatically renewed (true) or not
-- (false). Autorenewal only takes effect after the account is charged.
-- Type: Boolean Valid values: true | false Default: true Required: No.
rdAutoRenew :: Lens' RegisterDomain (Maybe Bool)
rdAutoRenew = lens _rdAutoRenew (\s a -> s { _rdAutoRenew = a })

-- | The name of a domain. Type: String Default: None Constraints: The domain
-- name can contain only the letters a through z, the numbers 0 through 9,
-- and hyphen (-). Internationalized Domain Names are not supported.
-- Required: Yes.
rdDomainName :: Lens' RegisterDomain Text
rdDomainName = lens _rdDomainName (\s a -> s { _rdDomainName = a })

-- | The number of years the domain will be registered. Domains are registered
-- for a minimum of one year. The maximum period depends on the top-level
-- domain. Type: Integer Default: 1 Valid values: Integer from 1 to 10
-- Required: Yes.
rdDurationInYears :: Lens' RegisterDomain Natural
rdDurationInYears =
    lens _rdDurationInYears (\s a -> s { _rdDurationInYears = a })
        . _Nat

-- | Reserved for future use.
rdIdnLangCode :: Lens' RegisterDomain (Maybe Text)
rdIdnLangCode = lens _rdIdnLangCode (\s a -> s { _rdIdnLangCode = a })

-- | Whether you want to conceal contact information from WHOIS queries. If
-- you specify true, WHOIS ("who is") queries will return contact
-- information for our registrar partner, Gandi, instead of the contact
-- information that you enter. Type: Boolean Default: true Valid values:
-- true | false Required: No.
rdPrivacyProtectAdminContact :: Lens' RegisterDomain (Maybe Bool)
rdPrivacyProtectAdminContact =
    lens _rdPrivacyProtectAdminContact
        (\s a -> s { _rdPrivacyProtectAdminContact = a })

-- | Whether you want to conceal contact information from WHOIS queries. If
-- you specify true, WHOIS ("who is") queries will return contact
-- information for our registrar partner, Gandi, instead of the contact
-- information that you enter. Type: Boolean Default: true Valid values:
-- true | false Required: No.
rdPrivacyProtectRegistrantContact :: Lens' RegisterDomain (Maybe Bool)
rdPrivacyProtectRegistrantContact =
    lens _rdPrivacyProtectRegistrantContact
        (\s a -> s { _rdPrivacyProtectRegistrantContact = a })

-- | Whether you want to conceal contact information from WHOIS queries. If
-- you specify true, WHOIS ("who is") queries will return contact
-- information for our registrar partner, Gandi, instead of the contact
-- information that you enter. Type: Boolean Default: true Valid values:
-- true | false Required: No.
rdPrivacyProtectTechContact :: Lens' RegisterDomain (Maybe Bool)
rdPrivacyProtectTechContact =
    lens _rdPrivacyProtectTechContact
        (\s a -> s { _rdPrivacyProtectTechContact = a })

-- | Provides detailed contact information. Type: Complex Children: FirstName,
-- MiddleName, LastName, ContactType, OrganizationName, AddressLine1,
-- AddressLine2, City, State, CountryCode, ZipCode, PhoneNumber, Email, Fax,
-- ExtraParams Required: Yes.
rdRegistrantContact :: Lens' RegisterDomain ContactDetail
rdRegistrantContact =
    lens _rdRegistrantContact (\s a -> s { _rdRegistrantContact = a })

-- | Provides detailed contact information. Type: Complex Children: FirstName,
-- MiddleName, LastName, ContactType, OrganizationName, AddressLine1,
-- AddressLine2, City, State, CountryCode, ZipCode, PhoneNumber, Email, Fax,
-- ExtraParams Required: Yes.
rdTechContact :: Lens' RegisterDomain ContactDetail
rdTechContact = lens _rdTechContact (\s a -> s { _rdTechContact = a })

instance ToPath RegisterDomain where
    toPath = const "/"

instance ToQuery RegisterDomain where
    toQuery = const mempty

instance ToHeaders RegisterDomain

instance ToBody RegisterDomain where
    toBody = toBody . encode . _rdDomainName

newtype RegisterDomainResponse = RegisterDomainResponse
    { _rdrOperationId :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'RegisterDomainResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdrOperationId' @::@ 'Text'
--
registerDomainResponse :: Text -- ^ 'rdrOperationId'
                       -> RegisterDomainResponse
registerDomainResponse p1 = RegisterDomainResponse
    { _rdrOperationId = p1
    }

-- | Identifier for tracking the progress of the request. To use this ID to
-- query the operation status, use GetOperationDetail. Type: String Default:
-- None Constraints: Maximum 255 characters.
rdrOperationId :: Lens' RegisterDomainResponse Text
rdrOperationId = lens _rdrOperationId (\s a -> s { _rdrOperationId = a })

instance AWSRequest RegisterDomain where
    type Sv RegisterDomain = Route53Domains
    type Rs RegisterDomain = RegisterDomainResponse

    request  = post
    response = jsonResponse $ \h o -> RegisterDomainResponse
        <$> o .: "OperationId"
