{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
-- Pricing. RegisterDomain Example POST / HTTP/1.1
-- host:route53domains.us-east-1.amazonaws.com x-amz-date:20140711T205230Z
-- authorization:AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20140711/us-east-1/route53domains/aws4_request,
-- 
-- SignedHeaders=content-length;content-type;host;user-agent;x-amz-date;x-amz-target,
-- Signature=[calculated-signature]
-- x-amz-target:Route53Domains_v20140515.RegisterDomain
-- user-agent:aws-sdk-java/1.8.3 Linux/2.6.18-164.el5PAE Java_HotSpot (TM
-- )_Server_VM/24.60-b09/1.7.0_60 content-type:application/x-amz-json-1.1
-- content-length:[number of characters in the JSON string] {
-- "DomainName":"example.com", "DurationInYears":1, "AutoRenew":true,
-- "AdminContact":{ "FirstName":"John", "MiddleName":"Richard",
-- "LastName":"Doe", "ContactType":"PERSON", "OrganizationName":"",
-- "AddressLine1":"123 Any Street", "AddressLine2":"", "City":"Any Town",
-- "State":"WA", "CountryCode":"US", "ZipCode":"98101",
-- "PhoneNumber":"+2065550100", "Email":"john@example.com",
-- "Fax":"+2065550101" }, "RegistrantContact":{ "FirstName":"John",
-- "MiddleName":"Richard", "LastName":"Doe", "ContactType":"PERSON",
-- "OrganizationName":"", "AddressLine1":"123 Any Street", "AddressLine2":"",
-- "City":"Any Town", "State":"WA", "CountryCode":"US", "ZipCode":"98101",
-- "PhoneNumber":"+2065550100", "Email":"john@example.com",
-- "Fax":"+2065550101" }, "TechContact":{ "FirstName":"John",
-- "MiddleName":"Richard", "LastName":"Doe", "ContactType":"PERSON",
-- "OrganizationName":"", "AddressLine1":"123 Any Street", "AddressLine2":"",
-- "City":"Any Town", "State":"WA", "CountryCode":"US", "ZipCode":"98101",
-- "PhoneNumber":"+2065550100", "Email":"john@example.com",
-- "Fax":"+2065550101" }, "PrivacyProtectAdminContact":true,
-- "PrivacyProtectRegistrantContact":true, "PrivacyProtectTechContact":true }
-- HTTP/1.1 200 Content-Length:[number of characters in the JSON string] {
-- "OperationId":"308c56712-faa4-40fe-94c8-b423069de3f6" }.
module Network.AWS.Route53Domains.RegisterDomain
    (
    -- * Request
      RegisterDomain
    -- ** Request constructor
    , mkRegisterDomain
    -- ** Request lenses
    , rdDomainName
    , rdIdnLangCode
    , rdDurationInYears
    , rdAutoRenew
    , rdAdminContact
    , rdRegistrantContact
    , rdTechContact
    , rdPrivacyProtectAdminContact
    , rdPrivacyProtectRegistrantContact
    , rdPrivacyProtectTechContact

    -- * Response
    , RegisterDomainResponse
    -- ** Response constructor
    , mkRegisterDomainResponse
    -- ** Response lenses
    , rdrOperationId
    ) where

import Network.AWS.Route53Domains.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | The RegisterDomain request includes the following elements.
data RegisterDomain = RegisterDomain
    { _rdDomainName :: !Text
    , _rdIdnLangCode :: !(Maybe Text)
    , _rdDurationInYears :: !Integer
    , _rdAutoRenew :: !(Maybe Bool)
    , _rdAdminContact :: ContactDetail
    , _rdRegistrantContact :: ContactDetail
    , _rdTechContact :: ContactDetail
    , _rdPrivacyProtectAdminContact :: !(Maybe Bool)
    , _rdPrivacyProtectRegistrantContact :: !(Maybe Bool)
    , _rdPrivacyProtectTechContact :: !(Maybe Bool)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RegisterDomain' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DomainName ::@ @Text@
--
-- * @IdnLangCode ::@ @Maybe Text@
--
-- * @DurationInYears ::@ @Integer@
--
-- * @AutoRenew ::@ @Maybe Bool@
--
-- * @AdminContact ::@ @ContactDetail@
--
-- * @RegistrantContact ::@ @ContactDetail@
--
-- * @TechContact ::@ @ContactDetail@
--
-- * @PrivacyProtectAdminContact ::@ @Maybe Bool@
--
-- * @PrivacyProtectRegistrantContact ::@ @Maybe Bool@
--
-- * @PrivacyProtectTechContact ::@ @Maybe Bool@
--
mkRegisterDomain :: Text -- ^ 'rdDomainName'
                 -> Integer -- ^ 'rdDurationInYears'
                 -> ContactDetail -- ^ 'rdAdminContact'
                 -> ContactDetail -- ^ 'rdRegistrantContact'
                 -> ContactDetail -- ^ 'rdTechContact'
                 -> RegisterDomain
mkRegisterDomain p1 p3 p5 p6 p7 = RegisterDomain
    { _rdDomainName = p1
    , _rdIdnLangCode = Nothing
    , _rdDurationInYears = p3
    , _rdAutoRenew = Nothing
    , _rdAdminContact = p5
    , _rdRegistrantContact = p6
    , _rdTechContact = p7
    , _rdPrivacyProtectAdminContact = Nothing
    , _rdPrivacyProtectRegistrantContact = Nothing
    , _rdPrivacyProtectTechContact = Nothing
    }

-- | The name of a domain. Type: String Default: None Constraints: The domain
-- name can contain only the letters a through z, the numbers 0 through 9, and
-- hyphen (-). Internationalized Domain Names are not supported. Required:
-- Yes.
rdDomainName :: Lens' RegisterDomain Text
rdDomainName = lens _rdDomainName (\s a -> s { _rdDomainName = a })

-- | Reserved for future use.
rdIdnLangCode :: Lens' RegisterDomain (Maybe Text)
rdIdnLangCode = lens _rdIdnLangCode (\s a -> s { _rdIdnLangCode = a })

-- | The number of years the domain will be registered. Domains are registered
-- for a minimum of one year. The maximum period depends on the top-level
-- domain. Type: Integer Default: 1 Valid values: Integer from 1 to 10
-- Required: Yes.
rdDurationInYears :: Lens' RegisterDomain Integer
rdDurationInYears =
    lens _rdDurationInYears (\s a -> s { _rdDurationInYears = a })

-- | Indicates whether the domain will be automatically renewed (true) or not
-- (false). Autorenewal only takes effect after the account is charged. Type:
-- Boolean Valid values: true | false Default: true Required: No.
rdAutoRenew :: Lens' RegisterDomain (Maybe Bool)
rdAutoRenew = lens _rdAutoRenew (\s a -> s { _rdAutoRenew = a })

-- | Provides detailed contact information. Type: Complex Children: FirstName,
-- MiddleName, LastName, ContactType, OrganizationName, AddressLine1,
-- AddressLine2, City, State, CountryCode, ZipCode, PhoneNumber, Email, Fax,
-- ExtraParams Required: Yes.
rdAdminContact :: Lens' RegisterDomain ContactDetail
rdAdminContact = lens _rdAdminContact (\s a -> s { _rdAdminContact = a })

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

-- | Whether you want to conceal contact information from WHOIS queries. If you
-- specify true, WHOIS ("who is") queries will return contact information for
-- our registrar partner, Gandi, instead of the contact information that you
-- enter. Type: Boolean Default: true Valid values: true | false Required: No.
rdPrivacyProtectAdminContact :: Lens' RegisterDomain (Maybe Bool)
rdPrivacyProtectAdminContact =
    lens _rdPrivacyProtectAdminContact
         (\s a -> s { _rdPrivacyProtectAdminContact = a })

-- | Whether you want to conceal contact information from WHOIS queries. If you
-- specify true, WHOIS ("who is") queries will return contact information for
-- our registrar partner, Gandi, instead of the contact information that you
-- enter. Type: Boolean Default: true Valid values: true | false Required: No.
rdPrivacyProtectRegistrantContact :: Lens' RegisterDomain (Maybe Bool)
rdPrivacyProtectRegistrantContact =
    lens _rdPrivacyProtectRegistrantContact
         (\s a -> s { _rdPrivacyProtectRegistrantContact = a })

-- | Whether you want to conceal contact information from WHOIS queries. If you
-- specify true, WHOIS ("who is") queries will return contact information for
-- our registrar partner, Gandi, instead of the contact information that you
-- enter. Type: Boolean Default: true Valid values: true | false Required: No.
rdPrivacyProtectTechContact :: Lens' RegisterDomain (Maybe Bool)
rdPrivacyProtectTechContact =
    lens _rdPrivacyProtectTechContact
         (\s a -> s { _rdPrivacyProtectTechContact = a })

instance ToPath RegisterDomain

instance ToQuery RegisterDomain

instance ToHeaders RegisterDomain

instance ToJSON RegisterDomain

-- | The RegisterDomain response includes the following element.
newtype RegisterDomainResponse = RegisterDomainResponse
    { _rdrOperationId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RegisterDomainResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @OperationId ::@ @Text@
--
mkRegisterDomainResponse :: Text -- ^ 'rdrOperationId'
                         -> RegisterDomainResponse
mkRegisterDomainResponse p1 = RegisterDomainResponse
    { _rdrOperationId = p1
    }

-- | Identifier for tracking the progress of the request. To use this ID to
-- query the operation status, use GetOperationDetail. Type: String Default:
-- None Constraints: Maximum 255 characters.
rdrOperationId :: Lens' RegisterDomainResponse Text
rdrOperationId = lens _rdrOperationId (\s a -> s { _rdrOperationId = a })

instance FromJSON RegisterDomainResponse

instance AWSRequest RegisterDomain where
    type Sv RegisterDomain = Route53Domains
    type Rs RegisterDomain = RegisterDomainResponse

    request = get
    response _ = jsonResponse
