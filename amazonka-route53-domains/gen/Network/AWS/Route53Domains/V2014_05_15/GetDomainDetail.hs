{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53Domains.V2014_05_15.GetDomainDetail
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation returns detailed information about the domain. The domain's
-- contact information is also returned as part of the output. GetDomainDetail
-- Example POST / HTTP/1.1 host:route53domains.us-east-1.amazonaws.com
-- x-amz-date:20140711T205230Z authorization:AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20140711/us-east-1/route53domains/aws4_request,
-- 
-- SignedHeaders=content-length;content-type;host;user-agent;x-amz-date;x-amz-target,
-- Signature=[calculated-signature]
-- x-amz-target:Route53Domains_v20140515.GetDomainDetail
-- user-agent:aws-sdk-java/1.8.3 Linux/2.6.18-164.el5PAE Java_HotSpot (TM
-- )_Server_VM/24.60-b09/1.7.0_60 content-type:application/x-amz-json-1.1
-- content-length:[number of characters in the JSON string] {
-- "DomainName":"example.com" } HTTP/1.1 200 Content-Length:[number of
-- characters in the JSON string] {
-- "AbuseContactEmail":"abuse@support.gandi.net",
-- "AbuseContactPhone":"+33.170377661", "AdminContact":{ "AddressLine1":"1 Any
-- Street", "AddressLine2":"", "City":"Anytown", "CountryCode":"US",
-- "Email":"john@example.com", "ExtraParams":[ ], "FirstName":"John",
-- "LastName":"Doe", "PhoneNumber":"+2065550100", "State":"WA",
-- "ZipCode":"98101" }, "AdminPrivacy":true, "AutoRenew":true,
-- "CreationDate":1400010459, "DomainName":"example.com",
-- "ExpirationDate":1431539259, "Nameservers":[ { "GlueIps":[ ],
-- "Name":"ns-2048.awsdns-64.com" }, { "GlueIps":[ ],
-- "Name":"ns-2051.awsdns-67.co.uk" }, { "GlueIps":[ ],
-- "Name":"ns-2050.awsdns-66.org" }, { "GlueIps":[ ],
-- "Name":"ns-2049.awsdns-65.net" } ], "RegistrantContact":{ "AddressLine1":"1
-- Any Street", "AddressLine2":"", "City":"Anytown", "CountryCode":"US",
-- "Email":"john@example.com", "ExtraParams":[ ], "FirstName":"John",
-- "LastName":"Doe", "PhoneNumber":"+2065550100", "State":"WA",
-- "ZipCode":"98101" }, "RegistrantPrivacy":true, "RegistrarName":"GANDI SAS",
-- "RegistrarUrl":"http://www.gandi.net", "Reseller":"Amazon", "StatusList":[
-- "clientTransferProhibited" ], "TechContact":{ "AddressLine1":"1 Any
-- Street", "AddressLine2":"", "City":"Anytown", "CountryCode":"US",
-- "Email":"john@example.com", "ExtraParams":[ ], "FirstName":"John",
-- "LastName":"Doe", "PhoneNumber":"+2065550100", "State":"WA",
-- "ZipCode":"98101" }, "TechPrivacy":true, "UpdatedDate":1400010459,
-- "WhoIsServer":"whois.gandi.net" }.
module Network.AWS.Route53Domains.V2014_05_15.GetDomainDetail where

import           Network.AWS.Route53Domains.V2014_05_15.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

data GetDomainDetail = GetDomainDetail
    { _gddrDomainName :: Text
      -- ^ The name of a domain. Type: String Default: None Constraints: The
      -- domain name can contain only the letters a through z, the numbers
      -- 0 through 9, and hyphen (-). Internationalized Domain Names are
      -- not supported. Required: Yes.
    } deriving (Show, Generic)

makeLenses ''GetDomainDetail

instance ToPath GetDomainDetail

instance ToQuery GetDomainDetail

instance ToHeaders GetDomainDetail

instance ToJSON GetDomainDetail

data GetDomainDetailResponse = GetDomainDetailResponse
    { _gddsRegistrantContact :: ContactDetail
      -- ^ Provides details about the domain registrant. Type: Complex
      -- Children: FirstName, MiddleName, LastName, ContactType,
      -- OrganizationName, AddressLine1, AddressLine2, City, State,
      -- CountryCode, ZipCode, PhoneNumber, Email, Fax, ExtraParams.
    , _gddsAdminContact :: ContactDetail
      -- ^ Provides details about the domain administrative contact. Type:
      -- Complex Children: FirstName, MiddleName, LastName, ContactType,
      -- OrganizationName, AddressLine1, AddressLine2, City, State,
      -- CountryCode, ZipCode, PhoneNumber, Email, Fax, ExtraParams.
    , _gddsTechContact :: ContactDetail
      -- ^ Provides details about the domain technical contact. Type:
      -- Complex Children: FirstName, MiddleName, LastName, ContactType,
      -- OrganizationName, AddressLine1, AddressLine2, City, State,
      -- CountryCode, ZipCode, PhoneNumber, Email, Fax, ExtraParams.
    , _gddsDomainName :: Text
      -- ^ The name of a domain. Type: String.
    , _gddsNameservers :: [Nameserver]
      -- ^ The name of the domain. Type: String.
    , _gddsTechPrivacy :: Maybe Bool
      -- ^ Specifies whether contact information for the tech contact is
      -- concealed from WHOIS queries. If the value is true, WHOIS ("who
      -- is") queries will return contact information for our registrar
      -- partner, Gandi, instead of the contact information that you
      -- enter. Type: Boolean.
    , _gddsRegistrantPrivacy :: Maybe Bool
      -- ^ Specifies whether contact information for the registrant contact
      -- is concealed from WHOIS queries. If the value is true, WHOIS
      -- ("who is") queries will return contact information for our
      -- registrar partner, Gandi, instead of the contact information that
      -- you enter. Type: Boolean.
    , _gddsAdminPrivacy :: Maybe Bool
      -- ^ Specifies whether contact information for the admin contact is
      -- concealed from WHOIS queries. If the value is true, WHOIS ("who
      -- is") queries will return contact information for our registrar
      -- partner, Gandi, instead of the contact information that you
      -- enter. Type: Boolean.
    , _gddsAutoRenew :: Maybe Bool
      -- ^ Specifies whether the domain registration is set to renew
      -- automatically. Type: Boolean.
    , _gddsAbuseContactPhone :: Maybe Text
      -- ^ Phone number for reporting abuse. Type: String.
    , _gddsDnsSec :: Maybe Text
      -- ^ Reserved for future use.
    , _gddsStatusList :: [Text]
      -- ^ An array of domain name status codes, also known as Extensible
      -- Provisioning Protocol (EPP) status codes. ICANN, the organization
      -- that maintains a central database of domain names, has developed
      -- a set of domain name status codes that tell you the status of a
      -- variety of operations on a domain name, for example, registering
      -- a domain name, transferring a domain name to another registrar,
      -- renewing the registration for a domain name, and so on. All
      -- registrars use this same set of status codes. For a current list
      -- of domain name status codes and an explanation of what each code
      -- means, go to the ICANN website and search for epp status codes.
      -- (Search on the ICANN website; web searches sometimes return an
      -- old version of the document.) Type: Array of String.
    , _gddsAbuseContactEmail :: Maybe Text
      -- ^ Email address to contact to report incorrect contact information
      -- for a domain, to report that the domain is being used to send
      -- spam, to report that someone is cybersquatting on a domain name,
      -- or report some other type of abuse. Type: String.
    , _gddsRegistrarName :: Maybe Text
      -- ^ Name of the registrar of the domain as identified in the
      -- registry. Amazon Route 53 domains are registered by registrar
      -- Gandi. The value is "GANDI SAS". Type: String.
    , _gddsRegistrarUrl :: Maybe Text
      -- ^ Web address of the registrar. Type: String.
    , _gddsWhoIsServer :: Maybe Text
      -- ^ The fully qualified name of the WHOIS server that can answer the
      -- WHOIS query for the domain. Type: String.
    , _gddsRegistryDomainId :: Maybe Text
      -- ^ Reserved for future use.
    , _gddsReseller :: Maybe Text
      -- ^ Reseller of the domain. Domains registered or transferred using
      -- Amazon Route 53 domains will have "Amazon" as the reseller. Type:
      -- String.
    , _gddsUpdatedDate :: Maybe ISO8601
      -- ^ The last updated date of the domain as found in the response to a
      -- WHOIS query. The date format is Unix time.
    , _gddsExpirationDate :: Maybe ISO8601
      -- ^ The date when the registration for the domain is set to expire.
      -- The date format is Unix time.
    , _gddsCreationDate :: Maybe ISO8601
      -- ^ The date when the domain was created as found in the response to
      -- a WHOIS query. The date format is Unix time.
    } deriving (Show, Generic)

makeLenses ''GetDomainDetailResponse

instance FromJSON GetDomainDetailResponse

instance AWSRequest GetDomainDetail where
    type Sv GetDomainDetail = Route53Domains
    type Rs GetDomainDetail = GetDomainDetailResponse

    request = get
    response _ = jsonResponse
