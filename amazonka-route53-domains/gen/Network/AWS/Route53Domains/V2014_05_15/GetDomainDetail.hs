{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
module Network.AWS.Route53Domains.V2014_05_15.GetDomainDetail
    (
    -- * Request
      GetDomainDetail
    -- ** Request constructor
    , mkGetDomainDetail
    -- ** Request lenses
    , gddDomainName

    -- * Response
    , GetDomainDetailResponse
    -- ** Response lenses
    , gddrsDomainName
    , gddrsNameservers
    , gddrsAutoRenew
    , gddrsAdminContact
    , gddrsRegistrantContact
    , gddrsTechContact
    , gddrsAdminPrivacy
    , gddrsRegistrantPrivacy
    , gddrsTechPrivacy
    , gddrsRegistrarName
    , gddrsWhoIsServer
    , gddrsRegistrarUrl
    , gddrsAbuseContactEmail
    , gddrsAbuseContactPhone
    , gddrsRegistryDomainId
    , gddrsCreationDate
    , gddrsUpdatedDate
    , gddrsExpirationDate
    , gddrsReseller
    , gddrsDnsSec
    , gddrsStatusList
    ) where

import           Network.AWS.Route53Domains.V2014_05_15.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | The GetDomainDetail request includes the following element.
newtype GetDomainDetail = GetDomainDetail
    { _gddDomainName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetDomainDetail' request.
mkGetDomainDetail :: Text -- ^ 'gddDomainName'
                  -> GetDomainDetail
mkGetDomainDetail p1 = GetDomainDetail
    { _gddDomainName = p1
    }

-- | The name of a domain. Type: String Default: None Constraints: The domain
-- name can contain only the letters a through z, the numbers 0 through 9, and
-- hyphen (-). Internationalized Domain Names are not supported. Required:
-- Yes.
gddDomainName :: Lens' GetDomainDetail Text
gddDomainName = lens _gddDomainName (\s a -> s { _gddDomainName = a })

instance ToPath GetDomainDetail

instance ToQuery GetDomainDetail

instance ToHeaders GetDomainDetail

instance ToJSON GetDomainDetail

-- | The GetDomainDetail response includes the following elements.
data GetDomainDetailResponse = GetDomainDetailResponse
    { _gddrsDomainName :: Text
    , _gddrsNameservers :: [Nameserver]
    , _gddrsAutoRenew :: Maybe Bool
    , _gddrsAdminContact :: ContactDetail
    , _gddrsRegistrantContact :: ContactDetail
    , _gddrsTechContact :: ContactDetail
    , _gddrsAdminPrivacy :: Maybe Bool
    , _gddrsRegistrantPrivacy :: Maybe Bool
    , _gddrsTechPrivacy :: Maybe Bool
    , _gddrsRegistrarName :: Maybe Text
    , _gddrsWhoIsServer :: Maybe Text
    , _gddrsRegistrarUrl :: Maybe Text
    , _gddrsAbuseContactEmail :: Maybe Text
    , _gddrsAbuseContactPhone :: Maybe Text
    , _gddrsRegistryDomainId :: Maybe Text
    , _gddrsCreationDate :: Maybe ISO8601
    , _gddrsUpdatedDate :: Maybe ISO8601
    , _gddrsExpirationDate :: Maybe ISO8601
    , _gddrsReseller :: Maybe Text
    , _gddrsDnsSec :: Maybe Text
    , _gddrsStatusList :: [Text]
    } deriving (Show, Generic)

-- | The name of a domain. Type: String.
gddrsDomainName :: Lens' GetDomainDetailResponse Text
gddrsDomainName = lens _gddrsDomainName (\s a -> s { _gddrsDomainName = a })

-- | The name of the domain. Type: String.
gddrsNameservers :: Lens' GetDomainDetailResponse [Nameserver]
gddrsNameservers =
    lens _gddrsNameservers (\s a -> s { _gddrsNameservers = a })

-- | Specifies whether the domain registration is set to renew automatically.
-- Type: Boolean.
gddrsAutoRenew :: Lens' GetDomainDetailResponse (Maybe Bool)
gddrsAutoRenew = lens _gddrsAutoRenew (\s a -> s { _gddrsAutoRenew = a })

-- | Provides details about the domain administrative contact. Type: Complex
-- Children: FirstName, MiddleName, LastName, ContactType, OrganizationName,
-- AddressLine1, AddressLine2, City, State, CountryCode, ZipCode, PhoneNumber,
-- Email, Fax, ExtraParams.
gddrsAdminContact :: Lens' GetDomainDetailResponse ContactDetail
gddrsAdminContact =
    lens _gddrsAdminContact (\s a -> s { _gddrsAdminContact = a })

-- | Provides details about the domain registrant. Type: Complex Children:
-- FirstName, MiddleName, LastName, ContactType, OrganizationName,
-- AddressLine1, AddressLine2, City, State, CountryCode, ZipCode, PhoneNumber,
-- Email, Fax, ExtraParams.
gddrsRegistrantContact :: Lens' GetDomainDetailResponse ContactDetail
gddrsRegistrantContact =
    lens _gddrsRegistrantContact (\s a -> s { _gddrsRegistrantContact = a })

-- | Provides details about the domain technical contact. Type: Complex
-- Children: FirstName, MiddleName, LastName, ContactType, OrganizationName,
-- AddressLine1, AddressLine2, City, State, CountryCode, ZipCode, PhoneNumber,
-- Email, Fax, ExtraParams.
gddrsTechContact :: Lens' GetDomainDetailResponse ContactDetail
gddrsTechContact =
    lens _gddrsTechContact (\s a -> s { _gddrsTechContact = a })

-- | Specifies whether contact information for the admin contact is concealed
-- from WHOIS queries. If the value is true, WHOIS ("who is") queries will
-- return contact information for our registrar partner, Gandi, instead of the
-- contact information that you enter. Type: Boolean.
gddrsAdminPrivacy :: Lens' GetDomainDetailResponse (Maybe Bool)
gddrsAdminPrivacy =
    lens _gddrsAdminPrivacy (\s a -> s { _gddrsAdminPrivacy = a })

-- | Specifies whether contact information for the registrant contact is
-- concealed from WHOIS queries. If the value is true, WHOIS ("who is")
-- queries will return contact information for our registrar partner, Gandi,
-- instead of the contact information that you enter. Type: Boolean.
gddrsRegistrantPrivacy :: Lens' GetDomainDetailResponse (Maybe Bool)
gddrsRegistrantPrivacy =
    lens _gddrsRegistrantPrivacy (\s a -> s { _gddrsRegistrantPrivacy = a })

-- | Specifies whether contact information for the tech contact is concealed
-- from WHOIS queries. If the value is true, WHOIS ("who is") queries will
-- return contact information for our registrar partner, Gandi, instead of the
-- contact information that you enter. Type: Boolean.
gddrsTechPrivacy :: Lens' GetDomainDetailResponse (Maybe Bool)
gddrsTechPrivacy =
    lens _gddrsTechPrivacy (\s a -> s { _gddrsTechPrivacy = a })

-- | Name of the registrar of the domain as identified in the registry. Amazon
-- Route 53 domains are registered by registrar Gandi. The value is "GANDI
-- SAS". Type: String.
gddrsRegistrarName :: Lens' GetDomainDetailResponse (Maybe Text)
gddrsRegistrarName =
    lens _gddrsRegistrarName (\s a -> s { _gddrsRegistrarName = a })

-- | The fully qualified name of the WHOIS server that can answer the WHOIS
-- query for the domain. Type: String.
gddrsWhoIsServer :: Lens' GetDomainDetailResponse (Maybe Text)
gddrsWhoIsServer =
    lens _gddrsWhoIsServer (\s a -> s { _gddrsWhoIsServer = a })

-- | Web address of the registrar. Type: String.
gddrsRegistrarUrl :: Lens' GetDomainDetailResponse (Maybe Text)
gddrsRegistrarUrl =
    lens _gddrsRegistrarUrl (\s a -> s { _gddrsRegistrarUrl = a })

-- | Email address to contact to report incorrect contact information for a
-- domain, to report that the domain is being used to send spam, to report
-- that someone is cybersquatting on a domain name, or report some other type
-- of abuse. Type: String.
gddrsAbuseContactEmail :: Lens' GetDomainDetailResponse (Maybe Text)
gddrsAbuseContactEmail =
    lens _gddrsAbuseContactEmail (\s a -> s { _gddrsAbuseContactEmail = a })

-- | Phone number for reporting abuse. Type: String.
gddrsAbuseContactPhone :: Lens' GetDomainDetailResponse (Maybe Text)
gddrsAbuseContactPhone =
    lens _gddrsAbuseContactPhone (\s a -> s { _gddrsAbuseContactPhone = a })

-- | Reserved for future use.
gddrsRegistryDomainId :: Lens' GetDomainDetailResponse (Maybe Text)
gddrsRegistryDomainId =
    lens _gddrsRegistryDomainId (\s a -> s { _gddrsRegistryDomainId = a })

-- | The date when the domain was created as found in the response to a WHOIS
-- query. The date format is Unix time.
gddrsCreationDate :: Lens' GetDomainDetailResponse (Maybe ISO8601)
gddrsCreationDate =
    lens _gddrsCreationDate (\s a -> s { _gddrsCreationDate = a })

-- | The last updated date of the domain as found in the response to a WHOIS
-- query. The date format is Unix time.
gddrsUpdatedDate :: Lens' GetDomainDetailResponse (Maybe ISO8601)
gddrsUpdatedDate =
    lens _gddrsUpdatedDate (\s a -> s { _gddrsUpdatedDate = a })

-- | The date when the registration for the domain is set to expire. The date
-- format is Unix time.
gddrsExpirationDate :: Lens' GetDomainDetailResponse (Maybe ISO8601)
gddrsExpirationDate =
    lens _gddrsExpirationDate (\s a -> s { _gddrsExpirationDate = a })

-- | Reseller of the domain. Domains registered or transferred using Amazon
-- Route 53 domains will have "Amazon" as the reseller. Type: String.
gddrsReseller :: Lens' GetDomainDetailResponse (Maybe Text)
gddrsReseller = lens _gddrsReseller (\s a -> s { _gddrsReseller = a })

-- | Reserved for future use.
gddrsDnsSec :: Lens' GetDomainDetailResponse (Maybe Text)
gddrsDnsSec = lens _gddrsDnsSec (\s a -> s { _gddrsDnsSec = a })

-- | An array of domain name status codes, also known as Extensible Provisioning
-- Protocol (EPP) status codes. ICANN, the organization that maintains a
-- central database of domain names, has developed a set of domain name status
-- codes that tell you the status of a variety of operations on a domain name,
-- for example, registering a domain name, transferring a domain name to
-- another registrar, renewing the registration for a domain name, and so on.
-- All registrars use this same set of status codes. For a current list of
-- domain name status codes and an explanation of what each code means, go to
-- the ICANN website and search for epp status codes. (Search on the ICANN
-- website; web searches sometimes return an old version of the document.)
-- Type: Array of String.
gddrsStatusList :: Lens' GetDomainDetailResponse [Text]
gddrsStatusList = lens _gddrsStatusList (\s a -> s { _gddrsStatusList = a })

instance FromJSON GetDomainDetailResponse

instance AWSRequest GetDomainDetail where
    type Sv GetDomainDetail = Route53Domains
    type Rs GetDomainDetail = GetDomainDetailResponse

    request = get
    response _ = jsonResponse
