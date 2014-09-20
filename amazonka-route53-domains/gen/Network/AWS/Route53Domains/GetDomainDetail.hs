{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53Domains.GetDomainDetail
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
module Network.AWS.Route53Domains.GetDomainDetail
    (
    -- * Request
      GetDomainDetail
    -- ** Request constructor
    , getDomainDetail
    -- ** Request lenses
    , gddDomainName

    -- * Response
    , GetDomainDetailResponse
    -- ** Response constructor
    , getDomainDetailResponse
    -- ** Response lenses
    , gddrDomainName
    , gddrNameservers
    , gddrAutoRenew
    , gddrAdminContact
    , gddrRegistrantContact
    , gddrTechContact
    , gddrAdminPrivacy
    , gddrRegistrantPrivacy
    , gddrTechPrivacy
    , gddrRegistrarName
    , gddrWhoIsServer
    , gddrRegistrarUrl
    , gddrAbuseContactEmail
    , gddrAbuseContactPhone
    , gddrRegistryDomainId
    , gddrCreationDate
    , gddrUpdatedDate
    , gddrExpirationDate
    , gddrReseller
    , gddrDnsSec
    , gddrStatusList
    ) where

import Network.AWS.Route53Domains.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | The GetDomainDetail request includes the following element.
newtype GetDomainDetail = GetDomainDetail
    { _gddDomainName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetDomainDetail' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DomainName ::@ @Text@
--
getDomainDetail :: Text -- ^ 'gddDomainName'
                -> GetDomainDetail
getDomainDetail p1 = GetDomainDetail
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
    { _gddrDomainName :: Text
    , _gddrNameservers :: [Nameserver]
    , _gddrAutoRenew :: Maybe Bool
    , _gddrAdminContact :: ContactDetail
    , _gddrRegistrantContact :: ContactDetail
    , _gddrTechContact :: ContactDetail
    , _gddrAdminPrivacy :: Maybe Bool
    , _gddrRegistrantPrivacy :: Maybe Bool
    , _gddrTechPrivacy :: Maybe Bool
    , _gddrRegistrarName :: Maybe Text
    , _gddrWhoIsServer :: Maybe Text
    , _gddrRegistrarUrl :: Maybe Text
    , _gddrAbuseContactEmail :: Maybe Text
    , _gddrAbuseContactPhone :: Maybe Text
    , _gddrRegistryDomainId :: Maybe Text
    , _gddrCreationDate :: Maybe ISO8601
    , _gddrUpdatedDate :: Maybe ISO8601
    , _gddrExpirationDate :: Maybe ISO8601
    , _gddrReseller :: Maybe Text
    , _gddrDnsSec :: Maybe Text
    , _gddrStatusList :: [Text]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetDomainDetailResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DomainName ::@ @Text@
--
-- * @Nameservers ::@ @[Nameserver]@
--
-- * @AutoRenew ::@ @Maybe Bool@
--
-- * @AdminContact ::@ @ContactDetail@
--
-- * @RegistrantContact ::@ @ContactDetail@
--
-- * @TechContact ::@ @ContactDetail@
--
-- * @AdminPrivacy ::@ @Maybe Bool@
--
-- * @RegistrantPrivacy ::@ @Maybe Bool@
--
-- * @TechPrivacy ::@ @Maybe Bool@
--
-- * @RegistrarName ::@ @Maybe Text@
--
-- * @WhoIsServer ::@ @Maybe Text@
--
-- * @RegistrarUrl ::@ @Maybe Text@
--
-- * @AbuseContactEmail ::@ @Maybe Text@
--
-- * @AbuseContactPhone ::@ @Maybe Text@
--
-- * @RegistryDomainId ::@ @Maybe Text@
--
-- * @CreationDate ::@ @Maybe ISO8601@
--
-- * @UpdatedDate ::@ @Maybe ISO8601@
--
-- * @ExpirationDate ::@ @Maybe ISO8601@
--
-- * @Reseller ::@ @Maybe Text@
--
-- * @DnsSec ::@ @Maybe Text@
--
-- * @StatusList ::@ @[Text]@
--
getDomainDetailResponse :: Text -- ^ 'gddrDomainName'
                        -> [Nameserver] -- ^ 'gddrNameservers'
                        -> ContactDetail -- ^ 'gddrAdminContact'
                        -> ContactDetail -- ^ 'gddrRegistrantContact'
                        -> ContactDetail -- ^ 'gddrTechContact'
                        -> GetDomainDetailResponse
getDomainDetailResponse p1 p2 p4 p5 p6 = GetDomainDetailResponse
    { _gddrDomainName = p1
    , _gddrNameservers = p2
    , _gddrAutoRenew = Nothing
    , _gddrAdminContact = p4
    , _gddrRegistrantContact = p5
    , _gddrTechContact = p6
    , _gddrAdminPrivacy = Nothing
    , _gddrRegistrantPrivacy = Nothing
    , _gddrTechPrivacy = Nothing
    , _gddrRegistrarName = Nothing
    , _gddrWhoIsServer = Nothing
    , _gddrRegistrarUrl = Nothing
    , _gddrAbuseContactEmail = Nothing
    , _gddrAbuseContactPhone = Nothing
    , _gddrRegistryDomainId = Nothing
    , _gddrCreationDate = Nothing
    , _gddrUpdatedDate = Nothing
    , _gddrExpirationDate = Nothing
    , _gddrReseller = Nothing
    , _gddrDnsSec = Nothing
    , _gddrStatusList = mempty
    }

-- | The name of a domain. Type: String.
gddrDomainName :: Lens' GetDomainDetailResponse Text
gddrDomainName = lens _gddrDomainName (\s a -> s { _gddrDomainName = a })

-- | The name of the domain. Type: String.
gddrNameservers :: Lens' GetDomainDetailResponse [Nameserver]
gddrNameservers = lens _gddrNameservers (\s a -> s { _gddrNameservers = a })

-- | Specifies whether the domain registration is set to renew automatically.
-- Type: Boolean.
gddrAutoRenew :: Lens' GetDomainDetailResponse (Maybe Bool)
gddrAutoRenew = lens _gddrAutoRenew (\s a -> s { _gddrAutoRenew = a })

-- | Provides details about the domain administrative contact. Type: Complex
-- Children: FirstName, MiddleName, LastName, ContactType, OrganizationName,
-- AddressLine1, AddressLine2, City, State, CountryCode, ZipCode, PhoneNumber,
-- Email, Fax, ExtraParams.
gddrAdminContact :: Lens' GetDomainDetailResponse ContactDetail
gddrAdminContact =
    lens _gddrAdminContact (\s a -> s { _gddrAdminContact = a })

-- | Provides details about the domain registrant. Type: Complex Children:
-- FirstName, MiddleName, LastName, ContactType, OrganizationName,
-- AddressLine1, AddressLine2, City, State, CountryCode, ZipCode, PhoneNumber,
-- Email, Fax, ExtraParams.
gddrRegistrantContact :: Lens' GetDomainDetailResponse ContactDetail
gddrRegistrantContact =
    lens _gddrRegistrantContact (\s a -> s { _gddrRegistrantContact = a })

-- | Provides details about the domain technical contact. Type: Complex
-- Children: FirstName, MiddleName, LastName, ContactType, OrganizationName,
-- AddressLine1, AddressLine2, City, State, CountryCode, ZipCode, PhoneNumber,
-- Email, Fax, ExtraParams.
gddrTechContact :: Lens' GetDomainDetailResponse ContactDetail
gddrTechContact = lens _gddrTechContact (\s a -> s { _gddrTechContact = a })

-- | Specifies whether contact information for the admin contact is concealed
-- from WHOIS queries. If the value is true, WHOIS ("who is") queries will
-- return contact information for our registrar partner, Gandi, instead of the
-- contact information that you enter. Type: Boolean.
gddrAdminPrivacy :: Lens' GetDomainDetailResponse (Maybe Bool)
gddrAdminPrivacy =
    lens _gddrAdminPrivacy (\s a -> s { _gddrAdminPrivacy = a })

-- | Specifies whether contact information for the registrant contact is
-- concealed from WHOIS queries. If the value is true, WHOIS ("who is")
-- queries will return contact information for our registrar partner, Gandi,
-- instead of the contact information that you enter. Type: Boolean.
gddrRegistrantPrivacy :: Lens' GetDomainDetailResponse (Maybe Bool)
gddrRegistrantPrivacy =
    lens _gddrRegistrantPrivacy (\s a -> s { _gddrRegistrantPrivacy = a })

-- | Specifies whether contact information for the tech contact is concealed
-- from WHOIS queries. If the value is true, WHOIS ("who is") queries will
-- return contact information for our registrar partner, Gandi, instead of the
-- contact information that you enter. Type: Boolean.
gddrTechPrivacy :: Lens' GetDomainDetailResponse (Maybe Bool)
gddrTechPrivacy = lens _gddrTechPrivacy (\s a -> s { _gddrTechPrivacy = a })

-- | Name of the registrar of the domain as identified in the registry. Amazon
-- Route 53 domains are registered by registrar Gandi. The value is "GANDI
-- SAS". Type: String.
gddrRegistrarName :: Lens' GetDomainDetailResponse (Maybe Text)
gddrRegistrarName =
    lens _gddrRegistrarName (\s a -> s { _gddrRegistrarName = a })

-- | The fully qualified name of the WHOIS server that can answer the WHOIS
-- query for the domain. Type: String.
gddrWhoIsServer :: Lens' GetDomainDetailResponse (Maybe Text)
gddrWhoIsServer = lens _gddrWhoIsServer (\s a -> s { _gddrWhoIsServer = a })

-- | Web address of the registrar. Type: String.
gddrRegistrarUrl :: Lens' GetDomainDetailResponse (Maybe Text)
gddrRegistrarUrl =
    lens _gddrRegistrarUrl (\s a -> s { _gddrRegistrarUrl = a })

-- | Email address to contact to report incorrect contact information for a
-- domain, to report that the domain is being used to send spam, to report
-- that someone is cybersquatting on a domain name, or report some other type
-- of abuse. Type: String.
gddrAbuseContactEmail :: Lens' GetDomainDetailResponse (Maybe Text)
gddrAbuseContactEmail =
    lens _gddrAbuseContactEmail (\s a -> s { _gddrAbuseContactEmail = a })

-- | Phone number for reporting abuse. Type: String.
gddrAbuseContactPhone :: Lens' GetDomainDetailResponse (Maybe Text)
gddrAbuseContactPhone =
    lens _gddrAbuseContactPhone (\s a -> s { _gddrAbuseContactPhone = a })

-- | Reserved for future use.
gddrRegistryDomainId :: Lens' GetDomainDetailResponse (Maybe Text)
gddrRegistryDomainId =
    lens _gddrRegistryDomainId (\s a -> s { _gddrRegistryDomainId = a })

-- | The date when the domain was created as found in the response to a WHOIS
-- query. The date format is Unix time.
gddrCreationDate :: Lens' GetDomainDetailResponse (Maybe ISO8601)
gddrCreationDate =
    lens _gddrCreationDate (\s a -> s { _gddrCreationDate = a })

-- | The last updated date of the domain as found in the response to a WHOIS
-- query. The date format is Unix time.
gddrUpdatedDate :: Lens' GetDomainDetailResponse (Maybe ISO8601)
gddrUpdatedDate = lens _gddrUpdatedDate (\s a -> s { _gddrUpdatedDate = a })

-- | The date when the registration for the domain is set to expire. The date
-- format is Unix time.
gddrExpirationDate :: Lens' GetDomainDetailResponse (Maybe ISO8601)
gddrExpirationDate =
    lens _gddrExpirationDate (\s a -> s { _gddrExpirationDate = a })

-- | Reseller of the domain. Domains registered or transferred using Amazon
-- Route 53 domains will have "Amazon" as the reseller. Type: String.
gddrReseller :: Lens' GetDomainDetailResponse (Maybe Text)
gddrReseller = lens _gddrReseller (\s a -> s { _gddrReseller = a })

-- | Reserved for future use.
gddrDnsSec :: Lens' GetDomainDetailResponse (Maybe Text)
gddrDnsSec = lens _gddrDnsSec (\s a -> s { _gddrDnsSec = a })

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
gddrStatusList :: Lens' GetDomainDetailResponse [Text]
gddrStatusList = lens _gddrStatusList (\s a -> s { _gddrStatusList = a })

instance FromJSON GetDomainDetailResponse

instance AWSRequest GetDomainDetail where
    type Sv GetDomainDetail = Route53Domains
    type Rs GetDomainDetail = GetDomainDetailResponse

    request = get
    response _ = jsonResponse
