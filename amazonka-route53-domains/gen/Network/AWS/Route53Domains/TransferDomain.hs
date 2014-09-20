{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53Domains.TransferDomain
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation transfers a domain from another registrar to Amazon Route
-- 53. Domains are registered by the AWS registrar, Gandi upon transfer. To
-- transfer a domain, you need to meet all the domain transfer criteria,
-- including the following: You must supply nameservers to transfer a domain.
-- You must disable the domain transfer lock (if any) before transferring the
-- domain. A minimum of 60 days must have elapsed since the domain's
-- registration or last transfer. We recommend you use the Amazon Route 53 as
-- the DNS service for your domain. You can create a hosted zone in Amazon
-- Route 53 for your current domain before transferring your domain. Note that
-- upon transfer, the domain duration is extended for a year if not otherwise
-- specified. Autorenew is enabled by default. If the transfer is successful,
-- this method returns an operation ID that you can use to track the progress
-- and completion of the action. If the request is not completed successfully,
-- the domain registrant will be notified by email. Transferring domains
-- charges your AWS account an amount based on the top-level domain. For more
-- information, see Amazon Route 53 Pricing. TransferDomain Example POST /
-- HTTP/1.1 host:route53domains.us-east-1.amazonaws.com
-- x-amz-date:20140711T205230Z authorization:AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20140711/us-east-1/route53domains/aws4_request,
-- 
-- SignedHeaders=content-length;content-type;host;user-agent;x-amz-date;x-amz-target,
-- Signature=[calculated-signature]
-- x-amz-target:Route53Domains_v20140515.TransferDomain
-- user-agent:aws-sdk-java/1.8.3 Linux/2.6.18-164.el5PAE Java_HotSpot (TM
-- )_Server_VM/24.60-b09/1.7.0_60 content-type:application/x-amz-json-1.1
-- content-length:[number of characters in the JSON string] {
-- "DomainName":"example.com", "DurationInYears":1, "Nameservers":[ {
-- "Name":"ns-2048.awsdns-64.com", "GlueIps":[ "192.0.2.11" ] }, {
-- "Name":"ns-2049.awsdns-65.net", "GlueIps":[ "192.0.2.12" ] } ],
-- "AuthCode":"a42qxjz1", "AutoRenew":true, "AdminContact":{
-- "FirstName":"John", "MiddleName":"Richard", "LastName":"Doe",
-- "ContactType":"PERSON", "OrganizationName":"", "AddressLine1":"123 Any
-- Street", "AddressLine2":"", "City":"Any Town", "State":"WA",
-- "CountryCode":"US", "ZipCode":"98101", "PhoneNumber":"+2065550100",
-- "Email":"john@example.com", "Fax":"+206555-0101" }, "RegistrantContact":{
-- "FirstName":"John", "MiddleName":"Richard", "LastName":"Doe",
-- "ContactType":"PERSON", "OrganizationName":"", "AddressLine1":"123 Any
-- Street", "AddressLine2":"", "City":"Any Town", "State":"WA",
-- "CountryCode":"US", "ZipCode":"98101", "PhoneNumber":"+2065550100",
-- "Email":"john@example.com", "Fax":"+206555-0101" }, "TechContact":{
-- "FirstName":"John", "MiddleName":"Richard", "LastName":"Doe",
-- "ContactType":"PERSON", "OrganizationName":"", "AddressLine1":"123 Any
-- Street", "AddressLine2":"", "City":"Any Town", "State":"WA",
-- "CountryCode":"US", "ZipCode":"98101", "PhoneNumber":"+2065550100",
-- "Email":"john@example.com", "Fax":"+206555-0101" },
-- "PrivacyProtectAdminContact":true, "PrivacyProtectRegistrantContact":true,
-- "PrivacyProtectTechContact":true, } HTTP/1.1 200 Content-Length:[number of
-- characters in the JSON string] {
-- "OperationId":"308c56712-faa4-40fe-94c8-b423069de3f6" }.
module Network.AWS.Route53Domains.TransferDomain
    (
    -- * Request
      TransferDomain
    -- ** Request constructor
    , transferDomain
    -- ** Request lenses
    , tdDomainName
    , tdIdnLangCode
    , tdDurationInYears
    , tdNameservers
    , tdAuthCode
    , tdAutoRenew
    , tdAdminContact
    , tdRegistrantContact
    , tdTechContact
    , tdPrivacyProtectAdminContact
    , tdPrivacyProtectRegistrantContact
    , tdPrivacyProtectTechContact

    -- * Response
    , TransferDomainResponse
    -- ** Response constructor
    , transferDomainResponse
    -- ** Response lenses
    , tdrOperationId
    ) where

import Network.AWS.Route53Domains.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | The TransferDomain request includes the following elements.
data TransferDomain = TransferDomain
    { _tdDomainName :: Text
    , _tdIdnLangCode :: Maybe Text
    , _tdDurationInYears :: !Integer
    , _tdNameservers :: [Nameserver]
    , _tdAuthCode :: Maybe Text
    , _tdAutoRenew :: Maybe Bool
    , _tdAdminContact :: ContactDetail
    , _tdRegistrantContact :: ContactDetail
    , _tdTechContact :: ContactDetail
    , _tdPrivacyProtectAdminContact :: Maybe Bool
    , _tdPrivacyProtectRegistrantContact :: Maybe Bool
    , _tdPrivacyProtectTechContact :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'TransferDomain' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DomainName ::@ @Text@
--
-- * @IdnLangCode ::@ @Maybe Text@
--
-- * @DurationInYears ::@ @Integer@
--
-- * @Nameservers ::@ @[Nameserver]@
--
-- * @AuthCode ::@ @Maybe Text@
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
transferDomain :: Text -- ^ 'tdDomainName'
               -> Integer -- ^ 'tdDurationInYears'
               -> [Nameserver] -- ^ 'tdNameservers'
               -> ContactDetail -- ^ 'tdAdminContact'
               -> ContactDetail -- ^ 'tdRegistrantContact'
               -> ContactDetail -- ^ 'tdTechContact'
               -> TransferDomain
transferDomain p1 p3 p4 p7 p8 p9 = TransferDomain
    { _tdDomainName = p1
    , _tdIdnLangCode = Nothing
    , _tdDurationInYears = p3
    , _tdNameservers = p4
    , _tdAuthCode = Nothing
    , _tdAutoRenew = Nothing
    , _tdAdminContact = p7
    , _tdRegistrantContact = p8
    , _tdTechContact = p9
    , _tdPrivacyProtectAdminContact = Nothing
    , _tdPrivacyProtectRegistrantContact = Nothing
    , _tdPrivacyProtectTechContact = Nothing
    }

-- | The name of a domain. Type: String Default: None Constraints: The domain
-- name can contain only the letters a through z, the numbers 0 through 9, and
-- hyphen (-). Internationalized Domain Names are not supported. Required:
-- Yes.
tdDomainName :: Lens' TransferDomain Text
tdDomainName = lens _tdDomainName (\s a -> s { _tdDomainName = a })

-- | Reserved for future use.
tdIdnLangCode :: Lens' TransferDomain (Maybe Text)
tdIdnLangCode = lens _tdIdnLangCode (\s a -> s { _tdIdnLangCode = a })

-- | The number of years the domain will be registered. Domains are registered
-- for a minimum of one year. The maximum period depends on the top-level
-- domain. Type: Integer Default: 1 Valid values: Integer from 1 to 10
-- Required: Yes.
tdDurationInYears :: Lens' TransferDomain Integer
tdDurationInYears =
    lens _tdDurationInYears (\s a -> s { _tdDurationInYears = a })

-- | Contains details for the host and glue IP addresses. Type: Complex
-- Children: GlueIps, Name.
tdNameservers :: Lens' TransferDomain [Nameserver]
tdNameservers = lens _tdNameservers (\s a -> s { _tdNameservers = a })

-- | The authorization code for the domain. You get this value from the current
-- registrar. Type: String Required: Yes.
tdAuthCode :: Lens' TransferDomain (Maybe Text)
tdAuthCode = lens _tdAuthCode (\s a -> s { _tdAuthCode = a })

-- | Indicates whether the domain will be automatically renewed (true) or not
-- (false). Autorenewal only takes effect after the account is charged. Type:
-- Boolean Valid values: true | false Default: true Required: No.
tdAutoRenew :: Lens' TransferDomain (Maybe Bool)
tdAutoRenew = lens _tdAutoRenew (\s a -> s { _tdAutoRenew = a })

-- | Provides detailed contact information. Type: Complex Children: FirstName,
-- MiddleName, LastName, ContactType, OrganizationName, AddressLine1,
-- AddressLine2, City, State, CountryCode, ZipCode, PhoneNumber, Email, Fax,
-- ExtraParams Required: Yes.
tdAdminContact :: Lens' TransferDomain ContactDetail
tdAdminContact = lens _tdAdminContact (\s a -> s { _tdAdminContact = a })

-- | Provides detailed contact information. Type: Complex Children: FirstName,
-- MiddleName, LastName, ContactType, OrganizationName, AddressLine1,
-- AddressLine2, City, State, CountryCode, ZipCode, PhoneNumber, Email, Fax,
-- ExtraParams Required: Yes.
tdRegistrantContact :: Lens' TransferDomain ContactDetail
tdRegistrantContact =
    lens _tdRegistrantContact (\s a -> s { _tdRegistrantContact = a })

-- | Provides detailed contact information. Type: Complex Children: FirstName,
-- MiddleName, LastName, ContactType, OrganizationName, AddressLine1,
-- AddressLine2, City, State, CountryCode, ZipCode, PhoneNumber, Email, Fax,
-- ExtraParams Required: Yes.
tdTechContact :: Lens' TransferDomain ContactDetail
tdTechContact = lens _tdTechContact (\s a -> s { _tdTechContact = a })

-- | Whether you want to conceal contact information from WHOIS queries. If you
-- specify true, WHOIS ("who is") queries will return contact information for
-- our registrar partner, Gandi, instead of the contact information that you
-- enter. Type: Boolean Default: true Valid values: true | false Required: No.
tdPrivacyProtectAdminContact :: Lens' TransferDomain (Maybe Bool)
tdPrivacyProtectAdminContact =
    lens _tdPrivacyProtectAdminContact
         (\s a -> s { _tdPrivacyProtectAdminContact = a })

-- | Whether you want to conceal contact information from WHOIS queries. If you
-- specify true, WHOIS ("who is") queries will return contact information for
-- our registrar partner, Gandi, instead of the contact information that you
-- enter. Type: Boolean Default: true Valid values: true | false Required: No.
tdPrivacyProtectRegistrantContact :: Lens' TransferDomain (Maybe Bool)
tdPrivacyProtectRegistrantContact =
    lens _tdPrivacyProtectRegistrantContact
         (\s a -> s { _tdPrivacyProtectRegistrantContact = a })

-- | Whether you want to conceal contact information from WHOIS queries. If you
-- specify true, WHOIS ("who is") queries will return contact information for
-- our registrar partner, Gandi, instead of the contact information that you
-- enter. Type: Boolean Default: true Valid values: true | false Required: No.
tdPrivacyProtectTechContact :: Lens' TransferDomain (Maybe Bool)
tdPrivacyProtectTechContact =
    lens _tdPrivacyProtectTechContact
         (\s a -> s { _tdPrivacyProtectTechContact = a })

instance ToPath TransferDomain

instance ToQuery TransferDomain

instance ToHeaders TransferDomain

instance ToJSON TransferDomain

-- | The TranserDomain response includes the following element.
newtype TransferDomainResponse = TransferDomainResponse
    { _tdrOperationId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'TransferDomainResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @OperationId ::@ @Text@
--
transferDomainResponse :: Text -- ^ 'tdrOperationId'
                       -> TransferDomainResponse
transferDomainResponse p1 = TransferDomainResponse
    { _tdrOperationId = p1
    }

-- | Identifier for tracking the progress of the request. To use this ID to
-- query the operation status, use GetOperationDetail. Type: String Default:
-- None Constraints: Maximum 255 characters.
tdrOperationId :: Lens' TransferDomainResponse Text
tdrOperationId = lens _tdrOperationId (\s a -> s { _tdrOperationId = a })

instance FromJSON TransferDomainResponse

instance AWSRequest TransferDomain where
    type Sv TransferDomain = Route53Domains
    type Rs TransferDomain = TransferDomainResponse

    request = get
    response _ = jsonResponse
