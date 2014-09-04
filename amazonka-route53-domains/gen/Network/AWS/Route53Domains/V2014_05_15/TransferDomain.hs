{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53Domains.V2014_05_15.TransferDomain
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
module Network.AWS.Route53Domains.V2014_05_15.TransferDomain
    (
    -- * Request
      TransferDomain
    -- ** Request constructor
    , mkTransferDomainRequest
    -- ** Request lenses
    , tdrDomainName
    , tdrIdnLangCode
    , tdrDurationInYears
    , tdrNameservers
    , tdrAuthCode
    , tdrAutoRenew
    , tdrAdminContact
    , tdrRegistrantContact
    , tdrTechContact
    , tdrPrivacyProtectAdminContact
    , tdrPrivacyProtectRegistrantContact
    , tdrPrivacyProtectTechContact

    -- * Response
    , TransferDomainResponse
    -- ** Response lenses
    , tdsOperationId
    ) where

import           Network.AWS.Route53Domains.V2014_05_15.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'TransferDomain' request.
mkTransferDomainRequest :: Text -- ^ 'tdrDomainName'
                        -> Integer -- ^ 'tdrDurationInYears'
                        -> [Nameserver] -- ^ 'tdrNameservers'
                        -> ContactDetail -- ^ 'tdrAdminContact'
                        -> ContactDetail -- ^ 'tdrRegistrantContact'
                        -> ContactDetail -- ^ 'tdrTechContact'
                        -> TransferDomain
mkTransferDomainRequest p1 p2 p3 p4 p5 p6 = TransferDomain
    { _tdrDomainName = p1
    , _tdrIdnLangCode = Nothing
    , _tdrDurationInYears = p3
    , _tdrNameservers = p4
    , _tdrAuthCode = Nothing
    , _tdrAutoRenew = Nothing
    , _tdrAdminContact = p7
    , _tdrRegistrantContact = p8
    , _tdrTechContact = p9
    , _tdrPrivacyProtectAdminContact = Nothing
    , _tdrPrivacyProtectRegistrantContact = Nothing
    , _tdrPrivacyProtectTechContact = Nothing
    }
{-# INLINE mkTransferDomainRequest #-}

data TransferDomain = TransferDomain
    { _tdrDomainName :: Text
      -- ^ The name of a domain. Type: String Default: None Constraints: The
      -- domain name can contain only the letters a through z, the numbers
      -- 0 through 9, and hyphen (-). Internationalized Domain Names are
      -- not supported. Required: Yes.
    , _tdrIdnLangCode :: Maybe Text
      -- ^ Reserved for future use.
    , _tdrDurationInYears :: Integer
      -- ^ The number of years the domain will be registered. Domains are
      -- registered for a minimum of one year. The maximum period depends
      -- on the top-level domain. Type: Integer Default: 1 Valid values:
      -- Integer from 1 to 10 Required: Yes.
    , _tdrNameservers :: [Nameserver]
      -- ^ Contains details for the host and glue IP addresses. Type:
      -- Complex Children: GlueIps, Name.
    , _tdrAuthCode :: Maybe Text
      -- ^ The authorization code for the domain. You get this value from
      -- the current registrar. Type: String Required: Yes.
    , _tdrAutoRenew :: Maybe Bool
      -- ^ Indicates whether the domain will be automatically renewed (true)
      -- or not (false). Autorenewal only takes effect after the account
      -- is charged. Type: Boolean Valid values: true | false Default:
      -- true Required: No.
    , _tdrAdminContact :: ContactDetail
      -- ^ Provides detailed contact information. Type: Complex Children:
      -- FirstName, MiddleName, LastName, ContactType, OrganizationName,
      -- AddressLine1, AddressLine2, City, State, CountryCode, ZipCode,
      -- PhoneNumber, Email, Fax, ExtraParams Required: Yes.
    , _tdrRegistrantContact :: ContactDetail
      -- ^ Provides detailed contact information. Type: Complex Children:
      -- FirstName, MiddleName, LastName, ContactType, OrganizationName,
      -- AddressLine1, AddressLine2, City, State, CountryCode, ZipCode,
      -- PhoneNumber, Email, Fax, ExtraParams Required: Yes.
    , _tdrTechContact :: ContactDetail
      -- ^ Provides detailed contact information. Type: Complex Children:
      -- FirstName, MiddleName, LastName, ContactType, OrganizationName,
      -- AddressLine1, AddressLine2, City, State, CountryCode, ZipCode,
      -- PhoneNumber, Email, Fax, ExtraParams Required: Yes.
    , _tdrPrivacyProtectAdminContact :: Maybe Bool
      -- ^ Whether you want to conceal contact information from WHOIS
      -- queries. If you specify true, WHOIS ("who is") queries will
      -- return contact information for our registrar partner, Gandi,
      -- instead of the contact information that you enter. Type: Boolean
      -- Default: true Valid values: true | false Required: No.
    , _tdrPrivacyProtectRegistrantContact :: Maybe Bool
      -- ^ Whether you want to conceal contact information from WHOIS
      -- queries. If you specify true, WHOIS ("who is") queries will
      -- return contact information for our registrar partner, Gandi,
      -- instead of the contact information that you enter. Type: Boolean
      -- Default: true Valid values: true | false Required: No.
    , _tdrPrivacyProtectTechContact :: Maybe Bool
      -- ^ Whether you want to conceal contact information from WHOIS
      -- queries. If you specify true, WHOIS ("who is") queries will
      -- return contact information for our registrar partner, Gandi,
      -- instead of the contact information that you enter. Type: Boolean
      -- Default: true Valid values: true | false Required: No.
    } deriving (Show, Generic)

-- | The name of a domain. Type: String Default: None Constraints: The domain
-- name can contain only the letters a through z, the numbers 0 through 9, and
-- hyphen (-). Internationalized Domain Names are not supported. Required:
-- Yes.
tdrDomainName :: Lens' TransferDomain (Text)
tdrDomainName = lens _tdrDomainName (\s a -> s { _tdrDomainName = a })
{-# INLINE tdrDomainName #-}

-- | Reserved for future use.
tdrIdnLangCode :: Lens' TransferDomain (Maybe Text)
tdrIdnLangCode = lens _tdrIdnLangCode (\s a -> s { _tdrIdnLangCode = a })
{-# INLINE tdrIdnLangCode #-}

-- | The number of years the domain will be registered. Domains are registered
-- for a minimum of one year. The maximum period depends on the top-level
-- domain. Type: Integer Default: 1 Valid values: Integer from 1 to 10
-- Required: Yes.
tdrDurationInYears :: Lens' TransferDomain (Integer)
tdrDurationInYears = lens _tdrDurationInYears (\s a -> s { _tdrDurationInYears = a })
{-# INLINE tdrDurationInYears #-}

-- | Contains details for the host and glue IP addresses. Type: Complex
-- Children: GlueIps, Name.
tdrNameservers :: Lens' TransferDomain ([Nameserver])
tdrNameservers = lens _tdrNameservers (\s a -> s { _tdrNameservers = a })
{-# INLINE tdrNameservers #-}

-- | The authorization code for the domain. You get this value from the current
-- registrar. Type: String Required: Yes.
tdrAuthCode :: Lens' TransferDomain (Maybe Text)
tdrAuthCode = lens _tdrAuthCode (\s a -> s { _tdrAuthCode = a })
{-# INLINE tdrAuthCode #-}

-- | Indicates whether the domain will be automatically renewed (true) or not
-- (false). Autorenewal only takes effect after the account is charged. Type:
-- Boolean Valid values: true | false Default: true Required: No.
tdrAutoRenew :: Lens' TransferDomain (Maybe Bool)
tdrAutoRenew = lens _tdrAutoRenew (\s a -> s { _tdrAutoRenew = a })
{-# INLINE tdrAutoRenew #-}

-- | Provides detailed contact information. Type: Complex Children: FirstName,
-- MiddleName, LastName, ContactType, OrganizationName, AddressLine1,
-- AddressLine2, City, State, CountryCode, ZipCode, PhoneNumber, Email, Fax,
-- ExtraParams Required: Yes.
tdrAdminContact :: Lens' TransferDomain (ContactDetail)
tdrAdminContact = lens _tdrAdminContact (\s a -> s { _tdrAdminContact = a })
{-# INLINE tdrAdminContact #-}

-- | Provides detailed contact information. Type: Complex Children: FirstName,
-- MiddleName, LastName, ContactType, OrganizationName, AddressLine1,
-- AddressLine2, City, State, CountryCode, ZipCode, PhoneNumber, Email, Fax,
-- ExtraParams Required: Yes.
tdrRegistrantContact :: Lens' TransferDomain (ContactDetail)
tdrRegistrantContact = lens _tdrRegistrantContact (\s a -> s { _tdrRegistrantContact = a })
{-# INLINE tdrRegistrantContact #-}

-- | Provides detailed contact information. Type: Complex Children: FirstName,
-- MiddleName, LastName, ContactType, OrganizationName, AddressLine1,
-- AddressLine2, City, State, CountryCode, ZipCode, PhoneNumber, Email, Fax,
-- ExtraParams Required: Yes.
tdrTechContact :: Lens' TransferDomain (ContactDetail)
tdrTechContact = lens _tdrTechContact (\s a -> s { _tdrTechContact = a })
{-# INLINE tdrTechContact #-}

-- | Whether you want to conceal contact information from WHOIS queries. If you
-- specify true, WHOIS ("who is") queries will return contact information for
-- our registrar partner, Gandi, instead of the contact information that you
-- enter. Type: Boolean Default: true Valid values: true | false Required: No.
tdrPrivacyProtectAdminContact :: Lens' TransferDomain (Maybe Bool)
tdrPrivacyProtectAdminContact = lens _tdrPrivacyProtectAdminContact (\s a -> s { _tdrPrivacyProtectAdminContact = a })
{-# INLINE tdrPrivacyProtectAdminContact #-}

-- | Whether you want to conceal contact information from WHOIS queries. If you
-- specify true, WHOIS ("who is") queries will return contact information for
-- our registrar partner, Gandi, instead of the contact information that you
-- enter. Type: Boolean Default: true Valid values: true | false Required: No.
tdrPrivacyProtectRegistrantContact :: Lens' TransferDomain (Maybe Bool)
tdrPrivacyProtectRegistrantContact = lens _tdrPrivacyProtectRegistrantContact (\s a -> s { _tdrPrivacyProtectRegistrantContact = a })
{-# INLINE tdrPrivacyProtectRegistrantContact #-}

-- | Whether you want to conceal contact information from WHOIS queries. If you
-- specify true, WHOIS ("who is") queries will return contact information for
-- our registrar partner, Gandi, instead of the contact information that you
-- enter. Type: Boolean Default: true Valid values: true | false Required: No.
tdrPrivacyProtectTechContact :: Lens' TransferDomain (Maybe Bool)
tdrPrivacyProtectTechContact = lens _tdrPrivacyProtectTechContact (\s a -> s { _tdrPrivacyProtectTechContact = a })
{-# INLINE tdrPrivacyProtectTechContact #-}

instance ToPath TransferDomain

instance ToQuery TransferDomain

instance ToHeaders TransferDomain

instance ToJSON TransferDomain

newtype TransferDomainResponse = TransferDomainResponse
    { _tdsOperationId :: Text
      -- ^ Identifier for tracking the progress of the request. To use this
      -- ID to query the operation status, use GetOperationDetail. Type:
      -- String Default: None Constraints: Maximum 255 characters.
    } deriving (Show, Generic)

-- | Identifier for tracking the progress of the request. To use this ID to
-- query the operation status, use GetOperationDetail. Type: String Default:
-- None Constraints: Maximum 255 characters.
tdsOperationId :: Lens' TransferDomainResponse (Text)
tdsOperationId = lens _tdsOperationId (\s a -> s { _tdsOperationId = a })
{-# INLINE tdsOperationId #-}

instance FromJSON TransferDomainResponse

instance AWSRequest TransferDomain where
    type Sv TransferDomain = Route53Domains
    type Rs TransferDomain = TransferDomainResponse

    request = get
    response _ = jsonResponse
