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
    , transferDomain
    -- ** Request lenses
    , tdrAdminContact
    , tdrRegistrantContact
    , tdrTechContact
    , tdrDomainName
    , tdrDurationInYears
    , tdrNameservers
    , tdrAutoRenew
    , tdrPrivacyProtectAdminContact
    , tdrPrivacyProtectRegistrantContact
    , tdrPrivacyProtectTechContact
    , tdrAuthCode
    , tdrIdnLangCode

    -- * Response
    , TransferDomainResponse
    -- ** Response lenses
    , tdsOperationId
    ) where

import           Network.AWS.Route53Domains.V2014_05_15.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'TransferDomain' request.
transferDomain :: ContactDetail -- ^ 'tdrAdminContact'
               -> ContactDetail -- ^ 'tdrRegistrantContact'
               -> ContactDetail -- ^ 'tdrTechContact'
               -> Text -- ^ 'tdrDomainName'
               -> Integer -- ^ 'tdrDurationInYears'
               -> [Nameserver] -- ^ 'tdrNameservers'
               -> TransferDomain
transferDomain p1 p2 p3 p4 p5 p6 = TransferDomain
    { _tdrAdminContact = p1
    , _tdrRegistrantContact = p2
    , _tdrTechContact = p3
    , _tdrDomainName = p4
    , _tdrDurationInYears = p5
    , _tdrNameservers = p6
    , _tdrAutoRenew = Nothing
    , _tdrPrivacyProtectAdminContact = Nothing
    , _tdrPrivacyProtectRegistrantContact = Nothing
    , _tdrPrivacyProtectTechContact = Nothing
    , _tdrAuthCode = Nothing
    , _tdrIdnLangCode = Nothing
    }

data TransferDomain = TransferDomain
    { _tdrAdminContact :: ContactDetail
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
    , _tdrDomainName :: Text
      -- ^ The name of a domain. Type: String Default: None Constraints: The
      -- domain name can contain only the letters a through z, the numbers
      -- 0 through 9, and hyphen (-). Internationalized Domain Names are
      -- not supported. Required: Yes.
    , _tdrDurationInYears :: Integer
      -- ^ The number of years the domain will be registered. Domains are
      -- registered for a minimum of one year. The maximum period depends
      -- on the top-level domain. Type: Integer Default: 1 Valid values:
      -- Integer from 1 to 10 Required: Yes.
    , _tdrNameservers :: [Nameserver]
      -- ^ Contains details for the host and glue IP addresses. Type:
      -- Complex Children: GlueIps, Name.
    , _tdrAutoRenew :: Maybe Bool
      -- ^ Indicates whether the domain will be automatically renewed (true)
      -- or not (false). Autorenewal only takes effect after the account
      -- is charged. Type: Boolean Valid values: true | false Default:
      -- true Required: No.
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
    , _tdrAuthCode :: Maybe Text
      -- ^ The authorization code for the domain. You get this value from
      -- the current registrar. Type: String Required: Yes.
    , _tdrIdnLangCode :: Maybe Text
      -- ^ Reserved for future use.
    } deriving (Show, Generic)

-- | Provides detailed contact information. Type: Complex Children: FirstName,
-- MiddleName, LastName, ContactType, OrganizationName, AddressLine1,
-- AddressLine2, City, State, CountryCode, ZipCode, PhoneNumber, Email, Fax,
-- ExtraParams Required: Yes.
tdrAdminContact
    :: Functor f
    => (ContactDetail
    -> f (ContactDetail))
    -> TransferDomain
    -> f TransferDomain
tdrAdminContact f x =
    (\y -> x { _tdrAdminContact = y })
       <$> f (_tdrAdminContact x)
{-# INLINE tdrAdminContact #-}

-- | Provides detailed contact information. Type: Complex Children: FirstName,
-- MiddleName, LastName, ContactType, OrganizationName, AddressLine1,
-- AddressLine2, City, State, CountryCode, ZipCode, PhoneNumber, Email, Fax,
-- ExtraParams Required: Yes.
tdrRegistrantContact
    :: Functor f
    => (ContactDetail
    -> f (ContactDetail))
    -> TransferDomain
    -> f TransferDomain
tdrRegistrantContact f x =
    (\y -> x { _tdrRegistrantContact = y })
       <$> f (_tdrRegistrantContact x)
{-# INLINE tdrRegistrantContact #-}

-- | Provides detailed contact information. Type: Complex Children: FirstName,
-- MiddleName, LastName, ContactType, OrganizationName, AddressLine1,
-- AddressLine2, City, State, CountryCode, ZipCode, PhoneNumber, Email, Fax,
-- ExtraParams Required: Yes.
tdrTechContact
    :: Functor f
    => (ContactDetail
    -> f (ContactDetail))
    -> TransferDomain
    -> f TransferDomain
tdrTechContact f x =
    (\y -> x { _tdrTechContact = y })
       <$> f (_tdrTechContact x)
{-# INLINE tdrTechContact #-}

-- | The name of a domain. Type: String Default: None Constraints: The domain
-- name can contain only the letters a through z, the numbers 0 through 9, and
-- hyphen (-). Internationalized Domain Names are not supported. Required:
-- Yes.
tdrDomainName
    :: Functor f
    => (Text
    -> f (Text))
    -> TransferDomain
    -> f TransferDomain
tdrDomainName f x =
    (\y -> x { _tdrDomainName = y })
       <$> f (_tdrDomainName x)
{-# INLINE tdrDomainName #-}

-- | The number of years the domain will be registered. Domains are registered
-- for a minimum of one year. The maximum period depends on the top-level
-- domain. Type: Integer Default: 1 Valid values: Integer from 1 to 10
-- Required: Yes.
tdrDurationInYears
    :: Functor f
    => (Integer
    -> f (Integer))
    -> TransferDomain
    -> f TransferDomain
tdrDurationInYears f x =
    (\y -> x { _tdrDurationInYears = y })
       <$> f (_tdrDurationInYears x)
{-# INLINE tdrDurationInYears #-}

-- | Contains details for the host and glue IP addresses. Type: Complex
-- Children: GlueIps, Name.
tdrNameservers
    :: Functor f
    => ([Nameserver]
    -> f ([Nameserver]))
    -> TransferDomain
    -> f TransferDomain
tdrNameservers f x =
    (\y -> x { _tdrNameservers = y })
       <$> f (_tdrNameservers x)
{-# INLINE tdrNameservers #-}

-- | Indicates whether the domain will be automatically renewed (true) or not
-- (false). Autorenewal only takes effect after the account is charged. Type:
-- Boolean Valid values: true | false Default: true Required: No.
tdrAutoRenew
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> TransferDomain
    -> f TransferDomain
tdrAutoRenew f x =
    (\y -> x { _tdrAutoRenew = y })
       <$> f (_tdrAutoRenew x)
{-# INLINE tdrAutoRenew #-}

-- | Whether you want to conceal contact information from WHOIS queries. If you
-- specify true, WHOIS ("who is") queries will return contact information for
-- our registrar partner, Gandi, instead of the contact information that you
-- enter. Type: Boolean Default: true Valid values: true | false Required: No.
tdrPrivacyProtectAdminContact
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> TransferDomain
    -> f TransferDomain
tdrPrivacyProtectAdminContact f x =
    (\y -> x { _tdrPrivacyProtectAdminContact = y })
       <$> f (_tdrPrivacyProtectAdminContact x)
{-# INLINE tdrPrivacyProtectAdminContact #-}

-- | Whether you want to conceal contact information from WHOIS queries. If you
-- specify true, WHOIS ("who is") queries will return contact information for
-- our registrar partner, Gandi, instead of the contact information that you
-- enter. Type: Boolean Default: true Valid values: true | false Required: No.
tdrPrivacyProtectRegistrantContact
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> TransferDomain
    -> f TransferDomain
tdrPrivacyProtectRegistrantContact f x =
    (\y -> x { _tdrPrivacyProtectRegistrantContact = y })
       <$> f (_tdrPrivacyProtectRegistrantContact x)
{-# INLINE tdrPrivacyProtectRegistrantContact #-}

-- | Whether you want to conceal contact information from WHOIS queries. If you
-- specify true, WHOIS ("who is") queries will return contact information for
-- our registrar partner, Gandi, instead of the contact information that you
-- enter. Type: Boolean Default: true Valid values: true | false Required: No.
tdrPrivacyProtectTechContact
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> TransferDomain
    -> f TransferDomain
tdrPrivacyProtectTechContact f x =
    (\y -> x { _tdrPrivacyProtectTechContact = y })
       <$> f (_tdrPrivacyProtectTechContact x)
{-# INLINE tdrPrivacyProtectTechContact #-}

-- | The authorization code for the domain. You get this value from the current
-- registrar. Type: String Required: Yes.
tdrAuthCode
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> TransferDomain
    -> f TransferDomain
tdrAuthCode f x =
    (\y -> x { _tdrAuthCode = y })
       <$> f (_tdrAuthCode x)
{-# INLINE tdrAuthCode #-}

-- | Reserved for future use.
tdrIdnLangCode
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> TransferDomain
    -> f TransferDomain
tdrIdnLangCode f x =
    (\y -> x { _tdrIdnLangCode = y })
       <$> f (_tdrIdnLangCode x)
{-# INLINE tdrIdnLangCode #-}

instance ToPath TransferDomain

instance ToQuery TransferDomain

instance ToHeaders TransferDomain

instance ToJSON TransferDomain

data TransferDomainResponse = TransferDomainResponse
    { _tdsOperationId :: Text
      -- ^ Identifier for tracking the progress of the request. To use this
      -- ID to query the operation status, use GetOperationDetail. Type:
      -- String Default: None Constraints: Maximum 255 characters.
    } deriving (Show, Generic)

-- | Identifier for tracking the progress of the request. To use this ID to
-- query the operation status, use GetOperationDetail. Type: String Default:
-- None Constraints: Maximum 255 characters.
tdsOperationId
    :: Functor f
    => (Text
    -> f (Text))
    -> TransferDomainResponse
    -> f TransferDomainResponse
tdsOperationId f x =
    (\y -> x { _tdsOperationId = y })
       <$> f (_tdsOperationId x)
{-# INLINE tdsOperationId #-}

instance FromJSON TransferDomainResponse

instance AWSRequest TransferDomain where
    type Sv TransferDomain = Route53Domains
    type Rs TransferDomain = TransferDomainResponse

    request = get
    response _ = jsonResponse
