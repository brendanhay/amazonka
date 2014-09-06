{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53Domains.V2014_05_15.CheckDomainAvailability
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation checks the availability of one domain name. You can access
-- this API without authenticating. Note that if the availability status of a
-- domain is pending, you must submit another request to determine the
-- availability of the domain name. CheckDomainAvailability Example POST /
-- HTTP/1.1 host:route53domains.us-east-1.amazonaws.com
-- x-amz-date:20140711T205225Z authorization:AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20140711/us-east-1/route53domains/aws4_request,
-- 
-- SignedHeaders=content-length;content-type;host;user-agent;x-amz-date;x-amz-target,
-- Signature=[calculated-signature]
-- x-amz-target:Route53Domains_v20140515.CheckDomainAvailability
-- user-agent:aws-sdk-java/1.8.3 Linux/2.6.18-164.el5PAE Java_HotSpot (TM
-- )_Server_VM/24.60-b09/1.7.0_60 content-type:application/x-amz-json- 1.1
-- content-length:[number of characters in the JSON string]
-- connections:Keep-Alive { "DomainName":"example.com" } HTTP/1.1 200
-- Content-Length:[number of characters in the JSON string] {
-- "Availability":"AVAILABLE" }.
module Network.AWS.Route53Domains.V2014_05_15.CheckDomainAvailability
    (
    -- * Request
      CheckDomainAvailability
    -- ** Request constructor
    , mkCheckDomainAvailability
    -- ** Request lenses
    , cdaDomainName
    , cdaIdnLangCode

    -- * Response
    , CheckDomainAvailabilityResponse
    -- ** Response lenses
    , cdarsAvailability
    ) where

import           Network.AWS.Route53Domains.V2014_05_15.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | The CheckDomainAvailability request contains the following elements.
data CheckDomainAvailability = CheckDomainAvailability
    { _cdaDomainName :: Text
    , _cdaIdnLangCode :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CheckDomainAvailability' request.
mkCheckDomainAvailability :: Text -- ^ 'cdaDomainName'
                          -> CheckDomainAvailability
mkCheckDomainAvailability p1 = CheckDomainAvailability
    { _cdaDomainName = p1
    , _cdaIdnLangCode = Nothing
    }
{-# INLINE mkCheckDomainAvailability #-}

-- | The name of a domain. Type: String Default: None Constraints: The domain
-- name can contain only the letters a through z, the numbers 0 through 9, and
-- hyphen (-). Internationalized Domain Names are not supported. Required:
-- Yes.
cdaDomainName :: Lens' CheckDomainAvailability Text
cdaDomainName = lens _cdaDomainName (\s a -> s { _cdaDomainName = a })
{-# INLINE cdaDomainName #-}

-- | Reserved for future use.
cdaIdnLangCode :: Lens' CheckDomainAvailability (Maybe Text)
cdaIdnLangCode = lens _cdaIdnLangCode (\s a -> s { _cdaIdnLangCode = a })
{-# INLINE cdaIdnLangCode #-}

instance ToPath CheckDomainAvailability

instance ToQuery CheckDomainAvailability

instance ToHeaders CheckDomainAvailability

instance ToJSON CheckDomainAvailability

-- | The CheckDomainAvailability response includes the following elements.
newtype CheckDomainAvailabilityResponse = CheckDomainAvailabilityResponse
    { _cdarsAvailability :: DomainAvailability
    } deriving (Show, Generic)

-- | Whether the domain name is available for registering. You can only register
-- domains designated as AVAILABLE. Type: String Valid values: AVAILABLE – The
-- domain name is available. AVAILABLE_RESERVED – The domain name is reserved
-- under specific conditions. AVAILABLE_PREORDER – The domain name is
-- available and can be preordered. UNAVAILABLE – The domain name is not
-- available. UNAVAILABLE_PREMIUM – The domain name is not available.
-- UNAVAILABLE_RESTRICTED – The domain name is forbidden. RESERVED – The
-- domain name has been reserved for another person or organization.
cdarsAvailability :: Lens' CheckDomainAvailabilityResponse DomainAvailability
cdarsAvailability =
    lens _cdarsAvailability (\s a -> s { _cdarsAvailability = a })
{-# INLINE cdarsAvailability #-}

instance FromJSON CheckDomainAvailabilityResponse

instance AWSRequest CheckDomainAvailability where
    type Sv CheckDomainAvailability = Route53Domains
    type Rs CheckDomainAvailability = CheckDomainAvailabilityResponse

    request = get
    response _ = jsonResponse
