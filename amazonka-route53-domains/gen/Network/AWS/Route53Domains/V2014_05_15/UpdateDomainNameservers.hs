{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53Domains.V2014_05_15.UpdateDomainNameservers
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation replaces the current set of name servers for the domain with
-- the specified set of name servers. If you use Amazon Route 53 as your DNS
-- service, specify the four name servers in the delegation set for the hosted
-- zone for the domain. If successful, this operation returns an operation ID
-- that you can use to track the progress and completion of the action. If the
-- request is not completed successfully, the domain registrant will be
-- notified by email. UpdateDomainNameservers Example POST / HTTP/1.1
-- host:route53domains.us-east-1.amazonaws.com x-amz-date:20140711T205230Z
-- authorization:AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20140711/us-east-1/route53domains/aws4_request,
-- 
-- SignedHeaders=content-length;content-type;host;user-agent;x-amz-date;x-amz-target,
-- Signature=[calculated-signature]
-- x-amz-target:Route53Domains_v20140515.UpdateDomainNameservers
-- user-agent:aws-sdk-java/1.8.3 Linux/2.6.18-164.el5PAE Java_HotSpot (TM
-- )_Server_VM/24.60-b09/1.7.0_60 content-type:application/x-amz-json-1.1
-- content-length:[number of characters in the JSON string] {
-- "DomainName":"example.com", "Nameservers":[ { "Name":"ns1.example.net" }, {
-- "Name":"ns1.example.com", "GlueIps":[ "192.0.2.44" ] } ] } HTTP/1.1 200
-- Content-Length:[number of characters in the JSON string] {
-- "OperationId":"0b370c79-faa4-40fe-94c8-b423069de3f6" }.
module Network.AWS.Route53Domains.V2014_05_15.UpdateDomainNameservers
    (
    -- * Request
      UpdateDomainNameservers
    -- ** Request constructor
    , updateDomainNameservers
    -- ** Request lenses
    , udnrDomainName
    , udnrNameservers

    -- * Response
    , UpdateDomainNameserversResponse
    -- ** Response lenses
    , udnsOperationId
    ) where

import           Network.AWS.Route53Domains.V2014_05_15.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'UpdateDomainNameservers' request.
updateDomainNameservers :: Text -- ^ 'udnrDomainName'
                        -> [Nameserver] -- ^ 'udnrNameservers'
                        -> UpdateDomainNameservers
updateDomainNameservers p1 p2 = UpdateDomainNameservers
    { _udnrDomainName = p1
    , _udnrNameservers = p2
    }

data UpdateDomainNameservers = UpdateDomainNameservers
    { _udnrDomainName :: Text
      -- ^ The name of a domain. Type: String Default: None Constraints: The
      -- domain name can contain only the letters a through z, the numbers
      -- 0 through 9, and hyphen (-). Internationalized Domain Names are
      -- not supported. Required: Yes.
    , _udnrNameservers :: [Nameserver]
      -- ^ A list of new name servers for the domain. Type: Complex
      -- Children: Name, GlueIps Required: Yes.
    } deriving (Show, Generic)

-- | The name of a domain. Type: String Default: None Constraints: The domain
-- name can contain only the letters a through z, the numbers 0 through 9, and
-- hyphen (-). Internationalized Domain Names are not supported. Required:
-- Yes.
udnrDomainName
    :: Functor f
    => (Text
    -> f (Text))
    -> UpdateDomainNameservers
    -> f UpdateDomainNameservers
udnrDomainName f x =
    (\y -> x { _udnrDomainName = y })
       <$> f (_udnrDomainName x)
{-# INLINE udnrDomainName #-}

-- | A list of new name servers for the domain. Type: Complex Children: Name,
-- GlueIps Required: Yes.
udnrNameservers
    :: Functor f
    => ([Nameserver]
    -> f ([Nameserver]))
    -> UpdateDomainNameservers
    -> f UpdateDomainNameservers
udnrNameservers f x =
    (\y -> x { _udnrNameservers = y })
       <$> f (_udnrNameservers x)
{-# INLINE udnrNameservers #-}

instance ToPath UpdateDomainNameservers

instance ToQuery UpdateDomainNameservers

instance ToHeaders UpdateDomainNameservers

instance ToJSON UpdateDomainNameservers

data UpdateDomainNameserversResponse = UpdateDomainNameserversResponse
    { _udnsOperationId :: Text
      -- ^ Identifier for tracking the progress of the request. To use this
      -- ID to query the operation status, use GetOperationDetail. Type:
      -- String Default: None Constraints: Maximum 255 characters.
    } deriving (Show, Generic)

-- | Identifier for tracking the progress of the request. To use this ID to
-- query the operation status, use GetOperationDetail. Type: String Default:
-- None Constraints: Maximum 255 characters.
udnsOperationId
    :: Functor f
    => (Text
    -> f (Text))
    -> UpdateDomainNameserversResponse
    -> f UpdateDomainNameserversResponse
udnsOperationId f x =
    (\y -> x { _udnsOperationId = y })
       <$> f (_udnsOperationId x)
{-# INLINE udnsOperationId #-}

instance FromJSON UpdateDomainNameserversResponse

instance AWSRequest UpdateDomainNameservers where
    type Sv UpdateDomainNameservers = Route53Domains
    type Rs UpdateDomainNameservers = UpdateDomainNameserversResponse

    request = get
    response _ = jsonResponse
