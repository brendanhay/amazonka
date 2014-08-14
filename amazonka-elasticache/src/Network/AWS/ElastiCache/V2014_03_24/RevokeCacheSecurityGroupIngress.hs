{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Module      : Network.AWS.ElastiCache.V2014_03_24.RevokeCacheSecurityGroupIngress
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The RevokeCacheSecurityGroupIngress operation revokes ingress from a cache
-- security group. Use this operation to disallow access from an Amazon EC2
-- security group that had been previously authorized.
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=RevokeCacheSecurityGroupIngress &EC2SecurityGroupName=default
-- &CacheSecurityGroupName=mygroup &EC2SecurityGroupOwnerId=1234-5678-1234
-- &Version=2014-03-24 &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2014-03-27T02%3A30%3A08.444Z &AWSAccessKeyId=YOUR-ACCESS-KEY
-- &Signature=YOUR-SIGNATURE revoking default 123456781234 mygroup
-- 123456789012 My security group 02ae3699-3650-11e0-a564-8f11342c56b0.
module Network.AWS.ElastiCache.V2014_03_24.RevokeCacheSecurityGroupIngress where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_03_24.Types
import Network.AWS.Prelude

data RevokeCacheSecurityGroupIngress = RevokeCacheSecurityGroupIngress
    { _rcsgimCacheSecurityGroupName :: Text
      -- ^ The name of the cache security group to revoke ingress from.
    , _rcsgimEC2SecurityGroupOwnerId :: Text
      -- ^ The AWS account number of the Amazon EC2 security group owner.
      -- Note that this is not the same thing as an AWS access key ID -
      -- you must provide a valid AWS account number for this parameter.
    , _rcsgimEC2SecurityGroupName :: Text
      -- ^ The name of the Amazon EC2 security group to revoke access from.
    } deriving (Show, Generic)

makeLenses ''RevokeCacheSecurityGroupIngress

instance ToQuery RevokeCacheSecurityGroupIngress where
    toQuery = genericQuery def

data RevokeCacheSecurityGroupIngressResponse = RevokeCacheSecurityGroupIngressResponse
    { _csgwCacheSecurityGroup :: Maybe CacheSecurityGroup
      -- ^ Represents the output of one of the following operations:
      -- AuthorizeCacheSecurityGroupIngress CreateCacheSecurityGroup
      -- RevokeCacheSecurityGroupIngress.
    } deriving (Show, Generic)

makeLenses ''RevokeCacheSecurityGroupIngressResponse

instance FromXML RevokeCacheSecurityGroupIngressResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RevokeCacheSecurityGroupIngress where
    type Sv RevokeCacheSecurityGroupIngress = ElastiCache
    type Rs RevokeCacheSecurityGroupIngress = RevokeCacheSecurityGroupIngressResponse

    request = post "RevokeCacheSecurityGroupIngress"
    response _ = xmlResponse
