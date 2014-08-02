{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.ElastiCache.V2014_03_24.CreateCacheParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CreateCacheParameterGroup operation creates a new cache parameter
-- group. A cache parameter group is a collection of parameters that you apply
-- to all of the nodes in a cache cluster.
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=CreateCacheParameterGroup
-- &Description=My%20first%20cache%20parameter%20group
-- &CacheParameterGroupFamily=memcached1.4
-- &CacheParameterGroupName=mycacheparametergroup1 &Version=2014-03-24
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2014-03-27T02%3A34%3A47.462Z &AWSAccessKeyId=YOUR-ACCESS-KEY
-- &Signature=YOUR-SIGNATURE mycacheparametergroup3 memcached1.4 My first
-- cache parameter group 05699541-b7f9-11e0-9326-b7275b9d4a6c.
module Network.AWS.ElastiCache.V2014_03_24.CreateCacheParameterGroup where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_03_24.Types
import Network.AWS.Prelude

data CreateCacheParameterGroup = CreateCacheParameterGroup
    { _ccpgmCacheParameterGroupFamily :: Text
      -- ^ The name of the cache parameter group family the cache parameter
      -- group can be used with. Valid values are: memcached1.4 | redis2.6
      -- | redis2.8.
    , _ccpgmCacheParameterGroupName :: Text
      -- ^ A user-specified name for the cache parameter group.
    , _ccpgmDescription :: Text
      -- ^ A user-specified description for the cache parameter group.
    } deriving (Generic)

makeLenses ''CreateCacheParameterGroup

instance ToQuery CreateCacheParameterGroup where
    toQuery = genericToQuery def

data CreateCacheParameterGroupResponse = CreateCacheParameterGroupResponse
    { _cpgxCacheParameterGroup :: Maybe CacheParameterGroup
      -- ^ Represents the output of a CreateCacheParameterGroup operation.
    } deriving (Generic)

makeLenses ''CreateCacheParameterGroupResponse

instance FromXML CreateCacheParameterGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateCacheParameterGroup where
    type Sv CreateCacheParameterGroup = ElastiCache
    type Rs CreateCacheParameterGroup = CreateCacheParameterGroupResponse

    request = post "CreateCacheParameterGroup"
    response _ = xmlResponse
