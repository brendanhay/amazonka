{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_07_15.CreateCacheParameterGroup
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
-- &SignatureVersion=4 &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z
-- &X-Amz-Credential= mycacheparametergroup3 memcached1.4 My first cache
-- parameter group 05699541-b7f9-11e0-9326-b7275b9d4a6c.
module Network.AWS.ElastiCache.V2014_07_15.CreateCacheParameterGroup
    (
    -- * Request
      CreateCacheParameterGroup
    -- ** Request constructor
    , createCacheParameterGroup
    -- ** Request lenses
    , ccpgmCacheParameterGroupName
    , ccpgmCacheParameterGroupFamily
    , ccpgmDescription

    -- * Response
    , CreateCacheParameterGroupResponse
    -- ** Response lenses
    , cpgwCacheParameterGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateCacheParameterGroup' request.
createCacheParameterGroup :: Text -- ^ 'ccpgmCacheParameterGroupName'
                          -> Text -- ^ 'ccpgmCacheParameterGroupFamily'
                          -> Text -- ^ 'ccpgmDescription'
                          -> CreateCacheParameterGroup
createCacheParameterGroup p1 p2 p3 = CreateCacheParameterGroup
    { _ccpgmCacheParameterGroupName = p1
    , _ccpgmCacheParameterGroupFamily = p2
    , _ccpgmDescription = p3
    }
{-# INLINE createCacheParameterGroup #-}

data CreateCacheParameterGroup = CreateCacheParameterGroup
    { _ccpgmCacheParameterGroupName :: Text
      -- ^ A user-specified name for the cache parameter group.
    , _ccpgmCacheParameterGroupFamily :: Text
      -- ^ The name of the cache parameter group family the cache parameter
      -- group can be used with. Valid values are: memcached1.4 | redis2.6
      -- | redis2.8.
    , _ccpgmDescription :: Text
      -- ^ A user-specified description for the cache parameter group.
    } deriving (Show, Generic)

-- | A user-specified name for the cache parameter group.
ccpgmCacheParameterGroupName :: Lens' CreateCacheParameterGroup (Text)
ccpgmCacheParameterGroupName f x =
    f (_ccpgmCacheParameterGroupName x)
        <&> \y -> x { _ccpgmCacheParameterGroupName = y }
{-# INLINE ccpgmCacheParameterGroupName #-}

-- | The name of the cache parameter group family the cache parameter group can
-- be used with. Valid values are: memcached1.4 | redis2.6 | redis2.8.
ccpgmCacheParameterGroupFamily :: Lens' CreateCacheParameterGroup (Text)
ccpgmCacheParameterGroupFamily f x =
    f (_ccpgmCacheParameterGroupFamily x)
        <&> \y -> x { _ccpgmCacheParameterGroupFamily = y }
{-# INLINE ccpgmCacheParameterGroupFamily #-}

-- | A user-specified description for the cache parameter group.
ccpgmDescription :: Lens' CreateCacheParameterGroup (Text)
ccpgmDescription f x =
    f (_ccpgmDescription x)
        <&> \y -> x { _ccpgmDescription = y }
{-# INLINE ccpgmDescription #-}

instance ToQuery CreateCacheParameterGroup where
    toQuery = genericQuery def

data CreateCacheParameterGroupResponse = CreateCacheParameterGroupResponse
    { _cpgwCacheParameterGroup :: Maybe CacheParameterGroup
      -- ^ Represents the output of a CreateCacheParameterGroup operation.
    } deriving (Show, Generic)

-- | Represents the output of a CreateCacheParameterGroup operation.
cpgwCacheParameterGroup :: Lens' CreateCacheParameterGroupResponse (Maybe CacheParameterGroup)
cpgwCacheParameterGroup f x =
    f (_cpgwCacheParameterGroup x)
        <&> \y -> x { _cpgwCacheParameterGroup = y }
{-# INLINE cpgwCacheParameterGroup #-}

instance FromXML CreateCacheParameterGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateCacheParameterGroup where
    type Sv CreateCacheParameterGroup = ElastiCache
    type Rs CreateCacheParameterGroup = CreateCacheParameterGroupResponse

    request = post "CreateCacheParameterGroup"
    response _ = xmlResponse
