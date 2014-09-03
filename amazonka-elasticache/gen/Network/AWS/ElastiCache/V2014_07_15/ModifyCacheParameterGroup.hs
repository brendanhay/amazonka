{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_07_15.ModifyCacheParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ModifyCacheParameterGroup operation modifies the parameters of a cache
-- parameter group. You can modify up to 20 parameters in a single request by
-- submitting a list parameter name and value pairs.
-- https://elasticache.us-east-1.amazonaws.com/
-- ?Action=ModifyCacheParameterGroup
-- ?ParameterNameValues.member.1.ParameterName=chunk_size_growth_factor
-- &ParameterNameValues.member.1.ParameterValue=1.02
-- &CacheParameterGroupName=mycacheparametergroup &Version=2014-03-24
-- &SignatureVersion=4 &SignatureMethod=HmacSHA256 &Timestamp=20140401T192317Z
-- &X-Amz-Credential= mycacheparametergroup
-- fcedeef2-b7ff-11e0-9326-b7275b9d4a6c.
module Network.AWS.ElastiCache.V2014_07_15.ModifyCacheParameterGroup
    (
    -- * Request
      ModifyCacheParameterGroup
    -- ** Request constructor
    , modifyCacheParameterGroup
    -- ** Request lenses
    , mcpgmParameterNameValues
    , mcpgmCacheParameterGroupName

    -- * Response
    , ModifyCacheParameterGroupResponse
    -- ** Response lenses
    , cpgnmCacheParameterGroupName
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ModifyCacheParameterGroup' request.
modifyCacheParameterGroup :: [ParameterNameValue] -- ^ 'mcpgmParameterNameValues'
                          -> Text -- ^ 'mcpgmCacheParameterGroupName'
                          -> ModifyCacheParameterGroup
modifyCacheParameterGroup p1 p2 = ModifyCacheParameterGroup
    { _mcpgmParameterNameValues = p1
    , _mcpgmCacheParameterGroupName = p2
    }

data ModifyCacheParameterGroup = ModifyCacheParameterGroup
    { _mcpgmParameterNameValues :: [ParameterNameValue]
      -- ^ An array of parameter names and values for the parameter update.
      -- You must supply at least one parameter name and value; subsequent
      -- arguments are optional. A maximum of 20 parameters may be
      -- modified per request.
    , _mcpgmCacheParameterGroupName :: Text
      -- ^ The name of the cache parameter group to modify.
    } deriving (Show, Generic)

-- | An array of parameter names and values for the parameter update. You must
-- supply at least one parameter name and value; subsequent arguments are
-- optional. A maximum of 20 parameters may be modified per request.
mcpgmParameterNameValues
    :: Functor f
    => ([ParameterNameValue]
    -> f ([ParameterNameValue]))
    -> ModifyCacheParameterGroup
    -> f ModifyCacheParameterGroup
mcpgmParameterNameValues f x =
    (\y -> x { _mcpgmParameterNameValues = y })
       <$> f (_mcpgmParameterNameValues x)
{-# INLINE mcpgmParameterNameValues #-}

-- | The name of the cache parameter group to modify.
mcpgmCacheParameterGroupName
    :: Functor f
    => (Text
    -> f (Text))
    -> ModifyCacheParameterGroup
    -> f ModifyCacheParameterGroup
mcpgmCacheParameterGroupName f x =
    (\y -> x { _mcpgmCacheParameterGroupName = y })
       <$> f (_mcpgmCacheParameterGroupName x)
{-# INLINE mcpgmCacheParameterGroupName #-}

instance ToQuery ModifyCacheParameterGroup where
    toQuery = genericQuery def

data ModifyCacheParameterGroupResponse = ModifyCacheParameterGroupResponse
    { _cpgnmCacheParameterGroupName :: Maybe Text
      -- ^ The name of the cache parameter group.
    } deriving (Show, Generic)

-- | The name of the cache parameter group.
cpgnmCacheParameterGroupName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ModifyCacheParameterGroupResponse
    -> f ModifyCacheParameterGroupResponse
cpgnmCacheParameterGroupName f x =
    (\y -> x { _cpgnmCacheParameterGroupName = y })
       <$> f (_cpgnmCacheParameterGroupName x)
{-# INLINE cpgnmCacheParameterGroupName #-}

instance FromXML ModifyCacheParameterGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ModifyCacheParameterGroup where
    type Sv ModifyCacheParameterGroup = ElastiCache
    type Rs ModifyCacheParameterGroup = ModifyCacheParameterGroupResponse

    request = post "ModifyCacheParameterGroup"
    response _ = xmlResponse
