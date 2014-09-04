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
    , mkModifyCacheParameterGroupMessage
    -- ** Request lenses
    , mcpgmCacheParameterGroupName
    , mcpgmParameterNameValues

    -- * Response
    , ModifyCacheParameterGroupResponse
    -- ** Response lenses
    , cpgnmCacheParameterGroupName
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyCacheParameterGroup' request.
mkModifyCacheParameterGroupMessage :: Text -- ^ 'mcpgmCacheParameterGroupName'
                                   -> [ParameterNameValue] -- ^ 'mcpgmParameterNameValues'
                                   -> ModifyCacheParameterGroup
mkModifyCacheParameterGroupMessage p1 p2 = ModifyCacheParameterGroup
    { _mcpgmCacheParameterGroupName = p1
    , _mcpgmParameterNameValues = p2
    }
{-# INLINE mkModifyCacheParameterGroupMessage #-}

data ModifyCacheParameterGroup = ModifyCacheParameterGroup
    { _mcpgmCacheParameterGroupName :: Text
      -- ^ The name of the cache parameter group to modify.
    , _mcpgmParameterNameValues :: [ParameterNameValue]
      -- ^ An array of parameter names and values for the parameter update.
      -- You must supply at least one parameter name and value; subsequent
      -- arguments are optional. A maximum of 20 parameters may be
      -- modified per request.
    } deriving (Show, Generic)

-- | The name of the cache parameter group to modify.
mcpgmCacheParameterGroupName :: Lens' ModifyCacheParameterGroup (Text)
mcpgmCacheParameterGroupName = lens _mcpgmCacheParameterGroupName (\s a -> s { _mcpgmCacheParameterGroupName = a })
{-# INLINE mcpgmCacheParameterGroupName #-}

-- | An array of parameter names and values for the parameter update. You must
-- supply at least one parameter name and value; subsequent arguments are
-- optional. A maximum of 20 parameters may be modified per request.
mcpgmParameterNameValues :: Lens' ModifyCacheParameterGroup ([ParameterNameValue])
mcpgmParameterNameValues = lens _mcpgmParameterNameValues (\s a -> s { _mcpgmParameterNameValues = a })
{-# INLINE mcpgmParameterNameValues #-}

instance ToQuery ModifyCacheParameterGroup where
    toQuery = genericQuery def

newtype ModifyCacheParameterGroupResponse = ModifyCacheParameterGroupResponse
    { _cpgnmCacheParameterGroupName :: Maybe Text
      -- ^ The name of the cache parameter group.
    } deriving (Show, Generic)

-- | The name of the cache parameter group.
cpgnmCacheParameterGroupName :: Lens' ModifyCacheParameterGroupResponse (Maybe Text)
cpgnmCacheParameterGroupName = lens _cpgnmCacheParameterGroupName (\s a -> s { _cpgnmCacheParameterGroupName = a })
{-# INLINE cpgnmCacheParameterGroupName #-}

instance FromXML ModifyCacheParameterGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ModifyCacheParameterGroup where
    type Sv ModifyCacheParameterGroup = ElastiCache
    type Rs ModifyCacheParameterGroup = ModifyCacheParameterGroupResponse

    request = post "ModifyCacheParameterGroup"
    response _ = xmlResponse
