{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.ModifyCacheParameterGroup
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
module Network.AWS.ElastiCache.ModifyCacheParameterGroup
    (
    -- * Request
      ModifyCacheParameterGroup
    -- ** Request constructor
    , modifyCacheParameterGroup
    -- ** Request lenses
    , mcpgCacheParameterGroupName
    , mcpgParameterNameValues

    -- * Response
    , ModifyCacheParameterGroupResponse
    -- ** Response constructor
    , modifyCacheParameterGroupResponse
    -- ** Response lenses
    , mcpgrCacheParameterGroupName
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import Network.AWS.Prelude

-- | Represents the input of a ModifyCacheParameterGroup operation.
data ModifyCacheParameterGroup = ModifyCacheParameterGroup
    { _mcpgCacheParameterGroupName :: Text
    , _mcpgParameterNameValues :: [ParameterNameValue]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyCacheParameterGroup' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CacheParameterGroupName ::@ @Text@
--
-- * @ParameterNameValues ::@ @[ParameterNameValue]@
--
modifyCacheParameterGroup :: Text -- ^ 'mcpgCacheParameterGroupName'
                          -> [ParameterNameValue] -- ^ 'mcpgParameterNameValues'
                          -> ModifyCacheParameterGroup
modifyCacheParameterGroup p1 p2 = ModifyCacheParameterGroup
    { _mcpgCacheParameterGroupName = p1
    , _mcpgParameterNameValues = p2
    }

-- | The name of the cache parameter group to modify.
mcpgCacheParameterGroupName :: Lens' ModifyCacheParameterGroup Text
mcpgCacheParameterGroupName =
    lens _mcpgCacheParameterGroupName
         (\s a -> s { _mcpgCacheParameterGroupName = a })

-- | An array of parameter names and values for the parameter update. You must
-- supply at least one parameter name and value; subsequent arguments are
-- optional. A maximum of 20 parameters may be modified per request.
mcpgParameterNameValues :: Lens' ModifyCacheParameterGroup [ParameterNameValue]
mcpgParameterNameValues =
    lens _mcpgParameterNameValues
         (\s a -> s { _mcpgParameterNameValues = a })

instance ToQuery ModifyCacheParameterGroup where
    toQuery = genericQuery def

-- | Represents the output of one of the following operations:
-- ModifyCacheParameterGroup ResetCacheParameterGroup.
newtype ModifyCacheParameterGroupResponse = ModifyCacheParameterGroupResponse
    { _mcpgrCacheParameterGroupName :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyCacheParameterGroupResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CacheParameterGroupName ::@ @Maybe Text@
--
modifyCacheParameterGroupResponse :: ModifyCacheParameterGroupResponse
modifyCacheParameterGroupResponse = ModifyCacheParameterGroupResponse
    { _mcpgrCacheParameterGroupName = Nothing
    }

-- | The name of the cache parameter group.
mcpgrCacheParameterGroupName :: Lens' ModifyCacheParameterGroupResponse (Maybe Text)
mcpgrCacheParameterGroupName =
    lens _mcpgrCacheParameterGroupName
         (\s a -> s { _mcpgrCacheParameterGroupName = a })

instance FromXML ModifyCacheParameterGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ModifyCacheParameterGroup where
    type Sv ModifyCacheParameterGroup = ElastiCache
    type Rs ModifyCacheParameterGroup = ModifyCacheParameterGroupResponse

    request = post "ModifyCacheParameterGroup"
    response _ = xmlResponse
