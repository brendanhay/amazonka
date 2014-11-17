{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_ModifyCacheParameterGroup.html>
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

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import qualified GHC.Exts

data ModifyCacheParameterGroup = ModifyCacheParameterGroup
    { _mcpgCacheParameterGroupName :: Text
    , _mcpgParameterNameValues     :: [ParameterNameValue]
    } deriving (Eq, Show, Generic)

-- | 'ModifyCacheParameterGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mcpgCacheParameterGroupName' @::@ 'Text'
--
-- * 'mcpgParameterNameValues' @::@ ['ParameterNameValue']
--
modifyCacheParameterGroup :: Text -- ^ 'mcpgCacheParameterGroupName'
                          -> ModifyCacheParameterGroup
modifyCacheParameterGroup p1 = ModifyCacheParameterGroup
    { _mcpgCacheParameterGroupName = p1
    , _mcpgParameterNameValues     = mempty
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
    lens _mcpgParameterNameValues (\s a -> s { _mcpgParameterNameValues = a })

newtype ModifyCacheParameterGroupResponse = ModifyCacheParameterGroupResponse
    { _mcpgrCacheParameterGroupName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'ModifyCacheParameterGroupResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mcpgrCacheParameterGroupName' @::@ 'Maybe' 'Text'
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

instance ToPath ModifyCacheParameterGroup where
    toPath = const "/"

instance ToQuery ModifyCacheParameterGroup

instance ToHeaders ModifyCacheParameterGroup

instance AWSRequest ModifyCacheParameterGroup where
    type Sv ModifyCacheParameterGroup = ElastiCache
    type Rs ModifyCacheParameterGroup = ModifyCacheParameterGroupResponse

    request  = post "ModifyCacheParameterGroup"
    response = xmlResponse

instance FromXML ModifyCacheParameterGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ModifyCacheParameterGroupResponse"
