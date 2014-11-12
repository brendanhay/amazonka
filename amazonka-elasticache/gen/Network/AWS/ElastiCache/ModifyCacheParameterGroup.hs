{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
module Network.AWS.ElastiCache.ModifyCacheParameterGroup
    (
    -- * Request
      ModifyCacheParameterGroupMessage
    -- ** Request constructor
    , modifyCacheParameterGroup
    -- ** Request lenses
    , mcpgmCacheParameterGroupName
    , mcpgmParameterNameValues

    -- * Response
    , CacheParameterGroupNameMessage
    -- ** Response constructor
    , cacheParameterGroupNameMessage
    -- ** Response lenses
    , cpgnmCacheParameterGroupName
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types

data ModifyCacheParameterGroupMessage = ModifyCacheParameterGroupMessage
    { _mcpgmCacheParameterGroupName :: Text
    , _mcpgmParameterNameValues     :: [ParameterNameValue]
    } deriving (Eq, Show, Generic)

-- | 'ModifyCacheParameterGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mcpgmCacheParameterGroupName' @::@ 'Text'
--
-- * 'mcpgmParameterNameValues' @::@ ['ParameterNameValue']
--
modifyCacheParameterGroup :: Text -- ^ 'mcpgmCacheParameterGroupName'
                          -> ModifyCacheParameterGroupMessage
modifyCacheParameterGroup p1 = ModifyCacheParameterGroupMessage
    { _mcpgmCacheParameterGroupName = p1
    , _mcpgmParameterNameValues     = mempty
    }

-- | The name of the cache parameter group to modify.
mcpgmCacheParameterGroupName :: Lens' ModifyCacheParameterGroupMessage Text
mcpgmCacheParameterGroupName =
    lens _mcpgmCacheParameterGroupName
        (\s a -> s { _mcpgmCacheParameterGroupName = a })

-- | An array of parameter names and values for the parameter update. You must
-- supply at least one parameter name and value; subsequent arguments are
-- optional. A maximum of 20 parameters may be modified per request.
mcpgmParameterNameValues :: Lens' ModifyCacheParameterGroupMessage [ParameterNameValue]
mcpgmParameterNameValues =
    lens _mcpgmParameterNameValues
        (\s a -> s { _mcpgmParameterNameValues = a })

instance ToQuery ModifyCacheParameterGroupMessage

instance ToPath ModifyCacheParameterGroupMessage where
    toPath = const "/"

instance AWSRequest ModifyCacheParameterGroupMessage where
    type Sv ModifyCacheParameterGroupMessage = ElastiCache
    type Rs ModifyCacheParameterGroupMessage = CacheParameterGroupNameMessage

    request  = post "ModifyCacheParameterGroup"
    response = xmlResponse $ const decodeCursor
