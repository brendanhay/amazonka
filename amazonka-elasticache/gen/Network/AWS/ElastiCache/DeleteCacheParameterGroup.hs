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

-- Module      : Network.AWS.ElastiCache.DeleteCacheParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DeleteCacheParameterGroup operation deletes the specified cache
-- parameter group. You cannot delete a cache parameter group if it is
-- associated with any cache clusters.
module Network.AWS.ElastiCache.DeleteCacheParameterGroup
    (
    -- * Request
      DeleteCacheParameterGroupMessage
    -- ** Request constructor
    , deleteCacheParameterGroupMessage
    -- ** Request lenses
    , dcpgm1CacheParameterGroupName

    -- * Response
    , DeleteCacheParameterGroupResponse
    -- ** Response constructor
    , deleteCacheParameterGroupResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types

newtype DeleteCacheParameterGroupMessage = DeleteCacheParameterGroupMessage
    { _dcpgm1CacheParameterGroupName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'DeleteCacheParameterGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcpgm1CacheParameterGroupName' @::@ 'Text'
--
deleteCacheParameterGroupMessage :: Text -- ^ 'dcpgm1CacheParameterGroupName'
                                 -> DeleteCacheParameterGroupMessage
deleteCacheParameterGroupMessage p1 = DeleteCacheParameterGroupMessage
    { _dcpgm1CacheParameterGroupName = p1
    }

-- | The name of the cache parameter group to delete.
dcpgm1CacheParameterGroupName :: Lens' DeleteCacheParameterGroupMessage Text
dcpgm1CacheParameterGroupName =
    lens _dcpgm1CacheParameterGroupName
        (\s a -> s { _dcpgm1CacheParameterGroupName = a })

instance ToPath DeleteCacheParameterGroupMessage where
    toPath = const "/"

instance ToQuery DeleteCacheParameterGroupMessage

data DeleteCacheParameterGroupResponse = DeleteCacheParameterGroupResponse

-- | 'DeleteCacheParameterGroupResponse' constructor.
deleteCacheParameterGroupResponse :: DeleteCacheParameterGroupResponse
deleteCacheParameterGroupResponse = DeleteCacheParameterGroupResponse

instance AWSRequest DeleteCacheParameterGroupMessage where
    type Sv DeleteCacheParameterGroupMessage = ElastiCache
    type Rs DeleteCacheParameterGroupMessage = DeleteCacheParameterGroupResponse

    request  = post "DeleteCacheParameterGroup"
    response = const (nullaryResponse DeleteCacheParameterGroupResponse)
