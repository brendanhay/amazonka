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

-- Module      : Network.AWS.ElastiCache.DeleteCacheSecurityGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DeleteCacheSecurityGroup operation deletes a cache security group.
module Network.AWS.ElastiCache.DeleteCacheSecurityGroup
    (
    -- * Request
      DeleteCacheSecurityGroup
    -- ** Request constructor
    , deleteCacheSecurityGroup
    -- ** Request lenses
    , dcsgCacheSecurityGroupName

    -- * Response
    , DeleteCacheSecurityGroupResponse
    -- ** Response constructor
    , deleteCacheSecurityGroupResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import qualified GHC.Exts

newtype DeleteCacheSecurityGroup = DeleteCacheSecurityGroup
    { _dcsgCacheSecurityGroupName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteCacheSecurityGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsgCacheSecurityGroupName' @::@ 'Text'
--
deleteCacheSecurityGroup :: Text -- ^ 'dcsgCacheSecurityGroupName'
                         -> DeleteCacheSecurityGroup
deleteCacheSecurityGroup p1 = DeleteCacheSecurityGroup
    { _dcsgCacheSecurityGroupName = p1
    }

-- | The name of the cache security group to delete.
dcsgCacheSecurityGroupName :: Lens' DeleteCacheSecurityGroup Text
dcsgCacheSecurityGroupName =
    lens _dcsgCacheSecurityGroupName
        (\s a -> s { _dcsgCacheSecurityGroupName = a })

instance ToQuery DeleteCacheSecurityGroup

instance ToPath DeleteCacheSecurityGroup where
    toPath = const "/"

data DeleteCacheSecurityGroupResponse = DeleteCacheSecurityGroupResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteCacheSecurityGroupResponse' constructor.
deleteCacheSecurityGroupResponse :: DeleteCacheSecurityGroupResponse
deleteCacheSecurityGroupResponse = DeleteCacheSecurityGroupResponse

instance AWSRequest DeleteCacheSecurityGroup where
    type Sv DeleteCacheSecurityGroup = ElastiCache
    type Rs DeleteCacheSecurityGroup = DeleteCacheSecurityGroupResponse

    request  = post "DeleteCacheSecurityGroup"
    response = nullaryResponse DeleteCacheSecurityGroupResponse
