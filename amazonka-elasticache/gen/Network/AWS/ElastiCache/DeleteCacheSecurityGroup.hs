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
      DeleteCacheSecurityGroupMessage
    -- ** Request constructor
    , deleteCacheSecurityGroupMessage
    -- ** Request lenses
    , dcsgmCacheSecurityGroupName

    -- * Response
    , DeleteCacheSecurityGroupResponse
    -- ** Response constructor
    , deleteCacheSecurityGroupResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types

newtype DeleteCacheSecurityGroupMessage = DeleteCacheSecurityGroupMessage
    { _dcsgmCacheSecurityGroupName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'DeleteCacheSecurityGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsgmCacheSecurityGroupName' @::@ 'Text'
--
deleteCacheSecurityGroupMessage :: Text -- ^ 'dcsgmCacheSecurityGroupName'
                                -> DeleteCacheSecurityGroupMessage
deleteCacheSecurityGroupMessage p1 = DeleteCacheSecurityGroupMessage
    { _dcsgmCacheSecurityGroupName = p1
    }

-- | The name of the cache security group to delete.
dcsgmCacheSecurityGroupName :: Lens' DeleteCacheSecurityGroupMessage Text
dcsgmCacheSecurityGroupName =
    lens _dcsgmCacheSecurityGroupName
        (\s a -> s { _dcsgmCacheSecurityGroupName = a })
instance ToQuery DeleteCacheSecurityGroupMessage

instance ToPath DeleteCacheSecurityGroupMessage where
    toPath = const "/"

data DeleteCacheSecurityGroupResponse = DeleteCacheSecurityGroupResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteCacheSecurityGroupResponse' constructor.
deleteCacheSecurityGroupResponse :: DeleteCacheSecurityGroupResponse
deleteCacheSecurityGroupResponse = DeleteCacheSecurityGroupResponse
instance FromXML DeleteCacheSecurityGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteCacheSecurityGroupResponse"

instance AWSRequest DeleteCacheSecurityGroupMessage where
    type Sv DeleteCacheSecurityGroupMessage = ElastiCache
    type Rs DeleteCacheSecurityGroupMessage = DeleteCacheSecurityGroupResponse

    request  = post "DeleteCacheSecurityGroup"
    response = nullaryResponse DeleteCacheSecurityGroupResponse
