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

-- Module      : Network.AWS.ElastiCache.ModifyCacheSubnetGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ModifyCacheSubnetGroup operation modifies an existing cache subnet
-- group.
module Network.AWS.ElastiCache.ModifyCacheSubnetGroup
    (
    -- * Request
      ModifyCacheSubnetGroupMessage
    -- ** Request constructor
    , modifyCacheSubnetGroup
    -- ** Request lenses
    , mcsgmCacheSubnetGroupDescription
    , mcsgmCacheSubnetGroupName
    , mcsgmSubnetIds

    -- * Response
    , ModifyCacheSubnetGroupResult
    -- ** Response constructor
    , modifyCacheSubnetGroupResponse
    -- ** Response lenses
    , mcsgrCacheSubnetGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types

data ModifyCacheSubnetGroupMessage = ModifyCacheSubnetGroupMessage
    { _mcsgmCacheSubnetGroupDescription :: Maybe Text
    , _mcsgmCacheSubnetGroupName        :: Text
    , _mcsgmSubnetIds                   :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'ModifyCacheSubnetGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mcsgmCacheSubnetGroupDescription' @::@ 'Maybe' 'Text'
--
-- * 'mcsgmCacheSubnetGroupName' @::@ 'Text'
--
-- * 'mcsgmSubnetIds' @::@ ['Text']
--
modifyCacheSubnetGroup :: Text -- ^ 'mcsgmCacheSubnetGroupName'
                       -> ModifyCacheSubnetGroupMessage
modifyCacheSubnetGroup p1 = ModifyCacheSubnetGroupMessage
    { _mcsgmCacheSubnetGroupName        = p1
    , _mcsgmCacheSubnetGroupDescription = Nothing
    , _mcsgmSubnetIds                   = mempty
    }

-- | A description for the cache subnet group.
mcsgmCacheSubnetGroupDescription :: Lens' ModifyCacheSubnetGroupMessage (Maybe Text)
mcsgmCacheSubnetGroupDescription =
    lens _mcsgmCacheSubnetGroupDescription
        (\s a -> s { _mcsgmCacheSubnetGroupDescription = a })

-- | The name for the cache subnet group. This value is stored as a lowercase
-- string. Constraints: Must contain no more than 255 alphanumeric
-- characters or hyphens. Example: mysubnetgroup.
mcsgmCacheSubnetGroupName :: Lens' ModifyCacheSubnetGroupMessage Text
mcsgmCacheSubnetGroupName =
    lens _mcsgmCacheSubnetGroupName
        (\s a -> s { _mcsgmCacheSubnetGroupName = a })

-- | The EC2 subnet IDs for the cache subnet group.
mcsgmSubnetIds :: Lens' ModifyCacheSubnetGroupMessage [Text]
mcsgmSubnetIds = lens _mcsgmSubnetIds (\s a -> s { _mcsgmSubnetIds = a })

instance ToPath ModifyCacheSubnetGroupMessage where
    toPath = const "/"

instance ToQuery ModifyCacheSubnetGroupMessage

newtype ModifyCacheSubnetGroupResult = ModifyCacheSubnetGroupResult
    { _mcsgrCacheSubnetGroup :: Maybe CacheSubnetGroup
    } deriving (Eq, Show, Generic)

-- | 'ModifyCacheSubnetGroupResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mcsgrCacheSubnetGroup' @::@ 'Maybe' 'CacheSubnetGroup'
--
modifyCacheSubnetGroupResponse :: ModifyCacheSubnetGroupResult
modifyCacheSubnetGroupResponse = ModifyCacheSubnetGroupResult
    { _mcsgrCacheSubnetGroup = Nothing
    }

mcsgrCacheSubnetGroup :: Lens' ModifyCacheSubnetGroupResult (Maybe CacheSubnetGroup)
mcsgrCacheSubnetGroup =
    lens _mcsgrCacheSubnetGroup (\s a -> s { _mcsgrCacheSubnetGroup = a })

instance AWSRequest ModifyCacheSubnetGroupMessage where
    type Sv ModifyCacheSubnetGroupMessage = ElastiCache
    type Rs ModifyCacheSubnetGroupMessage = ModifyCacheSubnetGroupResult

    request  = post "ModifyCacheSubnetGroup"
    response = xmlResponse $ \h x -> ModifyCacheSubnetGroupResult
        <$> x %| "CacheSubnetGroup"
