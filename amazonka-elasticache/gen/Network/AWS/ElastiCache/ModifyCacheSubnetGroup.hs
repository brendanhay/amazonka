{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.ModifyCacheSubnetGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | The /ModifyCacheSubnetGroup/ operation modifies an existing cache subnet group.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_ModifyCacheSubnetGroup.html>
module Network.AWS.ElastiCache.ModifyCacheSubnetGroup
    (
    -- * Request
      ModifyCacheSubnetGroup
    -- ** Request constructor
    , modifyCacheSubnetGroup
    -- ** Request lenses
    , mcsgCacheSubnetGroupDescription
    , mcsgCacheSubnetGroupName
    , mcsgSubnetIds

    -- * Response
    , ModifyCacheSubnetGroupResponse
    -- ** Response constructor
    , modifyCacheSubnetGroupResponse
    -- ** Response lenses
    , mcsgrCacheSubnetGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import qualified GHC.Exts

data ModifyCacheSubnetGroup = ModifyCacheSubnetGroup
    { _mcsgCacheSubnetGroupDescription :: Maybe Text
    , _mcsgCacheSubnetGroupName        :: Text
    , _mcsgSubnetIds                   :: List "member" Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ModifyCacheSubnetGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mcsgCacheSubnetGroupDescription' @::@ 'Maybe' 'Text'
--
-- * 'mcsgCacheSubnetGroupName' @::@ 'Text'
--
-- * 'mcsgSubnetIds' @::@ ['Text']
--
modifyCacheSubnetGroup :: Text -- ^ 'mcsgCacheSubnetGroupName'
                       -> ModifyCacheSubnetGroup
modifyCacheSubnetGroup p1 = ModifyCacheSubnetGroup
    { _mcsgCacheSubnetGroupName        = p1
    , _mcsgCacheSubnetGroupDescription = Nothing
    , _mcsgSubnetIds                   = mempty
    }

-- | A description for the cache subnet group.
mcsgCacheSubnetGroupDescription :: Lens' ModifyCacheSubnetGroup (Maybe Text)
mcsgCacheSubnetGroupDescription =
    lens _mcsgCacheSubnetGroupDescription
        (\s a -> s { _mcsgCacheSubnetGroupDescription = a })

-- | The name for the cache subnet group. This value is stored as a lowercase
-- string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters or
-- hyphens.
--
-- Example: 'mysubnetgroup'
mcsgCacheSubnetGroupName :: Lens' ModifyCacheSubnetGroup Text
mcsgCacheSubnetGroupName =
    lens _mcsgCacheSubnetGroupName
        (\s a -> s { _mcsgCacheSubnetGroupName = a })

-- | The EC2 subnet IDs for the cache subnet group.
mcsgSubnetIds :: Lens' ModifyCacheSubnetGroup [Text]
mcsgSubnetIds = lens _mcsgSubnetIds (\s a -> s { _mcsgSubnetIds = a }) . _List

newtype ModifyCacheSubnetGroupResponse = ModifyCacheSubnetGroupResponse
    { _mcsgrCacheSubnetGroup :: Maybe CacheSubnetGroup
    } deriving (Eq, Read, Show)

-- | 'ModifyCacheSubnetGroupResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mcsgrCacheSubnetGroup' @::@ 'Maybe' 'CacheSubnetGroup'
--
modifyCacheSubnetGroupResponse :: ModifyCacheSubnetGroupResponse
modifyCacheSubnetGroupResponse = ModifyCacheSubnetGroupResponse
    { _mcsgrCacheSubnetGroup = Nothing
    }

mcsgrCacheSubnetGroup :: Lens' ModifyCacheSubnetGroupResponse (Maybe CacheSubnetGroup)
mcsgrCacheSubnetGroup =
    lens _mcsgrCacheSubnetGroup (\s a -> s { _mcsgrCacheSubnetGroup = a })

instance ToPath ModifyCacheSubnetGroup where
    toPath = const "/"

instance ToQuery ModifyCacheSubnetGroup where
    toQuery ModifyCacheSubnetGroup{..} = mconcat
        [ "CacheSubnetGroupDescription" =? _mcsgCacheSubnetGroupDescription
        , "CacheSubnetGroupName"        =? _mcsgCacheSubnetGroupName
        , "SubnetIds"                   =? _mcsgSubnetIds
        ]

instance ToHeaders ModifyCacheSubnetGroup

instance AWSRequest ModifyCacheSubnetGroup where
    type Sv ModifyCacheSubnetGroup = ElastiCache
    type Rs ModifyCacheSubnetGroup = ModifyCacheSubnetGroupResponse

    request  = post "ModifyCacheSubnetGroup"
    response = xmlResponse

instance FromXML ModifyCacheSubnetGroupResponse where
    parseXML = withElement "ModifyCacheSubnetGroupResult" $ \x -> ModifyCacheSubnetGroupResponse
        <$> x .@? "CacheSubnetGroup"
