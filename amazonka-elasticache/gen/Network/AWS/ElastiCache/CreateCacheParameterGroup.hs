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

-- Module      : Network.AWS.ElastiCache.CreateCacheParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The /CreateCacheParameterGroup/ operation creates a new cache parameter group.
-- A cache parameter group is a collection of parameters that you apply to all
-- of the nodes in a cache cluster.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_CreateCacheParameterGroup.html>
module Network.AWS.ElastiCache.CreateCacheParameterGroup
    (
    -- * Request
      CreateCacheParameterGroup
    -- ** Request constructor
    , createCacheParameterGroup
    -- ** Request lenses
    , ccpgCacheParameterGroupFamily
    , ccpgCacheParameterGroupName
    , ccpgDescription

    -- * Response
    , CreateCacheParameterGroupResponse
    -- ** Response constructor
    , createCacheParameterGroupResponse
    -- ** Response lenses
    , ccpgrCacheParameterGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import qualified GHC.Exts

data CreateCacheParameterGroup = CreateCacheParameterGroup
    { _ccpgCacheParameterGroupFamily :: Text
    , _ccpgCacheParameterGroupName   :: Text
    , _ccpgDescription               :: Text
    } deriving (Eq, Ord, Show)

-- | 'CreateCacheParameterGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccpgCacheParameterGroupFamily' @::@ 'Text'
--
-- * 'ccpgCacheParameterGroupName' @::@ 'Text'
--
-- * 'ccpgDescription' @::@ 'Text'
--
createCacheParameterGroup :: Text -- ^ 'ccpgCacheParameterGroupName'
                          -> Text -- ^ 'ccpgCacheParameterGroupFamily'
                          -> Text -- ^ 'ccpgDescription'
                          -> CreateCacheParameterGroup
createCacheParameterGroup p1 p2 p3 = CreateCacheParameterGroup
    { _ccpgCacheParameterGroupName   = p1
    , _ccpgCacheParameterGroupFamily = p2
    , _ccpgDescription               = p3
    }

-- | The name of the cache parameter group family the cache parameter group can be
-- used with.
--
-- Valid values are: 'memcached1.4' | 'redis2.6' | 'redis2.8'
--
ccpgCacheParameterGroupFamily :: Lens' CreateCacheParameterGroup Text
ccpgCacheParameterGroupFamily =
    lens _ccpgCacheParameterGroupFamily
        (\s a -> s { _ccpgCacheParameterGroupFamily = a })

-- | A user-specified name for the cache parameter group.
--
ccpgCacheParameterGroupName :: Lens' CreateCacheParameterGroup Text
ccpgCacheParameterGroupName =
    lens _ccpgCacheParameterGroupName
        (\s a -> s { _ccpgCacheParameterGroupName = a })

-- | A user-specified description for the cache parameter group.
--
ccpgDescription :: Lens' CreateCacheParameterGroup Text
ccpgDescription = lens _ccpgDescription (\s a -> s { _ccpgDescription = a })

newtype CreateCacheParameterGroupResponse = CreateCacheParameterGroupResponse
    { _ccpgrCacheParameterGroup :: Maybe CacheParameterGroup
    } deriving (Eq, Show)

-- | 'CreateCacheParameterGroupResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccpgrCacheParameterGroup' @::@ 'Maybe' 'CacheParameterGroup'
--
createCacheParameterGroupResponse :: CreateCacheParameterGroupResponse
createCacheParameterGroupResponse = CreateCacheParameterGroupResponse
    { _ccpgrCacheParameterGroup = Nothing
    }

ccpgrCacheParameterGroup :: Lens' CreateCacheParameterGroupResponse (Maybe CacheParameterGroup)
ccpgrCacheParameterGroup =
    lens _ccpgrCacheParameterGroup
        (\s a -> s { _ccpgrCacheParameterGroup = a })

instance ToPath CreateCacheParameterGroup where
    toPath = const "/"

instance ToQuery CreateCacheParameterGroup where
    toQuery CreateCacheParameterGroup{..} = mconcat
        [ "CacheParameterGroupFamily" =? _ccpgCacheParameterGroupFamily
        , "CacheParameterGroupName"   =? _ccpgCacheParameterGroupName
        , "Description"               =? _ccpgDescription
        ]

instance ToHeaders CreateCacheParameterGroup

instance AWSRequest CreateCacheParameterGroup where
    type Sv CreateCacheParameterGroup = ElastiCache
    type Rs CreateCacheParameterGroup = CreateCacheParameterGroupResponse

    request  = post "CreateCacheParameterGroup"
    response = xmlResponse

instance FromXML CreateCacheParameterGroupResponse where
    parseXML = withElement "CreateCacheParameterGroupResult" $ \x -> CreateCacheParameterGroupResponse
        <$> x .@? "CacheParameterGroup"
