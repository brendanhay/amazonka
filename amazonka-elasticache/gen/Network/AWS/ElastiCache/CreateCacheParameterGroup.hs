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

-- Module      : Network.AWS.ElastiCache.CreateCacheParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CreateCacheParameterGroup operation creates a new cache parameter
-- group. A cache parameter group is a collection of parameters that you apply
-- to all of the nodes in a cache cluster.
module Network.AWS.ElastiCache.CreateCacheParameterGroup
    (
    -- * Request
      CreateCacheParameterGroupMessage
    -- ** Request constructor
    , createCacheParameterGroup
    -- ** Request lenses
    , ccpgmCacheParameterGroupFamily
    , ccpgmCacheParameterGroupName
    , ccpgmDescription

    -- * Response
    , CreateCacheParameterGroupResult
    -- ** Response constructor
    , createCacheParameterGroupResponse
    -- ** Response lenses
    , ccpgrCacheParameterGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types

data CreateCacheParameterGroupMessage = CreateCacheParameterGroupMessage
    { _ccpgmCacheParameterGroupFamily :: Text
    , _ccpgmCacheParameterGroupName   :: Text
    , _ccpgmDescription               :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateCacheParameterGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccpgmCacheParameterGroupFamily' @::@ 'Text'
--
-- * 'ccpgmCacheParameterGroupName' @::@ 'Text'
--
-- * 'ccpgmDescription' @::@ 'Text'
--
createCacheParameterGroup :: Text -- ^ 'ccpgmCacheParameterGroupName'
                          -> Text -- ^ 'ccpgmCacheParameterGroupFamily'
                          -> Text -- ^ 'ccpgmDescription'
                          -> CreateCacheParameterGroupMessage
createCacheParameterGroup p1 p2 p3 = CreateCacheParameterGroupMessage
    { _ccpgmCacheParameterGroupName   = p1
    , _ccpgmCacheParameterGroupFamily = p2
    , _ccpgmDescription               = p3
    }

-- | The name of the cache parameter group family the cache parameter group
-- can be used with. Valid values are: memcached1.4 | redis2.6 | redis2.8.
ccpgmCacheParameterGroupFamily :: Lens' CreateCacheParameterGroupMessage Text
ccpgmCacheParameterGroupFamily =
    lens _ccpgmCacheParameterGroupFamily
        (\s a -> s { _ccpgmCacheParameterGroupFamily = a })

-- | A user-specified name for the cache parameter group.
ccpgmCacheParameterGroupName :: Lens' CreateCacheParameterGroupMessage Text
ccpgmCacheParameterGroupName =
    lens _ccpgmCacheParameterGroupName
        (\s a -> s { _ccpgmCacheParameterGroupName = a })

-- | A user-specified description for the cache parameter group.
ccpgmDescription :: Lens' CreateCacheParameterGroupMessage Text
ccpgmDescription = lens _ccpgmDescription (\s a -> s { _ccpgmDescription = a })

instance ToQuery CreateCacheParameterGroupMessage

instance ToPath CreateCacheParameterGroupMessage where
    toPath = const "/"

newtype CreateCacheParameterGroupResult = CreateCacheParameterGroupResult
    { _ccpgrCacheParameterGroup :: Maybe CacheParameterGroup
    } deriving (Eq, Show, Generic)

-- | 'CreateCacheParameterGroupResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccpgrCacheParameterGroup' @::@ 'Maybe' 'CacheParameterGroup'
--
createCacheParameterGroupResponse :: CreateCacheParameterGroupResult
createCacheParameterGroupResponse = CreateCacheParameterGroupResult
    { _ccpgrCacheParameterGroup = Nothing
    }

ccpgrCacheParameterGroup :: Lens' CreateCacheParameterGroupResult (Maybe CacheParameterGroup)
ccpgrCacheParameterGroup =
    lens _ccpgrCacheParameterGroup
        (\s a -> s { _ccpgrCacheParameterGroup = a })

instance FromXML CreateCacheParameterGroupResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateCacheParameterGroupResult"

instance AWSRequest CreateCacheParameterGroupMessage where
    type Sv CreateCacheParameterGroupMessage = ElastiCache
    type Rs CreateCacheParameterGroupMessage = CreateCacheParameterGroupResult

    request  = post "CreateCacheParameterGroup"
    response = xmlResponse $ \h x -> CreateCacheParameterGroupResult
        <$> x %| "CacheParameterGroup"
