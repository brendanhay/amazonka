{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.ResetCacheParameterGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The ResetCacheParameterGroup operation modifies the parameters of a cache
-- parameter group to the engine or system default value. You can reset
-- specific parameters by submitting a list of parameter names. To reset the
-- entire cache parameter group, specify the ResetAllParameters and
-- CacheParameterGroupName parameters.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_ResetCacheParameterGroup.html>
module Network.AWS.ElastiCache.ResetCacheParameterGroup
    (
    -- * Request
      ResetCacheParameterGroup
    -- ** Request constructor
    , resetCacheParameterGroup
    -- ** Request lenses
    , rcpgCacheParameterGroupName
    , rcpgParameterNameValues
    , rcpgResetAllParameters

    -- * Response
    , ResetCacheParameterGroupResponse
    -- ** Response constructor
    , resetCacheParameterGroupResponse
    -- ** Response lenses
    , rcpgrCacheParameterGroupName
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import qualified GHC.Exts

data ResetCacheParameterGroup = ResetCacheParameterGroup
    { _rcpgCacheParameterGroupName :: Text
    , _rcpgParameterNameValues     :: [ParameterNameValue]
    , _rcpgResetAllParameters      :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | 'ResetCacheParameterGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcpgCacheParameterGroupName' @::@ 'Text'
--
-- * 'rcpgParameterNameValues' @::@ ['ParameterNameValue']
--
-- * 'rcpgResetAllParameters' @::@ 'Maybe' 'Bool'
--
resetCacheParameterGroup :: Text -- ^ 'rcpgCacheParameterGroupName'
                         -> ResetCacheParameterGroup
resetCacheParameterGroup p1 = ResetCacheParameterGroup
    { _rcpgCacheParameterGroupName = p1
    , _rcpgResetAllParameters      = Nothing
    , _rcpgParameterNameValues     = mempty
    }

-- | The name of the cache parameter group to reset.
rcpgCacheParameterGroupName :: Lens' ResetCacheParameterGroup Text
rcpgCacheParameterGroupName =
    lens _rcpgCacheParameterGroupName
        (\s a -> s { _rcpgCacheParameterGroupName = a })

-- | An array of parameter names to be reset. If you are not resetting the
-- entire cache parameter group, you must specify at least one parameter
-- name.
rcpgParameterNameValues :: Lens' ResetCacheParameterGroup [ParameterNameValue]
rcpgParameterNameValues =
    lens _rcpgParameterNameValues (\s a -> s { _rcpgParameterNameValues = a })

-- | If true, all parameters in the cache parameter group will be reset to
-- default values. If false, no such action occurs. Valid values: true |
-- false.
rcpgResetAllParameters :: Lens' ResetCacheParameterGroup (Maybe Bool)
rcpgResetAllParameters =
    lens _rcpgResetAllParameters (\s a -> s { _rcpgResetAllParameters = a })

newtype ResetCacheParameterGroupResponse = ResetCacheParameterGroupResponse
    { _rcpgrCacheParameterGroupName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'ResetCacheParameterGroupResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rcpgrCacheParameterGroupName' @::@ 'Maybe' 'Text'
--
resetCacheParameterGroupResponse :: ResetCacheParameterGroupResponse
resetCacheParameterGroupResponse = ResetCacheParameterGroupResponse
    { _rcpgrCacheParameterGroupName = Nothing
    }

-- | The name of the cache parameter group.
rcpgrCacheParameterGroupName :: Lens' ResetCacheParameterGroupResponse (Maybe Text)
rcpgrCacheParameterGroupName =
    lens _rcpgrCacheParameterGroupName
        (\s a -> s { _rcpgrCacheParameterGroupName = a })

instance ToPath ResetCacheParameterGroup where
    toPath = const "/"

instance ToQuery ResetCacheParameterGroup

instance ToHeaders ResetCacheParameterGroup

instance AWSRequest ResetCacheParameterGroup where
    type Sv ResetCacheParameterGroup = ElastiCache
    type Rs ResetCacheParameterGroup = ResetCacheParameterGroupResponse

    request  = post "ResetCacheParameterGroup"
    response = xmlResponse

instance FromXML ResetCacheParameterGroupResponse where
    parseXML = withElement "ResetCacheParameterGroupResult" $ \x ->
        ResetCacheParameterGroupResponse
            <$> x .@? "CacheParameterGroupName"
