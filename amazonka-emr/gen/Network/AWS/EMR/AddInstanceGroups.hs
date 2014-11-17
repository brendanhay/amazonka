{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EMR.AddInstanceGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | AddInstanceGroups adds an instance group to a running cluster.
--
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_AddInstanceGroups.html>
module Network.AWS.EMR.AddInstanceGroups
    (
    -- * Request
      AddInstanceGroups
    -- ** Request constructor
    , addInstanceGroups
    -- ** Request lenses
    , aigInstanceGroups
    , aigJobFlowId

    -- * Response
    , AddInstanceGroupsResponse
    -- ** Response constructor
    , addInstanceGroupsResponse
    -- ** Response lenses
    , aigrInstanceGroupIds
    , aigrJobFlowId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.EMR.Types
import qualified GHC.Exts

data AddInstanceGroups = AddInstanceGroups
    { _aigInstanceGroups :: [InstanceGroupConfig]
    , _aigJobFlowId      :: Text
    } deriving (Eq, Show, Generic)

-- | 'AddInstanceGroups' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aigInstanceGroups' @::@ ['InstanceGroupConfig']
--
-- * 'aigJobFlowId' @::@ 'Text'
--
addInstanceGroups :: Text -- ^ 'aigJobFlowId'
                  -> AddInstanceGroups
addInstanceGroups p1 = AddInstanceGroups
    { _aigJobFlowId      = p1
    , _aigInstanceGroups = mempty
    }

-- | Instance Groups to add.
aigInstanceGroups :: Lens' AddInstanceGroups [InstanceGroupConfig]
aigInstanceGroups =
    lens _aigInstanceGroups (\s a -> s { _aigInstanceGroups = a })

-- | Job flow in which to add the instance groups.
aigJobFlowId :: Lens' AddInstanceGroups Text
aigJobFlowId = lens _aigJobFlowId (\s a -> s { _aigJobFlowId = a })

data AddInstanceGroupsResponse = AddInstanceGroupsResponse
    { _aigrInstanceGroupIds :: [Text]
    , _aigrJobFlowId        :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'AddInstanceGroupsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aigrInstanceGroupIds' @::@ ['Text']
--
-- * 'aigrJobFlowId' @::@ 'Maybe' 'Text'
--
addInstanceGroupsResponse :: AddInstanceGroupsResponse
addInstanceGroupsResponse = AddInstanceGroupsResponse
    { _aigrJobFlowId        = Nothing
    , _aigrInstanceGroupIds = mempty
    }

-- | Instance group IDs of the newly created instance groups.
aigrInstanceGroupIds :: Lens' AddInstanceGroupsResponse [Text]
aigrInstanceGroupIds =
    lens _aigrInstanceGroupIds (\s a -> s { _aigrInstanceGroupIds = a })

-- | The job flow ID in which the instance groups are added.
aigrJobFlowId :: Lens' AddInstanceGroupsResponse (Maybe Text)
aigrJobFlowId = lens _aigrJobFlowId (\s a -> s { _aigrJobFlowId = a })

instance ToPath AddInstanceGroups where
    toPath = const "/"

instance ToQuery AddInstanceGroups where
    toQuery = const mempty

instance ToHeaders AddInstanceGroups
instance ToJSON AddInstanceGroups where
    toJSON = genericToJSON jsonOptions

instance AWSRequest AddInstanceGroups where
    type Sv AddInstanceGroups = EMR
    type Rs AddInstanceGroups = AddInstanceGroupsResponse

    request  = post
    response = jsonResponse

instance FromJSON AddInstanceGroupsResponse where
    parseJSON = genericParseJSON jsonOptions
