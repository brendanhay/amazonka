{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EMR.AddInstanceGroups
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

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
    , aigrJobFlowId
    , aigrInstanceGroupIds
    , aigrStatus
    ) where

import           Network.AWS.EMR.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Input to an AddInstanceGroups call.
--
-- /See:/ 'addInstanceGroups' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aigInstanceGroups'
--
-- * 'aigJobFlowId'
data AddInstanceGroups = AddInstanceGroups'
    { _aigInstanceGroups :: ![InstanceGroupConfig]
    , _aigJobFlowId      :: !Text
    } deriving (Eq,Read,Show)

-- | 'AddInstanceGroups' smart constructor.
addInstanceGroups :: Text -> AddInstanceGroups
addInstanceGroups pJobFlowId =
    AddInstanceGroups'
    { _aigInstanceGroups = mempty
    , _aigJobFlowId = pJobFlowId
    }

-- | Instance Groups to add.
aigInstanceGroups :: Lens' AddInstanceGroups [InstanceGroupConfig]
aigInstanceGroups = lens _aigInstanceGroups (\ s a -> s{_aigInstanceGroups = a});

-- | Job flow in which to add the instance groups.
aigJobFlowId :: Lens' AddInstanceGroups Text
aigJobFlowId = lens _aigJobFlowId (\ s a -> s{_aigJobFlowId = a});

instance AWSRequest AddInstanceGroups where
        type Sv AddInstanceGroups = EMR
        type Rs AddInstanceGroups = AddInstanceGroupsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 AddInstanceGroupsResponse' <$>
                   (x .?> "JobFlowId") <*>
                     (x .?> "InstanceGroupIds" .!@ mempty)
                     <*> (pure s))

instance ToHeaders AddInstanceGroups where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ElasticMapReduce.AddInstanceGroups" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AddInstanceGroups where
        toJSON AddInstanceGroups'{..}
          = object
              ["InstanceGroups" .= _aigInstanceGroups,
               "JobFlowId" .= _aigJobFlowId]

instance ToPath AddInstanceGroups where
        toPath = const "/"

instance ToQuery AddInstanceGroups where
        toQuery = const mempty

-- | Output from an AddInstanceGroups call.
--
-- /See:/ 'addInstanceGroupsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aigrJobFlowId'
--
-- * 'aigrInstanceGroupIds'
--
-- * 'aigrStatus'
data AddInstanceGroupsResponse = AddInstanceGroupsResponse'
    { _aigrJobFlowId        :: !(Maybe Text)
    , _aigrInstanceGroupIds :: !(Maybe [Text])
    , _aigrStatus           :: !Status
    } deriving (Eq,Show)

-- | 'AddInstanceGroupsResponse' smart constructor.
addInstanceGroupsResponse :: Status -> AddInstanceGroupsResponse
addInstanceGroupsResponse pStatus =
    AddInstanceGroupsResponse'
    { _aigrJobFlowId = Nothing
    , _aigrInstanceGroupIds = Nothing
    , _aigrStatus = pStatus
    }

-- | The job flow ID in which the instance groups are added.
aigrJobFlowId :: Lens' AddInstanceGroupsResponse (Maybe Text)
aigrJobFlowId = lens _aigrJobFlowId (\ s a -> s{_aigrJobFlowId = a});

-- | Instance group IDs of the newly created instance groups.
aigrInstanceGroupIds :: Lens' AddInstanceGroupsResponse [Text]
aigrInstanceGroupIds = lens _aigrInstanceGroupIds (\ s a -> s{_aigrInstanceGroupIds = a}) . _Default;

-- | FIXME: Undocumented member.
aigrStatus :: Lens' AddInstanceGroupsResponse Status
aigrStatus = lens _aigrStatus (\ s a -> s{_aigrStatus = a});
