{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.AddInstanceGroups
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- AddInstanceGroups adds an instance group to a running cluster.
--
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_AddInstanceGroups.html>
module Network.AWS.EMR.AddInstanceGroups
    (
    -- * Request
      AddInstanceGroups
    -- ** Request constructor
    , addInstanceGroups
    -- ** Request lenses
    , aigrqInstanceGroups
    , aigrqJobFlowId

    -- * Response
    , AddInstanceGroupsResponse
    -- ** Response constructor
    , addInstanceGroupsResponse
    -- ** Response lenses
    , aigrsJobFlowId
    , aigrsInstanceGroupIds
    , aigrsStatus
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
-- * 'aigrqInstanceGroups'
--
-- * 'aigrqJobFlowId'
data AddInstanceGroups = AddInstanceGroups'
    { _aigrqInstanceGroups :: ![InstanceGroupConfig]
    , _aigrqJobFlowId      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddInstanceGroups' smart constructor.
addInstanceGroups :: Text -> AddInstanceGroups
addInstanceGroups pJobFlowId =
    AddInstanceGroups'
    { _aigrqInstanceGroups = mempty
    , _aigrqJobFlowId = pJobFlowId
    }

-- | Instance Groups to add.
aigrqInstanceGroups :: Lens' AddInstanceGroups [InstanceGroupConfig]
aigrqInstanceGroups = lens _aigrqInstanceGroups (\ s a -> s{_aigrqInstanceGroups = a});

-- | Job flow in which to add the instance groups.
aigrqJobFlowId :: Lens' AddInstanceGroups Text
aigrqJobFlowId = lens _aigrqJobFlowId (\ s a -> s{_aigrqJobFlowId = a});

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
                     <*> (pure (fromEnum s)))

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
              ["InstanceGroups" .= _aigrqInstanceGroups,
               "JobFlowId" .= _aigrqJobFlowId]

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
-- * 'aigrsJobFlowId'
--
-- * 'aigrsInstanceGroupIds'
--
-- * 'aigrsStatus'
data AddInstanceGroupsResponse = AddInstanceGroupsResponse'
    { _aigrsJobFlowId        :: !(Maybe Text)
    , _aigrsInstanceGroupIds :: !(Maybe [Text])
    , _aigrsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddInstanceGroupsResponse' smart constructor.
addInstanceGroupsResponse :: Int -> AddInstanceGroupsResponse
addInstanceGroupsResponse pStatus =
    AddInstanceGroupsResponse'
    { _aigrsJobFlowId = Nothing
    , _aigrsInstanceGroupIds = Nothing
    , _aigrsStatus = pStatus
    }

-- | The job flow ID in which the instance groups are added.
aigrsJobFlowId :: Lens' AddInstanceGroupsResponse (Maybe Text)
aigrsJobFlowId = lens _aigrsJobFlowId (\ s a -> s{_aigrsJobFlowId = a});

-- | Instance group IDs of the newly created instance groups.
aigrsInstanceGroupIds :: Lens' AddInstanceGroupsResponse [Text]
aigrsInstanceGroupIds = lens _aigrsInstanceGroupIds (\ s a -> s{_aigrsInstanceGroupIds = a}) . _Default;

-- | FIXME: Undocumented member.
aigrsStatus :: Lens' AddInstanceGroupsResponse Int
aigrsStatus = lens _aigrsStatus (\ s a -> s{_aigrsStatus = a});
