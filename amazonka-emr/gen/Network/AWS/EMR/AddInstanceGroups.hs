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
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AddInstanceGroups adds an instance group to a running cluster.
--
-- /See:/ <http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_AddInstanceGroups.html AWS API Reference> for AddInstanceGroups.
module Network.AWS.EMR.AddInstanceGroups
    (
    -- * Creating a Request
      AddInstanceGroups
    , addInstanceGroups
    -- * Request Lenses
    , aigInstanceGroups
    , aigJobFlowId

    -- * Destructuring the Response
    , AddInstanceGroupsResponse
    , addInstanceGroupsResponse
    -- * Response Lenses
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
-- * 'aigInstanceGroups'
--
-- * 'aigJobFlowId'
data AddInstanceGroups = AddInstanceGroups'
    { _aigInstanceGroups :: ![InstanceGroupConfig]
    , _aigJobFlowId      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AddInstanceGroups' smart constructor.
addInstanceGroups :: Text -> AddInstanceGroups
addInstanceGroups pJobFlowId_ =
    AddInstanceGroups'
    { _aigInstanceGroups = mempty
    , _aigJobFlowId = pJobFlowId_
    }

-- | Instance Groups to add.
aigInstanceGroups :: Lens' AddInstanceGroups [InstanceGroupConfig]
aigInstanceGroups = lens _aigInstanceGroups (\ s a -> s{_aigInstanceGroups = a}) . _Coerce;

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
addInstanceGroupsResponse pStatus_ =
    AddInstanceGroupsResponse'
    { _aigrsJobFlowId = Nothing
    , _aigrsInstanceGroupIds = Nothing
    , _aigrsStatus = pStatus_
    }

-- | The job flow ID in which the instance groups are added.
aigrsJobFlowId :: Lens' AddInstanceGroupsResponse (Maybe Text)
aigrsJobFlowId = lens _aigrsJobFlowId (\ s a -> s{_aigrsJobFlowId = a});

-- | Instance group IDs of the newly created instance groups.
aigrsInstanceGroupIds :: Lens' AddInstanceGroupsResponse [Text]
aigrsInstanceGroupIds = lens _aigrsInstanceGroupIds (\ s a -> s{_aigrsInstanceGroupIds = a}) . _Default . _Coerce;

-- | Undocumented member.
aigrsStatus :: Lens' AddInstanceGroupsResponse Int
aigrsStatus = lens _aigrsStatus (\ s a -> s{_aigrsStatus = a});
