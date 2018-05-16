{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.AddInstanceGroups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more instance groups to a running cluster.
--
--
module Network.AWS.EMR.AddInstanceGroups
    (
    -- * Creating a Request
      addInstanceGroups
    , AddInstanceGroups
    -- * Request Lenses
    , aigInstanceGroups
    , aigJobFlowId

    -- * Destructuring the Response
    , addInstanceGroupsResponse
    , AddInstanceGroupsResponse
    -- * Response Lenses
    , aigrsJobFlowId
    , aigrsInstanceGroupIds
    , aigrsResponseStatus
    ) where

import Network.AWS.EMR.Types
import Network.AWS.EMR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Input to an AddInstanceGroups call.
--
--
--
-- /See:/ 'addInstanceGroups' smart constructor.
data AddInstanceGroups = AddInstanceGroups'
  { _aigInstanceGroups :: ![InstanceGroupConfig]
  , _aigJobFlowId      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddInstanceGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aigInstanceGroups' - Instance groups to add.
--
-- * 'aigJobFlowId' - Job flow in which to add the instance groups.
addInstanceGroups
    :: Text -- ^ 'aigJobFlowId'
    -> AddInstanceGroups
addInstanceGroups pJobFlowId_ =
  AddInstanceGroups' {_aigInstanceGroups = mempty, _aigJobFlowId = pJobFlowId_}


-- | Instance groups to add.
aigInstanceGroups :: Lens' AddInstanceGroups [InstanceGroupConfig]
aigInstanceGroups = lens _aigInstanceGroups (\ s a -> s{_aigInstanceGroups = a}) . _Coerce

-- | Job flow in which to add the instance groups.
aigJobFlowId :: Lens' AddInstanceGroups Text
aigJobFlowId = lens _aigJobFlowId (\ s a -> s{_aigJobFlowId = a})

instance AWSRequest AddInstanceGroups where
        type Rs AddInstanceGroups = AddInstanceGroupsResponse
        request = postJSON emr
        response
          = receiveJSON
              (\ s h x ->
                 AddInstanceGroupsResponse' <$>
                   (x .?> "JobFlowId") <*>
                     (x .?> "InstanceGroupIds" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable AddInstanceGroups where

instance NFData AddInstanceGroups where

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
              (catMaybes
                 [Just ("InstanceGroups" .= _aigInstanceGroups),
                  Just ("JobFlowId" .= _aigJobFlowId)])

instance ToPath AddInstanceGroups where
        toPath = const "/"

instance ToQuery AddInstanceGroups where
        toQuery = const mempty

-- | Output from an AddInstanceGroups call.
--
--
--
-- /See:/ 'addInstanceGroupsResponse' smart constructor.
data AddInstanceGroupsResponse = AddInstanceGroupsResponse'
  { _aigrsJobFlowId        :: !(Maybe Text)
  , _aigrsInstanceGroupIds :: !(Maybe [Text])
  , _aigrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddInstanceGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aigrsJobFlowId' - The job flow ID in which the instance groups are added.
--
-- * 'aigrsInstanceGroupIds' - Instance group IDs of the newly created instance groups.
--
-- * 'aigrsResponseStatus' - -- | The response status code.
addInstanceGroupsResponse
    :: Int -- ^ 'aigrsResponseStatus'
    -> AddInstanceGroupsResponse
addInstanceGroupsResponse pResponseStatus_ =
  AddInstanceGroupsResponse'
    { _aigrsJobFlowId = Nothing
    , _aigrsInstanceGroupIds = Nothing
    , _aigrsResponseStatus = pResponseStatus_
    }


-- | The job flow ID in which the instance groups are added.
aigrsJobFlowId :: Lens' AddInstanceGroupsResponse (Maybe Text)
aigrsJobFlowId = lens _aigrsJobFlowId (\ s a -> s{_aigrsJobFlowId = a})

-- | Instance group IDs of the newly created instance groups.
aigrsInstanceGroupIds :: Lens' AddInstanceGroupsResponse [Text]
aigrsInstanceGroupIds = lens _aigrsInstanceGroupIds (\ s a -> s{_aigrsInstanceGroupIds = a}) . _Default . _Coerce

-- | -- | The response status code.
aigrsResponseStatus :: Lens' AddInstanceGroupsResponse Int
aigrsResponseStatus = lens _aigrsResponseStatus (\ s a -> s{_aigrsResponseStatus = a})

instance NFData AddInstanceGroupsResponse where
