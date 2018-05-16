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
-- Module      : Network.AWS.DAX.UpdateCluster
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings for a DAX cluster. You can use this action to change one or more cluster configuration parameters by specifying the parameters and the new values.
--
--
module Network.AWS.DAX.UpdateCluster
    (
    -- * Creating a Request
      updateCluster
    , UpdateCluster
    -- * Request Lenses
    , ucSecurityGroupIds
    , ucPreferredMaintenanceWindow
    , ucNotificationTopicStatus
    , ucDescription
    , ucNotificationTopicARN
    , ucParameterGroupName
    , ucClusterName

    -- * Destructuring the Response
    , updateClusterResponse
    , UpdateClusterResponse
    -- * Response Lenses
    , ucrsCluster
    , ucrsResponseStatus
    ) where

import Network.AWS.DAX.Types
import Network.AWS.DAX.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateCluster' smart constructor.
data UpdateCluster = UpdateCluster'
  { _ucSecurityGroupIds           :: !(Maybe [Text])
  , _ucPreferredMaintenanceWindow :: !(Maybe Text)
  , _ucNotificationTopicStatus    :: !(Maybe Text)
  , _ucDescription                :: !(Maybe Text)
  , _ucNotificationTopicARN       :: !(Maybe Text)
  , _ucParameterGroupName         :: !(Maybe Text)
  , _ucClusterName                :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucSecurityGroupIds' - A list of user-specified security group IDs to be assigned to each node in the DAX cluster. If this parameter is not specified, DAX assigns the default VPC security group to each node.
--
-- * 'ucPreferredMaintenanceWindow' - A range of time when maintenance of DAX cluster software will be performed. For example: @sun:01:00-sun:09:00@ . Cluster maintenance normally takes less than 30 minutes, and is performed automatically within the maintenance window.
--
-- * 'ucNotificationTopicStatus' - The current state of the topic.
--
-- * 'ucDescription' - A description of the changes being made to the cluster.
--
-- * 'ucNotificationTopicARN' - The Amazon Resource Name (ARN) that identifies the topic.
--
-- * 'ucParameterGroupName' - The name of a parameter group for this cluster.
--
-- * 'ucClusterName' - The name of the DAX cluster to be modified.
updateCluster
    :: Text -- ^ 'ucClusterName'
    -> UpdateCluster
updateCluster pClusterName_ =
  UpdateCluster'
    { _ucSecurityGroupIds = Nothing
    , _ucPreferredMaintenanceWindow = Nothing
    , _ucNotificationTopicStatus = Nothing
    , _ucDescription = Nothing
    , _ucNotificationTopicARN = Nothing
    , _ucParameterGroupName = Nothing
    , _ucClusterName = pClusterName_
    }


-- | A list of user-specified security group IDs to be assigned to each node in the DAX cluster. If this parameter is not specified, DAX assigns the default VPC security group to each node.
ucSecurityGroupIds :: Lens' UpdateCluster [Text]
ucSecurityGroupIds = lens _ucSecurityGroupIds (\ s a -> s{_ucSecurityGroupIds = a}) . _Default . _Coerce

-- | A range of time when maintenance of DAX cluster software will be performed. For example: @sun:01:00-sun:09:00@ . Cluster maintenance normally takes less than 30 minutes, and is performed automatically within the maintenance window.
ucPreferredMaintenanceWindow :: Lens' UpdateCluster (Maybe Text)
ucPreferredMaintenanceWindow = lens _ucPreferredMaintenanceWindow (\ s a -> s{_ucPreferredMaintenanceWindow = a})

-- | The current state of the topic.
ucNotificationTopicStatus :: Lens' UpdateCluster (Maybe Text)
ucNotificationTopicStatus = lens _ucNotificationTopicStatus (\ s a -> s{_ucNotificationTopicStatus = a})

-- | A description of the changes being made to the cluster.
ucDescription :: Lens' UpdateCluster (Maybe Text)
ucDescription = lens _ucDescription (\ s a -> s{_ucDescription = a})

-- | The Amazon Resource Name (ARN) that identifies the topic.
ucNotificationTopicARN :: Lens' UpdateCluster (Maybe Text)
ucNotificationTopicARN = lens _ucNotificationTopicARN (\ s a -> s{_ucNotificationTopicARN = a})

-- | The name of a parameter group for this cluster.
ucParameterGroupName :: Lens' UpdateCluster (Maybe Text)
ucParameterGroupName = lens _ucParameterGroupName (\ s a -> s{_ucParameterGroupName = a})

-- | The name of the DAX cluster to be modified.
ucClusterName :: Lens' UpdateCluster Text
ucClusterName = lens _ucClusterName (\ s a -> s{_ucClusterName = a})

instance AWSRequest UpdateCluster where
        type Rs UpdateCluster = UpdateClusterResponse
        request = postJSON dax
        response
          = receiveJSON
              (\ s h x ->
                 UpdateClusterResponse' <$>
                   (x .?> "Cluster") <*> (pure (fromEnum s)))

instance Hashable UpdateCluster where

instance NFData UpdateCluster where

instance ToHeaders UpdateCluster where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDAXV3.UpdateCluster" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateCluster where
        toJSON UpdateCluster'{..}
          = object
              (catMaybes
                 [("SecurityGroupIds" .=) <$> _ucSecurityGroupIds,
                  ("PreferredMaintenanceWindow" .=) <$>
                    _ucPreferredMaintenanceWindow,
                  ("NotificationTopicStatus" .=) <$>
                    _ucNotificationTopicStatus,
                  ("Description" .=) <$> _ucDescription,
                  ("NotificationTopicArn" .=) <$>
                    _ucNotificationTopicARN,
                  ("ParameterGroupName" .=) <$> _ucParameterGroupName,
                  Just ("ClusterName" .= _ucClusterName)])

instance ToPath UpdateCluster where
        toPath = const "/"

instance ToQuery UpdateCluster where
        toQuery = const mempty

-- | /See:/ 'updateClusterResponse' smart constructor.
data UpdateClusterResponse = UpdateClusterResponse'
  { _ucrsCluster        :: !(Maybe Cluster)
  , _ucrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucrsCluster' - A description of the DAX cluster, after it has been modified.
--
-- * 'ucrsResponseStatus' - -- | The response status code.
updateClusterResponse
    :: Int -- ^ 'ucrsResponseStatus'
    -> UpdateClusterResponse
updateClusterResponse pResponseStatus_ =
  UpdateClusterResponse'
    {_ucrsCluster = Nothing, _ucrsResponseStatus = pResponseStatus_}


-- | A description of the DAX cluster, after it has been modified.
ucrsCluster :: Lens' UpdateClusterResponse (Maybe Cluster)
ucrsCluster = lens _ucrsCluster (\ s a -> s{_ucrsCluster = a})

-- | -- | The response status code.
ucrsResponseStatus :: Lens' UpdateClusterResponse Int
ucrsResponseStatus = lens _ucrsResponseStatus (\ s a -> s{_ucrsResponseStatus = a})

instance NFData UpdateClusterResponse where
