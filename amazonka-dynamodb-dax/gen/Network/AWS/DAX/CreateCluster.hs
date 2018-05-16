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
-- Module      : Network.AWS.DAX.CreateCluster
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a DAX cluster. All nodes in the cluster run the same DAX caching software.
--
--
module Network.AWS.DAX.CreateCluster
    (
    -- * Creating a Request
      createCluster
    , CreateCluster
    -- * Request Lenses
    , ccSecurityGroupIds
    , ccSubnetGroupName
    , ccPreferredMaintenanceWindow
    , ccAvailabilityZones
    , ccDescription
    , ccNotificationTopicARN
    , ccTags
    , ccParameterGroupName
    , ccClusterName
    , ccNodeType
    , ccReplicationFactor
    , ccIAMRoleARN

    -- * Destructuring the Response
    , createClusterResponse
    , CreateClusterResponse
    -- * Response Lenses
    , ccrsCluster
    , ccrsResponseStatus
    ) where

import Network.AWS.DAX.Types
import Network.AWS.DAX.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createCluster' smart constructor.
data CreateCluster = CreateCluster'
  { _ccSecurityGroupIds           :: !(Maybe [Text])
  , _ccSubnetGroupName            :: !(Maybe Text)
  , _ccPreferredMaintenanceWindow :: !(Maybe Text)
  , _ccAvailabilityZones          :: !(Maybe [Text])
  , _ccDescription                :: !(Maybe Text)
  , _ccNotificationTopicARN       :: !(Maybe Text)
  , _ccTags                       :: !(Maybe [Tag])
  , _ccParameterGroupName         :: !(Maybe Text)
  , _ccClusterName                :: !Text
  , _ccNodeType                   :: !Text
  , _ccReplicationFactor          :: !Int
  , _ccIAMRoleARN                 :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccSecurityGroupIds' - A list of security group IDs to be assigned to each node in the DAX cluster. (Each of the security group ID is system-generated.) If this parameter is not specified, DAX assigns the default VPC security group to each node.
--
-- * 'ccSubnetGroupName' - The name of the subnet group to be used for the replication group. /Important:/ DAX clusters can only run in an Amazon VPC environment. All of the subnets that you specify in a subnet group must exist in the same VPC.
--
-- * 'ccPreferredMaintenanceWindow' - Specifies the weekly time range during which maintenance on the DAX cluster is performed. It is specified as a range in the format ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window is a 60 minute period. Valid values for @ddd@ are:     * @sun@      * @mon@      * @tue@      * @wed@      * @thu@      * @fri@      * @sat@  Example: @sun:05:00-sun:09:00@
--
-- * 'ccAvailabilityZones' - The Availability Zones (AZs) in which the cluster nodes will be created. All nodes belonging to the cluster are placed in these Availability Zones. Use this parameter if you want to distribute the nodes across multiple AZs.
--
-- * 'ccDescription' - A description of the cluster.
--
-- * 'ccNotificationTopicARN' - The Amazon Resource Name (ARN) of the Amazon SNS topic to which notifications will be sent.
--
-- * 'ccTags' - A set of tags to associate with the DAX cluster.
--
-- * 'ccParameterGroupName' - The parameter group to be associated with the DAX cluster.
--
-- * 'ccClusterName' - The cluster identifier. This parameter is stored as a lowercase string. __Constraints:__      * A name must contain from 1 to 20 alphanumeric characters or hyphens.     * The first character must be a letter.     * A name cannot end with a hyphen or contain two consecutive hyphens.
--
-- * 'ccNodeType' - The compute and memory capacity of the nodes in the cluster.
--
-- * 'ccReplicationFactor' - The number of nodes in the DAX cluster. A replication factor of 1 will create a single-node cluster, without any read replicas. For additional fault tolerance, you can create a multiple node cluster with one or more read replicas. To do this, set /ReplicationFactor/ to 2 or more.
--
-- * 'ccIAMRoleARN' - A valid Amazon Resource Name (ARN) that identifies an IAM role. At runtime, DAX will assume this role and use the role's permissions to access DynamoDB on your behalf.
createCluster
    :: Text -- ^ 'ccClusterName'
    -> Text -- ^ 'ccNodeType'
    -> Int -- ^ 'ccReplicationFactor'
    -> Text -- ^ 'ccIAMRoleARN'
    -> CreateCluster
createCluster pClusterName_ pNodeType_ pReplicationFactor_ pIAMRoleARN_ =
  CreateCluster'
    { _ccSecurityGroupIds = Nothing
    , _ccSubnetGroupName = Nothing
    , _ccPreferredMaintenanceWindow = Nothing
    , _ccAvailabilityZones = Nothing
    , _ccDescription = Nothing
    , _ccNotificationTopicARN = Nothing
    , _ccTags = Nothing
    , _ccParameterGroupName = Nothing
    , _ccClusterName = pClusterName_
    , _ccNodeType = pNodeType_
    , _ccReplicationFactor = pReplicationFactor_
    , _ccIAMRoleARN = pIAMRoleARN_
    }


-- | A list of security group IDs to be assigned to each node in the DAX cluster. (Each of the security group ID is system-generated.) If this parameter is not specified, DAX assigns the default VPC security group to each node.
ccSecurityGroupIds :: Lens' CreateCluster [Text]
ccSecurityGroupIds = lens _ccSecurityGroupIds (\ s a -> s{_ccSecurityGroupIds = a}) . _Default . _Coerce

-- | The name of the subnet group to be used for the replication group. /Important:/ DAX clusters can only run in an Amazon VPC environment. All of the subnets that you specify in a subnet group must exist in the same VPC.
ccSubnetGroupName :: Lens' CreateCluster (Maybe Text)
ccSubnetGroupName = lens _ccSubnetGroupName (\ s a -> s{_ccSubnetGroupName = a})

-- | Specifies the weekly time range during which maintenance on the DAX cluster is performed. It is specified as a range in the format ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window is a 60 minute period. Valid values for @ddd@ are:     * @sun@      * @mon@      * @tue@      * @wed@      * @thu@      * @fri@      * @sat@  Example: @sun:05:00-sun:09:00@
ccPreferredMaintenanceWindow :: Lens' CreateCluster (Maybe Text)
ccPreferredMaintenanceWindow = lens _ccPreferredMaintenanceWindow (\ s a -> s{_ccPreferredMaintenanceWindow = a})

-- | The Availability Zones (AZs) in which the cluster nodes will be created. All nodes belonging to the cluster are placed in these Availability Zones. Use this parameter if you want to distribute the nodes across multiple AZs.
ccAvailabilityZones :: Lens' CreateCluster [Text]
ccAvailabilityZones = lens _ccAvailabilityZones (\ s a -> s{_ccAvailabilityZones = a}) . _Default . _Coerce

-- | A description of the cluster.
ccDescription :: Lens' CreateCluster (Maybe Text)
ccDescription = lens _ccDescription (\ s a -> s{_ccDescription = a})

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which notifications will be sent.
ccNotificationTopicARN :: Lens' CreateCluster (Maybe Text)
ccNotificationTopicARN = lens _ccNotificationTopicARN (\ s a -> s{_ccNotificationTopicARN = a})

-- | A set of tags to associate with the DAX cluster.
ccTags :: Lens' CreateCluster [Tag]
ccTags = lens _ccTags (\ s a -> s{_ccTags = a}) . _Default . _Coerce

-- | The parameter group to be associated with the DAX cluster.
ccParameterGroupName :: Lens' CreateCluster (Maybe Text)
ccParameterGroupName = lens _ccParameterGroupName (\ s a -> s{_ccParameterGroupName = a})

-- | The cluster identifier. This parameter is stored as a lowercase string. __Constraints:__      * A name must contain from 1 to 20 alphanumeric characters or hyphens.     * The first character must be a letter.     * A name cannot end with a hyphen or contain two consecutive hyphens.
ccClusterName :: Lens' CreateCluster Text
ccClusterName = lens _ccClusterName (\ s a -> s{_ccClusterName = a})

-- | The compute and memory capacity of the nodes in the cluster.
ccNodeType :: Lens' CreateCluster Text
ccNodeType = lens _ccNodeType (\ s a -> s{_ccNodeType = a})

-- | The number of nodes in the DAX cluster. A replication factor of 1 will create a single-node cluster, without any read replicas. For additional fault tolerance, you can create a multiple node cluster with one or more read replicas. To do this, set /ReplicationFactor/ to 2 or more.
ccReplicationFactor :: Lens' CreateCluster Int
ccReplicationFactor = lens _ccReplicationFactor (\ s a -> s{_ccReplicationFactor = a})

-- | A valid Amazon Resource Name (ARN) that identifies an IAM role. At runtime, DAX will assume this role and use the role's permissions to access DynamoDB on your behalf.
ccIAMRoleARN :: Lens' CreateCluster Text
ccIAMRoleARN = lens _ccIAMRoleARN (\ s a -> s{_ccIAMRoleARN = a})

instance AWSRequest CreateCluster where
        type Rs CreateCluster = CreateClusterResponse
        request = postJSON dax
        response
          = receiveJSON
              (\ s h x ->
                 CreateClusterResponse' <$>
                   (x .?> "Cluster") <*> (pure (fromEnum s)))

instance Hashable CreateCluster where

instance NFData CreateCluster where

instance ToHeaders CreateCluster where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDAXV3.CreateCluster" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateCluster where
        toJSON CreateCluster'{..}
          = object
              (catMaybes
                 [("SecurityGroupIds" .=) <$> _ccSecurityGroupIds,
                  ("SubnetGroupName" .=) <$> _ccSubnetGroupName,
                  ("PreferredMaintenanceWindow" .=) <$>
                    _ccPreferredMaintenanceWindow,
                  ("AvailabilityZones" .=) <$> _ccAvailabilityZones,
                  ("Description" .=) <$> _ccDescription,
                  ("NotificationTopicArn" .=) <$>
                    _ccNotificationTopicARN,
                  ("Tags" .=) <$> _ccTags,
                  ("ParameterGroupName" .=) <$> _ccParameterGroupName,
                  Just ("ClusterName" .= _ccClusterName),
                  Just ("NodeType" .= _ccNodeType),
                  Just ("ReplicationFactor" .= _ccReplicationFactor),
                  Just ("IamRoleArn" .= _ccIAMRoleARN)])

instance ToPath CreateCluster where
        toPath = const "/"

instance ToQuery CreateCluster where
        toQuery = const mempty

-- | /See:/ 'createClusterResponse' smart constructor.
data CreateClusterResponse = CreateClusterResponse'
  { _ccrsCluster        :: !(Maybe Cluster)
  , _ccrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccrsCluster' - A description of the DAX cluster that you have created.
--
-- * 'ccrsResponseStatus' - -- | The response status code.
createClusterResponse
    :: Int -- ^ 'ccrsResponseStatus'
    -> CreateClusterResponse
createClusterResponse pResponseStatus_ =
  CreateClusterResponse'
    {_ccrsCluster = Nothing, _ccrsResponseStatus = pResponseStatus_}


-- | A description of the DAX cluster that you have created.
ccrsCluster :: Lens' CreateClusterResponse (Maybe Cluster)
ccrsCluster = lens _ccrsCluster (\ s a -> s{_ccrsCluster = a})

-- | -- | The response status code.
ccrsResponseStatus :: Lens' CreateClusterResponse Int
ccrsResponseStatus = lens _ccrsResponseStatus (\ s a -> s{_ccrsResponseStatus = a})

instance NFData CreateClusterResponse where
