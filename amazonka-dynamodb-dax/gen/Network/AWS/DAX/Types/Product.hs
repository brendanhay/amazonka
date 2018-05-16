{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DAX.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DAX.Types.Product where

import Network.AWS.DAX.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains all of the attributes of a specific DAX cluster.
--
--
--
-- /See:/ 'cluster' smart constructor.
data Cluster = Cluster'
  { _cStatus                     :: !(Maybe Text)
  , _cIAMRoleARN                 :: !(Maybe Text)
  , _cClusterARN                 :: !(Maybe Text)
  , _cActiveNodes                :: !(Maybe Int)
  , _cSecurityGroups             :: !(Maybe [SecurityGroupMembership])
  , _cNotificationConfiguration  :: !(Maybe NotificationConfiguration)
  , _cNodeIdsToRemove            :: !(Maybe [Text])
  , _cTotalNodes                 :: !(Maybe Int)
  , _cPreferredMaintenanceWindow :: !(Maybe Text)
  , _cSubnetGroup                :: !(Maybe Text)
  , _cClusterName                :: !(Maybe Text)
  , _cNodeType                   :: !(Maybe Text)
  , _cNodes                      :: !(Maybe [Node])
  , _cClusterDiscoveryEndpoint   :: !(Maybe Endpoint)
  , _cDescription                :: !(Maybe Text)
  , _cParameterGroup             :: !(Maybe ParameterGroupStatus)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Cluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cStatus' - The current status of the cluster.
--
-- * 'cIAMRoleARN' - A valid Amazon Resource Name (ARN) that identifies an IAM role. At runtime, DAX will assume this role and use the role's permissions to access DynamoDB on your behalf.
--
-- * 'cClusterARN' - The Amazon Resource Name (ARN) that uniquely identifies the cluster.
--
-- * 'cActiveNodes' - The number of nodes in the cluster that are active (i.e., capable of serving requests).
--
-- * 'cSecurityGroups' - A list of security groups, and the status of each, for the nodes in the cluster.
--
-- * 'cNotificationConfiguration' - Describes a notification topic and its status. Notification topics are used for publishing DAX events to subscribers using Amazon Simple Notification Service (SNS).
--
-- * 'cNodeIdsToRemove' - A list of nodes to be removed from the cluster.
--
-- * 'cTotalNodes' - The total number of nodes in the cluster.
--
-- * 'cPreferredMaintenanceWindow' - A range of time when maintenance of DAX cluster software will be performed. For example: @sun:01:00-sun:09:00@ . Cluster maintenance normally takes less than 30 minutes, and is performed automatically within the maintenance window.
--
-- * 'cSubnetGroup' - The subnet group where the DAX cluster is running.
--
-- * 'cClusterName' - The name of the DAX cluster.
--
-- * 'cNodeType' - The node type for the nodes in the cluster. (All nodes in a DAX cluster are of the same type.)
--
-- * 'cNodes' - A list of nodes that are currently in the cluster.
--
-- * 'cClusterDiscoveryEndpoint' - The configuration endpoint for this DAX cluster, consisting of a DNS name and a port number. Client applications can specify this endpoint, rather than an individual node endpoint, and allow the DAX client software to intelligently route requests and responses to nodes in the DAX cluster.
--
-- * 'cDescription' - The description of the cluster.
--
-- * 'cParameterGroup' - The parameter group being used by nodes in the cluster.
cluster
    :: Cluster
cluster =
  Cluster'
    { _cStatus = Nothing
    , _cIAMRoleARN = Nothing
    , _cClusterARN = Nothing
    , _cActiveNodes = Nothing
    , _cSecurityGroups = Nothing
    , _cNotificationConfiguration = Nothing
    , _cNodeIdsToRemove = Nothing
    , _cTotalNodes = Nothing
    , _cPreferredMaintenanceWindow = Nothing
    , _cSubnetGroup = Nothing
    , _cClusterName = Nothing
    , _cNodeType = Nothing
    , _cNodes = Nothing
    , _cClusterDiscoveryEndpoint = Nothing
    , _cDescription = Nothing
    , _cParameterGroup = Nothing
    }


-- | The current status of the cluster.
cStatus :: Lens' Cluster (Maybe Text)
cStatus = lens _cStatus (\ s a -> s{_cStatus = a})

-- | A valid Amazon Resource Name (ARN) that identifies an IAM role. At runtime, DAX will assume this role and use the role's permissions to access DynamoDB on your behalf.
cIAMRoleARN :: Lens' Cluster (Maybe Text)
cIAMRoleARN = lens _cIAMRoleARN (\ s a -> s{_cIAMRoleARN = a})

-- | The Amazon Resource Name (ARN) that uniquely identifies the cluster.
cClusterARN :: Lens' Cluster (Maybe Text)
cClusterARN = lens _cClusterARN (\ s a -> s{_cClusterARN = a})

-- | The number of nodes in the cluster that are active (i.e., capable of serving requests).
cActiveNodes :: Lens' Cluster (Maybe Int)
cActiveNodes = lens _cActiveNodes (\ s a -> s{_cActiveNodes = a})

-- | A list of security groups, and the status of each, for the nodes in the cluster.
cSecurityGroups :: Lens' Cluster [SecurityGroupMembership]
cSecurityGroups = lens _cSecurityGroups (\ s a -> s{_cSecurityGroups = a}) . _Default . _Coerce

-- | Describes a notification topic and its status. Notification topics are used for publishing DAX events to subscribers using Amazon Simple Notification Service (SNS).
cNotificationConfiguration :: Lens' Cluster (Maybe NotificationConfiguration)
cNotificationConfiguration = lens _cNotificationConfiguration (\ s a -> s{_cNotificationConfiguration = a})

-- | A list of nodes to be removed from the cluster.
cNodeIdsToRemove :: Lens' Cluster [Text]
cNodeIdsToRemove = lens _cNodeIdsToRemove (\ s a -> s{_cNodeIdsToRemove = a}) . _Default . _Coerce

-- | The total number of nodes in the cluster.
cTotalNodes :: Lens' Cluster (Maybe Int)
cTotalNodes = lens _cTotalNodes (\ s a -> s{_cTotalNodes = a})

-- | A range of time when maintenance of DAX cluster software will be performed. For example: @sun:01:00-sun:09:00@ . Cluster maintenance normally takes less than 30 minutes, and is performed automatically within the maintenance window.
cPreferredMaintenanceWindow :: Lens' Cluster (Maybe Text)
cPreferredMaintenanceWindow = lens _cPreferredMaintenanceWindow (\ s a -> s{_cPreferredMaintenanceWindow = a})

-- | The subnet group where the DAX cluster is running.
cSubnetGroup :: Lens' Cluster (Maybe Text)
cSubnetGroup = lens _cSubnetGroup (\ s a -> s{_cSubnetGroup = a})

-- | The name of the DAX cluster.
cClusterName :: Lens' Cluster (Maybe Text)
cClusterName = lens _cClusterName (\ s a -> s{_cClusterName = a})

-- | The node type for the nodes in the cluster. (All nodes in a DAX cluster are of the same type.)
cNodeType :: Lens' Cluster (Maybe Text)
cNodeType = lens _cNodeType (\ s a -> s{_cNodeType = a})

-- | A list of nodes that are currently in the cluster.
cNodes :: Lens' Cluster [Node]
cNodes = lens _cNodes (\ s a -> s{_cNodes = a}) . _Default . _Coerce

-- | The configuration endpoint for this DAX cluster, consisting of a DNS name and a port number. Client applications can specify this endpoint, rather than an individual node endpoint, and allow the DAX client software to intelligently route requests and responses to nodes in the DAX cluster.
cClusterDiscoveryEndpoint :: Lens' Cluster (Maybe Endpoint)
cClusterDiscoveryEndpoint = lens _cClusterDiscoveryEndpoint (\ s a -> s{_cClusterDiscoveryEndpoint = a})

-- | The description of the cluster.
cDescription :: Lens' Cluster (Maybe Text)
cDescription = lens _cDescription (\ s a -> s{_cDescription = a})

-- | The parameter group being used by nodes in the cluster.
cParameterGroup :: Lens' Cluster (Maybe ParameterGroupStatus)
cParameterGroup = lens _cParameterGroup (\ s a -> s{_cParameterGroup = a})

instance FromJSON Cluster where
        parseJSON
          = withObject "Cluster"
              (\ x ->
                 Cluster' <$>
                   (x .:? "Status") <*> (x .:? "IamRoleArn") <*>
                     (x .:? "ClusterArn")
                     <*> (x .:? "ActiveNodes")
                     <*> (x .:? "SecurityGroups" .!= mempty)
                     <*> (x .:? "NotificationConfiguration")
                     <*> (x .:? "NodeIdsToRemove" .!= mempty)
                     <*> (x .:? "TotalNodes")
                     <*> (x .:? "PreferredMaintenanceWindow")
                     <*> (x .:? "SubnetGroup")
                     <*> (x .:? "ClusterName")
                     <*> (x .:? "NodeType")
                     <*> (x .:? "Nodes" .!= mempty)
                     <*> (x .:? "ClusterDiscoveryEndpoint")
                     <*> (x .:? "Description")
                     <*> (x .:? "ParameterGroup"))

instance Hashable Cluster where

instance NFData Cluster where

-- | Represents the information required for client programs to connect to the configuration endpoint for a DAX cluster, or to an individual node within the cluster.
--
--
--
-- /See:/ 'endpoint' smart constructor.
data Endpoint = Endpoint'
  { _eAddress :: !(Maybe Text)
  , _ePort    :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Endpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eAddress' - The DNS hostname of the endpoint.
--
-- * 'ePort' - The port number that applications should use to connect to the endpoint.
endpoint
    :: Endpoint
endpoint = Endpoint' {_eAddress = Nothing, _ePort = Nothing}


-- | The DNS hostname of the endpoint.
eAddress :: Lens' Endpoint (Maybe Text)
eAddress = lens _eAddress (\ s a -> s{_eAddress = a})

-- | The port number that applications should use to connect to the endpoint.
ePort :: Lens' Endpoint (Maybe Int)
ePort = lens _ePort (\ s a -> s{_ePort = a})

instance FromJSON Endpoint where
        parseJSON
          = withObject "Endpoint"
              (\ x ->
                 Endpoint' <$> (x .:? "Address") <*> (x .:? "Port"))

instance Hashable Endpoint where

instance NFData Endpoint where

-- | Represents a single occurrence of something interesting within the system. Some examples of events are creating a DAX cluster, adding or removing a node, or rebooting a node.
--
--
--
-- /See:/ 'event' smart constructor.
data Event = Event'
  { _eSourceName :: !(Maybe Text)
  , _eSourceType :: !(Maybe SourceType)
  , _eDate       :: !(Maybe POSIX)
  , _eMessage    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Event' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eSourceName' - The source of the event. For example, if the event occurred at the node level, the source would be the node ID.
--
-- * 'eSourceType' - Specifies the origin of this event - a cluster, a parameter group, a node ID, etc.
--
-- * 'eDate' - The date and time when the event occurred.
--
-- * 'eMessage' - A user-defined message associated with the event.
event
    :: Event
event =
  Event'
    { _eSourceName = Nothing
    , _eSourceType = Nothing
    , _eDate = Nothing
    , _eMessage = Nothing
    }


-- | The source of the event. For example, if the event occurred at the node level, the source would be the node ID.
eSourceName :: Lens' Event (Maybe Text)
eSourceName = lens _eSourceName (\ s a -> s{_eSourceName = a})

-- | Specifies the origin of this event - a cluster, a parameter group, a node ID, etc.
eSourceType :: Lens' Event (Maybe SourceType)
eSourceType = lens _eSourceType (\ s a -> s{_eSourceType = a})

-- | The date and time when the event occurred.
eDate :: Lens' Event (Maybe UTCTime)
eDate = lens _eDate (\ s a -> s{_eDate = a}) . mapping _Time

-- | A user-defined message associated with the event.
eMessage :: Lens' Event (Maybe Text)
eMessage = lens _eMessage (\ s a -> s{_eMessage = a})

instance FromJSON Event where
        parseJSON
          = withObject "Event"
              (\ x ->
                 Event' <$>
                   (x .:? "SourceName") <*> (x .:? "SourceType") <*>
                     (x .:? "Date")
                     <*> (x .:? "Message"))

instance Hashable Event where

instance NFData Event where

-- | Represents an individual node within a DAX cluster.
--
--
--
-- /See:/ 'node' smart constructor.
data Node = Node'
  { _nNodeStatus           :: !(Maybe Text)
  , _nParameterGroupStatus :: !(Maybe Text)
  , _nAvailabilityZone     :: !(Maybe Text)
  , _nNodeId               :: !(Maybe Text)
  , _nEndpoint             :: !(Maybe Endpoint)
  , _nNodeCreateTime       :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Node' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nNodeStatus' - The current status of the node. For example: @available@ .
--
-- * 'nParameterGroupStatus' - The status of the parameter group associated with this node. For example, @in-sync@ .
--
-- * 'nAvailabilityZone' - The Availability Zone (AZ) in which the node has been deployed.
--
-- * 'nNodeId' - A system-generated identifier for the node.
--
-- * 'nEndpoint' - The endpoint for the node, consisting of a DNS name and a port number. Client applications can connect directly to a node endpoint, if desired (as an alternative to allowing DAX client software to intelligently route requests and responses to nodes in the DAX cluster.
--
-- * 'nNodeCreateTime' - The date and time (in UNIX epoch format) when the node was launched.
node
    :: Node
node =
  Node'
    { _nNodeStatus = Nothing
    , _nParameterGroupStatus = Nothing
    , _nAvailabilityZone = Nothing
    , _nNodeId = Nothing
    , _nEndpoint = Nothing
    , _nNodeCreateTime = Nothing
    }


-- | The current status of the node. For example: @available@ .
nNodeStatus :: Lens' Node (Maybe Text)
nNodeStatus = lens _nNodeStatus (\ s a -> s{_nNodeStatus = a})

-- | The status of the parameter group associated with this node. For example, @in-sync@ .
nParameterGroupStatus :: Lens' Node (Maybe Text)
nParameterGroupStatus = lens _nParameterGroupStatus (\ s a -> s{_nParameterGroupStatus = a})

-- | The Availability Zone (AZ) in which the node has been deployed.
nAvailabilityZone :: Lens' Node (Maybe Text)
nAvailabilityZone = lens _nAvailabilityZone (\ s a -> s{_nAvailabilityZone = a})

-- | A system-generated identifier for the node.
nNodeId :: Lens' Node (Maybe Text)
nNodeId = lens _nNodeId (\ s a -> s{_nNodeId = a})

-- | The endpoint for the node, consisting of a DNS name and a port number. Client applications can connect directly to a node endpoint, if desired (as an alternative to allowing DAX client software to intelligently route requests and responses to nodes in the DAX cluster.
nEndpoint :: Lens' Node (Maybe Endpoint)
nEndpoint = lens _nEndpoint (\ s a -> s{_nEndpoint = a})

-- | The date and time (in UNIX epoch format) when the node was launched.
nNodeCreateTime :: Lens' Node (Maybe UTCTime)
nNodeCreateTime = lens _nNodeCreateTime (\ s a -> s{_nNodeCreateTime = a}) . mapping _Time

instance FromJSON Node where
        parseJSON
          = withObject "Node"
              (\ x ->
                 Node' <$>
                   (x .:? "NodeStatus") <*>
                     (x .:? "ParameterGroupStatus")
                     <*> (x .:? "AvailabilityZone")
                     <*> (x .:? "NodeId")
                     <*> (x .:? "Endpoint")
                     <*> (x .:? "NodeCreateTime"))

instance Hashable Node where

instance NFData Node where

-- | Represents a parameter value that is applicable to a particular node type.
--
--
--
-- /See:/ 'nodeTypeSpecificValue' smart constructor.
data NodeTypeSpecificValue = NodeTypeSpecificValue'
  { _ntsvValue    :: !(Maybe Text)
  , _ntsvNodeType :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NodeTypeSpecificValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ntsvValue' - The parameter value for this node type.
--
-- * 'ntsvNodeType' - A node type to which the parameter value applies.
nodeTypeSpecificValue
    :: NodeTypeSpecificValue
nodeTypeSpecificValue =
  NodeTypeSpecificValue' {_ntsvValue = Nothing, _ntsvNodeType = Nothing}


-- | The parameter value for this node type.
ntsvValue :: Lens' NodeTypeSpecificValue (Maybe Text)
ntsvValue = lens _ntsvValue (\ s a -> s{_ntsvValue = a})

-- | A node type to which the parameter value applies.
ntsvNodeType :: Lens' NodeTypeSpecificValue (Maybe Text)
ntsvNodeType = lens _ntsvNodeType (\ s a -> s{_ntsvNodeType = a})

instance FromJSON NodeTypeSpecificValue where
        parseJSON
          = withObject "NodeTypeSpecificValue"
              (\ x ->
                 NodeTypeSpecificValue' <$>
                   (x .:? "Value") <*> (x .:? "NodeType"))

instance Hashable NodeTypeSpecificValue where

instance NFData NodeTypeSpecificValue where

-- | Describes a notification topic and its status. Notification topics are used for publishing DAX events to subscribers using Amazon Simple Notification Service (SNS).
--
--
--
-- /See:/ 'notificationConfiguration' smart constructor.
data NotificationConfiguration = NotificationConfiguration'
  { _ncTopicStatus :: !(Maybe Text)
  , _ncTopicARN    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NotificationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncTopicStatus' - The current state of the topic.
--
-- * 'ncTopicARN' - The Amazon Resource Name (ARN) that identifies the topic.
notificationConfiguration
    :: NotificationConfiguration
notificationConfiguration =
  NotificationConfiguration' {_ncTopicStatus = Nothing, _ncTopicARN = Nothing}


-- | The current state of the topic.
ncTopicStatus :: Lens' NotificationConfiguration (Maybe Text)
ncTopicStatus = lens _ncTopicStatus (\ s a -> s{_ncTopicStatus = a})

-- | The Amazon Resource Name (ARN) that identifies the topic.
ncTopicARN :: Lens' NotificationConfiguration (Maybe Text)
ncTopicARN = lens _ncTopicARN (\ s a -> s{_ncTopicARN = a})

instance FromJSON NotificationConfiguration where
        parseJSON
          = withObject "NotificationConfiguration"
              (\ x ->
                 NotificationConfiguration' <$>
                   (x .:? "TopicStatus") <*> (x .:? "TopicArn"))

instance Hashable NotificationConfiguration where

instance NFData NotificationConfiguration where

-- | Describes an individual setting that controls some aspect of DAX behavior.
--
--
--
-- /See:/ 'parameter' smart constructor.
data Parameter = Parameter'
  { _pParameterValue         :: !(Maybe Text)
  , _pParameterType          :: !(Maybe ParameterType)
  , _pSource                 :: !(Maybe Text)
  , _pIsModifiable           :: !(Maybe IsModifiable)
  , _pDataType               :: !(Maybe Text)
  , _pNodeTypeSpecificValues :: !(Maybe [NodeTypeSpecificValue])
  , _pAllowedValues          :: !(Maybe Text)
  , _pParameterName          :: !(Maybe Text)
  , _pDescription            :: !(Maybe Text)
  , _pChangeType             :: !(Maybe ChangeType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Parameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pParameterValue' - The value for the parameter.
--
-- * 'pParameterType' - Determines whether the parameter can be applied to any nodes, or only nodes of a particular type.
--
-- * 'pSource' - How the parameter is defined. For example, @system@ denotes a system-defined parameter.
--
-- * 'pIsModifiable' - Whether the customer is allowed to modify the parameter.
--
-- * 'pDataType' - The data type of the parameter. For example, @integer@ :
--
-- * 'pNodeTypeSpecificValues' - A list of node types, and specific parameter values for each node.
--
-- * 'pAllowedValues' - A range of values within which the parameter can be set.
--
-- * 'pParameterName' - The name of the parameter.
--
-- * 'pDescription' - A description of the parameter
--
-- * 'pChangeType' - The conditions under which changes to this parameter can be applied. For example, @requires-reboot@ indicates that a new value for this parameter will only take effect if a node is rebooted.
parameter
    :: Parameter
parameter =
  Parameter'
    { _pParameterValue = Nothing
    , _pParameterType = Nothing
    , _pSource = Nothing
    , _pIsModifiable = Nothing
    , _pDataType = Nothing
    , _pNodeTypeSpecificValues = Nothing
    , _pAllowedValues = Nothing
    , _pParameterName = Nothing
    , _pDescription = Nothing
    , _pChangeType = Nothing
    }


-- | The value for the parameter.
pParameterValue :: Lens' Parameter (Maybe Text)
pParameterValue = lens _pParameterValue (\ s a -> s{_pParameterValue = a})

-- | Determines whether the parameter can be applied to any nodes, or only nodes of a particular type.
pParameterType :: Lens' Parameter (Maybe ParameterType)
pParameterType = lens _pParameterType (\ s a -> s{_pParameterType = a})

-- | How the parameter is defined. For example, @system@ denotes a system-defined parameter.
pSource :: Lens' Parameter (Maybe Text)
pSource = lens _pSource (\ s a -> s{_pSource = a})

-- | Whether the customer is allowed to modify the parameter.
pIsModifiable :: Lens' Parameter (Maybe IsModifiable)
pIsModifiable = lens _pIsModifiable (\ s a -> s{_pIsModifiable = a})

-- | The data type of the parameter. For example, @integer@ :
pDataType :: Lens' Parameter (Maybe Text)
pDataType = lens _pDataType (\ s a -> s{_pDataType = a})

-- | A list of node types, and specific parameter values for each node.
pNodeTypeSpecificValues :: Lens' Parameter [NodeTypeSpecificValue]
pNodeTypeSpecificValues = lens _pNodeTypeSpecificValues (\ s a -> s{_pNodeTypeSpecificValues = a}) . _Default . _Coerce

-- | A range of values within which the parameter can be set.
pAllowedValues :: Lens' Parameter (Maybe Text)
pAllowedValues = lens _pAllowedValues (\ s a -> s{_pAllowedValues = a})

-- | The name of the parameter.
pParameterName :: Lens' Parameter (Maybe Text)
pParameterName = lens _pParameterName (\ s a -> s{_pParameterName = a})

-- | A description of the parameter
pDescription :: Lens' Parameter (Maybe Text)
pDescription = lens _pDescription (\ s a -> s{_pDescription = a})

-- | The conditions under which changes to this parameter can be applied. For example, @requires-reboot@ indicates that a new value for this parameter will only take effect if a node is rebooted.
pChangeType :: Lens' Parameter (Maybe ChangeType)
pChangeType = lens _pChangeType (\ s a -> s{_pChangeType = a})

instance FromJSON Parameter where
        parseJSON
          = withObject "Parameter"
              (\ x ->
                 Parameter' <$>
                   (x .:? "ParameterValue") <*> (x .:? "ParameterType")
                     <*> (x .:? "Source")
                     <*> (x .:? "IsModifiable")
                     <*> (x .:? "DataType")
                     <*> (x .:? "NodeTypeSpecificValues" .!= mempty)
                     <*> (x .:? "AllowedValues")
                     <*> (x .:? "ParameterName")
                     <*> (x .:? "Description")
                     <*> (x .:? "ChangeType"))

instance Hashable Parameter where

instance NFData Parameter where

-- | A named set of parameters that are applied to all of the nodes in a DAX cluster.
--
--
--
-- /See:/ 'parameterGroup' smart constructor.
data ParameterGroup = ParameterGroup'
  { _pgDescription        :: !(Maybe Text)
  , _pgParameterGroupName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ParameterGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pgDescription' - A description of the parameter group.
--
-- * 'pgParameterGroupName' - The name of the parameter group.
parameterGroup
    :: ParameterGroup
parameterGroup =
  ParameterGroup' {_pgDescription = Nothing, _pgParameterGroupName = Nothing}


-- | A description of the parameter group.
pgDescription :: Lens' ParameterGroup (Maybe Text)
pgDescription = lens _pgDescription (\ s a -> s{_pgDescription = a})

-- | The name of the parameter group.
pgParameterGroupName :: Lens' ParameterGroup (Maybe Text)
pgParameterGroupName = lens _pgParameterGroupName (\ s a -> s{_pgParameterGroupName = a})

instance FromJSON ParameterGroup where
        parseJSON
          = withObject "ParameterGroup"
              (\ x ->
                 ParameterGroup' <$>
                   (x .:? "Description") <*>
                     (x .:? "ParameterGroupName"))

instance Hashable ParameterGroup where

instance NFData ParameterGroup where

-- | The status of a parameter group.
--
--
--
-- /See:/ 'parameterGroupStatus' smart constructor.
data ParameterGroupStatus = ParameterGroupStatus'
  { _pgsNodeIdsToReboot      :: !(Maybe [Text])
  , _pgsParameterApplyStatus :: !(Maybe Text)
  , _pgsParameterGroupName   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ParameterGroupStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pgsNodeIdsToReboot' - The node IDs of one or more nodes to be rebooted.
--
-- * 'pgsParameterApplyStatus' - The status of parameter updates.
--
-- * 'pgsParameterGroupName' - The name of the parameter group.
parameterGroupStatus
    :: ParameterGroupStatus
parameterGroupStatus =
  ParameterGroupStatus'
    { _pgsNodeIdsToReboot = Nothing
    , _pgsParameterApplyStatus = Nothing
    , _pgsParameterGroupName = Nothing
    }


-- | The node IDs of one or more nodes to be rebooted.
pgsNodeIdsToReboot :: Lens' ParameterGroupStatus [Text]
pgsNodeIdsToReboot = lens _pgsNodeIdsToReboot (\ s a -> s{_pgsNodeIdsToReboot = a}) . _Default . _Coerce

-- | The status of parameter updates.
pgsParameterApplyStatus :: Lens' ParameterGroupStatus (Maybe Text)
pgsParameterApplyStatus = lens _pgsParameterApplyStatus (\ s a -> s{_pgsParameterApplyStatus = a})

-- | The name of the parameter group.
pgsParameterGroupName :: Lens' ParameterGroupStatus (Maybe Text)
pgsParameterGroupName = lens _pgsParameterGroupName (\ s a -> s{_pgsParameterGroupName = a})

instance FromJSON ParameterGroupStatus where
        parseJSON
          = withObject "ParameterGroupStatus"
              (\ x ->
                 ParameterGroupStatus' <$>
                   (x .:? "NodeIdsToReboot" .!= mempty) <*>
                     (x .:? "ParameterApplyStatus")
                     <*> (x .:? "ParameterGroupName"))

instance Hashable ParameterGroupStatus where

instance NFData ParameterGroupStatus where

-- | An individual DAX parameter.
--
--
--
-- /See:/ 'parameterNameValue' smart constructor.
data ParameterNameValue = ParameterNameValue'
  { _pnvParameterValue :: !(Maybe Text)
  , _pnvParameterName  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ParameterNameValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pnvParameterValue' - The value of the parameter.
--
-- * 'pnvParameterName' - The name of the parameter.
parameterNameValue
    :: ParameterNameValue
parameterNameValue =
  ParameterNameValue'
    {_pnvParameterValue = Nothing, _pnvParameterName = Nothing}


-- | The value of the parameter.
pnvParameterValue :: Lens' ParameterNameValue (Maybe Text)
pnvParameterValue = lens _pnvParameterValue (\ s a -> s{_pnvParameterValue = a})

-- | The name of the parameter.
pnvParameterName :: Lens' ParameterNameValue (Maybe Text)
pnvParameterName = lens _pnvParameterName (\ s a -> s{_pnvParameterName = a})

instance Hashable ParameterNameValue where

instance NFData ParameterNameValue where

instance ToJSON ParameterNameValue where
        toJSON ParameterNameValue'{..}
          = object
              (catMaybes
                 [("ParameterValue" .=) <$> _pnvParameterValue,
                  ("ParameterName" .=) <$> _pnvParameterName])

-- | An individual VPC security group and its status.
--
--
--
-- /See:/ 'securityGroupMembership' smart constructor.
data SecurityGroupMembership = SecurityGroupMembership'
  { _sgmStatus                  :: !(Maybe Text)
  , _sgmSecurityGroupIdentifier :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SecurityGroupMembership' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgmStatus' - The status of this security group.
--
-- * 'sgmSecurityGroupIdentifier' - The unique ID for this security group.
securityGroupMembership
    :: SecurityGroupMembership
securityGroupMembership =
  SecurityGroupMembership'
    {_sgmStatus = Nothing, _sgmSecurityGroupIdentifier = Nothing}


-- | The status of this security group.
sgmStatus :: Lens' SecurityGroupMembership (Maybe Text)
sgmStatus = lens _sgmStatus (\ s a -> s{_sgmStatus = a})

-- | The unique ID for this security group.
sgmSecurityGroupIdentifier :: Lens' SecurityGroupMembership (Maybe Text)
sgmSecurityGroupIdentifier = lens _sgmSecurityGroupIdentifier (\ s a -> s{_sgmSecurityGroupIdentifier = a})

instance FromJSON SecurityGroupMembership where
        parseJSON
          = withObject "SecurityGroupMembership"
              (\ x ->
                 SecurityGroupMembership' <$>
                   (x .:? "Status") <*>
                     (x .:? "SecurityGroupIdentifier"))

instance Hashable SecurityGroupMembership where

instance NFData SecurityGroupMembership where

-- | Represents the subnet associated with a DAX cluster. This parameter refers to subnets defined in Amazon Virtual Private Cloud (Amazon VPC) and used with DAX.
--
--
--
-- /See:/ 'subnet' smart constructor.
data Subnet = Subnet'
  { _sSubnetIdentifier       :: !(Maybe Text)
  , _sSubnetAvailabilityZone :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Subnet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sSubnetIdentifier' - The system-assigned identifier for the subnet.
--
-- * 'sSubnetAvailabilityZone' - The Availability Zone (AZ) for subnet subnet.
subnet
    :: Subnet
subnet =
  Subnet' {_sSubnetIdentifier = Nothing, _sSubnetAvailabilityZone = Nothing}


-- | The system-assigned identifier for the subnet.
sSubnetIdentifier :: Lens' Subnet (Maybe Text)
sSubnetIdentifier = lens _sSubnetIdentifier (\ s a -> s{_sSubnetIdentifier = a})

-- | The Availability Zone (AZ) for subnet subnet.
sSubnetAvailabilityZone :: Lens' Subnet (Maybe Text)
sSubnetAvailabilityZone = lens _sSubnetAvailabilityZone (\ s a -> s{_sSubnetAvailabilityZone = a})

instance FromJSON Subnet where
        parseJSON
          = withObject "Subnet"
              (\ x ->
                 Subnet' <$>
                   (x .:? "SubnetIdentifier") <*>
                     (x .:? "SubnetAvailabilityZone"))

instance Hashable Subnet where

instance NFData Subnet where

-- | Represents the output of one of the following actions:
--
--
--     * /CreateSubnetGroup/
--
--     * /ModifySubnetGroup/
--
--
--
--
-- /See:/ 'subnetGroup' smart constructor.
data SubnetGroup = SubnetGroup'
  { _sgVPCId           :: !(Maybe Text)
  , _sgSubnets         :: !(Maybe [Subnet])
  , _sgSubnetGroupName :: !(Maybe Text)
  , _sgDescription     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SubnetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgVPCId' - The Amazon Virtual Private Cloud identifier (VPC ID) of the subnet group.
--
-- * 'sgSubnets' - A list of subnets associated with the subnet group.
--
-- * 'sgSubnetGroupName' - The name of the subnet group.
--
-- * 'sgDescription' - The description of the subnet group.
subnetGroup
    :: SubnetGroup
subnetGroup =
  SubnetGroup'
    { _sgVPCId = Nothing
    , _sgSubnets = Nothing
    , _sgSubnetGroupName = Nothing
    , _sgDescription = Nothing
    }


-- | The Amazon Virtual Private Cloud identifier (VPC ID) of the subnet group.
sgVPCId :: Lens' SubnetGroup (Maybe Text)
sgVPCId = lens _sgVPCId (\ s a -> s{_sgVPCId = a})

-- | A list of subnets associated with the subnet group.
sgSubnets :: Lens' SubnetGroup [Subnet]
sgSubnets = lens _sgSubnets (\ s a -> s{_sgSubnets = a}) . _Default . _Coerce

-- | The name of the subnet group.
sgSubnetGroupName :: Lens' SubnetGroup (Maybe Text)
sgSubnetGroupName = lens _sgSubnetGroupName (\ s a -> s{_sgSubnetGroupName = a})

-- | The description of the subnet group.
sgDescription :: Lens' SubnetGroup (Maybe Text)
sgDescription = lens _sgDescription (\ s a -> s{_sgDescription = a})

instance FromJSON SubnetGroup where
        parseJSON
          = withObject "SubnetGroup"
              (\ x ->
                 SubnetGroup' <$>
                   (x .:? "VpcId") <*> (x .:? "Subnets" .!= mempty) <*>
                     (x .:? "SubnetGroupName")
                     <*> (x .:? "Description"))

instance Hashable SubnetGroup where

instance NFData SubnetGroup where

-- | A description of a tag. Every tag is a key-value pair. You can add up to 50 tags to a single DAX cluster.
--
--
-- AWS-assigned tag names and values are automatically assigned the @aws:@ prefix, which the user cannot assign. AWS-assigned tag names do not count towards the tag limit of 50. User-assigned tag names have the prefix @user:@ .
--
-- You cannot backdate the application of a tag.
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag'
  { _tagValue :: !(Maybe Text)
  , _tagKey   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue' - The value of the tag. Tag values are case-sensitive and can be null.
--
-- * 'tagKey' - The key for the tag. Tag keys are case sensitive. Every DAX cluster can only have one tag with the same key. If you try to add an existing tag (same key), the existing tag value will be updated to the new value.
tag
    :: Tag
tag = Tag' {_tagValue = Nothing, _tagKey = Nothing}


-- | The value of the tag. Tag values are case-sensitive and can be null.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

-- | The key for the tag. Tag keys are case sensitive. Every DAX cluster can only have one tag with the same key. If you try to add an existing tag (same key), the existing tag value will be updated to the new value.
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

instance FromJSON Tag where
        parseJSON
          = withObject "Tag"
              (\ x -> Tag' <$> (x .:? "Value") <*> (x .:? "Key"))

instance Hashable Tag where

instance NFData Tag where

instance ToJSON Tag where
        toJSON Tag'{..}
          = object
              (catMaybes
                 [("Value" .=) <$> _tagValue, ("Key" .=) <$> _tagKey])
