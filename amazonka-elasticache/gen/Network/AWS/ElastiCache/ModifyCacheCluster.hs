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
-- Module      : Network.AWS.ElastiCache.ModifyCacheCluster
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The /ModifyCacheCluster/ action modifies the settings for a cache cluster. You can use this action to change one or more cluster configuration parameters by specifying the parameters and the new values.
module Network.AWS.ElastiCache.ModifyCacheCluster
    (
    -- * Creating a Request
      modifyCacheCluster
    , ModifyCacheCluster
    -- * Request Lenses
    , mccEngineVersion
    , mccCacheNodeType
    , mccSecurityGroupIds
    , mccAutoMinorVersionUpgrade
    , mccCacheParameterGroupName
    , mccSnapshotWindow
    , mccNewAvailabilityZones
    , mccPreferredMaintenanceWindow
    , mccCacheNodeIdsToRemove
    , mccSnapshotRetentionLimit
    , mccNotificationTopicStatus
    , mccAZMode
    , mccApplyImmediately
    , mccNotificationTopicARN
    , mccNumCacheNodes
    , mccCacheSecurityGroupNames
    , mccCacheClusterId

    -- * Destructuring the Response
    , modifyCacheClusterResponse
    , ModifyCacheClusterResponse
    -- * Response Lenses
    , mccrsCacheCluster
    , mccrsResponseStatus
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.ElastiCache.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /ModifyCacheCluster/ action.
--
-- /See:/ 'modifyCacheCluster' smart constructor.
data ModifyCacheCluster = ModifyCacheCluster'
    { _mccEngineVersion              :: !(Maybe Text)
    , _mccCacheNodeType              :: !(Maybe Text)
    , _mccSecurityGroupIds           :: !(Maybe [Text])
    , _mccAutoMinorVersionUpgrade    :: !(Maybe Bool)
    , _mccCacheParameterGroupName    :: !(Maybe Text)
    , _mccSnapshotWindow             :: !(Maybe Text)
    , _mccNewAvailabilityZones       :: !(Maybe [Text])
    , _mccPreferredMaintenanceWindow :: !(Maybe Text)
    , _mccCacheNodeIdsToRemove       :: !(Maybe [Text])
    , _mccSnapshotRetentionLimit     :: !(Maybe Int)
    , _mccNotificationTopicStatus    :: !(Maybe Text)
    , _mccAZMode                     :: !(Maybe AZMode)
    , _mccApplyImmediately           :: !(Maybe Bool)
    , _mccNotificationTopicARN       :: !(Maybe Text)
    , _mccNumCacheNodes              :: !(Maybe Int)
    , _mccCacheSecurityGroupNames    :: !(Maybe [Text])
    , _mccCacheClusterId             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ModifyCacheCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mccEngineVersion'
--
-- * 'mccCacheNodeType'
--
-- * 'mccSecurityGroupIds'
--
-- * 'mccAutoMinorVersionUpgrade'
--
-- * 'mccCacheParameterGroupName'
--
-- * 'mccSnapshotWindow'
--
-- * 'mccNewAvailabilityZones'
--
-- * 'mccPreferredMaintenanceWindow'
--
-- * 'mccCacheNodeIdsToRemove'
--
-- * 'mccSnapshotRetentionLimit'
--
-- * 'mccNotificationTopicStatus'
--
-- * 'mccAZMode'
--
-- * 'mccApplyImmediately'
--
-- * 'mccNotificationTopicARN'
--
-- * 'mccNumCacheNodes'
--
-- * 'mccCacheSecurityGroupNames'
--
-- * 'mccCacheClusterId'
modifyCacheCluster
    :: Text -- ^ 'mccCacheClusterId'
    -> ModifyCacheCluster
modifyCacheCluster pCacheClusterId_ =
    ModifyCacheCluster'
    { _mccEngineVersion = Nothing
    , _mccCacheNodeType = Nothing
    , _mccSecurityGroupIds = Nothing
    , _mccAutoMinorVersionUpgrade = Nothing
    , _mccCacheParameterGroupName = Nothing
    , _mccSnapshotWindow = Nothing
    , _mccNewAvailabilityZones = Nothing
    , _mccPreferredMaintenanceWindow = Nothing
    , _mccCacheNodeIdsToRemove = Nothing
    , _mccSnapshotRetentionLimit = Nothing
    , _mccNotificationTopicStatus = Nothing
    , _mccAZMode = Nothing
    , _mccApplyImmediately = Nothing
    , _mccNotificationTopicARN = Nothing
    , _mccNumCacheNodes = Nothing
    , _mccCacheSecurityGroupNames = Nothing
    , _mccCacheClusterId = pCacheClusterId_
    }

-- | The upgraded version of the cache engine to be run on the cache nodes.
--
-- __Important:__ You can upgrade to a newer engine version (see <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/SelectEngine.html#VersionManagement Selecting a Cache Engine and Version>), but you cannot downgrade to an earlier engine version. If you want to use an earlier engine version, you must delete the existing cache cluster and create it anew with the earlier engine version.
mccEngineVersion :: Lens' ModifyCacheCluster (Maybe Text)
mccEngineVersion = lens _mccEngineVersion (\ s a -> s{_mccEngineVersion = a});

-- | A valid cache node type that you want to scale this cache cluster to. The value of this parameter must be one of the /ScaleUpModifications/ values returned by the 'ListAllowedCacheNodeTypeModification' action.
mccCacheNodeType :: Lens' ModifyCacheCluster (Maybe Text)
mccCacheNodeType = lens _mccCacheNodeType (\ s a -> s{_mccCacheNodeType = a});

-- | Specifies the VPC Security Groups associated with the cache cluster.
--
-- This parameter can be used only with clusters that are created in an Amazon Virtual Private Cloud (VPC).
mccSecurityGroupIds :: Lens' ModifyCacheCluster [Text]
mccSecurityGroupIds = lens _mccSecurityGroupIds (\ s a -> s{_mccSecurityGroupIds = a}) . _Default . _Coerce;

-- | This parameter is currently disabled.
mccAutoMinorVersionUpgrade :: Lens' ModifyCacheCluster (Maybe Bool)
mccAutoMinorVersionUpgrade = lens _mccAutoMinorVersionUpgrade (\ s a -> s{_mccAutoMinorVersionUpgrade = a});

-- | The name of the cache parameter group to apply to this cache cluster. This change is asynchronously applied as soon as possible for parameters when the /ApplyImmediately/ parameter is specified as /true/ for this request.
mccCacheParameterGroupName :: Lens' ModifyCacheCluster (Maybe Text)
mccCacheParameterGroupName = lens _mccCacheParameterGroupName (\ s a -> s{_mccCacheParameterGroupName = a});

-- | The daily time range (in UTC) during which ElastiCache will begin taking a daily snapshot of your cache cluster.
mccSnapshotWindow :: Lens' ModifyCacheCluster (Maybe Text)
mccSnapshotWindow = lens _mccSnapshotWindow (\ s a -> s{_mccSnapshotWindow = a});

-- | The list of Availability Zones where the new Memcached cache nodes will be created.
--
-- This parameter is only valid when /NumCacheNodes/ in the request is greater than the sum of the number of active cache nodes and the number of cache nodes pending creation (which may be zero). The number of Availability Zones supplied in this list must match the cache nodes being added in this request.
--
-- This option is only supported on Memcached clusters.
--
-- Scenarios:
--
-- -   __Scenario 1:__ You have 3 active nodes and wish to add 2 nodes. Specify 'NumCacheNodes=5' (3 + 2) and optionally specify two Availability Zones for the two new nodes.
--
-- -   __Scenario 2:__ You have 3 active nodes and 2 nodes pending creation (from the scenario 1 call) and want to add 1 more node. Specify 'NumCacheNodes=6' ((3 + 2) + 1) and optionally specify an Availability Zone for the new node.
--
-- -   __Scenario 3:__ You want to cancel all pending actions. Specify 'NumCacheNodes=3' to cancel all pending actions.
--
-- The Availability Zone placement of nodes pending creation cannot be modified. If you wish to cancel any nodes pending creation, add 0 nodes by setting 'NumCacheNodes' to the number of current nodes.
--
-- If 'cross-az' is specified, existing Memcached nodes remain in their current Availability Zone. Only newly created nodes can be located in different Availability Zones. For guidance on how to move existing Memcached nodes to different Availability Zones, see the __Availability Zone Considerations__ section of <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheNode.Memcached.html Cache Node Considerations for Memcached>.
--
-- __Impact of new add\/remove requests upon pending requests__
--
-- -   Scenario-1
--
--     -   Pending Action: Delete
--
--     -   New Request: Delete
--
--     -   Result: The new delete, pending or immediate, replaces the pending delete.
--
-- -   Scenario-2
--
--     -   Pending Action: Delete
--
--     -   New Request: Create
--
--     -   Result: The new create, pending or immediate, replaces the pending delete.
--
-- -   Scenario-3
--
--     -   Pending Action: Create
--
--     -   New Request: Delete
--
--     -   Result: The new delete, pending or immediate, replaces the pending create.
--
-- -   Scenario-4
--
--     -   Pending Action: Create
--
--     -   New Request: Create
--
--     -   Result: The new create is added to the pending create.
--
--         __Important:__ If the new create request is __Apply Immediately - Yes__, all creates are performed immediately. If the new create request is __Apply Immediately - No__, all creates are pending.
--
-- Example:
--
-- 'NewAvailabilityZones.member.1=us-west-2a&amp;NewAvailabilityZones.member.2=us-west-2b&amp;NewAvailabilityZones.member.3=us-west-2c'
mccNewAvailabilityZones :: Lens' ModifyCacheCluster [Text]
mccNewAvailabilityZones = lens _mccNewAvailabilityZones (\ s a -> s{_mccNewAvailabilityZones = a}) . _Default . _Coerce;

-- | Specifies the weekly time range during which maintenance on the cache cluster is performed. It is specified as a range in the format ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window is a 60 minute period. Valid values for 'ddd' are:
--
-- -   'sun'
--
-- -   'mon'
--
-- -   'tue'
--
-- -   'wed'
--
-- -   'thu'
--
-- -   'fri'
--
-- -   'sat'
--
-- Example: 'sun:05:00-sun:09:00'
mccPreferredMaintenanceWindow :: Lens' ModifyCacheCluster (Maybe Text)
mccPreferredMaintenanceWindow = lens _mccPreferredMaintenanceWindow (\ s a -> s{_mccPreferredMaintenanceWindow = a});

-- | A list of cache node IDs to be removed. A node ID is a numeric identifier (0001, 0002, etc.). This parameter is only valid when /NumCacheNodes/ is less than the existing number of cache nodes. The number of cache node IDs supplied in this parameter must match the difference between the existing number of cache nodes in the cluster or pending cache nodes, whichever is greater, and the value of /NumCacheNodes/ in the request.
--
-- For example: If you have 3 active cache nodes, 7 pending cache nodes, and the number of cache nodes in this 'ModifyCacheCluser' call is 5, you must list 2 (7 - 5) cache node IDs to remove.
mccCacheNodeIdsToRemove :: Lens' ModifyCacheCluster [Text]
mccCacheNodeIdsToRemove = lens _mccCacheNodeIdsToRemove (\ s a -> s{_mccCacheNodeIdsToRemove = a}) . _Default . _Coerce;

-- | The number of days for which ElastiCache will retain automatic cache cluster snapshots before deleting them. For example, if you set /SnapshotRetentionLimit/ to 5, then a snapshot that was taken today will be retained for 5 days before being deleted.
--
-- If the value of SnapshotRetentionLimit is set to zero (0), backups are turned off.
mccSnapshotRetentionLimit :: Lens' ModifyCacheCluster (Maybe Int)
mccSnapshotRetentionLimit = lens _mccSnapshotRetentionLimit (\ s a -> s{_mccSnapshotRetentionLimit = a});

-- | The status of the Amazon SNS notification topic. Notifications are sent only if the status is /active/.
--
-- Valid values: 'active' | 'inactive'
mccNotificationTopicStatus :: Lens' ModifyCacheCluster (Maybe Text)
mccNotificationTopicStatus = lens _mccNotificationTopicStatus (\ s a -> s{_mccNotificationTopicStatus = a});

-- | Specifies whether the new nodes in this Memcached cache cluster are all created in a single Availability Zone or created across multiple Availability Zones.
--
-- Valid values: 'single-az' | 'cross-az'.
--
-- This option is only supported for Memcached cache clusters.
--
-- You cannot specify 'single-az' if the Memcached cache cluster already has cache nodes in different Availability Zones. If 'cross-az' is specified, existing Memcached nodes remain in their current Availability Zone.
--
-- Only newly created nodes will be located in different Availability Zones. For instructions on how to move existing Memcached nodes to different Availability Zones, see the __Availability Zone Considerations__ section of <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheNode.Memcached.html Cache Node Considerations for Memcached>.
mccAZMode :: Lens' ModifyCacheCluster (Maybe AZMode)
mccAZMode = lens _mccAZMode (\ s a -> s{_mccAZMode = a});

-- | If 'true', this parameter causes the modifications in this request and any pending modifications to be applied, asynchronously and as soon as possible, regardless of the /PreferredMaintenanceWindow/ setting for the cache cluster.
--
-- If 'false', then changes to the cache cluster are applied on the next maintenance reboot, or the next failure reboot, whichever occurs first.
--
-- If you perform a 'ModifyCacheCluster' before a pending modification is applied, the pending modification is replaced by the newer modification.
--
-- Valid values: 'true' | 'false'
--
-- Default: 'false'
mccApplyImmediately :: Lens' ModifyCacheCluster (Maybe Bool)
mccApplyImmediately = lens _mccApplyImmediately (\ s a -> s{_mccApplyImmediately = a});

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which notifications will be sent.
--
-- The Amazon SNS topic owner must be same as the cache cluster owner.
mccNotificationTopicARN :: Lens' ModifyCacheCluster (Maybe Text)
mccNotificationTopicARN = lens _mccNotificationTopicARN (\ s a -> s{_mccNotificationTopicARN = a});

-- | The number of cache nodes that the cache cluster should have. If the value for 'NumCacheNodes' is greater than the sum of the number of current cache nodes and the number of cache nodes pending creation (which may be zero), then more nodes will be added. If the value is less than the number of existing cache nodes, then nodes will be removed. If the value is equal to the number of current cache nodes, then any pending add or remove requests are canceled.
--
-- If you are removing cache nodes, you must use the 'CacheNodeIdsToRemove' parameter to provide the IDs of the specific cache nodes to remove.
--
-- For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20.
--
-- Adding or removing Memcached cache nodes can be applied immediately or as a pending action. See 'ApplyImmediately'.
--
-- A pending action to modify the number of cache nodes in a cluster during its maintenance window, whether by adding or removing nodes in accordance with the scale out architecture, is not queued. The customer\'s latest request to add or remove nodes to the cluster overrides any previous pending actions to modify the number of cache nodes in the cluster. For example, a request to remove 2 nodes would override a previous pending action to remove 3 nodes. Similarly, a request to add 2 nodes would override a previous pending action to remove 3 nodes and vice versa. As Memcached cache nodes may now be provisioned in different Availability Zones with flexible cache node placement, a request to add nodes does not automatically override a previous pending action to add nodes. The customer can modify the previous pending action to add more nodes or explicitly cancel the pending request and retry the new request. To cancel pending actions to modify the number of cache nodes in a cluster, use the 'ModifyCacheCluster' request and set /NumCacheNodes/ equal to the number of cache nodes currently in the cache cluster.
mccNumCacheNodes :: Lens' ModifyCacheCluster (Maybe Int)
mccNumCacheNodes = lens _mccNumCacheNodes (\ s a -> s{_mccNumCacheNodes = a});

-- | A list of cache security group names to authorize on this cache cluster. This change is asynchronously applied as soon as possible.
--
-- This parameter can be used only with clusters that are created outside of an Amazon Virtual Private Cloud (VPC).
--
-- Constraints: Must contain no more than 255 alphanumeric characters. Must not be \"Default\".
mccCacheSecurityGroupNames :: Lens' ModifyCacheCluster [Text]
mccCacheSecurityGroupNames = lens _mccCacheSecurityGroupNames (\ s a -> s{_mccCacheSecurityGroupNames = a}) . _Default . _Coerce;

-- | The cache cluster identifier. This value is stored as a lowercase string.
mccCacheClusterId :: Lens' ModifyCacheCluster Text
mccCacheClusterId = lens _mccCacheClusterId (\ s a -> s{_mccCacheClusterId = a});

instance AWSRequest ModifyCacheCluster where
        type Rs ModifyCacheCluster =
             ModifyCacheClusterResponse
        request = postQuery elastiCache
        response
          = receiveXMLWrapper "ModifyCacheClusterResult"
              (\ s h x ->
                 ModifyCacheClusterResponse' <$>
                   (x .@? "CacheCluster") <*> (pure (fromEnum s)))

instance Hashable ModifyCacheCluster

instance NFData ModifyCacheCluster

instance ToHeaders ModifyCacheCluster where
        toHeaders = const mempty

instance ToPath ModifyCacheCluster where
        toPath = const "/"

instance ToQuery ModifyCacheCluster where
        toQuery ModifyCacheCluster'{..}
          = mconcat
              ["Action" =: ("ModifyCacheCluster" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "EngineVersion" =: _mccEngineVersion,
               "CacheNodeType" =: _mccCacheNodeType,
               "SecurityGroupIds" =:
                 toQuery
                   (toQueryList "SecurityGroupId" <$>
                      _mccSecurityGroupIds),
               "AutoMinorVersionUpgrade" =:
                 _mccAutoMinorVersionUpgrade,
               "CacheParameterGroupName" =:
                 _mccCacheParameterGroupName,
               "SnapshotWindow" =: _mccSnapshotWindow,
               "NewAvailabilityZones" =:
                 toQuery
                   (toQueryList "PreferredAvailabilityZone" <$>
                      _mccNewAvailabilityZones),
               "PreferredMaintenanceWindow" =:
                 _mccPreferredMaintenanceWindow,
               "CacheNodeIdsToRemove" =:
                 toQuery
                   (toQueryList "CacheNodeId" <$>
                      _mccCacheNodeIdsToRemove),
               "SnapshotRetentionLimit" =:
                 _mccSnapshotRetentionLimit,
               "NotificationTopicStatus" =:
                 _mccNotificationTopicStatus,
               "AZMode" =: _mccAZMode,
               "ApplyImmediately" =: _mccApplyImmediately,
               "NotificationTopicArn" =: _mccNotificationTopicARN,
               "NumCacheNodes" =: _mccNumCacheNodes,
               "CacheSecurityGroupNames" =:
                 toQuery
                   (toQueryList "CacheSecurityGroupName" <$>
                      _mccCacheSecurityGroupNames),
               "CacheClusterId" =: _mccCacheClusterId]

-- | /See:/ 'modifyCacheClusterResponse' smart constructor.
data ModifyCacheClusterResponse = ModifyCacheClusterResponse'
    { _mccrsCacheCluster   :: !(Maybe CacheCluster)
    , _mccrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ModifyCacheClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mccrsCacheCluster'
--
-- * 'mccrsResponseStatus'
modifyCacheClusterResponse
    :: Int -- ^ 'mccrsResponseStatus'
    -> ModifyCacheClusterResponse
modifyCacheClusterResponse pResponseStatus_ =
    ModifyCacheClusterResponse'
    { _mccrsCacheCluster = Nothing
    , _mccrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
mccrsCacheCluster :: Lens' ModifyCacheClusterResponse (Maybe CacheCluster)
mccrsCacheCluster = lens _mccrsCacheCluster (\ s a -> s{_mccrsCacheCluster = a});

-- | The response status code.
mccrsResponseStatus :: Lens' ModifyCacheClusterResponse Int
mccrsResponseStatus = lens _mccrsResponseStatus (\ s a -> s{_mccrsResponseStatus = a});

instance NFData ModifyCacheClusterResponse
