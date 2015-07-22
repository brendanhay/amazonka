{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.ModifyCacheCluster
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The /ModifyCacheCluster/ action modifies the settings for a cache
-- cluster. You can use this action to change one or more cluster
-- configuration parameters by specifying the parameters and the new
-- values.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_ModifyCacheCluster.html>
module Network.AWS.ElastiCache.ModifyCacheCluster
    (
    -- * Request
      ModifyCacheCluster
    -- ** Request constructor
    , modifyCacheCluster
    -- ** Request lenses
    , mccrqEngineVersion
    , mccrqSecurityGroupIds
    , mccrqAutoMinorVersionUpgrade
    , mccrqCacheParameterGroupName
    , mccrqNewAvailabilityZones
    , mccrqSnapshotWindow
    , mccrqPreferredMaintenanceWindow
    , mccrqCacheNodeIdsToRemove
    , mccrqSnapshotRetentionLimit
    , mccrqAZMode
    , mccrqNotificationTopicStatus
    , mccrqApplyImmediately
    , mccrqNotificationTopicARN
    , mccrqNumCacheNodes
    , mccrqCacheSecurityGroupNames
    , mccrqCacheClusterId

    -- * Response
    , ModifyCacheClusterResponse
    -- ** Response constructor
    , modifyCacheClusterResponse
    -- ** Response lenses
    , mccrsCacheCluster
    , mccrsStatus
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /ModifyCacheCluster/ action.
--
-- /See:/ 'modifyCacheCluster' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mccrqEngineVersion'
--
-- * 'mccrqSecurityGroupIds'
--
-- * 'mccrqAutoMinorVersionUpgrade'
--
-- * 'mccrqCacheParameterGroupName'
--
-- * 'mccrqNewAvailabilityZones'
--
-- * 'mccrqSnapshotWindow'
--
-- * 'mccrqPreferredMaintenanceWindow'
--
-- * 'mccrqCacheNodeIdsToRemove'
--
-- * 'mccrqSnapshotRetentionLimit'
--
-- * 'mccrqAZMode'
--
-- * 'mccrqNotificationTopicStatus'
--
-- * 'mccrqApplyImmediately'
--
-- * 'mccrqNotificationTopicARN'
--
-- * 'mccrqNumCacheNodes'
--
-- * 'mccrqCacheSecurityGroupNames'
--
-- * 'mccrqCacheClusterId'
data ModifyCacheCluster = ModifyCacheCluster'
    { _mccrqEngineVersion              :: !(Maybe Text)
    , _mccrqSecurityGroupIds           :: !(Maybe [Text])
    , _mccrqAutoMinorVersionUpgrade    :: !(Maybe Bool)
    , _mccrqCacheParameterGroupName    :: !(Maybe Text)
    , _mccrqNewAvailabilityZones       :: !(Maybe [Text])
    , _mccrqSnapshotWindow             :: !(Maybe Text)
    , _mccrqPreferredMaintenanceWindow :: !(Maybe Text)
    , _mccrqCacheNodeIdsToRemove       :: !(Maybe [Text])
    , _mccrqSnapshotRetentionLimit     :: !(Maybe Int)
    , _mccrqAZMode                     :: !(Maybe AZMode)
    , _mccrqNotificationTopicStatus    :: !(Maybe Text)
    , _mccrqApplyImmediately           :: !(Maybe Bool)
    , _mccrqNotificationTopicARN       :: !(Maybe Text)
    , _mccrqNumCacheNodes              :: !(Maybe Int)
    , _mccrqCacheSecurityGroupNames    :: !(Maybe [Text])
    , _mccrqCacheClusterId             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyCacheCluster' smart constructor.
modifyCacheCluster :: Text -> ModifyCacheCluster
modifyCacheCluster pCacheClusterId_ =
    ModifyCacheCluster'
    { _mccrqEngineVersion = Nothing
    , _mccrqSecurityGroupIds = Nothing
    , _mccrqAutoMinorVersionUpgrade = Nothing
    , _mccrqCacheParameterGroupName = Nothing
    , _mccrqNewAvailabilityZones = Nothing
    , _mccrqSnapshotWindow = Nothing
    , _mccrqPreferredMaintenanceWindow = Nothing
    , _mccrqCacheNodeIdsToRemove = Nothing
    , _mccrqSnapshotRetentionLimit = Nothing
    , _mccrqAZMode = Nothing
    , _mccrqNotificationTopicStatus = Nothing
    , _mccrqApplyImmediately = Nothing
    , _mccrqNotificationTopicARN = Nothing
    , _mccrqNumCacheNodes = Nothing
    , _mccrqCacheSecurityGroupNames = Nothing
    , _mccrqCacheClusterId = pCacheClusterId_
    }

-- | The upgraded version of the cache engine to be run on the cache nodes.
mccrqEngineVersion :: Lens' ModifyCacheCluster (Maybe Text)
mccrqEngineVersion = lens _mccrqEngineVersion (\ s a -> s{_mccrqEngineVersion = a});

-- | Specifies the VPC Security Groups associated with the cache cluster.
--
-- This parameter can be used only with clusters that are created in an
-- Amazon Virtual Private Cloud (VPC).
mccrqSecurityGroupIds :: Lens' ModifyCacheCluster [Text]
mccrqSecurityGroupIds = lens _mccrqSecurityGroupIds (\ s a -> s{_mccrqSecurityGroupIds = a}) . _Default;

-- | This parameter is currently disabled.
mccrqAutoMinorVersionUpgrade :: Lens' ModifyCacheCluster (Maybe Bool)
mccrqAutoMinorVersionUpgrade = lens _mccrqAutoMinorVersionUpgrade (\ s a -> s{_mccrqAutoMinorVersionUpgrade = a});

-- | The name of the cache parameter group to apply to this cache cluster.
-- This change is asynchronously applied as soon as possible for parameters
-- when the /ApplyImmediately/ parameter is specified as /true/ for this
-- request.
mccrqCacheParameterGroupName :: Lens' ModifyCacheCluster (Maybe Text)
mccrqCacheParameterGroupName = lens _mccrqCacheParameterGroupName (\ s a -> s{_mccrqCacheParameterGroupName = a});

-- | The list of Availability Zones where the new Memcached cache nodes will
-- be created.
--
-- This parameter is only valid when /NumCacheNodes/ in the request is
-- greater than the sum of the number of active cache nodes and the number
-- of cache nodes pending creation (which may be zero). The number of
-- Availability Zones supplied in this list must match the cache nodes
-- being added in this request.
--
-- This option is only supported on Memcached clusters.
--
-- Scenarios:
--
-- -   __Scenario 1:__ You have 3 active nodes and wish to add 2 nodes.
--      Specify @NumCacheNodes=5@ (3 + 2) and optionally specify two
--     Availability Zones for the two new nodes.
-- -   __Scenario 2:__ You have 3 active nodes and 2 nodes pending creation
--     (from the scenario 1 call) and want to add 1 more node.
--      Specify @NumCacheNodes=6@ ((3 + 2) + 1)
-- -   __Scenario 3:__ You want to cancel all pending actions.
--      Specify @NumCacheNodes=3@ to cancel all pending actions.
--
-- The Availability Zone placement of nodes pending creation cannot be
-- modified. If you wish to cancel any nodes pending creation, add 0 nodes
-- by setting @NumCacheNodes@ to the number of current nodes.
--
-- If @cross-az@ is specified, existing Memcached nodes remain in their
-- current Availability Zone. Only newly created nodes can be located in
-- different Availability Zones. For guidance on how to move existing
-- Memcached nodes to different Availability Zones, see the __Availability
-- Zone Considerations__ section of
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheNode.Memcached.html Cache Node Considerations for Memcached>.
--
-- __Impact of new add\/remove requests upon pending requests__
--
-- >   --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- >   Scenarios    Pending action   New Request   Results
-- >   ------------ ---------------- ------------- ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- >   Scenario-1   Delete           Delete        The new delete, pending or immediate, replaces the pending delete.
-- >
-- >   Scenario-2   Delete           Create        The new create, pending or immediate, replaces the pending delete.
-- >
-- >   Scenario-3   Create           Delete        The new delete, pending or immediate, replaces the pending create.
-- >
-- >   Scenario-4   Create           Create        The new create is added to the pending create.
-- >                                                __Important:__
-- >                                               If the new create request is __Apply Immediately - Yes__, all creates are performed immediately. If the new create request is __Apply Immediately - No__, all creates are pending.
-- >   --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--
-- Example:
-- @NewAvailabilityZones.member.1=us-west-2a&NewAvailabilityZones.member.2=us-west-2b&NewAvailabilityZones.member.3=us-west-2c@
mccrqNewAvailabilityZones :: Lens' ModifyCacheCluster [Text]
mccrqNewAvailabilityZones = lens _mccrqNewAvailabilityZones (\ s a -> s{_mccrqNewAvailabilityZones = a}) . _Default;

-- | The daily time range (in UTC) during which ElastiCache will begin taking
-- a daily snapshot of your cache cluster.
mccrqSnapshotWindow :: Lens' ModifyCacheCluster (Maybe Text)
mccrqSnapshotWindow = lens _mccrqSnapshotWindow (\ s a -> s{_mccrqSnapshotWindow = a});

-- | Specifies the weekly time range during which maintenance on the cache
-- cluster is performed. It is specified as a range in the format
-- ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window
-- is a 60 minute period. Valid values for @ddd@ are:
--
-- -   @sun@
-- -   @mon@
-- -   @tue@
-- -   @wed@
-- -   @thu@
-- -   @fri@
-- -   @sat@
--
-- Example: @sun:05:00-sun:09:00@
mccrqPreferredMaintenanceWindow :: Lens' ModifyCacheCluster (Maybe Text)
mccrqPreferredMaintenanceWindow = lens _mccrqPreferredMaintenanceWindow (\ s a -> s{_mccrqPreferredMaintenanceWindow = a});

-- | A list of cache node IDs to be removed. A node ID is a numeric
-- identifier (0001, 0002, etc.). This parameter is only valid when
-- /NumCacheNodes/ is less than the existing number of cache nodes. The
-- number of cache node IDs supplied in this parameter must match the
-- difference between the existing number of cache nodes in the cluster or
-- pending cache nodes, whichever is greater, and the value of
-- /NumCacheNodes/ in the request.
--
-- For example: If you have 3 active cache nodes, 7 pending cache nodes,
-- and the number of cache nodes in this @ModifyCacheCluser@ call is 5, you
-- must list 2 (7 - 5) cache node IDs to remove.
mccrqCacheNodeIdsToRemove :: Lens' ModifyCacheCluster [Text]
mccrqCacheNodeIdsToRemove = lens _mccrqCacheNodeIdsToRemove (\ s a -> s{_mccrqCacheNodeIdsToRemove = a}) . _Default;

-- | The number of days for which ElastiCache will retain automatic cache
-- cluster snapshots before deleting them. For example, if you set
-- /SnapshotRetentionLimit/ to 5, then a snapshot that was taken today will
-- be retained for 5 days before being deleted.
--
-- __Important__
-- If the value of SnapshotRetentionLimit is set to zero (0), backups are
-- turned off.
mccrqSnapshotRetentionLimit :: Lens' ModifyCacheCluster (Maybe Int)
mccrqSnapshotRetentionLimit = lens _mccrqSnapshotRetentionLimit (\ s a -> s{_mccrqSnapshotRetentionLimit = a});

-- | Specifies whether the new nodes in this Memcached cache cluster are all
-- created in a single Availability Zone or created across multiple
-- Availability Zones.
--
-- Valid values: @single-az@ | @cross-az@.
--
-- This option is only supported for Memcached cache clusters.
--
-- You cannot specify @single-az@ if the Memcached cache cluster already
-- has cache nodes in different Availability Zones. If @cross-az@ is
-- specified, existing Memcached nodes remain in their current Availability
-- Zone.
--
-- Only newly created nodes will be located in different Availability
-- Zones. For instructions on how to move existing Memcached nodes to
-- different Availability Zones, see the __Availability Zone
-- Considerations__ section of
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheNode.Memcached.html Cache Node Considerations for Memcached>.
mccrqAZMode :: Lens' ModifyCacheCluster (Maybe AZMode)
mccrqAZMode = lens _mccrqAZMode (\ s a -> s{_mccrqAZMode = a});

-- | The status of the Amazon SNS notification topic. Notifications are sent
-- only if the status is /active/.
--
-- Valid values: @active@ | @inactive@
mccrqNotificationTopicStatus :: Lens' ModifyCacheCluster (Maybe Text)
mccrqNotificationTopicStatus = lens _mccrqNotificationTopicStatus (\ s a -> s{_mccrqNotificationTopicStatus = a});

-- | If @true@, this parameter causes the modifications in this request and
-- any pending modifications to be applied, asynchronously and as soon as
-- possible, regardless of the /PreferredMaintenanceWindow/ setting for the
-- cache cluster.
--
-- If @false@, then changes to the cache cluster are applied on the next
-- maintenance reboot, or the next failure reboot, whichever occurs first.
--
-- If you perform a @ModifyCacheCluster@ before a pending modification is
-- applied, the pending modification is replaced by the newer modification.
--
-- Valid values: @true@ | @false@
--
-- Default: @false@
mccrqApplyImmediately :: Lens' ModifyCacheCluster (Maybe Bool)
mccrqApplyImmediately = lens _mccrqApplyImmediately (\ s a -> s{_mccrqApplyImmediately = a});

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to which
-- notifications will be sent.
--
-- The Amazon SNS topic owner must be same as the cache cluster owner.
mccrqNotificationTopicARN :: Lens' ModifyCacheCluster (Maybe Text)
mccrqNotificationTopicARN = lens _mccrqNotificationTopicARN (\ s a -> s{_mccrqNotificationTopicARN = a});

-- | The number of cache nodes that the cache cluster should have. If the
-- value for @NumCacheNodes@ is greater than the sum of the number of
-- current cache nodes and the number of cache nodes pending creation
-- (which may be zero), then more nodes will be added. If the value is less
-- than the number of existing cache nodes, then nodes will be removed. If
-- the value is equal to the number of current cache nodes, then any
-- pending add or remove requests are canceled.
--
-- If you are removing cache nodes, you must use the @CacheNodeIdsToRemove@
-- parameter to provide the IDs of the specific cache nodes to remove.
--
-- For clusters running Redis, this value must be 1. For clusters running
-- Memcached, this value must be between 1 and 20.
--
-- __Note:__
-- Adding or removing Memcached cache nodes can be applied immediately or
-- as a pending action. See @ApplyImmediately@.
--  A pending action to modify the number of cache nodes in a cluster
-- during its maintenance window, whether by adding or removing nodes in
-- accordance with the scale out architecture, is not queued. The
-- customer\'s latest request to add or remove nodes to the cluster
-- overrides any previous pending actions to modify the number of cache
-- nodes in the cluster. For example, a request to remove 2 nodes would
-- override a previous pending action to remove 3 nodes. Similarly, a
-- request to add 2 nodes would override a previous pending action to
-- remove 3 nodes and vice versa. As Memcached cache nodes may now be
-- provisioned in different Availability Zones with flexible cache node
-- placement, a request to add nodes does not automatically override a
-- previous pending action to add nodes. The customer can modify the
-- previous pending action to add more nodes or explicitly cancel the
-- pending request and retry the new request. To cancel pending actions to
-- modify the number of cache nodes in a cluster, use the
-- @ModifyCacheCluster@ request and set /NumCacheNodes/ equal to the number
-- of cache nodes currently in the cache cluster.
mccrqNumCacheNodes :: Lens' ModifyCacheCluster (Maybe Int)
mccrqNumCacheNodes = lens _mccrqNumCacheNodes (\ s a -> s{_mccrqNumCacheNodes = a});

-- | A list of cache security group names to authorize on this cache cluster.
-- This change is asynchronously applied as soon as possible.
--
-- This parameter can be used only with clusters that are created outside
-- of an Amazon Virtual Private Cloud (VPC).
--
-- Constraints: Must contain no more than 255 alphanumeric characters. Must
-- not be \"Default\".
mccrqCacheSecurityGroupNames :: Lens' ModifyCacheCluster [Text]
mccrqCacheSecurityGroupNames = lens _mccrqCacheSecurityGroupNames (\ s a -> s{_mccrqCacheSecurityGroupNames = a}) . _Default;

-- | The cache cluster identifier. This value is stored as a lowercase
-- string.
mccrqCacheClusterId :: Lens' ModifyCacheCluster Text
mccrqCacheClusterId = lens _mccrqCacheClusterId (\ s a -> s{_mccrqCacheClusterId = a});

instance AWSRequest ModifyCacheCluster where
        type Sv ModifyCacheCluster = ElastiCache
        type Rs ModifyCacheCluster =
             ModifyCacheClusterResponse
        request = post
        response
          = receiveXMLWrapper "ModifyCacheClusterResult"
              (\ s h x ->
                 ModifyCacheClusterResponse' <$>
                   (x .@? "CacheCluster") <*> (pure (fromEnum s)))

instance ToHeaders ModifyCacheCluster where
        toHeaders = const mempty

instance ToPath ModifyCacheCluster where
        toPath = const "/"

instance ToQuery ModifyCacheCluster where
        toQuery ModifyCacheCluster'{..}
          = mconcat
              ["Action" =: ("ModifyCacheCluster" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "EngineVersion" =: _mccrqEngineVersion,
               "SecurityGroupIds" =:
                 toQuery
                   (toQueryList "SecurityGroupId" <$>
                      _mccrqSecurityGroupIds),
               "AutoMinorVersionUpgrade" =:
                 _mccrqAutoMinorVersionUpgrade,
               "CacheParameterGroupName" =:
                 _mccrqCacheParameterGroupName,
               "NewAvailabilityZones" =:
                 toQuery
                   (toQueryList "PreferredAvailabilityZone" <$>
                      _mccrqNewAvailabilityZones),
               "SnapshotWindow" =: _mccrqSnapshotWindow,
               "PreferredMaintenanceWindow" =:
                 _mccrqPreferredMaintenanceWindow,
               "CacheNodeIdsToRemove" =:
                 toQuery
                   (toQueryList "CacheNodeId" <$>
                      _mccrqCacheNodeIdsToRemove),
               "SnapshotRetentionLimit" =:
                 _mccrqSnapshotRetentionLimit,
               "AZMode" =: _mccrqAZMode,
               "NotificationTopicStatus" =:
                 _mccrqNotificationTopicStatus,
               "ApplyImmediately" =: _mccrqApplyImmediately,
               "NotificationTopicArn" =: _mccrqNotificationTopicARN,
               "NumCacheNodes" =: _mccrqNumCacheNodes,
               "CacheSecurityGroupNames" =:
                 toQuery
                   (toQueryList "CacheSecurityGroupName" <$>
                      _mccrqCacheSecurityGroupNames),
               "CacheClusterId" =: _mccrqCacheClusterId]

-- | /See:/ 'modifyCacheClusterResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mccrsCacheCluster'
--
-- * 'mccrsStatus'
data ModifyCacheClusterResponse = ModifyCacheClusterResponse'
    { _mccrsCacheCluster :: !(Maybe CacheCluster)
    , _mccrsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyCacheClusterResponse' smart constructor.
modifyCacheClusterResponse :: Int -> ModifyCacheClusterResponse
modifyCacheClusterResponse pStatus_ =
    ModifyCacheClusterResponse'
    { _mccrsCacheCluster = Nothing
    , _mccrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
mccrsCacheCluster :: Lens' ModifyCacheClusterResponse (Maybe CacheCluster)
mccrsCacheCluster = lens _mccrsCacheCluster (\ s a -> s{_mccrsCacheCluster = a});

-- | FIXME: Undocumented member.
mccrsStatus :: Lens' ModifyCacheClusterResponse Int
mccrsStatus = lens _mccrsStatus (\ s a -> s{_mccrsStatus = a});
