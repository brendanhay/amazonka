{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.CreateReplicationGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The /CreateReplicationGroup/ action creates a replication group. A
-- replication group is a collection of cache clusters, where one of the
-- cache clusters is a read\/write primary and the others are read-only
-- replicas. Writes to the primary are automatically propagated to the
-- replicas.
--
-- When you create a replication group, you must specify an existing cache
-- cluster that is in the primary role. When the replication group has been
-- successfully created, you can add one or more read replica replicas to
-- it, up to a total of five read replicas.
--
-- __Note:__ This action is valid only for Redis.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_CreateReplicationGroup.html>
module Network.AWS.ElastiCache.CreateReplicationGroup
    (
    -- * Request
      CreateReplicationGroup
    -- ** Request constructor
    , createReplicationGroup
    -- ** Request lenses
    , crgAutomaticFailoverEnabled
    , crgCacheNodeType
    , crgEngineVersion
    , crgSecurityGroupIds
    , crgAutoMinorVersionUpgrade
    , crgSnapshotARNs
    , crgCacheParameterGroupName
    , crgSnapshotWindow
    , crgPrimaryClusterId
    , crgEngine
    , crgPreferredMaintenanceWindow
    , crgCacheSubnetGroupName
    , crgSnapshotRetentionLimit
    , crgSnapshotName
    , crgPreferredCacheClusterAZs
    , crgNumCacheClusters
    , crgNotificationTopicARN
    , crgTags
    , crgCacheSecurityGroupNames
    , crgPort
    , crgReplicationGroupId
    , crgReplicationGroupDescription

    -- * Response
    , CreateReplicationGroupResponse
    -- ** Response constructor
    , createReplicationGroupResponse
    -- ** Response lenses
    , crgrsReplicationGroup
    , crgrsStatus
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /CreateReplicationGroup/ action.
--
-- /See:/ 'createReplicationGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crgAutomaticFailoverEnabled'
--
-- * 'crgCacheNodeType'
--
-- * 'crgEngineVersion'
--
-- * 'crgSecurityGroupIds'
--
-- * 'crgAutoMinorVersionUpgrade'
--
-- * 'crgSnapshotARNs'
--
-- * 'crgCacheParameterGroupName'
--
-- * 'crgSnapshotWindow'
--
-- * 'crgPrimaryClusterId'
--
-- * 'crgEngine'
--
-- * 'crgPreferredMaintenanceWindow'
--
-- * 'crgCacheSubnetGroupName'
--
-- * 'crgSnapshotRetentionLimit'
--
-- * 'crgSnapshotName'
--
-- * 'crgPreferredCacheClusterAZs'
--
-- * 'crgNumCacheClusters'
--
-- * 'crgNotificationTopicARN'
--
-- * 'crgTags'
--
-- * 'crgCacheSecurityGroupNames'
--
-- * 'crgPort'
--
-- * 'crgReplicationGroupId'
--
-- * 'crgReplicationGroupDescription'
data CreateReplicationGroup = CreateReplicationGroup'
    { _crgAutomaticFailoverEnabled    :: !(Maybe Bool)
    , _crgCacheNodeType               :: !(Maybe Text)
    , _crgEngineVersion               :: !(Maybe Text)
    , _crgSecurityGroupIds            :: !(Maybe [Text])
    , _crgAutoMinorVersionUpgrade     :: !(Maybe Bool)
    , _crgSnapshotARNs                :: !(Maybe [Text])
    , _crgCacheParameterGroupName     :: !(Maybe Text)
    , _crgSnapshotWindow              :: !(Maybe Text)
    , _crgPrimaryClusterId            :: !(Maybe Text)
    , _crgEngine                      :: !(Maybe Text)
    , _crgPreferredMaintenanceWindow  :: !(Maybe Text)
    , _crgCacheSubnetGroupName        :: !(Maybe Text)
    , _crgSnapshotRetentionLimit      :: !(Maybe Int)
    , _crgSnapshotName                :: !(Maybe Text)
    , _crgPreferredCacheClusterAZs    :: !(Maybe [Text])
    , _crgNumCacheClusters            :: !(Maybe Int)
    , _crgNotificationTopicARN        :: !(Maybe Text)
    , _crgTags                        :: !(Maybe [Tag])
    , _crgCacheSecurityGroupNames     :: !(Maybe [Text])
    , _crgPort                        :: !(Maybe Int)
    , _crgReplicationGroupId          :: !Text
    , _crgReplicationGroupDescription :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateReplicationGroup' smart constructor.
createReplicationGroup :: Text -> Text -> CreateReplicationGroup
createReplicationGroup pReplicationGroupId_ pReplicationGroupDescription_ =
    CreateReplicationGroup'
    { _crgAutomaticFailoverEnabled = Nothing
    , _crgCacheNodeType = Nothing
    , _crgEngineVersion = Nothing
    , _crgSecurityGroupIds = Nothing
    , _crgAutoMinorVersionUpgrade = Nothing
    , _crgSnapshotARNs = Nothing
    , _crgCacheParameterGroupName = Nothing
    , _crgSnapshotWindow = Nothing
    , _crgPrimaryClusterId = Nothing
    , _crgEngine = Nothing
    , _crgPreferredMaintenanceWindow = Nothing
    , _crgCacheSubnetGroupName = Nothing
    , _crgSnapshotRetentionLimit = Nothing
    , _crgSnapshotName = Nothing
    , _crgPreferredCacheClusterAZs = Nothing
    , _crgNumCacheClusters = Nothing
    , _crgNotificationTopicARN = Nothing
    , _crgTags = Nothing
    , _crgCacheSecurityGroupNames = Nothing
    , _crgPort = Nothing
    , _crgReplicationGroupId = pReplicationGroupId_
    , _crgReplicationGroupDescription = pReplicationGroupDescription_
    }

-- | Specifies whether a read-only replica will be automatically promoted to
-- read\/write primary if the existing primary fails.
--
-- If @true@, Multi-AZ is enabled for this replication group. If @false@,
-- Multi-AZ is disabled for this replication group.
--
-- Default: false
--
-- ElastiCache Multi-AZ replication groups is not supported on:
--
-- -   Redis versions earlier than 2.8.6.
-- -   T1 and T2 cache node types.
crgAutomaticFailoverEnabled :: Lens' CreateReplicationGroup (Maybe Bool)
crgAutomaticFailoverEnabled = lens _crgAutomaticFailoverEnabled (\ s a -> s{_crgAutomaticFailoverEnabled = a});

-- | The compute and memory capacity of the nodes in the node group.
--
-- Valid node types are as follows:
--
-- -   General purpose:
--     -   Current generation: @cache.t2.micro@, @cache.t2.small@,
--         @cache.t2.medium@, @cache.m3.medium@, @cache.m3.large@,
--         @cache.m3.xlarge@, @cache.m3.2xlarge@
--     -   Previous generation: @cache.t1.micro@, @cache.m1.small@,
--         @cache.m1.medium@, @cache.m1.large@, @cache.m1.xlarge@
-- -   Compute optimized: @cache.c1.xlarge@
-- -   Memory optimized
--     -   Current generation: @cache.r3.large@, @cache.r3.xlarge@,
--         @cache.r3.2xlarge@, @cache.r3.4xlarge@, @cache.r3.8xlarge@
--     -   Previous generation: @cache.m2.xlarge@, @cache.m2.2xlarge@,
--         @cache.m2.4xlarge@
--
-- __Notes:__
--
-- -   All t2 instances are created in an Amazon Virtual Private Cloud
--     (VPC).
-- -   Redis backup\/restore is not supported for t2 instances.
-- -   Redis Append-only files (AOF) functionality is not supported for t1
--     or t2 instances.
--
-- For a complete listing of cache node types and specifications, see
-- <http://aws.amazon.com/elasticache/details Amazon ElastiCache Product Features and Details>
-- and
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Memcached.html#CacheParameterGroups.Memcached.NodeSpecific Cache Node Type-Specific Parameters for Memcached>
-- or
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Redis.html#CacheParameterGroups.Redis.NodeSpecific Cache Node Type-Specific Parameters for Redis>.
crgCacheNodeType :: Lens' CreateReplicationGroup (Maybe Text)
crgCacheNodeType = lens _crgCacheNodeType (\ s a -> s{_crgCacheNodeType = a});

-- | The version number of the cache engine to be used for the cache clusters
-- in this replication group. To view the supported cache engine versions,
-- use the /DescribeCacheEngineVersions/ action.
crgEngineVersion :: Lens' CreateReplicationGroup (Maybe Text)
crgEngineVersion = lens _crgEngineVersion (\ s a -> s{_crgEngineVersion = a});

-- | One or more Amazon VPC security groups associated with this replication
-- group.
--
-- Use this parameter only when you are creating a replication group in an
-- Amazon Virtual Private Cloud (VPC).
crgSecurityGroupIds :: Lens' CreateReplicationGroup [Text]
crgSecurityGroupIds = lens _crgSecurityGroupIds (\ s a -> s{_crgSecurityGroupIds = a}) . _Default . _Coerce;

-- | This parameter is currently disabled.
crgAutoMinorVersionUpgrade :: Lens' CreateReplicationGroup (Maybe Bool)
crgAutoMinorVersionUpgrade = lens _crgAutoMinorVersionUpgrade (\ s a -> s{_crgAutoMinorVersionUpgrade = a});

-- | A single-element string list containing an Amazon Resource Name (ARN)
-- that uniquely identifies a Redis RDB snapshot file stored in Amazon S3.
-- The snapshot file will be used to populate the node group. The Amazon S3
-- object name in the ARN cannot contain any commas.
--
-- __Note:__ This parameter is only valid if the @Engine@ parameter is
-- @redis@.
--
-- Example of an Amazon S3 ARN: @arn:aws:s3:::my_bucket\/snapshot1.rdb@
crgSnapshotARNs :: Lens' CreateReplicationGroup [Text]
crgSnapshotARNs = lens _crgSnapshotARNs (\ s a -> s{_crgSnapshotARNs = a}) . _Default . _Coerce;

-- | The name of the parameter group to associate with this replication
-- group. If this argument is omitted, the default cache parameter group
-- for the specified engine is used.
crgCacheParameterGroupName :: Lens' CreateReplicationGroup (Maybe Text)
crgCacheParameterGroupName = lens _crgCacheParameterGroupName (\ s a -> s{_crgCacheParameterGroupName = a});

-- | The daily time range (in UTC) during which ElastiCache will begin taking
-- a daily snapshot of your node group.
--
-- Example: @05:00-09:00@
--
-- If you do not specify this parameter, then ElastiCache will
-- automatically choose an appropriate time range.
--
-- __Note:__ This parameter is only valid if the @Engine@ parameter is
-- @redis@.
crgSnapshotWindow :: Lens' CreateReplicationGroup (Maybe Text)
crgSnapshotWindow = lens _crgSnapshotWindow (\ s a -> s{_crgSnapshotWindow = a});

-- | The identifier of the cache cluster that will serve as the primary for
-- this replication group. This cache cluster must already exist and have a
-- status of /available/.
--
-- This parameter is not required if /NumCacheClusters/ is specified.
crgPrimaryClusterId :: Lens' CreateReplicationGroup (Maybe Text)
crgPrimaryClusterId = lens _crgPrimaryClusterId (\ s a -> s{_crgPrimaryClusterId = a});

-- | The name of the cache engine to be used for the cache clusters in this
-- replication group.
--
-- Default: redis
crgEngine :: Lens' CreateReplicationGroup (Maybe Text)
crgEngine = lens _crgEngine (\ s a -> s{_crgEngine = a});

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
crgPreferredMaintenanceWindow :: Lens' CreateReplicationGroup (Maybe Text)
crgPreferredMaintenanceWindow = lens _crgPreferredMaintenanceWindow (\ s a -> s{_crgPreferredMaintenanceWindow = a});

-- | The name of the cache subnet group to be used for the replication group.
crgCacheSubnetGroupName :: Lens' CreateReplicationGroup (Maybe Text)
crgCacheSubnetGroupName = lens _crgCacheSubnetGroupName (\ s a -> s{_crgCacheSubnetGroupName = a});

-- | The number of days for which ElastiCache will retain automatic snapshots
-- before deleting them. For example, if you set @SnapshotRetentionLimit@
-- to 5, then a snapshot that was taken today will be retained for 5 days
-- before being deleted.
--
-- __Note:__ This parameter is only valid if the @Engine@ parameter is
-- @redis@.
--
-- Default: 0 (i.e., automatic backups are disabled for this cache
-- cluster).
crgSnapshotRetentionLimit :: Lens' CreateReplicationGroup (Maybe Int)
crgSnapshotRetentionLimit = lens _crgSnapshotRetentionLimit (\ s a -> s{_crgSnapshotRetentionLimit = a});

-- | The name of a snapshot from which to restore data into the new node
-- group. The snapshot status changes to @restoring@ while the new node
-- group is being created.
--
-- __Note:__ This parameter is only valid if the @Engine@ parameter is
-- @redis@.
crgSnapshotName :: Lens' CreateReplicationGroup (Maybe Text)
crgSnapshotName = lens _crgSnapshotName (\ s a -> s{_crgSnapshotName = a});

-- | A list of EC2 availability zones in which the replication group\'s cache
-- clusters will be created. The order of the availability zones in the
-- list is not important.
--
-- If you are creating your replication group in an Amazon VPC
-- (recommended), you can only locate cache clusters in availability zones
-- associated with the subnets in the selected subnet group.
--
-- The number of availability zones listed must equal the value of
-- /NumCacheClusters/.
--
-- Default: system chosen availability zones.
--
-- Example: One Redis cache cluster in each of three availability zones.
-- PreferredAvailabilityZones.member.1=us-west-2a
-- PreferredAvailabilityZones.member.2=us-west-2c
-- PreferredAvailabilityZones.member.3=us-west-2c
crgPreferredCacheClusterAZs :: Lens' CreateReplicationGroup [Text]
crgPreferredCacheClusterAZs = lens _crgPreferredCacheClusterAZs (\ s a -> s{_crgPreferredCacheClusterAZs = a}) . _Default . _Coerce;

-- | The number of cache clusters this replication group will initially have.
--
-- If /Multi-AZ/ is @enabled@, the value of this parameter must be at least
-- 2.
--
-- The maximum permitted value for /NumCacheClusters/ is 6 (primary plus 5
-- replicas). If you need to exceed this limit, please fill out the
-- ElastiCache Limit Increase Request form at
-- <http://aws.amazon.com/contact-us/elasticache-node-limit-request>.
crgNumCacheClusters :: Lens' CreateReplicationGroup (Maybe Int)
crgNumCacheClusters = lens _crgNumCacheClusters (\ s a -> s{_crgNumCacheClusters = a});

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- (SNS) topic to which notifications will be sent.
--
-- The Amazon SNS topic owner must be the same as the cache cluster owner.
crgNotificationTopicARN :: Lens' CreateReplicationGroup (Maybe Text)
crgNotificationTopicARN = lens _crgNotificationTopicARN (\ s a -> s{_crgNotificationTopicARN = a});

-- | A list of cost allocation tags to be added to this resource. A tag is a
-- key-value pair. A tag key must be accompanied by a tag value.
crgTags :: Lens' CreateReplicationGroup [Tag]
crgTags = lens _crgTags (\ s a -> s{_crgTags = a}) . _Default . _Coerce;

-- | A list of cache security group names to associate with this replication
-- group.
crgCacheSecurityGroupNames :: Lens' CreateReplicationGroup [Text]
crgCacheSecurityGroupNames = lens _crgCacheSecurityGroupNames (\ s a -> s{_crgCacheSecurityGroupNames = a}) . _Default . _Coerce;

-- | The port number on which each member of the replication group will
-- accept connections.
crgPort :: Lens' CreateReplicationGroup (Maybe Int)
crgPort = lens _crgPort (\ s a -> s{_crgPort = a});

-- | The replication group identifier. This parameter is stored as a
-- lowercase string.
--
-- Constraints:
--
-- -   A name must contain from 1 to 20 alphanumeric characters or hyphens.
-- -   The first character must be a letter.
-- -   A name cannot end with a hyphen or contain two consecutive hyphens.
crgReplicationGroupId :: Lens' CreateReplicationGroup Text
crgReplicationGroupId = lens _crgReplicationGroupId (\ s a -> s{_crgReplicationGroupId = a});

-- | A user-created description for the replication group.
crgReplicationGroupDescription :: Lens' CreateReplicationGroup Text
crgReplicationGroupDescription = lens _crgReplicationGroupDescription (\ s a -> s{_crgReplicationGroupDescription = a});

instance AWSRequest CreateReplicationGroup where
        type Sv CreateReplicationGroup = ElastiCache
        type Rs CreateReplicationGroup =
             CreateReplicationGroupResponse
        request = post
        response
          = receiveXMLWrapper "CreateReplicationGroupResult"
              (\ s h x ->
                 CreateReplicationGroupResponse' <$>
                   (x .@? "ReplicationGroup") <*> (pure (fromEnum s)))

instance ToHeaders CreateReplicationGroup where
        toHeaders = const mempty

instance ToPath CreateReplicationGroup where
        toPath = const "/"

instance ToQuery CreateReplicationGroup where
        toQuery CreateReplicationGroup'{..}
          = mconcat
              ["Action" =:
                 ("CreateReplicationGroup" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "AutomaticFailoverEnabled" =:
                 _crgAutomaticFailoverEnabled,
               "CacheNodeType" =: _crgCacheNodeType,
               "EngineVersion" =: _crgEngineVersion,
               "SecurityGroupIds" =:
                 toQuery
                   (toQueryList "SecurityGroupId" <$>
                      _crgSecurityGroupIds),
               "AutoMinorVersionUpgrade" =:
                 _crgAutoMinorVersionUpgrade,
               "SnapshotArns" =:
                 toQuery
                   (toQueryList "SnapshotArn" <$> _crgSnapshotARNs),
               "CacheParameterGroupName" =:
                 _crgCacheParameterGroupName,
               "SnapshotWindow" =: _crgSnapshotWindow,
               "PrimaryClusterId" =: _crgPrimaryClusterId,
               "Engine" =: _crgEngine,
               "PreferredMaintenanceWindow" =:
                 _crgPreferredMaintenanceWindow,
               "CacheSubnetGroupName" =: _crgCacheSubnetGroupName,
               "SnapshotRetentionLimit" =:
                 _crgSnapshotRetentionLimit,
               "SnapshotName" =: _crgSnapshotName,
               "PreferredCacheClusterAZs" =:
                 toQuery
                   (toQueryList "AvailabilityZone" <$>
                      _crgPreferredCacheClusterAZs),
               "NumCacheClusters" =: _crgNumCacheClusters,
               "NotificationTopicArn" =: _crgNotificationTopicARN,
               "Tags" =: toQuery (toQueryList "Tag" <$> _crgTags),
               "CacheSecurityGroupNames" =:
                 toQuery
                   (toQueryList "CacheSecurityGroupName" <$>
                      _crgCacheSecurityGroupNames),
               "Port" =: _crgPort,
               "ReplicationGroupId" =: _crgReplicationGroupId,
               "ReplicationGroupDescription" =:
                 _crgReplicationGroupDescription]

-- | /See:/ 'createReplicationGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crgrsReplicationGroup'
--
-- * 'crgrsStatus'
data CreateReplicationGroupResponse = CreateReplicationGroupResponse'
    { _crgrsReplicationGroup :: !(Maybe ReplicationGroup)
    , _crgrsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateReplicationGroupResponse' smart constructor.
createReplicationGroupResponse :: Int -> CreateReplicationGroupResponse
createReplicationGroupResponse pStatus_ =
    CreateReplicationGroupResponse'
    { _crgrsReplicationGroup = Nothing
    , _crgrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
crgrsReplicationGroup :: Lens' CreateReplicationGroupResponse (Maybe ReplicationGroup)
crgrsReplicationGroup = lens _crgrsReplicationGroup (\ s a -> s{_crgrsReplicationGroup = a});

-- | FIXME: Undocumented member.
crgrsStatus :: Lens' CreateReplicationGroupResponse Int
crgrsStatus = lens _crgrsStatus (\ s a -> s{_crgrsStatus = a});
