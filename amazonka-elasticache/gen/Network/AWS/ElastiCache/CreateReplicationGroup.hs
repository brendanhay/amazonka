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
    , crgrqAutomaticFailoverEnabled
    , crgrqCacheNodeType
    , crgrqEngineVersion
    , crgrqSecurityGroupIds
    , crgrqAutoMinorVersionUpgrade
    , crgrqSnapshotARNs
    , crgrqCacheParameterGroupName
    , crgrqSnapshotWindow
    , crgrqPrimaryClusterId
    , crgrqEngine
    , crgrqPreferredMaintenanceWindow
    , crgrqCacheSubnetGroupName
    , crgrqSnapshotRetentionLimit
    , crgrqSnapshotName
    , crgrqPreferredCacheClusterAZs
    , crgrqNumCacheClusters
    , crgrqNotificationTopicARN
    , crgrqTags
    , crgrqCacheSecurityGroupNames
    , crgrqPort
    , crgrqReplicationGroupId
    , crgrqReplicationGroupDescription

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
-- * 'crgrqAutomaticFailoverEnabled'
--
-- * 'crgrqCacheNodeType'
--
-- * 'crgrqEngineVersion'
--
-- * 'crgrqSecurityGroupIds'
--
-- * 'crgrqAutoMinorVersionUpgrade'
--
-- * 'crgrqSnapshotARNs'
--
-- * 'crgrqCacheParameterGroupName'
--
-- * 'crgrqSnapshotWindow'
--
-- * 'crgrqPrimaryClusterId'
--
-- * 'crgrqEngine'
--
-- * 'crgrqPreferredMaintenanceWindow'
--
-- * 'crgrqCacheSubnetGroupName'
--
-- * 'crgrqSnapshotRetentionLimit'
--
-- * 'crgrqSnapshotName'
--
-- * 'crgrqPreferredCacheClusterAZs'
--
-- * 'crgrqNumCacheClusters'
--
-- * 'crgrqNotificationTopicARN'
--
-- * 'crgrqTags'
--
-- * 'crgrqCacheSecurityGroupNames'
--
-- * 'crgrqPort'
--
-- * 'crgrqReplicationGroupId'
--
-- * 'crgrqReplicationGroupDescription'
data CreateReplicationGroup = CreateReplicationGroup'
    { _crgrqAutomaticFailoverEnabled    :: !(Maybe Bool)
    , _crgrqCacheNodeType               :: !(Maybe Text)
    , _crgrqEngineVersion               :: !(Maybe Text)
    , _crgrqSecurityGroupIds            :: !(Maybe [Text])
    , _crgrqAutoMinorVersionUpgrade     :: !(Maybe Bool)
    , _crgrqSnapshotARNs                :: !(Maybe [Text])
    , _crgrqCacheParameterGroupName     :: !(Maybe Text)
    , _crgrqSnapshotWindow              :: !(Maybe Text)
    , _crgrqPrimaryClusterId            :: !(Maybe Text)
    , _crgrqEngine                      :: !(Maybe Text)
    , _crgrqPreferredMaintenanceWindow  :: !(Maybe Text)
    , _crgrqCacheSubnetGroupName        :: !(Maybe Text)
    , _crgrqSnapshotRetentionLimit      :: !(Maybe Int)
    , _crgrqSnapshotName                :: !(Maybe Text)
    , _crgrqPreferredCacheClusterAZs    :: !(Maybe [Text])
    , _crgrqNumCacheClusters            :: !(Maybe Int)
    , _crgrqNotificationTopicARN        :: !(Maybe Text)
    , _crgrqTags                        :: !(Maybe [Tag])
    , _crgrqCacheSecurityGroupNames     :: !(Maybe [Text])
    , _crgrqPort                        :: !(Maybe Int)
    , _crgrqReplicationGroupId          :: !Text
    , _crgrqReplicationGroupDescription :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateReplicationGroup' smart constructor.
createReplicationGroup :: Text -> Text -> CreateReplicationGroup
createReplicationGroup pReplicationGroupId pReplicationGroupDescription =
    CreateReplicationGroup'
    { _crgrqAutomaticFailoverEnabled = Nothing
    , _crgrqCacheNodeType = Nothing
    , _crgrqEngineVersion = Nothing
    , _crgrqSecurityGroupIds = Nothing
    , _crgrqAutoMinorVersionUpgrade = Nothing
    , _crgrqSnapshotARNs = Nothing
    , _crgrqCacheParameterGroupName = Nothing
    , _crgrqSnapshotWindow = Nothing
    , _crgrqPrimaryClusterId = Nothing
    , _crgrqEngine = Nothing
    , _crgrqPreferredMaintenanceWindow = Nothing
    , _crgrqCacheSubnetGroupName = Nothing
    , _crgrqSnapshotRetentionLimit = Nothing
    , _crgrqSnapshotName = Nothing
    , _crgrqPreferredCacheClusterAZs = Nothing
    , _crgrqNumCacheClusters = Nothing
    , _crgrqNotificationTopicARN = Nothing
    , _crgrqTags = Nothing
    , _crgrqCacheSecurityGroupNames = Nothing
    , _crgrqPort = Nothing
    , _crgrqReplicationGroupId = pReplicationGroupId
    , _crgrqReplicationGroupDescription = pReplicationGroupDescription
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
crgrqAutomaticFailoverEnabled :: Lens' CreateReplicationGroup (Maybe Bool)
crgrqAutomaticFailoverEnabled = lens _crgrqAutomaticFailoverEnabled (\ s a -> s{_crgrqAutomaticFailoverEnabled = a});

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
crgrqCacheNodeType :: Lens' CreateReplicationGroup (Maybe Text)
crgrqCacheNodeType = lens _crgrqCacheNodeType (\ s a -> s{_crgrqCacheNodeType = a});

-- | The version number of the cache engine to be used for the cache clusters
-- in this replication group. To view the supported cache engine versions,
-- use the /DescribeCacheEngineVersions/ action.
crgrqEngineVersion :: Lens' CreateReplicationGroup (Maybe Text)
crgrqEngineVersion = lens _crgrqEngineVersion (\ s a -> s{_crgrqEngineVersion = a});

-- | One or more Amazon VPC security groups associated with this replication
-- group.
--
-- Use this parameter only when you are creating a replication group in an
-- Amazon Virtual Private Cloud (VPC).
crgrqSecurityGroupIds :: Lens' CreateReplicationGroup [Text]
crgrqSecurityGroupIds = lens _crgrqSecurityGroupIds (\ s a -> s{_crgrqSecurityGroupIds = a}) . _Default;

-- | This parameter is currently disabled.
crgrqAutoMinorVersionUpgrade :: Lens' CreateReplicationGroup (Maybe Bool)
crgrqAutoMinorVersionUpgrade = lens _crgrqAutoMinorVersionUpgrade (\ s a -> s{_crgrqAutoMinorVersionUpgrade = a});

-- | A single-element string list containing an Amazon Resource Name (ARN)
-- that uniquely identifies a Redis RDB snapshot file stored in Amazon S3.
-- The snapshot file will be used to populate the node group. The Amazon S3
-- object name in the ARN cannot contain any commas.
--
-- __Note:__ This parameter is only valid if the @Engine@ parameter is
-- @redis@.
--
-- Example of an Amazon S3 ARN: @arn:aws:s3:::my_bucket\/snapshot1.rdb@
crgrqSnapshotARNs :: Lens' CreateReplicationGroup [Text]
crgrqSnapshotARNs = lens _crgrqSnapshotARNs (\ s a -> s{_crgrqSnapshotARNs = a}) . _Default;

-- | The name of the parameter group to associate with this replication
-- group. If this argument is omitted, the default cache parameter group
-- for the specified engine is used.
crgrqCacheParameterGroupName :: Lens' CreateReplicationGroup (Maybe Text)
crgrqCacheParameterGroupName = lens _crgrqCacheParameterGroupName (\ s a -> s{_crgrqCacheParameterGroupName = a});

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
crgrqSnapshotWindow :: Lens' CreateReplicationGroup (Maybe Text)
crgrqSnapshotWindow = lens _crgrqSnapshotWindow (\ s a -> s{_crgrqSnapshotWindow = a});

-- | The identifier of the cache cluster that will serve as the primary for
-- this replication group. This cache cluster must already exist and have a
-- status of /available/.
--
-- This parameter is not required if /NumCacheClusters/ is specified.
crgrqPrimaryClusterId :: Lens' CreateReplicationGroup (Maybe Text)
crgrqPrimaryClusterId = lens _crgrqPrimaryClusterId (\ s a -> s{_crgrqPrimaryClusterId = a});

-- | The name of the cache engine to be used for the cache clusters in this
-- replication group.
--
-- Default: redis
crgrqEngine :: Lens' CreateReplicationGroup (Maybe Text)
crgrqEngine = lens _crgrqEngine (\ s a -> s{_crgrqEngine = a});

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
crgrqPreferredMaintenanceWindow :: Lens' CreateReplicationGroup (Maybe Text)
crgrqPreferredMaintenanceWindow = lens _crgrqPreferredMaintenanceWindow (\ s a -> s{_crgrqPreferredMaintenanceWindow = a});

-- | The name of the cache subnet group to be used for the replication group.
crgrqCacheSubnetGroupName :: Lens' CreateReplicationGroup (Maybe Text)
crgrqCacheSubnetGroupName = lens _crgrqCacheSubnetGroupName (\ s a -> s{_crgrqCacheSubnetGroupName = a});

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
crgrqSnapshotRetentionLimit :: Lens' CreateReplicationGroup (Maybe Int)
crgrqSnapshotRetentionLimit = lens _crgrqSnapshotRetentionLimit (\ s a -> s{_crgrqSnapshotRetentionLimit = a});

-- | The name of a snapshot from which to restore data into the new node
-- group. The snapshot status changes to @restoring@ while the new node
-- group is being created.
--
-- __Note:__ This parameter is only valid if the @Engine@ parameter is
-- @redis@.
crgrqSnapshotName :: Lens' CreateReplicationGroup (Maybe Text)
crgrqSnapshotName = lens _crgrqSnapshotName (\ s a -> s{_crgrqSnapshotName = a});

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
crgrqPreferredCacheClusterAZs :: Lens' CreateReplicationGroup [Text]
crgrqPreferredCacheClusterAZs = lens _crgrqPreferredCacheClusterAZs (\ s a -> s{_crgrqPreferredCacheClusterAZs = a}) . _Default;

-- | The number of cache clusters this replication group will initially have.
--
-- If /Multi-AZ/ is @enabled@, the value of this parameter must be at least
-- 2.
--
-- The maximum permitted value for /NumCacheClusters/ is 6 (primary plus 5
-- replicas). If you need to exceed this limit, please fill out the
-- ElastiCache Limit Increase Request form at
-- <http://aws.amazon.com/contact-us/elasticache-node-limit-request>.
crgrqNumCacheClusters :: Lens' CreateReplicationGroup (Maybe Int)
crgrqNumCacheClusters = lens _crgrqNumCacheClusters (\ s a -> s{_crgrqNumCacheClusters = a});

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- (SNS) topic to which notifications will be sent.
--
-- The Amazon SNS topic owner must be the same as the cache cluster owner.
crgrqNotificationTopicARN :: Lens' CreateReplicationGroup (Maybe Text)
crgrqNotificationTopicARN = lens _crgrqNotificationTopicARN (\ s a -> s{_crgrqNotificationTopicARN = a});

-- | A list of cost allocation tags to be added to this resource. A tag is a
-- key-value pair. A tag key must be accompanied by a tag value.
crgrqTags :: Lens' CreateReplicationGroup [Tag]
crgrqTags = lens _crgrqTags (\ s a -> s{_crgrqTags = a}) . _Default;

-- | A list of cache security group names to associate with this replication
-- group.
crgrqCacheSecurityGroupNames :: Lens' CreateReplicationGroup [Text]
crgrqCacheSecurityGroupNames = lens _crgrqCacheSecurityGroupNames (\ s a -> s{_crgrqCacheSecurityGroupNames = a}) . _Default;

-- | The port number on which each member of the replication group will
-- accept connections.
crgrqPort :: Lens' CreateReplicationGroup (Maybe Int)
crgrqPort = lens _crgrqPort (\ s a -> s{_crgrqPort = a});

-- | The replication group identifier. This parameter is stored as a
-- lowercase string.
--
-- Constraints:
--
-- -   A name must contain from 1 to 20 alphanumeric characters or hyphens.
-- -   The first character must be a letter.
-- -   A name cannot end with a hyphen or contain two consecutive hyphens.
crgrqReplicationGroupId :: Lens' CreateReplicationGroup Text
crgrqReplicationGroupId = lens _crgrqReplicationGroupId (\ s a -> s{_crgrqReplicationGroupId = a});

-- | A user-created description for the replication group.
crgrqReplicationGroupDescription :: Lens' CreateReplicationGroup Text
crgrqReplicationGroupDescription = lens _crgrqReplicationGroupDescription (\ s a -> s{_crgrqReplicationGroupDescription = a});

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
                 _crgrqAutomaticFailoverEnabled,
               "CacheNodeType" =: _crgrqCacheNodeType,
               "EngineVersion" =: _crgrqEngineVersion,
               "SecurityGroupIds" =:
                 toQuery
                   (toQueryList "SecurityGroupId" <$>
                      _crgrqSecurityGroupIds),
               "AutoMinorVersionUpgrade" =:
                 _crgrqAutoMinorVersionUpgrade,
               "SnapshotArns" =:
                 toQuery
                   (toQueryList "SnapshotArn" <$> _crgrqSnapshotARNs),
               "CacheParameterGroupName" =:
                 _crgrqCacheParameterGroupName,
               "SnapshotWindow" =: _crgrqSnapshotWindow,
               "PrimaryClusterId" =: _crgrqPrimaryClusterId,
               "Engine" =: _crgrqEngine,
               "PreferredMaintenanceWindow" =:
                 _crgrqPreferredMaintenanceWindow,
               "CacheSubnetGroupName" =: _crgrqCacheSubnetGroupName,
               "SnapshotRetentionLimit" =:
                 _crgrqSnapshotRetentionLimit,
               "SnapshotName" =: _crgrqSnapshotName,
               "PreferredCacheClusterAZs" =:
                 toQuery
                   (toQueryList "AvailabilityZone" <$>
                      _crgrqPreferredCacheClusterAZs),
               "NumCacheClusters" =: _crgrqNumCacheClusters,
               "NotificationTopicArn" =: _crgrqNotificationTopicARN,
               "Tags" =: toQuery (toQueryList "Tag" <$> _crgrqTags),
               "CacheSecurityGroupNames" =:
                 toQuery
                   (toQueryList "CacheSecurityGroupName" <$>
                      _crgrqCacheSecurityGroupNames),
               "Port" =: _crgrqPort,
               "ReplicationGroupId" =: _crgrqReplicationGroupId,
               "ReplicationGroupDescription" =:
                 _crgrqReplicationGroupDescription]

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
createReplicationGroupResponse pStatus =
    CreateReplicationGroupResponse'
    { _crgrsReplicationGroup = Nothing
    , _crgrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
crgrsReplicationGroup :: Lens' CreateReplicationGroupResponse (Maybe ReplicationGroup)
crgrsReplicationGroup = lens _crgrsReplicationGroup (\ s a -> s{_crgrsReplicationGroup = a});

-- | FIXME: Undocumented member.
crgrsStatus :: Lens' CreateReplicationGroupResponse Int
crgrsStatus = lens _crgrsStatus (\ s a -> s{_crgrsStatus = a});
