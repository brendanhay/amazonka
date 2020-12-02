{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.Snapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.Snapshot where

import Network.AWS.ElastiCache.Types.AutomaticFailoverStatus
import Network.AWS.ElastiCache.Types.NodeSnapshot
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents a copy of an entire Redis cluster as of the time when the snapshot was taken.
--
--
--
-- /See:/ 'snapshot' smart constructor.
data Snapshot = Snapshot'
  { _sEngineVersion :: !(Maybe Text),
    _sCacheNodeType :: !(Maybe Text),
    _sCacheClusterCreateTime :: !(Maybe ISO8601),
    _sAutoMinorVersionUpgrade :: !(Maybe Bool),
    _sARN :: !(Maybe Text),
    _sCacheParameterGroupName :: !(Maybe Text),
    _sReplicationGroupDescription :: !(Maybe Text),
    _sVPCId :: !(Maybe Text),
    _sSnapshotStatus :: !(Maybe Text),
    _sSnapshotWindow :: !(Maybe Text),
    _sCacheClusterId :: !(Maybe Text),
    _sEngine :: !(Maybe Text),
    _sPreferredMaintenanceWindow :: !(Maybe Text),
    _sTopicARN :: !(Maybe Text),
    _sKMSKeyId :: !(Maybe Text),
    _sNodeSnapshots :: !(Maybe [NodeSnapshot]),
    _sCacheSubnetGroupName :: !(Maybe Text),
    _sPreferredAvailabilityZone :: !(Maybe Text),
    _sNumNodeGroups :: !(Maybe Int),
    _sSnapshotRetentionLimit :: !(Maybe Int),
    _sSnapshotName :: !(Maybe Text),
    _sPreferredOutpostARN :: !(Maybe Text),
    _sReplicationGroupId :: !(Maybe Text),
    _sNumCacheNodes :: !(Maybe Int),
    _sPort :: !(Maybe Int),
    _sAutomaticFailover :: !(Maybe AutomaticFailoverStatus),
    _sSnapshotSource :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Snapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sEngineVersion' - The version of the cache engine version that is used by the source cluster.
--
-- * 'sCacheNodeType' - The name of the compute and memory capacity node type for the source cluster. The following node types are supported by ElastiCache. Generally speaking, the current generation types provide more memory and computational power at lower cost when compared to their equivalent previous generation counterparts.     * General purpose:     * Current generation:  __M6g node types__ (available only for Redis engine version 5.0.6 onward and for Memcached engine version 1.5.16 onward). @cache.m6g.large@ , @cache.m6g.xlarge@ , @cache.m6g.2xlarge@ , @cache.m6g.4xlarge@ , @cache.m6g.8xlarge@ , @cache.m6g.12xlarge@ , @cache.m6g.16xlarge@  __M5 node types:__ @cache.m5.large@ , @cache.m5.xlarge@ , @cache.m5.2xlarge@ , @cache.m5.4xlarge@ , @cache.m5.12xlarge@ , @cache.m5.24xlarge@  __M4 node types:__ @cache.m4.large@ , @cache.m4.xlarge@ , @cache.m4.2xlarge@ , @cache.m4.4xlarge@ , @cache.m4.10xlarge@  __T3 node types:__ @cache.t3.micro@ , @cache.t3.small@ , @cache.t3.medium@  __T2 node types:__ @cache.t2.micro@ , @cache.t2.small@ , @cache.t2.medium@      * Previous generation: (not recommended) __T1 node types:__ @cache.t1.micro@  __M1 node types:__ @cache.m1.small@ , @cache.m1.medium@ , @cache.m1.large@ , @cache.m1.xlarge@  __M3 node types:__ @cache.m3.medium@ , @cache.m3.large@ , @cache.m3.xlarge@ , @cache.m3.2xlarge@      * Compute optimized:     * Previous generation: (not recommended) __C1 node types:__ @cache.c1.xlarge@      * Memory optimized:     * Current generation:  __R6g node types__ (available only for Redis engine version 5.0.6 onward and for Memcached engine version 1.5.16 onward). @cache.r6g.large@ , @cache.r6g.xlarge@ , @cache.r6g.2xlarge@ , @cache.r6g.4xlarge@ , @cache.r6g.8xlarge@ , @cache.r6g.12xlarge@ , @cache.r6g.16xlarge@  __R5 node types:__ @cache.r5.large@ , @cache.r5.xlarge@ , @cache.r5.2xlarge@ , @cache.r5.4xlarge@ , @cache.r5.12xlarge@ , @cache.r5.24xlarge@  __R4 node types:__ @cache.r4.large@ , @cache.r4.xlarge@ , @cache.r4.2xlarge@ , @cache.r4.4xlarge@ , @cache.r4.8xlarge@ , @cache.r4.16xlarge@      * Previous generation: (not recommended) __M2 node types:__ @cache.m2.xlarge@ , @cache.m2.2xlarge@ , @cache.m2.4xlarge@  __R3 node types:__ @cache.r3.large@ , @cache.r3.xlarge@ , @cache.r3.2xlarge@ , @cache.r3.4xlarge@ , @cache.r3.8xlarge@  __Additional node type info__      * All current generation instance types are created in Amazon VPC by default.     * Redis append-only files (AOF) are not supported for T1 or T2 instances.     * Redis Multi-AZ with automatic failover is not supported on T1 instances.     * Redis configuration variables @appendonly@ and @appendfsync@ are not supported on Redis version 2.8.22 and later.
--
-- * 'sCacheClusterCreateTime' - The date and time when the source cluster was created.
--
-- * 'sAutoMinorVersionUpgrade' - This parameter is currently disabled.
--
-- * 'sARN' - The ARN (Amazon Resource Name) of the snapshot.
--
-- * 'sCacheParameterGroupName' - The cache parameter group that is associated with the source cluster.
--
-- * 'sReplicationGroupDescription' - A description of the source replication group.
--
-- * 'sVPCId' - The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet group for the source cluster.
--
-- * 'sSnapshotStatus' - The status of the snapshot. Valid values: @creating@ | @available@ | @restoring@ | @copying@ | @deleting@ .
--
-- * 'sSnapshotWindow' - The daily time range during which ElastiCache takes daily snapshots of the source cluster.
--
-- * 'sCacheClusterId' - The user-supplied identifier of the source cluster.
--
-- * 'sEngine' - The name of the cache engine (@memcached@ or @redis@ ) used by the source cluster.
--
-- * 'sPreferredMaintenanceWindow' - Specifies the weekly time range during which maintenance on the cluster is performed. It is specified as a range in the format ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window is a 60 minute period. Valid values for @ddd@ are:     * @sun@      * @mon@      * @tue@      * @wed@      * @thu@      * @fri@      * @sat@  Example: @sun:23:00-mon:01:30@
--
-- * 'sTopicARN' - The Amazon Resource Name (ARN) for the topic used by the source cluster for publishing notifications.
--
-- * 'sKMSKeyId' - The ID of the KMS key used to encrypt the snapshot.
--
-- * 'sNodeSnapshots' - A list of the cache nodes in the source cluster.
--
-- * 'sCacheSubnetGroupName' - The name of the cache subnet group associated with the source cluster.
--
-- * 'sPreferredAvailabilityZone' - The name of the Availability Zone in which the source cluster is located.
--
-- * 'sNumNodeGroups' - The number of node groups (shards) in this snapshot. When restoring from a snapshot, the number of node groups (shards) in the snapshot and in the restored replication group must be the same.
--
-- * 'sSnapshotRetentionLimit' - For an automatic snapshot, the number of days for which ElastiCache retains the snapshot before deleting it. For manual snapshots, this field reflects the @SnapshotRetentionLimit@ for the source cluster when the snapshot was created. This field is otherwise ignored: Manual snapshots do not expire, and can only be deleted using the @DeleteSnapshot@ operation.  __Important__ If the value of SnapshotRetentionLimit is set to zero (0), backups are turned off.
--
-- * 'sSnapshotName' - The name of a snapshot. For an automatic snapshot, the name is system-generated. For a manual snapshot, this is the user-provided name.
--
-- * 'sPreferredOutpostARN' - The ARN (Amazon Resource Name) of the preferred outpost.
--
-- * 'sReplicationGroupId' - The unique identifier of the source replication group.
--
-- * 'sNumCacheNodes' - The number of cache nodes in the source cluster. For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20.
--
-- * 'sPort' - The port number used by each cache nodes in the source cluster.
--
-- * 'sAutomaticFailover' - Indicates the status of automatic failover for the source Redis replication group.
--
-- * 'sSnapshotSource' - Indicates whether the snapshot is from an automatic backup (@automated@ ) or was created manually (@manual@ ).
snapshot ::
  Snapshot
snapshot =
  Snapshot'
    { _sEngineVersion = Nothing,
      _sCacheNodeType = Nothing,
      _sCacheClusterCreateTime = Nothing,
      _sAutoMinorVersionUpgrade = Nothing,
      _sARN = Nothing,
      _sCacheParameterGroupName = Nothing,
      _sReplicationGroupDescription = Nothing,
      _sVPCId = Nothing,
      _sSnapshotStatus = Nothing,
      _sSnapshotWindow = Nothing,
      _sCacheClusterId = Nothing,
      _sEngine = Nothing,
      _sPreferredMaintenanceWindow = Nothing,
      _sTopicARN = Nothing,
      _sKMSKeyId = Nothing,
      _sNodeSnapshots = Nothing,
      _sCacheSubnetGroupName = Nothing,
      _sPreferredAvailabilityZone = Nothing,
      _sNumNodeGroups = Nothing,
      _sSnapshotRetentionLimit = Nothing,
      _sSnapshotName = Nothing,
      _sPreferredOutpostARN = Nothing,
      _sReplicationGroupId = Nothing,
      _sNumCacheNodes = Nothing,
      _sPort = Nothing,
      _sAutomaticFailover = Nothing,
      _sSnapshotSource = Nothing
    }

-- | The version of the cache engine version that is used by the source cluster.
sEngineVersion :: Lens' Snapshot (Maybe Text)
sEngineVersion = lens _sEngineVersion (\s a -> s {_sEngineVersion = a})

-- | The name of the compute and memory capacity node type for the source cluster. The following node types are supported by ElastiCache. Generally speaking, the current generation types provide more memory and computational power at lower cost when compared to their equivalent previous generation counterparts.     * General purpose:     * Current generation:  __M6g node types__ (available only for Redis engine version 5.0.6 onward and for Memcached engine version 1.5.16 onward). @cache.m6g.large@ , @cache.m6g.xlarge@ , @cache.m6g.2xlarge@ , @cache.m6g.4xlarge@ , @cache.m6g.8xlarge@ , @cache.m6g.12xlarge@ , @cache.m6g.16xlarge@  __M5 node types:__ @cache.m5.large@ , @cache.m5.xlarge@ , @cache.m5.2xlarge@ , @cache.m5.4xlarge@ , @cache.m5.12xlarge@ , @cache.m5.24xlarge@  __M4 node types:__ @cache.m4.large@ , @cache.m4.xlarge@ , @cache.m4.2xlarge@ , @cache.m4.4xlarge@ , @cache.m4.10xlarge@  __T3 node types:__ @cache.t3.micro@ , @cache.t3.small@ , @cache.t3.medium@  __T2 node types:__ @cache.t2.micro@ , @cache.t2.small@ , @cache.t2.medium@      * Previous generation: (not recommended) __T1 node types:__ @cache.t1.micro@  __M1 node types:__ @cache.m1.small@ , @cache.m1.medium@ , @cache.m1.large@ , @cache.m1.xlarge@  __M3 node types:__ @cache.m3.medium@ , @cache.m3.large@ , @cache.m3.xlarge@ , @cache.m3.2xlarge@      * Compute optimized:     * Previous generation: (not recommended) __C1 node types:__ @cache.c1.xlarge@      * Memory optimized:     * Current generation:  __R6g node types__ (available only for Redis engine version 5.0.6 onward and for Memcached engine version 1.5.16 onward). @cache.r6g.large@ , @cache.r6g.xlarge@ , @cache.r6g.2xlarge@ , @cache.r6g.4xlarge@ , @cache.r6g.8xlarge@ , @cache.r6g.12xlarge@ , @cache.r6g.16xlarge@  __R5 node types:__ @cache.r5.large@ , @cache.r5.xlarge@ , @cache.r5.2xlarge@ , @cache.r5.4xlarge@ , @cache.r5.12xlarge@ , @cache.r5.24xlarge@  __R4 node types:__ @cache.r4.large@ , @cache.r4.xlarge@ , @cache.r4.2xlarge@ , @cache.r4.4xlarge@ , @cache.r4.8xlarge@ , @cache.r4.16xlarge@      * Previous generation: (not recommended) __M2 node types:__ @cache.m2.xlarge@ , @cache.m2.2xlarge@ , @cache.m2.4xlarge@  __R3 node types:__ @cache.r3.large@ , @cache.r3.xlarge@ , @cache.r3.2xlarge@ , @cache.r3.4xlarge@ , @cache.r3.8xlarge@  __Additional node type info__      * All current generation instance types are created in Amazon VPC by default.     * Redis append-only files (AOF) are not supported for T1 or T2 instances.     * Redis Multi-AZ with automatic failover is not supported on T1 instances.     * Redis configuration variables @appendonly@ and @appendfsync@ are not supported on Redis version 2.8.22 and later.
sCacheNodeType :: Lens' Snapshot (Maybe Text)
sCacheNodeType = lens _sCacheNodeType (\s a -> s {_sCacheNodeType = a})

-- | The date and time when the source cluster was created.
sCacheClusterCreateTime :: Lens' Snapshot (Maybe UTCTime)
sCacheClusterCreateTime = lens _sCacheClusterCreateTime (\s a -> s {_sCacheClusterCreateTime = a}) . mapping _Time

-- | This parameter is currently disabled.
sAutoMinorVersionUpgrade :: Lens' Snapshot (Maybe Bool)
sAutoMinorVersionUpgrade = lens _sAutoMinorVersionUpgrade (\s a -> s {_sAutoMinorVersionUpgrade = a})

-- | The ARN (Amazon Resource Name) of the snapshot.
sARN :: Lens' Snapshot (Maybe Text)
sARN = lens _sARN (\s a -> s {_sARN = a})

-- | The cache parameter group that is associated with the source cluster.
sCacheParameterGroupName :: Lens' Snapshot (Maybe Text)
sCacheParameterGroupName = lens _sCacheParameterGroupName (\s a -> s {_sCacheParameterGroupName = a})

-- | A description of the source replication group.
sReplicationGroupDescription :: Lens' Snapshot (Maybe Text)
sReplicationGroupDescription = lens _sReplicationGroupDescription (\s a -> s {_sReplicationGroupDescription = a})

-- | The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet group for the source cluster.
sVPCId :: Lens' Snapshot (Maybe Text)
sVPCId = lens _sVPCId (\s a -> s {_sVPCId = a})

-- | The status of the snapshot. Valid values: @creating@ | @available@ | @restoring@ | @copying@ | @deleting@ .
sSnapshotStatus :: Lens' Snapshot (Maybe Text)
sSnapshotStatus = lens _sSnapshotStatus (\s a -> s {_sSnapshotStatus = a})

-- | The daily time range during which ElastiCache takes daily snapshots of the source cluster.
sSnapshotWindow :: Lens' Snapshot (Maybe Text)
sSnapshotWindow = lens _sSnapshotWindow (\s a -> s {_sSnapshotWindow = a})

-- | The user-supplied identifier of the source cluster.
sCacheClusterId :: Lens' Snapshot (Maybe Text)
sCacheClusterId = lens _sCacheClusterId (\s a -> s {_sCacheClusterId = a})

-- | The name of the cache engine (@memcached@ or @redis@ ) used by the source cluster.
sEngine :: Lens' Snapshot (Maybe Text)
sEngine = lens _sEngine (\s a -> s {_sEngine = a})

-- | Specifies the weekly time range during which maintenance on the cluster is performed. It is specified as a range in the format ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window is a 60 minute period. Valid values for @ddd@ are:     * @sun@      * @mon@      * @tue@      * @wed@      * @thu@      * @fri@      * @sat@  Example: @sun:23:00-mon:01:30@
sPreferredMaintenanceWindow :: Lens' Snapshot (Maybe Text)
sPreferredMaintenanceWindow = lens _sPreferredMaintenanceWindow (\s a -> s {_sPreferredMaintenanceWindow = a})

-- | The Amazon Resource Name (ARN) for the topic used by the source cluster for publishing notifications.
sTopicARN :: Lens' Snapshot (Maybe Text)
sTopicARN = lens _sTopicARN (\s a -> s {_sTopicARN = a})

-- | The ID of the KMS key used to encrypt the snapshot.
sKMSKeyId :: Lens' Snapshot (Maybe Text)
sKMSKeyId = lens _sKMSKeyId (\s a -> s {_sKMSKeyId = a})

-- | A list of the cache nodes in the source cluster.
sNodeSnapshots :: Lens' Snapshot [NodeSnapshot]
sNodeSnapshots = lens _sNodeSnapshots (\s a -> s {_sNodeSnapshots = a}) . _Default . _Coerce

-- | The name of the cache subnet group associated with the source cluster.
sCacheSubnetGroupName :: Lens' Snapshot (Maybe Text)
sCacheSubnetGroupName = lens _sCacheSubnetGroupName (\s a -> s {_sCacheSubnetGroupName = a})

-- | The name of the Availability Zone in which the source cluster is located.
sPreferredAvailabilityZone :: Lens' Snapshot (Maybe Text)
sPreferredAvailabilityZone = lens _sPreferredAvailabilityZone (\s a -> s {_sPreferredAvailabilityZone = a})

-- | The number of node groups (shards) in this snapshot. When restoring from a snapshot, the number of node groups (shards) in the snapshot and in the restored replication group must be the same.
sNumNodeGroups :: Lens' Snapshot (Maybe Int)
sNumNodeGroups = lens _sNumNodeGroups (\s a -> s {_sNumNodeGroups = a})

-- | For an automatic snapshot, the number of days for which ElastiCache retains the snapshot before deleting it. For manual snapshots, this field reflects the @SnapshotRetentionLimit@ for the source cluster when the snapshot was created. This field is otherwise ignored: Manual snapshots do not expire, and can only be deleted using the @DeleteSnapshot@ operation.  __Important__ If the value of SnapshotRetentionLimit is set to zero (0), backups are turned off.
sSnapshotRetentionLimit :: Lens' Snapshot (Maybe Int)
sSnapshotRetentionLimit = lens _sSnapshotRetentionLimit (\s a -> s {_sSnapshotRetentionLimit = a})

-- | The name of a snapshot. For an automatic snapshot, the name is system-generated. For a manual snapshot, this is the user-provided name.
sSnapshotName :: Lens' Snapshot (Maybe Text)
sSnapshotName = lens _sSnapshotName (\s a -> s {_sSnapshotName = a})

-- | The ARN (Amazon Resource Name) of the preferred outpost.
sPreferredOutpostARN :: Lens' Snapshot (Maybe Text)
sPreferredOutpostARN = lens _sPreferredOutpostARN (\s a -> s {_sPreferredOutpostARN = a})

-- | The unique identifier of the source replication group.
sReplicationGroupId :: Lens' Snapshot (Maybe Text)
sReplicationGroupId = lens _sReplicationGroupId (\s a -> s {_sReplicationGroupId = a})

-- | The number of cache nodes in the source cluster. For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20.
sNumCacheNodes :: Lens' Snapshot (Maybe Int)
sNumCacheNodes = lens _sNumCacheNodes (\s a -> s {_sNumCacheNodes = a})

-- | The port number used by each cache nodes in the source cluster.
sPort :: Lens' Snapshot (Maybe Int)
sPort = lens _sPort (\s a -> s {_sPort = a})

-- | Indicates the status of automatic failover for the source Redis replication group.
sAutomaticFailover :: Lens' Snapshot (Maybe AutomaticFailoverStatus)
sAutomaticFailover = lens _sAutomaticFailover (\s a -> s {_sAutomaticFailover = a})

-- | Indicates whether the snapshot is from an automatic backup (@automated@ ) or was created manually (@manual@ ).
sSnapshotSource :: Lens' Snapshot (Maybe Text)
sSnapshotSource = lens _sSnapshotSource (\s a -> s {_sSnapshotSource = a})

instance FromXML Snapshot where
  parseXML x =
    Snapshot'
      <$> (x .@? "EngineVersion")
      <*> (x .@? "CacheNodeType")
      <*> (x .@? "CacheClusterCreateTime")
      <*> (x .@? "AutoMinorVersionUpgrade")
      <*> (x .@? "ARN")
      <*> (x .@? "CacheParameterGroupName")
      <*> (x .@? "ReplicationGroupDescription")
      <*> (x .@? "VpcId")
      <*> (x .@? "SnapshotStatus")
      <*> (x .@? "SnapshotWindow")
      <*> (x .@? "CacheClusterId")
      <*> (x .@? "Engine")
      <*> (x .@? "PreferredMaintenanceWindow")
      <*> (x .@? "TopicArn")
      <*> (x .@? "KmsKeyId")
      <*> ( x .@? "NodeSnapshots" .!@ mempty
              >>= may (parseXMLList "NodeSnapshot")
          )
      <*> (x .@? "CacheSubnetGroupName")
      <*> (x .@? "PreferredAvailabilityZone")
      <*> (x .@? "NumNodeGroups")
      <*> (x .@? "SnapshotRetentionLimit")
      <*> (x .@? "SnapshotName")
      <*> (x .@? "PreferredOutpostArn")
      <*> (x .@? "ReplicationGroupId")
      <*> (x .@? "NumCacheNodes")
      <*> (x .@? "Port")
      <*> (x .@? "AutomaticFailover")
      <*> (x .@? "SnapshotSource")

instance Hashable Snapshot

instance NFData Snapshot
