{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElastiCache.Types.Product where

import Network.AWS.ElastiCache.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an Availability Zone in which the cluster is launched.
--
--
--
-- /See:/ 'availabilityZone' smart constructor.
newtype AvailabilityZone = AvailabilityZone'
  { _azName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AvailabilityZone' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'azName' - The name of the Availability Zone.
availabilityZone
    :: AvailabilityZone
availabilityZone = AvailabilityZone' {_azName = Nothing}


-- | The name of the Availability Zone.
azName :: Lens' AvailabilityZone (Maybe Text)
azName = lens _azName (\ s a -> s{_azName = a})

instance FromXML AvailabilityZone where
        parseXML x = AvailabilityZone' <$> (x .@? "Name")

instance Hashable AvailabilityZone where

instance NFData AvailabilityZone where

-- | Contains all of the attributes of a specific cluster.
--
--
--
-- /See:/ 'cacheCluster' smart constructor.
data CacheCluster = CacheCluster'
  { _ccEngineVersion              :: !(Maybe Text)
  , _ccCacheNodeType              :: !(Maybe Text)
  , _ccCacheNodes                 :: !(Maybe [CacheNode])
  , _ccCacheClusterCreateTime     :: !(Maybe ISO8601)
  , _ccAtRestEncryptionEnabled    :: !(Maybe Bool)
  , _ccAutoMinorVersionUpgrade    :: !(Maybe Bool)
  , _ccSecurityGroups             :: !(Maybe [SecurityGroupMembership])
  , _ccNotificationConfiguration  :: !(Maybe NotificationConfiguration)
  , _ccTransitEncryptionEnabled   :: !(Maybe Bool)
  , _ccSnapshotWindow             :: !(Maybe Text)
  , _ccCacheClusterId             :: !(Maybe Text)
  , _ccConfigurationEndpoint      :: !(Maybe Endpoint)
  , _ccEngine                     :: !(Maybe Text)
  , _ccCacheSecurityGroups        :: !(Maybe [CacheSecurityGroupMembership])
  , _ccAuthTokenEnabled           :: !(Maybe Bool)
  , _ccClientDownloadLandingPage  :: !(Maybe Text)
  , _ccPreferredMaintenanceWindow :: !(Maybe Text)
  , _ccCacheSubnetGroupName       :: !(Maybe Text)
  , _ccPreferredAvailabilityZone  :: !(Maybe Text)
  , _ccCacheParameterGroup        :: !(Maybe CacheParameterGroupStatus)
  , _ccCacheClusterStatus         :: !(Maybe Text)
  , _ccSnapshotRetentionLimit     :: !(Maybe Int)
  , _ccReplicationGroupId         :: !(Maybe Text)
  , _ccPendingModifiedValues      :: !(Maybe PendingModifiedValues)
  , _ccNumCacheNodes              :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CacheCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccEngineVersion' - The version of the cache engine that is used in this cluster.
--
-- * 'ccCacheNodeType' - The name of the compute and memory capacity node type for the cluster. The following node types are supported by ElastiCache. Generally speaking, the current generation types provide more memory and computational power at lower cost when compared to their equivalent previous generation counterparts.     * General purpose:     * Current generation:  __T2 node types:__ @cache.t2.micro@ , @cache.t2.small@ , @cache.t2.medium@  __M3 node types:__ @cache.m3.medium@ , @cache.m3.large@ , @cache.m3.xlarge@ , @cache.m3.2xlarge@  __M4 node types:__ @cache.m4.large@ , @cache.m4.xlarge@ , @cache.m4.2xlarge@ , @cache.m4.4xlarge@ , @cache.m4.10xlarge@      * Previous generation: (not recommended) __T1 node types:__ @cache.t1.micro@  __M1 node types:__ @cache.m1.small@ , @cache.m1.medium@ , @cache.m1.large@ , @cache.m1.xlarge@      * Compute optimized:     * Previous generation: (not recommended) __C1 node types:__ @cache.c1.xlarge@      * Memory optimized:     * Current generation:  __R3 node types:__ @cache.r3.large@ , @cache.r3.xlarge@ , @cache.r3.2xlarge@ , @cache.r3.4xlarge@ , @cache.r3.8xlarge@      * Previous generation: (not recommended) __M2 node types:__ @cache.m2.xlarge@ , @cache.m2.2xlarge@ , @cache.m2.4xlarge@  __Notes:__      * All T2 instances are created in an Amazon Virtual Private Cloud (Amazon VPC).     * Redis (cluster mode disabled): Redis backup/restore is not supported on T1 and T2 instances.      * Redis (cluster mode enabled): Backup/restore is not supported on T1 instances.     * Redis Append-only files (AOF) functionality is not supported for T1 or T2 instances. For a complete listing of node types and specifications, see <http://aws.amazon.com/elasticache/details Amazon ElastiCache Product Features and Details> and either <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Memcached.html#ParameterGroups.Memcached.NodeSpecific Cache Node Type-Specific Parameters for Memcached> or <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Redis.html#ParameterGroups.Redis.NodeSpecific Cache Node Type-Specific Parameters for Redis> .
--
-- * 'ccCacheNodes' - A list of cache nodes that are members of the cluster.
--
-- * 'ccCacheClusterCreateTime' - The date and time when the cluster was created.
--
-- * 'ccAtRestEncryptionEnabled' - A flag that enables encryption at-rest when set to @true@ . You cannot modify the value of @AtRestEncryptionEnabled@ after the cluster is created. To enable at-rest encryption on a cluster you must set @AtRestEncryptionEnabled@ to @true@ when you create a cluster. Default: @false@
--
-- * 'ccAutoMinorVersionUpgrade' - This parameter is currently disabled.
--
-- * 'ccSecurityGroups' - A list of VPC Security Groups associated with the cluster.
--
-- * 'ccNotificationConfiguration' - Describes a notification topic and its status. Notification topics are used for publishing ElastiCache events to subscribers using Amazon Simple Notification Service (SNS).
--
-- * 'ccTransitEncryptionEnabled' - A flag that enables in-transit encryption when set to @true@ . You cannot modify the value of @TransitEncryptionEnabled@ after the cluster is created. To enable in-transit encryption on a cluster you must set @TransitEncryptionEnabled@ to @true@ when you create a cluster. Default: @false@
--
-- * 'ccSnapshotWindow' - The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of your cluster. Example: @05:00-09:00@
--
-- * 'ccCacheClusterId' - The user-supplied identifier of the cluster. This identifier is a unique key that identifies a cluster.
--
-- * 'ccConfigurationEndpoint' - Represents a Memcached cluster endpoint which, if Automatic Discovery is enabled on the cluster, can be used by an application to connect to any node in the cluster. The configuration endpoint will always have @.cfg@ in it. Example: @mem-3.9dvc4r/.cfg/ .usw2.cache.amazonaws.com:11211@
--
-- * 'ccEngine' - The name of the cache engine (@memcached@ or @redis@ ) to be used for this cluster.
--
-- * 'ccCacheSecurityGroups' - A list of cache security group elements, composed of name and status sub-elements.
--
-- * 'ccAuthTokenEnabled' - A flag that enables using an @AuthToken@ (password) when issuing Redis commands. Default: @false@
--
-- * 'ccClientDownloadLandingPage' - The URL of the web page where you can download the latest ElastiCache client library.
--
-- * 'ccPreferredMaintenanceWindow' - Specifies the weekly time range during which maintenance on the cluster is performed. It is specified as a range in the format ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window is a 60 minute period. Valid values for @ddd@ are:     * @sun@      * @mon@      * @tue@      * @wed@      * @thu@      * @fri@      * @sat@  Example: @sun:23:00-mon:01:30@
--
-- * 'ccCacheSubnetGroupName' - The name of the cache subnet group associated with the cluster.
--
-- * 'ccPreferredAvailabilityZone' - The name of the Availability Zone in which the cluster is located or "Multiple" if the cache nodes are located in different Availability Zones.
--
-- * 'ccCacheParameterGroup' - Status of the cache parameter group.
--
-- * 'ccCacheClusterStatus' - The current state of this cluster, one of the following values: @available@ , @creating@ , @deleted@ , @deleting@ , @incompatible-network@ , @modifying@ , @rebooting cluster nodes@ , @restore-failed@ , or @snapshotting@ .
--
-- * 'ccSnapshotRetentionLimit' - The number of days for which ElastiCache retains automatic cluster snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is retained for 5 days before being deleted. /Important:/ If the value of SnapshotRetentionLimit is set to zero (0), backups are turned off.
--
-- * 'ccReplicationGroupId' - The replication group to which this cluster belongs. If this field is empty, the cluster is not associated with any replication group.
--
-- * 'ccPendingModifiedValues' - Undocumented member.
--
-- * 'ccNumCacheNodes' - The number of cache nodes in the cluster. For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20.
cacheCluster
    :: CacheCluster
cacheCluster =
  CacheCluster'
    { _ccEngineVersion = Nothing
    , _ccCacheNodeType = Nothing
    , _ccCacheNodes = Nothing
    , _ccCacheClusterCreateTime = Nothing
    , _ccAtRestEncryptionEnabled = Nothing
    , _ccAutoMinorVersionUpgrade = Nothing
    , _ccSecurityGroups = Nothing
    , _ccNotificationConfiguration = Nothing
    , _ccTransitEncryptionEnabled = Nothing
    , _ccSnapshotWindow = Nothing
    , _ccCacheClusterId = Nothing
    , _ccConfigurationEndpoint = Nothing
    , _ccEngine = Nothing
    , _ccCacheSecurityGroups = Nothing
    , _ccAuthTokenEnabled = Nothing
    , _ccClientDownloadLandingPage = Nothing
    , _ccPreferredMaintenanceWindow = Nothing
    , _ccCacheSubnetGroupName = Nothing
    , _ccPreferredAvailabilityZone = Nothing
    , _ccCacheParameterGroup = Nothing
    , _ccCacheClusterStatus = Nothing
    , _ccSnapshotRetentionLimit = Nothing
    , _ccReplicationGroupId = Nothing
    , _ccPendingModifiedValues = Nothing
    , _ccNumCacheNodes = Nothing
    }


-- | The version of the cache engine that is used in this cluster.
ccEngineVersion :: Lens' CacheCluster (Maybe Text)
ccEngineVersion = lens _ccEngineVersion (\ s a -> s{_ccEngineVersion = a})

-- | The name of the compute and memory capacity node type for the cluster. The following node types are supported by ElastiCache. Generally speaking, the current generation types provide more memory and computational power at lower cost when compared to their equivalent previous generation counterparts.     * General purpose:     * Current generation:  __T2 node types:__ @cache.t2.micro@ , @cache.t2.small@ , @cache.t2.medium@  __M3 node types:__ @cache.m3.medium@ , @cache.m3.large@ , @cache.m3.xlarge@ , @cache.m3.2xlarge@  __M4 node types:__ @cache.m4.large@ , @cache.m4.xlarge@ , @cache.m4.2xlarge@ , @cache.m4.4xlarge@ , @cache.m4.10xlarge@      * Previous generation: (not recommended) __T1 node types:__ @cache.t1.micro@  __M1 node types:__ @cache.m1.small@ , @cache.m1.medium@ , @cache.m1.large@ , @cache.m1.xlarge@      * Compute optimized:     * Previous generation: (not recommended) __C1 node types:__ @cache.c1.xlarge@      * Memory optimized:     * Current generation:  __R3 node types:__ @cache.r3.large@ , @cache.r3.xlarge@ , @cache.r3.2xlarge@ , @cache.r3.4xlarge@ , @cache.r3.8xlarge@      * Previous generation: (not recommended) __M2 node types:__ @cache.m2.xlarge@ , @cache.m2.2xlarge@ , @cache.m2.4xlarge@  __Notes:__      * All T2 instances are created in an Amazon Virtual Private Cloud (Amazon VPC).     * Redis (cluster mode disabled): Redis backup/restore is not supported on T1 and T2 instances.      * Redis (cluster mode enabled): Backup/restore is not supported on T1 instances.     * Redis Append-only files (AOF) functionality is not supported for T1 or T2 instances. For a complete listing of node types and specifications, see <http://aws.amazon.com/elasticache/details Amazon ElastiCache Product Features and Details> and either <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Memcached.html#ParameterGroups.Memcached.NodeSpecific Cache Node Type-Specific Parameters for Memcached> or <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Redis.html#ParameterGroups.Redis.NodeSpecific Cache Node Type-Specific Parameters for Redis> .
ccCacheNodeType :: Lens' CacheCluster (Maybe Text)
ccCacheNodeType = lens _ccCacheNodeType (\ s a -> s{_ccCacheNodeType = a})

-- | A list of cache nodes that are members of the cluster.
ccCacheNodes :: Lens' CacheCluster [CacheNode]
ccCacheNodes = lens _ccCacheNodes (\ s a -> s{_ccCacheNodes = a}) . _Default . _Coerce

-- | The date and time when the cluster was created.
ccCacheClusterCreateTime :: Lens' CacheCluster (Maybe UTCTime)
ccCacheClusterCreateTime = lens _ccCacheClusterCreateTime (\ s a -> s{_ccCacheClusterCreateTime = a}) . mapping _Time

-- | A flag that enables encryption at-rest when set to @true@ . You cannot modify the value of @AtRestEncryptionEnabled@ after the cluster is created. To enable at-rest encryption on a cluster you must set @AtRestEncryptionEnabled@ to @true@ when you create a cluster. Default: @false@
ccAtRestEncryptionEnabled :: Lens' CacheCluster (Maybe Bool)
ccAtRestEncryptionEnabled = lens _ccAtRestEncryptionEnabled (\ s a -> s{_ccAtRestEncryptionEnabled = a})

-- | This parameter is currently disabled.
ccAutoMinorVersionUpgrade :: Lens' CacheCluster (Maybe Bool)
ccAutoMinorVersionUpgrade = lens _ccAutoMinorVersionUpgrade (\ s a -> s{_ccAutoMinorVersionUpgrade = a})

-- | A list of VPC Security Groups associated with the cluster.
ccSecurityGroups :: Lens' CacheCluster [SecurityGroupMembership]
ccSecurityGroups = lens _ccSecurityGroups (\ s a -> s{_ccSecurityGroups = a}) . _Default . _Coerce

-- | Describes a notification topic and its status. Notification topics are used for publishing ElastiCache events to subscribers using Amazon Simple Notification Service (SNS).
ccNotificationConfiguration :: Lens' CacheCluster (Maybe NotificationConfiguration)
ccNotificationConfiguration = lens _ccNotificationConfiguration (\ s a -> s{_ccNotificationConfiguration = a})

-- | A flag that enables in-transit encryption when set to @true@ . You cannot modify the value of @TransitEncryptionEnabled@ after the cluster is created. To enable in-transit encryption on a cluster you must set @TransitEncryptionEnabled@ to @true@ when you create a cluster. Default: @false@
ccTransitEncryptionEnabled :: Lens' CacheCluster (Maybe Bool)
ccTransitEncryptionEnabled = lens _ccTransitEncryptionEnabled (\ s a -> s{_ccTransitEncryptionEnabled = a})

-- | The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of your cluster. Example: @05:00-09:00@
ccSnapshotWindow :: Lens' CacheCluster (Maybe Text)
ccSnapshotWindow = lens _ccSnapshotWindow (\ s a -> s{_ccSnapshotWindow = a})

-- | The user-supplied identifier of the cluster. This identifier is a unique key that identifies a cluster.
ccCacheClusterId :: Lens' CacheCluster (Maybe Text)
ccCacheClusterId = lens _ccCacheClusterId (\ s a -> s{_ccCacheClusterId = a})

-- | Represents a Memcached cluster endpoint which, if Automatic Discovery is enabled on the cluster, can be used by an application to connect to any node in the cluster. The configuration endpoint will always have @.cfg@ in it. Example: @mem-3.9dvc4r/.cfg/ .usw2.cache.amazonaws.com:11211@
ccConfigurationEndpoint :: Lens' CacheCluster (Maybe Endpoint)
ccConfigurationEndpoint = lens _ccConfigurationEndpoint (\ s a -> s{_ccConfigurationEndpoint = a})

-- | The name of the cache engine (@memcached@ or @redis@ ) to be used for this cluster.
ccEngine :: Lens' CacheCluster (Maybe Text)
ccEngine = lens _ccEngine (\ s a -> s{_ccEngine = a})

-- | A list of cache security group elements, composed of name and status sub-elements.
ccCacheSecurityGroups :: Lens' CacheCluster [CacheSecurityGroupMembership]
ccCacheSecurityGroups = lens _ccCacheSecurityGroups (\ s a -> s{_ccCacheSecurityGroups = a}) . _Default . _Coerce

-- | A flag that enables using an @AuthToken@ (password) when issuing Redis commands. Default: @false@
ccAuthTokenEnabled :: Lens' CacheCluster (Maybe Bool)
ccAuthTokenEnabled = lens _ccAuthTokenEnabled (\ s a -> s{_ccAuthTokenEnabled = a})

-- | The URL of the web page where you can download the latest ElastiCache client library.
ccClientDownloadLandingPage :: Lens' CacheCluster (Maybe Text)
ccClientDownloadLandingPage = lens _ccClientDownloadLandingPage (\ s a -> s{_ccClientDownloadLandingPage = a})

-- | Specifies the weekly time range during which maintenance on the cluster is performed. It is specified as a range in the format ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window is a 60 minute period. Valid values for @ddd@ are:     * @sun@      * @mon@      * @tue@      * @wed@      * @thu@      * @fri@      * @sat@  Example: @sun:23:00-mon:01:30@
ccPreferredMaintenanceWindow :: Lens' CacheCluster (Maybe Text)
ccPreferredMaintenanceWindow = lens _ccPreferredMaintenanceWindow (\ s a -> s{_ccPreferredMaintenanceWindow = a})

-- | The name of the cache subnet group associated with the cluster.
ccCacheSubnetGroupName :: Lens' CacheCluster (Maybe Text)
ccCacheSubnetGroupName = lens _ccCacheSubnetGroupName (\ s a -> s{_ccCacheSubnetGroupName = a})

-- | The name of the Availability Zone in which the cluster is located or "Multiple" if the cache nodes are located in different Availability Zones.
ccPreferredAvailabilityZone :: Lens' CacheCluster (Maybe Text)
ccPreferredAvailabilityZone = lens _ccPreferredAvailabilityZone (\ s a -> s{_ccPreferredAvailabilityZone = a})

-- | Status of the cache parameter group.
ccCacheParameterGroup :: Lens' CacheCluster (Maybe CacheParameterGroupStatus)
ccCacheParameterGroup = lens _ccCacheParameterGroup (\ s a -> s{_ccCacheParameterGroup = a})

-- | The current state of this cluster, one of the following values: @available@ , @creating@ , @deleted@ , @deleting@ , @incompatible-network@ , @modifying@ , @rebooting cluster nodes@ , @restore-failed@ , or @snapshotting@ .
ccCacheClusterStatus :: Lens' CacheCluster (Maybe Text)
ccCacheClusterStatus = lens _ccCacheClusterStatus (\ s a -> s{_ccCacheClusterStatus = a})

-- | The number of days for which ElastiCache retains automatic cluster snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is retained for 5 days before being deleted. /Important:/ If the value of SnapshotRetentionLimit is set to zero (0), backups are turned off.
ccSnapshotRetentionLimit :: Lens' CacheCluster (Maybe Int)
ccSnapshotRetentionLimit = lens _ccSnapshotRetentionLimit (\ s a -> s{_ccSnapshotRetentionLimit = a})

-- | The replication group to which this cluster belongs. If this field is empty, the cluster is not associated with any replication group.
ccReplicationGroupId :: Lens' CacheCluster (Maybe Text)
ccReplicationGroupId = lens _ccReplicationGroupId (\ s a -> s{_ccReplicationGroupId = a})

-- | Undocumented member.
ccPendingModifiedValues :: Lens' CacheCluster (Maybe PendingModifiedValues)
ccPendingModifiedValues = lens _ccPendingModifiedValues (\ s a -> s{_ccPendingModifiedValues = a})

-- | The number of cache nodes in the cluster. For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20.
ccNumCacheNodes :: Lens' CacheCluster (Maybe Int)
ccNumCacheNodes = lens _ccNumCacheNodes (\ s a -> s{_ccNumCacheNodes = a})

instance FromXML CacheCluster where
        parseXML x
          = CacheCluster' <$>
              (x .@? "EngineVersion") <*> (x .@? "CacheNodeType")
                <*>
                (x .@? "CacheNodes" .!@ mempty >>=
                   may (parseXMLList "CacheNode"))
                <*> (x .@? "CacheClusterCreateTime")
                <*> (x .@? "AtRestEncryptionEnabled")
                <*> (x .@? "AutoMinorVersionUpgrade")
                <*>
                (x .@? "SecurityGroups" .!@ mempty >>=
                   may (parseXMLList "member"))
                <*> (x .@? "NotificationConfiguration")
                <*> (x .@? "TransitEncryptionEnabled")
                <*> (x .@? "SnapshotWindow")
                <*> (x .@? "CacheClusterId")
                <*> (x .@? "ConfigurationEndpoint")
                <*> (x .@? "Engine")
                <*>
                (x .@? "CacheSecurityGroups" .!@ mempty >>=
                   may (parseXMLList "CacheSecurityGroup"))
                <*> (x .@? "AuthTokenEnabled")
                <*> (x .@? "ClientDownloadLandingPage")
                <*> (x .@? "PreferredMaintenanceWindow")
                <*> (x .@? "CacheSubnetGroupName")
                <*> (x .@? "PreferredAvailabilityZone")
                <*> (x .@? "CacheParameterGroup")
                <*> (x .@? "CacheClusterStatus")
                <*> (x .@? "SnapshotRetentionLimit")
                <*> (x .@? "ReplicationGroupId")
                <*> (x .@? "PendingModifiedValues")
                <*> (x .@? "NumCacheNodes")

instance Hashable CacheCluster where

instance NFData CacheCluster where

-- | Provides all of the details about a particular cache engine version.
--
--
--
-- /See:/ 'cacheEngineVersion' smart constructor.
data CacheEngineVersion = CacheEngineVersion'
  { _cevEngineVersion                 :: !(Maybe Text)
  , _cevCacheParameterGroupFamily     :: !(Maybe Text)
  , _cevCacheEngineDescription        :: !(Maybe Text)
  , _cevEngine                        :: !(Maybe Text)
  , _cevCacheEngineVersionDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CacheEngineVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cevEngineVersion' - The version number of the cache engine.
--
-- * 'cevCacheParameterGroupFamily' - The name of the cache parameter group family associated with this cache engine. Valid values are: @memcached1.4@ | @redis2.6@ | @redis2.8@ | @redis3.2@
--
-- * 'cevCacheEngineDescription' - The description of the cache engine.
--
-- * 'cevEngine' - The name of the cache engine.
--
-- * 'cevCacheEngineVersionDescription' - The description of the cache engine version.
cacheEngineVersion
    :: CacheEngineVersion
cacheEngineVersion =
  CacheEngineVersion'
    { _cevEngineVersion = Nothing
    , _cevCacheParameterGroupFamily = Nothing
    , _cevCacheEngineDescription = Nothing
    , _cevEngine = Nothing
    , _cevCacheEngineVersionDescription = Nothing
    }


-- | The version number of the cache engine.
cevEngineVersion :: Lens' CacheEngineVersion (Maybe Text)
cevEngineVersion = lens _cevEngineVersion (\ s a -> s{_cevEngineVersion = a})

-- | The name of the cache parameter group family associated with this cache engine. Valid values are: @memcached1.4@ | @redis2.6@ | @redis2.8@ | @redis3.2@
cevCacheParameterGroupFamily :: Lens' CacheEngineVersion (Maybe Text)
cevCacheParameterGroupFamily = lens _cevCacheParameterGroupFamily (\ s a -> s{_cevCacheParameterGroupFamily = a})

-- | The description of the cache engine.
cevCacheEngineDescription :: Lens' CacheEngineVersion (Maybe Text)
cevCacheEngineDescription = lens _cevCacheEngineDescription (\ s a -> s{_cevCacheEngineDescription = a})

-- | The name of the cache engine.
cevEngine :: Lens' CacheEngineVersion (Maybe Text)
cevEngine = lens _cevEngine (\ s a -> s{_cevEngine = a})

-- | The description of the cache engine version.
cevCacheEngineVersionDescription :: Lens' CacheEngineVersion (Maybe Text)
cevCacheEngineVersionDescription = lens _cevCacheEngineVersionDescription (\ s a -> s{_cevCacheEngineVersionDescription = a})

instance FromXML CacheEngineVersion where
        parseXML x
          = CacheEngineVersion' <$>
              (x .@? "EngineVersion") <*>
                (x .@? "CacheParameterGroupFamily")
                <*> (x .@? "CacheEngineDescription")
                <*> (x .@? "Engine")
                <*> (x .@? "CacheEngineVersionDescription")

instance Hashable CacheEngineVersion where

instance NFData CacheEngineVersion where

-- | Represents an individual cache node within a cluster. Each cache node runs its own instance of the cluster's protocol-compliant caching software - either Memcached or Redis.
--
--
-- The following node types are supported by ElastiCache. Generally speaking, the current generation types provide more memory and computational power at lower cost when compared to their equivalent previous generation counterparts.
--
--     * General purpose:
--
--     * Current generation:
--
-- __T2 node types:__ @cache.t2.micro@ , @cache.t2.small@ , @cache.t2.medium@
--
-- __M3 node types:__ @cache.m3.medium@ , @cache.m3.large@ , @cache.m3.xlarge@ , @cache.m3.2xlarge@
--
-- __M4 node types:__ @cache.m4.large@ , @cache.m4.xlarge@ , @cache.m4.2xlarge@ , @cache.m4.4xlarge@ , @cache.m4.10xlarge@
--
--     * Previous generation: (not recommended)
--
-- __T1 node types:__ @cache.t1.micro@
--
-- __M1 node types:__ @cache.m1.small@ , @cache.m1.medium@ , @cache.m1.large@ , @cache.m1.xlarge@
--
--
--
--     * Compute optimized:
--
--     * Previous generation: (not recommended)
--
-- __C1 node types:__ @cache.c1.xlarge@
--
--
--
--     * Memory optimized:
--
--     * Current generation:
--
-- __R3 node types:__ @cache.r3.large@ , @cache.r3.xlarge@ , @cache.r3.2xlarge@ , @cache.r3.4xlarge@ , @cache.r3.8xlarge@
--
--     * Previous generation: (not recommended)
--
-- __M2 node types:__ @cache.m2.xlarge@ , @cache.m2.2xlarge@ , @cache.m2.4xlarge@
--
--
--
--
--
-- __Notes:__
--
--     * All T2 instances are created in an Amazon Virtual Private Cloud (Amazon VPC).
--
--     * Redis (cluster mode disabled): Redis backup/restore is not supported on T1 and T2 instances.
--
--     * Redis (cluster mode enabled): Backup/restore is not supported on T1 instances.
--
--     * Redis Append-only files (AOF) functionality is not supported for T1 or T2 instances.
--
--
--
-- For a complete listing of node types and specifications, see <http://aws.amazon.com/elasticache/details Amazon ElastiCache Product Features and Details> and either <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Memcached.html#ParameterGroups.Memcached.NodeSpecific Cache Node Type-Specific Parameters for Memcached> or <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Redis.html#ParameterGroups.Redis.NodeSpecific Cache Node Type-Specific Parameters for Redis> .
--
--
-- /See:/ 'cacheNode' smart constructor.
data CacheNode = CacheNode'
  { _cnSourceCacheNodeId        :: !(Maybe Text)
  , _cnParameterGroupStatus     :: !(Maybe Text)
  , _cnCacheNodeCreateTime      :: !(Maybe ISO8601)
  , _cnCustomerAvailabilityZone :: !(Maybe Text)
  , _cnCacheNodeId              :: !(Maybe Text)
  , _cnCacheNodeStatus          :: !(Maybe Text)
  , _cnEndpoint                 :: !(Maybe Endpoint)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CacheNode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cnSourceCacheNodeId' - The ID of the primary node to which this read replica node is synchronized. If this field is empty, this node is not associated with a primary cluster.
--
-- * 'cnParameterGroupStatus' - The status of the parameter group applied to this cache node.
--
-- * 'cnCacheNodeCreateTime' - The date and time when the cache node was created.
--
-- * 'cnCustomerAvailabilityZone' - The Availability Zone where this node was created and now resides.
--
-- * 'cnCacheNodeId' - The cache node identifier. A node ID is a numeric identifier (0001, 0002, etc.). The combination of cluster ID and node ID uniquely identifies every cache node used in a customer's AWS account.
--
-- * 'cnCacheNodeStatus' - The current state of this cache node.
--
-- * 'cnEndpoint' - The hostname for connecting to this cache node.
cacheNode
    :: CacheNode
cacheNode =
  CacheNode'
    { _cnSourceCacheNodeId = Nothing
    , _cnParameterGroupStatus = Nothing
    , _cnCacheNodeCreateTime = Nothing
    , _cnCustomerAvailabilityZone = Nothing
    , _cnCacheNodeId = Nothing
    , _cnCacheNodeStatus = Nothing
    , _cnEndpoint = Nothing
    }


-- | The ID of the primary node to which this read replica node is synchronized. If this field is empty, this node is not associated with a primary cluster.
cnSourceCacheNodeId :: Lens' CacheNode (Maybe Text)
cnSourceCacheNodeId = lens _cnSourceCacheNodeId (\ s a -> s{_cnSourceCacheNodeId = a})

-- | The status of the parameter group applied to this cache node.
cnParameterGroupStatus :: Lens' CacheNode (Maybe Text)
cnParameterGroupStatus = lens _cnParameterGroupStatus (\ s a -> s{_cnParameterGroupStatus = a})

-- | The date and time when the cache node was created.
cnCacheNodeCreateTime :: Lens' CacheNode (Maybe UTCTime)
cnCacheNodeCreateTime = lens _cnCacheNodeCreateTime (\ s a -> s{_cnCacheNodeCreateTime = a}) . mapping _Time

-- | The Availability Zone where this node was created and now resides.
cnCustomerAvailabilityZone :: Lens' CacheNode (Maybe Text)
cnCustomerAvailabilityZone = lens _cnCustomerAvailabilityZone (\ s a -> s{_cnCustomerAvailabilityZone = a})

-- | The cache node identifier. A node ID is a numeric identifier (0001, 0002, etc.). The combination of cluster ID and node ID uniquely identifies every cache node used in a customer's AWS account.
cnCacheNodeId :: Lens' CacheNode (Maybe Text)
cnCacheNodeId = lens _cnCacheNodeId (\ s a -> s{_cnCacheNodeId = a})

-- | The current state of this cache node.
cnCacheNodeStatus :: Lens' CacheNode (Maybe Text)
cnCacheNodeStatus = lens _cnCacheNodeStatus (\ s a -> s{_cnCacheNodeStatus = a})

-- | The hostname for connecting to this cache node.
cnEndpoint :: Lens' CacheNode (Maybe Endpoint)
cnEndpoint = lens _cnEndpoint (\ s a -> s{_cnEndpoint = a})

instance FromXML CacheNode where
        parseXML x
          = CacheNode' <$>
              (x .@? "SourceCacheNodeId") <*>
                (x .@? "ParameterGroupStatus")
                <*> (x .@? "CacheNodeCreateTime")
                <*> (x .@? "CustomerAvailabilityZone")
                <*> (x .@? "CacheNodeId")
                <*> (x .@? "CacheNodeStatus")
                <*> (x .@? "Endpoint")

instance Hashable CacheNode where

instance NFData CacheNode where

-- | A parameter that has a different value for each cache node type it is applied to. For example, in a Redis cluster, a @cache.m1.large@ cache node type would have a larger @maxmemory@ value than a @cache.m1.small@ type.
--
--
--
-- /See:/ 'cacheNodeTypeSpecificParameter' smart constructor.
data CacheNodeTypeSpecificParameter = CacheNodeTypeSpecificParameter'
  { _cntspCacheNodeTypeSpecificValues :: !(Maybe [CacheNodeTypeSpecificValue])
  , _cntspMinimumEngineVersion        :: !(Maybe Text)
  , _cntspSource                      :: !(Maybe Text)
  , _cntspIsModifiable                :: !(Maybe Bool)
  , _cntspDataType                    :: !(Maybe Text)
  , _cntspAllowedValues               :: !(Maybe Text)
  , _cntspParameterName               :: !(Maybe Text)
  , _cntspDescription                 :: !(Maybe Text)
  , _cntspChangeType                  :: !(Maybe ChangeType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CacheNodeTypeSpecificParameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cntspCacheNodeTypeSpecificValues' - A list of cache node types and their corresponding values for this parameter.
--
-- * 'cntspMinimumEngineVersion' - The earliest cache engine version to which the parameter can apply.
--
-- * 'cntspSource' - The source of the parameter value.
--
-- * 'cntspIsModifiable' - Indicates whether (@true@ ) or not (@false@ ) the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed.
--
-- * 'cntspDataType' - The valid data type for the parameter.
--
-- * 'cntspAllowedValues' - The valid range of values for the parameter.
--
-- * 'cntspParameterName' - The name of the parameter.
--
-- * 'cntspDescription' - A description of the parameter.
--
-- * 'cntspChangeType' - Indicates whether a change to the parameter is applied immediately or requires a reboot for the change to be applied. You can force a reboot or wait until the next maintenance window's reboot. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/Clusters.Rebooting.html Rebooting a Cluster> .
cacheNodeTypeSpecificParameter
    :: CacheNodeTypeSpecificParameter
cacheNodeTypeSpecificParameter =
  CacheNodeTypeSpecificParameter'
    { _cntspCacheNodeTypeSpecificValues = Nothing
    , _cntspMinimumEngineVersion = Nothing
    , _cntspSource = Nothing
    , _cntspIsModifiable = Nothing
    , _cntspDataType = Nothing
    , _cntspAllowedValues = Nothing
    , _cntspParameterName = Nothing
    , _cntspDescription = Nothing
    , _cntspChangeType = Nothing
    }


-- | A list of cache node types and their corresponding values for this parameter.
cntspCacheNodeTypeSpecificValues :: Lens' CacheNodeTypeSpecificParameter [CacheNodeTypeSpecificValue]
cntspCacheNodeTypeSpecificValues = lens _cntspCacheNodeTypeSpecificValues (\ s a -> s{_cntspCacheNodeTypeSpecificValues = a}) . _Default . _Coerce

-- | The earliest cache engine version to which the parameter can apply.
cntspMinimumEngineVersion :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspMinimumEngineVersion = lens _cntspMinimumEngineVersion (\ s a -> s{_cntspMinimumEngineVersion = a})

-- | The source of the parameter value.
cntspSource :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspSource = lens _cntspSource (\ s a -> s{_cntspSource = a})

-- | Indicates whether (@true@ ) or not (@false@ ) the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed.
cntspIsModifiable :: Lens' CacheNodeTypeSpecificParameter (Maybe Bool)
cntspIsModifiable = lens _cntspIsModifiable (\ s a -> s{_cntspIsModifiable = a})

-- | The valid data type for the parameter.
cntspDataType :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspDataType = lens _cntspDataType (\ s a -> s{_cntspDataType = a})

-- | The valid range of values for the parameter.
cntspAllowedValues :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspAllowedValues = lens _cntspAllowedValues (\ s a -> s{_cntspAllowedValues = a})

-- | The name of the parameter.
cntspParameterName :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspParameterName = lens _cntspParameterName (\ s a -> s{_cntspParameterName = a})

-- | A description of the parameter.
cntspDescription :: Lens' CacheNodeTypeSpecificParameter (Maybe Text)
cntspDescription = lens _cntspDescription (\ s a -> s{_cntspDescription = a})

-- | Indicates whether a change to the parameter is applied immediately or requires a reboot for the change to be applied. You can force a reboot or wait until the next maintenance window's reboot. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/Clusters.Rebooting.html Rebooting a Cluster> .
cntspChangeType :: Lens' CacheNodeTypeSpecificParameter (Maybe ChangeType)
cntspChangeType = lens _cntspChangeType (\ s a -> s{_cntspChangeType = a})

instance FromXML CacheNodeTypeSpecificParameter where
        parseXML x
          = CacheNodeTypeSpecificParameter' <$>
              (x .@? "CacheNodeTypeSpecificValues" .!@ mempty >>=
                 may (parseXMLList "CacheNodeTypeSpecificValue"))
                <*> (x .@? "MinimumEngineVersion")
                <*> (x .@? "Source")
                <*> (x .@? "IsModifiable")
                <*> (x .@? "DataType")
                <*> (x .@? "AllowedValues")
                <*> (x .@? "ParameterName")
                <*> (x .@? "Description")
                <*> (x .@? "ChangeType")

instance Hashable CacheNodeTypeSpecificParameter
         where

instance NFData CacheNodeTypeSpecificParameter where

-- | A value that applies only to a certain cache node type.
--
--
--
-- /See:/ 'cacheNodeTypeSpecificValue' smart constructor.
data CacheNodeTypeSpecificValue = CacheNodeTypeSpecificValue'
  { _cntsvCacheNodeType :: !(Maybe Text)
  , _cntsvValue         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CacheNodeTypeSpecificValue' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cntsvCacheNodeType' - The cache node type for which this value applies.
--
-- * 'cntsvValue' - The value for the cache node type.
cacheNodeTypeSpecificValue
    :: CacheNodeTypeSpecificValue
cacheNodeTypeSpecificValue =
  CacheNodeTypeSpecificValue'
    {_cntsvCacheNodeType = Nothing, _cntsvValue = Nothing}


-- | The cache node type for which this value applies.
cntsvCacheNodeType :: Lens' CacheNodeTypeSpecificValue (Maybe Text)
cntsvCacheNodeType = lens _cntsvCacheNodeType (\ s a -> s{_cntsvCacheNodeType = a})

-- | The value for the cache node type.
cntsvValue :: Lens' CacheNodeTypeSpecificValue (Maybe Text)
cntsvValue = lens _cntsvValue (\ s a -> s{_cntsvValue = a})

instance FromXML CacheNodeTypeSpecificValue where
        parseXML x
          = CacheNodeTypeSpecificValue' <$>
              (x .@? "CacheNodeType") <*> (x .@? "Value")

instance Hashable CacheNodeTypeSpecificValue where

instance NFData CacheNodeTypeSpecificValue where

-- | Represents the output of a @CreateCacheParameterGroup@ operation.
--
--
--
-- /See:/ 'cacheParameterGroup' smart constructor.
data CacheParameterGroup = CacheParameterGroup'
  { _cpgCacheParameterGroupFamily :: !(Maybe Text)
  , _cpgCacheParameterGroupName   :: !(Maybe Text)
  , _cpgDescription               :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CacheParameterGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpgCacheParameterGroupFamily' - The name of the cache parameter group family that this cache parameter group is compatible with. Valid values are: @memcached1.4@ | @redis2.6@ | @redis2.8@ | @redis3.2@
--
-- * 'cpgCacheParameterGroupName' - The name of the cache parameter group.
--
-- * 'cpgDescription' - The description for this cache parameter group.
cacheParameterGroup
    :: CacheParameterGroup
cacheParameterGroup =
  CacheParameterGroup'
    { _cpgCacheParameterGroupFamily = Nothing
    , _cpgCacheParameterGroupName = Nothing
    , _cpgDescription = Nothing
    }


-- | The name of the cache parameter group family that this cache parameter group is compatible with. Valid values are: @memcached1.4@ | @redis2.6@ | @redis2.8@ | @redis3.2@
cpgCacheParameterGroupFamily :: Lens' CacheParameterGroup (Maybe Text)
cpgCacheParameterGroupFamily = lens _cpgCacheParameterGroupFamily (\ s a -> s{_cpgCacheParameterGroupFamily = a})

-- | The name of the cache parameter group.
cpgCacheParameterGroupName :: Lens' CacheParameterGroup (Maybe Text)
cpgCacheParameterGroupName = lens _cpgCacheParameterGroupName (\ s a -> s{_cpgCacheParameterGroupName = a})

-- | The description for this cache parameter group.
cpgDescription :: Lens' CacheParameterGroup (Maybe Text)
cpgDescription = lens _cpgDescription (\ s a -> s{_cpgDescription = a})

instance FromXML CacheParameterGroup where
        parseXML x
          = CacheParameterGroup' <$>
              (x .@? "CacheParameterGroupFamily") <*>
                (x .@? "CacheParameterGroupName")
                <*> (x .@? "Description")

instance Hashable CacheParameterGroup where

instance NFData CacheParameterGroup where

-- | Represents the output of one of the following operations:
--
--
--     * @ModifyCacheParameterGroup@
--
--     * @ResetCacheParameterGroup@
--
--
--
--
-- /See:/ 'cacheParameterGroupNameMessage' smart constructor.
newtype CacheParameterGroupNameMessage = CacheParameterGroupNameMessage'
  { _cpgnmCacheParameterGroupName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CacheParameterGroupNameMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpgnmCacheParameterGroupName' - The name of the cache parameter group.
cacheParameterGroupNameMessage
    :: CacheParameterGroupNameMessage
cacheParameterGroupNameMessage =
  CacheParameterGroupNameMessage' {_cpgnmCacheParameterGroupName = Nothing}


-- | The name of the cache parameter group.
cpgnmCacheParameterGroupName :: Lens' CacheParameterGroupNameMessage (Maybe Text)
cpgnmCacheParameterGroupName = lens _cpgnmCacheParameterGroupName (\ s a -> s{_cpgnmCacheParameterGroupName = a})

instance FromXML CacheParameterGroupNameMessage where
        parseXML x
          = CacheParameterGroupNameMessage' <$>
              (x .@? "CacheParameterGroupName")

instance Hashable CacheParameterGroupNameMessage
         where

instance NFData CacheParameterGroupNameMessage where

-- | Status of the cache parameter group.
--
--
--
-- /See:/ 'cacheParameterGroupStatus' smart constructor.
data CacheParameterGroupStatus = CacheParameterGroupStatus'
  { _cpgsCacheParameterGroupName :: !(Maybe Text)
  , _cpgsCacheNodeIdsToReboot    :: !(Maybe [Text])
  , _cpgsParameterApplyStatus    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CacheParameterGroupStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpgsCacheParameterGroupName' - The name of the cache parameter group.
--
-- * 'cpgsCacheNodeIdsToReboot' - A list of the cache node IDs which need to be rebooted for parameter changes to be applied. A node ID is a numeric identifier (0001, 0002, etc.).
--
-- * 'cpgsParameterApplyStatus' - The status of parameter updates.
cacheParameterGroupStatus
    :: CacheParameterGroupStatus
cacheParameterGroupStatus =
  CacheParameterGroupStatus'
    { _cpgsCacheParameterGroupName = Nothing
    , _cpgsCacheNodeIdsToReboot = Nothing
    , _cpgsParameterApplyStatus = Nothing
    }


-- | The name of the cache parameter group.
cpgsCacheParameterGroupName :: Lens' CacheParameterGroupStatus (Maybe Text)
cpgsCacheParameterGroupName = lens _cpgsCacheParameterGroupName (\ s a -> s{_cpgsCacheParameterGroupName = a})

-- | A list of the cache node IDs which need to be rebooted for parameter changes to be applied. A node ID is a numeric identifier (0001, 0002, etc.).
cpgsCacheNodeIdsToReboot :: Lens' CacheParameterGroupStatus [Text]
cpgsCacheNodeIdsToReboot = lens _cpgsCacheNodeIdsToReboot (\ s a -> s{_cpgsCacheNodeIdsToReboot = a}) . _Default . _Coerce

-- | The status of parameter updates.
cpgsParameterApplyStatus :: Lens' CacheParameterGroupStatus (Maybe Text)
cpgsParameterApplyStatus = lens _cpgsParameterApplyStatus (\ s a -> s{_cpgsParameterApplyStatus = a})

instance FromXML CacheParameterGroupStatus where
        parseXML x
          = CacheParameterGroupStatus' <$>
              (x .@? "CacheParameterGroupName") <*>
                (x .@? "CacheNodeIdsToReboot" .!@ mempty >>=
                   may (parseXMLList "CacheNodeId"))
                <*> (x .@? "ParameterApplyStatus")

instance Hashable CacheParameterGroupStatus where

instance NFData CacheParameterGroupStatus where

-- | Represents the output of one of the following operations:
--
--
--     * @AuthorizeCacheSecurityGroupIngress@
--
--     * @CreateCacheSecurityGroup@
--
--     * @RevokeCacheSecurityGroupIngress@
--
--
--
--
-- /See:/ 'cacheSecurityGroup' smart constructor.
data CacheSecurityGroup = CacheSecurityGroup'
  { _csgCacheSecurityGroupName :: !(Maybe Text)
  , _csgOwnerId                :: !(Maybe Text)
  , _csgEC2SecurityGroups      :: !(Maybe [EC2SecurityGroup])
  , _csgDescription            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CacheSecurityGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csgCacheSecurityGroupName' - The name of the cache security group.
--
-- * 'csgOwnerId' - The AWS account ID of the cache security group owner.
--
-- * 'csgEC2SecurityGroups' - A list of Amazon EC2 security groups that are associated with this cache security group.
--
-- * 'csgDescription' - The description of the cache security group.
cacheSecurityGroup
    :: CacheSecurityGroup
cacheSecurityGroup =
  CacheSecurityGroup'
    { _csgCacheSecurityGroupName = Nothing
    , _csgOwnerId = Nothing
    , _csgEC2SecurityGroups = Nothing
    , _csgDescription = Nothing
    }


-- | The name of the cache security group.
csgCacheSecurityGroupName :: Lens' CacheSecurityGroup (Maybe Text)
csgCacheSecurityGroupName = lens _csgCacheSecurityGroupName (\ s a -> s{_csgCacheSecurityGroupName = a})

-- | The AWS account ID of the cache security group owner.
csgOwnerId :: Lens' CacheSecurityGroup (Maybe Text)
csgOwnerId = lens _csgOwnerId (\ s a -> s{_csgOwnerId = a})

-- | A list of Amazon EC2 security groups that are associated with this cache security group.
csgEC2SecurityGroups :: Lens' CacheSecurityGroup [EC2SecurityGroup]
csgEC2SecurityGroups = lens _csgEC2SecurityGroups (\ s a -> s{_csgEC2SecurityGroups = a}) . _Default . _Coerce

-- | The description of the cache security group.
csgDescription :: Lens' CacheSecurityGroup (Maybe Text)
csgDescription = lens _csgDescription (\ s a -> s{_csgDescription = a})

instance FromXML CacheSecurityGroup where
        parseXML x
          = CacheSecurityGroup' <$>
              (x .@? "CacheSecurityGroupName") <*>
                (x .@? "OwnerId")
                <*>
                (x .@? "EC2SecurityGroups" .!@ mempty >>=
                   may (parseXMLList "EC2SecurityGroup"))
                <*> (x .@? "Description")

instance Hashable CacheSecurityGroup where

instance NFData CacheSecurityGroup where

-- | Represents a cluster's status within a particular cache security group.
--
--
--
-- /See:/ 'cacheSecurityGroupMembership' smart constructor.
data CacheSecurityGroupMembership = CacheSecurityGroupMembership'
  { _csgmStatus                 :: !(Maybe Text)
  , _csgmCacheSecurityGroupName :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CacheSecurityGroupMembership' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csgmStatus' - The membership status in the cache security group. The status changes when a cache security group is modified, or when the cache security groups assigned to a cluster are modified.
--
-- * 'csgmCacheSecurityGroupName' - The name of the cache security group.
cacheSecurityGroupMembership
    :: CacheSecurityGroupMembership
cacheSecurityGroupMembership =
  CacheSecurityGroupMembership'
    {_csgmStatus = Nothing, _csgmCacheSecurityGroupName = Nothing}


-- | The membership status in the cache security group. The status changes when a cache security group is modified, or when the cache security groups assigned to a cluster are modified.
csgmStatus :: Lens' CacheSecurityGroupMembership (Maybe Text)
csgmStatus = lens _csgmStatus (\ s a -> s{_csgmStatus = a})

-- | The name of the cache security group.
csgmCacheSecurityGroupName :: Lens' CacheSecurityGroupMembership (Maybe Text)
csgmCacheSecurityGroupName = lens _csgmCacheSecurityGroupName (\ s a -> s{_csgmCacheSecurityGroupName = a})

instance FromXML CacheSecurityGroupMembership where
        parseXML x
          = CacheSecurityGroupMembership' <$>
              (x .@? "Status") <*> (x .@? "CacheSecurityGroupName")

instance Hashable CacheSecurityGroupMembership where

instance NFData CacheSecurityGroupMembership where

-- | Represents the output of one of the following operations:
--
--
--     * @CreateCacheSubnetGroup@
--
--     * @ModifyCacheSubnetGroup@
--
--
--
--
-- /See:/ 'cacheSubnetGroup' smart constructor.
data CacheSubnetGroup = CacheSubnetGroup'
  { _csgVPCId                       :: !(Maybe Text)
  , _csgSubnets                     :: !(Maybe [Subnet])
  , _csgCacheSubnetGroupName        :: !(Maybe Text)
  , _csgCacheSubnetGroupDescription :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CacheSubnetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csgVPCId' - The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet group.
--
-- * 'csgSubnets' - A list of subnets associated with the cache subnet group.
--
-- * 'csgCacheSubnetGroupName' - The name of the cache subnet group.
--
-- * 'csgCacheSubnetGroupDescription' - The description of the cache subnet group.
cacheSubnetGroup
    :: CacheSubnetGroup
cacheSubnetGroup =
  CacheSubnetGroup'
    { _csgVPCId = Nothing
    , _csgSubnets = Nothing
    , _csgCacheSubnetGroupName = Nothing
    , _csgCacheSubnetGroupDescription = Nothing
    }


-- | The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet group.
csgVPCId :: Lens' CacheSubnetGroup (Maybe Text)
csgVPCId = lens _csgVPCId (\ s a -> s{_csgVPCId = a})

-- | A list of subnets associated with the cache subnet group.
csgSubnets :: Lens' CacheSubnetGroup [Subnet]
csgSubnets = lens _csgSubnets (\ s a -> s{_csgSubnets = a}) . _Default . _Coerce

-- | The name of the cache subnet group.
csgCacheSubnetGroupName :: Lens' CacheSubnetGroup (Maybe Text)
csgCacheSubnetGroupName = lens _csgCacheSubnetGroupName (\ s a -> s{_csgCacheSubnetGroupName = a})

-- | The description of the cache subnet group.
csgCacheSubnetGroupDescription :: Lens' CacheSubnetGroup (Maybe Text)
csgCacheSubnetGroupDescription = lens _csgCacheSubnetGroupDescription (\ s a -> s{_csgCacheSubnetGroupDescription = a})

instance FromXML CacheSubnetGroup where
        parseXML x
          = CacheSubnetGroup' <$>
              (x .@? "VpcId") <*>
                (x .@? "Subnets" .!@ mempty >>=
                   may (parseXMLList "Subnet"))
                <*> (x .@? "CacheSubnetGroupName")
                <*> (x .@? "CacheSubnetGroupDescription")

instance Hashable CacheSubnetGroup where

instance NFData CacheSubnetGroup where

-- | Provides ownership and status information for an Amazon EC2 security group.
--
--
--
-- /See:/ 'ec2SecurityGroup' smart constructor.
data EC2SecurityGroup = EC2SecurityGroup'
  { _esgStatus                  :: !(Maybe Text)
  , _esgEC2SecurityGroupOwnerId :: !(Maybe Text)
  , _esgEC2SecurityGroupName    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EC2SecurityGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esgStatus' - The status of the Amazon EC2 security group.
--
-- * 'esgEC2SecurityGroupOwnerId' - The AWS account ID of the Amazon EC2 security group owner.
--
-- * 'esgEC2SecurityGroupName' - The name of the Amazon EC2 security group.
ec2SecurityGroup
    :: EC2SecurityGroup
ec2SecurityGroup =
  EC2SecurityGroup'
    { _esgStatus = Nothing
    , _esgEC2SecurityGroupOwnerId = Nothing
    , _esgEC2SecurityGroupName = Nothing
    }


-- | The status of the Amazon EC2 security group.
esgStatus :: Lens' EC2SecurityGroup (Maybe Text)
esgStatus = lens _esgStatus (\ s a -> s{_esgStatus = a})

-- | The AWS account ID of the Amazon EC2 security group owner.
esgEC2SecurityGroupOwnerId :: Lens' EC2SecurityGroup (Maybe Text)
esgEC2SecurityGroupOwnerId = lens _esgEC2SecurityGroupOwnerId (\ s a -> s{_esgEC2SecurityGroupOwnerId = a})

-- | The name of the Amazon EC2 security group.
esgEC2SecurityGroupName :: Lens' EC2SecurityGroup (Maybe Text)
esgEC2SecurityGroupName = lens _esgEC2SecurityGroupName (\ s a -> s{_esgEC2SecurityGroupName = a})

instance FromXML EC2SecurityGroup where
        parseXML x
          = EC2SecurityGroup' <$>
              (x .@? "Status") <*>
                (x .@? "EC2SecurityGroupOwnerId")
                <*> (x .@? "EC2SecurityGroupName")

instance Hashable EC2SecurityGroup where

instance NFData EC2SecurityGroup where

-- | Represents the information required for client programs to connect to a cache node.
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
-- * 'eAddress' - The DNS hostname of the cache node.
--
-- * 'ePort' - The port number that the cache engine is listening on.
endpoint
    :: Endpoint
endpoint = Endpoint' {_eAddress = Nothing, _ePort = Nothing}


-- | The DNS hostname of the cache node.
eAddress :: Lens' Endpoint (Maybe Text)
eAddress = lens _eAddress (\ s a -> s{_eAddress = a})

-- | The port number that the cache engine is listening on.
ePort :: Lens' Endpoint (Maybe Int)
ePort = lens _ePort (\ s a -> s{_ePort = a})

instance FromXML Endpoint where
        parseXML x
          = Endpoint' <$> (x .@? "Address") <*> (x .@? "Port")

instance Hashable Endpoint where

instance NFData Endpoint where

-- | Represents the output of a @DescribeEngineDefaultParameters@ operation.
--
--
--
-- /See:/ 'engineDefaults' smart constructor.
data EngineDefaults = EngineDefaults'
  { _edCacheParameterGroupFamily :: !(Maybe Text)
  , _edCacheNodeTypeSpecificParameters :: !(Maybe [CacheNodeTypeSpecificParameter])
  , _edMarker :: !(Maybe Text)
  , _edParameters :: !(Maybe [Parameter])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EngineDefaults' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'edCacheParameterGroupFamily' - Specifies the name of the cache parameter group family to which the engine default parameters apply. Valid values are: @memcached1.4@ | @redis2.6@ | @redis2.8@ | @redis3.2@
--
-- * 'edCacheNodeTypeSpecificParameters' - A list of parameters specific to a particular cache node type. Each element in the list contains detailed information about one parameter.
--
-- * 'edMarker' - Provides an identifier to allow retrieval of paginated results.
--
-- * 'edParameters' - Contains a list of engine default parameters.
engineDefaults
    :: EngineDefaults
engineDefaults =
  EngineDefaults'
    { _edCacheParameterGroupFamily = Nothing
    , _edCacheNodeTypeSpecificParameters = Nothing
    , _edMarker = Nothing
    , _edParameters = Nothing
    }


-- | Specifies the name of the cache parameter group family to which the engine default parameters apply. Valid values are: @memcached1.4@ | @redis2.6@ | @redis2.8@ | @redis3.2@
edCacheParameterGroupFamily :: Lens' EngineDefaults (Maybe Text)
edCacheParameterGroupFamily = lens _edCacheParameterGroupFamily (\ s a -> s{_edCacheParameterGroupFamily = a})

-- | A list of parameters specific to a particular cache node type. Each element in the list contains detailed information about one parameter.
edCacheNodeTypeSpecificParameters :: Lens' EngineDefaults [CacheNodeTypeSpecificParameter]
edCacheNodeTypeSpecificParameters = lens _edCacheNodeTypeSpecificParameters (\ s a -> s{_edCacheNodeTypeSpecificParameters = a}) . _Default . _Coerce

-- | Provides an identifier to allow retrieval of paginated results.
edMarker :: Lens' EngineDefaults (Maybe Text)
edMarker = lens _edMarker (\ s a -> s{_edMarker = a})

-- | Contains a list of engine default parameters.
edParameters :: Lens' EngineDefaults [Parameter]
edParameters = lens _edParameters (\ s a -> s{_edParameters = a}) . _Default . _Coerce

instance FromXML EngineDefaults where
        parseXML x
          = EngineDefaults' <$>
              (x .@? "CacheParameterGroupFamily") <*>
                (x .@? "CacheNodeTypeSpecificParameters" .!@ mempty
                   >>=
                   may (parseXMLList "CacheNodeTypeSpecificParameter"))
                <*> (x .@? "Marker")
                <*>
                (x .@? "Parameters" .!@ mempty >>=
                   may (parseXMLList "Parameter"))

instance Hashable EngineDefaults where

instance NFData EngineDefaults where

-- | Represents a single occurrence of something interesting within the system. Some examples of events are creating a cluster, adding or removing a cache node, or rebooting a node.
--
--
--
-- /See:/ 'event' smart constructor.
data Event = Event'
  { _eSourceType       :: !(Maybe SourceType)
  , _eSourceIdentifier :: !(Maybe Text)
  , _eDate             :: !(Maybe ISO8601)
  , _eMessage          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Event' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eSourceType' - Specifies the origin of this event - a cluster, a parameter group, a security group, etc.
--
-- * 'eSourceIdentifier' - The identifier for the source of the event. For example, if the event occurred at the cluster level, the identifier would be the name of the cluster.
--
-- * 'eDate' - The date and time when the event occurred.
--
-- * 'eMessage' - The text of the event.
event
    :: Event
event =
  Event'
    { _eSourceType = Nothing
    , _eSourceIdentifier = Nothing
    , _eDate = Nothing
    , _eMessage = Nothing
    }


-- | Specifies the origin of this event - a cluster, a parameter group, a security group, etc.
eSourceType :: Lens' Event (Maybe SourceType)
eSourceType = lens _eSourceType (\ s a -> s{_eSourceType = a})

-- | The identifier for the source of the event. For example, if the event occurred at the cluster level, the identifier would be the name of the cluster.
eSourceIdentifier :: Lens' Event (Maybe Text)
eSourceIdentifier = lens _eSourceIdentifier (\ s a -> s{_eSourceIdentifier = a})

-- | The date and time when the event occurred.
eDate :: Lens' Event (Maybe UTCTime)
eDate = lens _eDate (\ s a -> s{_eDate = a}) . mapping _Time

-- | The text of the event.
eMessage :: Lens' Event (Maybe Text)
eMessage = lens _eMessage (\ s a -> s{_eMessage = a})

instance FromXML Event where
        parseXML x
          = Event' <$>
              (x .@? "SourceType") <*> (x .@? "SourceIdentifier")
                <*> (x .@? "Date")
                <*> (x .@? "Message")

instance Hashable Event where

instance NFData Event where

-- | Represents a collection of cache nodes in a replication group. One node in the node group is the read/write primary node. All the other nodes are read-only Replica nodes.
--
--
--
-- /See:/ 'nodeGroup' smart constructor.
data NodeGroup = NodeGroup'
  { _ngStatus           :: !(Maybe Text)
  , _ngPrimaryEndpoint  :: !(Maybe Endpoint)
  , _ngSlots            :: !(Maybe Text)
  , _ngNodeGroupMembers :: !(Maybe [NodeGroupMember])
  , _ngNodeGroupId      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NodeGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ngStatus' - The current state of this replication group - @creating@ , @available@ , etc.
--
-- * 'ngPrimaryEndpoint' - The endpoint of the primary node in this node group (shard).
--
-- * 'ngSlots' - The keyspace for this node group (shard).
--
-- * 'ngNodeGroupMembers' - A list containing information about individual nodes within the node group (shard).
--
-- * 'ngNodeGroupId' - The identifier for the node group (shard). A Redis (cluster mode disabled) replication group contains only 1 node group; therefore, the node group ID is 0001. A Redis (cluster mode enabled) replication group contains 1 to 15 node groups numbered 0001 to 0015.
nodeGroup
    :: NodeGroup
nodeGroup =
  NodeGroup'
    { _ngStatus = Nothing
    , _ngPrimaryEndpoint = Nothing
    , _ngSlots = Nothing
    , _ngNodeGroupMembers = Nothing
    , _ngNodeGroupId = Nothing
    }


-- | The current state of this replication group - @creating@ , @available@ , etc.
ngStatus :: Lens' NodeGroup (Maybe Text)
ngStatus = lens _ngStatus (\ s a -> s{_ngStatus = a})

-- | The endpoint of the primary node in this node group (shard).
ngPrimaryEndpoint :: Lens' NodeGroup (Maybe Endpoint)
ngPrimaryEndpoint = lens _ngPrimaryEndpoint (\ s a -> s{_ngPrimaryEndpoint = a})

-- | The keyspace for this node group (shard).
ngSlots :: Lens' NodeGroup (Maybe Text)
ngSlots = lens _ngSlots (\ s a -> s{_ngSlots = a})

-- | A list containing information about individual nodes within the node group (shard).
ngNodeGroupMembers :: Lens' NodeGroup [NodeGroupMember]
ngNodeGroupMembers = lens _ngNodeGroupMembers (\ s a -> s{_ngNodeGroupMembers = a}) . _Default . _Coerce

-- | The identifier for the node group (shard). A Redis (cluster mode disabled) replication group contains only 1 node group; therefore, the node group ID is 0001. A Redis (cluster mode enabled) replication group contains 1 to 15 node groups numbered 0001 to 0015.
ngNodeGroupId :: Lens' NodeGroup (Maybe Text)
ngNodeGroupId = lens _ngNodeGroupId (\ s a -> s{_ngNodeGroupId = a})

instance FromXML NodeGroup where
        parseXML x
          = NodeGroup' <$>
              (x .@? "Status") <*> (x .@? "PrimaryEndpoint") <*>
                (x .@? "Slots")
                <*>
                (x .@? "NodeGroupMembers" .!@ mempty >>=
                   may (parseXMLList "NodeGroupMember"))
                <*> (x .@? "NodeGroupId")

instance Hashable NodeGroup where

instance NFData NodeGroup where

-- | Node group (shard) configuration options. Each node group (shard) configuration has the following: @Slots@ , @PrimaryAvailabilityZone@ , @ReplicaAvailabilityZones@ , @ReplicaCount@ .
--
--
--
-- /See:/ 'nodeGroupConfiguration' smart constructor.
data NodeGroupConfiguration = NodeGroupConfiguration'
  { _ngcSlots                    :: !(Maybe Text)
  , _ngcReplicaCount             :: !(Maybe Int)
  , _ngcPrimaryAvailabilityZone  :: !(Maybe Text)
  , _ngcReplicaAvailabilityZones :: !(Maybe [Text])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NodeGroupConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ngcSlots' - A string that specifies the keyspace for a particular node group. Keyspaces range from 0 to 16,383. The string is in the format @startkey-endkey@ . Example: @"0-3999"@
--
-- * 'ngcReplicaCount' - The number of read replica nodes in this node group (shard).
--
-- * 'ngcPrimaryAvailabilityZone' - The Availability Zone where the primary node of this node group (shard) is launched.
--
-- * 'ngcReplicaAvailabilityZones' - A list of Availability Zones to be used for the read replicas. The number of Availability Zones in this list must match the value of @ReplicaCount@ or @ReplicasPerNodeGroup@ if not specified.
nodeGroupConfiguration
    :: NodeGroupConfiguration
nodeGroupConfiguration =
  NodeGroupConfiguration'
    { _ngcSlots = Nothing
    , _ngcReplicaCount = Nothing
    , _ngcPrimaryAvailabilityZone = Nothing
    , _ngcReplicaAvailabilityZones = Nothing
    }


-- | A string that specifies the keyspace for a particular node group. Keyspaces range from 0 to 16,383. The string is in the format @startkey-endkey@ . Example: @"0-3999"@
ngcSlots :: Lens' NodeGroupConfiguration (Maybe Text)
ngcSlots = lens _ngcSlots (\ s a -> s{_ngcSlots = a})

-- | The number of read replica nodes in this node group (shard).
ngcReplicaCount :: Lens' NodeGroupConfiguration (Maybe Int)
ngcReplicaCount = lens _ngcReplicaCount (\ s a -> s{_ngcReplicaCount = a})

-- | The Availability Zone where the primary node of this node group (shard) is launched.
ngcPrimaryAvailabilityZone :: Lens' NodeGroupConfiguration (Maybe Text)
ngcPrimaryAvailabilityZone = lens _ngcPrimaryAvailabilityZone (\ s a -> s{_ngcPrimaryAvailabilityZone = a})

-- | A list of Availability Zones to be used for the read replicas. The number of Availability Zones in this list must match the value of @ReplicaCount@ or @ReplicasPerNodeGroup@ if not specified.
ngcReplicaAvailabilityZones :: Lens' NodeGroupConfiguration [Text]
ngcReplicaAvailabilityZones = lens _ngcReplicaAvailabilityZones (\ s a -> s{_ngcReplicaAvailabilityZones = a}) . _Default . _Coerce

instance FromXML NodeGroupConfiguration where
        parseXML x
          = NodeGroupConfiguration' <$>
              (x .@? "Slots") <*> (x .@? "ReplicaCount") <*>
                (x .@? "PrimaryAvailabilityZone")
                <*>
                (x .@? "ReplicaAvailabilityZones" .!@ mempty >>=
                   may (parseXMLList "AvailabilityZone"))

instance Hashable NodeGroupConfiguration where

instance NFData NodeGroupConfiguration where

instance ToQuery NodeGroupConfiguration where
        toQuery NodeGroupConfiguration'{..}
          = mconcat
              ["Slots" =: _ngcSlots,
               "ReplicaCount" =: _ngcReplicaCount,
               "PrimaryAvailabilityZone" =:
                 _ngcPrimaryAvailabilityZone,
               "ReplicaAvailabilityZones" =:
                 toQuery
                   (toQueryList "AvailabilityZone" <$>
                      _ngcReplicaAvailabilityZones)]

-- | Represents a single node within a node group (shard).
--
--
--
-- /See:/ 'nodeGroupMember' smart constructor.
data NodeGroupMember = NodeGroupMember'
  { _ngmCacheClusterId            :: !(Maybe Text)
  , _ngmCacheNodeId               :: !(Maybe Text)
  , _ngmPreferredAvailabilityZone :: !(Maybe Text)
  , _ngmCurrentRole               :: !(Maybe Text)
  , _ngmReadEndpoint              :: !(Maybe Endpoint)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NodeGroupMember' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ngmCacheClusterId' - The ID of the cluster to which the node belongs.
--
-- * 'ngmCacheNodeId' - The ID of the node within its cluster. A node ID is a numeric identifier (0001, 0002, etc.).
--
-- * 'ngmPreferredAvailabilityZone' - The name of the Availability Zone in which the node is located.
--
-- * 'ngmCurrentRole' - The role that is currently assigned to the node - @primary@ or @replica@ .
--
-- * 'ngmReadEndpoint' - Undocumented member.
nodeGroupMember
    :: NodeGroupMember
nodeGroupMember =
  NodeGroupMember'
    { _ngmCacheClusterId = Nothing
    , _ngmCacheNodeId = Nothing
    , _ngmPreferredAvailabilityZone = Nothing
    , _ngmCurrentRole = Nothing
    , _ngmReadEndpoint = Nothing
    }


-- | The ID of the cluster to which the node belongs.
ngmCacheClusterId :: Lens' NodeGroupMember (Maybe Text)
ngmCacheClusterId = lens _ngmCacheClusterId (\ s a -> s{_ngmCacheClusterId = a})

-- | The ID of the node within its cluster. A node ID is a numeric identifier (0001, 0002, etc.).
ngmCacheNodeId :: Lens' NodeGroupMember (Maybe Text)
ngmCacheNodeId = lens _ngmCacheNodeId (\ s a -> s{_ngmCacheNodeId = a})

-- | The name of the Availability Zone in which the node is located.
ngmPreferredAvailabilityZone :: Lens' NodeGroupMember (Maybe Text)
ngmPreferredAvailabilityZone = lens _ngmPreferredAvailabilityZone (\ s a -> s{_ngmPreferredAvailabilityZone = a})

-- | The role that is currently assigned to the node - @primary@ or @replica@ .
ngmCurrentRole :: Lens' NodeGroupMember (Maybe Text)
ngmCurrentRole = lens _ngmCurrentRole (\ s a -> s{_ngmCurrentRole = a})

-- | Undocumented member.
ngmReadEndpoint :: Lens' NodeGroupMember (Maybe Endpoint)
ngmReadEndpoint = lens _ngmReadEndpoint (\ s a -> s{_ngmReadEndpoint = a})

instance FromXML NodeGroupMember where
        parseXML x
          = NodeGroupMember' <$>
              (x .@? "CacheClusterId") <*> (x .@? "CacheNodeId")
                <*> (x .@? "PreferredAvailabilityZone")
                <*> (x .@? "CurrentRole")
                <*> (x .@? "ReadEndpoint")

instance Hashable NodeGroupMember where

instance NFData NodeGroupMember where

-- | Represents an individual cache node in a snapshot of a cluster.
--
--
--
-- /See:/ 'nodeSnapshot' smart constructor.
data NodeSnapshot = NodeSnapshot'
  { _nsNodeGroupConfiguration :: !(Maybe NodeGroupConfiguration)
  , _nsCacheNodeCreateTime    :: !(Maybe ISO8601)
  , _nsCacheClusterId         :: !(Maybe Text)
  , _nsCacheNodeId            :: !(Maybe Text)
  , _nsNodeGroupId            :: !(Maybe Text)
  , _nsSnapshotCreateTime     :: !(Maybe ISO8601)
  , _nsCacheSize              :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'NodeSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nsNodeGroupConfiguration' - The configuration for the source node group (shard).
--
-- * 'nsCacheNodeCreateTime' - The date and time when the cache node was created in the source cluster.
--
-- * 'nsCacheClusterId' - A unique identifier for the source cluster.
--
-- * 'nsCacheNodeId' - The cache node identifier for the node in the source cluster.
--
-- * 'nsNodeGroupId' - A unique identifier for the source node group (shard).
--
-- * 'nsSnapshotCreateTime' - The date and time when the source node's metadata and cache data set was obtained for the snapshot.
--
-- * 'nsCacheSize' - The size of the cache on the source cache node.
nodeSnapshot
    :: NodeSnapshot
nodeSnapshot =
  NodeSnapshot'
    { _nsNodeGroupConfiguration = Nothing
    , _nsCacheNodeCreateTime = Nothing
    , _nsCacheClusterId = Nothing
    , _nsCacheNodeId = Nothing
    , _nsNodeGroupId = Nothing
    , _nsSnapshotCreateTime = Nothing
    , _nsCacheSize = Nothing
    }


-- | The configuration for the source node group (shard).
nsNodeGroupConfiguration :: Lens' NodeSnapshot (Maybe NodeGroupConfiguration)
nsNodeGroupConfiguration = lens _nsNodeGroupConfiguration (\ s a -> s{_nsNodeGroupConfiguration = a})

-- | The date and time when the cache node was created in the source cluster.
nsCacheNodeCreateTime :: Lens' NodeSnapshot (Maybe UTCTime)
nsCacheNodeCreateTime = lens _nsCacheNodeCreateTime (\ s a -> s{_nsCacheNodeCreateTime = a}) . mapping _Time

-- | A unique identifier for the source cluster.
nsCacheClusterId :: Lens' NodeSnapshot (Maybe Text)
nsCacheClusterId = lens _nsCacheClusterId (\ s a -> s{_nsCacheClusterId = a})

-- | The cache node identifier for the node in the source cluster.
nsCacheNodeId :: Lens' NodeSnapshot (Maybe Text)
nsCacheNodeId = lens _nsCacheNodeId (\ s a -> s{_nsCacheNodeId = a})

-- | A unique identifier for the source node group (shard).
nsNodeGroupId :: Lens' NodeSnapshot (Maybe Text)
nsNodeGroupId = lens _nsNodeGroupId (\ s a -> s{_nsNodeGroupId = a})

-- | The date and time when the source node's metadata and cache data set was obtained for the snapshot.
nsSnapshotCreateTime :: Lens' NodeSnapshot (Maybe UTCTime)
nsSnapshotCreateTime = lens _nsSnapshotCreateTime (\ s a -> s{_nsSnapshotCreateTime = a}) . mapping _Time

-- | The size of the cache on the source cache node.
nsCacheSize :: Lens' NodeSnapshot (Maybe Text)
nsCacheSize = lens _nsCacheSize (\ s a -> s{_nsCacheSize = a})

instance FromXML NodeSnapshot where
        parseXML x
          = NodeSnapshot' <$>
              (x .@? "NodeGroupConfiguration") <*>
                (x .@? "CacheNodeCreateTime")
                <*> (x .@? "CacheClusterId")
                <*> (x .@? "CacheNodeId")
                <*> (x .@? "NodeGroupId")
                <*> (x .@? "SnapshotCreateTime")
                <*> (x .@? "CacheSize")

instance Hashable NodeSnapshot where

instance NFData NodeSnapshot where

-- | Describes a notification topic and its status. Notification topics are used for publishing ElastiCache events to subscribers using Amazon Simple Notification Service (SNS).
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

instance FromXML NotificationConfiguration where
        parseXML x
          = NotificationConfiguration' <$>
              (x .@? "TopicStatus") <*> (x .@? "TopicArn")

instance Hashable NotificationConfiguration where

instance NFData NotificationConfiguration where

-- | Describes an individual setting that controls some aspect of ElastiCache behavior.
--
--
--
-- /See:/ 'parameter' smart constructor.
data Parameter = Parameter'
  { _pParameterValue       :: !(Maybe Text)
  , _pMinimumEngineVersion :: !(Maybe Text)
  , _pSource               :: !(Maybe Text)
  , _pIsModifiable         :: !(Maybe Bool)
  , _pDataType             :: !(Maybe Text)
  , _pAllowedValues        :: !(Maybe Text)
  , _pParameterName        :: !(Maybe Text)
  , _pDescription          :: !(Maybe Text)
  , _pChangeType           :: !(Maybe ChangeType)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Parameter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pParameterValue' - The value of the parameter.
--
-- * 'pMinimumEngineVersion' - The earliest cache engine version to which the parameter can apply.
--
-- * 'pSource' - The source of the parameter.
--
-- * 'pIsModifiable' - Indicates whether (@true@ ) or not (@false@ ) the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed.
--
-- * 'pDataType' - The valid data type for the parameter.
--
-- * 'pAllowedValues' - The valid range of values for the parameter.
--
-- * 'pParameterName' - The name of the parameter.
--
-- * 'pDescription' - A description of the parameter.
--
-- * 'pChangeType' - Indicates whether a change to the parameter is applied immediately or requires a reboot for the change to be applied. You can force a reboot or wait until the next maintenance window's reboot. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/Clusters.Rebooting.html Rebooting a Cluster> .
parameter
    :: Parameter
parameter =
  Parameter'
    { _pParameterValue = Nothing
    , _pMinimumEngineVersion = Nothing
    , _pSource = Nothing
    , _pIsModifiable = Nothing
    , _pDataType = Nothing
    , _pAllowedValues = Nothing
    , _pParameterName = Nothing
    , _pDescription = Nothing
    , _pChangeType = Nothing
    }


-- | The value of the parameter.
pParameterValue :: Lens' Parameter (Maybe Text)
pParameterValue = lens _pParameterValue (\ s a -> s{_pParameterValue = a})

-- | The earliest cache engine version to which the parameter can apply.
pMinimumEngineVersion :: Lens' Parameter (Maybe Text)
pMinimumEngineVersion = lens _pMinimumEngineVersion (\ s a -> s{_pMinimumEngineVersion = a})

-- | The source of the parameter.
pSource :: Lens' Parameter (Maybe Text)
pSource = lens _pSource (\ s a -> s{_pSource = a})

-- | Indicates whether (@true@ ) or not (@false@ ) the parameter can be modified. Some parameters have security or operational implications that prevent them from being changed.
pIsModifiable :: Lens' Parameter (Maybe Bool)
pIsModifiable = lens _pIsModifiable (\ s a -> s{_pIsModifiable = a})

-- | The valid data type for the parameter.
pDataType :: Lens' Parameter (Maybe Text)
pDataType = lens _pDataType (\ s a -> s{_pDataType = a})

-- | The valid range of values for the parameter.
pAllowedValues :: Lens' Parameter (Maybe Text)
pAllowedValues = lens _pAllowedValues (\ s a -> s{_pAllowedValues = a})

-- | The name of the parameter.
pParameterName :: Lens' Parameter (Maybe Text)
pParameterName = lens _pParameterName (\ s a -> s{_pParameterName = a})

-- | A description of the parameter.
pDescription :: Lens' Parameter (Maybe Text)
pDescription = lens _pDescription (\ s a -> s{_pDescription = a})

-- | Indicates whether a change to the parameter is applied immediately or requires a reboot for the change to be applied. You can force a reboot or wait until the next maintenance window's reboot. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/Clusters.Rebooting.html Rebooting a Cluster> .
pChangeType :: Lens' Parameter (Maybe ChangeType)
pChangeType = lens _pChangeType (\ s a -> s{_pChangeType = a})

instance FromXML Parameter where
        parseXML x
          = Parameter' <$>
              (x .@? "ParameterValue") <*>
                (x .@? "MinimumEngineVersion")
                <*> (x .@? "Source")
                <*> (x .@? "IsModifiable")
                <*> (x .@? "DataType")
                <*> (x .@? "AllowedValues")
                <*> (x .@? "ParameterName")
                <*> (x .@? "Description")
                <*> (x .@? "ChangeType")

instance Hashable Parameter where

instance NFData Parameter where

-- | Describes a name-value pair that is used to update the value of a parameter.
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

instance ToQuery ParameterNameValue where
        toQuery ParameterNameValue'{..}
          = mconcat
              ["ParameterValue" =: _pnvParameterValue,
               "ParameterName" =: _pnvParameterName]

-- | A group of settings that are applied to the cluster in the future, or that are currently being applied.
--
--
--
-- /See:/ 'pendingModifiedValues' smart constructor.
data PendingModifiedValues = PendingModifiedValues'
  { _pmvEngineVersion        :: !(Maybe Text)
  , _pmvCacheNodeType        :: !(Maybe Text)
  , _pmvCacheNodeIdsToRemove :: !(Maybe [Text])
  , _pmvNumCacheNodes        :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PendingModifiedValues' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmvEngineVersion' - The new cache engine version that the cluster runs.
--
-- * 'pmvCacheNodeType' - The cache node type that this cluster or replication group is scaled to.
--
-- * 'pmvCacheNodeIdsToRemove' - A list of cache node IDs that are being removed (or will be removed) from the cluster. A node ID is a numeric identifier (0001, 0002, etc.).
--
-- * 'pmvNumCacheNodes' - The new number of cache nodes for the cluster. For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20.
pendingModifiedValues
    :: PendingModifiedValues
pendingModifiedValues =
  PendingModifiedValues'
    { _pmvEngineVersion = Nothing
    , _pmvCacheNodeType = Nothing
    , _pmvCacheNodeIdsToRemove = Nothing
    , _pmvNumCacheNodes = Nothing
    }


-- | The new cache engine version that the cluster runs.
pmvEngineVersion :: Lens' PendingModifiedValues (Maybe Text)
pmvEngineVersion = lens _pmvEngineVersion (\ s a -> s{_pmvEngineVersion = a})

-- | The cache node type that this cluster or replication group is scaled to.
pmvCacheNodeType :: Lens' PendingModifiedValues (Maybe Text)
pmvCacheNodeType = lens _pmvCacheNodeType (\ s a -> s{_pmvCacheNodeType = a})

-- | A list of cache node IDs that are being removed (or will be removed) from the cluster. A node ID is a numeric identifier (0001, 0002, etc.).
pmvCacheNodeIdsToRemove :: Lens' PendingModifiedValues [Text]
pmvCacheNodeIdsToRemove = lens _pmvCacheNodeIdsToRemove (\ s a -> s{_pmvCacheNodeIdsToRemove = a}) . _Default . _Coerce

-- | The new number of cache nodes for the cluster. For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20.
pmvNumCacheNodes :: Lens' PendingModifiedValues (Maybe Int)
pmvNumCacheNodes = lens _pmvNumCacheNodes (\ s a -> s{_pmvNumCacheNodes = a})

instance FromXML PendingModifiedValues where
        parseXML x
          = PendingModifiedValues' <$>
              (x .@? "EngineVersion") <*> (x .@? "CacheNodeType")
                <*>
                (x .@? "CacheNodeIdsToRemove" .!@ mempty >>=
                   may (parseXMLList "CacheNodeId"))
                <*> (x .@? "NumCacheNodes")

instance Hashable PendingModifiedValues where

instance NFData PendingModifiedValues where

-- | Contains the specific price and frequency of a recurring charges for a reserved cache node, or for a reserved cache node offering.
--
--
--
-- /See:/ 'recurringCharge' smart constructor.
data RecurringCharge = RecurringCharge'
  { _rcRecurringChargeFrequency :: !(Maybe Text)
  , _rcRecurringChargeAmount    :: !(Maybe Double)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RecurringCharge' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcRecurringChargeFrequency' - The frequency of the recurring charge.
--
-- * 'rcRecurringChargeAmount' - The monetary amount of the recurring charge.
recurringCharge
    :: RecurringCharge
recurringCharge =
  RecurringCharge'
    {_rcRecurringChargeFrequency = Nothing, _rcRecurringChargeAmount = Nothing}


-- | The frequency of the recurring charge.
rcRecurringChargeFrequency :: Lens' RecurringCharge (Maybe Text)
rcRecurringChargeFrequency = lens _rcRecurringChargeFrequency (\ s a -> s{_rcRecurringChargeFrequency = a})

-- | The monetary amount of the recurring charge.
rcRecurringChargeAmount :: Lens' RecurringCharge (Maybe Double)
rcRecurringChargeAmount = lens _rcRecurringChargeAmount (\ s a -> s{_rcRecurringChargeAmount = a})

instance FromXML RecurringCharge where
        parseXML x
          = RecurringCharge' <$>
              (x .@? "RecurringChargeFrequency") <*>
                (x .@? "RecurringChargeAmount")

instance Hashable RecurringCharge where

instance NFData RecurringCharge where

-- | Contains all of the attributes of a specific Redis replication group.
--
--
--
-- /See:/ 'replicationGroup' smart constructor.
data ReplicationGroup = ReplicationGroup'
  { _rgStatus :: !(Maybe Text)
  , _rgCacheNodeType :: !(Maybe Text)
  , _rgNodeGroups :: !(Maybe [NodeGroup])
  , _rgSnapshottingClusterId :: !(Maybe Text)
  , _rgClusterEnabled :: !(Maybe Bool)
  , _rgAtRestEncryptionEnabled :: !(Maybe Bool)
  , _rgTransitEncryptionEnabled :: !(Maybe Bool)
  , _rgSnapshotWindow :: !(Maybe Text)
  , _rgConfigurationEndpoint :: !(Maybe Endpoint)
  , _rgAuthTokenEnabled :: !(Maybe Bool)
  , _rgMemberClusters :: !(Maybe [Text])
  , _rgSnapshotRetentionLimit :: !(Maybe Int)
  , _rgDescription :: !(Maybe Text)
  , _rgReplicationGroupId :: !(Maybe Text)
  , _rgPendingModifiedValues :: !(Maybe ReplicationGroupPendingModifiedValues)
  , _rgAutomaticFailover :: !(Maybe AutomaticFailoverStatus)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplicationGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgStatus' - The current state of this replication group - @creating@ , @available@ , @modifying@ , @deleting@ , @create-failed@ , @snapshotting@ .
--
-- * 'rgCacheNodeType' - The name of the compute and memory capacity node type for each node in the replication group.
--
-- * 'rgNodeGroups' - A list of node groups in this replication group. For Redis (cluster mode disabled) replication groups, this is a single-element list. For Redis (cluster mode enabled) replication groups, the list contains an entry for each node group (shard).
--
-- * 'rgSnapshottingClusterId' - The cluster ID that is used as the daily snapshot source for the replication group.
--
-- * 'rgClusterEnabled' - A flag indicating whether or not this replication group is cluster enabled; i.e., whether its data can be partitioned across multiple shards (API/CLI: node groups). Valid values: @true@ | @false@
--
-- * 'rgAtRestEncryptionEnabled' - A flag that enables encryption at-rest when set to @true@ . You cannot modify the value of @AtRestEncryptionEnabled@ after the cluster is created. To enable encryption at-rest on a cluster you must set @AtRestEncryptionEnabled@ to @true@ when you create a cluster. Default: @false@
--
-- * 'rgTransitEncryptionEnabled' - A flag that enables in-transit encryption when set to @true@ . You cannot modify the value of @TransitEncryptionEnabled@ after the cluster is created. To enable in-transit encryption on a cluster you must set @TransitEncryptionEnabled@ to @true@ when you create a cluster. Default: @false@
--
-- * 'rgSnapshotWindow' - The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of your node group (shard). Example: @05:00-09:00@  If you do not specify this parameter, ElastiCache automatically chooses an appropriate time range.
--
-- * 'rgConfigurationEndpoint' - The configuration endpoint for this replication group. Use the configuration endpoint to connect to this replication group.
--
-- * 'rgAuthTokenEnabled' - A flag that enables using an @AuthToken@ (password) when issuing Redis commands. Default: @false@
--
-- * 'rgMemberClusters' - The identifiers of all the nodes that are part of this replication group.
--
-- * 'rgSnapshotRetentionLimit' - The number of days for which ElastiCache retains automatic cluster snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is retained for 5 days before being deleted. /Important:/ If the value of @SnapshotRetentionLimit@ is set to zero (0), backups are turned off.
--
-- * 'rgDescription' - The user supplied description of the replication group.
--
-- * 'rgReplicationGroupId' - The identifier for the replication group.
--
-- * 'rgPendingModifiedValues' - A group of settings to be applied to the replication group, either immediately or during the next maintenance window.
--
-- * 'rgAutomaticFailover' - Indicates the status of Multi-AZ with automatic failover for this Redis replication group. Amazon ElastiCache for Redis does not support Multi-AZ with automatic failover on:     * Redis versions earlier than 2.8.6.     * Redis (cluster mode disabled): T1 and T2 cache node types.     * Redis (cluster mode enabled): T1 node types.
replicationGroup
    :: ReplicationGroup
replicationGroup =
  ReplicationGroup'
    { _rgStatus = Nothing
    , _rgCacheNodeType = Nothing
    , _rgNodeGroups = Nothing
    , _rgSnapshottingClusterId = Nothing
    , _rgClusterEnabled = Nothing
    , _rgAtRestEncryptionEnabled = Nothing
    , _rgTransitEncryptionEnabled = Nothing
    , _rgSnapshotWindow = Nothing
    , _rgConfigurationEndpoint = Nothing
    , _rgAuthTokenEnabled = Nothing
    , _rgMemberClusters = Nothing
    , _rgSnapshotRetentionLimit = Nothing
    , _rgDescription = Nothing
    , _rgReplicationGroupId = Nothing
    , _rgPendingModifiedValues = Nothing
    , _rgAutomaticFailover = Nothing
    }


-- | The current state of this replication group - @creating@ , @available@ , @modifying@ , @deleting@ , @create-failed@ , @snapshotting@ .
rgStatus :: Lens' ReplicationGroup (Maybe Text)
rgStatus = lens _rgStatus (\ s a -> s{_rgStatus = a})

-- | The name of the compute and memory capacity node type for each node in the replication group.
rgCacheNodeType :: Lens' ReplicationGroup (Maybe Text)
rgCacheNodeType = lens _rgCacheNodeType (\ s a -> s{_rgCacheNodeType = a})

-- | A list of node groups in this replication group. For Redis (cluster mode disabled) replication groups, this is a single-element list. For Redis (cluster mode enabled) replication groups, the list contains an entry for each node group (shard).
rgNodeGroups :: Lens' ReplicationGroup [NodeGroup]
rgNodeGroups = lens _rgNodeGroups (\ s a -> s{_rgNodeGroups = a}) . _Default . _Coerce

-- | The cluster ID that is used as the daily snapshot source for the replication group.
rgSnapshottingClusterId :: Lens' ReplicationGroup (Maybe Text)
rgSnapshottingClusterId = lens _rgSnapshottingClusterId (\ s a -> s{_rgSnapshottingClusterId = a})

-- | A flag indicating whether or not this replication group is cluster enabled; i.e., whether its data can be partitioned across multiple shards (API/CLI: node groups). Valid values: @true@ | @false@
rgClusterEnabled :: Lens' ReplicationGroup (Maybe Bool)
rgClusterEnabled = lens _rgClusterEnabled (\ s a -> s{_rgClusterEnabled = a})

-- | A flag that enables encryption at-rest when set to @true@ . You cannot modify the value of @AtRestEncryptionEnabled@ after the cluster is created. To enable encryption at-rest on a cluster you must set @AtRestEncryptionEnabled@ to @true@ when you create a cluster. Default: @false@
rgAtRestEncryptionEnabled :: Lens' ReplicationGroup (Maybe Bool)
rgAtRestEncryptionEnabled = lens _rgAtRestEncryptionEnabled (\ s a -> s{_rgAtRestEncryptionEnabled = a})

-- | A flag that enables in-transit encryption when set to @true@ . You cannot modify the value of @TransitEncryptionEnabled@ after the cluster is created. To enable in-transit encryption on a cluster you must set @TransitEncryptionEnabled@ to @true@ when you create a cluster. Default: @false@
rgTransitEncryptionEnabled :: Lens' ReplicationGroup (Maybe Bool)
rgTransitEncryptionEnabled = lens _rgTransitEncryptionEnabled (\ s a -> s{_rgTransitEncryptionEnabled = a})

-- | The daily time range (in UTC) during which ElastiCache begins taking a daily snapshot of your node group (shard). Example: @05:00-09:00@  If you do not specify this parameter, ElastiCache automatically chooses an appropriate time range.
rgSnapshotWindow :: Lens' ReplicationGroup (Maybe Text)
rgSnapshotWindow = lens _rgSnapshotWindow (\ s a -> s{_rgSnapshotWindow = a})

-- | The configuration endpoint for this replication group. Use the configuration endpoint to connect to this replication group.
rgConfigurationEndpoint :: Lens' ReplicationGroup (Maybe Endpoint)
rgConfigurationEndpoint = lens _rgConfigurationEndpoint (\ s a -> s{_rgConfigurationEndpoint = a})

-- | A flag that enables using an @AuthToken@ (password) when issuing Redis commands. Default: @false@
rgAuthTokenEnabled :: Lens' ReplicationGroup (Maybe Bool)
rgAuthTokenEnabled = lens _rgAuthTokenEnabled (\ s a -> s{_rgAuthTokenEnabled = a})

-- | The identifiers of all the nodes that are part of this replication group.
rgMemberClusters :: Lens' ReplicationGroup [Text]
rgMemberClusters = lens _rgMemberClusters (\ s a -> s{_rgMemberClusters = a}) . _Default . _Coerce

-- | The number of days for which ElastiCache retains automatic cluster snapshots before deleting them. For example, if you set @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is retained for 5 days before being deleted. /Important:/ If the value of @SnapshotRetentionLimit@ is set to zero (0), backups are turned off.
rgSnapshotRetentionLimit :: Lens' ReplicationGroup (Maybe Int)
rgSnapshotRetentionLimit = lens _rgSnapshotRetentionLimit (\ s a -> s{_rgSnapshotRetentionLimit = a})

-- | The user supplied description of the replication group.
rgDescription :: Lens' ReplicationGroup (Maybe Text)
rgDescription = lens _rgDescription (\ s a -> s{_rgDescription = a})

-- | The identifier for the replication group.
rgReplicationGroupId :: Lens' ReplicationGroup (Maybe Text)
rgReplicationGroupId = lens _rgReplicationGroupId (\ s a -> s{_rgReplicationGroupId = a})

-- | A group of settings to be applied to the replication group, either immediately or during the next maintenance window.
rgPendingModifiedValues :: Lens' ReplicationGroup (Maybe ReplicationGroupPendingModifiedValues)
rgPendingModifiedValues = lens _rgPendingModifiedValues (\ s a -> s{_rgPendingModifiedValues = a})

-- | Indicates the status of Multi-AZ with automatic failover for this Redis replication group. Amazon ElastiCache for Redis does not support Multi-AZ with automatic failover on:     * Redis versions earlier than 2.8.6.     * Redis (cluster mode disabled): T1 and T2 cache node types.     * Redis (cluster mode enabled): T1 node types.
rgAutomaticFailover :: Lens' ReplicationGroup (Maybe AutomaticFailoverStatus)
rgAutomaticFailover = lens _rgAutomaticFailover (\ s a -> s{_rgAutomaticFailover = a})

instance FromXML ReplicationGroup where
        parseXML x
          = ReplicationGroup' <$>
              (x .@? "Status") <*> (x .@? "CacheNodeType") <*>
                (x .@? "NodeGroups" .!@ mempty >>=
                   may (parseXMLList "NodeGroup"))
                <*> (x .@? "SnapshottingClusterId")
                <*> (x .@? "ClusterEnabled")
                <*> (x .@? "AtRestEncryptionEnabled")
                <*> (x .@? "TransitEncryptionEnabled")
                <*> (x .@? "SnapshotWindow")
                <*> (x .@? "ConfigurationEndpoint")
                <*> (x .@? "AuthTokenEnabled")
                <*>
                (x .@? "MemberClusters" .!@ mempty >>=
                   may (parseXMLList "ClusterId"))
                <*> (x .@? "SnapshotRetentionLimit")
                <*> (x .@? "Description")
                <*> (x .@? "ReplicationGroupId")
                <*> (x .@? "PendingModifiedValues")
                <*> (x .@? "AutomaticFailover")

instance Hashable ReplicationGroup where

instance NFData ReplicationGroup where

-- | The settings to be applied to the Redis replication group, either immediately or during the next maintenance window.
--
--
--
-- /See:/ 'replicationGroupPendingModifiedValues' smart constructor.
data ReplicationGroupPendingModifiedValues = ReplicationGroupPendingModifiedValues'
  { _rgpmvResharding              :: !(Maybe ReshardingStatus)
  , _rgpmvPrimaryClusterId        :: !(Maybe Text)
  , _rgpmvAutomaticFailoverStatus :: !(Maybe PendingAutomaticFailoverStatus)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplicationGroupPendingModifiedValues' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rgpmvResharding' - The status of an online resharding operation.
--
-- * 'rgpmvPrimaryClusterId' - The primary cluster ID that is applied immediately (if @--apply-immediately@ was specified), or during the next maintenance window.
--
-- * 'rgpmvAutomaticFailoverStatus' - Indicates the status of Multi-AZ with automatic failover for this Redis replication group. Amazon ElastiCache for Redis does not support Multi-AZ with automatic failover on:     * Redis versions earlier than 2.8.6.     * Redis (cluster mode disabled): T1 and T2 cache node types.     * Redis (cluster mode enabled): T1 node types.
replicationGroupPendingModifiedValues
    :: ReplicationGroupPendingModifiedValues
replicationGroupPendingModifiedValues =
  ReplicationGroupPendingModifiedValues'
    { _rgpmvResharding = Nothing
    , _rgpmvPrimaryClusterId = Nothing
    , _rgpmvAutomaticFailoverStatus = Nothing
    }


-- | The status of an online resharding operation.
rgpmvResharding :: Lens' ReplicationGroupPendingModifiedValues (Maybe ReshardingStatus)
rgpmvResharding = lens _rgpmvResharding (\ s a -> s{_rgpmvResharding = a})

-- | The primary cluster ID that is applied immediately (if @--apply-immediately@ was specified), or during the next maintenance window.
rgpmvPrimaryClusterId :: Lens' ReplicationGroupPendingModifiedValues (Maybe Text)
rgpmvPrimaryClusterId = lens _rgpmvPrimaryClusterId (\ s a -> s{_rgpmvPrimaryClusterId = a})

-- | Indicates the status of Multi-AZ with automatic failover for this Redis replication group. Amazon ElastiCache for Redis does not support Multi-AZ with automatic failover on:     * Redis versions earlier than 2.8.6.     * Redis (cluster mode disabled): T1 and T2 cache node types.     * Redis (cluster mode enabled): T1 node types.
rgpmvAutomaticFailoverStatus :: Lens' ReplicationGroupPendingModifiedValues (Maybe PendingAutomaticFailoverStatus)
rgpmvAutomaticFailoverStatus = lens _rgpmvAutomaticFailoverStatus (\ s a -> s{_rgpmvAutomaticFailoverStatus = a})

instance FromXML
           ReplicationGroupPendingModifiedValues
         where
        parseXML x
          = ReplicationGroupPendingModifiedValues' <$>
              (x .@? "Resharding") <*> (x .@? "PrimaryClusterId")
                <*> (x .@? "AutomaticFailoverStatus")

instance Hashable
           ReplicationGroupPendingModifiedValues
         where

instance NFData ReplicationGroupPendingModifiedValues
         where

-- | Represents the output of a @PurchaseReservedCacheNodesOffering@ operation.
--
--
--
-- /See:/ 'reservedCacheNode' smart constructor.
data ReservedCacheNode = ReservedCacheNode'
  { _rcnCacheNodeType                :: !(Maybe Text)
  , _rcnState                        :: !(Maybe Text)
  , _rcnStartTime                    :: !(Maybe ISO8601)
  , _rcnProductDescription           :: !(Maybe Text)
  , _rcnCacheNodeCount               :: !(Maybe Int)
  , _rcnReservedCacheNodeId          :: !(Maybe Text)
  , _rcnRecurringCharges             :: !(Maybe [RecurringCharge])
  , _rcnOfferingType                 :: !(Maybe Text)
  , _rcnUsagePrice                   :: !(Maybe Double)
  , _rcnFixedPrice                   :: !(Maybe Double)
  , _rcnDuration                     :: !(Maybe Int)
  , _rcnReservedCacheNodesOfferingId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReservedCacheNode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcnCacheNodeType' - The cache node type for the reserved cache nodes. The following node types are supported by ElastiCache. Generally speaking, the current generation types provide more memory and computational power at lower cost when compared to their equivalent previous generation counterparts.     * General purpose:     * Current generation:  __T2 node types:__ @cache.t2.micro@ , @cache.t2.small@ , @cache.t2.medium@  __M3 node types:__ @cache.m3.medium@ , @cache.m3.large@ , @cache.m3.xlarge@ , @cache.m3.2xlarge@  __M4 node types:__ @cache.m4.large@ , @cache.m4.xlarge@ , @cache.m4.2xlarge@ , @cache.m4.4xlarge@ , @cache.m4.10xlarge@      * Previous generation: (not recommended) __T1 node types:__ @cache.t1.micro@  __M1 node types:__ @cache.m1.small@ , @cache.m1.medium@ , @cache.m1.large@ , @cache.m1.xlarge@      * Compute optimized:     * Previous generation: (not recommended) __C1 node types:__ @cache.c1.xlarge@      * Memory optimized:     * Current generation:  __R3 node types:__ @cache.r3.large@ , @cache.r3.xlarge@ , @cache.r3.2xlarge@ , @cache.r3.4xlarge@ , @cache.r3.8xlarge@      * Previous generation: (not recommended) __M2 node types:__ @cache.m2.xlarge@ , @cache.m2.2xlarge@ , @cache.m2.4xlarge@  __Notes:__      * All T2 instances are created in an Amazon Virtual Private Cloud (Amazon VPC).     * Redis (cluster mode disabled): Redis backup/restore is not supported on T1 and T2 instances.      * Redis (cluster mode enabled): Backup/restore is not supported on T1 instances.     * Redis Append-only files (AOF) functionality is not supported for T1 or T2 instances. For a complete listing of node types and specifications, see <http://aws.amazon.com/elasticache/details Amazon ElastiCache Product Features and Details> and either <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Memcached.html#ParameterGroups.Memcached.NodeSpecific Cache Node Type-Specific Parameters for Memcached> or <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Redis.html#ParameterGroups.Redis.NodeSpecific Cache Node Type-Specific Parameters for Redis> .
--
-- * 'rcnState' - The state of the reserved cache node.
--
-- * 'rcnStartTime' - The time the reservation started.
--
-- * 'rcnProductDescription' - The description of the reserved cache node.
--
-- * 'rcnCacheNodeCount' - The number of cache nodes that have been reserved.
--
-- * 'rcnReservedCacheNodeId' - The unique identifier for the reservation.
--
-- * 'rcnRecurringCharges' - The recurring price charged to run this reserved cache node.
--
-- * 'rcnOfferingType' - The offering type of this reserved cache node.
--
-- * 'rcnUsagePrice' - The hourly price charged for this reserved cache node.
--
-- * 'rcnFixedPrice' - The fixed price charged for this reserved cache node.
--
-- * 'rcnDuration' - The duration of the reservation in seconds.
--
-- * 'rcnReservedCacheNodesOfferingId' - The offering identifier.
reservedCacheNode
    :: ReservedCacheNode
reservedCacheNode =
  ReservedCacheNode'
    { _rcnCacheNodeType = Nothing
    , _rcnState = Nothing
    , _rcnStartTime = Nothing
    , _rcnProductDescription = Nothing
    , _rcnCacheNodeCount = Nothing
    , _rcnReservedCacheNodeId = Nothing
    , _rcnRecurringCharges = Nothing
    , _rcnOfferingType = Nothing
    , _rcnUsagePrice = Nothing
    , _rcnFixedPrice = Nothing
    , _rcnDuration = Nothing
    , _rcnReservedCacheNodesOfferingId = Nothing
    }


-- | The cache node type for the reserved cache nodes. The following node types are supported by ElastiCache. Generally speaking, the current generation types provide more memory and computational power at lower cost when compared to their equivalent previous generation counterparts.     * General purpose:     * Current generation:  __T2 node types:__ @cache.t2.micro@ , @cache.t2.small@ , @cache.t2.medium@  __M3 node types:__ @cache.m3.medium@ , @cache.m3.large@ , @cache.m3.xlarge@ , @cache.m3.2xlarge@  __M4 node types:__ @cache.m4.large@ , @cache.m4.xlarge@ , @cache.m4.2xlarge@ , @cache.m4.4xlarge@ , @cache.m4.10xlarge@      * Previous generation: (not recommended) __T1 node types:__ @cache.t1.micro@  __M1 node types:__ @cache.m1.small@ , @cache.m1.medium@ , @cache.m1.large@ , @cache.m1.xlarge@      * Compute optimized:     * Previous generation: (not recommended) __C1 node types:__ @cache.c1.xlarge@      * Memory optimized:     * Current generation:  __R3 node types:__ @cache.r3.large@ , @cache.r3.xlarge@ , @cache.r3.2xlarge@ , @cache.r3.4xlarge@ , @cache.r3.8xlarge@      * Previous generation: (not recommended) __M2 node types:__ @cache.m2.xlarge@ , @cache.m2.2xlarge@ , @cache.m2.4xlarge@  __Notes:__      * All T2 instances are created in an Amazon Virtual Private Cloud (Amazon VPC).     * Redis (cluster mode disabled): Redis backup/restore is not supported on T1 and T2 instances.      * Redis (cluster mode enabled): Backup/restore is not supported on T1 instances.     * Redis Append-only files (AOF) functionality is not supported for T1 or T2 instances. For a complete listing of node types and specifications, see <http://aws.amazon.com/elasticache/details Amazon ElastiCache Product Features and Details> and either <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Memcached.html#ParameterGroups.Memcached.NodeSpecific Cache Node Type-Specific Parameters for Memcached> or <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Redis.html#ParameterGroups.Redis.NodeSpecific Cache Node Type-Specific Parameters for Redis> .
rcnCacheNodeType :: Lens' ReservedCacheNode (Maybe Text)
rcnCacheNodeType = lens _rcnCacheNodeType (\ s a -> s{_rcnCacheNodeType = a})

-- | The state of the reserved cache node.
rcnState :: Lens' ReservedCacheNode (Maybe Text)
rcnState = lens _rcnState (\ s a -> s{_rcnState = a})

-- | The time the reservation started.
rcnStartTime :: Lens' ReservedCacheNode (Maybe UTCTime)
rcnStartTime = lens _rcnStartTime (\ s a -> s{_rcnStartTime = a}) . mapping _Time

-- | The description of the reserved cache node.
rcnProductDescription :: Lens' ReservedCacheNode (Maybe Text)
rcnProductDescription = lens _rcnProductDescription (\ s a -> s{_rcnProductDescription = a})

-- | The number of cache nodes that have been reserved.
rcnCacheNodeCount :: Lens' ReservedCacheNode (Maybe Int)
rcnCacheNodeCount = lens _rcnCacheNodeCount (\ s a -> s{_rcnCacheNodeCount = a})

-- | The unique identifier for the reservation.
rcnReservedCacheNodeId :: Lens' ReservedCacheNode (Maybe Text)
rcnReservedCacheNodeId = lens _rcnReservedCacheNodeId (\ s a -> s{_rcnReservedCacheNodeId = a})

-- | The recurring price charged to run this reserved cache node.
rcnRecurringCharges :: Lens' ReservedCacheNode [RecurringCharge]
rcnRecurringCharges = lens _rcnRecurringCharges (\ s a -> s{_rcnRecurringCharges = a}) . _Default . _Coerce

-- | The offering type of this reserved cache node.
rcnOfferingType :: Lens' ReservedCacheNode (Maybe Text)
rcnOfferingType = lens _rcnOfferingType (\ s a -> s{_rcnOfferingType = a})

-- | The hourly price charged for this reserved cache node.
rcnUsagePrice :: Lens' ReservedCacheNode (Maybe Double)
rcnUsagePrice = lens _rcnUsagePrice (\ s a -> s{_rcnUsagePrice = a})

-- | The fixed price charged for this reserved cache node.
rcnFixedPrice :: Lens' ReservedCacheNode (Maybe Double)
rcnFixedPrice = lens _rcnFixedPrice (\ s a -> s{_rcnFixedPrice = a})

-- | The duration of the reservation in seconds.
rcnDuration :: Lens' ReservedCacheNode (Maybe Int)
rcnDuration = lens _rcnDuration (\ s a -> s{_rcnDuration = a})

-- | The offering identifier.
rcnReservedCacheNodesOfferingId :: Lens' ReservedCacheNode (Maybe Text)
rcnReservedCacheNodesOfferingId = lens _rcnReservedCacheNodesOfferingId (\ s a -> s{_rcnReservedCacheNodesOfferingId = a})

instance FromXML ReservedCacheNode where
        parseXML x
          = ReservedCacheNode' <$>
              (x .@? "CacheNodeType") <*> (x .@? "State") <*>
                (x .@? "StartTime")
                <*> (x .@? "ProductDescription")
                <*> (x .@? "CacheNodeCount")
                <*> (x .@? "ReservedCacheNodeId")
                <*>
                (x .@? "RecurringCharges" .!@ mempty >>=
                   may (parseXMLList "RecurringCharge"))
                <*> (x .@? "OfferingType")
                <*> (x .@? "UsagePrice")
                <*> (x .@? "FixedPrice")
                <*> (x .@? "Duration")
                <*> (x .@? "ReservedCacheNodesOfferingId")

instance Hashable ReservedCacheNode where

instance NFData ReservedCacheNode where

-- | Describes all of the attributes of a reserved cache node offering.
--
--
--
-- /See:/ 'reservedCacheNodesOffering' smart constructor.
data ReservedCacheNodesOffering = ReservedCacheNodesOffering'
  { _rcnoCacheNodeType                :: !(Maybe Text)
  , _rcnoProductDescription           :: !(Maybe Text)
  , _rcnoRecurringCharges             :: !(Maybe [RecurringCharge])
  , _rcnoOfferingType                 :: !(Maybe Text)
  , _rcnoUsagePrice                   :: !(Maybe Double)
  , _rcnoFixedPrice                   :: !(Maybe Double)
  , _rcnoDuration                     :: !(Maybe Int)
  , _rcnoReservedCacheNodesOfferingId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReservedCacheNodesOffering' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcnoCacheNodeType' - The cache node type for the reserved cache node. The following node types are supported by ElastiCache. Generally speaking, the current generation types provide more memory and computational power at lower cost when compared to their equivalent previous generation counterparts.     * General purpose:     * Current generation:  __T2 node types:__ @cache.t2.micro@ , @cache.t2.small@ , @cache.t2.medium@  __M3 node types:__ @cache.m3.medium@ , @cache.m3.large@ , @cache.m3.xlarge@ , @cache.m3.2xlarge@  __M4 node types:__ @cache.m4.large@ , @cache.m4.xlarge@ , @cache.m4.2xlarge@ , @cache.m4.4xlarge@ , @cache.m4.10xlarge@      * Previous generation: (not recommended) __T1 node types:__ @cache.t1.micro@  __M1 node types:__ @cache.m1.small@ , @cache.m1.medium@ , @cache.m1.large@ , @cache.m1.xlarge@      * Compute optimized:     * Previous generation: (not recommended) __C1 node types:__ @cache.c1.xlarge@      * Memory optimized:     * Current generation:  __R3 node types:__ @cache.r3.large@ , @cache.r3.xlarge@ , @cache.r3.2xlarge@ , @cache.r3.4xlarge@ , @cache.r3.8xlarge@      * Previous generation: (not recommended) __M2 node types:__ @cache.m2.xlarge@ , @cache.m2.2xlarge@ , @cache.m2.4xlarge@  __Notes:__      * All T2 instances are created in an Amazon Virtual Private Cloud (Amazon VPC).     * Redis (cluster mode disabled): Redis backup/restore is not supported on T1 and T2 instances.      * Redis (cluster mode enabled): Backup/restore is not supported on T1 instances.     * Redis Append-only files (AOF) functionality is not supported for T1 or T2 instances. For a complete listing of node types and specifications, see <http://aws.amazon.com/elasticache/details Amazon ElastiCache Product Features and Details> and either <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Memcached.html#ParameterGroups.Memcached.NodeSpecific Cache Node Type-Specific Parameters for Memcached> or <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Redis.html#ParameterGroups.Redis.NodeSpecific Cache Node Type-Specific Parameters for Redis> .
--
-- * 'rcnoProductDescription' - The cache engine used by the offering.
--
-- * 'rcnoRecurringCharges' - The recurring price charged to run this reserved cache node.
--
-- * 'rcnoOfferingType' - The offering type.
--
-- * 'rcnoUsagePrice' - The hourly price charged for this offering.
--
-- * 'rcnoFixedPrice' - The fixed price charged for this offering.
--
-- * 'rcnoDuration' - The duration of the offering. in seconds.
--
-- * 'rcnoReservedCacheNodesOfferingId' - A unique identifier for the reserved cache node offering.
reservedCacheNodesOffering
    :: ReservedCacheNodesOffering
reservedCacheNodesOffering =
  ReservedCacheNodesOffering'
    { _rcnoCacheNodeType = Nothing
    , _rcnoProductDescription = Nothing
    , _rcnoRecurringCharges = Nothing
    , _rcnoOfferingType = Nothing
    , _rcnoUsagePrice = Nothing
    , _rcnoFixedPrice = Nothing
    , _rcnoDuration = Nothing
    , _rcnoReservedCacheNodesOfferingId = Nothing
    }


-- | The cache node type for the reserved cache node. The following node types are supported by ElastiCache. Generally speaking, the current generation types provide more memory and computational power at lower cost when compared to their equivalent previous generation counterparts.     * General purpose:     * Current generation:  __T2 node types:__ @cache.t2.micro@ , @cache.t2.small@ , @cache.t2.medium@  __M3 node types:__ @cache.m3.medium@ , @cache.m3.large@ , @cache.m3.xlarge@ , @cache.m3.2xlarge@  __M4 node types:__ @cache.m4.large@ , @cache.m4.xlarge@ , @cache.m4.2xlarge@ , @cache.m4.4xlarge@ , @cache.m4.10xlarge@      * Previous generation: (not recommended) __T1 node types:__ @cache.t1.micro@  __M1 node types:__ @cache.m1.small@ , @cache.m1.medium@ , @cache.m1.large@ , @cache.m1.xlarge@      * Compute optimized:     * Previous generation: (not recommended) __C1 node types:__ @cache.c1.xlarge@      * Memory optimized:     * Current generation:  __R3 node types:__ @cache.r3.large@ , @cache.r3.xlarge@ , @cache.r3.2xlarge@ , @cache.r3.4xlarge@ , @cache.r3.8xlarge@      * Previous generation: (not recommended) __M2 node types:__ @cache.m2.xlarge@ , @cache.m2.2xlarge@ , @cache.m2.4xlarge@  __Notes:__      * All T2 instances are created in an Amazon Virtual Private Cloud (Amazon VPC).     * Redis (cluster mode disabled): Redis backup/restore is not supported on T1 and T2 instances.      * Redis (cluster mode enabled): Backup/restore is not supported on T1 instances.     * Redis Append-only files (AOF) functionality is not supported for T1 or T2 instances. For a complete listing of node types and specifications, see <http://aws.amazon.com/elasticache/details Amazon ElastiCache Product Features and Details> and either <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Memcached.html#ParameterGroups.Memcached.NodeSpecific Cache Node Type-Specific Parameters for Memcached> or <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Redis.html#ParameterGroups.Redis.NodeSpecific Cache Node Type-Specific Parameters for Redis> .
rcnoCacheNodeType :: Lens' ReservedCacheNodesOffering (Maybe Text)
rcnoCacheNodeType = lens _rcnoCacheNodeType (\ s a -> s{_rcnoCacheNodeType = a})

-- | The cache engine used by the offering.
rcnoProductDescription :: Lens' ReservedCacheNodesOffering (Maybe Text)
rcnoProductDescription = lens _rcnoProductDescription (\ s a -> s{_rcnoProductDescription = a})

-- | The recurring price charged to run this reserved cache node.
rcnoRecurringCharges :: Lens' ReservedCacheNodesOffering [RecurringCharge]
rcnoRecurringCharges = lens _rcnoRecurringCharges (\ s a -> s{_rcnoRecurringCharges = a}) . _Default . _Coerce

-- | The offering type.
rcnoOfferingType :: Lens' ReservedCacheNodesOffering (Maybe Text)
rcnoOfferingType = lens _rcnoOfferingType (\ s a -> s{_rcnoOfferingType = a})

-- | The hourly price charged for this offering.
rcnoUsagePrice :: Lens' ReservedCacheNodesOffering (Maybe Double)
rcnoUsagePrice = lens _rcnoUsagePrice (\ s a -> s{_rcnoUsagePrice = a})

-- | The fixed price charged for this offering.
rcnoFixedPrice :: Lens' ReservedCacheNodesOffering (Maybe Double)
rcnoFixedPrice = lens _rcnoFixedPrice (\ s a -> s{_rcnoFixedPrice = a})

-- | The duration of the offering. in seconds.
rcnoDuration :: Lens' ReservedCacheNodesOffering (Maybe Int)
rcnoDuration = lens _rcnoDuration (\ s a -> s{_rcnoDuration = a})

-- | A unique identifier for the reserved cache node offering.
rcnoReservedCacheNodesOfferingId :: Lens' ReservedCacheNodesOffering (Maybe Text)
rcnoReservedCacheNodesOfferingId = lens _rcnoReservedCacheNodesOfferingId (\ s a -> s{_rcnoReservedCacheNodesOfferingId = a})

instance FromXML ReservedCacheNodesOffering where
        parseXML x
          = ReservedCacheNodesOffering' <$>
              (x .@? "CacheNodeType") <*>
                (x .@? "ProductDescription")
                <*>
                (x .@? "RecurringCharges" .!@ mempty >>=
                   may (parseXMLList "RecurringCharge"))
                <*> (x .@? "OfferingType")
                <*> (x .@? "UsagePrice")
                <*> (x .@? "FixedPrice")
                <*> (x .@? "Duration")
                <*> (x .@? "ReservedCacheNodesOfferingId")

instance Hashable ReservedCacheNodesOffering where

instance NFData ReservedCacheNodesOffering where

-- | A list of @PreferredAvailabilityZones@ objects that specifies the configuration of a node group in the resharded cluster.
--
--
--
-- /See:/ 'reshardingConfiguration' smart constructor.
newtype ReshardingConfiguration = ReshardingConfiguration'
  { _rcPreferredAvailabilityZones :: Maybe [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReshardingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcPreferredAvailabilityZones' - A list of preferred availability zones for the nodes in this cluster.
reshardingConfiguration
    :: ReshardingConfiguration
reshardingConfiguration =
  ReshardingConfiguration' {_rcPreferredAvailabilityZones = Nothing}


-- | A list of preferred availability zones for the nodes in this cluster.
rcPreferredAvailabilityZones :: Lens' ReshardingConfiguration [Text]
rcPreferredAvailabilityZones = lens _rcPreferredAvailabilityZones (\ s a -> s{_rcPreferredAvailabilityZones = a}) . _Default . _Coerce

instance Hashable ReshardingConfiguration where

instance NFData ReshardingConfiguration where

instance ToQuery ReshardingConfiguration where
        toQuery ReshardingConfiguration'{..}
          = mconcat
              ["PreferredAvailabilityZones" =:
                 toQuery
                   (toQueryList "AvailabilityZone" <$>
                      _rcPreferredAvailabilityZones)]

-- | The status of an online resharding operation.
--
--
--
-- /See:/ 'reshardingStatus' smart constructor.
newtype ReshardingStatus = ReshardingStatus'
  { _rsSlotMigration :: Maybe SlotMigration
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReshardingStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsSlotMigration' - Represents the progress of an online resharding operation.
reshardingStatus
    :: ReshardingStatus
reshardingStatus = ReshardingStatus' {_rsSlotMigration = Nothing}


-- | Represents the progress of an online resharding operation.
rsSlotMigration :: Lens' ReshardingStatus (Maybe SlotMigration)
rsSlotMigration = lens _rsSlotMigration (\ s a -> s{_rsSlotMigration = a})

instance FromXML ReshardingStatus where
        parseXML x
          = ReshardingStatus' <$> (x .@? "SlotMigration")

instance Hashable ReshardingStatus where

instance NFData ReshardingStatus where

-- | Represents a single cache security group and its status.
--
--
--
-- /See:/ 'securityGroupMembership' smart constructor.
data SecurityGroupMembership = SecurityGroupMembership'
  { _sgmStatus          :: !(Maybe Text)
  , _sgmSecurityGroupId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SecurityGroupMembership' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sgmStatus' - The status of the cache security group membership. The status changes whenever a cache security group is modified, or when the cache security groups assigned to a cluster are modified.
--
-- * 'sgmSecurityGroupId' - The identifier of the cache security group.
securityGroupMembership
    :: SecurityGroupMembership
securityGroupMembership =
  SecurityGroupMembership' {_sgmStatus = Nothing, _sgmSecurityGroupId = Nothing}


-- | The status of the cache security group membership. The status changes whenever a cache security group is modified, or when the cache security groups assigned to a cluster are modified.
sgmStatus :: Lens' SecurityGroupMembership (Maybe Text)
sgmStatus = lens _sgmStatus (\ s a -> s{_sgmStatus = a})

-- | The identifier of the cache security group.
sgmSecurityGroupId :: Lens' SecurityGroupMembership (Maybe Text)
sgmSecurityGroupId = lens _sgmSecurityGroupId (\ s a -> s{_sgmSecurityGroupId = a})

instance FromXML SecurityGroupMembership where
        parseXML x
          = SecurityGroupMembership' <$>
              (x .@? "Status") <*> (x .@? "SecurityGroupId")

instance Hashable SecurityGroupMembership where

instance NFData SecurityGroupMembership where

-- | Represents the progress of an online resharding operation.
--
--
--
-- /See:/ 'slotMigration' smart constructor.
newtype SlotMigration = SlotMigration'
  { _smProgressPercentage :: Maybe Double
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SlotMigration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smProgressPercentage' - The percentage of the slot migration that is complete.
slotMigration
    :: SlotMigration
slotMigration = SlotMigration' {_smProgressPercentage = Nothing}


-- | The percentage of the slot migration that is complete.
smProgressPercentage :: Lens' SlotMigration (Maybe Double)
smProgressPercentage = lens _smProgressPercentage (\ s a -> s{_smProgressPercentage = a})

instance FromXML SlotMigration where
        parseXML x
          = SlotMigration' <$> (x .@? "ProgressPercentage")

instance Hashable SlotMigration where

instance NFData SlotMigration where

-- | Represents a copy of an entire Redis cluster as of the time when the snapshot was taken.
--
--
--
-- /See:/ 'snapshot' smart constructor.
data Snapshot = Snapshot'
  { _sEngineVersion               :: !(Maybe Text)
  , _sCacheNodeType               :: !(Maybe Text)
  , _sCacheClusterCreateTime      :: !(Maybe ISO8601)
  , _sAutoMinorVersionUpgrade     :: !(Maybe Bool)
  , _sCacheParameterGroupName     :: !(Maybe Text)
  , _sReplicationGroupDescription :: !(Maybe Text)
  , _sVPCId                       :: !(Maybe Text)
  , _sSnapshotStatus              :: !(Maybe Text)
  , _sSnapshotWindow              :: !(Maybe Text)
  , _sCacheClusterId              :: !(Maybe Text)
  , _sEngine                      :: !(Maybe Text)
  , _sPreferredMaintenanceWindow  :: !(Maybe Text)
  , _sTopicARN                    :: !(Maybe Text)
  , _sNodeSnapshots               :: !(Maybe [NodeSnapshot])
  , _sCacheSubnetGroupName        :: !(Maybe Text)
  , _sPreferredAvailabilityZone   :: !(Maybe Text)
  , _sNumNodeGroups               :: !(Maybe Int)
  , _sSnapshotRetentionLimit      :: !(Maybe Int)
  , _sSnapshotName                :: !(Maybe Text)
  , _sReplicationGroupId          :: !(Maybe Text)
  , _sNumCacheNodes               :: !(Maybe Int)
  , _sPort                        :: !(Maybe Int)
  , _sAutomaticFailover           :: !(Maybe AutomaticFailoverStatus)
  , _sSnapshotSource              :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Snapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sEngineVersion' - The version of the cache engine version that is used by the source cluster.
--
-- * 'sCacheNodeType' - The name of the compute and memory capacity node type for the source cluster. The following node types are supported by ElastiCache. Generally speaking, the current generation types provide more memory and computational power at lower cost when compared to their equivalent previous generation counterparts.     * General purpose:     * Current generation:  __T2 node types:__ @cache.t2.micro@ , @cache.t2.small@ , @cache.t2.medium@  __M3 node types:__ @cache.m3.medium@ , @cache.m3.large@ , @cache.m3.xlarge@ , @cache.m3.2xlarge@  __M4 node types:__ @cache.m4.large@ , @cache.m4.xlarge@ , @cache.m4.2xlarge@ , @cache.m4.4xlarge@ , @cache.m4.10xlarge@      * Previous generation: (not recommended) __T1 node types:__ @cache.t1.micro@  __M1 node types:__ @cache.m1.small@ , @cache.m1.medium@ , @cache.m1.large@ , @cache.m1.xlarge@      * Compute optimized:     * Previous generation: (not recommended) __C1 node types:__ @cache.c1.xlarge@      * Memory optimized:     * Current generation:  __R3 node types:__ @cache.r3.large@ , @cache.r3.xlarge@ , @cache.r3.2xlarge@ , @cache.r3.4xlarge@ , @cache.r3.8xlarge@      * Previous generation: (not recommended) __M2 node types:__ @cache.m2.xlarge@ , @cache.m2.2xlarge@ , @cache.m2.4xlarge@  __Notes:__      * All T2 instances are created in an Amazon Virtual Private Cloud (Amazon VPC).     * Redis (cluster mode disabled): Redis backup/restore is not supported on T1 and T2 instances.      * Redis (cluster mode enabled): Backup/restore is not supported on T1 instances.     * Redis Append-only files (AOF) functionality is not supported for T1 or T2 instances. For a complete listing of node types and specifications, see <http://aws.amazon.com/elasticache/details Amazon ElastiCache Product Features and Details> and either <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Memcached.html#ParameterGroups.Memcached.NodeSpecific Cache Node Type-Specific Parameters for Memcached> or <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Redis.html#ParameterGroups.Redis.NodeSpecific Cache Node Type-Specific Parameters for Redis> .
--
-- * 'sCacheClusterCreateTime' - The date and time when the source cluster was created.
--
-- * 'sAutoMinorVersionUpgrade' - This parameter is currently disabled.
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
-- * 'sReplicationGroupId' - The unique identifier of the source replication group.
--
-- * 'sNumCacheNodes' - The number of cache nodes in the source cluster. For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20.
--
-- * 'sPort' - The port number used by each cache nodes in the source cluster.
--
-- * 'sAutomaticFailover' - Indicates the status of Multi-AZ with automatic failover for the source Redis replication group. Amazon ElastiCache for Redis does not support Multi-AZ with automatic failover on:     * Redis versions earlier than 2.8.6.     * Redis (cluster mode disabled): T1 and T2 cache node types.     * Redis (cluster mode enabled): T1 node types.
--
-- * 'sSnapshotSource' - Indicates whether the snapshot is from an automatic backup (@automated@ ) or was created manually (@manual@ ).
snapshot
    :: Snapshot
snapshot =
  Snapshot'
    { _sEngineVersion = Nothing
    , _sCacheNodeType = Nothing
    , _sCacheClusterCreateTime = Nothing
    , _sAutoMinorVersionUpgrade = Nothing
    , _sCacheParameterGroupName = Nothing
    , _sReplicationGroupDescription = Nothing
    , _sVPCId = Nothing
    , _sSnapshotStatus = Nothing
    , _sSnapshotWindow = Nothing
    , _sCacheClusterId = Nothing
    , _sEngine = Nothing
    , _sPreferredMaintenanceWindow = Nothing
    , _sTopicARN = Nothing
    , _sNodeSnapshots = Nothing
    , _sCacheSubnetGroupName = Nothing
    , _sPreferredAvailabilityZone = Nothing
    , _sNumNodeGroups = Nothing
    , _sSnapshotRetentionLimit = Nothing
    , _sSnapshotName = Nothing
    , _sReplicationGroupId = Nothing
    , _sNumCacheNodes = Nothing
    , _sPort = Nothing
    , _sAutomaticFailover = Nothing
    , _sSnapshotSource = Nothing
    }


-- | The version of the cache engine version that is used by the source cluster.
sEngineVersion :: Lens' Snapshot (Maybe Text)
sEngineVersion = lens _sEngineVersion (\ s a -> s{_sEngineVersion = a})

-- | The name of the compute and memory capacity node type for the source cluster. The following node types are supported by ElastiCache. Generally speaking, the current generation types provide more memory and computational power at lower cost when compared to their equivalent previous generation counterparts.     * General purpose:     * Current generation:  __T2 node types:__ @cache.t2.micro@ , @cache.t2.small@ , @cache.t2.medium@  __M3 node types:__ @cache.m3.medium@ , @cache.m3.large@ , @cache.m3.xlarge@ , @cache.m3.2xlarge@  __M4 node types:__ @cache.m4.large@ , @cache.m4.xlarge@ , @cache.m4.2xlarge@ , @cache.m4.4xlarge@ , @cache.m4.10xlarge@      * Previous generation: (not recommended) __T1 node types:__ @cache.t1.micro@  __M1 node types:__ @cache.m1.small@ , @cache.m1.medium@ , @cache.m1.large@ , @cache.m1.xlarge@      * Compute optimized:     * Previous generation: (not recommended) __C1 node types:__ @cache.c1.xlarge@      * Memory optimized:     * Current generation:  __R3 node types:__ @cache.r3.large@ , @cache.r3.xlarge@ , @cache.r3.2xlarge@ , @cache.r3.4xlarge@ , @cache.r3.8xlarge@      * Previous generation: (not recommended) __M2 node types:__ @cache.m2.xlarge@ , @cache.m2.2xlarge@ , @cache.m2.4xlarge@  __Notes:__      * All T2 instances are created in an Amazon Virtual Private Cloud (Amazon VPC).     * Redis (cluster mode disabled): Redis backup/restore is not supported on T1 and T2 instances.      * Redis (cluster mode enabled): Backup/restore is not supported on T1 instances.     * Redis Append-only files (AOF) functionality is not supported for T1 or T2 instances. For a complete listing of node types and specifications, see <http://aws.amazon.com/elasticache/details Amazon ElastiCache Product Features and Details> and either <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Memcached.html#ParameterGroups.Memcached.NodeSpecific Cache Node Type-Specific Parameters for Memcached> or <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Redis.html#ParameterGroups.Redis.NodeSpecific Cache Node Type-Specific Parameters for Redis> .
sCacheNodeType :: Lens' Snapshot (Maybe Text)
sCacheNodeType = lens _sCacheNodeType (\ s a -> s{_sCacheNodeType = a})

-- | The date and time when the source cluster was created.
sCacheClusterCreateTime :: Lens' Snapshot (Maybe UTCTime)
sCacheClusterCreateTime = lens _sCacheClusterCreateTime (\ s a -> s{_sCacheClusterCreateTime = a}) . mapping _Time

-- | This parameter is currently disabled.
sAutoMinorVersionUpgrade :: Lens' Snapshot (Maybe Bool)
sAutoMinorVersionUpgrade = lens _sAutoMinorVersionUpgrade (\ s a -> s{_sAutoMinorVersionUpgrade = a})

-- | The cache parameter group that is associated with the source cluster.
sCacheParameterGroupName :: Lens' Snapshot (Maybe Text)
sCacheParameterGroupName = lens _sCacheParameterGroupName (\ s a -> s{_sCacheParameterGroupName = a})

-- | A description of the source replication group.
sReplicationGroupDescription :: Lens' Snapshot (Maybe Text)
sReplicationGroupDescription = lens _sReplicationGroupDescription (\ s a -> s{_sReplicationGroupDescription = a})

-- | The Amazon Virtual Private Cloud identifier (VPC ID) of the cache subnet group for the source cluster.
sVPCId :: Lens' Snapshot (Maybe Text)
sVPCId = lens _sVPCId (\ s a -> s{_sVPCId = a})

-- | The status of the snapshot. Valid values: @creating@ | @available@ | @restoring@ | @copying@ | @deleting@ .
sSnapshotStatus :: Lens' Snapshot (Maybe Text)
sSnapshotStatus = lens _sSnapshotStatus (\ s a -> s{_sSnapshotStatus = a})

-- | The daily time range during which ElastiCache takes daily snapshots of the source cluster.
sSnapshotWindow :: Lens' Snapshot (Maybe Text)
sSnapshotWindow = lens _sSnapshotWindow (\ s a -> s{_sSnapshotWindow = a})

-- | The user-supplied identifier of the source cluster.
sCacheClusterId :: Lens' Snapshot (Maybe Text)
sCacheClusterId = lens _sCacheClusterId (\ s a -> s{_sCacheClusterId = a})

-- | The name of the cache engine (@memcached@ or @redis@ ) used by the source cluster.
sEngine :: Lens' Snapshot (Maybe Text)
sEngine = lens _sEngine (\ s a -> s{_sEngine = a})

-- | Specifies the weekly time range during which maintenance on the cluster is performed. It is specified as a range in the format ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window is a 60 minute period. Valid values for @ddd@ are:     * @sun@      * @mon@      * @tue@      * @wed@      * @thu@      * @fri@      * @sat@  Example: @sun:23:00-mon:01:30@
sPreferredMaintenanceWindow :: Lens' Snapshot (Maybe Text)
sPreferredMaintenanceWindow = lens _sPreferredMaintenanceWindow (\ s a -> s{_sPreferredMaintenanceWindow = a})

-- | The Amazon Resource Name (ARN) for the topic used by the source cluster for publishing notifications.
sTopicARN :: Lens' Snapshot (Maybe Text)
sTopicARN = lens _sTopicARN (\ s a -> s{_sTopicARN = a})

-- | A list of the cache nodes in the source cluster.
sNodeSnapshots :: Lens' Snapshot [NodeSnapshot]
sNodeSnapshots = lens _sNodeSnapshots (\ s a -> s{_sNodeSnapshots = a}) . _Default . _Coerce

-- | The name of the cache subnet group associated with the source cluster.
sCacheSubnetGroupName :: Lens' Snapshot (Maybe Text)
sCacheSubnetGroupName = lens _sCacheSubnetGroupName (\ s a -> s{_sCacheSubnetGroupName = a})

-- | The name of the Availability Zone in which the source cluster is located.
sPreferredAvailabilityZone :: Lens' Snapshot (Maybe Text)
sPreferredAvailabilityZone = lens _sPreferredAvailabilityZone (\ s a -> s{_sPreferredAvailabilityZone = a})

-- | The number of node groups (shards) in this snapshot. When restoring from a snapshot, the number of node groups (shards) in the snapshot and in the restored replication group must be the same.
sNumNodeGroups :: Lens' Snapshot (Maybe Int)
sNumNodeGroups = lens _sNumNodeGroups (\ s a -> s{_sNumNodeGroups = a})

-- | For an automatic snapshot, the number of days for which ElastiCache retains the snapshot before deleting it. For manual snapshots, this field reflects the @SnapshotRetentionLimit@ for the source cluster when the snapshot was created. This field is otherwise ignored: Manual snapshots do not expire, and can only be deleted using the @DeleteSnapshot@ operation.  __Important__ If the value of SnapshotRetentionLimit is set to zero (0), backups are turned off.
sSnapshotRetentionLimit :: Lens' Snapshot (Maybe Int)
sSnapshotRetentionLimit = lens _sSnapshotRetentionLimit (\ s a -> s{_sSnapshotRetentionLimit = a})

-- | The name of a snapshot. For an automatic snapshot, the name is system-generated. For a manual snapshot, this is the user-provided name.
sSnapshotName :: Lens' Snapshot (Maybe Text)
sSnapshotName = lens _sSnapshotName (\ s a -> s{_sSnapshotName = a})

-- | The unique identifier of the source replication group.
sReplicationGroupId :: Lens' Snapshot (Maybe Text)
sReplicationGroupId = lens _sReplicationGroupId (\ s a -> s{_sReplicationGroupId = a})

-- | The number of cache nodes in the source cluster. For clusters running Redis, this value must be 1. For clusters running Memcached, this value must be between 1 and 20.
sNumCacheNodes :: Lens' Snapshot (Maybe Int)
sNumCacheNodes = lens _sNumCacheNodes (\ s a -> s{_sNumCacheNodes = a})

-- | The port number used by each cache nodes in the source cluster.
sPort :: Lens' Snapshot (Maybe Int)
sPort = lens _sPort (\ s a -> s{_sPort = a})

-- | Indicates the status of Multi-AZ with automatic failover for the source Redis replication group. Amazon ElastiCache for Redis does not support Multi-AZ with automatic failover on:     * Redis versions earlier than 2.8.6.     * Redis (cluster mode disabled): T1 and T2 cache node types.     * Redis (cluster mode enabled): T1 node types.
sAutomaticFailover :: Lens' Snapshot (Maybe AutomaticFailoverStatus)
sAutomaticFailover = lens _sAutomaticFailover (\ s a -> s{_sAutomaticFailover = a})

-- | Indicates whether the snapshot is from an automatic backup (@automated@ ) or was created manually (@manual@ ).
sSnapshotSource :: Lens' Snapshot (Maybe Text)
sSnapshotSource = lens _sSnapshotSource (\ s a -> s{_sSnapshotSource = a})

instance FromXML Snapshot where
        parseXML x
          = Snapshot' <$>
              (x .@? "EngineVersion") <*> (x .@? "CacheNodeType")
                <*> (x .@? "CacheClusterCreateTime")
                <*> (x .@? "AutoMinorVersionUpgrade")
                <*> (x .@? "CacheParameterGroupName")
                <*> (x .@? "ReplicationGroupDescription")
                <*> (x .@? "VpcId")
                <*> (x .@? "SnapshotStatus")
                <*> (x .@? "SnapshotWindow")
                <*> (x .@? "CacheClusterId")
                <*> (x .@? "Engine")
                <*> (x .@? "PreferredMaintenanceWindow")
                <*> (x .@? "TopicArn")
                <*>
                (x .@? "NodeSnapshots" .!@ mempty >>=
                   may (parseXMLList "NodeSnapshot"))
                <*> (x .@? "CacheSubnetGroupName")
                <*> (x .@? "PreferredAvailabilityZone")
                <*> (x .@? "NumNodeGroups")
                <*> (x .@? "SnapshotRetentionLimit")
                <*> (x .@? "SnapshotName")
                <*> (x .@? "ReplicationGroupId")
                <*> (x .@? "NumCacheNodes")
                <*> (x .@? "Port")
                <*> (x .@? "AutomaticFailover")
                <*> (x .@? "SnapshotSource")

instance Hashable Snapshot where

instance NFData Snapshot where

-- | Represents the subnet associated with a cluster. This parameter refers to subnets defined in Amazon Virtual Private Cloud (Amazon VPC) and used with ElastiCache.
--
--
--
-- /See:/ 'subnet' smart constructor.
data Subnet = Subnet'
  { _sSubnetIdentifier       :: !(Maybe Text)
  , _sSubnetAvailabilityZone :: !(Maybe AvailabilityZone)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Subnet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sSubnetIdentifier' - The unique identifier for the subnet.
--
-- * 'sSubnetAvailabilityZone' - The Availability Zone associated with the subnet.
subnet
    :: Subnet
subnet =
  Subnet' {_sSubnetIdentifier = Nothing, _sSubnetAvailabilityZone = Nothing}


-- | The unique identifier for the subnet.
sSubnetIdentifier :: Lens' Subnet (Maybe Text)
sSubnetIdentifier = lens _sSubnetIdentifier (\ s a -> s{_sSubnetIdentifier = a})

-- | The Availability Zone associated with the subnet.
sSubnetAvailabilityZone :: Lens' Subnet (Maybe AvailabilityZone)
sSubnetAvailabilityZone = lens _sSubnetAvailabilityZone (\ s a -> s{_sSubnetAvailabilityZone = a})

instance FromXML Subnet where
        parseXML x
          = Subnet' <$>
              (x .@? "SubnetIdentifier") <*>
                (x .@? "SubnetAvailabilityZone")

instance Hashable Subnet where

instance NFData Subnet where

-- | A cost allocation Tag that can be added to an ElastiCache cluster or replication group. Tags are composed of a Key/Value pair. A tag with a null Value is permitted.
--
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
-- * 'tagValue' - The tag's value. May be null.
--
-- * 'tagKey' - The key for the tag. May not be null.
tag
    :: Tag
tag = Tag' {_tagValue = Nothing, _tagKey = Nothing}


-- | The tag's value. May be null.
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\ s a -> s{_tagValue = a})

-- | The key for the tag. May not be null.
tagKey :: Lens' Tag (Maybe Text)
tagKey = lens _tagKey (\ s a -> s{_tagKey = a})

instance FromXML Tag where
        parseXML x
          = Tag' <$> (x .@? "Value") <*> (x .@? "Key")

instance Hashable Tag where

instance NFData Tag where

instance ToQuery Tag where
        toQuery Tag'{..}
          = mconcat ["Value" =: _tagValue, "Key" =: _tagKey]

-- | Represents the output from the @AddTagsToResource@ , @ListTagsForResource@ , and @RemoveTagsFromResource@ operations.
--
--
--
-- /See:/ 'tagListMessage' smart constructor.
newtype TagListMessage = TagListMessage'
  { _tlmTagList :: Maybe [Tag]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagListMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tlmTagList' - A list of cost allocation tags as key-value pairs.
tagListMessage
    :: TagListMessage
tagListMessage = TagListMessage' {_tlmTagList = Nothing}


-- | A list of cost allocation tags as key-value pairs.
tlmTagList :: Lens' TagListMessage [Tag]
tlmTagList = lens _tlmTagList (\ s a -> s{_tlmTagList = a}) . _Default . _Coerce

instance FromXML TagListMessage where
        parseXML x
          = TagListMessage' <$>
              (x .@? "TagList" .!@ mempty >>=
                 may (parseXMLList "Tag"))

instance Hashable TagListMessage where

instance NFData TagListMessage where
