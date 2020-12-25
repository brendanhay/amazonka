{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBCluster
  ( DBCluster (..),

    -- * Smart constructor
    mkDBCluster,

    -- * Lenses
    dbcActivityStreamKinesisStreamName,
    dbcActivityStreamKmsKeyId,
    dbcActivityStreamMode,
    dbcActivityStreamStatus,
    dbcAllocatedStorage,
    dbcAssociatedRoles,
    dbcAvailabilityZones,
    dbcBacktrackConsumedChangeRecords,
    dbcBacktrackWindow,
    dbcBackupRetentionPeriod,
    dbcCapacity,
    dbcCharacterSetName,
    dbcCloneGroupId,
    dbcClusterCreateTime,
    dbcCopyTagsToSnapshot,
    dbcCrossAccountClone,
    dbcCustomEndpoints,
    dbcDBClusterArn,
    dbcDBClusterIdentifier,
    dbcDBClusterMembers,
    dbcDBClusterOptionGroupMemberships,
    dbcDBClusterParameterGroup,
    dbcDBSubnetGroup,
    dbcDatabaseName,
    dbcDbClusterResourceId,
    dbcDeletionProtection,
    dbcDomainMemberships,
    dbcEarliestBacktrackTime,
    dbcEarliestRestorableTime,
    dbcEnabledCloudwatchLogsExports,
    dbcEndpoint,
    dbcEngine,
    dbcEngineMode,
    dbcEngineVersion,
    dbcGlobalWriteForwardingRequested,
    dbcGlobalWriteForwardingStatus,
    dbcHostedZoneId,
    dbcHttpEndpointEnabled,
    dbcIAMDatabaseAuthenticationEnabled,
    dbcKmsKeyId,
    dbcLatestRestorableTime,
    dbcMasterUsername,
    dbcMultiAZ,
    dbcPercentProgress,
    dbcPort,
    dbcPreferredBackupWindow,
    dbcPreferredMaintenanceWindow,
    dbcReadReplicaIdentifiers,
    dbcReaderEndpoint,
    dbcReplicationSourceIdentifier,
    dbcScalingConfigurationInfo,
    dbcStatus,
    dbcStorageEncrypted,
    dbcTagList,
    dbcVpcSecurityGroups,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.ActivityStreamKinesisStreamName as Types
import qualified Network.AWS.RDS.Types.ActivityStreamKmsKeyId as Types
import qualified Network.AWS.RDS.Types.ActivityStreamMode as Types
import qualified Network.AWS.RDS.Types.ActivityStreamStatus as Types
import qualified Network.AWS.RDS.Types.CharacterSetName as Types
import qualified Network.AWS.RDS.Types.CloneGroupId as Types
import qualified Network.AWS.RDS.Types.DBClusterArn as Types
import qualified Network.AWS.RDS.Types.DBClusterIdentifier as Types
import qualified Network.AWS.RDS.Types.DBClusterMember as Types
import qualified Network.AWS.RDS.Types.DBClusterOptionGroupStatus as Types
import qualified Network.AWS.RDS.Types.DBClusterRole as Types
import qualified Network.AWS.RDS.Types.DatabaseName as Types
import qualified Network.AWS.RDS.Types.DbClusterResourceId as Types
import qualified Network.AWS.RDS.Types.DomainMembership as Types
import qualified Network.AWS.RDS.Types.Engine as Types
import qualified Network.AWS.RDS.Types.EngineMode as Types
import qualified Network.AWS.RDS.Types.EngineVersion as Types
import qualified Network.AWS.RDS.Types.HostedZoneId as Types
import qualified Network.AWS.RDS.Types.KmsKeyId as Types
import qualified Network.AWS.RDS.Types.MasterUsername as Types
import qualified Network.AWS.RDS.Types.PercentProgress as Types
import qualified Network.AWS.RDS.Types.PreferredBackupWindow as Types
import qualified Network.AWS.RDS.Types.PreferredMaintenanceWindow as Types
import qualified Network.AWS.RDS.Types.ReaderEndpoint as Types
import qualified Network.AWS.RDS.Types.ReplicationSourceIdentifier as Types
import qualified Network.AWS.RDS.Types.ScalingConfigurationInfo as Types
import qualified Network.AWS.RDS.Types.Status as Types
import qualified Network.AWS.RDS.Types.String as Types
import qualified Network.AWS.RDS.Types.Tag as Types
import qualified Network.AWS.RDS.Types.VpcSecurityGroupMembership as Types
import qualified Network.AWS.RDS.Types.WriteForwardingStatus as Types

-- | Contains the details of an Amazon Aurora DB cluster.
--
-- This data type is used as a response element in the @DescribeDBClusters@ , @StopDBCluster@ , and @StartDBCluster@ actions.
--
-- /See:/ 'mkDBCluster' smart constructor.
data DBCluster = DBCluster'
  { -- | The name of the Amazon Kinesis data stream used for the database activity stream.
    activityStreamKinesisStreamName :: Core.Maybe Types.ActivityStreamKinesisStreamName,
    -- | The AWS KMS key identifier used for encrypting messages in the database activity stream.
    activityStreamKmsKeyId :: Core.Maybe Types.ActivityStreamKmsKeyId,
    -- | The mode of the database activity stream. Database events such as a change or access generate an activity stream event. The database session can handle these events either synchronously or asynchronously.
    activityStreamMode :: Core.Maybe Types.ActivityStreamMode,
    -- | The status of the database activity stream.
    activityStreamStatus :: Core.Maybe Types.ActivityStreamStatus,
    -- | For all database engines except Amazon Aurora, @AllocatedStorage@ specifies the allocated storage size in gibibytes (GiB). For Aurora, @AllocatedStorage@ always returns 1, because Aurora DB cluster storage size isn't fixed, but instead automatically adjusts as needed.
    allocatedStorage :: Core.Maybe Core.Int,
    -- | Provides a list of the AWS Identity and Access Management (IAM) roles that are associated with the DB cluster. IAM roles that are associated with a DB cluster grant permission for the DB cluster to access other AWS services on your behalf.
    associatedRoles :: Core.Maybe [Types.DBClusterRole],
    -- | Provides the list of Availability Zones (AZs) where instances in the DB cluster can be created.
    availabilityZones :: Core.Maybe [Types.String],
    -- | The number of change records stored for Backtrack.
    backtrackConsumedChangeRecords :: Core.Maybe Core.Integer,
    -- | The target backtrack window, in seconds. If this value is set to 0, backtracking is disabled for the DB cluster. Otherwise, backtracking is enabled.
    backtrackWindow :: Core.Maybe Core.Integer,
    -- | Specifies the number of days for which automatic DB snapshots are retained.
    backupRetentionPeriod :: Core.Maybe Core.Int,
    -- | The current capacity of an Aurora Serverless DB cluster. The capacity is 0 (zero) when the cluster is paused.
    --
    -- For more information about Aurora Serverless, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html Using Amazon Aurora Serverless> in the /Amazon Aurora User Guide/ .
    capacity :: Core.Maybe Core.Int,
    -- | If present, specifies the name of the character set that this cluster is associated with.
    characterSetName :: Core.Maybe Types.CharacterSetName,
    -- | Identifies the clone group to which the DB cluster is associated.
    cloneGroupId :: Core.Maybe Types.CloneGroupId,
    -- | Specifies the time when the DB cluster was created, in Universal Coordinated Time (UTC).
    clusterCreateTime :: Core.Maybe Core.UTCTime,
    -- | Specifies whether tags are copied from the DB cluster to snapshots of the DB cluster.
    copyTagsToSnapshot :: Core.Maybe Core.Bool,
    -- | Specifies whether the DB cluster is a clone of a DB cluster owned by a different AWS account.
    crossAccountClone :: Core.Maybe Core.Bool,
    -- | Identifies all custom endpoints associated with the cluster.
    customEndpoints :: Core.Maybe [Types.String],
    -- | The Amazon Resource Name (ARN) for the DB cluster.
    dBClusterArn :: Core.Maybe Types.DBClusterArn,
    -- | Contains a user-supplied DB cluster identifier. This identifier is the unique key that identifies a DB cluster.
    dBClusterIdentifier :: Core.Maybe Types.DBClusterIdentifier,
    -- | Provides the list of instances that make up the DB cluster.
    dBClusterMembers :: Core.Maybe [Types.DBClusterMember],
    -- | Provides the list of option group memberships for this DB cluster.
    dBClusterOptionGroupMemberships :: Core.Maybe [Types.DBClusterOptionGroupStatus],
    -- | Specifies the name of the DB cluster parameter group for the DB cluster.
    dBClusterParameterGroup :: Core.Maybe Types.String,
    -- | Specifies information on the subnet group associated with the DB cluster, including the name, description, and subnets in the subnet group.
    dBSubnetGroup :: Core.Maybe Types.String,
    -- | Contains the name of the initial database of this DB cluster that was provided at create time, if one was specified when the DB cluster was created. This same name is returned for the life of the DB cluster.
    databaseName :: Core.Maybe Types.DatabaseName,
    -- | The AWS Region-unique, immutable identifier for the DB cluster. This identifier is found in AWS CloudTrail log entries whenever the AWS KMS key for the DB cluster is accessed.
    dbClusterResourceId :: Core.Maybe Types.DbClusterResourceId,
    -- | Indicates if the DB cluster has deletion protection enabled. The database can't be deleted when deletion protection is enabled.
    deletionProtection :: Core.Maybe Core.Bool,
    -- | The Active Directory Domain membership records associated with the DB cluster.
    domainMemberships :: Core.Maybe [Types.DomainMembership],
    -- | The earliest time to which a DB cluster can be backtracked.
    earliestBacktrackTime :: Core.Maybe Core.UTCTime,
    -- | The earliest time to which a database can be restored with point-in-time restore.
    earliestRestorableTime :: Core.Maybe Core.UTCTime,
    -- | A list of log types that this DB cluster is configured to export to CloudWatch Logs.
    --
    -- Log types vary by DB engine. For information about the log types for each DB engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html Amazon RDS Database Log Files> in the /Amazon Aurora User Guide./
    enabledCloudwatchLogsExports :: Core.Maybe [Types.String],
    -- | Specifies the connection endpoint for the primary instance of the DB cluster.
    endpoint :: Core.Maybe Types.String,
    -- | The name of the database engine to be used for this DB cluster.
    engine :: Core.Maybe Types.Engine,
    -- | The DB engine mode of the DB cluster, either @provisioned@ , @serverless@ , @parallelquery@ , @global@ , or @multimaster@ .
    --
    -- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBCluster.html CreateDBCluster> .
    engineMode :: Core.Maybe Types.EngineMode,
    -- | Indicates the database engine version.
    engineVersion :: Core.Maybe Types.EngineVersion,
    -- | Specifies whether you have requested to enable write forwarding for a secondary cluster in an Aurora global database. Because write forwarding takes time to enable, check the value of @GlobalWriteForwardingStatus@ to confirm that the request has completed before using the write forwarding feature for this cluster.
    globalWriteForwardingRequested :: Core.Maybe Core.Bool,
    -- | Specifies whether a secondary cluster in an Aurora global database has write forwarding enabled, not enabled, or is in the process of enabling it.
    globalWriteForwardingStatus :: Core.Maybe Types.WriteForwardingStatus,
    -- | Specifies the ID that Amazon Route 53 assigns when you create a hosted zone.
    hostedZoneId :: Core.Maybe Types.HostedZoneId,
    -- | A value that indicates whether the HTTP endpoint for an Aurora Serverless DB cluster is enabled.
    --
    -- When enabled, the HTTP endpoint provides a connectionless web service API for running SQL queries on the Aurora Serverless DB cluster. You can also query your database from inside the RDS console with the query editor.
    -- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/data-api.html Using the Data API for Aurora Serverless> in the /Amazon Aurora User Guide/ .
    httpEndpointEnabled :: Core.Maybe Core.Bool,
    -- | A value that indicates whether the mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled.
    iAMDatabaseAuthenticationEnabled :: Core.Maybe Core.Bool,
    -- | If @StorageEncrypted@ is enabled, the AWS KMS key identifier for the encrypted DB cluster.
    kmsKeyId :: Core.Maybe Types.KmsKeyId,
    -- | Specifies the latest time to which a database can be restored with point-in-time restore.
    latestRestorableTime :: Core.Maybe Core.UTCTime,
    -- | Contains the master username for the DB cluster.
    masterUsername :: Core.Maybe Types.MasterUsername,
    -- | Specifies whether the DB cluster has instances in multiple Availability Zones.
    multiAZ :: Core.Maybe Core.Bool,
    -- | Specifies the progress of the operation as a percentage.
    percentProgress :: Core.Maybe Types.PercentProgress,
    -- | Specifies the port that the database engine is listening on.
    port :: Core.Maybe Core.Int,
    -- | Specifies the daily time range during which automated backups are created if automated backups are enabled, as determined by the @BackupRetentionPeriod@ .
    preferredBackupWindow :: Core.Maybe Types.PreferredBackupWindow,
    -- | Specifies the weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC).
    preferredMaintenanceWindow :: Core.Maybe Types.PreferredMaintenanceWindow,
    -- | Contains one or more identifiers of the read replicas associated with this DB cluster.
    readReplicaIdentifiers :: Core.Maybe [Types.String],
    -- | The reader endpoint for the DB cluster. The reader endpoint for a DB cluster load-balances connections across the Aurora Replicas that are available in a DB cluster. As clients request new connections to the reader endpoint, Aurora distributes the connection requests among the Aurora Replicas in the DB cluster. This functionality can help balance your read workload across multiple Aurora Replicas in your DB cluster.
    --
    -- If a failover occurs, and the Aurora Replica that you are connected to is promoted to be the primary instance, your connection is dropped. To continue sending your read workload to other Aurora Replicas in the cluster, you can then reconnect to the reader endpoint.
    readerEndpoint :: Core.Maybe Types.ReaderEndpoint,
    -- | Contains the identifier of the source DB cluster if this DB cluster is a read replica.
    replicationSourceIdentifier :: Core.Maybe Types.ReplicationSourceIdentifier,
    scalingConfigurationInfo :: Core.Maybe Types.ScalingConfigurationInfo,
    -- | Specifies the current state of this DB cluster.
    status :: Core.Maybe Types.Status,
    -- | Specifies whether the DB cluster is encrypted.
    storageEncrypted :: Core.Maybe Core.Bool,
    tagList :: Core.Maybe [Types.Tag],
    -- | Provides a list of VPC security groups that the DB cluster belongs to.
    vpcSecurityGroups :: Core.Maybe [Types.VpcSecurityGroupMembership]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DBCluster' value with any optional fields omitted.
mkDBCluster ::
  DBCluster
mkDBCluster =
  DBCluster'
    { activityStreamKinesisStreamName = Core.Nothing,
      activityStreamKmsKeyId = Core.Nothing,
      activityStreamMode = Core.Nothing,
      activityStreamStatus = Core.Nothing,
      allocatedStorage = Core.Nothing,
      associatedRoles = Core.Nothing,
      availabilityZones = Core.Nothing,
      backtrackConsumedChangeRecords = Core.Nothing,
      backtrackWindow = Core.Nothing,
      backupRetentionPeriod = Core.Nothing,
      capacity = Core.Nothing,
      characterSetName = Core.Nothing,
      cloneGroupId = Core.Nothing,
      clusterCreateTime = Core.Nothing,
      copyTagsToSnapshot = Core.Nothing,
      crossAccountClone = Core.Nothing,
      customEndpoints = Core.Nothing,
      dBClusterArn = Core.Nothing,
      dBClusterIdentifier = Core.Nothing,
      dBClusterMembers = Core.Nothing,
      dBClusterOptionGroupMemberships = Core.Nothing,
      dBClusterParameterGroup = Core.Nothing,
      dBSubnetGroup = Core.Nothing,
      databaseName = Core.Nothing,
      dbClusterResourceId = Core.Nothing,
      deletionProtection = Core.Nothing,
      domainMemberships = Core.Nothing,
      earliestBacktrackTime = Core.Nothing,
      earliestRestorableTime = Core.Nothing,
      enabledCloudwatchLogsExports = Core.Nothing,
      endpoint = Core.Nothing,
      engine = Core.Nothing,
      engineMode = Core.Nothing,
      engineVersion = Core.Nothing,
      globalWriteForwardingRequested = Core.Nothing,
      globalWriteForwardingStatus = Core.Nothing,
      hostedZoneId = Core.Nothing,
      httpEndpointEnabled = Core.Nothing,
      iAMDatabaseAuthenticationEnabled = Core.Nothing,
      kmsKeyId = Core.Nothing,
      latestRestorableTime = Core.Nothing,
      masterUsername = Core.Nothing,
      multiAZ = Core.Nothing,
      percentProgress = Core.Nothing,
      port = Core.Nothing,
      preferredBackupWindow = Core.Nothing,
      preferredMaintenanceWindow = Core.Nothing,
      readReplicaIdentifiers = Core.Nothing,
      readerEndpoint = Core.Nothing,
      replicationSourceIdentifier = Core.Nothing,
      scalingConfigurationInfo = Core.Nothing,
      status = Core.Nothing,
      storageEncrypted = Core.Nothing,
      tagList = Core.Nothing,
      vpcSecurityGroups = Core.Nothing
    }

-- | The name of the Amazon Kinesis data stream used for the database activity stream.
--
-- /Note:/ Consider using 'activityStreamKinesisStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcActivityStreamKinesisStreamName :: Lens.Lens' DBCluster (Core.Maybe Types.ActivityStreamKinesisStreamName)
dbcActivityStreamKinesisStreamName = Lens.field @"activityStreamKinesisStreamName"
{-# DEPRECATED dbcActivityStreamKinesisStreamName "Use generic-lens or generic-optics with 'activityStreamKinesisStreamName' instead." #-}

-- | The AWS KMS key identifier used for encrypting messages in the database activity stream.
--
-- /Note:/ Consider using 'activityStreamKmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcActivityStreamKmsKeyId :: Lens.Lens' DBCluster (Core.Maybe Types.ActivityStreamKmsKeyId)
dbcActivityStreamKmsKeyId = Lens.field @"activityStreamKmsKeyId"
{-# DEPRECATED dbcActivityStreamKmsKeyId "Use generic-lens or generic-optics with 'activityStreamKmsKeyId' instead." #-}

-- | The mode of the database activity stream. Database events such as a change or access generate an activity stream event. The database session can handle these events either synchronously or asynchronously.
--
-- /Note:/ Consider using 'activityStreamMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcActivityStreamMode :: Lens.Lens' DBCluster (Core.Maybe Types.ActivityStreamMode)
dbcActivityStreamMode = Lens.field @"activityStreamMode"
{-# DEPRECATED dbcActivityStreamMode "Use generic-lens or generic-optics with 'activityStreamMode' instead." #-}

-- | The status of the database activity stream.
--
-- /Note:/ Consider using 'activityStreamStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcActivityStreamStatus :: Lens.Lens' DBCluster (Core.Maybe Types.ActivityStreamStatus)
dbcActivityStreamStatus = Lens.field @"activityStreamStatus"
{-# DEPRECATED dbcActivityStreamStatus "Use generic-lens or generic-optics with 'activityStreamStatus' instead." #-}

-- | For all database engines except Amazon Aurora, @AllocatedStorage@ specifies the allocated storage size in gibibytes (GiB). For Aurora, @AllocatedStorage@ always returns 1, because Aurora DB cluster storage size isn't fixed, but instead automatically adjusts as needed.
--
-- /Note:/ Consider using 'allocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcAllocatedStorage :: Lens.Lens' DBCluster (Core.Maybe Core.Int)
dbcAllocatedStorage = Lens.field @"allocatedStorage"
{-# DEPRECATED dbcAllocatedStorage "Use generic-lens or generic-optics with 'allocatedStorage' instead." #-}

-- | Provides a list of the AWS Identity and Access Management (IAM) roles that are associated with the DB cluster. IAM roles that are associated with a DB cluster grant permission for the DB cluster to access other AWS services on your behalf.
--
-- /Note:/ Consider using 'associatedRoles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcAssociatedRoles :: Lens.Lens' DBCluster (Core.Maybe [Types.DBClusterRole])
dbcAssociatedRoles = Lens.field @"associatedRoles"
{-# DEPRECATED dbcAssociatedRoles "Use generic-lens or generic-optics with 'associatedRoles' instead." #-}

-- | Provides the list of Availability Zones (AZs) where instances in the DB cluster can be created.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcAvailabilityZones :: Lens.Lens' DBCluster (Core.Maybe [Types.String])
dbcAvailabilityZones = Lens.field @"availabilityZones"
{-# DEPRECATED dbcAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The number of change records stored for Backtrack.
--
-- /Note:/ Consider using 'backtrackConsumedChangeRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcBacktrackConsumedChangeRecords :: Lens.Lens' DBCluster (Core.Maybe Core.Integer)
dbcBacktrackConsumedChangeRecords = Lens.field @"backtrackConsumedChangeRecords"
{-# DEPRECATED dbcBacktrackConsumedChangeRecords "Use generic-lens or generic-optics with 'backtrackConsumedChangeRecords' instead." #-}

-- | The target backtrack window, in seconds. If this value is set to 0, backtracking is disabled for the DB cluster. Otherwise, backtracking is enabled.
--
-- /Note:/ Consider using 'backtrackWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcBacktrackWindow :: Lens.Lens' DBCluster (Core.Maybe Core.Integer)
dbcBacktrackWindow = Lens.field @"backtrackWindow"
{-# DEPRECATED dbcBacktrackWindow "Use generic-lens or generic-optics with 'backtrackWindow' instead." #-}

-- | Specifies the number of days for which automatic DB snapshots are retained.
--
-- /Note:/ Consider using 'backupRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcBackupRetentionPeriod :: Lens.Lens' DBCluster (Core.Maybe Core.Int)
dbcBackupRetentionPeriod = Lens.field @"backupRetentionPeriod"
{-# DEPRECATED dbcBackupRetentionPeriod "Use generic-lens or generic-optics with 'backupRetentionPeriod' instead." #-}

-- | The current capacity of an Aurora Serverless DB cluster. The capacity is 0 (zero) when the cluster is paused.
--
-- For more information about Aurora Serverless, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html Using Amazon Aurora Serverless> in the /Amazon Aurora User Guide/ .
--
-- /Note:/ Consider using 'capacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcCapacity :: Lens.Lens' DBCluster (Core.Maybe Core.Int)
dbcCapacity = Lens.field @"capacity"
{-# DEPRECATED dbcCapacity "Use generic-lens or generic-optics with 'capacity' instead." #-}

-- | If present, specifies the name of the character set that this cluster is associated with.
--
-- /Note:/ Consider using 'characterSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcCharacterSetName :: Lens.Lens' DBCluster (Core.Maybe Types.CharacterSetName)
dbcCharacterSetName = Lens.field @"characterSetName"
{-# DEPRECATED dbcCharacterSetName "Use generic-lens or generic-optics with 'characterSetName' instead." #-}

-- | Identifies the clone group to which the DB cluster is associated.
--
-- /Note:/ Consider using 'cloneGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcCloneGroupId :: Lens.Lens' DBCluster (Core.Maybe Types.CloneGroupId)
dbcCloneGroupId = Lens.field @"cloneGroupId"
{-# DEPRECATED dbcCloneGroupId "Use generic-lens or generic-optics with 'cloneGroupId' instead." #-}

-- | Specifies the time when the DB cluster was created, in Universal Coordinated Time (UTC).
--
-- /Note:/ Consider using 'clusterCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcClusterCreateTime :: Lens.Lens' DBCluster (Core.Maybe Core.UTCTime)
dbcClusterCreateTime = Lens.field @"clusterCreateTime"
{-# DEPRECATED dbcClusterCreateTime "Use generic-lens or generic-optics with 'clusterCreateTime' instead." #-}

-- | Specifies whether tags are copied from the DB cluster to snapshots of the DB cluster.
--
-- /Note:/ Consider using 'copyTagsToSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcCopyTagsToSnapshot :: Lens.Lens' DBCluster (Core.Maybe Core.Bool)
dbcCopyTagsToSnapshot = Lens.field @"copyTagsToSnapshot"
{-# DEPRECATED dbcCopyTagsToSnapshot "Use generic-lens or generic-optics with 'copyTagsToSnapshot' instead." #-}

-- | Specifies whether the DB cluster is a clone of a DB cluster owned by a different AWS account.
--
-- /Note:/ Consider using 'crossAccountClone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcCrossAccountClone :: Lens.Lens' DBCluster (Core.Maybe Core.Bool)
dbcCrossAccountClone = Lens.field @"crossAccountClone"
{-# DEPRECATED dbcCrossAccountClone "Use generic-lens or generic-optics with 'crossAccountClone' instead." #-}

-- | Identifies all custom endpoints associated with the cluster.
--
-- /Note:/ Consider using 'customEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcCustomEndpoints :: Lens.Lens' DBCluster (Core.Maybe [Types.String])
dbcCustomEndpoints = Lens.field @"customEndpoints"
{-# DEPRECATED dbcCustomEndpoints "Use generic-lens or generic-optics with 'customEndpoints' instead." #-}

-- | The Amazon Resource Name (ARN) for the DB cluster.
--
-- /Note:/ Consider using 'dBClusterArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcDBClusterArn :: Lens.Lens' DBCluster (Core.Maybe Types.DBClusterArn)
dbcDBClusterArn = Lens.field @"dBClusterArn"
{-# DEPRECATED dbcDBClusterArn "Use generic-lens or generic-optics with 'dBClusterArn' instead." #-}

-- | Contains a user-supplied DB cluster identifier. This identifier is the unique key that identifies a DB cluster.
--
-- /Note:/ Consider using 'dBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcDBClusterIdentifier :: Lens.Lens' DBCluster (Core.Maybe Types.DBClusterIdentifier)
dbcDBClusterIdentifier = Lens.field @"dBClusterIdentifier"
{-# DEPRECATED dbcDBClusterIdentifier "Use generic-lens or generic-optics with 'dBClusterIdentifier' instead." #-}

-- | Provides the list of instances that make up the DB cluster.
--
-- /Note:/ Consider using 'dBClusterMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcDBClusterMembers :: Lens.Lens' DBCluster (Core.Maybe [Types.DBClusterMember])
dbcDBClusterMembers = Lens.field @"dBClusterMembers"
{-# DEPRECATED dbcDBClusterMembers "Use generic-lens or generic-optics with 'dBClusterMembers' instead." #-}

-- | Provides the list of option group memberships for this DB cluster.
--
-- /Note:/ Consider using 'dBClusterOptionGroupMemberships' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcDBClusterOptionGroupMemberships :: Lens.Lens' DBCluster (Core.Maybe [Types.DBClusterOptionGroupStatus])
dbcDBClusterOptionGroupMemberships = Lens.field @"dBClusterOptionGroupMemberships"
{-# DEPRECATED dbcDBClusterOptionGroupMemberships "Use generic-lens or generic-optics with 'dBClusterOptionGroupMemberships' instead." #-}

-- | Specifies the name of the DB cluster parameter group for the DB cluster.
--
-- /Note:/ Consider using 'dBClusterParameterGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcDBClusterParameterGroup :: Lens.Lens' DBCluster (Core.Maybe Types.String)
dbcDBClusterParameterGroup = Lens.field @"dBClusterParameterGroup"
{-# DEPRECATED dbcDBClusterParameterGroup "Use generic-lens or generic-optics with 'dBClusterParameterGroup' instead." #-}

-- | Specifies information on the subnet group associated with the DB cluster, including the name, description, and subnets in the subnet group.
--
-- /Note:/ Consider using 'dBSubnetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcDBSubnetGroup :: Lens.Lens' DBCluster (Core.Maybe Types.String)
dbcDBSubnetGroup = Lens.field @"dBSubnetGroup"
{-# DEPRECATED dbcDBSubnetGroup "Use generic-lens or generic-optics with 'dBSubnetGroup' instead." #-}

-- | Contains the name of the initial database of this DB cluster that was provided at create time, if one was specified when the DB cluster was created. This same name is returned for the life of the DB cluster.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcDatabaseName :: Lens.Lens' DBCluster (Core.Maybe Types.DatabaseName)
dbcDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED dbcDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The AWS Region-unique, immutable identifier for the DB cluster. This identifier is found in AWS CloudTrail log entries whenever the AWS KMS key for the DB cluster is accessed.
--
-- /Note:/ Consider using 'dbClusterResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcDbClusterResourceId :: Lens.Lens' DBCluster (Core.Maybe Types.DbClusterResourceId)
dbcDbClusterResourceId = Lens.field @"dbClusterResourceId"
{-# DEPRECATED dbcDbClusterResourceId "Use generic-lens or generic-optics with 'dbClusterResourceId' instead." #-}

-- | Indicates if the DB cluster has deletion protection enabled. The database can't be deleted when deletion protection is enabled.
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcDeletionProtection :: Lens.Lens' DBCluster (Core.Maybe Core.Bool)
dbcDeletionProtection = Lens.field @"deletionProtection"
{-# DEPRECATED dbcDeletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead." #-}

-- | The Active Directory Domain membership records associated with the DB cluster.
--
-- /Note:/ Consider using 'domainMemberships' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcDomainMemberships :: Lens.Lens' DBCluster (Core.Maybe [Types.DomainMembership])
dbcDomainMemberships = Lens.field @"domainMemberships"
{-# DEPRECATED dbcDomainMemberships "Use generic-lens or generic-optics with 'domainMemberships' instead." #-}

-- | The earliest time to which a DB cluster can be backtracked.
--
-- /Note:/ Consider using 'earliestBacktrackTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcEarliestBacktrackTime :: Lens.Lens' DBCluster (Core.Maybe Core.UTCTime)
dbcEarliestBacktrackTime = Lens.field @"earliestBacktrackTime"
{-# DEPRECATED dbcEarliestBacktrackTime "Use generic-lens or generic-optics with 'earliestBacktrackTime' instead." #-}

-- | The earliest time to which a database can be restored with point-in-time restore.
--
-- /Note:/ Consider using 'earliestRestorableTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcEarliestRestorableTime :: Lens.Lens' DBCluster (Core.Maybe Core.UTCTime)
dbcEarliestRestorableTime = Lens.field @"earliestRestorableTime"
{-# DEPRECATED dbcEarliestRestorableTime "Use generic-lens or generic-optics with 'earliestRestorableTime' instead." #-}

-- | A list of log types that this DB cluster is configured to export to CloudWatch Logs.
--
-- Log types vary by DB engine. For information about the log types for each DB engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html Amazon RDS Database Log Files> in the /Amazon Aurora User Guide./
--
-- /Note:/ Consider using 'enabledCloudwatchLogsExports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcEnabledCloudwatchLogsExports :: Lens.Lens' DBCluster (Core.Maybe [Types.String])
dbcEnabledCloudwatchLogsExports = Lens.field @"enabledCloudwatchLogsExports"
{-# DEPRECATED dbcEnabledCloudwatchLogsExports "Use generic-lens or generic-optics with 'enabledCloudwatchLogsExports' instead." #-}

-- | Specifies the connection endpoint for the primary instance of the DB cluster.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcEndpoint :: Lens.Lens' DBCluster (Core.Maybe Types.String)
dbcEndpoint = Lens.field @"endpoint"
{-# DEPRECATED dbcEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

-- | The name of the database engine to be used for this DB cluster.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcEngine :: Lens.Lens' DBCluster (Core.Maybe Types.Engine)
dbcEngine = Lens.field @"engine"
{-# DEPRECATED dbcEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The DB engine mode of the DB cluster, either @provisioned@ , @serverless@ , @parallelquery@ , @global@ , or @multimaster@ .
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBCluster.html CreateDBCluster> .
--
-- /Note:/ Consider using 'engineMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcEngineMode :: Lens.Lens' DBCluster (Core.Maybe Types.EngineMode)
dbcEngineMode = Lens.field @"engineMode"
{-# DEPRECATED dbcEngineMode "Use generic-lens or generic-optics with 'engineMode' instead." #-}

-- | Indicates the database engine version.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcEngineVersion :: Lens.Lens' DBCluster (Core.Maybe Types.EngineVersion)
dbcEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED dbcEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | Specifies whether you have requested to enable write forwarding for a secondary cluster in an Aurora global database. Because write forwarding takes time to enable, check the value of @GlobalWriteForwardingStatus@ to confirm that the request has completed before using the write forwarding feature for this cluster.
--
-- /Note:/ Consider using 'globalWriteForwardingRequested' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcGlobalWriteForwardingRequested :: Lens.Lens' DBCluster (Core.Maybe Core.Bool)
dbcGlobalWriteForwardingRequested = Lens.field @"globalWriteForwardingRequested"
{-# DEPRECATED dbcGlobalWriteForwardingRequested "Use generic-lens or generic-optics with 'globalWriteForwardingRequested' instead." #-}

-- | Specifies whether a secondary cluster in an Aurora global database has write forwarding enabled, not enabled, or is in the process of enabling it.
--
-- /Note:/ Consider using 'globalWriteForwardingStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcGlobalWriteForwardingStatus :: Lens.Lens' DBCluster (Core.Maybe Types.WriteForwardingStatus)
dbcGlobalWriteForwardingStatus = Lens.field @"globalWriteForwardingStatus"
{-# DEPRECATED dbcGlobalWriteForwardingStatus "Use generic-lens or generic-optics with 'globalWriteForwardingStatus' instead." #-}

-- | Specifies the ID that Amazon Route 53 assigns when you create a hosted zone.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcHostedZoneId :: Lens.Lens' DBCluster (Core.Maybe Types.HostedZoneId)
dbcHostedZoneId = Lens.field @"hostedZoneId"
{-# DEPRECATED dbcHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | A value that indicates whether the HTTP endpoint for an Aurora Serverless DB cluster is enabled.
--
-- When enabled, the HTTP endpoint provides a connectionless web service API for running SQL queries on the Aurora Serverless DB cluster. You can also query your database from inside the RDS console with the query editor.
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/data-api.html Using the Data API for Aurora Serverless> in the /Amazon Aurora User Guide/ .
--
-- /Note:/ Consider using 'httpEndpointEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcHttpEndpointEnabled :: Lens.Lens' DBCluster (Core.Maybe Core.Bool)
dbcHttpEndpointEnabled = Lens.field @"httpEndpointEnabled"
{-# DEPRECATED dbcHttpEndpointEnabled "Use generic-lens or generic-optics with 'httpEndpointEnabled' instead." #-}

-- | A value that indicates whether the mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled.
--
-- /Note:/ Consider using 'iAMDatabaseAuthenticationEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcIAMDatabaseAuthenticationEnabled :: Lens.Lens' DBCluster (Core.Maybe Core.Bool)
dbcIAMDatabaseAuthenticationEnabled = Lens.field @"iAMDatabaseAuthenticationEnabled"
{-# DEPRECATED dbcIAMDatabaseAuthenticationEnabled "Use generic-lens or generic-optics with 'iAMDatabaseAuthenticationEnabled' instead." #-}

-- | If @StorageEncrypted@ is enabled, the AWS KMS key identifier for the encrypted DB cluster.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcKmsKeyId :: Lens.Lens' DBCluster (Core.Maybe Types.KmsKeyId)
dbcKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED dbcKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | Specifies the latest time to which a database can be restored with point-in-time restore.
--
-- /Note:/ Consider using 'latestRestorableTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcLatestRestorableTime :: Lens.Lens' DBCluster (Core.Maybe Core.UTCTime)
dbcLatestRestorableTime = Lens.field @"latestRestorableTime"
{-# DEPRECATED dbcLatestRestorableTime "Use generic-lens or generic-optics with 'latestRestorableTime' instead." #-}

-- | Contains the master username for the DB cluster.
--
-- /Note:/ Consider using 'masterUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcMasterUsername :: Lens.Lens' DBCluster (Core.Maybe Types.MasterUsername)
dbcMasterUsername = Lens.field @"masterUsername"
{-# DEPRECATED dbcMasterUsername "Use generic-lens or generic-optics with 'masterUsername' instead." #-}

-- | Specifies whether the DB cluster has instances in multiple Availability Zones.
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcMultiAZ :: Lens.Lens' DBCluster (Core.Maybe Core.Bool)
dbcMultiAZ = Lens.field @"multiAZ"
{-# DEPRECATED dbcMultiAZ "Use generic-lens or generic-optics with 'multiAZ' instead." #-}

-- | Specifies the progress of the operation as a percentage.
--
-- /Note:/ Consider using 'percentProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcPercentProgress :: Lens.Lens' DBCluster (Core.Maybe Types.PercentProgress)
dbcPercentProgress = Lens.field @"percentProgress"
{-# DEPRECATED dbcPercentProgress "Use generic-lens or generic-optics with 'percentProgress' instead." #-}

-- | Specifies the port that the database engine is listening on.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcPort :: Lens.Lens' DBCluster (Core.Maybe Core.Int)
dbcPort = Lens.field @"port"
{-# DEPRECATED dbcPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | Specifies the daily time range during which automated backups are created if automated backups are enabled, as determined by the @BackupRetentionPeriod@ .
--
-- /Note:/ Consider using 'preferredBackupWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcPreferredBackupWindow :: Lens.Lens' DBCluster (Core.Maybe Types.PreferredBackupWindow)
dbcPreferredBackupWindow = Lens.field @"preferredBackupWindow"
{-# DEPRECATED dbcPreferredBackupWindow "Use generic-lens or generic-optics with 'preferredBackupWindow' instead." #-}

-- | Specifies the weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC).
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcPreferredMaintenanceWindow :: Lens.Lens' DBCluster (Core.Maybe Types.PreferredMaintenanceWindow)
dbcPreferredMaintenanceWindow = Lens.field @"preferredMaintenanceWindow"
{-# DEPRECATED dbcPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | Contains one or more identifiers of the read replicas associated with this DB cluster.
--
-- /Note:/ Consider using 'readReplicaIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcReadReplicaIdentifiers :: Lens.Lens' DBCluster (Core.Maybe [Types.String])
dbcReadReplicaIdentifiers = Lens.field @"readReplicaIdentifiers"
{-# DEPRECATED dbcReadReplicaIdentifiers "Use generic-lens or generic-optics with 'readReplicaIdentifiers' instead." #-}

-- | The reader endpoint for the DB cluster. The reader endpoint for a DB cluster load-balances connections across the Aurora Replicas that are available in a DB cluster. As clients request new connections to the reader endpoint, Aurora distributes the connection requests among the Aurora Replicas in the DB cluster. This functionality can help balance your read workload across multiple Aurora Replicas in your DB cluster.
--
-- If a failover occurs, and the Aurora Replica that you are connected to is promoted to be the primary instance, your connection is dropped. To continue sending your read workload to other Aurora Replicas in the cluster, you can then reconnect to the reader endpoint.
--
-- /Note:/ Consider using 'readerEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcReaderEndpoint :: Lens.Lens' DBCluster (Core.Maybe Types.ReaderEndpoint)
dbcReaderEndpoint = Lens.field @"readerEndpoint"
{-# DEPRECATED dbcReaderEndpoint "Use generic-lens or generic-optics with 'readerEndpoint' instead." #-}

-- | Contains the identifier of the source DB cluster if this DB cluster is a read replica.
--
-- /Note:/ Consider using 'replicationSourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcReplicationSourceIdentifier :: Lens.Lens' DBCluster (Core.Maybe Types.ReplicationSourceIdentifier)
dbcReplicationSourceIdentifier = Lens.field @"replicationSourceIdentifier"
{-# DEPRECATED dbcReplicationSourceIdentifier "Use generic-lens or generic-optics with 'replicationSourceIdentifier' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'scalingConfigurationInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcScalingConfigurationInfo :: Lens.Lens' DBCluster (Core.Maybe Types.ScalingConfigurationInfo)
dbcScalingConfigurationInfo = Lens.field @"scalingConfigurationInfo"
{-# DEPRECATED dbcScalingConfigurationInfo "Use generic-lens or generic-optics with 'scalingConfigurationInfo' instead." #-}

-- | Specifies the current state of this DB cluster.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcStatus :: Lens.Lens' DBCluster (Core.Maybe Types.Status)
dbcStatus = Lens.field @"status"
{-# DEPRECATED dbcStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Specifies whether the DB cluster is encrypted.
--
-- /Note:/ Consider using 'storageEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcStorageEncrypted :: Lens.Lens' DBCluster (Core.Maybe Core.Bool)
dbcStorageEncrypted = Lens.field @"storageEncrypted"
{-# DEPRECATED dbcStorageEncrypted "Use generic-lens or generic-optics with 'storageEncrypted' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcTagList :: Lens.Lens' DBCluster (Core.Maybe [Types.Tag])
dbcTagList = Lens.field @"tagList"
{-# DEPRECATED dbcTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

-- | Provides a list of VPC security groups that the DB cluster belongs to.
--
-- /Note:/ Consider using 'vpcSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dbcVpcSecurityGroups :: Lens.Lens' DBCluster (Core.Maybe [Types.VpcSecurityGroupMembership])
dbcVpcSecurityGroups = Lens.field @"vpcSecurityGroups"
{-# DEPRECATED dbcVpcSecurityGroups "Use generic-lens or generic-optics with 'vpcSecurityGroups' instead." #-}

instance Core.FromXML DBCluster where
  parseXML x =
    DBCluster'
      Core.<$> (x Core..@? "ActivityStreamKinesisStreamName")
      Core.<*> (x Core..@? "ActivityStreamKmsKeyId")
      Core.<*> (x Core..@? "ActivityStreamMode")
      Core.<*> (x Core..@? "ActivityStreamStatus")
      Core.<*> (x Core..@? "AllocatedStorage")
      Core.<*> ( x Core..@? "AssociatedRoles"
                   Core..<@> Core.parseXMLList "DBClusterRole"
               )
      Core.<*> ( x Core..@? "AvailabilityZones"
                   Core..<@> Core.parseXMLList "AvailabilityZone"
               )
      Core.<*> (x Core..@? "BacktrackConsumedChangeRecords")
      Core.<*> (x Core..@? "BacktrackWindow")
      Core.<*> (x Core..@? "BackupRetentionPeriod")
      Core.<*> (x Core..@? "Capacity")
      Core.<*> (x Core..@? "CharacterSetName")
      Core.<*> (x Core..@? "CloneGroupId")
      Core.<*> (x Core..@? "ClusterCreateTime")
      Core.<*> (x Core..@? "CopyTagsToSnapshot")
      Core.<*> (x Core..@? "CrossAccountClone")
      Core.<*> (x Core..@? "CustomEndpoints" Core..<@> Core.parseXMLList "member")
      Core.<*> (x Core..@? "DBClusterArn")
      Core.<*> (x Core..@? "DBClusterIdentifier")
      Core.<*> ( x Core..@? "DBClusterMembers"
                   Core..<@> Core.parseXMLList "DBClusterMember"
               )
      Core.<*> ( x Core..@? "DBClusterOptionGroupMemberships"
                   Core..<@> Core.parseXMLList "DBClusterOptionGroup"
               )
      Core.<*> (x Core..@? "DBClusterParameterGroup")
      Core.<*> (x Core..@? "DBSubnetGroup")
      Core.<*> (x Core..@? "DatabaseName")
      Core.<*> (x Core..@? "DbClusterResourceId")
      Core.<*> (x Core..@? "DeletionProtection")
      Core.<*> ( x Core..@? "DomainMemberships"
                   Core..<@> Core.parseXMLList "DomainMembership"
               )
      Core.<*> (x Core..@? "EarliestBacktrackTime")
      Core.<*> (x Core..@? "EarliestRestorableTime")
      Core.<*> ( x Core..@? "EnabledCloudwatchLogsExports"
                   Core..<@> Core.parseXMLList "member"
               )
      Core.<*> (x Core..@? "Endpoint")
      Core.<*> (x Core..@? "Engine")
      Core.<*> (x Core..@? "EngineMode")
      Core.<*> (x Core..@? "EngineVersion")
      Core.<*> (x Core..@? "GlobalWriteForwardingRequested")
      Core.<*> (x Core..@? "GlobalWriteForwardingStatus")
      Core.<*> (x Core..@? "HostedZoneId")
      Core.<*> (x Core..@? "HttpEndpointEnabled")
      Core.<*> (x Core..@? "IAMDatabaseAuthenticationEnabled")
      Core.<*> (x Core..@? "KmsKeyId")
      Core.<*> (x Core..@? "LatestRestorableTime")
      Core.<*> (x Core..@? "MasterUsername")
      Core.<*> (x Core..@? "MultiAZ")
      Core.<*> (x Core..@? "PercentProgress")
      Core.<*> (x Core..@? "Port")
      Core.<*> (x Core..@? "PreferredBackupWindow")
      Core.<*> (x Core..@? "PreferredMaintenanceWindow")
      Core.<*> ( x Core..@? "ReadReplicaIdentifiers"
                   Core..<@> Core.parseXMLList "ReadReplicaIdentifier"
               )
      Core.<*> (x Core..@? "ReaderEndpoint")
      Core.<*> (x Core..@? "ReplicationSourceIdentifier")
      Core.<*> (x Core..@? "ScalingConfigurationInfo")
      Core.<*> (x Core..@? "Status")
      Core.<*> (x Core..@? "StorageEncrypted")
      Core.<*> (x Core..@? "TagList" Core..<@> Core.parseXMLList "Tag")
      Core.<*> ( x Core..@? "VpcSecurityGroups"
                   Core..<@> Core.parseXMLList "VpcSecurityGroupMembership"
               )
