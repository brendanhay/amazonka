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
    dcBacktrackConsumedChangeRecords,
    dcEngineVersion,
    dcStatus,
    dcDeletionProtection,
    dcStorageEncrypted,
    dcDBClusterIdentifier,
    dcDBClusterMembers,
    dcReadReplicaIdentifiers,
    dcReplicationSourceIdentifier,
    dcActivityStreamKinesisStreamName,
    dcHostedZoneId,
    dcDBClusterParameterGroup,
    dcMasterUsername,
    dcIAMDatabaseAuthenticationEnabled,
    dcGlobalWriteForwardingRequested,
    dcEarliestBacktrackTime,
    dcBacktrackWindow,
    dcTagList,
    dcDBClusterResourceId,
    dcEarliestRestorableTime,
    dcCustomEndpoints,
    dcEngine,
    dcHTTPEndpointEnabled,
    dcDBClusterARN,
    dcCloneGroupId,
    dcLatestRestorableTime,
    dcCrossAccountClone,
    dcCapacity,
    dcPreferredMaintenanceWindow,
    dcAvailabilityZones,
    dcCharacterSetName,
    dcKMSKeyId,
    dcPreferredBackupWindow,
    dcAssociatedRoles,
    dcVPCSecurityGroups,
    dcBackupRetentionPeriod,
    dcDBSubnetGroup,
    dcActivityStreamMode,
    dcDatabaseName,
    dcMultiAZ,
    dcEngineMode,
    dcEnabledCloudwatchLogsExports,
    dcActivityStreamStatus,
    dcAllocatedStorage,
    dcCopyTagsToSnapshot,
    dcClusterCreateTime,
    dcEndpoint,
    dcScalingConfigurationInfo,
    dcActivityStreamKMSKeyId,
    dcPercentProgress,
    dcReaderEndpoint,
    dcGlobalWriteForwardingStatus,
    dcPort,
    dcDomainMemberships,
    dcDBClusterOptionGroupMemberships,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types.ActivityStreamMode
import Network.AWS.RDS.Types.ActivityStreamStatus
import Network.AWS.RDS.Types.DBClusterMember
import Network.AWS.RDS.Types.DBClusterOptionGroupStatus
import Network.AWS.RDS.Types.DBClusterRole
import Network.AWS.RDS.Types.DomainMembership
import Network.AWS.RDS.Types.ScalingConfigurationInfo
import Network.AWS.RDS.Types.Tag
import Network.AWS.RDS.Types.VPCSecurityGroupMembership
import Network.AWS.RDS.Types.WriteForwardingStatus

-- | Contains the details of an Amazon Aurora DB cluster.
--
-- This data type is used as a response element in the @DescribeDBClusters@ , @StopDBCluster@ , and @StartDBCluster@ actions.
--
-- /See:/ 'mkDBCluster' smart constructor.
data DBCluster = DBCluster'
  { backtrackConsumedChangeRecords ::
      Lude.Maybe Lude.Integer,
    engineVersion :: Lude.Maybe Lude.Text,
    status :: Lude.Maybe Lude.Text,
    deletionProtection :: Lude.Maybe Lude.Bool,
    storageEncrypted :: Lude.Maybe Lude.Bool,
    dbClusterIdentifier :: Lude.Maybe Lude.Text,
    dbClusterMembers :: Lude.Maybe [DBClusterMember],
    readReplicaIdentifiers :: Lude.Maybe [Lude.Text],
    replicationSourceIdentifier :: Lude.Maybe Lude.Text,
    activityStreamKinesisStreamName :: Lude.Maybe Lude.Text,
    hostedZoneId :: Lude.Maybe Lude.Text,
    dbClusterParameterGroup :: Lude.Maybe Lude.Text,
    masterUsername :: Lude.Maybe Lude.Text,
    iamDatabaseAuthenticationEnabled :: Lude.Maybe Lude.Bool,
    globalWriteForwardingRequested :: Lude.Maybe Lude.Bool,
    earliestBacktrackTime :: Lude.Maybe Lude.DateTime,
    backtrackWindow :: Lude.Maybe Lude.Integer,
    tagList :: Lude.Maybe [Tag],
    dbClusterResourceId :: Lude.Maybe Lude.Text,
    earliestRestorableTime :: Lude.Maybe Lude.DateTime,
    customEndpoints :: Lude.Maybe [Lude.Text],
    engine :: Lude.Maybe Lude.Text,
    hTTPEndpointEnabled :: Lude.Maybe Lude.Bool,
    dbClusterARN :: Lude.Maybe Lude.Text,
    cloneGroupId :: Lude.Maybe Lude.Text,
    latestRestorableTime :: Lude.Maybe Lude.DateTime,
    crossAccountClone :: Lude.Maybe Lude.Bool,
    capacity :: Lude.Maybe Lude.Int,
    preferredMaintenanceWindow :: Lude.Maybe Lude.Text,
    availabilityZones :: Lude.Maybe [Lude.Text],
    characterSetName :: Lude.Maybe Lude.Text,
    kmsKeyId :: Lude.Maybe Lude.Text,
    preferredBackupWindow :: Lude.Maybe Lude.Text,
    associatedRoles :: Lude.Maybe [DBClusterRole],
    vpcSecurityGroups :: Lude.Maybe [VPCSecurityGroupMembership],
    backupRetentionPeriod :: Lude.Maybe Lude.Int,
    dbSubnetGroup :: Lude.Maybe Lude.Text,
    activityStreamMode :: Lude.Maybe ActivityStreamMode,
    databaseName :: Lude.Maybe Lude.Text,
    multiAZ :: Lude.Maybe Lude.Bool,
    engineMode :: Lude.Maybe Lude.Text,
    enabledCloudwatchLogsExports :: Lude.Maybe [Lude.Text],
    activityStreamStatus :: Lude.Maybe ActivityStreamStatus,
    allocatedStorage :: Lude.Maybe Lude.Int,
    copyTagsToSnapshot :: Lude.Maybe Lude.Bool,
    clusterCreateTime :: Lude.Maybe Lude.DateTime,
    endpoint :: Lude.Maybe Lude.Text,
    scalingConfigurationInfo :: Lude.Maybe ScalingConfigurationInfo,
    activityStreamKMSKeyId :: Lude.Maybe Lude.Text,
    percentProgress :: Lude.Maybe Lude.Text,
    readerEndpoint :: Lude.Maybe Lude.Text,
    globalWriteForwardingStatus :: Lude.Maybe WriteForwardingStatus,
    port :: Lude.Maybe Lude.Int,
    domainMemberships :: Lude.Maybe [DomainMembership],
    dbClusterOptionGroupMemberships ::
      Lude.Maybe [DBClusterOptionGroupStatus]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DBCluster' with the minimum fields required to make a request.
--
-- * 'activityStreamKMSKeyId' - The AWS KMS key identifier used for encrypting messages in the database activity stream.
-- * 'activityStreamKinesisStreamName' - The name of the Amazon Kinesis data stream used for the database activity stream.
-- * 'activityStreamMode' - The mode of the database activity stream. Database events such as a change or access generate an activity stream event. The database session can handle these events either synchronously or asynchronously.
-- * 'activityStreamStatus' - The status of the database activity stream.
-- * 'allocatedStorage' - For all database engines except Amazon Aurora, @AllocatedStorage@ specifies the allocated storage size in gibibytes (GiB). For Aurora, @AllocatedStorage@ always returns 1, because Aurora DB cluster storage size isn't fixed, but instead automatically adjusts as needed.
-- * 'associatedRoles' - Provides a list of the AWS Identity and Access Management (IAM) roles that are associated with the DB cluster. IAM roles that are associated with a DB cluster grant permission for the DB cluster to access other AWS services on your behalf.
-- * 'availabilityZones' - Provides the list of Availability Zones (AZs) where instances in the DB cluster can be created.
-- * 'backtrackConsumedChangeRecords' - The number of change records stored for Backtrack.
-- * 'backtrackWindow' - The target backtrack window, in seconds. If this value is set to 0, backtracking is disabled for the DB cluster. Otherwise, backtracking is enabled.
-- * 'backupRetentionPeriod' - Specifies the number of days for which automatic DB snapshots are retained.
-- * 'capacity' - The current capacity of an Aurora Serverless DB cluster. The capacity is 0 (zero) when the cluster is paused.
--
-- For more information about Aurora Serverless, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html Using Amazon Aurora Serverless> in the /Amazon Aurora User Guide/ .
-- * 'characterSetName' - If present, specifies the name of the character set that this cluster is associated with.
-- * 'cloneGroupId' - Identifies the clone group to which the DB cluster is associated.
-- * 'clusterCreateTime' - Specifies the time when the DB cluster was created, in Universal Coordinated Time (UTC).
-- * 'copyTagsToSnapshot' - Specifies whether tags are copied from the DB cluster to snapshots of the DB cluster.
-- * 'crossAccountClone' - Specifies whether the DB cluster is a clone of a DB cluster owned by a different AWS account.
-- * 'customEndpoints' - Identifies all custom endpoints associated with the cluster.
-- * 'databaseName' - Contains the name of the initial database of this DB cluster that was provided at create time, if one was specified when the DB cluster was created. This same name is returned for the life of the DB cluster.
-- * 'dbClusterARN' - The Amazon Resource Name (ARN) for the DB cluster.
-- * 'dbClusterIdentifier' - Contains a user-supplied DB cluster identifier. This identifier is the unique key that identifies a DB cluster.
-- * 'dbClusterMembers' - Provides the list of instances that make up the DB cluster.
-- * 'dbClusterOptionGroupMemberships' - Provides the list of option group memberships for this DB cluster.
-- * 'dbClusterParameterGroup' - Specifies the name of the DB cluster parameter group for the DB cluster.
-- * 'dbClusterResourceId' - The AWS Region-unique, immutable identifier for the DB cluster. This identifier is found in AWS CloudTrail log entries whenever the AWS KMS key for the DB cluster is accessed.
-- * 'dbSubnetGroup' - Specifies information on the subnet group associated with the DB cluster, including the name, description, and subnets in the subnet group.
-- * 'deletionProtection' - Indicates if the DB cluster has deletion protection enabled. The database can't be deleted when deletion protection is enabled.
-- * 'domainMemberships' - The Active Directory Domain membership records associated with the DB cluster.
-- * 'earliestBacktrackTime' - The earliest time to which a DB cluster can be backtracked.
-- * 'earliestRestorableTime' - The earliest time to which a database can be restored with point-in-time restore.
-- * 'enabledCloudwatchLogsExports' - A list of log types that this DB cluster is configured to export to CloudWatch Logs.
--
-- Log types vary by DB engine. For information about the log types for each DB engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html Amazon RDS Database Log Files> in the /Amazon Aurora User Guide./
-- * 'endpoint' - Specifies the connection endpoint for the primary instance of the DB cluster.
-- * 'engine' - The name of the database engine to be used for this DB cluster.
-- * 'engineMode' - The DB engine mode of the DB cluster, either @provisioned@ , @serverless@ , @parallelquery@ , @global@ , or @multimaster@ .
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBCluster.html CreateDBCluster> .
-- * 'engineVersion' - Indicates the database engine version.
-- * 'globalWriteForwardingRequested' - Specifies whether you have requested to enable write forwarding for a secondary cluster in an Aurora global database. Because write forwarding takes time to enable, check the value of @GlobalWriteForwardingStatus@ to confirm that the request has completed before using the write forwarding feature for this cluster.
-- * 'globalWriteForwardingStatus' - Specifies whether a secondary cluster in an Aurora global database has write forwarding enabled, not enabled, or is in the process of enabling it.
-- * 'hTTPEndpointEnabled' - A value that indicates whether the HTTP endpoint for an Aurora Serverless DB cluster is enabled.
--
-- When enabled, the HTTP endpoint provides a connectionless web service API for running SQL queries on the Aurora Serverless DB cluster. You can also query your database from inside the RDS console with the query editor.
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/data-api.html Using the Data API for Aurora Serverless> in the /Amazon Aurora User Guide/ .
-- * 'hostedZoneId' - Specifies the ID that Amazon Route 53 assigns when you create a hosted zone.
-- * 'iamDatabaseAuthenticationEnabled' - A value that indicates whether the mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled.
-- * 'kmsKeyId' - If @StorageEncrypted@ is enabled, the AWS KMS key identifier for the encrypted DB cluster.
-- * 'latestRestorableTime' - Specifies the latest time to which a database can be restored with point-in-time restore.
-- * 'masterUsername' - Contains the master username for the DB cluster.
-- * 'multiAZ' - Specifies whether the DB cluster has instances in multiple Availability Zones.
-- * 'percentProgress' - Specifies the progress of the operation as a percentage.
-- * 'port' - Specifies the port that the database engine is listening on.
-- * 'preferredBackupWindow' - Specifies the daily time range during which automated backups are created if automated backups are enabled, as determined by the @BackupRetentionPeriod@ .
-- * 'preferredMaintenanceWindow' - Specifies the weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC).
-- * 'readReplicaIdentifiers' - Contains one or more identifiers of the read replicas associated with this DB cluster.
-- * 'readerEndpoint' - The reader endpoint for the DB cluster. The reader endpoint for a DB cluster load-balances connections across the Aurora Replicas that are available in a DB cluster. As clients request new connections to the reader endpoint, Aurora distributes the connection requests among the Aurora Replicas in the DB cluster. This functionality can help balance your read workload across multiple Aurora Replicas in your DB cluster.
--
-- If a failover occurs, and the Aurora Replica that you are connected to is promoted to be the primary instance, your connection is dropped. To continue sending your read workload to other Aurora Replicas in the cluster, you can then reconnect to the reader endpoint.
-- * 'replicationSourceIdentifier' - Contains the identifier of the source DB cluster if this DB cluster is a read replica.
-- * 'scalingConfigurationInfo' - Undocumented field.
-- * 'status' - Specifies the current state of this DB cluster.
-- * 'storageEncrypted' - Specifies whether the DB cluster is encrypted.
-- * 'tagList' - Undocumented field.
-- * 'vpcSecurityGroups' - Provides a list of VPC security groups that the DB cluster belongs to.
mkDBCluster ::
  DBCluster
mkDBCluster =
  DBCluster'
    { backtrackConsumedChangeRecords = Lude.Nothing,
      engineVersion = Lude.Nothing,
      status = Lude.Nothing,
      deletionProtection = Lude.Nothing,
      storageEncrypted = Lude.Nothing,
      dbClusterIdentifier = Lude.Nothing,
      dbClusterMembers = Lude.Nothing,
      readReplicaIdentifiers = Lude.Nothing,
      replicationSourceIdentifier = Lude.Nothing,
      activityStreamKinesisStreamName = Lude.Nothing,
      hostedZoneId = Lude.Nothing,
      dbClusterParameterGroup = Lude.Nothing,
      masterUsername = Lude.Nothing,
      iamDatabaseAuthenticationEnabled = Lude.Nothing,
      globalWriteForwardingRequested = Lude.Nothing,
      earliestBacktrackTime = Lude.Nothing,
      backtrackWindow = Lude.Nothing,
      tagList = Lude.Nothing,
      dbClusterResourceId = Lude.Nothing,
      earliestRestorableTime = Lude.Nothing,
      customEndpoints = Lude.Nothing,
      engine = Lude.Nothing,
      hTTPEndpointEnabled = Lude.Nothing,
      dbClusterARN = Lude.Nothing,
      cloneGroupId = Lude.Nothing,
      latestRestorableTime = Lude.Nothing,
      crossAccountClone = Lude.Nothing,
      capacity = Lude.Nothing,
      preferredMaintenanceWindow = Lude.Nothing,
      availabilityZones = Lude.Nothing,
      characterSetName = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      preferredBackupWindow = Lude.Nothing,
      associatedRoles = Lude.Nothing,
      vpcSecurityGroups = Lude.Nothing,
      backupRetentionPeriod = Lude.Nothing,
      dbSubnetGroup = Lude.Nothing,
      activityStreamMode = Lude.Nothing,
      databaseName = Lude.Nothing,
      multiAZ = Lude.Nothing,
      engineMode = Lude.Nothing,
      enabledCloudwatchLogsExports = Lude.Nothing,
      activityStreamStatus = Lude.Nothing,
      allocatedStorage = Lude.Nothing,
      copyTagsToSnapshot = Lude.Nothing,
      clusterCreateTime = Lude.Nothing,
      endpoint = Lude.Nothing,
      scalingConfigurationInfo = Lude.Nothing,
      activityStreamKMSKeyId = Lude.Nothing,
      percentProgress = Lude.Nothing,
      readerEndpoint = Lude.Nothing,
      globalWriteForwardingStatus = Lude.Nothing,
      port = Lude.Nothing,
      domainMemberships = Lude.Nothing,
      dbClusterOptionGroupMemberships = Lude.Nothing
    }

-- | The number of change records stored for Backtrack.
--
-- /Note:/ Consider using 'backtrackConsumedChangeRecords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcBacktrackConsumedChangeRecords :: Lens.Lens' DBCluster (Lude.Maybe Lude.Integer)
dcBacktrackConsumedChangeRecords = Lens.lens (backtrackConsumedChangeRecords :: DBCluster -> Lude.Maybe Lude.Integer) (\s a -> s {backtrackConsumedChangeRecords = a} :: DBCluster)
{-# DEPRECATED dcBacktrackConsumedChangeRecords "Use generic-lens or generic-optics with 'backtrackConsumedChangeRecords' instead." #-}

-- | Indicates the database engine version.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcEngineVersion :: Lens.Lens' DBCluster (Lude.Maybe Lude.Text)
dcEngineVersion = Lens.lens (engineVersion :: DBCluster -> Lude.Maybe Lude.Text) (\s a -> s {engineVersion = a} :: DBCluster)
{-# DEPRECATED dcEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | Specifies the current state of this DB cluster.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcStatus :: Lens.Lens' DBCluster (Lude.Maybe Lude.Text)
dcStatus = Lens.lens (status :: DBCluster -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: DBCluster)
{-# DEPRECATED dcStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Indicates if the DB cluster has deletion protection enabled. The database can't be deleted when deletion protection is enabled.
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcDeletionProtection :: Lens.Lens' DBCluster (Lude.Maybe Lude.Bool)
dcDeletionProtection = Lens.lens (deletionProtection :: DBCluster -> Lude.Maybe Lude.Bool) (\s a -> s {deletionProtection = a} :: DBCluster)
{-# DEPRECATED dcDeletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead." #-}

-- | Specifies whether the DB cluster is encrypted.
--
-- /Note:/ Consider using 'storageEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcStorageEncrypted :: Lens.Lens' DBCluster (Lude.Maybe Lude.Bool)
dcStorageEncrypted = Lens.lens (storageEncrypted :: DBCluster -> Lude.Maybe Lude.Bool) (\s a -> s {storageEncrypted = a} :: DBCluster)
{-# DEPRECATED dcStorageEncrypted "Use generic-lens or generic-optics with 'storageEncrypted' instead." #-}

-- | Contains a user-supplied DB cluster identifier. This identifier is the unique key that identifies a DB cluster.
--
-- /Note:/ Consider using 'dbClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcDBClusterIdentifier :: Lens.Lens' DBCluster (Lude.Maybe Lude.Text)
dcDBClusterIdentifier = Lens.lens (dbClusterIdentifier :: DBCluster -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterIdentifier = a} :: DBCluster)
{-# DEPRECATED dcDBClusterIdentifier "Use generic-lens or generic-optics with 'dbClusterIdentifier' instead." #-}

-- | Provides the list of instances that make up the DB cluster.
--
-- /Note:/ Consider using 'dbClusterMembers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcDBClusterMembers :: Lens.Lens' DBCluster (Lude.Maybe [DBClusterMember])
dcDBClusterMembers = Lens.lens (dbClusterMembers :: DBCluster -> Lude.Maybe [DBClusterMember]) (\s a -> s {dbClusterMembers = a} :: DBCluster)
{-# DEPRECATED dcDBClusterMembers "Use generic-lens or generic-optics with 'dbClusterMembers' instead." #-}

-- | Contains one or more identifiers of the read replicas associated with this DB cluster.
--
-- /Note:/ Consider using 'readReplicaIdentifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcReadReplicaIdentifiers :: Lens.Lens' DBCluster (Lude.Maybe [Lude.Text])
dcReadReplicaIdentifiers = Lens.lens (readReplicaIdentifiers :: DBCluster -> Lude.Maybe [Lude.Text]) (\s a -> s {readReplicaIdentifiers = a} :: DBCluster)
{-# DEPRECATED dcReadReplicaIdentifiers "Use generic-lens or generic-optics with 'readReplicaIdentifiers' instead." #-}

-- | Contains the identifier of the source DB cluster if this DB cluster is a read replica.
--
-- /Note:/ Consider using 'replicationSourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcReplicationSourceIdentifier :: Lens.Lens' DBCluster (Lude.Maybe Lude.Text)
dcReplicationSourceIdentifier = Lens.lens (replicationSourceIdentifier :: DBCluster -> Lude.Maybe Lude.Text) (\s a -> s {replicationSourceIdentifier = a} :: DBCluster)
{-# DEPRECATED dcReplicationSourceIdentifier "Use generic-lens or generic-optics with 'replicationSourceIdentifier' instead." #-}

-- | The name of the Amazon Kinesis data stream used for the database activity stream.
--
-- /Note:/ Consider using 'activityStreamKinesisStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcActivityStreamKinesisStreamName :: Lens.Lens' DBCluster (Lude.Maybe Lude.Text)
dcActivityStreamKinesisStreamName = Lens.lens (activityStreamKinesisStreamName :: DBCluster -> Lude.Maybe Lude.Text) (\s a -> s {activityStreamKinesisStreamName = a} :: DBCluster)
{-# DEPRECATED dcActivityStreamKinesisStreamName "Use generic-lens or generic-optics with 'activityStreamKinesisStreamName' instead." #-}

-- | Specifies the ID that Amazon Route 53 assigns when you create a hosted zone.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcHostedZoneId :: Lens.Lens' DBCluster (Lude.Maybe Lude.Text)
dcHostedZoneId = Lens.lens (hostedZoneId :: DBCluster -> Lude.Maybe Lude.Text) (\s a -> s {hostedZoneId = a} :: DBCluster)
{-# DEPRECATED dcHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | Specifies the name of the DB cluster parameter group for the DB cluster.
--
-- /Note:/ Consider using 'dbClusterParameterGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcDBClusterParameterGroup :: Lens.Lens' DBCluster (Lude.Maybe Lude.Text)
dcDBClusterParameterGroup = Lens.lens (dbClusterParameterGroup :: DBCluster -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterParameterGroup = a} :: DBCluster)
{-# DEPRECATED dcDBClusterParameterGroup "Use generic-lens or generic-optics with 'dbClusterParameterGroup' instead." #-}

-- | Contains the master username for the DB cluster.
--
-- /Note:/ Consider using 'masterUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcMasterUsername :: Lens.Lens' DBCluster (Lude.Maybe Lude.Text)
dcMasterUsername = Lens.lens (masterUsername :: DBCluster -> Lude.Maybe Lude.Text) (\s a -> s {masterUsername = a} :: DBCluster)
{-# DEPRECATED dcMasterUsername "Use generic-lens or generic-optics with 'masterUsername' instead." #-}

-- | A value that indicates whether the mapping of AWS Identity and Access Management (IAM) accounts to database accounts is enabled.
--
-- /Note:/ Consider using 'iamDatabaseAuthenticationEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcIAMDatabaseAuthenticationEnabled :: Lens.Lens' DBCluster (Lude.Maybe Lude.Bool)
dcIAMDatabaseAuthenticationEnabled = Lens.lens (iamDatabaseAuthenticationEnabled :: DBCluster -> Lude.Maybe Lude.Bool) (\s a -> s {iamDatabaseAuthenticationEnabled = a} :: DBCluster)
{-# DEPRECATED dcIAMDatabaseAuthenticationEnabled "Use generic-lens or generic-optics with 'iamDatabaseAuthenticationEnabled' instead." #-}

-- | Specifies whether you have requested to enable write forwarding for a secondary cluster in an Aurora global database. Because write forwarding takes time to enable, check the value of @GlobalWriteForwardingStatus@ to confirm that the request has completed before using the write forwarding feature for this cluster.
--
-- /Note:/ Consider using 'globalWriteForwardingRequested' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcGlobalWriteForwardingRequested :: Lens.Lens' DBCluster (Lude.Maybe Lude.Bool)
dcGlobalWriteForwardingRequested = Lens.lens (globalWriteForwardingRequested :: DBCluster -> Lude.Maybe Lude.Bool) (\s a -> s {globalWriteForwardingRequested = a} :: DBCluster)
{-# DEPRECATED dcGlobalWriteForwardingRequested "Use generic-lens or generic-optics with 'globalWriteForwardingRequested' instead." #-}

-- | The earliest time to which a DB cluster can be backtracked.
--
-- /Note:/ Consider using 'earliestBacktrackTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcEarliestBacktrackTime :: Lens.Lens' DBCluster (Lude.Maybe Lude.DateTime)
dcEarliestBacktrackTime = Lens.lens (earliestBacktrackTime :: DBCluster -> Lude.Maybe Lude.DateTime) (\s a -> s {earliestBacktrackTime = a} :: DBCluster)
{-# DEPRECATED dcEarliestBacktrackTime "Use generic-lens or generic-optics with 'earliestBacktrackTime' instead." #-}

-- | The target backtrack window, in seconds. If this value is set to 0, backtracking is disabled for the DB cluster. Otherwise, backtracking is enabled.
--
-- /Note:/ Consider using 'backtrackWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcBacktrackWindow :: Lens.Lens' DBCluster (Lude.Maybe Lude.Integer)
dcBacktrackWindow = Lens.lens (backtrackWindow :: DBCluster -> Lude.Maybe Lude.Integer) (\s a -> s {backtrackWindow = a} :: DBCluster)
{-# DEPRECATED dcBacktrackWindow "Use generic-lens or generic-optics with 'backtrackWindow' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcTagList :: Lens.Lens' DBCluster (Lude.Maybe [Tag])
dcTagList = Lens.lens (tagList :: DBCluster -> Lude.Maybe [Tag]) (\s a -> s {tagList = a} :: DBCluster)
{-# DEPRECATED dcTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

-- | The AWS Region-unique, immutable identifier for the DB cluster. This identifier is found in AWS CloudTrail log entries whenever the AWS KMS key for the DB cluster is accessed.
--
-- /Note:/ Consider using 'dbClusterResourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcDBClusterResourceId :: Lens.Lens' DBCluster (Lude.Maybe Lude.Text)
dcDBClusterResourceId = Lens.lens (dbClusterResourceId :: DBCluster -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterResourceId = a} :: DBCluster)
{-# DEPRECATED dcDBClusterResourceId "Use generic-lens or generic-optics with 'dbClusterResourceId' instead." #-}

-- | The earliest time to which a database can be restored with point-in-time restore.
--
-- /Note:/ Consider using 'earliestRestorableTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcEarliestRestorableTime :: Lens.Lens' DBCluster (Lude.Maybe Lude.DateTime)
dcEarliestRestorableTime = Lens.lens (earliestRestorableTime :: DBCluster -> Lude.Maybe Lude.DateTime) (\s a -> s {earliestRestorableTime = a} :: DBCluster)
{-# DEPRECATED dcEarliestRestorableTime "Use generic-lens or generic-optics with 'earliestRestorableTime' instead." #-}

-- | Identifies all custom endpoints associated with the cluster.
--
-- /Note:/ Consider using 'customEndpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcCustomEndpoints :: Lens.Lens' DBCluster (Lude.Maybe [Lude.Text])
dcCustomEndpoints = Lens.lens (customEndpoints :: DBCluster -> Lude.Maybe [Lude.Text]) (\s a -> s {customEndpoints = a} :: DBCluster)
{-# DEPRECATED dcCustomEndpoints "Use generic-lens or generic-optics with 'customEndpoints' instead." #-}

-- | The name of the database engine to be used for this DB cluster.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcEngine :: Lens.Lens' DBCluster (Lude.Maybe Lude.Text)
dcEngine = Lens.lens (engine :: DBCluster -> Lude.Maybe Lude.Text) (\s a -> s {engine = a} :: DBCluster)
{-# DEPRECATED dcEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | A value that indicates whether the HTTP endpoint for an Aurora Serverless DB cluster is enabled.
--
-- When enabled, the HTTP endpoint provides a connectionless web service API for running SQL queries on the Aurora Serverless DB cluster. You can also query your database from inside the RDS console with the query editor.
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/data-api.html Using the Data API for Aurora Serverless> in the /Amazon Aurora User Guide/ .
--
-- /Note:/ Consider using 'hTTPEndpointEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcHTTPEndpointEnabled :: Lens.Lens' DBCluster (Lude.Maybe Lude.Bool)
dcHTTPEndpointEnabled = Lens.lens (hTTPEndpointEnabled :: DBCluster -> Lude.Maybe Lude.Bool) (\s a -> s {hTTPEndpointEnabled = a} :: DBCluster)
{-# DEPRECATED dcHTTPEndpointEnabled "Use generic-lens or generic-optics with 'hTTPEndpointEnabled' instead." #-}

-- | The Amazon Resource Name (ARN) for the DB cluster.
--
-- /Note:/ Consider using 'dbClusterARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcDBClusterARN :: Lens.Lens' DBCluster (Lude.Maybe Lude.Text)
dcDBClusterARN = Lens.lens (dbClusterARN :: DBCluster -> Lude.Maybe Lude.Text) (\s a -> s {dbClusterARN = a} :: DBCluster)
{-# DEPRECATED dcDBClusterARN "Use generic-lens or generic-optics with 'dbClusterARN' instead." #-}

-- | Identifies the clone group to which the DB cluster is associated.
--
-- /Note:/ Consider using 'cloneGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcCloneGroupId :: Lens.Lens' DBCluster (Lude.Maybe Lude.Text)
dcCloneGroupId = Lens.lens (cloneGroupId :: DBCluster -> Lude.Maybe Lude.Text) (\s a -> s {cloneGroupId = a} :: DBCluster)
{-# DEPRECATED dcCloneGroupId "Use generic-lens or generic-optics with 'cloneGroupId' instead." #-}

-- | Specifies the latest time to which a database can be restored with point-in-time restore.
--
-- /Note:/ Consider using 'latestRestorableTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcLatestRestorableTime :: Lens.Lens' DBCluster (Lude.Maybe Lude.DateTime)
dcLatestRestorableTime = Lens.lens (latestRestorableTime :: DBCluster -> Lude.Maybe Lude.DateTime) (\s a -> s {latestRestorableTime = a} :: DBCluster)
{-# DEPRECATED dcLatestRestorableTime "Use generic-lens or generic-optics with 'latestRestorableTime' instead." #-}

-- | Specifies whether the DB cluster is a clone of a DB cluster owned by a different AWS account.
--
-- /Note:/ Consider using 'crossAccountClone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcCrossAccountClone :: Lens.Lens' DBCluster (Lude.Maybe Lude.Bool)
dcCrossAccountClone = Lens.lens (crossAccountClone :: DBCluster -> Lude.Maybe Lude.Bool) (\s a -> s {crossAccountClone = a} :: DBCluster)
{-# DEPRECATED dcCrossAccountClone "Use generic-lens or generic-optics with 'crossAccountClone' instead." #-}

-- | The current capacity of an Aurora Serverless DB cluster. The capacity is 0 (zero) when the cluster is paused.
--
-- For more information about Aurora Serverless, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html Using Amazon Aurora Serverless> in the /Amazon Aurora User Guide/ .
--
-- /Note:/ Consider using 'capacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcCapacity :: Lens.Lens' DBCluster (Lude.Maybe Lude.Int)
dcCapacity = Lens.lens (capacity :: DBCluster -> Lude.Maybe Lude.Int) (\s a -> s {capacity = a} :: DBCluster)
{-# DEPRECATED dcCapacity "Use generic-lens or generic-optics with 'capacity' instead." #-}

-- | Specifies the weekly time range during which system maintenance can occur, in Universal Coordinated Time (UTC).
--
-- /Note:/ Consider using 'preferredMaintenanceWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcPreferredMaintenanceWindow :: Lens.Lens' DBCluster (Lude.Maybe Lude.Text)
dcPreferredMaintenanceWindow = Lens.lens (preferredMaintenanceWindow :: DBCluster -> Lude.Maybe Lude.Text) (\s a -> s {preferredMaintenanceWindow = a} :: DBCluster)
{-# DEPRECATED dcPreferredMaintenanceWindow "Use generic-lens or generic-optics with 'preferredMaintenanceWindow' instead." #-}

-- | Provides the list of Availability Zones (AZs) where instances in the DB cluster can be created.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcAvailabilityZones :: Lens.Lens' DBCluster (Lude.Maybe [Lude.Text])
dcAvailabilityZones = Lens.lens (availabilityZones :: DBCluster -> Lude.Maybe [Lude.Text]) (\s a -> s {availabilityZones = a} :: DBCluster)
{-# DEPRECATED dcAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | If present, specifies the name of the character set that this cluster is associated with.
--
-- /Note:/ Consider using 'characterSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcCharacterSetName :: Lens.Lens' DBCluster (Lude.Maybe Lude.Text)
dcCharacterSetName = Lens.lens (characterSetName :: DBCluster -> Lude.Maybe Lude.Text) (\s a -> s {characterSetName = a} :: DBCluster)
{-# DEPRECATED dcCharacterSetName "Use generic-lens or generic-optics with 'characterSetName' instead." #-}

-- | If @StorageEncrypted@ is enabled, the AWS KMS key identifier for the encrypted DB cluster.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcKMSKeyId :: Lens.Lens' DBCluster (Lude.Maybe Lude.Text)
dcKMSKeyId = Lens.lens (kmsKeyId :: DBCluster -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: DBCluster)
{-# DEPRECATED dcKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | Specifies the daily time range during which automated backups are created if automated backups are enabled, as determined by the @BackupRetentionPeriod@ .
--
-- /Note:/ Consider using 'preferredBackupWindow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcPreferredBackupWindow :: Lens.Lens' DBCluster (Lude.Maybe Lude.Text)
dcPreferredBackupWindow = Lens.lens (preferredBackupWindow :: DBCluster -> Lude.Maybe Lude.Text) (\s a -> s {preferredBackupWindow = a} :: DBCluster)
{-# DEPRECATED dcPreferredBackupWindow "Use generic-lens or generic-optics with 'preferredBackupWindow' instead." #-}

-- | Provides a list of the AWS Identity and Access Management (IAM) roles that are associated with the DB cluster. IAM roles that are associated with a DB cluster grant permission for the DB cluster to access other AWS services on your behalf.
--
-- /Note:/ Consider using 'associatedRoles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcAssociatedRoles :: Lens.Lens' DBCluster (Lude.Maybe [DBClusterRole])
dcAssociatedRoles = Lens.lens (associatedRoles :: DBCluster -> Lude.Maybe [DBClusterRole]) (\s a -> s {associatedRoles = a} :: DBCluster)
{-# DEPRECATED dcAssociatedRoles "Use generic-lens or generic-optics with 'associatedRoles' instead." #-}

-- | Provides a list of VPC security groups that the DB cluster belongs to.
--
-- /Note:/ Consider using 'vpcSecurityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcVPCSecurityGroups :: Lens.Lens' DBCluster (Lude.Maybe [VPCSecurityGroupMembership])
dcVPCSecurityGroups = Lens.lens (vpcSecurityGroups :: DBCluster -> Lude.Maybe [VPCSecurityGroupMembership]) (\s a -> s {vpcSecurityGroups = a} :: DBCluster)
{-# DEPRECATED dcVPCSecurityGroups "Use generic-lens or generic-optics with 'vpcSecurityGroups' instead." #-}

-- | Specifies the number of days for which automatic DB snapshots are retained.
--
-- /Note:/ Consider using 'backupRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcBackupRetentionPeriod :: Lens.Lens' DBCluster (Lude.Maybe Lude.Int)
dcBackupRetentionPeriod = Lens.lens (backupRetentionPeriod :: DBCluster -> Lude.Maybe Lude.Int) (\s a -> s {backupRetentionPeriod = a} :: DBCluster)
{-# DEPRECATED dcBackupRetentionPeriod "Use generic-lens or generic-optics with 'backupRetentionPeriod' instead." #-}

-- | Specifies information on the subnet group associated with the DB cluster, including the name, description, and subnets in the subnet group.
--
-- /Note:/ Consider using 'dbSubnetGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcDBSubnetGroup :: Lens.Lens' DBCluster (Lude.Maybe Lude.Text)
dcDBSubnetGroup = Lens.lens (dbSubnetGroup :: DBCluster -> Lude.Maybe Lude.Text) (\s a -> s {dbSubnetGroup = a} :: DBCluster)
{-# DEPRECATED dcDBSubnetGroup "Use generic-lens or generic-optics with 'dbSubnetGroup' instead." #-}

-- | The mode of the database activity stream. Database events such as a change or access generate an activity stream event. The database session can handle these events either synchronously or asynchronously.
--
-- /Note:/ Consider using 'activityStreamMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcActivityStreamMode :: Lens.Lens' DBCluster (Lude.Maybe ActivityStreamMode)
dcActivityStreamMode = Lens.lens (activityStreamMode :: DBCluster -> Lude.Maybe ActivityStreamMode) (\s a -> s {activityStreamMode = a} :: DBCluster)
{-# DEPRECATED dcActivityStreamMode "Use generic-lens or generic-optics with 'activityStreamMode' instead." #-}

-- | Contains the name of the initial database of this DB cluster that was provided at create time, if one was specified when the DB cluster was created. This same name is returned for the life of the DB cluster.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcDatabaseName :: Lens.Lens' DBCluster (Lude.Maybe Lude.Text)
dcDatabaseName = Lens.lens (databaseName :: DBCluster -> Lude.Maybe Lude.Text) (\s a -> s {databaseName = a} :: DBCluster)
{-# DEPRECATED dcDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | Specifies whether the DB cluster has instances in multiple Availability Zones.
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcMultiAZ :: Lens.Lens' DBCluster (Lude.Maybe Lude.Bool)
dcMultiAZ = Lens.lens (multiAZ :: DBCluster -> Lude.Maybe Lude.Bool) (\s a -> s {multiAZ = a} :: DBCluster)
{-# DEPRECATED dcMultiAZ "Use generic-lens or generic-optics with 'multiAZ' instead." #-}

-- | The DB engine mode of the DB cluster, either @provisioned@ , @serverless@ , @parallelquery@ , @global@ , or @multimaster@ .
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBCluster.html CreateDBCluster> .
--
-- /Note:/ Consider using 'engineMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcEngineMode :: Lens.Lens' DBCluster (Lude.Maybe Lude.Text)
dcEngineMode = Lens.lens (engineMode :: DBCluster -> Lude.Maybe Lude.Text) (\s a -> s {engineMode = a} :: DBCluster)
{-# DEPRECATED dcEngineMode "Use generic-lens or generic-optics with 'engineMode' instead." #-}

-- | A list of log types that this DB cluster is configured to export to CloudWatch Logs.
--
-- Log types vary by DB engine. For information about the log types for each DB engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html Amazon RDS Database Log Files> in the /Amazon Aurora User Guide./
--
-- /Note:/ Consider using 'enabledCloudwatchLogsExports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcEnabledCloudwatchLogsExports :: Lens.Lens' DBCluster (Lude.Maybe [Lude.Text])
dcEnabledCloudwatchLogsExports = Lens.lens (enabledCloudwatchLogsExports :: DBCluster -> Lude.Maybe [Lude.Text]) (\s a -> s {enabledCloudwatchLogsExports = a} :: DBCluster)
{-# DEPRECATED dcEnabledCloudwatchLogsExports "Use generic-lens or generic-optics with 'enabledCloudwatchLogsExports' instead." #-}

-- | The status of the database activity stream.
--
-- /Note:/ Consider using 'activityStreamStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcActivityStreamStatus :: Lens.Lens' DBCluster (Lude.Maybe ActivityStreamStatus)
dcActivityStreamStatus = Lens.lens (activityStreamStatus :: DBCluster -> Lude.Maybe ActivityStreamStatus) (\s a -> s {activityStreamStatus = a} :: DBCluster)
{-# DEPRECATED dcActivityStreamStatus "Use generic-lens or generic-optics with 'activityStreamStatus' instead." #-}

-- | For all database engines except Amazon Aurora, @AllocatedStorage@ specifies the allocated storage size in gibibytes (GiB). For Aurora, @AllocatedStorage@ always returns 1, because Aurora DB cluster storage size isn't fixed, but instead automatically adjusts as needed.
--
-- /Note:/ Consider using 'allocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcAllocatedStorage :: Lens.Lens' DBCluster (Lude.Maybe Lude.Int)
dcAllocatedStorage = Lens.lens (allocatedStorage :: DBCluster -> Lude.Maybe Lude.Int) (\s a -> s {allocatedStorage = a} :: DBCluster)
{-# DEPRECATED dcAllocatedStorage "Use generic-lens or generic-optics with 'allocatedStorage' instead." #-}

-- | Specifies whether tags are copied from the DB cluster to snapshots of the DB cluster.
--
-- /Note:/ Consider using 'copyTagsToSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcCopyTagsToSnapshot :: Lens.Lens' DBCluster (Lude.Maybe Lude.Bool)
dcCopyTagsToSnapshot = Lens.lens (copyTagsToSnapshot :: DBCluster -> Lude.Maybe Lude.Bool) (\s a -> s {copyTagsToSnapshot = a} :: DBCluster)
{-# DEPRECATED dcCopyTagsToSnapshot "Use generic-lens or generic-optics with 'copyTagsToSnapshot' instead." #-}

-- | Specifies the time when the DB cluster was created, in Universal Coordinated Time (UTC).
--
-- /Note:/ Consider using 'clusterCreateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcClusterCreateTime :: Lens.Lens' DBCluster (Lude.Maybe Lude.DateTime)
dcClusterCreateTime = Lens.lens (clusterCreateTime :: DBCluster -> Lude.Maybe Lude.DateTime) (\s a -> s {clusterCreateTime = a} :: DBCluster)
{-# DEPRECATED dcClusterCreateTime "Use generic-lens or generic-optics with 'clusterCreateTime' instead." #-}

-- | Specifies the connection endpoint for the primary instance of the DB cluster.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcEndpoint :: Lens.Lens' DBCluster (Lude.Maybe Lude.Text)
dcEndpoint = Lens.lens (endpoint :: DBCluster -> Lude.Maybe Lude.Text) (\s a -> s {endpoint = a} :: DBCluster)
{-# DEPRECATED dcEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'scalingConfigurationInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcScalingConfigurationInfo :: Lens.Lens' DBCluster (Lude.Maybe ScalingConfigurationInfo)
dcScalingConfigurationInfo = Lens.lens (scalingConfigurationInfo :: DBCluster -> Lude.Maybe ScalingConfigurationInfo) (\s a -> s {scalingConfigurationInfo = a} :: DBCluster)
{-# DEPRECATED dcScalingConfigurationInfo "Use generic-lens or generic-optics with 'scalingConfigurationInfo' instead." #-}

-- | The AWS KMS key identifier used for encrypting messages in the database activity stream.
--
-- /Note:/ Consider using 'activityStreamKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcActivityStreamKMSKeyId :: Lens.Lens' DBCluster (Lude.Maybe Lude.Text)
dcActivityStreamKMSKeyId = Lens.lens (activityStreamKMSKeyId :: DBCluster -> Lude.Maybe Lude.Text) (\s a -> s {activityStreamKMSKeyId = a} :: DBCluster)
{-# DEPRECATED dcActivityStreamKMSKeyId "Use generic-lens or generic-optics with 'activityStreamKMSKeyId' instead." #-}

-- | Specifies the progress of the operation as a percentage.
--
-- /Note:/ Consider using 'percentProgress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcPercentProgress :: Lens.Lens' DBCluster (Lude.Maybe Lude.Text)
dcPercentProgress = Lens.lens (percentProgress :: DBCluster -> Lude.Maybe Lude.Text) (\s a -> s {percentProgress = a} :: DBCluster)
{-# DEPRECATED dcPercentProgress "Use generic-lens or generic-optics with 'percentProgress' instead." #-}

-- | The reader endpoint for the DB cluster. The reader endpoint for a DB cluster load-balances connections across the Aurora Replicas that are available in a DB cluster. As clients request new connections to the reader endpoint, Aurora distributes the connection requests among the Aurora Replicas in the DB cluster. This functionality can help balance your read workload across multiple Aurora Replicas in your DB cluster.
--
-- If a failover occurs, and the Aurora Replica that you are connected to is promoted to be the primary instance, your connection is dropped. To continue sending your read workload to other Aurora Replicas in the cluster, you can then reconnect to the reader endpoint.
--
-- /Note:/ Consider using 'readerEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcReaderEndpoint :: Lens.Lens' DBCluster (Lude.Maybe Lude.Text)
dcReaderEndpoint = Lens.lens (readerEndpoint :: DBCluster -> Lude.Maybe Lude.Text) (\s a -> s {readerEndpoint = a} :: DBCluster)
{-# DEPRECATED dcReaderEndpoint "Use generic-lens or generic-optics with 'readerEndpoint' instead." #-}

-- | Specifies whether a secondary cluster in an Aurora global database has write forwarding enabled, not enabled, or is in the process of enabling it.
--
-- /Note:/ Consider using 'globalWriteForwardingStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcGlobalWriteForwardingStatus :: Lens.Lens' DBCluster (Lude.Maybe WriteForwardingStatus)
dcGlobalWriteForwardingStatus = Lens.lens (globalWriteForwardingStatus :: DBCluster -> Lude.Maybe WriteForwardingStatus) (\s a -> s {globalWriteForwardingStatus = a} :: DBCluster)
{-# DEPRECATED dcGlobalWriteForwardingStatus "Use generic-lens or generic-optics with 'globalWriteForwardingStatus' instead." #-}

-- | Specifies the port that the database engine is listening on.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcPort :: Lens.Lens' DBCluster (Lude.Maybe Lude.Int)
dcPort = Lens.lens (port :: DBCluster -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: DBCluster)
{-# DEPRECATED dcPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | The Active Directory Domain membership records associated with the DB cluster.
--
-- /Note:/ Consider using 'domainMemberships' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcDomainMemberships :: Lens.Lens' DBCluster (Lude.Maybe [DomainMembership])
dcDomainMemberships = Lens.lens (domainMemberships :: DBCluster -> Lude.Maybe [DomainMembership]) (\s a -> s {domainMemberships = a} :: DBCluster)
{-# DEPRECATED dcDomainMemberships "Use generic-lens or generic-optics with 'domainMemberships' instead." #-}

-- | Provides the list of option group memberships for this DB cluster.
--
-- /Note:/ Consider using 'dbClusterOptionGroupMemberships' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcDBClusterOptionGroupMemberships :: Lens.Lens' DBCluster (Lude.Maybe [DBClusterOptionGroupStatus])
dcDBClusterOptionGroupMemberships = Lens.lens (dbClusterOptionGroupMemberships :: DBCluster -> Lude.Maybe [DBClusterOptionGroupStatus]) (\s a -> s {dbClusterOptionGroupMemberships = a} :: DBCluster)
{-# DEPRECATED dcDBClusterOptionGroupMemberships "Use generic-lens or generic-optics with 'dbClusterOptionGroupMemberships' instead." #-}

instance Lude.FromXML DBCluster where
  parseXML x =
    DBCluster'
      Lude.<$> (x Lude..@? "BacktrackConsumedChangeRecords")
      Lude.<*> (x Lude..@? "EngineVersion")
      Lude.<*> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "DeletionProtection")
      Lude.<*> (x Lude..@? "StorageEncrypted")
      Lude.<*> (x Lude..@? "DBClusterIdentifier")
      Lude.<*> ( x Lude..@? "DBClusterMembers" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "DBClusterMember")
               )
      Lude.<*> ( x Lude..@? "ReadReplicaIdentifiers" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "ReadReplicaIdentifier")
               )
      Lude.<*> (x Lude..@? "ReplicationSourceIdentifier")
      Lude.<*> (x Lude..@? "ActivityStreamKinesisStreamName")
      Lude.<*> (x Lude..@? "HostedZoneId")
      Lude.<*> (x Lude..@? "DBClusterParameterGroup")
      Lude.<*> (x Lude..@? "MasterUsername")
      Lude.<*> (x Lude..@? "IAMDatabaseAuthenticationEnabled")
      Lude.<*> (x Lude..@? "GlobalWriteForwardingRequested")
      Lude.<*> (x Lude..@? "EarliestBacktrackTime")
      Lude.<*> (x Lude..@? "BacktrackWindow")
      Lude.<*> ( x Lude..@? "TagList" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Tag")
               )
      Lude.<*> (x Lude..@? "DbClusterResourceId")
      Lude.<*> (x Lude..@? "EarliestRestorableTime")
      Lude.<*> ( x Lude..@? "CustomEndpoints" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "Engine")
      Lude.<*> (x Lude..@? "HttpEndpointEnabled")
      Lude.<*> (x Lude..@? "DBClusterArn")
      Lude.<*> (x Lude..@? "CloneGroupId")
      Lude.<*> (x Lude..@? "LatestRestorableTime")
      Lude.<*> (x Lude..@? "CrossAccountClone")
      Lude.<*> (x Lude..@? "Capacity")
      Lude.<*> (x Lude..@? "PreferredMaintenanceWindow")
      Lude.<*> ( x Lude..@? "AvailabilityZones" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "AvailabilityZone")
               )
      Lude.<*> (x Lude..@? "CharacterSetName")
      Lude.<*> (x Lude..@? "KmsKeyId")
      Lude.<*> (x Lude..@? "PreferredBackupWindow")
      Lude.<*> ( x Lude..@? "AssociatedRoles" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "DBClusterRole")
               )
      Lude.<*> ( x Lude..@? "VpcSecurityGroups" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "VpcSecurityGroupMembership")
               )
      Lude.<*> (x Lude..@? "BackupRetentionPeriod")
      Lude.<*> (x Lude..@? "DBSubnetGroup")
      Lude.<*> (x Lude..@? "ActivityStreamMode")
      Lude.<*> (x Lude..@? "DatabaseName")
      Lude.<*> (x Lude..@? "MultiAZ")
      Lude.<*> (x Lude..@? "EngineMode")
      Lude.<*> ( x Lude..@? "EnabledCloudwatchLogsExports" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "ActivityStreamStatus")
      Lude.<*> (x Lude..@? "AllocatedStorage")
      Lude.<*> (x Lude..@? "CopyTagsToSnapshot")
      Lude.<*> (x Lude..@? "ClusterCreateTime")
      Lude.<*> (x Lude..@? "Endpoint")
      Lude.<*> (x Lude..@? "ScalingConfigurationInfo")
      Lude.<*> (x Lude..@? "ActivityStreamKmsKeyId")
      Lude.<*> (x Lude..@? "PercentProgress")
      Lude.<*> (x Lude..@? "ReaderEndpoint")
      Lude.<*> (x Lude..@? "GlobalWriteForwardingStatus")
      Lude.<*> (x Lude..@? "Port")
      Lude.<*> ( x Lude..@? "DomainMemberships" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "DomainMembership")
               )
      Lude.<*> ( x Lude..@? "DBClusterOptionGroupMemberships" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "DBClusterOptionGroup")
               )
