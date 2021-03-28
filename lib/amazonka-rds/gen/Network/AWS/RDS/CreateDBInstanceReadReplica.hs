{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateDBInstanceReadReplica
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB instance that acts as a read replica for an existing source DB instance. You can create a read replica for a DB instance running MySQL, MariaDB, Oracle, PostgreSQL, or SQL Server. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_ReadRepl.html Working with Read Replicas> in the /Amazon RDS User Guide/ . 
--
-- Amazon Aurora doesn't support this action. Call the @CreateDBInstance@ action to create a DB instance for an Aurora DB cluster.
-- All read replica DB instances are created with backups disabled. All other DB instance attributes (including DB security groups and DB parameter groups) are inherited from the source DB instance, except as specified.
-- /Important:/ Your source DB instance must have backup retention enabled. 
module Network.AWS.RDS.CreateDBInstanceReadReplica
    (
    -- * Creating a request
      CreateDBInstanceReadReplica (..)
    , mkCreateDBInstanceReadReplica
    -- ** Request lenses
    , cdbirrDBInstanceIdentifier
    , cdbirrSourceDBInstanceIdentifier
    , cdbirrAutoMinorVersionUpgrade
    , cdbirrAvailabilityZone
    , cdbirrCopyTagsToSnapshot
    , cdbirrDBInstanceClass
    , cdbirrDBParameterGroupName
    , cdbirrDBSubnetGroupName
    , cdbirrDeletionProtection
    , cdbirrDomain
    , cdbirrDomainIAMRoleName
    , cdbirrEnableCloudwatchLogsExports
    , cdbirrEnableIAMDatabaseAuthentication
    , cdbirrEnablePerformanceInsights
    , cdbirrIops
    , cdbirrKmsKeyId
    , cdbirrMaxAllocatedStorage
    , cdbirrMonitoringInterval
    , cdbirrMonitoringRoleArn
    , cdbirrMultiAZ
    , cdbirrOptionGroupName
    , cdbirrPerformanceInsightsKMSKeyId
    , cdbirrPerformanceInsightsRetentionPeriod
    , cdbirrPort
    , cdbirrPreSignedUrl
    , cdbirrProcessorFeatures
    , cdbirrPubliclyAccessible
    , cdbirrReplicaMode
    , cdbirrStorageType
    , cdbirrTags
    , cdbirrUseDefaultProcessorFeatures
    , cdbirrVpcSecurityGroupIds

    -- * Destructuring the response
    , CreateDBInstanceReadReplicaResponse (..)
    , mkCreateDBInstanceReadReplicaResponse
    -- ** Response lenses
    , cdbirrrrsDBInstance
    , cdbirrrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDBInstanceReadReplica' smart constructor.
data CreateDBInstanceReadReplica = CreateDBInstanceReadReplica'
  { dBInstanceIdentifier :: Core.Text
    -- ^ The DB instance identifier of the read replica. This identifier is the unique key that identifies a DB instance. This parameter is stored as a lowercase string.
  , sourceDBInstanceIdentifier :: Core.Text
    -- ^ The identifier of the DB instance that will act as the source for the read replica. Each DB instance can have up to five read replicas.
--
-- Constraints:
--
--     * Must be the identifier of an existing MySQL, MariaDB, Oracle, PostgreSQL, or SQL Server DB instance.
--
--
--     * Can specify a DB instance that is a MySQL read replica only if the source is running MySQL 5.6 or later.
--
--
--     * For the limitations of Oracle read replicas, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Read Replica Limitations with Oracle> in the /Amazon RDS User Guide/ .
--
--
--     * For the limitations of SQL Server read replicas, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/SQLServer.ReadReplicas.Limitations.html Read Replica Limitations with Microsoft SQL Server> in the /Amazon RDS User Guide/ .
--
--
--     * Can specify a PostgreSQL DB instance only if the source is running PostgreSQL 9.3.5 or later (9.4.7 and higher for cross-region replication).
--
--
--     * The specified DB instance must have automatic backups enabled, that is, its backup retention period must be greater than 0.
--
--
--     * If the source DB instance is in the same AWS Region as the read replica, specify a valid DB instance identifier.
--
--
--     * If the source DB instance is in a different AWS Region from the read replica, specify a valid DB instance ARN. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS> in the /Amazon RDS User Guide/ . This doesn't apply to SQL Server, which doesn't support cross-region replicas.
--
--
  , autoMinorVersionUpgrade :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether minor engine upgrades are applied automatically to the read replica during the maintenance window.
--
-- Default: Inherits from the source DB instance
  , availabilityZone :: Core.Maybe Core.Text
    -- ^ The Availability Zone (AZ) where the read replica will be created.
--
-- Default: A random, system-chosen Availability Zone in the endpoint's AWS Region.
-- Example: @us-east-1d@ 
  , copyTagsToSnapshot :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether to copy all tags from the read replica to snapshots of the read replica. By default, tags are not copied.
  , dBInstanceClass :: Core.Maybe Core.Text
    -- ^ The compute and memory capacity of the read replica, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the /Amazon RDS User Guide./ 
--
-- Default: Inherits from the source DB instance.
  , dBParameterGroupName :: Core.Maybe Core.Text
    -- ^ The name of the DB parameter group to associate with this DB instance.
--
-- If you do not specify a value for @DBParameterGroupName@ , then Amazon RDS uses the @DBParameterGroup@ of source DB instance for a same region read replica, or the default @DBParameterGroup@ for the specified DB engine for a cross region read replica.
-- Constraints:
--
--     * Must be 1 to 255 letters, numbers, or hyphens.
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
  , dBSubnetGroupName :: Core.Maybe Core.Text
    -- ^ Specifies a DB subnet group for the DB instance. The new DB instance is created in the VPC associated with the DB subnet group. If no DB subnet group is specified, then the new DB instance isn't created in a VPC.
--
-- Constraints:
--
--     * Can only be specified if the source DB instance identifier specifies a DB instance in another AWS Region.
--
--
--     * If supplied, must match the name of an existing DBSubnetGroup.
--
--
--     * The specified DB subnet group must be in the same AWS Region in which the operation is running.
--
--
--     * All read replicas in one AWS Region that are created from the same source DB instance must either:>
--
--     * Specify DB subnet groups from the same VPC. All these read replicas are created in the same VPC.
--
--
--     * Not specify a DB subnet group. All these read replicas are created outside of any VPC.
--
--
--
--
-- Example: @mySubnetgroup@ 
  , deletionProtection :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> . 
  , domain :: Core.Maybe Core.Text
    -- ^ The Active Directory directory ID to create the DB instance in. Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be created in an Active Directory Domain.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon RDS User Guide/ .
  , domainIAMRoleName :: Core.Maybe Core.Text
    -- ^ Specify the name of the IAM role to be used when making API calls to the Directory Service.
  , enableCloudwatchLogsExports :: Core.Maybe [Core.Text]
    -- ^ The list of logs that the new DB instance is to export to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs > in the /Amazon RDS User Guide/ .
  , enableIAMDatabaseAuthentication :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- For more information about IAM database authentication, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL> in the /Amazon RDS User Guide./ 
  , enablePerformanceInsights :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether to enable Performance Insights for the read replica. 
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights> in the /Amazon RDS User Guide/ . 
  , iops :: Core.Maybe Core.Int
    -- ^ The amount of Provisioned IOPS (input/output operations per second) to be initially allocated for the DB instance.
  , kmsKeyId :: Core.Maybe Core.Text
    -- ^ The AWS KMS key ID for an encrypted read replica. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
--
-- If you create an encrypted read replica in the same AWS Region as the source DB instance, then you do not have to specify a value for this parameter. The read replica is encrypted with the same KMS key as the source DB instance.
-- If you create an encrypted read replica in a different AWS Region, then you must specify a KMS key for the destination AWS Region. KMS encryption keys are specific to the AWS Region that they are created in, and you can't use encryption keys from one AWS Region in another AWS Region.
-- You can't create an encrypted read replica from an unencrypted DB instance.
  , maxAllocatedStorage :: Core.Maybe Core.Int
    -- ^ The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
  , monitoringInterval :: Core.Maybe Core.Int
    -- ^ The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the read replica. To disable collecting Enhanced Monitoring metrics, specify 0. The default is 0.
--
-- If @MonitoringRoleArn@ is specified, then you must also set @MonitoringInterval@ to a value other than 0.
-- Valid Values: @0, 1, 5, 10, 15, 30, 60@ 
  , monitoringRoleArn :: Core.Maybe Core.Text
    -- ^ The ARN for the IAM role that permits RDS to send enhanced monitoring metrics to Amazon CloudWatch Logs. For example, @arn:aws:iam:123456789012:role/emaccess@ . For information on creating a monitoring role, go to <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.html#USER_Monitoring.OS.IAMRole To create an IAM role for Amazon RDS Enhanced Monitoring> in the /Amazon RDS User Guide/ .
--
-- If @MonitoringInterval@ is set to a value other than 0, then you must supply a @MonitoringRoleArn@ value.
  , multiAZ :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether the read replica is in a Multi-AZ deployment. 
--
-- You can create a read replica as a Multi-AZ DB instance. RDS creates a standby of your replica in another Availability Zone for failover support for the replica. Creating your read replica as a Multi-AZ DB instance is independent of whether the source database is a Multi-AZ DB instance. 
  , optionGroupName :: Core.Maybe Core.Text
    -- ^ The option group the DB instance is associated with. If omitted, the option group associated with the source instance is used.
  , performanceInsightsKMSKeyId :: Core.Maybe Core.Text
    -- ^ The AWS KMS key identifier for encryption of Performance Insights data. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
--
-- If you do not specify a value for @PerformanceInsightsKMSKeyId@ , then Amazon RDS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
  , performanceInsightsRetentionPeriod :: Core.Maybe Core.Int
    -- ^ The amount of time, in days, to retain Performance Insights data. Valid values are 7 or 731 (2 years). 
  , port :: Core.Maybe Core.Int
    -- ^ The port number that the DB instance uses for connections.
--
-- Default: Inherits from the source DB instance
-- Valid Values: @1150-65535@ 
  , preSignedUrl :: Core.Maybe Core.Text
    -- ^ The URL that contains a Signature Version 4 signed request for the @CreateDBInstanceReadReplica@ API action in the source AWS Region that contains the source DB instance. 
--
-- You must specify this parameter when you create an encrypted read replica from another AWS Region by using the Amazon RDS API. Don't specify @PreSignedUrl@ when you are creating an encrypted read replica in the same AWS Region.
-- The presigned URL must be a valid request for the @CreateDBInstanceReadReplica@ API action that can be executed in the source AWS Region that contains the encrypted source DB instance. The presigned URL request must contain the following parameter values: 
--
--     * @DestinationRegion@ - The AWS Region that the encrypted read replica is created in. This AWS Region is the same one where the @CreateDBInstanceReadReplica@ action is called that contains this presigned URL.
-- For example, if you create an encrypted DB instance in the us-west-1 AWS Region, from a source DB instance in the us-east-2 AWS Region, then you call the @CreateDBInstanceReadReplica@ action in the us-east-1 AWS Region and provide a presigned URL that contains a call to the @CreateDBInstanceReadReplica@ action in the us-west-2 AWS Region. For this example, the @DestinationRegion@ in the presigned URL must be set to the us-east-1 AWS Region. 
--
--
--     * @KmsKeyId@ - The AWS KMS key identifier for the key to use to encrypt the read replica in the destination AWS Region. This is the same identifier for both the @CreateDBInstanceReadReplica@ action that is called in the destination AWS Region, and the action contained in the presigned URL. 
--
--
--     * @SourceDBInstanceIdentifier@ - The DB instance identifier for the encrypted DB instance to be replicated. This identifier must be in the Amazon Resource Name (ARN) format for the source AWS Region. For example, if you are creating an encrypted read replica from a DB instance in the us-west-2 AWS Region, then your @SourceDBInstanceIdentifier@ looks like the following example: @arn:aws:rds:us-west-2:123456789012:instance:mysql-instance1-20161115@ . 
--
--
-- To learn how to generate a Signature Version 4 signed request, see <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (AWS Signature Version 4)> and <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> . 
  , processorFeatures :: Core.Maybe [Types.ProcessorFeature]
    -- ^ The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
  , publiclyAccessible :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether the DB instance is publicly accessible.
--
-- When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it.
-- When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address.
-- For more information, see 'CreateDBInstance' .
  , replicaMode :: Core.Maybe Types.ReplicaMode
    -- ^ The open mode of the replica database: mounted or read-only.
--
-- Mounted DB replicas are included in Oracle Enterprise Edition. The main use case for mounted replicas is cross-Region disaster recovery. The primary database doesn't use Active Data Guard to transmit information to the mounted replica. Because it doesn't accept user connections, a mounted replica can't serve a read-only workload.
-- You can create a combination of mounted and read-only DB replicas for the same primary DB instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Working with Oracle Read Replicas for Amazon RDS> in the /Amazon RDS User Guide/ .
  , storageType :: Core.Maybe Core.Text
    -- ^ Specifies the storage type to be associated with the read replica.
--
-- Valid values: @standard | gp2 | io1@ 
-- If you specify @io1@ , you must also include a value for the @Iops@ parameter. 
-- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@ 
  , tags :: Core.Maybe [Types.Tag]
  , useDefaultProcessorFeatures :: Core.Maybe Core.Bool
    -- ^ A value that indicates whether the DB instance class of the DB instance uses its default processor features.
  , vpcSecurityGroupIds :: Core.Maybe [Core.Text]
    -- ^ A list of EC2 VPC security groups to associate with the read replica. 
--
-- Default: The default EC2 VPC security group for the DB subnet group's VPC. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDBInstanceReadReplica' value with any optional fields omitted.
mkCreateDBInstanceReadReplica
    :: Core.Text -- ^ 'dBInstanceIdentifier'
    -> Core.Text -- ^ 'sourceDBInstanceIdentifier'
    -> CreateDBInstanceReadReplica
mkCreateDBInstanceReadReplica dBInstanceIdentifier
  sourceDBInstanceIdentifier
  = CreateDBInstanceReadReplica'{dBInstanceIdentifier,
                                 sourceDBInstanceIdentifier, autoMinorVersionUpgrade = Core.Nothing,
                                 availabilityZone = Core.Nothing, copyTagsToSnapshot = Core.Nothing,
                                 dBInstanceClass = Core.Nothing,
                                 dBParameterGroupName = Core.Nothing,
                                 dBSubnetGroupName = Core.Nothing,
                                 deletionProtection = Core.Nothing, domain = Core.Nothing,
                                 domainIAMRoleName = Core.Nothing,
                                 enableCloudwatchLogsExports = Core.Nothing,
                                 enableIAMDatabaseAuthentication = Core.Nothing,
                                 enablePerformanceInsights = Core.Nothing, iops = Core.Nothing,
                                 kmsKeyId = Core.Nothing, maxAllocatedStorage = Core.Nothing,
                                 monitoringInterval = Core.Nothing,
                                 monitoringRoleArn = Core.Nothing, multiAZ = Core.Nothing,
                                 optionGroupName = Core.Nothing,
                                 performanceInsightsKMSKeyId = Core.Nothing,
                                 performanceInsightsRetentionPeriod = Core.Nothing,
                                 port = Core.Nothing, preSignedUrl = Core.Nothing,
                                 processorFeatures = Core.Nothing,
                                 publiclyAccessible = Core.Nothing, replicaMode = Core.Nothing,
                                 storageType = Core.Nothing, tags = Core.Nothing,
                                 useDefaultProcessorFeatures = Core.Nothing,
                                 vpcSecurityGroupIds = Core.Nothing}

-- | The DB instance identifier of the read replica. This identifier is the unique key that identifies a DB instance. This parameter is stored as a lowercase string.
--
-- /Note:/ Consider using 'dBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrDBInstanceIdentifier :: Lens.Lens' CreateDBInstanceReadReplica Core.Text
cdbirrDBInstanceIdentifier = Lens.field @"dBInstanceIdentifier"
{-# INLINEABLE cdbirrDBInstanceIdentifier #-}
{-# DEPRECATED dBInstanceIdentifier "Use generic-lens or generic-optics with 'dBInstanceIdentifier' instead"  #-}

-- | The identifier of the DB instance that will act as the source for the read replica. Each DB instance can have up to five read replicas.
--
-- Constraints:
--
--     * Must be the identifier of an existing MySQL, MariaDB, Oracle, PostgreSQL, or SQL Server DB instance.
--
--
--     * Can specify a DB instance that is a MySQL read replica only if the source is running MySQL 5.6 or later.
--
--
--     * For the limitations of Oracle read replicas, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Read Replica Limitations with Oracle> in the /Amazon RDS User Guide/ .
--
--
--     * For the limitations of SQL Server read replicas, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/SQLServer.ReadReplicas.Limitations.html Read Replica Limitations with Microsoft SQL Server> in the /Amazon RDS User Guide/ .
--
--
--     * Can specify a PostgreSQL DB instance only if the source is running PostgreSQL 9.3.5 or later (9.4.7 and higher for cross-region replication).
--
--
--     * The specified DB instance must have automatic backups enabled, that is, its backup retention period must be greater than 0.
--
--
--     * If the source DB instance is in the same AWS Region as the read replica, specify a valid DB instance identifier.
--
--
--     * If the source DB instance is in a different AWS Region from the read replica, specify a valid DB instance ARN. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS> in the /Amazon RDS User Guide/ . This doesn't apply to SQL Server, which doesn't support cross-region replicas.
--
--
--
-- /Note:/ Consider using 'sourceDBInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrSourceDBInstanceIdentifier :: Lens.Lens' CreateDBInstanceReadReplica Core.Text
cdbirrSourceDBInstanceIdentifier = Lens.field @"sourceDBInstanceIdentifier"
{-# INLINEABLE cdbirrSourceDBInstanceIdentifier #-}
{-# DEPRECATED sourceDBInstanceIdentifier "Use generic-lens or generic-optics with 'sourceDBInstanceIdentifier' instead"  #-}

-- | A value that indicates whether minor engine upgrades are applied automatically to the read replica during the maintenance window.
--
-- Default: Inherits from the source DB instance
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrAutoMinorVersionUpgrade :: Lens.Lens' CreateDBInstanceReadReplica (Core.Maybe Core.Bool)
cdbirrAutoMinorVersionUpgrade = Lens.field @"autoMinorVersionUpgrade"
{-# INLINEABLE cdbirrAutoMinorVersionUpgrade #-}
{-# DEPRECATED autoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead"  #-}

-- | The Availability Zone (AZ) where the read replica will be created.
--
-- Default: A random, system-chosen Availability Zone in the endpoint's AWS Region.
-- Example: @us-east-1d@ 
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrAvailabilityZone :: Lens.Lens' CreateDBInstanceReadReplica (Core.Maybe Core.Text)
cdbirrAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE cdbirrAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | A value that indicates whether to copy all tags from the read replica to snapshots of the read replica. By default, tags are not copied.
--
-- /Note:/ Consider using 'copyTagsToSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrCopyTagsToSnapshot :: Lens.Lens' CreateDBInstanceReadReplica (Core.Maybe Core.Bool)
cdbirrCopyTagsToSnapshot = Lens.field @"copyTagsToSnapshot"
{-# INLINEABLE cdbirrCopyTagsToSnapshot #-}
{-# DEPRECATED copyTagsToSnapshot "Use generic-lens or generic-optics with 'copyTagsToSnapshot' instead"  #-}

-- | The compute and memory capacity of the read replica, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the /Amazon RDS User Guide./ 
--
-- Default: Inherits from the source DB instance.
--
-- /Note:/ Consider using 'dBInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrDBInstanceClass :: Lens.Lens' CreateDBInstanceReadReplica (Core.Maybe Core.Text)
cdbirrDBInstanceClass = Lens.field @"dBInstanceClass"
{-# INLINEABLE cdbirrDBInstanceClass #-}
{-# DEPRECATED dBInstanceClass "Use generic-lens or generic-optics with 'dBInstanceClass' instead"  #-}

-- | The name of the DB parameter group to associate with this DB instance.
--
-- If you do not specify a value for @DBParameterGroupName@ , then Amazon RDS uses the @DBParameterGroup@ of source DB instance for a same region read replica, or the default @DBParameterGroup@ for the specified DB engine for a cross region read replica.
-- Constraints:
--
--     * Must be 1 to 255 letters, numbers, or hyphens.
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
--
-- /Note:/ Consider using 'dBParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrDBParameterGroupName :: Lens.Lens' CreateDBInstanceReadReplica (Core.Maybe Core.Text)
cdbirrDBParameterGroupName = Lens.field @"dBParameterGroupName"
{-# INLINEABLE cdbirrDBParameterGroupName #-}
{-# DEPRECATED dBParameterGroupName "Use generic-lens or generic-optics with 'dBParameterGroupName' instead"  #-}

-- | Specifies a DB subnet group for the DB instance. The new DB instance is created in the VPC associated with the DB subnet group. If no DB subnet group is specified, then the new DB instance isn't created in a VPC.
--
-- Constraints:
--
--     * Can only be specified if the source DB instance identifier specifies a DB instance in another AWS Region.
--
--
--     * If supplied, must match the name of an existing DBSubnetGroup.
--
--
--     * The specified DB subnet group must be in the same AWS Region in which the operation is running.
--
--
--     * All read replicas in one AWS Region that are created from the same source DB instance must either:>
--
--     * Specify DB subnet groups from the same VPC. All these read replicas are created in the same VPC.
--
--
--     * Not specify a DB subnet group. All these read replicas are created outside of any VPC.
--
--
--
--
-- Example: @mySubnetgroup@ 
--
-- /Note:/ Consider using 'dBSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrDBSubnetGroupName :: Lens.Lens' CreateDBInstanceReadReplica (Core.Maybe Core.Text)
cdbirrDBSubnetGroupName = Lens.field @"dBSubnetGroupName"
{-# INLINEABLE cdbirrDBSubnetGroupName #-}
{-# DEPRECATED dBSubnetGroupName "Use generic-lens or generic-optics with 'dBSubnetGroupName' instead"  #-}

-- | A value that indicates whether the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> . 
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrDeletionProtection :: Lens.Lens' CreateDBInstanceReadReplica (Core.Maybe Core.Bool)
cdbirrDeletionProtection = Lens.field @"deletionProtection"
{-# INLINEABLE cdbirrDeletionProtection #-}
{-# DEPRECATED deletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead"  #-}

-- | The Active Directory directory ID to create the DB instance in. Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be created in an Active Directory Domain.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon RDS User Guide/ .
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrDomain :: Lens.Lens' CreateDBInstanceReadReplica (Core.Maybe Core.Text)
cdbirrDomain = Lens.field @"domain"
{-# INLINEABLE cdbirrDomain #-}
{-# DEPRECATED domain "Use generic-lens or generic-optics with 'domain' instead"  #-}

-- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
--
-- /Note:/ Consider using 'domainIAMRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrDomainIAMRoleName :: Lens.Lens' CreateDBInstanceReadReplica (Core.Maybe Core.Text)
cdbirrDomainIAMRoleName = Lens.field @"domainIAMRoleName"
{-# INLINEABLE cdbirrDomainIAMRoleName #-}
{-# DEPRECATED domainIAMRoleName "Use generic-lens or generic-optics with 'domainIAMRoleName' instead"  #-}

-- | The list of logs that the new DB instance is to export to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs > in the /Amazon RDS User Guide/ .
--
-- /Note:/ Consider using 'enableCloudwatchLogsExports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrEnableCloudwatchLogsExports :: Lens.Lens' CreateDBInstanceReadReplica (Core.Maybe [Core.Text])
cdbirrEnableCloudwatchLogsExports = Lens.field @"enableCloudwatchLogsExports"
{-# INLINEABLE cdbirrEnableCloudwatchLogsExports #-}
{-# DEPRECATED enableCloudwatchLogsExports "Use generic-lens or generic-optics with 'enableCloudwatchLogsExports' instead"  #-}

-- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- For more information about IAM database authentication, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL> in the /Amazon RDS User Guide./ 
--
-- /Note:/ Consider using 'enableIAMDatabaseAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrEnableIAMDatabaseAuthentication :: Lens.Lens' CreateDBInstanceReadReplica (Core.Maybe Core.Bool)
cdbirrEnableIAMDatabaseAuthentication = Lens.field @"enableIAMDatabaseAuthentication"
{-# INLINEABLE cdbirrEnableIAMDatabaseAuthentication #-}
{-# DEPRECATED enableIAMDatabaseAuthentication "Use generic-lens or generic-optics with 'enableIAMDatabaseAuthentication' instead"  #-}

-- | A value that indicates whether to enable Performance Insights for the read replica. 
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights> in the /Amazon RDS User Guide/ . 
--
-- /Note:/ Consider using 'enablePerformanceInsights' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrEnablePerformanceInsights :: Lens.Lens' CreateDBInstanceReadReplica (Core.Maybe Core.Bool)
cdbirrEnablePerformanceInsights = Lens.field @"enablePerformanceInsights"
{-# INLINEABLE cdbirrEnablePerformanceInsights #-}
{-# DEPRECATED enablePerformanceInsights "Use generic-lens or generic-optics with 'enablePerformanceInsights' instead"  #-}

-- | The amount of Provisioned IOPS (input/output operations per second) to be initially allocated for the DB instance.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrIops :: Lens.Lens' CreateDBInstanceReadReplica (Core.Maybe Core.Int)
cdbirrIops = Lens.field @"iops"
{-# INLINEABLE cdbirrIops #-}
{-# DEPRECATED iops "Use generic-lens or generic-optics with 'iops' instead"  #-}

-- | The AWS KMS key ID for an encrypted read replica. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
--
-- If you create an encrypted read replica in the same AWS Region as the source DB instance, then you do not have to specify a value for this parameter. The read replica is encrypted with the same KMS key as the source DB instance.
-- If you create an encrypted read replica in a different AWS Region, then you must specify a KMS key for the destination AWS Region. KMS encryption keys are specific to the AWS Region that they are created in, and you can't use encryption keys from one AWS Region in another AWS Region.
-- You can't create an encrypted read replica from an unencrypted DB instance.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrKmsKeyId :: Lens.Lens' CreateDBInstanceReadReplica (Core.Maybe Core.Text)
cdbirrKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE cdbirrKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
--
-- /Note:/ Consider using 'maxAllocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrMaxAllocatedStorage :: Lens.Lens' CreateDBInstanceReadReplica (Core.Maybe Core.Int)
cdbirrMaxAllocatedStorage = Lens.field @"maxAllocatedStorage"
{-# INLINEABLE cdbirrMaxAllocatedStorage #-}
{-# DEPRECATED maxAllocatedStorage "Use generic-lens or generic-optics with 'maxAllocatedStorage' instead"  #-}

-- | The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the read replica. To disable collecting Enhanced Monitoring metrics, specify 0. The default is 0.
--
-- If @MonitoringRoleArn@ is specified, then you must also set @MonitoringInterval@ to a value other than 0.
-- Valid Values: @0, 1, 5, 10, 15, 30, 60@ 
--
-- /Note:/ Consider using 'monitoringInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrMonitoringInterval :: Lens.Lens' CreateDBInstanceReadReplica (Core.Maybe Core.Int)
cdbirrMonitoringInterval = Lens.field @"monitoringInterval"
{-# INLINEABLE cdbirrMonitoringInterval #-}
{-# DEPRECATED monitoringInterval "Use generic-lens or generic-optics with 'monitoringInterval' instead"  #-}

-- | The ARN for the IAM role that permits RDS to send enhanced monitoring metrics to Amazon CloudWatch Logs. For example, @arn:aws:iam:123456789012:role/emaccess@ . For information on creating a monitoring role, go to <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.html#USER_Monitoring.OS.IAMRole To create an IAM role for Amazon RDS Enhanced Monitoring> in the /Amazon RDS User Guide/ .
--
-- If @MonitoringInterval@ is set to a value other than 0, then you must supply a @MonitoringRoleArn@ value.
--
-- /Note:/ Consider using 'monitoringRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrMonitoringRoleArn :: Lens.Lens' CreateDBInstanceReadReplica (Core.Maybe Core.Text)
cdbirrMonitoringRoleArn = Lens.field @"monitoringRoleArn"
{-# INLINEABLE cdbirrMonitoringRoleArn #-}
{-# DEPRECATED monitoringRoleArn "Use generic-lens or generic-optics with 'monitoringRoleArn' instead"  #-}

-- | A value that indicates whether the read replica is in a Multi-AZ deployment. 
--
-- You can create a read replica as a Multi-AZ DB instance. RDS creates a standby of your replica in another Availability Zone for failover support for the replica. Creating your read replica as a Multi-AZ DB instance is independent of whether the source database is a Multi-AZ DB instance. 
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrMultiAZ :: Lens.Lens' CreateDBInstanceReadReplica (Core.Maybe Core.Bool)
cdbirrMultiAZ = Lens.field @"multiAZ"
{-# INLINEABLE cdbirrMultiAZ #-}
{-# DEPRECATED multiAZ "Use generic-lens or generic-optics with 'multiAZ' instead"  #-}

-- | The option group the DB instance is associated with. If omitted, the option group associated with the source instance is used.
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrOptionGroupName :: Lens.Lens' CreateDBInstanceReadReplica (Core.Maybe Core.Text)
cdbirrOptionGroupName = Lens.field @"optionGroupName"
{-# INLINEABLE cdbirrOptionGroupName #-}
{-# DEPRECATED optionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead"  #-}

-- | The AWS KMS key identifier for encryption of Performance Insights data. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
--
-- If you do not specify a value for @PerformanceInsightsKMSKeyId@ , then Amazon RDS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- /Note:/ Consider using 'performanceInsightsKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrPerformanceInsightsKMSKeyId :: Lens.Lens' CreateDBInstanceReadReplica (Core.Maybe Core.Text)
cdbirrPerformanceInsightsKMSKeyId = Lens.field @"performanceInsightsKMSKeyId"
{-# INLINEABLE cdbirrPerformanceInsightsKMSKeyId #-}
{-# DEPRECATED performanceInsightsKMSKeyId "Use generic-lens or generic-optics with 'performanceInsightsKMSKeyId' instead"  #-}

-- | The amount of time, in days, to retain Performance Insights data. Valid values are 7 or 731 (2 years). 
--
-- /Note:/ Consider using 'performanceInsightsRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrPerformanceInsightsRetentionPeriod :: Lens.Lens' CreateDBInstanceReadReplica (Core.Maybe Core.Int)
cdbirrPerformanceInsightsRetentionPeriod = Lens.field @"performanceInsightsRetentionPeriod"
{-# INLINEABLE cdbirrPerformanceInsightsRetentionPeriod #-}
{-# DEPRECATED performanceInsightsRetentionPeriod "Use generic-lens or generic-optics with 'performanceInsightsRetentionPeriod' instead"  #-}

-- | The port number that the DB instance uses for connections.
--
-- Default: Inherits from the source DB instance
-- Valid Values: @1150-65535@ 
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrPort :: Lens.Lens' CreateDBInstanceReadReplica (Core.Maybe Core.Int)
cdbirrPort = Lens.field @"port"
{-# INLINEABLE cdbirrPort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

-- | The URL that contains a Signature Version 4 signed request for the @CreateDBInstanceReadReplica@ API action in the source AWS Region that contains the source DB instance. 
--
-- You must specify this parameter when you create an encrypted read replica from another AWS Region by using the Amazon RDS API. Don't specify @PreSignedUrl@ when you are creating an encrypted read replica in the same AWS Region.
-- The presigned URL must be a valid request for the @CreateDBInstanceReadReplica@ API action that can be executed in the source AWS Region that contains the encrypted source DB instance. The presigned URL request must contain the following parameter values: 
--
--     * @DestinationRegion@ - The AWS Region that the encrypted read replica is created in. This AWS Region is the same one where the @CreateDBInstanceReadReplica@ action is called that contains this presigned URL.
-- For example, if you create an encrypted DB instance in the us-west-1 AWS Region, from a source DB instance in the us-east-2 AWS Region, then you call the @CreateDBInstanceReadReplica@ action in the us-east-1 AWS Region and provide a presigned URL that contains a call to the @CreateDBInstanceReadReplica@ action in the us-west-2 AWS Region. For this example, the @DestinationRegion@ in the presigned URL must be set to the us-east-1 AWS Region. 
--
--
--     * @KmsKeyId@ - The AWS KMS key identifier for the key to use to encrypt the read replica in the destination AWS Region. This is the same identifier for both the @CreateDBInstanceReadReplica@ action that is called in the destination AWS Region, and the action contained in the presigned URL. 
--
--
--     * @SourceDBInstanceIdentifier@ - The DB instance identifier for the encrypted DB instance to be replicated. This identifier must be in the Amazon Resource Name (ARN) format for the source AWS Region. For example, if you are creating an encrypted read replica from a DB instance in the us-west-2 AWS Region, then your @SourceDBInstanceIdentifier@ looks like the following example: @arn:aws:rds:us-west-2:123456789012:instance:mysql-instance1-20161115@ . 
--
--
-- To learn how to generate a Signature Version 4 signed request, see <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (AWS Signature Version 4)> and <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process> . 
--
-- /Note:/ Consider using 'preSignedUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrPreSignedUrl :: Lens.Lens' CreateDBInstanceReadReplica (Core.Maybe Core.Text)
cdbirrPreSignedUrl = Lens.field @"preSignedUrl"
{-# INLINEABLE cdbirrPreSignedUrl #-}
{-# DEPRECATED preSignedUrl "Use generic-lens or generic-optics with 'preSignedUrl' instead"  #-}

-- | The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
--
-- /Note:/ Consider using 'processorFeatures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrProcessorFeatures :: Lens.Lens' CreateDBInstanceReadReplica (Core.Maybe [Types.ProcessorFeature])
cdbirrProcessorFeatures = Lens.field @"processorFeatures"
{-# INLINEABLE cdbirrProcessorFeatures #-}
{-# DEPRECATED processorFeatures "Use generic-lens or generic-optics with 'processorFeatures' instead"  #-}

-- | A value that indicates whether the DB instance is publicly accessible.
--
-- When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it.
-- When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address.
-- For more information, see 'CreateDBInstance' .
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrPubliclyAccessible :: Lens.Lens' CreateDBInstanceReadReplica (Core.Maybe Core.Bool)
cdbirrPubliclyAccessible = Lens.field @"publiclyAccessible"
{-# INLINEABLE cdbirrPubliclyAccessible #-}
{-# DEPRECATED publiclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead"  #-}

-- | The open mode of the replica database: mounted or read-only.
--
-- Mounted DB replicas are included in Oracle Enterprise Edition. The main use case for mounted replicas is cross-Region disaster recovery. The primary database doesn't use Active Data Guard to transmit information to the mounted replica. Because it doesn't accept user connections, a mounted replica can't serve a read-only workload.
-- You can create a combination of mounted and read-only DB replicas for the same primary DB instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Working with Oracle Read Replicas for Amazon RDS> in the /Amazon RDS User Guide/ .
--
-- /Note:/ Consider using 'replicaMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrReplicaMode :: Lens.Lens' CreateDBInstanceReadReplica (Core.Maybe Types.ReplicaMode)
cdbirrReplicaMode = Lens.field @"replicaMode"
{-# INLINEABLE cdbirrReplicaMode #-}
{-# DEPRECATED replicaMode "Use generic-lens or generic-optics with 'replicaMode' instead"  #-}

-- | Specifies the storage type to be associated with the read replica.
--
-- Valid values: @standard | gp2 | io1@ 
-- If you specify @io1@ , you must also include a value for the @Iops@ parameter. 
-- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@ 
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrStorageType :: Lens.Lens' CreateDBInstanceReadReplica (Core.Maybe Core.Text)
cdbirrStorageType = Lens.field @"storageType"
{-# INLINEABLE cdbirrStorageType #-}
{-# DEPRECATED storageType "Use generic-lens or generic-optics with 'storageType' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrTags :: Lens.Lens' CreateDBInstanceReadReplica (Core.Maybe [Types.Tag])
cdbirrTags = Lens.field @"tags"
{-# INLINEABLE cdbirrTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | A value that indicates whether the DB instance class of the DB instance uses its default processor features.
--
-- /Note:/ Consider using 'useDefaultProcessorFeatures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrUseDefaultProcessorFeatures :: Lens.Lens' CreateDBInstanceReadReplica (Core.Maybe Core.Bool)
cdbirrUseDefaultProcessorFeatures = Lens.field @"useDefaultProcessorFeatures"
{-# INLINEABLE cdbirrUseDefaultProcessorFeatures #-}
{-# DEPRECATED useDefaultProcessorFeatures "Use generic-lens or generic-optics with 'useDefaultProcessorFeatures' instead"  #-}

-- | A list of EC2 VPC security groups to associate with the read replica. 
--
-- Default: The default EC2 VPC security group for the DB subnet group's VPC. 
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrVpcSecurityGroupIds :: Lens.Lens' CreateDBInstanceReadReplica (Core.Maybe [Core.Text])
cdbirrVpcSecurityGroupIds = Lens.field @"vpcSecurityGroupIds"
{-# INLINEABLE cdbirrVpcSecurityGroupIds #-}
{-# DEPRECATED vpcSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead"  #-}

instance Core.ToQuery CreateDBInstanceReadReplica where
        toQuery CreateDBInstanceReadReplica{..}
          = Core.toQueryPair "Action"
              ("CreateDBInstanceReadReplica" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "DBInstanceIdentifier" dBInstanceIdentifier
              Core.<>
              Core.toQueryPair "SourceDBInstanceIdentifier"
                sourceDBInstanceIdentifier
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AutoMinorVersionUpgrade")
                autoMinorVersionUpgrade
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AvailabilityZone")
                availabilityZone
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CopyTagsToSnapshot")
                copyTagsToSnapshot
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DBInstanceClass")
                dBInstanceClass
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DBParameterGroupName")
                dBParameterGroupName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DBSubnetGroupName")
                dBSubnetGroupName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DeletionProtection")
                deletionProtection
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Domain") domain
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DomainIAMRoleName")
                domainIAMRoleName
              Core.<>
              Core.toQueryPair "EnableCloudwatchLogsExports"
                (Core.maybe Core.mempty (Core.toQueryList "member")
                   enableCloudwatchLogsExports)
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "EnableIAMDatabaseAuthentication")
                enableIAMDatabaseAuthentication
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "EnablePerformanceInsights")
                enablePerformanceInsights
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Iops") iops
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "KmsKeyId") kmsKeyId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxAllocatedStorage")
                maxAllocatedStorage
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MonitoringInterval")
                monitoringInterval
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MonitoringRoleArn")
                monitoringRoleArn
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "MultiAZ") multiAZ
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "OptionGroupName")
                optionGroupName
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "PerformanceInsightsKMSKeyId")
                performanceInsightsKMSKeyId
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "PerformanceInsightsRetentionPeriod")
                performanceInsightsRetentionPeriod
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Port") port
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PreSignedUrl")
                preSignedUrl
              Core.<>
              Core.toQueryPair "ProcessorFeatures"
                (Core.maybe Core.mempty (Core.toQueryList "ProcessorFeature")
                   processorFeatures)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PubliclyAccessible")
                publiclyAccessible
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ReplicaMode") replicaMode
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "StorageType") storageType
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "Tag") tags)
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "UseDefaultProcessorFeatures")
                useDefaultProcessorFeatures
              Core.<>
              Core.toQueryPair "VpcSecurityGroupIds"
                (Core.maybe Core.mempty (Core.toQueryList "VpcSecurityGroupId")
                   vpcSecurityGroupIds)

instance Core.ToHeaders CreateDBInstanceReadReplica where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateDBInstanceReadReplica where
        type Rs CreateDBInstanceReadReplica =
             CreateDBInstanceReadReplicaResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "CreateDBInstanceReadReplicaResult"
              (\ s h x ->
                 CreateDBInstanceReadReplicaResponse' Core.<$>
                   (x Core..@? "DBInstance") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateDBInstanceReadReplicaResponse' smart constructor.
data CreateDBInstanceReadReplicaResponse = CreateDBInstanceReadReplicaResponse'
  { dBInstance :: Core.Maybe Types.DBInstance
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateDBInstanceReadReplicaResponse' value with any optional fields omitted.
mkCreateDBInstanceReadReplicaResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDBInstanceReadReplicaResponse
mkCreateDBInstanceReadReplicaResponse responseStatus
  = CreateDBInstanceReadReplicaResponse'{dBInstance = Core.Nothing,
                                         responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrrrsDBInstance :: Lens.Lens' CreateDBInstanceReadReplicaResponse (Core.Maybe Types.DBInstance)
cdbirrrrsDBInstance = Lens.field @"dBInstance"
{-# INLINEABLE cdbirrrrsDBInstance #-}
{-# DEPRECATED dBInstance "Use generic-lens or generic-optics with 'dBInstance' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbirrrrsResponseStatus :: Lens.Lens' CreateDBInstanceReadReplicaResponse Core.Int
cdbirrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdbirrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
