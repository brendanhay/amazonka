{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateDBInstanceReadReplica (..),
    mkCreateDBInstanceReadReplica,

    -- ** Request lenses
    cdirrDeletionProtection,
    cdirrPubliclyAccessible,
    cdirrAutoMinorVersionUpgrade,
    cdirrDBSubnetGroupName,
    cdirrMonitoringRoleARN,
    cdirrIOPS,
    cdirrDomain,
    cdirrReplicaMode,
    cdirrMonitoringInterval,
    cdirrPreSignedURL,
    cdirrSourceDBInstanceIdentifier,
    cdirrProcessorFeatures,
    cdirrDBInstanceClass,
    cdirrPerformanceInsightsRetentionPeriod,
    cdirrDBInstanceIdentifier,
    cdirrMaxAllocatedStorage,
    cdirrEnablePerformanceInsights,
    cdirrKMSKeyId,
    cdirrDBParameterGroupName,
    cdirrAvailabilityZone,
    cdirrPerformanceInsightsKMSKeyId,
    cdirrVPCSecurityGroupIds,
    cdirrMultiAZ,
    cdirrOptionGroupName,
    cdirrCopyTagsToSnapshot,
    cdirrDomainIAMRoleName,
    cdirrTags,
    cdirrPort,
    cdirrEnableIAMDatabaseAuthentication,
    cdirrUseDefaultProcessorFeatures,
    cdirrStorageType,
    cdirrEnableCloudwatchLogsExports,

    -- * Destructuring the response
    CreateDBInstanceReadReplicaResponse (..),
    mkCreateDBInstanceReadReplicaResponse,

    -- ** Response lenses
    cdirrrsDBInstance,
    cdirrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateDBInstanceReadReplica' smart constructor.
data CreateDBInstanceReadReplica = CreateDBInstanceReadReplica'
  { -- | A value that indicates whether the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> .
    deletionProtection :: Lude.Maybe Lude.Bool,
    -- | A value that indicates whether the DB instance is publicly accessible.
    --
    -- When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it.
    -- When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address.
    -- For more information, see 'CreateDBInstance' .
    publiclyAccessible :: Lude.Maybe Lude.Bool,
    -- | A value that indicates whether minor engine upgrades are applied automatically to the read replica during the maintenance window.
    --
    -- Default: Inherits from the source DB instance
    autoMinorVersionUpgrade :: Lude.Maybe Lude.Bool,
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
    dbSubnetGroupName :: Lude.Maybe Lude.Text,
    -- | The ARN for the IAM role that permits RDS to send enhanced monitoring metrics to Amazon CloudWatch Logs. For example, @arn:aws:iam:123456789012:role/emaccess@ . For information on creating a monitoring role, go to <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.html#USER_Monitoring.OS.IAMRole To create an IAM role for Amazon RDS Enhanced Monitoring> in the /Amazon RDS User Guide/ .
    --
    -- If @MonitoringInterval@ is set to a value other than 0, then you must supply a @MonitoringRoleArn@ value.
    monitoringRoleARN :: Lude.Maybe Lude.Text,
    -- | The amount of Provisioned IOPS (input/output operations per second) to be initially allocated for the DB instance.
    iops :: Lude.Maybe Lude.Int,
    -- | The Active Directory directory ID to create the DB instance in. Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be created in an Active Directory Domain.
    --
    -- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon RDS User Guide/ .
    domain :: Lude.Maybe Lude.Text,
    -- | The open mode of the replica database: mounted or read-only.
    --
    -- Mounted DB replicas are included in Oracle Enterprise Edition. The main use case for mounted replicas is cross-Region disaster recovery. The primary database doesn't use Active Data Guard to transmit information to the mounted replica. Because it doesn't accept user connections, a mounted replica can't serve a read-only workload.
    -- You can create a combination of mounted and read-only DB replicas for the same primary DB instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Working with Oracle Read Replicas for Amazon RDS> in the /Amazon RDS User Guide/ .
    replicaMode :: Lude.Maybe ReplicaMode,
    -- | The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the read replica. To disable collecting Enhanced Monitoring metrics, specify 0. The default is 0.
    --
    -- If @MonitoringRoleArn@ is specified, then you must also set @MonitoringInterval@ to a value other than 0.
    -- Valid Values: @0, 1, 5, 10, 15, 30, 60@
    monitoringInterval :: Lude.Maybe Lude.Int,
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
    preSignedURL :: Lude.Maybe Lude.Text,
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
    sourceDBInstanceIdentifier :: Lude.Text,
    -- | The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
    processorFeatures :: Lude.Maybe [ProcessorFeature],
    -- | The compute and memory capacity of the read replica, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the /Amazon RDS User Guide./
    --
    -- Default: Inherits from the source DB instance.
    dbInstanceClass :: Lude.Maybe Lude.Text,
    -- | The amount of time, in days, to retain Performance Insights data. Valid values are 7 or 731 (2 years).
    performanceInsightsRetentionPeriod :: Lude.Maybe Lude.Int,
    -- | The DB instance identifier of the read replica. This identifier is the unique key that identifies a DB instance. This parameter is stored as a lowercase string.
    dbInstanceIdentifier :: Lude.Text,
    -- | The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
    maxAllocatedStorage :: Lude.Maybe Lude.Int,
    -- | A value that indicates whether to enable Performance Insights for the read replica.
    --
    -- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights> in the /Amazon RDS User Guide/ .
    enablePerformanceInsights :: Lude.Maybe Lude.Bool,
    -- | The AWS KMS key ID for an encrypted read replica. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
    --
    -- If you create an encrypted read replica in the same AWS Region as the source DB instance, then you do not have to specify a value for this parameter. The read replica is encrypted with the same KMS key as the source DB instance.
    -- If you create an encrypted read replica in a different AWS Region, then you must specify a KMS key for the destination AWS Region. KMS encryption keys are specific to the AWS Region that they are created in, and you can't use encryption keys from one AWS Region in another AWS Region.
    -- You can't create an encrypted read replica from an unencrypted DB instance.
    kmsKeyId :: Lude.Maybe Lude.Text,
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
    dbParameterGroupName :: Lude.Maybe Lude.Text,
    -- | The Availability Zone (AZ) where the read replica will be created.
    --
    -- Default: A random, system-chosen Availability Zone in the endpoint's AWS Region.
    -- Example: @us-east-1d@
    availabilityZone :: Lude.Maybe Lude.Text,
    -- | The AWS KMS key identifier for encryption of Performance Insights data. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
    --
    -- If you do not specify a value for @PerformanceInsightsKMSKeyId@ , then Amazon RDS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
    performanceInsightsKMSKeyId :: Lude.Maybe Lude.Text,
    -- | A list of EC2 VPC security groups to associate with the read replica.
    --
    -- Default: The default EC2 VPC security group for the DB subnet group's VPC.
    vpcSecurityGroupIds :: Lude.Maybe [Lude.Text],
    -- | A value that indicates whether the read replica is in a Multi-AZ deployment.
    --
    -- You can create a read replica as a Multi-AZ DB instance. RDS creates a standby of your replica in another Availability Zone for failover support for the replica. Creating your read replica as a Multi-AZ DB instance is independent of whether the source database is a Multi-AZ DB instance.
    multiAZ :: Lude.Maybe Lude.Bool,
    -- | The option group the DB instance is associated with. If omitted, the option group associated with the source instance is used.
    optionGroupName :: Lude.Maybe Lude.Text,
    -- | A value that indicates whether to copy all tags from the read replica to snapshots of the read replica. By default, tags are not copied.
    copyTagsToSnapshot :: Lude.Maybe Lude.Bool,
    -- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
    domainIAMRoleName :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    -- | The port number that the DB instance uses for connections.
    --
    -- Default: Inherits from the source DB instance
    -- Valid Values: @1150-65535@
    port :: Lude.Maybe Lude.Int,
    -- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
    --
    -- For more information about IAM database authentication, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL> in the /Amazon RDS User Guide./
    enableIAMDatabaseAuthentication :: Lude.Maybe Lude.Bool,
    -- | A value that indicates whether the DB instance class of the DB instance uses its default processor features.
    useDefaultProcessorFeatures :: Lude.Maybe Lude.Bool,
    -- | Specifies the storage type to be associated with the read replica.
    --
    -- Valid values: @standard | gp2 | io1@
    -- If you specify @io1@ , you must also include a value for the @Iops@ parameter.
    -- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
    storageType :: Lude.Maybe Lude.Text,
    -- | The list of logs that the new DB instance is to export to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs > in the /Amazon RDS User Guide/ .
    enableCloudwatchLogsExports :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDBInstanceReadReplica' with the minimum fields required to make a request.
--
-- * 'deletionProtection' - A value that indicates whether the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> .
-- * 'publiclyAccessible' - A value that indicates whether the DB instance is publicly accessible.
--
-- When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it.
-- When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address.
-- For more information, see 'CreateDBInstance' .
-- * 'autoMinorVersionUpgrade' - A value that indicates whether minor engine upgrades are applied automatically to the read replica during the maintenance window.
--
-- Default: Inherits from the source DB instance
-- * 'dbSubnetGroupName' - Specifies a DB subnet group for the DB instance. The new DB instance is created in the VPC associated with the DB subnet group. If no DB subnet group is specified, then the new DB instance isn't created in a VPC.
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
-- * 'monitoringRoleARN' - The ARN for the IAM role that permits RDS to send enhanced monitoring metrics to Amazon CloudWatch Logs. For example, @arn:aws:iam:123456789012:role/emaccess@ . For information on creating a monitoring role, go to <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.html#USER_Monitoring.OS.IAMRole To create an IAM role for Amazon RDS Enhanced Monitoring> in the /Amazon RDS User Guide/ .
--
-- If @MonitoringInterval@ is set to a value other than 0, then you must supply a @MonitoringRoleArn@ value.
-- * 'iops' - The amount of Provisioned IOPS (input/output operations per second) to be initially allocated for the DB instance.
-- * 'domain' - The Active Directory directory ID to create the DB instance in. Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be created in an Active Directory Domain.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon RDS User Guide/ .
-- * 'replicaMode' - The open mode of the replica database: mounted or read-only.
--
-- Mounted DB replicas are included in Oracle Enterprise Edition. The main use case for mounted replicas is cross-Region disaster recovery. The primary database doesn't use Active Data Guard to transmit information to the mounted replica. Because it doesn't accept user connections, a mounted replica can't serve a read-only workload.
-- You can create a combination of mounted and read-only DB replicas for the same primary DB instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Working with Oracle Read Replicas for Amazon RDS> in the /Amazon RDS User Guide/ .
-- * 'monitoringInterval' - The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the read replica. To disable collecting Enhanced Monitoring metrics, specify 0. The default is 0.
--
-- If @MonitoringRoleArn@ is specified, then you must also set @MonitoringInterval@ to a value other than 0.
-- Valid Values: @0, 1, 5, 10, 15, 30, 60@
-- * 'preSignedURL' - The URL that contains a Signature Version 4 signed request for the @CreateDBInstanceReadReplica@ API action in the source AWS Region that contains the source DB instance.
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
-- * 'sourceDBInstanceIdentifier' - The identifier of the DB instance that will act as the source for the read replica. Each DB instance can have up to five read replicas.
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
-- * 'processorFeatures' - The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
-- * 'dbInstanceClass' - The compute and memory capacity of the read replica, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the /Amazon RDS User Guide./
--
-- Default: Inherits from the source DB instance.
-- * 'performanceInsightsRetentionPeriod' - The amount of time, in days, to retain Performance Insights data. Valid values are 7 or 731 (2 years).
-- * 'dbInstanceIdentifier' - The DB instance identifier of the read replica. This identifier is the unique key that identifies a DB instance. This parameter is stored as a lowercase string.
-- * 'maxAllocatedStorage' - The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
-- * 'enablePerformanceInsights' - A value that indicates whether to enable Performance Insights for the read replica.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights> in the /Amazon RDS User Guide/ .
-- * 'kmsKeyId' - The AWS KMS key ID for an encrypted read replica. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
--
-- If you create an encrypted read replica in the same AWS Region as the source DB instance, then you do not have to specify a value for this parameter. The read replica is encrypted with the same KMS key as the source DB instance.
-- If you create an encrypted read replica in a different AWS Region, then you must specify a KMS key for the destination AWS Region. KMS encryption keys are specific to the AWS Region that they are created in, and you can't use encryption keys from one AWS Region in another AWS Region.
-- You can't create an encrypted read replica from an unencrypted DB instance.
-- * 'dbParameterGroupName' - The name of the DB parameter group to associate with this DB instance.
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
-- * 'availabilityZone' - The Availability Zone (AZ) where the read replica will be created.
--
-- Default: A random, system-chosen Availability Zone in the endpoint's AWS Region.
-- Example: @us-east-1d@
-- * 'performanceInsightsKMSKeyId' - The AWS KMS key identifier for encryption of Performance Insights data. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
--
-- If you do not specify a value for @PerformanceInsightsKMSKeyId@ , then Amazon RDS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
-- * 'vpcSecurityGroupIds' - A list of EC2 VPC security groups to associate with the read replica.
--
-- Default: The default EC2 VPC security group for the DB subnet group's VPC.
-- * 'multiAZ' - A value that indicates whether the read replica is in a Multi-AZ deployment.
--
-- You can create a read replica as a Multi-AZ DB instance. RDS creates a standby of your replica in another Availability Zone for failover support for the replica. Creating your read replica as a Multi-AZ DB instance is independent of whether the source database is a Multi-AZ DB instance.
-- * 'optionGroupName' - The option group the DB instance is associated with. If omitted, the option group associated with the source instance is used.
-- * 'copyTagsToSnapshot' - A value that indicates whether to copy all tags from the read replica to snapshots of the read replica. By default, tags are not copied.
-- * 'domainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the Directory Service.
-- * 'tags' -
-- * 'port' - The port number that the DB instance uses for connections.
--
-- Default: Inherits from the source DB instance
-- Valid Values: @1150-65535@
-- * 'enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- For more information about IAM database authentication, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL> in the /Amazon RDS User Guide./
-- * 'useDefaultProcessorFeatures' - A value that indicates whether the DB instance class of the DB instance uses its default processor features.
-- * 'storageType' - Specifies the storage type to be associated with the read replica.
--
-- Valid values: @standard | gp2 | io1@
-- If you specify @io1@ , you must also include a value for the @Iops@ parameter.
-- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
-- * 'enableCloudwatchLogsExports' - The list of logs that the new DB instance is to export to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs > in the /Amazon RDS User Guide/ .
mkCreateDBInstanceReadReplica ::
  -- | 'sourceDBInstanceIdentifier'
  Lude.Text ->
  -- | 'dbInstanceIdentifier'
  Lude.Text ->
  CreateDBInstanceReadReplica
mkCreateDBInstanceReadReplica
  pSourceDBInstanceIdentifier_
  pDBInstanceIdentifier_ =
    CreateDBInstanceReadReplica'
      { deletionProtection = Lude.Nothing,
        publiclyAccessible = Lude.Nothing,
        autoMinorVersionUpgrade = Lude.Nothing,
        dbSubnetGroupName = Lude.Nothing,
        monitoringRoleARN = Lude.Nothing,
        iops = Lude.Nothing,
        domain = Lude.Nothing,
        replicaMode = Lude.Nothing,
        monitoringInterval = Lude.Nothing,
        preSignedURL = Lude.Nothing,
        sourceDBInstanceIdentifier = pSourceDBInstanceIdentifier_,
        processorFeatures = Lude.Nothing,
        dbInstanceClass = Lude.Nothing,
        performanceInsightsRetentionPeriod = Lude.Nothing,
        dbInstanceIdentifier = pDBInstanceIdentifier_,
        maxAllocatedStorage = Lude.Nothing,
        enablePerformanceInsights = Lude.Nothing,
        kmsKeyId = Lude.Nothing,
        dbParameterGroupName = Lude.Nothing,
        availabilityZone = Lude.Nothing,
        performanceInsightsKMSKeyId = Lude.Nothing,
        vpcSecurityGroupIds = Lude.Nothing,
        multiAZ = Lude.Nothing,
        optionGroupName = Lude.Nothing,
        copyTagsToSnapshot = Lude.Nothing,
        domainIAMRoleName = Lude.Nothing,
        tags = Lude.Nothing,
        port = Lude.Nothing,
        enableIAMDatabaseAuthentication = Lude.Nothing,
        useDefaultProcessorFeatures = Lude.Nothing,
        storageType = Lude.Nothing,
        enableCloudwatchLogsExports = Lude.Nothing
      }

-- | A value that indicates whether the DB instance has deletion protection enabled. The database can't be deleted when deletion protection is enabled. By default, deletion protection is disabled. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance> .
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdirrDeletionProtection :: Lens.Lens' CreateDBInstanceReadReplica (Lude.Maybe Lude.Bool)
cdirrDeletionProtection = Lens.lens (deletionProtection :: CreateDBInstanceReadReplica -> Lude.Maybe Lude.Bool) (\s a -> s {deletionProtection = a} :: CreateDBInstanceReadReplica)
{-# DEPRECATED cdirrDeletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead." #-}

-- | A value that indicates whether the DB instance is publicly accessible.
--
-- When the DB instance is publicly accessible, its DNS endpoint resolves to the private IP address from within the DB instance's VPC, and to the public IP address from outside of the DB instance's VPC. Access to the DB instance is ultimately controlled by the security group it uses, and that public access is not permitted if the security group assigned to the DB instance doesn't permit it.
-- When the DB instance isn't publicly accessible, it is an internal DB instance with a DNS name that resolves to a private IP address.
-- For more information, see 'CreateDBInstance' .
--
-- /Note:/ Consider using 'publiclyAccessible' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdirrPubliclyAccessible :: Lens.Lens' CreateDBInstanceReadReplica (Lude.Maybe Lude.Bool)
cdirrPubliclyAccessible = Lens.lens (publiclyAccessible :: CreateDBInstanceReadReplica -> Lude.Maybe Lude.Bool) (\s a -> s {publiclyAccessible = a} :: CreateDBInstanceReadReplica)
{-# DEPRECATED cdirrPubliclyAccessible "Use generic-lens or generic-optics with 'publiclyAccessible' instead." #-}

-- | A value that indicates whether minor engine upgrades are applied automatically to the read replica during the maintenance window.
--
-- Default: Inherits from the source DB instance
--
-- /Note:/ Consider using 'autoMinorVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdirrAutoMinorVersionUpgrade :: Lens.Lens' CreateDBInstanceReadReplica (Lude.Maybe Lude.Bool)
cdirrAutoMinorVersionUpgrade = Lens.lens (autoMinorVersionUpgrade :: CreateDBInstanceReadReplica -> Lude.Maybe Lude.Bool) (\s a -> s {autoMinorVersionUpgrade = a} :: CreateDBInstanceReadReplica)
{-# DEPRECATED cdirrAutoMinorVersionUpgrade "Use generic-lens or generic-optics with 'autoMinorVersionUpgrade' instead." #-}

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
-- /Note:/ Consider using 'dbSubnetGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdirrDBSubnetGroupName :: Lens.Lens' CreateDBInstanceReadReplica (Lude.Maybe Lude.Text)
cdirrDBSubnetGroupName = Lens.lens (dbSubnetGroupName :: CreateDBInstanceReadReplica -> Lude.Maybe Lude.Text) (\s a -> s {dbSubnetGroupName = a} :: CreateDBInstanceReadReplica)
{-# DEPRECATED cdirrDBSubnetGroupName "Use generic-lens or generic-optics with 'dbSubnetGroupName' instead." #-}

-- | The ARN for the IAM role that permits RDS to send enhanced monitoring metrics to Amazon CloudWatch Logs. For example, @arn:aws:iam:123456789012:role/emaccess@ . For information on creating a monitoring role, go to <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.html#USER_Monitoring.OS.IAMRole To create an IAM role for Amazon RDS Enhanced Monitoring> in the /Amazon RDS User Guide/ .
--
-- If @MonitoringInterval@ is set to a value other than 0, then you must supply a @MonitoringRoleArn@ value.
--
-- /Note:/ Consider using 'monitoringRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdirrMonitoringRoleARN :: Lens.Lens' CreateDBInstanceReadReplica (Lude.Maybe Lude.Text)
cdirrMonitoringRoleARN = Lens.lens (monitoringRoleARN :: CreateDBInstanceReadReplica -> Lude.Maybe Lude.Text) (\s a -> s {monitoringRoleARN = a} :: CreateDBInstanceReadReplica)
{-# DEPRECATED cdirrMonitoringRoleARN "Use generic-lens or generic-optics with 'monitoringRoleARN' instead." #-}

-- | The amount of Provisioned IOPS (input/output operations per second) to be initially allocated for the DB instance.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdirrIOPS :: Lens.Lens' CreateDBInstanceReadReplica (Lude.Maybe Lude.Int)
cdirrIOPS = Lens.lens (iops :: CreateDBInstanceReadReplica -> Lude.Maybe Lude.Int) (\s a -> s {iops = a} :: CreateDBInstanceReadReplica)
{-# DEPRECATED cdirrIOPS "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | The Active Directory directory ID to create the DB instance in. Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB instances can be created in an Active Directory Domain.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication> in the /Amazon RDS User Guide/ .
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdirrDomain :: Lens.Lens' CreateDBInstanceReadReplica (Lude.Maybe Lude.Text)
cdirrDomain = Lens.lens (domain :: CreateDBInstanceReadReplica -> Lude.Maybe Lude.Text) (\s a -> s {domain = a} :: CreateDBInstanceReadReplica)
{-# DEPRECATED cdirrDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | The open mode of the replica database: mounted or read-only.
--
-- Mounted DB replicas are included in Oracle Enterprise Edition. The main use case for mounted replicas is cross-Region disaster recovery. The primary database doesn't use Active Data Guard to transmit information to the mounted replica. Because it doesn't accept user connections, a mounted replica can't serve a read-only workload.
-- You can create a combination of mounted and read-only DB replicas for the same primary DB instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Working with Oracle Read Replicas for Amazon RDS> in the /Amazon RDS User Guide/ .
--
-- /Note:/ Consider using 'replicaMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdirrReplicaMode :: Lens.Lens' CreateDBInstanceReadReplica (Lude.Maybe ReplicaMode)
cdirrReplicaMode = Lens.lens (replicaMode :: CreateDBInstanceReadReplica -> Lude.Maybe ReplicaMode) (\s a -> s {replicaMode = a} :: CreateDBInstanceReadReplica)
{-# DEPRECATED cdirrReplicaMode "Use generic-lens or generic-optics with 'replicaMode' instead." #-}

-- | The interval, in seconds, between points when Enhanced Monitoring metrics are collected for the read replica. To disable collecting Enhanced Monitoring metrics, specify 0. The default is 0.
--
-- If @MonitoringRoleArn@ is specified, then you must also set @MonitoringInterval@ to a value other than 0.
-- Valid Values: @0, 1, 5, 10, 15, 30, 60@
--
-- /Note:/ Consider using 'monitoringInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdirrMonitoringInterval :: Lens.Lens' CreateDBInstanceReadReplica (Lude.Maybe Lude.Int)
cdirrMonitoringInterval = Lens.lens (monitoringInterval :: CreateDBInstanceReadReplica -> Lude.Maybe Lude.Int) (\s a -> s {monitoringInterval = a} :: CreateDBInstanceReadReplica)
{-# DEPRECATED cdirrMonitoringInterval "Use generic-lens or generic-optics with 'monitoringInterval' instead." #-}

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
-- /Note:/ Consider using 'preSignedURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdirrPreSignedURL :: Lens.Lens' CreateDBInstanceReadReplica (Lude.Maybe Lude.Text)
cdirrPreSignedURL = Lens.lens (preSignedURL :: CreateDBInstanceReadReplica -> Lude.Maybe Lude.Text) (\s a -> s {preSignedURL = a} :: CreateDBInstanceReadReplica)
{-# DEPRECATED cdirrPreSignedURL "Use generic-lens or generic-optics with 'preSignedURL' instead." #-}

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
cdirrSourceDBInstanceIdentifier :: Lens.Lens' CreateDBInstanceReadReplica Lude.Text
cdirrSourceDBInstanceIdentifier = Lens.lens (sourceDBInstanceIdentifier :: CreateDBInstanceReadReplica -> Lude.Text) (\s a -> s {sourceDBInstanceIdentifier = a} :: CreateDBInstanceReadReplica)
{-# DEPRECATED cdirrSourceDBInstanceIdentifier "Use generic-lens or generic-optics with 'sourceDBInstanceIdentifier' instead." #-}

-- | The number of CPU cores and the number of threads per core for the DB instance class of the DB instance.
--
-- /Note:/ Consider using 'processorFeatures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdirrProcessorFeatures :: Lens.Lens' CreateDBInstanceReadReplica (Lude.Maybe [ProcessorFeature])
cdirrProcessorFeatures = Lens.lens (processorFeatures :: CreateDBInstanceReadReplica -> Lude.Maybe [ProcessorFeature]) (\s a -> s {processorFeatures = a} :: CreateDBInstanceReadReplica)
{-# DEPRECATED cdirrProcessorFeatures "Use generic-lens or generic-optics with 'processorFeatures' instead." #-}

-- | The compute and memory capacity of the read replica, for example, @db.m4.large@ . Not all DB instance classes are available in all AWS Regions, or for all database engines. For the full list of DB instance classes, and availability for your engine, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class> in the /Amazon RDS User Guide./
--
-- Default: Inherits from the source DB instance.
--
-- /Note:/ Consider using 'dbInstanceClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdirrDBInstanceClass :: Lens.Lens' CreateDBInstanceReadReplica (Lude.Maybe Lude.Text)
cdirrDBInstanceClass = Lens.lens (dbInstanceClass :: CreateDBInstanceReadReplica -> Lude.Maybe Lude.Text) (\s a -> s {dbInstanceClass = a} :: CreateDBInstanceReadReplica)
{-# DEPRECATED cdirrDBInstanceClass "Use generic-lens or generic-optics with 'dbInstanceClass' instead." #-}

-- | The amount of time, in days, to retain Performance Insights data. Valid values are 7 or 731 (2 years).
--
-- /Note:/ Consider using 'performanceInsightsRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdirrPerformanceInsightsRetentionPeriod :: Lens.Lens' CreateDBInstanceReadReplica (Lude.Maybe Lude.Int)
cdirrPerformanceInsightsRetentionPeriod = Lens.lens (performanceInsightsRetentionPeriod :: CreateDBInstanceReadReplica -> Lude.Maybe Lude.Int) (\s a -> s {performanceInsightsRetentionPeriod = a} :: CreateDBInstanceReadReplica)
{-# DEPRECATED cdirrPerformanceInsightsRetentionPeriod "Use generic-lens or generic-optics with 'performanceInsightsRetentionPeriod' instead." #-}

-- | The DB instance identifier of the read replica. This identifier is the unique key that identifies a DB instance. This parameter is stored as a lowercase string.
--
-- /Note:/ Consider using 'dbInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdirrDBInstanceIdentifier :: Lens.Lens' CreateDBInstanceReadReplica Lude.Text
cdirrDBInstanceIdentifier = Lens.lens (dbInstanceIdentifier :: CreateDBInstanceReadReplica -> Lude.Text) (\s a -> s {dbInstanceIdentifier = a} :: CreateDBInstanceReadReplica)
{-# DEPRECATED cdirrDBInstanceIdentifier "Use generic-lens or generic-optics with 'dbInstanceIdentifier' instead." #-}

-- | The upper limit to which Amazon RDS can automatically scale the storage of the DB instance.
--
-- /Note:/ Consider using 'maxAllocatedStorage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdirrMaxAllocatedStorage :: Lens.Lens' CreateDBInstanceReadReplica (Lude.Maybe Lude.Int)
cdirrMaxAllocatedStorage = Lens.lens (maxAllocatedStorage :: CreateDBInstanceReadReplica -> Lude.Maybe Lude.Int) (\s a -> s {maxAllocatedStorage = a} :: CreateDBInstanceReadReplica)
{-# DEPRECATED cdirrMaxAllocatedStorage "Use generic-lens or generic-optics with 'maxAllocatedStorage' instead." #-}

-- | A value that indicates whether to enable Performance Insights for the read replica.
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights> in the /Amazon RDS User Guide/ .
--
-- /Note:/ Consider using 'enablePerformanceInsights' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdirrEnablePerformanceInsights :: Lens.Lens' CreateDBInstanceReadReplica (Lude.Maybe Lude.Bool)
cdirrEnablePerformanceInsights = Lens.lens (enablePerformanceInsights :: CreateDBInstanceReadReplica -> Lude.Maybe Lude.Bool) (\s a -> s {enablePerformanceInsights = a} :: CreateDBInstanceReadReplica)
{-# DEPRECATED cdirrEnablePerformanceInsights "Use generic-lens or generic-optics with 'enablePerformanceInsights' instead." #-}

-- | The AWS KMS key ID for an encrypted read replica. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
--
-- If you create an encrypted read replica in the same AWS Region as the source DB instance, then you do not have to specify a value for this parameter. The read replica is encrypted with the same KMS key as the source DB instance.
-- If you create an encrypted read replica in a different AWS Region, then you must specify a KMS key for the destination AWS Region. KMS encryption keys are specific to the AWS Region that they are created in, and you can't use encryption keys from one AWS Region in another AWS Region.
-- You can't create an encrypted read replica from an unencrypted DB instance.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdirrKMSKeyId :: Lens.Lens' CreateDBInstanceReadReplica (Lude.Maybe Lude.Text)
cdirrKMSKeyId = Lens.lens (kmsKeyId :: CreateDBInstanceReadReplica -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: CreateDBInstanceReadReplica)
{-# DEPRECATED cdirrKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

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
-- /Note:/ Consider using 'dbParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdirrDBParameterGroupName :: Lens.Lens' CreateDBInstanceReadReplica (Lude.Maybe Lude.Text)
cdirrDBParameterGroupName = Lens.lens (dbParameterGroupName :: CreateDBInstanceReadReplica -> Lude.Maybe Lude.Text) (\s a -> s {dbParameterGroupName = a} :: CreateDBInstanceReadReplica)
{-# DEPRECATED cdirrDBParameterGroupName "Use generic-lens or generic-optics with 'dbParameterGroupName' instead." #-}

-- | The Availability Zone (AZ) where the read replica will be created.
--
-- Default: A random, system-chosen Availability Zone in the endpoint's AWS Region.
-- Example: @us-east-1d@
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdirrAvailabilityZone :: Lens.Lens' CreateDBInstanceReadReplica (Lude.Maybe Lude.Text)
cdirrAvailabilityZone = Lens.lens (availabilityZone :: CreateDBInstanceReadReplica -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: CreateDBInstanceReadReplica)
{-# DEPRECATED cdirrAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The AWS KMS key identifier for encryption of Performance Insights data. The KMS key ID is the Amazon Resource Name (ARN), KMS key identifier, or the KMS key alias for the KMS encryption key.
--
-- If you do not specify a value for @PerformanceInsightsKMSKeyId@ , then Amazon RDS uses your default encryption key. AWS KMS creates the default encryption key for your AWS account. Your AWS account has a different default encryption key for each AWS Region.
--
-- /Note:/ Consider using 'performanceInsightsKMSKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdirrPerformanceInsightsKMSKeyId :: Lens.Lens' CreateDBInstanceReadReplica (Lude.Maybe Lude.Text)
cdirrPerformanceInsightsKMSKeyId = Lens.lens (performanceInsightsKMSKeyId :: CreateDBInstanceReadReplica -> Lude.Maybe Lude.Text) (\s a -> s {performanceInsightsKMSKeyId = a} :: CreateDBInstanceReadReplica)
{-# DEPRECATED cdirrPerformanceInsightsKMSKeyId "Use generic-lens or generic-optics with 'performanceInsightsKMSKeyId' instead." #-}

-- | A list of EC2 VPC security groups to associate with the read replica.
--
-- Default: The default EC2 VPC security group for the DB subnet group's VPC.
--
-- /Note:/ Consider using 'vpcSecurityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdirrVPCSecurityGroupIds :: Lens.Lens' CreateDBInstanceReadReplica (Lude.Maybe [Lude.Text])
cdirrVPCSecurityGroupIds = Lens.lens (vpcSecurityGroupIds :: CreateDBInstanceReadReplica -> Lude.Maybe [Lude.Text]) (\s a -> s {vpcSecurityGroupIds = a} :: CreateDBInstanceReadReplica)
{-# DEPRECATED cdirrVPCSecurityGroupIds "Use generic-lens or generic-optics with 'vpcSecurityGroupIds' instead." #-}

-- | A value that indicates whether the read replica is in a Multi-AZ deployment.
--
-- You can create a read replica as a Multi-AZ DB instance. RDS creates a standby of your replica in another Availability Zone for failover support for the replica. Creating your read replica as a Multi-AZ DB instance is independent of whether the source database is a Multi-AZ DB instance.
--
-- /Note:/ Consider using 'multiAZ' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdirrMultiAZ :: Lens.Lens' CreateDBInstanceReadReplica (Lude.Maybe Lude.Bool)
cdirrMultiAZ = Lens.lens (multiAZ :: CreateDBInstanceReadReplica -> Lude.Maybe Lude.Bool) (\s a -> s {multiAZ = a} :: CreateDBInstanceReadReplica)
{-# DEPRECATED cdirrMultiAZ "Use generic-lens or generic-optics with 'multiAZ' instead." #-}

-- | The option group the DB instance is associated with. If omitted, the option group associated with the source instance is used.
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdirrOptionGroupName :: Lens.Lens' CreateDBInstanceReadReplica (Lude.Maybe Lude.Text)
cdirrOptionGroupName = Lens.lens (optionGroupName :: CreateDBInstanceReadReplica -> Lude.Maybe Lude.Text) (\s a -> s {optionGroupName = a} :: CreateDBInstanceReadReplica)
{-# DEPRECATED cdirrOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

-- | A value that indicates whether to copy all tags from the read replica to snapshots of the read replica. By default, tags are not copied.
--
-- /Note:/ Consider using 'copyTagsToSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdirrCopyTagsToSnapshot :: Lens.Lens' CreateDBInstanceReadReplica (Lude.Maybe Lude.Bool)
cdirrCopyTagsToSnapshot = Lens.lens (copyTagsToSnapshot :: CreateDBInstanceReadReplica -> Lude.Maybe Lude.Bool) (\s a -> s {copyTagsToSnapshot = a} :: CreateDBInstanceReadReplica)
{-# DEPRECATED cdirrCopyTagsToSnapshot "Use generic-lens or generic-optics with 'copyTagsToSnapshot' instead." #-}

-- | Specify the name of the IAM role to be used when making API calls to the Directory Service.
--
-- /Note:/ Consider using 'domainIAMRoleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdirrDomainIAMRoleName :: Lens.Lens' CreateDBInstanceReadReplica (Lude.Maybe Lude.Text)
cdirrDomainIAMRoleName = Lens.lens (domainIAMRoleName :: CreateDBInstanceReadReplica -> Lude.Maybe Lude.Text) (\s a -> s {domainIAMRoleName = a} :: CreateDBInstanceReadReplica)
{-# DEPRECATED cdirrDomainIAMRoleName "Use generic-lens or generic-optics with 'domainIAMRoleName' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdirrTags :: Lens.Lens' CreateDBInstanceReadReplica (Lude.Maybe [Tag])
cdirrTags = Lens.lens (tags :: CreateDBInstanceReadReplica -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateDBInstanceReadReplica)
{-# DEPRECATED cdirrTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The port number that the DB instance uses for connections.
--
-- Default: Inherits from the source DB instance
-- Valid Values: @1150-65535@
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdirrPort :: Lens.Lens' CreateDBInstanceReadReplica (Lude.Maybe Lude.Int)
cdirrPort = Lens.lens (port :: CreateDBInstanceReadReplica -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: CreateDBInstanceReadReplica)
{-# DEPRECATED cdirrPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | A value that indicates whether to enable mapping of AWS Identity and Access Management (IAM) accounts to database accounts. By default, mapping is disabled.
--
-- For more information about IAM database authentication, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL> in the /Amazon RDS User Guide./
--
-- /Note:/ Consider using 'enableIAMDatabaseAuthentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdirrEnableIAMDatabaseAuthentication :: Lens.Lens' CreateDBInstanceReadReplica (Lude.Maybe Lude.Bool)
cdirrEnableIAMDatabaseAuthentication = Lens.lens (enableIAMDatabaseAuthentication :: CreateDBInstanceReadReplica -> Lude.Maybe Lude.Bool) (\s a -> s {enableIAMDatabaseAuthentication = a} :: CreateDBInstanceReadReplica)
{-# DEPRECATED cdirrEnableIAMDatabaseAuthentication "Use generic-lens or generic-optics with 'enableIAMDatabaseAuthentication' instead." #-}

-- | A value that indicates whether the DB instance class of the DB instance uses its default processor features.
--
-- /Note:/ Consider using 'useDefaultProcessorFeatures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdirrUseDefaultProcessorFeatures :: Lens.Lens' CreateDBInstanceReadReplica (Lude.Maybe Lude.Bool)
cdirrUseDefaultProcessorFeatures = Lens.lens (useDefaultProcessorFeatures :: CreateDBInstanceReadReplica -> Lude.Maybe Lude.Bool) (\s a -> s {useDefaultProcessorFeatures = a} :: CreateDBInstanceReadReplica)
{-# DEPRECATED cdirrUseDefaultProcessorFeatures "Use generic-lens or generic-optics with 'useDefaultProcessorFeatures' instead." #-}

-- | Specifies the storage type to be associated with the read replica.
--
-- Valid values: @standard | gp2 | io1@
-- If you specify @io1@ , you must also include a value for the @Iops@ parameter.
-- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
--
-- /Note:/ Consider using 'storageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdirrStorageType :: Lens.Lens' CreateDBInstanceReadReplica (Lude.Maybe Lude.Text)
cdirrStorageType = Lens.lens (storageType :: CreateDBInstanceReadReplica -> Lude.Maybe Lude.Text) (\s a -> s {storageType = a} :: CreateDBInstanceReadReplica)
{-# DEPRECATED cdirrStorageType "Use generic-lens or generic-optics with 'storageType' instead." #-}

-- | The list of logs that the new DB instance is to export to CloudWatch Logs. The values in the list depend on the DB engine being used. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs > in the /Amazon RDS User Guide/ .
--
-- /Note:/ Consider using 'enableCloudwatchLogsExports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdirrEnableCloudwatchLogsExports :: Lens.Lens' CreateDBInstanceReadReplica (Lude.Maybe [Lude.Text])
cdirrEnableCloudwatchLogsExports = Lens.lens (enableCloudwatchLogsExports :: CreateDBInstanceReadReplica -> Lude.Maybe [Lude.Text]) (\s a -> s {enableCloudwatchLogsExports = a} :: CreateDBInstanceReadReplica)
{-# DEPRECATED cdirrEnableCloudwatchLogsExports "Use generic-lens or generic-optics with 'enableCloudwatchLogsExports' instead." #-}

instance Lude.AWSRequest CreateDBInstanceReadReplica where
  type
    Rs CreateDBInstanceReadReplica =
      CreateDBInstanceReadReplicaResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "CreateDBInstanceReadReplicaResult"
      ( \s h x ->
          CreateDBInstanceReadReplicaResponse'
            Lude.<$> (x Lude..@? "DBInstance") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDBInstanceReadReplica where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateDBInstanceReadReplica where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDBInstanceReadReplica where
  toQuery CreateDBInstanceReadReplica' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("CreateDBInstanceReadReplica" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DeletionProtection" Lude.=: deletionProtection,
        "PubliclyAccessible" Lude.=: publiclyAccessible,
        "AutoMinorVersionUpgrade" Lude.=: autoMinorVersionUpgrade,
        "DBSubnetGroupName" Lude.=: dbSubnetGroupName,
        "MonitoringRoleArn" Lude.=: monitoringRoleARN,
        "Iops" Lude.=: iops,
        "Domain" Lude.=: domain,
        "ReplicaMode" Lude.=: replicaMode,
        "MonitoringInterval" Lude.=: monitoringInterval,
        "PreSignedUrl" Lude.=: preSignedURL,
        "SourceDBInstanceIdentifier" Lude.=: sourceDBInstanceIdentifier,
        "ProcessorFeatures"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "ProcessorFeature" Lude.<$> processorFeatures),
        "DBInstanceClass" Lude.=: dbInstanceClass,
        "PerformanceInsightsRetentionPeriod"
          Lude.=: performanceInsightsRetentionPeriod,
        "DBInstanceIdentifier" Lude.=: dbInstanceIdentifier,
        "MaxAllocatedStorage" Lude.=: maxAllocatedStorage,
        "EnablePerformanceInsights" Lude.=: enablePerformanceInsights,
        "KmsKeyId" Lude.=: kmsKeyId,
        "DBParameterGroupName" Lude.=: dbParameterGroupName,
        "AvailabilityZone" Lude.=: availabilityZone,
        "PerformanceInsightsKMSKeyId" Lude.=: performanceInsightsKMSKeyId,
        "VpcSecurityGroupIds"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "VpcSecurityGroupId"
                Lude.<$> vpcSecurityGroupIds
            ),
        "MultiAZ" Lude.=: multiAZ,
        "OptionGroupName" Lude.=: optionGroupName,
        "CopyTagsToSnapshot" Lude.=: copyTagsToSnapshot,
        "DomainIAMRoleName" Lude.=: domainIAMRoleName,
        "Tags" Lude.=: Lude.toQuery (Lude.toQueryList "Tag" Lude.<$> tags),
        "Port" Lude.=: port,
        "EnableIAMDatabaseAuthentication"
          Lude.=: enableIAMDatabaseAuthentication,
        "UseDefaultProcessorFeatures" Lude.=: useDefaultProcessorFeatures,
        "StorageType" Lude.=: storageType,
        "EnableCloudwatchLogsExports"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> enableCloudwatchLogsExports)
      ]

-- | /See:/ 'mkCreateDBInstanceReadReplicaResponse' smart constructor.
data CreateDBInstanceReadReplicaResponse = CreateDBInstanceReadReplicaResponse'
  { dbInstance :: Lude.Maybe DBInstance,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDBInstanceReadReplicaResponse' with the minimum fields required to make a request.
--
-- * 'dbInstance' -
-- * 'responseStatus' - The response status code.
mkCreateDBInstanceReadReplicaResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDBInstanceReadReplicaResponse
mkCreateDBInstanceReadReplicaResponse pResponseStatus_ =
  CreateDBInstanceReadReplicaResponse'
    { dbInstance = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dbInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdirrrsDBInstance :: Lens.Lens' CreateDBInstanceReadReplicaResponse (Lude.Maybe DBInstance)
cdirrrsDBInstance = Lens.lens (dbInstance :: CreateDBInstanceReadReplicaResponse -> Lude.Maybe DBInstance) (\s a -> s {dbInstance = a} :: CreateDBInstanceReadReplicaResponse)
{-# DEPRECATED cdirrrsDBInstance "Use generic-lens or generic-optics with 'dbInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdirrrsResponseStatus :: Lens.Lens' CreateDBInstanceReadReplicaResponse Lude.Int
cdirrrsResponseStatus = Lens.lens (responseStatus :: CreateDBInstanceReadReplicaResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDBInstanceReadReplicaResponse)
{-# DEPRECATED cdirrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
