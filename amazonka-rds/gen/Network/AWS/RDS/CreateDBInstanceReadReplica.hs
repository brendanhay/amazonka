{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateDBInstanceReadReplica
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB instance that acts as a read replica for an existing
-- source DB instance. You can create a read replica for a DB instance
-- running MySQL, MariaDB, Oracle, PostgreSQL, or SQL Server. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_ReadRepl.html Working with Read Replicas>
-- in the /Amazon RDS User Guide/.
--
-- Amazon Aurora doesn\'t support this action. Call the @CreateDBInstance@
-- action to create a DB instance for an Aurora DB cluster.
--
-- All read replica DB instances are created with backups disabled. All
-- other DB instance attributes (including DB security groups and DB
-- parameter groups) are inherited from the source DB instance, except as
-- specified.
--
-- Your source DB instance must have backup retention enabled.
module Network.AWS.RDS.CreateDBInstanceReadReplica
  ( -- * Creating a Request
    CreateDBInstanceReadReplica (..),
    newCreateDBInstanceReadReplica,

    -- * Request Lenses
    createDBInstanceReadReplica_deletionProtection,
    createDBInstanceReadReplica_enablePerformanceInsights,
    createDBInstanceReadReplica_maxAllocatedStorage,
    createDBInstanceReadReplica_enableIAMDatabaseAuthentication,
    createDBInstanceReadReplica_enableCloudwatchLogsExports,
    createDBInstanceReadReplica_storageType,
    createDBInstanceReadReplica_useDefaultProcessorFeatures,
    createDBInstanceReadReplica_monitoringInterval,
    createDBInstanceReadReplica_optionGroupName,
    createDBInstanceReadReplica_domain,
    createDBInstanceReadReplica_monitoringRoleArn,
    createDBInstanceReadReplica_dbSubnetGroupName,
    createDBInstanceReadReplica_multiAZ,
    createDBInstanceReadReplica_publiclyAccessible,
    createDBInstanceReadReplica_vpcSecurityGroupIds,
    createDBInstanceReadReplica_performanceInsightsKMSKeyId,
    createDBInstanceReadReplica_kmsKeyId,
    createDBInstanceReadReplica_dbParameterGroupName,
    createDBInstanceReadReplica_availabilityZone,
    createDBInstanceReadReplica_performanceInsightsRetentionPeriod,
    createDBInstanceReadReplica_tags,
    createDBInstanceReadReplica_processorFeatures,
    createDBInstanceReadReplica_port,
    createDBInstanceReadReplica_dbInstanceClass,
    createDBInstanceReadReplica_domainIAMRoleName,
    createDBInstanceReadReplica_preSignedUrl,
    createDBInstanceReadReplica_copyTagsToSnapshot,
    createDBInstanceReadReplica_replicaMode,
    createDBInstanceReadReplica_iops,
    createDBInstanceReadReplica_autoMinorVersionUpgrade,
    createDBInstanceReadReplica_dbInstanceIdentifier,
    createDBInstanceReadReplica_sourceDBInstanceIdentifier,

    -- * Destructuring the Response
    CreateDBInstanceReadReplicaResponse (..),
    newCreateDBInstanceReadReplicaResponse,

    -- * Response Lenses
    createDBInstanceReadReplicaResponse_dbInstance,
    createDBInstanceReadReplicaResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDBInstanceReadReplica' smart constructor.
data CreateDBInstanceReadReplica = CreateDBInstanceReadReplica'
  { -- | A value that indicates whether the DB instance has deletion protection
    -- enabled. The database can\'t be deleted when deletion protection is
    -- enabled. By default, deletion protection is disabled. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether to enable Performance Insights for the
    -- read replica.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights>
    -- in the /Amazon RDS User Guide/.
    enablePerformanceInsights :: Prelude.Maybe Prelude.Bool,
    -- | The upper limit to which Amazon RDS can automatically scale the storage
    -- of the DB instance.
    --
    -- For more information about this setting, including limitations that
    -- apply to it, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
    -- in the /Amazon RDS User Guide/.
    maxAllocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | A value that indicates whether to enable mapping of AWS Identity and
    -- Access Management (IAM) accounts to database accounts. By default,
    -- mapping is disabled.
    --
    -- For more information about IAM database authentication, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
    -- in the /Amazon RDS User Guide./
    enableIAMDatabaseAuthentication :: Prelude.Maybe Prelude.Bool,
    -- | The list of logs that the new DB instance is to export to CloudWatch
    -- Logs. The values in the list depend on the DB engine being used. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
    -- in the /Amazon RDS User Guide/.
    enableCloudwatchLogsExports :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the storage type to be associated with the read replica.
    --
    -- Valid values: @standard | gp2 | io1@
    --
    -- If you specify @io1@, you must also include a value for the @Iops@
    -- parameter.
    --
    -- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
    storageType :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the DB instance class of the DB instance
    -- uses its default processor features.
    useDefaultProcessorFeatures :: Prelude.Maybe Prelude.Bool,
    -- | The interval, in seconds, between points when Enhanced Monitoring
    -- metrics are collected for the read replica. To disable collecting
    -- Enhanced Monitoring metrics, specify 0. The default is 0.
    --
    -- If @MonitoringRoleArn@ is specified, then you must also set
    -- @MonitoringInterval@ to a value other than 0.
    --
    -- Valid Values: @0, 1, 5, 10, 15, 30, 60@
    monitoringInterval :: Prelude.Maybe Prelude.Int,
    -- | The option group the DB instance is associated with. If omitted, the
    -- option group associated with the source instance is used.
    --
    -- For SQL Server, you must use the option group associated with the source
    -- instance.
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | The Active Directory directory ID to create the DB instance in.
    -- Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB
    -- instances can be created in an Active Directory Domain.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
    -- in the /Amazon RDS User Guide/.
    domain :: Prelude.Maybe Prelude.Text,
    -- | The ARN for the IAM role that permits RDS to send enhanced monitoring
    -- metrics to Amazon CloudWatch Logs. For example,
    -- @arn:aws:iam:123456789012:role\/emaccess@. For information on creating a
    -- monitoring role, go to
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.html#USER_Monitoring.OS.IAMRole To create an IAM role for Amazon RDS Enhanced Monitoring>
    -- in the /Amazon RDS User Guide/.
    --
    -- If @MonitoringInterval@ is set to a value other than 0, then you must
    -- supply a @MonitoringRoleArn@ value.
    monitoringRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies a DB subnet group for the DB instance. The new DB instance is
    -- created in the VPC associated with the DB subnet group. If no DB subnet
    -- group is specified, then the new DB instance isn\'t created in a VPC.
    --
    -- Constraints:
    --
    -- -   Can only be specified if the source DB instance identifier specifies
    --     a DB instance in another AWS Region.
    --
    -- -   If supplied, must match the name of an existing DBSubnetGroup.
    --
    -- -   The specified DB subnet group must be in the same AWS Region in
    --     which the operation is running.
    --
    -- -   All read replicas in one AWS Region that are created from the same
    --     source DB instance must either:>
    --
    --     -   Specify DB subnet groups from the same VPC. All these read
    --         replicas are created in the same VPC.
    --
    --     -   Not specify a DB subnet group. All these read replicas are
    --         created outside of any VPC.
    --
    -- Example: @mySubnetgroup@
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the read replica is in a Multi-AZ
    -- deployment.
    --
    -- You can create a read replica as a Multi-AZ DB instance. RDS creates a
    -- standby of your replica in another Availability Zone for failover
    -- support for the replica. Creating your read replica as a Multi-AZ DB
    -- instance is independent of whether the source database is a Multi-AZ DB
    -- instance.
    multiAZ :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether the DB instance is publicly accessible.
    --
    -- When the DB instance is publicly accessible, its DNS endpoint resolves
    -- to the private IP address from within the DB instance\'s VPC, and to the
    -- public IP address from outside of the DB instance\'s VPC. Access to the
    -- DB instance is ultimately controlled by the security group it uses, and
    -- that public access is not permitted if the security group assigned to
    -- the DB instance doesn\'t permit it.
    --
    -- When the DB instance isn\'t publicly accessible, it is an internal DB
    -- instance with a DNS name that resolves to a private IP address.
    --
    -- For more information, see CreateDBInstance.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | A list of EC2 VPC security groups to associate with the read replica.
    --
    -- Default: The default EC2 VPC security group for the DB subnet group\'s
    -- VPC.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The AWS KMS key identifier for encryption of Performance Insights data.
    --
    -- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
    -- name for the AWS KMS customer master key (CMK).
    --
    -- If you do not specify a value for @PerformanceInsightsKMSKeyId@, then
    -- Amazon RDS uses your default CMK. There is a default CMK for your AWS
    -- account. Your AWS account has a different default CMK for each AWS
    -- Region.
    performanceInsightsKMSKeyId :: Prelude.Maybe Prelude.Text,
    -- | The AWS KMS key identifier for an encrypted read replica.
    --
    -- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
    -- name for the AWS KMS CMK.
    --
    -- If you create an encrypted read replica in the same AWS Region as the
    -- source DB instance, then do not specify a value for this parameter. A
    -- read replica in the same Region is always encrypted with the same AWS
    -- KMS CMK as the source DB instance.
    --
    -- If you create an encrypted read replica in a different AWS Region, then
    -- you must specify a AWS KMS key identifier for the destination AWS
    -- Region. AWS KMS CMKs are specific to the AWS Region that they are
    -- created in, and you can\'t use CMKs from one AWS Region in another AWS
    -- Region.
    --
    -- You can\'t create an encrypted read replica from an unencrypted DB
    -- instance.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The name of the DB parameter group to associate with this DB instance.
    --
    -- If you do not specify a value for @DBParameterGroupName@, then Amazon
    -- RDS uses the @DBParameterGroup@ of source DB instance for a same region
    -- read replica, or the default @DBParameterGroup@ for the specified DB
    -- engine for a cross region read replica.
    --
    -- Currently, specifying a parameter group for this operation is only
    -- supported for Oracle DB instances.
    --
    -- Constraints:
    --
    -- -   Must be 1 to 255 letters, numbers, or hyphens.
    --
    -- -   First character must be a letter
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens
    dbParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone (AZ) where the read replica will be created.
    --
    -- Default: A random, system-chosen Availability Zone in the endpoint\'s
    -- AWS Region.
    --
    -- Example: @us-east-1d@
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The amount of time, in days, to retain Performance Insights data. Valid
    -- values are 7 or 731 (2 years).
    performanceInsightsRetentionPeriod :: Prelude.Maybe Prelude.Int,
    tags :: Prelude.Maybe [Tag],
    -- | The number of CPU cores and the number of threads per core for the DB
    -- instance class of the DB instance.
    processorFeatures :: Prelude.Maybe [ProcessorFeature],
    -- | The port number that the DB instance uses for connections.
    --
    -- Default: Inherits from the source DB instance
    --
    -- Valid Values: @1150-65535@
    port :: Prelude.Maybe Prelude.Int,
    -- | The compute and memory capacity of the read replica, for example,
    -- @db.m4.large@. Not all DB instance classes are available in all AWS
    -- Regions, or for all database engines. For the full list of DB instance
    -- classes, and availability for your engine, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
    -- in the /Amazon RDS User Guide./
    --
    -- Default: Inherits from the source DB instance.
    dbInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | Specify the name of the IAM role to be used when making API calls to the
    -- Directory Service.
    domainIAMRoleName :: Prelude.Maybe Prelude.Text,
    -- | The URL that contains a Signature Version 4 signed request for the
    -- @CreateDBInstanceReadReplica@ API action in the source AWS Region that
    -- contains the source DB instance.
    --
    -- You must specify this parameter when you create an encrypted read
    -- replica from another AWS Region by using the Amazon RDS API. Don\'t
    -- specify @PreSignedUrl@ when you are creating an encrypted read replica
    -- in the same AWS Region.
    --
    -- The presigned URL must be a valid request for the
    -- @CreateDBInstanceReadReplica@ API action that can be executed in the
    -- source AWS Region that contains the encrypted source DB instance. The
    -- presigned URL request must contain the following parameter values:
    --
    -- -   @DestinationRegion@ - The AWS Region that the encrypted read replica
    --     is created in. This AWS Region is the same one where the
    --     @CreateDBInstanceReadReplica@ action is called that contains this
    --     presigned URL.
    --
    --     For example, if you create an encrypted DB instance in the us-west-1
    --     AWS Region, from a source DB instance in the us-east-2 AWS Region,
    --     then you call the @CreateDBInstanceReadReplica@ action in the
    --     us-east-1 AWS Region and provide a presigned URL that contains a
    --     call to the @CreateDBInstanceReadReplica@ action in the us-west-2
    --     AWS Region. For this example, the @DestinationRegion@ in the
    --     presigned URL must be set to the us-east-1 AWS Region.
    --
    -- -   @KmsKeyId@ - The AWS KMS key identifier for the key to use to
    --     encrypt the read replica in the destination AWS Region. This is the
    --     same identifier for both the @CreateDBInstanceReadReplica@ action
    --     that is called in the destination AWS Region, and the action
    --     contained in the presigned URL.
    --
    -- -   @SourceDBInstanceIdentifier@ - The DB instance identifier for the
    --     encrypted DB instance to be replicated. This identifier must be in
    --     the Amazon Resource Name (ARN) format for the source AWS Region. For
    --     example, if you are creating an encrypted read replica from a DB
    --     instance in the us-west-2 AWS Region, then your
    --     @SourceDBInstanceIdentifier@ looks like the following example:
    --     @arn:aws:rds:us-west-2:123456789012:instance:mysql-instance1-20161115@.
    --
    -- To learn how to generate a Signature Version 4 signed request, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (AWS Signature Version 4)>
    -- and
    -- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process>.
    --
    -- If you are using an AWS SDK tool or the AWS CLI, you can specify
    -- @SourceRegion@ (or @--source-region@ for the AWS CLI) instead of
    -- specifying @PreSignedUrl@ manually. Specifying @SourceRegion@
    -- autogenerates a presigned URL that is a valid request for the operation
    -- that can be executed in the source AWS Region.
    --
    -- @SourceRegion@ isn\'t supported for SQL Server, because SQL Server on
    -- Amazon RDS doesn\'t support cross-region read replicas.
    preSignedUrl :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to copy all tags from the read replica to
    -- snapshots of the read replica. By default, tags are not copied.
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | The open mode of the replica database: mounted or read-only.
    --
    -- This parameter is only supported for Oracle DB instances.
    --
    -- Mounted DB replicas are included in Oracle Enterprise Edition. The main
    -- use case for mounted replicas is cross-Region disaster recovery. The
    -- primary database doesn\'t use Active Data Guard to transmit information
    -- to the mounted replica. Because it doesn\'t accept user connections, a
    -- mounted replica can\'t serve a read-only workload.
    --
    -- You can create a combination of mounted and read-only DB replicas for
    -- the same primary DB instance. For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Working with Oracle Read Replicas for Amazon RDS>
    -- in the /Amazon RDS User Guide/.
    replicaMode :: Prelude.Maybe ReplicaMode,
    -- | The amount of Provisioned IOPS (input\/output operations per second) to
    -- be initially allocated for the DB instance.
    iops :: Prelude.Maybe Prelude.Int,
    -- | A value that indicates whether minor engine upgrades are applied
    -- automatically to the read replica during the maintenance window.
    --
    -- Default: Inherits from the source DB instance
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The DB instance identifier of the read replica. This identifier is the
    -- unique key that identifies a DB instance. This parameter is stored as a
    -- lowercase string.
    dbInstanceIdentifier :: Prelude.Text,
    -- | The identifier of the DB instance that will act as the source for the
    -- read replica. Each DB instance can have up to five read replicas.
    --
    -- Constraints:
    --
    -- -   Must be the identifier of an existing MySQL, MariaDB, Oracle,
    --     PostgreSQL, or SQL Server DB instance.
    --
    -- -   Can specify a DB instance that is a MySQL read replica only if the
    --     source is running MySQL 5.6 or later.
    --
    -- -   For the limitations of Oracle read replicas, see
    --     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Read Replica Limitations with Oracle>
    --     in the /Amazon RDS User Guide/.
    --
    -- -   For the limitations of SQL Server read replicas, see
    --     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/SQLServer.ReadReplicas.Limitations.html Read Replica Limitations with Microsoft SQL Server>
    --     in the /Amazon RDS User Guide/.
    --
    -- -   Can specify a PostgreSQL DB instance only if the source is running
    --     PostgreSQL 9.3.5 or later (9.4.7 and higher for cross-region
    --     replication).
    --
    -- -   The specified DB instance must have automatic backups enabled, that
    --     is, its backup retention period must be greater than 0.
    --
    -- -   If the source DB instance is in the same AWS Region as the read
    --     replica, specify a valid DB instance identifier.
    --
    -- -   If the source DB instance is in a different AWS Region from the read
    --     replica, specify a valid DB instance ARN. For more information, see
    --     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS>
    --     in the /Amazon RDS User Guide/. This doesn\'t apply to SQL Server,
    --     which doesn\'t support cross-region replicas.
    sourceDBInstanceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateDBInstanceReadReplica' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deletionProtection', 'createDBInstanceReadReplica_deletionProtection' - A value that indicates whether the DB instance has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
--
-- 'enablePerformanceInsights', 'createDBInstanceReadReplica_enablePerformanceInsights' - A value that indicates whether to enable Performance Insights for the
-- read replica.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights>
-- in the /Amazon RDS User Guide/.
--
-- 'maxAllocatedStorage', 'createDBInstanceReadReplica_maxAllocatedStorage' - The upper limit to which Amazon RDS can automatically scale the storage
-- of the DB instance.
--
-- For more information about this setting, including limitations that
-- apply to it, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
-- in the /Amazon RDS User Guide/.
--
-- 'enableIAMDatabaseAuthentication', 'createDBInstanceReadReplica_enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of AWS Identity and
-- Access Management (IAM) accounts to database accounts. By default,
-- mapping is disabled.
--
-- For more information about IAM database authentication, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
-- in the /Amazon RDS User Guide./
--
-- 'enableCloudwatchLogsExports', 'createDBInstanceReadReplica_enableCloudwatchLogsExports' - The list of logs that the new DB instance is to export to CloudWatch
-- Logs. The values in the list depend on the DB engine being used. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon RDS User Guide/.
--
-- 'storageType', 'createDBInstanceReadReplica_storageType' - Specifies the storage type to be associated with the read replica.
--
-- Valid values: @standard | gp2 | io1@
--
-- If you specify @io1@, you must also include a value for the @Iops@
-- parameter.
--
-- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
--
-- 'useDefaultProcessorFeatures', 'createDBInstanceReadReplica_useDefaultProcessorFeatures' - A value that indicates whether the DB instance class of the DB instance
-- uses its default processor features.
--
-- 'monitoringInterval', 'createDBInstanceReadReplica_monitoringInterval' - The interval, in seconds, between points when Enhanced Monitoring
-- metrics are collected for the read replica. To disable collecting
-- Enhanced Monitoring metrics, specify 0. The default is 0.
--
-- If @MonitoringRoleArn@ is specified, then you must also set
-- @MonitoringInterval@ to a value other than 0.
--
-- Valid Values: @0, 1, 5, 10, 15, 30, 60@
--
-- 'optionGroupName', 'createDBInstanceReadReplica_optionGroupName' - The option group the DB instance is associated with. If omitted, the
-- option group associated with the source instance is used.
--
-- For SQL Server, you must use the option group associated with the source
-- instance.
--
-- 'domain', 'createDBInstanceReadReplica_domain' - The Active Directory directory ID to create the DB instance in.
-- Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB
-- instances can be created in an Active Directory Domain.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon RDS User Guide/.
--
-- 'monitoringRoleArn', 'createDBInstanceReadReplica_monitoringRoleArn' - The ARN for the IAM role that permits RDS to send enhanced monitoring
-- metrics to Amazon CloudWatch Logs. For example,
-- @arn:aws:iam:123456789012:role\/emaccess@. For information on creating a
-- monitoring role, go to
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.html#USER_Monitoring.OS.IAMRole To create an IAM role for Amazon RDS Enhanced Monitoring>
-- in the /Amazon RDS User Guide/.
--
-- If @MonitoringInterval@ is set to a value other than 0, then you must
-- supply a @MonitoringRoleArn@ value.
--
-- 'dbSubnetGroupName', 'createDBInstanceReadReplica_dbSubnetGroupName' - Specifies a DB subnet group for the DB instance. The new DB instance is
-- created in the VPC associated with the DB subnet group. If no DB subnet
-- group is specified, then the new DB instance isn\'t created in a VPC.
--
-- Constraints:
--
-- -   Can only be specified if the source DB instance identifier specifies
--     a DB instance in another AWS Region.
--
-- -   If supplied, must match the name of an existing DBSubnetGroup.
--
-- -   The specified DB subnet group must be in the same AWS Region in
--     which the operation is running.
--
-- -   All read replicas in one AWS Region that are created from the same
--     source DB instance must either:>
--
--     -   Specify DB subnet groups from the same VPC. All these read
--         replicas are created in the same VPC.
--
--     -   Not specify a DB subnet group. All these read replicas are
--         created outside of any VPC.
--
-- Example: @mySubnetgroup@
--
-- 'multiAZ', 'createDBInstanceReadReplica_multiAZ' - A value that indicates whether the read replica is in a Multi-AZ
-- deployment.
--
-- You can create a read replica as a Multi-AZ DB instance. RDS creates a
-- standby of your replica in another Availability Zone for failover
-- support for the replica. Creating your read replica as a Multi-AZ DB
-- instance is independent of whether the source database is a Multi-AZ DB
-- instance.
--
-- 'publiclyAccessible', 'createDBInstanceReadReplica_publiclyAccessible' - A value that indicates whether the DB instance is publicly accessible.
--
-- When the DB instance is publicly accessible, its DNS endpoint resolves
-- to the private IP address from within the DB instance\'s VPC, and to the
-- public IP address from outside of the DB instance\'s VPC. Access to the
-- DB instance is ultimately controlled by the security group it uses, and
-- that public access is not permitted if the security group assigned to
-- the DB instance doesn\'t permit it.
--
-- When the DB instance isn\'t publicly accessible, it is an internal DB
-- instance with a DNS name that resolves to a private IP address.
--
-- For more information, see CreateDBInstance.
--
-- 'vpcSecurityGroupIds', 'createDBInstanceReadReplica_vpcSecurityGroupIds' - A list of EC2 VPC security groups to associate with the read replica.
--
-- Default: The default EC2 VPC security group for the DB subnet group\'s
-- VPC.
--
-- 'performanceInsightsKMSKeyId', 'createDBInstanceReadReplica_performanceInsightsKMSKeyId' - The AWS KMS key identifier for encryption of Performance Insights data.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS customer master key (CMK).
--
-- If you do not specify a value for @PerformanceInsightsKMSKeyId@, then
-- Amazon RDS uses your default CMK. There is a default CMK for your AWS
-- account. Your AWS account has a different default CMK for each AWS
-- Region.
--
-- 'kmsKeyId', 'createDBInstanceReadReplica_kmsKeyId' - The AWS KMS key identifier for an encrypted read replica.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS CMK.
--
-- If you create an encrypted read replica in the same AWS Region as the
-- source DB instance, then do not specify a value for this parameter. A
-- read replica in the same Region is always encrypted with the same AWS
-- KMS CMK as the source DB instance.
--
-- If you create an encrypted read replica in a different AWS Region, then
-- you must specify a AWS KMS key identifier for the destination AWS
-- Region. AWS KMS CMKs are specific to the AWS Region that they are
-- created in, and you can\'t use CMKs from one AWS Region in another AWS
-- Region.
--
-- You can\'t create an encrypted read replica from an unencrypted DB
-- instance.
--
-- 'dbParameterGroupName', 'createDBInstanceReadReplica_dbParameterGroupName' - The name of the DB parameter group to associate with this DB instance.
--
-- If you do not specify a value for @DBParameterGroupName@, then Amazon
-- RDS uses the @DBParameterGroup@ of source DB instance for a same region
-- read replica, or the default @DBParameterGroup@ for the specified DB
-- engine for a cross region read replica.
--
-- Currently, specifying a parameter group for this operation is only
-- supported for Oracle DB instances.
--
-- Constraints:
--
-- -   Must be 1 to 255 letters, numbers, or hyphens.
--
-- -   First character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
--
-- 'availabilityZone', 'createDBInstanceReadReplica_availabilityZone' - The Availability Zone (AZ) where the read replica will be created.
--
-- Default: A random, system-chosen Availability Zone in the endpoint\'s
-- AWS Region.
--
-- Example: @us-east-1d@
--
-- 'performanceInsightsRetentionPeriod', 'createDBInstanceReadReplica_performanceInsightsRetentionPeriod' - The amount of time, in days, to retain Performance Insights data. Valid
-- values are 7 or 731 (2 years).
--
-- 'tags', 'createDBInstanceReadReplica_tags' - Undocumented member.
--
-- 'processorFeatures', 'createDBInstanceReadReplica_processorFeatures' - The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
--
-- 'port', 'createDBInstanceReadReplica_port' - The port number that the DB instance uses for connections.
--
-- Default: Inherits from the source DB instance
--
-- Valid Values: @1150-65535@
--
-- 'dbInstanceClass', 'createDBInstanceReadReplica_dbInstanceClass' - The compute and memory capacity of the read replica, for example,
-- @db.m4.large@. Not all DB instance classes are available in all AWS
-- Regions, or for all database engines. For the full list of DB instance
-- classes, and availability for your engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
-- in the /Amazon RDS User Guide./
--
-- Default: Inherits from the source DB instance.
--
-- 'domainIAMRoleName', 'createDBInstanceReadReplica_domainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
--
-- 'preSignedUrl', 'createDBInstanceReadReplica_preSignedUrl' - The URL that contains a Signature Version 4 signed request for the
-- @CreateDBInstanceReadReplica@ API action in the source AWS Region that
-- contains the source DB instance.
--
-- You must specify this parameter when you create an encrypted read
-- replica from another AWS Region by using the Amazon RDS API. Don\'t
-- specify @PreSignedUrl@ when you are creating an encrypted read replica
-- in the same AWS Region.
--
-- The presigned URL must be a valid request for the
-- @CreateDBInstanceReadReplica@ API action that can be executed in the
-- source AWS Region that contains the encrypted source DB instance. The
-- presigned URL request must contain the following parameter values:
--
-- -   @DestinationRegion@ - The AWS Region that the encrypted read replica
--     is created in. This AWS Region is the same one where the
--     @CreateDBInstanceReadReplica@ action is called that contains this
--     presigned URL.
--
--     For example, if you create an encrypted DB instance in the us-west-1
--     AWS Region, from a source DB instance in the us-east-2 AWS Region,
--     then you call the @CreateDBInstanceReadReplica@ action in the
--     us-east-1 AWS Region and provide a presigned URL that contains a
--     call to the @CreateDBInstanceReadReplica@ action in the us-west-2
--     AWS Region. For this example, the @DestinationRegion@ in the
--     presigned URL must be set to the us-east-1 AWS Region.
--
-- -   @KmsKeyId@ - The AWS KMS key identifier for the key to use to
--     encrypt the read replica in the destination AWS Region. This is the
--     same identifier for both the @CreateDBInstanceReadReplica@ action
--     that is called in the destination AWS Region, and the action
--     contained in the presigned URL.
--
-- -   @SourceDBInstanceIdentifier@ - The DB instance identifier for the
--     encrypted DB instance to be replicated. This identifier must be in
--     the Amazon Resource Name (ARN) format for the source AWS Region. For
--     example, if you are creating an encrypted read replica from a DB
--     instance in the us-west-2 AWS Region, then your
--     @SourceDBInstanceIdentifier@ looks like the following example:
--     @arn:aws:rds:us-west-2:123456789012:instance:mysql-instance1-20161115@.
--
-- To learn how to generate a Signature Version 4 signed request, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (AWS Signature Version 4)>
-- and
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process>.
--
-- If you are using an AWS SDK tool or the AWS CLI, you can specify
-- @SourceRegion@ (or @--source-region@ for the AWS CLI) instead of
-- specifying @PreSignedUrl@ manually. Specifying @SourceRegion@
-- autogenerates a presigned URL that is a valid request for the operation
-- that can be executed in the source AWS Region.
--
-- @SourceRegion@ isn\'t supported for SQL Server, because SQL Server on
-- Amazon RDS doesn\'t support cross-region read replicas.
--
-- 'copyTagsToSnapshot', 'createDBInstanceReadReplica_copyTagsToSnapshot' - A value that indicates whether to copy all tags from the read replica to
-- snapshots of the read replica. By default, tags are not copied.
--
-- 'replicaMode', 'createDBInstanceReadReplica_replicaMode' - The open mode of the replica database: mounted or read-only.
--
-- This parameter is only supported for Oracle DB instances.
--
-- Mounted DB replicas are included in Oracle Enterprise Edition. The main
-- use case for mounted replicas is cross-Region disaster recovery. The
-- primary database doesn\'t use Active Data Guard to transmit information
-- to the mounted replica. Because it doesn\'t accept user connections, a
-- mounted replica can\'t serve a read-only workload.
--
-- You can create a combination of mounted and read-only DB replicas for
-- the same primary DB instance. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Working with Oracle Read Replicas for Amazon RDS>
-- in the /Amazon RDS User Guide/.
--
-- 'iops', 'createDBInstanceReadReplica_iops' - The amount of Provisioned IOPS (input\/output operations per second) to
-- be initially allocated for the DB instance.
--
-- 'autoMinorVersionUpgrade', 'createDBInstanceReadReplica_autoMinorVersionUpgrade' - A value that indicates whether minor engine upgrades are applied
-- automatically to the read replica during the maintenance window.
--
-- Default: Inherits from the source DB instance
--
-- 'dbInstanceIdentifier', 'createDBInstanceReadReplica_dbInstanceIdentifier' - The DB instance identifier of the read replica. This identifier is the
-- unique key that identifies a DB instance. This parameter is stored as a
-- lowercase string.
--
-- 'sourceDBInstanceIdentifier', 'createDBInstanceReadReplica_sourceDBInstanceIdentifier' - The identifier of the DB instance that will act as the source for the
-- read replica. Each DB instance can have up to five read replicas.
--
-- Constraints:
--
-- -   Must be the identifier of an existing MySQL, MariaDB, Oracle,
--     PostgreSQL, or SQL Server DB instance.
--
-- -   Can specify a DB instance that is a MySQL read replica only if the
--     source is running MySQL 5.6 or later.
--
-- -   For the limitations of Oracle read replicas, see
--     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Read Replica Limitations with Oracle>
--     in the /Amazon RDS User Guide/.
--
-- -   For the limitations of SQL Server read replicas, see
--     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/SQLServer.ReadReplicas.Limitations.html Read Replica Limitations with Microsoft SQL Server>
--     in the /Amazon RDS User Guide/.
--
-- -   Can specify a PostgreSQL DB instance only if the source is running
--     PostgreSQL 9.3.5 or later (9.4.7 and higher for cross-region
--     replication).
--
-- -   The specified DB instance must have automatic backups enabled, that
--     is, its backup retention period must be greater than 0.
--
-- -   If the source DB instance is in the same AWS Region as the read
--     replica, specify a valid DB instance identifier.
--
-- -   If the source DB instance is in a different AWS Region from the read
--     replica, specify a valid DB instance ARN. For more information, see
--     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS>
--     in the /Amazon RDS User Guide/. This doesn\'t apply to SQL Server,
--     which doesn\'t support cross-region replicas.
newCreateDBInstanceReadReplica ::
  -- | 'dbInstanceIdentifier'
  Prelude.Text ->
  -- | 'sourceDBInstanceIdentifier'
  Prelude.Text ->
  CreateDBInstanceReadReplica
newCreateDBInstanceReadReplica
  pDBInstanceIdentifier_
  pSourceDBInstanceIdentifier_ =
    CreateDBInstanceReadReplica'
      { deletionProtection =
          Prelude.Nothing,
        enablePerformanceInsights = Prelude.Nothing,
        maxAllocatedStorage = Prelude.Nothing,
        enableIAMDatabaseAuthentication =
          Prelude.Nothing,
        enableCloudwatchLogsExports = Prelude.Nothing,
        storageType = Prelude.Nothing,
        useDefaultProcessorFeatures = Prelude.Nothing,
        monitoringInterval = Prelude.Nothing,
        optionGroupName = Prelude.Nothing,
        domain = Prelude.Nothing,
        monitoringRoleArn = Prelude.Nothing,
        dbSubnetGroupName = Prelude.Nothing,
        multiAZ = Prelude.Nothing,
        publiclyAccessible = Prelude.Nothing,
        vpcSecurityGroupIds = Prelude.Nothing,
        performanceInsightsKMSKeyId = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        dbParameterGroupName = Prelude.Nothing,
        availabilityZone = Prelude.Nothing,
        performanceInsightsRetentionPeriod =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        processorFeatures = Prelude.Nothing,
        port = Prelude.Nothing,
        dbInstanceClass = Prelude.Nothing,
        domainIAMRoleName = Prelude.Nothing,
        preSignedUrl = Prelude.Nothing,
        copyTagsToSnapshot = Prelude.Nothing,
        replicaMode = Prelude.Nothing,
        iops = Prelude.Nothing,
        autoMinorVersionUpgrade = Prelude.Nothing,
        dbInstanceIdentifier = pDBInstanceIdentifier_,
        sourceDBInstanceIdentifier =
          pSourceDBInstanceIdentifier_
      }

-- | A value that indicates whether the DB instance has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
createDBInstanceReadReplica_deletionProtection :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Bool)
createDBInstanceReadReplica_deletionProtection = Lens.lens (\CreateDBInstanceReadReplica' {deletionProtection} -> deletionProtection) (\s@CreateDBInstanceReadReplica' {} a -> s {deletionProtection = a} :: CreateDBInstanceReadReplica)

-- | A value that indicates whether to enable Performance Insights for the
-- read replica.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights>
-- in the /Amazon RDS User Guide/.
createDBInstanceReadReplica_enablePerformanceInsights :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Bool)
createDBInstanceReadReplica_enablePerformanceInsights = Lens.lens (\CreateDBInstanceReadReplica' {enablePerformanceInsights} -> enablePerformanceInsights) (\s@CreateDBInstanceReadReplica' {} a -> s {enablePerformanceInsights = a} :: CreateDBInstanceReadReplica)

-- | The upper limit to which Amazon RDS can automatically scale the storage
-- of the DB instance.
--
-- For more information about this setting, including limitations that
-- apply to it, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
-- in the /Amazon RDS User Guide/.
createDBInstanceReadReplica_maxAllocatedStorage :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Int)
createDBInstanceReadReplica_maxAllocatedStorage = Lens.lens (\CreateDBInstanceReadReplica' {maxAllocatedStorage} -> maxAllocatedStorage) (\s@CreateDBInstanceReadReplica' {} a -> s {maxAllocatedStorage = a} :: CreateDBInstanceReadReplica)

-- | A value that indicates whether to enable mapping of AWS Identity and
-- Access Management (IAM) accounts to database accounts. By default,
-- mapping is disabled.
--
-- For more information about IAM database authentication, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
-- in the /Amazon RDS User Guide./
createDBInstanceReadReplica_enableIAMDatabaseAuthentication :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Bool)
createDBInstanceReadReplica_enableIAMDatabaseAuthentication = Lens.lens (\CreateDBInstanceReadReplica' {enableIAMDatabaseAuthentication} -> enableIAMDatabaseAuthentication) (\s@CreateDBInstanceReadReplica' {} a -> s {enableIAMDatabaseAuthentication = a} :: CreateDBInstanceReadReplica)

-- | The list of logs that the new DB instance is to export to CloudWatch
-- Logs. The values in the list depend on the DB engine being used. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon RDS User Guide/.
createDBInstanceReadReplica_enableCloudwatchLogsExports :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe [Prelude.Text])
createDBInstanceReadReplica_enableCloudwatchLogsExports = Lens.lens (\CreateDBInstanceReadReplica' {enableCloudwatchLogsExports} -> enableCloudwatchLogsExports) (\s@CreateDBInstanceReadReplica' {} a -> s {enableCloudwatchLogsExports = a} :: CreateDBInstanceReadReplica) Prelude.. Lens.mapping Prelude._Coerce

-- | Specifies the storage type to be associated with the read replica.
--
-- Valid values: @standard | gp2 | io1@
--
-- If you specify @io1@, you must also include a value for the @Iops@
-- parameter.
--
-- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
createDBInstanceReadReplica_storageType :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Text)
createDBInstanceReadReplica_storageType = Lens.lens (\CreateDBInstanceReadReplica' {storageType} -> storageType) (\s@CreateDBInstanceReadReplica' {} a -> s {storageType = a} :: CreateDBInstanceReadReplica)

-- | A value that indicates whether the DB instance class of the DB instance
-- uses its default processor features.
createDBInstanceReadReplica_useDefaultProcessorFeatures :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Bool)
createDBInstanceReadReplica_useDefaultProcessorFeatures = Lens.lens (\CreateDBInstanceReadReplica' {useDefaultProcessorFeatures} -> useDefaultProcessorFeatures) (\s@CreateDBInstanceReadReplica' {} a -> s {useDefaultProcessorFeatures = a} :: CreateDBInstanceReadReplica)

-- | The interval, in seconds, between points when Enhanced Monitoring
-- metrics are collected for the read replica. To disable collecting
-- Enhanced Monitoring metrics, specify 0. The default is 0.
--
-- If @MonitoringRoleArn@ is specified, then you must also set
-- @MonitoringInterval@ to a value other than 0.
--
-- Valid Values: @0, 1, 5, 10, 15, 30, 60@
createDBInstanceReadReplica_monitoringInterval :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Int)
createDBInstanceReadReplica_monitoringInterval = Lens.lens (\CreateDBInstanceReadReplica' {monitoringInterval} -> monitoringInterval) (\s@CreateDBInstanceReadReplica' {} a -> s {monitoringInterval = a} :: CreateDBInstanceReadReplica)

-- | The option group the DB instance is associated with. If omitted, the
-- option group associated with the source instance is used.
--
-- For SQL Server, you must use the option group associated with the source
-- instance.
createDBInstanceReadReplica_optionGroupName :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Text)
createDBInstanceReadReplica_optionGroupName = Lens.lens (\CreateDBInstanceReadReplica' {optionGroupName} -> optionGroupName) (\s@CreateDBInstanceReadReplica' {} a -> s {optionGroupName = a} :: CreateDBInstanceReadReplica)

-- | The Active Directory directory ID to create the DB instance in.
-- Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB
-- instances can be created in an Active Directory Domain.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon RDS User Guide/.
createDBInstanceReadReplica_domain :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Text)
createDBInstanceReadReplica_domain = Lens.lens (\CreateDBInstanceReadReplica' {domain} -> domain) (\s@CreateDBInstanceReadReplica' {} a -> s {domain = a} :: CreateDBInstanceReadReplica)

-- | The ARN for the IAM role that permits RDS to send enhanced monitoring
-- metrics to Amazon CloudWatch Logs. For example,
-- @arn:aws:iam:123456789012:role\/emaccess@. For information on creating a
-- monitoring role, go to
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.html#USER_Monitoring.OS.IAMRole To create an IAM role for Amazon RDS Enhanced Monitoring>
-- in the /Amazon RDS User Guide/.
--
-- If @MonitoringInterval@ is set to a value other than 0, then you must
-- supply a @MonitoringRoleArn@ value.
createDBInstanceReadReplica_monitoringRoleArn :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Text)
createDBInstanceReadReplica_monitoringRoleArn = Lens.lens (\CreateDBInstanceReadReplica' {monitoringRoleArn} -> monitoringRoleArn) (\s@CreateDBInstanceReadReplica' {} a -> s {monitoringRoleArn = a} :: CreateDBInstanceReadReplica)

-- | Specifies a DB subnet group for the DB instance. The new DB instance is
-- created in the VPC associated with the DB subnet group. If no DB subnet
-- group is specified, then the new DB instance isn\'t created in a VPC.
--
-- Constraints:
--
-- -   Can only be specified if the source DB instance identifier specifies
--     a DB instance in another AWS Region.
--
-- -   If supplied, must match the name of an existing DBSubnetGroup.
--
-- -   The specified DB subnet group must be in the same AWS Region in
--     which the operation is running.
--
-- -   All read replicas in one AWS Region that are created from the same
--     source DB instance must either:>
--
--     -   Specify DB subnet groups from the same VPC. All these read
--         replicas are created in the same VPC.
--
--     -   Not specify a DB subnet group. All these read replicas are
--         created outside of any VPC.
--
-- Example: @mySubnetgroup@
createDBInstanceReadReplica_dbSubnetGroupName :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Text)
createDBInstanceReadReplica_dbSubnetGroupName = Lens.lens (\CreateDBInstanceReadReplica' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@CreateDBInstanceReadReplica' {} a -> s {dbSubnetGroupName = a} :: CreateDBInstanceReadReplica)

-- | A value that indicates whether the read replica is in a Multi-AZ
-- deployment.
--
-- You can create a read replica as a Multi-AZ DB instance. RDS creates a
-- standby of your replica in another Availability Zone for failover
-- support for the replica. Creating your read replica as a Multi-AZ DB
-- instance is independent of whether the source database is a Multi-AZ DB
-- instance.
createDBInstanceReadReplica_multiAZ :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Bool)
createDBInstanceReadReplica_multiAZ = Lens.lens (\CreateDBInstanceReadReplica' {multiAZ} -> multiAZ) (\s@CreateDBInstanceReadReplica' {} a -> s {multiAZ = a} :: CreateDBInstanceReadReplica)

-- | A value that indicates whether the DB instance is publicly accessible.
--
-- When the DB instance is publicly accessible, its DNS endpoint resolves
-- to the private IP address from within the DB instance\'s VPC, and to the
-- public IP address from outside of the DB instance\'s VPC. Access to the
-- DB instance is ultimately controlled by the security group it uses, and
-- that public access is not permitted if the security group assigned to
-- the DB instance doesn\'t permit it.
--
-- When the DB instance isn\'t publicly accessible, it is an internal DB
-- instance with a DNS name that resolves to a private IP address.
--
-- For more information, see CreateDBInstance.
createDBInstanceReadReplica_publiclyAccessible :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Bool)
createDBInstanceReadReplica_publiclyAccessible = Lens.lens (\CreateDBInstanceReadReplica' {publiclyAccessible} -> publiclyAccessible) (\s@CreateDBInstanceReadReplica' {} a -> s {publiclyAccessible = a} :: CreateDBInstanceReadReplica)

-- | A list of EC2 VPC security groups to associate with the read replica.
--
-- Default: The default EC2 VPC security group for the DB subnet group\'s
-- VPC.
createDBInstanceReadReplica_vpcSecurityGroupIds :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe [Prelude.Text])
createDBInstanceReadReplica_vpcSecurityGroupIds = Lens.lens (\CreateDBInstanceReadReplica' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@CreateDBInstanceReadReplica' {} a -> s {vpcSecurityGroupIds = a} :: CreateDBInstanceReadReplica) Prelude.. Lens.mapping Prelude._Coerce

-- | The AWS KMS key identifier for encryption of Performance Insights data.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS customer master key (CMK).
--
-- If you do not specify a value for @PerformanceInsightsKMSKeyId@, then
-- Amazon RDS uses your default CMK. There is a default CMK for your AWS
-- account. Your AWS account has a different default CMK for each AWS
-- Region.
createDBInstanceReadReplica_performanceInsightsKMSKeyId :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Text)
createDBInstanceReadReplica_performanceInsightsKMSKeyId = Lens.lens (\CreateDBInstanceReadReplica' {performanceInsightsKMSKeyId} -> performanceInsightsKMSKeyId) (\s@CreateDBInstanceReadReplica' {} a -> s {performanceInsightsKMSKeyId = a} :: CreateDBInstanceReadReplica)

-- | The AWS KMS key identifier for an encrypted read replica.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS CMK.
--
-- If you create an encrypted read replica in the same AWS Region as the
-- source DB instance, then do not specify a value for this parameter. A
-- read replica in the same Region is always encrypted with the same AWS
-- KMS CMK as the source DB instance.
--
-- If you create an encrypted read replica in a different AWS Region, then
-- you must specify a AWS KMS key identifier for the destination AWS
-- Region. AWS KMS CMKs are specific to the AWS Region that they are
-- created in, and you can\'t use CMKs from one AWS Region in another AWS
-- Region.
--
-- You can\'t create an encrypted read replica from an unencrypted DB
-- instance.
createDBInstanceReadReplica_kmsKeyId :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Text)
createDBInstanceReadReplica_kmsKeyId = Lens.lens (\CreateDBInstanceReadReplica' {kmsKeyId} -> kmsKeyId) (\s@CreateDBInstanceReadReplica' {} a -> s {kmsKeyId = a} :: CreateDBInstanceReadReplica)

-- | The name of the DB parameter group to associate with this DB instance.
--
-- If you do not specify a value for @DBParameterGroupName@, then Amazon
-- RDS uses the @DBParameterGroup@ of source DB instance for a same region
-- read replica, or the default @DBParameterGroup@ for the specified DB
-- engine for a cross region read replica.
--
-- Currently, specifying a parameter group for this operation is only
-- supported for Oracle DB instances.
--
-- Constraints:
--
-- -   Must be 1 to 255 letters, numbers, or hyphens.
--
-- -   First character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
createDBInstanceReadReplica_dbParameterGroupName :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Text)
createDBInstanceReadReplica_dbParameterGroupName = Lens.lens (\CreateDBInstanceReadReplica' {dbParameterGroupName} -> dbParameterGroupName) (\s@CreateDBInstanceReadReplica' {} a -> s {dbParameterGroupName = a} :: CreateDBInstanceReadReplica)

-- | The Availability Zone (AZ) where the read replica will be created.
--
-- Default: A random, system-chosen Availability Zone in the endpoint\'s
-- AWS Region.
--
-- Example: @us-east-1d@
createDBInstanceReadReplica_availabilityZone :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Text)
createDBInstanceReadReplica_availabilityZone = Lens.lens (\CreateDBInstanceReadReplica' {availabilityZone} -> availabilityZone) (\s@CreateDBInstanceReadReplica' {} a -> s {availabilityZone = a} :: CreateDBInstanceReadReplica)

-- | The amount of time, in days, to retain Performance Insights data. Valid
-- values are 7 or 731 (2 years).
createDBInstanceReadReplica_performanceInsightsRetentionPeriod :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Int)
createDBInstanceReadReplica_performanceInsightsRetentionPeriod = Lens.lens (\CreateDBInstanceReadReplica' {performanceInsightsRetentionPeriod} -> performanceInsightsRetentionPeriod) (\s@CreateDBInstanceReadReplica' {} a -> s {performanceInsightsRetentionPeriod = a} :: CreateDBInstanceReadReplica)

-- | Undocumented member.
createDBInstanceReadReplica_tags :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe [Tag])
createDBInstanceReadReplica_tags = Lens.lens (\CreateDBInstanceReadReplica' {tags} -> tags) (\s@CreateDBInstanceReadReplica' {} a -> s {tags = a} :: CreateDBInstanceReadReplica) Prelude.. Lens.mapping Prelude._Coerce

-- | The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
createDBInstanceReadReplica_processorFeatures :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe [ProcessorFeature])
createDBInstanceReadReplica_processorFeatures = Lens.lens (\CreateDBInstanceReadReplica' {processorFeatures} -> processorFeatures) (\s@CreateDBInstanceReadReplica' {} a -> s {processorFeatures = a} :: CreateDBInstanceReadReplica) Prelude.. Lens.mapping Prelude._Coerce

-- | The port number that the DB instance uses for connections.
--
-- Default: Inherits from the source DB instance
--
-- Valid Values: @1150-65535@
createDBInstanceReadReplica_port :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Int)
createDBInstanceReadReplica_port = Lens.lens (\CreateDBInstanceReadReplica' {port} -> port) (\s@CreateDBInstanceReadReplica' {} a -> s {port = a} :: CreateDBInstanceReadReplica)

-- | The compute and memory capacity of the read replica, for example,
-- @db.m4.large@. Not all DB instance classes are available in all AWS
-- Regions, or for all database engines. For the full list of DB instance
-- classes, and availability for your engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
-- in the /Amazon RDS User Guide./
--
-- Default: Inherits from the source DB instance.
createDBInstanceReadReplica_dbInstanceClass :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Text)
createDBInstanceReadReplica_dbInstanceClass = Lens.lens (\CreateDBInstanceReadReplica' {dbInstanceClass} -> dbInstanceClass) (\s@CreateDBInstanceReadReplica' {} a -> s {dbInstanceClass = a} :: CreateDBInstanceReadReplica)

-- | Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
createDBInstanceReadReplica_domainIAMRoleName :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Text)
createDBInstanceReadReplica_domainIAMRoleName = Lens.lens (\CreateDBInstanceReadReplica' {domainIAMRoleName} -> domainIAMRoleName) (\s@CreateDBInstanceReadReplica' {} a -> s {domainIAMRoleName = a} :: CreateDBInstanceReadReplica)

-- | The URL that contains a Signature Version 4 signed request for the
-- @CreateDBInstanceReadReplica@ API action in the source AWS Region that
-- contains the source DB instance.
--
-- You must specify this parameter when you create an encrypted read
-- replica from another AWS Region by using the Amazon RDS API. Don\'t
-- specify @PreSignedUrl@ when you are creating an encrypted read replica
-- in the same AWS Region.
--
-- The presigned URL must be a valid request for the
-- @CreateDBInstanceReadReplica@ API action that can be executed in the
-- source AWS Region that contains the encrypted source DB instance. The
-- presigned URL request must contain the following parameter values:
--
-- -   @DestinationRegion@ - The AWS Region that the encrypted read replica
--     is created in. This AWS Region is the same one where the
--     @CreateDBInstanceReadReplica@ action is called that contains this
--     presigned URL.
--
--     For example, if you create an encrypted DB instance in the us-west-1
--     AWS Region, from a source DB instance in the us-east-2 AWS Region,
--     then you call the @CreateDBInstanceReadReplica@ action in the
--     us-east-1 AWS Region and provide a presigned URL that contains a
--     call to the @CreateDBInstanceReadReplica@ action in the us-west-2
--     AWS Region. For this example, the @DestinationRegion@ in the
--     presigned URL must be set to the us-east-1 AWS Region.
--
-- -   @KmsKeyId@ - The AWS KMS key identifier for the key to use to
--     encrypt the read replica in the destination AWS Region. This is the
--     same identifier for both the @CreateDBInstanceReadReplica@ action
--     that is called in the destination AWS Region, and the action
--     contained in the presigned URL.
--
-- -   @SourceDBInstanceIdentifier@ - The DB instance identifier for the
--     encrypted DB instance to be replicated. This identifier must be in
--     the Amazon Resource Name (ARN) format for the source AWS Region. For
--     example, if you are creating an encrypted read replica from a DB
--     instance in the us-west-2 AWS Region, then your
--     @SourceDBInstanceIdentifier@ looks like the following example:
--     @arn:aws:rds:us-west-2:123456789012:instance:mysql-instance1-20161115@.
--
-- To learn how to generate a Signature Version 4 signed request, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (AWS Signature Version 4)>
-- and
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process>.
--
-- If you are using an AWS SDK tool or the AWS CLI, you can specify
-- @SourceRegion@ (or @--source-region@ for the AWS CLI) instead of
-- specifying @PreSignedUrl@ manually. Specifying @SourceRegion@
-- autogenerates a presigned URL that is a valid request for the operation
-- that can be executed in the source AWS Region.
--
-- @SourceRegion@ isn\'t supported for SQL Server, because SQL Server on
-- Amazon RDS doesn\'t support cross-region read replicas.
createDBInstanceReadReplica_preSignedUrl :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Text)
createDBInstanceReadReplica_preSignedUrl = Lens.lens (\CreateDBInstanceReadReplica' {preSignedUrl} -> preSignedUrl) (\s@CreateDBInstanceReadReplica' {} a -> s {preSignedUrl = a} :: CreateDBInstanceReadReplica)

-- | A value that indicates whether to copy all tags from the read replica to
-- snapshots of the read replica. By default, tags are not copied.
createDBInstanceReadReplica_copyTagsToSnapshot :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Bool)
createDBInstanceReadReplica_copyTagsToSnapshot = Lens.lens (\CreateDBInstanceReadReplica' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@CreateDBInstanceReadReplica' {} a -> s {copyTagsToSnapshot = a} :: CreateDBInstanceReadReplica)

-- | The open mode of the replica database: mounted or read-only.
--
-- This parameter is only supported for Oracle DB instances.
--
-- Mounted DB replicas are included in Oracle Enterprise Edition. The main
-- use case for mounted replicas is cross-Region disaster recovery. The
-- primary database doesn\'t use Active Data Guard to transmit information
-- to the mounted replica. Because it doesn\'t accept user connections, a
-- mounted replica can\'t serve a read-only workload.
--
-- You can create a combination of mounted and read-only DB replicas for
-- the same primary DB instance. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Working with Oracle Read Replicas for Amazon RDS>
-- in the /Amazon RDS User Guide/.
createDBInstanceReadReplica_replicaMode :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe ReplicaMode)
createDBInstanceReadReplica_replicaMode = Lens.lens (\CreateDBInstanceReadReplica' {replicaMode} -> replicaMode) (\s@CreateDBInstanceReadReplica' {} a -> s {replicaMode = a} :: CreateDBInstanceReadReplica)

-- | The amount of Provisioned IOPS (input\/output operations per second) to
-- be initially allocated for the DB instance.
createDBInstanceReadReplica_iops :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Int)
createDBInstanceReadReplica_iops = Lens.lens (\CreateDBInstanceReadReplica' {iops} -> iops) (\s@CreateDBInstanceReadReplica' {} a -> s {iops = a} :: CreateDBInstanceReadReplica)

-- | A value that indicates whether minor engine upgrades are applied
-- automatically to the read replica during the maintenance window.
--
-- Default: Inherits from the source DB instance
createDBInstanceReadReplica_autoMinorVersionUpgrade :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Bool)
createDBInstanceReadReplica_autoMinorVersionUpgrade = Lens.lens (\CreateDBInstanceReadReplica' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@CreateDBInstanceReadReplica' {} a -> s {autoMinorVersionUpgrade = a} :: CreateDBInstanceReadReplica)

-- | The DB instance identifier of the read replica. This identifier is the
-- unique key that identifies a DB instance. This parameter is stored as a
-- lowercase string.
createDBInstanceReadReplica_dbInstanceIdentifier :: Lens.Lens' CreateDBInstanceReadReplica Prelude.Text
createDBInstanceReadReplica_dbInstanceIdentifier = Lens.lens (\CreateDBInstanceReadReplica' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@CreateDBInstanceReadReplica' {} a -> s {dbInstanceIdentifier = a} :: CreateDBInstanceReadReplica)

-- | The identifier of the DB instance that will act as the source for the
-- read replica. Each DB instance can have up to five read replicas.
--
-- Constraints:
--
-- -   Must be the identifier of an existing MySQL, MariaDB, Oracle,
--     PostgreSQL, or SQL Server DB instance.
--
-- -   Can specify a DB instance that is a MySQL read replica only if the
--     source is running MySQL 5.6 or later.
--
-- -   For the limitations of Oracle read replicas, see
--     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Read Replica Limitations with Oracle>
--     in the /Amazon RDS User Guide/.
--
-- -   For the limitations of SQL Server read replicas, see
--     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/SQLServer.ReadReplicas.Limitations.html Read Replica Limitations with Microsoft SQL Server>
--     in the /Amazon RDS User Guide/.
--
-- -   Can specify a PostgreSQL DB instance only if the source is running
--     PostgreSQL 9.3.5 or later (9.4.7 and higher for cross-region
--     replication).
--
-- -   The specified DB instance must have automatic backups enabled, that
--     is, its backup retention period must be greater than 0.
--
-- -   If the source DB instance is in the same AWS Region as the read
--     replica, specify a valid DB instance identifier.
--
-- -   If the source DB instance is in a different AWS Region from the read
--     replica, specify a valid DB instance ARN. For more information, see
--     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS>
--     in the /Amazon RDS User Guide/. This doesn\'t apply to SQL Server,
--     which doesn\'t support cross-region replicas.
createDBInstanceReadReplica_sourceDBInstanceIdentifier :: Lens.Lens' CreateDBInstanceReadReplica Prelude.Text
createDBInstanceReadReplica_sourceDBInstanceIdentifier = Lens.lens (\CreateDBInstanceReadReplica' {sourceDBInstanceIdentifier} -> sourceDBInstanceIdentifier) (\s@CreateDBInstanceReadReplica' {} a -> s {sourceDBInstanceIdentifier = a} :: CreateDBInstanceReadReplica)

instance
  Prelude.AWSRequest
    CreateDBInstanceReadReplica
  where
  type
    Rs CreateDBInstanceReadReplica =
      CreateDBInstanceReadReplicaResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateDBInstanceReadReplicaResult"
      ( \s h x ->
          CreateDBInstanceReadReplicaResponse'
            Prelude.<$> (x Prelude..@? "DBInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDBInstanceReadReplica

instance Prelude.NFData CreateDBInstanceReadReplica

instance
  Prelude.ToHeaders
    CreateDBInstanceReadReplica
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CreateDBInstanceReadReplica where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateDBInstanceReadReplica where
  toQuery CreateDBInstanceReadReplica' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "CreateDBInstanceReadReplica" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2014-10-31" :: Prelude.ByteString),
        "DeletionProtection" Prelude.=: deletionProtection,
        "EnablePerformanceInsights"
          Prelude.=: enablePerformanceInsights,
        "MaxAllocatedStorage" Prelude.=: maxAllocatedStorage,
        "EnableIAMDatabaseAuthentication"
          Prelude.=: enableIAMDatabaseAuthentication,
        "EnableCloudwatchLogsExports"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> enableCloudwatchLogsExports
            ),
        "StorageType" Prelude.=: storageType,
        "UseDefaultProcessorFeatures"
          Prelude.=: useDefaultProcessorFeatures,
        "MonitoringInterval" Prelude.=: monitoringInterval,
        "OptionGroupName" Prelude.=: optionGroupName,
        "Domain" Prelude.=: domain,
        "MonitoringRoleArn" Prelude.=: monitoringRoleArn,
        "DBSubnetGroupName" Prelude.=: dbSubnetGroupName,
        "MultiAZ" Prelude.=: multiAZ,
        "PubliclyAccessible" Prelude.=: publiclyAccessible,
        "VpcSecurityGroupIds"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "VpcSecurityGroupId"
                Prelude.<$> vpcSecurityGroupIds
            ),
        "PerformanceInsightsKMSKeyId"
          Prelude.=: performanceInsightsKMSKeyId,
        "KmsKeyId" Prelude.=: kmsKeyId,
        "DBParameterGroupName"
          Prelude.=: dbParameterGroupName,
        "AvailabilityZone" Prelude.=: availabilityZone,
        "PerformanceInsightsRetentionPeriod"
          Prelude.=: performanceInsightsRetentionPeriod,
        "Tags"
          Prelude.=: Prelude.toQuery
            (Prelude.toQueryList "Tag" Prelude.<$> tags),
        "ProcessorFeatures"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "ProcessorFeature"
                Prelude.<$> processorFeatures
            ),
        "Port" Prelude.=: port,
        "DBInstanceClass" Prelude.=: dbInstanceClass,
        "DomainIAMRoleName" Prelude.=: domainIAMRoleName,
        "PreSignedUrl" Prelude.=: preSignedUrl,
        "CopyTagsToSnapshot" Prelude.=: copyTagsToSnapshot,
        "ReplicaMode" Prelude.=: replicaMode,
        "Iops" Prelude.=: iops,
        "AutoMinorVersionUpgrade"
          Prelude.=: autoMinorVersionUpgrade,
        "DBInstanceIdentifier"
          Prelude.=: dbInstanceIdentifier,
        "SourceDBInstanceIdentifier"
          Prelude.=: sourceDBInstanceIdentifier
      ]

-- | /See:/ 'newCreateDBInstanceReadReplicaResponse' smart constructor.
data CreateDBInstanceReadReplicaResponse = CreateDBInstanceReadReplicaResponse'
  { dbInstance :: Prelude.Maybe DBInstance,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateDBInstanceReadReplicaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbInstance', 'createDBInstanceReadReplicaResponse_dbInstance' - Undocumented member.
--
-- 'httpStatus', 'createDBInstanceReadReplicaResponse_httpStatus' - The response's http status code.
newCreateDBInstanceReadReplicaResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDBInstanceReadReplicaResponse
newCreateDBInstanceReadReplicaResponse pHttpStatus_ =
  CreateDBInstanceReadReplicaResponse'
    { dbInstance =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createDBInstanceReadReplicaResponse_dbInstance :: Lens.Lens' CreateDBInstanceReadReplicaResponse (Prelude.Maybe DBInstance)
createDBInstanceReadReplicaResponse_dbInstance = Lens.lens (\CreateDBInstanceReadReplicaResponse' {dbInstance} -> dbInstance) (\s@CreateDBInstanceReadReplicaResponse' {} a -> s {dbInstance = a} :: CreateDBInstanceReadReplicaResponse)

-- | The response's http status code.
createDBInstanceReadReplicaResponse_httpStatus :: Lens.Lens' CreateDBInstanceReadReplicaResponse Prelude.Int
createDBInstanceReadReplicaResponse_httpStatus = Lens.lens (\CreateDBInstanceReadReplicaResponse' {httpStatus} -> httpStatus) (\s@CreateDBInstanceReadReplicaResponse' {} a -> s {httpStatus = a} :: CreateDBInstanceReadReplicaResponse)

instance
  Prelude.NFData
    CreateDBInstanceReadReplicaResponse
