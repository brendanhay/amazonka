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
-- Module      : Amazonka.RDS.CreateDBInstanceReadReplica
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
module Amazonka.RDS.CreateDBInstanceReadReplica
  ( -- * Creating a Request
    CreateDBInstanceReadReplica (..),
    newCreateDBInstanceReadReplica,

    -- * Request Lenses
    createDBInstanceReadReplica_deletionProtection,
    createDBInstanceReadReplica_publiclyAccessible,
    createDBInstanceReadReplica_autoMinorVersionUpgrade,
    createDBInstanceReadReplica_dbSubnetGroupName,
    createDBInstanceReadReplica_monitoringRoleArn,
    createDBInstanceReadReplica_iops,
    createDBInstanceReadReplica_domain,
    createDBInstanceReadReplica_replicaMode,
    createDBInstanceReadReplica_monitoringInterval,
    createDBInstanceReadReplica_preSignedUrl,
    createDBInstanceReadReplica_processorFeatures,
    createDBInstanceReadReplica_dbInstanceClass,
    createDBInstanceReadReplica_performanceInsightsRetentionPeriod,
    createDBInstanceReadReplica_destinationRegion,
    createDBInstanceReadReplica_maxAllocatedStorage,
    createDBInstanceReadReplica_enablePerformanceInsights,
    createDBInstanceReadReplica_kmsKeyId,
    createDBInstanceReadReplica_dbParameterGroupName,
    createDBInstanceReadReplica_availabilityZone,
    createDBInstanceReadReplica_performanceInsightsKMSKeyId,
    createDBInstanceReadReplica_vpcSecurityGroupIds,
    createDBInstanceReadReplica_multiAZ,
    createDBInstanceReadReplica_optionGroupName,
    createDBInstanceReadReplica_copyTagsToSnapshot,
    createDBInstanceReadReplica_domainIAMRoleName,
    createDBInstanceReadReplica_tags,
    createDBInstanceReadReplica_port,
    createDBInstanceReadReplica_enableIAMDatabaseAuthentication,
    createDBInstanceReadReplica_useDefaultProcessorFeatures,
    createDBInstanceReadReplica_storageType,
    createDBInstanceReadReplica_enableCloudwatchLogsExports,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDBInstanceReadReplica' smart constructor.
data CreateDBInstanceReadReplica = CreateDBInstanceReadReplica'
  { -- | A value that indicates whether the DB instance has deletion protection
    -- enabled. The database can\'t be deleted when deletion protection is
    -- enabled. By default, deletion protection is disabled. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
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
    -- | A value that indicates whether minor engine upgrades are applied
    -- automatically to the read replica during the maintenance window.
    --
    -- Default: Inherits from the source DB instance
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | Specifies a DB subnet group for the DB instance. The new DB instance is
    -- created in the VPC associated with the DB subnet group. If no DB subnet
    -- group is specified, then the new DB instance isn\'t created in a VPC.
    --
    -- Constraints:
    --
    -- -   Can only be specified if the source DB instance identifier specifies
    --     a DB instance in another Amazon Web Services Region.
    --
    -- -   If supplied, must match the name of an existing DBSubnetGroup.
    --
    -- -   The specified DB subnet group must be in the same Amazon Web
    --     Services Region in which the operation is running.
    --
    -- -   All read replicas in one Amazon Web Services Region that are created
    --     from the same source DB instance must either:>
    --
    --     -   Specify DB subnet groups from the same VPC. All these read
    --         replicas are created in the same VPC.
    --
    --     -   Not specify a DB subnet group. All these read replicas are
    --         created outside of any VPC.
    --
    -- Example: @mySubnetgroup@
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
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
    -- | The amount of Provisioned IOPS (input\/output operations per second) to
    -- be initially allocated for the DB instance.
    iops :: Prelude.Maybe Prelude.Int,
    -- | The Active Directory directory ID to create the DB instance in.
    -- Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB
    -- instances can be created in an Active Directory Domain.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
    -- in the /Amazon RDS User Guide/.
    domain :: Prelude.Maybe Prelude.Text,
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
    -- | The interval, in seconds, between points when Enhanced Monitoring
    -- metrics are collected for the read replica. To disable collecting
    -- Enhanced Monitoring metrics, specify 0. The default is 0.
    --
    -- If @MonitoringRoleArn@ is specified, then you must also set
    -- @MonitoringInterval@ to a value other than 0.
    --
    -- Valid Values: @0, 1, 5, 10, 15, 30, 60@
    monitoringInterval :: Prelude.Maybe Prelude.Int,
    -- | The URL that contains a Signature Version 4 signed request for the
    -- @CreateDBInstanceReadReplica@ API action in the source Amazon Web
    -- Services Region that contains the source DB instance.
    --
    -- You must specify this parameter when you create an encrypted read
    -- replica from another Amazon Web Services Region by using the Amazon RDS
    -- API. Don\'t specify @PreSignedUrl@ when you are creating an encrypted
    -- read replica in the same Amazon Web Services Region.
    --
    -- The presigned URL must be a valid request for the
    -- @CreateDBInstanceReadReplica@ API action that can be executed in the
    -- source Amazon Web Services Region that contains the encrypted source DB
    -- instance. The presigned URL request must contain the following parameter
    -- values:
    --
    -- -   @DestinationRegion@ - The Amazon Web Services Region that the
    --     encrypted read replica is created in. This Amazon Web Services
    --     Region is the same one where the @CreateDBInstanceReadReplica@
    --     action is called that contains this presigned URL.
    --
    --     For example, if you create an encrypted DB instance in the us-west-1
    --     Amazon Web Services Region, from a source DB instance in the
    --     us-east-2 Amazon Web Services Region, then you call the
    --     @CreateDBInstanceReadReplica@ action in the us-east-1 Amazon Web
    --     Services Region and provide a presigned URL that contains a call to
    --     the @CreateDBInstanceReadReplica@ action in the us-west-2 Amazon Web
    --     Services Region. For this example, the @DestinationRegion@ in the
    --     presigned URL must be set to the us-east-1 Amazon Web Services
    --     Region.
    --
    -- -   @KmsKeyId@ - The Amazon Web Services KMS key identifier for the key
    --     to use to encrypt the read replica in the destination Amazon Web
    --     Services Region. This is the same identifier for both the
    --     @CreateDBInstanceReadReplica@ action that is called in the
    --     destination Amazon Web Services Region, and the action contained in
    --     the presigned URL.
    --
    -- -   @SourceDBInstanceIdentifier@ - The DB instance identifier for the
    --     encrypted DB instance to be replicated. This identifier must be in
    --     the Amazon Resource Name (ARN) format for the source Amazon Web
    --     Services Region. For example, if you are creating an encrypted read
    --     replica from a DB instance in the us-west-2 Amazon Web Services
    --     Region, then your @SourceDBInstanceIdentifier@ looks like the
    --     following example:
    --     @arn:aws:rds:us-west-2:123456789012:instance:mysql-instance1-20161115@.
    --
    -- To learn how to generate a Signature Version 4 signed request, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (Amazon Web Services Signature Version 4)>
    -- and
    -- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process>.
    --
    -- If you are using an Amazon Web Services SDK tool or the CLI, you can
    -- specify @SourceRegion@ (or @--source-region@ for the CLI) instead of
    -- specifying @PreSignedUrl@ manually. Specifying @SourceRegion@
    -- autogenerates a presigned URL that is a valid request for the operation
    -- that can be executed in the source Amazon Web Services Region.
    --
    -- @SourceRegion@ isn\'t supported for SQL Server, because SQL Server on
    -- Amazon RDS doesn\'t support cross-region read replicas.
    preSignedUrl :: Prelude.Maybe Prelude.Text,
    -- | The number of CPU cores and the number of threads per core for the DB
    -- instance class of the DB instance.
    processorFeatures :: Prelude.Maybe [ProcessorFeature],
    -- | The compute and memory capacity of the read replica, for example,
    -- @db.m4.large@. Not all DB instance classes are available in all Amazon
    -- Web Services Regions, or for all database engines. For the full list of
    -- DB instance classes, and availability for your engine, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
    -- in the /Amazon RDS User Guide./
    --
    -- Default: Inherits from the source DB instance.
    dbInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | The amount of time, in days, to retain Performance Insights data. Valid
    -- values are 7 or 731 (2 years).
    performanceInsightsRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | Pseudo-parameter used when populating the @PreSignedUrl@ of a
    -- cross-region @CreateDBInstanceReadReplica@ request. To replicate from
    -- region @SRC@ to region @DST@, send a request to region @DST@. In that
    -- request, pass a @PreSignedUrl@ for region @SRC@ with @DestinationRegion@
    -- set to region @DST@.
    destinationRegion :: Prelude.Maybe Prelude.Text,
    -- | The upper limit in gibibytes (GiB) to which Amazon RDS can automatically
    -- scale the storage of the DB instance.
    --
    -- For more information about this setting, including limitations that
    -- apply to it, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
    -- in the /Amazon RDS User Guide/.
    maxAllocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | A value that indicates whether to enable Performance Insights for the
    -- read replica.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights>
    -- in the /Amazon RDS User Guide/.
    enablePerformanceInsights :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Web Services KMS key identifier for an encrypted read
    -- replica.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the Amazon Web Services KMS CMK.
    --
    -- If you create an encrypted read replica in the same Amazon Web Services
    -- Region as the source DB instance, then do not specify a value for this
    -- parameter. A read replica in the same Region is always encrypted with
    -- the same Amazon Web Services KMS CMK as the source DB instance.
    --
    -- If you create an encrypted read replica in a different Amazon Web
    -- Services Region, then you must specify a Amazon Web Services KMS key
    -- identifier for the destination Amazon Web Services Region. Amazon Web
    -- Services KMS CMKs are specific to the Amazon Web Services Region that
    -- they are created in, and you can\'t use CMKs from one Amazon Web
    -- Services Region in another Amazon Web Services Region.
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
    -- Amazon Web Services Region.
    --
    -- Example: @us-east-1d@
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services KMS key identifier for encryption of Performance
    -- Insights data.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the Amazon Web Services KMS customer master key
    -- (CMK).
    --
    -- If you do not specify a value for @PerformanceInsightsKMSKeyId@, then
    -- Amazon RDS uses your default CMK. There is a default CMK for your Amazon
    -- Web Services account. Your Amazon Web Services account has a different
    -- default CMK for each Amazon Web Services Region.
    performanceInsightsKMSKeyId :: Prelude.Maybe Prelude.Text,
    -- | A list of EC2 VPC security groups to associate with the read replica.
    --
    -- Default: The default EC2 VPC security group for the DB subnet group\'s
    -- VPC.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | A value that indicates whether the read replica is in a Multi-AZ
    -- deployment.
    --
    -- You can create a read replica as a Multi-AZ DB instance. RDS creates a
    -- standby of your replica in another Availability Zone for failover
    -- support for the replica. Creating your read replica as a Multi-AZ DB
    -- instance is independent of whether the source database is a Multi-AZ DB
    -- instance.
    multiAZ :: Prelude.Maybe Prelude.Bool,
    -- | The option group the DB instance is associated with. If omitted, the
    -- option group associated with the source instance is used.
    --
    -- For SQL Server, you must use the option group associated with the source
    -- instance.
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to copy all tags from the read replica to
    -- snapshots of the read replica. By default, tags are not copied.
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | Specify the name of the IAM role to be used when making API calls to the
    -- Directory Service.
    domainIAMRoleName :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe [Tag],
    -- | The port number that the DB instance uses for connections.
    --
    -- Default: Inherits from the source DB instance
    --
    -- Valid Values: @1150-65535@
    port :: Prelude.Maybe Prelude.Int,
    -- | A value that indicates whether to enable mapping of Amazon Web Services
    -- Identity and Access Management (IAM) accounts to database accounts. By
    -- default, mapping is disabled.
    --
    -- For more information about IAM database authentication, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
    -- in the /Amazon RDS User Guide./
    enableIAMDatabaseAuthentication :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether the DB instance class of the DB instance
    -- uses its default processor features.
    useDefaultProcessorFeatures :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the storage type to be associated with the read replica.
    --
    -- Valid values: @standard | gp2 | io1@
    --
    -- If you specify @io1@, you must also include a value for the @Iops@
    -- parameter.
    --
    -- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
    storageType :: Prelude.Maybe Prelude.Text,
    -- | The list of logs that the new DB instance is to export to CloudWatch
    -- Logs. The values in the list depend on the DB engine being used. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
    -- in the /Amazon RDS User Guide/.
    enableCloudwatchLogsExports :: Prelude.Maybe [Prelude.Text],
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
    -- -   If the source DB instance is in the same Amazon Web Services Region
    --     as the read replica, specify a valid DB instance identifier.
    --
    -- -   If the source DB instance is in a different Amazon Web Services
    --     Region from the read replica, specify a valid DB instance ARN. For
    --     more information, see
    --     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS>
    --     in the /Amazon RDS User Guide/. This doesn\'t apply to SQL Server,
    --     which doesn\'t support cross-region replicas.
    sourceDBInstanceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'autoMinorVersionUpgrade', 'createDBInstanceReadReplica_autoMinorVersionUpgrade' - A value that indicates whether minor engine upgrades are applied
-- automatically to the read replica during the maintenance window.
--
-- Default: Inherits from the source DB instance
--
-- 'dbSubnetGroupName', 'createDBInstanceReadReplica_dbSubnetGroupName' - Specifies a DB subnet group for the DB instance. The new DB instance is
-- created in the VPC associated with the DB subnet group. If no DB subnet
-- group is specified, then the new DB instance isn\'t created in a VPC.
--
-- Constraints:
--
-- -   Can only be specified if the source DB instance identifier specifies
--     a DB instance in another Amazon Web Services Region.
--
-- -   If supplied, must match the name of an existing DBSubnetGroup.
--
-- -   The specified DB subnet group must be in the same Amazon Web
--     Services Region in which the operation is running.
--
-- -   All read replicas in one Amazon Web Services Region that are created
--     from the same source DB instance must either:>
--
--     -   Specify DB subnet groups from the same VPC. All these read
--         replicas are created in the same VPC.
--
--     -   Not specify a DB subnet group. All these read replicas are
--         created outside of any VPC.
--
-- Example: @mySubnetgroup@
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
-- 'iops', 'createDBInstanceReadReplica_iops' - The amount of Provisioned IOPS (input\/output operations per second) to
-- be initially allocated for the DB instance.
--
-- 'domain', 'createDBInstanceReadReplica_domain' - The Active Directory directory ID to create the DB instance in.
-- Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB
-- instances can be created in an Active Directory Domain.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon RDS User Guide/.
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
-- 'monitoringInterval', 'createDBInstanceReadReplica_monitoringInterval' - The interval, in seconds, between points when Enhanced Monitoring
-- metrics are collected for the read replica. To disable collecting
-- Enhanced Monitoring metrics, specify 0. The default is 0.
--
-- If @MonitoringRoleArn@ is specified, then you must also set
-- @MonitoringInterval@ to a value other than 0.
--
-- Valid Values: @0, 1, 5, 10, 15, 30, 60@
--
-- 'preSignedUrl', 'createDBInstanceReadReplica_preSignedUrl' - The URL that contains a Signature Version 4 signed request for the
-- @CreateDBInstanceReadReplica@ API action in the source Amazon Web
-- Services Region that contains the source DB instance.
--
-- You must specify this parameter when you create an encrypted read
-- replica from another Amazon Web Services Region by using the Amazon RDS
-- API. Don\'t specify @PreSignedUrl@ when you are creating an encrypted
-- read replica in the same Amazon Web Services Region.
--
-- The presigned URL must be a valid request for the
-- @CreateDBInstanceReadReplica@ API action that can be executed in the
-- source Amazon Web Services Region that contains the encrypted source DB
-- instance. The presigned URL request must contain the following parameter
-- values:
--
-- -   @DestinationRegion@ - The Amazon Web Services Region that the
--     encrypted read replica is created in. This Amazon Web Services
--     Region is the same one where the @CreateDBInstanceReadReplica@
--     action is called that contains this presigned URL.
--
--     For example, if you create an encrypted DB instance in the us-west-1
--     Amazon Web Services Region, from a source DB instance in the
--     us-east-2 Amazon Web Services Region, then you call the
--     @CreateDBInstanceReadReplica@ action in the us-east-1 Amazon Web
--     Services Region and provide a presigned URL that contains a call to
--     the @CreateDBInstanceReadReplica@ action in the us-west-2 Amazon Web
--     Services Region. For this example, the @DestinationRegion@ in the
--     presigned URL must be set to the us-east-1 Amazon Web Services
--     Region.
--
-- -   @KmsKeyId@ - The Amazon Web Services KMS key identifier for the key
--     to use to encrypt the read replica in the destination Amazon Web
--     Services Region. This is the same identifier for both the
--     @CreateDBInstanceReadReplica@ action that is called in the
--     destination Amazon Web Services Region, and the action contained in
--     the presigned URL.
--
-- -   @SourceDBInstanceIdentifier@ - The DB instance identifier for the
--     encrypted DB instance to be replicated. This identifier must be in
--     the Amazon Resource Name (ARN) format for the source Amazon Web
--     Services Region. For example, if you are creating an encrypted read
--     replica from a DB instance in the us-west-2 Amazon Web Services
--     Region, then your @SourceDBInstanceIdentifier@ looks like the
--     following example:
--     @arn:aws:rds:us-west-2:123456789012:instance:mysql-instance1-20161115@.
--
-- To learn how to generate a Signature Version 4 signed request, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (Amazon Web Services Signature Version 4)>
-- and
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process>.
--
-- If you are using an Amazon Web Services SDK tool or the CLI, you can
-- specify @SourceRegion@ (or @--source-region@ for the CLI) instead of
-- specifying @PreSignedUrl@ manually. Specifying @SourceRegion@
-- autogenerates a presigned URL that is a valid request for the operation
-- that can be executed in the source Amazon Web Services Region.
--
-- @SourceRegion@ isn\'t supported for SQL Server, because SQL Server on
-- Amazon RDS doesn\'t support cross-region read replicas.
--
-- 'processorFeatures', 'createDBInstanceReadReplica_processorFeatures' - The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
--
-- 'dbInstanceClass', 'createDBInstanceReadReplica_dbInstanceClass' - The compute and memory capacity of the read replica, for example,
-- @db.m4.large@. Not all DB instance classes are available in all Amazon
-- Web Services Regions, or for all database engines. For the full list of
-- DB instance classes, and availability for your engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
-- in the /Amazon RDS User Guide./
--
-- Default: Inherits from the source DB instance.
--
-- 'performanceInsightsRetentionPeriod', 'createDBInstanceReadReplica_performanceInsightsRetentionPeriod' - The amount of time, in days, to retain Performance Insights data. Valid
-- values are 7 or 731 (2 years).
--
-- 'destinationRegion', 'createDBInstanceReadReplica_destinationRegion' - Pseudo-parameter used when populating the @PreSignedUrl@ of a
-- cross-region @CreateDBInstanceReadReplica@ request. To replicate from
-- region @SRC@ to region @DST@, send a request to region @DST@. In that
-- request, pass a @PreSignedUrl@ for region @SRC@ with @DestinationRegion@
-- set to region @DST@.
--
-- 'maxAllocatedStorage', 'createDBInstanceReadReplica_maxAllocatedStorage' - The upper limit in gibibytes (GiB) to which Amazon RDS can automatically
-- scale the storage of the DB instance.
--
-- For more information about this setting, including limitations that
-- apply to it, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
-- in the /Amazon RDS User Guide/.
--
-- 'enablePerformanceInsights', 'createDBInstanceReadReplica_enablePerformanceInsights' - A value that indicates whether to enable Performance Insights for the
-- read replica.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights>
-- in the /Amazon RDS User Guide/.
--
-- 'kmsKeyId', 'createDBInstanceReadReplica_kmsKeyId' - The Amazon Web Services KMS key identifier for an encrypted read
-- replica.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the Amazon Web Services KMS CMK.
--
-- If you create an encrypted read replica in the same Amazon Web Services
-- Region as the source DB instance, then do not specify a value for this
-- parameter. A read replica in the same Region is always encrypted with
-- the same Amazon Web Services KMS CMK as the source DB instance.
--
-- If you create an encrypted read replica in a different Amazon Web
-- Services Region, then you must specify a Amazon Web Services KMS key
-- identifier for the destination Amazon Web Services Region. Amazon Web
-- Services KMS CMKs are specific to the Amazon Web Services Region that
-- they are created in, and you can\'t use CMKs from one Amazon Web
-- Services Region in another Amazon Web Services Region.
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
-- Amazon Web Services Region.
--
-- Example: @us-east-1d@
--
-- 'performanceInsightsKMSKeyId', 'createDBInstanceReadReplica_performanceInsightsKMSKeyId' - The Amazon Web Services KMS key identifier for encryption of Performance
-- Insights data.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the Amazon Web Services KMS customer master key
-- (CMK).
--
-- If you do not specify a value for @PerformanceInsightsKMSKeyId@, then
-- Amazon RDS uses your default CMK. There is a default CMK for your Amazon
-- Web Services account. Your Amazon Web Services account has a different
-- default CMK for each Amazon Web Services Region.
--
-- 'vpcSecurityGroupIds', 'createDBInstanceReadReplica_vpcSecurityGroupIds' - A list of EC2 VPC security groups to associate with the read replica.
--
-- Default: The default EC2 VPC security group for the DB subnet group\'s
-- VPC.
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
-- 'optionGroupName', 'createDBInstanceReadReplica_optionGroupName' - The option group the DB instance is associated with. If omitted, the
-- option group associated with the source instance is used.
--
-- For SQL Server, you must use the option group associated with the source
-- instance.
--
-- 'copyTagsToSnapshot', 'createDBInstanceReadReplica_copyTagsToSnapshot' - A value that indicates whether to copy all tags from the read replica to
-- snapshots of the read replica. By default, tags are not copied.
--
-- 'domainIAMRoleName', 'createDBInstanceReadReplica_domainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
--
-- 'tags', 'createDBInstanceReadReplica_tags' - Undocumented member.
--
-- 'port', 'createDBInstanceReadReplica_port' - The port number that the DB instance uses for connections.
--
-- Default: Inherits from the source DB instance
--
-- Valid Values: @1150-65535@
--
-- 'enableIAMDatabaseAuthentication', 'createDBInstanceReadReplica_enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts. By
-- default, mapping is disabled.
--
-- For more information about IAM database authentication, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
-- in the /Amazon RDS User Guide./
--
-- 'useDefaultProcessorFeatures', 'createDBInstanceReadReplica_useDefaultProcessorFeatures' - A value that indicates whether the DB instance class of the DB instance
-- uses its default processor features.
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
-- 'enableCloudwatchLogsExports', 'createDBInstanceReadReplica_enableCloudwatchLogsExports' - The list of logs that the new DB instance is to export to CloudWatch
-- Logs. The values in the list depend on the DB engine being used. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon RDS User Guide/.
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
-- -   If the source DB instance is in the same Amazon Web Services Region
--     as the read replica, specify a valid DB instance identifier.
--
-- -   If the source DB instance is in a different Amazon Web Services
--     Region from the read replica, specify a valid DB instance ARN. For
--     more information, see
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
        publiclyAccessible = Prelude.Nothing,
        autoMinorVersionUpgrade = Prelude.Nothing,
        dbSubnetGroupName = Prelude.Nothing,
        monitoringRoleArn = Prelude.Nothing,
        iops = Prelude.Nothing,
        domain = Prelude.Nothing,
        replicaMode = Prelude.Nothing,
        monitoringInterval = Prelude.Nothing,
        preSignedUrl = Prelude.Nothing,
        processorFeatures = Prelude.Nothing,
        dbInstanceClass = Prelude.Nothing,
        performanceInsightsRetentionPeriod =
          Prelude.Nothing,
        destinationRegion = Prelude.Nothing,
        maxAllocatedStorage = Prelude.Nothing,
        enablePerformanceInsights = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        dbParameterGroupName = Prelude.Nothing,
        availabilityZone = Prelude.Nothing,
        performanceInsightsKMSKeyId = Prelude.Nothing,
        vpcSecurityGroupIds = Prelude.Nothing,
        multiAZ = Prelude.Nothing,
        optionGroupName = Prelude.Nothing,
        copyTagsToSnapshot = Prelude.Nothing,
        domainIAMRoleName = Prelude.Nothing,
        tags = Prelude.Nothing,
        port = Prelude.Nothing,
        enableIAMDatabaseAuthentication =
          Prelude.Nothing,
        useDefaultProcessorFeatures = Prelude.Nothing,
        storageType = Prelude.Nothing,
        enableCloudwatchLogsExports = Prelude.Nothing,
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

-- | A value that indicates whether minor engine upgrades are applied
-- automatically to the read replica during the maintenance window.
--
-- Default: Inherits from the source DB instance
createDBInstanceReadReplica_autoMinorVersionUpgrade :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Bool)
createDBInstanceReadReplica_autoMinorVersionUpgrade = Lens.lens (\CreateDBInstanceReadReplica' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@CreateDBInstanceReadReplica' {} a -> s {autoMinorVersionUpgrade = a} :: CreateDBInstanceReadReplica)

-- | Specifies a DB subnet group for the DB instance. The new DB instance is
-- created in the VPC associated with the DB subnet group. If no DB subnet
-- group is specified, then the new DB instance isn\'t created in a VPC.
--
-- Constraints:
--
-- -   Can only be specified if the source DB instance identifier specifies
--     a DB instance in another Amazon Web Services Region.
--
-- -   If supplied, must match the name of an existing DBSubnetGroup.
--
-- -   The specified DB subnet group must be in the same Amazon Web
--     Services Region in which the operation is running.
--
-- -   All read replicas in one Amazon Web Services Region that are created
--     from the same source DB instance must either:>
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

-- | The amount of Provisioned IOPS (input\/output operations per second) to
-- be initially allocated for the DB instance.
createDBInstanceReadReplica_iops :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Int)
createDBInstanceReadReplica_iops = Lens.lens (\CreateDBInstanceReadReplica' {iops} -> iops) (\s@CreateDBInstanceReadReplica' {} a -> s {iops = a} :: CreateDBInstanceReadReplica)

-- | The Active Directory directory ID to create the DB instance in.
-- Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB
-- instances can be created in an Active Directory Domain.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon RDS User Guide/.
createDBInstanceReadReplica_domain :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Text)
createDBInstanceReadReplica_domain = Lens.lens (\CreateDBInstanceReadReplica' {domain} -> domain) (\s@CreateDBInstanceReadReplica' {} a -> s {domain = a} :: CreateDBInstanceReadReplica)

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

-- | The URL that contains a Signature Version 4 signed request for the
-- @CreateDBInstanceReadReplica@ API action in the source Amazon Web
-- Services Region that contains the source DB instance.
--
-- You must specify this parameter when you create an encrypted read
-- replica from another Amazon Web Services Region by using the Amazon RDS
-- API. Don\'t specify @PreSignedUrl@ when you are creating an encrypted
-- read replica in the same Amazon Web Services Region.
--
-- The presigned URL must be a valid request for the
-- @CreateDBInstanceReadReplica@ API action that can be executed in the
-- source Amazon Web Services Region that contains the encrypted source DB
-- instance. The presigned URL request must contain the following parameter
-- values:
--
-- -   @DestinationRegion@ - The Amazon Web Services Region that the
--     encrypted read replica is created in. This Amazon Web Services
--     Region is the same one where the @CreateDBInstanceReadReplica@
--     action is called that contains this presigned URL.
--
--     For example, if you create an encrypted DB instance in the us-west-1
--     Amazon Web Services Region, from a source DB instance in the
--     us-east-2 Amazon Web Services Region, then you call the
--     @CreateDBInstanceReadReplica@ action in the us-east-1 Amazon Web
--     Services Region and provide a presigned URL that contains a call to
--     the @CreateDBInstanceReadReplica@ action in the us-west-2 Amazon Web
--     Services Region. For this example, the @DestinationRegion@ in the
--     presigned URL must be set to the us-east-1 Amazon Web Services
--     Region.
--
-- -   @KmsKeyId@ - The Amazon Web Services KMS key identifier for the key
--     to use to encrypt the read replica in the destination Amazon Web
--     Services Region. This is the same identifier for both the
--     @CreateDBInstanceReadReplica@ action that is called in the
--     destination Amazon Web Services Region, and the action contained in
--     the presigned URL.
--
-- -   @SourceDBInstanceIdentifier@ - The DB instance identifier for the
--     encrypted DB instance to be replicated. This identifier must be in
--     the Amazon Resource Name (ARN) format for the source Amazon Web
--     Services Region. For example, if you are creating an encrypted read
--     replica from a DB instance in the us-west-2 Amazon Web Services
--     Region, then your @SourceDBInstanceIdentifier@ looks like the
--     following example:
--     @arn:aws:rds:us-west-2:123456789012:instance:mysql-instance1-20161115@.
--
-- To learn how to generate a Signature Version 4 signed request, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/sigv4-query-string-auth.html Authenticating Requests: Using Query Parameters (Amazon Web Services Signature Version 4)>
-- and
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 Signing Process>.
--
-- If you are using an Amazon Web Services SDK tool or the CLI, you can
-- specify @SourceRegion@ (or @--source-region@ for the CLI) instead of
-- specifying @PreSignedUrl@ manually. Specifying @SourceRegion@
-- autogenerates a presigned URL that is a valid request for the operation
-- that can be executed in the source Amazon Web Services Region.
--
-- @SourceRegion@ isn\'t supported for SQL Server, because SQL Server on
-- Amazon RDS doesn\'t support cross-region read replicas.
createDBInstanceReadReplica_preSignedUrl :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Text)
createDBInstanceReadReplica_preSignedUrl = Lens.lens (\CreateDBInstanceReadReplica' {preSignedUrl} -> preSignedUrl) (\s@CreateDBInstanceReadReplica' {} a -> s {preSignedUrl = a} :: CreateDBInstanceReadReplica)

-- | The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
createDBInstanceReadReplica_processorFeatures :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe [ProcessorFeature])
createDBInstanceReadReplica_processorFeatures = Lens.lens (\CreateDBInstanceReadReplica' {processorFeatures} -> processorFeatures) (\s@CreateDBInstanceReadReplica' {} a -> s {processorFeatures = a} :: CreateDBInstanceReadReplica) Prelude.. Lens.mapping Lens.coerced

-- | The compute and memory capacity of the read replica, for example,
-- @db.m4.large@. Not all DB instance classes are available in all Amazon
-- Web Services Regions, or for all database engines. For the full list of
-- DB instance classes, and availability for your engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
-- in the /Amazon RDS User Guide./
--
-- Default: Inherits from the source DB instance.
createDBInstanceReadReplica_dbInstanceClass :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Text)
createDBInstanceReadReplica_dbInstanceClass = Lens.lens (\CreateDBInstanceReadReplica' {dbInstanceClass} -> dbInstanceClass) (\s@CreateDBInstanceReadReplica' {} a -> s {dbInstanceClass = a} :: CreateDBInstanceReadReplica)

-- | The amount of time, in days, to retain Performance Insights data. Valid
-- values are 7 or 731 (2 years).
createDBInstanceReadReplica_performanceInsightsRetentionPeriod :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Int)
createDBInstanceReadReplica_performanceInsightsRetentionPeriod = Lens.lens (\CreateDBInstanceReadReplica' {performanceInsightsRetentionPeriod} -> performanceInsightsRetentionPeriod) (\s@CreateDBInstanceReadReplica' {} a -> s {performanceInsightsRetentionPeriod = a} :: CreateDBInstanceReadReplica)

-- | Pseudo-parameter used when populating the @PreSignedUrl@ of a
-- cross-region @CreateDBInstanceReadReplica@ request. To replicate from
-- region @SRC@ to region @DST@, send a request to region @DST@. In that
-- request, pass a @PreSignedUrl@ for region @SRC@ with @DestinationRegion@
-- set to region @DST@.
createDBInstanceReadReplica_destinationRegion :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Text)
createDBInstanceReadReplica_destinationRegion = Lens.lens (\CreateDBInstanceReadReplica' {destinationRegion} -> destinationRegion) (\s@CreateDBInstanceReadReplica' {} a -> s {destinationRegion = a} :: CreateDBInstanceReadReplica)

-- | The upper limit in gibibytes (GiB) to which Amazon RDS can automatically
-- scale the storage of the DB instance.
--
-- For more information about this setting, including limitations that
-- apply to it, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
-- in the /Amazon RDS User Guide/.
createDBInstanceReadReplica_maxAllocatedStorage :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Int)
createDBInstanceReadReplica_maxAllocatedStorage = Lens.lens (\CreateDBInstanceReadReplica' {maxAllocatedStorage} -> maxAllocatedStorage) (\s@CreateDBInstanceReadReplica' {} a -> s {maxAllocatedStorage = a} :: CreateDBInstanceReadReplica)

-- | A value that indicates whether to enable Performance Insights for the
-- read replica.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights>
-- in the /Amazon RDS User Guide/.
createDBInstanceReadReplica_enablePerformanceInsights :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Bool)
createDBInstanceReadReplica_enablePerformanceInsights = Lens.lens (\CreateDBInstanceReadReplica' {enablePerformanceInsights} -> enablePerformanceInsights) (\s@CreateDBInstanceReadReplica' {} a -> s {enablePerformanceInsights = a} :: CreateDBInstanceReadReplica)

-- | The Amazon Web Services KMS key identifier for an encrypted read
-- replica.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the Amazon Web Services KMS CMK.
--
-- If you create an encrypted read replica in the same Amazon Web Services
-- Region as the source DB instance, then do not specify a value for this
-- parameter. A read replica in the same Region is always encrypted with
-- the same Amazon Web Services KMS CMK as the source DB instance.
--
-- If you create an encrypted read replica in a different Amazon Web
-- Services Region, then you must specify a Amazon Web Services KMS key
-- identifier for the destination Amazon Web Services Region. Amazon Web
-- Services KMS CMKs are specific to the Amazon Web Services Region that
-- they are created in, and you can\'t use CMKs from one Amazon Web
-- Services Region in another Amazon Web Services Region.
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
-- Amazon Web Services Region.
--
-- Example: @us-east-1d@
createDBInstanceReadReplica_availabilityZone :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Text)
createDBInstanceReadReplica_availabilityZone = Lens.lens (\CreateDBInstanceReadReplica' {availabilityZone} -> availabilityZone) (\s@CreateDBInstanceReadReplica' {} a -> s {availabilityZone = a} :: CreateDBInstanceReadReplica)

-- | The Amazon Web Services KMS key identifier for encryption of Performance
-- Insights data.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the Amazon Web Services KMS customer master key
-- (CMK).
--
-- If you do not specify a value for @PerformanceInsightsKMSKeyId@, then
-- Amazon RDS uses your default CMK. There is a default CMK for your Amazon
-- Web Services account. Your Amazon Web Services account has a different
-- default CMK for each Amazon Web Services Region.
createDBInstanceReadReplica_performanceInsightsKMSKeyId :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Text)
createDBInstanceReadReplica_performanceInsightsKMSKeyId = Lens.lens (\CreateDBInstanceReadReplica' {performanceInsightsKMSKeyId} -> performanceInsightsKMSKeyId) (\s@CreateDBInstanceReadReplica' {} a -> s {performanceInsightsKMSKeyId = a} :: CreateDBInstanceReadReplica)

-- | A list of EC2 VPC security groups to associate with the read replica.
--
-- Default: The default EC2 VPC security group for the DB subnet group\'s
-- VPC.
createDBInstanceReadReplica_vpcSecurityGroupIds :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe [Prelude.Text])
createDBInstanceReadReplica_vpcSecurityGroupIds = Lens.lens (\CreateDBInstanceReadReplica' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@CreateDBInstanceReadReplica' {} a -> s {vpcSecurityGroupIds = a} :: CreateDBInstanceReadReplica) Prelude.. Lens.mapping Lens.coerced

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

-- | The option group the DB instance is associated with. If omitted, the
-- option group associated with the source instance is used.
--
-- For SQL Server, you must use the option group associated with the source
-- instance.
createDBInstanceReadReplica_optionGroupName :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Text)
createDBInstanceReadReplica_optionGroupName = Lens.lens (\CreateDBInstanceReadReplica' {optionGroupName} -> optionGroupName) (\s@CreateDBInstanceReadReplica' {} a -> s {optionGroupName = a} :: CreateDBInstanceReadReplica)

-- | A value that indicates whether to copy all tags from the read replica to
-- snapshots of the read replica. By default, tags are not copied.
createDBInstanceReadReplica_copyTagsToSnapshot :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Bool)
createDBInstanceReadReplica_copyTagsToSnapshot = Lens.lens (\CreateDBInstanceReadReplica' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@CreateDBInstanceReadReplica' {} a -> s {copyTagsToSnapshot = a} :: CreateDBInstanceReadReplica)

-- | Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
createDBInstanceReadReplica_domainIAMRoleName :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Text)
createDBInstanceReadReplica_domainIAMRoleName = Lens.lens (\CreateDBInstanceReadReplica' {domainIAMRoleName} -> domainIAMRoleName) (\s@CreateDBInstanceReadReplica' {} a -> s {domainIAMRoleName = a} :: CreateDBInstanceReadReplica)

-- | Undocumented member.
createDBInstanceReadReplica_tags :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe [Tag])
createDBInstanceReadReplica_tags = Lens.lens (\CreateDBInstanceReadReplica' {tags} -> tags) (\s@CreateDBInstanceReadReplica' {} a -> s {tags = a} :: CreateDBInstanceReadReplica) Prelude.. Lens.mapping Lens.coerced

-- | The port number that the DB instance uses for connections.
--
-- Default: Inherits from the source DB instance
--
-- Valid Values: @1150-65535@
createDBInstanceReadReplica_port :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Int)
createDBInstanceReadReplica_port = Lens.lens (\CreateDBInstanceReadReplica' {port} -> port) (\s@CreateDBInstanceReadReplica' {} a -> s {port = a} :: CreateDBInstanceReadReplica)

-- | A value that indicates whether to enable mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts. By
-- default, mapping is disabled.
--
-- For more information about IAM database authentication, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
-- in the /Amazon RDS User Guide./
createDBInstanceReadReplica_enableIAMDatabaseAuthentication :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Bool)
createDBInstanceReadReplica_enableIAMDatabaseAuthentication = Lens.lens (\CreateDBInstanceReadReplica' {enableIAMDatabaseAuthentication} -> enableIAMDatabaseAuthentication) (\s@CreateDBInstanceReadReplica' {} a -> s {enableIAMDatabaseAuthentication = a} :: CreateDBInstanceReadReplica)

-- | A value that indicates whether the DB instance class of the DB instance
-- uses its default processor features.
createDBInstanceReadReplica_useDefaultProcessorFeatures :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Bool)
createDBInstanceReadReplica_useDefaultProcessorFeatures = Lens.lens (\CreateDBInstanceReadReplica' {useDefaultProcessorFeatures} -> useDefaultProcessorFeatures) (\s@CreateDBInstanceReadReplica' {} a -> s {useDefaultProcessorFeatures = a} :: CreateDBInstanceReadReplica)

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

-- | The list of logs that the new DB instance is to export to CloudWatch
-- Logs. The values in the list depend on the DB engine being used. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon RDS User Guide/.
createDBInstanceReadReplica_enableCloudwatchLogsExports :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe [Prelude.Text])
createDBInstanceReadReplica_enableCloudwatchLogsExports = Lens.lens (\CreateDBInstanceReadReplica' {enableCloudwatchLogsExports} -> enableCloudwatchLogsExports) (\s@CreateDBInstanceReadReplica' {} a -> s {enableCloudwatchLogsExports = a} :: CreateDBInstanceReadReplica) Prelude.. Lens.mapping Lens.coerced

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
-- -   If the source DB instance is in the same Amazon Web Services Region
--     as the read replica, specify a valid DB instance identifier.
--
-- -   If the source DB instance is in a different Amazon Web Services
--     Region from the read replica, specify a valid DB instance ARN. For
--     more information, see
--     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS>
--     in the /Amazon RDS User Guide/. This doesn\'t apply to SQL Server,
--     which doesn\'t support cross-region replicas.
createDBInstanceReadReplica_sourceDBInstanceIdentifier :: Lens.Lens' CreateDBInstanceReadReplica Prelude.Text
createDBInstanceReadReplica_sourceDBInstanceIdentifier = Lens.lens (\CreateDBInstanceReadReplica' {sourceDBInstanceIdentifier} -> sourceDBInstanceIdentifier) (\s@CreateDBInstanceReadReplica' {} a -> s {sourceDBInstanceIdentifier = a} :: CreateDBInstanceReadReplica)

instance Core.AWSRequest CreateDBInstanceReadReplica where
  type
    AWSResponse CreateDBInstanceReadReplica =
      CreateDBInstanceReadReplicaResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateDBInstanceReadReplicaResult"
      ( \s h x ->
          CreateDBInstanceReadReplicaResponse'
            Prelude.<$> (x Core..@? "DBInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDBInstanceReadReplica where
  hashWithSalt salt' CreateDBInstanceReadReplica' {..} =
    salt'
      `Prelude.hashWithSalt` sourceDBInstanceIdentifier
      `Prelude.hashWithSalt` dbInstanceIdentifier
      `Prelude.hashWithSalt` enableCloudwatchLogsExports
      `Prelude.hashWithSalt` storageType
      `Prelude.hashWithSalt` useDefaultProcessorFeatures
      `Prelude.hashWithSalt` enableIAMDatabaseAuthentication
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` domainIAMRoleName
      `Prelude.hashWithSalt` copyTagsToSnapshot
      `Prelude.hashWithSalt` optionGroupName
      `Prelude.hashWithSalt` multiAZ
      `Prelude.hashWithSalt` vpcSecurityGroupIds
      `Prelude.hashWithSalt` performanceInsightsKMSKeyId
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` dbParameterGroupName
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` enablePerformanceInsights
      `Prelude.hashWithSalt` maxAllocatedStorage
      `Prelude.hashWithSalt` destinationRegion
      `Prelude.hashWithSalt` performanceInsightsRetentionPeriod
      `Prelude.hashWithSalt` dbInstanceClass
      `Prelude.hashWithSalt` processorFeatures
      `Prelude.hashWithSalt` preSignedUrl
      `Prelude.hashWithSalt` monitoringInterval
      `Prelude.hashWithSalt` replicaMode
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` monitoringRoleArn
      `Prelude.hashWithSalt` dbSubnetGroupName
      `Prelude.hashWithSalt` autoMinorVersionUpgrade
      `Prelude.hashWithSalt` publiclyAccessible
      `Prelude.hashWithSalt` deletionProtection

instance Prelude.NFData CreateDBInstanceReadReplica where
  rnf CreateDBInstanceReadReplica' {..} =
    Prelude.rnf deletionProtection
      `Prelude.seq` Prelude.rnf sourceDBInstanceIdentifier
      `Prelude.seq` Prelude.rnf dbInstanceIdentifier
      `Prelude.seq` Prelude.rnf enableCloudwatchLogsExports
      `Prelude.seq` Prelude.rnf storageType
      `Prelude.seq` Prelude.rnf useDefaultProcessorFeatures
      `Prelude.seq` Prelude.rnf enableIAMDatabaseAuthentication
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf domainIAMRoleName
      `Prelude.seq` Prelude.rnf copyTagsToSnapshot
      `Prelude.seq` Prelude.rnf optionGroupName
      `Prelude.seq` Prelude.rnf multiAZ
      `Prelude.seq` Prelude.rnf vpcSecurityGroupIds
      `Prelude.seq` Prelude.rnf performanceInsightsKMSKeyId
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf dbParameterGroupName
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf enablePerformanceInsights
      `Prelude.seq` Prelude.rnf maxAllocatedStorage
      `Prelude.seq` Prelude.rnf destinationRegion
      `Prelude.seq` Prelude.rnf performanceInsightsRetentionPeriod
      `Prelude.seq` Prelude.rnf dbInstanceClass
      `Prelude.seq` Prelude.rnf processorFeatures
      `Prelude.seq` Prelude.rnf preSignedUrl
      `Prelude.seq` Prelude.rnf monitoringInterval
      `Prelude.seq` Prelude.rnf replicaMode
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf iops
      `Prelude.seq` Prelude.rnf monitoringRoleArn
      `Prelude.seq` Prelude.rnf dbSubnetGroupName
      `Prelude.seq` Prelude.rnf autoMinorVersionUpgrade
      `Prelude.seq` Prelude.rnf publiclyAccessible

instance Core.ToHeaders CreateDBInstanceReadReplica where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateDBInstanceReadReplica where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateDBInstanceReadReplica where
  toQuery CreateDBInstanceReadReplica' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "CreateDBInstanceReadReplica" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "DeletionProtection" Core.=: deletionProtection,
        "PubliclyAccessible" Core.=: publiclyAccessible,
        "AutoMinorVersionUpgrade"
          Core.=: autoMinorVersionUpgrade,
        "DBSubnetGroupName" Core.=: dbSubnetGroupName,
        "MonitoringRoleArn" Core.=: monitoringRoleArn,
        "Iops" Core.=: iops,
        "Domain" Core.=: domain,
        "ReplicaMode" Core.=: replicaMode,
        "MonitoringInterval" Core.=: monitoringInterval,
        "PreSignedUrl" Core.=: preSignedUrl,
        "ProcessorFeatures"
          Core.=: Core.toQuery
            ( Core.toQueryList "ProcessorFeature"
                Prelude.<$> processorFeatures
            ),
        "DBInstanceClass" Core.=: dbInstanceClass,
        "PerformanceInsightsRetentionPeriod"
          Core.=: performanceInsightsRetentionPeriod,
        "DestinationRegion" Core.=: destinationRegion,
        "MaxAllocatedStorage" Core.=: maxAllocatedStorage,
        "EnablePerformanceInsights"
          Core.=: enablePerformanceInsights,
        "KmsKeyId" Core.=: kmsKeyId,
        "DBParameterGroupName" Core.=: dbParameterGroupName,
        "AvailabilityZone" Core.=: availabilityZone,
        "PerformanceInsightsKMSKeyId"
          Core.=: performanceInsightsKMSKeyId,
        "VpcSecurityGroupIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "VpcSecurityGroupId"
                Prelude.<$> vpcSecurityGroupIds
            ),
        "MultiAZ" Core.=: multiAZ,
        "OptionGroupName" Core.=: optionGroupName,
        "CopyTagsToSnapshot" Core.=: copyTagsToSnapshot,
        "DomainIAMRoleName" Core.=: domainIAMRoleName,
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "Tag" Prelude.<$> tags),
        "Port" Core.=: port,
        "EnableIAMDatabaseAuthentication"
          Core.=: enableIAMDatabaseAuthentication,
        "UseDefaultProcessorFeatures"
          Core.=: useDefaultProcessorFeatures,
        "StorageType" Core.=: storageType,
        "EnableCloudwatchLogsExports"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> enableCloudwatchLogsExports
            ),
        "DBInstanceIdentifier" Core.=: dbInstanceIdentifier,
        "SourceDBInstanceIdentifier"
          Core.=: sourceDBInstanceIdentifier
      ]

-- | /See:/ 'newCreateDBInstanceReadReplicaResponse' smart constructor.
data CreateDBInstanceReadReplicaResponse = CreateDBInstanceReadReplicaResponse'
  { dbInstance :: Prelude.Maybe DBInstance,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf CreateDBInstanceReadReplicaResponse' {..} =
    Prelude.rnf dbInstance
      `Prelude.seq` Prelude.rnf httpStatus
