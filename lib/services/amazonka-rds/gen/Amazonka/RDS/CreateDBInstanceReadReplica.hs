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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- Amazon Aurora doesn\'t support this operation. Call the
-- @CreateDBInstance@ operation to create a DB instance for an Aurora DB
-- cluster.
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
    createDBInstanceReadReplica_autoMinorVersionUpgrade,
    createDBInstanceReadReplica_availabilityZone,
    createDBInstanceReadReplica_copyTagsToSnapshot,
    createDBInstanceReadReplica_customIamInstanceProfile,
    createDBInstanceReadReplica_dbInstanceClass,
    createDBInstanceReadReplica_dbParameterGroupName,
    createDBInstanceReadReplica_dbSubnetGroupName,
    createDBInstanceReadReplica_deletionProtection,
    createDBInstanceReadReplica_destinationRegion,
    createDBInstanceReadReplica_domain,
    createDBInstanceReadReplica_domainIAMRoleName,
    createDBInstanceReadReplica_enableCloudwatchLogsExports,
    createDBInstanceReadReplica_enableIAMDatabaseAuthentication,
    createDBInstanceReadReplica_enablePerformanceInsights,
    createDBInstanceReadReplica_iops,
    createDBInstanceReadReplica_kmsKeyId,
    createDBInstanceReadReplica_maxAllocatedStorage,
    createDBInstanceReadReplica_monitoringInterval,
    createDBInstanceReadReplica_monitoringRoleArn,
    createDBInstanceReadReplica_multiAZ,
    createDBInstanceReadReplica_networkType,
    createDBInstanceReadReplica_optionGroupName,
    createDBInstanceReadReplica_performanceInsightsKMSKeyId,
    createDBInstanceReadReplica_performanceInsightsRetentionPeriod,
    createDBInstanceReadReplica_port,
    createDBInstanceReadReplica_preSignedUrl,
    createDBInstanceReadReplica_processorFeatures,
    createDBInstanceReadReplica_publiclyAccessible,
    createDBInstanceReadReplica_replicaMode,
    createDBInstanceReadReplica_storageThroughput,
    createDBInstanceReadReplica_storageType,
    createDBInstanceReadReplica_tags,
    createDBInstanceReadReplica_useDefaultProcessorFeatures,
    createDBInstanceReadReplica_vpcSecurityGroupIds,
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDBInstanceReadReplica' smart constructor.
data CreateDBInstanceReadReplica = CreateDBInstanceReadReplica'
  { -- | A value that indicates whether minor engine upgrades are applied
    -- automatically to the read replica during the maintenance window.
    --
    -- This setting doesn\'t apply to RDS Custom.
    --
    -- Default: Inherits from the source DB instance
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The Availability Zone (AZ) where the read replica will be created.
    --
    -- Default: A random, system-chosen Availability Zone in the endpoint\'s
    -- Amazon Web Services Region.
    --
    -- Example: @us-east-1d@
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to copy all tags from the read replica to
    -- snapshots of the read replica. By default, tags are not copied.
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | The instance profile associated with the underlying Amazon EC2 instance
    -- of an RDS Custom DB instance. The instance profile must meet the
    -- following requirements:
    --
    -- -   The profile must exist in your account.
    --
    -- -   The profile must have an IAM role that Amazon EC2 has permissions to
    --     assume.
    --
    -- -   The instance profile name and the associated IAM role name must
    --     start with the prefix @AWSRDSCustom@.
    --
    -- For the list of permissions required for the IAM role, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/custom-setup-orcl.html#custom-setup-orcl.iam-vpc Configure IAM and your VPC>
    -- in the /Amazon RDS User Guide/.
    --
    -- This setting is required for RDS Custom.
    customIamInstanceProfile :: Prelude.Maybe Prelude.Text,
    -- | The compute and memory capacity of the read replica, for example
    -- db.m4.large. Not all DB instance classes are available in all Amazon Web
    -- Services Regions, or for all database engines. For the full list of DB
    -- instance classes, and availability for your engine, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
    -- in the /Amazon RDS User Guide/.
    --
    -- Default: Inherits from the source DB instance.
    dbInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | The name of the DB parameter group to associate with this DB instance.
    --
    -- If you do not specify a value for @DBParameterGroupName@, then Amazon
    -- RDS uses the @DBParameterGroup@ of source DB instance for a same Region
    -- read replica, or the default @DBParameterGroup@ for the specified DB
    -- engine for a cross-Region read replica.
    --
    -- Specifying a parameter group for this operation is only supported for
    -- MySQL and Oracle DB instances. It isn\'t supported for RDS Custom.
    --
    -- Constraints:
    --
    -- -   Must be 1 to 255 letters, numbers, or hyphens.
    --
    -- -   First character must be a letter
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens
    dbParameterGroupName :: Prelude.Maybe Prelude.Text,
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
    -- Example: @mydbsubnetgroup@
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the DB instance has deletion protection
    -- enabled. The database can\'t be deleted when deletion protection is
    -- enabled. By default, deletion protection isn\'t enabled. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | Pseudo-parameter used when populating the @PreSignedUrl@ of a
    -- cross-region @CreateDBInstanceReadReplica@ request. To replicate from
    -- region @SRC@ to region @DST@, send a request to region @DST@. In that
    -- request, pass a @PreSignedUrl@ for region @SRC@ with @DestinationRegion@
    -- set to region @DST@.
    destinationRegion :: Prelude.Maybe Prelude.Text,
    -- | The Active Directory directory ID to create the DB instance in.
    -- Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB
    -- instances can be created in an Active Directory Domain.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
    -- in the /Amazon RDS User Guide/.
    --
    -- This setting doesn\'t apply to RDS Custom.
    domain :: Prelude.Maybe Prelude.Text,
    -- | Specify the name of the IAM role to be used when making API calls to the
    -- Directory Service.
    --
    -- This setting doesn\'t apply to RDS Custom.
    domainIAMRoleName :: Prelude.Maybe Prelude.Text,
    -- | The list of logs that the new DB instance is to export to CloudWatch
    -- Logs. The values in the list depend on the DB engine being used. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
    -- in the /Amazon RDS User Guide/.
    --
    -- This setting doesn\'t apply to RDS Custom.
    enableCloudwatchLogsExports :: Prelude.Maybe [Prelude.Text],
    -- | A value that indicates whether to enable mapping of Amazon Web Services
    -- Identity and Access Management (IAM) accounts to database accounts. By
    -- default, mapping isn\'t enabled.
    --
    -- For more information about IAM database authentication, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
    -- in the /Amazon RDS User Guide/.
    --
    -- This setting doesn\'t apply to RDS Custom.
    enableIAMDatabaseAuthentication :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether to enable Performance Insights for the
    -- read replica.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights>
    -- in the /Amazon RDS User Guide/.
    --
    -- This setting doesn\'t apply to RDS Custom.
    enablePerformanceInsights :: Prelude.Maybe Prelude.Bool,
    -- | The amount of Provisioned IOPS (input\/output operations per second) to
    -- be initially allocated for the DB instance.
    iops :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Web Services KMS key identifier for an encrypted read
    -- replica.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the KMS key.
    --
    -- If you create an encrypted read replica in the same Amazon Web Services
    -- Region as the source DB instance, then do not specify a value for this
    -- parameter. A read replica in the same Amazon Web Services Region is
    -- always encrypted with the same KMS key as the source DB instance.
    --
    -- If you create an encrypted read replica in a different Amazon Web
    -- Services Region, then you must specify a KMS key identifier for the
    -- destination Amazon Web Services Region. KMS keys are specific to the
    -- Amazon Web Services Region that they are created in, and you can\'t use
    -- KMS keys from one Amazon Web Services Region in another Amazon Web
    -- Services Region.
    --
    -- You can\'t create an encrypted read replica from an unencrypted DB
    -- instance.
    --
    -- This setting doesn\'t apply to RDS Custom, which uses the same KMS key
    -- as the primary replica.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The upper limit in gibibytes (GiB) to which Amazon RDS can automatically
    -- scale the storage of the DB instance.
    --
    -- For more information about this setting, including limitations that
    -- apply to it, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
    -- in the /Amazon RDS User Guide/.
    maxAllocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | The interval, in seconds, between points when Enhanced Monitoring
    -- metrics are collected for the read replica. To disable collecting
    -- Enhanced Monitoring metrics, specify 0. The default is 0.
    --
    -- If @MonitoringRoleArn@ is specified, then you must also set
    -- @MonitoringInterval@ to a value other than 0.
    --
    -- This setting doesn\'t apply to RDS Custom.
    --
    -- Valid Values: @0, 1, 5, 10, 15, 30, 60@
    monitoringInterval :: Prelude.Maybe Prelude.Int,
    -- | The ARN for the IAM role that permits RDS to send enhanced monitoring
    -- metrics to Amazon CloudWatch Logs. For example,
    -- @arn:aws:iam:123456789012:role\/emaccess@. For information on creating a
    -- monitoring role, go to
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.html#USER_Monitoring.OS.IAMRole To create an IAM role for Amazon RDS Enhanced Monitoring>
    -- in the /Amazon RDS User Guide/.
    --
    -- If @MonitoringInterval@ is set to a value other than 0, then you must
    -- supply a @MonitoringRoleArn@ value.
    --
    -- This setting doesn\'t apply to RDS Custom.
    monitoringRoleArn :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the read replica is in a Multi-AZ
    -- deployment.
    --
    -- You can create a read replica as a Multi-AZ DB instance. RDS creates a
    -- standby of your replica in another Availability Zone for failover
    -- support for the replica. Creating your read replica as a Multi-AZ DB
    -- instance is independent of whether the source database is a Multi-AZ DB
    -- instance.
    --
    -- This setting doesn\'t apply to RDS Custom.
    multiAZ :: Prelude.Maybe Prelude.Bool,
    -- | The network type of the DB instance.
    --
    -- Valid values:
    --
    -- -   @IPV4@
    --
    -- -   @DUAL@
    --
    -- The network type is determined by the @DBSubnetGroup@ specified for read
    -- replica. A @DBSubnetGroup@ can support only the IPv4 protocol or the
    -- IPv4 and the IPv6 protocols (@DUAL@).
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html Working with a DB instance in a VPC>
    -- in the /Amazon RDS User Guide./
    networkType :: Prelude.Maybe Prelude.Text,
    -- | The option group the DB instance is associated with. If omitted, the
    -- option group associated with the source instance is used.
    --
    -- For SQL Server, you must use the option group associated with the source
    -- instance.
    --
    -- This setting doesn\'t apply to RDS Custom.
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services KMS key identifier for encryption of Performance
    -- Insights data.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the KMS key.
    --
    -- If you do not specify a value for @PerformanceInsightsKMSKeyId@, then
    -- Amazon RDS uses your default KMS key. There is a default KMS key for
    -- your Amazon Web Services account. Your Amazon Web Services account has a
    -- different default KMS key for each Amazon Web Services Region.
    --
    -- This setting doesn\'t apply to RDS Custom.
    performanceInsightsKMSKeyId :: Prelude.Maybe Prelude.Text,
    -- | The number of days to retain Performance Insights data. The default is 7
    -- days. The following values are valid:
    --
    -- -   7
    --
    -- -   /month/ * 31, where /month/ is a number of months from 1-23
    --
    -- -   731
    --
    -- For example, the following values are valid:
    --
    -- -   93 (3 months * 31)
    --
    -- -   341 (11 months * 31)
    --
    -- -   589 (19 months * 31)
    --
    -- -   731
    --
    -- If you specify a retention period such as 94, which isn\'t a valid
    -- value, RDS issues an error.
    --
    -- This setting doesn\'t apply to RDS Custom.
    performanceInsightsRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The port number that the DB instance uses for connections.
    --
    -- Default: Inherits from the source DB instance
    --
    -- Valid Values: @1150-65535@
    port :: Prelude.Maybe Prelude.Int,
    -- | When you are creating a read replica from one Amazon Web Services
    -- GovCloud (US) Region to another or from one China Amazon Web Services
    -- Region to another, the URL that contains a Signature Version 4 signed
    -- request for the @CreateDBInstanceReadReplica@ API operation in the
    -- source Amazon Web Services Region that contains the source DB instance.
    --
    -- This setting applies only to Amazon Web Services GovCloud (US) Regions
    -- and China Amazon Web Services Regions. It\'s ignored in other Amazon Web
    -- Services Regions.
    --
    -- You must specify this parameter when you create an encrypted read
    -- replica from another Amazon Web Services Region by using the Amazon RDS
    -- API. Don\'t specify @PreSignedUrl@ when you are creating an encrypted
    -- read replica in the same Amazon Web Services Region.
    --
    -- The presigned URL must be a valid request for the
    -- @CreateDBInstanceReadReplica@ API operation that can run in the source
    -- Amazon Web Services Region that contains the encrypted source DB
    -- instance. The presigned URL request must contain the following parameter
    -- values:
    --
    -- -   @DestinationRegion@ - The Amazon Web Services Region that the
    --     encrypted read replica is created in. This Amazon Web Services
    --     Region is the same one where the @CreateDBInstanceReadReplica@
    --     operation is called that contains this presigned URL.
    --
    --     For example, if you create an encrypted DB instance in the us-west-1
    --     Amazon Web Services Region, from a source DB instance in the
    --     us-east-2 Amazon Web Services Region, then you call the
    --     @CreateDBInstanceReadReplica@ operation in the us-east-1 Amazon Web
    --     Services Region and provide a presigned URL that contains a call to
    --     the @CreateDBInstanceReadReplica@ operation in the us-west-2 Amazon
    --     Web Services Region. For this example, the @DestinationRegion@ in
    --     the presigned URL must be set to the us-east-1 Amazon Web Services
    --     Region.
    --
    -- -   @KmsKeyId@ - The KMS key identifier for the key to use to encrypt
    --     the read replica in the destination Amazon Web Services Region. This
    --     is the same identifier for both the @CreateDBInstanceReadReplica@
    --     operation that is called in the destination Amazon Web Services
    --     Region, and the operation contained in the presigned URL.
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
    -- that can run in the source Amazon Web Services Region.
    --
    -- @SourceRegion@ isn\'t supported for SQL Server, because Amazon RDS for
    -- SQL Server doesn\'t support cross-Region read replicas.
    --
    -- This setting doesn\'t apply to RDS Custom.
    preSignedUrl :: Prelude.Maybe Prelude.Text,
    -- | The number of CPU cores and the number of threads per core for the DB
    -- instance class of the DB instance.
    --
    -- This setting doesn\'t apply to RDS Custom.
    processorFeatures :: Prelude.Maybe [ProcessorFeature],
    -- | A value that indicates whether the DB instance is publicly accessible.
    --
    -- When the DB cluster is publicly accessible, its Domain Name System (DNS)
    -- endpoint resolves to the private IP address from within the DB
    -- cluster\'s virtual private cloud (VPC). It resolves to the public IP
    -- address from outside of the DB cluster\'s VPC. Access to the DB cluster
    -- is ultimately controlled by the security group it uses. That public
    -- access isn\'t permitted if the security group assigned to the DB cluster
    -- doesn\'t permit it.
    --
    -- When the DB instance isn\'t publicly accessible, it is an internal DB
    -- instance with a DNS name that resolves to a private IP address.
    --
    -- For more information, see CreateDBInstance.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | The open mode of the replica database: mounted or read-only.
    --
    -- This parameter is only supported for Oracle DB instances.
    --
    -- Mounted DB replicas are included in Oracle Database Enterprise Edition.
    -- The main use case for mounted replicas is cross-Region disaster
    -- recovery. The primary database doesn\'t use Active Data Guard to
    -- transmit information to the mounted replica. Because it doesn\'t accept
    -- user connections, a mounted replica can\'t serve a read-only workload.
    --
    -- You can create a combination of mounted and read-only DB replicas for
    -- the same primary DB instance. For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Working with Oracle Read Replicas for Amazon RDS>
    -- in the /Amazon RDS User Guide/.
    --
    -- For RDS Custom, you must specify this parameter and set it to @mounted@.
    -- The value won\'t be set by default. After replica creation, you can
    -- manage the open mode manually.
    replicaMode :: Prelude.Maybe ReplicaMode,
    -- | Specifies the storage throughput value for the read replica.
    --
    -- This setting doesn\'t apply to RDS Custom or Amazon Aurora.
    storageThroughput :: Prelude.Maybe Prelude.Int,
    -- | Specifies the storage type to be associated with the read replica.
    --
    -- Valid values: @gp2 | gp3 | io1 | standard@
    --
    -- If you specify @io1@ or @gp3@, you must also include a value for the
    -- @Iops@ parameter.
    --
    -- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
    storageType :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe [Tag],
    -- | A value that indicates whether the DB instance class of the DB instance
    -- uses its default processor features.
    --
    -- This setting doesn\'t apply to RDS Custom.
    useDefaultProcessorFeatures :: Prelude.Maybe Prelude.Bool,
    -- | A list of Amazon EC2 VPC security groups to associate with the read
    -- replica.
    --
    -- This setting doesn\'t apply to RDS Custom.
    --
    -- Default: The default EC2 VPC security group for the DB subnet group\'s
    -- VPC.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
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
    --     PostgreSQL 9.3.5 or later (9.4.7 and higher for cross-Region
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
    --     in the /Amazon RDS User Guide/. This doesn\'t apply to SQL Server or
    --     RDS Custom, which don\'t support cross-Region replicas.
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
-- 'autoMinorVersionUpgrade', 'createDBInstanceReadReplica_autoMinorVersionUpgrade' - A value that indicates whether minor engine upgrades are applied
-- automatically to the read replica during the maintenance window.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- Default: Inherits from the source DB instance
--
-- 'availabilityZone', 'createDBInstanceReadReplica_availabilityZone' - The Availability Zone (AZ) where the read replica will be created.
--
-- Default: A random, system-chosen Availability Zone in the endpoint\'s
-- Amazon Web Services Region.
--
-- Example: @us-east-1d@
--
-- 'copyTagsToSnapshot', 'createDBInstanceReadReplica_copyTagsToSnapshot' - A value that indicates whether to copy all tags from the read replica to
-- snapshots of the read replica. By default, tags are not copied.
--
-- 'customIamInstanceProfile', 'createDBInstanceReadReplica_customIamInstanceProfile' - The instance profile associated with the underlying Amazon EC2 instance
-- of an RDS Custom DB instance. The instance profile must meet the
-- following requirements:
--
-- -   The profile must exist in your account.
--
-- -   The profile must have an IAM role that Amazon EC2 has permissions to
--     assume.
--
-- -   The instance profile name and the associated IAM role name must
--     start with the prefix @AWSRDSCustom@.
--
-- For the list of permissions required for the IAM role, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/custom-setup-orcl.html#custom-setup-orcl.iam-vpc Configure IAM and your VPC>
-- in the /Amazon RDS User Guide/.
--
-- This setting is required for RDS Custom.
--
-- 'dbInstanceClass', 'createDBInstanceReadReplica_dbInstanceClass' - The compute and memory capacity of the read replica, for example
-- db.m4.large. Not all DB instance classes are available in all Amazon Web
-- Services Regions, or for all database engines. For the full list of DB
-- instance classes, and availability for your engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
-- in the /Amazon RDS User Guide/.
--
-- Default: Inherits from the source DB instance.
--
-- 'dbParameterGroupName', 'createDBInstanceReadReplica_dbParameterGroupName' - The name of the DB parameter group to associate with this DB instance.
--
-- If you do not specify a value for @DBParameterGroupName@, then Amazon
-- RDS uses the @DBParameterGroup@ of source DB instance for a same Region
-- read replica, or the default @DBParameterGroup@ for the specified DB
-- engine for a cross-Region read replica.
--
-- Specifying a parameter group for this operation is only supported for
-- MySQL and Oracle DB instances. It isn\'t supported for RDS Custom.
--
-- Constraints:
--
-- -   Must be 1 to 255 letters, numbers, or hyphens.
--
-- -   First character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
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
-- Example: @mydbsubnetgroup@
--
-- 'deletionProtection', 'createDBInstanceReadReplica_deletionProtection' - A value that indicates whether the DB instance has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection isn\'t enabled. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
--
-- 'destinationRegion', 'createDBInstanceReadReplica_destinationRegion' - Pseudo-parameter used when populating the @PreSignedUrl@ of a
-- cross-region @CreateDBInstanceReadReplica@ request. To replicate from
-- region @SRC@ to region @DST@, send a request to region @DST@. In that
-- request, pass a @PreSignedUrl@ for region @SRC@ with @DestinationRegion@
-- set to region @DST@.
--
-- 'domain', 'createDBInstanceReadReplica_domain' - The Active Directory directory ID to create the DB instance in.
-- Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB
-- instances can be created in an Active Directory Domain.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon RDS User Guide/.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- 'domainIAMRoleName', 'createDBInstanceReadReplica_domainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- 'enableCloudwatchLogsExports', 'createDBInstanceReadReplica_enableCloudwatchLogsExports' - The list of logs that the new DB instance is to export to CloudWatch
-- Logs. The values in the list depend on the DB engine being used. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon RDS User Guide/.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- 'enableIAMDatabaseAuthentication', 'createDBInstanceReadReplica_enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts. By
-- default, mapping isn\'t enabled.
--
-- For more information about IAM database authentication, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
-- in the /Amazon RDS User Guide/.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- 'enablePerformanceInsights', 'createDBInstanceReadReplica_enablePerformanceInsights' - A value that indicates whether to enable Performance Insights for the
-- read replica.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights>
-- in the /Amazon RDS User Guide/.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- 'iops', 'createDBInstanceReadReplica_iops' - The amount of Provisioned IOPS (input\/output operations per second) to
-- be initially allocated for the DB instance.
--
-- 'kmsKeyId', 'createDBInstanceReadReplica_kmsKeyId' - The Amazon Web Services KMS key identifier for an encrypted read
-- replica.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key.
--
-- If you create an encrypted read replica in the same Amazon Web Services
-- Region as the source DB instance, then do not specify a value for this
-- parameter. A read replica in the same Amazon Web Services Region is
-- always encrypted with the same KMS key as the source DB instance.
--
-- If you create an encrypted read replica in a different Amazon Web
-- Services Region, then you must specify a KMS key identifier for the
-- destination Amazon Web Services Region. KMS keys are specific to the
-- Amazon Web Services Region that they are created in, and you can\'t use
-- KMS keys from one Amazon Web Services Region in another Amazon Web
-- Services Region.
--
-- You can\'t create an encrypted read replica from an unencrypted DB
-- instance.
--
-- This setting doesn\'t apply to RDS Custom, which uses the same KMS key
-- as the primary replica.
--
-- 'maxAllocatedStorage', 'createDBInstanceReadReplica_maxAllocatedStorage' - The upper limit in gibibytes (GiB) to which Amazon RDS can automatically
-- scale the storage of the DB instance.
--
-- For more information about this setting, including limitations that
-- apply to it, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
-- in the /Amazon RDS User Guide/.
--
-- 'monitoringInterval', 'createDBInstanceReadReplica_monitoringInterval' - The interval, in seconds, between points when Enhanced Monitoring
-- metrics are collected for the read replica. To disable collecting
-- Enhanced Monitoring metrics, specify 0. The default is 0.
--
-- If @MonitoringRoleArn@ is specified, then you must also set
-- @MonitoringInterval@ to a value other than 0.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- Valid Values: @0, 1, 5, 10, 15, 30, 60@
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
-- This setting doesn\'t apply to RDS Custom.
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
-- This setting doesn\'t apply to RDS Custom.
--
-- 'networkType', 'createDBInstanceReadReplica_networkType' - The network type of the DB instance.
--
-- Valid values:
--
-- -   @IPV4@
--
-- -   @DUAL@
--
-- The network type is determined by the @DBSubnetGroup@ specified for read
-- replica. A @DBSubnetGroup@ can support only the IPv4 protocol or the
-- IPv4 and the IPv6 protocols (@DUAL@).
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html Working with a DB instance in a VPC>
-- in the /Amazon RDS User Guide./
--
-- 'optionGroupName', 'createDBInstanceReadReplica_optionGroupName' - The option group the DB instance is associated with. If omitted, the
-- option group associated with the source instance is used.
--
-- For SQL Server, you must use the option group associated with the source
-- instance.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- 'performanceInsightsKMSKeyId', 'createDBInstanceReadReplica_performanceInsightsKMSKeyId' - The Amazon Web Services KMS key identifier for encryption of Performance
-- Insights data.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key.
--
-- If you do not specify a value for @PerformanceInsightsKMSKeyId@, then
-- Amazon RDS uses your default KMS key. There is a default KMS key for
-- your Amazon Web Services account. Your Amazon Web Services account has a
-- different default KMS key for each Amazon Web Services Region.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- 'performanceInsightsRetentionPeriod', 'createDBInstanceReadReplica_performanceInsightsRetentionPeriod' - The number of days to retain Performance Insights data. The default is 7
-- days. The following values are valid:
--
-- -   7
--
-- -   /month/ * 31, where /month/ is a number of months from 1-23
--
-- -   731
--
-- For example, the following values are valid:
--
-- -   93 (3 months * 31)
--
-- -   341 (11 months * 31)
--
-- -   589 (19 months * 31)
--
-- -   731
--
-- If you specify a retention period such as 94, which isn\'t a valid
-- value, RDS issues an error.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- 'port', 'createDBInstanceReadReplica_port' - The port number that the DB instance uses for connections.
--
-- Default: Inherits from the source DB instance
--
-- Valid Values: @1150-65535@
--
-- 'preSignedUrl', 'createDBInstanceReadReplica_preSignedUrl' - When you are creating a read replica from one Amazon Web Services
-- GovCloud (US) Region to another or from one China Amazon Web Services
-- Region to another, the URL that contains a Signature Version 4 signed
-- request for the @CreateDBInstanceReadReplica@ API operation in the
-- source Amazon Web Services Region that contains the source DB instance.
--
-- This setting applies only to Amazon Web Services GovCloud (US) Regions
-- and China Amazon Web Services Regions. It\'s ignored in other Amazon Web
-- Services Regions.
--
-- You must specify this parameter when you create an encrypted read
-- replica from another Amazon Web Services Region by using the Amazon RDS
-- API. Don\'t specify @PreSignedUrl@ when you are creating an encrypted
-- read replica in the same Amazon Web Services Region.
--
-- The presigned URL must be a valid request for the
-- @CreateDBInstanceReadReplica@ API operation that can run in the source
-- Amazon Web Services Region that contains the encrypted source DB
-- instance. The presigned URL request must contain the following parameter
-- values:
--
-- -   @DestinationRegion@ - The Amazon Web Services Region that the
--     encrypted read replica is created in. This Amazon Web Services
--     Region is the same one where the @CreateDBInstanceReadReplica@
--     operation is called that contains this presigned URL.
--
--     For example, if you create an encrypted DB instance in the us-west-1
--     Amazon Web Services Region, from a source DB instance in the
--     us-east-2 Amazon Web Services Region, then you call the
--     @CreateDBInstanceReadReplica@ operation in the us-east-1 Amazon Web
--     Services Region and provide a presigned URL that contains a call to
--     the @CreateDBInstanceReadReplica@ operation in the us-west-2 Amazon
--     Web Services Region. For this example, the @DestinationRegion@ in
--     the presigned URL must be set to the us-east-1 Amazon Web Services
--     Region.
--
-- -   @KmsKeyId@ - The KMS key identifier for the key to use to encrypt
--     the read replica in the destination Amazon Web Services Region. This
--     is the same identifier for both the @CreateDBInstanceReadReplica@
--     operation that is called in the destination Amazon Web Services
--     Region, and the operation contained in the presigned URL.
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
-- that can run in the source Amazon Web Services Region.
--
-- @SourceRegion@ isn\'t supported for SQL Server, because Amazon RDS for
-- SQL Server doesn\'t support cross-Region read replicas.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- 'processorFeatures', 'createDBInstanceReadReplica_processorFeatures' - The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- 'publiclyAccessible', 'createDBInstanceReadReplica_publiclyAccessible' - A value that indicates whether the DB instance is publicly accessible.
--
-- When the DB cluster is publicly accessible, its Domain Name System (DNS)
-- endpoint resolves to the private IP address from within the DB
-- cluster\'s virtual private cloud (VPC). It resolves to the public IP
-- address from outside of the DB cluster\'s VPC. Access to the DB cluster
-- is ultimately controlled by the security group it uses. That public
-- access isn\'t permitted if the security group assigned to the DB cluster
-- doesn\'t permit it.
--
-- When the DB instance isn\'t publicly accessible, it is an internal DB
-- instance with a DNS name that resolves to a private IP address.
--
-- For more information, see CreateDBInstance.
--
-- 'replicaMode', 'createDBInstanceReadReplica_replicaMode' - The open mode of the replica database: mounted or read-only.
--
-- This parameter is only supported for Oracle DB instances.
--
-- Mounted DB replicas are included in Oracle Database Enterprise Edition.
-- The main use case for mounted replicas is cross-Region disaster
-- recovery. The primary database doesn\'t use Active Data Guard to
-- transmit information to the mounted replica. Because it doesn\'t accept
-- user connections, a mounted replica can\'t serve a read-only workload.
--
-- You can create a combination of mounted and read-only DB replicas for
-- the same primary DB instance. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Working with Oracle Read Replicas for Amazon RDS>
-- in the /Amazon RDS User Guide/.
--
-- For RDS Custom, you must specify this parameter and set it to @mounted@.
-- The value won\'t be set by default. After replica creation, you can
-- manage the open mode manually.
--
-- 'storageThroughput', 'createDBInstanceReadReplica_storageThroughput' - Specifies the storage throughput value for the read replica.
--
-- This setting doesn\'t apply to RDS Custom or Amazon Aurora.
--
-- 'storageType', 'createDBInstanceReadReplica_storageType' - Specifies the storage type to be associated with the read replica.
--
-- Valid values: @gp2 | gp3 | io1 | standard@
--
-- If you specify @io1@ or @gp3@, you must also include a value for the
-- @Iops@ parameter.
--
-- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
--
-- 'tags', 'createDBInstanceReadReplica_tags' - Undocumented member.
--
-- 'useDefaultProcessorFeatures', 'createDBInstanceReadReplica_useDefaultProcessorFeatures' - A value that indicates whether the DB instance class of the DB instance
-- uses its default processor features.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- 'vpcSecurityGroupIds', 'createDBInstanceReadReplica_vpcSecurityGroupIds' - A list of Amazon EC2 VPC security groups to associate with the read
-- replica.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- Default: The default EC2 VPC security group for the DB subnet group\'s
-- VPC.
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
--     PostgreSQL 9.3.5 or later (9.4.7 and higher for cross-Region
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
--     in the /Amazon RDS User Guide/. This doesn\'t apply to SQL Server or
--     RDS Custom, which don\'t support cross-Region replicas.
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
      { autoMinorVersionUpgrade =
          Prelude.Nothing,
        availabilityZone = Prelude.Nothing,
        copyTagsToSnapshot = Prelude.Nothing,
        customIamInstanceProfile = Prelude.Nothing,
        dbInstanceClass = Prelude.Nothing,
        dbParameterGroupName = Prelude.Nothing,
        dbSubnetGroupName = Prelude.Nothing,
        deletionProtection = Prelude.Nothing,
        destinationRegion = Prelude.Nothing,
        domain = Prelude.Nothing,
        domainIAMRoleName = Prelude.Nothing,
        enableCloudwatchLogsExports = Prelude.Nothing,
        enableIAMDatabaseAuthentication =
          Prelude.Nothing,
        enablePerformanceInsights = Prelude.Nothing,
        iops = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        maxAllocatedStorage = Prelude.Nothing,
        monitoringInterval = Prelude.Nothing,
        monitoringRoleArn = Prelude.Nothing,
        multiAZ = Prelude.Nothing,
        networkType = Prelude.Nothing,
        optionGroupName = Prelude.Nothing,
        performanceInsightsKMSKeyId = Prelude.Nothing,
        performanceInsightsRetentionPeriod =
          Prelude.Nothing,
        port = Prelude.Nothing,
        preSignedUrl = Prelude.Nothing,
        processorFeatures = Prelude.Nothing,
        publiclyAccessible = Prelude.Nothing,
        replicaMode = Prelude.Nothing,
        storageThroughput = Prelude.Nothing,
        storageType = Prelude.Nothing,
        tags = Prelude.Nothing,
        useDefaultProcessorFeatures = Prelude.Nothing,
        vpcSecurityGroupIds = Prelude.Nothing,
        dbInstanceIdentifier = pDBInstanceIdentifier_,
        sourceDBInstanceIdentifier =
          pSourceDBInstanceIdentifier_
      }

-- | A value that indicates whether minor engine upgrades are applied
-- automatically to the read replica during the maintenance window.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- Default: Inherits from the source DB instance
createDBInstanceReadReplica_autoMinorVersionUpgrade :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Bool)
createDBInstanceReadReplica_autoMinorVersionUpgrade = Lens.lens (\CreateDBInstanceReadReplica' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@CreateDBInstanceReadReplica' {} a -> s {autoMinorVersionUpgrade = a} :: CreateDBInstanceReadReplica)

-- | The Availability Zone (AZ) where the read replica will be created.
--
-- Default: A random, system-chosen Availability Zone in the endpoint\'s
-- Amazon Web Services Region.
--
-- Example: @us-east-1d@
createDBInstanceReadReplica_availabilityZone :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Text)
createDBInstanceReadReplica_availabilityZone = Lens.lens (\CreateDBInstanceReadReplica' {availabilityZone} -> availabilityZone) (\s@CreateDBInstanceReadReplica' {} a -> s {availabilityZone = a} :: CreateDBInstanceReadReplica)

-- | A value that indicates whether to copy all tags from the read replica to
-- snapshots of the read replica. By default, tags are not copied.
createDBInstanceReadReplica_copyTagsToSnapshot :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Bool)
createDBInstanceReadReplica_copyTagsToSnapshot = Lens.lens (\CreateDBInstanceReadReplica' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@CreateDBInstanceReadReplica' {} a -> s {copyTagsToSnapshot = a} :: CreateDBInstanceReadReplica)

-- | The instance profile associated with the underlying Amazon EC2 instance
-- of an RDS Custom DB instance. The instance profile must meet the
-- following requirements:
--
-- -   The profile must exist in your account.
--
-- -   The profile must have an IAM role that Amazon EC2 has permissions to
--     assume.
--
-- -   The instance profile name and the associated IAM role name must
--     start with the prefix @AWSRDSCustom@.
--
-- For the list of permissions required for the IAM role, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/custom-setup-orcl.html#custom-setup-orcl.iam-vpc Configure IAM and your VPC>
-- in the /Amazon RDS User Guide/.
--
-- This setting is required for RDS Custom.
createDBInstanceReadReplica_customIamInstanceProfile :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Text)
createDBInstanceReadReplica_customIamInstanceProfile = Lens.lens (\CreateDBInstanceReadReplica' {customIamInstanceProfile} -> customIamInstanceProfile) (\s@CreateDBInstanceReadReplica' {} a -> s {customIamInstanceProfile = a} :: CreateDBInstanceReadReplica)

-- | The compute and memory capacity of the read replica, for example
-- db.m4.large. Not all DB instance classes are available in all Amazon Web
-- Services Regions, or for all database engines. For the full list of DB
-- instance classes, and availability for your engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
-- in the /Amazon RDS User Guide/.
--
-- Default: Inherits from the source DB instance.
createDBInstanceReadReplica_dbInstanceClass :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Text)
createDBInstanceReadReplica_dbInstanceClass = Lens.lens (\CreateDBInstanceReadReplica' {dbInstanceClass} -> dbInstanceClass) (\s@CreateDBInstanceReadReplica' {} a -> s {dbInstanceClass = a} :: CreateDBInstanceReadReplica)

-- | The name of the DB parameter group to associate with this DB instance.
--
-- If you do not specify a value for @DBParameterGroupName@, then Amazon
-- RDS uses the @DBParameterGroup@ of source DB instance for a same Region
-- read replica, or the default @DBParameterGroup@ for the specified DB
-- engine for a cross-Region read replica.
--
-- Specifying a parameter group for this operation is only supported for
-- MySQL and Oracle DB instances. It isn\'t supported for RDS Custom.
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
-- Example: @mydbsubnetgroup@
createDBInstanceReadReplica_dbSubnetGroupName :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Text)
createDBInstanceReadReplica_dbSubnetGroupName = Lens.lens (\CreateDBInstanceReadReplica' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@CreateDBInstanceReadReplica' {} a -> s {dbSubnetGroupName = a} :: CreateDBInstanceReadReplica)

-- | A value that indicates whether the DB instance has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection isn\'t enabled. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
createDBInstanceReadReplica_deletionProtection :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Bool)
createDBInstanceReadReplica_deletionProtection = Lens.lens (\CreateDBInstanceReadReplica' {deletionProtection} -> deletionProtection) (\s@CreateDBInstanceReadReplica' {} a -> s {deletionProtection = a} :: CreateDBInstanceReadReplica)

-- | Pseudo-parameter used when populating the @PreSignedUrl@ of a
-- cross-region @CreateDBInstanceReadReplica@ request. To replicate from
-- region @SRC@ to region @DST@, send a request to region @DST@. In that
-- request, pass a @PreSignedUrl@ for region @SRC@ with @DestinationRegion@
-- set to region @DST@.
createDBInstanceReadReplica_destinationRegion :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Text)
createDBInstanceReadReplica_destinationRegion = Lens.lens (\CreateDBInstanceReadReplica' {destinationRegion} -> destinationRegion) (\s@CreateDBInstanceReadReplica' {} a -> s {destinationRegion = a} :: CreateDBInstanceReadReplica)

-- | The Active Directory directory ID to create the DB instance in.
-- Currently, only MySQL, Microsoft SQL Server, Oracle, and PostgreSQL DB
-- instances can be created in an Active Directory Domain.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon RDS User Guide/.
--
-- This setting doesn\'t apply to RDS Custom.
createDBInstanceReadReplica_domain :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Text)
createDBInstanceReadReplica_domain = Lens.lens (\CreateDBInstanceReadReplica' {domain} -> domain) (\s@CreateDBInstanceReadReplica' {} a -> s {domain = a} :: CreateDBInstanceReadReplica)

-- | Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
--
-- This setting doesn\'t apply to RDS Custom.
createDBInstanceReadReplica_domainIAMRoleName :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Text)
createDBInstanceReadReplica_domainIAMRoleName = Lens.lens (\CreateDBInstanceReadReplica' {domainIAMRoleName} -> domainIAMRoleName) (\s@CreateDBInstanceReadReplica' {} a -> s {domainIAMRoleName = a} :: CreateDBInstanceReadReplica)

-- | The list of logs that the new DB instance is to export to CloudWatch
-- Logs. The values in the list depend on the DB engine being used. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon RDS User Guide/.
--
-- This setting doesn\'t apply to RDS Custom.
createDBInstanceReadReplica_enableCloudwatchLogsExports :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe [Prelude.Text])
createDBInstanceReadReplica_enableCloudwatchLogsExports = Lens.lens (\CreateDBInstanceReadReplica' {enableCloudwatchLogsExports} -> enableCloudwatchLogsExports) (\s@CreateDBInstanceReadReplica' {} a -> s {enableCloudwatchLogsExports = a} :: CreateDBInstanceReadReplica) Prelude.. Lens.mapping Lens.coerced

-- | A value that indicates whether to enable mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts. By
-- default, mapping isn\'t enabled.
--
-- For more information about IAM database authentication, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
-- in the /Amazon RDS User Guide/.
--
-- This setting doesn\'t apply to RDS Custom.
createDBInstanceReadReplica_enableIAMDatabaseAuthentication :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Bool)
createDBInstanceReadReplica_enableIAMDatabaseAuthentication = Lens.lens (\CreateDBInstanceReadReplica' {enableIAMDatabaseAuthentication} -> enableIAMDatabaseAuthentication) (\s@CreateDBInstanceReadReplica' {} a -> s {enableIAMDatabaseAuthentication = a} :: CreateDBInstanceReadReplica)

-- | A value that indicates whether to enable Performance Insights for the
-- read replica.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights>
-- in the /Amazon RDS User Guide/.
--
-- This setting doesn\'t apply to RDS Custom.
createDBInstanceReadReplica_enablePerformanceInsights :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Bool)
createDBInstanceReadReplica_enablePerformanceInsights = Lens.lens (\CreateDBInstanceReadReplica' {enablePerformanceInsights} -> enablePerformanceInsights) (\s@CreateDBInstanceReadReplica' {} a -> s {enablePerformanceInsights = a} :: CreateDBInstanceReadReplica)

-- | The amount of Provisioned IOPS (input\/output operations per second) to
-- be initially allocated for the DB instance.
createDBInstanceReadReplica_iops :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Int)
createDBInstanceReadReplica_iops = Lens.lens (\CreateDBInstanceReadReplica' {iops} -> iops) (\s@CreateDBInstanceReadReplica' {} a -> s {iops = a} :: CreateDBInstanceReadReplica)

-- | The Amazon Web Services KMS key identifier for an encrypted read
-- replica.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key.
--
-- If you create an encrypted read replica in the same Amazon Web Services
-- Region as the source DB instance, then do not specify a value for this
-- parameter. A read replica in the same Amazon Web Services Region is
-- always encrypted with the same KMS key as the source DB instance.
--
-- If you create an encrypted read replica in a different Amazon Web
-- Services Region, then you must specify a KMS key identifier for the
-- destination Amazon Web Services Region. KMS keys are specific to the
-- Amazon Web Services Region that they are created in, and you can\'t use
-- KMS keys from one Amazon Web Services Region in another Amazon Web
-- Services Region.
--
-- You can\'t create an encrypted read replica from an unencrypted DB
-- instance.
--
-- This setting doesn\'t apply to RDS Custom, which uses the same KMS key
-- as the primary replica.
createDBInstanceReadReplica_kmsKeyId :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Text)
createDBInstanceReadReplica_kmsKeyId = Lens.lens (\CreateDBInstanceReadReplica' {kmsKeyId} -> kmsKeyId) (\s@CreateDBInstanceReadReplica' {} a -> s {kmsKeyId = a} :: CreateDBInstanceReadReplica)

-- | The upper limit in gibibytes (GiB) to which Amazon RDS can automatically
-- scale the storage of the DB instance.
--
-- For more information about this setting, including limitations that
-- apply to it, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
-- in the /Amazon RDS User Guide/.
createDBInstanceReadReplica_maxAllocatedStorage :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Int)
createDBInstanceReadReplica_maxAllocatedStorage = Lens.lens (\CreateDBInstanceReadReplica' {maxAllocatedStorage} -> maxAllocatedStorage) (\s@CreateDBInstanceReadReplica' {} a -> s {maxAllocatedStorage = a} :: CreateDBInstanceReadReplica)

-- | The interval, in seconds, between points when Enhanced Monitoring
-- metrics are collected for the read replica. To disable collecting
-- Enhanced Monitoring metrics, specify 0. The default is 0.
--
-- If @MonitoringRoleArn@ is specified, then you must also set
-- @MonitoringInterval@ to a value other than 0.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- Valid Values: @0, 1, 5, 10, 15, 30, 60@
createDBInstanceReadReplica_monitoringInterval :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Int)
createDBInstanceReadReplica_monitoringInterval = Lens.lens (\CreateDBInstanceReadReplica' {monitoringInterval} -> monitoringInterval) (\s@CreateDBInstanceReadReplica' {} a -> s {monitoringInterval = a} :: CreateDBInstanceReadReplica)

-- | The ARN for the IAM role that permits RDS to send enhanced monitoring
-- metrics to Amazon CloudWatch Logs. For example,
-- @arn:aws:iam:123456789012:role\/emaccess@. For information on creating a
-- monitoring role, go to
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.html#USER_Monitoring.OS.IAMRole To create an IAM role for Amazon RDS Enhanced Monitoring>
-- in the /Amazon RDS User Guide/.
--
-- If @MonitoringInterval@ is set to a value other than 0, then you must
-- supply a @MonitoringRoleArn@ value.
--
-- This setting doesn\'t apply to RDS Custom.
createDBInstanceReadReplica_monitoringRoleArn :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Text)
createDBInstanceReadReplica_monitoringRoleArn = Lens.lens (\CreateDBInstanceReadReplica' {monitoringRoleArn} -> monitoringRoleArn) (\s@CreateDBInstanceReadReplica' {} a -> s {monitoringRoleArn = a} :: CreateDBInstanceReadReplica)

-- | A value that indicates whether the read replica is in a Multi-AZ
-- deployment.
--
-- You can create a read replica as a Multi-AZ DB instance. RDS creates a
-- standby of your replica in another Availability Zone for failover
-- support for the replica. Creating your read replica as a Multi-AZ DB
-- instance is independent of whether the source database is a Multi-AZ DB
-- instance.
--
-- This setting doesn\'t apply to RDS Custom.
createDBInstanceReadReplica_multiAZ :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Bool)
createDBInstanceReadReplica_multiAZ = Lens.lens (\CreateDBInstanceReadReplica' {multiAZ} -> multiAZ) (\s@CreateDBInstanceReadReplica' {} a -> s {multiAZ = a} :: CreateDBInstanceReadReplica)

-- | The network type of the DB instance.
--
-- Valid values:
--
-- -   @IPV4@
--
-- -   @DUAL@
--
-- The network type is determined by the @DBSubnetGroup@ specified for read
-- replica. A @DBSubnetGroup@ can support only the IPv4 protocol or the
-- IPv4 and the IPv6 protocols (@DUAL@).
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html Working with a DB instance in a VPC>
-- in the /Amazon RDS User Guide./
createDBInstanceReadReplica_networkType :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Text)
createDBInstanceReadReplica_networkType = Lens.lens (\CreateDBInstanceReadReplica' {networkType} -> networkType) (\s@CreateDBInstanceReadReplica' {} a -> s {networkType = a} :: CreateDBInstanceReadReplica)

-- | The option group the DB instance is associated with. If omitted, the
-- option group associated with the source instance is used.
--
-- For SQL Server, you must use the option group associated with the source
-- instance.
--
-- This setting doesn\'t apply to RDS Custom.
createDBInstanceReadReplica_optionGroupName :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Text)
createDBInstanceReadReplica_optionGroupName = Lens.lens (\CreateDBInstanceReadReplica' {optionGroupName} -> optionGroupName) (\s@CreateDBInstanceReadReplica' {} a -> s {optionGroupName = a} :: CreateDBInstanceReadReplica)

-- | The Amazon Web Services KMS key identifier for encryption of Performance
-- Insights data.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key.
--
-- If you do not specify a value for @PerformanceInsightsKMSKeyId@, then
-- Amazon RDS uses your default KMS key. There is a default KMS key for
-- your Amazon Web Services account. Your Amazon Web Services account has a
-- different default KMS key for each Amazon Web Services Region.
--
-- This setting doesn\'t apply to RDS Custom.
createDBInstanceReadReplica_performanceInsightsKMSKeyId :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Text)
createDBInstanceReadReplica_performanceInsightsKMSKeyId = Lens.lens (\CreateDBInstanceReadReplica' {performanceInsightsKMSKeyId} -> performanceInsightsKMSKeyId) (\s@CreateDBInstanceReadReplica' {} a -> s {performanceInsightsKMSKeyId = a} :: CreateDBInstanceReadReplica)

-- | The number of days to retain Performance Insights data. The default is 7
-- days. The following values are valid:
--
-- -   7
--
-- -   /month/ * 31, where /month/ is a number of months from 1-23
--
-- -   731
--
-- For example, the following values are valid:
--
-- -   93 (3 months * 31)
--
-- -   341 (11 months * 31)
--
-- -   589 (19 months * 31)
--
-- -   731
--
-- If you specify a retention period such as 94, which isn\'t a valid
-- value, RDS issues an error.
--
-- This setting doesn\'t apply to RDS Custom.
createDBInstanceReadReplica_performanceInsightsRetentionPeriod :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Int)
createDBInstanceReadReplica_performanceInsightsRetentionPeriod = Lens.lens (\CreateDBInstanceReadReplica' {performanceInsightsRetentionPeriod} -> performanceInsightsRetentionPeriod) (\s@CreateDBInstanceReadReplica' {} a -> s {performanceInsightsRetentionPeriod = a} :: CreateDBInstanceReadReplica)

-- | The port number that the DB instance uses for connections.
--
-- Default: Inherits from the source DB instance
--
-- Valid Values: @1150-65535@
createDBInstanceReadReplica_port :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Int)
createDBInstanceReadReplica_port = Lens.lens (\CreateDBInstanceReadReplica' {port} -> port) (\s@CreateDBInstanceReadReplica' {} a -> s {port = a} :: CreateDBInstanceReadReplica)

-- | When you are creating a read replica from one Amazon Web Services
-- GovCloud (US) Region to another or from one China Amazon Web Services
-- Region to another, the URL that contains a Signature Version 4 signed
-- request for the @CreateDBInstanceReadReplica@ API operation in the
-- source Amazon Web Services Region that contains the source DB instance.
--
-- This setting applies only to Amazon Web Services GovCloud (US) Regions
-- and China Amazon Web Services Regions. It\'s ignored in other Amazon Web
-- Services Regions.
--
-- You must specify this parameter when you create an encrypted read
-- replica from another Amazon Web Services Region by using the Amazon RDS
-- API. Don\'t specify @PreSignedUrl@ when you are creating an encrypted
-- read replica in the same Amazon Web Services Region.
--
-- The presigned URL must be a valid request for the
-- @CreateDBInstanceReadReplica@ API operation that can run in the source
-- Amazon Web Services Region that contains the encrypted source DB
-- instance. The presigned URL request must contain the following parameter
-- values:
--
-- -   @DestinationRegion@ - The Amazon Web Services Region that the
--     encrypted read replica is created in. This Amazon Web Services
--     Region is the same one where the @CreateDBInstanceReadReplica@
--     operation is called that contains this presigned URL.
--
--     For example, if you create an encrypted DB instance in the us-west-1
--     Amazon Web Services Region, from a source DB instance in the
--     us-east-2 Amazon Web Services Region, then you call the
--     @CreateDBInstanceReadReplica@ operation in the us-east-1 Amazon Web
--     Services Region and provide a presigned URL that contains a call to
--     the @CreateDBInstanceReadReplica@ operation in the us-west-2 Amazon
--     Web Services Region. For this example, the @DestinationRegion@ in
--     the presigned URL must be set to the us-east-1 Amazon Web Services
--     Region.
--
-- -   @KmsKeyId@ - The KMS key identifier for the key to use to encrypt
--     the read replica in the destination Amazon Web Services Region. This
--     is the same identifier for both the @CreateDBInstanceReadReplica@
--     operation that is called in the destination Amazon Web Services
--     Region, and the operation contained in the presigned URL.
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
-- that can run in the source Amazon Web Services Region.
--
-- @SourceRegion@ isn\'t supported for SQL Server, because Amazon RDS for
-- SQL Server doesn\'t support cross-Region read replicas.
--
-- This setting doesn\'t apply to RDS Custom.
createDBInstanceReadReplica_preSignedUrl :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Text)
createDBInstanceReadReplica_preSignedUrl = Lens.lens (\CreateDBInstanceReadReplica' {preSignedUrl} -> preSignedUrl) (\s@CreateDBInstanceReadReplica' {} a -> s {preSignedUrl = a} :: CreateDBInstanceReadReplica)

-- | The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
--
-- This setting doesn\'t apply to RDS Custom.
createDBInstanceReadReplica_processorFeatures :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe [ProcessorFeature])
createDBInstanceReadReplica_processorFeatures = Lens.lens (\CreateDBInstanceReadReplica' {processorFeatures} -> processorFeatures) (\s@CreateDBInstanceReadReplica' {} a -> s {processorFeatures = a} :: CreateDBInstanceReadReplica) Prelude.. Lens.mapping Lens.coerced

-- | A value that indicates whether the DB instance is publicly accessible.
--
-- When the DB cluster is publicly accessible, its Domain Name System (DNS)
-- endpoint resolves to the private IP address from within the DB
-- cluster\'s virtual private cloud (VPC). It resolves to the public IP
-- address from outside of the DB cluster\'s VPC. Access to the DB cluster
-- is ultimately controlled by the security group it uses. That public
-- access isn\'t permitted if the security group assigned to the DB cluster
-- doesn\'t permit it.
--
-- When the DB instance isn\'t publicly accessible, it is an internal DB
-- instance with a DNS name that resolves to a private IP address.
--
-- For more information, see CreateDBInstance.
createDBInstanceReadReplica_publiclyAccessible :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Bool)
createDBInstanceReadReplica_publiclyAccessible = Lens.lens (\CreateDBInstanceReadReplica' {publiclyAccessible} -> publiclyAccessible) (\s@CreateDBInstanceReadReplica' {} a -> s {publiclyAccessible = a} :: CreateDBInstanceReadReplica)

-- | The open mode of the replica database: mounted or read-only.
--
-- This parameter is only supported for Oracle DB instances.
--
-- Mounted DB replicas are included in Oracle Database Enterprise Edition.
-- The main use case for mounted replicas is cross-Region disaster
-- recovery. The primary database doesn\'t use Active Data Guard to
-- transmit information to the mounted replica. Because it doesn\'t accept
-- user connections, a mounted replica can\'t serve a read-only workload.
--
-- You can create a combination of mounted and read-only DB replicas for
-- the same primary DB instance. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Working with Oracle Read Replicas for Amazon RDS>
-- in the /Amazon RDS User Guide/.
--
-- For RDS Custom, you must specify this parameter and set it to @mounted@.
-- The value won\'t be set by default. After replica creation, you can
-- manage the open mode manually.
createDBInstanceReadReplica_replicaMode :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe ReplicaMode)
createDBInstanceReadReplica_replicaMode = Lens.lens (\CreateDBInstanceReadReplica' {replicaMode} -> replicaMode) (\s@CreateDBInstanceReadReplica' {} a -> s {replicaMode = a} :: CreateDBInstanceReadReplica)

-- | Specifies the storage throughput value for the read replica.
--
-- This setting doesn\'t apply to RDS Custom or Amazon Aurora.
createDBInstanceReadReplica_storageThroughput :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Int)
createDBInstanceReadReplica_storageThroughput = Lens.lens (\CreateDBInstanceReadReplica' {storageThroughput} -> storageThroughput) (\s@CreateDBInstanceReadReplica' {} a -> s {storageThroughput = a} :: CreateDBInstanceReadReplica)

-- | Specifies the storage type to be associated with the read replica.
--
-- Valid values: @gp2 | gp3 | io1 | standard@
--
-- If you specify @io1@ or @gp3@, you must also include a value for the
-- @Iops@ parameter.
--
-- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
createDBInstanceReadReplica_storageType :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Text)
createDBInstanceReadReplica_storageType = Lens.lens (\CreateDBInstanceReadReplica' {storageType} -> storageType) (\s@CreateDBInstanceReadReplica' {} a -> s {storageType = a} :: CreateDBInstanceReadReplica)

-- | Undocumented member.
createDBInstanceReadReplica_tags :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe [Tag])
createDBInstanceReadReplica_tags = Lens.lens (\CreateDBInstanceReadReplica' {tags} -> tags) (\s@CreateDBInstanceReadReplica' {} a -> s {tags = a} :: CreateDBInstanceReadReplica) Prelude.. Lens.mapping Lens.coerced

-- | A value that indicates whether the DB instance class of the DB instance
-- uses its default processor features.
--
-- This setting doesn\'t apply to RDS Custom.
createDBInstanceReadReplica_useDefaultProcessorFeatures :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe Prelude.Bool)
createDBInstanceReadReplica_useDefaultProcessorFeatures = Lens.lens (\CreateDBInstanceReadReplica' {useDefaultProcessorFeatures} -> useDefaultProcessorFeatures) (\s@CreateDBInstanceReadReplica' {} a -> s {useDefaultProcessorFeatures = a} :: CreateDBInstanceReadReplica)

-- | A list of Amazon EC2 VPC security groups to associate with the read
-- replica.
--
-- This setting doesn\'t apply to RDS Custom.
--
-- Default: The default EC2 VPC security group for the DB subnet group\'s
-- VPC.
createDBInstanceReadReplica_vpcSecurityGroupIds :: Lens.Lens' CreateDBInstanceReadReplica (Prelude.Maybe [Prelude.Text])
createDBInstanceReadReplica_vpcSecurityGroupIds = Lens.lens (\CreateDBInstanceReadReplica' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@CreateDBInstanceReadReplica' {} a -> s {vpcSecurityGroupIds = a} :: CreateDBInstanceReadReplica) Prelude.. Lens.mapping Lens.coerced

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
--     PostgreSQL 9.3.5 or later (9.4.7 and higher for cross-Region
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
--     in the /Amazon RDS User Guide/. This doesn\'t apply to SQL Server or
--     RDS Custom, which don\'t support cross-Region replicas.
createDBInstanceReadReplica_sourceDBInstanceIdentifier :: Lens.Lens' CreateDBInstanceReadReplica Prelude.Text
createDBInstanceReadReplica_sourceDBInstanceIdentifier = Lens.lens (\CreateDBInstanceReadReplica' {sourceDBInstanceIdentifier} -> sourceDBInstanceIdentifier) (\s@CreateDBInstanceReadReplica' {} a -> s {sourceDBInstanceIdentifier = a} :: CreateDBInstanceReadReplica)

instance Core.AWSRequest CreateDBInstanceReadReplica where
  type
    AWSResponse CreateDBInstanceReadReplica =
      CreateDBInstanceReadReplicaResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateDBInstanceReadReplicaResult"
      ( \s h x ->
          CreateDBInstanceReadReplicaResponse'
            Prelude.<$> (x Data..@? "DBInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDBInstanceReadReplica where
  hashWithSalt _salt CreateDBInstanceReadReplica' {..} =
    _salt
      `Prelude.hashWithSalt` autoMinorVersionUpgrade
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` copyTagsToSnapshot
      `Prelude.hashWithSalt` customIamInstanceProfile
      `Prelude.hashWithSalt` dbInstanceClass
      `Prelude.hashWithSalt` dbParameterGroupName
      `Prelude.hashWithSalt` dbSubnetGroupName
      `Prelude.hashWithSalt` deletionProtection
      `Prelude.hashWithSalt` destinationRegion
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` domainIAMRoleName
      `Prelude.hashWithSalt` enableCloudwatchLogsExports
      `Prelude.hashWithSalt` enableIAMDatabaseAuthentication
      `Prelude.hashWithSalt` enablePerformanceInsights
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` maxAllocatedStorage
      `Prelude.hashWithSalt` monitoringInterval
      `Prelude.hashWithSalt` monitoringRoleArn
      `Prelude.hashWithSalt` multiAZ
      `Prelude.hashWithSalt` networkType
      `Prelude.hashWithSalt` optionGroupName
      `Prelude.hashWithSalt` performanceInsightsKMSKeyId
      `Prelude.hashWithSalt` performanceInsightsRetentionPeriod
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` preSignedUrl
      `Prelude.hashWithSalt` processorFeatures
      `Prelude.hashWithSalt` publiclyAccessible
      `Prelude.hashWithSalt` replicaMode
      `Prelude.hashWithSalt` storageThroughput
      `Prelude.hashWithSalt` storageType
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` useDefaultProcessorFeatures
      `Prelude.hashWithSalt` vpcSecurityGroupIds
      `Prelude.hashWithSalt` dbInstanceIdentifier
      `Prelude.hashWithSalt` sourceDBInstanceIdentifier

instance Prelude.NFData CreateDBInstanceReadReplica where
  rnf CreateDBInstanceReadReplica' {..} =
    Prelude.rnf autoMinorVersionUpgrade
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf copyTagsToSnapshot
      `Prelude.seq` Prelude.rnf customIamInstanceProfile
      `Prelude.seq` Prelude.rnf dbInstanceClass
      `Prelude.seq` Prelude.rnf dbParameterGroupName
      `Prelude.seq` Prelude.rnf dbSubnetGroupName
      `Prelude.seq` Prelude.rnf deletionProtection
      `Prelude.seq` Prelude.rnf destinationRegion
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf domainIAMRoleName
      `Prelude.seq` Prelude.rnf enableCloudwatchLogsExports
      `Prelude.seq` Prelude.rnf enableIAMDatabaseAuthentication
      `Prelude.seq` Prelude.rnf enablePerformanceInsights
      `Prelude.seq` Prelude.rnf iops
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf maxAllocatedStorage
      `Prelude.seq` Prelude.rnf monitoringInterval
      `Prelude.seq` Prelude.rnf monitoringRoleArn
      `Prelude.seq` Prelude.rnf multiAZ
      `Prelude.seq` Prelude.rnf networkType
      `Prelude.seq` Prelude.rnf
        optionGroupName
      `Prelude.seq` Prelude.rnf
        performanceInsightsKMSKeyId
      `Prelude.seq` Prelude.rnf
        performanceInsightsRetentionPeriod
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf
        preSignedUrl
      `Prelude.seq` Prelude.rnf
        processorFeatures
      `Prelude.seq` Prelude.rnf
        publiclyAccessible
      `Prelude.seq` Prelude.rnf
        replicaMode
      `Prelude.seq` Prelude.rnf
        storageThroughput
      `Prelude.seq` Prelude.rnf
        storageType
      `Prelude.seq` Prelude.rnf
        tags
      `Prelude.seq` Prelude.rnf
        useDefaultProcessorFeatures
      `Prelude.seq` Prelude.rnf
        vpcSecurityGroupIds
      `Prelude.seq` Prelude.rnf
        dbInstanceIdentifier
      `Prelude.seq` Prelude.rnf
        sourceDBInstanceIdentifier

instance Data.ToHeaders CreateDBInstanceReadReplica where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateDBInstanceReadReplica where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDBInstanceReadReplica where
  toQuery CreateDBInstanceReadReplica' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "CreateDBInstanceReadReplica" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "AutoMinorVersionUpgrade"
          Data.=: autoMinorVersionUpgrade,
        "AvailabilityZone" Data.=: availabilityZone,
        "CopyTagsToSnapshot" Data.=: copyTagsToSnapshot,
        "CustomIamInstanceProfile"
          Data.=: customIamInstanceProfile,
        "DBInstanceClass" Data.=: dbInstanceClass,
        "DBParameterGroupName" Data.=: dbParameterGroupName,
        "DBSubnetGroupName" Data.=: dbSubnetGroupName,
        "DeletionProtection" Data.=: deletionProtection,
        "DestinationRegion" Data.=: destinationRegion,
        "Domain" Data.=: domain,
        "DomainIAMRoleName" Data.=: domainIAMRoleName,
        "EnableCloudwatchLogsExports"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> enableCloudwatchLogsExports
            ),
        "EnableIAMDatabaseAuthentication"
          Data.=: enableIAMDatabaseAuthentication,
        "EnablePerformanceInsights"
          Data.=: enablePerformanceInsights,
        "Iops" Data.=: iops,
        "KmsKeyId" Data.=: kmsKeyId,
        "MaxAllocatedStorage" Data.=: maxAllocatedStorage,
        "MonitoringInterval" Data.=: monitoringInterval,
        "MonitoringRoleArn" Data.=: monitoringRoleArn,
        "MultiAZ" Data.=: multiAZ,
        "NetworkType" Data.=: networkType,
        "OptionGroupName" Data.=: optionGroupName,
        "PerformanceInsightsKMSKeyId"
          Data.=: performanceInsightsKMSKeyId,
        "PerformanceInsightsRetentionPeriod"
          Data.=: performanceInsightsRetentionPeriod,
        "Port" Data.=: port,
        "PreSignedUrl" Data.=: preSignedUrl,
        "ProcessorFeatures"
          Data.=: Data.toQuery
            ( Data.toQueryList "ProcessorFeature"
                Prelude.<$> processorFeatures
            ),
        "PubliclyAccessible" Data.=: publiclyAccessible,
        "ReplicaMode" Data.=: replicaMode,
        "StorageThroughput" Data.=: storageThroughput,
        "StorageType" Data.=: storageType,
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "Tag" Prelude.<$> tags),
        "UseDefaultProcessorFeatures"
          Data.=: useDefaultProcessorFeatures,
        "VpcSecurityGroupIds"
          Data.=: Data.toQuery
            ( Data.toQueryList "VpcSecurityGroupId"
                Prelude.<$> vpcSecurityGroupIds
            ),
        "DBInstanceIdentifier" Data.=: dbInstanceIdentifier,
        "SourceDBInstanceIdentifier"
          Data.=: sourceDBInstanceIdentifier
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
