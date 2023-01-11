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
-- Module      : Amazonka.RDS.CreateDBCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon Aurora DB cluster or Multi-AZ DB cluster.
--
-- You can use the @ReplicationSourceIdentifier@ parameter to create an
-- Amazon Aurora DB cluster as a read replica of another DB cluster or
-- Amazon RDS MySQL or PostgreSQL DB instance.
--
-- For more information on Amazon Aurora, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What is Amazon Aurora?>
-- in the /Amazon Aurora User Guide/.
--
-- For more information on Multi-AZ DB clusters, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/multi-az-db-clusters-concepts.html Multi-AZ deployments with two readable standby DB instances>
-- in the /Amazon RDS User Guide/.
module Amazonka.RDS.CreateDBCluster
  ( -- * Creating a Request
    CreateDBCluster (..),
    newCreateDBCluster,

    -- * Request Lenses
    createDBCluster_allocatedStorage,
    createDBCluster_autoMinorVersionUpgrade,
    createDBCluster_availabilityZones,
    createDBCluster_backtrackWindow,
    createDBCluster_backupRetentionPeriod,
    createDBCluster_characterSetName,
    createDBCluster_copyTagsToSnapshot,
    createDBCluster_dbClusterInstanceClass,
    createDBCluster_dbClusterParameterGroupName,
    createDBCluster_dbSubnetGroupName,
    createDBCluster_dbSystemId,
    createDBCluster_databaseName,
    createDBCluster_deletionProtection,
    createDBCluster_destinationRegion,
    createDBCluster_domain,
    createDBCluster_domainIAMRoleName,
    createDBCluster_enableCloudwatchLogsExports,
    createDBCluster_enableGlobalWriteForwarding,
    createDBCluster_enableHttpEndpoint,
    createDBCluster_enableIAMDatabaseAuthentication,
    createDBCluster_enablePerformanceInsights,
    createDBCluster_engineMode,
    createDBCluster_engineVersion,
    createDBCluster_globalClusterIdentifier,
    createDBCluster_iops,
    createDBCluster_kmsKeyId,
    createDBCluster_manageMasterUserPassword,
    createDBCluster_masterUserPassword,
    createDBCluster_masterUserSecretKmsKeyId,
    createDBCluster_masterUsername,
    createDBCluster_monitoringInterval,
    createDBCluster_monitoringRoleArn,
    createDBCluster_networkType,
    createDBCluster_optionGroupName,
    createDBCluster_performanceInsightsKMSKeyId,
    createDBCluster_performanceInsightsRetentionPeriod,
    createDBCluster_port,
    createDBCluster_preSignedUrl,
    createDBCluster_preferredBackupWindow,
    createDBCluster_preferredMaintenanceWindow,
    createDBCluster_publiclyAccessible,
    createDBCluster_replicationSourceIdentifier,
    createDBCluster_scalingConfiguration,
    createDBCluster_serverlessV2ScalingConfiguration,
    createDBCluster_storageEncrypted,
    createDBCluster_storageType,
    createDBCluster_tags,
    createDBCluster_vpcSecurityGroupIds,
    createDBCluster_dbClusterIdentifier,
    createDBCluster_engine,

    -- * Destructuring the Response
    CreateDBClusterResponse (..),
    newCreateDBClusterResponse,

    -- * Response Lenses
    createDBClusterResponse_dbCluster,
    createDBClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newCreateDBCluster' smart constructor.
data CreateDBCluster = CreateDBCluster'
  { -- | The amount of storage in gibibytes (GiB) to allocate to each DB instance
    -- in the Multi-AZ DB cluster.
    --
    -- This setting is required to create a Multi-AZ DB cluster.
    --
    -- Valid for: Multi-AZ DB clusters only
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | A value that indicates whether minor engine upgrades are applied
    -- automatically to the DB cluster during the maintenance window. By
    -- default, minor engine upgrades are applied automatically.
    --
    -- Valid for: Multi-AZ DB clusters only
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | A list of Availability Zones (AZs) where DB instances in the DB cluster
    -- can be created.
    --
    -- For information on Amazon Web Services Regions and Availability Zones,
    -- see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Concepts.RegionsAndAvailabilityZones.html Choosing the Regions and Availability Zones>
    -- in the /Amazon Aurora User Guide/.
    --
    -- Valid for: Aurora DB clusters only
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | The target backtrack window, in seconds. To disable backtracking, set
    -- this value to 0.
    --
    -- Default: 0
    --
    -- Constraints:
    --
    -- -   If specified, this value must be set to a number from 0 to 259,200
    --     (72 hours).
    --
    -- Valid for: Aurora MySQL DB clusters only
    backtrackWindow :: Prelude.Maybe Prelude.Integer,
    -- | The number of days for which automated backups are retained.
    --
    -- Default: 1
    --
    -- Constraints:
    --
    -- -   Must be a value from 1 to 35
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | A value that indicates that the DB cluster should be associated with the
    -- specified CharacterSet.
    --
    -- Valid for: Aurora DB clusters only
    characterSetName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to copy all tags from the DB cluster to
    -- snapshots of the DB cluster. The default is not to copy them.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | The compute and memory capacity of each DB instance in the Multi-AZ DB
    -- cluster, for example db.m6gd.xlarge. Not all DB instance classes are
    -- available in all Amazon Web Services Regions, or for all database
    -- engines.
    --
    -- For the full list of DB instance classes and availability for your
    -- engine, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB instance class>
    -- in the /Amazon RDS User Guide/.
    --
    -- This setting is required to create a Multi-AZ DB cluster.
    --
    -- Valid for: Multi-AZ DB clusters only
    dbClusterInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | The name of the DB cluster parameter group to associate with this DB
    -- cluster. If you do not specify a value, then the default DB cluster
    -- parameter group for the specified DB engine and version is used.
    --
    -- Constraints:
    --
    -- -   If supplied, must match the name of an existing DB cluster parameter
    --     group.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    dbClusterParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | A DB subnet group to associate with this DB cluster.
    --
    -- This setting is required to create a Multi-AZ DB cluster.
    --
    -- Constraints: Must match the name of an existing DBSubnetGroup. Must not
    -- be default.
    --
    -- Example: @mydbsubnetgroup@
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | Reserved for future use.
    dbSystemId :: Prelude.Maybe Prelude.Text,
    -- | The name for your database of up to 64 alphanumeric characters. If you
    -- do not provide a name, Amazon RDS doesn\'t create a database in the DB
    -- cluster you are creating.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the DB cluster has deletion protection
    -- enabled. The database can\'t be deleted when deletion protection is
    -- enabled. By default, deletion protection isn\'t enabled.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | Pseudo-parameter used when populating the @PreSignedUrl@ of a
    -- cross-region @CreateDBCluster@ request. To replicate from region @SRC@
    -- to region @DST@, send a request to region @DST@. In that request, pass a
    -- @PreSignedUrl@ for region @SRC@ with @DestinationRegion@ set to region
    -- @DST@.
    destinationRegion :: Prelude.Maybe Prelude.Text,
    -- | The Active Directory directory ID to create the DB cluster in.
    --
    -- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos
    -- authentication to authenticate users that connect to the DB cluster.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos authentication>
    -- in the /Amazon Aurora User Guide/.
    --
    -- Valid for: Aurora DB clusters only
    domain :: Prelude.Maybe Prelude.Text,
    -- | Specify the name of the IAM role to be used when making API calls to the
    -- Directory Service.
    --
    -- Valid for: Aurora DB clusters only
    domainIAMRoleName :: Prelude.Maybe Prelude.Text,
    -- | The list of log types that need to be enabled for exporting to
    -- CloudWatch Logs. The values in the list depend on the DB engine being
    -- used.
    --
    -- __RDS for MySQL__
    --
    -- Possible values are @error@, @general@, and @slowquery@.
    --
    -- __RDS for PostgreSQL__
    --
    -- Possible values are @postgresql@ and @upgrade@.
    --
    -- __Aurora MySQL__
    --
    -- Possible values are @audit@, @error@, @general@, and @slowquery@.
    --
    -- __Aurora PostgreSQL__
    --
    -- Possible value is @postgresql@.
    --
    -- For more information about exporting CloudWatch Logs for Amazon RDS, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
    -- in the /Amazon RDS User Guide/.
    --
    -- For more information about exporting CloudWatch Logs for Amazon Aurora,
    -- see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
    -- in the /Amazon Aurora User Guide/.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    enableCloudwatchLogsExports :: Prelude.Maybe [Prelude.Text],
    -- | A value that indicates whether to enable this DB cluster to forward
    -- write operations to the primary cluster of an Aurora global database
    -- (GlobalCluster). By default, write operations are not allowed on Aurora
    -- DB clusters that are secondary clusters in an Aurora global database.
    --
    -- You can set this value only on Aurora DB clusters that are members of an
    -- Aurora global database. With this parameter enabled, a secondary cluster
    -- can forward writes to the current primary cluster and the resulting
    -- changes are replicated back to this cluster. For the primary DB cluster
    -- of an Aurora global database, this value is used immediately if the
    -- primary is demoted by the FailoverGlobalCluster API operation, but it
    -- does nothing until then.
    --
    -- Valid for: Aurora DB clusters only
    enableGlobalWriteForwarding :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether to enable the HTTP endpoint for an Aurora
    -- Serverless v1 DB cluster. By default, the HTTP endpoint is disabled.
    --
    -- When enabled, the HTTP endpoint provides a connectionless web service
    -- API for running SQL queries on the Aurora Serverless v1 DB cluster. You
    -- can also query your database from inside the RDS console with the query
    -- editor.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/data-api.html Using the Data API for Aurora Serverless v1>
    -- in the /Amazon Aurora User Guide/.
    --
    -- Valid for: Aurora DB clusters only
    enableHttpEndpoint :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether to enable mapping of Amazon Web Services
    -- Identity and Access Management (IAM) accounts to database accounts. By
    -- default, mapping isn\'t enabled.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
    -- in the /Amazon Aurora User Guide/.
    --
    -- Valid for: Aurora DB clusters only
    enableIAMDatabaseAuthentication :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether to turn on Performance Insights for the
    -- DB cluster.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights>
    -- in the /Amazon RDS User Guide/.
    --
    -- Valid for: Multi-AZ DB clusters only
    enablePerformanceInsights :: Prelude.Maybe Prelude.Bool,
    -- | The DB engine mode of the DB cluster, either @provisioned@,
    -- @serverless@, @parallelquery@, @global@, or @multimaster@.
    --
    -- The @parallelquery@ engine mode isn\'t required for Aurora MySQL version
    -- 1.23 and higher 1.x versions, and version 2.09 and higher 2.x versions.
    --
    -- The @global@ engine mode isn\'t required for Aurora MySQL version 1.22
    -- and higher 1.x versions, and @global@ engine mode isn\'t required for
    -- any 2.x versions.
    --
    -- The @multimaster@ engine mode only applies for DB clusters created with
    -- Aurora MySQL version 5.6.10a.
    --
    -- The @serverless@ engine mode only applies for Aurora Serverless v1 DB
    -- clusters.
    --
    -- For Aurora PostgreSQL, the @global@ engine mode isn\'t required, and
    -- both the @parallelquery@ and the @multimaster@ engine modes currently
    -- aren\'t supported.
    --
    -- Limitations and requirements apply to some DB engine modes. For more
    -- information, see the following sections in the /Amazon Aurora User
    -- Guide/:
    --
    -- -   <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html#aurora-serverless.limitations Limitations of Aurora Serverless v1>
    --
    -- -   <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless-v2.requirements.html Requirements for Aurora Serverless v2>
    --
    -- -   <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-mysql-parallel-query.html#aurora-mysql-parallel-query-limitations Limitations of Parallel Query>
    --
    -- -   <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-global-database.html#aurora-global-database.limitations Limitations of Aurora Global Databases>
    --
    -- -   <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-multi-master.html#aurora-multi-master-limitations Limitations of Multi-Master Clusters>
    --
    -- Valid for: Aurora DB clusters only
    engineMode :: Prelude.Maybe Prelude.Text,
    -- | The version number of the database engine to use.
    --
    -- To list all of the available engine versions for MySQL 5.6-compatible
    -- Aurora, use the following command:
    --
    -- @aws rds describe-db-engine-versions --engine aurora --query \"DBEngineVersions[].EngineVersion\"@
    --
    -- To list all of the available engine versions for MySQL 5.7-compatible
    -- and MySQL 8.0-compatible Aurora, use the following command:
    --
    -- @aws rds describe-db-engine-versions --engine aurora-mysql --query \"DBEngineVersions[].EngineVersion\"@
    --
    -- To list all of the available engine versions for Aurora PostgreSQL, use
    -- the following command:
    --
    -- @aws rds describe-db-engine-versions --engine aurora-postgresql --query \"DBEngineVersions[].EngineVersion\"@
    --
    -- To list all of the available engine versions for RDS for MySQL, use the
    -- following command:
    --
    -- @aws rds describe-db-engine-versions --engine mysql --query \"DBEngineVersions[].EngineVersion\"@
    --
    -- To list all of the available engine versions for RDS for PostgreSQL, use
    -- the following command:
    --
    -- @aws rds describe-db-engine-versions --engine postgres --query \"DBEngineVersions[].EngineVersion\"@
    --
    -- __Aurora MySQL__
    --
    -- For information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/AuroraMySQL.Updates.html MySQL on Amazon RDS Versions>
    -- in the /Amazon Aurora User Guide/.
    --
    -- __Aurora PostgreSQL__
    --
    -- For information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/AuroraPostgreSQL.Updates.20180305.html Amazon Aurora PostgreSQL releases and engine versions>
    -- in the /Amazon Aurora User Guide/.
    --
    -- __MySQL__
    --
    -- For information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_MySQL.html#MySQL.Concepts.VersionMgmt MySQL on Amazon RDS Versions>
    -- in the /Amazon RDS User Guide/.
    --
    -- __PostgreSQL__
    --
    -- For information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_PostgreSQL.html#PostgreSQL.Concepts Amazon RDS for PostgreSQL versions and extensions>
    -- in the /Amazon RDS User Guide/.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The global cluster ID of an Aurora cluster that becomes the primary
    -- cluster in the new global database cluster.
    --
    -- Valid for: Aurora DB clusters only
    globalClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The amount of Provisioned IOPS (input\/output operations per second) to
    -- be initially allocated for each DB instance in the Multi-AZ DB cluster.
    --
    -- For information about valid IOPS values, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS storage>
    -- in the /Amazon RDS User Guide/.
    --
    -- This setting is required to create a Multi-AZ DB cluster.
    --
    -- Constraints: Must be a multiple between .5 and 50 of the storage amount
    -- for the DB cluster.
    --
    -- Valid for: Multi-AZ DB clusters only
    iops :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Web Services KMS key identifier for an encrypted DB cluster.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the KMS key. To use a KMS key in a different
    -- Amazon Web Services account, specify the key ARN or alias ARN.
    --
    -- When a KMS key isn\'t specified in @KmsKeyId@:
    --
    -- -   If @ReplicationSourceIdentifier@ identifies an encrypted source,
    --     then Amazon RDS will use the KMS key used to encrypt the source.
    --     Otherwise, Amazon RDS will use your default KMS key.
    --
    -- -   If the @StorageEncrypted@ parameter is enabled and
    --     @ReplicationSourceIdentifier@ isn\'t specified, then Amazon RDS will
    --     use your default KMS key.
    --
    -- There is a default KMS key for your Amazon Web Services account. Your
    -- Amazon Web Services account has a different default KMS key for each
    -- Amazon Web Services Region.
    --
    -- If you create a read replica of an encrypted DB cluster in another
    -- Amazon Web Services Region, you must set @KmsKeyId@ to a KMS key
    -- identifier that is valid in the destination Amazon Web Services Region.
    -- This KMS key is used to encrypt the read replica in that Amazon Web
    -- Services Region.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to manage the master user password with
    -- Amazon Web Services Secrets Manager.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
    -- in the /Amazon RDS User Guide/ and
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
    -- in the /Amazon Aurora User Guide./
    --
    -- Constraints:
    --
    -- -   Can\'t manage the master user password with Amazon Web Services
    --     Secrets Manager if @MasterUserPassword@ is specified.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    manageMasterUserPassword :: Prelude.Maybe Prelude.Bool,
    -- | The password for the master database user. This password can contain any
    -- printable ASCII character except \"\/\", \"\"\", or \"\@\".
    --
    -- Constraints:
    --
    -- -   Must contain from 8 to 41 characters.
    --
    -- -   Can\'t be specified if @ManageMasterUserPassword@ is turned on.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    masterUserPassword :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services KMS key identifier to encrypt a secret that is
    -- automatically generated and managed in Amazon Web Services Secrets
    -- Manager.
    --
    -- This setting is valid only if the master user password is managed by RDS
    -- in Amazon Web Services Secrets Manager for the DB cluster.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the KMS key. To use a KMS key in a different
    -- Amazon Web Services account, specify the key ARN or alias ARN.
    --
    -- If you don\'t specify @MasterUserSecretKmsKeyId@, then the
    -- @aws\/secretsmanager@ KMS key is used to encrypt the secret. If the
    -- secret is in a different Amazon Web Services account, then you can\'t
    -- use the @aws\/secretsmanager@ KMS key to encrypt the secret, and you
    -- must use a customer managed KMS key.
    --
    -- There is a default KMS key for your Amazon Web Services account. Your
    -- Amazon Web Services account has a different default KMS key for each
    -- Amazon Web Services Region.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    masterUserSecretKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The name of the master user for the DB cluster.
    --
    -- Constraints:
    --
    -- -   Must be 1 to 16 letters or numbers.
    --
    -- -   First character must be a letter.
    --
    -- -   Can\'t be a reserved word for the chosen database engine.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    masterUsername :: Prelude.Maybe Prelude.Text,
    -- | The interval, in seconds, between points when Enhanced Monitoring
    -- metrics are collected for the DB cluster. To turn off collecting
    -- Enhanced Monitoring metrics, specify 0. The default is 0.
    --
    -- If @MonitoringRoleArn@ is specified, also set @MonitoringInterval@ to a
    -- value other than 0.
    --
    -- Valid Values: @0, 1, 5, 10, 15, 30, 60@
    --
    -- Valid for: Multi-AZ DB clusters only
    monitoringInterval :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) for the IAM role that permits RDS to send
    -- Enhanced Monitoring metrics to Amazon CloudWatch Logs. An example is
    -- @arn:aws:iam:123456789012:role\/emaccess@. For information on creating a
    -- monitoring role, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.OS.html#USER_Monitoring.OS.Enabling Setting up and enabling Enhanced Monitoring>
    -- in the /Amazon RDS User Guide/.
    --
    -- If @MonitoringInterval@ is set to a value other than 0, supply a
    -- @MonitoringRoleArn@ value.
    --
    -- Valid for: Multi-AZ DB clusters only
    monitoringRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The network type of the DB cluster.
    --
    -- Valid values:
    --
    -- -   @IPV4@
    --
    -- -   @DUAL@
    --
    -- The network type is determined by the @DBSubnetGroup@ specified for the
    -- DB cluster. A @DBSubnetGroup@ can support only the IPv4 protocol or the
    -- IPv4 and the IPv6 protocols (@DUAL@).
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html Working with a DB instance in a VPC>
    -- in the /Amazon Aurora User Guide./
    --
    -- Valid for: Aurora DB clusters only
    networkType :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates that the DB cluster should be associated with the
    -- specified option group.
    --
    -- DB clusters are associated with a default option group that can\'t be
    -- modified.
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services KMS key identifier for encryption of Performance
    -- Insights data.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the KMS key.
    --
    -- If you don\'t specify a value for @PerformanceInsightsKMSKeyId@, then
    -- Amazon RDS uses your default KMS key. There is a default KMS key for
    -- your Amazon Web Services account. Your Amazon Web Services account has a
    -- different default KMS key for each Amazon Web Services Region.
    --
    -- Valid for: Multi-AZ DB clusters only
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
    -- Valid for: Multi-AZ DB clusters only
    performanceInsightsRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The port number on which the instances in the DB cluster accept
    -- connections.
    --
    -- __RDS for MySQL and Aurora MySQL__
    --
    -- Default: @3306@
    --
    -- Valid values: @1150-65535@
    --
    -- __RDS for PostgreSQL and Aurora PostgreSQL__
    --
    -- Default: @5432@
    --
    -- Valid values: @1150-65535@
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    port :: Prelude.Maybe Prelude.Int,
    -- | When you are replicating a DB cluster from one Amazon Web Services
    -- GovCloud (US) Region to another, an URL that contains a Signature
    -- Version 4 signed request for the @CreateDBCluster@ operation to be
    -- called in the source Amazon Web Services Region where the DB cluster is
    -- replicated from. Specify @PreSignedUrl@ only when you are performing
    -- cross-Region replication from an encrypted DB cluster.
    --
    -- The presigned URL must be a valid request for the @CreateDBCluster@ API
    -- operation that can run in the source Amazon Web Services Region that
    -- contains the encrypted DB cluster to copy.
    --
    -- The presigned URL request must contain the following parameter values:
    --
    -- -   @KmsKeyId@ - The KMS key identifier for the KMS key to use to
    --     encrypt the copy of the DB cluster in the destination Amazon Web
    --     Services Region. This should refer to the same KMS key for both the
    --     @CreateDBCluster@ operation that is called in the destination Amazon
    --     Web Services Region, and the operation contained in the presigned
    --     URL.
    --
    -- -   @DestinationRegion@ - The name of the Amazon Web Services Region
    --     that Aurora read replica will be created in.
    --
    -- -   @ReplicationSourceIdentifier@ - The DB cluster identifier for the
    --     encrypted DB cluster to be copied. This identifier must be in the
    --     Amazon Resource Name (ARN) format for the source Amazon Web Services
    --     Region. For example, if you are copying an encrypted DB cluster from
    --     the us-west-2 Amazon Web Services Region, then your
    --     @ReplicationSourceIdentifier@ would look like Example:
    --     @arn:aws:rds:us-west-2:123456789012:cluster:aurora-cluster1@.
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
    -- Valid for: Aurora DB clusters only
    preSignedUrl :: Prelude.Maybe Prelude.Text,
    -- | The daily time range during which automated backups are created if
    -- automated backups are enabled using the @BackupRetentionPeriod@
    -- parameter.
    --
    -- The default is a 30-minute window selected at random from an 8-hour
    -- block of time for each Amazon Web Services Region. To view the time
    -- blocks available, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.Backups.BackupWindow Backup window>
    -- in the /Amazon Aurora User Guide/.
    --
    -- Constraints:
    --
    -- -   Must be in the format @hh24:mi-hh24:mi@.
    --
    -- -   Must be in Universal Coordinated Time (UTC).
    --
    -- -   Must not conflict with the preferred maintenance window.
    --
    -- -   Must be at least 30 minutes.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    preferredBackupWindow :: Prelude.Maybe Prelude.Text,
    -- | The weekly time range during which system maintenance can occur, in
    -- Universal Coordinated Time (UTC).
    --
    -- Format: @ddd:hh24:mi-ddd:hh24:mi@
    --
    -- The default is a 30-minute window selected at random from an 8-hour
    -- block of time for each Amazon Web Services Region, occurring on a random
    -- day of the week. To see the time blocks available, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred DB Cluster Maintenance Window>
    -- in the /Amazon Aurora User Guide/.
    --
    -- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
    --
    -- Constraints: Minimum 30-minute window.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the DB cluster is publicly accessible.
    --
    -- When the DB cluster is publicly accessible, its Domain Name System (DNS)
    -- endpoint resolves to the private IP address from within the DB
    -- cluster\'s virtual private cloud (VPC). It resolves to the public IP
    -- address from outside of the DB cluster\'s VPC. Access to the DB cluster
    -- is ultimately controlled by the security group it uses. That public
    -- access isn\'t permitted if the security group assigned to the DB cluster
    -- doesn\'t permit it.
    --
    -- When the DB cluster isn\'t publicly accessible, it is an internal DB
    -- cluster with a DNS name that resolves to a private IP address.
    --
    -- Default: The default behavior varies depending on whether
    -- @DBSubnetGroupName@ is specified.
    --
    -- If @DBSubnetGroupName@ isn\'t specified, and @PubliclyAccessible@ isn\'t
    -- specified, the following applies:
    --
    -- -   If the default VPC in the target Region doesn’t have an internet
    --     gateway attached to it, the DB cluster is private.
    --
    -- -   If the default VPC in the target Region has an internet gateway
    --     attached to it, the DB cluster is public.
    --
    -- If @DBSubnetGroupName@ is specified, and @PubliclyAccessible@ isn\'t
    -- specified, the following applies:
    --
    -- -   If the subnets are part of a VPC that doesn’t have an internet
    --     gateway attached to it, the DB cluster is private.
    --
    -- -   If the subnets are part of a VPC that has an internet gateway
    --     attached to it, the DB cluster is public.
    --
    -- Valid for: Multi-AZ DB clusters only
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the source DB instance or DB cluster
    -- if this DB cluster is created as a read replica.
    --
    -- Valid for: Aurora DB clusters only
    replicationSourceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | For DB clusters in @serverless@ DB engine mode, the scaling properties
    -- of the DB cluster.
    --
    -- Valid for: Aurora DB clusters only
    scalingConfiguration :: Prelude.Maybe ScalingConfiguration,
    serverlessV2ScalingConfiguration :: Prelude.Maybe ServerlessV2ScalingConfiguration,
    -- | A value that indicates whether the DB cluster is encrypted.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    storageEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the storage type to be associated with the DB cluster.
    --
    -- This setting is required to create a Multi-AZ DB cluster.
    --
    -- Valid values: @io1@
    --
    -- When specified, a value for the @Iops@ parameter is required.
    --
    -- Default: @io1@
    --
    -- Valid for: Multi-AZ DB clusters only
    storageType :: Prelude.Maybe Prelude.Text,
    -- | Tags to assign to the DB cluster.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    tags :: Prelude.Maybe [Tag],
    -- | A list of EC2 VPC security groups to associate with this DB cluster.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The DB cluster identifier. This parameter is stored as a lowercase
    -- string.
    --
    -- Constraints:
    --
    -- -   Must contain from 1 to 63 letters, numbers, or hyphens.
    --
    -- -   First character must be a letter.
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens.
    --
    -- Example: @my-cluster1@
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    dbClusterIdentifier :: Prelude.Text,
    -- | The name of the database engine to be used for this DB cluster.
    --
    -- Valid Values:
    --
    -- -   @aurora@ (for MySQL 5.6-compatible Aurora)
    --
    -- -   @aurora-mysql@ (for MySQL 5.7-compatible and MySQL 8.0-compatible
    --     Aurora)
    --
    -- -   @aurora-postgresql@
    --
    -- -   @mysql@
    --
    -- -   @postgres@
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    engine :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDBCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allocatedStorage', 'createDBCluster_allocatedStorage' - The amount of storage in gibibytes (GiB) to allocate to each DB instance
-- in the Multi-AZ DB cluster.
--
-- This setting is required to create a Multi-AZ DB cluster.
--
-- Valid for: Multi-AZ DB clusters only
--
-- 'autoMinorVersionUpgrade', 'createDBCluster_autoMinorVersionUpgrade' - A value that indicates whether minor engine upgrades are applied
-- automatically to the DB cluster during the maintenance window. By
-- default, minor engine upgrades are applied automatically.
--
-- Valid for: Multi-AZ DB clusters only
--
-- 'availabilityZones', 'createDBCluster_availabilityZones' - A list of Availability Zones (AZs) where DB instances in the DB cluster
-- can be created.
--
-- For information on Amazon Web Services Regions and Availability Zones,
-- see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Concepts.RegionsAndAvailabilityZones.html Choosing the Regions and Availability Zones>
-- in the /Amazon Aurora User Guide/.
--
-- Valid for: Aurora DB clusters only
--
-- 'backtrackWindow', 'createDBCluster_backtrackWindow' - The target backtrack window, in seconds. To disable backtracking, set
-- this value to 0.
--
-- Default: 0
--
-- Constraints:
--
-- -   If specified, this value must be set to a number from 0 to 259,200
--     (72 hours).
--
-- Valid for: Aurora MySQL DB clusters only
--
-- 'backupRetentionPeriod', 'createDBCluster_backupRetentionPeriod' - The number of days for which automated backups are retained.
--
-- Default: 1
--
-- Constraints:
--
-- -   Must be a value from 1 to 35
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'characterSetName', 'createDBCluster_characterSetName' - A value that indicates that the DB cluster should be associated with the
-- specified CharacterSet.
--
-- Valid for: Aurora DB clusters only
--
-- 'copyTagsToSnapshot', 'createDBCluster_copyTagsToSnapshot' - A value that indicates whether to copy all tags from the DB cluster to
-- snapshots of the DB cluster. The default is not to copy them.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'dbClusterInstanceClass', 'createDBCluster_dbClusterInstanceClass' - The compute and memory capacity of each DB instance in the Multi-AZ DB
-- cluster, for example db.m6gd.xlarge. Not all DB instance classes are
-- available in all Amazon Web Services Regions, or for all database
-- engines.
--
-- For the full list of DB instance classes and availability for your
-- engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB instance class>
-- in the /Amazon RDS User Guide/.
--
-- This setting is required to create a Multi-AZ DB cluster.
--
-- Valid for: Multi-AZ DB clusters only
--
-- 'dbClusterParameterGroupName', 'createDBCluster_dbClusterParameterGroupName' - The name of the DB cluster parameter group to associate with this DB
-- cluster. If you do not specify a value, then the default DB cluster
-- parameter group for the specified DB engine and version is used.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing DB cluster parameter
--     group.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'dbSubnetGroupName', 'createDBCluster_dbSubnetGroupName' - A DB subnet group to associate with this DB cluster.
--
-- This setting is required to create a Multi-AZ DB cluster.
--
-- Constraints: Must match the name of an existing DBSubnetGroup. Must not
-- be default.
--
-- Example: @mydbsubnetgroup@
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'dbSystemId', 'createDBCluster_dbSystemId' - Reserved for future use.
--
-- 'databaseName', 'createDBCluster_databaseName' - The name for your database of up to 64 alphanumeric characters. If you
-- do not provide a name, Amazon RDS doesn\'t create a database in the DB
-- cluster you are creating.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'deletionProtection', 'createDBCluster_deletionProtection' - A value that indicates whether the DB cluster has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection isn\'t enabled.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'destinationRegion', 'createDBCluster_destinationRegion' - Pseudo-parameter used when populating the @PreSignedUrl@ of a
-- cross-region @CreateDBCluster@ request. To replicate from region @SRC@
-- to region @DST@, send a request to region @DST@. In that request, pass a
-- @PreSignedUrl@ for region @SRC@ with @DestinationRegion@ set to region
-- @DST@.
--
-- 'domain', 'createDBCluster_domain' - The Active Directory directory ID to create the DB cluster in.
--
-- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos
-- authentication to authenticate users that connect to the DB cluster.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos authentication>
-- in the /Amazon Aurora User Guide/.
--
-- Valid for: Aurora DB clusters only
--
-- 'domainIAMRoleName', 'createDBCluster_domainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
--
-- Valid for: Aurora DB clusters only
--
-- 'enableCloudwatchLogsExports', 'createDBCluster_enableCloudwatchLogsExports' - The list of log types that need to be enabled for exporting to
-- CloudWatch Logs. The values in the list depend on the DB engine being
-- used.
--
-- __RDS for MySQL__
--
-- Possible values are @error@, @general@, and @slowquery@.
--
-- __RDS for PostgreSQL__
--
-- Possible values are @postgresql@ and @upgrade@.
--
-- __Aurora MySQL__
--
-- Possible values are @audit@, @error@, @general@, and @slowquery@.
--
-- __Aurora PostgreSQL__
--
-- Possible value is @postgresql@.
--
-- For more information about exporting CloudWatch Logs for Amazon RDS, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon RDS User Guide/.
--
-- For more information about exporting CloudWatch Logs for Amazon Aurora,
-- see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon Aurora User Guide/.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'enableGlobalWriteForwarding', 'createDBCluster_enableGlobalWriteForwarding' - A value that indicates whether to enable this DB cluster to forward
-- write operations to the primary cluster of an Aurora global database
-- (GlobalCluster). By default, write operations are not allowed on Aurora
-- DB clusters that are secondary clusters in an Aurora global database.
--
-- You can set this value only on Aurora DB clusters that are members of an
-- Aurora global database. With this parameter enabled, a secondary cluster
-- can forward writes to the current primary cluster and the resulting
-- changes are replicated back to this cluster. For the primary DB cluster
-- of an Aurora global database, this value is used immediately if the
-- primary is demoted by the FailoverGlobalCluster API operation, but it
-- does nothing until then.
--
-- Valid for: Aurora DB clusters only
--
-- 'enableHttpEndpoint', 'createDBCluster_enableHttpEndpoint' - A value that indicates whether to enable the HTTP endpoint for an Aurora
-- Serverless v1 DB cluster. By default, the HTTP endpoint is disabled.
--
-- When enabled, the HTTP endpoint provides a connectionless web service
-- API for running SQL queries on the Aurora Serverless v1 DB cluster. You
-- can also query your database from inside the RDS console with the query
-- editor.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/data-api.html Using the Data API for Aurora Serverless v1>
-- in the /Amazon Aurora User Guide/.
--
-- Valid for: Aurora DB clusters only
--
-- 'enableIAMDatabaseAuthentication', 'createDBCluster_enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts. By
-- default, mapping isn\'t enabled.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
-- in the /Amazon Aurora User Guide/.
--
-- Valid for: Aurora DB clusters only
--
-- 'enablePerformanceInsights', 'createDBCluster_enablePerformanceInsights' - A value that indicates whether to turn on Performance Insights for the
-- DB cluster.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights>
-- in the /Amazon RDS User Guide/.
--
-- Valid for: Multi-AZ DB clusters only
--
-- 'engineMode', 'createDBCluster_engineMode' - The DB engine mode of the DB cluster, either @provisioned@,
-- @serverless@, @parallelquery@, @global@, or @multimaster@.
--
-- The @parallelquery@ engine mode isn\'t required for Aurora MySQL version
-- 1.23 and higher 1.x versions, and version 2.09 and higher 2.x versions.
--
-- The @global@ engine mode isn\'t required for Aurora MySQL version 1.22
-- and higher 1.x versions, and @global@ engine mode isn\'t required for
-- any 2.x versions.
--
-- The @multimaster@ engine mode only applies for DB clusters created with
-- Aurora MySQL version 5.6.10a.
--
-- The @serverless@ engine mode only applies for Aurora Serverless v1 DB
-- clusters.
--
-- For Aurora PostgreSQL, the @global@ engine mode isn\'t required, and
-- both the @parallelquery@ and the @multimaster@ engine modes currently
-- aren\'t supported.
--
-- Limitations and requirements apply to some DB engine modes. For more
-- information, see the following sections in the /Amazon Aurora User
-- Guide/:
--
-- -   <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html#aurora-serverless.limitations Limitations of Aurora Serverless v1>
--
-- -   <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless-v2.requirements.html Requirements for Aurora Serverless v2>
--
-- -   <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-mysql-parallel-query.html#aurora-mysql-parallel-query-limitations Limitations of Parallel Query>
--
-- -   <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-global-database.html#aurora-global-database.limitations Limitations of Aurora Global Databases>
--
-- -   <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-multi-master.html#aurora-multi-master-limitations Limitations of Multi-Master Clusters>
--
-- Valid for: Aurora DB clusters only
--
-- 'engineVersion', 'createDBCluster_engineVersion' - The version number of the database engine to use.
--
-- To list all of the available engine versions for MySQL 5.6-compatible
-- Aurora, use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora --query \"DBEngineVersions[].EngineVersion\"@
--
-- To list all of the available engine versions for MySQL 5.7-compatible
-- and MySQL 8.0-compatible Aurora, use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora-mysql --query \"DBEngineVersions[].EngineVersion\"@
--
-- To list all of the available engine versions for Aurora PostgreSQL, use
-- the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora-postgresql --query \"DBEngineVersions[].EngineVersion\"@
--
-- To list all of the available engine versions for RDS for MySQL, use the
-- following command:
--
-- @aws rds describe-db-engine-versions --engine mysql --query \"DBEngineVersions[].EngineVersion\"@
--
-- To list all of the available engine versions for RDS for PostgreSQL, use
-- the following command:
--
-- @aws rds describe-db-engine-versions --engine postgres --query \"DBEngineVersions[].EngineVersion\"@
--
-- __Aurora MySQL__
--
-- For information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/AuroraMySQL.Updates.html MySQL on Amazon RDS Versions>
-- in the /Amazon Aurora User Guide/.
--
-- __Aurora PostgreSQL__
--
-- For information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/AuroraPostgreSQL.Updates.20180305.html Amazon Aurora PostgreSQL releases and engine versions>
-- in the /Amazon Aurora User Guide/.
--
-- __MySQL__
--
-- For information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_MySQL.html#MySQL.Concepts.VersionMgmt MySQL on Amazon RDS Versions>
-- in the /Amazon RDS User Guide/.
--
-- __PostgreSQL__
--
-- For information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_PostgreSQL.html#PostgreSQL.Concepts Amazon RDS for PostgreSQL versions and extensions>
-- in the /Amazon RDS User Guide/.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'globalClusterIdentifier', 'createDBCluster_globalClusterIdentifier' - The global cluster ID of an Aurora cluster that becomes the primary
-- cluster in the new global database cluster.
--
-- Valid for: Aurora DB clusters only
--
-- 'iops', 'createDBCluster_iops' - The amount of Provisioned IOPS (input\/output operations per second) to
-- be initially allocated for each DB instance in the Multi-AZ DB cluster.
--
-- For information about valid IOPS values, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS storage>
-- in the /Amazon RDS User Guide/.
--
-- This setting is required to create a Multi-AZ DB cluster.
--
-- Constraints: Must be a multiple between .5 and 50 of the storage amount
-- for the DB cluster.
--
-- Valid for: Multi-AZ DB clusters only
--
-- 'kmsKeyId', 'createDBCluster_kmsKeyId' - The Amazon Web Services KMS key identifier for an encrypted DB cluster.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key. To use a KMS key in a different
-- Amazon Web Services account, specify the key ARN or alias ARN.
--
-- When a KMS key isn\'t specified in @KmsKeyId@:
--
-- -   If @ReplicationSourceIdentifier@ identifies an encrypted source,
--     then Amazon RDS will use the KMS key used to encrypt the source.
--     Otherwise, Amazon RDS will use your default KMS key.
--
-- -   If the @StorageEncrypted@ parameter is enabled and
--     @ReplicationSourceIdentifier@ isn\'t specified, then Amazon RDS will
--     use your default KMS key.
--
-- There is a default KMS key for your Amazon Web Services account. Your
-- Amazon Web Services account has a different default KMS key for each
-- Amazon Web Services Region.
--
-- If you create a read replica of an encrypted DB cluster in another
-- Amazon Web Services Region, you must set @KmsKeyId@ to a KMS key
-- identifier that is valid in the destination Amazon Web Services Region.
-- This KMS key is used to encrypt the read replica in that Amazon Web
-- Services Region.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'manageMasterUserPassword', 'createDBCluster_manageMasterUserPassword' - A value that indicates whether to manage the master user password with
-- Amazon Web Services Secrets Manager.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
-- in the /Amazon RDS User Guide/ and
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
-- in the /Amazon Aurora User Guide./
--
-- Constraints:
--
-- -   Can\'t manage the master user password with Amazon Web Services
--     Secrets Manager if @MasterUserPassword@ is specified.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'masterUserPassword', 'createDBCluster_masterUserPassword' - The password for the master database user. This password can contain any
-- printable ASCII character except \"\/\", \"\"\", or \"\@\".
--
-- Constraints:
--
-- -   Must contain from 8 to 41 characters.
--
-- -   Can\'t be specified if @ManageMasterUserPassword@ is turned on.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'masterUserSecretKmsKeyId', 'createDBCluster_masterUserSecretKmsKeyId' - The Amazon Web Services KMS key identifier to encrypt a secret that is
-- automatically generated and managed in Amazon Web Services Secrets
-- Manager.
--
-- This setting is valid only if the master user password is managed by RDS
-- in Amazon Web Services Secrets Manager for the DB cluster.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key. To use a KMS key in a different
-- Amazon Web Services account, specify the key ARN or alias ARN.
--
-- If you don\'t specify @MasterUserSecretKmsKeyId@, then the
-- @aws\/secretsmanager@ KMS key is used to encrypt the secret. If the
-- secret is in a different Amazon Web Services account, then you can\'t
-- use the @aws\/secretsmanager@ KMS key to encrypt the secret, and you
-- must use a customer managed KMS key.
--
-- There is a default KMS key for your Amazon Web Services account. Your
-- Amazon Web Services account has a different default KMS key for each
-- Amazon Web Services Region.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'masterUsername', 'createDBCluster_masterUsername' - The name of the master user for the DB cluster.
--
-- Constraints:
--
-- -   Must be 1 to 16 letters or numbers.
--
-- -   First character must be a letter.
--
-- -   Can\'t be a reserved word for the chosen database engine.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'monitoringInterval', 'createDBCluster_monitoringInterval' - The interval, in seconds, between points when Enhanced Monitoring
-- metrics are collected for the DB cluster. To turn off collecting
-- Enhanced Monitoring metrics, specify 0. The default is 0.
--
-- If @MonitoringRoleArn@ is specified, also set @MonitoringInterval@ to a
-- value other than 0.
--
-- Valid Values: @0, 1, 5, 10, 15, 30, 60@
--
-- Valid for: Multi-AZ DB clusters only
--
-- 'monitoringRoleArn', 'createDBCluster_monitoringRoleArn' - The Amazon Resource Name (ARN) for the IAM role that permits RDS to send
-- Enhanced Monitoring metrics to Amazon CloudWatch Logs. An example is
-- @arn:aws:iam:123456789012:role\/emaccess@. For information on creating a
-- monitoring role, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.OS.html#USER_Monitoring.OS.Enabling Setting up and enabling Enhanced Monitoring>
-- in the /Amazon RDS User Guide/.
--
-- If @MonitoringInterval@ is set to a value other than 0, supply a
-- @MonitoringRoleArn@ value.
--
-- Valid for: Multi-AZ DB clusters only
--
-- 'networkType', 'createDBCluster_networkType' - The network type of the DB cluster.
--
-- Valid values:
--
-- -   @IPV4@
--
-- -   @DUAL@
--
-- The network type is determined by the @DBSubnetGroup@ specified for the
-- DB cluster. A @DBSubnetGroup@ can support only the IPv4 protocol or the
-- IPv4 and the IPv6 protocols (@DUAL@).
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html Working with a DB instance in a VPC>
-- in the /Amazon Aurora User Guide./
--
-- Valid for: Aurora DB clusters only
--
-- 'optionGroupName', 'createDBCluster_optionGroupName' - A value that indicates that the DB cluster should be associated with the
-- specified option group.
--
-- DB clusters are associated with a default option group that can\'t be
-- modified.
--
-- 'performanceInsightsKMSKeyId', 'createDBCluster_performanceInsightsKMSKeyId' - The Amazon Web Services KMS key identifier for encryption of Performance
-- Insights data.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key.
--
-- If you don\'t specify a value for @PerformanceInsightsKMSKeyId@, then
-- Amazon RDS uses your default KMS key. There is a default KMS key for
-- your Amazon Web Services account. Your Amazon Web Services account has a
-- different default KMS key for each Amazon Web Services Region.
--
-- Valid for: Multi-AZ DB clusters only
--
-- 'performanceInsightsRetentionPeriod', 'createDBCluster_performanceInsightsRetentionPeriod' - The number of days to retain Performance Insights data. The default is 7
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
-- Valid for: Multi-AZ DB clusters only
--
-- 'port', 'createDBCluster_port' - The port number on which the instances in the DB cluster accept
-- connections.
--
-- __RDS for MySQL and Aurora MySQL__
--
-- Default: @3306@
--
-- Valid values: @1150-65535@
--
-- __RDS for PostgreSQL and Aurora PostgreSQL__
--
-- Default: @5432@
--
-- Valid values: @1150-65535@
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'preSignedUrl', 'createDBCluster_preSignedUrl' - When you are replicating a DB cluster from one Amazon Web Services
-- GovCloud (US) Region to another, an URL that contains a Signature
-- Version 4 signed request for the @CreateDBCluster@ operation to be
-- called in the source Amazon Web Services Region where the DB cluster is
-- replicated from. Specify @PreSignedUrl@ only when you are performing
-- cross-Region replication from an encrypted DB cluster.
--
-- The presigned URL must be a valid request for the @CreateDBCluster@ API
-- operation that can run in the source Amazon Web Services Region that
-- contains the encrypted DB cluster to copy.
--
-- The presigned URL request must contain the following parameter values:
--
-- -   @KmsKeyId@ - The KMS key identifier for the KMS key to use to
--     encrypt the copy of the DB cluster in the destination Amazon Web
--     Services Region. This should refer to the same KMS key for both the
--     @CreateDBCluster@ operation that is called in the destination Amazon
--     Web Services Region, and the operation contained in the presigned
--     URL.
--
-- -   @DestinationRegion@ - The name of the Amazon Web Services Region
--     that Aurora read replica will be created in.
--
-- -   @ReplicationSourceIdentifier@ - The DB cluster identifier for the
--     encrypted DB cluster to be copied. This identifier must be in the
--     Amazon Resource Name (ARN) format for the source Amazon Web Services
--     Region. For example, if you are copying an encrypted DB cluster from
--     the us-west-2 Amazon Web Services Region, then your
--     @ReplicationSourceIdentifier@ would look like Example:
--     @arn:aws:rds:us-west-2:123456789012:cluster:aurora-cluster1@.
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
-- Valid for: Aurora DB clusters only
--
-- 'preferredBackupWindow', 'createDBCluster_preferredBackupWindow' - The daily time range during which automated backups are created if
-- automated backups are enabled using the @BackupRetentionPeriod@
-- parameter.
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Web Services Region. To view the time
-- blocks available, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.Backups.BackupWindow Backup window>
-- in the /Amazon Aurora User Guide/.
--
-- Constraints:
--
-- -   Must be in the format @hh24:mi-hh24:mi@.
--
-- -   Must be in Universal Coordinated Time (UTC).
--
-- -   Must not conflict with the preferred maintenance window.
--
-- -   Must be at least 30 minutes.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'preferredMaintenanceWindow', 'createDBCluster_preferredMaintenanceWindow' - The weekly time range during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Web Services Region, occurring on a random
-- day of the week. To see the time blocks available, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred DB Cluster Maintenance Window>
-- in the /Amazon Aurora User Guide/.
--
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
--
-- Constraints: Minimum 30-minute window.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'publiclyAccessible', 'createDBCluster_publiclyAccessible' - A value that indicates whether the DB cluster is publicly accessible.
--
-- When the DB cluster is publicly accessible, its Domain Name System (DNS)
-- endpoint resolves to the private IP address from within the DB
-- cluster\'s virtual private cloud (VPC). It resolves to the public IP
-- address from outside of the DB cluster\'s VPC. Access to the DB cluster
-- is ultimately controlled by the security group it uses. That public
-- access isn\'t permitted if the security group assigned to the DB cluster
-- doesn\'t permit it.
--
-- When the DB cluster isn\'t publicly accessible, it is an internal DB
-- cluster with a DNS name that resolves to a private IP address.
--
-- Default: The default behavior varies depending on whether
-- @DBSubnetGroupName@ is specified.
--
-- If @DBSubnetGroupName@ isn\'t specified, and @PubliclyAccessible@ isn\'t
-- specified, the following applies:
--
-- -   If the default VPC in the target Region doesn’t have an internet
--     gateway attached to it, the DB cluster is private.
--
-- -   If the default VPC in the target Region has an internet gateway
--     attached to it, the DB cluster is public.
--
-- If @DBSubnetGroupName@ is specified, and @PubliclyAccessible@ isn\'t
-- specified, the following applies:
--
-- -   If the subnets are part of a VPC that doesn’t have an internet
--     gateway attached to it, the DB cluster is private.
--
-- -   If the subnets are part of a VPC that has an internet gateway
--     attached to it, the DB cluster is public.
--
-- Valid for: Multi-AZ DB clusters only
--
-- 'replicationSourceIdentifier', 'createDBCluster_replicationSourceIdentifier' - The Amazon Resource Name (ARN) of the source DB instance or DB cluster
-- if this DB cluster is created as a read replica.
--
-- Valid for: Aurora DB clusters only
--
-- 'scalingConfiguration', 'createDBCluster_scalingConfiguration' - For DB clusters in @serverless@ DB engine mode, the scaling properties
-- of the DB cluster.
--
-- Valid for: Aurora DB clusters only
--
-- 'serverlessV2ScalingConfiguration', 'createDBCluster_serverlessV2ScalingConfiguration' - Undocumented member.
--
-- 'storageEncrypted', 'createDBCluster_storageEncrypted' - A value that indicates whether the DB cluster is encrypted.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'storageType', 'createDBCluster_storageType' - Specifies the storage type to be associated with the DB cluster.
--
-- This setting is required to create a Multi-AZ DB cluster.
--
-- Valid values: @io1@
--
-- When specified, a value for the @Iops@ parameter is required.
--
-- Default: @io1@
--
-- Valid for: Multi-AZ DB clusters only
--
-- 'tags', 'createDBCluster_tags' - Tags to assign to the DB cluster.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'vpcSecurityGroupIds', 'createDBCluster_vpcSecurityGroupIds' - A list of EC2 VPC security groups to associate with this DB cluster.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'dbClusterIdentifier', 'createDBCluster_dbClusterIdentifier' - The DB cluster identifier. This parameter is stored as a lowercase
-- string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens.
--
-- -   First character must be a letter.
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens.
--
-- Example: @my-cluster1@
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'engine', 'createDBCluster_engine' - The name of the database engine to be used for this DB cluster.
--
-- Valid Values:
--
-- -   @aurora@ (for MySQL 5.6-compatible Aurora)
--
-- -   @aurora-mysql@ (for MySQL 5.7-compatible and MySQL 8.0-compatible
--     Aurora)
--
-- -   @aurora-postgresql@
--
-- -   @mysql@
--
-- -   @postgres@
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
newCreateDBCluster ::
  -- | 'dbClusterIdentifier'
  Prelude.Text ->
  -- | 'engine'
  Prelude.Text ->
  CreateDBCluster
newCreateDBCluster pDBClusterIdentifier_ pEngine_ =
  CreateDBCluster'
    { allocatedStorage =
        Prelude.Nothing,
      autoMinorVersionUpgrade = Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      backtrackWindow = Prelude.Nothing,
      backupRetentionPeriod = Prelude.Nothing,
      characterSetName = Prelude.Nothing,
      copyTagsToSnapshot = Prelude.Nothing,
      dbClusterInstanceClass = Prelude.Nothing,
      dbClusterParameterGroupName = Prelude.Nothing,
      dbSubnetGroupName = Prelude.Nothing,
      dbSystemId = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      deletionProtection = Prelude.Nothing,
      destinationRegion = Prelude.Nothing,
      domain = Prelude.Nothing,
      domainIAMRoleName = Prelude.Nothing,
      enableCloudwatchLogsExports = Prelude.Nothing,
      enableGlobalWriteForwarding = Prelude.Nothing,
      enableHttpEndpoint = Prelude.Nothing,
      enableIAMDatabaseAuthentication = Prelude.Nothing,
      enablePerformanceInsights = Prelude.Nothing,
      engineMode = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      globalClusterIdentifier = Prelude.Nothing,
      iops = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      manageMasterUserPassword = Prelude.Nothing,
      masterUserPassword = Prelude.Nothing,
      masterUserSecretKmsKeyId = Prelude.Nothing,
      masterUsername = Prelude.Nothing,
      monitoringInterval = Prelude.Nothing,
      monitoringRoleArn = Prelude.Nothing,
      networkType = Prelude.Nothing,
      optionGroupName = Prelude.Nothing,
      performanceInsightsKMSKeyId = Prelude.Nothing,
      performanceInsightsRetentionPeriod = Prelude.Nothing,
      port = Prelude.Nothing,
      preSignedUrl = Prelude.Nothing,
      preferredBackupWindow = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      publiclyAccessible = Prelude.Nothing,
      replicationSourceIdentifier = Prelude.Nothing,
      scalingConfiguration = Prelude.Nothing,
      serverlessV2ScalingConfiguration = Prelude.Nothing,
      storageEncrypted = Prelude.Nothing,
      storageType = Prelude.Nothing,
      tags = Prelude.Nothing,
      vpcSecurityGroupIds = Prelude.Nothing,
      dbClusterIdentifier = pDBClusterIdentifier_,
      engine = pEngine_
    }

-- | The amount of storage in gibibytes (GiB) to allocate to each DB instance
-- in the Multi-AZ DB cluster.
--
-- This setting is required to create a Multi-AZ DB cluster.
--
-- Valid for: Multi-AZ DB clusters only
createDBCluster_allocatedStorage :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Int)
createDBCluster_allocatedStorage = Lens.lens (\CreateDBCluster' {allocatedStorage} -> allocatedStorage) (\s@CreateDBCluster' {} a -> s {allocatedStorage = a} :: CreateDBCluster)

-- | A value that indicates whether minor engine upgrades are applied
-- automatically to the DB cluster during the maintenance window. By
-- default, minor engine upgrades are applied automatically.
--
-- Valid for: Multi-AZ DB clusters only
createDBCluster_autoMinorVersionUpgrade :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Bool)
createDBCluster_autoMinorVersionUpgrade = Lens.lens (\CreateDBCluster' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@CreateDBCluster' {} a -> s {autoMinorVersionUpgrade = a} :: CreateDBCluster)

-- | A list of Availability Zones (AZs) where DB instances in the DB cluster
-- can be created.
--
-- For information on Amazon Web Services Regions and Availability Zones,
-- see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Concepts.RegionsAndAvailabilityZones.html Choosing the Regions and Availability Zones>
-- in the /Amazon Aurora User Guide/.
--
-- Valid for: Aurora DB clusters only
createDBCluster_availabilityZones :: Lens.Lens' CreateDBCluster (Prelude.Maybe [Prelude.Text])
createDBCluster_availabilityZones = Lens.lens (\CreateDBCluster' {availabilityZones} -> availabilityZones) (\s@CreateDBCluster' {} a -> s {availabilityZones = a} :: CreateDBCluster) Prelude.. Lens.mapping Lens.coerced

-- | The target backtrack window, in seconds. To disable backtracking, set
-- this value to 0.
--
-- Default: 0
--
-- Constraints:
--
-- -   If specified, this value must be set to a number from 0 to 259,200
--     (72 hours).
--
-- Valid for: Aurora MySQL DB clusters only
createDBCluster_backtrackWindow :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Integer)
createDBCluster_backtrackWindow = Lens.lens (\CreateDBCluster' {backtrackWindow} -> backtrackWindow) (\s@CreateDBCluster' {} a -> s {backtrackWindow = a} :: CreateDBCluster)

-- | The number of days for which automated backups are retained.
--
-- Default: 1
--
-- Constraints:
--
-- -   Must be a value from 1 to 35
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
createDBCluster_backupRetentionPeriod :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Int)
createDBCluster_backupRetentionPeriod = Lens.lens (\CreateDBCluster' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@CreateDBCluster' {} a -> s {backupRetentionPeriod = a} :: CreateDBCluster)

-- | A value that indicates that the DB cluster should be associated with the
-- specified CharacterSet.
--
-- Valid for: Aurora DB clusters only
createDBCluster_characterSetName :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_characterSetName = Lens.lens (\CreateDBCluster' {characterSetName} -> characterSetName) (\s@CreateDBCluster' {} a -> s {characterSetName = a} :: CreateDBCluster)

-- | A value that indicates whether to copy all tags from the DB cluster to
-- snapshots of the DB cluster. The default is not to copy them.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
createDBCluster_copyTagsToSnapshot :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Bool)
createDBCluster_copyTagsToSnapshot = Lens.lens (\CreateDBCluster' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@CreateDBCluster' {} a -> s {copyTagsToSnapshot = a} :: CreateDBCluster)

-- | The compute and memory capacity of each DB instance in the Multi-AZ DB
-- cluster, for example db.m6gd.xlarge. Not all DB instance classes are
-- available in all Amazon Web Services Regions, or for all database
-- engines.
--
-- For the full list of DB instance classes and availability for your
-- engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB instance class>
-- in the /Amazon RDS User Guide/.
--
-- This setting is required to create a Multi-AZ DB cluster.
--
-- Valid for: Multi-AZ DB clusters only
createDBCluster_dbClusterInstanceClass :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_dbClusterInstanceClass = Lens.lens (\CreateDBCluster' {dbClusterInstanceClass} -> dbClusterInstanceClass) (\s@CreateDBCluster' {} a -> s {dbClusterInstanceClass = a} :: CreateDBCluster)

-- | The name of the DB cluster parameter group to associate with this DB
-- cluster. If you do not specify a value, then the default DB cluster
-- parameter group for the specified DB engine and version is used.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing DB cluster parameter
--     group.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
createDBCluster_dbClusterParameterGroupName :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_dbClusterParameterGroupName = Lens.lens (\CreateDBCluster' {dbClusterParameterGroupName} -> dbClusterParameterGroupName) (\s@CreateDBCluster' {} a -> s {dbClusterParameterGroupName = a} :: CreateDBCluster)

-- | A DB subnet group to associate with this DB cluster.
--
-- This setting is required to create a Multi-AZ DB cluster.
--
-- Constraints: Must match the name of an existing DBSubnetGroup. Must not
-- be default.
--
-- Example: @mydbsubnetgroup@
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
createDBCluster_dbSubnetGroupName :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_dbSubnetGroupName = Lens.lens (\CreateDBCluster' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@CreateDBCluster' {} a -> s {dbSubnetGroupName = a} :: CreateDBCluster)

-- | Reserved for future use.
createDBCluster_dbSystemId :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_dbSystemId = Lens.lens (\CreateDBCluster' {dbSystemId} -> dbSystemId) (\s@CreateDBCluster' {} a -> s {dbSystemId = a} :: CreateDBCluster)

-- | The name for your database of up to 64 alphanumeric characters. If you
-- do not provide a name, Amazon RDS doesn\'t create a database in the DB
-- cluster you are creating.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
createDBCluster_databaseName :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_databaseName = Lens.lens (\CreateDBCluster' {databaseName} -> databaseName) (\s@CreateDBCluster' {} a -> s {databaseName = a} :: CreateDBCluster)

-- | A value that indicates whether the DB cluster has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection isn\'t enabled.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
createDBCluster_deletionProtection :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Bool)
createDBCluster_deletionProtection = Lens.lens (\CreateDBCluster' {deletionProtection} -> deletionProtection) (\s@CreateDBCluster' {} a -> s {deletionProtection = a} :: CreateDBCluster)

-- | Pseudo-parameter used when populating the @PreSignedUrl@ of a
-- cross-region @CreateDBCluster@ request. To replicate from region @SRC@
-- to region @DST@, send a request to region @DST@. In that request, pass a
-- @PreSignedUrl@ for region @SRC@ with @DestinationRegion@ set to region
-- @DST@.
createDBCluster_destinationRegion :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_destinationRegion = Lens.lens (\CreateDBCluster' {destinationRegion} -> destinationRegion) (\s@CreateDBCluster' {} a -> s {destinationRegion = a} :: CreateDBCluster)

-- | The Active Directory directory ID to create the DB cluster in.
--
-- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos
-- authentication to authenticate users that connect to the DB cluster.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos authentication>
-- in the /Amazon Aurora User Guide/.
--
-- Valid for: Aurora DB clusters only
createDBCluster_domain :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_domain = Lens.lens (\CreateDBCluster' {domain} -> domain) (\s@CreateDBCluster' {} a -> s {domain = a} :: CreateDBCluster)

-- | Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
--
-- Valid for: Aurora DB clusters only
createDBCluster_domainIAMRoleName :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_domainIAMRoleName = Lens.lens (\CreateDBCluster' {domainIAMRoleName} -> domainIAMRoleName) (\s@CreateDBCluster' {} a -> s {domainIAMRoleName = a} :: CreateDBCluster)

-- | The list of log types that need to be enabled for exporting to
-- CloudWatch Logs. The values in the list depend on the DB engine being
-- used.
--
-- __RDS for MySQL__
--
-- Possible values are @error@, @general@, and @slowquery@.
--
-- __RDS for PostgreSQL__
--
-- Possible values are @postgresql@ and @upgrade@.
--
-- __Aurora MySQL__
--
-- Possible values are @audit@, @error@, @general@, and @slowquery@.
--
-- __Aurora PostgreSQL__
--
-- Possible value is @postgresql@.
--
-- For more information about exporting CloudWatch Logs for Amazon RDS, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon RDS User Guide/.
--
-- For more information about exporting CloudWatch Logs for Amazon Aurora,
-- see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon Aurora User Guide/.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
createDBCluster_enableCloudwatchLogsExports :: Lens.Lens' CreateDBCluster (Prelude.Maybe [Prelude.Text])
createDBCluster_enableCloudwatchLogsExports = Lens.lens (\CreateDBCluster' {enableCloudwatchLogsExports} -> enableCloudwatchLogsExports) (\s@CreateDBCluster' {} a -> s {enableCloudwatchLogsExports = a} :: CreateDBCluster) Prelude.. Lens.mapping Lens.coerced

-- | A value that indicates whether to enable this DB cluster to forward
-- write operations to the primary cluster of an Aurora global database
-- (GlobalCluster). By default, write operations are not allowed on Aurora
-- DB clusters that are secondary clusters in an Aurora global database.
--
-- You can set this value only on Aurora DB clusters that are members of an
-- Aurora global database. With this parameter enabled, a secondary cluster
-- can forward writes to the current primary cluster and the resulting
-- changes are replicated back to this cluster. For the primary DB cluster
-- of an Aurora global database, this value is used immediately if the
-- primary is demoted by the FailoverGlobalCluster API operation, but it
-- does nothing until then.
--
-- Valid for: Aurora DB clusters only
createDBCluster_enableGlobalWriteForwarding :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Bool)
createDBCluster_enableGlobalWriteForwarding = Lens.lens (\CreateDBCluster' {enableGlobalWriteForwarding} -> enableGlobalWriteForwarding) (\s@CreateDBCluster' {} a -> s {enableGlobalWriteForwarding = a} :: CreateDBCluster)

-- | A value that indicates whether to enable the HTTP endpoint for an Aurora
-- Serverless v1 DB cluster. By default, the HTTP endpoint is disabled.
--
-- When enabled, the HTTP endpoint provides a connectionless web service
-- API for running SQL queries on the Aurora Serverless v1 DB cluster. You
-- can also query your database from inside the RDS console with the query
-- editor.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/data-api.html Using the Data API for Aurora Serverless v1>
-- in the /Amazon Aurora User Guide/.
--
-- Valid for: Aurora DB clusters only
createDBCluster_enableHttpEndpoint :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Bool)
createDBCluster_enableHttpEndpoint = Lens.lens (\CreateDBCluster' {enableHttpEndpoint} -> enableHttpEndpoint) (\s@CreateDBCluster' {} a -> s {enableHttpEndpoint = a} :: CreateDBCluster)

-- | A value that indicates whether to enable mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts. By
-- default, mapping isn\'t enabled.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
-- in the /Amazon Aurora User Guide/.
--
-- Valid for: Aurora DB clusters only
createDBCluster_enableIAMDatabaseAuthentication :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Bool)
createDBCluster_enableIAMDatabaseAuthentication = Lens.lens (\CreateDBCluster' {enableIAMDatabaseAuthentication} -> enableIAMDatabaseAuthentication) (\s@CreateDBCluster' {} a -> s {enableIAMDatabaseAuthentication = a} :: CreateDBCluster)

-- | A value that indicates whether to turn on Performance Insights for the
-- DB cluster.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights>
-- in the /Amazon RDS User Guide/.
--
-- Valid for: Multi-AZ DB clusters only
createDBCluster_enablePerformanceInsights :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Bool)
createDBCluster_enablePerformanceInsights = Lens.lens (\CreateDBCluster' {enablePerformanceInsights} -> enablePerformanceInsights) (\s@CreateDBCluster' {} a -> s {enablePerformanceInsights = a} :: CreateDBCluster)

-- | The DB engine mode of the DB cluster, either @provisioned@,
-- @serverless@, @parallelquery@, @global@, or @multimaster@.
--
-- The @parallelquery@ engine mode isn\'t required for Aurora MySQL version
-- 1.23 and higher 1.x versions, and version 2.09 and higher 2.x versions.
--
-- The @global@ engine mode isn\'t required for Aurora MySQL version 1.22
-- and higher 1.x versions, and @global@ engine mode isn\'t required for
-- any 2.x versions.
--
-- The @multimaster@ engine mode only applies for DB clusters created with
-- Aurora MySQL version 5.6.10a.
--
-- The @serverless@ engine mode only applies for Aurora Serverless v1 DB
-- clusters.
--
-- For Aurora PostgreSQL, the @global@ engine mode isn\'t required, and
-- both the @parallelquery@ and the @multimaster@ engine modes currently
-- aren\'t supported.
--
-- Limitations and requirements apply to some DB engine modes. For more
-- information, see the following sections in the /Amazon Aurora User
-- Guide/:
--
-- -   <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless.html#aurora-serverless.limitations Limitations of Aurora Serverless v1>
--
-- -   <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-serverless-v2.requirements.html Requirements for Aurora Serverless v2>
--
-- -   <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-mysql-parallel-query.html#aurora-mysql-parallel-query-limitations Limitations of Parallel Query>
--
-- -   <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-global-database.html#aurora-global-database.limitations Limitations of Aurora Global Databases>
--
-- -   <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-multi-master.html#aurora-multi-master-limitations Limitations of Multi-Master Clusters>
--
-- Valid for: Aurora DB clusters only
createDBCluster_engineMode :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_engineMode = Lens.lens (\CreateDBCluster' {engineMode} -> engineMode) (\s@CreateDBCluster' {} a -> s {engineMode = a} :: CreateDBCluster)

-- | The version number of the database engine to use.
--
-- To list all of the available engine versions for MySQL 5.6-compatible
-- Aurora, use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora --query \"DBEngineVersions[].EngineVersion\"@
--
-- To list all of the available engine versions for MySQL 5.7-compatible
-- and MySQL 8.0-compatible Aurora, use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora-mysql --query \"DBEngineVersions[].EngineVersion\"@
--
-- To list all of the available engine versions for Aurora PostgreSQL, use
-- the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora-postgresql --query \"DBEngineVersions[].EngineVersion\"@
--
-- To list all of the available engine versions for RDS for MySQL, use the
-- following command:
--
-- @aws rds describe-db-engine-versions --engine mysql --query \"DBEngineVersions[].EngineVersion\"@
--
-- To list all of the available engine versions for RDS for PostgreSQL, use
-- the following command:
--
-- @aws rds describe-db-engine-versions --engine postgres --query \"DBEngineVersions[].EngineVersion\"@
--
-- __Aurora MySQL__
--
-- For information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/AuroraMySQL.Updates.html MySQL on Amazon RDS Versions>
-- in the /Amazon Aurora User Guide/.
--
-- __Aurora PostgreSQL__
--
-- For information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/AuroraPostgreSQL.Updates.20180305.html Amazon Aurora PostgreSQL releases and engine versions>
-- in the /Amazon Aurora User Guide/.
--
-- __MySQL__
--
-- For information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_MySQL.html#MySQL.Concepts.VersionMgmt MySQL on Amazon RDS Versions>
-- in the /Amazon RDS User Guide/.
--
-- __PostgreSQL__
--
-- For information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_PostgreSQL.html#PostgreSQL.Concepts Amazon RDS for PostgreSQL versions and extensions>
-- in the /Amazon RDS User Guide/.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
createDBCluster_engineVersion :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_engineVersion = Lens.lens (\CreateDBCluster' {engineVersion} -> engineVersion) (\s@CreateDBCluster' {} a -> s {engineVersion = a} :: CreateDBCluster)

-- | The global cluster ID of an Aurora cluster that becomes the primary
-- cluster in the new global database cluster.
--
-- Valid for: Aurora DB clusters only
createDBCluster_globalClusterIdentifier :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_globalClusterIdentifier = Lens.lens (\CreateDBCluster' {globalClusterIdentifier} -> globalClusterIdentifier) (\s@CreateDBCluster' {} a -> s {globalClusterIdentifier = a} :: CreateDBCluster)

-- | The amount of Provisioned IOPS (input\/output operations per second) to
-- be initially allocated for each DB instance in the Multi-AZ DB cluster.
--
-- For information about valid IOPS values, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS storage>
-- in the /Amazon RDS User Guide/.
--
-- This setting is required to create a Multi-AZ DB cluster.
--
-- Constraints: Must be a multiple between .5 and 50 of the storage amount
-- for the DB cluster.
--
-- Valid for: Multi-AZ DB clusters only
createDBCluster_iops :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Int)
createDBCluster_iops = Lens.lens (\CreateDBCluster' {iops} -> iops) (\s@CreateDBCluster' {} a -> s {iops = a} :: CreateDBCluster)

-- | The Amazon Web Services KMS key identifier for an encrypted DB cluster.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key. To use a KMS key in a different
-- Amazon Web Services account, specify the key ARN or alias ARN.
--
-- When a KMS key isn\'t specified in @KmsKeyId@:
--
-- -   If @ReplicationSourceIdentifier@ identifies an encrypted source,
--     then Amazon RDS will use the KMS key used to encrypt the source.
--     Otherwise, Amazon RDS will use your default KMS key.
--
-- -   If the @StorageEncrypted@ parameter is enabled and
--     @ReplicationSourceIdentifier@ isn\'t specified, then Amazon RDS will
--     use your default KMS key.
--
-- There is a default KMS key for your Amazon Web Services account. Your
-- Amazon Web Services account has a different default KMS key for each
-- Amazon Web Services Region.
--
-- If you create a read replica of an encrypted DB cluster in another
-- Amazon Web Services Region, you must set @KmsKeyId@ to a KMS key
-- identifier that is valid in the destination Amazon Web Services Region.
-- This KMS key is used to encrypt the read replica in that Amazon Web
-- Services Region.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
createDBCluster_kmsKeyId :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_kmsKeyId = Lens.lens (\CreateDBCluster' {kmsKeyId} -> kmsKeyId) (\s@CreateDBCluster' {} a -> s {kmsKeyId = a} :: CreateDBCluster)

-- | A value that indicates whether to manage the master user password with
-- Amazon Web Services Secrets Manager.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
-- in the /Amazon RDS User Guide/ and
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
-- in the /Amazon Aurora User Guide./
--
-- Constraints:
--
-- -   Can\'t manage the master user password with Amazon Web Services
--     Secrets Manager if @MasterUserPassword@ is specified.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
createDBCluster_manageMasterUserPassword :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Bool)
createDBCluster_manageMasterUserPassword = Lens.lens (\CreateDBCluster' {manageMasterUserPassword} -> manageMasterUserPassword) (\s@CreateDBCluster' {} a -> s {manageMasterUserPassword = a} :: CreateDBCluster)

-- | The password for the master database user. This password can contain any
-- printable ASCII character except \"\/\", \"\"\", or \"\@\".
--
-- Constraints:
--
-- -   Must contain from 8 to 41 characters.
--
-- -   Can\'t be specified if @ManageMasterUserPassword@ is turned on.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
createDBCluster_masterUserPassword :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_masterUserPassword = Lens.lens (\CreateDBCluster' {masterUserPassword} -> masterUserPassword) (\s@CreateDBCluster' {} a -> s {masterUserPassword = a} :: CreateDBCluster)

-- | The Amazon Web Services KMS key identifier to encrypt a secret that is
-- automatically generated and managed in Amazon Web Services Secrets
-- Manager.
--
-- This setting is valid only if the master user password is managed by RDS
-- in Amazon Web Services Secrets Manager for the DB cluster.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key. To use a KMS key in a different
-- Amazon Web Services account, specify the key ARN or alias ARN.
--
-- If you don\'t specify @MasterUserSecretKmsKeyId@, then the
-- @aws\/secretsmanager@ KMS key is used to encrypt the secret. If the
-- secret is in a different Amazon Web Services account, then you can\'t
-- use the @aws\/secretsmanager@ KMS key to encrypt the secret, and you
-- must use a customer managed KMS key.
--
-- There is a default KMS key for your Amazon Web Services account. Your
-- Amazon Web Services account has a different default KMS key for each
-- Amazon Web Services Region.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
createDBCluster_masterUserSecretKmsKeyId :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_masterUserSecretKmsKeyId = Lens.lens (\CreateDBCluster' {masterUserSecretKmsKeyId} -> masterUserSecretKmsKeyId) (\s@CreateDBCluster' {} a -> s {masterUserSecretKmsKeyId = a} :: CreateDBCluster)

-- | The name of the master user for the DB cluster.
--
-- Constraints:
--
-- -   Must be 1 to 16 letters or numbers.
--
-- -   First character must be a letter.
--
-- -   Can\'t be a reserved word for the chosen database engine.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
createDBCluster_masterUsername :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_masterUsername = Lens.lens (\CreateDBCluster' {masterUsername} -> masterUsername) (\s@CreateDBCluster' {} a -> s {masterUsername = a} :: CreateDBCluster)

-- | The interval, in seconds, between points when Enhanced Monitoring
-- metrics are collected for the DB cluster. To turn off collecting
-- Enhanced Monitoring metrics, specify 0. The default is 0.
--
-- If @MonitoringRoleArn@ is specified, also set @MonitoringInterval@ to a
-- value other than 0.
--
-- Valid Values: @0, 1, 5, 10, 15, 30, 60@
--
-- Valid for: Multi-AZ DB clusters only
createDBCluster_monitoringInterval :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Int)
createDBCluster_monitoringInterval = Lens.lens (\CreateDBCluster' {monitoringInterval} -> monitoringInterval) (\s@CreateDBCluster' {} a -> s {monitoringInterval = a} :: CreateDBCluster)

-- | The Amazon Resource Name (ARN) for the IAM role that permits RDS to send
-- Enhanced Monitoring metrics to Amazon CloudWatch Logs. An example is
-- @arn:aws:iam:123456789012:role\/emaccess@. For information on creating a
-- monitoring role, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.OS.html#USER_Monitoring.OS.Enabling Setting up and enabling Enhanced Monitoring>
-- in the /Amazon RDS User Guide/.
--
-- If @MonitoringInterval@ is set to a value other than 0, supply a
-- @MonitoringRoleArn@ value.
--
-- Valid for: Multi-AZ DB clusters only
createDBCluster_monitoringRoleArn :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_monitoringRoleArn = Lens.lens (\CreateDBCluster' {monitoringRoleArn} -> monitoringRoleArn) (\s@CreateDBCluster' {} a -> s {monitoringRoleArn = a} :: CreateDBCluster)

-- | The network type of the DB cluster.
--
-- Valid values:
--
-- -   @IPV4@
--
-- -   @DUAL@
--
-- The network type is determined by the @DBSubnetGroup@ specified for the
-- DB cluster. A @DBSubnetGroup@ can support only the IPv4 protocol or the
-- IPv4 and the IPv6 protocols (@DUAL@).
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html Working with a DB instance in a VPC>
-- in the /Amazon Aurora User Guide./
--
-- Valid for: Aurora DB clusters only
createDBCluster_networkType :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_networkType = Lens.lens (\CreateDBCluster' {networkType} -> networkType) (\s@CreateDBCluster' {} a -> s {networkType = a} :: CreateDBCluster)

-- | A value that indicates that the DB cluster should be associated with the
-- specified option group.
--
-- DB clusters are associated with a default option group that can\'t be
-- modified.
createDBCluster_optionGroupName :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_optionGroupName = Lens.lens (\CreateDBCluster' {optionGroupName} -> optionGroupName) (\s@CreateDBCluster' {} a -> s {optionGroupName = a} :: CreateDBCluster)

-- | The Amazon Web Services KMS key identifier for encryption of Performance
-- Insights data.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key.
--
-- If you don\'t specify a value for @PerformanceInsightsKMSKeyId@, then
-- Amazon RDS uses your default KMS key. There is a default KMS key for
-- your Amazon Web Services account. Your Amazon Web Services account has a
-- different default KMS key for each Amazon Web Services Region.
--
-- Valid for: Multi-AZ DB clusters only
createDBCluster_performanceInsightsKMSKeyId :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_performanceInsightsKMSKeyId = Lens.lens (\CreateDBCluster' {performanceInsightsKMSKeyId} -> performanceInsightsKMSKeyId) (\s@CreateDBCluster' {} a -> s {performanceInsightsKMSKeyId = a} :: CreateDBCluster)

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
-- Valid for: Multi-AZ DB clusters only
createDBCluster_performanceInsightsRetentionPeriod :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Int)
createDBCluster_performanceInsightsRetentionPeriod = Lens.lens (\CreateDBCluster' {performanceInsightsRetentionPeriod} -> performanceInsightsRetentionPeriod) (\s@CreateDBCluster' {} a -> s {performanceInsightsRetentionPeriod = a} :: CreateDBCluster)

-- | The port number on which the instances in the DB cluster accept
-- connections.
--
-- __RDS for MySQL and Aurora MySQL__
--
-- Default: @3306@
--
-- Valid values: @1150-65535@
--
-- __RDS for PostgreSQL and Aurora PostgreSQL__
--
-- Default: @5432@
--
-- Valid values: @1150-65535@
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
createDBCluster_port :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Int)
createDBCluster_port = Lens.lens (\CreateDBCluster' {port} -> port) (\s@CreateDBCluster' {} a -> s {port = a} :: CreateDBCluster)

-- | When you are replicating a DB cluster from one Amazon Web Services
-- GovCloud (US) Region to another, an URL that contains a Signature
-- Version 4 signed request for the @CreateDBCluster@ operation to be
-- called in the source Amazon Web Services Region where the DB cluster is
-- replicated from. Specify @PreSignedUrl@ only when you are performing
-- cross-Region replication from an encrypted DB cluster.
--
-- The presigned URL must be a valid request for the @CreateDBCluster@ API
-- operation that can run in the source Amazon Web Services Region that
-- contains the encrypted DB cluster to copy.
--
-- The presigned URL request must contain the following parameter values:
--
-- -   @KmsKeyId@ - The KMS key identifier for the KMS key to use to
--     encrypt the copy of the DB cluster in the destination Amazon Web
--     Services Region. This should refer to the same KMS key for both the
--     @CreateDBCluster@ operation that is called in the destination Amazon
--     Web Services Region, and the operation contained in the presigned
--     URL.
--
-- -   @DestinationRegion@ - The name of the Amazon Web Services Region
--     that Aurora read replica will be created in.
--
-- -   @ReplicationSourceIdentifier@ - The DB cluster identifier for the
--     encrypted DB cluster to be copied. This identifier must be in the
--     Amazon Resource Name (ARN) format for the source Amazon Web Services
--     Region. For example, if you are copying an encrypted DB cluster from
--     the us-west-2 Amazon Web Services Region, then your
--     @ReplicationSourceIdentifier@ would look like Example:
--     @arn:aws:rds:us-west-2:123456789012:cluster:aurora-cluster1@.
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
-- Valid for: Aurora DB clusters only
createDBCluster_preSignedUrl :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_preSignedUrl = Lens.lens (\CreateDBCluster' {preSignedUrl} -> preSignedUrl) (\s@CreateDBCluster' {} a -> s {preSignedUrl = a} :: CreateDBCluster)

-- | The daily time range during which automated backups are created if
-- automated backups are enabled using the @BackupRetentionPeriod@
-- parameter.
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Web Services Region. To view the time
-- blocks available, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.Backups.BackupWindow Backup window>
-- in the /Amazon Aurora User Guide/.
--
-- Constraints:
--
-- -   Must be in the format @hh24:mi-hh24:mi@.
--
-- -   Must be in Universal Coordinated Time (UTC).
--
-- -   Must not conflict with the preferred maintenance window.
--
-- -   Must be at least 30 minutes.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
createDBCluster_preferredBackupWindow :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_preferredBackupWindow = Lens.lens (\CreateDBCluster' {preferredBackupWindow} -> preferredBackupWindow) (\s@CreateDBCluster' {} a -> s {preferredBackupWindow = a} :: CreateDBCluster)

-- | The weekly time range during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Web Services Region, occurring on a random
-- day of the week. To see the time blocks available, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred DB Cluster Maintenance Window>
-- in the /Amazon Aurora User Guide/.
--
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
--
-- Constraints: Minimum 30-minute window.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
createDBCluster_preferredMaintenanceWindow :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_preferredMaintenanceWindow = Lens.lens (\CreateDBCluster' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@CreateDBCluster' {} a -> s {preferredMaintenanceWindow = a} :: CreateDBCluster)

-- | A value that indicates whether the DB cluster is publicly accessible.
--
-- When the DB cluster is publicly accessible, its Domain Name System (DNS)
-- endpoint resolves to the private IP address from within the DB
-- cluster\'s virtual private cloud (VPC). It resolves to the public IP
-- address from outside of the DB cluster\'s VPC. Access to the DB cluster
-- is ultimately controlled by the security group it uses. That public
-- access isn\'t permitted if the security group assigned to the DB cluster
-- doesn\'t permit it.
--
-- When the DB cluster isn\'t publicly accessible, it is an internal DB
-- cluster with a DNS name that resolves to a private IP address.
--
-- Default: The default behavior varies depending on whether
-- @DBSubnetGroupName@ is specified.
--
-- If @DBSubnetGroupName@ isn\'t specified, and @PubliclyAccessible@ isn\'t
-- specified, the following applies:
--
-- -   If the default VPC in the target Region doesn’t have an internet
--     gateway attached to it, the DB cluster is private.
--
-- -   If the default VPC in the target Region has an internet gateway
--     attached to it, the DB cluster is public.
--
-- If @DBSubnetGroupName@ is specified, and @PubliclyAccessible@ isn\'t
-- specified, the following applies:
--
-- -   If the subnets are part of a VPC that doesn’t have an internet
--     gateway attached to it, the DB cluster is private.
--
-- -   If the subnets are part of a VPC that has an internet gateway
--     attached to it, the DB cluster is public.
--
-- Valid for: Multi-AZ DB clusters only
createDBCluster_publiclyAccessible :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Bool)
createDBCluster_publiclyAccessible = Lens.lens (\CreateDBCluster' {publiclyAccessible} -> publiclyAccessible) (\s@CreateDBCluster' {} a -> s {publiclyAccessible = a} :: CreateDBCluster)

-- | The Amazon Resource Name (ARN) of the source DB instance or DB cluster
-- if this DB cluster is created as a read replica.
--
-- Valid for: Aurora DB clusters only
createDBCluster_replicationSourceIdentifier :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_replicationSourceIdentifier = Lens.lens (\CreateDBCluster' {replicationSourceIdentifier} -> replicationSourceIdentifier) (\s@CreateDBCluster' {} a -> s {replicationSourceIdentifier = a} :: CreateDBCluster)

-- | For DB clusters in @serverless@ DB engine mode, the scaling properties
-- of the DB cluster.
--
-- Valid for: Aurora DB clusters only
createDBCluster_scalingConfiguration :: Lens.Lens' CreateDBCluster (Prelude.Maybe ScalingConfiguration)
createDBCluster_scalingConfiguration = Lens.lens (\CreateDBCluster' {scalingConfiguration} -> scalingConfiguration) (\s@CreateDBCluster' {} a -> s {scalingConfiguration = a} :: CreateDBCluster)

-- | Undocumented member.
createDBCluster_serverlessV2ScalingConfiguration :: Lens.Lens' CreateDBCluster (Prelude.Maybe ServerlessV2ScalingConfiguration)
createDBCluster_serverlessV2ScalingConfiguration = Lens.lens (\CreateDBCluster' {serverlessV2ScalingConfiguration} -> serverlessV2ScalingConfiguration) (\s@CreateDBCluster' {} a -> s {serverlessV2ScalingConfiguration = a} :: CreateDBCluster)

-- | A value that indicates whether the DB cluster is encrypted.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
createDBCluster_storageEncrypted :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Bool)
createDBCluster_storageEncrypted = Lens.lens (\CreateDBCluster' {storageEncrypted} -> storageEncrypted) (\s@CreateDBCluster' {} a -> s {storageEncrypted = a} :: CreateDBCluster)

-- | Specifies the storage type to be associated with the DB cluster.
--
-- This setting is required to create a Multi-AZ DB cluster.
--
-- Valid values: @io1@
--
-- When specified, a value for the @Iops@ parameter is required.
--
-- Default: @io1@
--
-- Valid for: Multi-AZ DB clusters only
createDBCluster_storageType :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_storageType = Lens.lens (\CreateDBCluster' {storageType} -> storageType) (\s@CreateDBCluster' {} a -> s {storageType = a} :: CreateDBCluster)

-- | Tags to assign to the DB cluster.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
createDBCluster_tags :: Lens.Lens' CreateDBCluster (Prelude.Maybe [Tag])
createDBCluster_tags = Lens.lens (\CreateDBCluster' {tags} -> tags) (\s@CreateDBCluster' {} a -> s {tags = a} :: CreateDBCluster) Prelude.. Lens.mapping Lens.coerced

-- | A list of EC2 VPC security groups to associate with this DB cluster.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
createDBCluster_vpcSecurityGroupIds :: Lens.Lens' CreateDBCluster (Prelude.Maybe [Prelude.Text])
createDBCluster_vpcSecurityGroupIds = Lens.lens (\CreateDBCluster' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@CreateDBCluster' {} a -> s {vpcSecurityGroupIds = a} :: CreateDBCluster) Prelude.. Lens.mapping Lens.coerced

-- | The DB cluster identifier. This parameter is stored as a lowercase
-- string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens.
--
-- -   First character must be a letter.
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens.
--
-- Example: @my-cluster1@
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
createDBCluster_dbClusterIdentifier :: Lens.Lens' CreateDBCluster Prelude.Text
createDBCluster_dbClusterIdentifier = Lens.lens (\CreateDBCluster' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@CreateDBCluster' {} a -> s {dbClusterIdentifier = a} :: CreateDBCluster)

-- | The name of the database engine to be used for this DB cluster.
--
-- Valid Values:
--
-- -   @aurora@ (for MySQL 5.6-compatible Aurora)
--
-- -   @aurora-mysql@ (for MySQL 5.7-compatible and MySQL 8.0-compatible
--     Aurora)
--
-- -   @aurora-postgresql@
--
-- -   @mysql@
--
-- -   @postgres@
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
createDBCluster_engine :: Lens.Lens' CreateDBCluster Prelude.Text
createDBCluster_engine = Lens.lens (\CreateDBCluster' {engine} -> engine) (\s@CreateDBCluster' {} a -> s {engine = a} :: CreateDBCluster)

instance Core.AWSRequest CreateDBCluster where
  type
    AWSResponse CreateDBCluster =
      CreateDBClusterResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateDBClusterResult"
      ( \s h x ->
          CreateDBClusterResponse'
            Prelude.<$> (x Data..@? "DBCluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDBCluster where
  hashWithSalt _salt CreateDBCluster' {..} =
    _salt `Prelude.hashWithSalt` allocatedStorage
      `Prelude.hashWithSalt` autoMinorVersionUpgrade
      `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` backtrackWindow
      `Prelude.hashWithSalt` backupRetentionPeriod
      `Prelude.hashWithSalt` characterSetName
      `Prelude.hashWithSalt` copyTagsToSnapshot
      `Prelude.hashWithSalt` dbClusterInstanceClass
      `Prelude.hashWithSalt` dbClusterParameterGroupName
      `Prelude.hashWithSalt` dbSubnetGroupName
      `Prelude.hashWithSalt` dbSystemId
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` deletionProtection
      `Prelude.hashWithSalt` destinationRegion
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` domainIAMRoleName
      `Prelude.hashWithSalt` enableCloudwatchLogsExports
      `Prelude.hashWithSalt` enableGlobalWriteForwarding
      `Prelude.hashWithSalt` enableHttpEndpoint
      `Prelude.hashWithSalt` enableIAMDatabaseAuthentication
      `Prelude.hashWithSalt` enablePerformanceInsights
      `Prelude.hashWithSalt` engineMode
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` globalClusterIdentifier
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` manageMasterUserPassword
      `Prelude.hashWithSalt` masterUserPassword
      `Prelude.hashWithSalt` masterUserSecretKmsKeyId
      `Prelude.hashWithSalt` masterUsername
      `Prelude.hashWithSalt` monitoringInterval
      `Prelude.hashWithSalt` monitoringRoleArn
      `Prelude.hashWithSalt` networkType
      `Prelude.hashWithSalt` optionGroupName
      `Prelude.hashWithSalt` performanceInsightsKMSKeyId
      `Prelude.hashWithSalt` performanceInsightsRetentionPeriod
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` preSignedUrl
      `Prelude.hashWithSalt` preferredBackupWindow
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` publiclyAccessible
      `Prelude.hashWithSalt` replicationSourceIdentifier
      `Prelude.hashWithSalt` scalingConfiguration
      `Prelude.hashWithSalt` serverlessV2ScalingConfiguration
      `Prelude.hashWithSalt` storageEncrypted
      `Prelude.hashWithSalt` storageType
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcSecurityGroupIds
      `Prelude.hashWithSalt` dbClusterIdentifier
      `Prelude.hashWithSalt` engine

instance Prelude.NFData CreateDBCluster where
  rnf CreateDBCluster' {..} =
    Prelude.rnf allocatedStorage
      `Prelude.seq` Prelude.rnf autoMinorVersionUpgrade
      `Prelude.seq` Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf backtrackWindow
      `Prelude.seq` Prelude.rnf backupRetentionPeriod
      `Prelude.seq` Prelude.rnf characterSetName
      `Prelude.seq` Prelude.rnf copyTagsToSnapshot
      `Prelude.seq` Prelude.rnf dbClusterInstanceClass
      `Prelude.seq` Prelude.rnf dbClusterParameterGroupName
      `Prelude.seq` Prelude.rnf dbSubnetGroupName
      `Prelude.seq` Prelude.rnf dbSystemId
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf deletionProtection
      `Prelude.seq` Prelude.rnf destinationRegion
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf domainIAMRoleName
      `Prelude.seq` Prelude.rnf
        enableCloudwatchLogsExports
      `Prelude.seq` Prelude.rnf
        enableGlobalWriteForwarding
      `Prelude.seq` Prelude.rnf enableHttpEndpoint
      `Prelude.seq` Prelude.rnf
        enableIAMDatabaseAuthentication
      `Prelude.seq` Prelude.rnf
        enablePerformanceInsights
      `Prelude.seq` Prelude.rnf engineMode
      `Prelude.seq` Prelude.rnf
        engineVersion
      `Prelude.seq` Prelude.rnf
        globalClusterIdentifier
      `Prelude.seq` Prelude.rnf iops
      `Prelude.seq` Prelude.rnf
        kmsKeyId
      `Prelude.seq` Prelude.rnf
        manageMasterUserPassword
      `Prelude.seq` Prelude.rnf
        masterUserPassword
      `Prelude.seq` Prelude.rnf
        masterUserSecretKmsKeyId
      `Prelude.seq` Prelude.rnf
        masterUsername
      `Prelude.seq` Prelude.rnf
        monitoringInterval
      `Prelude.seq` Prelude.rnf
        monitoringRoleArn
      `Prelude.seq` Prelude.rnf
        networkType
      `Prelude.seq` Prelude.rnf
        optionGroupName
      `Prelude.seq` Prelude.rnf
        performanceInsightsKMSKeyId
      `Prelude.seq` Prelude.rnf
        performanceInsightsRetentionPeriod
      `Prelude.seq` Prelude.rnf
        port
      `Prelude.seq` Prelude.rnf
        preSignedUrl
      `Prelude.seq` Prelude.rnf
        preferredBackupWindow
      `Prelude.seq` Prelude.rnf
        preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf
        publiclyAccessible
      `Prelude.seq` Prelude.rnf
        replicationSourceIdentifier
      `Prelude.seq` Prelude.rnf
        scalingConfiguration
      `Prelude.seq` Prelude.rnf
        serverlessV2ScalingConfiguration
      `Prelude.seq` Prelude.rnf
        storageEncrypted
      `Prelude.seq` Prelude.rnf
        storageType
      `Prelude.seq` Prelude.rnf
        tags
      `Prelude.seq` Prelude.rnf
        vpcSecurityGroupIds
      `Prelude.seq` Prelude.rnf
        dbClusterIdentifier
      `Prelude.seq` Prelude.rnf
        engine

instance Data.ToHeaders CreateDBCluster where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateDBCluster where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDBCluster where
  toQuery CreateDBCluster' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateDBCluster" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "AllocatedStorage" Data.=: allocatedStorage,
        "AutoMinorVersionUpgrade"
          Data.=: autoMinorVersionUpgrade,
        "AvailabilityZones"
          Data.=: Data.toQuery
            ( Data.toQueryList "AvailabilityZone"
                Prelude.<$> availabilityZones
            ),
        "BacktrackWindow" Data.=: backtrackWindow,
        "BackupRetentionPeriod"
          Data.=: backupRetentionPeriod,
        "CharacterSetName" Data.=: characterSetName,
        "CopyTagsToSnapshot" Data.=: copyTagsToSnapshot,
        "DBClusterInstanceClass"
          Data.=: dbClusterInstanceClass,
        "DBClusterParameterGroupName"
          Data.=: dbClusterParameterGroupName,
        "DBSubnetGroupName" Data.=: dbSubnetGroupName,
        "DBSystemId" Data.=: dbSystemId,
        "DatabaseName" Data.=: databaseName,
        "DeletionProtection" Data.=: deletionProtection,
        "DestinationRegion" Data.=: destinationRegion,
        "Domain" Data.=: domain,
        "DomainIAMRoleName" Data.=: domainIAMRoleName,
        "EnableCloudwatchLogsExports"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> enableCloudwatchLogsExports
            ),
        "EnableGlobalWriteForwarding"
          Data.=: enableGlobalWriteForwarding,
        "EnableHttpEndpoint" Data.=: enableHttpEndpoint,
        "EnableIAMDatabaseAuthentication"
          Data.=: enableIAMDatabaseAuthentication,
        "EnablePerformanceInsights"
          Data.=: enablePerformanceInsights,
        "EngineMode" Data.=: engineMode,
        "EngineVersion" Data.=: engineVersion,
        "GlobalClusterIdentifier"
          Data.=: globalClusterIdentifier,
        "Iops" Data.=: iops,
        "KmsKeyId" Data.=: kmsKeyId,
        "ManageMasterUserPassword"
          Data.=: manageMasterUserPassword,
        "MasterUserPassword" Data.=: masterUserPassword,
        "MasterUserSecretKmsKeyId"
          Data.=: masterUserSecretKmsKeyId,
        "MasterUsername" Data.=: masterUsername,
        "MonitoringInterval" Data.=: monitoringInterval,
        "MonitoringRoleArn" Data.=: monitoringRoleArn,
        "NetworkType" Data.=: networkType,
        "OptionGroupName" Data.=: optionGroupName,
        "PerformanceInsightsKMSKeyId"
          Data.=: performanceInsightsKMSKeyId,
        "PerformanceInsightsRetentionPeriod"
          Data.=: performanceInsightsRetentionPeriod,
        "Port" Data.=: port,
        "PreSignedUrl" Data.=: preSignedUrl,
        "PreferredBackupWindow"
          Data.=: preferredBackupWindow,
        "PreferredMaintenanceWindow"
          Data.=: preferredMaintenanceWindow,
        "PubliclyAccessible" Data.=: publiclyAccessible,
        "ReplicationSourceIdentifier"
          Data.=: replicationSourceIdentifier,
        "ScalingConfiguration" Data.=: scalingConfiguration,
        "ServerlessV2ScalingConfiguration"
          Data.=: serverlessV2ScalingConfiguration,
        "StorageEncrypted" Data.=: storageEncrypted,
        "StorageType" Data.=: storageType,
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "Tag" Prelude.<$> tags),
        "VpcSecurityGroupIds"
          Data.=: Data.toQuery
            ( Data.toQueryList "VpcSecurityGroupId"
                Prelude.<$> vpcSecurityGroupIds
            ),
        "DBClusterIdentifier" Data.=: dbClusterIdentifier,
        "Engine" Data.=: engine
      ]

-- | /See:/ 'newCreateDBClusterResponse' smart constructor.
data CreateDBClusterResponse = CreateDBClusterResponse'
  { dbCluster :: Prelude.Maybe DBCluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDBClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbCluster', 'createDBClusterResponse_dbCluster' - Undocumented member.
--
-- 'httpStatus', 'createDBClusterResponse_httpStatus' - The response's http status code.
newCreateDBClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDBClusterResponse
newCreateDBClusterResponse pHttpStatus_ =
  CreateDBClusterResponse'
    { dbCluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createDBClusterResponse_dbCluster :: Lens.Lens' CreateDBClusterResponse (Prelude.Maybe DBCluster)
createDBClusterResponse_dbCluster = Lens.lens (\CreateDBClusterResponse' {dbCluster} -> dbCluster) (\s@CreateDBClusterResponse' {} a -> s {dbCluster = a} :: CreateDBClusterResponse)

-- | The response's http status code.
createDBClusterResponse_httpStatus :: Lens.Lens' CreateDBClusterResponse Prelude.Int
createDBClusterResponse_httpStatus = Lens.lens (\CreateDBClusterResponse' {httpStatus} -> httpStatus) (\s@CreateDBClusterResponse' {} a -> s {httpStatus = a} :: CreateDBClusterResponse)

instance Prelude.NFData CreateDBClusterResponse where
  rnf CreateDBClusterResponse' {..} =
    Prelude.rnf dbCluster
      `Prelude.seq` Prelude.rnf httpStatus
