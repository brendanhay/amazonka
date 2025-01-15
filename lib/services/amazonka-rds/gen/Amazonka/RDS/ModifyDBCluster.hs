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
-- Module      : Amazonka.RDS.ModifyDBCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify the settings for an Amazon Aurora DB cluster or a Multi-AZ DB
-- cluster. You can change one or more settings by specifying these
-- parameters and the new values in the request.
--
-- For more information on Amazon Aurora DB clusters, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What is Amazon Aurora?>
-- in the /Amazon Aurora User Guide/.
--
-- For more information on Multi-AZ DB clusters, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/multi-az-db-clusters-concepts.html Multi-AZ deployments with two readable standby DB instances>
-- in the /Amazon RDS User Guide/.
module Amazonka.RDS.ModifyDBCluster
  ( -- * Creating a Request
    ModifyDBCluster (..),
    newModifyDBCluster,

    -- * Request Lenses
    modifyDBCluster_allocatedStorage,
    modifyDBCluster_allowMajorVersionUpgrade,
    modifyDBCluster_applyImmediately,
    modifyDBCluster_autoMinorVersionUpgrade,
    modifyDBCluster_backtrackWindow,
    modifyDBCluster_backupRetentionPeriod,
    modifyDBCluster_cloudwatchLogsExportConfiguration,
    modifyDBCluster_copyTagsToSnapshot,
    modifyDBCluster_dbClusterInstanceClass,
    modifyDBCluster_dbClusterParameterGroupName,
    modifyDBCluster_dbInstanceParameterGroupName,
    modifyDBCluster_deletionProtection,
    modifyDBCluster_domain,
    modifyDBCluster_domainIAMRoleName,
    modifyDBCluster_enableGlobalWriteForwarding,
    modifyDBCluster_enableHttpEndpoint,
    modifyDBCluster_enableIAMDatabaseAuthentication,
    modifyDBCluster_enablePerformanceInsights,
    modifyDBCluster_engineVersion,
    modifyDBCluster_iops,
    modifyDBCluster_manageMasterUserPassword,
    modifyDBCluster_masterUserPassword,
    modifyDBCluster_masterUserSecretKmsKeyId,
    modifyDBCluster_monitoringInterval,
    modifyDBCluster_monitoringRoleArn,
    modifyDBCluster_networkType,
    modifyDBCluster_newDBClusterIdentifier,
    modifyDBCluster_optionGroupName,
    modifyDBCluster_performanceInsightsKMSKeyId,
    modifyDBCluster_performanceInsightsRetentionPeriod,
    modifyDBCluster_port,
    modifyDBCluster_preferredBackupWindow,
    modifyDBCluster_preferredMaintenanceWindow,
    modifyDBCluster_rotateMasterUserPassword,
    modifyDBCluster_scalingConfiguration,
    modifyDBCluster_serverlessV2ScalingConfiguration,
    modifyDBCluster_storageType,
    modifyDBCluster_vpcSecurityGroupIds,
    modifyDBCluster_dbClusterIdentifier,

    -- * Destructuring the Response
    ModifyDBClusterResponse (..),
    newModifyDBClusterResponse,

    -- * Response Lenses
    modifyDBClusterResponse_dbCluster,
    modifyDBClusterResponse_httpStatus,
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
-- /See:/ 'newModifyDBCluster' smart constructor.
data ModifyDBCluster = ModifyDBCluster'
  { -- | The amount of storage in gibibytes (GiB) to allocate to each DB instance
    -- in the Multi-AZ DB cluster.
    --
    -- Type: Integer
    --
    -- Valid for: Multi-AZ DB clusters only
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | A value that indicates whether major version upgrades are allowed.
    --
    -- Constraints: You must allow major version upgrades when specifying a
    -- value for the @EngineVersion@ parameter that is a different major
    -- version than the DB cluster\'s current version.
    --
    -- Valid for: Aurora DB clusters only
    allowMajorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether the modifications in this request and any
    -- pending modifications are asynchronously applied as soon as possible,
    -- regardless of the @PreferredMaintenanceWindow@ setting for the DB
    -- cluster. If this parameter is disabled, changes to the DB cluster are
    -- applied during the next maintenance window.
    --
    -- The @ApplyImmediately@ parameter only affects the
    -- @EnableIAMDatabaseAuthentication@, @MasterUserPassword@, and
    -- @NewDBClusterIdentifier@ values. If the @ApplyImmediately@ parameter is
    -- disabled, then changes to the @EnableIAMDatabaseAuthentication@,
    -- @MasterUserPassword@, and @NewDBClusterIdentifier@ values are applied
    -- during the next maintenance window. All other changes are applied
    -- immediately, regardless of the value of the @ApplyImmediately@
    -- parameter.
    --
    -- By default, this parameter is disabled.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    applyImmediately :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether minor engine upgrades are applied
    -- automatically to the DB cluster during the maintenance window. By
    -- default, minor engine upgrades are applied automatically.
    --
    -- Valid for: Multi-AZ DB clusters only
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
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
    -- | The number of days for which automated backups are retained. Specify a
    -- minimum value of 1.
    --
    -- Default: 1
    --
    -- Constraints:
    --
    -- -   Must be a value from 1 to 35
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The configuration setting for the log types to be enabled for export to
    -- CloudWatch Logs for a specific DB cluster. The values in the list depend
    -- on the DB engine being used.
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
    cloudwatchLogsExportConfiguration :: Prelude.Maybe CloudwatchLogsExportConfiguration,
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
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
    -- in the /Amazon RDS User Guide/.
    --
    -- Valid for: Multi-AZ DB clusters only
    dbClusterInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | The name of the DB cluster parameter group to use for the DB cluster.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    dbClusterParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The name of the DB parameter group to apply to all instances of the DB
    -- cluster.
    --
    -- When you apply a parameter group using the
    -- @DBInstanceParameterGroupName@ parameter, the DB cluster isn\'t rebooted
    -- automatically. Also, parameter changes are applied immediately rather
    -- than during the next maintenance window.
    --
    -- Default: The existing name setting
    --
    -- Constraints:
    --
    -- -   The DB parameter group must be in the same DB parameter group family
    --     as this DB cluster.
    --
    -- -   The @DBInstanceParameterGroupName@ parameter is valid in combination
    --     with the @AllowMajorVersionUpgrade@ parameter for a major version
    --     upgrade only.
    --
    -- Valid for: Aurora DB clusters only
    dbInstanceParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the DB cluster has deletion protection
    -- enabled. The database can\'t be deleted when deletion protection is
    -- enabled. By default, deletion protection isn\'t enabled.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | The Active Directory directory ID to move the DB cluster to. Specify
    -- @none@ to remove the cluster from its current domain. The domain must be
    -- created prior to this operation.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication>
    -- in the /Amazon Aurora User Guide/.
    --
    -- Valid for: Aurora DB clusters only
    domain :: Prelude.Maybe Prelude.Text,
    -- | Specify the name of the IAM role to be used when making API calls to the
    -- Directory Service.
    --
    -- Valid for: Aurora DB clusters only
    domainIAMRoleName :: Prelude.Maybe Prelude.Text,
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
    -- | The version number of the database engine to which you want to upgrade.
    -- Changing this parameter results in an outage. The change is applied
    -- during the next maintenance window unless @ApplyImmediately@ is enabled.
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
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The amount of Provisioned IOPS (input\/output operations per second) to
    -- be initially allocated for each DB instance in the Multi-AZ DB cluster.
    --
    -- For information about valid IOPS values, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS storage>
    -- in the /Amazon RDS User Guide/.
    --
    -- Constraints: Must be a multiple between .5 and 50 of the storage amount
    -- for the DB cluster.
    --
    -- Valid for: Multi-AZ DB clusters only
    iops :: Prelude.Maybe Prelude.Int,
    -- | A value that indicates whether to manage the master user password with
    -- Amazon Web Services Secrets Manager.
    --
    -- If the DB cluster doesn\'t manage the master user password with Amazon
    -- Web Services Secrets Manager, you can turn on this management. In this
    -- case, you can\'t specify @MasterUserPassword@.
    --
    -- If the DB cluster already manages the master user password with Amazon
    -- Web Services Secrets Manager, and you specify that the master user
    -- password is not managed with Amazon Web Services Secrets Manager, then
    -- you must specify @MasterUserPassword@. In this case, RDS deletes the
    -- secret and uses the new password for the master user specified by
    -- @MasterUserPassword@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
    -- in the /Amazon RDS User Guide/ and
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
    -- in the /Amazon Aurora User Guide./
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    manageMasterUserPassword :: Prelude.Maybe Prelude.Bool,
    -- | The new password for the master database user. This password can contain
    -- any printable ASCII character except \"\/\", \"\"\", or \"\@\".
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
    -- This setting is valid only if both of the following conditions are met:
    --
    -- -   The DB cluster doesn\'t manage the master user password in Amazon
    --     Web Services Secrets Manager.
    --
    --     If the DB cluster already manages the master user password in Amazon
    --     Web Services Secrets Manager, you can\'t change the KMS key that is
    --     used to encrypt the secret.
    --
    -- -   You are turning on @ManageMasterUserPassword@ to manage the master
    --     user password in Amazon Web Services Secrets Manager.
    --
    --     If you are turning on @ManageMasterUserPassword@ and don\'t specify
    --     @MasterUserSecretKmsKeyId@, then the @aws\/secretsmanager@ KMS key
    --     is used to encrypt the secret. If the secret is in a different
    --     Amazon Web Services account, then you can\'t use the
    --     @aws\/secretsmanager@ KMS key to encrypt the secret, and you must
    --     use a customer managed KMS key.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the KMS key. To use a KMS key in a different
    -- Amazon Web Services account, specify the key ARN or alias ARN.
    --
    -- There is a default KMS key for your Amazon Web Services account. Your
    -- Amazon Web Services account has a different default KMS key for each
    -- Amazon Web Services Region.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    masterUserSecretKmsKeyId :: Prelude.Maybe Prelude.Text,
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
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.html#USER_Monitoring.OS.IAMRole To create an IAM role for Amazon RDS Enhanced Monitoring>
    -- in the /Amazon RDS User Guide./
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
    -- | The new DB cluster identifier for the DB cluster when renaming a DB
    -- cluster. This value is stored as a lowercase string.
    --
    -- Constraints:
    --
    -- -   Must contain from 1 to 63 letters, numbers, or hyphens
    --
    -- -   The first character must be a letter
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens
    --
    -- Example: @my-cluster2@
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    newDBClusterIdentifier' :: Prelude.Maybe Prelude.Text,
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
    -- | The port number on which the DB cluster accepts connections.
    --
    -- Constraints: Value must be @1150-65535@
    --
    -- Default: The same port as the original DB cluster.
    --
    -- Valid for: Aurora DB clusters only
    port :: Prelude.Maybe Prelude.Int,
    -- | The daily time range during which automated backups are created if
    -- automated backups are enabled, using the @BackupRetentionPeriod@
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
    -- | A value that indicates whether to rotate the secret managed by Amazon
    -- Web Services Secrets Manager for the master user password.
    --
    -- This setting is valid only if the master user password is managed by RDS
    -- in Amazon Web Services Secrets Manager for the DB cluster. The secret
    -- value contains the updated password.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
    -- in the /Amazon RDS User Guide/ and
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
    -- in the /Amazon Aurora User Guide./
    --
    -- Constraints:
    --
    -- -   You must apply the change immediately when rotating the master user
    --     password.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    rotateMasterUserPassword :: Prelude.Maybe Prelude.Bool,
    -- | The scaling properties of the DB cluster. You can only modify scaling
    -- properties for DB clusters in @serverless@ DB engine mode.
    --
    -- Valid for: Aurora DB clusters only
    scalingConfiguration :: Prelude.Maybe ScalingConfiguration,
    serverlessV2ScalingConfiguration :: Prelude.Maybe ServerlessV2ScalingConfiguration,
    -- | Specifies the storage type to be associated with the DB cluster.
    --
    -- Valid values: @io1@
    --
    -- When specified, a value for the @Iops@ parameter is required.
    --
    -- Default: @io1@
    --
    -- Valid for: Multi-AZ DB clusters only
    storageType :: Prelude.Maybe Prelude.Text,
    -- | A list of VPC security groups that the DB cluster will belong to.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The DB cluster identifier for the cluster being modified. This parameter
    -- isn\'t case-sensitive.
    --
    -- Constraints: This identifier must match the identifier of an existing DB
    -- cluster.
    --
    -- Valid for: Aurora DB clusters and Multi-AZ DB clusters
    dbClusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyDBCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allocatedStorage', 'modifyDBCluster_allocatedStorage' - The amount of storage in gibibytes (GiB) to allocate to each DB instance
-- in the Multi-AZ DB cluster.
--
-- Type: Integer
--
-- Valid for: Multi-AZ DB clusters only
--
-- 'allowMajorVersionUpgrade', 'modifyDBCluster_allowMajorVersionUpgrade' - A value that indicates whether major version upgrades are allowed.
--
-- Constraints: You must allow major version upgrades when specifying a
-- value for the @EngineVersion@ parameter that is a different major
-- version than the DB cluster\'s current version.
--
-- Valid for: Aurora DB clusters only
--
-- 'applyImmediately', 'modifyDBCluster_applyImmediately' - A value that indicates whether the modifications in this request and any
-- pending modifications are asynchronously applied as soon as possible,
-- regardless of the @PreferredMaintenanceWindow@ setting for the DB
-- cluster. If this parameter is disabled, changes to the DB cluster are
-- applied during the next maintenance window.
--
-- The @ApplyImmediately@ parameter only affects the
-- @EnableIAMDatabaseAuthentication@, @MasterUserPassword@, and
-- @NewDBClusterIdentifier@ values. If the @ApplyImmediately@ parameter is
-- disabled, then changes to the @EnableIAMDatabaseAuthentication@,
-- @MasterUserPassword@, and @NewDBClusterIdentifier@ values are applied
-- during the next maintenance window. All other changes are applied
-- immediately, regardless of the value of the @ApplyImmediately@
-- parameter.
--
-- By default, this parameter is disabled.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'autoMinorVersionUpgrade', 'modifyDBCluster_autoMinorVersionUpgrade' - A value that indicates whether minor engine upgrades are applied
-- automatically to the DB cluster during the maintenance window. By
-- default, minor engine upgrades are applied automatically.
--
-- Valid for: Multi-AZ DB clusters only
--
-- 'backtrackWindow', 'modifyDBCluster_backtrackWindow' - The target backtrack window, in seconds. To disable backtracking, set
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
-- 'backupRetentionPeriod', 'modifyDBCluster_backupRetentionPeriod' - The number of days for which automated backups are retained. Specify a
-- minimum value of 1.
--
-- Default: 1
--
-- Constraints:
--
-- -   Must be a value from 1 to 35
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'cloudwatchLogsExportConfiguration', 'modifyDBCluster_cloudwatchLogsExportConfiguration' - The configuration setting for the log types to be enabled for export to
-- CloudWatch Logs for a specific DB cluster. The values in the list depend
-- on the DB engine being used.
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
-- 'copyTagsToSnapshot', 'modifyDBCluster_copyTagsToSnapshot' - A value that indicates whether to copy all tags from the DB cluster to
-- snapshots of the DB cluster. The default is not to copy them.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'dbClusterInstanceClass', 'modifyDBCluster_dbClusterInstanceClass' - The compute and memory capacity of each DB instance in the Multi-AZ DB
-- cluster, for example db.m6gd.xlarge. Not all DB instance classes are
-- available in all Amazon Web Services Regions, or for all database
-- engines.
--
-- For the full list of DB instance classes and availability for your
-- engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
-- in the /Amazon RDS User Guide/.
--
-- Valid for: Multi-AZ DB clusters only
--
-- 'dbClusterParameterGroupName', 'modifyDBCluster_dbClusterParameterGroupName' - The name of the DB cluster parameter group to use for the DB cluster.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'dbInstanceParameterGroupName', 'modifyDBCluster_dbInstanceParameterGroupName' - The name of the DB parameter group to apply to all instances of the DB
-- cluster.
--
-- When you apply a parameter group using the
-- @DBInstanceParameterGroupName@ parameter, the DB cluster isn\'t rebooted
-- automatically. Also, parameter changes are applied immediately rather
-- than during the next maintenance window.
--
-- Default: The existing name setting
--
-- Constraints:
--
-- -   The DB parameter group must be in the same DB parameter group family
--     as this DB cluster.
--
-- -   The @DBInstanceParameterGroupName@ parameter is valid in combination
--     with the @AllowMajorVersionUpgrade@ parameter for a major version
--     upgrade only.
--
-- Valid for: Aurora DB clusters only
--
-- 'deletionProtection', 'modifyDBCluster_deletionProtection' - A value that indicates whether the DB cluster has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection isn\'t enabled.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'domain', 'modifyDBCluster_domain' - The Active Directory directory ID to move the DB cluster to. Specify
-- @none@ to remove the cluster from its current domain. The domain must be
-- created prior to this operation.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon Aurora User Guide/.
--
-- Valid for: Aurora DB clusters only
--
-- 'domainIAMRoleName', 'modifyDBCluster_domainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
--
-- Valid for: Aurora DB clusters only
--
-- 'enableGlobalWriteForwarding', 'modifyDBCluster_enableGlobalWriteForwarding' - A value that indicates whether to enable this DB cluster to forward
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
-- 'enableHttpEndpoint', 'modifyDBCluster_enableHttpEndpoint' - A value that indicates whether to enable the HTTP endpoint for an Aurora
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
-- 'enableIAMDatabaseAuthentication', 'modifyDBCluster_enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts. By
-- default, mapping isn\'t enabled.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
-- in the /Amazon Aurora User Guide/.
--
-- Valid for: Aurora DB clusters only
--
-- 'enablePerformanceInsights', 'modifyDBCluster_enablePerformanceInsights' - A value that indicates whether to turn on Performance Insights for the
-- DB cluster.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights>
-- in the /Amazon RDS User Guide/.
--
-- Valid for: Multi-AZ DB clusters only
--
-- 'engineVersion', 'modifyDBCluster_engineVersion' - The version number of the database engine to which you want to upgrade.
-- Changing this parameter results in an outage. The change is applied
-- during the next maintenance window unless @ApplyImmediately@ is enabled.
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
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'iops', 'modifyDBCluster_iops' - The amount of Provisioned IOPS (input\/output operations per second) to
-- be initially allocated for each DB instance in the Multi-AZ DB cluster.
--
-- For information about valid IOPS values, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS storage>
-- in the /Amazon RDS User Guide/.
--
-- Constraints: Must be a multiple between .5 and 50 of the storage amount
-- for the DB cluster.
--
-- Valid for: Multi-AZ DB clusters only
--
-- 'manageMasterUserPassword', 'modifyDBCluster_manageMasterUserPassword' - A value that indicates whether to manage the master user password with
-- Amazon Web Services Secrets Manager.
--
-- If the DB cluster doesn\'t manage the master user password with Amazon
-- Web Services Secrets Manager, you can turn on this management. In this
-- case, you can\'t specify @MasterUserPassword@.
--
-- If the DB cluster already manages the master user password with Amazon
-- Web Services Secrets Manager, and you specify that the master user
-- password is not managed with Amazon Web Services Secrets Manager, then
-- you must specify @MasterUserPassword@. In this case, RDS deletes the
-- secret and uses the new password for the master user specified by
-- @MasterUserPassword@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
-- in the /Amazon RDS User Guide/ and
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
-- in the /Amazon Aurora User Guide./
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'masterUserPassword', 'modifyDBCluster_masterUserPassword' - The new password for the master database user. This password can contain
-- any printable ASCII character except \"\/\", \"\"\", or \"\@\".
--
-- Constraints:
--
-- -   Must contain from 8 to 41 characters.
--
-- -   Can\'t be specified if @ManageMasterUserPassword@ is turned on.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'masterUserSecretKmsKeyId', 'modifyDBCluster_masterUserSecretKmsKeyId' - The Amazon Web Services KMS key identifier to encrypt a secret that is
-- automatically generated and managed in Amazon Web Services Secrets
-- Manager.
--
-- This setting is valid only if both of the following conditions are met:
--
-- -   The DB cluster doesn\'t manage the master user password in Amazon
--     Web Services Secrets Manager.
--
--     If the DB cluster already manages the master user password in Amazon
--     Web Services Secrets Manager, you can\'t change the KMS key that is
--     used to encrypt the secret.
--
-- -   You are turning on @ManageMasterUserPassword@ to manage the master
--     user password in Amazon Web Services Secrets Manager.
--
--     If you are turning on @ManageMasterUserPassword@ and don\'t specify
--     @MasterUserSecretKmsKeyId@, then the @aws\/secretsmanager@ KMS key
--     is used to encrypt the secret. If the secret is in a different
--     Amazon Web Services account, then you can\'t use the
--     @aws\/secretsmanager@ KMS key to encrypt the secret, and you must
--     use a customer managed KMS key.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key. To use a KMS key in a different
-- Amazon Web Services account, specify the key ARN or alias ARN.
--
-- There is a default KMS key for your Amazon Web Services account. Your
-- Amazon Web Services account has a different default KMS key for each
-- Amazon Web Services Region.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'monitoringInterval', 'modifyDBCluster_monitoringInterval' - The interval, in seconds, between points when Enhanced Monitoring
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
-- 'monitoringRoleArn', 'modifyDBCluster_monitoringRoleArn' - The Amazon Resource Name (ARN) for the IAM role that permits RDS to send
-- Enhanced Monitoring metrics to Amazon CloudWatch Logs. An example is
-- @arn:aws:iam:123456789012:role\/emaccess@. For information on creating a
-- monitoring role, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.html#USER_Monitoring.OS.IAMRole To create an IAM role for Amazon RDS Enhanced Monitoring>
-- in the /Amazon RDS User Guide./
--
-- If @MonitoringInterval@ is set to a value other than 0, supply a
-- @MonitoringRoleArn@ value.
--
-- Valid for: Multi-AZ DB clusters only
--
-- 'networkType', 'modifyDBCluster_networkType' - The network type of the DB cluster.
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
-- 'newDBClusterIdentifier'', 'modifyDBCluster_newDBClusterIdentifier' - The new DB cluster identifier for the DB cluster when renaming a DB
-- cluster. This value is stored as a lowercase string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens
--
-- -   The first character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
--
-- Example: @my-cluster2@
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'optionGroupName', 'modifyDBCluster_optionGroupName' - A value that indicates that the DB cluster should be associated with the
-- specified option group.
--
-- DB clusters are associated with a default option group that can\'t be
-- modified.
--
-- 'performanceInsightsKMSKeyId', 'modifyDBCluster_performanceInsightsKMSKeyId' - The Amazon Web Services KMS key identifier for encryption of Performance
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
-- 'performanceInsightsRetentionPeriod', 'modifyDBCluster_performanceInsightsRetentionPeriod' - The number of days to retain Performance Insights data. The default is 7
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
-- 'port', 'modifyDBCluster_port' - The port number on which the DB cluster accepts connections.
--
-- Constraints: Value must be @1150-65535@
--
-- Default: The same port as the original DB cluster.
--
-- Valid for: Aurora DB clusters only
--
-- 'preferredBackupWindow', 'modifyDBCluster_preferredBackupWindow' - The daily time range during which automated backups are created if
-- automated backups are enabled, using the @BackupRetentionPeriod@
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
-- 'preferredMaintenanceWindow', 'modifyDBCluster_preferredMaintenanceWindow' - The weekly time range during which system maintenance can occur, in
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
-- 'rotateMasterUserPassword', 'modifyDBCluster_rotateMasterUserPassword' - A value that indicates whether to rotate the secret managed by Amazon
-- Web Services Secrets Manager for the master user password.
--
-- This setting is valid only if the master user password is managed by RDS
-- in Amazon Web Services Secrets Manager for the DB cluster. The secret
-- value contains the updated password.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
-- in the /Amazon RDS User Guide/ and
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
-- in the /Amazon Aurora User Guide./
--
-- Constraints:
--
-- -   You must apply the change immediately when rotating the master user
--     password.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'scalingConfiguration', 'modifyDBCluster_scalingConfiguration' - The scaling properties of the DB cluster. You can only modify scaling
-- properties for DB clusters in @serverless@ DB engine mode.
--
-- Valid for: Aurora DB clusters only
--
-- 'serverlessV2ScalingConfiguration', 'modifyDBCluster_serverlessV2ScalingConfiguration' - Undocumented member.
--
-- 'storageType', 'modifyDBCluster_storageType' - Specifies the storage type to be associated with the DB cluster.
--
-- Valid values: @io1@
--
-- When specified, a value for the @Iops@ parameter is required.
--
-- Default: @io1@
--
-- Valid for: Multi-AZ DB clusters only
--
-- 'vpcSecurityGroupIds', 'modifyDBCluster_vpcSecurityGroupIds' - A list of VPC security groups that the DB cluster will belong to.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
--
-- 'dbClusterIdentifier', 'modifyDBCluster_dbClusterIdentifier' - The DB cluster identifier for the cluster being modified. This parameter
-- isn\'t case-sensitive.
--
-- Constraints: This identifier must match the identifier of an existing DB
-- cluster.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
newModifyDBCluster ::
  -- | 'dbClusterIdentifier'
  Prelude.Text ->
  ModifyDBCluster
newModifyDBCluster pDBClusterIdentifier_ =
  ModifyDBCluster'
    { allocatedStorage =
        Prelude.Nothing,
      allowMajorVersionUpgrade = Prelude.Nothing,
      applyImmediately = Prelude.Nothing,
      autoMinorVersionUpgrade = Prelude.Nothing,
      backtrackWindow = Prelude.Nothing,
      backupRetentionPeriod = Prelude.Nothing,
      cloudwatchLogsExportConfiguration = Prelude.Nothing,
      copyTagsToSnapshot = Prelude.Nothing,
      dbClusterInstanceClass = Prelude.Nothing,
      dbClusterParameterGroupName = Prelude.Nothing,
      dbInstanceParameterGroupName = Prelude.Nothing,
      deletionProtection = Prelude.Nothing,
      domain = Prelude.Nothing,
      domainIAMRoleName = Prelude.Nothing,
      enableGlobalWriteForwarding = Prelude.Nothing,
      enableHttpEndpoint = Prelude.Nothing,
      enableIAMDatabaseAuthentication = Prelude.Nothing,
      enablePerformanceInsights = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      iops = Prelude.Nothing,
      manageMasterUserPassword = Prelude.Nothing,
      masterUserPassword = Prelude.Nothing,
      masterUserSecretKmsKeyId = Prelude.Nothing,
      monitoringInterval = Prelude.Nothing,
      monitoringRoleArn = Prelude.Nothing,
      networkType = Prelude.Nothing,
      newDBClusterIdentifier' = Prelude.Nothing,
      optionGroupName = Prelude.Nothing,
      performanceInsightsKMSKeyId = Prelude.Nothing,
      performanceInsightsRetentionPeriod = Prelude.Nothing,
      port = Prelude.Nothing,
      preferredBackupWindow = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      rotateMasterUserPassword = Prelude.Nothing,
      scalingConfiguration = Prelude.Nothing,
      serverlessV2ScalingConfiguration = Prelude.Nothing,
      storageType = Prelude.Nothing,
      vpcSecurityGroupIds = Prelude.Nothing,
      dbClusterIdentifier = pDBClusterIdentifier_
    }

-- | The amount of storage in gibibytes (GiB) to allocate to each DB instance
-- in the Multi-AZ DB cluster.
--
-- Type: Integer
--
-- Valid for: Multi-AZ DB clusters only
modifyDBCluster_allocatedStorage :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Int)
modifyDBCluster_allocatedStorage = Lens.lens (\ModifyDBCluster' {allocatedStorage} -> allocatedStorage) (\s@ModifyDBCluster' {} a -> s {allocatedStorage = a} :: ModifyDBCluster)

-- | A value that indicates whether major version upgrades are allowed.
--
-- Constraints: You must allow major version upgrades when specifying a
-- value for the @EngineVersion@ parameter that is a different major
-- version than the DB cluster\'s current version.
--
-- Valid for: Aurora DB clusters only
modifyDBCluster_allowMajorVersionUpgrade :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Bool)
modifyDBCluster_allowMajorVersionUpgrade = Lens.lens (\ModifyDBCluster' {allowMajorVersionUpgrade} -> allowMajorVersionUpgrade) (\s@ModifyDBCluster' {} a -> s {allowMajorVersionUpgrade = a} :: ModifyDBCluster)

-- | A value that indicates whether the modifications in this request and any
-- pending modifications are asynchronously applied as soon as possible,
-- regardless of the @PreferredMaintenanceWindow@ setting for the DB
-- cluster. If this parameter is disabled, changes to the DB cluster are
-- applied during the next maintenance window.
--
-- The @ApplyImmediately@ parameter only affects the
-- @EnableIAMDatabaseAuthentication@, @MasterUserPassword@, and
-- @NewDBClusterIdentifier@ values. If the @ApplyImmediately@ parameter is
-- disabled, then changes to the @EnableIAMDatabaseAuthentication@,
-- @MasterUserPassword@, and @NewDBClusterIdentifier@ values are applied
-- during the next maintenance window. All other changes are applied
-- immediately, regardless of the value of the @ApplyImmediately@
-- parameter.
--
-- By default, this parameter is disabled.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
modifyDBCluster_applyImmediately :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Bool)
modifyDBCluster_applyImmediately = Lens.lens (\ModifyDBCluster' {applyImmediately} -> applyImmediately) (\s@ModifyDBCluster' {} a -> s {applyImmediately = a} :: ModifyDBCluster)

-- | A value that indicates whether minor engine upgrades are applied
-- automatically to the DB cluster during the maintenance window. By
-- default, minor engine upgrades are applied automatically.
--
-- Valid for: Multi-AZ DB clusters only
modifyDBCluster_autoMinorVersionUpgrade :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Bool)
modifyDBCluster_autoMinorVersionUpgrade = Lens.lens (\ModifyDBCluster' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@ModifyDBCluster' {} a -> s {autoMinorVersionUpgrade = a} :: ModifyDBCluster)

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
modifyDBCluster_backtrackWindow :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Integer)
modifyDBCluster_backtrackWindow = Lens.lens (\ModifyDBCluster' {backtrackWindow} -> backtrackWindow) (\s@ModifyDBCluster' {} a -> s {backtrackWindow = a} :: ModifyDBCluster)

-- | The number of days for which automated backups are retained. Specify a
-- minimum value of 1.
--
-- Default: 1
--
-- Constraints:
--
-- -   Must be a value from 1 to 35
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
modifyDBCluster_backupRetentionPeriod :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Int)
modifyDBCluster_backupRetentionPeriod = Lens.lens (\ModifyDBCluster' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@ModifyDBCluster' {} a -> s {backupRetentionPeriod = a} :: ModifyDBCluster)

-- | The configuration setting for the log types to be enabled for export to
-- CloudWatch Logs for a specific DB cluster. The values in the list depend
-- on the DB engine being used.
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
modifyDBCluster_cloudwatchLogsExportConfiguration :: Lens.Lens' ModifyDBCluster (Prelude.Maybe CloudwatchLogsExportConfiguration)
modifyDBCluster_cloudwatchLogsExportConfiguration = Lens.lens (\ModifyDBCluster' {cloudwatchLogsExportConfiguration} -> cloudwatchLogsExportConfiguration) (\s@ModifyDBCluster' {} a -> s {cloudwatchLogsExportConfiguration = a} :: ModifyDBCluster)

-- | A value that indicates whether to copy all tags from the DB cluster to
-- snapshots of the DB cluster. The default is not to copy them.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
modifyDBCluster_copyTagsToSnapshot :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Bool)
modifyDBCluster_copyTagsToSnapshot = Lens.lens (\ModifyDBCluster' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@ModifyDBCluster' {} a -> s {copyTagsToSnapshot = a} :: ModifyDBCluster)

-- | The compute and memory capacity of each DB instance in the Multi-AZ DB
-- cluster, for example db.m6gd.xlarge. Not all DB instance classes are
-- available in all Amazon Web Services Regions, or for all database
-- engines.
--
-- For the full list of DB instance classes and availability for your
-- engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
-- in the /Amazon RDS User Guide/.
--
-- Valid for: Multi-AZ DB clusters only
modifyDBCluster_dbClusterInstanceClass :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Text)
modifyDBCluster_dbClusterInstanceClass = Lens.lens (\ModifyDBCluster' {dbClusterInstanceClass} -> dbClusterInstanceClass) (\s@ModifyDBCluster' {} a -> s {dbClusterInstanceClass = a} :: ModifyDBCluster)

-- | The name of the DB cluster parameter group to use for the DB cluster.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
modifyDBCluster_dbClusterParameterGroupName :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Text)
modifyDBCluster_dbClusterParameterGroupName = Lens.lens (\ModifyDBCluster' {dbClusterParameterGroupName} -> dbClusterParameterGroupName) (\s@ModifyDBCluster' {} a -> s {dbClusterParameterGroupName = a} :: ModifyDBCluster)

-- | The name of the DB parameter group to apply to all instances of the DB
-- cluster.
--
-- When you apply a parameter group using the
-- @DBInstanceParameterGroupName@ parameter, the DB cluster isn\'t rebooted
-- automatically. Also, parameter changes are applied immediately rather
-- than during the next maintenance window.
--
-- Default: The existing name setting
--
-- Constraints:
--
-- -   The DB parameter group must be in the same DB parameter group family
--     as this DB cluster.
--
-- -   The @DBInstanceParameterGroupName@ parameter is valid in combination
--     with the @AllowMajorVersionUpgrade@ parameter for a major version
--     upgrade only.
--
-- Valid for: Aurora DB clusters only
modifyDBCluster_dbInstanceParameterGroupName :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Text)
modifyDBCluster_dbInstanceParameterGroupName = Lens.lens (\ModifyDBCluster' {dbInstanceParameterGroupName} -> dbInstanceParameterGroupName) (\s@ModifyDBCluster' {} a -> s {dbInstanceParameterGroupName = a} :: ModifyDBCluster)

-- | A value that indicates whether the DB cluster has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection isn\'t enabled.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
modifyDBCluster_deletionProtection :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Bool)
modifyDBCluster_deletionProtection = Lens.lens (\ModifyDBCluster' {deletionProtection} -> deletionProtection) (\s@ModifyDBCluster' {} a -> s {deletionProtection = a} :: ModifyDBCluster)

-- | The Active Directory directory ID to move the DB cluster to. Specify
-- @none@ to remove the cluster from its current domain. The domain must be
-- created prior to this operation.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon Aurora User Guide/.
--
-- Valid for: Aurora DB clusters only
modifyDBCluster_domain :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Text)
modifyDBCluster_domain = Lens.lens (\ModifyDBCluster' {domain} -> domain) (\s@ModifyDBCluster' {} a -> s {domain = a} :: ModifyDBCluster)

-- | Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
--
-- Valid for: Aurora DB clusters only
modifyDBCluster_domainIAMRoleName :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Text)
modifyDBCluster_domainIAMRoleName = Lens.lens (\ModifyDBCluster' {domainIAMRoleName} -> domainIAMRoleName) (\s@ModifyDBCluster' {} a -> s {domainIAMRoleName = a} :: ModifyDBCluster)

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
modifyDBCluster_enableGlobalWriteForwarding :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Bool)
modifyDBCluster_enableGlobalWriteForwarding = Lens.lens (\ModifyDBCluster' {enableGlobalWriteForwarding} -> enableGlobalWriteForwarding) (\s@ModifyDBCluster' {} a -> s {enableGlobalWriteForwarding = a} :: ModifyDBCluster)

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
modifyDBCluster_enableHttpEndpoint :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Bool)
modifyDBCluster_enableHttpEndpoint = Lens.lens (\ModifyDBCluster' {enableHttpEndpoint} -> enableHttpEndpoint) (\s@ModifyDBCluster' {} a -> s {enableHttpEndpoint = a} :: ModifyDBCluster)

-- | A value that indicates whether to enable mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts. By
-- default, mapping isn\'t enabled.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
-- in the /Amazon Aurora User Guide/.
--
-- Valid for: Aurora DB clusters only
modifyDBCluster_enableIAMDatabaseAuthentication :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Bool)
modifyDBCluster_enableIAMDatabaseAuthentication = Lens.lens (\ModifyDBCluster' {enableIAMDatabaseAuthentication} -> enableIAMDatabaseAuthentication) (\s@ModifyDBCluster' {} a -> s {enableIAMDatabaseAuthentication = a} :: ModifyDBCluster)

-- | A value that indicates whether to turn on Performance Insights for the
-- DB cluster.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights>
-- in the /Amazon RDS User Guide/.
--
-- Valid for: Multi-AZ DB clusters only
modifyDBCluster_enablePerformanceInsights :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Bool)
modifyDBCluster_enablePerformanceInsights = Lens.lens (\ModifyDBCluster' {enablePerformanceInsights} -> enablePerformanceInsights) (\s@ModifyDBCluster' {} a -> s {enablePerformanceInsights = a} :: ModifyDBCluster)

-- | The version number of the database engine to which you want to upgrade.
-- Changing this parameter results in an outage. The change is applied
-- during the next maintenance window unless @ApplyImmediately@ is enabled.
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
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
modifyDBCluster_engineVersion :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Text)
modifyDBCluster_engineVersion = Lens.lens (\ModifyDBCluster' {engineVersion} -> engineVersion) (\s@ModifyDBCluster' {} a -> s {engineVersion = a} :: ModifyDBCluster)

-- | The amount of Provisioned IOPS (input\/output operations per second) to
-- be initially allocated for each DB instance in the Multi-AZ DB cluster.
--
-- For information about valid IOPS values, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Storage.html#USER_PIOPS Amazon RDS Provisioned IOPS storage>
-- in the /Amazon RDS User Guide/.
--
-- Constraints: Must be a multiple between .5 and 50 of the storage amount
-- for the DB cluster.
--
-- Valid for: Multi-AZ DB clusters only
modifyDBCluster_iops :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Int)
modifyDBCluster_iops = Lens.lens (\ModifyDBCluster' {iops} -> iops) (\s@ModifyDBCluster' {} a -> s {iops = a} :: ModifyDBCluster)

-- | A value that indicates whether to manage the master user password with
-- Amazon Web Services Secrets Manager.
--
-- If the DB cluster doesn\'t manage the master user password with Amazon
-- Web Services Secrets Manager, you can turn on this management. In this
-- case, you can\'t specify @MasterUserPassword@.
--
-- If the DB cluster already manages the master user password with Amazon
-- Web Services Secrets Manager, and you specify that the master user
-- password is not managed with Amazon Web Services Secrets Manager, then
-- you must specify @MasterUserPassword@. In this case, RDS deletes the
-- secret and uses the new password for the master user specified by
-- @MasterUserPassword@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
-- in the /Amazon RDS User Guide/ and
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
-- in the /Amazon Aurora User Guide./
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
modifyDBCluster_manageMasterUserPassword :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Bool)
modifyDBCluster_manageMasterUserPassword = Lens.lens (\ModifyDBCluster' {manageMasterUserPassword} -> manageMasterUserPassword) (\s@ModifyDBCluster' {} a -> s {manageMasterUserPassword = a} :: ModifyDBCluster)

-- | The new password for the master database user. This password can contain
-- any printable ASCII character except \"\/\", \"\"\", or \"\@\".
--
-- Constraints:
--
-- -   Must contain from 8 to 41 characters.
--
-- -   Can\'t be specified if @ManageMasterUserPassword@ is turned on.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
modifyDBCluster_masterUserPassword :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Text)
modifyDBCluster_masterUserPassword = Lens.lens (\ModifyDBCluster' {masterUserPassword} -> masterUserPassword) (\s@ModifyDBCluster' {} a -> s {masterUserPassword = a} :: ModifyDBCluster)

-- | The Amazon Web Services KMS key identifier to encrypt a secret that is
-- automatically generated and managed in Amazon Web Services Secrets
-- Manager.
--
-- This setting is valid only if both of the following conditions are met:
--
-- -   The DB cluster doesn\'t manage the master user password in Amazon
--     Web Services Secrets Manager.
--
--     If the DB cluster already manages the master user password in Amazon
--     Web Services Secrets Manager, you can\'t change the KMS key that is
--     used to encrypt the secret.
--
-- -   You are turning on @ManageMasterUserPassword@ to manage the master
--     user password in Amazon Web Services Secrets Manager.
--
--     If you are turning on @ManageMasterUserPassword@ and don\'t specify
--     @MasterUserSecretKmsKeyId@, then the @aws\/secretsmanager@ KMS key
--     is used to encrypt the secret. If the secret is in a different
--     Amazon Web Services account, then you can\'t use the
--     @aws\/secretsmanager@ KMS key to encrypt the secret, and you must
--     use a customer managed KMS key.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the KMS key. To use a KMS key in a different
-- Amazon Web Services account, specify the key ARN or alias ARN.
--
-- There is a default KMS key for your Amazon Web Services account. Your
-- Amazon Web Services account has a different default KMS key for each
-- Amazon Web Services Region.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
modifyDBCluster_masterUserSecretKmsKeyId :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Text)
modifyDBCluster_masterUserSecretKmsKeyId = Lens.lens (\ModifyDBCluster' {masterUserSecretKmsKeyId} -> masterUserSecretKmsKeyId) (\s@ModifyDBCluster' {} a -> s {masterUserSecretKmsKeyId = a} :: ModifyDBCluster)

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
modifyDBCluster_monitoringInterval :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Int)
modifyDBCluster_monitoringInterval = Lens.lens (\ModifyDBCluster' {monitoringInterval} -> monitoringInterval) (\s@ModifyDBCluster' {} a -> s {monitoringInterval = a} :: ModifyDBCluster)

-- | The Amazon Resource Name (ARN) for the IAM role that permits RDS to send
-- Enhanced Monitoring metrics to Amazon CloudWatch Logs. An example is
-- @arn:aws:iam:123456789012:role\/emaccess@. For information on creating a
-- monitoring role, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.html#USER_Monitoring.OS.IAMRole To create an IAM role for Amazon RDS Enhanced Monitoring>
-- in the /Amazon RDS User Guide./
--
-- If @MonitoringInterval@ is set to a value other than 0, supply a
-- @MonitoringRoleArn@ value.
--
-- Valid for: Multi-AZ DB clusters only
modifyDBCluster_monitoringRoleArn :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Text)
modifyDBCluster_monitoringRoleArn = Lens.lens (\ModifyDBCluster' {monitoringRoleArn} -> monitoringRoleArn) (\s@ModifyDBCluster' {} a -> s {monitoringRoleArn = a} :: ModifyDBCluster)

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
modifyDBCluster_networkType :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Text)
modifyDBCluster_networkType = Lens.lens (\ModifyDBCluster' {networkType} -> networkType) (\s@ModifyDBCluster' {} a -> s {networkType = a} :: ModifyDBCluster)

-- | The new DB cluster identifier for the DB cluster when renaming a DB
-- cluster. This value is stored as a lowercase string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens
--
-- -   The first character must be a letter
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens
--
-- Example: @my-cluster2@
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
modifyDBCluster_newDBClusterIdentifier :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Text)
modifyDBCluster_newDBClusterIdentifier = Lens.lens (\ModifyDBCluster' {newDBClusterIdentifier'} -> newDBClusterIdentifier') (\s@ModifyDBCluster' {} a -> s {newDBClusterIdentifier' = a} :: ModifyDBCluster)

-- | A value that indicates that the DB cluster should be associated with the
-- specified option group.
--
-- DB clusters are associated with a default option group that can\'t be
-- modified.
modifyDBCluster_optionGroupName :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Text)
modifyDBCluster_optionGroupName = Lens.lens (\ModifyDBCluster' {optionGroupName} -> optionGroupName) (\s@ModifyDBCluster' {} a -> s {optionGroupName = a} :: ModifyDBCluster)

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
modifyDBCluster_performanceInsightsKMSKeyId :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Text)
modifyDBCluster_performanceInsightsKMSKeyId = Lens.lens (\ModifyDBCluster' {performanceInsightsKMSKeyId} -> performanceInsightsKMSKeyId) (\s@ModifyDBCluster' {} a -> s {performanceInsightsKMSKeyId = a} :: ModifyDBCluster)

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
modifyDBCluster_performanceInsightsRetentionPeriod :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Int)
modifyDBCluster_performanceInsightsRetentionPeriod = Lens.lens (\ModifyDBCluster' {performanceInsightsRetentionPeriod} -> performanceInsightsRetentionPeriod) (\s@ModifyDBCluster' {} a -> s {performanceInsightsRetentionPeriod = a} :: ModifyDBCluster)

-- | The port number on which the DB cluster accepts connections.
--
-- Constraints: Value must be @1150-65535@
--
-- Default: The same port as the original DB cluster.
--
-- Valid for: Aurora DB clusters only
modifyDBCluster_port :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Int)
modifyDBCluster_port = Lens.lens (\ModifyDBCluster' {port} -> port) (\s@ModifyDBCluster' {} a -> s {port = a} :: ModifyDBCluster)

-- | The daily time range during which automated backups are created if
-- automated backups are enabled, using the @BackupRetentionPeriod@
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
modifyDBCluster_preferredBackupWindow :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Text)
modifyDBCluster_preferredBackupWindow = Lens.lens (\ModifyDBCluster' {preferredBackupWindow} -> preferredBackupWindow) (\s@ModifyDBCluster' {} a -> s {preferredBackupWindow = a} :: ModifyDBCluster)

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
modifyDBCluster_preferredMaintenanceWindow :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Text)
modifyDBCluster_preferredMaintenanceWindow = Lens.lens (\ModifyDBCluster' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@ModifyDBCluster' {} a -> s {preferredMaintenanceWindow = a} :: ModifyDBCluster)

-- | A value that indicates whether to rotate the secret managed by Amazon
-- Web Services Secrets Manager for the master user password.
--
-- This setting is valid only if the master user password is managed by RDS
-- in Amazon Web Services Secrets Manager for the DB cluster. The secret
-- value contains the updated password.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
-- in the /Amazon RDS User Guide/ and
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/rds-secrets-manager.html Password management with Amazon Web Services Secrets Manager>
-- in the /Amazon Aurora User Guide./
--
-- Constraints:
--
-- -   You must apply the change immediately when rotating the master user
--     password.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
modifyDBCluster_rotateMasterUserPassword :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Bool)
modifyDBCluster_rotateMasterUserPassword = Lens.lens (\ModifyDBCluster' {rotateMasterUserPassword} -> rotateMasterUserPassword) (\s@ModifyDBCluster' {} a -> s {rotateMasterUserPassword = a} :: ModifyDBCluster)

-- | The scaling properties of the DB cluster. You can only modify scaling
-- properties for DB clusters in @serverless@ DB engine mode.
--
-- Valid for: Aurora DB clusters only
modifyDBCluster_scalingConfiguration :: Lens.Lens' ModifyDBCluster (Prelude.Maybe ScalingConfiguration)
modifyDBCluster_scalingConfiguration = Lens.lens (\ModifyDBCluster' {scalingConfiguration} -> scalingConfiguration) (\s@ModifyDBCluster' {} a -> s {scalingConfiguration = a} :: ModifyDBCluster)

-- | Undocumented member.
modifyDBCluster_serverlessV2ScalingConfiguration :: Lens.Lens' ModifyDBCluster (Prelude.Maybe ServerlessV2ScalingConfiguration)
modifyDBCluster_serverlessV2ScalingConfiguration = Lens.lens (\ModifyDBCluster' {serverlessV2ScalingConfiguration} -> serverlessV2ScalingConfiguration) (\s@ModifyDBCluster' {} a -> s {serverlessV2ScalingConfiguration = a} :: ModifyDBCluster)

-- | Specifies the storage type to be associated with the DB cluster.
--
-- Valid values: @io1@
--
-- When specified, a value for the @Iops@ parameter is required.
--
-- Default: @io1@
--
-- Valid for: Multi-AZ DB clusters only
modifyDBCluster_storageType :: Lens.Lens' ModifyDBCluster (Prelude.Maybe Prelude.Text)
modifyDBCluster_storageType = Lens.lens (\ModifyDBCluster' {storageType} -> storageType) (\s@ModifyDBCluster' {} a -> s {storageType = a} :: ModifyDBCluster)

-- | A list of VPC security groups that the DB cluster will belong to.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
modifyDBCluster_vpcSecurityGroupIds :: Lens.Lens' ModifyDBCluster (Prelude.Maybe [Prelude.Text])
modifyDBCluster_vpcSecurityGroupIds = Lens.lens (\ModifyDBCluster' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@ModifyDBCluster' {} a -> s {vpcSecurityGroupIds = a} :: ModifyDBCluster) Prelude.. Lens.mapping Lens.coerced

-- | The DB cluster identifier for the cluster being modified. This parameter
-- isn\'t case-sensitive.
--
-- Constraints: This identifier must match the identifier of an existing DB
-- cluster.
--
-- Valid for: Aurora DB clusters and Multi-AZ DB clusters
modifyDBCluster_dbClusterIdentifier :: Lens.Lens' ModifyDBCluster Prelude.Text
modifyDBCluster_dbClusterIdentifier = Lens.lens (\ModifyDBCluster' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@ModifyDBCluster' {} a -> s {dbClusterIdentifier = a} :: ModifyDBCluster)

instance Core.AWSRequest ModifyDBCluster where
  type
    AWSResponse ModifyDBCluster =
      ModifyDBClusterResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ModifyDBClusterResult"
      ( \s h x ->
          ModifyDBClusterResponse'
            Prelude.<$> (x Data..@? "DBCluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyDBCluster where
  hashWithSalt _salt ModifyDBCluster' {..} =
    _salt
      `Prelude.hashWithSalt` allocatedStorage
      `Prelude.hashWithSalt` allowMajorVersionUpgrade
      `Prelude.hashWithSalt` applyImmediately
      `Prelude.hashWithSalt` autoMinorVersionUpgrade
      `Prelude.hashWithSalt` backtrackWindow
      `Prelude.hashWithSalt` backupRetentionPeriod
      `Prelude.hashWithSalt` cloudwatchLogsExportConfiguration
      `Prelude.hashWithSalt` copyTagsToSnapshot
      `Prelude.hashWithSalt` dbClusterInstanceClass
      `Prelude.hashWithSalt` dbClusterParameterGroupName
      `Prelude.hashWithSalt` dbInstanceParameterGroupName
      `Prelude.hashWithSalt` deletionProtection
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` domainIAMRoleName
      `Prelude.hashWithSalt` enableGlobalWriteForwarding
      `Prelude.hashWithSalt` enableHttpEndpoint
      `Prelude.hashWithSalt` enableIAMDatabaseAuthentication
      `Prelude.hashWithSalt` enablePerformanceInsights
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` manageMasterUserPassword
      `Prelude.hashWithSalt` masterUserPassword
      `Prelude.hashWithSalt` masterUserSecretKmsKeyId
      `Prelude.hashWithSalt` monitoringInterval
      `Prelude.hashWithSalt` monitoringRoleArn
      `Prelude.hashWithSalt` networkType
      `Prelude.hashWithSalt` newDBClusterIdentifier'
      `Prelude.hashWithSalt` optionGroupName
      `Prelude.hashWithSalt` performanceInsightsKMSKeyId
      `Prelude.hashWithSalt` performanceInsightsRetentionPeriod
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` preferredBackupWindow
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` rotateMasterUserPassword
      `Prelude.hashWithSalt` scalingConfiguration
      `Prelude.hashWithSalt` serverlessV2ScalingConfiguration
      `Prelude.hashWithSalt` storageType
      `Prelude.hashWithSalt` vpcSecurityGroupIds
      `Prelude.hashWithSalt` dbClusterIdentifier

instance Prelude.NFData ModifyDBCluster where
  rnf ModifyDBCluster' {..} =
    Prelude.rnf allocatedStorage
      `Prelude.seq` Prelude.rnf allowMajorVersionUpgrade
      `Prelude.seq` Prelude.rnf applyImmediately
      `Prelude.seq` Prelude.rnf autoMinorVersionUpgrade
      `Prelude.seq` Prelude.rnf backtrackWindow
      `Prelude.seq` Prelude.rnf backupRetentionPeriod
      `Prelude.seq` Prelude.rnf cloudwatchLogsExportConfiguration
      `Prelude.seq` Prelude.rnf copyTagsToSnapshot
      `Prelude.seq` Prelude.rnf dbClusterInstanceClass
      `Prelude.seq` Prelude.rnf dbClusterParameterGroupName
      `Prelude.seq` Prelude.rnf dbInstanceParameterGroupName
      `Prelude.seq` Prelude.rnf deletionProtection
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf domainIAMRoleName
      `Prelude.seq` Prelude.rnf enableGlobalWriteForwarding
      `Prelude.seq` Prelude.rnf enableHttpEndpoint
      `Prelude.seq` Prelude.rnf
        enableIAMDatabaseAuthentication
      `Prelude.seq` Prelude.rnf
        enablePerformanceInsights
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf iops
      `Prelude.seq` Prelude.rnf
        manageMasterUserPassword
      `Prelude.seq` Prelude.rnf
        masterUserPassword
      `Prelude.seq` Prelude.rnf
        masterUserSecretKmsKeyId
      `Prelude.seq` Prelude.rnf
        monitoringInterval
      `Prelude.seq` Prelude.rnf
        monitoringRoleArn
      `Prelude.seq` Prelude.rnf
        networkType
      `Prelude.seq` Prelude.rnf
        newDBClusterIdentifier'
      `Prelude.seq` Prelude.rnf
        optionGroupName
      `Prelude.seq` Prelude.rnf
        performanceInsightsKMSKeyId
      `Prelude.seq` Prelude.rnf
        performanceInsightsRetentionPeriod
      `Prelude.seq` Prelude.rnf
        port
      `Prelude.seq` Prelude.rnf
        preferredBackupWindow
      `Prelude.seq` Prelude.rnf
        preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf
        rotateMasterUserPassword
      `Prelude.seq` Prelude.rnf
        scalingConfiguration
      `Prelude.seq` Prelude.rnf
        serverlessV2ScalingConfiguration
      `Prelude.seq` Prelude.rnf
        storageType
      `Prelude.seq` Prelude.rnf
        vpcSecurityGroupIds
      `Prelude.seq` Prelude.rnf
        dbClusterIdentifier

instance Data.ToHeaders ModifyDBCluster where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyDBCluster where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyDBCluster where
  toQuery ModifyDBCluster' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyDBCluster" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "AllocatedStorage" Data.=: allocatedStorage,
        "AllowMajorVersionUpgrade"
          Data.=: allowMajorVersionUpgrade,
        "ApplyImmediately" Data.=: applyImmediately,
        "AutoMinorVersionUpgrade"
          Data.=: autoMinorVersionUpgrade,
        "BacktrackWindow" Data.=: backtrackWindow,
        "BackupRetentionPeriod"
          Data.=: backupRetentionPeriod,
        "CloudwatchLogsExportConfiguration"
          Data.=: cloudwatchLogsExportConfiguration,
        "CopyTagsToSnapshot" Data.=: copyTagsToSnapshot,
        "DBClusterInstanceClass"
          Data.=: dbClusterInstanceClass,
        "DBClusterParameterGroupName"
          Data.=: dbClusterParameterGroupName,
        "DBInstanceParameterGroupName"
          Data.=: dbInstanceParameterGroupName,
        "DeletionProtection" Data.=: deletionProtection,
        "Domain" Data.=: domain,
        "DomainIAMRoleName" Data.=: domainIAMRoleName,
        "EnableGlobalWriteForwarding"
          Data.=: enableGlobalWriteForwarding,
        "EnableHttpEndpoint" Data.=: enableHttpEndpoint,
        "EnableIAMDatabaseAuthentication"
          Data.=: enableIAMDatabaseAuthentication,
        "EnablePerformanceInsights"
          Data.=: enablePerformanceInsights,
        "EngineVersion" Data.=: engineVersion,
        "Iops" Data.=: iops,
        "ManageMasterUserPassword"
          Data.=: manageMasterUserPassword,
        "MasterUserPassword" Data.=: masterUserPassword,
        "MasterUserSecretKmsKeyId"
          Data.=: masterUserSecretKmsKeyId,
        "MonitoringInterval" Data.=: monitoringInterval,
        "MonitoringRoleArn" Data.=: monitoringRoleArn,
        "NetworkType" Data.=: networkType,
        "NewDBClusterIdentifier"
          Data.=: newDBClusterIdentifier',
        "OptionGroupName" Data.=: optionGroupName,
        "PerformanceInsightsKMSKeyId"
          Data.=: performanceInsightsKMSKeyId,
        "PerformanceInsightsRetentionPeriod"
          Data.=: performanceInsightsRetentionPeriod,
        "Port" Data.=: port,
        "PreferredBackupWindow"
          Data.=: preferredBackupWindow,
        "PreferredMaintenanceWindow"
          Data.=: preferredMaintenanceWindow,
        "RotateMasterUserPassword"
          Data.=: rotateMasterUserPassword,
        "ScalingConfiguration" Data.=: scalingConfiguration,
        "ServerlessV2ScalingConfiguration"
          Data.=: serverlessV2ScalingConfiguration,
        "StorageType" Data.=: storageType,
        "VpcSecurityGroupIds"
          Data.=: Data.toQuery
            ( Data.toQueryList "VpcSecurityGroupId"
                Prelude.<$> vpcSecurityGroupIds
            ),
        "DBClusterIdentifier" Data.=: dbClusterIdentifier
      ]

-- | /See:/ 'newModifyDBClusterResponse' smart constructor.
data ModifyDBClusterResponse = ModifyDBClusterResponse'
  { dbCluster :: Prelude.Maybe DBCluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyDBClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbCluster', 'modifyDBClusterResponse_dbCluster' - Undocumented member.
--
-- 'httpStatus', 'modifyDBClusterResponse_httpStatus' - The response's http status code.
newModifyDBClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyDBClusterResponse
newModifyDBClusterResponse pHttpStatus_ =
  ModifyDBClusterResponse'
    { dbCluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyDBClusterResponse_dbCluster :: Lens.Lens' ModifyDBClusterResponse (Prelude.Maybe DBCluster)
modifyDBClusterResponse_dbCluster = Lens.lens (\ModifyDBClusterResponse' {dbCluster} -> dbCluster) (\s@ModifyDBClusterResponse' {} a -> s {dbCluster = a} :: ModifyDBClusterResponse)

-- | The response's http status code.
modifyDBClusterResponse_httpStatus :: Lens.Lens' ModifyDBClusterResponse Prelude.Int
modifyDBClusterResponse_httpStatus = Lens.lens (\ModifyDBClusterResponse' {httpStatus} -> httpStatus) (\s@ModifyDBClusterResponse' {} a -> s {httpStatus = a} :: ModifyDBClusterResponse)

instance Prelude.NFData ModifyDBClusterResponse where
  rnf ModifyDBClusterResponse' {..} =
    Prelude.rnf dbCluster `Prelude.seq`
      Prelude.rnf httpStatus
