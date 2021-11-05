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
-- Module      : Amazonka.RDS.ModifyDBInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies settings for a DB instance. You can change one or more database
-- configuration parameters by specifying these parameters and the new
-- values in the request. To learn what modifications you can make to your
-- DB instance, call @DescribeValidDBInstanceModifications@ before you call
-- @ModifyDBInstance@.
module Amazonka.RDS.ModifyDBInstance
  ( -- * Creating a Request
    ModifyDBInstance (..),
    newModifyDBInstance,

    -- * Request Lenses
    modifyDBInstance_engineVersion,
    modifyDBInstance_dbSecurityGroups,
    modifyDBInstance_dbPortNumber,
    modifyDBInstance_deletionProtection,
    modifyDBInstance_masterUserPassword,
    modifyDBInstance_publiclyAccessible,
    modifyDBInstance_autoMinorVersionUpgrade,
    modifyDBInstance_dbSubnetGroupName,
    modifyDBInstance_monitoringRoleArn,
    modifyDBInstance_iops,
    modifyDBInstance_allowMajorVersionUpgrade,
    modifyDBInstance_newDBInstanceIdentifier,
    modifyDBInstance_domain,
    modifyDBInstance_enableCustomerOwnedIp,
    modifyDBInstance_replicaMode,
    modifyDBInstance_monitoringInterval,
    modifyDBInstance_cloudwatchLogsExportConfiguration,
    modifyDBInstance_certificateRotationRestart,
    modifyDBInstance_tdeCredentialPassword,
    modifyDBInstance_processorFeatures,
    modifyDBInstance_dbInstanceClass,
    modifyDBInstance_promotionTier,
    modifyDBInstance_awsBackupRecoveryPointArn,
    modifyDBInstance_licenseModel,
    modifyDBInstance_preferredMaintenanceWindow,
    modifyDBInstance_performanceInsightsRetentionPeriod,
    modifyDBInstance_cACertificateIdentifier,
    modifyDBInstance_maxAllocatedStorage,
    modifyDBInstance_enablePerformanceInsights,
    modifyDBInstance_dbParameterGroupName,
    modifyDBInstance_preferredBackupWindow,
    modifyDBInstance_backupRetentionPeriod,
    modifyDBInstance_performanceInsightsKMSKeyId,
    modifyDBInstance_vpcSecurityGroupIds,
    modifyDBInstance_multiAZ,
    modifyDBInstance_allocatedStorage,
    modifyDBInstance_applyImmediately,
    modifyDBInstance_optionGroupName,
    modifyDBInstance_copyTagsToSnapshot,
    modifyDBInstance_tdeCredentialArn,
    modifyDBInstance_domainIAMRoleName,
    modifyDBInstance_enableIAMDatabaseAuthentication,
    modifyDBInstance_useDefaultProcessorFeatures,
    modifyDBInstance_storageType,
    modifyDBInstance_dbInstanceIdentifier,

    -- * Destructuring the Response
    ModifyDBInstanceResponse (..),
    newModifyDBInstanceResponse,

    -- * Response Lenses
    modifyDBInstanceResponse_dbInstance,
    modifyDBInstanceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newModifyDBInstance' smart constructor.
data ModifyDBInstance = ModifyDBInstance'
  { -- | The version number of the database engine to upgrade to. Changing this
    -- parameter results in an outage and the change is applied during the next
    -- maintenance window unless the @ApplyImmediately@ parameter is enabled
    -- for this request.
    --
    -- For major version upgrades, if a nondefault DB parameter group is
    -- currently in use, a new DB parameter group in the DB parameter group
    -- family for the new engine version must be specified. The new DB
    -- parameter group can be the default for that DB parameter group family.
    --
    -- If you specify only a major version, Amazon RDS will update the DB
    -- instance to the default minor version if the current minor version is
    -- lower. For information about valid engine versions, see
    -- @CreateDBInstance@, or call @DescribeDBEngineVersions@.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | A list of DB security groups to authorize on this DB instance. Changing
    -- this setting doesn\'t result in an outage and the change is
    -- asynchronously applied as soon as possible.
    --
    -- Constraints:
    --
    -- -   If supplied, must match existing DBSecurityGroups.
    dbSecurityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The port number on which the database accepts connections.
    --
    -- The value of the @DBPortNumber@ parameter must not match any of the port
    -- values specified for options in the option group for the DB instance.
    --
    -- Your database will restart when you change the @DBPortNumber@ value
    -- regardless of the value of the @ApplyImmediately@ parameter.
    --
    -- __MySQL__
    --
    -- Default: @3306@
    --
    -- Valid values: @1150-65535@
    --
    -- __MariaDB__
    --
    -- Default: @3306@
    --
    -- Valid values: @1150-65535@
    --
    -- __PostgreSQL__
    --
    -- Default: @5432@
    --
    -- Valid values: @1150-65535@
    --
    -- Type: Integer
    --
    -- __Oracle__
    --
    -- Default: @1521@
    --
    -- Valid values: @1150-65535@
    --
    -- __SQL Server__
    --
    -- Default: @1433@
    --
    -- Valid values: @1150-65535@ except @1234@, @1434@, @3260@, @3343@,
    -- @3389@, @47001@, and @49152-49156@.
    --
    -- __Amazon Aurora__
    --
    -- Default: @3306@
    --
    -- Valid values: @1150-65535@
    dbPortNumber :: Prelude.Maybe Prelude.Int,
    -- | A value that indicates whether the DB instance has deletion protection
    -- enabled. The database can\'t be deleted when deletion protection is
    -- enabled. By default, deletion protection is disabled. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | The new password for the master user. The password can include any
    -- printable ASCII character except \"\/\", \"\"\", or \"\@\".
    --
    -- Changing this parameter doesn\'t result in an outage and the change is
    -- asynchronously applied as soon as possible. Between the time of the
    -- request and the completion of the request, the @MasterUserPassword@
    -- element exists in the @PendingModifiedValues@ element of the operation
    -- response.
    --
    -- __Amazon Aurora__
    --
    -- Not applicable. The password for the master user is managed by the DB
    -- cluster. For more information, see @ModifyDBCluster@.
    --
    -- Default: Uses existing setting
    --
    -- __MariaDB__
    --
    -- Constraints: Must contain from 8 to 41 characters.
    --
    -- __Microsoft SQL Server__
    --
    -- Constraints: Must contain from 8 to 128 characters.
    --
    -- __MySQL__
    --
    -- Constraints: Must contain from 8 to 41 characters.
    --
    -- __Oracle__
    --
    -- Constraints: Must contain from 8 to 30 characters.
    --
    -- __PostgreSQL__
    --
    -- Constraints: Must contain from 8 to 128 characters.
    --
    -- Amazon RDS API actions never return the password, so this action
    -- provides a way to regain access to a primary instance user if the
    -- password is lost. This includes restoring privileges that might have
    -- been accidentally revoked.
    masterUserPassword :: Prelude.Maybe Prelude.Text,
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
    -- @PubliclyAccessible@ only applies to DB instances in a VPC. The DB
    -- instance must be part of a public subnet and @PubliclyAccessible@ must
    -- be enabled for it to be publicly accessible.
    --
    -- Changes to the @PubliclyAccessible@ parameter are applied immediately
    -- regardless of the value of the @ApplyImmediately@ parameter.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether minor version upgrades are applied
    -- automatically to the DB instance during the maintenance window. Changing
    -- this parameter doesn\'t result in an outage except in the following case
    -- and the change is asynchronously applied as soon as possible. An outage
    -- results if this parameter is enabled during the maintenance window, and
    -- a newer minor version is available, and RDS has enabled auto patching
    -- for that engine version.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The new DB subnet group for the DB instance. You can use this parameter
    -- to move your DB instance to a different VPC. If your DB instance isn\'t
    -- in a VPC, you can also use this parameter to move your DB instance into
    -- a VPC. For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html#USER_VPC.Non-VPC2VPC Working with a DB instance in a VPC>
    -- in the /Amazon RDS User Guide./
    --
    -- Changing the subnet group causes an outage during the change. The change
    -- is applied during the next maintenance window, unless you enable
    -- @ApplyImmediately@.
    --
    -- Constraints: If supplied, must match the name of an existing
    -- DBSubnetGroup.
    --
    -- Example: @mySubnetGroup@
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | The ARN for the IAM role that permits RDS to send enhanced monitoring
    -- metrics to Amazon CloudWatch Logs. For example,
    -- @arn:aws:iam:123456789012:role\/emaccess@. For information on creating a
    -- monitoring role, go to
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.html#USER_Monitoring.OS.IAMRole To create an IAM role for Amazon RDS Enhanced Monitoring>
    -- in the /Amazon RDS User Guide./
    --
    -- If @MonitoringInterval@ is set to a value other than 0, then you must
    -- supply a @MonitoringRoleArn@ value.
    monitoringRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The new Provisioned IOPS (I\/O operations per second) value for the RDS
    -- instance.
    --
    -- Changing this setting doesn\'t result in an outage and the change is
    -- applied during the next maintenance window unless the @ApplyImmediately@
    -- parameter is enabled for this request. If you are migrating from
    -- Provisioned IOPS to standard storage, set this value to 0. The DB
    -- instance will require a reboot for the change in storage type to take
    -- effect.
    --
    -- If you choose to migrate your DB instance from using standard storage to
    -- using Provisioned IOPS, or from using Provisioned IOPS to using standard
    -- storage, the process can take time. The duration of the migration
    -- depends on several factors such as database load, storage size, storage
    -- type (standard or Provisioned IOPS), amount of IOPS provisioned (if
    -- any), and the number of prior scale storage operations. Typical
    -- migration times are under 24 hours, but the process can take up to
    -- several days in some cases. During the migration, the DB instance is
    -- available for use, but might experience performance degradation. While
    -- the migration takes place, nightly backups for the instance are
    -- suspended. No other Amazon RDS operations can take place for the
    -- instance, including modifying the instance, rebooting the instance,
    -- deleting the instance, creating a read replica for the instance, and
    -- creating a DB snapshot of the instance.
    --
    -- Constraints: For MariaDB, MySQL, Oracle, and PostgreSQL, the value
    -- supplied must be at least 10% greater than the current value. Values
    -- that are not at least 10% greater than the existing value are rounded up
    -- so that they are 10% greater than the current value.
    --
    -- Default: Uses existing setting
    iops :: Prelude.Maybe Prelude.Int,
    -- | A value that indicates whether major version upgrades are allowed.
    -- Changing this parameter doesn\'t result in an outage and the change is
    -- asynchronously applied as soon as possible.
    --
    -- Constraints: Major version upgrades must be allowed when specifying a
    -- value for the EngineVersion parameter that is a different major version
    -- than the DB instance\'s current version.
    allowMajorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The new DB instance identifier for the DB instance when renaming a DB
    -- instance. When you change the DB instance identifier, an instance reboot
    -- occurs immediately if you enable @ApplyImmediately@, or will occur
    -- during the next maintenance window if you disable Apply Immediately.
    -- This value is stored as a lowercase string.
    --
    -- Constraints:
    --
    -- -   Must contain from 1 to 63 letters, numbers, or hyphens.
    --
    -- -   The first character must be a letter.
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens.
    --
    -- Example: @mydbinstance@
    newDBInstanceIdentifier' :: Prelude.Maybe Prelude.Text,
    -- | The Active Directory directory ID to move the DB instance to. Specify
    -- @none@ to remove the instance from its current domain. The domain must
    -- be created prior to this operation. Currently, only MySQL, Microsoft SQL
    -- Server, Oracle, and PostgreSQL DB instances can be created in an Active
    -- Directory Domain.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
    -- in the /Amazon RDS User Guide/.
    domain :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to enable a customer-owned IP address
    -- (CoIP) for an RDS on Outposts DB instance.
    --
    -- A /CoIP/ provides local or external connectivity to resources in your
    -- Outpost subnets through your on-premises network. For some use cases, a
    -- CoIP can provide lower latency for connections to the DB instance from
    -- outside of its virtual private cloud (VPC) on your local network.
    --
    -- For more information about RDS on Outposts, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Working with Amazon RDS on Amazon Web Services Outposts>
    -- in the /Amazon RDS User Guide/.
    --
    -- For more information about CoIPs, see
    -- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-networking-components.html#ip-addressing Customer-owned IP addresses>
    -- in the /Amazon Web Services Outposts User Guide/.
    enableCustomerOwnedIp :: Prelude.Maybe Prelude.Bool,
    -- | A value that sets the open mode of a replica database to either mounted
    -- or read-only.
    --
    -- Currently, this parameter is only supported for Oracle DB instances.
    --
    -- Mounted DB replicas are included in Oracle Enterprise Edition. The main
    -- use case for mounted replicas is cross-Region disaster recovery. The
    -- primary database doesn\'t use Active Data Guard to transmit information
    -- to the mounted replica. Because it doesn\'t accept user connections, a
    -- mounted replica can\'t serve a read-only workload. For more information,
    -- see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Working with Oracle Read Replicas for Amazon RDS>
    -- in the /Amazon RDS User Guide/.
    replicaMode :: Prelude.Maybe ReplicaMode,
    -- | The interval, in seconds, between points when Enhanced Monitoring
    -- metrics are collected for the DB instance. To disable collecting
    -- Enhanced Monitoring metrics, specify 0. The default is 0.
    --
    -- If @MonitoringRoleArn@ is specified, then you must also set
    -- @MonitoringInterval@ to a value other than 0.
    --
    -- Valid Values: @0, 1, 5, 10, 15, 30, 60@
    monitoringInterval :: Prelude.Maybe Prelude.Int,
    -- | The configuration setting for the log types to be enabled for export to
    -- CloudWatch Logs for a specific DB instance.
    --
    -- A change to the @CloudwatchLogsExportConfiguration@ parameter is always
    -- applied to the DB instance immediately. Therefore, the
    -- @ApplyImmediately@ parameter has no effect.
    cloudwatchLogsExportConfiguration :: Prelude.Maybe CloudwatchLogsExportConfiguration,
    -- | A value that indicates whether the DB instance is restarted when you
    -- rotate your SSL\/TLS certificate.
    --
    -- By default, the DB instance is restarted when you rotate your SSL\/TLS
    -- certificate. The certificate is not updated until the DB instance is
    -- restarted.
    --
    -- Set this parameter only if you are /not/ using SSL\/TLS to connect to
    -- the DB instance.
    --
    -- If you are using SSL\/TLS to connect to the DB instance, follow the
    -- appropriate instructions for your DB engine to rotate your SSL\/TLS
    -- certificate:
    --
    -- -   For more information about rotating your SSL\/TLS certificate for
    --     RDS DB engines, see
    --     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.SSL-certificate-rotation.html Rotating Your SSL\/TLS Certificate.>
    --     in the /Amazon RDS User Guide./
    --
    -- -   For more information about rotating your SSL\/TLS certificate for
    --     Aurora DB engines, see
    --     <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.SSL-certificate-rotation.html Rotating Your SSL\/TLS Certificate>
    --     in the /Amazon Aurora User Guide./
    certificateRotationRestart :: Prelude.Maybe Prelude.Bool,
    -- | The password for the given ARN from the key store in order to access the
    -- device.
    tdeCredentialPassword :: Prelude.Maybe Prelude.Text,
    -- | The number of CPU cores and the number of threads per core for the DB
    -- instance class of the DB instance.
    processorFeatures :: Prelude.Maybe [ProcessorFeature],
    -- | The new compute and memory capacity of the DB instance, for example,
    -- @db.m4.large@. Not all DB instance classes are available in all Amazon
    -- Web Services Regions, or for all database engines. For the full list of
    -- DB instance classes, and availability for your engine, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
    -- in the /Amazon RDS User Guide./
    --
    -- If you modify the DB instance class, an outage occurs during the change.
    -- The change is applied during the next maintenance window, unless
    -- @ApplyImmediately@ is enabled for this request.
    --
    -- Default: Uses existing setting
    dbInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | A value that specifies the order in which an Aurora Replica is promoted
    -- to the primary instance after a failure of the existing primary
    -- instance. For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster>
    -- in the /Amazon Aurora User Guide/.
    --
    -- Default: 1
    --
    -- Valid Values: 0 - 15
    promotionTier :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the recovery point in Amazon Web
    -- Services Backup.
    awsBackupRecoveryPointArn :: Prelude.Maybe Prelude.Text,
    -- | The license model for the DB instance.
    --
    -- Valid values: @license-included@ | @bring-your-own-license@ |
    -- @general-public-license@
    licenseModel :: Prelude.Maybe Prelude.Text,
    -- | The weekly time range (in UTC) during which system maintenance can
    -- occur, which might result in an outage. Changing this parameter doesn\'t
    -- result in an outage, except in the following situation, and the change
    -- is asynchronously applied as soon as possible. If there are pending
    -- actions that cause a reboot, and the maintenance window is changed to
    -- include the current time, then changing this parameter will cause a
    -- reboot of the DB instance. If moving this window to the current time,
    -- there must be at least 30 minutes between the current time and end of
    -- the window to ensure pending changes are applied.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Maintenance.html#Concepts.DBMaintenance Amazon RDS Maintenance Window>
    -- in the /Amazon RDS User Guide./
    --
    -- Default: Uses existing setting
    --
    -- Format: ddd:hh24:mi-ddd:hh24:mi
    --
    -- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
    --
    -- Constraints: Must be at least 30 minutes
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | The amount of time, in days, to retain Performance Insights data. Valid
    -- values are 7 or 731 (2 years).
    performanceInsightsRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | Indicates the certificate that needs to be associated with the instance.
    cACertificateIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The upper limit in gibibytes (GiB) to which Amazon RDS can automatically
    -- scale the storage of the DB instance.
    --
    -- For more information about this setting, including limitations that
    -- apply to it, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
    -- in the /Amazon RDS User Guide/.
    maxAllocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | A value that indicates whether to enable Performance Insights for the DB
    -- instance.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights>
    -- in the /Amazon Relational Database Service User Guide/.
    enablePerformanceInsights :: Prelude.Maybe Prelude.Bool,
    -- | The name of the DB parameter group to apply to the DB instance. Changing
    -- this setting doesn\'t result in an outage. The parameter group name
    -- itself is changed immediately, but the actual parameter changes are not
    -- applied until you reboot the instance without failover. In this case,
    -- the DB instance isn\'t rebooted automatically and the parameter changes
    -- isn\'t applied during the next maintenance window.
    --
    -- Default: Uses existing setting
    --
    -- Constraints: The DB parameter group must be in the same DB parameter
    -- group family as this DB instance.
    dbParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The daily time range during which automated backups are created if
    -- automated backups are enabled, as determined by the
    -- @BackupRetentionPeriod@ parameter. Changing this parameter doesn\'t
    -- result in an outage and the change is asynchronously applied as soon as
    -- possible. The default is a 30-minute window selected at random from an
    -- 8-hour block of time for each Amazon Web Services Region. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithAutomatedBackups.html#USER_WorkingWithAutomatedBackups.BackupWindow Backup window>
    -- in the /Amazon RDS User Guide./
    --
    -- __Amazon Aurora__
    --
    -- Not applicable. The daily time range for creating automated backups is
    -- managed by the DB cluster. For more information, see @ModifyDBCluster@.
    --
    -- Constraints:
    --
    -- -   Must be in the format hh24:mi-hh24:mi
    --
    -- -   Must be in Universal Time Coordinated (UTC)
    --
    -- -   Must not conflict with the preferred maintenance window
    --
    -- -   Must be at least 30 minutes
    preferredBackupWindow :: Prelude.Maybe Prelude.Text,
    -- | The number of days to retain automated backups. Setting this parameter
    -- to a positive number enables backups. Setting this parameter to 0
    -- disables automated backups.
    --
    -- Enabling and disabling backups can result in a brief I\/O suspension
    -- that lasts from a few seconds to a few minutes, depending on the size
    -- and class of your DB instance.
    --
    -- These changes are applied during the next maintenance window unless the
    -- @ApplyImmediately@ parameter is enabled for this request. If you change
    -- the parameter from one non-zero value to another non-zero value, the
    -- change is asynchronously applied as soon as possible.
    --
    -- __Amazon Aurora__
    --
    -- Not applicable. The retention period for automated backups is managed by
    -- the DB cluster. For more information, see @ModifyDBCluster@.
    --
    -- Default: Uses existing setting
    --
    -- Constraints:
    --
    -- -   Must be a value from 0 to 35
    --
    -- -   Can be specified for a MySQL read replica only if the source is
    --     running MySQL 5.6 or later
    --
    -- -   Can be specified for a PostgreSQL read replica only if the source is
    --     running PostgreSQL 9.3.5
    --
    -- -   Can\'t be set to 0 if the DB instance is a source to read replicas
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
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
    -- | A list of EC2 VPC security groups to authorize on this DB instance. This
    -- change is asynchronously applied as soon as possible.
    --
    -- __Amazon Aurora__
    --
    -- Not applicable. The associated list of EC2 VPC security groups is
    -- managed by the DB cluster. For more information, see @ModifyDBCluster@.
    --
    -- Constraints:
    --
    -- -   If supplied, must match existing VpcSecurityGroupIds.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | A value that indicates whether the DB instance is a Multi-AZ deployment.
    -- Changing this parameter doesn\'t result in an outage and the change is
    -- applied during the next maintenance window unless the @ApplyImmediately@
    -- parameter is enabled for this request.
    multiAZ :: Prelude.Maybe Prelude.Bool,
    -- | The new amount of storage in gibibytes (GiB) to allocate for the DB
    -- instance.
    --
    -- For MariaDB, MySQL, Oracle, and PostgreSQL, the value supplied must be
    -- at least 10% greater than the current value. Values that are not at
    -- least 10% greater than the existing value are rounded up so that they
    -- are 10% greater than the current value.
    --
    -- For the valid values for allocated storage for each engine, see
    -- @CreateDBInstance@.
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | A value that indicates whether the modifications in this request and any
    -- pending modifications are asynchronously applied as soon as possible,
    -- regardless of the @PreferredMaintenanceWindow@ setting for the DB
    -- instance. By default, this parameter is disabled.
    --
    -- If this parameter is disabled, changes to the DB instance are applied
    -- during the next maintenance window. Some parameter changes can cause an
    -- outage and are applied on the next call to RebootDBInstance, or the next
    -- failure reboot. Review the table of parameters in
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.DBInstance.Modifying.html Modifying a DB Instance>
    -- in the /Amazon RDS User Guide./ to see the impact of enabling or
    -- disabling @ApplyImmediately@ for each modified parameter and to
    -- determine when the changes are applied.
    applyImmediately :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates the DB instance should be associated with the
    -- specified option group. Changing this parameter doesn\'t result in an
    -- outage except in the following case and the change is applied during the
    -- next maintenance window unless the @ApplyImmediately@ parameter is
    -- enabled for this request. If the parameter change results in an option
    -- group that enables OEM, this change can cause a brief (sub-second)
    -- period during which new connections are rejected but existing
    -- connections are not interrupted.
    --
    -- Permanent options, such as the TDE option for Oracle Advanced Security
    -- TDE, can\'t be removed from an option group, and that option group
    -- can\'t be removed from a DB instance once it is associated with a DB
    -- instance
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to copy all tags from the DB instance to
    -- snapshots of the DB instance. By default, tags are not copied.
    --
    -- __Amazon Aurora__
    --
    -- Not applicable. Copying tags to snapshots is managed by the DB cluster.
    -- Setting this value for an Aurora DB instance has no effect on the DB
    -- cluster setting. For more information, see @ModifyDBCluster@.
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | The ARN from the key store with which to associate the instance for TDE
    -- encryption.
    tdeCredentialArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the IAM role to use when making API calls to the Directory
    -- Service.
    domainIAMRoleName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to enable mapping of Amazon Web Services
    -- Identity and Access Management (IAM) accounts to database accounts. By
    -- default, mapping is disabled.
    --
    -- This setting doesn\'t apply to Amazon Aurora. Mapping Amazon Web
    -- Services IAM accounts to database accounts is managed by the DB cluster.
    --
    -- For more information about IAM database authentication, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
    -- in the /Amazon RDS User Guide./
    enableIAMDatabaseAuthentication :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether the DB instance class of the DB instance
    -- uses its default processor features.
    useDefaultProcessorFeatures :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the storage type to be associated with the DB instance.
    --
    -- If you specify Provisioned IOPS (@io1@), you must also include a value
    -- for the @Iops@ parameter.
    --
    -- If you choose to migrate your DB instance from using standard storage to
    -- using Provisioned IOPS, or from using Provisioned IOPS to using standard
    -- storage, the process can take time. The duration of the migration
    -- depends on several factors such as database load, storage size, storage
    -- type (standard or Provisioned IOPS), amount of IOPS provisioned (if
    -- any), and the number of prior scale storage operations. Typical
    -- migration times are under 24 hours, but the process can take up to
    -- several days in some cases. During the migration, the DB instance is
    -- available for use, but might experience performance degradation. While
    -- the migration takes place, nightly backups for the instance are
    -- suspended. No other Amazon RDS operations can take place for the
    -- instance, including modifying the instance, rebooting the instance,
    -- deleting the instance, creating a read replica for the instance, and
    -- creating a DB snapshot of the instance.
    --
    -- Valid values: @standard | gp2 | io1@
    --
    -- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
    storageType :: Prelude.Maybe Prelude.Text,
    -- | The DB instance identifier. This value is stored as a lowercase string.
    --
    -- Constraints:
    --
    -- -   Must match the identifier of an existing DBInstance.
    dbInstanceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyDBInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineVersion', 'modifyDBInstance_engineVersion' - The version number of the database engine to upgrade to. Changing this
-- parameter results in an outage and the change is applied during the next
-- maintenance window unless the @ApplyImmediately@ parameter is enabled
-- for this request.
--
-- For major version upgrades, if a nondefault DB parameter group is
-- currently in use, a new DB parameter group in the DB parameter group
-- family for the new engine version must be specified. The new DB
-- parameter group can be the default for that DB parameter group family.
--
-- If you specify only a major version, Amazon RDS will update the DB
-- instance to the default minor version if the current minor version is
-- lower. For information about valid engine versions, see
-- @CreateDBInstance@, or call @DescribeDBEngineVersions@.
--
-- 'dbSecurityGroups', 'modifyDBInstance_dbSecurityGroups' - A list of DB security groups to authorize on this DB instance. Changing
-- this setting doesn\'t result in an outage and the change is
-- asynchronously applied as soon as possible.
--
-- Constraints:
--
-- -   If supplied, must match existing DBSecurityGroups.
--
-- 'dbPortNumber', 'modifyDBInstance_dbPortNumber' - The port number on which the database accepts connections.
--
-- The value of the @DBPortNumber@ parameter must not match any of the port
-- values specified for options in the option group for the DB instance.
--
-- Your database will restart when you change the @DBPortNumber@ value
-- regardless of the value of the @ApplyImmediately@ parameter.
--
-- __MySQL__
--
-- Default: @3306@
--
-- Valid values: @1150-65535@
--
-- __MariaDB__
--
-- Default: @3306@
--
-- Valid values: @1150-65535@
--
-- __PostgreSQL__
--
-- Default: @5432@
--
-- Valid values: @1150-65535@
--
-- Type: Integer
--
-- __Oracle__
--
-- Default: @1521@
--
-- Valid values: @1150-65535@
--
-- __SQL Server__
--
-- Default: @1433@
--
-- Valid values: @1150-65535@ except @1234@, @1434@, @3260@, @3343@,
-- @3389@, @47001@, and @49152-49156@.
--
-- __Amazon Aurora__
--
-- Default: @3306@
--
-- Valid values: @1150-65535@
--
-- 'deletionProtection', 'modifyDBInstance_deletionProtection' - A value that indicates whether the DB instance has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
--
-- 'masterUserPassword', 'modifyDBInstance_masterUserPassword' - The new password for the master user. The password can include any
-- printable ASCII character except \"\/\", \"\"\", or \"\@\".
--
-- Changing this parameter doesn\'t result in an outage and the change is
-- asynchronously applied as soon as possible. Between the time of the
-- request and the completion of the request, the @MasterUserPassword@
-- element exists in the @PendingModifiedValues@ element of the operation
-- response.
--
-- __Amazon Aurora__
--
-- Not applicable. The password for the master user is managed by the DB
-- cluster. For more information, see @ModifyDBCluster@.
--
-- Default: Uses existing setting
--
-- __MariaDB__
--
-- Constraints: Must contain from 8 to 41 characters.
--
-- __Microsoft SQL Server__
--
-- Constraints: Must contain from 8 to 128 characters.
--
-- __MySQL__
--
-- Constraints: Must contain from 8 to 41 characters.
--
-- __Oracle__
--
-- Constraints: Must contain from 8 to 30 characters.
--
-- __PostgreSQL__
--
-- Constraints: Must contain from 8 to 128 characters.
--
-- Amazon RDS API actions never return the password, so this action
-- provides a way to regain access to a primary instance user if the
-- password is lost. This includes restoring privileges that might have
-- been accidentally revoked.
--
-- 'publiclyAccessible', 'modifyDBInstance_publiclyAccessible' - A value that indicates whether the DB instance is publicly accessible.
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
-- @PubliclyAccessible@ only applies to DB instances in a VPC. The DB
-- instance must be part of a public subnet and @PubliclyAccessible@ must
-- be enabled for it to be publicly accessible.
--
-- Changes to the @PubliclyAccessible@ parameter are applied immediately
-- regardless of the value of the @ApplyImmediately@ parameter.
--
-- 'autoMinorVersionUpgrade', 'modifyDBInstance_autoMinorVersionUpgrade' - A value that indicates whether minor version upgrades are applied
-- automatically to the DB instance during the maintenance window. Changing
-- this parameter doesn\'t result in an outage except in the following case
-- and the change is asynchronously applied as soon as possible. An outage
-- results if this parameter is enabled during the maintenance window, and
-- a newer minor version is available, and RDS has enabled auto patching
-- for that engine version.
--
-- 'dbSubnetGroupName', 'modifyDBInstance_dbSubnetGroupName' - The new DB subnet group for the DB instance. You can use this parameter
-- to move your DB instance to a different VPC. If your DB instance isn\'t
-- in a VPC, you can also use this parameter to move your DB instance into
-- a VPC. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html#USER_VPC.Non-VPC2VPC Working with a DB instance in a VPC>
-- in the /Amazon RDS User Guide./
--
-- Changing the subnet group causes an outage during the change. The change
-- is applied during the next maintenance window, unless you enable
-- @ApplyImmediately@.
--
-- Constraints: If supplied, must match the name of an existing
-- DBSubnetGroup.
--
-- Example: @mySubnetGroup@
--
-- 'monitoringRoleArn', 'modifyDBInstance_monitoringRoleArn' - The ARN for the IAM role that permits RDS to send enhanced monitoring
-- metrics to Amazon CloudWatch Logs. For example,
-- @arn:aws:iam:123456789012:role\/emaccess@. For information on creating a
-- monitoring role, go to
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.html#USER_Monitoring.OS.IAMRole To create an IAM role for Amazon RDS Enhanced Monitoring>
-- in the /Amazon RDS User Guide./
--
-- If @MonitoringInterval@ is set to a value other than 0, then you must
-- supply a @MonitoringRoleArn@ value.
--
-- 'iops', 'modifyDBInstance_iops' - The new Provisioned IOPS (I\/O operations per second) value for the RDS
-- instance.
--
-- Changing this setting doesn\'t result in an outage and the change is
-- applied during the next maintenance window unless the @ApplyImmediately@
-- parameter is enabled for this request. If you are migrating from
-- Provisioned IOPS to standard storage, set this value to 0. The DB
-- instance will require a reboot for the change in storage type to take
-- effect.
--
-- If you choose to migrate your DB instance from using standard storage to
-- using Provisioned IOPS, or from using Provisioned IOPS to using standard
-- storage, the process can take time. The duration of the migration
-- depends on several factors such as database load, storage size, storage
-- type (standard or Provisioned IOPS), amount of IOPS provisioned (if
-- any), and the number of prior scale storage operations. Typical
-- migration times are under 24 hours, but the process can take up to
-- several days in some cases. During the migration, the DB instance is
-- available for use, but might experience performance degradation. While
-- the migration takes place, nightly backups for the instance are
-- suspended. No other Amazon RDS operations can take place for the
-- instance, including modifying the instance, rebooting the instance,
-- deleting the instance, creating a read replica for the instance, and
-- creating a DB snapshot of the instance.
--
-- Constraints: For MariaDB, MySQL, Oracle, and PostgreSQL, the value
-- supplied must be at least 10% greater than the current value. Values
-- that are not at least 10% greater than the existing value are rounded up
-- so that they are 10% greater than the current value.
--
-- Default: Uses existing setting
--
-- 'allowMajorVersionUpgrade', 'modifyDBInstance_allowMajorVersionUpgrade' - A value that indicates whether major version upgrades are allowed.
-- Changing this parameter doesn\'t result in an outage and the change is
-- asynchronously applied as soon as possible.
--
-- Constraints: Major version upgrades must be allowed when specifying a
-- value for the EngineVersion parameter that is a different major version
-- than the DB instance\'s current version.
--
-- 'newDBInstanceIdentifier'', 'modifyDBInstance_newDBInstanceIdentifier' - The new DB instance identifier for the DB instance when renaming a DB
-- instance. When you change the DB instance identifier, an instance reboot
-- occurs immediately if you enable @ApplyImmediately@, or will occur
-- during the next maintenance window if you disable Apply Immediately.
-- This value is stored as a lowercase string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens.
--
-- -   The first character must be a letter.
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens.
--
-- Example: @mydbinstance@
--
-- 'domain', 'modifyDBInstance_domain' - The Active Directory directory ID to move the DB instance to. Specify
-- @none@ to remove the instance from its current domain. The domain must
-- be created prior to this operation. Currently, only MySQL, Microsoft SQL
-- Server, Oracle, and PostgreSQL DB instances can be created in an Active
-- Directory Domain.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon RDS User Guide/.
--
-- 'enableCustomerOwnedIp', 'modifyDBInstance_enableCustomerOwnedIp' - A value that indicates whether to enable a customer-owned IP address
-- (CoIP) for an RDS on Outposts DB instance.
--
-- A /CoIP/ provides local or external connectivity to resources in your
-- Outpost subnets through your on-premises network. For some use cases, a
-- CoIP can provide lower latency for connections to the DB instance from
-- outside of its virtual private cloud (VPC) on your local network.
--
-- For more information about RDS on Outposts, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Working with Amazon RDS on Amazon Web Services Outposts>
-- in the /Amazon RDS User Guide/.
--
-- For more information about CoIPs, see
-- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-networking-components.html#ip-addressing Customer-owned IP addresses>
-- in the /Amazon Web Services Outposts User Guide/.
--
-- 'replicaMode', 'modifyDBInstance_replicaMode' - A value that sets the open mode of a replica database to either mounted
-- or read-only.
--
-- Currently, this parameter is only supported for Oracle DB instances.
--
-- Mounted DB replicas are included in Oracle Enterprise Edition. The main
-- use case for mounted replicas is cross-Region disaster recovery. The
-- primary database doesn\'t use Active Data Guard to transmit information
-- to the mounted replica. Because it doesn\'t accept user connections, a
-- mounted replica can\'t serve a read-only workload. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Working with Oracle Read Replicas for Amazon RDS>
-- in the /Amazon RDS User Guide/.
--
-- 'monitoringInterval', 'modifyDBInstance_monitoringInterval' - The interval, in seconds, between points when Enhanced Monitoring
-- metrics are collected for the DB instance. To disable collecting
-- Enhanced Monitoring metrics, specify 0. The default is 0.
--
-- If @MonitoringRoleArn@ is specified, then you must also set
-- @MonitoringInterval@ to a value other than 0.
--
-- Valid Values: @0, 1, 5, 10, 15, 30, 60@
--
-- 'cloudwatchLogsExportConfiguration', 'modifyDBInstance_cloudwatchLogsExportConfiguration' - The configuration setting for the log types to be enabled for export to
-- CloudWatch Logs for a specific DB instance.
--
-- A change to the @CloudwatchLogsExportConfiguration@ parameter is always
-- applied to the DB instance immediately. Therefore, the
-- @ApplyImmediately@ parameter has no effect.
--
-- 'certificateRotationRestart', 'modifyDBInstance_certificateRotationRestart' - A value that indicates whether the DB instance is restarted when you
-- rotate your SSL\/TLS certificate.
--
-- By default, the DB instance is restarted when you rotate your SSL\/TLS
-- certificate. The certificate is not updated until the DB instance is
-- restarted.
--
-- Set this parameter only if you are /not/ using SSL\/TLS to connect to
-- the DB instance.
--
-- If you are using SSL\/TLS to connect to the DB instance, follow the
-- appropriate instructions for your DB engine to rotate your SSL\/TLS
-- certificate:
--
-- -   For more information about rotating your SSL\/TLS certificate for
--     RDS DB engines, see
--     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.SSL-certificate-rotation.html Rotating Your SSL\/TLS Certificate.>
--     in the /Amazon RDS User Guide./
--
-- -   For more information about rotating your SSL\/TLS certificate for
--     Aurora DB engines, see
--     <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.SSL-certificate-rotation.html Rotating Your SSL\/TLS Certificate>
--     in the /Amazon Aurora User Guide./
--
-- 'tdeCredentialPassword', 'modifyDBInstance_tdeCredentialPassword' - The password for the given ARN from the key store in order to access the
-- device.
--
-- 'processorFeatures', 'modifyDBInstance_processorFeatures' - The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
--
-- 'dbInstanceClass', 'modifyDBInstance_dbInstanceClass' - The new compute and memory capacity of the DB instance, for example,
-- @db.m4.large@. Not all DB instance classes are available in all Amazon
-- Web Services Regions, or for all database engines. For the full list of
-- DB instance classes, and availability for your engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
-- in the /Amazon RDS User Guide./
--
-- If you modify the DB instance class, an outage occurs during the change.
-- The change is applied during the next maintenance window, unless
-- @ApplyImmediately@ is enabled for this request.
--
-- Default: Uses existing setting
--
-- 'promotionTier', 'modifyDBInstance_promotionTier' - A value that specifies the order in which an Aurora Replica is promoted
-- to the primary instance after a failure of the existing primary
-- instance. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster>
-- in the /Amazon Aurora User Guide/.
--
-- Default: 1
--
-- Valid Values: 0 - 15
--
-- 'awsBackupRecoveryPointArn', 'modifyDBInstance_awsBackupRecoveryPointArn' - The Amazon Resource Name (ARN) of the recovery point in Amazon Web
-- Services Backup.
--
-- 'licenseModel', 'modifyDBInstance_licenseModel' - The license model for the DB instance.
--
-- Valid values: @license-included@ | @bring-your-own-license@ |
-- @general-public-license@
--
-- 'preferredMaintenanceWindow', 'modifyDBInstance_preferredMaintenanceWindow' - The weekly time range (in UTC) during which system maintenance can
-- occur, which might result in an outage. Changing this parameter doesn\'t
-- result in an outage, except in the following situation, and the change
-- is asynchronously applied as soon as possible. If there are pending
-- actions that cause a reboot, and the maintenance window is changed to
-- include the current time, then changing this parameter will cause a
-- reboot of the DB instance. If moving this window to the current time,
-- there must be at least 30 minutes between the current time and end of
-- the window to ensure pending changes are applied.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Maintenance.html#Concepts.DBMaintenance Amazon RDS Maintenance Window>
-- in the /Amazon RDS User Guide./
--
-- Default: Uses existing setting
--
-- Format: ddd:hh24:mi-ddd:hh24:mi
--
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
--
-- Constraints: Must be at least 30 minutes
--
-- 'performanceInsightsRetentionPeriod', 'modifyDBInstance_performanceInsightsRetentionPeriod' - The amount of time, in days, to retain Performance Insights data. Valid
-- values are 7 or 731 (2 years).
--
-- 'cACertificateIdentifier', 'modifyDBInstance_cACertificateIdentifier' - Indicates the certificate that needs to be associated with the instance.
--
-- 'maxAllocatedStorage', 'modifyDBInstance_maxAllocatedStorage' - The upper limit in gibibytes (GiB) to which Amazon RDS can automatically
-- scale the storage of the DB instance.
--
-- For more information about this setting, including limitations that
-- apply to it, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
-- in the /Amazon RDS User Guide/.
--
-- 'enablePerformanceInsights', 'modifyDBInstance_enablePerformanceInsights' - A value that indicates whether to enable Performance Insights for the DB
-- instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights>
-- in the /Amazon Relational Database Service User Guide/.
--
-- 'dbParameterGroupName', 'modifyDBInstance_dbParameterGroupName' - The name of the DB parameter group to apply to the DB instance. Changing
-- this setting doesn\'t result in an outage. The parameter group name
-- itself is changed immediately, but the actual parameter changes are not
-- applied until you reboot the instance without failover. In this case,
-- the DB instance isn\'t rebooted automatically and the parameter changes
-- isn\'t applied during the next maintenance window.
--
-- Default: Uses existing setting
--
-- Constraints: The DB parameter group must be in the same DB parameter
-- group family as this DB instance.
--
-- 'preferredBackupWindow', 'modifyDBInstance_preferredBackupWindow' - The daily time range during which automated backups are created if
-- automated backups are enabled, as determined by the
-- @BackupRetentionPeriod@ parameter. Changing this parameter doesn\'t
-- result in an outage and the change is asynchronously applied as soon as
-- possible. The default is a 30-minute window selected at random from an
-- 8-hour block of time for each Amazon Web Services Region. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithAutomatedBackups.html#USER_WorkingWithAutomatedBackups.BackupWindow Backup window>
-- in the /Amazon RDS User Guide./
--
-- __Amazon Aurora__
--
-- Not applicable. The daily time range for creating automated backups is
-- managed by the DB cluster. For more information, see @ModifyDBCluster@.
--
-- Constraints:
--
-- -   Must be in the format hh24:mi-hh24:mi
--
-- -   Must be in Universal Time Coordinated (UTC)
--
-- -   Must not conflict with the preferred maintenance window
--
-- -   Must be at least 30 minutes
--
-- 'backupRetentionPeriod', 'modifyDBInstance_backupRetentionPeriod' - The number of days to retain automated backups. Setting this parameter
-- to a positive number enables backups. Setting this parameter to 0
-- disables automated backups.
--
-- Enabling and disabling backups can result in a brief I\/O suspension
-- that lasts from a few seconds to a few minutes, depending on the size
-- and class of your DB instance.
--
-- These changes are applied during the next maintenance window unless the
-- @ApplyImmediately@ parameter is enabled for this request. If you change
-- the parameter from one non-zero value to another non-zero value, the
-- change is asynchronously applied as soon as possible.
--
-- __Amazon Aurora__
--
-- Not applicable. The retention period for automated backups is managed by
-- the DB cluster. For more information, see @ModifyDBCluster@.
--
-- Default: Uses existing setting
--
-- Constraints:
--
-- -   Must be a value from 0 to 35
--
-- -   Can be specified for a MySQL read replica only if the source is
--     running MySQL 5.6 or later
--
-- -   Can be specified for a PostgreSQL read replica only if the source is
--     running PostgreSQL 9.3.5
--
-- -   Can\'t be set to 0 if the DB instance is a source to read replicas
--
-- 'performanceInsightsKMSKeyId', 'modifyDBInstance_performanceInsightsKMSKeyId' - The Amazon Web Services KMS key identifier for encryption of Performance
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
-- 'vpcSecurityGroupIds', 'modifyDBInstance_vpcSecurityGroupIds' - A list of EC2 VPC security groups to authorize on this DB instance. This
-- change is asynchronously applied as soon as possible.
--
-- __Amazon Aurora__
--
-- Not applicable. The associated list of EC2 VPC security groups is
-- managed by the DB cluster. For more information, see @ModifyDBCluster@.
--
-- Constraints:
--
-- -   If supplied, must match existing VpcSecurityGroupIds.
--
-- 'multiAZ', 'modifyDBInstance_multiAZ' - A value that indicates whether the DB instance is a Multi-AZ deployment.
-- Changing this parameter doesn\'t result in an outage and the change is
-- applied during the next maintenance window unless the @ApplyImmediately@
-- parameter is enabled for this request.
--
-- 'allocatedStorage', 'modifyDBInstance_allocatedStorage' - The new amount of storage in gibibytes (GiB) to allocate for the DB
-- instance.
--
-- For MariaDB, MySQL, Oracle, and PostgreSQL, the value supplied must be
-- at least 10% greater than the current value. Values that are not at
-- least 10% greater than the existing value are rounded up so that they
-- are 10% greater than the current value.
--
-- For the valid values for allocated storage for each engine, see
-- @CreateDBInstance@.
--
-- 'applyImmediately', 'modifyDBInstance_applyImmediately' - A value that indicates whether the modifications in this request and any
-- pending modifications are asynchronously applied as soon as possible,
-- regardless of the @PreferredMaintenanceWindow@ setting for the DB
-- instance. By default, this parameter is disabled.
--
-- If this parameter is disabled, changes to the DB instance are applied
-- during the next maintenance window. Some parameter changes can cause an
-- outage and are applied on the next call to RebootDBInstance, or the next
-- failure reboot. Review the table of parameters in
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.DBInstance.Modifying.html Modifying a DB Instance>
-- in the /Amazon RDS User Guide./ to see the impact of enabling or
-- disabling @ApplyImmediately@ for each modified parameter and to
-- determine when the changes are applied.
--
-- 'optionGroupName', 'modifyDBInstance_optionGroupName' - A value that indicates the DB instance should be associated with the
-- specified option group. Changing this parameter doesn\'t result in an
-- outage except in the following case and the change is applied during the
-- next maintenance window unless the @ApplyImmediately@ parameter is
-- enabled for this request. If the parameter change results in an option
-- group that enables OEM, this change can cause a brief (sub-second)
-- period during which new connections are rejected but existing
-- connections are not interrupted.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security
-- TDE, can\'t be removed from an option group, and that option group
-- can\'t be removed from a DB instance once it is associated with a DB
-- instance
--
-- 'copyTagsToSnapshot', 'modifyDBInstance_copyTagsToSnapshot' - A value that indicates whether to copy all tags from the DB instance to
-- snapshots of the DB instance. By default, tags are not copied.
--
-- __Amazon Aurora__
--
-- Not applicable. Copying tags to snapshots is managed by the DB cluster.
-- Setting this value for an Aurora DB instance has no effect on the DB
-- cluster setting. For more information, see @ModifyDBCluster@.
--
-- 'tdeCredentialArn', 'modifyDBInstance_tdeCredentialArn' - The ARN from the key store with which to associate the instance for TDE
-- encryption.
--
-- 'domainIAMRoleName', 'modifyDBInstance_domainIAMRoleName' - The name of the IAM role to use when making API calls to the Directory
-- Service.
--
-- 'enableIAMDatabaseAuthentication', 'modifyDBInstance_enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts. By
-- default, mapping is disabled.
--
-- This setting doesn\'t apply to Amazon Aurora. Mapping Amazon Web
-- Services IAM accounts to database accounts is managed by the DB cluster.
--
-- For more information about IAM database authentication, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
-- in the /Amazon RDS User Guide./
--
-- 'useDefaultProcessorFeatures', 'modifyDBInstance_useDefaultProcessorFeatures' - A value that indicates whether the DB instance class of the DB instance
-- uses its default processor features.
--
-- 'storageType', 'modifyDBInstance_storageType' - Specifies the storage type to be associated with the DB instance.
--
-- If you specify Provisioned IOPS (@io1@), you must also include a value
-- for the @Iops@ parameter.
--
-- If you choose to migrate your DB instance from using standard storage to
-- using Provisioned IOPS, or from using Provisioned IOPS to using standard
-- storage, the process can take time. The duration of the migration
-- depends on several factors such as database load, storage size, storage
-- type (standard or Provisioned IOPS), amount of IOPS provisioned (if
-- any), and the number of prior scale storage operations. Typical
-- migration times are under 24 hours, but the process can take up to
-- several days in some cases. During the migration, the DB instance is
-- available for use, but might experience performance degradation. While
-- the migration takes place, nightly backups for the instance are
-- suspended. No other Amazon RDS operations can take place for the
-- instance, including modifying the instance, rebooting the instance,
-- deleting the instance, creating a read replica for the instance, and
-- creating a DB snapshot of the instance.
--
-- Valid values: @standard | gp2 | io1@
--
-- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
--
-- 'dbInstanceIdentifier', 'modifyDBInstance_dbInstanceIdentifier' - The DB instance identifier. This value is stored as a lowercase string.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DBInstance.
newModifyDBInstance ::
  -- | 'dbInstanceIdentifier'
  Prelude.Text ->
  ModifyDBInstance
newModifyDBInstance pDBInstanceIdentifier_ =
  ModifyDBInstance'
    { engineVersion = Prelude.Nothing,
      dbSecurityGroups = Prelude.Nothing,
      dbPortNumber = Prelude.Nothing,
      deletionProtection = Prelude.Nothing,
      masterUserPassword = Prelude.Nothing,
      publiclyAccessible = Prelude.Nothing,
      autoMinorVersionUpgrade = Prelude.Nothing,
      dbSubnetGroupName = Prelude.Nothing,
      monitoringRoleArn = Prelude.Nothing,
      iops = Prelude.Nothing,
      allowMajorVersionUpgrade = Prelude.Nothing,
      newDBInstanceIdentifier' = Prelude.Nothing,
      domain = Prelude.Nothing,
      enableCustomerOwnedIp = Prelude.Nothing,
      replicaMode = Prelude.Nothing,
      monitoringInterval = Prelude.Nothing,
      cloudwatchLogsExportConfiguration = Prelude.Nothing,
      certificateRotationRestart = Prelude.Nothing,
      tdeCredentialPassword = Prelude.Nothing,
      processorFeatures = Prelude.Nothing,
      dbInstanceClass = Prelude.Nothing,
      promotionTier = Prelude.Nothing,
      awsBackupRecoveryPointArn = Prelude.Nothing,
      licenseModel = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      performanceInsightsRetentionPeriod = Prelude.Nothing,
      cACertificateIdentifier = Prelude.Nothing,
      maxAllocatedStorage = Prelude.Nothing,
      enablePerformanceInsights = Prelude.Nothing,
      dbParameterGroupName = Prelude.Nothing,
      preferredBackupWindow = Prelude.Nothing,
      backupRetentionPeriod = Prelude.Nothing,
      performanceInsightsKMSKeyId = Prelude.Nothing,
      vpcSecurityGroupIds = Prelude.Nothing,
      multiAZ = Prelude.Nothing,
      allocatedStorage = Prelude.Nothing,
      applyImmediately = Prelude.Nothing,
      optionGroupName = Prelude.Nothing,
      copyTagsToSnapshot = Prelude.Nothing,
      tdeCredentialArn = Prelude.Nothing,
      domainIAMRoleName = Prelude.Nothing,
      enableIAMDatabaseAuthentication = Prelude.Nothing,
      useDefaultProcessorFeatures = Prelude.Nothing,
      storageType = Prelude.Nothing,
      dbInstanceIdentifier = pDBInstanceIdentifier_
    }

-- | The version number of the database engine to upgrade to. Changing this
-- parameter results in an outage and the change is applied during the next
-- maintenance window unless the @ApplyImmediately@ parameter is enabled
-- for this request.
--
-- For major version upgrades, if a nondefault DB parameter group is
-- currently in use, a new DB parameter group in the DB parameter group
-- family for the new engine version must be specified. The new DB
-- parameter group can be the default for that DB parameter group family.
--
-- If you specify only a major version, Amazon RDS will update the DB
-- instance to the default minor version if the current minor version is
-- lower. For information about valid engine versions, see
-- @CreateDBInstance@, or call @DescribeDBEngineVersions@.
modifyDBInstance_engineVersion :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_engineVersion = Lens.lens (\ModifyDBInstance' {engineVersion} -> engineVersion) (\s@ModifyDBInstance' {} a -> s {engineVersion = a} :: ModifyDBInstance)

-- | A list of DB security groups to authorize on this DB instance. Changing
-- this setting doesn\'t result in an outage and the change is
-- asynchronously applied as soon as possible.
--
-- Constraints:
--
-- -   If supplied, must match existing DBSecurityGroups.
modifyDBInstance_dbSecurityGroups :: Lens.Lens' ModifyDBInstance (Prelude.Maybe [Prelude.Text])
modifyDBInstance_dbSecurityGroups = Lens.lens (\ModifyDBInstance' {dbSecurityGroups} -> dbSecurityGroups) (\s@ModifyDBInstance' {} a -> s {dbSecurityGroups = a} :: ModifyDBInstance) Prelude.. Lens.mapping Lens.coerced

-- | The port number on which the database accepts connections.
--
-- The value of the @DBPortNumber@ parameter must not match any of the port
-- values specified for options in the option group for the DB instance.
--
-- Your database will restart when you change the @DBPortNumber@ value
-- regardless of the value of the @ApplyImmediately@ parameter.
--
-- __MySQL__
--
-- Default: @3306@
--
-- Valid values: @1150-65535@
--
-- __MariaDB__
--
-- Default: @3306@
--
-- Valid values: @1150-65535@
--
-- __PostgreSQL__
--
-- Default: @5432@
--
-- Valid values: @1150-65535@
--
-- Type: Integer
--
-- __Oracle__
--
-- Default: @1521@
--
-- Valid values: @1150-65535@
--
-- __SQL Server__
--
-- Default: @1433@
--
-- Valid values: @1150-65535@ except @1234@, @1434@, @3260@, @3343@,
-- @3389@, @47001@, and @49152-49156@.
--
-- __Amazon Aurora__
--
-- Default: @3306@
--
-- Valid values: @1150-65535@
modifyDBInstance_dbPortNumber :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Int)
modifyDBInstance_dbPortNumber = Lens.lens (\ModifyDBInstance' {dbPortNumber} -> dbPortNumber) (\s@ModifyDBInstance' {} a -> s {dbPortNumber = a} :: ModifyDBInstance)

-- | A value that indicates whether the DB instance has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
modifyDBInstance_deletionProtection :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Bool)
modifyDBInstance_deletionProtection = Lens.lens (\ModifyDBInstance' {deletionProtection} -> deletionProtection) (\s@ModifyDBInstance' {} a -> s {deletionProtection = a} :: ModifyDBInstance)

-- | The new password for the master user. The password can include any
-- printable ASCII character except \"\/\", \"\"\", or \"\@\".
--
-- Changing this parameter doesn\'t result in an outage and the change is
-- asynchronously applied as soon as possible. Between the time of the
-- request and the completion of the request, the @MasterUserPassword@
-- element exists in the @PendingModifiedValues@ element of the operation
-- response.
--
-- __Amazon Aurora__
--
-- Not applicable. The password for the master user is managed by the DB
-- cluster. For more information, see @ModifyDBCluster@.
--
-- Default: Uses existing setting
--
-- __MariaDB__
--
-- Constraints: Must contain from 8 to 41 characters.
--
-- __Microsoft SQL Server__
--
-- Constraints: Must contain from 8 to 128 characters.
--
-- __MySQL__
--
-- Constraints: Must contain from 8 to 41 characters.
--
-- __Oracle__
--
-- Constraints: Must contain from 8 to 30 characters.
--
-- __PostgreSQL__
--
-- Constraints: Must contain from 8 to 128 characters.
--
-- Amazon RDS API actions never return the password, so this action
-- provides a way to regain access to a primary instance user if the
-- password is lost. This includes restoring privileges that might have
-- been accidentally revoked.
modifyDBInstance_masterUserPassword :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_masterUserPassword = Lens.lens (\ModifyDBInstance' {masterUserPassword} -> masterUserPassword) (\s@ModifyDBInstance' {} a -> s {masterUserPassword = a} :: ModifyDBInstance)

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
-- @PubliclyAccessible@ only applies to DB instances in a VPC. The DB
-- instance must be part of a public subnet and @PubliclyAccessible@ must
-- be enabled for it to be publicly accessible.
--
-- Changes to the @PubliclyAccessible@ parameter are applied immediately
-- regardless of the value of the @ApplyImmediately@ parameter.
modifyDBInstance_publiclyAccessible :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Bool)
modifyDBInstance_publiclyAccessible = Lens.lens (\ModifyDBInstance' {publiclyAccessible} -> publiclyAccessible) (\s@ModifyDBInstance' {} a -> s {publiclyAccessible = a} :: ModifyDBInstance)

-- | A value that indicates whether minor version upgrades are applied
-- automatically to the DB instance during the maintenance window. Changing
-- this parameter doesn\'t result in an outage except in the following case
-- and the change is asynchronously applied as soon as possible. An outage
-- results if this parameter is enabled during the maintenance window, and
-- a newer minor version is available, and RDS has enabled auto patching
-- for that engine version.
modifyDBInstance_autoMinorVersionUpgrade :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Bool)
modifyDBInstance_autoMinorVersionUpgrade = Lens.lens (\ModifyDBInstance' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@ModifyDBInstance' {} a -> s {autoMinorVersionUpgrade = a} :: ModifyDBInstance)

-- | The new DB subnet group for the DB instance. You can use this parameter
-- to move your DB instance to a different VPC. If your DB instance isn\'t
-- in a VPC, you can also use this parameter to move your DB instance into
-- a VPC. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_VPC.WorkingWithRDSInstanceinaVPC.html#USER_VPC.Non-VPC2VPC Working with a DB instance in a VPC>
-- in the /Amazon RDS User Guide./
--
-- Changing the subnet group causes an outage during the change. The change
-- is applied during the next maintenance window, unless you enable
-- @ApplyImmediately@.
--
-- Constraints: If supplied, must match the name of an existing
-- DBSubnetGroup.
--
-- Example: @mySubnetGroup@
modifyDBInstance_dbSubnetGroupName :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_dbSubnetGroupName = Lens.lens (\ModifyDBInstance' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@ModifyDBInstance' {} a -> s {dbSubnetGroupName = a} :: ModifyDBInstance)

-- | The ARN for the IAM role that permits RDS to send enhanced monitoring
-- metrics to Amazon CloudWatch Logs. For example,
-- @arn:aws:iam:123456789012:role\/emaccess@. For information on creating a
-- monitoring role, go to
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.html#USER_Monitoring.OS.IAMRole To create an IAM role for Amazon RDS Enhanced Monitoring>
-- in the /Amazon RDS User Guide./
--
-- If @MonitoringInterval@ is set to a value other than 0, then you must
-- supply a @MonitoringRoleArn@ value.
modifyDBInstance_monitoringRoleArn :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_monitoringRoleArn = Lens.lens (\ModifyDBInstance' {monitoringRoleArn} -> monitoringRoleArn) (\s@ModifyDBInstance' {} a -> s {monitoringRoleArn = a} :: ModifyDBInstance)

-- | The new Provisioned IOPS (I\/O operations per second) value for the RDS
-- instance.
--
-- Changing this setting doesn\'t result in an outage and the change is
-- applied during the next maintenance window unless the @ApplyImmediately@
-- parameter is enabled for this request. If you are migrating from
-- Provisioned IOPS to standard storage, set this value to 0. The DB
-- instance will require a reboot for the change in storage type to take
-- effect.
--
-- If you choose to migrate your DB instance from using standard storage to
-- using Provisioned IOPS, or from using Provisioned IOPS to using standard
-- storage, the process can take time. The duration of the migration
-- depends on several factors such as database load, storage size, storage
-- type (standard or Provisioned IOPS), amount of IOPS provisioned (if
-- any), and the number of prior scale storage operations. Typical
-- migration times are under 24 hours, but the process can take up to
-- several days in some cases. During the migration, the DB instance is
-- available for use, but might experience performance degradation. While
-- the migration takes place, nightly backups for the instance are
-- suspended. No other Amazon RDS operations can take place for the
-- instance, including modifying the instance, rebooting the instance,
-- deleting the instance, creating a read replica for the instance, and
-- creating a DB snapshot of the instance.
--
-- Constraints: For MariaDB, MySQL, Oracle, and PostgreSQL, the value
-- supplied must be at least 10% greater than the current value. Values
-- that are not at least 10% greater than the existing value are rounded up
-- so that they are 10% greater than the current value.
--
-- Default: Uses existing setting
modifyDBInstance_iops :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Int)
modifyDBInstance_iops = Lens.lens (\ModifyDBInstance' {iops} -> iops) (\s@ModifyDBInstance' {} a -> s {iops = a} :: ModifyDBInstance)

-- | A value that indicates whether major version upgrades are allowed.
-- Changing this parameter doesn\'t result in an outage and the change is
-- asynchronously applied as soon as possible.
--
-- Constraints: Major version upgrades must be allowed when specifying a
-- value for the EngineVersion parameter that is a different major version
-- than the DB instance\'s current version.
modifyDBInstance_allowMajorVersionUpgrade :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Bool)
modifyDBInstance_allowMajorVersionUpgrade = Lens.lens (\ModifyDBInstance' {allowMajorVersionUpgrade} -> allowMajorVersionUpgrade) (\s@ModifyDBInstance' {} a -> s {allowMajorVersionUpgrade = a} :: ModifyDBInstance)

-- | The new DB instance identifier for the DB instance when renaming a DB
-- instance. When you change the DB instance identifier, an instance reboot
-- occurs immediately if you enable @ApplyImmediately@, or will occur
-- during the next maintenance window if you disable Apply Immediately.
-- This value is stored as a lowercase string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens.
--
-- -   The first character must be a letter.
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens.
--
-- Example: @mydbinstance@
modifyDBInstance_newDBInstanceIdentifier :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_newDBInstanceIdentifier = Lens.lens (\ModifyDBInstance' {newDBInstanceIdentifier'} -> newDBInstanceIdentifier') (\s@ModifyDBInstance' {} a -> s {newDBInstanceIdentifier' = a} :: ModifyDBInstance)

-- | The Active Directory directory ID to move the DB instance to. Specify
-- @none@ to remove the instance from its current domain. The domain must
-- be created prior to this operation. Currently, only MySQL, Microsoft SQL
-- Server, Oracle, and PostgreSQL DB instances can be created in an Active
-- Directory Domain.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon RDS User Guide/.
modifyDBInstance_domain :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_domain = Lens.lens (\ModifyDBInstance' {domain} -> domain) (\s@ModifyDBInstance' {} a -> s {domain = a} :: ModifyDBInstance)

-- | A value that indicates whether to enable a customer-owned IP address
-- (CoIP) for an RDS on Outposts DB instance.
--
-- A /CoIP/ provides local or external connectivity to resources in your
-- Outpost subnets through your on-premises network. For some use cases, a
-- CoIP can provide lower latency for connections to the DB instance from
-- outside of its virtual private cloud (VPC) on your local network.
--
-- For more information about RDS on Outposts, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Working with Amazon RDS on Amazon Web Services Outposts>
-- in the /Amazon RDS User Guide/.
--
-- For more information about CoIPs, see
-- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-networking-components.html#ip-addressing Customer-owned IP addresses>
-- in the /Amazon Web Services Outposts User Guide/.
modifyDBInstance_enableCustomerOwnedIp :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Bool)
modifyDBInstance_enableCustomerOwnedIp = Lens.lens (\ModifyDBInstance' {enableCustomerOwnedIp} -> enableCustomerOwnedIp) (\s@ModifyDBInstance' {} a -> s {enableCustomerOwnedIp = a} :: ModifyDBInstance)

-- | A value that sets the open mode of a replica database to either mounted
-- or read-only.
--
-- Currently, this parameter is only supported for Oracle DB instances.
--
-- Mounted DB replicas are included in Oracle Enterprise Edition. The main
-- use case for mounted replicas is cross-Region disaster recovery. The
-- primary database doesn\'t use Active Data Guard to transmit information
-- to the mounted replica. Because it doesn\'t accept user connections, a
-- mounted replica can\'t serve a read-only workload. For more information,
-- see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/oracle-read-replicas.html Working with Oracle Read Replicas for Amazon RDS>
-- in the /Amazon RDS User Guide/.
modifyDBInstance_replicaMode :: Lens.Lens' ModifyDBInstance (Prelude.Maybe ReplicaMode)
modifyDBInstance_replicaMode = Lens.lens (\ModifyDBInstance' {replicaMode} -> replicaMode) (\s@ModifyDBInstance' {} a -> s {replicaMode = a} :: ModifyDBInstance)

-- | The interval, in seconds, between points when Enhanced Monitoring
-- metrics are collected for the DB instance. To disable collecting
-- Enhanced Monitoring metrics, specify 0. The default is 0.
--
-- If @MonitoringRoleArn@ is specified, then you must also set
-- @MonitoringInterval@ to a value other than 0.
--
-- Valid Values: @0, 1, 5, 10, 15, 30, 60@
modifyDBInstance_monitoringInterval :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Int)
modifyDBInstance_monitoringInterval = Lens.lens (\ModifyDBInstance' {monitoringInterval} -> monitoringInterval) (\s@ModifyDBInstance' {} a -> s {monitoringInterval = a} :: ModifyDBInstance)

-- | The configuration setting for the log types to be enabled for export to
-- CloudWatch Logs for a specific DB instance.
--
-- A change to the @CloudwatchLogsExportConfiguration@ parameter is always
-- applied to the DB instance immediately. Therefore, the
-- @ApplyImmediately@ parameter has no effect.
modifyDBInstance_cloudwatchLogsExportConfiguration :: Lens.Lens' ModifyDBInstance (Prelude.Maybe CloudwatchLogsExportConfiguration)
modifyDBInstance_cloudwatchLogsExportConfiguration = Lens.lens (\ModifyDBInstance' {cloudwatchLogsExportConfiguration} -> cloudwatchLogsExportConfiguration) (\s@ModifyDBInstance' {} a -> s {cloudwatchLogsExportConfiguration = a} :: ModifyDBInstance)

-- | A value that indicates whether the DB instance is restarted when you
-- rotate your SSL\/TLS certificate.
--
-- By default, the DB instance is restarted when you rotate your SSL\/TLS
-- certificate. The certificate is not updated until the DB instance is
-- restarted.
--
-- Set this parameter only if you are /not/ using SSL\/TLS to connect to
-- the DB instance.
--
-- If you are using SSL\/TLS to connect to the DB instance, follow the
-- appropriate instructions for your DB engine to rotate your SSL\/TLS
-- certificate:
--
-- -   For more information about rotating your SSL\/TLS certificate for
--     RDS DB engines, see
--     <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.SSL-certificate-rotation.html Rotating Your SSL\/TLS Certificate.>
--     in the /Amazon RDS User Guide./
--
-- -   For more information about rotating your SSL\/TLS certificate for
--     Aurora DB engines, see
--     <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.SSL-certificate-rotation.html Rotating Your SSL\/TLS Certificate>
--     in the /Amazon Aurora User Guide./
modifyDBInstance_certificateRotationRestart :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Bool)
modifyDBInstance_certificateRotationRestart = Lens.lens (\ModifyDBInstance' {certificateRotationRestart} -> certificateRotationRestart) (\s@ModifyDBInstance' {} a -> s {certificateRotationRestart = a} :: ModifyDBInstance)

-- | The password for the given ARN from the key store in order to access the
-- device.
modifyDBInstance_tdeCredentialPassword :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_tdeCredentialPassword = Lens.lens (\ModifyDBInstance' {tdeCredentialPassword} -> tdeCredentialPassword) (\s@ModifyDBInstance' {} a -> s {tdeCredentialPassword = a} :: ModifyDBInstance)

-- | The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
modifyDBInstance_processorFeatures :: Lens.Lens' ModifyDBInstance (Prelude.Maybe [ProcessorFeature])
modifyDBInstance_processorFeatures = Lens.lens (\ModifyDBInstance' {processorFeatures} -> processorFeatures) (\s@ModifyDBInstance' {} a -> s {processorFeatures = a} :: ModifyDBInstance) Prelude.. Lens.mapping Lens.coerced

-- | The new compute and memory capacity of the DB instance, for example,
-- @db.m4.large@. Not all DB instance classes are available in all Amazon
-- Web Services Regions, or for all database engines. For the full list of
-- DB instance classes, and availability for your engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
-- in the /Amazon RDS User Guide./
--
-- If you modify the DB instance class, an outage occurs during the change.
-- The change is applied during the next maintenance window, unless
-- @ApplyImmediately@ is enabled for this request.
--
-- Default: Uses existing setting
modifyDBInstance_dbInstanceClass :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_dbInstanceClass = Lens.lens (\ModifyDBInstance' {dbInstanceClass} -> dbInstanceClass) (\s@ModifyDBInstance' {} a -> s {dbInstanceClass = a} :: ModifyDBInstance)

-- | A value that specifies the order in which an Aurora Replica is promoted
-- to the primary instance after a failure of the existing primary
-- instance. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster>
-- in the /Amazon Aurora User Guide/.
--
-- Default: 1
--
-- Valid Values: 0 - 15
modifyDBInstance_promotionTier :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Int)
modifyDBInstance_promotionTier = Lens.lens (\ModifyDBInstance' {promotionTier} -> promotionTier) (\s@ModifyDBInstance' {} a -> s {promotionTier = a} :: ModifyDBInstance)

-- | The Amazon Resource Name (ARN) of the recovery point in Amazon Web
-- Services Backup.
modifyDBInstance_awsBackupRecoveryPointArn :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_awsBackupRecoveryPointArn = Lens.lens (\ModifyDBInstance' {awsBackupRecoveryPointArn} -> awsBackupRecoveryPointArn) (\s@ModifyDBInstance' {} a -> s {awsBackupRecoveryPointArn = a} :: ModifyDBInstance)

-- | The license model for the DB instance.
--
-- Valid values: @license-included@ | @bring-your-own-license@ |
-- @general-public-license@
modifyDBInstance_licenseModel :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_licenseModel = Lens.lens (\ModifyDBInstance' {licenseModel} -> licenseModel) (\s@ModifyDBInstance' {} a -> s {licenseModel = a} :: ModifyDBInstance)

-- | The weekly time range (in UTC) during which system maintenance can
-- occur, which might result in an outage. Changing this parameter doesn\'t
-- result in an outage, except in the following situation, and the change
-- is asynchronously applied as soon as possible. If there are pending
-- actions that cause a reboot, and the maintenance window is changed to
-- include the current time, then changing this parameter will cause a
-- reboot of the DB instance. If moving this window to the current time,
-- there must be at least 30 minutes between the current time and end of
-- the window to ensure pending changes are applied.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.Maintenance.html#Concepts.DBMaintenance Amazon RDS Maintenance Window>
-- in the /Amazon RDS User Guide./
--
-- Default: Uses existing setting
--
-- Format: ddd:hh24:mi-ddd:hh24:mi
--
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
--
-- Constraints: Must be at least 30 minutes
modifyDBInstance_preferredMaintenanceWindow :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_preferredMaintenanceWindow = Lens.lens (\ModifyDBInstance' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@ModifyDBInstance' {} a -> s {preferredMaintenanceWindow = a} :: ModifyDBInstance)

-- | The amount of time, in days, to retain Performance Insights data. Valid
-- values are 7 or 731 (2 years).
modifyDBInstance_performanceInsightsRetentionPeriod :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Int)
modifyDBInstance_performanceInsightsRetentionPeriod = Lens.lens (\ModifyDBInstance' {performanceInsightsRetentionPeriod} -> performanceInsightsRetentionPeriod) (\s@ModifyDBInstance' {} a -> s {performanceInsightsRetentionPeriod = a} :: ModifyDBInstance)

-- | Indicates the certificate that needs to be associated with the instance.
modifyDBInstance_cACertificateIdentifier :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_cACertificateIdentifier = Lens.lens (\ModifyDBInstance' {cACertificateIdentifier} -> cACertificateIdentifier) (\s@ModifyDBInstance' {} a -> s {cACertificateIdentifier = a} :: ModifyDBInstance)

-- | The upper limit in gibibytes (GiB) to which Amazon RDS can automatically
-- scale the storage of the DB instance.
--
-- For more information about this setting, including limitations that
-- apply to it, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
-- in the /Amazon RDS User Guide/.
modifyDBInstance_maxAllocatedStorage :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Int)
modifyDBInstance_maxAllocatedStorage = Lens.lens (\ModifyDBInstance' {maxAllocatedStorage} -> maxAllocatedStorage) (\s@ModifyDBInstance' {} a -> s {maxAllocatedStorage = a} :: ModifyDBInstance)

-- | A value that indicates whether to enable Performance Insights for the DB
-- instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights>
-- in the /Amazon Relational Database Service User Guide/.
modifyDBInstance_enablePerformanceInsights :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Bool)
modifyDBInstance_enablePerformanceInsights = Lens.lens (\ModifyDBInstance' {enablePerformanceInsights} -> enablePerformanceInsights) (\s@ModifyDBInstance' {} a -> s {enablePerformanceInsights = a} :: ModifyDBInstance)

-- | The name of the DB parameter group to apply to the DB instance. Changing
-- this setting doesn\'t result in an outage. The parameter group name
-- itself is changed immediately, but the actual parameter changes are not
-- applied until you reboot the instance without failover. In this case,
-- the DB instance isn\'t rebooted automatically and the parameter changes
-- isn\'t applied during the next maintenance window.
--
-- Default: Uses existing setting
--
-- Constraints: The DB parameter group must be in the same DB parameter
-- group family as this DB instance.
modifyDBInstance_dbParameterGroupName :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_dbParameterGroupName = Lens.lens (\ModifyDBInstance' {dbParameterGroupName} -> dbParameterGroupName) (\s@ModifyDBInstance' {} a -> s {dbParameterGroupName = a} :: ModifyDBInstance)

-- | The daily time range during which automated backups are created if
-- automated backups are enabled, as determined by the
-- @BackupRetentionPeriod@ parameter. Changing this parameter doesn\'t
-- result in an outage and the change is asynchronously applied as soon as
-- possible. The default is a 30-minute window selected at random from an
-- 8-hour block of time for each Amazon Web Services Region. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_WorkingWithAutomatedBackups.html#USER_WorkingWithAutomatedBackups.BackupWindow Backup window>
-- in the /Amazon RDS User Guide./
--
-- __Amazon Aurora__
--
-- Not applicable. The daily time range for creating automated backups is
-- managed by the DB cluster. For more information, see @ModifyDBCluster@.
--
-- Constraints:
--
-- -   Must be in the format hh24:mi-hh24:mi
--
-- -   Must be in Universal Time Coordinated (UTC)
--
-- -   Must not conflict with the preferred maintenance window
--
-- -   Must be at least 30 minutes
modifyDBInstance_preferredBackupWindow :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_preferredBackupWindow = Lens.lens (\ModifyDBInstance' {preferredBackupWindow} -> preferredBackupWindow) (\s@ModifyDBInstance' {} a -> s {preferredBackupWindow = a} :: ModifyDBInstance)

-- | The number of days to retain automated backups. Setting this parameter
-- to a positive number enables backups. Setting this parameter to 0
-- disables automated backups.
--
-- Enabling and disabling backups can result in a brief I\/O suspension
-- that lasts from a few seconds to a few minutes, depending on the size
-- and class of your DB instance.
--
-- These changes are applied during the next maintenance window unless the
-- @ApplyImmediately@ parameter is enabled for this request. If you change
-- the parameter from one non-zero value to another non-zero value, the
-- change is asynchronously applied as soon as possible.
--
-- __Amazon Aurora__
--
-- Not applicable. The retention period for automated backups is managed by
-- the DB cluster. For more information, see @ModifyDBCluster@.
--
-- Default: Uses existing setting
--
-- Constraints:
--
-- -   Must be a value from 0 to 35
--
-- -   Can be specified for a MySQL read replica only if the source is
--     running MySQL 5.6 or later
--
-- -   Can be specified for a PostgreSQL read replica only if the source is
--     running PostgreSQL 9.3.5
--
-- -   Can\'t be set to 0 if the DB instance is a source to read replicas
modifyDBInstance_backupRetentionPeriod :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Int)
modifyDBInstance_backupRetentionPeriod = Lens.lens (\ModifyDBInstance' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@ModifyDBInstance' {} a -> s {backupRetentionPeriod = a} :: ModifyDBInstance)

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
modifyDBInstance_performanceInsightsKMSKeyId :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_performanceInsightsKMSKeyId = Lens.lens (\ModifyDBInstance' {performanceInsightsKMSKeyId} -> performanceInsightsKMSKeyId) (\s@ModifyDBInstance' {} a -> s {performanceInsightsKMSKeyId = a} :: ModifyDBInstance)

-- | A list of EC2 VPC security groups to authorize on this DB instance. This
-- change is asynchronously applied as soon as possible.
--
-- __Amazon Aurora__
--
-- Not applicable. The associated list of EC2 VPC security groups is
-- managed by the DB cluster. For more information, see @ModifyDBCluster@.
--
-- Constraints:
--
-- -   If supplied, must match existing VpcSecurityGroupIds.
modifyDBInstance_vpcSecurityGroupIds :: Lens.Lens' ModifyDBInstance (Prelude.Maybe [Prelude.Text])
modifyDBInstance_vpcSecurityGroupIds = Lens.lens (\ModifyDBInstance' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@ModifyDBInstance' {} a -> s {vpcSecurityGroupIds = a} :: ModifyDBInstance) Prelude.. Lens.mapping Lens.coerced

-- | A value that indicates whether the DB instance is a Multi-AZ deployment.
-- Changing this parameter doesn\'t result in an outage and the change is
-- applied during the next maintenance window unless the @ApplyImmediately@
-- parameter is enabled for this request.
modifyDBInstance_multiAZ :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Bool)
modifyDBInstance_multiAZ = Lens.lens (\ModifyDBInstance' {multiAZ} -> multiAZ) (\s@ModifyDBInstance' {} a -> s {multiAZ = a} :: ModifyDBInstance)

-- | The new amount of storage in gibibytes (GiB) to allocate for the DB
-- instance.
--
-- For MariaDB, MySQL, Oracle, and PostgreSQL, the value supplied must be
-- at least 10% greater than the current value. Values that are not at
-- least 10% greater than the existing value are rounded up so that they
-- are 10% greater than the current value.
--
-- For the valid values for allocated storage for each engine, see
-- @CreateDBInstance@.
modifyDBInstance_allocatedStorage :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Int)
modifyDBInstance_allocatedStorage = Lens.lens (\ModifyDBInstance' {allocatedStorage} -> allocatedStorage) (\s@ModifyDBInstance' {} a -> s {allocatedStorage = a} :: ModifyDBInstance)

-- | A value that indicates whether the modifications in this request and any
-- pending modifications are asynchronously applied as soon as possible,
-- regardless of the @PreferredMaintenanceWindow@ setting for the DB
-- instance. By default, this parameter is disabled.
--
-- If this parameter is disabled, changes to the DB instance are applied
-- during the next maintenance window. Some parameter changes can cause an
-- outage and are applied on the next call to RebootDBInstance, or the next
-- failure reboot. Review the table of parameters in
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.DBInstance.Modifying.html Modifying a DB Instance>
-- in the /Amazon RDS User Guide./ to see the impact of enabling or
-- disabling @ApplyImmediately@ for each modified parameter and to
-- determine when the changes are applied.
modifyDBInstance_applyImmediately :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Bool)
modifyDBInstance_applyImmediately = Lens.lens (\ModifyDBInstance' {applyImmediately} -> applyImmediately) (\s@ModifyDBInstance' {} a -> s {applyImmediately = a} :: ModifyDBInstance)

-- | A value that indicates the DB instance should be associated with the
-- specified option group. Changing this parameter doesn\'t result in an
-- outage except in the following case and the change is applied during the
-- next maintenance window unless the @ApplyImmediately@ parameter is
-- enabled for this request. If the parameter change results in an option
-- group that enables OEM, this change can cause a brief (sub-second)
-- period during which new connections are rejected but existing
-- connections are not interrupted.
--
-- Permanent options, such as the TDE option for Oracle Advanced Security
-- TDE, can\'t be removed from an option group, and that option group
-- can\'t be removed from a DB instance once it is associated with a DB
-- instance
modifyDBInstance_optionGroupName :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_optionGroupName = Lens.lens (\ModifyDBInstance' {optionGroupName} -> optionGroupName) (\s@ModifyDBInstance' {} a -> s {optionGroupName = a} :: ModifyDBInstance)

-- | A value that indicates whether to copy all tags from the DB instance to
-- snapshots of the DB instance. By default, tags are not copied.
--
-- __Amazon Aurora__
--
-- Not applicable. Copying tags to snapshots is managed by the DB cluster.
-- Setting this value for an Aurora DB instance has no effect on the DB
-- cluster setting. For more information, see @ModifyDBCluster@.
modifyDBInstance_copyTagsToSnapshot :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Bool)
modifyDBInstance_copyTagsToSnapshot = Lens.lens (\ModifyDBInstance' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@ModifyDBInstance' {} a -> s {copyTagsToSnapshot = a} :: ModifyDBInstance)

-- | The ARN from the key store with which to associate the instance for TDE
-- encryption.
modifyDBInstance_tdeCredentialArn :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_tdeCredentialArn = Lens.lens (\ModifyDBInstance' {tdeCredentialArn} -> tdeCredentialArn) (\s@ModifyDBInstance' {} a -> s {tdeCredentialArn = a} :: ModifyDBInstance)

-- | The name of the IAM role to use when making API calls to the Directory
-- Service.
modifyDBInstance_domainIAMRoleName :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_domainIAMRoleName = Lens.lens (\ModifyDBInstance' {domainIAMRoleName} -> domainIAMRoleName) (\s@ModifyDBInstance' {} a -> s {domainIAMRoleName = a} :: ModifyDBInstance)

-- | A value that indicates whether to enable mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts. By
-- default, mapping is disabled.
--
-- This setting doesn\'t apply to Amazon Aurora. Mapping Amazon Web
-- Services IAM accounts to database accounts is managed by the DB cluster.
--
-- For more information about IAM database authentication, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
-- in the /Amazon RDS User Guide./
modifyDBInstance_enableIAMDatabaseAuthentication :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Bool)
modifyDBInstance_enableIAMDatabaseAuthentication = Lens.lens (\ModifyDBInstance' {enableIAMDatabaseAuthentication} -> enableIAMDatabaseAuthentication) (\s@ModifyDBInstance' {} a -> s {enableIAMDatabaseAuthentication = a} :: ModifyDBInstance)

-- | A value that indicates whether the DB instance class of the DB instance
-- uses its default processor features.
modifyDBInstance_useDefaultProcessorFeatures :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Bool)
modifyDBInstance_useDefaultProcessorFeatures = Lens.lens (\ModifyDBInstance' {useDefaultProcessorFeatures} -> useDefaultProcessorFeatures) (\s@ModifyDBInstance' {} a -> s {useDefaultProcessorFeatures = a} :: ModifyDBInstance)

-- | Specifies the storage type to be associated with the DB instance.
--
-- If you specify Provisioned IOPS (@io1@), you must also include a value
-- for the @Iops@ parameter.
--
-- If you choose to migrate your DB instance from using standard storage to
-- using Provisioned IOPS, or from using Provisioned IOPS to using standard
-- storage, the process can take time. The duration of the migration
-- depends on several factors such as database load, storage size, storage
-- type (standard or Provisioned IOPS), amount of IOPS provisioned (if
-- any), and the number of prior scale storage operations. Typical
-- migration times are under 24 hours, but the process can take up to
-- several days in some cases. During the migration, the DB instance is
-- available for use, but might experience performance degradation. While
-- the migration takes place, nightly backups for the instance are
-- suspended. No other Amazon RDS operations can take place for the
-- instance, including modifying the instance, rebooting the instance,
-- deleting the instance, creating a read replica for the instance, and
-- creating a DB snapshot of the instance.
--
-- Valid values: @standard | gp2 | io1@
--
-- Default: @io1@ if the @Iops@ parameter is specified, otherwise @gp2@
modifyDBInstance_storageType :: Lens.Lens' ModifyDBInstance (Prelude.Maybe Prelude.Text)
modifyDBInstance_storageType = Lens.lens (\ModifyDBInstance' {storageType} -> storageType) (\s@ModifyDBInstance' {} a -> s {storageType = a} :: ModifyDBInstance)

-- | The DB instance identifier. This value is stored as a lowercase string.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DBInstance.
modifyDBInstance_dbInstanceIdentifier :: Lens.Lens' ModifyDBInstance Prelude.Text
modifyDBInstance_dbInstanceIdentifier = Lens.lens (\ModifyDBInstance' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@ModifyDBInstance' {} a -> s {dbInstanceIdentifier = a} :: ModifyDBInstance)

instance Core.AWSRequest ModifyDBInstance where
  type
    AWSResponse ModifyDBInstance =
      ModifyDBInstanceResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyDBInstanceResult"
      ( \s h x ->
          ModifyDBInstanceResponse'
            Prelude.<$> (x Core..@? "DBInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyDBInstance

instance Prelude.NFData ModifyDBInstance

instance Core.ToHeaders ModifyDBInstance where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifyDBInstance where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifyDBInstance where
  toQuery ModifyDBInstance' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ModifyDBInstance" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "EngineVersion" Core.=: engineVersion,
        "DBSecurityGroups"
          Core.=: Core.toQuery
            ( Core.toQueryList "DBSecurityGroupName"
                Prelude.<$> dbSecurityGroups
            ),
        "DBPortNumber" Core.=: dbPortNumber,
        "DeletionProtection" Core.=: deletionProtection,
        "MasterUserPassword" Core.=: masterUserPassword,
        "PubliclyAccessible" Core.=: publiclyAccessible,
        "AutoMinorVersionUpgrade"
          Core.=: autoMinorVersionUpgrade,
        "DBSubnetGroupName" Core.=: dbSubnetGroupName,
        "MonitoringRoleArn" Core.=: monitoringRoleArn,
        "Iops" Core.=: iops,
        "AllowMajorVersionUpgrade"
          Core.=: allowMajorVersionUpgrade,
        "NewDBInstanceIdentifier"
          Core.=: newDBInstanceIdentifier',
        "Domain" Core.=: domain,
        "EnableCustomerOwnedIp"
          Core.=: enableCustomerOwnedIp,
        "ReplicaMode" Core.=: replicaMode,
        "MonitoringInterval" Core.=: monitoringInterval,
        "CloudwatchLogsExportConfiguration"
          Core.=: cloudwatchLogsExportConfiguration,
        "CertificateRotationRestart"
          Core.=: certificateRotationRestart,
        "TdeCredentialPassword"
          Core.=: tdeCredentialPassword,
        "ProcessorFeatures"
          Core.=: Core.toQuery
            ( Core.toQueryList "ProcessorFeature"
                Prelude.<$> processorFeatures
            ),
        "DBInstanceClass" Core.=: dbInstanceClass,
        "PromotionTier" Core.=: promotionTier,
        "AwsBackupRecoveryPointArn"
          Core.=: awsBackupRecoveryPointArn,
        "LicenseModel" Core.=: licenseModel,
        "PreferredMaintenanceWindow"
          Core.=: preferredMaintenanceWindow,
        "PerformanceInsightsRetentionPeriod"
          Core.=: performanceInsightsRetentionPeriod,
        "CACertificateIdentifier"
          Core.=: cACertificateIdentifier,
        "MaxAllocatedStorage" Core.=: maxAllocatedStorage,
        "EnablePerformanceInsights"
          Core.=: enablePerformanceInsights,
        "DBParameterGroupName" Core.=: dbParameterGroupName,
        "PreferredBackupWindow"
          Core.=: preferredBackupWindow,
        "BackupRetentionPeriod"
          Core.=: backupRetentionPeriod,
        "PerformanceInsightsKMSKeyId"
          Core.=: performanceInsightsKMSKeyId,
        "VpcSecurityGroupIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "VpcSecurityGroupId"
                Prelude.<$> vpcSecurityGroupIds
            ),
        "MultiAZ" Core.=: multiAZ,
        "AllocatedStorage" Core.=: allocatedStorage,
        "ApplyImmediately" Core.=: applyImmediately,
        "OptionGroupName" Core.=: optionGroupName,
        "CopyTagsToSnapshot" Core.=: copyTagsToSnapshot,
        "TdeCredentialArn" Core.=: tdeCredentialArn,
        "DomainIAMRoleName" Core.=: domainIAMRoleName,
        "EnableIAMDatabaseAuthentication"
          Core.=: enableIAMDatabaseAuthentication,
        "UseDefaultProcessorFeatures"
          Core.=: useDefaultProcessorFeatures,
        "StorageType" Core.=: storageType,
        "DBInstanceIdentifier" Core.=: dbInstanceIdentifier
      ]

-- | /See:/ 'newModifyDBInstanceResponse' smart constructor.
data ModifyDBInstanceResponse = ModifyDBInstanceResponse'
  { dbInstance :: Prelude.Maybe DBInstance,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyDBInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbInstance', 'modifyDBInstanceResponse_dbInstance' - Undocumented member.
--
-- 'httpStatus', 'modifyDBInstanceResponse_httpStatus' - The response's http status code.
newModifyDBInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyDBInstanceResponse
newModifyDBInstanceResponse pHttpStatus_ =
  ModifyDBInstanceResponse'
    { dbInstance =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyDBInstanceResponse_dbInstance :: Lens.Lens' ModifyDBInstanceResponse (Prelude.Maybe DBInstance)
modifyDBInstanceResponse_dbInstance = Lens.lens (\ModifyDBInstanceResponse' {dbInstance} -> dbInstance) (\s@ModifyDBInstanceResponse' {} a -> s {dbInstance = a} :: ModifyDBInstanceResponse)

-- | The response's http status code.
modifyDBInstanceResponse_httpStatus :: Lens.Lens' ModifyDBInstanceResponse Prelude.Int
modifyDBInstanceResponse_httpStatus = Lens.lens (\ModifyDBInstanceResponse' {httpStatus} -> httpStatus) (\s@ModifyDBInstanceResponse' {} a -> s {httpStatus = a} :: ModifyDBInstanceResponse)

instance Prelude.NFData ModifyDBInstanceResponse
