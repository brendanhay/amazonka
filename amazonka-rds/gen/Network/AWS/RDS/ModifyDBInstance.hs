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
-- Module      : Network.AWS.RDS.ModifyDBInstance
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
module Network.AWS.RDS.ModifyDBInstance
  ( -- * Creating a Request
    ModifyDBInstance (..),
    newModifyDBInstance,

    -- * Request Lenses
    modifyDBInstance_backupRetentionPeriod,
    modifyDBInstance_deletionProtection,
    modifyDBInstance_preferredBackupWindow,
    modifyDBInstance_dbPortNumber,
    modifyDBInstance_cACertificateIdentifier,
    modifyDBInstance_enablePerformanceInsights,
    modifyDBInstance_dbSecurityGroups,
    modifyDBInstance_maxAllocatedStorage,
    modifyDBInstance_enableIAMDatabaseAuthentication,
    modifyDBInstance_storageType,
    modifyDBInstance_useDefaultProcessorFeatures,
    modifyDBInstance_monitoringInterval,
    modifyDBInstance_optionGroupName,
    modifyDBInstance_domain,
    modifyDBInstance_allowMajorVersionUpgrade,
    modifyDBInstance_monitoringRoleArn,
    modifyDBInstance_dbSubnetGroupName,
    modifyDBInstance_masterUserPassword,
    modifyDBInstance_multiAZ,
    modifyDBInstance_publiclyAccessible,
    modifyDBInstance_vpcSecurityGroupIds,
    modifyDBInstance_performanceInsightsKMSKeyId,
    modifyDBInstance_dbParameterGroupName,
    modifyDBInstance_engineVersion,
    modifyDBInstance_preferredMaintenanceWindow,
    modifyDBInstance_performanceInsightsRetentionPeriod,
    modifyDBInstance_licenseModel,
    modifyDBInstance_tdeCredentialPassword,
    modifyDBInstance_promotionTier,
    modifyDBInstance_processorFeatures,
    modifyDBInstance_awsBackupRecoveryPointArn,
    modifyDBInstance_dbInstanceClass,
    modifyDBInstance_domainIAMRoleName,
    modifyDBInstance_certificateRotationRestart,
    modifyDBInstance_tdeCredentialArn,
    modifyDBInstance_enableCustomerOwnedIp,
    modifyDBInstance_cloudwatchLogsExportConfiguration,
    modifyDBInstance_copyTagsToSnapshot,
    modifyDBInstance_replicaMode,
    modifyDBInstance_newDBInstanceIdentifier,
    modifyDBInstance_allocatedStorage,
    modifyDBInstance_applyImmediately,
    modifyDBInstance_iops,
    modifyDBInstance_autoMinorVersionUpgrade,
    modifyDBInstance_dbInstanceIdentifier,

    -- * Destructuring the Response
    ModifyDBInstanceResponse (..),
    newModifyDBInstanceResponse,

    -- * Response Lenses
    modifyDBInstanceResponse_dbInstance,
    modifyDBInstanceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newModifyDBInstance' smart constructor.
data ModifyDBInstance = ModifyDBInstance'
  { -- | The number of days to retain automated backups. Setting this parameter
    -- to a positive number enables backups. Setting this parameter to 0
    -- disables automated backups.
    --
    -- Changing this parameter can result in an outage if you change from 0 to
    -- a non-zero value or from a non-zero value to 0. These changes are
    -- applied during the next maintenance window unless the @ApplyImmediately@
    -- parameter is enabled for this request. If you change the parameter from
    -- one non-zero value to another non-zero value, the change is
    -- asynchronously applied as soon as possible.
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
    backupRetentionPeriod :: Core.Maybe Core.Int,
    -- | A value that indicates whether the DB instance has deletion protection
    -- enabled. The database can\'t be deleted when deletion protection is
    -- enabled. By default, deletion protection is disabled. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
    deletionProtection :: Core.Maybe Core.Bool,
    -- | The daily time range during which automated backups are created if
    -- automated backups are enabled, as determined by the
    -- @BackupRetentionPeriod@ parameter. Changing this parameter doesn\'t
    -- result in an outage and the change is asynchronously applied as soon as
    -- possible.
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
    preferredBackupWindow :: Core.Maybe Core.Text,
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
    dbPortNumber :: Core.Maybe Core.Int,
    -- | Indicates the certificate that needs to be associated with the instance.
    cACertificateIdentifier :: Core.Maybe Core.Text,
    -- | A value that indicates whether to enable Performance Insights for the DB
    -- instance.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights>
    -- in the /Amazon Relational Database Service User Guide/.
    enablePerformanceInsights :: Core.Maybe Core.Bool,
    -- | A list of DB security groups to authorize on this DB instance. Changing
    -- this setting doesn\'t result in an outage and the change is
    -- asynchronously applied as soon as possible.
    --
    -- Constraints:
    --
    -- -   If supplied, must match existing DBSecurityGroups.
    dbSecurityGroups :: Core.Maybe [Core.Text],
    -- | The upper limit to which Amazon RDS can automatically scale the storage
    -- of the DB instance.
    --
    -- For more information about this setting, including limitations that
    -- apply to it, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
    -- in the /Amazon RDS User Guide/.
    maxAllocatedStorage :: Core.Maybe Core.Int,
    -- | A value that indicates whether to enable mapping of AWS Identity and
    -- Access Management (IAM) accounts to database accounts. By default,
    -- mapping is disabled.
    --
    -- This setting doesn\'t apply to Amazon Aurora. Mapping AWS IAM accounts
    -- to database accounts is managed by the DB cluster.
    --
    -- For more information about IAM database authentication, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
    -- in the /Amazon RDS User Guide./
    enableIAMDatabaseAuthentication :: Core.Maybe Core.Bool,
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
    storageType :: Core.Maybe Core.Text,
    -- | A value that indicates whether the DB instance class of the DB instance
    -- uses its default processor features.
    useDefaultProcessorFeatures :: Core.Maybe Core.Bool,
    -- | The interval, in seconds, between points when Enhanced Monitoring
    -- metrics are collected for the DB instance. To disable collecting
    -- Enhanced Monitoring metrics, specify 0. The default is 0.
    --
    -- If @MonitoringRoleArn@ is specified, then you must also set
    -- @MonitoringInterval@ to a value other than 0.
    --
    -- Valid Values: @0, 1, 5, 10, 15, 30, 60@
    monitoringInterval :: Core.Maybe Core.Int,
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
    optionGroupName :: Core.Maybe Core.Text,
    -- | The Active Directory directory ID to move the DB instance to. Specify
    -- @none@ to remove the instance from its current domain. The domain must
    -- be created prior to this operation. Currently, only MySQL, Microsoft SQL
    -- Server, Oracle, and PostgreSQL DB instances can be created in an Active
    -- Directory Domain.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
    -- in the /Amazon RDS User Guide/.
    domain :: Core.Maybe Core.Text,
    -- | A value that indicates whether major version upgrades are allowed.
    -- Changing this parameter doesn\'t result in an outage and the change is
    -- asynchronously applied as soon as possible.
    --
    -- Constraints: Major version upgrades must be allowed when specifying a
    -- value for the EngineVersion parameter that is a different major version
    -- than the DB instance\'s current version.
    allowMajorVersionUpgrade :: Core.Maybe Core.Bool,
    -- | The ARN for the IAM role that permits RDS to send enhanced monitoring
    -- metrics to Amazon CloudWatch Logs. For example,
    -- @arn:aws:iam:123456789012:role\/emaccess@. For information on creating a
    -- monitoring role, go to
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.html#USER_Monitoring.OS.IAMRole To create an IAM role for Amazon RDS Enhanced Monitoring>
    -- in the /Amazon RDS User Guide./
    --
    -- If @MonitoringInterval@ is set to a value other than 0, then you must
    -- supply a @MonitoringRoleArn@ value.
    monitoringRoleArn :: Core.Maybe Core.Text,
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
    dbSubnetGroupName :: Core.Maybe Core.Text,
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
    masterUserPassword :: Core.Maybe Core.Text,
    -- | A value that indicates whether the DB instance is a Multi-AZ deployment.
    -- Changing this parameter doesn\'t result in an outage and the change is
    -- applied during the next maintenance window unless the @ApplyImmediately@
    -- parameter is enabled for this request.
    multiAZ :: Core.Maybe Core.Bool,
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
    publiclyAccessible :: Core.Maybe Core.Bool,
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
    vpcSecurityGroupIds :: Core.Maybe [Core.Text],
    -- | The AWS KMS key identifier for encryption of Performance Insights data.
    --
    -- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
    -- name for the AWS KMS customer master key (CMK).
    --
    -- If you do not specify a value for @PerformanceInsightsKMSKeyId@, then
    -- Amazon RDS uses your default CMK. There is a default CMK for your AWS
    -- account. Your AWS account has a different default CMK for each AWS
    -- Region.
    performanceInsightsKMSKeyId :: Core.Maybe Core.Text,
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
    dbParameterGroupName :: Core.Maybe Core.Text,
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
    engineVersion :: Core.Maybe Core.Text,
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
    -- Default: Uses existing setting
    --
    -- Format: ddd:hh24:mi-ddd:hh24:mi
    --
    -- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
    --
    -- Constraints: Must be at least 30 minutes
    preferredMaintenanceWindow :: Core.Maybe Core.Text,
    -- | The amount of time, in days, to retain Performance Insights data. Valid
    -- values are 7 or 731 (2 years).
    performanceInsightsRetentionPeriod :: Core.Maybe Core.Int,
    -- | The license model for the DB instance.
    --
    -- Valid values: @license-included@ | @bring-your-own-license@ |
    -- @general-public-license@
    licenseModel :: Core.Maybe Core.Text,
    -- | The password for the given ARN from the key store in order to access the
    -- device.
    tdeCredentialPassword :: Core.Maybe Core.Text,
    -- | A value that specifies the order in which an Aurora Replica is promoted
    -- to the primary instance after a failure of the existing primary
    -- instance. For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster>
    -- in the /Amazon Aurora User Guide/.
    --
    -- Default: 1
    --
    -- Valid Values: 0 - 15
    promotionTier :: Core.Maybe Core.Int,
    -- | The number of CPU cores and the number of threads per core for the DB
    -- instance class of the DB instance.
    processorFeatures :: Core.Maybe [ProcessorFeature],
    -- | The Amazon Resource Name (ARN) of the recovery point in AWS Backup.
    awsBackupRecoveryPointArn :: Core.Maybe Core.Text,
    -- | The new compute and memory capacity of the DB instance, for example,
    -- @db.m4.large@. Not all DB instance classes are available in all AWS
    -- Regions, or for all database engines. For the full list of DB instance
    -- classes, and availability for your engine, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
    -- in the /Amazon RDS User Guide./
    --
    -- If you modify the DB instance class, an outage occurs during the change.
    -- The change is applied during the next maintenance window, unless
    -- @ApplyImmediately@ is enabled for this request.
    --
    -- Default: Uses existing setting
    dbInstanceClass :: Core.Maybe Core.Text,
    -- | The name of the IAM role to use when making API calls to the Directory
    -- Service.
    domainIAMRoleName :: Core.Maybe Core.Text,
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
    certificateRotationRestart :: Core.Maybe Core.Bool,
    -- | The ARN from the key store with which to associate the instance for TDE
    -- encryption.
    tdeCredentialArn :: Core.Maybe Core.Text,
    -- | A value that indicates whether to enable a customer-owned IP address
    -- (CoIP) for an RDS on Outposts DB instance.
    --
    -- A /CoIP/ provides local or external connectivity to resources in your
    -- Outpost subnets through your on-premises network. For some use cases, a
    -- CoIP can provide lower latency for connections to the DB instance from
    -- outside of its virtual private cloud (VPC) on your local network.
    --
    -- For more information about RDS on Outposts, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Working with Amazon RDS on AWS Outposts>
    -- in the /Amazon RDS User Guide/.
    --
    -- For more information about CoIPs, see
    -- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-networking-components.html#ip-addressing Customer-owned IP addresses>
    -- in the /AWS Outposts User Guide/.
    enableCustomerOwnedIp :: Core.Maybe Core.Bool,
    -- | The configuration setting for the log types to be enabled for export to
    -- CloudWatch Logs for a specific DB instance.
    --
    -- A change to the @CloudwatchLogsExportConfiguration@ parameter is always
    -- applied to the DB instance immediately. Therefore, the
    -- @ApplyImmediately@ parameter has no effect.
    cloudwatchLogsExportConfiguration :: Core.Maybe CloudwatchLogsExportConfiguration,
    -- | A value that indicates whether to copy all tags from the DB instance to
    -- snapshots of the DB instance. By default, tags are not copied.
    --
    -- __Amazon Aurora__
    --
    -- Not applicable. Copying tags to snapshots is managed by the DB cluster.
    -- Setting this value for an Aurora DB instance has no effect on the DB
    -- cluster setting. For more information, see @ModifyDBCluster@.
    copyTagsToSnapshot :: Core.Maybe Core.Bool,
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
    replicaMode :: Core.Maybe ReplicaMode,
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
    newDBInstanceIdentifier' :: Core.Maybe Core.Text,
    -- | The new amount of storage (in gibibytes) to allocate for the DB
    -- instance.
    --
    -- For MariaDB, MySQL, Oracle, and PostgreSQL, the value supplied must be
    -- at least 10% greater than the current value. Values that are not at
    -- least 10% greater than the existing value are rounded up so that they
    -- are 10% greater than the current value.
    --
    -- For the valid values for allocated storage for each engine, see
    -- @CreateDBInstance@.
    allocatedStorage :: Core.Maybe Core.Int,
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
    applyImmediately :: Core.Maybe Core.Bool,
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
    iops :: Core.Maybe Core.Int,
    -- | A value that indicates whether minor version upgrades are applied
    -- automatically to the DB instance during the maintenance window. Changing
    -- this parameter doesn\'t result in an outage except in the following case
    -- and the change is asynchronously applied as soon as possible. An outage
    -- results if this parameter is enabled during the maintenance window, and
    -- a newer minor version is available, and RDS has enabled auto patching
    -- for that engine version.
    autoMinorVersionUpgrade :: Core.Maybe Core.Bool,
    -- | The DB instance identifier. This value is stored as a lowercase string.
    --
    -- Constraints:
    --
    -- -   Must match the identifier of an existing DBInstance.
    dbInstanceIdentifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyDBInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupRetentionPeriod', 'modifyDBInstance_backupRetentionPeriod' - The number of days to retain automated backups. Setting this parameter
-- to a positive number enables backups. Setting this parameter to 0
-- disables automated backups.
--
-- Changing this parameter can result in an outage if you change from 0 to
-- a non-zero value or from a non-zero value to 0. These changes are
-- applied during the next maintenance window unless the @ApplyImmediately@
-- parameter is enabled for this request. If you change the parameter from
-- one non-zero value to another non-zero value, the change is
-- asynchronously applied as soon as possible.
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
-- 'deletionProtection', 'modifyDBInstance_deletionProtection' - A value that indicates whether the DB instance has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
--
-- 'preferredBackupWindow', 'modifyDBInstance_preferredBackupWindow' - The daily time range during which automated backups are created if
-- automated backups are enabled, as determined by the
-- @BackupRetentionPeriod@ parameter. Changing this parameter doesn\'t
-- result in an outage and the change is asynchronously applied as soon as
-- possible.
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
-- 'cACertificateIdentifier', 'modifyDBInstance_cACertificateIdentifier' - Indicates the certificate that needs to be associated with the instance.
--
-- 'enablePerformanceInsights', 'modifyDBInstance_enablePerformanceInsights' - A value that indicates whether to enable Performance Insights for the DB
-- instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights>
-- in the /Amazon Relational Database Service User Guide/.
--
-- 'dbSecurityGroups', 'modifyDBInstance_dbSecurityGroups' - A list of DB security groups to authorize on this DB instance. Changing
-- this setting doesn\'t result in an outage and the change is
-- asynchronously applied as soon as possible.
--
-- Constraints:
--
-- -   If supplied, must match existing DBSecurityGroups.
--
-- 'maxAllocatedStorage', 'modifyDBInstance_maxAllocatedStorage' - The upper limit to which Amazon RDS can automatically scale the storage
-- of the DB instance.
--
-- For more information about this setting, including limitations that
-- apply to it, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
-- in the /Amazon RDS User Guide/.
--
-- 'enableIAMDatabaseAuthentication', 'modifyDBInstance_enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of AWS Identity and
-- Access Management (IAM) accounts to database accounts. By default,
-- mapping is disabled.
--
-- This setting doesn\'t apply to Amazon Aurora. Mapping AWS IAM accounts
-- to database accounts is managed by the DB cluster.
--
-- For more information about IAM database authentication, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
-- in the /Amazon RDS User Guide./
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
-- 'useDefaultProcessorFeatures', 'modifyDBInstance_useDefaultProcessorFeatures' - A value that indicates whether the DB instance class of the DB instance
-- uses its default processor features.
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
-- 'allowMajorVersionUpgrade', 'modifyDBInstance_allowMajorVersionUpgrade' - A value that indicates whether major version upgrades are allowed.
-- Changing this parameter doesn\'t result in an outage and the change is
-- asynchronously applied as soon as possible.
--
-- Constraints: Major version upgrades must be allowed when specifying a
-- value for the EngineVersion parameter that is a different major version
-- than the DB instance\'s current version.
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
-- 'multiAZ', 'modifyDBInstance_multiAZ' - A value that indicates whether the DB instance is a Multi-AZ deployment.
-- Changing this parameter doesn\'t result in an outage and the change is
-- applied during the next maintenance window unless the @ApplyImmediately@
-- parameter is enabled for this request.
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
-- 'performanceInsightsKMSKeyId', 'modifyDBInstance_performanceInsightsKMSKeyId' - The AWS KMS key identifier for encryption of Performance Insights data.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS customer master key (CMK).
--
-- If you do not specify a value for @PerformanceInsightsKMSKeyId@, then
-- Amazon RDS uses your default CMK. There is a default CMK for your AWS
-- account. Your AWS account has a different default CMK for each AWS
-- Region.
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
-- 'licenseModel', 'modifyDBInstance_licenseModel' - The license model for the DB instance.
--
-- Valid values: @license-included@ | @bring-your-own-license@ |
-- @general-public-license@
--
-- 'tdeCredentialPassword', 'modifyDBInstance_tdeCredentialPassword' - The password for the given ARN from the key store in order to access the
-- device.
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
-- 'processorFeatures', 'modifyDBInstance_processorFeatures' - The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
--
-- 'awsBackupRecoveryPointArn', 'modifyDBInstance_awsBackupRecoveryPointArn' - The Amazon Resource Name (ARN) of the recovery point in AWS Backup.
--
-- 'dbInstanceClass', 'modifyDBInstance_dbInstanceClass' - The new compute and memory capacity of the DB instance, for example,
-- @db.m4.large@. Not all DB instance classes are available in all AWS
-- Regions, or for all database engines. For the full list of DB instance
-- classes, and availability for your engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
-- in the /Amazon RDS User Guide./
--
-- If you modify the DB instance class, an outage occurs during the change.
-- The change is applied during the next maintenance window, unless
-- @ApplyImmediately@ is enabled for this request.
--
-- Default: Uses existing setting
--
-- 'domainIAMRoleName', 'modifyDBInstance_domainIAMRoleName' - The name of the IAM role to use when making API calls to the Directory
-- Service.
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
-- 'tdeCredentialArn', 'modifyDBInstance_tdeCredentialArn' - The ARN from the key store with which to associate the instance for TDE
-- encryption.
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
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Working with Amazon RDS on AWS Outposts>
-- in the /Amazon RDS User Guide/.
--
-- For more information about CoIPs, see
-- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-networking-components.html#ip-addressing Customer-owned IP addresses>
-- in the /AWS Outposts User Guide/.
--
-- 'cloudwatchLogsExportConfiguration', 'modifyDBInstance_cloudwatchLogsExportConfiguration' - The configuration setting for the log types to be enabled for export to
-- CloudWatch Logs for a specific DB instance.
--
-- A change to the @CloudwatchLogsExportConfiguration@ parameter is always
-- applied to the DB instance immediately. Therefore, the
-- @ApplyImmediately@ parameter has no effect.
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
-- 'allocatedStorage', 'modifyDBInstance_allocatedStorage' - The new amount of storage (in gibibytes) to allocate for the DB
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
-- 'autoMinorVersionUpgrade', 'modifyDBInstance_autoMinorVersionUpgrade' - A value that indicates whether minor version upgrades are applied
-- automatically to the DB instance during the maintenance window. Changing
-- this parameter doesn\'t result in an outage except in the following case
-- and the change is asynchronously applied as soon as possible. An outage
-- results if this parameter is enabled during the maintenance window, and
-- a newer minor version is available, and RDS has enabled auto patching
-- for that engine version.
--
-- 'dbInstanceIdentifier', 'modifyDBInstance_dbInstanceIdentifier' - The DB instance identifier. This value is stored as a lowercase string.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DBInstance.
newModifyDBInstance ::
  -- | 'dbInstanceIdentifier'
  Core.Text ->
  ModifyDBInstance
newModifyDBInstance pDBInstanceIdentifier_ =
  ModifyDBInstance'
    { backupRetentionPeriod =
        Core.Nothing,
      deletionProtection = Core.Nothing,
      preferredBackupWindow = Core.Nothing,
      dbPortNumber = Core.Nothing,
      cACertificateIdentifier = Core.Nothing,
      enablePerformanceInsights = Core.Nothing,
      dbSecurityGroups = Core.Nothing,
      maxAllocatedStorage = Core.Nothing,
      enableIAMDatabaseAuthentication = Core.Nothing,
      storageType = Core.Nothing,
      useDefaultProcessorFeatures = Core.Nothing,
      monitoringInterval = Core.Nothing,
      optionGroupName = Core.Nothing,
      domain = Core.Nothing,
      allowMajorVersionUpgrade = Core.Nothing,
      monitoringRoleArn = Core.Nothing,
      dbSubnetGroupName = Core.Nothing,
      masterUserPassword = Core.Nothing,
      multiAZ = Core.Nothing,
      publiclyAccessible = Core.Nothing,
      vpcSecurityGroupIds = Core.Nothing,
      performanceInsightsKMSKeyId = Core.Nothing,
      dbParameterGroupName = Core.Nothing,
      engineVersion = Core.Nothing,
      preferredMaintenanceWindow = Core.Nothing,
      performanceInsightsRetentionPeriod = Core.Nothing,
      licenseModel = Core.Nothing,
      tdeCredentialPassword = Core.Nothing,
      promotionTier = Core.Nothing,
      processorFeatures = Core.Nothing,
      awsBackupRecoveryPointArn = Core.Nothing,
      dbInstanceClass = Core.Nothing,
      domainIAMRoleName = Core.Nothing,
      certificateRotationRestart = Core.Nothing,
      tdeCredentialArn = Core.Nothing,
      enableCustomerOwnedIp = Core.Nothing,
      cloudwatchLogsExportConfiguration = Core.Nothing,
      copyTagsToSnapshot = Core.Nothing,
      replicaMode = Core.Nothing,
      newDBInstanceIdentifier' = Core.Nothing,
      allocatedStorage = Core.Nothing,
      applyImmediately = Core.Nothing,
      iops = Core.Nothing,
      autoMinorVersionUpgrade = Core.Nothing,
      dbInstanceIdentifier = pDBInstanceIdentifier_
    }

-- | The number of days to retain automated backups. Setting this parameter
-- to a positive number enables backups. Setting this parameter to 0
-- disables automated backups.
--
-- Changing this parameter can result in an outage if you change from 0 to
-- a non-zero value or from a non-zero value to 0. These changes are
-- applied during the next maintenance window unless the @ApplyImmediately@
-- parameter is enabled for this request. If you change the parameter from
-- one non-zero value to another non-zero value, the change is
-- asynchronously applied as soon as possible.
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
modifyDBInstance_backupRetentionPeriod :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Int)
modifyDBInstance_backupRetentionPeriod = Lens.lens (\ModifyDBInstance' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@ModifyDBInstance' {} a -> s {backupRetentionPeriod = a} :: ModifyDBInstance)

-- | A value that indicates whether the DB instance has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_DeleteInstance.html Deleting a DB Instance>.
modifyDBInstance_deletionProtection :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Bool)
modifyDBInstance_deletionProtection = Lens.lens (\ModifyDBInstance' {deletionProtection} -> deletionProtection) (\s@ModifyDBInstance' {} a -> s {deletionProtection = a} :: ModifyDBInstance)

-- | The daily time range during which automated backups are created if
-- automated backups are enabled, as determined by the
-- @BackupRetentionPeriod@ parameter. Changing this parameter doesn\'t
-- result in an outage and the change is asynchronously applied as soon as
-- possible.
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
modifyDBInstance_preferredBackupWindow :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Text)
modifyDBInstance_preferredBackupWindow = Lens.lens (\ModifyDBInstance' {preferredBackupWindow} -> preferredBackupWindow) (\s@ModifyDBInstance' {} a -> s {preferredBackupWindow = a} :: ModifyDBInstance)

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
modifyDBInstance_dbPortNumber :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Int)
modifyDBInstance_dbPortNumber = Lens.lens (\ModifyDBInstance' {dbPortNumber} -> dbPortNumber) (\s@ModifyDBInstance' {} a -> s {dbPortNumber = a} :: ModifyDBInstance)

-- | Indicates the certificate that needs to be associated with the instance.
modifyDBInstance_cACertificateIdentifier :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Text)
modifyDBInstance_cACertificateIdentifier = Lens.lens (\ModifyDBInstance' {cACertificateIdentifier} -> cACertificateIdentifier) (\s@ModifyDBInstance' {} a -> s {cACertificateIdentifier = a} :: ModifyDBInstance)

-- | A value that indicates whether to enable Performance Insights for the DB
-- instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PerfInsights.html Using Amazon Performance Insights>
-- in the /Amazon Relational Database Service User Guide/.
modifyDBInstance_enablePerformanceInsights :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Bool)
modifyDBInstance_enablePerformanceInsights = Lens.lens (\ModifyDBInstance' {enablePerformanceInsights} -> enablePerformanceInsights) (\s@ModifyDBInstance' {} a -> s {enablePerformanceInsights = a} :: ModifyDBInstance)

-- | A list of DB security groups to authorize on this DB instance. Changing
-- this setting doesn\'t result in an outage and the change is
-- asynchronously applied as soon as possible.
--
-- Constraints:
--
-- -   If supplied, must match existing DBSecurityGroups.
modifyDBInstance_dbSecurityGroups :: Lens.Lens' ModifyDBInstance (Core.Maybe [Core.Text])
modifyDBInstance_dbSecurityGroups = Lens.lens (\ModifyDBInstance' {dbSecurityGroups} -> dbSecurityGroups) (\s@ModifyDBInstance' {} a -> s {dbSecurityGroups = a} :: ModifyDBInstance) Core.. Lens.mapping Lens._Coerce

-- | The upper limit to which Amazon RDS can automatically scale the storage
-- of the DB instance.
--
-- For more information about this setting, including limitations that
-- apply to it, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_PIOPS.StorageTypes.html#USER_PIOPS.Autoscaling Managing capacity automatically with Amazon RDS storage autoscaling>
-- in the /Amazon RDS User Guide/.
modifyDBInstance_maxAllocatedStorage :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Int)
modifyDBInstance_maxAllocatedStorage = Lens.lens (\ModifyDBInstance' {maxAllocatedStorage} -> maxAllocatedStorage) (\s@ModifyDBInstance' {} a -> s {maxAllocatedStorage = a} :: ModifyDBInstance)

-- | A value that indicates whether to enable mapping of AWS Identity and
-- Access Management (IAM) accounts to database accounts. By default,
-- mapping is disabled.
--
-- This setting doesn\'t apply to Amazon Aurora. Mapping AWS IAM accounts
-- to database accounts is managed by the DB cluster.
--
-- For more information about IAM database authentication, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication for MySQL and PostgreSQL>
-- in the /Amazon RDS User Guide./
modifyDBInstance_enableIAMDatabaseAuthentication :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Bool)
modifyDBInstance_enableIAMDatabaseAuthentication = Lens.lens (\ModifyDBInstance' {enableIAMDatabaseAuthentication} -> enableIAMDatabaseAuthentication) (\s@ModifyDBInstance' {} a -> s {enableIAMDatabaseAuthentication = a} :: ModifyDBInstance)

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
modifyDBInstance_storageType :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Text)
modifyDBInstance_storageType = Lens.lens (\ModifyDBInstance' {storageType} -> storageType) (\s@ModifyDBInstance' {} a -> s {storageType = a} :: ModifyDBInstance)

-- | A value that indicates whether the DB instance class of the DB instance
-- uses its default processor features.
modifyDBInstance_useDefaultProcessorFeatures :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Bool)
modifyDBInstance_useDefaultProcessorFeatures = Lens.lens (\ModifyDBInstance' {useDefaultProcessorFeatures} -> useDefaultProcessorFeatures) (\s@ModifyDBInstance' {} a -> s {useDefaultProcessorFeatures = a} :: ModifyDBInstance)

-- | The interval, in seconds, between points when Enhanced Monitoring
-- metrics are collected for the DB instance. To disable collecting
-- Enhanced Monitoring metrics, specify 0. The default is 0.
--
-- If @MonitoringRoleArn@ is specified, then you must also set
-- @MonitoringInterval@ to a value other than 0.
--
-- Valid Values: @0, 1, 5, 10, 15, 30, 60@
modifyDBInstance_monitoringInterval :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Int)
modifyDBInstance_monitoringInterval = Lens.lens (\ModifyDBInstance' {monitoringInterval} -> monitoringInterval) (\s@ModifyDBInstance' {} a -> s {monitoringInterval = a} :: ModifyDBInstance)

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
modifyDBInstance_optionGroupName :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Text)
modifyDBInstance_optionGroupName = Lens.lens (\ModifyDBInstance' {optionGroupName} -> optionGroupName) (\s@ModifyDBInstance' {} a -> s {optionGroupName = a} :: ModifyDBInstance)

-- | The Active Directory directory ID to move the DB instance to. Specify
-- @none@ to remove the instance from its current domain. The domain must
-- be created prior to this operation. Currently, only MySQL, Microsoft SQL
-- Server, Oracle, and PostgreSQL DB instances can be created in an Active
-- Directory Domain.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon RDS User Guide/.
modifyDBInstance_domain :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Text)
modifyDBInstance_domain = Lens.lens (\ModifyDBInstance' {domain} -> domain) (\s@ModifyDBInstance' {} a -> s {domain = a} :: ModifyDBInstance)

-- | A value that indicates whether major version upgrades are allowed.
-- Changing this parameter doesn\'t result in an outage and the change is
-- asynchronously applied as soon as possible.
--
-- Constraints: Major version upgrades must be allowed when specifying a
-- value for the EngineVersion parameter that is a different major version
-- than the DB instance\'s current version.
modifyDBInstance_allowMajorVersionUpgrade :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Bool)
modifyDBInstance_allowMajorVersionUpgrade = Lens.lens (\ModifyDBInstance' {allowMajorVersionUpgrade} -> allowMajorVersionUpgrade) (\s@ModifyDBInstance' {} a -> s {allowMajorVersionUpgrade = a} :: ModifyDBInstance)

-- | The ARN for the IAM role that permits RDS to send enhanced monitoring
-- metrics to Amazon CloudWatch Logs. For example,
-- @arn:aws:iam:123456789012:role\/emaccess@. For information on creating a
-- monitoring role, go to
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Monitoring.html#USER_Monitoring.OS.IAMRole To create an IAM role for Amazon RDS Enhanced Monitoring>
-- in the /Amazon RDS User Guide./
--
-- If @MonitoringInterval@ is set to a value other than 0, then you must
-- supply a @MonitoringRoleArn@ value.
modifyDBInstance_monitoringRoleArn :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Text)
modifyDBInstance_monitoringRoleArn = Lens.lens (\ModifyDBInstance' {monitoringRoleArn} -> monitoringRoleArn) (\s@ModifyDBInstance' {} a -> s {monitoringRoleArn = a} :: ModifyDBInstance)

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
modifyDBInstance_dbSubnetGroupName :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Text)
modifyDBInstance_dbSubnetGroupName = Lens.lens (\ModifyDBInstance' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@ModifyDBInstance' {} a -> s {dbSubnetGroupName = a} :: ModifyDBInstance)

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
modifyDBInstance_masterUserPassword :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Text)
modifyDBInstance_masterUserPassword = Lens.lens (\ModifyDBInstance' {masterUserPassword} -> masterUserPassword) (\s@ModifyDBInstance' {} a -> s {masterUserPassword = a} :: ModifyDBInstance)

-- | A value that indicates whether the DB instance is a Multi-AZ deployment.
-- Changing this parameter doesn\'t result in an outage and the change is
-- applied during the next maintenance window unless the @ApplyImmediately@
-- parameter is enabled for this request.
modifyDBInstance_multiAZ :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Bool)
modifyDBInstance_multiAZ = Lens.lens (\ModifyDBInstance' {multiAZ} -> multiAZ) (\s@ModifyDBInstance' {} a -> s {multiAZ = a} :: ModifyDBInstance)

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
modifyDBInstance_publiclyAccessible :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Bool)
modifyDBInstance_publiclyAccessible = Lens.lens (\ModifyDBInstance' {publiclyAccessible} -> publiclyAccessible) (\s@ModifyDBInstance' {} a -> s {publiclyAccessible = a} :: ModifyDBInstance)

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
modifyDBInstance_vpcSecurityGroupIds :: Lens.Lens' ModifyDBInstance (Core.Maybe [Core.Text])
modifyDBInstance_vpcSecurityGroupIds = Lens.lens (\ModifyDBInstance' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@ModifyDBInstance' {} a -> s {vpcSecurityGroupIds = a} :: ModifyDBInstance) Core.. Lens.mapping Lens._Coerce

-- | The AWS KMS key identifier for encryption of Performance Insights data.
--
-- The AWS KMS key identifier is the key ARN, key ID, alias ARN, or alias
-- name for the AWS KMS customer master key (CMK).
--
-- If you do not specify a value for @PerformanceInsightsKMSKeyId@, then
-- Amazon RDS uses your default CMK. There is a default CMK for your AWS
-- account. Your AWS account has a different default CMK for each AWS
-- Region.
modifyDBInstance_performanceInsightsKMSKeyId :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Text)
modifyDBInstance_performanceInsightsKMSKeyId = Lens.lens (\ModifyDBInstance' {performanceInsightsKMSKeyId} -> performanceInsightsKMSKeyId) (\s@ModifyDBInstance' {} a -> s {performanceInsightsKMSKeyId = a} :: ModifyDBInstance)

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
modifyDBInstance_dbParameterGroupName :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Text)
modifyDBInstance_dbParameterGroupName = Lens.lens (\ModifyDBInstance' {dbParameterGroupName} -> dbParameterGroupName) (\s@ModifyDBInstance' {} a -> s {dbParameterGroupName = a} :: ModifyDBInstance)

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
modifyDBInstance_engineVersion :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Text)
modifyDBInstance_engineVersion = Lens.lens (\ModifyDBInstance' {engineVersion} -> engineVersion) (\s@ModifyDBInstance' {} a -> s {engineVersion = a} :: ModifyDBInstance)

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
-- Default: Uses existing setting
--
-- Format: ddd:hh24:mi-ddd:hh24:mi
--
-- Valid Days: Mon | Tue | Wed | Thu | Fri | Sat | Sun
--
-- Constraints: Must be at least 30 minutes
modifyDBInstance_preferredMaintenanceWindow :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Text)
modifyDBInstance_preferredMaintenanceWindow = Lens.lens (\ModifyDBInstance' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@ModifyDBInstance' {} a -> s {preferredMaintenanceWindow = a} :: ModifyDBInstance)

-- | The amount of time, in days, to retain Performance Insights data. Valid
-- values are 7 or 731 (2 years).
modifyDBInstance_performanceInsightsRetentionPeriod :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Int)
modifyDBInstance_performanceInsightsRetentionPeriod = Lens.lens (\ModifyDBInstance' {performanceInsightsRetentionPeriod} -> performanceInsightsRetentionPeriod) (\s@ModifyDBInstance' {} a -> s {performanceInsightsRetentionPeriod = a} :: ModifyDBInstance)

-- | The license model for the DB instance.
--
-- Valid values: @license-included@ | @bring-your-own-license@ |
-- @general-public-license@
modifyDBInstance_licenseModel :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Text)
modifyDBInstance_licenseModel = Lens.lens (\ModifyDBInstance' {licenseModel} -> licenseModel) (\s@ModifyDBInstance' {} a -> s {licenseModel = a} :: ModifyDBInstance)

-- | The password for the given ARN from the key store in order to access the
-- device.
modifyDBInstance_tdeCredentialPassword :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Text)
modifyDBInstance_tdeCredentialPassword = Lens.lens (\ModifyDBInstance' {tdeCredentialPassword} -> tdeCredentialPassword) (\s@ModifyDBInstance' {} a -> s {tdeCredentialPassword = a} :: ModifyDBInstance)

-- | A value that specifies the order in which an Aurora Replica is promoted
-- to the primary instance after a failure of the existing primary
-- instance. For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster>
-- in the /Amazon Aurora User Guide/.
--
-- Default: 1
--
-- Valid Values: 0 - 15
modifyDBInstance_promotionTier :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Int)
modifyDBInstance_promotionTier = Lens.lens (\ModifyDBInstance' {promotionTier} -> promotionTier) (\s@ModifyDBInstance' {} a -> s {promotionTier = a} :: ModifyDBInstance)

-- | The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
modifyDBInstance_processorFeatures :: Lens.Lens' ModifyDBInstance (Core.Maybe [ProcessorFeature])
modifyDBInstance_processorFeatures = Lens.lens (\ModifyDBInstance' {processorFeatures} -> processorFeatures) (\s@ModifyDBInstance' {} a -> s {processorFeatures = a} :: ModifyDBInstance) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the recovery point in AWS Backup.
modifyDBInstance_awsBackupRecoveryPointArn :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Text)
modifyDBInstance_awsBackupRecoveryPointArn = Lens.lens (\ModifyDBInstance' {awsBackupRecoveryPointArn} -> awsBackupRecoveryPointArn) (\s@ModifyDBInstance' {} a -> s {awsBackupRecoveryPointArn = a} :: ModifyDBInstance)

-- | The new compute and memory capacity of the DB instance, for example,
-- @db.m4.large@. Not all DB instance classes are available in all AWS
-- Regions, or for all database engines. For the full list of DB instance
-- classes, and availability for your engine, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Concepts.DBInstanceClass.html DB Instance Class>
-- in the /Amazon RDS User Guide./
--
-- If you modify the DB instance class, an outage occurs during the change.
-- The change is applied during the next maintenance window, unless
-- @ApplyImmediately@ is enabled for this request.
--
-- Default: Uses existing setting
modifyDBInstance_dbInstanceClass :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Text)
modifyDBInstance_dbInstanceClass = Lens.lens (\ModifyDBInstance' {dbInstanceClass} -> dbInstanceClass) (\s@ModifyDBInstance' {} a -> s {dbInstanceClass = a} :: ModifyDBInstance)

-- | The name of the IAM role to use when making API calls to the Directory
-- Service.
modifyDBInstance_domainIAMRoleName :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Text)
modifyDBInstance_domainIAMRoleName = Lens.lens (\ModifyDBInstance' {domainIAMRoleName} -> domainIAMRoleName) (\s@ModifyDBInstance' {} a -> s {domainIAMRoleName = a} :: ModifyDBInstance)

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
modifyDBInstance_certificateRotationRestart :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Bool)
modifyDBInstance_certificateRotationRestart = Lens.lens (\ModifyDBInstance' {certificateRotationRestart} -> certificateRotationRestart) (\s@ModifyDBInstance' {} a -> s {certificateRotationRestart = a} :: ModifyDBInstance)

-- | The ARN from the key store with which to associate the instance for TDE
-- encryption.
modifyDBInstance_tdeCredentialArn :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Text)
modifyDBInstance_tdeCredentialArn = Lens.lens (\ModifyDBInstance' {tdeCredentialArn} -> tdeCredentialArn) (\s@ModifyDBInstance' {} a -> s {tdeCredentialArn = a} :: ModifyDBInstance)

-- | A value that indicates whether to enable a customer-owned IP address
-- (CoIP) for an RDS on Outposts DB instance.
--
-- A /CoIP/ provides local or external connectivity to resources in your
-- Outpost subnets through your on-premises network. For some use cases, a
-- CoIP can provide lower latency for connections to the DB instance from
-- outside of its virtual private cloud (VPC) on your local network.
--
-- For more information about RDS on Outposts, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/rds-on-outposts.html Working with Amazon RDS on AWS Outposts>
-- in the /Amazon RDS User Guide/.
--
-- For more information about CoIPs, see
-- <https://docs.aws.amazon.com/outposts/latest/userguide/outposts-networking-components.html#ip-addressing Customer-owned IP addresses>
-- in the /AWS Outposts User Guide/.
modifyDBInstance_enableCustomerOwnedIp :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Bool)
modifyDBInstance_enableCustomerOwnedIp = Lens.lens (\ModifyDBInstance' {enableCustomerOwnedIp} -> enableCustomerOwnedIp) (\s@ModifyDBInstance' {} a -> s {enableCustomerOwnedIp = a} :: ModifyDBInstance)

-- | The configuration setting for the log types to be enabled for export to
-- CloudWatch Logs for a specific DB instance.
--
-- A change to the @CloudwatchLogsExportConfiguration@ parameter is always
-- applied to the DB instance immediately. Therefore, the
-- @ApplyImmediately@ parameter has no effect.
modifyDBInstance_cloudwatchLogsExportConfiguration :: Lens.Lens' ModifyDBInstance (Core.Maybe CloudwatchLogsExportConfiguration)
modifyDBInstance_cloudwatchLogsExportConfiguration = Lens.lens (\ModifyDBInstance' {cloudwatchLogsExportConfiguration} -> cloudwatchLogsExportConfiguration) (\s@ModifyDBInstance' {} a -> s {cloudwatchLogsExportConfiguration = a} :: ModifyDBInstance)

-- | A value that indicates whether to copy all tags from the DB instance to
-- snapshots of the DB instance. By default, tags are not copied.
--
-- __Amazon Aurora__
--
-- Not applicable. Copying tags to snapshots is managed by the DB cluster.
-- Setting this value for an Aurora DB instance has no effect on the DB
-- cluster setting. For more information, see @ModifyDBCluster@.
modifyDBInstance_copyTagsToSnapshot :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Bool)
modifyDBInstance_copyTagsToSnapshot = Lens.lens (\ModifyDBInstance' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@ModifyDBInstance' {} a -> s {copyTagsToSnapshot = a} :: ModifyDBInstance)

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
modifyDBInstance_replicaMode :: Lens.Lens' ModifyDBInstance (Core.Maybe ReplicaMode)
modifyDBInstance_replicaMode = Lens.lens (\ModifyDBInstance' {replicaMode} -> replicaMode) (\s@ModifyDBInstance' {} a -> s {replicaMode = a} :: ModifyDBInstance)

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
modifyDBInstance_newDBInstanceIdentifier :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Text)
modifyDBInstance_newDBInstanceIdentifier = Lens.lens (\ModifyDBInstance' {newDBInstanceIdentifier'} -> newDBInstanceIdentifier') (\s@ModifyDBInstance' {} a -> s {newDBInstanceIdentifier' = a} :: ModifyDBInstance)

-- | The new amount of storage (in gibibytes) to allocate for the DB
-- instance.
--
-- For MariaDB, MySQL, Oracle, and PostgreSQL, the value supplied must be
-- at least 10% greater than the current value. Values that are not at
-- least 10% greater than the existing value are rounded up so that they
-- are 10% greater than the current value.
--
-- For the valid values for allocated storage for each engine, see
-- @CreateDBInstance@.
modifyDBInstance_allocatedStorage :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Int)
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
modifyDBInstance_applyImmediately :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Bool)
modifyDBInstance_applyImmediately = Lens.lens (\ModifyDBInstance' {applyImmediately} -> applyImmediately) (\s@ModifyDBInstance' {} a -> s {applyImmediately = a} :: ModifyDBInstance)

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
modifyDBInstance_iops :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Int)
modifyDBInstance_iops = Lens.lens (\ModifyDBInstance' {iops} -> iops) (\s@ModifyDBInstance' {} a -> s {iops = a} :: ModifyDBInstance)

-- | A value that indicates whether minor version upgrades are applied
-- automatically to the DB instance during the maintenance window. Changing
-- this parameter doesn\'t result in an outage except in the following case
-- and the change is asynchronously applied as soon as possible. An outage
-- results if this parameter is enabled during the maintenance window, and
-- a newer minor version is available, and RDS has enabled auto patching
-- for that engine version.
modifyDBInstance_autoMinorVersionUpgrade :: Lens.Lens' ModifyDBInstance (Core.Maybe Core.Bool)
modifyDBInstance_autoMinorVersionUpgrade = Lens.lens (\ModifyDBInstance' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@ModifyDBInstance' {} a -> s {autoMinorVersionUpgrade = a} :: ModifyDBInstance)

-- | The DB instance identifier. This value is stored as a lowercase string.
--
-- Constraints:
--
-- -   Must match the identifier of an existing DBInstance.
modifyDBInstance_dbInstanceIdentifier :: Lens.Lens' ModifyDBInstance Core.Text
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
            Core.<$> (x Core..@? "DBInstance")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyDBInstance

instance Core.NFData ModifyDBInstance

instance Core.ToHeaders ModifyDBInstance where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifyDBInstance where
  toPath = Core.const "/"

instance Core.ToQuery ModifyDBInstance where
  toQuery ModifyDBInstance' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ModifyDBInstance" :: Core.ByteString),
        "Version" Core.=: ("2014-10-31" :: Core.ByteString),
        "BackupRetentionPeriod"
          Core.=: backupRetentionPeriod,
        "DeletionProtection" Core.=: deletionProtection,
        "PreferredBackupWindow"
          Core.=: preferredBackupWindow,
        "DBPortNumber" Core.=: dbPortNumber,
        "CACertificateIdentifier"
          Core.=: cACertificateIdentifier,
        "EnablePerformanceInsights"
          Core.=: enablePerformanceInsights,
        "DBSecurityGroups"
          Core.=: Core.toQuery
            ( Core.toQueryList "DBSecurityGroupName"
                Core.<$> dbSecurityGroups
            ),
        "MaxAllocatedStorage" Core.=: maxAllocatedStorage,
        "EnableIAMDatabaseAuthentication"
          Core.=: enableIAMDatabaseAuthentication,
        "StorageType" Core.=: storageType,
        "UseDefaultProcessorFeatures"
          Core.=: useDefaultProcessorFeatures,
        "MonitoringInterval" Core.=: monitoringInterval,
        "OptionGroupName" Core.=: optionGroupName,
        "Domain" Core.=: domain,
        "AllowMajorVersionUpgrade"
          Core.=: allowMajorVersionUpgrade,
        "MonitoringRoleArn" Core.=: monitoringRoleArn,
        "DBSubnetGroupName" Core.=: dbSubnetGroupName,
        "MasterUserPassword" Core.=: masterUserPassword,
        "MultiAZ" Core.=: multiAZ,
        "PubliclyAccessible" Core.=: publiclyAccessible,
        "VpcSecurityGroupIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "VpcSecurityGroupId"
                Core.<$> vpcSecurityGroupIds
            ),
        "PerformanceInsightsKMSKeyId"
          Core.=: performanceInsightsKMSKeyId,
        "DBParameterGroupName" Core.=: dbParameterGroupName,
        "EngineVersion" Core.=: engineVersion,
        "PreferredMaintenanceWindow"
          Core.=: preferredMaintenanceWindow,
        "PerformanceInsightsRetentionPeriod"
          Core.=: performanceInsightsRetentionPeriod,
        "LicenseModel" Core.=: licenseModel,
        "TdeCredentialPassword"
          Core.=: tdeCredentialPassword,
        "PromotionTier" Core.=: promotionTier,
        "ProcessorFeatures"
          Core.=: Core.toQuery
            ( Core.toQueryList "ProcessorFeature"
                Core.<$> processorFeatures
            ),
        "AwsBackupRecoveryPointArn"
          Core.=: awsBackupRecoveryPointArn,
        "DBInstanceClass" Core.=: dbInstanceClass,
        "DomainIAMRoleName" Core.=: domainIAMRoleName,
        "CertificateRotationRestart"
          Core.=: certificateRotationRestart,
        "TdeCredentialArn" Core.=: tdeCredentialArn,
        "EnableCustomerOwnedIp"
          Core.=: enableCustomerOwnedIp,
        "CloudwatchLogsExportConfiguration"
          Core.=: cloudwatchLogsExportConfiguration,
        "CopyTagsToSnapshot" Core.=: copyTagsToSnapshot,
        "ReplicaMode" Core.=: replicaMode,
        "NewDBInstanceIdentifier"
          Core.=: newDBInstanceIdentifier',
        "AllocatedStorage" Core.=: allocatedStorage,
        "ApplyImmediately" Core.=: applyImmediately,
        "Iops" Core.=: iops,
        "AutoMinorVersionUpgrade"
          Core.=: autoMinorVersionUpgrade,
        "DBInstanceIdentifier" Core.=: dbInstanceIdentifier
      ]

-- | /See:/ 'newModifyDBInstanceResponse' smart constructor.
data ModifyDBInstanceResponse = ModifyDBInstanceResponse'
  { dbInstance :: Core.Maybe DBInstance,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ModifyDBInstanceResponse
newModifyDBInstanceResponse pHttpStatus_ =
  ModifyDBInstanceResponse'
    { dbInstance =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyDBInstanceResponse_dbInstance :: Lens.Lens' ModifyDBInstanceResponse (Core.Maybe DBInstance)
modifyDBInstanceResponse_dbInstance = Lens.lens (\ModifyDBInstanceResponse' {dbInstance} -> dbInstance) (\s@ModifyDBInstanceResponse' {} a -> s {dbInstance = a} :: ModifyDBInstanceResponse)

-- | The response's http status code.
modifyDBInstanceResponse_httpStatus :: Lens.Lens' ModifyDBInstanceResponse Core.Int
modifyDBInstanceResponse_httpStatus = Lens.lens (\ModifyDBInstanceResponse' {httpStatus} -> httpStatus) (\s@ModifyDBInstanceResponse' {} a -> s {httpStatus = a} :: ModifyDBInstanceResponse)

instance Core.NFData ModifyDBInstanceResponse
