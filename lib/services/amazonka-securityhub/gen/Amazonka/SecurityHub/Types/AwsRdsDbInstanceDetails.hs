{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SecurityHub.Types.AwsRdsDbInstanceDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRdsDbInstanceDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsRdsDbDomainMembership
import Amazonka.SecurityHub.Types.AwsRdsDbInstanceAssociatedRole
import Amazonka.SecurityHub.Types.AwsRdsDbInstanceEndpoint
import Amazonka.SecurityHub.Types.AwsRdsDbInstanceVpcSecurityGroup
import Amazonka.SecurityHub.Types.AwsRdsDbOptionGroupMembership
import Amazonka.SecurityHub.Types.AwsRdsDbParameterGroup
import Amazonka.SecurityHub.Types.AwsRdsDbPendingModifiedValues
import Amazonka.SecurityHub.Types.AwsRdsDbProcessorFeature
import Amazonka.SecurityHub.Types.AwsRdsDbStatusInfo
import Amazonka.SecurityHub.Types.AwsRdsDbSubnetGroup

-- | Contains the details of an Amazon RDS DB instance.
--
-- /See:/ 'newAwsRdsDbInstanceDetails' smart constructor.
data AwsRdsDbInstanceDetails = AwsRdsDbInstanceDetails'
  { -- | The amount of storage (in gigabytes) to initially allocate for the DB
    -- instance.
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | The IAM roles associated with the DB instance.
    associatedRoles :: Prelude.Maybe [AwsRdsDbInstanceAssociatedRole],
    -- | Indicates whether minor version patches are applied automatically.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The Availability Zone where the DB instance will be created.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The number of days for which to retain automated backups.
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The identifier of the CA certificate for this DB instance.
    cACertificateIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The name of the character set that this DB instance is associated with.
    characterSetName :: Prelude.Maybe Prelude.Text,
    -- | Whether to copy resource tags to snapshots of the DB instance.
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | If the DB instance is a member of a DB cluster, contains the name of the
    -- DB cluster that the DB instance is a member of.
    dbClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Contains the name of the compute and memory capacity class of the DB
    -- instance.
    dbInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | Contains a user-supplied database identifier. This identifier is the
    -- unique key that identifies a DB instance.
    dbInstanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The meaning of this parameter differs according to the database engine
    -- you use.
    --
    -- __MySQL, MariaDB, SQL Server, PostgreSQL__
    --
    -- Contains the name of the initial database of this instance that was
    -- provided at create time, if one was specified when the DB instance was
    -- created. This same name is returned for the life of the DB instance.
    --
    -- __Oracle__
    --
    -- Contains the Oracle System ID (SID) of the created DB instance. Not
    -- shown when the returned parameters do not apply to an Oracle DB
    -- instance.
    dbName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the port that the DB instance listens on. If the DB instance
    -- is part of a DB cluster, this can be a different port than the DB
    -- cluster port.
    dbInstancePort :: Prelude.Maybe Prelude.Int,
    -- | The current status of the DB instance.
    dbInstanceStatus :: Prelude.Maybe Prelude.Text,
    -- | A list of the DB parameter groups to assign to the DB instance.
    dbParameterGroups :: Prelude.Maybe [AwsRdsDbParameterGroup],
    -- | A list of the DB security groups to assign to the DB instance.
    dbSecurityGroups :: Prelude.Maybe [Prelude.Text],
    -- | Information about the subnet group that is associated with the DB
    -- instance.
    dbSubnetGroup :: Prelude.Maybe AwsRdsDbSubnetGroup,
    -- | The Amazon Web Services Region-unique, immutable identifier for the DB
    -- instance. This identifier is found in CloudTrail log entries whenever
    -- the KMS key for the DB instance is accessed.
    dbiResourceId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the DB instance has deletion protection enabled.
    --
    -- When deletion protection is enabled, the database cannot be deleted.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | The Active Directory domain membership records associated with the DB
    -- instance.
    domainMemberships :: Prelude.Maybe [AwsRdsDbDomainMembership],
    -- | A list of log types that this DB instance is configured to export to
    -- CloudWatch Logs.
    enabledCloudWatchLogsExports :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the connection endpoint.
    endpoint :: Prelude.Maybe AwsRdsDbInstanceEndpoint,
    -- | Provides the name of the database engine to use for this DB instance.
    engine :: Prelude.Maybe Prelude.Text,
    -- | Indicates the database engine version.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the CloudWatch Logs log stream that receives the enhanced
    -- monitoring metrics data for the DB instance.
    enhancedMonitoringResourceArn :: Prelude.Maybe Prelude.Text,
    -- | True if mapping of IAM accounts to database accounts is enabled, and
    -- otherwise false.
    --
    -- IAM database authentication can be enabled for the following database
    -- engines.
    --
    -- -   For MySQL 5.6, minor version 5.6.34 or higher
    --
    -- -   For MySQL 5.7, minor version 5.7.16 or higher
    --
    -- -   Aurora 5.6 or higher
    iAMDatabaseAuthenticationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Indicates when the DB instance was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    instanceCreateTime :: Prelude.Maybe Prelude.Text,
    -- | Specifies the provisioned IOPS (I\/O operations per second) for this DB
    -- instance.
    iops :: Prelude.Maybe Prelude.Int,
    -- | If @StorageEncrypted@ is true, the KMS key identifier for the encrypted
    -- DB instance.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the latest time to which a database can be restored with
    -- point-in-time restore.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    latestRestorableTime :: Prelude.Maybe Prelude.Text,
    -- | License model information for this DB instance.
    licenseModel :: Prelude.Maybe Prelude.Text,
    listenerEndpoint :: Prelude.Maybe AwsRdsDbInstanceEndpoint,
    -- | The master user name of the DB instance.
    masterUsername :: Prelude.Maybe Prelude.Text,
    -- | The upper limit to which Amazon RDS can automatically scale the storage
    -- of the DB instance.
    maxAllocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | The interval, in seconds, between points when enhanced monitoring
    -- metrics are collected for the DB instance.
    monitoringInterval :: Prelude.Maybe Prelude.Int,
    -- | The ARN for the IAM role that permits Amazon RDS to send enhanced
    -- monitoring metrics to CloudWatch Logs.
    monitoringRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Whether the DB instance is a multiple Availability Zone deployment.
    multiAz :: Prelude.Maybe Prelude.Bool,
    -- | The list of option group memberships for this DB instance.
    optionGroupMemberships :: Prelude.Maybe [AwsRdsDbOptionGroupMembership],
    -- | Changes to the DB instance that are currently pending.
    pendingModifiedValues :: Prelude.Maybe AwsRdsDbPendingModifiedValues,
    -- | Indicates whether Performance Insights is enabled for the DB instance.
    performanceInsightsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of the KMS key used to encrypt the Performance Insights
    -- data.
    performanceInsightsKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The number of days to retain Performance Insights data.
    performanceInsightsRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The range of time each day when automated backups are created, if
    -- automated backups are enabled.
    --
    -- Uses the format @HH:MM-HH:MM@. For example, @04:52-05:22@.
    preferredBackupWindow :: Prelude.Maybe Prelude.Text,
    -- | The weekly time range during which system maintenance can occur, in
    -- Universal Coordinated Time (UTC).
    --
    -- Uses the format @\<day>:HH:MM-\<day>:HH:MM@.
    --
    -- For the day values, use @mon@|@tue@|@wed@|@thu@|@fri@|@sat@|@sun@.
    --
    -- For example, @sun:09:32-sun:10:02@.
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | The number of CPU cores and the number of threads per core for the DB
    -- instance class of the DB instance.
    processorFeatures :: Prelude.Maybe [AwsRdsDbProcessorFeature],
    -- | The order in which to promote an Aurora replica to the primary instance
    -- after a failure of the existing primary instance.
    promotionTier :: Prelude.Maybe Prelude.Int,
    -- | Specifies the accessibility options for the DB instance.
    --
    -- A value of true specifies an Internet-facing instance with a publicly
    -- resolvable DNS name, which resolves to a public IP address.
    --
    -- A value of false specifies an internal instance with a DNS name that
    -- resolves to a private IP address.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | List of identifiers of Aurora DB clusters to which the RDS DB instance
    -- is replicated as a read replica.
    readReplicaDBClusterIdentifiers :: Prelude.Maybe [Prelude.Text],
    -- | List of identifiers of the read replicas associated with this DB
    -- instance.
    readReplicaDBInstanceIdentifiers :: Prelude.Maybe [Prelude.Text],
    -- | If this DB instance is a read replica, contains the identifier of the
    -- source DB instance.
    readReplicaSourceDBInstanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | For a DB instance with multi-Availability Zone support, the name of the
    -- secondary Availability Zone.
    secondaryAvailabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The status of a read replica. If the instance isn\'t a read replica,
    -- this is empty.
    statusInfos :: Prelude.Maybe [AwsRdsDbStatusInfo],
    -- | Specifies whether the DB instance is encrypted.
    storageEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | The storage type for the DB instance.
    storageType :: Prelude.Maybe Prelude.Text,
    -- | The ARN from the key store with which the instance is associated for TDE
    -- encryption.
    tdeCredentialArn :: Prelude.Maybe Prelude.Text,
    -- | The time zone of the DB instance.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | A list of VPC security groups that the DB instance belongs to.
    vpcSecurityGroups :: Prelude.Maybe [AwsRdsDbInstanceVpcSecurityGroup]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRdsDbInstanceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allocatedStorage', 'awsRdsDbInstanceDetails_allocatedStorage' - The amount of storage (in gigabytes) to initially allocate for the DB
-- instance.
--
-- 'associatedRoles', 'awsRdsDbInstanceDetails_associatedRoles' - The IAM roles associated with the DB instance.
--
-- 'autoMinorVersionUpgrade', 'awsRdsDbInstanceDetails_autoMinorVersionUpgrade' - Indicates whether minor version patches are applied automatically.
--
-- 'availabilityZone', 'awsRdsDbInstanceDetails_availabilityZone' - The Availability Zone where the DB instance will be created.
--
-- 'backupRetentionPeriod', 'awsRdsDbInstanceDetails_backupRetentionPeriod' - The number of days for which to retain automated backups.
--
-- 'cACertificateIdentifier', 'awsRdsDbInstanceDetails_cACertificateIdentifier' - The identifier of the CA certificate for this DB instance.
--
-- 'characterSetName', 'awsRdsDbInstanceDetails_characterSetName' - The name of the character set that this DB instance is associated with.
--
-- 'copyTagsToSnapshot', 'awsRdsDbInstanceDetails_copyTagsToSnapshot' - Whether to copy resource tags to snapshots of the DB instance.
--
-- 'dbClusterIdentifier', 'awsRdsDbInstanceDetails_dbClusterIdentifier' - If the DB instance is a member of a DB cluster, contains the name of the
-- DB cluster that the DB instance is a member of.
--
-- 'dbInstanceClass', 'awsRdsDbInstanceDetails_dbInstanceClass' - Contains the name of the compute and memory capacity class of the DB
-- instance.
--
-- 'dbInstanceIdentifier', 'awsRdsDbInstanceDetails_dbInstanceIdentifier' - Contains a user-supplied database identifier. This identifier is the
-- unique key that identifies a DB instance.
--
-- 'dbName', 'awsRdsDbInstanceDetails_dbName' - The meaning of this parameter differs according to the database engine
-- you use.
--
-- __MySQL, MariaDB, SQL Server, PostgreSQL__
--
-- Contains the name of the initial database of this instance that was
-- provided at create time, if one was specified when the DB instance was
-- created. This same name is returned for the life of the DB instance.
--
-- __Oracle__
--
-- Contains the Oracle System ID (SID) of the created DB instance. Not
-- shown when the returned parameters do not apply to an Oracle DB
-- instance.
--
-- 'dbInstancePort', 'awsRdsDbInstanceDetails_dbInstancePort' - Specifies the port that the DB instance listens on. If the DB instance
-- is part of a DB cluster, this can be a different port than the DB
-- cluster port.
--
-- 'dbInstanceStatus', 'awsRdsDbInstanceDetails_dbInstanceStatus' - The current status of the DB instance.
--
-- 'dbParameterGroups', 'awsRdsDbInstanceDetails_dbParameterGroups' - A list of the DB parameter groups to assign to the DB instance.
--
-- 'dbSecurityGroups', 'awsRdsDbInstanceDetails_dbSecurityGroups' - A list of the DB security groups to assign to the DB instance.
--
-- 'dbSubnetGroup', 'awsRdsDbInstanceDetails_dbSubnetGroup' - Information about the subnet group that is associated with the DB
-- instance.
--
-- 'dbiResourceId', 'awsRdsDbInstanceDetails_dbiResourceId' - The Amazon Web Services Region-unique, immutable identifier for the DB
-- instance. This identifier is found in CloudTrail log entries whenever
-- the KMS key for the DB instance is accessed.
--
-- 'deletionProtection', 'awsRdsDbInstanceDetails_deletionProtection' - Indicates whether the DB instance has deletion protection enabled.
--
-- When deletion protection is enabled, the database cannot be deleted.
--
-- 'domainMemberships', 'awsRdsDbInstanceDetails_domainMemberships' - The Active Directory domain membership records associated with the DB
-- instance.
--
-- 'enabledCloudWatchLogsExports', 'awsRdsDbInstanceDetails_enabledCloudWatchLogsExports' - A list of log types that this DB instance is configured to export to
-- CloudWatch Logs.
--
-- 'endpoint', 'awsRdsDbInstanceDetails_endpoint' - Specifies the connection endpoint.
--
-- 'engine', 'awsRdsDbInstanceDetails_engine' - Provides the name of the database engine to use for this DB instance.
--
-- 'engineVersion', 'awsRdsDbInstanceDetails_engineVersion' - Indicates the database engine version.
--
-- 'enhancedMonitoringResourceArn', 'awsRdsDbInstanceDetails_enhancedMonitoringResourceArn' - The ARN of the CloudWatch Logs log stream that receives the enhanced
-- monitoring metrics data for the DB instance.
--
-- 'iAMDatabaseAuthenticationEnabled', 'awsRdsDbInstanceDetails_iAMDatabaseAuthenticationEnabled' - True if mapping of IAM accounts to database accounts is enabled, and
-- otherwise false.
--
-- IAM database authentication can be enabled for the following database
-- engines.
--
-- -   For MySQL 5.6, minor version 5.6.34 or higher
--
-- -   For MySQL 5.7, minor version 5.7.16 or higher
--
-- -   Aurora 5.6 or higher
--
-- 'instanceCreateTime', 'awsRdsDbInstanceDetails_instanceCreateTime' - Indicates when the DB instance was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'iops', 'awsRdsDbInstanceDetails_iops' - Specifies the provisioned IOPS (I\/O operations per second) for this DB
-- instance.
--
-- 'kmsKeyId', 'awsRdsDbInstanceDetails_kmsKeyId' - If @StorageEncrypted@ is true, the KMS key identifier for the encrypted
-- DB instance.
--
-- 'latestRestorableTime', 'awsRdsDbInstanceDetails_latestRestorableTime' - Specifies the latest time to which a database can be restored with
-- point-in-time restore.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'licenseModel', 'awsRdsDbInstanceDetails_licenseModel' - License model information for this DB instance.
--
-- 'listenerEndpoint', 'awsRdsDbInstanceDetails_listenerEndpoint' - Undocumented member.
--
-- 'masterUsername', 'awsRdsDbInstanceDetails_masterUsername' - The master user name of the DB instance.
--
-- 'maxAllocatedStorage', 'awsRdsDbInstanceDetails_maxAllocatedStorage' - The upper limit to which Amazon RDS can automatically scale the storage
-- of the DB instance.
--
-- 'monitoringInterval', 'awsRdsDbInstanceDetails_monitoringInterval' - The interval, in seconds, between points when enhanced monitoring
-- metrics are collected for the DB instance.
--
-- 'monitoringRoleArn', 'awsRdsDbInstanceDetails_monitoringRoleArn' - The ARN for the IAM role that permits Amazon RDS to send enhanced
-- monitoring metrics to CloudWatch Logs.
--
-- 'multiAz', 'awsRdsDbInstanceDetails_multiAz' - Whether the DB instance is a multiple Availability Zone deployment.
--
-- 'optionGroupMemberships', 'awsRdsDbInstanceDetails_optionGroupMemberships' - The list of option group memberships for this DB instance.
--
-- 'pendingModifiedValues', 'awsRdsDbInstanceDetails_pendingModifiedValues' - Changes to the DB instance that are currently pending.
--
-- 'performanceInsightsEnabled', 'awsRdsDbInstanceDetails_performanceInsightsEnabled' - Indicates whether Performance Insights is enabled for the DB instance.
--
-- 'performanceInsightsKmsKeyId', 'awsRdsDbInstanceDetails_performanceInsightsKmsKeyId' - The identifier of the KMS key used to encrypt the Performance Insights
-- data.
--
-- 'performanceInsightsRetentionPeriod', 'awsRdsDbInstanceDetails_performanceInsightsRetentionPeriod' - The number of days to retain Performance Insights data.
--
-- 'preferredBackupWindow', 'awsRdsDbInstanceDetails_preferredBackupWindow' - The range of time each day when automated backups are created, if
-- automated backups are enabled.
--
-- Uses the format @HH:MM-HH:MM@. For example, @04:52-05:22@.
--
-- 'preferredMaintenanceWindow', 'awsRdsDbInstanceDetails_preferredMaintenanceWindow' - The weekly time range during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- Uses the format @\<day>:HH:MM-\<day>:HH:MM@.
--
-- For the day values, use @mon@|@tue@|@wed@|@thu@|@fri@|@sat@|@sun@.
--
-- For example, @sun:09:32-sun:10:02@.
--
-- 'processorFeatures', 'awsRdsDbInstanceDetails_processorFeatures' - The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
--
-- 'promotionTier', 'awsRdsDbInstanceDetails_promotionTier' - The order in which to promote an Aurora replica to the primary instance
-- after a failure of the existing primary instance.
--
-- 'publiclyAccessible', 'awsRdsDbInstanceDetails_publiclyAccessible' - Specifies the accessibility options for the DB instance.
--
-- A value of true specifies an Internet-facing instance with a publicly
-- resolvable DNS name, which resolves to a public IP address.
--
-- A value of false specifies an internal instance with a DNS name that
-- resolves to a private IP address.
--
-- 'readReplicaDBClusterIdentifiers', 'awsRdsDbInstanceDetails_readReplicaDBClusterIdentifiers' - List of identifiers of Aurora DB clusters to which the RDS DB instance
-- is replicated as a read replica.
--
-- 'readReplicaDBInstanceIdentifiers', 'awsRdsDbInstanceDetails_readReplicaDBInstanceIdentifiers' - List of identifiers of the read replicas associated with this DB
-- instance.
--
-- 'readReplicaSourceDBInstanceIdentifier', 'awsRdsDbInstanceDetails_readReplicaSourceDBInstanceIdentifier' - If this DB instance is a read replica, contains the identifier of the
-- source DB instance.
--
-- 'secondaryAvailabilityZone', 'awsRdsDbInstanceDetails_secondaryAvailabilityZone' - For a DB instance with multi-Availability Zone support, the name of the
-- secondary Availability Zone.
--
-- 'statusInfos', 'awsRdsDbInstanceDetails_statusInfos' - The status of a read replica. If the instance isn\'t a read replica,
-- this is empty.
--
-- 'storageEncrypted', 'awsRdsDbInstanceDetails_storageEncrypted' - Specifies whether the DB instance is encrypted.
--
-- 'storageType', 'awsRdsDbInstanceDetails_storageType' - The storage type for the DB instance.
--
-- 'tdeCredentialArn', 'awsRdsDbInstanceDetails_tdeCredentialArn' - The ARN from the key store with which the instance is associated for TDE
-- encryption.
--
-- 'timezone', 'awsRdsDbInstanceDetails_timezone' - The time zone of the DB instance.
--
-- 'vpcSecurityGroups', 'awsRdsDbInstanceDetails_vpcSecurityGroups' - A list of VPC security groups that the DB instance belongs to.
newAwsRdsDbInstanceDetails ::
  AwsRdsDbInstanceDetails
newAwsRdsDbInstanceDetails =
  AwsRdsDbInstanceDetails'
    { allocatedStorage =
        Prelude.Nothing,
      associatedRoles = Prelude.Nothing,
      autoMinorVersionUpgrade = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      backupRetentionPeriod = Prelude.Nothing,
      cACertificateIdentifier = Prelude.Nothing,
      characterSetName = Prelude.Nothing,
      copyTagsToSnapshot = Prelude.Nothing,
      dbClusterIdentifier = Prelude.Nothing,
      dbInstanceClass = Prelude.Nothing,
      dbInstanceIdentifier = Prelude.Nothing,
      dbName = Prelude.Nothing,
      dbInstancePort = Prelude.Nothing,
      dbInstanceStatus = Prelude.Nothing,
      dbParameterGroups = Prelude.Nothing,
      dbSecurityGroups = Prelude.Nothing,
      dbSubnetGroup = Prelude.Nothing,
      dbiResourceId = Prelude.Nothing,
      deletionProtection = Prelude.Nothing,
      domainMemberships = Prelude.Nothing,
      enabledCloudWatchLogsExports = Prelude.Nothing,
      endpoint = Prelude.Nothing,
      engine = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      enhancedMonitoringResourceArn = Prelude.Nothing,
      iAMDatabaseAuthenticationEnabled = Prelude.Nothing,
      instanceCreateTime = Prelude.Nothing,
      iops = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      latestRestorableTime = Prelude.Nothing,
      licenseModel = Prelude.Nothing,
      listenerEndpoint = Prelude.Nothing,
      masterUsername = Prelude.Nothing,
      maxAllocatedStorage = Prelude.Nothing,
      monitoringInterval = Prelude.Nothing,
      monitoringRoleArn = Prelude.Nothing,
      multiAz = Prelude.Nothing,
      optionGroupMemberships = Prelude.Nothing,
      pendingModifiedValues = Prelude.Nothing,
      performanceInsightsEnabled = Prelude.Nothing,
      performanceInsightsKmsKeyId = Prelude.Nothing,
      performanceInsightsRetentionPeriod =
        Prelude.Nothing,
      preferredBackupWindow = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      processorFeatures = Prelude.Nothing,
      promotionTier = Prelude.Nothing,
      publiclyAccessible = Prelude.Nothing,
      readReplicaDBClusterIdentifiers = Prelude.Nothing,
      readReplicaDBInstanceIdentifiers = Prelude.Nothing,
      readReplicaSourceDBInstanceIdentifier =
        Prelude.Nothing,
      secondaryAvailabilityZone = Prelude.Nothing,
      statusInfos = Prelude.Nothing,
      storageEncrypted = Prelude.Nothing,
      storageType = Prelude.Nothing,
      tdeCredentialArn = Prelude.Nothing,
      timezone = Prelude.Nothing,
      vpcSecurityGroups = Prelude.Nothing
    }

-- | The amount of storage (in gigabytes) to initially allocate for the DB
-- instance.
awsRdsDbInstanceDetails_allocatedStorage :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Int)
awsRdsDbInstanceDetails_allocatedStorage = Lens.lens (\AwsRdsDbInstanceDetails' {allocatedStorage} -> allocatedStorage) (\s@AwsRdsDbInstanceDetails' {} a -> s {allocatedStorage = a} :: AwsRdsDbInstanceDetails)

-- | The IAM roles associated with the DB instance.
awsRdsDbInstanceDetails_associatedRoles :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe [AwsRdsDbInstanceAssociatedRole])
awsRdsDbInstanceDetails_associatedRoles = Lens.lens (\AwsRdsDbInstanceDetails' {associatedRoles} -> associatedRoles) (\s@AwsRdsDbInstanceDetails' {} a -> s {associatedRoles = a} :: AwsRdsDbInstanceDetails) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether minor version patches are applied automatically.
awsRdsDbInstanceDetails_autoMinorVersionUpgrade :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Bool)
awsRdsDbInstanceDetails_autoMinorVersionUpgrade = Lens.lens (\AwsRdsDbInstanceDetails' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@AwsRdsDbInstanceDetails' {} a -> s {autoMinorVersionUpgrade = a} :: AwsRdsDbInstanceDetails)

-- | The Availability Zone where the DB instance will be created.
awsRdsDbInstanceDetails_availabilityZone :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Text)
awsRdsDbInstanceDetails_availabilityZone = Lens.lens (\AwsRdsDbInstanceDetails' {availabilityZone} -> availabilityZone) (\s@AwsRdsDbInstanceDetails' {} a -> s {availabilityZone = a} :: AwsRdsDbInstanceDetails)

-- | The number of days for which to retain automated backups.
awsRdsDbInstanceDetails_backupRetentionPeriod :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Int)
awsRdsDbInstanceDetails_backupRetentionPeriod = Lens.lens (\AwsRdsDbInstanceDetails' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@AwsRdsDbInstanceDetails' {} a -> s {backupRetentionPeriod = a} :: AwsRdsDbInstanceDetails)

-- | The identifier of the CA certificate for this DB instance.
awsRdsDbInstanceDetails_cACertificateIdentifier :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Text)
awsRdsDbInstanceDetails_cACertificateIdentifier = Lens.lens (\AwsRdsDbInstanceDetails' {cACertificateIdentifier} -> cACertificateIdentifier) (\s@AwsRdsDbInstanceDetails' {} a -> s {cACertificateIdentifier = a} :: AwsRdsDbInstanceDetails)

-- | The name of the character set that this DB instance is associated with.
awsRdsDbInstanceDetails_characterSetName :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Text)
awsRdsDbInstanceDetails_characterSetName = Lens.lens (\AwsRdsDbInstanceDetails' {characterSetName} -> characterSetName) (\s@AwsRdsDbInstanceDetails' {} a -> s {characterSetName = a} :: AwsRdsDbInstanceDetails)

-- | Whether to copy resource tags to snapshots of the DB instance.
awsRdsDbInstanceDetails_copyTagsToSnapshot :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Bool)
awsRdsDbInstanceDetails_copyTagsToSnapshot = Lens.lens (\AwsRdsDbInstanceDetails' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@AwsRdsDbInstanceDetails' {} a -> s {copyTagsToSnapshot = a} :: AwsRdsDbInstanceDetails)

-- | If the DB instance is a member of a DB cluster, contains the name of the
-- DB cluster that the DB instance is a member of.
awsRdsDbInstanceDetails_dbClusterIdentifier :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Text)
awsRdsDbInstanceDetails_dbClusterIdentifier = Lens.lens (\AwsRdsDbInstanceDetails' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@AwsRdsDbInstanceDetails' {} a -> s {dbClusterIdentifier = a} :: AwsRdsDbInstanceDetails)

-- | Contains the name of the compute and memory capacity class of the DB
-- instance.
awsRdsDbInstanceDetails_dbInstanceClass :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Text)
awsRdsDbInstanceDetails_dbInstanceClass = Lens.lens (\AwsRdsDbInstanceDetails' {dbInstanceClass} -> dbInstanceClass) (\s@AwsRdsDbInstanceDetails' {} a -> s {dbInstanceClass = a} :: AwsRdsDbInstanceDetails)

-- | Contains a user-supplied database identifier. This identifier is the
-- unique key that identifies a DB instance.
awsRdsDbInstanceDetails_dbInstanceIdentifier :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Text)
awsRdsDbInstanceDetails_dbInstanceIdentifier = Lens.lens (\AwsRdsDbInstanceDetails' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@AwsRdsDbInstanceDetails' {} a -> s {dbInstanceIdentifier = a} :: AwsRdsDbInstanceDetails)

-- | The meaning of this parameter differs according to the database engine
-- you use.
--
-- __MySQL, MariaDB, SQL Server, PostgreSQL__
--
-- Contains the name of the initial database of this instance that was
-- provided at create time, if one was specified when the DB instance was
-- created. This same name is returned for the life of the DB instance.
--
-- __Oracle__
--
-- Contains the Oracle System ID (SID) of the created DB instance. Not
-- shown when the returned parameters do not apply to an Oracle DB
-- instance.
awsRdsDbInstanceDetails_dbName :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Text)
awsRdsDbInstanceDetails_dbName = Lens.lens (\AwsRdsDbInstanceDetails' {dbName} -> dbName) (\s@AwsRdsDbInstanceDetails' {} a -> s {dbName = a} :: AwsRdsDbInstanceDetails)

-- | Specifies the port that the DB instance listens on. If the DB instance
-- is part of a DB cluster, this can be a different port than the DB
-- cluster port.
awsRdsDbInstanceDetails_dbInstancePort :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Int)
awsRdsDbInstanceDetails_dbInstancePort = Lens.lens (\AwsRdsDbInstanceDetails' {dbInstancePort} -> dbInstancePort) (\s@AwsRdsDbInstanceDetails' {} a -> s {dbInstancePort = a} :: AwsRdsDbInstanceDetails)

-- | The current status of the DB instance.
awsRdsDbInstanceDetails_dbInstanceStatus :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Text)
awsRdsDbInstanceDetails_dbInstanceStatus = Lens.lens (\AwsRdsDbInstanceDetails' {dbInstanceStatus} -> dbInstanceStatus) (\s@AwsRdsDbInstanceDetails' {} a -> s {dbInstanceStatus = a} :: AwsRdsDbInstanceDetails)

-- | A list of the DB parameter groups to assign to the DB instance.
awsRdsDbInstanceDetails_dbParameterGroups :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe [AwsRdsDbParameterGroup])
awsRdsDbInstanceDetails_dbParameterGroups = Lens.lens (\AwsRdsDbInstanceDetails' {dbParameterGroups} -> dbParameterGroups) (\s@AwsRdsDbInstanceDetails' {} a -> s {dbParameterGroups = a} :: AwsRdsDbInstanceDetails) Prelude.. Lens.mapping Lens.coerced

-- | A list of the DB security groups to assign to the DB instance.
awsRdsDbInstanceDetails_dbSecurityGroups :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe [Prelude.Text])
awsRdsDbInstanceDetails_dbSecurityGroups = Lens.lens (\AwsRdsDbInstanceDetails' {dbSecurityGroups} -> dbSecurityGroups) (\s@AwsRdsDbInstanceDetails' {} a -> s {dbSecurityGroups = a} :: AwsRdsDbInstanceDetails) Prelude.. Lens.mapping Lens.coerced

-- | Information about the subnet group that is associated with the DB
-- instance.
awsRdsDbInstanceDetails_dbSubnetGroup :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe AwsRdsDbSubnetGroup)
awsRdsDbInstanceDetails_dbSubnetGroup = Lens.lens (\AwsRdsDbInstanceDetails' {dbSubnetGroup} -> dbSubnetGroup) (\s@AwsRdsDbInstanceDetails' {} a -> s {dbSubnetGroup = a} :: AwsRdsDbInstanceDetails)

-- | The Amazon Web Services Region-unique, immutable identifier for the DB
-- instance. This identifier is found in CloudTrail log entries whenever
-- the KMS key for the DB instance is accessed.
awsRdsDbInstanceDetails_dbiResourceId :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Text)
awsRdsDbInstanceDetails_dbiResourceId = Lens.lens (\AwsRdsDbInstanceDetails' {dbiResourceId} -> dbiResourceId) (\s@AwsRdsDbInstanceDetails' {} a -> s {dbiResourceId = a} :: AwsRdsDbInstanceDetails)

-- | Indicates whether the DB instance has deletion protection enabled.
--
-- When deletion protection is enabled, the database cannot be deleted.
awsRdsDbInstanceDetails_deletionProtection :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Bool)
awsRdsDbInstanceDetails_deletionProtection = Lens.lens (\AwsRdsDbInstanceDetails' {deletionProtection} -> deletionProtection) (\s@AwsRdsDbInstanceDetails' {} a -> s {deletionProtection = a} :: AwsRdsDbInstanceDetails)

-- | The Active Directory domain membership records associated with the DB
-- instance.
awsRdsDbInstanceDetails_domainMemberships :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe [AwsRdsDbDomainMembership])
awsRdsDbInstanceDetails_domainMemberships = Lens.lens (\AwsRdsDbInstanceDetails' {domainMemberships} -> domainMemberships) (\s@AwsRdsDbInstanceDetails' {} a -> s {domainMemberships = a} :: AwsRdsDbInstanceDetails) Prelude.. Lens.mapping Lens.coerced

-- | A list of log types that this DB instance is configured to export to
-- CloudWatch Logs.
awsRdsDbInstanceDetails_enabledCloudWatchLogsExports :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe [Prelude.Text])
awsRdsDbInstanceDetails_enabledCloudWatchLogsExports = Lens.lens (\AwsRdsDbInstanceDetails' {enabledCloudWatchLogsExports} -> enabledCloudWatchLogsExports) (\s@AwsRdsDbInstanceDetails' {} a -> s {enabledCloudWatchLogsExports = a} :: AwsRdsDbInstanceDetails) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the connection endpoint.
awsRdsDbInstanceDetails_endpoint :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe AwsRdsDbInstanceEndpoint)
awsRdsDbInstanceDetails_endpoint = Lens.lens (\AwsRdsDbInstanceDetails' {endpoint} -> endpoint) (\s@AwsRdsDbInstanceDetails' {} a -> s {endpoint = a} :: AwsRdsDbInstanceDetails)

-- | Provides the name of the database engine to use for this DB instance.
awsRdsDbInstanceDetails_engine :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Text)
awsRdsDbInstanceDetails_engine = Lens.lens (\AwsRdsDbInstanceDetails' {engine} -> engine) (\s@AwsRdsDbInstanceDetails' {} a -> s {engine = a} :: AwsRdsDbInstanceDetails)

-- | Indicates the database engine version.
awsRdsDbInstanceDetails_engineVersion :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Text)
awsRdsDbInstanceDetails_engineVersion = Lens.lens (\AwsRdsDbInstanceDetails' {engineVersion} -> engineVersion) (\s@AwsRdsDbInstanceDetails' {} a -> s {engineVersion = a} :: AwsRdsDbInstanceDetails)

-- | The ARN of the CloudWatch Logs log stream that receives the enhanced
-- monitoring metrics data for the DB instance.
awsRdsDbInstanceDetails_enhancedMonitoringResourceArn :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Text)
awsRdsDbInstanceDetails_enhancedMonitoringResourceArn = Lens.lens (\AwsRdsDbInstanceDetails' {enhancedMonitoringResourceArn} -> enhancedMonitoringResourceArn) (\s@AwsRdsDbInstanceDetails' {} a -> s {enhancedMonitoringResourceArn = a} :: AwsRdsDbInstanceDetails)

-- | True if mapping of IAM accounts to database accounts is enabled, and
-- otherwise false.
--
-- IAM database authentication can be enabled for the following database
-- engines.
--
-- -   For MySQL 5.6, minor version 5.6.34 or higher
--
-- -   For MySQL 5.7, minor version 5.7.16 or higher
--
-- -   Aurora 5.6 or higher
awsRdsDbInstanceDetails_iAMDatabaseAuthenticationEnabled :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Bool)
awsRdsDbInstanceDetails_iAMDatabaseAuthenticationEnabled = Lens.lens (\AwsRdsDbInstanceDetails' {iAMDatabaseAuthenticationEnabled} -> iAMDatabaseAuthenticationEnabled) (\s@AwsRdsDbInstanceDetails' {} a -> s {iAMDatabaseAuthenticationEnabled = a} :: AwsRdsDbInstanceDetails)

-- | Indicates when the DB instance was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsRdsDbInstanceDetails_instanceCreateTime :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Text)
awsRdsDbInstanceDetails_instanceCreateTime = Lens.lens (\AwsRdsDbInstanceDetails' {instanceCreateTime} -> instanceCreateTime) (\s@AwsRdsDbInstanceDetails' {} a -> s {instanceCreateTime = a} :: AwsRdsDbInstanceDetails)

-- | Specifies the provisioned IOPS (I\/O operations per second) for this DB
-- instance.
awsRdsDbInstanceDetails_iops :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Int)
awsRdsDbInstanceDetails_iops = Lens.lens (\AwsRdsDbInstanceDetails' {iops} -> iops) (\s@AwsRdsDbInstanceDetails' {} a -> s {iops = a} :: AwsRdsDbInstanceDetails)

-- | If @StorageEncrypted@ is true, the KMS key identifier for the encrypted
-- DB instance.
awsRdsDbInstanceDetails_kmsKeyId :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Text)
awsRdsDbInstanceDetails_kmsKeyId = Lens.lens (\AwsRdsDbInstanceDetails' {kmsKeyId} -> kmsKeyId) (\s@AwsRdsDbInstanceDetails' {} a -> s {kmsKeyId = a} :: AwsRdsDbInstanceDetails)

-- | Specifies the latest time to which a database can be restored with
-- point-in-time restore.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsRdsDbInstanceDetails_latestRestorableTime :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Text)
awsRdsDbInstanceDetails_latestRestorableTime = Lens.lens (\AwsRdsDbInstanceDetails' {latestRestorableTime} -> latestRestorableTime) (\s@AwsRdsDbInstanceDetails' {} a -> s {latestRestorableTime = a} :: AwsRdsDbInstanceDetails)

-- | License model information for this DB instance.
awsRdsDbInstanceDetails_licenseModel :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Text)
awsRdsDbInstanceDetails_licenseModel = Lens.lens (\AwsRdsDbInstanceDetails' {licenseModel} -> licenseModel) (\s@AwsRdsDbInstanceDetails' {} a -> s {licenseModel = a} :: AwsRdsDbInstanceDetails)

-- | Undocumented member.
awsRdsDbInstanceDetails_listenerEndpoint :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe AwsRdsDbInstanceEndpoint)
awsRdsDbInstanceDetails_listenerEndpoint = Lens.lens (\AwsRdsDbInstanceDetails' {listenerEndpoint} -> listenerEndpoint) (\s@AwsRdsDbInstanceDetails' {} a -> s {listenerEndpoint = a} :: AwsRdsDbInstanceDetails)

-- | The master user name of the DB instance.
awsRdsDbInstanceDetails_masterUsername :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Text)
awsRdsDbInstanceDetails_masterUsername = Lens.lens (\AwsRdsDbInstanceDetails' {masterUsername} -> masterUsername) (\s@AwsRdsDbInstanceDetails' {} a -> s {masterUsername = a} :: AwsRdsDbInstanceDetails)

-- | The upper limit to which Amazon RDS can automatically scale the storage
-- of the DB instance.
awsRdsDbInstanceDetails_maxAllocatedStorage :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Int)
awsRdsDbInstanceDetails_maxAllocatedStorage = Lens.lens (\AwsRdsDbInstanceDetails' {maxAllocatedStorage} -> maxAllocatedStorage) (\s@AwsRdsDbInstanceDetails' {} a -> s {maxAllocatedStorage = a} :: AwsRdsDbInstanceDetails)

-- | The interval, in seconds, between points when enhanced monitoring
-- metrics are collected for the DB instance.
awsRdsDbInstanceDetails_monitoringInterval :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Int)
awsRdsDbInstanceDetails_monitoringInterval = Lens.lens (\AwsRdsDbInstanceDetails' {monitoringInterval} -> monitoringInterval) (\s@AwsRdsDbInstanceDetails' {} a -> s {monitoringInterval = a} :: AwsRdsDbInstanceDetails)

-- | The ARN for the IAM role that permits Amazon RDS to send enhanced
-- monitoring metrics to CloudWatch Logs.
awsRdsDbInstanceDetails_monitoringRoleArn :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Text)
awsRdsDbInstanceDetails_monitoringRoleArn = Lens.lens (\AwsRdsDbInstanceDetails' {monitoringRoleArn} -> monitoringRoleArn) (\s@AwsRdsDbInstanceDetails' {} a -> s {monitoringRoleArn = a} :: AwsRdsDbInstanceDetails)

-- | Whether the DB instance is a multiple Availability Zone deployment.
awsRdsDbInstanceDetails_multiAz :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Bool)
awsRdsDbInstanceDetails_multiAz = Lens.lens (\AwsRdsDbInstanceDetails' {multiAz} -> multiAz) (\s@AwsRdsDbInstanceDetails' {} a -> s {multiAz = a} :: AwsRdsDbInstanceDetails)

-- | The list of option group memberships for this DB instance.
awsRdsDbInstanceDetails_optionGroupMemberships :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe [AwsRdsDbOptionGroupMembership])
awsRdsDbInstanceDetails_optionGroupMemberships = Lens.lens (\AwsRdsDbInstanceDetails' {optionGroupMemberships} -> optionGroupMemberships) (\s@AwsRdsDbInstanceDetails' {} a -> s {optionGroupMemberships = a} :: AwsRdsDbInstanceDetails) Prelude.. Lens.mapping Lens.coerced

-- | Changes to the DB instance that are currently pending.
awsRdsDbInstanceDetails_pendingModifiedValues :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe AwsRdsDbPendingModifiedValues)
awsRdsDbInstanceDetails_pendingModifiedValues = Lens.lens (\AwsRdsDbInstanceDetails' {pendingModifiedValues} -> pendingModifiedValues) (\s@AwsRdsDbInstanceDetails' {} a -> s {pendingModifiedValues = a} :: AwsRdsDbInstanceDetails)

-- | Indicates whether Performance Insights is enabled for the DB instance.
awsRdsDbInstanceDetails_performanceInsightsEnabled :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Bool)
awsRdsDbInstanceDetails_performanceInsightsEnabled = Lens.lens (\AwsRdsDbInstanceDetails' {performanceInsightsEnabled} -> performanceInsightsEnabled) (\s@AwsRdsDbInstanceDetails' {} a -> s {performanceInsightsEnabled = a} :: AwsRdsDbInstanceDetails)

-- | The identifier of the KMS key used to encrypt the Performance Insights
-- data.
awsRdsDbInstanceDetails_performanceInsightsKmsKeyId :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Text)
awsRdsDbInstanceDetails_performanceInsightsKmsKeyId = Lens.lens (\AwsRdsDbInstanceDetails' {performanceInsightsKmsKeyId} -> performanceInsightsKmsKeyId) (\s@AwsRdsDbInstanceDetails' {} a -> s {performanceInsightsKmsKeyId = a} :: AwsRdsDbInstanceDetails)

-- | The number of days to retain Performance Insights data.
awsRdsDbInstanceDetails_performanceInsightsRetentionPeriod :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Int)
awsRdsDbInstanceDetails_performanceInsightsRetentionPeriod = Lens.lens (\AwsRdsDbInstanceDetails' {performanceInsightsRetentionPeriod} -> performanceInsightsRetentionPeriod) (\s@AwsRdsDbInstanceDetails' {} a -> s {performanceInsightsRetentionPeriod = a} :: AwsRdsDbInstanceDetails)

-- | The range of time each day when automated backups are created, if
-- automated backups are enabled.
--
-- Uses the format @HH:MM-HH:MM@. For example, @04:52-05:22@.
awsRdsDbInstanceDetails_preferredBackupWindow :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Text)
awsRdsDbInstanceDetails_preferredBackupWindow = Lens.lens (\AwsRdsDbInstanceDetails' {preferredBackupWindow} -> preferredBackupWindow) (\s@AwsRdsDbInstanceDetails' {} a -> s {preferredBackupWindow = a} :: AwsRdsDbInstanceDetails)

-- | The weekly time range during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- Uses the format @\<day>:HH:MM-\<day>:HH:MM@.
--
-- For the day values, use @mon@|@tue@|@wed@|@thu@|@fri@|@sat@|@sun@.
--
-- For example, @sun:09:32-sun:10:02@.
awsRdsDbInstanceDetails_preferredMaintenanceWindow :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Text)
awsRdsDbInstanceDetails_preferredMaintenanceWindow = Lens.lens (\AwsRdsDbInstanceDetails' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@AwsRdsDbInstanceDetails' {} a -> s {preferredMaintenanceWindow = a} :: AwsRdsDbInstanceDetails)

-- | The number of CPU cores and the number of threads per core for the DB
-- instance class of the DB instance.
awsRdsDbInstanceDetails_processorFeatures :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe [AwsRdsDbProcessorFeature])
awsRdsDbInstanceDetails_processorFeatures = Lens.lens (\AwsRdsDbInstanceDetails' {processorFeatures} -> processorFeatures) (\s@AwsRdsDbInstanceDetails' {} a -> s {processorFeatures = a} :: AwsRdsDbInstanceDetails) Prelude.. Lens.mapping Lens.coerced

-- | The order in which to promote an Aurora replica to the primary instance
-- after a failure of the existing primary instance.
awsRdsDbInstanceDetails_promotionTier :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Int)
awsRdsDbInstanceDetails_promotionTier = Lens.lens (\AwsRdsDbInstanceDetails' {promotionTier} -> promotionTier) (\s@AwsRdsDbInstanceDetails' {} a -> s {promotionTier = a} :: AwsRdsDbInstanceDetails)

-- | Specifies the accessibility options for the DB instance.
--
-- A value of true specifies an Internet-facing instance with a publicly
-- resolvable DNS name, which resolves to a public IP address.
--
-- A value of false specifies an internal instance with a DNS name that
-- resolves to a private IP address.
awsRdsDbInstanceDetails_publiclyAccessible :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Bool)
awsRdsDbInstanceDetails_publiclyAccessible = Lens.lens (\AwsRdsDbInstanceDetails' {publiclyAccessible} -> publiclyAccessible) (\s@AwsRdsDbInstanceDetails' {} a -> s {publiclyAccessible = a} :: AwsRdsDbInstanceDetails)

-- | List of identifiers of Aurora DB clusters to which the RDS DB instance
-- is replicated as a read replica.
awsRdsDbInstanceDetails_readReplicaDBClusterIdentifiers :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe [Prelude.Text])
awsRdsDbInstanceDetails_readReplicaDBClusterIdentifiers = Lens.lens (\AwsRdsDbInstanceDetails' {readReplicaDBClusterIdentifiers} -> readReplicaDBClusterIdentifiers) (\s@AwsRdsDbInstanceDetails' {} a -> s {readReplicaDBClusterIdentifiers = a} :: AwsRdsDbInstanceDetails) Prelude.. Lens.mapping Lens.coerced

-- | List of identifiers of the read replicas associated with this DB
-- instance.
awsRdsDbInstanceDetails_readReplicaDBInstanceIdentifiers :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe [Prelude.Text])
awsRdsDbInstanceDetails_readReplicaDBInstanceIdentifiers = Lens.lens (\AwsRdsDbInstanceDetails' {readReplicaDBInstanceIdentifiers} -> readReplicaDBInstanceIdentifiers) (\s@AwsRdsDbInstanceDetails' {} a -> s {readReplicaDBInstanceIdentifiers = a} :: AwsRdsDbInstanceDetails) Prelude.. Lens.mapping Lens.coerced

-- | If this DB instance is a read replica, contains the identifier of the
-- source DB instance.
awsRdsDbInstanceDetails_readReplicaSourceDBInstanceIdentifier :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Text)
awsRdsDbInstanceDetails_readReplicaSourceDBInstanceIdentifier = Lens.lens (\AwsRdsDbInstanceDetails' {readReplicaSourceDBInstanceIdentifier} -> readReplicaSourceDBInstanceIdentifier) (\s@AwsRdsDbInstanceDetails' {} a -> s {readReplicaSourceDBInstanceIdentifier = a} :: AwsRdsDbInstanceDetails)

-- | For a DB instance with multi-Availability Zone support, the name of the
-- secondary Availability Zone.
awsRdsDbInstanceDetails_secondaryAvailabilityZone :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Text)
awsRdsDbInstanceDetails_secondaryAvailabilityZone = Lens.lens (\AwsRdsDbInstanceDetails' {secondaryAvailabilityZone} -> secondaryAvailabilityZone) (\s@AwsRdsDbInstanceDetails' {} a -> s {secondaryAvailabilityZone = a} :: AwsRdsDbInstanceDetails)

-- | The status of a read replica. If the instance isn\'t a read replica,
-- this is empty.
awsRdsDbInstanceDetails_statusInfos :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe [AwsRdsDbStatusInfo])
awsRdsDbInstanceDetails_statusInfos = Lens.lens (\AwsRdsDbInstanceDetails' {statusInfos} -> statusInfos) (\s@AwsRdsDbInstanceDetails' {} a -> s {statusInfos = a} :: AwsRdsDbInstanceDetails) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether the DB instance is encrypted.
awsRdsDbInstanceDetails_storageEncrypted :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Bool)
awsRdsDbInstanceDetails_storageEncrypted = Lens.lens (\AwsRdsDbInstanceDetails' {storageEncrypted} -> storageEncrypted) (\s@AwsRdsDbInstanceDetails' {} a -> s {storageEncrypted = a} :: AwsRdsDbInstanceDetails)

-- | The storage type for the DB instance.
awsRdsDbInstanceDetails_storageType :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Text)
awsRdsDbInstanceDetails_storageType = Lens.lens (\AwsRdsDbInstanceDetails' {storageType} -> storageType) (\s@AwsRdsDbInstanceDetails' {} a -> s {storageType = a} :: AwsRdsDbInstanceDetails)

-- | The ARN from the key store with which the instance is associated for TDE
-- encryption.
awsRdsDbInstanceDetails_tdeCredentialArn :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Text)
awsRdsDbInstanceDetails_tdeCredentialArn = Lens.lens (\AwsRdsDbInstanceDetails' {tdeCredentialArn} -> tdeCredentialArn) (\s@AwsRdsDbInstanceDetails' {} a -> s {tdeCredentialArn = a} :: AwsRdsDbInstanceDetails)

-- | The time zone of the DB instance.
awsRdsDbInstanceDetails_timezone :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe Prelude.Text)
awsRdsDbInstanceDetails_timezone = Lens.lens (\AwsRdsDbInstanceDetails' {timezone} -> timezone) (\s@AwsRdsDbInstanceDetails' {} a -> s {timezone = a} :: AwsRdsDbInstanceDetails)

-- | A list of VPC security groups that the DB instance belongs to.
awsRdsDbInstanceDetails_vpcSecurityGroups :: Lens.Lens' AwsRdsDbInstanceDetails (Prelude.Maybe [AwsRdsDbInstanceVpcSecurityGroup])
awsRdsDbInstanceDetails_vpcSecurityGroups = Lens.lens (\AwsRdsDbInstanceDetails' {vpcSecurityGroups} -> vpcSecurityGroups) (\s@AwsRdsDbInstanceDetails' {} a -> s {vpcSecurityGroups = a} :: AwsRdsDbInstanceDetails) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AwsRdsDbInstanceDetails where
  parseJSON =
    Data.withObject
      "AwsRdsDbInstanceDetails"
      ( \x ->
          AwsRdsDbInstanceDetails'
            Prelude.<$> (x Data..:? "AllocatedStorage")
            Prelude.<*> ( x
                            Data..:? "AssociatedRoles"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "AutoMinorVersionUpgrade")
            Prelude.<*> (x Data..:? "AvailabilityZone")
            Prelude.<*> (x Data..:? "BackupRetentionPeriod")
            Prelude.<*> (x Data..:? "CACertificateIdentifier")
            Prelude.<*> (x Data..:? "CharacterSetName")
            Prelude.<*> (x Data..:? "CopyTagsToSnapshot")
            Prelude.<*> (x Data..:? "DBClusterIdentifier")
            Prelude.<*> (x Data..:? "DBInstanceClass")
            Prelude.<*> (x Data..:? "DBInstanceIdentifier")
            Prelude.<*> (x Data..:? "DBName")
            Prelude.<*> (x Data..:? "DbInstancePort")
            Prelude.<*> (x Data..:? "DbInstanceStatus")
            Prelude.<*> ( x
                            Data..:? "DbParameterGroups"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "DbSecurityGroups"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "DbSubnetGroup")
            Prelude.<*> (x Data..:? "DbiResourceId")
            Prelude.<*> (x Data..:? "DeletionProtection")
            Prelude.<*> ( x
                            Data..:? "DomainMemberships"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "EnabledCloudWatchLogsExports"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Endpoint")
            Prelude.<*> (x Data..:? "Engine")
            Prelude.<*> (x Data..:? "EngineVersion")
            Prelude.<*> (x Data..:? "EnhancedMonitoringResourceArn")
            Prelude.<*> (x Data..:? "IAMDatabaseAuthenticationEnabled")
            Prelude.<*> (x Data..:? "InstanceCreateTime")
            Prelude.<*> (x Data..:? "Iops")
            Prelude.<*> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..:? "LatestRestorableTime")
            Prelude.<*> (x Data..:? "LicenseModel")
            Prelude.<*> (x Data..:? "ListenerEndpoint")
            Prelude.<*> (x Data..:? "MasterUsername")
            Prelude.<*> (x Data..:? "MaxAllocatedStorage")
            Prelude.<*> (x Data..:? "MonitoringInterval")
            Prelude.<*> (x Data..:? "MonitoringRoleArn")
            Prelude.<*> (x Data..:? "MultiAz")
            Prelude.<*> ( x
                            Data..:? "OptionGroupMemberships"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "PendingModifiedValues")
            Prelude.<*> (x Data..:? "PerformanceInsightsEnabled")
            Prelude.<*> (x Data..:? "PerformanceInsightsKmsKeyId")
            Prelude.<*> (x Data..:? "PerformanceInsightsRetentionPeriod")
            Prelude.<*> (x Data..:? "PreferredBackupWindow")
            Prelude.<*> (x Data..:? "PreferredMaintenanceWindow")
            Prelude.<*> ( x
                            Data..:? "ProcessorFeatures"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "PromotionTier")
            Prelude.<*> (x Data..:? "PubliclyAccessible")
            Prelude.<*> ( x
                            Data..:? "ReadReplicaDBClusterIdentifiers"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "ReadReplicaDBInstanceIdentifiers"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ReadReplicaSourceDBInstanceIdentifier")
            Prelude.<*> (x Data..:? "SecondaryAvailabilityZone")
            Prelude.<*> (x Data..:? "StatusInfos" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "StorageEncrypted")
            Prelude.<*> (x Data..:? "StorageType")
            Prelude.<*> (x Data..:? "TdeCredentialArn")
            Prelude.<*> (x Data..:? "Timezone")
            Prelude.<*> ( x
                            Data..:? "VpcSecurityGroups"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AwsRdsDbInstanceDetails where
  hashWithSalt _salt AwsRdsDbInstanceDetails' {..} =
    _salt
      `Prelude.hashWithSalt` allocatedStorage
      `Prelude.hashWithSalt` associatedRoles
      `Prelude.hashWithSalt` autoMinorVersionUpgrade
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` backupRetentionPeriod
      `Prelude.hashWithSalt` cACertificateIdentifier
      `Prelude.hashWithSalt` characterSetName
      `Prelude.hashWithSalt` copyTagsToSnapshot
      `Prelude.hashWithSalt` dbClusterIdentifier
      `Prelude.hashWithSalt` dbInstanceClass
      `Prelude.hashWithSalt` dbInstanceIdentifier
      `Prelude.hashWithSalt` dbName
      `Prelude.hashWithSalt` dbInstancePort
      `Prelude.hashWithSalt` dbInstanceStatus
      `Prelude.hashWithSalt` dbParameterGroups
      `Prelude.hashWithSalt` dbSecurityGroups
      `Prelude.hashWithSalt` dbSubnetGroup
      `Prelude.hashWithSalt` dbiResourceId
      `Prelude.hashWithSalt` deletionProtection
      `Prelude.hashWithSalt` domainMemberships
      `Prelude.hashWithSalt` enabledCloudWatchLogsExports
      `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` enhancedMonitoringResourceArn
      `Prelude.hashWithSalt` iAMDatabaseAuthenticationEnabled
      `Prelude.hashWithSalt` instanceCreateTime
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` latestRestorableTime
      `Prelude.hashWithSalt` licenseModel
      `Prelude.hashWithSalt` listenerEndpoint
      `Prelude.hashWithSalt` masterUsername
      `Prelude.hashWithSalt` maxAllocatedStorage
      `Prelude.hashWithSalt` monitoringInterval
      `Prelude.hashWithSalt` monitoringRoleArn
      `Prelude.hashWithSalt` multiAz
      `Prelude.hashWithSalt` optionGroupMemberships
      `Prelude.hashWithSalt` pendingModifiedValues
      `Prelude.hashWithSalt` performanceInsightsEnabled
      `Prelude.hashWithSalt` performanceInsightsKmsKeyId
      `Prelude.hashWithSalt` performanceInsightsRetentionPeriod
      `Prelude.hashWithSalt` preferredBackupWindow
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` processorFeatures
      `Prelude.hashWithSalt` promotionTier
      `Prelude.hashWithSalt` publiclyAccessible
      `Prelude.hashWithSalt` readReplicaDBClusterIdentifiers
      `Prelude.hashWithSalt` readReplicaDBInstanceIdentifiers
      `Prelude.hashWithSalt` readReplicaSourceDBInstanceIdentifier
      `Prelude.hashWithSalt` secondaryAvailabilityZone
      `Prelude.hashWithSalt` statusInfos
      `Prelude.hashWithSalt` storageEncrypted
      `Prelude.hashWithSalt` storageType
      `Prelude.hashWithSalt` tdeCredentialArn
      `Prelude.hashWithSalt` timezone
      `Prelude.hashWithSalt` vpcSecurityGroups

instance Prelude.NFData AwsRdsDbInstanceDetails where
  rnf AwsRdsDbInstanceDetails' {..} =
    Prelude.rnf allocatedStorage
      `Prelude.seq` Prelude.rnf associatedRoles
      `Prelude.seq` Prelude.rnf autoMinorVersionUpgrade
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf backupRetentionPeriod
      `Prelude.seq` Prelude.rnf cACertificateIdentifier
      `Prelude.seq` Prelude.rnf characterSetName
      `Prelude.seq` Prelude.rnf copyTagsToSnapshot
      `Prelude.seq` Prelude.rnf dbClusterIdentifier
      `Prelude.seq` Prelude.rnf dbInstanceClass
      `Prelude.seq` Prelude.rnf dbInstanceIdentifier
      `Prelude.seq` Prelude.rnf dbName
      `Prelude.seq` Prelude.rnf dbInstancePort
      `Prelude.seq` Prelude.rnf dbInstanceStatus
      `Prelude.seq` Prelude.rnf dbParameterGroups
      `Prelude.seq` Prelude.rnf dbSecurityGroups
      `Prelude.seq` Prelude.rnf dbSubnetGroup
      `Prelude.seq` Prelude.rnf dbiResourceId
      `Prelude.seq` Prelude.rnf deletionProtection
      `Prelude.seq` Prelude.rnf domainMemberships
      `Prelude.seq` Prelude.rnf
        enabledCloudWatchLogsExports
      `Prelude.seq` Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf
        engineVersion
      `Prelude.seq` Prelude.rnf
        enhancedMonitoringResourceArn
      `Prelude.seq` Prelude.rnf
        iAMDatabaseAuthenticationEnabled
      `Prelude.seq` Prelude.rnf
        instanceCreateTime
      `Prelude.seq` Prelude.rnf
        iops
      `Prelude.seq` Prelude.rnf
        kmsKeyId
      `Prelude.seq` Prelude.rnf
        latestRestorableTime
      `Prelude.seq` Prelude.rnf
        licenseModel
      `Prelude.seq` Prelude.rnf
        listenerEndpoint
      `Prelude.seq` Prelude.rnf
        masterUsername
      `Prelude.seq` Prelude.rnf
        maxAllocatedStorage
      `Prelude.seq` Prelude.rnf
        monitoringInterval
      `Prelude.seq` Prelude.rnf
        monitoringRoleArn
      `Prelude.seq` Prelude.rnf
        multiAz
      `Prelude.seq` Prelude.rnf
        optionGroupMemberships
      `Prelude.seq` Prelude.rnf
        pendingModifiedValues
      `Prelude.seq` Prelude.rnf
        performanceInsightsEnabled
      `Prelude.seq` Prelude.rnf
        performanceInsightsKmsKeyId
      `Prelude.seq` Prelude.rnf
        performanceInsightsRetentionPeriod
      `Prelude.seq` Prelude.rnf
        preferredBackupWindow
      `Prelude.seq` Prelude.rnf
        preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf
        processorFeatures
      `Prelude.seq` Prelude.rnf
        promotionTier
      `Prelude.seq` Prelude.rnf
        publiclyAccessible
      `Prelude.seq` Prelude.rnf
        readReplicaDBClusterIdentifiers
      `Prelude.seq` Prelude.rnf
        readReplicaDBInstanceIdentifiers
      `Prelude.seq` Prelude.rnf
        readReplicaSourceDBInstanceIdentifier
      `Prelude.seq` Prelude.rnf
        secondaryAvailabilityZone
      `Prelude.seq` Prelude.rnf
        statusInfos
      `Prelude.seq` Prelude.rnf
        storageEncrypted
      `Prelude.seq` Prelude.rnf
        storageType
      `Prelude.seq` Prelude.rnf
        tdeCredentialArn
      `Prelude.seq` Prelude.rnf
        timezone
      `Prelude.seq` Prelude.rnf
        vpcSecurityGroups

instance Data.ToJSON AwsRdsDbInstanceDetails where
  toJSON AwsRdsDbInstanceDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AllocatedStorage" Data..=)
              Prelude.<$> allocatedStorage,
            ("AssociatedRoles" Data..=)
              Prelude.<$> associatedRoles,
            ("AutoMinorVersionUpgrade" Data..=)
              Prelude.<$> autoMinorVersionUpgrade,
            ("AvailabilityZone" Data..=)
              Prelude.<$> availabilityZone,
            ("BackupRetentionPeriod" Data..=)
              Prelude.<$> backupRetentionPeriod,
            ("CACertificateIdentifier" Data..=)
              Prelude.<$> cACertificateIdentifier,
            ("CharacterSetName" Data..=)
              Prelude.<$> characterSetName,
            ("CopyTagsToSnapshot" Data..=)
              Prelude.<$> copyTagsToSnapshot,
            ("DBClusterIdentifier" Data..=)
              Prelude.<$> dbClusterIdentifier,
            ("DBInstanceClass" Data..=)
              Prelude.<$> dbInstanceClass,
            ("DBInstanceIdentifier" Data..=)
              Prelude.<$> dbInstanceIdentifier,
            ("DBName" Data..=) Prelude.<$> dbName,
            ("DbInstancePort" Data..=)
              Prelude.<$> dbInstancePort,
            ("DbInstanceStatus" Data..=)
              Prelude.<$> dbInstanceStatus,
            ("DbParameterGroups" Data..=)
              Prelude.<$> dbParameterGroups,
            ("DbSecurityGroups" Data..=)
              Prelude.<$> dbSecurityGroups,
            ("DbSubnetGroup" Data..=) Prelude.<$> dbSubnetGroup,
            ("DbiResourceId" Data..=) Prelude.<$> dbiResourceId,
            ("DeletionProtection" Data..=)
              Prelude.<$> deletionProtection,
            ("DomainMemberships" Data..=)
              Prelude.<$> domainMemberships,
            ("EnabledCloudWatchLogsExports" Data..=)
              Prelude.<$> enabledCloudWatchLogsExports,
            ("Endpoint" Data..=) Prelude.<$> endpoint,
            ("Engine" Data..=) Prelude.<$> engine,
            ("EngineVersion" Data..=) Prelude.<$> engineVersion,
            ("EnhancedMonitoringResourceArn" Data..=)
              Prelude.<$> enhancedMonitoringResourceArn,
            ("IAMDatabaseAuthenticationEnabled" Data..=)
              Prelude.<$> iAMDatabaseAuthenticationEnabled,
            ("InstanceCreateTime" Data..=)
              Prelude.<$> instanceCreateTime,
            ("Iops" Data..=) Prelude.<$> iops,
            ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("LatestRestorableTime" Data..=)
              Prelude.<$> latestRestorableTime,
            ("LicenseModel" Data..=) Prelude.<$> licenseModel,
            ("ListenerEndpoint" Data..=)
              Prelude.<$> listenerEndpoint,
            ("MasterUsername" Data..=)
              Prelude.<$> masterUsername,
            ("MaxAllocatedStorage" Data..=)
              Prelude.<$> maxAllocatedStorage,
            ("MonitoringInterval" Data..=)
              Prelude.<$> monitoringInterval,
            ("MonitoringRoleArn" Data..=)
              Prelude.<$> monitoringRoleArn,
            ("MultiAz" Data..=) Prelude.<$> multiAz,
            ("OptionGroupMemberships" Data..=)
              Prelude.<$> optionGroupMemberships,
            ("PendingModifiedValues" Data..=)
              Prelude.<$> pendingModifiedValues,
            ("PerformanceInsightsEnabled" Data..=)
              Prelude.<$> performanceInsightsEnabled,
            ("PerformanceInsightsKmsKeyId" Data..=)
              Prelude.<$> performanceInsightsKmsKeyId,
            ("PerformanceInsightsRetentionPeriod" Data..=)
              Prelude.<$> performanceInsightsRetentionPeriod,
            ("PreferredBackupWindow" Data..=)
              Prelude.<$> preferredBackupWindow,
            ("PreferredMaintenanceWindow" Data..=)
              Prelude.<$> preferredMaintenanceWindow,
            ("ProcessorFeatures" Data..=)
              Prelude.<$> processorFeatures,
            ("PromotionTier" Data..=) Prelude.<$> promotionTier,
            ("PubliclyAccessible" Data..=)
              Prelude.<$> publiclyAccessible,
            ("ReadReplicaDBClusterIdentifiers" Data..=)
              Prelude.<$> readReplicaDBClusterIdentifiers,
            ("ReadReplicaDBInstanceIdentifiers" Data..=)
              Prelude.<$> readReplicaDBInstanceIdentifiers,
            ("ReadReplicaSourceDBInstanceIdentifier" Data..=)
              Prelude.<$> readReplicaSourceDBInstanceIdentifier,
            ("SecondaryAvailabilityZone" Data..=)
              Prelude.<$> secondaryAvailabilityZone,
            ("StatusInfos" Data..=) Prelude.<$> statusInfos,
            ("StorageEncrypted" Data..=)
              Prelude.<$> storageEncrypted,
            ("StorageType" Data..=) Prelude.<$> storageType,
            ("TdeCredentialArn" Data..=)
              Prelude.<$> tdeCredentialArn,
            ("Timezone" Data..=) Prelude.<$> timezone,
            ("VpcSecurityGroups" Data..=)
              Prelude.<$> vpcSecurityGroups
          ]
      )
