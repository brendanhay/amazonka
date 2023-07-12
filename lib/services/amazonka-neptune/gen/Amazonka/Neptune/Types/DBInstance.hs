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
-- Module      : Amazonka.Neptune.Types.DBInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Neptune.Types.DBInstance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Neptune.Types.DBInstanceStatusInfo
import Amazonka.Neptune.Types.DBParameterGroupStatus
import Amazonka.Neptune.Types.DBSecurityGroupMembership
import Amazonka.Neptune.Types.DBSubnetGroup
import Amazonka.Neptune.Types.DomainMembership
import Amazonka.Neptune.Types.Endpoint
import Amazonka.Neptune.Types.OptionGroupMembership
import Amazonka.Neptune.Types.PendingModifiedValues
import Amazonka.Neptune.Types.VpcSecurityGroupMembership
import qualified Amazonka.Prelude as Prelude

-- | Contains the details of an Amazon Neptune DB instance.
--
-- This data type is used as a response element in the DescribeDBInstances
-- action.
--
-- /See:/ 'newDBInstance' smart constructor.
data DBInstance = DBInstance'
  { -- | Not supported by Neptune.
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | Indicates that minor version patches are applied automatically.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the name of the Availability Zone the DB instance is located
    -- in.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | Specifies the number of days for which automatic DB snapshots are
    -- retained.
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The identifier of the CA certificate for this DB instance.
    cACertificateIdentifier :: Prelude.Maybe Prelude.Text,
    -- | /(Not supported by Neptune)/
    characterSetName :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether tags are copied from the DB instance to snapshots of
    -- the DB instance.
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | If the DB instance is a member of a DB cluster, contains the name of the
    -- DB cluster that the DB instance is a member of.
    dbClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the DB instance.
    dbInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | Contains the name of the compute and memory capacity class of the DB
    -- instance.
    dbInstanceClass :: Prelude.Maybe Prelude.Text,
    -- | Contains a user-supplied database identifier. This identifier is the
    -- unique key that identifies a DB instance.
    dbInstanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Specifies the current state of this database.
    dbInstanceStatus :: Prelude.Maybe Prelude.Text,
    -- | The database name.
    dbName :: Prelude.Maybe Prelude.Text,
    -- | Provides the list of DB parameter groups applied to this DB instance.
    dbParameterGroups :: Prelude.Maybe [DBParameterGroupStatus],
    -- | Provides List of DB security group elements containing only
    -- @DBSecurityGroup.Name@ and @DBSecurityGroup.Status@ subelements.
    dbSecurityGroups :: Prelude.Maybe [DBSecurityGroupMembership],
    -- | Specifies information on the subnet group associated with the DB
    -- instance, including the name, description, and subnets in the subnet
    -- group.
    dbSubnetGroup :: Prelude.Maybe DBSubnetGroup,
    -- | Specifies the port that the DB instance listens on. If the DB instance
    -- is part of a DB cluster, this can be a different port than the DB
    -- cluster port.
    dbInstancePort :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Region-unique, immutable identifier for the DB instance. This
    -- identifier is found in Amazon CloudTrail log entries whenever the Amazon
    -- KMS key for the DB instance is accessed.
    dbiResourceId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether or not the DB instance has deletion protection
    -- enabled. The instance can\'t be deleted when deletion protection is
    -- enabled. See
    -- <https://docs.aws.amazon.com/neptune/latest/userguide/manage-console-instances-delete.html Deleting a DB Instance>.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | Not supported
    domainMemberships :: Prelude.Maybe [DomainMembership],
    -- | A list of log types that this DB instance is configured to export to
    -- CloudWatch Logs.
    enabledCloudwatchLogsExports :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the connection endpoint.
    endpoint :: Prelude.Maybe Endpoint,
    -- | Provides the name of the database engine to be used for this DB
    -- instance.
    engine :: Prelude.Maybe Prelude.Text,
    -- | Indicates the database engine version.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon CloudWatch Logs log stream
    -- that receives the Enhanced Monitoring metrics data for the DB instance.
    enhancedMonitoringResourceArn :: Prelude.Maybe Prelude.Text,
    -- | True if Amazon Identity and Access Management (IAM) authentication is
    -- enabled, and otherwise false.
    iAMDatabaseAuthenticationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Provides the date and time the DB instance was created.
    instanceCreateTime :: Prelude.Maybe Data.ISO8601,
    -- | Specifies the Provisioned IOPS (I\/O operations per second) value.
    iops :: Prelude.Maybe Prelude.Int,
    -- | Not supported: The encryption for DB instances is managed by the DB
    -- cluster.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the latest time to which a database can be restored with
    -- point-in-time restore.
    latestRestorableTime :: Prelude.Maybe Data.ISO8601,
    -- | License model information for this DB instance.
    licenseModel :: Prelude.Maybe Prelude.Text,
    -- | Not supported by Neptune.
    masterUsername :: Prelude.Maybe Prelude.Text,
    -- | The interval, in seconds, between points when Enhanced Monitoring
    -- metrics are collected for the DB instance.
    monitoringInterval :: Prelude.Maybe Prelude.Int,
    -- | The ARN for the IAM role that permits Neptune to send Enhanced
    -- Monitoring metrics to Amazon CloudWatch Logs.
    monitoringRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies if the DB instance is a Multi-AZ deployment.
    multiAZ :: Prelude.Maybe Prelude.Bool,
    -- | /(Not supported by Neptune)/
    optionGroupMemberships :: Prelude.Maybe [OptionGroupMembership],
    -- | Specifies that changes to the DB instance are pending. This element is
    -- only included when changes are pending. Specific changes are identified
    -- by subelements.
    pendingModifiedValues :: Prelude.Maybe PendingModifiedValues,
    -- | /(Not supported by Neptune)/
    performanceInsightsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | /(Not supported by Neptune)/
    performanceInsightsKMSKeyId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the daily time range during which automated backups are
    -- created if automated backups are enabled, as determined by the
    -- @BackupRetentionPeriod@.
    preferredBackupWindow :: Prelude.Maybe Prelude.Text,
    -- | Specifies the weekly time range during which system maintenance can
    -- occur, in Universal Coordinated Time (UTC).
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | A value that specifies the order in which a Read Replica is promoted to
    -- the primary instance after a failure of the existing primary instance.
    promotionTier :: Prelude.Maybe Prelude.Int,
    -- | This flag should no longer be used.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | Contains one or more identifiers of DB clusters that are Read Replicas
    -- of this DB instance.
    readReplicaDBClusterIdentifiers :: Prelude.Maybe [Prelude.Text],
    -- | Contains one or more identifiers of the Read Replicas associated with
    -- this DB instance.
    readReplicaDBInstanceIdentifiers :: Prelude.Maybe [Prelude.Text],
    -- | Contains the identifier of the source DB instance if this DB instance is
    -- a Read Replica.
    readReplicaSourceDBInstanceIdentifier :: Prelude.Maybe Prelude.Text,
    -- | If present, specifies the name of the secondary Availability Zone for a
    -- DB instance with multi-AZ support.
    secondaryAvailabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The status of a Read Replica. If the instance is not a Read Replica,
    -- this is blank.
    statusInfos :: Prelude.Maybe [DBInstanceStatusInfo],
    -- | Not supported: The encryption for DB instances is managed by the DB
    -- cluster.
    storageEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the storage type associated with DB instance.
    storageType :: Prelude.Maybe Prelude.Text,
    -- | The ARN from the key store with which the instance is associated for TDE
    -- encryption.
    tdeCredentialArn :: Prelude.Maybe Prelude.Text,
    -- | Not supported.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | Provides a list of VPC security group elements that the DB instance
    -- belongs to.
    vpcSecurityGroups :: Prelude.Maybe [VpcSecurityGroupMembership]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DBInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allocatedStorage', 'dbInstance_allocatedStorage' - Not supported by Neptune.
--
-- 'autoMinorVersionUpgrade', 'dbInstance_autoMinorVersionUpgrade' - Indicates that minor version patches are applied automatically.
--
-- 'availabilityZone', 'dbInstance_availabilityZone' - Specifies the name of the Availability Zone the DB instance is located
-- in.
--
-- 'backupRetentionPeriod', 'dbInstance_backupRetentionPeriod' - Specifies the number of days for which automatic DB snapshots are
-- retained.
--
-- 'cACertificateIdentifier', 'dbInstance_cACertificateIdentifier' - The identifier of the CA certificate for this DB instance.
--
-- 'characterSetName', 'dbInstance_characterSetName' - /(Not supported by Neptune)/
--
-- 'copyTagsToSnapshot', 'dbInstance_copyTagsToSnapshot' - Specifies whether tags are copied from the DB instance to snapshots of
-- the DB instance.
--
-- 'dbClusterIdentifier', 'dbInstance_dbClusterIdentifier' - If the DB instance is a member of a DB cluster, contains the name of the
-- DB cluster that the DB instance is a member of.
--
-- 'dbInstanceArn', 'dbInstance_dbInstanceArn' - The Amazon Resource Name (ARN) for the DB instance.
--
-- 'dbInstanceClass', 'dbInstance_dbInstanceClass' - Contains the name of the compute and memory capacity class of the DB
-- instance.
--
-- 'dbInstanceIdentifier', 'dbInstance_dbInstanceIdentifier' - Contains a user-supplied database identifier. This identifier is the
-- unique key that identifies a DB instance.
--
-- 'dbInstanceStatus', 'dbInstance_dbInstanceStatus' - Specifies the current state of this database.
--
-- 'dbName', 'dbInstance_dbName' - The database name.
--
-- 'dbParameterGroups', 'dbInstance_dbParameterGroups' - Provides the list of DB parameter groups applied to this DB instance.
--
-- 'dbSecurityGroups', 'dbInstance_dbSecurityGroups' - Provides List of DB security group elements containing only
-- @DBSecurityGroup.Name@ and @DBSecurityGroup.Status@ subelements.
--
-- 'dbSubnetGroup', 'dbInstance_dbSubnetGroup' - Specifies information on the subnet group associated with the DB
-- instance, including the name, description, and subnets in the subnet
-- group.
--
-- 'dbInstancePort', 'dbInstance_dbInstancePort' - Specifies the port that the DB instance listens on. If the DB instance
-- is part of a DB cluster, this can be a different port than the DB
-- cluster port.
--
-- 'dbiResourceId', 'dbInstance_dbiResourceId' - The Amazon Region-unique, immutable identifier for the DB instance. This
-- identifier is found in Amazon CloudTrail log entries whenever the Amazon
-- KMS key for the DB instance is accessed.
--
-- 'deletionProtection', 'dbInstance_deletionProtection' - Indicates whether or not the DB instance has deletion protection
-- enabled. The instance can\'t be deleted when deletion protection is
-- enabled. See
-- <https://docs.aws.amazon.com/neptune/latest/userguide/manage-console-instances-delete.html Deleting a DB Instance>.
--
-- 'domainMemberships', 'dbInstance_domainMemberships' - Not supported
--
-- 'enabledCloudwatchLogsExports', 'dbInstance_enabledCloudwatchLogsExports' - A list of log types that this DB instance is configured to export to
-- CloudWatch Logs.
--
-- 'endpoint', 'dbInstance_endpoint' - Specifies the connection endpoint.
--
-- 'engine', 'dbInstance_engine' - Provides the name of the database engine to be used for this DB
-- instance.
--
-- 'engineVersion', 'dbInstance_engineVersion' - Indicates the database engine version.
--
-- 'enhancedMonitoringResourceArn', 'dbInstance_enhancedMonitoringResourceArn' - The Amazon Resource Name (ARN) of the Amazon CloudWatch Logs log stream
-- that receives the Enhanced Monitoring metrics data for the DB instance.
--
-- 'iAMDatabaseAuthenticationEnabled', 'dbInstance_iAMDatabaseAuthenticationEnabled' - True if Amazon Identity and Access Management (IAM) authentication is
-- enabled, and otherwise false.
--
-- 'instanceCreateTime', 'dbInstance_instanceCreateTime' - Provides the date and time the DB instance was created.
--
-- 'iops', 'dbInstance_iops' - Specifies the Provisioned IOPS (I\/O operations per second) value.
--
-- 'kmsKeyId', 'dbInstance_kmsKeyId' - Not supported: The encryption for DB instances is managed by the DB
-- cluster.
--
-- 'latestRestorableTime', 'dbInstance_latestRestorableTime' - Specifies the latest time to which a database can be restored with
-- point-in-time restore.
--
-- 'licenseModel', 'dbInstance_licenseModel' - License model information for this DB instance.
--
-- 'masterUsername', 'dbInstance_masterUsername' - Not supported by Neptune.
--
-- 'monitoringInterval', 'dbInstance_monitoringInterval' - The interval, in seconds, between points when Enhanced Monitoring
-- metrics are collected for the DB instance.
--
-- 'monitoringRoleArn', 'dbInstance_monitoringRoleArn' - The ARN for the IAM role that permits Neptune to send Enhanced
-- Monitoring metrics to Amazon CloudWatch Logs.
--
-- 'multiAZ', 'dbInstance_multiAZ' - Specifies if the DB instance is a Multi-AZ deployment.
--
-- 'optionGroupMemberships', 'dbInstance_optionGroupMemberships' - /(Not supported by Neptune)/
--
-- 'pendingModifiedValues', 'dbInstance_pendingModifiedValues' - Specifies that changes to the DB instance are pending. This element is
-- only included when changes are pending. Specific changes are identified
-- by subelements.
--
-- 'performanceInsightsEnabled', 'dbInstance_performanceInsightsEnabled' - /(Not supported by Neptune)/
--
-- 'performanceInsightsKMSKeyId', 'dbInstance_performanceInsightsKMSKeyId' - /(Not supported by Neptune)/
--
-- 'preferredBackupWindow', 'dbInstance_preferredBackupWindow' - Specifies the daily time range during which automated backups are
-- created if automated backups are enabled, as determined by the
-- @BackupRetentionPeriod@.
--
-- 'preferredMaintenanceWindow', 'dbInstance_preferredMaintenanceWindow' - Specifies the weekly time range during which system maintenance can
-- occur, in Universal Coordinated Time (UTC).
--
-- 'promotionTier', 'dbInstance_promotionTier' - A value that specifies the order in which a Read Replica is promoted to
-- the primary instance after a failure of the existing primary instance.
--
-- 'publiclyAccessible', 'dbInstance_publiclyAccessible' - This flag should no longer be used.
--
-- 'readReplicaDBClusterIdentifiers', 'dbInstance_readReplicaDBClusterIdentifiers' - Contains one or more identifiers of DB clusters that are Read Replicas
-- of this DB instance.
--
-- 'readReplicaDBInstanceIdentifiers', 'dbInstance_readReplicaDBInstanceIdentifiers' - Contains one or more identifiers of the Read Replicas associated with
-- this DB instance.
--
-- 'readReplicaSourceDBInstanceIdentifier', 'dbInstance_readReplicaSourceDBInstanceIdentifier' - Contains the identifier of the source DB instance if this DB instance is
-- a Read Replica.
--
-- 'secondaryAvailabilityZone', 'dbInstance_secondaryAvailabilityZone' - If present, specifies the name of the secondary Availability Zone for a
-- DB instance with multi-AZ support.
--
-- 'statusInfos', 'dbInstance_statusInfos' - The status of a Read Replica. If the instance is not a Read Replica,
-- this is blank.
--
-- 'storageEncrypted', 'dbInstance_storageEncrypted' - Not supported: The encryption for DB instances is managed by the DB
-- cluster.
--
-- 'storageType', 'dbInstance_storageType' - Specifies the storage type associated with DB instance.
--
-- 'tdeCredentialArn', 'dbInstance_tdeCredentialArn' - The ARN from the key store with which the instance is associated for TDE
-- encryption.
--
-- 'timezone', 'dbInstance_timezone' - Not supported.
--
-- 'vpcSecurityGroups', 'dbInstance_vpcSecurityGroups' - Provides a list of VPC security group elements that the DB instance
-- belongs to.
newDBInstance ::
  DBInstance
newDBInstance =
  DBInstance'
    { allocatedStorage = Prelude.Nothing,
      autoMinorVersionUpgrade = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      backupRetentionPeriod = Prelude.Nothing,
      cACertificateIdentifier = Prelude.Nothing,
      characterSetName = Prelude.Nothing,
      copyTagsToSnapshot = Prelude.Nothing,
      dbClusterIdentifier = Prelude.Nothing,
      dbInstanceArn = Prelude.Nothing,
      dbInstanceClass = Prelude.Nothing,
      dbInstanceIdentifier = Prelude.Nothing,
      dbInstanceStatus = Prelude.Nothing,
      dbName = Prelude.Nothing,
      dbParameterGroups = Prelude.Nothing,
      dbSecurityGroups = Prelude.Nothing,
      dbSubnetGroup = Prelude.Nothing,
      dbInstancePort = Prelude.Nothing,
      dbiResourceId = Prelude.Nothing,
      deletionProtection = Prelude.Nothing,
      domainMemberships = Prelude.Nothing,
      enabledCloudwatchLogsExports = Prelude.Nothing,
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
      masterUsername = Prelude.Nothing,
      monitoringInterval = Prelude.Nothing,
      monitoringRoleArn = Prelude.Nothing,
      multiAZ = Prelude.Nothing,
      optionGroupMemberships = Prelude.Nothing,
      pendingModifiedValues = Prelude.Nothing,
      performanceInsightsEnabled = Prelude.Nothing,
      performanceInsightsKMSKeyId = Prelude.Nothing,
      preferredBackupWindow = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
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

-- | Not supported by Neptune.
dbInstance_allocatedStorage :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Int)
dbInstance_allocatedStorage = Lens.lens (\DBInstance' {allocatedStorage} -> allocatedStorage) (\s@DBInstance' {} a -> s {allocatedStorage = a} :: DBInstance)

-- | Indicates that minor version patches are applied automatically.
dbInstance_autoMinorVersionUpgrade :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Bool)
dbInstance_autoMinorVersionUpgrade = Lens.lens (\DBInstance' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@DBInstance' {} a -> s {autoMinorVersionUpgrade = a} :: DBInstance)

-- | Specifies the name of the Availability Zone the DB instance is located
-- in.
dbInstance_availabilityZone :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_availabilityZone = Lens.lens (\DBInstance' {availabilityZone} -> availabilityZone) (\s@DBInstance' {} a -> s {availabilityZone = a} :: DBInstance)

-- | Specifies the number of days for which automatic DB snapshots are
-- retained.
dbInstance_backupRetentionPeriod :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Int)
dbInstance_backupRetentionPeriod = Lens.lens (\DBInstance' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@DBInstance' {} a -> s {backupRetentionPeriod = a} :: DBInstance)

-- | The identifier of the CA certificate for this DB instance.
dbInstance_cACertificateIdentifier :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_cACertificateIdentifier = Lens.lens (\DBInstance' {cACertificateIdentifier} -> cACertificateIdentifier) (\s@DBInstance' {} a -> s {cACertificateIdentifier = a} :: DBInstance)

-- | /(Not supported by Neptune)/
dbInstance_characterSetName :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_characterSetName = Lens.lens (\DBInstance' {characterSetName} -> characterSetName) (\s@DBInstance' {} a -> s {characterSetName = a} :: DBInstance)

-- | Specifies whether tags are copied from the DB instance to snapshots of
-- the DB instance.
dbInstance_copyTagsToSnapshot :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Bool)
dbInstance_copyTagsToSnapshot = Lens.lens (\DBInstance' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@DBInstance' {} a -> s {copyTagsToSnapshot = a} :: DBInstance)

-- | If the DB instance is a member of a DB cluster, contains the name of the
-- DB cluster that the DB instance is a member of.
dbInstance_dbClusterIdentifier :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_dbClusterIdentifier = Lens.lens (\DBInstance' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@DBInstance' {} a -> s {dbClusterIdentifier = a} :: DBInstance)

-- | The Amazon Resource Name (ARN) for the DB instance.
dbInstance_dbInstanceArn :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_dbInstanceArn = Lens.lens (\DBInstance' {dbInstanceArn} -> dbInstanceArn) (\s@DBInstance' {} a -> s {dbInstanceArn = a} :: DBInstance)

-- | Contains the name of the compute and memory capacity class of the DB
-- instance.
dbInstance_dbInstanceClass :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_dbInstanceClass = Lens.lens (\DBInstance' {dbInstanceClass} -> dbInstanceClass) (\s@DBInstance' {} a -> s {dbInstanceClass = a} :: DBInstance)

-- | Contains a user-supplied database identifier. This identifier is the
-- unique key that identifies a DB instance.
dbInstance_dbInstanceIdentifier :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_dbInstanceIdentifier = Lens.lens (\DBInstance' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@DBInstance' {} a -> s {dbInstanceIdentifier = a} :: DBInstance)

-- | Specifies the current state of this database.
dbInstance_dbInstanceStatus :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_dbInstanceStatus = Lens.lens (\DBInstance' {dbInstanceStatus} -> dbInstanceStatus) (\s@DBInstance' {} a -> s {dbInstanceStatus = a} :: DBInstance)

-- | The database name.
dbInstance_dbName :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_dbName = Lens.lens (\DBInstance' {dbName} -> dbName) (\s@DBInstance' {} a -> s {dbName = a} :: DBInstance)

-- | Provides the list of DB parameter groups applied to this DB instance.
dbInstance_dbParameterGroups :: Lens.Lens' DBInstance (Prelude.Maybe [DBParameterGroupStatus])
dbInstance_dbParameterGroups = Lens.lens (\DBInstance' {dbParameterGroups} -> dbParameterGroups) (\s@DBInstance' {} a -> s {dbParameterGroups = a} :: DBInstance) Prelude.. Lens.mapping Lens.coerced

-- | Provides List of DB security group elements containing only
-- @DBSecurityGroup.Name@ and @DBSecurityGroup.Status@ subelements.
dbInstance_dbSecurityGroups :: Lens.Lens' DBInstance (Prelude.Maybe [DBSecurityGroupMembership])
dbInstance_dbSecurityGroups = Lens.lens (\DBInstance' {dbSecurityGroups} -> dbSecurityGroups) (\s@DBInstance' {} a -> s {dbSecurityGroups = a} :: DBInstance) Prelude.. Lens.mapping Lens.coerced

-- | Specifies information on the subnet group associated with the DB
-- instance, including the name, description, and subnets in the subnet
-- group.
dbInstance_dbSubnetGroup :: Lens.Lens' DBInstance (Prelude.Maybe DBSubnetGroup)
dbInstance_dbSubnetGroup = Lens.lens (\DBInstance' {dbSubnetGroup} -> dbSubnetGroup) (\s@DBInstance' {} a -> s {dbSubnetGroup = a} :: DBInstance)

-- | Specifies the port that the DB instance listens on. If the DB instance
-- is part of a DB cluster, this can be a different port than the DB
-- cluster port.
dbInstance_dbInstancePort :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Int)
dbInstance_dbInstancePort = Lens.lens (\DBInstance' {dbInstancePort} -> dbInstancePort) (\s@DBInstance' {} a -> s {dbInstancePort = a} :: DBInstance)

-- | The Amazon Region-unique, immutable identifier for the DB instance. This
-- identifier is found in Amazon CloudTrail log entries whenever the Amazon
-- KMS key for the DB instance is accessed.
dbInstance_dbiResourceId :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_dbiResourceId = Lens.lens (\DBInstance' {dbiResourceId} -> dbiResourceId) (\s@DBInstance' {} a -> s {dbiResourceId = a} :: DBInstance)

-- | Indicates whether or not the DB instance has deletion protection
-- enabled. The instance can\'t be deleted when deletion protection is
-- enabled. See
-- <https://docs.aws.amazon.com/neptune/latest/userguide/manage-console-instances-delete.html Deleting a DB Instance>.
dbInstance_deletionProtection :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Bool)
dbInstance_deletionProtection = Lens.lens (\DBInstance' {deletionProtection} -> deletionProtection) (\s@DBInstance' {} a -> s {deletionProtection = a} :: DBInstance)

-- | Not supported
dbInstance_domainMemberships :: Lens.Lens' DBInstance (Prelude.Maybe [DomainMembership])
dbInstance_domainMemberships = Lens.lens (\DBInstance' {domainMemberships} -> domainMemberships) (\s@DBInstance' {} a -> s {domainMemberships = a} :: DBInstance) Prelude.. Lens.mapping Lens.coerced

-- | A list of log types that this DB instance is configured to export to
-- CloudWatch Logs.
dbInstance_enabledCloudwatchLogsExports :: Lens.Lens' DBInstance (Prelude.Maybe [Prelude.Text])
dbInstance_enabledCloudwatchLogsExports = Lens.lens (\DBInstance' {enabledCloudwatchLogsExports} -> enabledCloudwatchLogsExports) (\s@DBInstance' {} a -> s {enabledCloudwatchLogsExports = a} :: DBInstance) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the connection endpoint.
dbInstance_endpoint :: Lens.Lens' DBInstance (Prelude.Maybe Endpoint)
dbInstance_endpoint = Lens.lens (\DBInstance' {endpoint} -> endpoint) (\s@DBInstance' {} a -> s {endpoint = a} :: DBInstance)

-- | Provides the name of the database engine to be used for this DB
-- instance.
dbInstance_engine :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_engine = Lens.lens (\DBInstance' {engine} -> engine) (\s@DBInstance' {} a -> s {engine = a} :: DBInstance)

-- | Indicates the database engine version.
dbInstance_engineVersion :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_engineVersion = Lens.lens (\DBInstance' {engineVersion} -> engineVersion) (\s@DBInstance' {} a -> s {engineVersion = a} :: DBInstance)

-- | The Amazon Resource Name (ARN) of the Amazon CloudWatch Logs log stream
-- that receives the Enhanced Monitoring metrics data for the DB instance.
dbInstance_enhancedMonitoringResourceArn :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_enhancedMonitoringResourceArn = Lens.lens (\DBInstance' {enhancedMonitoringResourceArn} -> enhancedMonitoringResourceArn) (\s@DBInstance' {} a -> s {enhancedMonitoringResourceArn = a} :: DBInstance)

-- | True if Amazon Identity and Access Management (IAM) authentication is
-- enabled, and otherwise false.
dbInstance_iAMDatabaseAuthenticationEnabled :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Bool)
dbInstance_iAMDatabaseAuthenticationEnabled = Lens.lens (\DBInstance' {iAMDatabaseAuthenticationEnabled} -> iAMDatabaseAuthenticationEnabled) (\s@DBInstance' {} a -> s {iAMDatabaseAuthenticationEnabled = a} :: DBInstance)

-- | Provides the date and time the DB instance was created.
dbInstance_instanceCreateTime :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.UTCTime)
dbInstance_instanceCreateTime = Lens.lens (\DBInstance' {instanceCreateTime} -> instanceCreateTime) (\s@DBInstance' {} a -> s {instanceCreateTime = a} :: DBInstance) Prelude.. Lens.mapping Data._Time

-- | Specifies the Provisioned IOPS (I\/O operations per second) value.
dbInstance_iops :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Int)
dbInstance_iops = Lens.lens (\DBInstance' {iops} -> iops) (\s@DBInstance' {} a -> s {iops = a} :: DBInstance)

-- | Not supported: The encryption for DB instances is managed by the DB
-- cluster.
dbInstance_kmsKeyId :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_kmsKeyId = Lens.lens (\DBInstance' {kmsKeyId} -> kmsKeyId) (\s@DBInstance' {} a -> s {kmsKeyId = a} :: DBInstance)

-- | Specifies the latest time to which a database can be restored with
-- point-in-time restore.
dbInstance_latestRestorableTime :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.UTCTime)
dbInstance_latestRestorableTime = Lens.lens (\DBInstance' {latestRestorableTime} -> latestRestorableTime) (\s@DBInstance' {} a -> s {latestRestorableTime = a} :: DBInstance) Prelude.. Lens.mapping Data._Time

-- | License model information for this DB instance.
dbInstance_licenseModel :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_licenseModel = Lens.lens (\DBInstance' {licenseModel} -> licenseModel) (\s@DBInstance' {} a -> s {licenseModel = a} :: DBInstance)

-- | Not supported by Neptune.
dbInstance_masterUsername :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_masterUsername = Lens.lens (\DBInstance' {masterUsername} -> masterUsername) (\s@DBInstance' {} a -> s {masterUsername = a} :: DBInstance)

-- | The interval, in seconds, between points when Enhanced Monitoring
-- metrics are collected for the DB instance.
dbInstance_monitoringInterval :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Int)
dbInstance_monitoringInterval = Lens.lens (\DBInstance' {monitoringInterval} -> monitoringInterval) (\s@DBInstance' {} a -> s {monitoringInterval = a} :: DBInstance)

-- | The ARN for the IAM role that permits Neptune to send Enhanced
-- Monitoring metrics to Amazon CloudWatch Logs.
dbInstance_monitoringRoleArn :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_monitoringRoleArn = Lens.lens (\DBInstance' {monitoringRoleArn} -> monitoringRoleArn) (\s@DBInstance' {} a -> s {monitoringRoleArn = a} :: DBInstance)

-- | Specifies if the DB instance is a Multi-AZ deployment.
dbInstance_multiAZ :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Bool)
dbInstance_multiAZ = Lens.lens (\DBInstance' {multiAZ} -> multiAZ) (\s@DBInstance' {} a -> s {multiAZ = a} :: DBInstance)

-- | /(Not supported by Neptune)/
dbInstance_optionGroupMemberships :: Lens.Lens' DBInstance (Prelude.Maybe [OptionGroupMembership])
dbInstance_optionGroupMemberships = Lens.lens (\DBInstance' {optionGroupMemberships} -> optionGroupMemberships) (\s@DBInstance' {} a -> s {optionGroupMemberships = a} :: DBInstance) Prelude.. Lens.mapping Lens.coerced

-- | Specifies that changes to the DB instance are pending. This element is
-- only included when changes are pending. Specific changes are identified
-- by subelements.
dbInstance_pendingModifiedValues :: Lens.Lens' DBInstance (Prelude.Maybe PendingModifiedValues)
dbInstance_pendingModifiedValues = Lens.lens (\DBInstance' {pendingModifiedValues} -> pendingModifiedValues) (\s@DBInstance' {} a -> s {pendingModifiedValues = a} :: DBInstance)

-- | /(Not supported by Neptune)/
dbInstance_performanceInsightsEnabled :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Bool)
dbInstance_performanceInsightsEnabled = Lens.lens (\DBInstance' {performanceInsightsEnabled} -> performanceInsightsEnabled) (\s@DBInstance' {} a -> s {performanceInsightsEnabled = a} :: DBInstance)

-- | /(Not supported by Neptune)/
dbInstance_performanceInsightsKMSKeyId :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_performanceInsightsKMSKeyId = Lens.lens (\DBInstance' {performanceInsightsKMSKeyId} -> performanceInsightsKMSKeyId) (\s@DBInstance' {} a -> s {performanceInsightsKMSKeyId = a} :: DBInstance)

-- | Specifies the daily time range during which automated backups are
-- created if automated backups are enabled, as determined by the
-- @BackupRetentionPeriod@.
dbInstance_preferredBackupWindow :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_preferredBackupWindow = Lens.lens (\DBInstance' {preferredBackupWindow} -> preferredBackupWindow) (\s@DBInstance' {} a -> s {preferredBackupWindow = a} :: DBInstance)

-- | Specifies the weekly time range during which system maintenance can
-- occur, in Universal Coordinated Time (UTC).
dbInstance_preferredMaintenanceWindow :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_preferredMaintenanceWindow = Lens.lens (\DBInstance' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@DBInstance' {} a -> s {preferredMaintenanceWindow = a} :: DBInstance)

-- | A value that specifies the order in which a Read Replica is promoted to
-- the primary instance after a failure of the existing primary instance.
dbInstance_promotionTier :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Int)
dbInstance_promotionTier = Lens.lens (\DBInstance' {promotionTier} -> promotionTier) (\s@DBInstance' {} a -> s {promotionTier = a} :: DBInstance)

-- | This flag should no longer be used.
dbInstance_publiclyAccessible :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Bool)
dbInstance_publiclyAccessible = Lens.lens (\DBInstance' {publiclyAccessible} -> publiclyAccessible) (\s@DBInstance' {} a -> s {publiclyAccessible = a} :: DBInstance)

-- | Contains one or more identifiers of DB clusters that are Read Replicas
-- of this DB instance.
dbInstance_readReplicaDBClusterIdentifiers :: Lens.Lens' DBInstance (Prelude.Maybe [Prelude.Text])
dbInstance_readReplicaDBClusterIdentifiers = Lens.lens (\DBInstance' {readReplicaDBClusterIdentifiers} -> readReplicaDBClusterIdentifiers) (\s@DBInstance' {} a -> s {readReplicaDBClusterIdentifiers = a} :: DBInstance) Prelude.. Lens.mapping Lens.coerced

-- | Contains one or more identifiers of the Read Replicas associated with
-- this DB instance.
dbInstance_readReplicaDBInstanceIdentifiers :: Lens.Lens' DBInstance (Prelude.Maybe [Prelude.Text])
dbInstance_readReplicaDBInstanceIdentifiers = Lens.lens (\DBInstance' {readReplicaDBInstanceIdentifiers} -> readReplicaDBInstanceIdentifiers) (\s@DBInstance' {} a -> s {readReplicaDBInstanceIdentifiers = a} :: DBInstance) Prelude.. Lens.mapping Lens.coerced

-- | Contains the identifier of the source DB instance if this DB instance is
-- a Read Replica.
dbInstance_readReplicaSourceDBInstanceIdentifier :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_readReplicaSourceDBInstanceIdentifier = Lens.lens (\DBInstance' {readReplicaSourceDBInstanceIdentifier} -> readReplicaSourceDBInstanceIdentifier) (\s@DBInstance' {} a -> s {readReplicaSourceDBInstanceIdentifier = a} :: DBInstance)

-- | If present, specifies the name of the secondary Availability Zone for a
-- DB instance with multi-AZ support.
dbInstance_secondaryAvailabilityZone :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_secondaryAvailabilityZone = Lens.lens (\DBInstance' {secondaryAvailabilityZone} -> secondaryAvailabilityZone) (\s@DBInstance' {} a -> s {secondaryAvailabilityZone = a} :: DBInstance)

-- | The status of a Read Replica. If the instance is not a Read Replica,
-- this is blank.
dbInstance_statusInfos :: Lens.Lens' DBInstance (Prelude.Maybe [DBInstanceStatusInfo])
dbInstance_statusInfos = Lens.lens (\DBInstance' {statusInfos} -> statusInfos) (\s@DBInstance' {} a -> s {statusInfos = a} :: DBInstance) Prelude.. Lens.mapping Lens.coerced

-- | Not supported: The encryption for DB instances is managed by the DB
-- cluster.
dbInstance_storageEncrypted :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Bool)
dbInstance_storageEncrypted = Lens.lens (\DBInstance' {storageEncrypted} -> storageEncrypted) (\s@DBInstance' {} a -> s {storageEncrypted = a} :: DBInstance)

-- | Specifies the storage type associated with DB instance.
dbInstance_storageType :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_storageType = Lens.lens (\DBInstance' {storageType} -> storageType) (\s@DBInstance' {} a -> s {storageType = a} :: DBInstance)

-- | The ARN from the key store with which the instance is associated for TDE
-- encryption.
dbInstance_tdeCredentialArn :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_tdeCredentialArn = Lens.lens (\DBInstance' {tdeCredentialArn} -> tdeCredentialArn) (\s@DBInstance' {} a -> s {tdeCredentialArn = a} :: DBInstance)

-- | Not supported.
dbInstance_timezone :: Lens.Lens' DBInstance (Prelude.Maybe Prelude.Text)
dbInstance_timezone = Lens.lens (\DBInstance' {timezone} -> timezone) (\s@DBInstance' {} a -> s {timezone = a} :: DBInstance)

-- | Provides a list of VPC security group elements that the DB instance
-- belongs to.
dbInstance_vpcSecurityGroups :: Lens.Lens' DBInstance (Prelude.Maybe [VpcSecurityGroupMembership])
dbInstance_vpcSecurityGroups = Lens.lens (\DBInstance' {vpcSecurityGroups} -> vpcSecurityGroups) (\s@DBInstance' {} a -> s {vpcSecurityGroups = a} :: DBInstance) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML DBInstance where
  parseXML x =
    DBInstance'
      Prelude.<$> (x Data..@? "AllocatedStorage")
      Prelude.<*> (x Data..@? "AutoMinorVersionUpgrade")
      Prelude.<*> (x Data..@? "AvailabilityZone")
      Prelude.<*> (x Data..@? "BackupRetentionPeriod")
      Prelude.<*> (x Data..@? "CACertificateIdentifier")
      Prelude.<*> (x Data..@? "CharacterSetName")
      Prelude.<*> (x Data..@? "CopyTagsToSnapshot")
      Prelude.<*> (x Data..@? "DBClusterIdentifier")
      Prelude.<*> (x Data..@? "DBInstanceArn")
      Prelude.<*> (x Data..@? "DBInstanceClass")
      Prelude.<*> (x Data..@? "DBInstanceIdentifier")
      Prelude.<*> (x Data..@? "DBInstanceStatus")
      Prelude.<*> (x Data..@? "DBName")
      Prelude.<*> ( x
                      Data..@? "DBParameterGroups"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "DBParameterGroup")
                  )
      Prelude.<*> ( x
                      Data..@? "DBSecurityGroups"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "DBSecurityGroup")
                  )
      Prelude.<*> (x Data..@? "DBSubnetGroup")
      Prelude.<*> (x Data..@? "DbInstancePort")
      Prelude.<*> (x Data..@? "DbiResourceId")
      Prelude.<*> (x Data..@? "DeletionProtection")
      Prelude.<*> ( x
                      Data..@? "DomainMemberships"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "DomainMembership")
                  )
      Prelude.<*> ( x
                      Data..@? "EnabledCloudwatchLogsExports"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )
      Prelude.<*> (x Data..@? "Endpoint")
      Prelude.<*> (x Data..@? "Engine")
      Prelude.<*> (x Data..@? "EngineVersion")
      Prelude.<*> (x Data..@? "EnhancedMonitoringResourceArn")
      Prelude.<*> (x Data..@? "IAMDatabaseAuthenticationEnabled")
      Prelude.<*> (x Data..@? "InstanceCreateTime")
      Prelude.<*> (x Data..@? "Iops")
      Prelude.<*> (x Data..@? "KmsKeyId")
      Prelude.<*> (x Data..@? "LatestRestorableTime")
      Prelude.<*> (x Data..@? "LicenseModel")
      Prelude.<*> (x Data..@? "MasterUsername")
      Prelude.<*> (x Data..@? "MonitoringInterval")
      Prelude.<*> (x Data..@? "MonitoringRoleArn")
      Prelude.<*> (x Data..@? "MultiAZ")
      Prelude.<*> ( x
                      Data..@? "OptionGroupMemberships"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "OptionGroupMembership")
                  )
      Prelude.<*> (x Data..@? "PendingModifiedValues")
      Prelude.<*> (x Data..@? "PerformanceInsightsEnabled")
      Prelude.<*> (x Data..@? "PerformanceInsightsKMSKeyId")
      Prelude.<*> (x Data..@? "PreferredBackupWindow")
      Prelude.<*> (x Data..@? "PreferredMaintenanceWindow")
      Prelude.<*> (x Data..@? "PromotionTier")
      Prelude.<*> (x Data..@? "PubliclyAccessible")
      Prelude.<*> ( x
                      Data..@? "ReadReplicaDBClusterIdentifiers"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        (Data.parseXMLList "ReadReplicaDBClusterIdentifier")
                  )
      Prelude.<*> ( x
                      Data..@? "ReadReplicaDBInstanceIdentifiers"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        ( Data.parseXMLList
                            "ReadReplicaDBInstanceIdentifier"
                        )
                  )
      Prelude.<*> (x Data..@? "ReadReplicaSourceDBInstanceIdentifier")
      Prelude.<*> (x Data..@? "SecondaryAvailabilityZone")
      Prelude.<*> ( x
                      Data..@? "StatusInfos"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "DBInstanceStatusInfo")
                  )
      Prelude.<*> (x Data..@? "StorageEncrypted")
      Prelude.<*> (x Data..@? "StorageType")
      Prelude.<*> (x Data..@? "TdeCredentialArn")
      Prelude.<*> (x Data..@? "Timezone")
      Prelude.<*> ( x
                      Data..@? "VpcSecurityGroups"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        (Data.parseXMLList "VpcSecurityGroupMembership")
                  )

instance Prelude.Hashable DBInstance where
  hashWithSalt _salt DBInstance' {..} =
    _salt
      `Prelude.hashWithSalt` allocatedStorage
      `Prelude.hashWithSalt` autoMinorVersionUpgrade
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` backupRetentionPeriod
      `Prelude.hashWithSalt` cACertificateIdentifier
      `Prelude.hashWithSalt` characterSetName
      `Prelude.hashWithSalt` copyTagsToSnapshot
      `Prelude.hashWithSalt` dbClusterIdentifier
      `Prelude.hashWithSalt` dbInstanceArn
      `Prelude.hashWithSalt` dbInstanceClass
      `Prelude.hashWithSalt` dbInstanceIdentifier
      `Prelude.hashWithSalt` dbInstanceStatus
      `Prelude.hashWithSalt` dbName
      `Prelude.hashWithSalt` dbParameterGroups
      `Prelude.hashWithSalt` dbSecurityGroups
      `Prelude.hashWithSalt` dbSubnetGroup
      `Prelude.hashWithSalt` dbInstancePort
      `Prelude.hashWithSalt` dbiResourceId
      `Prelude.hashWithSalt` deletionProtection
      `Prelude.hashWithSalt` domainMemberships
      `Prelude.hashWithSalt` enabledCloudwatchLogsExports
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
      `Prelude.hashWithSalt` masterUsername
      `Prelude.hashWithSalt` monitoringInterval
      `Prelude.hashWithSalt` monitoringRoleArn
      `Prelude.hashWithSalt` multiAZ
      `Prelude.hashWithSalt` optionGroupMemberships
      `Prelude.hashWithSalt` pendingModifiedValues
      `Prelude.hashWithSalt` performanceInsightsEnabled
      `Prelude.hashWithSalt` performanceInsightsKMSKeyId
      `Prelude.hashWithSalt` preferredBackupWindow
      `Prelude.hashWithSalt` preferredMaintenanceWindow
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

instance Prelude.NFData DBInstance where
  rnf DBInstance' {..} =
    Prelude.rnf allocatedStorage
      `Prelude.seq` Prelude.rnf autoMinorVersionUpgrade
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf backupRetentionPeriod
      `Prelude.seq` Prelude.rnf cACertificateIdentifier
      `Prelude.seq` Prelude.rnf characterSetName
      `Prelude.seq` Prelude.rnf copyTagsToSnapshot
      `Prelude.seq` Prelude.rnf dbClusterIdentifier
      `Prelude.seq` Prelude.rnf dbInstanceArn
      `Prelude.seq` Prelude.rnf dbInstanceClass
      `Prelude.seq` Prelude.rnf dbInstanceIdentifier
      `Prelude.seq` Prelude.rnf dbInstanceStatus
      `Prelude.seq` Prelude.rnf dbName
      `Prelude.seq` Prelude.rnf dbParameterGroups
      `Prelude.seq` Prelude.rnf dbSecurityGroups
      `Prelude.seq` Prelude.rnf dbSubnetGroup
      `Prelude.seq` Prelude.rnf dbInstancePort
      `Prelude.seq` Prelude.rnf dbiResourceId
      `Prelude.seq` Prelude.rnf deletionProtection
      `Prelude.seq` Prelude.rnf domainMemberships
      `Prelude.seq` Prelude.rnf
        enabledCloudwatchLogsExports
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
        masterUsername
      `Prelude.seq` Prelude.rnf
        monitoringInterval
      `Prelude.seq` Prelude.rnf
        monitoringRoleArn
      `Prelude.seq` Prelude.rnf
        multiAZ
      `Prelude.seq` Prelude.rnf
        optionGroupMemberships
      `Prelude.seq` Prelude.rnf
        pendingModifiedValues
      `Prelude.seq` Prelude.rnf
        performanceInsightsEnabled
      `Prelude.seq` Prelude.rnf
        performanceInsightsKMSKeyId
      `Prelude.seq` Prelude.rnf
        preferredBackupWindow
      `Prelude.seq` Prelude.rnf
        preferredMaintenanceWindow
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
