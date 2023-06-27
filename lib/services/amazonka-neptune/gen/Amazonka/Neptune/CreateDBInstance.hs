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
-- Module      : Amazonka.Neptune.CreateDBInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB instance.
module Amazonka.Neptune.CreateDBInstance
  ( -- * Creating a Request
    CreateDBInstance (..),
    newCreateDBInstance,

    -- * Request Lenses
    createDBInstance_allocatedStorage,
    createDBInstance_autoMinorVersionUpgrade,
    createDBInstance_availabilityZone,
    createDBInstance_backupRetentionPeriod,
    createDBInstance_characterSetName,
    createDBInstance_copyTagsToSnapshot,
    createDBInstance_dbName,
    createDBInstance_dbParameterGroupName,
    createDBInstance_dbSecurityGroups,
    createDBInstance_dbSubnetGroupName,
    createDBInstance_deletionProtection,
    createDBInstance_domain,
    createDBInstance_domainIAMRoleName,
    createDBInstance_enableCloudwatchLogsExports,
    createDBInstance_enableIAMDatabaseAuthentication,
    createDBInstance_enablePerformanceInsights,
    createDBInstance_engineVersion,
    createDBInstance_iops,
    createDBInstance_kmsKeyId,
    createDBInstance_licenseModel,
    createDBInstance_masterUserPassword,
    createDBInstance_masterUsername,
    createDBInstance_monitoringInterval,
    createDBInstance_monitoringRoleArn,
    createDBInstance_multiAZ,
    createDBInstance_optionGroupName,
    createDBInstance_performanceInsightsKMSKeyId,
    createDBInstance_port,
    createDBInstance_preferredBackupWindow,
    createDBInstance_preferredMaintenanceWindow,
    createDBInstance_promotionTier,
    createDBInstance_publiclyAccessible,
    createDBInstance_storageEncrypted,
    createDBInstance_storageType,
    createDBInstance_tags,
    createDBInstance_tdeCredentialArn,
    createDBInstance_tdeCredentialPassword,
    createDBInstance_timezone,
    createDBInstance_vpcSecurityGroupIds,
    createDBInstance_dbInstanceIdentifier,
    createDBInstance_dbInstanceClass,
    createDBInstance_engine,
    createDBInstance_dbClusterIdentifier,

    -- * Destructuring the Response
    CreateDBInstanceResponse (..),
    newCreateDBInstanceResponse,

    -- * Response Lenses
    createDBInstanceResponse_dbInstance,
    createDBInstanceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Neptune.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDBInstance' smart constructor.
data CreateDBInstance = CreateDBInstance'
  { -- | Not supported by Neptune.
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | Indicates that minor engine upgrades are applied automatically to the DB
    -- instance during the maintenance window.
    --
    -- Default: @true@
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The EC2 Availability Zone that the DB instance is created in
    --
    -- Default: A random, system-chosen Availability Zone in the endpoint\'s
    -- Amazon Region.
    --
    -- Example: @us-east-1d@
    --
    -- Constraint: The AvailabilityZone parameter can\'t be specified if the
    -- MultiAZ parameter is set to @true@. The specified Availability Zone must
    -- be in the same Amazon Region as the current endpoint.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The number of days for which automated backups are retained.
    --
    -- Not applicable. The retention period for automated backups is managed by
    -- the DB cluster. For more information, see CreateDBCluster.
    --
    -- Default: 1
    --
    -- Constraints:
    --
    -- -   Must be a value from 0 to 35
    --
    -- -   Cannot be set to 0 if the DB instance is a source to Read Replicas
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | /(Not supported by Neptune)/
    characterSetName :: Prelude.Maybe Prelude.Text,
    -- | True to copy all tags from the DB instance to snapshots of the DB
    -- instance, and otherwise false. The default is false.
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | Not supported.
    dbName :: Prelude.Maybe Prelude.Text,
    -- | The name of the DB parameter group to associate with this DB instance.
    -- If this argument is omitted, the default DBParameterGroup for the
    -- specified engine is used.
    --
    -- Constraints:
    --
    -- -   Must be 1 to 255 letters, numbers, or hyphens.
    --
    -- -   First character must be a letter
    --
    -- -   Cannot end with a hyphen or contain two consecutive hyphens
    dbParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | A list of DB security groups to associate with this DB instance.
    --
    -- Default: The default DB security group for the database engine.
    dbSecurityGroups :: Prelude.Maybe [Prelude.Text],
    -- | A DB subnet group to associate with this DB instance.
    --
    -- If there is no DB subnet group, then it is a non-VPC DB instance.
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the DB instance has deletion protection
    -- enabled. The database can\'t be deleted when deletion protection is
    -- enabled. By default, deletion protection is disabled. See
    -- <https://docs.aws.amazon.com/neptune/latest/userguide/manage-console-instances-delete.html Deleting a DB Instance>.
    --
    -- DB instances in a DB cluster can be deleted even when deletion
    -- protection is enabled in their parent DB cluster.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | Specify the Active Directory Domain to create the instance in.
    domain :: Prelude.Maybe Prelude.Text,
    -- | Specify the name of the IAM role to be used when making API calls to the
    -- Directory Service.
    domainIAMRoleName :: Prelude.Maybe Prelude.Text,
    -- | The list of log types that need to be enabled for exporting to
    -- CloudWatch Logs.
    enableCloudwatchLogsExports :: Prelude.Maybe [Prelude.Text],
    -- | Not supported by Neptune (ignored).
    enableIAMDatabaseAuthentication :: Prelude.Maybe Prelude.Bool,
    -- | /(Not supported by Neptune)/
    enablePerformanceInsights :: Prelude.Maybe Prelude.Bool,
    -- | The version number of the database engine to use. Currently, setting
    -- this parameter has no effect.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The amount of Provisioned IOPS (input\/output operations per second) to
    -- be initially allocated for the DB instance.
    iops :: Prelude.Maybe Prelude.Int,
    -- | The Amazon KMS key identifier for an encrypted DB instance.
    --
    -- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS
    -- encryption key. If you are creating a DB instance with the same Amazon
    -- account that owns the KMS encryption key used to encrypt the new DB
    -- instance, then you can use the KMS key alias instead of the ARN for the
    -- KM encryption key.
    --
    -- Not applicable. The KMS key identifier is managed by the DB cluster. For
    -- more information, see CreateDBCluster.
    --
    -- If the @StorageEncrypted@ parameter is true, and you do not specify a
    -- value for the @KmsKeyId@ parameter, then Amazon Neptune will use your
    -- default encryption key. Amazon KMS creates the default encryption key
    -- for your Amazon account. Your Amazon account has a different default
    -- encryption key for each Amazon Region.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | License model information for this DB instance.
    --
    -- Valid values: @license-included@ | @bring-your-own-license@ |
    -- @general-public-license@
    licenseModel :: Prelude.Maybe Prelude.Text,
    -- | Not supported by Neptune.
    masterUserPassword :: Prelude.Maybe Prelude.Text,
    -- | Not supported by Neptune.
    masterUsername :: Prelude.Maybe Prelude.Text,
    -- | The interval, in seconds, between points when Enhanced Monitoring
    -- metrics are collected for the DB instance. To disable collecting
    -- Enhanced Monitoring metrics, specify 0. The default is 0.
    --
    -- If @MonitoringRoleArn@ is specified, then you must also set
    -- @MonitoringInterval@ to a value other than 0.
    --
    -- Valid Values: @0, 1, 5, 10, 15, 30, 60@
    monitoringInterval :: Prelude.Maybe Prelude.Int,
    -- | The ARN for the IAM role that permits Neptune to send enhanced
    -- monitoring metrics to Amazon CloudWatch Logs. For example,
    -- @arn:aws:iam:123456789012:role\/emaccess@.
    --
    -- If @MonitoringInterval@ is set to a value other than 0, then you must
    -- supply a @MonitoringRoleArn@ value.
    monitoringRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies if the DB instance is a Multi-AZ deployment. You can\'t set
    -- the AvailabilityZone parameter if the MultiAZ parameter is set to true.
    multiAZ :: Prelude.Maybe Prelude.Bool,
    -- | /(Not supported by Neptune)/
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | /(Not supported by Neptune)/
    performanceInsightsKMSKeyId :: Prelude.Maybe Prelude.Text,
    -- | The port number on which the database accepts connections.
    --
    -- Not applicable. The port is managed by the DB cluster. For more
    -- information, see CreateDBCluster.
    --
    -- Default: @8182@
    --
    -- Type: Integer
    port :: Prelude.Maybe Prelude.Int,
    -- | The daily time range during which automated backups are created.
    --
    -- Not applicable. The daily time range for creating automated backups is
    -- managed by the DB cluster. For more information, see CreateDBCluster.
    preferredBackupWindow :: Prelude.Maybe Prelude.Text,
    -- | The time range each week during which system maintenance can occur, in
    -- Universal Coordinated Time (UTC).
    --
    -- Format: @ddd:hh24:mi-ddd:hh24:mi@
    --
    -- The default is a 30-minute window selected at random from an 8-hour
    -- block of time for each Amazon Region, occurring on a random day of the
    -- week.
    --
    -- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
    --
    -- Constraints: Minimum 30-minute window.
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | A value that specifies the order in which an Read Replica is promoted to
    -- the primary instance after a failure of the existing primary instance.
    --
    -- Default: 1
    --
    -- Valid Values: 0 - 15
    promotionTier :: Prelude.Maybe Prelude.Int,
    -- | This flag should no longer be used.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether the DB instance is encrypted.
    --
    -- Not applicable. The encryption for DB instances is managed by the DB
    -- cluster. For more information, see CreateDBCluster.
    --
    -- Default: false
    storageEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the storage type to be associated with the DB instance.
    --
    -- Not applicable. Storage is managed by the DB Cluster.
    storageType :: Prelude.Maybe Prelude.Text,
    -- | The tags to assign to the new instance.
    tags :: Prelude.Maybe [Tag],
    -- | The ARN from the key store with which to associate the instance for TDE
    -- encryption.
    tdeCredentialArn :: Prelude.Maybe Prelude.Text,
    -- | The password for the given ARN from the key store in order to access the
    -- device.
    tdeCredentialPassword :: Prelude.Maybe Prelude.Text,
    -- | The time zone of the DB instance.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | A list of EC2 VPC security groups to associate with this DB instance.
    --
    -- Not applicable. The associated list of EC2 VPC security groups is
    -- managed by the DB cluster. For more information, see CreateDBCluster.
    --
    -- Default: The default EC2 VPC security group for the DB subnet group\'s
    -- VPC.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The DB instance identifier. This parameter is stored as a lowercase
    -- string.
    --
    -- Constraints:
    --
    -- -   Must contain from 1 to 63 letters, numbers, or hyphens.
    --
    -- -   First character must be a letter.
    --
    -- -   Cannot end with a hyphen or contain two consecutive hyphens.
    --
    -- Example: @mydbinstance@
    dbInstanceIdentifier :: Prelude.Text,
    -- | The compute and memory capacity of the DB instance, for example,
    -- @db.m4.large@. Not all DB instance classes are available in all Amazon
    -- Regions.
    dbInstanceClass :: Prelude.Text,
    -- | The name of the database engine to be used for this instance.
    --
    -- Valid Values: @neptune@
    engine :: Prelude.Text,
    -- | The identifier of the DB cluster that the instance will belong to.
    --
    -- For information on creating a DB cluster, see CreateDBCluster.
    --
    -- Type: String
    dbClusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDBInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allocatedStorage', 'createDBInstance_allocatedStorage' - Not supported by Neptune.
--
-- 'autoMinorVersionUpgrade', 'createDBInstance_autoMinorVersionUpgrade' - Indicates that minor engine upgrades are applied automatically to the DB
-- instance during the maintenance window.
--
-- Default: @true@
--
-- 'availabilityZone', 'createDBInstance_availabilityZone' - The EC2 Availability Zone that the DB instance is created in
--
-- Default: A random, system-chosen Availability Zone in the endpoint\'s
-- Amazon Region.
--
-- Example: @us-east-1d@
--
-- Constraint: The AvailabilityZone parameter can\'t be specified if the
-- MultiAZ parameter is set to @true@. The specified Availability Zone must
-- be in the same Amazon Region as the current endpoint.
--
-- 'backupRetentionPeriod', 'createDBInstance_backupRetentionPeriod' - The number of days for which automated backups are retained.
--
-- Not applicable. The retention period for automated backups is managed by
-- the DB cluster. For more information, see CreateDBCluster.
--
-- Default: 1
--
-- Constraints:
--
-- -   Must be a value from 0 to 35
--
-- -   Cannot be set to 0 if the DB instance is a source to Read Replicas
--
-- 'characterSetName', 'createDBInstance_characterSetName' - /(Not supported by Neptune)/
--
-- 'copyTagsToSnapshot', 'createDBInstance_copyTagsToSnapshot' - True to copy all tags from the DB instance to snapshots of the DB
-- instance, and otherwise false. The default is false.
--
-- 'dbName', 'createDBInstance_dbName' - Not supported.
--
-- 'dbParameterGroupName', 'createDBInstance_dbParameterGroupName' - The name of the DB parameter group to associate with this DB instance.
-- If this argument is omitted, the default DBParameterGroup for the
-- specified engine is used.
--
-- Constraints:
--
-- -   Must be 1 to 255 letters, numbers, or hyphens.
--
-- -   First character must be a letter
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens
--
-- 'dbSecurityGroups', 'createDBInstance_dbSecurityGroups' - A list of DB security groups to associate with this DB instance.
--
-- Default: The default DB security group for the database engine.
--
-- 'dbSubnetGroupName', 'createDBInstance_dbSubnetGroupName' - A DB subnet group to associate with this DB instance.
--
-- If there is no DB subnet group, then it is a non-VPC DB instance.
--
-- 'deletionProtection', 'createDBInstance_deletionProtection' - A value that indicates whether the DB instance has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled. See
-- <https://docs.aws.amazon.com/neptune/latest/userguide/manage-console-instances-delete.html Deleting a DB Instance>.
--
-- DB instances in a DB cluster can be deleted even when deletion
-- protection is enabled in their parent DB cluster.
--
-- 'domain', 'createDBInstance_domain' - Specify the Active Directory Domain to create the instance in.
--
-- 'domainIAMRoleName', 'createDBInstance_domainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
--
-- 'enableCloudwatchLogsExports', 'createDBInstance_enableCloudwatchLogsExports' - The list of log types that need to be enabled for exporting to
-- CloudWatch Logs.
--
-- 'enableIAMDatabaseAuthentication', 'createDBInstance_enableIAMDatabaseAuthentication' - Not supported by Neptune (ignored).
--
-- 'enablePerformanceInsights', 'createDBInstance_enablePerformanceInsights' - /(Not supported by Neptune)/
--
-- 'engineVersion', 'createDBInstance_engineVersion' - The version number of the database engine to use. Currently, setting
-- this parameter has no effect.
--
-- 'iops', 'createDBInstance_iops' - The amount of Provisioned IOPS (input\/output operations per second) to
-- be initially allocated for the DB instance.
--
-- 'kmsKeyId', 'createDBInstance_kmsKeyId' - The Amazon KMS key identifier for an encrypted DB instance.
--
-- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS
-- encryption key. If you are creating a DB instance with the same Amazon
-- account that owns the KMS encryption key used to encrypt the new DB
-- instance, then you can use the KMS key alias instead of the ARN for the
-- KM encryption key.
--
-- Not applicable. The KMS key identifier is managed by the DB cluster. For
-- more information, see CreateDBCluster.
--
-- If the @StorageEncrypted@ parameter is true, and you do not specify a
-- value for the @KmsKeyId@ parameter, then Amazon Neptune will use your
-- default encryption key. Amazon KMS creates the default encryption key
-- for your Amazon account. Your Amazon account has a different default
-- encryption key for each Amazon Region.
--
-- 'licenseModel', 'createDBInstance_licenseModel' - License model information for this DB instance.
--
-- Valid values: @license-included@ | @bring-your-own-license@ |
-- @general-public-license@
--
-- 'masterUserPassword', 'createDBInstance_masterUserPassword' - Not supported by Neptune.
--
-- 'masterUsername', 'createDBInstance_masterUsername' - Not supported by Neptune.
--
-- 'monitoringInterval', 'createDBInstance_monitoringInterval' - The interval, in seconds, between points when Enhanced Monitoring
-- metrics are collected for the DB instance. To disable collecting
-- Enhanced Monitoring metrics, specify 0. The default is 0.
--
-- If @MonitoringRoleArn@ is specified, then you must also set
-- @MonitoringInterval@ to a value other than 0.
--
-- Valid Values: @0, 1, 5, 10, 15, 30, 60@
--
-- 'monitoringRoleArn', 'createDBInstance_monitoringRoleArn' - The ARN for the IAM role that permits Neptune to send enhanced
-- monitoring metrics to Amazon CloudWatch Logs. For example,
-- @arn:aws:iam:123456789012:role\/emaccess@.
--
-- If @MonitoringInterval@ is set to a value other than 0, then you must
-- supply a @MonitoringRoleArn@ value.
--
-- 'multiAZ', 'createDBInstance_multiAZ' - Specifies if the DB instance is a Multi-AZ deployment. You can\'t set
-- the AvailabilityZone parameter if the MultiAZ parameter is set to true.
--
-- 'optionGroupName', 'createDBInstance_optionGroupName' - /(Not supported by Neptune)/
--
-- 'performanceInsightsKMSKeyId', 'createDBInstance_performanceInsightsKMSKeyId' - /(Not supported by Neptune)/
--
-- 'port', 'createDBInstance_port' - The port number on which the database accepts connections.
--
-- Not applicable. The port is managed by the DB cluster. For more
-- information, see CreateDBCluster.
--
-- Default: @8182@
--
-- Type: Integer
--
-- 'preferredBackupWindow', 'createDBInstance_preferredBackupWindow' - The daily time range during which automated backups are created.
--
-- Not applicable. The daily time range for creating automated backups is
-- managed by the DB cluster. For more information, see CreateDBCluster.
--
-- 'preferredMaintenanceWindow', 'createDBInstance_preferredMaintenanceWindow' - The time range each week during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Region, occurring on a random day of the
-- week.
--
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
--
-- Constraints: Minimum 30-minute window.
--
-- 'promotionTier', 'createDBInstance_promotionTier' - A value that specifies the order in which an Read Replica is promoted to
-- the primary instance after a failure of the existing primary instance.
--
-- Default: 1
--
-- Valid Values: 0 - 15
--
-- 'publiclyAccessible', 'createDBInstance_publiclyAccessible' - This flag should no longer be used.
--
-- 'storageEncrypted', 'createDBInstance_storageEncrypted' - Specifies whether the DB instance is encrypted.
--
-- Not applicable. The encryption for DB instances is managed by the DB
-- cluster. For more information, see CreateDBCluster.
--
-- Default: false
--
-- 'storageType', 'createDBInstance_storageType' - Specifies the storage type to be associated with the DB instance.
--
-- Not applicable. Storage is managed by the DB Cluster.
--
-- 'tags', 'createDBInstance_tags' - The tags to assign to the new instance.
--
-- 'tdeCredentialArn', 'createDBInstance_tdeCredentialArn' - The ARN from the key store with which to associate the instance for TDE
-- encryption.
--
-- 'tdeCredentialPassword', 'createDBInstance_tdeCredentialPassword' - The password for the given ARN from the key store in order to access the
-- device.
--
-- 'timezone', 'createDBInstance_timezone' - The time zone of the DB instance.
--
-- 'vpcSecurityGroupIds', 'createDBInstance_vpcSecurityGroupIds' - A list of EC2 VPC security groups to associate with this DB instance.
--
-- Not applicable. The associated list of EC2 VPC security groups is
-- managed by the DB cluster. For more information, see CreateDBCluster.
--
-- Default: The default EC2 VPC security group for the DB subnet group\'s
-- VPC.
--
-- 'dbInstanceIdentifier', 'createDBInstance_dbInstanceIdentifier' - The DB instance identifier. This parameter is stored as a lowercase
-- string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens.
--
-- -   First character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- Example: @mydbinstance@
--
-- 'dbInstanceClass', 'createDBInstance_dbInstanceClass' - The compute and memory capacity of the DB instance, for example,
-- @db.m4.large@. Not all DB instance classes are available in all Amazon
-- Regions.
--
-- 'engine', 'createDBInstance_engine' - The name of the database engine to be used for this instance.
--
-- Valid Values: @neptune@
--
-- 'dbClusterIdentifier', 'createDBInstance_dbClusterIdentifier' - The identifier of the DB cluster that the instance will belong to.
--
-- For information on creating a DB cluster, see CreateDBCluster.
--
-- Type: String
newCreateDBInstance ::
  -- | 'dbInstanceIdentifier'
  Prelude.Text ->
  -- | 'dbInstanceClass'
  Prelude.Text ->
  -- | 'engine'
  Prelude.Text ->
  -- | 'dbClusterIdentifier'
  Prelude.Text ->
  CreateDBInstance
newCreateDBInstance
  pDBInstanceIdentifier_
  pDBInstanceClass_
  pEngine_
  pDBClusterIdentifier_ =
    CreateDBInstance'
      { allocatedStorage =
          Prelude.Nothing,
        autoMinorVersionUpgrade = Prelude.Nothing,
        availabilityZone = Prelude.Nothing,
        backupRetentionPeriod = Prelude.Nothing,
        characterSetName = Prelude.Nothing,
        copyTagsToSnapshot = Prelude.Nothing,
        dbName = Prelude.Nothing,
        dbParameterGroupName = Prelude.Nothing,
        dbSecurityGroups = Prelude.Nothing,
        dbSubnetGroupName = Prelude.Nothing,
        deletionProtection = Prelude.Nothing,
        domain = Prelude.Nothing,
        domainIAMRoleName = Prelude.Nothing,
        enableCloudwatchLogsExports = Prelude.Nothing,
        enableIAMDatabaseAuthentication = Prelude.Nothing,
        enablePerformanceInsights = Prelude.Nothing,
        engineVersion = Prelude.Nothing,
        iops = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        licenseModel = Prelude.Nothing,
        masterUserPassword = Prelude.Nothing,
        masterUsername = Prelude.Nothing,
        monitoringInterval = Prelude.Nothing,
        monitoringRoleArn = Prelude.Nothing,
        multiAZ = Prelude.Nothing,
        optionGroupName = Prelude.Nothing,
        performanceInsightsKMSKeyId = Prelude.Nothing,
        port = Prelude.Nothing,
        preferredBackupWindow = Prelude.Nothing,
        preferredMaintenanceWindow = Prelude.Nothing,
        promotionTier = Prelude.Nothing,
        publiclyAccessible = Prelude.Nothing,
        storageEncrypted = Prelude.Nothing,
        storageType = Prelude.Nothing,
        tags = Prelude.Nothing,
        tdeCredentialArn = Prelude.Nothing,
        tdeCredentialPassword = Prelude.Nothing,
        timezone = Prelude.Nothing,
        vpcSecurityGroupIds = Prelude.Nothing,
        dbInstanceIdentifier = pDBInstanceIdentifier_,
        dbInstanceClass = pDBInstanceClass_,
        engine = pEngine_,
        dbClusterIdentifier = pDBClusterIdentifier_
      }

-- | Not supported by Neptune.
createDBInstance_allocatedStorage :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Int)
createDBInstance_allocatedStorage = Lens.lens (\CreateDBInstance' {allocatedStorage} -> allocatedStorage) (\s@CreateDBInstance' {} a -> s {allocatedStorage = a} :: CreateDBInstance)

-- | Indicates that minor engine upgrades are applied automatically to the DB
-- instance during the maintenance window.
--
-- Default: @true@
createDBInstance_autoMinorVersionUpgrade :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Bool)
createDBInstance_autoMinorVersionUpgrade = Lens.lens (\CreateDBInstance' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@CreateDBInstance' {} a -> s {autoMinorVersionUpgrade = a} :: CreateDBInstance)

-- | The EC2 Availability Zone that the DB instance is created in
--
-- Default: A random, system-chosen Availability Zone in the endpoint\'s
-- Amazon Region.
--
-- Example: @us-east-1d@
--
-- Constraint: The AvailabilityZone parameter can\'t be specified if the
-- MultiAZ parameter is set to @true@. The specified Availability Zone must
-- be in the same Amazon Region as the current endpoint.
createDBInstance_availabilityZone :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_availabilityZone = Lens.lens (\CreateDBInstance' {availabilityZone} -> availabilityZone) (\s@CreateDBInstance' {} a -> s {availabilityZone = a} :: CreateDBInstance)

-- | The number of days for which automated backups are retained.
--
-- Not applicable. The retention period for automated backups is managed by
-- the DB cluster. For more information, see CreateDBCluster.
--
-- Default: 1
--
-- Constraints:
--
-- -   Must be a value from 0 to 35
--
-- -   Cannot be set to 0 if the DB instance is a source to Read Replicas
createDBInstance_backupRetentionPeriod :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Int)
createDBInstance_backupRetentionPeriod = Lens.lens (\CreateDBInstance' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@CreateDBInstance' {} a -> s {backupRetentionPeriod = a} :: CreateDBInstance)

-- | /(Not supported by Neptune)/
createDBInstance_characterSetName :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_characterSetName = Lens.lens (\CreateDBInstance' {characterSetName} -> characterSetName) (\s@CreateDBInstance' {} a -> s {characterSetName = a} :: CreateDBInstance)

-- | True to copy all tags from the DB instance to snapshots of the DB
-- instance, and otherwise false. The default is false.
createDBInstance_copyTagsToSnapshot :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Bool)
createDBInstance_copyTagsToSnapshot = Lens.lens (\CreateDBInstance' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@CreateDBInstance' {} a -> s {copyTagsToSnapshot = a} :: CreateDBInstance)

-- | Not supported.
createDBInstance_dbName :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_dbName = Lens.lens (\CreateDBInstance' {dbName} -> dbName) (\s@CreateDBInstance' {} a -> s {dbName = a} :: CreateDBInstance)

-- | The name of the DB parameter group to associate with this DB instance.
-- If this argument is omitted, the default DBParameterGroup for the
-- specified engine is used.
--
-- Constraints:
--
-- -   Must be 1 to 255 letters, numbers, or hyphens.
--
-- -   First character must be a letter
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens
createDBInstance_dbParameterGroupName :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_dbParameterGroupName = Lens.lens (\CreateDBInstance' {dbParameterGroupName} -> dbParameterGroupName) (\s@CreateDBInstance' {} a -> s {dbParameterGroupName = a} :: CreateDBInstance)

-- | A list of DB security groups to associate with this DB instance.
--
-- Default: The default DB security group for the database engine.
createDBInstance_dbSecurityGroups :: Lens.Lens' CreateDBInstance (Prelude.Maybe [Prelude.Text])
createDBInstance_dbSecurityGroups = Lens.lens (\CreateDBInstance' {dbSecurityGroups} -> dbSecurityGroups) (\s@CreateDBInstance' {} a -> s {dbSecurityGroups = a} :: CreateDBInstance) Prelude.. Lens.mapping Lens.coerced

-- | A DB subnet group to associate with this DB instance.
--
-- If there is no DB subnet group, then it is a non-VPC DB instance.
createDBInstance_dbSubnetGroupName :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_dbSubnetGroupName = Lens.lens (\CreateDBInstance' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@CreateDBInstance' {} a -> s {dbSubnetGroupName = a} :: CreateDBInstance)

-- | A value that indicates whether the DB instance has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled. See
-- <https://docs.aws.amazon.com/neptune/latest/userguide/manage-console-instances-delete.html Deleting a DB Instance>.
--
-- DB instances in a DB cluster can be deleted even when deletion
-- protection is enabled in their parent DB cluster.
createDBInstance_deletionProtection :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Bool)
createDBInstance_deletionProtection = Lens.lens (\CreateDBInstance' {deletionProtection} -> deletionProtection) (\s@CreateDBInstance' {} a -> s {deletionProtection = a} :: CreateDBInstance)

-- | Specify the Active Directory Domain to create the instance in.
createDBInstance_domain :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_domain = Lens.lens (\CreateDBInstance' {domain} -> domain) (\s@CreateDBInstance' {} a -> s {domain = a} :: CreateDBInstance)

-- | Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
createDBInstance_domainIAMRoleName :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_domainIAMRoleName = Lens.lens (\CreateDBInstance' {domainIAMRoleName} -> domainIAMRoleName) (\s@CreateDBInstance' {} a -> s {domainIAMRoleName = a} :: CreateDBInstance)

-- | The list of log types that need to be enabled for exporting to
-- CloudWatch Logs.
createDBInstance_enableCloudwatchLogsExports :: Lens.Lens' CreateDBInstance (Prelude.Maybe [Prelude.Text])
createDBInstance_enableCloudwatchLogsExports = Lens.lens (\CreateDBInstance' {enableCloudwatchLogsExports} -> enableCloudwatchLogsExports) (\s@CreateDBInstance' {} a -> s {enableCloudwatchLogsExports = a} :: CreateDBInstance) Prelude.. Lens.mapping Lens.coerced

-- | Not supported by Neptune (ignored).
createDBInstance_enableIAMDatabaseAuthentication :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Bool)
createDBInstance_enableIAMDatabaseAuthentication = Lens.lens (\CreateDBInstance' {enableIAMDatabaseAuthentication} -> enableIAMDatabaseAuthentication) (\s@CreateDBInstance' {} a -> s {enableIAMDatabaseAuthentication = a} :: CreateDBInstance)

-- | /(Not supported by Neptune)/
createDBInstance_enablePerformanceInsights :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Bool)
createDBInstance_enablePerformanceInsights = Lens.lens (\CreateDBInstance' {enablePerformanceInsights} -> enablePerformanceInsights) (\s@CreateDBInstance' {} a -> s {enablePerformanceInsights = a} :: CreateDBInstance)

-- | The version number of the database engine to use. Currently, setting
-- this parameter has no effect.
createDBInstance_engineVersion :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_engineVersion = Lens.lens (\CreateDBInstance' {engineVersion} -> engineVersion) (\s@CreateDBInstance' {} a -> s {engineVersion = a} :: CreateDBInstance)

-- | The amount of Provisioned IOPS (input\/output operations per second) to
-- be initially allocated for the DB instance.
createDBInstance_iops :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Int)
createDBInstance_iops = Lens.lens (\CreateDBInstance' {iops} -> iops) (\s@CreateDBInstance' {} a -> s {iops = a} :: CreateDBInstance)

-- | The Amazon KMS key identifier for an encrypted DB instance.
--
-- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS
-- encryption key. If you are creating a DB instance with the same Amazon
-- account that owns the KMS encryption key used to encrypt the new DB
-- instance, then you can use the KMS key alias instead of the ARN for the
-- KM encryption key.
--
-- Not applicable. The KMS key identifier is managed by the DB cluster. For
-- more information, see CreateDBCluster.
--
-- If the @StorageEncrypted@ parameter is true, and you do not specify a
-- value for the @KmsKeyId@ parameter, then Amazon Neptune will use your
-- default encryption key. Amazon KMS creates the default encryption key
-- for your Amazon account. Your Amazon account has a different default
-- encryption key for each Amazon Region.
createDBInstance_kmsKeyId :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_kmsKeyId = Lens.lens (\CreateDBInstance' {kmsKeyId} -> kmsKeyId) (\s@CreateDBInstance' {} a -> s {kmsKeyId = a} :: CreateDBInstance)

-- | License model information for this DB instance.
--
-- Valid values: @license-included@ | @bring-your-own-license@ |
-- @general-public-license@
createDBInstance_licenseModel :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_licenseModel = Lens.lens (\CreateDBInstance' {licenseModel} -> licenseModel) (\s@CreateDBInstance' {} a -> s {licenseModel = a} :: CreateDBInstance)

-- | Not supported by Neptune.
createDBInstance_masterUserPassword :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_masterUserPassword = Lens.lens (\CreateDBInstance' {masterUserPassword} -> masterUserPassword) (\s@CreateDBInstance' {} a -> s {masterUserPassword = a} :: CreateDBInstance)

-- | Not supported by Neptune.
createDBInstance_masterUsername :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_masterUsername = Lens.lens (\CreateDBInstance' {masterUsername} -> masterUsername) (\s@CreateDBInstance' {} a -> s {masterUsername = a} :: CreateDBInstance)

-- | The interval, in seconds, between points when Enhanced Monitoring
-- metrics are collected for the DB instance. To disable collecting
-- Enhanced Monitoring metrics, specify 0. The default is 0.
--
-- If @MonitoringRoleArn@ is specified, then you must also set
-- @MonitoringInterval@ to a value other than 0.
--
-- Valid Values: @0, 1, 5, 10, 15, 30, 60@
createDBInstance_monitoringInterval :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Int)
createDBInstance_monitoringInterval = Lens.lens (\CreateDBInstance' {monitoringInterval} -> monitoringInterval) (\s@CreateDBInstance' {} a -> s {monitoringInterval = a} :: CreateDBInstance)

-- | The ARN for the IAM role that permits Neptune to send enhanced
-- monitoring metrics to Amazon CloudWatch Logs. For example,
-- @arn:aws:iam:123456789012:role\/emaccess@.
--
-- If @MonitoringInterval@ is set to a value other than 0, then you must
-- supply a @MonitoringRoleArn@ value.
createDBInstance_monitoringRoleArn :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_monitoringRoleArn = Lens.lens (\CreateDBInstance' {monitoringRoleArn} -> monitoringRoleArn) (\s@CreateDBInstance' {} a -> s {monitoringRoleArn = a} :: CreateDBInstance)

-- | Specifies if the DB instance is a Multi-AZ deployment. You can\'t set
-- the AvailabilityZone parameter if the MultiAZ parameter is set to true.
createDBInstance_multiAZ :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Bool)
createDBInstance_multiAZ = Lens.lens (\CreateDBInstance' {multiAZ} -> multiAZ) (\s@CreateDBInstance' {} a -> s {multiAZ = a} :: CreateDBInstance)

-- | /(Not supported by Neptune)/
createDBInstance_optionGroupName :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_optionGroupName = Lens.lens (\CreateDBInstance' {optionGroupName} -> optionGroupName) (\s@CreateDBInstance' {} a -> s {optionGroupName = a} :: CreateDBInstance)

-- | /(Not supported by Neptune)/
createDBInstance_performanceInsightsKMSKeyId :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_performanceInsightsKMSKeyId = Lens.lens (\CreateDBInstance' {performanceInsightsKMSKeyId} -> performanceInsightsKMSKeyId) (\s@CreateDBInstance' {} a -> s {performanceInsightsKMSKeyId = a} :: CreateDBInstance)

-- | The port number on which the database accepts connections.
--
-- Not applicable. The port is managed by the DB cluster. For more
-- information, see CreateDBCluster.
--
-- Default: @8182@
--
-- Type: Integer
createDBInstance_port :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Int)
createDBInstance_port = Lens.lens (\CreateDBInstance' {port} -> port) (\s@CreateDBInstance' {} a -> s {port = a} :: CreateDBInstance)

-- | The daily time range during which automated backups are created.
--
-- Not applicable. The daily time range for creating automated backups is
-- managed by the DB cluster. For more information, see CreateDBCluster.
createDBInstance_preferredBackupWindow :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_preferredBackupWindow = Lens.lens (\CreateDBInstance' {preferredBackupWindow} -> preferredBackupWindow) (\s@CreateDBInstance' {} a -> s {preferredBackupWindow = a} :: CreateDBInstance)

-- | The time range each week during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Region, occurring on a random day of the
-- week.
--
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
--
-- Constraints: Minimum 30-minute window.
createDBInstance_preferredMaintenanceWindow :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_preferredMaintenanceWindow = Lens.lens (\CreateDBInstance' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@CreateDBInstance' {} a -> s {preferredMaintenanceWindow = a} :: CreateDBInstance)

-- | A value that specifies the order in which an Read Replica is promoted to
-- the primary instance after a failure of the existing primary instance.
--
-- Default: 1
--
-- Valid Values: 0 - 15
createDBInstance_promotionTier :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Int)
createDBInstance_promotionTier = Lens.lens (\CreateDBInstance' {promotionTier} -> promotionTier) (\s@CreateDBInstance' {} a -> s {promotionTier = a} :: CreateDBInstance)

-- | This flag should no longer be used.
createDBInstance_publiclyAccessible :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Bool)
createDBInstance_publiclyAccessible = Lens.lens (\CreateDBInstance' {publiclyAccessible} -> publiclyAccessible) (\s@CreateDBInstance' {} a -> s {publiclyAccessible = a} :: CreateDBInstance)

-- | Specifies whether the DB instance is encrypted.
--
-- Not applicable. The encryption for DB instances is managed by the DB
-- cluster. For more information, see CreateDBCluster.
--
-- Default: false
createDBInstance_storageEncrypted :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Bool)
createDBInstance_storageEncrypted = Lens.lens (\CreateDBInstance' {storageEncrypted} -> storageEncrypted) (\s@CreateDBInstance' {} a -> s {storageEncrypted = a} :: CreateDBInstance)

-- | Specifies the storage type to be associated with the DB instance.
--
-- Not applicable. Storage is managed by the DB Cluster.
createDBInstance_storageType :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_storageType = Lens.lens (\CreateDBInstance' {storageType} -> storageType) (\s@CreateDBInstance' {} a -> s {storageType = a} :: CreateDBInstance)

-- | The tags to assign to the new instance.
createDBInstance_tags :: Lens.Lens' CreateDBInstance (Prelude.Maybe [Tag])
createDBInstance_tags = Lens.lens (\CreateDBInstance' {tags} -> tags) (\s@CreateDBInstance' {} a -> s {tags = a} :: CreateDBInstance) Prelude.. Lens.mapping Lens.coerced

-- | The ARN from the key store with which to associate the instance for TDE
-- encryption.
createDBInstance_tdeCredentialArn :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_tdeCredentialArn = Lens.lens (\CreateDBInstance' {tdeCredentialArn} -> tdeCredentialArn) (\s@CreateDBInstance' {} a -> s {tdeCredentialArn = a} :: CreateDBInstance)

-- | The password for the given ARN from the key store in order to access the
-- device.
createDBInstance_tdeCredentialPassword :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_tdeCredentialPassword = Lens.lens (\CreateDBInstance' {tdeCredentialPassword} -> tdeCredentialPassword) (\s@CreateDBInstance' {} a -> s {tdeCredentialPassword = a} :: CreateDBInstance)

-- | The time zone of the DB instance.
createDBInstance_timezone :: Lens.Lens' CreateDBInstance (Prelude.Maybe Prelude.Text)
createDBInstance_timezone = Lens.lens (\CreateDBInstance' {timezone} -> timezone) (\s@CreateDBInstance' {} a -> s {timezone = a} :: CreateDBInstance)

-- | A list of EC2 VPC security groups to associate with this DB instance.
--
-- Not applicable. The associated list of EC2 VPC security groups is
-- managed by the DB cluster. For more information, see CreateDBCluster.
--
-- Default: The default EC2 VPC security group for the DB subnet group\'s
-- VPC.
createDBInstance_vpcSecurityGroupIds :: Lens.Lens' CreateDBInstance (Prelude.Maybe [Prelude.Text])
createDBInstance_vpcSecurityGroupIds = Lens.lens (\CreateDBInstance' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@CreateDBInstance' {} a -> s {vpcSecurityGroupIds = a} :: CreateDBInstance) Prelude.. Lens.mapping Lens.coerced

-- | The DB instance identifier. This parameter is stored as a lowercase
-- string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens.
--
-- -   First character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- Example: @mydbinstance@
createDBInstance_dbInstanceIdentifier :: Lens.Lens' CreateDBInstance Prelude.Text
createDBInstance_dbInstanceIdentifier = Lens.lens (\CreateDBInstance' {dbInstanceIdentifier} -> dbInstanceIdentifier) (\s@CreateDBInstance' {} a -> s {dbInstanceIdentifier = a} :: CreateDBInstance)

-- | The compute and memory capacity of the DB instance, for example,
-- @db.m4.large@. Not all DB instance classes are available in all Amazon
-- Regions.
createDBInstance_dbInstanceClass :: Lens.Lens' CreateDBInstance Prelude.Text
createDBInstance_dbInstanceClass = Lens.lens (\CreateDBInstance' {dbInstanceClass} -> dbInstanceClass) (\s@CreateDBInstance' {} a -> s {dbInstanceClass = a} :: CreateDBInstance)

-- | The name of the database engine to be used for this instance.
--
-- Valid Values: @neptune@
createDBInstance_engine :: Lens.Lens' CreateDBInstance Prelude.Text
createDBInstance_engine = Lens.lens (\CreateDBInstance' {engine} -> engine) (\s@CreateDBInstance' {} a -> s {engine = a} :: CreateDBInstance)

-- | The identifier of the DB cluster that the instance will belong to.
--
-- For information on creating a DB cluster, see CreateDBCluster.
--
-- Type: String
createDBInstance_dbClusterIdentifier :: Lens.Lens' CreateDBInstance Prelude.Text
createDBInstance_dbClusterIdentifier = Lens.lens (\CreateDBInstance' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@CreateDBInstance' {} a -> s {dbClusterIdentifier = a} :: CreateDBInstance)

instance Core.AWSRequest CreateDBInstance where
  type
    AWSResponse CreateDBInstance =
      CreateDBInstanceResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateDBInstanceResult"
      ( \s h x ->
          CreateDBInstanceResponse'
            Prelude.<$> (x Data..@? "DBInstance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDBInstance where
  hashWithSalt _salt CreateDBInstance' {..} =
    _salt
      `Prelude.hashWithSalt` allocatedStorage
      `Prelude.hashWithSalt` autoMinorVersionUpgrade
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` backupRetentionPeriod
      `Prelude.hashWithSalt` characterSetName
      `Prelude.hashWithSalt` copyTagsToSnapshot
      `Prelude.hashWithSalt` dbName
      `Prelude.hashWithSalt` dbParameterGroupName
      `Prelude.hashWithSalt` dbSecurityGroups
      `Prelude.hashWithSalt` dbSubnetGroupName
      `Prelude.hashWithSalt` deletionProtection
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` domainIAMRoleName
      `Prelude.hashWithSalt` enableCloudwatchLogsExports
      `Prelude.hashWithSalt` enableIAMDatabaseAuthentication
      `Prelude.hashWithSalt` enablePerformanceInsights
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` licenseModel
      `Prelude.hashWithSalt` masterUserPassword
      `Prelude.hashWithSalt` masterUsername
      `Prelude.hashWithSalt` monitoringInterval
      `Prelude.hashWithSalt` monitoringRoleArn
      `Prelude.hashWithSalt` multiAZ
      `Prelude.hashWithSalt` optionGroupName
      `Prelude.hashWithSalt` performanceInsightsKMSKeyId
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` preferredBackupWindow
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` promotionTier
      `Prelude.hashWithSalt` publiclyAccessible
      `Prelude.hashWithSalt` storageEncrypted
      `Prelude.hashWithSalt` storageType
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` tdeCredentialArn
      `Prelude.hashWithSalt` tdeCredentialPassword
      `Prelude.hashWithSalt` timezone
      `Prelude.hashWithSalt` vpcSecurityGroupIds
      `Prelude.hashWithSalt` dbInstanceIdentifier
      `Prelude.hashWithSalt` dbInstanceClass
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` dbClusterIdentifier

instance Prelude.NFData CreateDBInstance where
  rnf CreateDBInstance' {..} =
    Prelude.rnf allocatedStorage
      `Prelude.seq` Prelude.rnf autoMinorVersionUpgrade
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf backupRetentionPeriod
      `Prelude.seq` Prelude.rnf characterSetName
      `Prelude.seq` Prelude.rnf copyTagsToSnapshot
      `Prelude.seq` Prelude.rnf dbName
      `Prelude.seq` Prelude.rnf dbParameterGroupName
      `Prelude.seq` Prelude.rnf dbSecurityGroups
      `Prelude.seq` Prelude.rnf dbSubnetGroupName
      `Prelude.seq` Prelude.rnf deletionProtection
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf domainIAMRoleName
      `Prelude.seq` Prelude.rnf enableCloudwatchLogsExports
      `Prelude.seq` Prelude.rnf
        enableIAMDatabaseAuthentication
      `Prelude.seq` Prelude.rnf enablePerformanceInsights
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf iops
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf licenseModel
      `Prelude.seq` Prelude.rnf
        masterUserPassword
      `Prelude.seq` Prelude.rnf masterUsername
      `Prelude.seq` Prelude.rnf
        monitoringInterval
      `Prelude.seq` Prelude.rnf
        monitoringRoleArn
      `Prelude.seq` Prelude.rnf multiAZ
      `Prelude.seq` Prelude.rnf
        optionGroupName
      `Prelude.seq` Prelude.rnf
        performanceInsightsKMSKeyId
      `Prelude.seq` Prelude.rnf
        port
      `Prelude.seq` Prelude.rnf
        preferredBackupWindow
      `Prelude.seq` Prelude.rnf
        preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf
        promotionTier
      `Prelude.seq` Prelude.rnf
        publiclyAccessible
      `Prelude.seq` Prelude.rnf
        storageEncrypted
      `Prelude.seq` Prelude.rnf
        storageType
      `Prelude.seq` Prelude.rnf
        tags
      `Prelude.seq` Prelude.rnf
        tdeCredentialArn
      `Prelude.seq` Prelude.rnf
        tdeCredentialPassword
      `Prelude.seq` Prelude.rnf
        timezone
      `Prelude.seq` Prelude.rnf
        vpcSecurityGroupIds
      `Prelude.seq` Prelude.rnf
        dbInstanceIdentifier
      `Prelude.seq` Prelude.rnf
        dbInstanceClass
      `Prelude.seq` Prelude.rnf
        engine
      `Prelude.seq` Prelude.rnf
        dbClusterIdentifier

instance Data.ToHeaders CreateDBInstance where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateDBInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDBInstance where
  toQuery CreateDBInstance' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateDBInstance" :: Prelude.ByteString),
        "Version"
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "AllocatedStorage" Data.=: allocatedStorage,
        "AutoMinorVersionUpgrade"
          Data.=: autoMinorVersionUpgrade,
        "AvailabilityZone" Data.=: availabilityZone,
        "BackupRetentionPeriod"
          Data.=: backupRetentionPeriod,
        "CharacterSetName" Data.=: characterSetName,
        "CopyTagsToSnapshot" Data.=: copyTagsToSnapshot,
        "DBName" Data.=: dbName,
        "DBParameterGroupName" Data.=: dbParameterGroupName,
        "DBSecurityGroups"
          Data.=: Data.toQuery
            ( Data.toQueryList "DBSecurityGroupName"
                Prelude.<$> dbSecurityGroups
            ),
        "DBSubnetGroupName" Data.=: dbSubnetGroupName,
        "DeletionProtection" Data.=: deletionProtection,
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
        "EngineVersion" Data.=: engineVersion,
        "Iops" Data.=: iops,
        "KmsKeyId" Data.=: kmsKeyId,
        "LicenseModel" Data.=: licenseModel,
        "MasterUserPassword" Data.=: masterUserPassword,
        "MasterUsername" Data.=: masterUsername,
        "MonitoringInterval" Data.=: monitoringInterval,
        "MonitoringRoleArn" Data.=: monitoringRoleArn,
        "MultiAZ" Data.=: multiAZ,
        "OptionGroupName" Data.=: optionGroupName,
        "PerformanceInsightsKMSKeyId"
          Data.=: performanceInsightsKMSKeyId,
        "Port" Data.=: port,
        "PreferredBackupWindow"
          Data.=: preferredBackupWindow,
        "PreferredMaintenanceWindow"
          Data.=: preferredMaintenanceWindow,
        "PromotionTier" Data.=: promotionTier,
        "PubliclyAccessible" Data.=: publiclyAccessible,
        "StorageEncrypted" Data.=: storageEncrypted,
        "StorageType" Data.=: storageType,
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "Tag" Prelude.<$> tags),
        "TdeCredentialArn" Data.=: tdeCredentialArn,
        "TdeCredentialPassword"
          Data.=: tdeCredentialPassword,
        "Timezone" Data.=: timezone,
        "VpcSecurityGroupIds"
          Data.=: Data.toQuery
            ( Data.toQueryList "VpcSecurityGroupId"
                Prelude.<$> vpcSecurityGroupIds
            ),
        "DBInstanceIdentifier" Data.=: dbInstanceIdentifier,
        "DBInstanceClass" Data.=: dbInstanceClass,
        "Engine" Data.=: engine,
        "DBClusterIdentifier" Data.=: dbClusterIdentifier
      ]

-- | /See:/ 'newCreateDBInstanceResponse' smart constructor.
data CreateDBInstanceResponse = CreateDBInstanceResponse'
  { dbInstance :: Prelude.Maybe DBInstance,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDBInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbInstance', 'createDBInstanceResponse_dbInstance' - Undocumented member.
--
-- 'httpStatus', 'createDBInstanceResponse_httpStatus' - The response's http status code.
newCreateDBInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDBInstanceResponse
newCreateDBInstanceResponse pHttpStatus_ =
  CreateDBInstanceResponse'
    { dbInstance =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createDBInstanceResponse_dbInstance :: Lens.Lens' CreateDBInstanceResponse (Prelude.Maybe DBInstance)
createDBInstanceResponse_dbInstance = Lens.lens (\CreateDBInstanceResponse' {dbInstance} -> dbInstance) (\s@CreateDBInstanceResponse' {} a -> s {dbInstance = a} :: CreateDBInstanceResponse)

-- | The response's http status code.
createDBInstanceResponse_httpStatus :: Lens.Lens' CreateDBInstanceResponse Prelude.Int
createDBInstanceResponse_httpStatus = Lens.lens (\CreateDBInstanceResponse' {httpStatus} -> httpStatus) (\s@CreateDBInstanceResponse' {} a -> s {httpStatus = a} :: CreateDBInstanceResponse)

instance Prelude.NFData CreateDBInstanceResponse where
  rnf CreateDBInstanceResponse' {..} =
    Prelude.rnf dbInstance
      `Prelude.seq` Prelude.rnf httpStatus
