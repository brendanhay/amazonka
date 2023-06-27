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
-- Module      : Amazonka.Neptune.CreateDBCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon Neptune DB cluster.
--
-- You can use the @ReplicationSourceIdentifier@ parameter to create the DB
-- cluster as a Read Replica of another DB cluster or Amazon Neptune DB
-- instance.
--
-- Note that when you create a new cluster using @CreateDBCluster@
-- directly, deletion protection is disabled by default (when you create a
-- new production cluster in the console, deletion protection is enabled by
-- default). You can only delete a DB cluster if its @DeletionProtection@
-- field is set to @false@.
module Amazonka.Neptune.CreateDBCluster
  ( -- * Creating a Request
    CreateDBCluster (..),
    newCreateDBCluster,

    -- * Request Lenses
    createDBCluster_availabilityZones,
    createDBCluster_backupRetentionPeriod,
    createDBCluster_characterSetName,
    createDBCluster_copyTagsToSnapshot,
    createDBCluster_dbClusterParameterGroupName,
    createDBCluster_dbSubnetGroupName,
    createDBCluster_databaseName,
    createDBCluster_deletionProtection,
    createDBCluster_enableCloudwatchLogsExports,
    createDBCluster_enableIAMDatabaseAuthentication,
    createDBCluster_engineVersion,
    createDBCluster_globalClusterIdentifier,
    createDBCluster_kmsKeyId,
    createDBCluster_masterUserPassword,
    createDBCluster_masterUsername,
    createDBCluster_optionGroupName,
    createDBCluster_port,
    createDBCluster_preSignedUrl,
    createDBCluster_preferredBackupWindow,
    createDBCluster_preferredMaintenanceWindow,
    createDBCluster_replicationSourceIdentifier,
    createDBCluster_serverlessV2ScalingConfiguration,
    createDBCluster_storageEncrypted,
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
import Amazonka.Neptune.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDBCluster' smart constructor.
data CreateDBCluster = CreateDBCluster'
  { -- | A list of EC2 Availability Zones that instances in the DB cluster can be
    -- created in.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | The number of days for which automated backups are retained. You must
    -- specify a minimum value of 1.
    --
    -- Default: 1
    --
    -- Constraints:
    --
    -- -   Must be a value from 1 to 35
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | /(Not supported by Neptune)/
    characterSetName :: Prelude.Maybe Prelude.Text,
    -- | /If set to @true@, tags are copied to any snapshot of the DB cluster
    -- that is created./
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | The name of the DB cluster parameter group to associate with this DB
    -- cluster. If this argument is omitted, the default is used.
    --
    -- Constraints:
    --
    -- -   If supplied, must match the name of an existing
    --     DBClusterParameterGroup.
    dbClusterParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | A DB subnet group to associate with this DB cluster.
    --
    -- Constraints: Must match the name of an existing DBSubnetGroup. Must not
    -- be default.
    --
    -- Example: @mySubnetgroup@
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | The name for your database of up to 64 alpha-numeric characters. If you
    -- do not provide a name, Amazon Neptune will not create a database in the
    -- DB cluster you are creating.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the DB cluster has deletion protection
    -- enabled. The database can\'t be deleted when deletion protection is
    -- enabled. By default, deletion protection is enabled.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | The list of log types that need to be enabled for exporting to
    -- CloudWatch Logs.
    enableCloudwatchLogsExports :: Prelude.Maybe [Prelude.Text],
    -- | If set to @true@, enables Amazon Identity and Access Management (IAM)
    -- authentication for the entire DB cluster (this cannot be set at an
    -- instance level).
    --
    -- Default: @false@.
    enableIAMDatabaseAuthentication :: Prelude.Maybe Prelude.Bool,
    -- | The version number of the database engine to use for the new DB cluster.
    --
    -- Example: @1.0.2.1@
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Neptune global database to which this new DB cluster
    -- should be added.
    globalClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The Amazon KMS key identifier for an encrypted DB cluster.
    --
    -- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS
    -- encryption key. If you are creating a DB cluster with the same Amazon
    -- account that owns the KMS encryption key used to encrypt the new DB
    -- cluster, then you can use the KMS key alias instead of the ARN for the
    -- KMS encryption key.
    --
    -- If an encryption key is not specified in @KmsKeyId@:
    --
    -- -   If @ReplicationSourceIdentifier@ identifies an encrypted source,
    --     then Amazon Neptune will use the encryption key used to encrypt the
    --     source. Otherwise, Amazon Neptune will use your default encryption
    --     key.
    --
    -- -   If the @StorageEncrypted@ parameter is true and
    --     @ReplicationSourceIdentifier@ is not specified, then Amazon Neptune
    --     will use your default encryption key.
    --
    -- Amazon KMS creates the default encryption key for your Amazon account.
    -- Your Amazon account has a different default encryption key for each
    -- Amazon Region.
    --
    -- If you create a Read Replica of an encrypted DB cluster in another
    -- Amazon Region, you must set @KmsKeyId@ to a KMS key ID that is valid in
    -- the destination Amazon Region. This key is used to encrypt the Read
    -- Replica in that Amazon Region.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Not supported by Neptune.
    masterUserPassword :: Prelude.Maybe Prelude.Text,
    -- | Not supported by Neptune.
    masterUsername :: Prelude.Maybe Prelude.Text,
    -- | /(Not supported by Neptune)/
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | The port number on which the instances in the DB cluster accept
    -- connections.
    --
    -- Default: @8182@
    port :: Prelude.Maybe Prelude.Int,
    -- | This parameter is not currently supported.
    preSignedUrl :: Prelude.Maybe Prelude.Text,
    -- | The daily time range during which automated backups are created if
    -- automated backups are enabled using the @BackupRetentionPeriod@
    -- parameter.
    --
    -- The default is a 30-minute window selected at random from an 8-hour
    -- block of time for each Amazon Region. To see the time blocks available,
    -- see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/AdjustingTheMaintenanceWindow.html Adjusting the Preferred Maintenance Window>
    -- in the /Amazon Neptune User Guide./
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
    preferredBackupWindow :: Prelude.Maybe Prelude.Text,
    -- | The weekly time range during which system maintenance can occur, in
    -- Universal Coordinated Time (UTC).
    --
    -- Format: @ddd:hh24:mi-ddd:hh24:mi@
    --
    -- The default is a 30-minute window selected at random from an 8-hour
    -- block of time for each Amazon Region, occurring on a random day of the
    -- week. To see the time blocks available, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/AdjustingTheMaintenanceWindow.html Adjusting the Preferred Maintenance Window>
    -- in the /Amazon Neptune User Guide./
    --
    -- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
    --
    -- Constraints: Minimum 30-minute window.
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the source DB instance or DB cluster
    -- if this DB cluster is created as a Read Replica.
    replicationSourceIdentifier :: Prelude.Maybe Prelude.Text,
    serverlessV2ScalingConfiguration :: Prelude.Maybe ServerlessV2ScalingConfiguration,
    -- | Specifies whether the DB cluster is encrypted.
    storageEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | The tags to assign to the new DB cluster.
    tags :: Prelude.Maybe [Tag],
    -- | A list of EC2 VPC security groups to associate with this DB cluster.
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
    -- -   Cannot end with a hyphen or contain two consecutive hyphens.
    --
    -- Example: @my-cluster1@
    dbClusterIdentifier :: Prelude.Text,
    -- | The name of the database engine to be used for this DB cluster.
    --
    -- Valid Values: @neptune@
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
-- 'availabilityZones', 'createDBCluster_availabilityZones' - A list of EC2 Availability Zones that instances in the DB cluster can be
-- created in.
--
-- 'backupRetentionPeriod', 'createDBCluster_backupRetentionPeriod' - The number of days for which automated backups are retained. You must
-- specify a minimum value of 1.
--
-- Default: 1
--
-- Constraints:
--
-- -   Must be a value from 1 to 35
--
-- 'characterSetName', 'createDBCluster_characterSetName' - /(Not supported by Neptune)/
--
-- 'copyTagsToSnapshot', 'createDBCluster_copyTagsToSnapshot' - /If set to @true@, tags are copied to any snapshot of the DB cluster
-- that is created./
--
-- 'dbClusterParameterGroupName', 'createDBCluster_dbClusterParameterGroupName' - The name of the DB cluster parameter group to associate with this DB
-- cluster. If this argument is omitted, the default is used.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing
--     DBClusterParameterGroup.
--
-- 'dbSubnetGroupName', 'createDBCluster_dbSubnetGroupName' - A DB subnet group to associate with this DB cluster.
--
-- Constraints: Must match the name of an existing DBSubnetGroup. Must not
-- be default.
--
-- Example: @mySubnetgroup@
--
-- 'databaseName', 'createDBCluster_databaseName' - The name for your database of up to 64 alpha-numeric characters. If you
-- do not provide a name, Amazon Neptune will not create a database in the
-- DB cluster you are creating.
--
-- 'deletionProtection', 'createDBCluster_deletionProtection' - A value that indicates whether the DB cluster has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is enabled.
--
-- 'enableCloudwatchLogsExports', 'createDBCluster_enableCloudwatchLogsExports' - The list of log types that need to be enabled for exporting to
-- CloudWatch Logs.
--
-- 'enableIAMDatabaseAuthentication', 'createDBCluster_enableIAMDatabaseAuthentication' - If set to @true@, enables Amazon Identity and Access Management (IAM)
-- authentication for the entire DB cluster (this cannot be set at an
-- instance level).
--
-- Default: @false@.
--
-- 'engineVersion', 'createDBCluster_engineVersion' - The version number of the database engine to use for the new DB cluster.
--
-- Example: @1.0.2.1@
--
-- 'globalClusterIdentifier', 'createDBCluster_globalClusterIdentifier' - The ID of the Neptune global database to which this new DB cluster
-- should be added.
--
-- 'kmsKeyId', 'createDBCluster_kmsKeyId' - The Amazon KMS key identifier for an encrypted DB cluster.
--
-- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS
-- encryption key. If you are creating a DB cluster with the same Amazon
-- account that owns the KMS encryption key used to encrypt the new DB
-- cluster, then you can use the KMS key alias instead of the ARN for the
-- KMS encryption key.
--
-- If an encryption key is not specified in @KmsKeyId@:
--
-- -   If @ReplicationSourceIdentifier@ identifies an encrypted source,
--     then Amazon Neptune will use the encryption key used to encrypt the
--     source. Otherwise, Amazon Neptune will use your default encryption
--     key.
--
-- -   If the @StorageEncrypted@ parameter is true and
--     @ReplicationSourceIdentifier@ is not specified, then Amazon Neptune
--     will use your default encryption key.
--
-- Amazon KMS creates the default encryption key for your Amazon account.
-- Your Amazon account has a different default encryption key for each
-- Amazon Region.
--
-- If you create a Read Replica of an encrypted DB cluster in another
-- Amazon Region, you must set @KmsKeyId@ to a KMS key ID that is valid in
-- the destination Amazon Region. This key is used to encrypt the Read
-- Replica in that Amazon Region.
--
-- 'masterUserPassword', 'createDBCluster_masterUserPassword' - Not supported by Neptune.
--
-- 'masterUsername', 'createDBCluster_masterUsername' - Not supported by Neptune.
--
-- 'optionGroupName', 'createDBCluster_optionGroupName' - /(Not supported by Neptune)/
--
-- 'port', 'createDBCluster_port' - The port number on which the instances in the DB cluster accept
-- connections.
--
-- Default: @8182@
--
-- 'preSignedUrl', 'createDBCluster_preSignedUrl' - This parameter is not currently supported.
--
-- 'preferredBackupWindow', 'createDBCluster_preferredBackupWindow' - The daily time range during which automated backups are created if
-- automated backups are enabled using the @BackupRetentionPeriod@
-- parameter.
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Region. To see the time blocks available,
-- see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/AdjustingTheMaintenanceWindow.html Adjusting the Preferred Maintenance Window>
-- in the /Amazon Neptune User Guide./
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
-- 'preferredMaintenanceWindow', 'createDBCluster_preferredMaintenanceWindow' - The weekly time range during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Region, occurring on a random day of the
-- week. To see the time blocks available, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/AdjustingTheMaintenanceWindow.html Adjusting the Preferred Maintenance Window>
-- in the /Amazon Neptune User Guide./
--
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
--
-- Constraints: Minimum 30-minute window.
--
-- 'replicationSourceIdentifier', 'createDBCluster_replicationSourceIdentifier' - The Amazon Resource Name (ARN) of the source DB instance or DB cluster
-- if this DB cluster is created as a Read Replica.
--
-- 'serverlessV2ScalingConfiguration', 'createDBCluster_serverlessV2ScalingConfiguration' - Undocumented member.
--
-- 'storageEncrypted', 'createDBCluster_storageEncrypted' - Specifies whether the DB cluster is encrypted.
--
-- 'tags', 'createDBCluster_tags' - The tags to assign to the new DB cluster.
--
-- 'vpcSecurityGroupIds', 'createDBCluster_vpcSecurityGroupIds' - A list of EC2 VPC security groups to associate with this DB cluster.
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
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- Example: @my-cluster1@
--
-- 'engine', 'createDBCluster_engine' - The name of the database engine to be used for this DB cluster.
--
-- Valid Values: @neptune@
newCreateDBCluster ::
  -- | 'dbClusterIdentifier'
  Prelude.Text ->
  -- | 'engine'
  Prelude.Text ->
  CreateDBCluster
newCreateDBCluster pDBClusterIdentifier_ pEngine_ =
  CreateDBCluster'
    { availabilityZones =
        Prelude.Nothing,
      backupRetentionPeriod = Prelude.Nothing,
      characterSetName = Prelude.Nothing,
      copyTagsToSnapshot = Prelude.Nothing,
      dbClusterParameterGroupName = Prelude.Nothing,
      dbSubnetGroupName = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      deletionProtection = Prelude.Nothing,
      enableCloudwatchLogsExports = Prelude.Nothing,
      enableIAMDatabaseAuthentication = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      globalClusterIdentifier = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      masterUserPassword = Prelude.Nothing,
      masterUsername = Prelude.Nothing,
      optionGroupName = Prelude.Nothing,
      port = Prelude.Nothing,
      preSignedUrl = Prelude.Nothing,
      preferredBackupWindow = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      replicationSourceIdentifier = Prelude.Nothing,
      serverlessV2ScalingConfiguration = Prelude.Nothing,
      storageEncrypted = Prelude.Nothing,
      tags = Prelude.Nothing,
      vpcSecurityGroupIds = Prelude.Nothing,
      dbClusterIdentifier = pDBClusterIdentifier_,
      engine = pEngine_
    }

-- | A list of EC2 Availability Zones that instances in the DB cluster can be
-- created in.
createDBCluster_availabilityZones :: Lens.Lens' CreateDBCluster (Prelude.Maybe [Prelude.Text])
createDBCluster_availabilityZones = Lens.lens (\CreateDBCluster' {availabilityZones} -> availabilityZones) (\s@CreateDBCluster' {} a -> s {availabilityZones = a} :: CreateDBCluster) Prelude.. Lens.mapping Lens.coerced

-- | The number of days for which automated backups are retained. You must
-- specify a minimum value of 1.
--
-- Default: 1
--
-- Constraints:
--
-- -   Must be a value from 1 to 35
createDBCluster_backupRetentionPeriod :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Int)
createDBCluster_backupRetentionPeriod = Lens.lens (\CreateDBCluster' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@CreateDBCluster' {} a -> s {backupRetentionPeriod = a} :: CreateDBCluster)

-- | /(Not supported by Neptune)/
createDBCluster_characterSetName :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_characterSetName = Lens.lens (\CreateDBCluster' {characterSetName} -> characterSetName) (\s@CreateDBCluster' {} a -> s {characterSetName = a} :: CreateDBCluster)

-- | /If set to @true@, tags are copied to any snapshot of the DB cluster
-- that is created./
createDBCluster_copyTagsToSnapshot :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Bool)
createDBCluster_copyTagsToSnapshot = Lens.lens (\CreateDBCluster' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@CreateDBCluster' {} a -> s {copyTagsToSnapshot = a} :: CreateDBCluster)

-- | The name of the DB cluster parameter group to associate with this DB
-- cluster. If this argument is omitted, the default is used.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing
--     DBClusterParameterGroup.
createDBCluster_dbClusterParameterGroupName :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_dbClusterParameterGroupName = Lens.lens (\CreateDBCluster' {dbClusterParameterGroupName} -> dbClusterParameterGroupName) (\s@CreateDBCluster' {} a -> s {dbClusterParameterGroupName = a} :: CreateDBCluster)

-- | A DB subnet group to associate with this DB cluster.
--
-- Constraints: Must match the name of an existing DBSubnetGroup. Must not
-- be default.
--
-- Example: @mySubnetgroup@
createDBCluster_dbSubnetGroupName :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_dbSubnetGroupName = Lens.lens (\CreateDBCluster' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@CreateDBCluster' {} a -> s {dbSubnetGroupName = a} :: CreateDBCluster)

-- | The name for your database of up to 64 alpha-numeric characters. If you
-- do not provide a name, Amazon Neptune will not create a database in the
-- DB cluster you are creating.
createDBCluster_databaseName :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_databaseName = Lens.lens (\CreateDBCluster' {databaseName} -> databaseName) (\s@CreateDBCluster' {} a -> s {databaseName = a} :: CreateDBCluster)

-- | A value that indicates whether the DB cluster has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is enabled.
createDBCluster_deletionProtection :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Bool)
createDBCluster_deletionProtection = Lens.lens (\CreateDBCluster' {deletionProtection} -> deletionProtection) (\s@CreateDBCluster' {} a -> s {deletionProtection = a} :: CreateDBCluster)

-- | The list of log types that need to be enabled for exporting to
-- CloudWatch Logs.
createDBCluster_enableCloudwatchLogsExports :: Lens.Lens' CreateDBCluster (Prelude.Maybe [Prelude.Text])
createDBCluster_enableCloudwatchLogsExports = Lens.lens (\CreateDBCluster' {enableCloudwatchLogsExports} -> enableCloudwatchLogsExports) (\s@CreateDBCluster' {} a -> s {enableCloudwatchLogsExports = a} :: CreateDBCluster) Prelude.. Lens.mapping Lens.coerced

-- | If set to @true@, enables Amazon Identity and Access Management (IAM)
-- authentication for the entire DB cluster (this cannot be set at an
-- instance level).
--
-- Default: @false@.
createDBCluster_enableIAMDatabaseAuthentication :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Bool)
createDBCluster_enableIAMDatabaseAuthentication = Lens.lens (\CreateDBCluster' {enableIAMDatabaseAuthentication} -> enableIAMDatabaseAuthentication) (\s@CreateDBCluster' {} a -> s {enableIAMDatabaseAuthentication = a} :: CreateDBCluster)

-- | The version number of the database engine to use for the new DB cluster.
--
-- Example: @1.0.2.1@
createDBCluster_engineVersion :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_engineVersion = Lens.lens (\CreateDBCluster' {engineVersion} -> engineVersion) (\s@CreateDBCluster' {} a -> s {engineVersion = a} :: CreateDBCluster)

-- | The ID of the Neptune global database to which this new DB cluster
-- should be added.
createDBCluster_globalClusterIdentifier :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_globalClusterIdentifier = Lens.lens (\CreateDBCluster' {globalClusterIdentifier} -> globalClusterIdentifier) (\s@CreateDBCluster' {} a -> s {globalClusterIdentifier = a} :: CreateDBCluster)

-- | The Amazon KMS key identifier for an encrypted DB cluster.
--
-- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS
-- encryption key. If you are creating a DB cluster with the same Amazon
-- account that owns the KMS encryption key used to encrypt the new DB
-- cluster, then you can use the KMS key alias instead of the ARN for the
-- KMS encryption key.
--
-- If an encryption key is not specified in @KmsKeyId@:
--
-- -   If @ReplicationSourceIdentifier@ identifies an encrypted source,
--     then Amazon Neptune will use the encryption key used to encrypt the
--     source. Otherwise, Amazon Neptune will use your default encryption
--     key.
--
-- -   If the @StorageEncrypted@ parameter is true and
--     @ReplicationSourceIdentifier@ is not specified, then Amazon Neptune
--     will use your default encryption key.
--
-- Amazon KMS creates the default encryption key for your Amazon account.
-- Your Amazon account has a different default encryption key for each
-- Amazon Region.
--
-- If you create a Read Replica of an encrypted DB cluster in another
-- Amazon Region, you must set @KmsKeyId@ to a KMS key ID that is valid in
-- the destination Amazon Region. This key is used to encrypt the Read
-- Replica in that Amazon Region.
createDBCluster_kmsKeyId :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_kmsKeyId = Lens.lens (\CreateDBCluster' {kmsKeyId} -> kmsKeyId) (\s@CreateDBCluster' {} a -> s {kmsKeyId = a} :: CreateDBCluster)

-- | Not supported by Neptune.
createDBCluster_masterUserPassword :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_masterUserPassword = Lens.lens (\CreateDBCluster' {masterUserPassword} -> masterUserPassword) (\s@CreateDBCluster' {} a -> s {masterUserPassword = a} :: CreateDBCluster)

-- | Not supported by Neptune.
createDBCluster_masterUsername :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_masterUsername = Lens.lens (\CreateDBCluster' {masterUsername} -> masterUsername) (\s@CreateDBCluster' {} a -> s {masterUsername = a} :: CreateDBCluster)

-- | /(Not supported by Neptune)/
createDBCluster_optionGroupName :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_optionGroupName = Lens.lens (\CreateDBCluster' {optionGroupName} -> optionGroupName) (\s@CreateDBCluster' {} a -> s {optionGroupName = a} :: CreateDBCluster)

-- | The port number on which the instances in the DB cluster accept
-- connections.
--
-- Default: @8182@
createDBCluster_port :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Int)
createDBCluster_port = Lens.lens (\CreateDBCluster' {port} -> port) (\s@CreateDBCluster' {} a -> s {port = a} :: CreateDBCluster)

-- | This parameter is not currently supported.
createDBCluster_preSignedUrl :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_preSignedUrl = Lens.lens (\CreateDBCluster' {preSignedUrl} -> preSignedUrl) (\s@CreateDBCluster' {} a -> s {preSignedUrl = a} :: CreateDBCluster)

-- | The daily time range during which automated backups are created if
-- automated backups are enabled using the @BackupRetentionPeriod@
-- parameter.
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Region. To see the time blocks available,
-- see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/AdjustingTheMaintenanceWindow.html Adjusting the Preferred Maintenance Window>
-- in the /Amazon Neptune User Guide./
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
createDBCluster_preferredBackupWindow :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_preferredBackupWindow = Lens.lens (\CreateDBCluster' {preferredBackupWindow} -> preferredBackupWindow) (\s@CreateDBCluster' {} a -> s {preferredBackupWindow = a} :: CreateDBCluster)

-- | The weekly time range during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Region, occurring on a random day of the
-- week. To see the time blocks available, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/AdjustingTheMaintenanceWindow.html Adjusting the Preferred Maintenance Window>
-- in the /Amazon Neptune User Guide./
--
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
--
-- Constraints: Minimum 30-minute window.
createDBCluster_preferredMaintenanceWindow :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_preferredMaintenanceWindow = Lens.lens (\CreateDBCluster' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@CreateDBCluster' {} a -> s {preferredMaintenanceWindow = a} :: CreateDBCluster)

-- | The Amazon Resource Name (ARN) of the source DB instance or DB cluster
-- if this DB cluster is created as a Read Replica.
createDBCluster_replicationSourceIdentifier :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_replicationSourceIdentifier = Lens.lens (\CreateDBCluster' {replicationSourceIdentifier} -> replicationSourceIdentifier) (\s@CreateDBCluster' {} a -> s {replicationSourceIdentifier = a} :: CreateDBCluster)

-- | Undocumented member.
createDBCluster_serverlessV2ScalingConfiguration :: Lens.Lens' CreateDBCluster (Prelude.Maybe ServerlessV2ScalingConfiguration)
createDBCluster_serverlessV2ScalingConfiguration = Lens.lens (\CreateDBCluster' {serverlessV2ScalingConfiguration} -> serverlessV2ScalingConfiguration) (\s@CreateDBCluster' {} a -> s {serverlessV2ScalingConfiguration = a} :: CreateDBCluster)

-- | Specifies whether the DB cluster is encrypted.
createDBCluster_storageEncrypted :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Bool)
createDBCluster_storageEncrypted = Lens.lens (\CreateDBCluster' {storageEncrypted} -> storageEncrypted) (\s@CreateDBCluster' {} a -> s {storageEncrypted = a} :: CreateDBCluster)

-- | The tags to assign to the new DB cluster.
createDBCluster_tags :: Lens.Lens' CreateDBCluster (Prelude.Maybe [Tag])
createDBCluster_tags = Lens.lens (\CreateDBCluster' {tags} -> tags) (\s@CreateDBCluster' {} a -> s {tags = a} :: CreateDBCluster) Prelude.. Lens.mapping Lens.coerced

-- | A list of EC2 VPC security groups to associate with this DB cluster.
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
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- Example: @my-cluster1@
createDBCluster_dbClusterIdentifier :: Lens.Lens' CreateDBCluster Prelude.Text
createDBCluster_dbClusterIdentifier = Lens.lens (\CreateDBCluster' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@CreateDBCluster' {} a -> s {dbClusterIdentifier = a} :: CreateDBCluster)

-- | The name of the database engine to be used for this DB cluster.
--
-- Valid Values: @neptune@
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
    _salt
      `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` backupRetentionPeriod
      `Prelude.hashWithSalt` characterSetName
      `Prelude.hashWithSalt` copyTagsToSnapshot
      `Prelude.hashWithSalt` dbClusterParameterGroupName
      `Prelude.hashWithSalt` dbSubnetGroupName
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` deletionProtection
      `Prelude.hashWithSalt` enableCloudwatchLogsExports
      `Prelude.hashWithSalt` enableIAMDatabaseAuthentication
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` globalClusterIdentifier
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` masterUserPassword
      `Prelude.hashWithSalt` masterUsername
      `Prelude.hashWithSalt` optionGroupName
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` preSignedUrl
      `Prelude.hashWithSalt` preferredBackupWindow
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` replicationSourceIdentifier
      `Prelude.hashWithSalt` serverlessV2ScalingConfiguration
      `Prelude.hashWithSalt` storageEncrypted
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcSecurityGroupIds
      `Prelude.hashWithSalt` dbClusterIdentifier
      `Prelude.hashWithSalt` engine

instance Prelude.NFData CreateDBCluster where
  rnf CreateDBCluster' {..} =
    Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf backupRetentionPeriod
      `Prelude.seq` Prelude.rnf characterSetName
      `Prelude.seq` Prelude.rnf copyTagsToSnapshot
      `Prelude.seq` Prelude.rnf dbClusterParameterGroupName
      `Prelude.seq` Prelude.rnf dbSubnetGroupName
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf deletionProtection
      `Prelude.seq` Prelude.rnf enableCloudwatchLogsExports
      `Prelude.seq` Prelude.rnf enableIAMDatabaseAuthentication
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf globalClusterIdentifier
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf masterUserPassword
      `Prelude.seq` Prelude.rnf masterUsername
      `Prelude.seq` Prelude.rnf optionGroupName
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf preSignedUrl
      `Prelude.seq` Prelude.rnf
        preferredBackupWindow
      `Prelude.seq` Prelude.rnf
        preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf
        replicationSourceIdentifier
      `Prelude.seq` Prelude.rnf
        serverlessV2ScalingConfiguration
      `Prelude.seq` Prelude.rnf
        storageEncrypted
      `Prelude.seq` Prelude.rnf tags
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
        "AvailabilityZones"
          Data.=: Data.toQuery
            ( Data.toQueryList "AvailabilityZone"
                Prelude.<$> availabilityZones
            ),
        "BackupRetentionPeriod"
          Data.=: backupRetentionPeriod,
        "CharacterSetName" Data.=: characterSetName,
        "CopyTagsToSnapshot" Data.=: copyTagsToSnapshot,
        "DBClusterParameterGroupName"
          Data.=: dbClusterParameterGroupName,
        "DBSubnetGroupName" Data.=: dbSubnetGroupName,
        "DatabaseName" Data.=: databaseName,
        "DeletionProtection" Data.=: deletionProtection,
        "EnableCloudwatchLogsExports"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> enableCloudwatchLogsExports
            ),
        "EnableIAMDatabaseAuthentication"
          Data.=: enableIAMDatabaseAuthentication,
        "EngineVersion" Data.=: engineVersion,
        "GlobalClusterIdentifier"
          Data.=: globalClusterIdentifier,
        "KmsKeyId" Data.=: kmsKeyId,
        "MasterUserPassword" Data.=: masterUserPassword,
        "MasterUsername" Data.=: masterUsername,
        "OptionGroupName" Data.=: optionGroupName,
        "Port" Data.=: port,
        "PreSignedUrl" Data.=: preSignedUrl,
        "PreferredBackupWindow"
          Data.=: preferredBackupWindow,
        "PreferredMaintenanceWindow"
          Data.=: preferredMaintenanceWindow,
        "ReplicationSourceIdentifier"
          Data.=: replicationSourceIdentifier,
        "ServerlessV2ScalingConfiguration"
          Data.=: serverlessV2ScalingConfiguration,
        "StorageEncrypted" Data.=: storageEncrypted,
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
