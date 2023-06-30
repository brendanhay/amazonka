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
-- Module      : Amazonka.DocumentDB.CreateDBCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon DocumentDB cluster.
module Amazonka.DocumentDB.CreateDBCluster
  ( -- * Creating a Request
    CreateDBCluster (..),
    newCreateDBCluster,

    -- * Request Lenses
    createDBCluster_availabilityZones,
    createDBCluster_backupRetentionPeriod,
    createDBCluster_dbClusterParameterGroupName,
    createDBCluster_dbSubnetGroupName,
    createDBCluster_deletionProtection,
    createDBCluster_enableCloudwatchLogsExports,
    createDBCluster_engineVersion,
    createDBCluster_globalClusterIdentifier,
    createDBCluster_kmsKeyId,
    createDBCluster_masterUserPassword,
    createDBCluster_masterUsername,
    createDBCluster_port,
    createDBCluster_preSignedUrl,
    createDBCluster_preferredBackupWindow,
    createDBCluster_preferredMaintenanceWindow,
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
import Amazonka.DocumentDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input to CreateDBCluster.
--
-- /See:/ 'newCreateDBCluster' smart constructor.
data CreateDBCluster = CreateDBCluster'
  { -- | A list of Amazon EC2 Availability Zones that instances in the cluster
    -- can be created in.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | The number of days for which automated backups are retained. You must
    -- specify a minimum value of 1.
    --
    -- Default: 1
    --
    -- Constraints:
    --
    -- -   Must be a value from 1 to 35.
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The name of the cluster parameter group to associate with this cluster.
    dbClusterParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | A subnet group to associate with this cluster.
    --
    -- Constraints: Must match the name of an existing @DBSubnetGroup@. Must
    -- not be default.
    --
    -- Example: @mySubnetgroup@
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether this cluster can be deleted. If @DeletionProtection@
    -- is enabled, the cluster cannot be deleted unless it is modified and
    -- @DeletionProtection@ is disabled. @DeletionProtection@ protects clusters
    -- from being accidentally deleted.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | A list of log types that need to be enabled for exporting to Amazon
    -- CloudWatch Logs. You can enable audit logs or profiler logs. For more
    -- information, see
    -- <https://docs.aws.amazon.com/documentdb/latest/developerguide/event-auditing.html Auditing Amazon DocumentDB Events>
    -- and
    -- <https://docs.aws.amazon.com/documentdb/latest/developerguide/profiling.html Profiling Amazon DocumentDB Operations>.
    enableCloudwatchLogsExports :: Prelude.Maybe [Prelude.Text],
    -- | The version number of the database engine to use. The @--engine-version@
    -- will default to the latest major engine version. For production
    -- workloads, we recommend explicitly declaring this parameter with the
    -- intended major engine version.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The cluster identifier of the new global cluster.
    globalClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The KMS key identifier for an encrypted cluster.
    --
    -- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS
    -- encryption key. If you are creating a cluster using the same Amazon Web
    -- Services account that owns the KMS encryption key that is used to
    -- encrypt the new cluster, you can use the KMS key alias instead of the
    -- ARN for the KMS encryption key.
    --
    -- If an encryption key is not specified in @KmsKeyId@:
    --
    -- -   If the @StorageEncrypted@ parameter is @true@, Amazon DocumentDB
    --     uses your default encryption key.
    --
    -- KMS creates the default encryption key for your Amazon Web Services
    -- account. Your Amazon Web Services account has a different default
    -- encryption key for each Amazon Web Services Regions.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The password for the master database user. This password can contain any
    -- printable ASCII character except forward slash (\/), double quote (\"),
    -- or the \"at\" symbol (\@).
    --
    -- Constraints: Must contain from 8 to 100 characters.
    masterUserPassword :: Prelude.Maybe Prelude.Text,
    -- | The name of the master user for the cluster.
    --
    -- Constraints:
    --
    -- -   Must be from 1 to 63 letters or numbers.
    --
    -- -   The first character must be a letter.
    --
    -- -   Cannot be a reserved word for the chosen database engine.
    masterUsername :: Prelude.Maybe Prelude.Text,
    -- | The port number on which the instances in the cluster accept
    -- connections.
    port :: Prelude.Maybe Prelude.Int,
    -- | Not currently supported.
    preSignedUrl :: Prelude.Maybe Prelude.Text,
    -- | The daily time range during which automated backups are created if
    -- automated backups are enabled using the @BackupRetentionPeriod@
    -- parameter.
    --
    -- The default is a 30-minute window selected at random from an 8-hour
    -- block of time for each Amazon Web Services Region.
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
    -- block of time for each Amazon Web Services Region, occurring on a random
    -- day of the week.
    --
    -- Valid days: Mon, Tue, Wed, Thu, Fri, Sat, Sun
    --
    -- Constraints: Minimum 30-minute window.
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the cluster is encrypted.
    storageEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | The tags to be assigned to the cluster.
    tags :: Prelude.Maybe [Tag],
    -- | A list of EC2 VPC security groups to associate with this cluster.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The cluster identifier. This parameter is stored as a lowercase string.
    --
    -- Constraints:
    --
    -- -   Must contain from 1 to 63 letters, numbers, or hyphens.
    --
    -- -   The first character must be a letter.
    --
    -- -   Cannot end with a hyphen or contain two consecutive hyphens.
    --
    -- Example: @my-cluster@
    dbClusterIdentifier :: Prelude.Text,
    -- | The name of the database engine to be used for this cluster.
    --
    -- Valid values: @docdb@
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
-- 'availabilityZones', 'createDBCluster_availabilityZones' - A list of Amazon EC2 Availability Zones that instances in the cluster
-- can be created in.
--
-- 'backupRetentionPeriod', 'createDBCluster_backupRetentionPeriod' - The number of days for which automated backups are retained. You must
-- specify a minimum value of 1.
--
-- Default: 1
--
-- Constraints:
--
-- -   Must be a value from 1 to 35.
--
-- 'dbClusterParameterGroupName', 'createDBCluster_dbClusterParameterGroupName' - The name of the cluster parameter group to associate with this cluster.
--
-- 'dbSubnetGroupName', 'createDBCluster_dbSubnetGroupName' - A subnet group to associate with this cluster.
--
-- Constraints: Must match the name of an existing @DBSubnetGroup@. Must
-- not be default.
--
-- Example: @mySubnetgroup@
--
-- 'deletionProtection', 'createDBCluster_deletionProtection' - Specifies whether this cluster can be deleted. If @DeletionProtection@
-- is enabled, the cluster cannot be deleted unless it is modified and
-- @DeletionProtection@ is disabled. @DeletionProtection@ protects clusters
-- from being accidentally deleted.
--
-- 'enableCloudwatchLogsExports', 'createDBCluster_enableCloudwatchLogsExports' - A list of log types that need to be enabled for exporting to Amazon
-- CloudWatch Logs. You can enable audit logs or profiler logs. For more
-- information, see
-- <https://docs.aws.amazon.com/documentdb/latest/developerguide/event-auditing.html Auditing Amazon DocumentDB Events>
-- and
-- <https://docs.aws.amazon.com/documentdb/latest/developerguide/profiling.html Profiling Amazon DocumentDB Operations>.
--
-- 'engineVersion', 'createDBCluster_engineVersion' - The version number of the database engine to use. The @--engine-version@
-- will default to the latest major engine version. For production
-- workloads, we recommend explicitly declaring this parameter with the
-- intended major engine version.
--
-- 'globalClusterIdentifier', 'createDBCluster_globalClusterIdentifier' - The cluster identifier of the new global cluster.
--
-- 'kmsKeyId', 'createDBCluster_kmsKeyId' - The KMS key identifier for an encrypted cluster.
--
-- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS
-- encryption key. If you are creating a cluster using the same Amazon Web
-- Services account that owns the KMS encryption key that is used to
-- encrypt the new cluster, you can use the KMS key alias instead of the
-- ARN for the KMS encryption key.
--
-- If an encryption key is not specified in @KmsKeyId@:
--
-- -   If the @StorageEncrypted@ parameter is @true@, Amazon DocumentDB
--     uses your default encryption key.
--
-- KMS creates the default encryption key for your Amazon Web Services
-- account. Your Amazon Web Services account has a different default
-- encryption key for each Amazon Web Services Regions.
--
-- 'masterUserPassword', 'createDBCluster_masterUserPassword' - The password for the master database user. This password can contain any
-- printable ASCII character except forward slash (\/), double quote (\"),
-- or the \"at\" symbol (\@).
--
-- Constraints: Must contain from 8 to 100 characters.
--
-- 'masterUsername', 'createDBCluster_masterUsername' - The name of the master user for the cluster.
--
-- Constraints:
--
-- -   Must be from 1 to 63 letters or numbers.
--
-- -   The first character must be a letter.
--
-- -   Cannot be a reserved word for the chosen database engine.
--
-- 'port', 'createDBCluster_port' - The port number on which the instances in the cluster accept
-- connections.
--
-- 'preSignedUrl', 'createDBCluster_preSignedUrl' - Not currently supported.
--
-- 'preferredBackupWindow', 'createDBCluster_preferredBackupWindow' - The daily time range during which automated backups are created if
-- automated backups are enabled using the @BackupRetentionPeriod@
-- parameter.
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Web Services Region.
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
-- block of time for each Amazon Web Services Region, occurring on a random
-- day of the week.
--
-- Valid days: Mon, Tue, Wed, Thu, Fri, Sat, Sun
--
-- Constraints: Minimum 30-minute window.
--
-- 'storageEncrypted', 'createDBCluster_storageEncrypted' - Specifies whether the cluster is encrypted.
--
-- 'tags', 'createDBCluster_tags' - The tags to be assigned to the cluster.
--
-- 'vpcSecurityGroupIds', 'createDBCluster_vpcSecurityGroupIds' - A list of EC2 VPC security groups to associate with this cluster.
--
-- 'dbClusterIdentifier', 'createDBCluster_dbClusterIdentifier' - The cluster identifier. This parameter is stored as a lowercase string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens.
--
-- -   The first character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- Example: @my-cluster@
--
-- 'engine', 'createDBCluster_engine' - The name of the database engine to be used for this cluster.
--
-- Valid values: @docdb@
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
      dbClusterParameterGroupName = Prelude.Nothing,
      dbSubnetGroupName = Prelude.Nothing,
      deletionProtection = Prelude.Nothing,
      enableCloudwatchLogsExports = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      globalClusterIdentifier = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      masterUserPassword = Prelude.Nothing,
      masterUsername = Prelude.Nothing,
      port = Prelude.Nothing,
      preSignedUrl = Prelude.Nothing,
      preferredBackupWindow = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      storageEncrypted = Prelude.Nothing,
      tags = Prelude.Nothing,
      vpcSecurityGroupIds = Prelude.Nothing,
      dbClusterIdentifier = pDBClusterIdentifier_,
      engine = pEngine_
    }

-- | A list of Amazon EC2 Availability Zones that instances in the cluster
-- can be created in.
createDBCluster_availabilityZones :: Lens.Lens' CreateDBCluster (Prelude.Maybe [Prelude.Text])
createDBCluster_availabilityZones = Lens.lens (\CreateDBCluster' {availabilityZones} -> availabilityZones) (\s@CreateDBCluster' {} a -> s {availabilityZones = a} :: CreateDBCluster) Prelude.. Lens.mapping Lens.coerced

-- | The number of days for which automated backups are retained. You must
-- specify a minimum value of 1.
--
-- Default: 1
--
-- Constraints:
--
-- -   Must be a value from 1 to 35.
createDBCluster_backupRetentionPeriod :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Int)
createDBCluster_backupRetentionPeriod = Lens.lens (\CreateDBCluster' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@CreateDBCluster' {} a -> s {backupRetentionPeriod = a} :: CreateDBCluster)

-- | The name of the cluster parameter group to associate with this cluster.
createDBCluster_dbClusterParameterGroupName :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_dbClusterParameterGroupName = Lens.lens (\CreateDBCluster' {dbClusterParameterGroupName} -> dbClusterParameterGroupName) (\s@CreateDBCluster' {} a -> s {dbClusterParameterGroupName = a} :: CreateDBCluster)

-- | A subnet group to associate with this cluster.
--
-- Constraints: Must match the name of an existing @DBSubnetGroup@. Must
-- not be default.
--
-- Example: @mySubnetgroup@
createDBCluster_dbSubnetGroupName :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_dbSubnetGroupName = Lens.lens (\CreateDBCluster' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@CreateDBCluster' {} a -> s {dbSubnetGroupName = a} :: CreateDBCluster)

-- | Specifies whether this cluster can be deleted. If @DeletionProtection@
-- is enabled, the cluster cannot be deleted unless it is modified and
-- @DeletionProtection@ is disabled. @DeletionProtection@ protects clusters
-- from being accidentally deleted.
createDBCluster_deletionProtection :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Bool)
createDBCluster_deletionProtection = Lens.lens (\CreateDBCluster' {deletionProtection} -> deletionProtection) (\s@CreateDBCluster' {} a -> s {deletionProtection = a} :: CreateDBCluster)

-- | A list of log types that need to be enabled for exporting to Amazon
-- CloudWatch Logs. You can enable audit logs or profiler logs. For more
-- information, see
-- <https://docs.aws.amazon.com/documentdb/latest/developerguide/event-auditing.html Auditing Amazon DocumentDB Events>
-- and
-- <https://docs.aws.amazon.com/documentdb/latest/developerguide/profiling.html Profiling Amazon DocumentDB Operations>.
createDBCluster_enableCloudwatchLogsExports :: Lens.Lens' CreateDBCluster (Prelude.Maybe [Prelude.Text])
createDBCluster_enableCloudwatchLogsExports = Lens.lens (\CreateDBCluster' {enableCloudwatchLogsExports} -> enableCloudwatchLogsExports) (\s@CreateDBCluster' {} a -> s {enableCloudwatchLogsExports = a} :: CreateDBCluster) Prelude.. Lens.mapping Lens.coerced

-- | The version number of the database engine to use. The @--engine-version@
-- will default to the latest major engine version. For production
-- workloads, we recommend explicitly declaring this parameter with the
-- intended major engine version.
createDBCluster_engineVersion :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_engineVersion = Lens.lens (\CreateDBCluster' {engineVersion} -> engineVersion) (\s@CreateDBCluster' {} a -> s {engineVersion = a} :: CreateDBCluster)

-- | The cluster identifier of the new global cluster.
createDBCluster_globalClusterIdentifier :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_globalClusterIdentifier = Lens.lens (\CreateDBCluster' {globalClusterIdentifier} -> globalClusterIdentifier) (\s@CreateDBCluster' {} a -> s {globalClusterIdentifier = a} :: CreateDBCluster)

-- | The KMS key identifier for an encrypted cluster.
--
-- The KMS key identifier is the Amazon Resource Name (ARN) for the KMS
-- encryption key. If you are creating a cluster using the same Amazon Web
-- Services account that owns the KMS encryption key that is used to
-- encrypt the new cluster, you can use the KMS key alias instead of the
-- ARN for the KMS encryption key.
--
-- If an encryption key is not specified in @KmsKeyId@:
--
-- -   If the @StorageEncrypted@ parameter is @true@, Amazon DocumentDB
--     uses your default encryption key.
--
-- KMS creates the default encryption key for your Amazon Web Services
-- account. Your Amazon Web Services account has a different default
-- encryption key for each Amazon Web Services Regions.
createDBCluster_kmsKeyId :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_kmsKeyId = Lens.lens (\CreateDBCluster' {kmsKeyId} -> kmsKeyId) (\s@CreateDBCluster' {} a -> s {kmsKeyId = a} :: CreateDBCluster)

-- | The password for the master database user. This password can contain any
-- printable ASCII character except forward slash (\/), double quote (\"),
-- or the \"at\" symbol (\@).
--
-- Constraints: Must contain from 8 to 100 characters.
createDBCluster_masterUserPassword :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_masterUserPassword = Lens.lens (\CreateDBCluster' {masterUserPassword} -> masterUserPassword) (\s@CreateDBCluster' {} a -> s {masterUserPassword = a} :: CreateDBCluster)

-- | The name of the master user for the cluster.
--
-- Constraints:
--
-- -   Must be from 1 to 63 letters or numbers.
--
-- -   The first character must be a letter.
--
-- -   Cannot be a reserved word for the chosen database engine.
createDBCluster_masterUsername :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_masterUsername = Lens.lens (\CreateDBCluster' {masterUsername} -> masterUsername) (\s@CreateDBCluster' {} a -> s {masterUsername = a} :: CreateDBCluster)

-- | The port number on which the instances in the cluster accept
-- connections.
createDBCluster_port :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Int)
createDBCluster_port = Lens.lens (\CreateDBCluster' {port} -> port) (\s@CreateDBCluster' {} a -> s {port = a} :: CreateDBCluster)

-- | Not currently supported.
createDBCluster_preSignedUrl :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_preSignedUrl = Lens.lens (\CreateDBCluster' {preSignedUrl} -> preSignedUrl) (\s@CreateDBCluster' {} a -> s {preSignedUrl = a} :: CreateDBCluster)

-- | The daily time range during which automated backups are created if
-- automated backups are enabled using the @BackupRetentionPeriod@
-- parameter.
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Web Services Region.
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
-- block of time for each Amazon Web Services Region, occurring on a random
-- day of the week.
--
-- Valid days: Mon, Tue, Wed, Thu, Fri, Sat, Sun
--
-- Constraints: Minimum 30-minute window.
createDBCluster_preferredMaintenanceWindow :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Text)
createDBCluster_preferredMaintenanceWindow = Lens.lens (\CreateDBCluster' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@CreateDBCluster' {} a -> s {preferredMaintenanceWindow = a} :: CreateDBCluster)

-- | Specifies whether the cluster is encrypted.
createDBCluster_storageEncrypted :: Lens.Lens' CreateDBCluster (Prelude.Maybe Prelude.Bool)
createDBCluster_storageEncrypted = Lens.lens (\CreateDBCluster' {storageEncrypted} -> storageEncrypted) (\s@CreateDBCluster' {} a -> s {storageEncrypted = a} :: CreateDBCluster)

-- | The tags to be assigned to the cluster.
createDBCluster_tags :: Lens.Lens' CreateDBCluster (Prelude.Maybe [Tag])
createDBCluster_tags = Lens.lens (\CreateDBCluster' {tags} -> tags) (\s@CreateDBCluster' {} a -> s {tags = a} :: CreateDBCluster) Prelude.. Lens.mapping Lens.coerced

-- | A list of EC2 VPC security groups to associate with this cluster.
createDBCluster_vpcSecurityGroupIds :: Lens.Lens' CreateDBCluster (Prelude.Maybe [Prelude.Text])
createDBCluster_vpcSecurityGroupIds = Lens.lens (\CreateDBCluster' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@CreateDBCluster' {} a -> s {vpcSecurityGroupIds = a} :: CreateDBCluster) Prelude.. Lens.mapping Lens.coerced

-- | The cluster identifier. This parameter is stored as a lowercase string.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens.
--
-- -   The first character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- Example: @my-cluster@
createDBCluster_dbClusterIdentifier :: Lens.Lens' CreateDBCluster Prelude.Text
createDBCluster_dbClusterIdentifier = Lens.lens (\CreateDBCluster' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@CreateDBCluster' {} a -> s {dbClusterIdentifier = a} :: CreateDBCluster)

-- | The name of the database engine to be used for this cluster.
--
-- Valid values: @docdb@
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
      `Prelude.hashWithSalt` dbClusterParameterGroupName
      `Prelude.hashWithSalt` dbSubnetGroupName
      `Prelude.hashWithSalt` deletionProtection
      `Prelude.hashWithSalt` enableCloudwatchLogsExports
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` globalClusterIdentifier
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` masterUserPassword
      `Prelude.hashWithSalt` masterUsername
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` preSignedUrl
      `Prelude.hashWithSalt` preferredBackupWindow
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` storageEncrypted
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcSecurityGroupIds
      `Prelude.hashWithSalt` dbClusterIdentifier
      `Prelude.hashWithSalt` engine

instance Prelude.NFData CreateDBCluster where
  rnf CreateDBCluster' {..} =
    Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf backupRetentionPeriod
      `Prelude.seq` Prelude.rnf dbClusterParameterGroupName
      `Prelude.seq` Prelude.rnf dbSubnetGroupName
      `Prelude.seq` Prelude.rnf deletionProtection
      `Prelude.seq` Prelude.rnf enableCloudwatchLogsExports
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf globalClusterIdentifier
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf masterUserPassword
      `Prelude.seq` Prelude.rnf masterUsername
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf preSignedUrl
      `Prelude.seq` Prelude.rnf preferredBackupWindow
      `Prelude.seq` Prelude.rnf preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf storageEncrypted
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpcSecurityGroupIds
      `Prelude.seq` Prelude.rnf dbClusterIdentifier
      `Prelude.seq` Prelude.rnf engine

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
        "DBClusterParameterGroupName"
          Data.=: dbClusterParameterGroupName,
        "DBSubnetGroupName" Data.=: dbSubnetGroupName,
        "DeletionProtection" Data.=: deletionProtection,
        "EnableCloudwatchLogsExports"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> enableCloudwatchLogsExports
            ),
        "EngineVersion" Data.=: engineVersion,
        "GlobalClusterIdentifier"
          Data.=: globalClusterIdentifier,
        "KmsKeyId" Data.=: kmsKeyId,
        "MasterUserPassword" Data.=: masterUserPassword,
        "MasterUsername" Data.=: masterUsername,
        "Port" Data.=: port,
        "PreSignedUrl" Data.=: preSignedUrl,
        "PreferredBackupWindow"
          Data.=: preferredBackupWindow,
        "PreferredMaintenanceWindow"
          Data.=: preferredMaintenanceWindow,
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
