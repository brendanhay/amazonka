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
-- Module      : Amazonka.RDS.Types.ClusterPendingModifiedValues
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.ClusterPendingModifiedValues where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types.PendingCloudwatchLogsExports

-- | This data type is used as a response element in the @ModifyDBCluster@
-- operation and contains changes that will be applied during the next
-- maintenance window.
--
-- /See:/ 'newClusterPendingModifiedValues' smart constructor.
data ClusterPendingModifiedValues = ClusterPendingModifiedValues'
  { -- | The allocated storage size in gibibytes (GiB) for all database engines
    -- except Amazon Aurora. For Aurora, @AllocatedStorage@ always returns 1,
    -- because Aurora DB cluster storage size isn\'t fixed, but instead
    -- automatically adjusts as needed.
    allocatedStorage :: Prelude.Maybe Prelude.Int,
    -- | The number of days for which automatic DB snapshots are retained.
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The DBClusterIdentifier value for the DB cluster.
    dbClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The database engine version.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether mapping of Amazon Web Services Identity
    -- and Access Management (IAM) accounts to database accounts is enabled.
    iAMDatabaseAuthenticationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The Provisioned IOPS (I\/O operations per second) value. This setting is
    -- only for non-Aurora Multi-AZ DB clusters.
    iops :: Prelude.Maybe Prelude.Int,
    -- | The master credentials for the DB cluster.
    masterUserPassword :: Prelude.Maybe Prelude.Text,
    pendingCloudwatchLogsExports :: Prelude.Maybe PendingCloudwatchLogsExports
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClusterPendingModifiedValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allocatedStorage', 'clusterPendingModifiedValues_allocatedStorage' - The allocated storage size in gibibytes (GiB) for all database engines
-- except Amazon Aurora. For Aurora, @AllocatedStorage@ always returns 1,
-- because Aurora DB cluster storage size isn\'t fixed, but instead
-- automatically adjusts as needed.
--
-- 'backupRetentionPeriod', 'clusterPendingModifiedValues_backupRetentionPeriod' - The number of days for which automatic DB snapshots are retained.
--
-- 'dbClusterIdentifier', 'clusterPendingModifiedValues_dbClusterIdentifier' - The DBClusterIdentifier value for the DB cluster.
--
-- 'engineVersion', 'clusterPendingModifiedValues_engineVersion' - The database engine version.
--
-- 'iAMDatabaseAuthenticationEnabled', 'clusterPendingModifiedValues_iAMDatabaseAuthenticationEnabled' - A value that indicates whether mapping of Amazon Web Services Identity
-- and Access Management (IAM) accounts to database accounts is enabled.
--
-- 'iops', 'clusterPendingModifiedValues_iops' - The Provisioned IOPS (I\/O operations per second) value. This setting is
-- only for non-Aurora Multi-AZ DB clusters.
--
-- 'masterUserPassword', 'clusterPendingModifiedValues_masterUserPassword' - The master credentials for the DB cluster.
--
-- 'pendingCloudwatchLogsExports', 'clusterPendingModifiedValues_pendingCloudwatchLogsExports' - Undocumented member.
newClusterPendingModifiedValues ::
  ClusterPendingModifiedValues
newClusterPendingModifiedValues =
  ClusterPendingModifiedValues'
    { allocatedStorage =
        Prelude.Nothing,
      backupRetentionPeriod = Prelude.Nothing,
      dbClusterIdentifier = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      iAMDatabaseAuthenticationEnabled =
        Prelude.Nothing,
      iops = Prelude.Nothing,
      masterUserPassword = Prelude.Nothing,
      pendingCloudwatchLogsExports =
        Prelude.Nothing
    }

-- | The allocated storage size in gibibytes (GiB) for all database engines
-- except Amazon Aurora. For Aurora, @AllocatedStorage@ always returns 1,
-- because Aurora DB cluster storage size isn\'t fixed, but instead
-- automatically adjusts as needed.
clusterPendingModifiedValues_allocatedStorage :: Lens.Lens' ClusterPendingModifiedValues (Prelude.Maybe Prelude.Int)
clusterPendingModifiedValues_allocatedStorage = Lens.lens (\ClusterPendingModifiedValues' {allocatedStorage} -> allocatedStorage) (\s@ClusterPendingModifiedValues' {} a -> s {allocatedStorage = a} :: ClusterPendingModifiedValues)

-- | The number of days for which automatic DB snapshots are retained.
clusterPendingModifiedValues_backupRetentionPeriod :: Lens.Lens' ClusterPendingModifiedValues (Prelude.Maybe Prelude.Int)
clusterPendingModifiedValues_backupRetentionPeriod = Lens.lens (\ClusterPendingModifiedValues' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@ClusterPendingModifiedValues' {} a -> s {backupRetentionPeriod = a} :: ClusterPendingModifiedValues)

-- | The DBClusterIdentifier value for the DB cluster.
clusterPendingModifiedValues_dbClusterIdentifier :: Lens.Lens' ClusterPendingModifiedValues (Prelude.Maybe Prelude.Text)
clusterPendingModifiedValues_dbClusterIdentifier = Lens.lens (\ClusterPendingModifiedValues' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@ClusterPendingModifiedValues' {} a -> s {dbClusterIdentifier = a} :: ClusterPendingModifiedValues)

-- | The database engine version.
clusterPendingModifiedValues_engineVersion :: Lens.Lens' ClusterPendingModifiedValues (Prelude.Maybe Prelude.Text)
clusterPendingModifiedValues_engineVersion = Lens.lens (\ClusterPendingModifiedValues' {engineVersion} -> engineVersion) (\s@ClusterPendingModifiedValues' {} a -> s {engineVersion = a} :: ClusterPendingModifiedValues)

-- | A value that indicates whether mapping of Amazon Web Services Identity
-- and Access Management (IAM) accounts to database accounts is enabled.
clusterPendingModifiedValues_iAMDatabaseAuthenticationEnabled :: Lens.Lens' ClusterPendingModifiedValues (Prelude.Maybe Prelude.Bool)
clusterPendingModifiedValues_iAMDatabaseAuthenticationEnabled = Lens.lens (\ClusterPendingModifiedValues' {iAMDatabaseAuthenticationEnabled} -> iAMDatabaseAuthenticationEnabled) (\s@ClusterPendingModifiedValues' {} a -> s {iAMDatabaseAuthenticationEnabled = a} :: ClusterPendingModifiedValues)

-- | The Provisioned IOPS (I\/O operations per second) value. This setting is
-- only for non-Aurora Multi-AZ DB clusters.
clusterPendingModifiedValues_iops :: Lens.Lens' ClusterPendingModifiedValues (Prelude.Maybe Prelude.Int)
clusterPendingModifiedValues_iops = Lens.lens (\ClusterPendingModifiedValues' {iops} -> iops) (\s@ClusterPendingModifiedValues' {} a -> s {iops = a} :: ClusterPendingModifiedValues)

-- | The master credentials for the DB cluster.
clusterPendingModifiedValues_masterUserPassword :: Lens.Lens' ClusterPendingModifiedValues (Prelude.Maybe Prelude.Text)
clusterPendingModifiedValues_masterUserPassword = Lens.lens (\ClusterPendingModifiedValues' {masterUserPassword} -> masterUserPassword) (\s@ClusterPendingModifiedValues' {} a -> s {masterUserPassword = a} :: ClusterPendingModifiedValues)

-- | Undocumented member.
clusterPendingModifiedValues_pendingCloudwatchLogsExports :: Lens.Lens' ClusterPendingModifiedValues (Prelude.Maybe PendingCloudwatchLogsExports)
clusterPendingModifiedValues_pendingCloudwatchLogsExports = Lens.lens (\ClusterPendingModifiedValues' {pendingCloudwatchLogsExports} -> pendingCloudwatchLogsExports) (\s@ClusterPendingModifiedValues' {} a -> s {pendingCloudwatchLogsExports = a} :: ClusterPendingModifiedValues)

instance Data.FromXML ClusterPendingModifiedValues where
  parseXML x =
    ClusterPendingModifiedValues'
      Prelude.<$> (x Data..@? "AllocatedStorage")
      Prelude.<*> (x Data..@? "BackupRetentionPeriod")
      Prelude.<*> (x Data..@? "DBClusterIdentifier")
      Prelude.<*> (x Data..@? "EngineVersion")
      Prelude.<*> (x Data..@? "IAMDatabaseAuthenticationEnabled")
      Prelude.<*> (x Data..@? "Iops")
      Prelude.<*> (x Data..@? "MasterUserPassword")
      Prelude.<*> (x Data..@? "PendingCloudwatchLogsExports")

instance
  Prelude.Hashable
    ClusterPendingModifiedValues
  where
  hashWithSalt _salt ClusterPendingModifiedValues' {..} =
    _salt
      `Prelude.hashWithSalt` allocatedStorage
      `Prelude.hashWithSalt` backupRetentionPeriod
      `Prelude.hashWithSalt` dbClusterIdentifier
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` iAMDatabaseAuthenticationEnabled
      `Prelude.hashWithSalt` iops
      `Prelude.hashWithSalt` masterUserPassword
      `Prelude.hashWithSalt` pendingCloudwatchLogsExports

instance Prelude.NFData ClusterPendingModifiedValues where
  rnf ClusterPendingModifiedValues' {..} =
    Prelude.rnf allocatedStorage `Prelude.seq`
      Prelude.rnf backupRetentionPeriod `Prelude.seq`
        Prelude.rnf dbClusterIdentifier `Prelude.seq`
          Prelude.rnf engineVersion `Prelude.seq`
            Prelude.rnf iAMDatabaseAuthenticationEnabled `Prelude.seq`
              Prelude.rnf iops `Prelude.seq`
                Prelude.rnf masterUserPassword `Prelude.seq`
                  Prelude.rnf pendingCloudwatchLogsExports
