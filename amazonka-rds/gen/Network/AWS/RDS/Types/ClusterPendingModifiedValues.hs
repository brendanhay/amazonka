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
-- Module      : Network.AWS.RDS.Types.ClusterPendingModifiedValues
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.ClusterPendingModifiedValues where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types.PendingCloudwatchLogsExports

-- | This data type is used as a response element in the @ModifyDBCluster@
-- operation and contains changes that will be applied during the next
-- maintenance window.
--
-- /See:/ 'newClusterPendingModifiedValues' smart constructor.
data ClusterPendingModifiedValues = ClusterPendingModifiedValues'
  { -- | The master credentials for the DB cluster.
    masterUserPassword :: Core.Maybe Core.Text,
    pendingCloudwatchLogsExports :: Core.Maybe PendingCloudwatchLogsExports,
    -- | The DBClusterIdentifier value for the DB cluster.
    dbClusterIdentifier :: Core.Maybe Core.Text,
    -- | The database engine version.
    engineVersion :: Core.Maybe Core.Text,
    -- | A value that indicates whether mapping of AWS Identity and Access
    -- Management (IAM) accounts to database accounts is enabled.
    iAMDatabaseAuthenticationEnabled :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ClusterPendingModifiedValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'masterUserPassword', 'clusterPendingModifiedValues_masterUserPassword' - The master credentials for the DB cluster.
--
-- 'pendingCloudwatchLogsExports', 'clusterPendingModifiedValues_pendingCloudwatchLogsExports' - Undocumented member.
--
-- 'dbClusterIdentifier', 'clusterPendingModifiedValues_dbClusterIdentifier' - The DBClusterIdentifier value for the DB cluster.
--
-- 'engineVersion', 'clusterPendingModifiedValues_engineVersion' - The database engine version.
--
-- 'iAMDatabaseAuthenticationEnabled', 'clusterPendingModifiedValues_iAMDatabaseAuthenticationEnabled' - A value that indicates whether mapping of AWS Identity and Access
-- Management (IAM) accounts to database accounts is enabled.
newClusterPendingModifiedValues ::
  ClusterPendingModifiedValues
newClusterPendingModifiedValues =
  ClusterPendingModifiedValues'
    { masterUserPassword =
        Core.Nothing,
      pendingCloudwatchLogsExports = Core.Nothing,
      dbClusterIdentifier = Core.Nothing,
      engineVersion = Core.Nothing,
      iAMDatabaseAuthenticationEnabled =
        Core.Nothing
    }

-- | The master credentials for the DB cluster.
clusterPendingModifiedValues_masterUserPassword :: Lens.Lens' ClusterPendingModifiedValues (Core.Maybe Core.Text)
clusterPendingModifiedValues_masterUserPassword = Lens.lens (\ClusterPendingModifiedValues' {masterUserPassword} -> masterUserPassword) (\s@ClusterPendingModifiedValues' {} a -> s {masterUserPassword = a} :: ClusterPendingModifiedValues)

-- | Undocumented member.
clusterPendingModifiedValues_pendingCloudwatchLogsExports :: Lens.Lens' ClusterPendingModifiedValues (Core.Maybe PendingCloudwatchLogsExports)
clusterPendingModifiedValues_pendingCloudwatchLogsExports = Lens.lens (\ClusterPendingModifiedValues' {pendingCloudwatchLogsExports} -> pendingCloudwatchLogsExports) (\s@ClusterPendingModifiedValues' {} a -> s {pendingCloudwatchLogsExports = a} :: ClusterPendingModifiedValues)

-- | The DBClusterIdentifier value for the DB cluster.
clusterPendingModifiedValues_dbClusterIdentifier :: Lens.Lens' ClusterPendingModifiedValues (Core.Maybe Core.Text)
clusterPendingModifiedValues_dbClusterIdentifier = Lens.lens (\ClusterPendingModifiedValues' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@ClusterPendingModifiedValues' {} a -> s {dbClusterIdentifier = a} :: ClusterPendingModifiedValues)

-- | The database engine version.
clusterPendingModifiedValues_engineVersion :: Lens.Lens' ClusterPendingModifiedValues (Core.Maybe Core.Text)
clusterPendingModifiedValues_engineVersion = Lens.lens (\ClusterPendingModifiedValues' {engineVersion} -> engineVersion) (\s@ClusterPendingModifiedValues' {} a -> s {engineVersion = a} :: ClusterPendingModifiedValues)

-- | A value that indicates whether mapping of AWS Identity and Access
-- Management (IAM) accounts to database accounts is enabled.
clusterPendingModifiedValues_iAMDatabaseAuthenticationEnabled :: Lens.Lens' ClusterPendingModifiedValues (Core.Maybe Core.Bool)
clusterPendingModifiedValues_iAMDatabaseAuthenticationEnabled = Lens.lens (\ClusterPendingModifiedValues' {iAMDatabaseAuthenticationEnabled} -> iAMDatabaseAuthenticationEnabled) (\s@ClusterPendingModifiedValues' {} a -> s {iAMDatabaseAuthenticationEnabled = a} :: ClusterPendingModifiedValues)

instance Core.FromXML ClusterPendingModifiedValues where
  parseXML x =
    ClusterPendingModifiedValues'
      Core.<$> (x Core..@? "MasterUserPassword")
      Core.<*> (x Core..@? "PendingCloudwatchLogsExports")
      Core.<*> (x Core..@? "DBClusterIdentifier")
      Core.<*> (x Core..@? "EngineVersion")
      Core.<*> (x Core..@? "IAMDatabaseAuthenticationEnabled")

instance Core.Hashable ClusterPendingModifiedValues

instance Core.NFData ClusterPendingModifiedValues
