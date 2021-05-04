{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types.PendingCloudwatchLogsExports

-- | This data type is used as a response element in the @ModifyDBCluster@
-- operation and contains changes that will be applied during the next
-- maintenance window.
--
-- /See:/ 'newClusterPendingModifiedValues' smart constructor.
data ClusterPendingModifiedValues = ClusterPendingModifiedValues'
  { -- | The master credentials for the DB cluster.
    masterUserPassword :: Prelude.Maybe Prelude.Text,
    pendingCloudwatchLogsExports :: Prelude.Maybe PendingCloudwatchLogsExports,
    -- | The DBClusterIdentifier value for the DB cluster.
    dbClusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The database engine version.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether mapping of AWS Identity and Access
    -- Management (IAM) accounts to database accounts is enabled.
    iAMDatabaseAuthenticationEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      pendingCloudwatchLogsExports =
        Prelude.Nothing,
      dbClusterIdentifier = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      iAMDatabaseAuthenticationEnabled =
        Prelude.Nothing
    }

-- | The master credentials for the DB cluster.
clusterPendingModifiedValues_masterUserPassword :: Lens.Lens' ClusterPendingModifiedValues (Prelude.Maybe Prelude.Text)
clusterPendingModifiedValues_masterUserPassword = Lens.lens (\ClusterPendingModifiedValues' {masterUserPassword} -> masterUserPassword) (\s@ClusterPendingModifiedValues' {} a -> s {masterUserPassword = a} :: ClusterPendingModifiedValues)

-- | Undocumented member.
clusterPendingModifiedValues_pendingCloudwatchLogsExports :: Lens.Lens' ClusterPendingModifiedValues (Prelude.Maybe PendingCloudwatchLogsExports)
clusterPendingModifiedValues_pendingCloudwatchLogsExports = Lens.lens (\ClusterPendingModifiedValues' {pendingCloudwatchLogsExports} -> pendingCloudwatchLogsExports) (\s@ClusterPendingModifiedValues' {} a -> s {pendingCloudwatchLogsExports = a} :: ClusterPendingModifiedValues)

-- | The DBClusterIdentifier value for the DB cluster.
clusterPendingModifiedValues_dbClusterIdentifier :: Lens.Lens' ClusterPendingModifiedValues (Prelude.Maybe Prelude.Text)
clusterPendingModifiedValues_dbClusterIdentifier = Lens.lens (\ClusterPendingModifiedValues' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@ClusterPendingModifiedValues' {} a -> s {dbClusterIdentifier = a} :: ClusterPendingModifiedValues)

-- | The database engine version.
clusterPendingModifiedValues_engineVersion :: Lens.Lens' ClusterPendingModifiedValues (Prelude.Maybe Prelude.Text)
clusterPendingModifiedValues_engineVersion = Lens.lens (\ClusterPendingModifiedValues' {engineVersion} -> engineVersion) (\s@ClusterPendingModifiedValues' {} a -> s {engineVersion = a} :: ClusterPendingModifiedValues)

-- | A value that indicates whether mapping of AWS Identity and Access
-- Management (IAM) accounts to database accounts is enabled.
clusterPendingModifiedValues_iAMDatabaseAuthenticationEnabled :: Lens.Lens' ClusterPendingModifiedValues (Prelude.Maybe Prelude.Bool)
clusterPendingModifiedValues_iAMDatabaseAuthenticationEnabled = Lens.lens (\ClusterPendingModifiedValues' {iAMDatabaseAuthenticationEnabled} -> iAMDatabaseAuthenticationEnabled) (\s@ClusterPendingModifiedValues' {} a -> s {iAMDatabaseAuthenticationEnabled = a} :: ClusterPendingModifiedValues)

instance Prelude.FromXML ClusterPendingModifiedValues where
  parseXML x =
    ClusterPendingModifiedValues'
      Prelude.<$> (x Prelude..@? "MasterUserPassword")
      Prelude.<*> (x Prelude..@? "PendingCloudwatchLogsExports")
      Prelude.<*> (x Prelude..@? "DBClusterIdentifier")
      Prelude.<*> (x Prelude..@? "EngineVersion")
      Prelude.<*> (x Prelude..@? "IAMDatabaseAuthenticationEnabled")

instance
  Prelude.Hashable
    ClusterPendingModifiedValues

instance Prelude.NFData ClusterPendingModifiedValues
