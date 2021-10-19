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
-- Module      : Network.AWS.Lightsail.Types.PendingModifiedRelationalDatabaseValues
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.PendingModifiedRelationalDatabaseValues where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a pending database value modification.
--
-- /See:/ 'newPendingModifiedRelationalDatabaseValues' smart constructor.
data PendingModifiedRelationalDatabaseValues = PendingModifiedRelationalDatabaseValues'
  { -- | The database engine version.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The password for the master user of the database.
    masterUserPassword :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value indicating whether automated backup retention is
    -- enabled.
    backupRetentionEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PendingModifiedRelationalDatabaseValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineVersion', 'pendingModifiedRelationalDatabaseValues_engineVersion' - The database engine version.
--
-- 'masterUserPassword', 'pendingModifiedRelationalDatabaseValues_masterUserPassword' - The password for the master user of the database.
--
-- 'backupRetentionEnabled', 'pendingModifiedRelationalDatabaseValues_backupRetentionEnabled' - A Boolean value indicating whether automated backup retention is
-- enabled.
newPendingModifiedRelationalDatabaseValues ::
  PendingModifiedRelationalDatabaseValues
newPendingModifiedRelationalDatabaseValues =
  PendingModifiedRelationalDatabaseValues'
    { engineVersion =
        Prelude.Nothing,
      masterUserPassword =
        Prelude.Nothing,
      backupRetentionEnabled =
        Prelude.Nothing
    }

-- | The database engine version.
pendingModifiedRelationalDatabaseValues_engineVersion :: Lens.Lens' PendingModifiedRelationalDatabaseValues (Prelude.Maybe Prelude.Text)
pendingModifiedRelationalDatabaseValues_engineVersion = Lens.lens (\PendingModifiedRelationalDatabaseValues' {engineVersion} -> engineVersion) (\s@PendingModifiedRelationalDatabaseValues' {} a -> s {engineVersion = a} :: PendingModifiedRelationalDatabaseValues)

-- | The password for the master user of the database.
pendingModifiedRelationalDatabaseValues_masterUserPassword :: Lens.Lens' PendingModifiedRelationalDatabaseValues (Prelude.Maybe Prelude.Text)
pendingModifiedRelationalDatabaseValues_masterUserPassword = Lens.lens (\PendingModifiedRelationalDatabaseValues' {masterUserPassword} -> masterUserPassword) (\s@PendingModifiedRelationalDatabaseValues' {} a -> s {masterUserPassword = a} :: PendingModifiedRelationalDatabaseValues)

-- | A Boolean value indicating whether automated backup retention is
-- enabled.
pendingModifiedRelationalDatabaseValues_backupRetentionEnabled :: Lens.Lens' PendingModifiedRelationalDatabaseValues (Prelude.Maybe Prelude.Bool)
pendingModifiedRelationalDatabaseValues_backupRetentionEnabled = Lens.lens (\PendingModifiedRelationalDatabaseValues' {backupRetentionEnabled} -> backupRetentionEnabled) (\s@PendingModifiedRelationalDatabaseValues' {} a -> s {backupRetentionEnabled = a} :: PendingModifiedRelationalDatabaseValues)

instance
  Core.FromJSON
    PendingModifiedRelationalDatabaseValues
  where
  parseJSON =
    Core.withObject
      "PendingModifiedRelationalDatabaseValues"
      ( \x ->
          PendingModifiedRelationalDatabaseValues'
            Prelude.<$> (x Core..:? "engineVersion")
            Prelude.<*> (x Core..:? "masterUserPassword")
            Prelude.<*> (x Core..:? "backupRetentionEnabled")
      )

instance
  Prelude.Hashable
    PendingModifiedRelationalDatabaseValues

instance
  Prelude.NFData
    PendingModifiedRelationalDatabaseValues
