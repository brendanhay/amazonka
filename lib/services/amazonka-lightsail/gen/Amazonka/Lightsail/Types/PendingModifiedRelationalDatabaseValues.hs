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
-- Module      : Amazonka.Lightsail.Types.PendingModifiedRelationalDatabaseValues
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.PendingModifiedRelationalDatabaseValues where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a pending database value modification.
--
-- /See:/ 'newPendingModifiedRelationalDatabaseValues' smart constructor.
data PendingModifiedRelationalDatabaseValues = PendingModifiedRelationalDatabaseValues'
  { -- | A Boolean value indicating whether automated backup retention is
    -- enabled.
    backupRetentionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The database engine version.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The password for the master user of the database.
    masterUserPassword :: Prelude.Maybe Prelude.Text
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
-- 'backupRetentionEnabled', 'pendingModifiedRelationalDatabaseValues_backupRetentionEnabled' - A Boolean value indicating whether automated backup retention is
-- enabled.
--
-- 'engineVersion', 'pendingModifiedRelationalDatabaseValues_engineVersion' - The database engine version.
--
-- 'masterUserPassword', 'pendingModifiedRelationalDatabaseValues_masterUserPassword' - The password for the master user of the database.
newPendingModifiedRelationalDatabaseValues ::
  PendingModifiedRelationalDatabaseValues
newPendingModifiedRelationalDatabaseValues =
  PendingModifiedRelationalDatabaseValues'
    { backupRetentionEnabled =
        Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      masterUserPassword =
        Prelude.Nothing
    }

-- | A Boolean value indicating whether automated backup retention is
-- enabled.
pendingModifiedRelationalDatabaseValues_backupRetentionEnabled :: Lens.Lens' PendingModifiedRelationalDatabaseValues (Prelude.Maybe Prelude.Bool)
pendingModifiedRelationalDatabaseValues_backupRetentionEnabled = Lens.lens (\PendingModifiedRelationalDatabaseValues' {backupRetentionEnabled} -> backupRetentionEnabled) (\s@PendingModifiedRelationalDatabaseValues' {} a -> s {backupRetentionEnabled = a} :: PendingModifiedRelationalDatabaseValues)

-- | The database engine version.
pendingModifiedRelationalDatabaseValues_engineVersion :: Lens.Lens' PendingModifiedRelationalDatabaseValues (Prelude.Maybe Prelude.Text)
pendingModifiedRelationalDatabaseValues_engineVersion = Lens.lens (\PendingModifiedRelationalDatabaseValues' {engineVersion} -> engineVersion) (\s@PendingModifiedRelationalDatabaseValues' {} a -> s {engineVersion = a} :: PendingModifiedRelationalDatabaseValues)

-- | The password for the master user of the database.
pendingModifiedRelationalDatabaseValues_masterUserPassword :: Lens.Lens' PendingModifiedRelationalDatabaseValues (Prelude.Maybe Prelude.Text)
pendingModifiedRelationalDatabaseValues_masterUserPassword = Lens.lens (\PendingModifiedRelationalDatabaseValues' {masterUserPassword} -> masterUserPassword) (\s@PendingModifiedRelationalDatabaseValues' {} a -> s {masterUserPassword = a} :: PendingModifiedRelationalDatabaseValues)

instance
  Data.FromJSON
    PendingModifiedRelationalDatabaseValues
  where
  parseJSON =
    Data.withObject
      "PendingModifiedRelationalDatabaseValues"
      ( \x ->
          PendingModifiedRelationalDatabaseValues'
            Prelude.<$> (x Data..:? "backupRetentionEnabled")
            Prelude.<*> (x Data..:? "engineVersion")
            Prelude.<*> (x Data..:? "masterUserPassword")
      )

instance
  Prelude.Hashable
    PendingModifiedRelationalDatabaseValues
  where
  hashWithSalt
    _salt
    PendingModifiedRelationalDatabaseValues' {..} =
      _salt
        `Prelude.hashWithSalt` backupRetentionEnabled
        `Prelude.hashWithSalt` engineVersion
        `Prelude.hashWithSalt` masterUserPassword

instance
  Prelude.NFData
    PendingModifiedRelationalDatabaseValues
  where
  rnf PendingModifiedRelationalDatabaseValues' {..} =
    Prelude.rnf backupRetentionEnabled
      `Prelude.seq` Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf masterUserPassword
