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
-- Module      : Amazonka.MigrationHubStrategy.Types.NoDatabaseMigrationPreference
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.NoDatabaseMigrationPreference where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.TargetDatabaseEngine
import qualified Amazonka.Prelude as Prelude

-- | The object containing details about database migration preferences, when
-- you have no particular preference.
--
-- /See:/ 'newNoDatabaseMigrationPreference' smart constructor.
data NoDatabaseMigrationPreference = NoDatabaseMigrationPreference'
  { -- | The target database engine for database migration preference that you
    -- specify.
    targetDatabaseEngine :: Prelude.NonEmpty TargetDatabaseEngine
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NoDatabaseMigrationPreference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetDatabaseEngine', 'noDatabaseMigrationPreference_targetDatabaseEngine' - The target database engine for database migration preference that you
-- specify.
newNoDatabaseMigrationPreference ::
  -- | 'targetDatabaseEngine'
  Prelude.NonEmpty TargetDatabaseEngine ->
  NoDatabaseMigrationPreference
newNoDatabaseMigrationPreference
  pTargetDatabaseEngine_ =
    NoDatabaseMigrationPreference'
      { targetDatabaseEngine =
          Lens.coerced Lens.# pTargetDatabaseEngine_
      }

-- | The target database engine for database migration preference that you
-- specify.
noDatabaseMigrationPreference_targetDatabaseEngine :: Lens.Lens' NoDatabaseMigrationPreference (Prelude.NonEmpty TargetDatabaseEngine)
noDatabaseMigrationPreference_targetDatabaseEngine = Lens.lens (\NoDatabaseMigrationPreference' {targetDatabaseEngine} -> targetDatabaseEngine) (\s@NoDatabaseMigrationPreference' {} a -> s {targetDatabaseEngine = a} :: NoDatabaseMigrationPreference) Prelude.. Lens.coerced

instance Data.FromJSON NoDatabaseMigrationPreference where
  parseJSON =
    Data.withObject
      "NoDatabaseMigrationPreference"
      ( \x ->
          NoDatabaseMigrationPreference'
            Prelude.<$> (x Data..: "targetDatabaseEngine")
      )

instance
  Prelude.Hashable
    NoDatabaseMigrationPreference
  where
  hashWithSalt _salt NoDatabaseMigrationPreference' {..} =
    _salt `Prelude.hashWithSalt` targetDatabaseEngine

instance Prelude.NFData NoDatabaseMigrationPreference where
  rnf NoDatabaseMigrationPreference' {..} =
    Prelude.rnf targetDatabaseEngine

instance Data.ToJSON NoDatabaseMigrationPreference where
  toJSON NoDatabaseMigrationPreference' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "targetDatabaseEngine"
                  Data..= targetDatabaseEngine
              )
          ]
      )
