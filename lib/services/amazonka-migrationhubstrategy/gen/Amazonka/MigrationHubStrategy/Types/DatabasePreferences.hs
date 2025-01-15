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
-- Module      : Amazonka.MigrationHubStrategy.Types.DatabasePreferences
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.DatabasePreferences where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.DatabaseManagementPreference
import Amazonka.MigrationHubStrategy.Types.DatabaseMigrationPreference
import qualified Amazonka.Prelude as Prelude

-- | Preferences on managing your databases on AWS.
--
-- /See:/ 'newDatabasePreferences' smart constructor.
data DatabasePreferences = DatabasePreferences'
  { -- | Specifies whether you\'re interested in self-managed databases or
    -- databases managed by AWS.
    databaseManagementPreference :: Prelude.Maybe DatabaseManagementPreference,
    -- | Specifies your preferred migration path.
    databaseMigrationPreference :: Prelude.Maybe DatabaseMigrationPreference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatabasePreferences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'databaseManagementPreference', 'databasePreferences_databaseManagementPreference' - Specifies whether you\'re interested in self-managed databases or
-- databases managed by AWS.
--
-- 'databaseMigrationPreference', 'databasePreferences_databaseMigrationPreference' - Specifies your preferred migration path.
newDatabasePreferences ::
  DatabasePreferences
newDatabasePreferences =
  DatabasePreferences'
    { databaseManagementPreference =
        Prelude.Nothing,
      databaseMigrationPreference = Prelude.Nothing
    }

-- | Specifies whether you\'re interested in self-managed databases or
-- databases managed by AWS.
databasePreferences_databaseManagementPreference :: Lens.Lens' DatabasePreferences (Prelude.Maybe DatabaseManagementPreference)
databasePreferences_databaseManagementPreference = Lens.lens (\DatabasePreferences' {databaseManagementPreference} -> databaseManagementPreference) (\s@DatabasePreferences' {} a -> s {databaseManagementPreference = a} :: DatabasePreferences)

-- | Specifies your preferred migration path.
databasePreferences_databaseMigrationPreference :: Lens.Lens' DatabasePreferences (Prelude.Maybe DatabaseMigrationPreference)
databasePreferences_databaseMigrationPreference = Lens.lens (\DatabasePreferences' {databaseMigrationPreference} -> databaseMigrationPreference) (\s@DatabasePreferences' {} a -> s {databaseMigrationPreference = a} :: DatabasePreferences)

instance Data.FromJSON DatabasePreferences where
  parseJSON =
    Data.withObject
      "DatabasePreferences"
      ( \x ->
          DatabasePreferences'
            Prelude.<$> (x Data..:? "databaseManagementPreference")
            Prelude.<*> (x Data..:? "databaseMigrationPreference")
      )

instance Prelude.Hashable DatabasePreferences where
  hashWithSalt _salt DatabasePreferences' {..} =
    _salt
      `Prelude.hashWithSalt` databaseManagementPreference
      `Prelude.hashWithSalt` databaseMigrationPreference

instance Prelude.NFData DatabasePreferences where
  rnf DatabasePreferences' {..} =
    Prelude.rnf databaseManagementPreference `Prelude.seq`
      Prelude.rnf databaseMigrationPreference

instance Data.ToJSON DatabasePreferences where
  toJSON DatabasePreferences' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("databaseManagementPreference" Data..=)
              Prelude.<$> databaseManagementPreference,
            ("databaseMigrationPreference" Data..=)
              Prelude.<$> databaseMigrationPreference
          ]
      )
