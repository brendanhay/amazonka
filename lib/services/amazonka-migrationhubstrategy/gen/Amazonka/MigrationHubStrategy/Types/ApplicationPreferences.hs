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
-- Module      : Amazonka.MigrationHubStrategy.Types.ApplicationPreferences
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.ApplicationPreferences where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.ManagementPreference
import qualified Amazonka.Prelude as Prelude

-- | Application preferences that you specify.
--
-- /See:/ 'newApplicationPreferences' smart constructor.
data ApplicationPreferences = ApplicationPreferences'
  { -- | Application preferences that you specify to prefer managed environment.
    managementPreference :: Prelude.Maybe ManagementPreference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationPreferences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'managementPreference', 'applicationPreferences_managementPreference' - Application preferences that you specify to prefer managed environment.
newApplicationPreferences ::
  ApplicationPreferences
newApplicationPreferences =
  ApplicationPreferences'
    { managementPreference =
        Prelude.Nothing
    }

-- | Application preferences that you specify to prefer managed environment.
applicationPreferences_managementPreference :: Lens.Lens' ApplicationPreferences (Prelude.Maybe ManagementPreference)
applicationPreferences_managementPreference = Lens.lens (\ApplicationPreferences' {managementPreference} -> managementPreference) (\s@ApplicationPreferences' {} a -> s {managementPreference = a} :: ApplicationPreferences)

instance Data.FromJSON ApplicationPreferences where
  parseJSON =
    Data.withObject
      "ApplicationPreferences"
      ( \x ->
          ApplicationPreferences'
            Prelude.<$> (x Data..:? "managementPreference")
      )

instance Prelude.Hashable ApplicationPreferences where
  hashWithSalt _salt ApplicationPreferences' {..} =
    _salt `Prelude.hashWithSalt` managementPreference

instance Prelude.NFData ApplicationPreferences where
  rnf ApplicationPreferences' {..} =
    Prelude.rnf managementPreference

instance Data.ToJSON ApplicationPreferences where
  toJSON ApplicationPreferences' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("managementPreference" Data..=)
              Prelude.<$> managementPreference
          ]
      )
