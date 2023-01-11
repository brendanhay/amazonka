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
-- Module      : Amazonka.MigrationHubStrategy.Types.NoManagementPreference
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.NoManagementPreference where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.NoPreferenceTargetDestination
import qualified Amazonka.Prelude as Prelude

-- | Object containing the choice of application destination that you
-- specify.
--
-- /See:/ 'newNoManagementPreference' smart constructor.
data NoManagementPreference = NoManagementPreference'
  { -- | The choice of application destination that you specify.
    targetDestination :: Prelude.NonEmpty NoPreferenceTargetDestination
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NoManagementPreference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetDestination', 'noManagementPreference_targetDestination' - The choice of application destination that you specify.
newNoManagementPreference ::
  -- | 'targetDestination'
  Prelude.NonEmpty NoPreferenceTargetDestination ->
  NoManagementPreference
newNoManagementPreference pTargetDestination_ =
  NoManagementPreference'
    { targetDestination =
        Lens.coerced Lens.# pTargetDestination_
    }

-- | The choice of application destination that you specify.
noManagementPreference_targetDestination :: Lens.Lens' NoManagementPreference (Prelude.NonEmpty NoPreferenceTargetDestination)
noManagementPreference_targetDestination = Lens.lens (\NoManagementPreference' {targetDestination} -> targetDestination) (\s@NoManagementPreference' {} a -> s {targetDestination = a} :: NoManagementPreference) Prelude.. Lens.coerced

instance Data.FromJSON NoManagementPreference where
  parseJSON =
    Data.withObject
      "NoManagementPreference"
      ( \x ->
          NoManagementPreference'
            Prelude.<$> (x Data..: "targetDestination")
      )

instance Prelude.Hashable NoManagementPreference where
  hashWithSalt _salt NoManagementPreference' {..} =
    _salt `Prelude.hashWithSalt` targetDestination

instance Prelude.NFData NoManagementPreference where
  rnf NoManagementPreference' {..} =
    Prelude.rnf targetDestination

instance Data.ToJSON NoManagementPreference where
  toJSON NoManagementPreference' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("targetDestination" Data..= targetDestination)
          ]
      )
