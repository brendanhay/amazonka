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
-- Module      : Amazonka.MigrationHubStrategy.Types.ManagementPreference
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.ManagementPreference where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.AwsManagedResources
import Amazonka.MigrationHubStrategy.Types.NoManagementPreference
import Amazonka.MigrationHubStrategy.Types.SelfManageResources
import qualified Amazonka.Prelude as Prelude

-- | Preferences for migrating an application to AWS.
--
-- /See:/ 'newManagementPreference' smart constructor.
data ManagementPreference = ManagementPreference'
  { -- | Indicates interest in solutions that are managed by AWS.
    awsManagedResources :: Prelude.Maybe AwsManagedResources,
    -- | No specific preference.
    noPreference :: Prelude.Maybe NoManagementPreference,
    -- | Indicates interest in managing your own resources on AWS.
    selfManageResources :: Prelude.Maybe SelfManageResources
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ManagementPreference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsManagedResources', 'managementPreference_awsManagedResources' - Indicates interest in solutions that are managed by AWS.
--
-- 'noPreference', 'managementPreference_noPreference' - No specific preference.
--
-- 'selfManageResources', 'managementPreference_selfManageResources' - Indicates interest in managing your own resources on AWS.
newManagementPreference ::
  ManagementPreference
newManagementPreference =
  ManagementPreference'
    { awsManagedResources =
        Prelude.Nothing,
      noPreference = Prelude.Nothing,
      selfManageResources = Prelude.Nothing
    }

-- | Indicates interest in solutions that are managed by AWS.
managementPreference_awsManagedResources :: Lens.Lens' ManagementPreference (Prelude.Maybe AwsManagedResources)
managementPreference_awsManagedResources = Lens.lens (\ManagementPreference' {awsManagedResources} -> awsManagedResources) (\s@ManagementPreference' {} a -> s {awsManagedResources = a} :: ManagementPreference)

-- | No specific preference.
managementPreference_noPreference :: Lens.Lens' ManagementPreference (Prelude.Maybe NoManagementPreference)
managementPreference_noPreference = Lens.lens (\ManagementPreference' {noPreference} -> noPreference) (\s@ManagementPreference' {} a -> s {noPreference = a} :: ManagementPreference)

-- | Indicates interest in managing your own resources on AWS.
managementPreference_selfManageResources :: Lens.Lens' ManagementPreference (Prelude.Maybe SelfManageResources)
managementPreference_selfManageResources = Lens.lens (\ManagementPreference' {selfManageResources} -> selfManageResources) (\s@ManagementPreference' {} a -> s {selfManageResources = a} :: ManagementPreference)

instance Data.FromJSON ManagementPreference where
  parseJSON =
    Data.withObject
      "ManagementPreference"
      ( \x ->
          ManagementPreference'
            Prelude.<$> (x Data..:? "awsManagedResources")
            Prelude.<*> (x Data..:? "noPreference")
            Prelude.<*> (x Data..:? "selfManageResources")
      )

instance Prelude.Hashable ManagementPreference where
  hashWithSalt _salt ManagementPreference' {..} =
    _salt
      `Prelude.hashWithSalt` awsManagedResources
      `Prelude.hashWithSalt` noPreference
      `Prelude.hashWithSalt` selfManageResources

instance Prelude.NFData ManagementPreference where
  rnf ManagementPreference' {..} =
    Prelude.rnf awsManagedResources
      `Prelude.seq` Prelude.rnf noPreference
      `Prelude.seq` Prelude.rnf selfManageResources

instance Data.ToJSON ManagementPreference where
  toJSON ManagementPreference' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("awsManagedResources" Data..=)
              Prelude.<$> awsManagedResources,
            ("noPreference" Data..=) Prelude.<$> noPreference,
            ("selfManageResources" Data..=)
              Prelude.<$> selfManageResources
          ]
      )
