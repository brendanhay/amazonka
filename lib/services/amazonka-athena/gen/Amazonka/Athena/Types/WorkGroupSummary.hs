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
-- Module      : Amazonka.Athena.Types.WorkGroupSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.WorkGroupSummary where

import Amazonka.Athena.Types.EngineVersion
import Amazonka.Athena.Types.WorkGroupState
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The summary information for the workgroup, which includes its name,
-- state, description, and the date and time it was created.
--
-- /See:/ 'newWorkGroupSummary' smart constructor.
data WorkGroupSummary = WorkGroupSummary'
  { -- | The name of the workgroup.
    name :: Prelude.Maybe Prelude.Text,
    -- | The state of the workgroup.
    state :: Prelude.Maybe WorkGroupState,
    -- | The workgroup description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The workgroup creation date and time.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The engine version setting for all queries on the workgroup. Queries on
    -- the @AmazonAthenaPreviewFunctionality@ workgroup run on the preview
    -- engine regardless of this setting.
    engineVersion :: Prelude.Maybe EngineVersion
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkGroupSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'workGroupSummary_name' - The name of the workgroup.
--
-- 'state', 'workGroupSummary_state' - The state of the workgroup.
--
-- 'description', 'workGroupSummary_description' - The workgroup description.
--
-- 'creationTime', 'workGroupSummary_creationTime' - The workgroup creation date and time.
--
-- 'engineVersion', 'workGroupSummary_engineVersion' - The engine version setting for all queries on the workgroup. Queries on
-- the @AmazonAthenaPreviewFunctionality@ workgroup run on the preview
-- engine regardless of this setting.
newWorkGroupSummary ::
  WorkGroupSummary
newWorkGroupSummary =
  WorkGroupSummary'
    { name = Prelude.Nothing,
      state = Prelude.Nothing,
      description = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      engineVersion = Prelude.Nothing
    }

-- | The name of the workgroup.
workGroupSummary_name :: Lens.Lens' WorkGroupSummary (Prelude.Maybe Prelude.Text)
workGroupSummary_name = Lens.lens (\WorkGroupSummary' {name} -> name) (\s@WorkGroupSummary' {} a -> s {name = a} :: WorkGroupSummary)

-- | The state of the workgroup.
workGroupSummary_state :: Lens.Lens' WorkGroupSummary (Prelude.Maybe WorkGroupState)
workGroupSummary_state = Lens.lens (\WorkGroupSummary' {state} -> state) (\s@WorkGroupSummary' {} a -> s {state = a} :: WorkGroupSummary)

-- | The workgroup description.
workGroupSummary_description :: Lens.Lens' WorkGroupSummary (Prelude.Maybe Prelude.Text)
workGroupSummary_description = Lens.lens (\WorkGroupSummary' {description} -> description) (\s@WorkGroupSummary' {} a -> s {description = a} :: WorkGroupSummary)

-- | The workgroup creation date and time.
workGroupSummary_creationTime :: Lens.Lens' WorkGroupSummary (Prelude.Maybe Prelude.UTCTime)
workGroupSummary_creationTime = Lens.lens (\WorkGroupSummary' {creationTime} -> creationTime) (\s@WorkGroupSummary' {} a -> s {creationTime = a} :: WorkGroupSummary) Prelude.. Lens.mapping Data._Time

-- | The engine version setting for all queries on the workgroup. Queries on
-- the @AmazonAthenaPreviewFunctionality@ workgroup run on the preview
-- engine regardless of this setting.
workGroupSummary_engineVersion :: Lens.Lens' WorkGroupSummary (Prelude.Maybe EngineVersion)
workGroupSummary_engineVersion = Lens.lens (\WorkGroupSummary' {engineVersion} -> engineVersion) (\s@WorkGroupSummary' {} a -> s {engineVersion = a} :: WorkGroupSummary)

instance Data.FromJSON WorkGroupSummary where
  parseJSON =
    Data.withObject
      "WorkGroupSummary"
      ( \x ->
          WorkGroupSummary'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "EngineVersion")
      )

instance Prelude.Hashable WorkGroupSummary where
  hashWithSalt _salt WorkGroupSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` engineVersion

instance Prelude.NFData WorkGroupSummary where
  rnf WorkGroupSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf engineVersion
