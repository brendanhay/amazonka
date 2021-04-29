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
-- Module      : Network.AWS.Athena.Types.WorkGroupSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.WorkGroupSummary where

import Network.AWS.Athena.Types.EngineVersion
import Network.AWS.Athena.Types.WorkGroupState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The summary information for the workgroup, which includes its name,
-- state, description, and the date and time it was created.
--
-- /See:/ 'newWorkGroupSummary' smart constructor.
data WorkGroupSummary = WorkGroupSummary'
  { -- | The workgroup creation date and time.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The state of the workgroup.
    state :: Prelude.Maybe WorkGroupState,
    -- | The name of the workgroup.
    name :: Prelude.Maybe Prelude.Text,
    -- | The engine version setting for all queries on the workgroup. Queries on
    -- the @AmazonAthenaPreviewFunctionality@ workgroup run on the preview
    -- engine regardless of this setting.
    engineVersion :: Prelude.Maybe EngineVersion,
    -- | The workgroup description.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'WorkGroupSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'workGroupSummary_creationTime' - The workgroup creation date and time.
--
-- 'state', 'workGroupSummary_state' - The state of the workgroup.
--
-- 'name', 'workGroupSummary_name' - The name of the workgroup.
--
-- 'engineVersion', 'workGroupSummary_engineVersion' - The engine version setting for all queries on the workgroup. Queries on
-- the @AmazonAthenaPreviewFunctionality@ workgroup run on the preview
-- engine regardless of this setting.
--
-- 'description', 'workGroupSummary_description' - The workgroup description.
newWorkGroupSummary ::
  WorkGroupSummary
newWorkGroupSummary =
  WorkGroupSummary'
    { creationTime = Prelude.Nothing,
      state = Prelude.Nothing,
      name = Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The workgroup creation date and time.
workGroupSummary_creationTime :: Lens.Lens' WorkGroupSummary (Prelude.Maybe Prelude.UTCTime)
workGroupSummary_creationTime = Lens.lens (\WorkGroupSummary' {creationTime} -> creationTime) (\s@WorkGroupSummary' {} a -> s {creationTime = a} :: WorkGroupSummary) Prelude.. Lens.mapping Prelude._Time

-- | The state of the workgroup.
workGroupSummary_state :: Lens.Lens' WorkGroupSummary (Prelude.Maybe WorkGroupState)
workGroupSummary_state = Lens.lens (\WorkGroupSummary' {state} -> state) (\s@WorkGroupSummary' {} a -> s {state = a} :: WorkGroupSummary)

-- | The name of the workgroup.
workGroupSummary_name :: Lens.Lens' WorkGroupSummary (Prelude.Maybe Prelude.Text)
workGroupSummary_name = Lens.lens (\WorkGroupSummary' {name} -> name) (\s@WorkGroupSummary' {} a -> s {name = a} :: WorkGroupSummary)

-- | The engine version setting for all queries on the workgroup. Queries on
-- the @AmazonAthenaPreviewFunctionality@ workgroup run on the preview
-- engine regardless of this setting.
workGroupSummary_engineVersion :: Lens.Lens' WorkGroupSummary (Prelude.Maybe EngineVersion)
workGroupSummary_engineVersion = Lens.lens (\WorkGroupSummary' {engineVersion} -> engineVersion) (\s@WorkGroupSummary' {} a -> s {engineVersion = a} :: WorkGroupSummary)

-- | The workgroup description.
workGroupSummary_description :: Lens.Lens' WorkGroupSummary (Prelude.Maybe Prelude.Text)
workGroupSummary_description = Lens.lens (\WorkGroupSummary' {description} -> description) (\s@WorkGroupSummary' {} a -> s {description = a} :: WorkGroupSummary)

instance Prelude.FromJSON WorkGroupSummary where
  parseJSON =
    Prelude.withObject
      "WorkGroupSummary"
      ( \x ->
          WorkGroupSummary'
            Prelude.<$> (x Prelude..:? "CreationTime")
            Prelude.<*> (x Prelude..:? "State")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "EngineVersion")
            Prelude.<*> (x Prelude..:? "Description")
      )

instance Prelude.Hashable WorkGroupSummary

instance Prelude.NFData WorkGroupSummary
