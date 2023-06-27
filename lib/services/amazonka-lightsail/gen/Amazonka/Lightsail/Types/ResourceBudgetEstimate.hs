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
-- Module      : Amazonka.Lightsail.Types.ResourceBudgetEstimate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.ResourceBudgetEstimate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.CostEstimate
import Amazonka.Lightsail.Types.ResourceType
import qualified Amazonka.Prelude as Prelude

-- | Describes the estimated cost or usage that a budget tracks.
--
-- /See:/ 'newResourceBudgetEstimate' smart constructor.
data ResourceBudgetEstimate = ResourceBudgetEstimate'
  { -- | The cost estimate for the specified budget.
    costEstimates :: Prelude.Maybe [CostEstimate],
    -- | The estimate end time.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The resource name.
    resourceName :: Prelude.Maybe Prelude.Text,
    -- | The type of resource the budget will track.
    resourceType :: Prelude.Maybe ResourceType,
    -- | The estimate start time.
    startTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceBudgetEstimate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'costEstimates', 'resourceBudgetEstimate_costEstimates' - The cost estimate for the specified budget.
--
-- 'endTime', 'resourceBudgetEstimate_endTime' - The estimate end time.
--
-- 'resourceName', 'resourceBudgetEstimate_resourceName' - The resource name.
--
-- 'resourceType', 'resourceBudgetEstimate_resourceType' - The type of resource the budget will track.
--
-- 'startTime', 'resourceBudgetEstimate_startTime' - The estimate start time.
newResourceBudgetEstimate ::
  ResourceBudgetEstimate
newResourceBudgetEstimate =
  ResourceBudgetEstimate'
    { costEstimates =
        Prelude.Nothing,
      endTime = Prelude.Nothing,
      resourceName = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | The cost estimate for the specified budget.
resourceBudgetEstimate_costEstimates :: Lens.Lens' ResourceBudgetEstimate (Prelude.Maybe [CostEstimate])
resourceBudgetEstimate_costEstimates = Lens.lens (\ResourceBudgetEstimate' {costEstimates} -> costEstimates) (\s@ResourceBudgetEstimate' {} a -> s {costEstimates = a} :: ResourceBudgetEstimate) Prelude.. Lens.mapping Lens.coerced

-- | The estimate end time.
resourceBudgetEstimate_endTime :: Lens.Lens' ResourceBudgetEstimate (Prelude.Maybe Prelude.UTCTime)
resourceBudgetEstimate_endTime = Lens.lens (\ResourceBudgetEstimate' {endTime} -> endTime) (\s@ResourceBudgetEstimate' {} a -> s {endTime = a} :: ResourceBudgetEstimate) Prelude.. Lens.mapping Data._Time

-- | The resource name.
resourceBudgetEstimate_resourceName :: Lens.Lens' ResourceBudgetEstimate (Prelude.Maybe Prelude.Text)
resourceBudgetEstimate_resourceName = Lens.lens (\ResourceBudgetEstimate' {resourceName} -> resourceName) (\s@ResourceBudgetEstimate' {} a -> s {resourceName = a} :: ResourceBudgetEstimate)

-- | The type of resource the budget will track.
resourceBudgetEstimate_resourceType :: Lens.Lens' ResourceBudgetEstimate (Prelude.Maybe ResourceType)
resourceBudgetEstimate_resourceType = Lens.lens (\ResourceBudgetEstimate' {resourceType} -> resourceType) (\s@ResourceBudgetEstimate' {} a -> s {resourceType = a} :: ResourceBudgetEstimate)

-- | The estimate start time.
resourceBudgetEstimate_startTime :: Lens.Lens' ResourceBudgetEstimate (Prelude.Maybe Prelude.UTCTime)
resourceBudgetEstimate_startTime = Lens.lens (\ResourceBudgetEstimate' {startTime} -> startTime) (\s@ResourceBudgetEstimate' {} a -> s {startTime = a} :: ResourceBudgetEstimate) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON ResourceBudgetEstimate where
  parseJSON =
    Data.withObject
      "ResourceBudgetEstimate"
      ( \x ->
          ResourceBudgetEstimate'
            Prelude.<$> (x Data..:? "costEstimates" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "endTime")
            Prelude.<*> (x Data..:? "resourceName")
            Prelude.<*> (x Data..:? "resourceType")
            Prelude.<*> (x Data..:? "startTime")
      )

instance Prelude.Hashable ResourceBudgetEstimate where
  hashWithSalt _salt ResourceBudgetEstimate' {..} =
    _salt
      `Prelude.hashWithSalt` costEstimates
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData ResourceBudgetEstimate where
  rnf ResourceBudgetEstimate' {..} =
    Prelude.rnf costEstimates
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf startTime
