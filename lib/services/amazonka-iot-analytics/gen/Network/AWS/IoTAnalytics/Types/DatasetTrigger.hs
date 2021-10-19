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
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetTrigger
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetTrigger where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types.Schedule
import Network.AWS.IoTAnalytics.Types.TriggeringDataset
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The @DatasetTrigger@ that specifies when the dataset is automatically
-- updated.
--
-- /See:/ 'newDatasetTrigger' smart constructor.
data DatasetTrigger = DatasetTrigger'
  { -- | The dataset whose content creation triggers the creation of this
    -- dataset\'s contents.
    dataset :: Prelude.Maybe TriggeringDataset,
    -- | The Schedule when the trigger is initiated.
    schedule :: Prelude.Maybe Schedule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetTrigger' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataset', 'datasetTrigger_dataset' - The dataset whose content creation triggers the creation of this
-- dataset\'s contents.
--
-- 'schedule', 'datasetTrigger_schedule' - The Schedule when the trigger is initiated.
newDatasetTrigger ::
  DatasetTrigger
newDatasetTrigger =
  DatasetTrigger'
    { dataset = Prelude.Nothing,
      schedule = Prelude.Nothing
    }

-- | The dataset whose content creation triggers the creation of this
-- dataset\'s contents.
datasetTrigger_dataset :: Lens.Lens' DatasetTrigger (Prelude.Maybe TriggeringDataset)
datasetTrigger_dataset = Lens.lens (\DatasetTrigger' {dataset} -> dataset) (\s@DatasetTrigger' {} a -> s {dataset = a} :: DatasetTrigger)

-- | The Schedule when the trigger is initiated.
datasetTrigger_schedule :: Lens.Lens' DatasetTrigger (Prelude.Maybe Schedule)
datasetTrigger_schedule = Lens.lens (\DatasetTrigger' {schedule} -> schedule) (\s@DatasetTrigger' {} a -> s {schedule = a} :: DatasetTrigger)

instance Core.FromJSON DatasetTrigger where
  parseJSON =
    Core.withObject
      "DatasetTrigger"
      ( \x ->
          DatasetTrigger'
            Prelude.<$> (x Core..:? "dataset")
            Prelude.<*> (x Core..:? "schedule")
      )

instance Prelude.Hashable DatasetTrigger

instance Prelude.NFData DatasetTrigger

instance Core.ToJSON DatasetTrigger where
  toJSON DatasetTrigger' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("dataset" Core..=) Prelude.<$> dataset,
            ("schedule" Core..=) Prelude.<$> schedule
          ]
      )
