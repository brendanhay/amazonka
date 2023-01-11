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
-- Module      : Amazonka.IoTAnalytics.Types.DatasetTrigger
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.DatasetTrigger where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types.Schedule
import Amazonka.IoTAnalytics.Types.TriggeringDataset
import qualified Amazonka.Prelude as Prelude

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

instance Data.FromJSON DatasetTrigger where
  parseJSON =
    Data.withObject
      "DatasetTrigger"
      ( \x ->
          DatasetTrigger'
            Prelude.<$> (x Data..:? "dataset")
            Prelude.<*> (x Data..:? "schedule")
      )

instance Prelude.Hashable DatasetTrigger where
  hashWithSalt _salt DatasetTrigger' {..} =
    _salt `Prelude.hashWithSalt` dataset
      `Prelude.hashWithSalt` schedule

instance Prelude.NFData DatasetTrigger where
  rnf DatasetTrigger' {..} =
    Prelude.rnf dataset
      `Prelude.seq` Prelude.rnf schedule

instance Data.ToJSON DatasetTrigger where
  toJSON DatasetTrigger' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("dataset" Data..=) Prelude.<$> dataset,
            ("schedule" Data..=) Prelude.<$> schedule
          ]
      )
