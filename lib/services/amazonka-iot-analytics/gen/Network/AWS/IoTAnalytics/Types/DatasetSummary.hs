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
-- Module      : Amazonka.IoTAnalytics.Types.DatasetSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.DatasetSummary where

import qualified Amazonka.Core as Core
import Amazonka.IoTAnalytics.Types.DatasetActionSummary
import Amazonka.IoTAnalytics.Types.DatasetStatus
import Amazonka.IoTAnalytics.Types.DatasetTrigger
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A summary of information about a dataset.
--
-- /See:/ 'newDatasetSummary' smart constructor.
data DatasetSummary = DatasetSummary'
  { -- | The time the dataset was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The status of the dataset.
    status :: Prelude.Maybe DatasetStatus,
    -- | A list of @DataActionSummary@ objects.
    actions :: Prelude.Maybe (Prelude.NonEmpty DatasetActionSummary),
    -- | A list of triggers. A trigger causes dataset content to be populated at
    -- a specified time interval or when another dataset is populated. The list
    -- of triggers can be empty or contain up to five @DataSetTrigger@ objects
    triggers :: Prelude.Maybe [DatasetTrigger],
    -- | The name of the dataset.
    datasetName :: Prelude.Maybe Prelude.Text,
    -- | The last time the dataset was updated.
    lastUpdateTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'datasetSummary_creationTime' - The time the dataset was created.
--
-- 'status', 'datasetSummary_status' - The status of the dataset.
--
-- 'actions', 'datasetSummary_actions' - A list of @DataActionSummary@ objects.
--
-- 'triggers', 'datasetSummary_triggers' - A list of triggers. A trigger causes dataset content to be populated at
-- a specified time interval or when another dataset is populated. The list
-- of triggers can be empty or contain up to five @DataSetTrigger@ objects
--
-- 'datasetName', 'datasetSummary_datasetName' - The name of the dataset.
--
-- 'lastUpdateTime', 'datasetSummary_lastUpdateTime' - The last time the dataset was updated.
newDatasetSummary ::
  DatasetSummary
newDatasetSummary =
  DatasetSummary'
    { creationTime = Prelude.Nothing,
      status = Prelude.Nothing,
      actions = Prelude.Nothing,
      triggers = Prelude.Nothing,
      datasetName = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing
    }

-- | The time the dataset was created.
datasetSummary_creationTime :: Lens.Lens' DatasetSummary (Prelude.Maybe Prelude.UTCTime)
datasetSummary_creationTime = Lens.lens (\DatasetSummary' {creationTime} -> creationTime) (\s@DatasetSummary' {} a -> s {creationTime = a} :: DatasetSummary) Prelude.. Lens.mapping Core._Time

-- | The status of the dataset.
datasetSummary_status :: Lens.Lens' DatasetSummary (Prelude.Maybe DatasetStatus)
datasetSummary_status = Lens.lens (\DatasetSummary' {status} -> status) (\s@DatasetSummary' {} a -> s {status = a} :: DatasetSummary)

-- | A list of @DataActionSummary@ objects.
datasetSummary_actions :: Lens.Lens' DatasetSummary (Prelude.Maybe (Prelude.NonEmpty DatasetActionSummary))
datasetSummary_actions = Lens.lens (\DatasetSummary' {actions} -> actions) (\s@DatasetSummary' {} a -> s {actions = a} :: DatasetSummary) Prelude.. Lens.mapping Lens.coerced

-- | A list of triggers. A trigger causes dataset content to be populated at
-- a specified time interval or when another dataset is populated. The list
-- of triggers can be empty or contain up to five @DataSetTrigger@ objects
datasetSummary_triggers :: Lens.Lens' DatasetSummary (Prelude.Maybe [DatasetTrigger])
datasetSummary_triggers = Lens.lens (\DatasetSummary' {triggers} -> triggers) (\s@DatasetSummary' {} a -> s {triggers = a} :: DatasetSummary) Prelude.. Lens.mapping Lens.coerced

-- | The name of the dataset.
datasetSummary_datasetName :: Lens.Lens' DatasetSummary (Prelude.Maybe Prelude.Text)
datasetSummary_datasetName = Lens.lens (\DatasetSummary' {datasetName} -> datasetName) (\s@DatasetSummary' {} a -> s {datasetName = a} :: DatasetSummary)

-- | The last time the dataset was updated.
datasetSummary_lastUpdateTime :: Lens.Lens' DatasetSummary (Prelude.Maybe Prelude.UTCTime)
datasetSummary_lastUpdateTime = Lens.lens (\DatasetSummary' {lastUpdateTime} -> lastUpdateTime) (\s@DatasetSummary' {} a -> s {lastUpdateTime = a} :: DatasetSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON DatasetSummary where
  parseJSON =
    Core.withObject
      "DatasetSummary"
      ( \x ->
          DatasetSummary'
            Prelude.<$> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "actions")
            Prelude.<*> (x Core..:? "triggers" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "datasetName")
            Prelude.<*> (x Core..:? "lastUpdateTime")
      )

instance Prelude.Hashable DatasetSummary

instance Prelude.NFData DatasetSummary
