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
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetSummary where

import Network.AWS.IoTAnalytics.Types.DatasetActionSummary
import Network.AWS.IoTAnalytics.Types.DatasetStatus
import Network.AWS.IoTAnalytics.Types.DatasetTrigger
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A summary of information about a data set.
--
-- /See:/ 'newDatasetSummary' smart constructor.
data DatasetSummary = DatasetSummary'
  { -- | The status of the data set.
    status :: Prelude.Maybe DatasetStatus,
    -- | The time the data set was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The last time the data set was updated.
    lastUpdateTime :: Prelude.Maybe Prelude.POSIX,
    -- | A list of triggers. A trigger causes data set content to be populated at
    -- a specified time interval or when another data set is populated. The
    -- list of triggers can be empty or contain up to five @DataSetTrigger@
    -- objects
    triggers :: Prelude.Maybe [DatasetTrigger],
    -- | A list of @DataActionSummary@ objects.
    actions :: Prelude.Maybe (Prelude.NonEmpty DatasetActionSummary),
    -- | The name of the data set.
    datasetName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DatasetSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'datasetSummary_status' - The status of the data set.
--
-- 'creationTime', 'datasetSummary_creationTime' - The time the data set was created.
--
-- 'lastUpdateTime', 'datasetSummary_lastUpdateTime' - The last time the data set was updated.
--
-- 'triggers', 'datasetSummary_triggers' - A list of triggers. A trigger causes data set content to be populated at
-- a specified time interval or when another data set is populated. The
-- list of triggers can be empty or contain up to five @DataSetTrigger@
-- objects
--
-- 'actions', 'datasetSummary_actions' - A list of @DataActionSummary@ objects.
--
-- 'datasetName', 'datasetSummary_datasetName' - The name of the data set.
newDatasetSummary ::
  DatasetSummary
newDatasetSummary =
  DatasetSummary'
    { status = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      triggers = Prelude.Nothing,
      actions = Prelude.Nothing,
      datasetName = Prelude.Nothing
    }

-- | The status of the data set.
datasetSummary_status :: Lens.Lens' DatasetSummary (Prelude.Maybe DatasetStatus)
datasetSummary_status = Lens.lens (\DatasetSummary' {status} -> status) (\s@DatasetSummary' {} a -> s {status = a} :: DatasetSummary)

-- | The time the data set was created.
datasetSummary_creationTime :: Lens.Lens' DatasetSummary (Prelude.Maybe Prelude.UTCTime)
datasetSummary_creationTime = Lens.lens (\DatasetSummary' {creationTime} -> creationTime) (\s@DatasetSummary' {} a -> s {creationTime = a} :: DatasetSummary) Prelude.. Lens.mapping Prelude._Time

-- | The last time the data set was updated.
datasetSummary_lastUpdateTime :: Lens.Lens' DatasetSummary (Prelude.Maybe Prelude.UTCTime)
datasetSummary_lastUpdateTime = Lens.lens (\DatasetSummary' {lastUpdateTime} -> lastUpdateTime) (\s@DatasetSummary' {} a -> s {lastUpdateTime = a} :: DatasetSummary) Prelude.. Lens.mapping Prelude._Time

-- | A list of triggers. A trigger causes data set content to be populated at
-- a specified time interval or when another data set is populated. The
-- list of triggers can be empty or contain up to five @DataSetTrigger@
-- objects
datasetSummary_triggers :: Lens.Lens' DatasetSummary (Prelude.Maybe [DatasetTrigger])
datasetSummary_triggers = Lens.lens (\DatasetSummary' {triggers} -> triggers) (\s@DatasetSummary' {} a -> s {triggers = a} :: DatasetSummary) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of @DataActionSummary@ objects.
datasetSummary_actions :: Lens.Lens' DatasetSummary (Prelude.Maybe (Prelude.NonEmpty DatasetActionSummary))
datasetSummary_actions = Lens.lens (\DatasetSummary' {actions} -> actions) (\s@DatasetSummary' {} a -> s {actions = a} :: DatasetSummary) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the data set.
datasetSummary_datasetName :: Lens.Lens' DatasetSummary (Prelude.Maybe Prelude.Text)
datasetSummary_datasetName = Lens.lens (\DatasetSummary' {datasetName} -> datasetName) (\s@DatasetSummary' {} a -> s {datasetName = a} :: DatasetSummary)

instance Prelude.FromJSON DatasetSummary where
  parseJSON =
    Prelude.withObject
      "DatasetSummary"
      ( \x ->
          DatasetSummary'
            Prelude.<$> (x Prelude..:? "status")
            Prelude.<*> (x Prelude..:? "creationTime")
            Prelude.<*> (x Prelude..:? "lastUpdateTime")
            Prelude.<*> (x Prelude..:? "triggers" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "actions")
            Prelude.<*> (x Prelude..:? "datasetName")
      )

instance Prelude.Hashable DatasetSummary

instance Prelude.NFData DatasetSummary
