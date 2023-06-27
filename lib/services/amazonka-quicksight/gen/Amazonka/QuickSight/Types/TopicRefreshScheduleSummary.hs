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
-- Module      : Amazonka.QuickSight.Types.TopicRefreshScheduleSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TopicRefreshScheduleSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TopicRefreshSchedule

-- | A summary of the refresh schedule details for a dataset.
--
-- /See:/ 'newTopicRefreshScheduleSummary' smart constructor.
data TopicRefreshScheduleSummary = TopicRefreshScheduleSummary'
  { -- | The Amazon Resource Name (ARN) of the dataset.
    datasetArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the dataset.
    datasetId :: Prelude.Maybe Prelude.Text,
    -- | The name of the dataset.
    datasetName :: Prelude.Maybe Prelude.Text,
    -- | The definition of a refresh schedule.
    refreshSchedule :: Prelude.Maybe TopicRefreshSchedule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TopicRefreshScheduleSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetArn', 'topicRefreshScheduleSummary_datasetArn' - The Amazon Resource Name (ARN) of the dataset.
--
-- 'datasetId', 'topicRefreshScheduleSummary_datasetId' - The ID of the dataset.
--
-- 'datasetName', 'topicRefreshScheduleSummary_datasetName' - The name of the dataset.
--
-- 'refreshSchedule', 'topicRefreshScheduleSummary_refreshSchedule' - The definition of a refresh schedule.
newTopicRefreshScheduleSummary ::
  TopicRefreshScheduleSummary
newTopicRefreshScheduleSummary =
  TopicRefreshScheduleSummary'
    { datasetArn =
        Prelude.Nothing,
      datasetId = Prelude.Nothing,
      datasetName = Prelude.Nothing,
      refreshSchedule = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the dataset.
topicRefreshScheduleSummary_datasetArn :: Lens.Lens' TopicRefreshScheduleSummary (Prelude.Maybe Prelude.Text)
topicRefreshScheduleSummary_datasetArn = Lens.lens (\TopicRefreshScheduleSummary' {datasetArn} -> datasetArn) (\s@TopicRefreshScheduleSummary' {} a -> s {datasetArn = a} :: TopicRefreshScheduleSummary)

-- | The ID of the dataset.
topicRefreshScheduleSummary_datasetId :: Lens.Lens' TopicRefreshScheduleSummary (Prelude.Maybe Prelude.Text)
topicRefreshScheduleSummary_datasetId = Lens.lens (\TopicRefreshScheduleSummary' {datasetId} -> datasetId) (\s@TopicRefreshScheduleSummary' {} a -> s {datasetId = a} :: TopicRefreshScheduleSummary)

-- | The name of the dataset.
topicRefreshScheduleSummary_datasetName :: Lens.Lens' TopicRefreshScheduleSummary (Prelude.Maybe Prelude.Text)
topicRefreshScheduleSummary_datasetName = Lens.lens (\TopicRefreshScheduleSummary' {datasetName} -> datasetName) (\s@TopicRefreshScheduleSummary' {} a -> s {datasetName = a} :: TopicRefreshScheduleSummary)

-- | The definition of a refresh schedule.
topicRefreshScheduleSummary_refreshSchedule :: Lens.Lens' TopicRefreshScheduleSummary (Prelude.Maybe TopicRefreshSchedule)
topicRefreshScheduleSummary_refreshSchedule = Lens.lens (\TopicRefreshScheduleSummary' {refreshSchedule} -> refreshSchedule) (\s@TopicRefreshScheduleSummary' {} a -> s {refreshSchedule = a} :: TopicRefreshScheduleSummary)

instance Data.FromJSON TopicRefreshScheduleSummary where
  parseJSON =
    Data.withObject
      "TopicRefreshScheduleSummary"
      ( \x ->
          TopicRefreshScheduleSummary'
            Prelude.<$> (x Data..:? "DatasetArn")
            Prelude.<*> (x Data..:? "DatasetId")
            Prelude.<*> (x Data..:? "DatasetName")
            Prelude.<*> (x Data..:? "RefreshSchedule")
      )

instance Prelude.Hashable TopicRefreshScheduleSummary where
  hashWithSalt _salt TopicRefreshScheduleSummary' {..} =
    _salt
      `Prelude.hashWithSalt` datasetArn
      `Prelude.hashWithSalt` datasetId
      `Prelude.hashWithSalt` datasetName
      `Prelude.hashWithSalt` refreshSchedule

instance Prelude.NFData TopicRefreshScheduleSummary where
  rnf TopicRefreshScheduleSummary' {..} =
    Prelude.rnf datasetArn
      `Prelude.seq` Prelude.rnf datasetId
      `Prelude.seq` Prelude.rnf datasetName
      `Prelude.seq` Prelude.rnf refreshSchedule
