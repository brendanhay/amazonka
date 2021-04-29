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
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetContentSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetContentSummary where

import Network.AWS.IoTAnalytics.Types.DatasetContentStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Summary information about dataset contents.
--
-- /See:/ 'newDatasetContentSummary' smart constructor.
data DatasetContentSummary = DatasetContentSummary'
  { -- | The status of the data set contents.
    status :: Prelude.Maybe DatasetContentStatus,
    -- | The actual time the creation of the dataset contents was started.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The time the dataset content status was updated to SUCCEEDED or FAILED.
    completionTime :: Prelude.Maybe Prelude.POSIX,
    -- | The time the creation of the dataset contents was scheduled to start.
    scheduleTime :: Prelude.Maybe Prelude.POSIX,
    -- | The version of the dataset contents.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DatasetContentSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'datasetContentSummary_status' - The status of the data set contents.
--
-- 'creationTime', 'datasetContentSummary_creationTime' - The actual time the creation of the dataset contents was started.
--
-- 'completionTime', 'datasetContentSummary_completionTime' - The time the dataset content status was updated to SUCCEEDED or FAILED.
--
-- 'scheduleTime', 'datasetContentSummary_scheduleTime' - The time the creation of the dataset contents was scheduled to start.
--
-- 'version', 'datasetContentSummary_version' - The version of the dataset contents.
newDatasetContentSummary ::
  DatasetContentSummary
newDatasetContentSummary =
  DatasetContentSummary'
    { status = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      completionTime = Prelude.Nothing,
      scheduleTime = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The status of the data set contents.
datasetContentSummary_status :: Lens.Lens' DatasetContentSummary (Prelude.Maybe DatasetContentStatus)
datasetContentSummary_status = Lens.lens (\DatasetContentSummary' {status} -> status) (\s@DatasetContentSummary' {} a -> s {status = a} :: DatasetContentSummary)

-- | The actual time the creation of the dataset contents was started.
datasetContentSummary_creationTime :: Lens.Lens' DatasetContentSummary (Prelude.Maybe Prelude.UTCTime)
datasetContentSummary_creationTime = Lens.lens (\DatasetContentSummary' {creationTime} -> creationTime) (\s@DatasetContentSummary' {} a -> s {creationTime = a} :: DatasetContentSummary) Prelude.. Lens.mapping Prelude._Time

-- | The time the dataset content status was updated to SUCCEEDED or FAILED.
datasetContentSummary_completionTime :: Lens.Lens' DatasetContentSummary (Prelude.Maybe Prelude.UTCTime)
datasetContentSummary_completionTime = Lens.lens (\DatasetContentSummary' {completionTime} -> completionTime) (\s@DatasetContentSummary' {} a -> s {completionTime = a} :: DatasetContentSummary) Prelude.. Lens.mapping Prelude._Time

-- | The time the creation of the dataset contents was scheduled to start.
datasetContentSummary_scheduleTime :: Lens.Lens' DatasetContentSummary (Prelude.Maybe Prelude.UTCTime)
datasetContentSummary_scheduleTime = Lens.lens (\DatasetContentSummary' {scheduleTime} -> scheduleTime) (\s@DatasetContentSummary' {} a -> s {scheduleTime = a} :: DatasetContentSummary) Prelude.. Lens.mapping Prelude._Time

-- | The version of the dataset contents.
datasetContentSummary_version :: Lens.Lens' DatasetContentSummary (Prelude.Maybe Prelude.Text)
datasetContentSummary_version = Lens.lens (\DatasetContentSummary' {version} -> version) (\s@DatasetContentSummary' {} a -> s {version = a} :: DatasetContentSummary)

instance Prelude.FromJSON DatasetContentSummary where
  parseJSON =
    Prelude.withObject
      "DatasetContentSummary"
      ( \x ->
          DatasetContentSummary'
            Prelude.<$> (x Prelude..:? "status")
            Prelude.<*> (x Prelude..:? "creationTime")
            Prelude.<*> (x Prelude..:? "completionTime")
            Prelude.<*> (x Prelude..:? "scheduleTime")
            Prelude.<*> (x Prelude..:? "version")
      )

instance Prelude.Hashable DatasetContentSummary

instance Prelude.NFData DatasetContentSummary
