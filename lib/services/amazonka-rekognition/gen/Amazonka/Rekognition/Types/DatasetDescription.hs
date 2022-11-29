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
-- Module      : Amazonka.Rekognition.Types.DatasetDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.DatasetDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.DatasetStats
import Amazonka.Rekognition.Types.DatasetStatus
import Amazonka.Rekognition.Types.DatasetStatusMessageCode

-- | A description for a dataset. For more information, see DescribeDataset.
--
-- The status fields @Status@, @StatusMessage@, and @StatusMessageCode@
-- reflect the last operation on the dataset.
--
-- /See:/ 'newDatasetDescription' smart constructor.
data DatasetDescription = DatasetDescription'
  { -- | The status message code for the dataset operation. If a service error
    -- occurs, try the API call again later. If a client error occurs, check
    -- the input parameters to the dataset API call that failed.
    statusMessageCode :: Prelude.Maybe DatasetStatusMessageCode,
    -- | The Unix timestamp for the date and time that the dataset was last
    -- updated.
    lastUpdatedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The status of the dataset.
    status :: Prelude.Maybe DatasetStatus,
    -- | The status message code for the dataset.
    datasetStats :: Prelude.Maybe DatasetStats,
    -- | The Unix timestamp for the time and date that the dataset was created.
    creationTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The status message for the dataset.
    statusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessageCode', 'datasetDescription_statusMessageCode' - The status message code for the dataset operation. If a service error
-- occurs, try the API call again later. If a client error occurs, check
-- the input parameters to the dataset API call that failed.
--
-- 'lastUpdatedTimestamp', 'datasetDescription_lastUpdatedTimestamp' - The Unix timestamp for the date and time that the dataset was last
-- updated.
--
-- 'status', 'datasetDescription_status' - The status of the dataset.
--
-- 'datasetStats', 'datasetDescription_datasetStats' - The status message code for the dataset.
--
-- 'creationTimestamp', 'datasetDescription_creationTimestamp' - The Unix timestamp for the time and date that the dataset was created.
--
-- 'statusMessage', 'datasetDescription_statusMessage' - The status message for the dataset.
newDatasetDescription ::
  DatasetDescription
newDatasetDescription =
  DatasetDescription'
    { statusMessageCode =
        Prelude.Nothing,
      lastUpdatedTimestamp = Prelude.Nothing,
      status = Prelude.Nothing,
      datasetStats = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | The status message code for the dataset operation. If a service error
-- occurs, try the API call again later. If a client error occurs, check
-- the input parameters to the dataset API call that failed.
datasetDescription_statusMessageCode :: Lens.Lens' DatasetDescription (Prelude.Maybe DatasetStatusMessageCode)
datasetDescription_statusMessageCode = Lens.lens (\DatasetDescription' {statusMessageCode} -> statusMessageCode) (\s@DatasetDescription' {} a -> s {statusMessageCode = a} :: DatasetDescription)

-- | The Unix timestamp for the date and time that the dataset was last
-- updated.
datasetDescription_lastUpdatedTimestamp :: Lens.Lens' DatasetDescription (Prelude.Maybe Prelude.UTCTime)
datasetDescription_lastUpdatedTimestamp = Lens.lens (\DatasetDescription' {lastUpdatedTimestamp} -> lastUpdatedTimestamp) (\s@DatasetDescription' {} a -> s {lastUpdatedTimestamp = a} :: DatasetDescription) Prelude.. Lens.mapping Core._Time

-- | The status of the dataset.
datasetDescription_status :: Lens.Lens' DatasetDescription (Prelude.Maybe DatasetStatus)
datasetDescription_status = Lens.lens (\DatasetDescription' {status} -> status) (\s@DatasetDescription' {} a -> s {status = a} :: DatasetDescription)

-- | The status message code for the dataset.
datasetDescription_datasetStats :: Lens.Lens' DatasetDescription (Prelude.Maybe DatasetStats)
datasetDescription_datasetStats = Lens.lens (\DatasetDescription' {datasetStats} -> datasetStats) (\s@DatasetDescription' {} a -> s {datasetStats = a} :: DatasetDescription)

-- | The Unix timestamp for the time and date that the dataset was created.
datasetDescription_creationTimestamp :: Lens.Lens' DatasetDescription (Prelude.Maybe Prelude.UTCTime)
datasetDescription_creationTimestamp = Lens.lens (\DatasetDescription' {creationTimestamp} -> creationTimestamp) (\s@DatasetDescription' {} a -> s {creationTimestamp = a} :: DatasetDescription) Prelude.. Lens.mapping Core._Time

-- | The status message for the dataset.
datasetDescription_statusMessage :: Lens.Lens' DatasetDescription (Prelude.Maybe Prelude.Text)
datasetDescription_statusMessage = Lens.lens (\DatasetDescription' {statusMessage} -> statusMessage) (\s@DatasetDescription' {} a -> s {statusMessage = a} :: DatasetDescription)

instance Core.FromJSON DatasetDescription where
  parseJSON =
    Core.withObject
      "DatasetDescription"
      ( \x ->
          DatasetDescription'
            Prelude.<$> (x Core..:? "StatusMessageCode")
            Prelude.<*> (x Core..:? "LastUpdatedTimestamp")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "DatasetStats")
            Prelude.<*> (x Core..:? "CreationTimestamp")
            Prelude.<*> (x Core..:? "StatusMessage")
      )

instance Prelude.Hashable DatasetDescription where
  hashWithSalt _salt DatasetDescription' {..} =
    _salt `Prelude.hashWithSalt` statusMessageCode
      `Prelude.hashWithSalt` lastUpdatedTimestamp
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` datasetStats
      `Prelude.hashWithSalt` creationTimestamp
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData DatasetDescription where
  rnf DatasetDescription' {..} =
    Prelude.rnf statusMessageCode
      `Prelude.seq` Prelude.rnf lastUpdatedTimestamp
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf datasetStats
      `Prelude.seq` Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf statusMessage
