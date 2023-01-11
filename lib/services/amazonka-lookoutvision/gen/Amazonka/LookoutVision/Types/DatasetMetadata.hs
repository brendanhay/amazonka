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
-- Module      : Amazonka.LookoutVision.Types.DatasetMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutVision.Types.DatasetMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutVision.Types.DatasetStatus
import qualified Amazonka.Prelude as Prelude

-- | Summary information for an Amazon Lookout for Vision dataset. For more
-- information, see DescribeDataset and ProjectDescription.
--
-- /See:/ 'newDatasetMetadata' smart constructor.
data DatasetMetadata = DatasetMetadata'
  { -- | The Unix timestamp for the date and time that the dataset was created.
    creationTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The type of the dataset.
    datasetType :: Prelude.Maybe Prelude.Text,
    -- | The status for the dataset.
    status :: Prelude.Maybe DatasetStatus,
    -- | The status message for the dataset.
    statusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'datasetMetadata_creationTimestamp' - The Unix timestamp for the date and time that the dataset was created.
--
-- 'datasetType', 'datasetMetadata_datasetType' - The type of the dataset.
--
-- 'status', 'datasetMetadata_status' - The status for the dataset.
--
-- 'statusMessage', 'datasetMetadata_statusMessage' - The status message for the dataset.
newDatasetMetadata ::
  DatasetMetadata
newDatasetMetadata =
  DatasetMetadata'
    { creationTimestamp =
        Prelude.Nothing,
      datasetType = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | The Unix timestamp for the date and time that the dataset was created.
datasetMetadata_creationTimestamp :: Lens.Lens' DatasetMetadata (Prelude.Maybe Prelude.UTCTime)
datasetMetadata_creationTimestamp = Lens.lens (\DatasetMetadata' {creationTimestamp} -> creationTimestamp) (\s@DatasetMetadata' {} a -> s {creationTimestamp = a} :: DatasetMetadata) Prelude.. Lens.mapping Data._Time

-- | The type of the dataset.
datasetMetadata_datasetType :: Lens.Lens' DatasetMetadata (Prelude.Maybe Prelude.Text)
datasetMetadata_datasetType = Lens.lens (\DatasetMetadata' {datasetType} -> datasetType) (\s@DatasetMetadata' {} a -> s {datasetType = a} :: DatasetMetadata)

-- | The status for the dataset.
datasetMetadata_status :: Lens.Lens' DatasetMetadata (Prelude.Maybe DatasetStatus)
datasetMetadata_status = Lens.lens (\DatasetMetadata' {status} -> status) (\s@DatasetMetadata' {} a -> s {status = a} :: DatasetMetadata)

-- | The status message for the dataset.
datasetMetadata_statusMessage :: Lens.Lens' DatasetMetadata (Prelude.Maybe Prelude.Text)
datasetMetadata_statusMessage = Lens.lens (\DatasetMetadata' {statusMessage} -> statusMessage) (\s@DatasetMetadata' {} a -> s {statusMessage = a} :: DatasetMetadata)

instance Data.FromJSON DatasetMetadata where
  parseJSON =
    Data.withObject
      "DatasetMetadata"
      ( \x ->
          DatasetMetadata'
            Prelude.<$> (x Data..:? "CreationTimestamp")
            Prelude.<*> (x Data..:? "DatasetType")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusMessage")
      )

instance Prelude.Hashable DatasetMetadata where
  hashWithSalt _salt DatasetMetadata' {..} =
    _salt `Prelude.hashWithSalt` creationTimestamp
      `Prelude.hashWithSalt` datasetType
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData DatasetMetadata where
  rnf DatasetMetadata' {..} =
    Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf datasetType
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
