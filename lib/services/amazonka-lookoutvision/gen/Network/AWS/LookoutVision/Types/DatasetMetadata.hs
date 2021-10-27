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
-- Module      : Network.AWS.LookoutVision.Types.DatasetMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LookoutVision.Types.DatasetMetadata where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LookoutVision.Types.DatasetStatus
import qualified Network.AWS.Prelude as Prelude

-- | Sumary information for an Amazon Lookout for Vision dataset.
--
-- /See:/ 'newDatasetMetadata' smart constructor.
data DatasetMetadata = DatasetMetadata'
  { -- | The status for the dataset.
    status :: Prelude.Maybe DatasetStatus,
    -- | The status message for the dataset.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The Unix timestamp for the date and time that the dataset was created.
    creationTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The type of the dataset.
    datasetType :: Prelude.Maybe Prelude.Text
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
-- 'status', 'datasetMetadata_status' - The status for the dataset.
--
-- 'statusMessage', 'datasetMetadata_statusMessage' - The status message for the dataset.
--
-- 'creationTimestamp', 'datasetMetadata_creationTimestamp' - The Unix timestamp for the date and time that the dataset was created.
--
-- 'datasetType', 'datasetMetadata_datasetType' - The type of the dataset.
newDatasetMetadata ::
  DatasetMetadata
newDatasetMetadata =
  DatasetMetadata'
    { status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      datasetType = Prelude.Nothing
    }

-- | The status for the dataset.
datasetMetadata_status :: Lens.Lens' DatasetMetadata (Prelude.Maybe DatasetStatus)
datasetMetadata_status = Lens.lens (\DatasetMetadata' {status} -> status) (\s@DatasetMetadata' {} a -> s {status = a} :: DatasetMetadata)

-- | The status message for the dataset.
datasetMetadata_statusMessage :: Lens.Lens' DatasetMetadata (Prelude.Maybe Prelude.Text)
datasetMetadata_statusMessage = Lens.lens (\DatasetMetadata' {statusMessage} -> statusMessage) (\s@DatasetMetadata' {} a -> s {statusMessage = a} :: DatasetMetadata)

-- | The Unix timestamp for the date and time that the dataset was created.
datasetMetadata_creationTimestamp :: Lens.Lens' DatasetMetadata (Prelude.Maybe Prelude.UTCTime)
datasetMetadata_creationTimestamp = Lens.lens (\DatasetMetadata' {creationTimestamp} -> creationTimestamp) (\s@DatasetMetadata' {} a -> s {creationTimestamp = a} :: DatasetMetadata) Prelude.. Lens.mapping Core._Time

-- | The type of the dataset.
datasetMetadata_datasetType :: Lens.Lens' DatasetMetadata (Prelude.Maybe Prelude.Text)
datasetMetadata_datasetType = Lens.lens (\DatasetMetadata' {datasetType} -> datasetType) (\s@DatasetMetadata' {} a -> s {datasetType = a} :: DatasetMetadata)

instance Core.FromJSON DatasetMetadata where
  parseJSON =
    Core.withObject
      "DatasetMetadata"
      ( \x ->
          DatasetMetadata'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "StatusMessage")
            Prelude.<*> (x Core..:? "CreationTimestamp")
            Prelude.<*> (x Core..:? "DatasetType")
      )

instance Prelude.Hashable DatasetMetadata

instance Prelude.NFData DatasetMetadata
