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
-- Module      : Amazonka.Rekognition.Types.DatasetMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.DatasetMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.DatasetStatus
import Amazonka.Rekognition.Types.DatasetStatusMessageCode
import Amazonka.Rekognition.Types.DatasetType

-- | Summary information for an Amazon Rekognition Custom Labels dataset. For
-- more information, see ProjectDescription.
--
-- /See:/ 'newDatasetMetadata' smart constructor.
data DatasetMetadata = DatasetMetadata'
  { -- | The status message code for the dataset operation. If a service error
    -- occurs, try the API call again later. If a client error occurs, check
    -- the input parameters to the dataset API call that failed.
    statusMessageCode :: Prelude.Maybe DatasetStatusMessageCode,
    -- | The type of the dataset.
    datasetType :: Prelude.Maybe DatasetType,
    -- | The status for the dataset.
    status :: Prelude.Maybe DatasetStatus,
    -- | The Unix timestamp for the date and time that the dataset was created.
    creationTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) for the dataset.
    datasetArn :: Prelude.Maybe Prelude.Text,
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
-- 'statusMessageCode', 'datasetMetadata_statusMessageCode' - The status message code for the dataset operation. If a service error
-- occurs, try the API call again later. If a client error occurs, check
-- the input parameters to the dataset API call that failed.
--
-- 'datasetType', 'datasetMetadata_datasetType' - The type of the dataset.
--
-- 'status', 'datasetMetadata_status' - The status for the dataset.
--
-- 'creationTimestamp', 'datasetMetadata_creationTimestamp' - The Unix timestamp for the date and time that the dataset was created.
--
-- 'datasetArn', 'datasetMetadata_datasetArn' - The Amazon Resource Name (ARN) for the dataset.
--
-- 'statusMessage', 'datasetMetadata_statusMessage' - The status message for the dataset.
newDatasetMetadata ::
  DatasetMetadata
newDatasetMetadata =
  DatasetMetadata'
    { statusMessageCode =
        Prelude.Nothing,
      datasetType = Prelude.Nothing,
      status = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      datasetArn = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | The status message code for the dataset operation. If a service error
-- occurs, try the API call again later. If a client error occurs, check
-- the input parameters to the dataset API call that failed.
datasetMetadata_statusMessageCode :: Lens.Lens' DatasetMetadata (Prelude.Maybe DatasetStatusMessageCode)
datasetMetadata_statusMessageCode = Lens.lens (\DatasetMetadata' {statusMessageCode} -> statusMessageCode) (\s@DatasetMetadata' {} a -> s {statusMessageCode = a} :: DatasetMetadata)

-- | The type of the dataset.
datasetMetadata_datasetType :: Lens.Lens' DatasetMetadata (Prelude.Maybe DatasetType)
datasetMetadata_datasetType = Lens.lens (\DatasetMetadata' {datasetType} -> datasetType) (\s@DatasetMetadata' {} a -> s {datasetType = a} :: DatasetMetadata)

-- | The status for the dataset.
datasetMetadata_status :: Lens.Lens' DatasetMetadata (Prelude.Maybe DatasetStatus)
datasetMetadata_status = Lens.lens (\DatasetMetadata' {status} -> status) (\s@DatasetMetadata' {} a -> s {status = a} :: DatasetMetadata)

-- | The Unix timestamp for the date and time that the dataset was created.
datasetMetadata_creationTimestamp :: Lens.Lens' DatasetMetadata (Prelude.Maybe Prelude.UTCTime)
datasetMetadata_creationTimestamp = Lens.lens (\DatasetMetadata' {creationTimestamp} -> creationTimestamp) (\s@DatasetMetadata' {} a -> s {creationTimestamp = a} :: DatasetMetadata) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) for the dataset.
datasetMetadata_datasetArn :: Lens.Lens' DatasetMetadata (Prelude.Maybe Prelude.Text)
datasetMetadata_datasetArn = Lens.lens (\DatasetMetadata' {datasetArn} -> datasetArn) (\s@DatasetMetadata' {} a -> s {datasetArn = a} :: DatasetMetadata)

-- | The status message for the dataset.
datasetMetadata_statusMessage :: Lens.Lens' DatasetMetadata (Prelude.Maybe Prelude.Text)
datasetMetadata_statusMessage = Lens.lens (\DatasetMetadata' {statusMessage} -> statusMessage) (\s@DatasetMetadata' {} a -> s {statusMessage = a} :: DatasetMetadata)

instance Core.FromJSON DatasetMetadata where
  parseJSON =
    Core.withObject
      "DatasetMetadata"
      ( \x ->
          DatasetMetadata'
            Prelude.<$> (x Core..:? "StatusMessageCode")
            Prelude.<*> (x Core..:? "DatasetType")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "CreationTimestamp")
            Prelude.<*> (x Core..:? "DatasetArn")
            Prelude.<*> (x Core..:? "StatusMessage")
      )

instance Prelude.Hashable DatasetMetadata where
  hashWithSalt _salt DatasetMetadata' {..} =
    _salt `Prelude.hashWithSalt` statusMessageCode
      `Prelude.hashWithSalt` datasetType
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` creationTimestamp
      `Prelude.hashWithSalt` datasetArn
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData DatasetMetadata where
  rnf DatasetMetadata' {..} =
    Prelude.rnf statusMessageCode
      `Prelude.seq` Prelude.rnf datasetType
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf datasetArn
      `Prelude.seq` Prelude.rnf statusMessage
