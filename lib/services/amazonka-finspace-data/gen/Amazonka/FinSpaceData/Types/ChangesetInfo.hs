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
-- Module      : Amazonka.FinSpaceData.Types.ChangesetInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpaceData.Types.ChangesetInfo where

import qualified Amazonka.Core as Core
import Amazonka.FinSpaceData.Types.ChangeType
import Amazonka.FinSpaceData.Types.ChangesetStatus
import Amazonka.FinSpaceData.Types.ErrorInfo
import Amazonka.FinSpaceData.Types.FormatType
import Amazonka.FinSpaceData.Types.SourceType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A changeset is unit of data in a dataset.
--
-- /See:/ 'newChangesetInfo' smart constructor.
data ChangesetInfo = ChangesetInfo'
  { -- | Source path from which the files to create the changeset are sourced.
    sourceParams :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Unique identifier of the changeset that is updated a changeset.
    updatedByChangesetId :: Prelude.Maybe Prelude.Text,
    -- | Change type indicates how a changeset is applied to a dataset.
    --
    -- -   @REPLACE@ - Changeset is considered as a replacement to all prior
    --     loaded changesets.
    --
    -- -   @APPEND@ - Changeset is considered as an addition to the end of all
    --     prior loaded changesets.
    --
    -- -   @MODIFY@ - Changeset is considered as a replacement to a specific
    --     prior ingested changeset.
    changeType :: Prelude.Maybe ChangeType,
    -- | The timestamp at which the changeset was created in FinSpace.
    createTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The ARN identifier of the changeset.
    changesetArn :: Prelude.Maybe Prelude.Text,
    -- | Structure of the source file(s).
    formatParams :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Format type of the input files loaded into the changeset.
    formatType :: Prelude.Maybe FormatType,
    -- | The status of changeset creation operation.
    status :: Prelude.Maybe ChangesetStatus,
    -- | Type of the data source from which the files to create the changeset are
    -- sourced.
    --
    -- -   @S3@ - Amazon S3.
    sourceType :: Prelude.Maybe SourceType,
    -- | Unique identifier for a changeset.
    id :: Prelude.Maybe Prelude.Text,
    -- | Tags associated with the changeset.
    changesetLabels :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Unique identifier of the changeset that is updated.
    updatesChangesetId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the FinSpace dataset in which the changeset is
    -- created.
    datasetId :: Prelude.Maybe Prelude.Text,
    -- | The structure with error messages.
    errorInfo :: Prelude.Maybe ErrorInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChangesetInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceParams', 'changesetInfo_sourceParams' - Source path from which the files to create the changeset are sourced.
--
-- 'updatedByChangesetId', 'changesetInfo_updatedByChangesetId' - Unique identifier of the changeset that is updated a changeset.
--
-- 'changeType', 'changesetInfo_changeType' - Change type indicates how a changeset is applied to a dataset.
--
-- -   @REPLACE@ - Changeset is considered as a replacement to all prior
--     loaded changesets.
--
-- -   @APPEND@ - Changeset is considered as an addition to the end of all
--     prior loaded changesets.
--
-- -   @MODIFY@ - Changeset is considered as a replacement to a specific
--     prior ingested changeset.
--
-- 'createTimestamp', 'changesetInfo_createTimestamp' - The timestamp at which the changeset was created in FinSpace.
--
-- 'changesetArn', 'changesetInfo_changesetArn' - The ARN identifier of the changeset.
--
-- 'formatParams', 'changesetInfo_formatParams' - Structure of the source file(s).
--
-- 'formatType', 'changesetInfo_formatType' - Format type of the input files loaded into the changeset.
--
-- 'status', 'changesetInfo_status' - The status of changeset creation operation.
--
-- 'sourceType', 'changesetInfo_sourceType' - Type of the data source from which the files to create the changeset are
-- sourced.
--
-- -   @S3@ - Amazon S3.
--
-- 'id', 'changesetInfo_id' - Unique identifier for a changeset.
--
-- 'changesetLabels', 'changesetInfo_changesetLabels' - Tags associated with the changeset.
--
-- 'updatesChangesetId', 'changesetInfo_updatesChangesetId' - Unique identifier of the changeset that is updated.
--
-- 'datasetId', 'changesetInfo_datasetId' - The unique identifier for the FinSpace dataset in which the changeset is
-- created.
--
-- 'errorInfo', 'changesetInfo_errorInfo' - The structure with error messages.
newChangesetInfo ::
  ChangesetInfo
newChangesetInfo =
  ChangesetInfo'
    { sourceParams = Prelude.Nothing,
      updatedByChangesetId = Prelude.Nothing,
      changeType = Prelude.Nothing,
      createTimestamp = Prelude.Nothing,
      changesetArn = Prelude.Nothing,
      formatParams = Prelude.Nothing,
      formatType = Prelude.Nothing,
      status = Prelude.Nothing,
      sourceType = Prelude.Nothing,
      id = Prelude.Nothing,
      changesetLabels = Prelude.Nothing,
      updatesChangesetId = Prelude.Nothing,
      datasetId = Prelude.Nothing,
      errorInfo = Prelude.Nothing
    }

-- | Source path from which the files to create the changeset are sourced.
changesetInfo_sourceParams :: Lens.Lens' ChangesetInfo (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
changesetInfo_sourceParams = Lens.lens (\ChangesetInfo' {sourceParams} -> sourceParams) (\s@ChangesetInfo' {} a -> s {sourceParams = a} :: ChangesetInfo) Prelude.. Lens.mapping Lens.coerced

-- | Unique identifier of the changeset that is updated a changeset.
changesetInfo_updatedByChangesetId :: Lens.Lens' ChangesetInfo (Prelude.Maybe Prelude.Text)
changesetInfo_updatedByChangesetId = Lens.lens (\ChangesetInfo' {updatedByChangesetId} -> updatedByChangesetId) (\s@ChangesetInfo' {} a -> s {updatedByChangesetId = a} :: ChangesetInfo)

-- | Change type indicates how a changeset is applied to a dataset.
--
-- -   @REPLACE@ - Changeset is considered as a replacement to all prior
--     loaded changesets.
--
-- -   @APPEND@ - Changeset is considered as an addition to the end of all
--     prior loaded changesets.
--
-- -   @MODIFY@ - Changeset is considered as a replacement to a specific
--     prior ingested changeset.
changesetInfo_changeType :: Lens.Lens' ChangesetInfo (Prelude.Maybe ChangeType)
changesetInfo_changeType = Lens.lens (\ChangesetInfo' {changeType} -> changeType) (\s@ChangesetInfo' {} a -> s {changeType = a} :: ChangesetInfo)

-- | The timestamp at which the changeset was created in FinSpace.
changesetInfo_createTimestamp :: Lens.Lens' ChangesetInfo (Prelude.Maybe Prelude.UTCTime)
changesetInfo_createTimestamp = Lens.lens (\ChangesetInfo' {createTimestamp} -> createTimestamp) (\s@ChangesetInfo' {} a -> s {createTimestamp = a} :: ChangesetInfo) Prelude.. Lens.mapping Core._Time

-- | The ARN identifier of the changeset.
changesetInfo_changesetArn :: Lens.Lens' ChangesetInfo (Prelude.Maybe Prelude.Text)
changesetInfo_changesetArn = Lens.lens (\ChangesetInfo' {changesetArn} -> changesetArn) (\s@ChangesetInfo' {} a -> s {changesetArn = a} :: ChangesetInfo)

-- | Structure of the source file(s).
changesetInfo_formatParams :: Lens.Lens' ChangesetInfo (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
changesetInfo_formatParams = Lens.lens (\ChangesetInfo' {formatParams} -> formatParams) (\s@ChangesetInfo' {} a -> s {formatParams = a} :: ChangesetInfo) Prelude.. Lens.mapping Lens.coerced

-- | Format type of the input files loaded into the changeset.
changesetInfo_formatType :: Lens.Lens' ChangesetInfo (Prelude.Maybe FormatType)
changesetInfo_formatType = Lens.lens (\ChangesetInfo' {formatType} -> formatType) (\s@ChangesetInfo' {} a -> s {formatType = a} :: ChangesetInfo)

-- | The status of changeset creation operation.
changesetInfo_status :: Lens.Lens' ChangesetInfo (Prelude.Maybe ChangesetStatus)
changesetInfo_status = Lens.lens (\ChangesetInfo' {status} -> status) (\s@ChangesetInfo' {} a -> s {status = a} :: ChangesetInfo)

-- | Type of the data source from which the files to create the changeset are
-- sourced.
--
-- -   @S3@ - Amazon S3.
changesetInfo_sourceType :: Lens.Lens' ChangesetInfo (Prelude.Maybe SourceType)
changesetInfo_sourceType = Lens.lens (\ChangesetInfo' {sourceType} -> sourceType) (\s@ChangesetInfo' {} a -> s {sourceType = a} :: ChangesetInfo)

-- | Unique identifier for a changeset.
changesetInfo_id :: Lens.Lens' ChangesetInfo (Prelude.Maybe Prelude.Text)
changesetInfo_id = Lens.lens (\ChangesetInfo' {id} -> id) (\s@ChangesetInfo' {} a -> s {id = a} :: ChangesetInfo)

-- | Tags associated with the changeset.
changesetInfo_changesetLabels :: Lens.Lens' ChangesetInfo (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
changesetInfo_changesetLabels = Lens.lens (\ChangesetInfo' {changesetLabels} -> changesetLabels) (\s@ChangesetInfo' {} a -> s {changesetLabels = a} :: ChangesetInfo) Prelude.. Lens.mapping Lens.coerced

-- | Unique identifier of the changeset that is updated.
changesetInfo_updatesChangesetId :: Lens.Lens' ChangesetInfo (Prelude.Maybe Prelude.Text)
changesetInfo_updatesChangesetId = Lens.lens (\ChangesetInfo' {updatesChangesetId} -> updatesChangesetId) (\s@ChangesetInfo' {} a -> s {updatesChangesetId = a} :: ChangesetInfo)

-- | The unique identifier for the FinSpace dataset in which the changeset is
-- created.
changesetInfo_datasetId :: Lens.Lens' ChangesetInfo (Prelude.Maybe Prelude.Text)
changesetInfo_datasetId = Lens.lens (\ChangesetInfo' {datasetId} -> datasetId) (\s@ChangesetInfo' {} a -> s {datasetId = a} :: ChangesetInfo)

-- | The structure with error messages.
changesetInfo_errorInfo :: Lens.Lens' ChangesetInfo (Prelude.Maybe ErrorInfo)
changesetInfo_errorInfo = Lens.lens (\ChangesetInfo' {errorInfo} -> errorInfo) (\s@ChangesetInfo' {} a -> s {errorInfo = a} :: ChangesetInfo)

instance Core.FromJSON ChangesetInfo where
  parseJSON =
    Core.withObject
      "ChangesetInfo"
      ( \x ->
          ChangesetInfo'
            Prelude.<$> (x Core..:? "sourceParams" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "updatedByChangesetId")
            Prelude.<*> (x Core..:? "changeType")
            Prelude.<*> (x Core..:? "createTimestamp")
            Prelude.<*> (x Core..:? "changesetArn")
            Prelude.<*> (x Core..:? "formatParams" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "formatType")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "sourceType")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> ( x Core..:? "changesetLabels"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "updatesChangesetId")
            Prelude.<*> (x Core..:? "datasetId")
            Prelude.<*> (x Core..:? "errorInfo")
      )

instance Prelude.Hashable ChangesetInfo where
  hashWithSalt _salt ChangesetInfo' {..} =
    _salt `Prelude.hashWithSalt` sourceParams
      `Prelude.hashWithSalt` updatedByChangesetId
      `Prelude.hashWithSalt` changeType
      `Prelude.hashWithSalt` createTimestamp
      `Prelude.hashWithSalt` changesetArn
      `Prelude.hashWithSalt` formatParams
      `Prelude.hashWithSalt` formatType
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` sourceType
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` changesetLabels
      `Prelude.hashWithSalt` updatesChangesetId
      `Prelude.hashWithSalt` datasetId
      `Prelude.hashWithSalt` errorInfo

instance Prelude.NFData ChangesetInfo where
  rnf ChangesetInfo' {..} =
    Prelude.rnf sourceParams
      `Prelude.seq` Prelude.rnf updatedByChangesetId
      `Prelude.seq` Prelude.rnf changeType
      `Prelude.seq` Prelude.rnf createTimestamp
      `Prelude.seq` Prelude.rnf changesetArn
      `Prelude.seq` Prelude.rnf formatParams
      `Prelude.seq` Prelude.rnf formatType
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf sourceType
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf changesetLabels
      `Prelude.seq` Prelude.rnf updatesChangesetId
      `Prelude.seq` Prelude.rnf datasetId
      `Prelude.seq` Prelude.rnf errorInfo
