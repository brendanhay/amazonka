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
  { -- | The status of changeset creation operation.
    status :: Prelude.Maybe ChangesetStatus,
    -- | Type of the data source from which the files to create the changeset are
    -- sourced.
    --
    -- -   @S3@ - Amazon S3.
    sourceType :: Prelude.Maybe SourceType,
    -- | Source path from which the files to create the changeset are sourced.
    sourceParams :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Tags associated with the changeset.
    changesetLabels :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Unique identifier of the changeset that is updated a changeset.
    updatedByChangesetId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the FinSpace dataset in which the changeset is
    -- created.
    datasetId :: Prelude.Maybe Prelude.Text,
    -- | Structure of the source file(s).
    formatParams :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The timestamp at which the changeset was created in FinSpace.
    createTimestamp :: Prelude.Maybe Core.POSIX,
    -- | Unique identifier for a changeset.
    id :: Prelude.Maybe Prelude.Text,
    -- | Format type of the input files loaded into the changeset.
    formatType :: Prelude.Maybe FormatType,
    -- | Unique identifier of the changeset that is updated.
    updatesChangesetId :: Prelude.Maybe Prelude.Text,
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
    -- | The structure with error messages.
    errorInfo :: Prelude.Maybe ErrorInfo,
    -- | The ARN identifier of the changeset.
    changesetArn :: Prelude.Maybe Prelude.Text
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
-- 'status', 'changesetInfo_status' - The status of changeset creation operation.
--
-- 'sourceType', 'changesetInfo_sourceType' - Type of the data source from which the files to create the changeset are
-- sourced.
--
-- -   @S3@ - Amazon S3.
--
-- 'sourceParams', 'changesetInfo_sourceParams' - Source path from which the files to create the changeset are sourced.
--
-- 'changesetLabels', 'changesetInfo_changesetLabels' - Tags associated with the changeset.
--
-- 'updatedByChangesetId', 'changesetInfo_updatedByChangesetId' - Unique identifier of the changeset that is updated a changeset.
--
-- 'datasetId', 'changesetInfo_datasetId' - The unique identifier for the FinSpace dataset in which the changeset is
-- created.
--
-- 'formatParams', 'changesetInfo_formatParams' - Structure of the source file(s).
--
-- 'createTimestamp', 'changesetInfo_createTimestamp' - The timestamp at which the changeset was created in FinSpace.
--
-- 'id', 'changesetInfo_id' - Unique identifier for a changeset.
--
-- 'formatType', 'changesetInfo_formatType' - Format type of the input files loaded into the changeset.
--
-- 'updatesChangesetId', 'changesetInfo_updatesChangesetId' - Unique identifier of the changeset that is updated.
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
-- 'errorInfo', 'changesetInfo_errorInfo' - The structure with error messages.
--
-- 'changesetArn', 'changesetInfo_changesetArn' - The ARN identifier of the changeset.
newChangesetInfo ::
  ChangesetInfo
newChangesetInfo =
  ChangesetInfo'
    { status = Prelude.Nothing,
      sourceType = Prelude.Nothing,
      sourceParams = Prelude.Nothing,
      changesetLabels = Prelude.Nothing,
      updatedByChangesetId = Prelude.Nothing,
      datasetId = Prelude.Nothing,
      formatParams = Prelude.Nothing,
      createTimestamp = Prelude.Nothing,
      id = Prelude.Nothing,
      formatType = Prelude.Nothing,
      updatesChangesetId = Prelude.Nothing,
      changeType = Prelude.Nothing,
      errorInfo = Prelude.Nothing,
      changesetArn = Prelude.Nothing
    }

-- | The status of changeset creation operation.
changesetInfo_status :: Lens.Lens' ChangesetInfo (Prelude.Maybe ChangesetStatus)
changesetInfo_status = Lens.lens (\ChangesetInfo' {status} -> status) (\s@ChangesetInfo' {} a -> s {status = a} :: ChangesetInfo)

-- | Type of the data source from which the files to create the changeset are
-- sourced.
--
-- -   @S3@ - Amazon S3.
changesetInfo_sourceType :: Lens.Lens' ChangesetInfo (Prelude.Maybe SourceType)
changesetInfo_sourceType = Lens.lens (\ChangesetInfo' {sourceType} -> sourceType) (\s@ChangesetInfo' {} a -> s {sourceType = a} :: ChangesetInfo)

-- | Source path from which the files to create the changeset are sourced.
changesetInfo_sourceParams :: Lens.Lens' ChangesetInfo (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
changesetInfo_sourceParams = Lens.lens (\ChangesetInfo' {sourceParams} -> sourceParams) (\s@ChangesetInfo' {} a -> s {sourceParams = a} :: ChangesetInfo) Prelude.. Lens.mapping Lens.coerced

-- | Tags associated with the changeset.
changesetInfo_changesetLabels :: Lens.Lens' ChangesetInfo (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
changesetInfo_changesetLabels = Lens.lens (\ChangesetInfo' {changesetLabels} -> changesetLabels) (\s@ChangesetInfo' {} a -> s {changesetLabels = a} :: ChangesetInfo) Prelude.. Lens.mapping Lens.coerced

-- | Unique identifier of the changeset that is updated a changeset.
changesetInfo_updatedByChangesetId :: Lens.Lens' ChangesetInfo (Prelude.Maybe Prelude.Text)
changesetInfo_updatedByChangesetId = Lens.lens (\ChangesetInfo' {updatedByChangesetId} -> updatedByChangesetId) (\s@ChangesetInfo' {} a -> s {updatedByChangesetId = a} :: ChangesetInfo)

-- | The unique identifier for the FinSpace dataset in which the changeset is
-- created.
changesetInfo_datasetId :: Lens.Lens' ChangesetInfo (Prelude.Maybe Prelude.Text)
changesetInfo_datasetId = Lens.lens (\ChangesetInfo' {datasetId} -> datasetId) (\s@ChangesetInfo' {} a -> s {datasetId = a} :: ChangesetInfo)

-- | Structure of the source file(s).
changesetInfo_formatParams :: Lens.Lens' ChangesetInfo (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
changesetInfo_formatParams = Lens.lens (\ChangesetInfo' {formatParams} -> formatParams) (\s@ChangesetInfo' {} a -> s {formatParams = a} :: ChangesetInfo) Prelude.. Lens.mapping Lens.coerced

-- | The timestamp at which the changeset was created in FinSpace.
changesetInfo_createTimestamp :: Lens.Lens' ChangesetInfo (Prelude.Maybe Prelude.UTCTime)
changesetInfo_createTimestamp = Lens.lens (\ChangesetInfo' {createTimestamp} -> createTimestamp) (\s@ChangesetInfo' {} a -> s {createTimestamp = a} :: ChangesetInfo) Prelude.. Lens.mapping Core._Time

-- | Unique identifier for a changeset.
changesetInfo_id :: Lens.Lens' ChangesetInfo (Prelude.Maybe Prelude.Text)
changesetInfo_id = Lens.lens (\ChangesetInfo' {id} -> id) (\s@ChangesetInfo' {} a -> s {id = a} :: ChangesetInfo)

-- | Format type of the input files loaded into the changeset.
changesetInfo_formatType :: Lens.Lens' ChangesetInfo (Prelude.Maybe FormatType)
changesetInfo_formatType = Lens.lens (\ChangesetInfo' {formatType} -> formatType) (\s@ChangesetInfo' {} a -> s {formatType = a} :: ChangesetInfo)

-- | Unique identifier of the changeset that is updated.
changesetInfo_updatesChangesetId :: Lens.Lens' ChangesetInfo (Prelude.Maybe Prelude.Text)
changesetInfo_updatesChangesetId = Lens.lens (\ChangesetInfo' {updatesChangesetId} -> updatesChangesetId) (\s@ChangesetInfo' {} a -> s {updatesChangesetId = a} :: ChangesetInfo)

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

-- | The structure with error messages.
changesetInfo_errorInfo :: Lens.Lens' ChangesetInfo (Prelude.Maybe ErrorInfo)
changesetInfo_errorInfo = Lens.lens (\ChangesetInfo' {errorInfo} -> errorInfo) (\s@ChangesetInfo' {} a -> s {errorInfo = a} :: ChangesetInfo)

-- | The ARN identifier of the changeset.
changesetInfo_changesetArn :: Lens.Lens' ChangesetInfo (Prelude.Maybe Prelude.Text)
changesetInfo_changesetArn = Lens.lens (\ChangesetInfo' {changesetArn} -> changesetArn) (\s@ChangesetInfo' {} a -> s {changesetArn = a} :: ChangesetInfo)

instance Core.FromJSON ChangesetInfo where
  parseJSON =
    Core.withObject
      "ChangesetInfo"
      ( \x ->
          ChangesetInfo'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "sourceType")
            Prelude.<*> (x Core..:? "sourceParams" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "changesetLabels"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "updatedByChangesetId")
            Prelude.<*> (x Core..:? "datasetId")
            Prelude.<*> (x Core..:? "formatParams" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "createTimestamp")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "formatType")
            Prelude.<*> (x Core..:? "updatesChangesetId")
            Prelude.<*> (x Core..:? "changeType")
            Prelude.<*> (x Core..:? "errorInfo")
            Prelude.<*> (x Core..:? "changesetArn")
      )

instance Prelude.Hashable ChangesetInfo

instance Prelude.NFData ChangesetInfo
