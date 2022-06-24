{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.FinSpaceData.CreateChangeset
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new changeset in a FinSpace dataset.
module Amazonka.FinSpaceData.CreateChangeset
  ( -- * Creating a Request
    CreateChangeset (..),
    newCreateChangeset,

    -- * Request Lenses
    createChangeset_tags,
    createChangeset_formatParams,
    createChangeset_formatType,
    createChangeset_datasetId,
    createChangeset_changeType,
    createChangeset_sourceType,
    createChangeset_sourceParams,

    -- * Destructuring the Response
    CreateChangesetResponse (..),
    newCreateChangesetResponse,

    -- * Response Lenses
    createChangesetResponse_changeset,
    createChangesetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.FinSpaceData.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateChangeset' smart constructor.
data CreateChangeset = CreateChangeset'
  { -- | Metadata tags to apply to this changeset.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Options that define the structure of the source file(s).
    formatParams :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Format type of the input files being loaded into the changeset.
    formatType :: Prelude.Maybe FormatType,
    -- | The unique identifier for the FinSpace dataset in which the changeset
    -- will be created.
    datasetId :: Prelude.Text,
    -- | Option to indicate how a changeset will be applied to a dataset.
    --
    -- -   @REPLACE@ - Changeset will be considered as a replacement to all
    --     prior loaded changesets.
    --
    -- -   @APPEND@ - Changeset will be considered as an addition to the end of
    --     all prior loaded changesets.
    changeType :: ChangeType,
    -- | Type of the data source from which the files to create the changeset
    -- will be sourced.
    --
    -- -   @S3@ - Amazon S3.
    sourceType :: SourceType,
    -- | Source path from which the files to create the changeset will be
    -- sourced.
    sourceParams :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateChangeset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createChangeset_tags' - Metadata tags to apply to this changeset.
--
-- 'formatParams', 'createChangeset_formatParams' - Options that define the structure of the source file(s).
--
-- 'formatType', 'createChangeset_formatType' - Format type of the input files being loaded into the changeset.
--
-- 'datasetId', 'createChangeset_datasetId' - The unique identifier for the FinSpace dataset in which the changeset
-- will be created.
--
-- 'changeType', 'createChangeset_changeType' - Option to indicate how a changeset will be applied to a dataset.
--
-- -   @REPLACE@ - Changeset will be considered as a replacement to all
--     prior loaded changesets.
--
-- -   @APPEND@ - Changeset will be considered as an addition to the end of
--     all prior loaded changesets.
--
-- 'sourceType', 'createChangeset_sourceType' - Type of the data source from which the files to create the changeset
-- will be sourced.
--
-- -   @S3@ - Amazon S3.
--
-- 'sourceParams', 'createChangeset_sourceParams' - Source path from which the files to create the changeset will be
-- sourced.
newCreateChangeset ::
  -- | 'datasetId'
  Prelude.Text ->
  -- | 'changeType'
  ChangeType ->
  -- | 'sourceType'
  SourceType ->
  CreateChangeset
newCreateChangeset
  pDatasetId_
  pChangeType_
  pSourceType_ =
    CreateChangeset'
      { tags = Prelude.Nothing,
        formatParams = Prelude.Nothing,
        formatType = Prelude.Nothing,
        datasetId = pDatasetId_,
        changeType = pChangeType_,
        sourceType = pSourceType_,
        sourceParams = Prelude.mempty
      }

-- | Metadata tags to apply to this changeset.
createChangeset_tags :: Lens.Lens' CreateChangeset (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createChangeset_tags = Lens.lens (\CreateChangeset' {tags} -> tags) (\s@CreateChangeset' {} a -> s {tags = a} :: CreateChangeset) Prelude.. Lens.mapping Lens.coerced

-- | Options that define the structure of the source file(s).
createChangeset_formatParams :: Lens.Lens' CreateChangeset (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createChangeset_formatParams = Lens.lens (\CreateChangeset' {formatParams} -> formatParams) (\s@CreateChangeset' {} a -> s {formatParams = a} :: CreateChangeset) Prelude.. Lens.mapping Lens.coerced

-- | Format type of the input files being loaded into the changeset.
createChangeset_formatType :: Lens.Lens' CreateChangeset (Prelude.Maybe FormatType)
createChangeset_formatType = Lens.lens (\CreateChangeset' {formatType} -> formatType) (\s@CreateChangeset' {} a -> s {formatType = a} :: CreateChangeset)

-- | The unique identifier for the FinSpace dataset in which the changeset
-- will be created.
createChangeset_datasetId :: Lens.Lens' CreateChangeset Prelude.Text
createChangeset_datasetId = Lens.lens (\CreateChangeset' {datasetId} -> datasetId) (\s@CreateChangeset' {} a -> s {datasetId = a} :: CreateChangeset)

-- | Option to indicate how a changeset will be applied to a dataset.
--
-- -   @REPLACE@ - Changeset will be considered as a replacement to all
--     prior loaded changesets.
--
-- -   @APPEND@ - Changeset will be considered as an addition to the end of
--     all prior loaded changesets.
createChangeset_changeType :: Lens.Lens' CreateChangeset ChangeType
createChangeset_changeType = Lens.lens (\CreateChangeset' {changeType} -> changeType) (\s@CreateChangeset' {} a -> s {changeType = a} :: CreateChangeset)

-- | Type of the data source from which the files to create the changeset
-- will be sourced.
--
-- -   @S3@ - Amazon S3.
createChangeset_sourceType :: Lens.Lens' CreateChangeset SourceType
createChangeset_sourceType = Lens.lens (\CreateChangeset' {sourceType} -> sourceType) (\s@CreateChangeset' {} a -> s {sourceType = a} :: CreateChangeset)

-- | Source path from which the files to create the changeset will be
-- sourced.
createChangeset_sourceParams :: Lens.Lens' CreateChangeset (Prelude.HashMap Prelude.Text Prelude.Text)
createChangeset_sourceParams = Lens.lens (\CreateChangeset' {sourceParams} -> sourceParams) (\s@CreateChangeset' {} a -> s {sourceParams = a} :: CreateChangeset) Prelude.. Lens.coerced

instance Core.AWSRequest CreateChangeset where
  type
    AWSResponse CreateChangeset =
      CreateChangesetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateChangesetResponse'
            Prelude.<$> (x Core..?> "changeset")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateChangeset where
  hashWithSalt _salt CreateChangeset' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` formatParams
      `Prelude.hashWithSalt` formatType
      `Prelude.hashWithSalt` datasetId
      `Prelude.hashWithSalt` changeType
      `Prelude.hashWithSalt` sourceType
      `Prelude.hashWithSalt` sourceParams

instance Prelude.NFData CreateChangeset where
  rnf CreateChangeset' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf formatParams
      `Prelude.seq` Prelude.rnf formatType
      `Prelude.seq` Prelude.rnf datasetId
      `Prelude.seq` Prelude.rnf changeType
      `Prelude.seq` Prelude.rnf sourceType
      `Prelude.seq` Prelude.rnf sourceParams

instance Core.ToHeaders CreateChangeset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateChangeset where
  toJSON CreateChangeset' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("formatParams" Core..=) Prelude.<$> formatParams,
            ("formatType" Core..=) Prelude.<$> formatType,
            Prelude.Just ("changeType" Core..= changeType),
            Prelude.Just ("sourceType" Core..= sourceType),
            Prelude.Just ("sourceParams" Core..= sourceParams)
          ]
      )

instance Core.ToPath CreateChangeset where
  toPath CreateChangeset' {..} =
    Prelude.mconcat
      ["/datasets/", Core.toBS datasetId, "/changesets"]

instance Core.ToQuery CreateChangeset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateChangesetResponse' smart constructor.
data CreateChangesetResponse = CreateChangesetResponse'
  { -- | Returns the changeset details.
    changeset :: Prelude.Maybe ChangesetInfo,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateChangesetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeset', 'createChangesetResponse_changeset' - Returns the changeset details.
--
-- 'httpStatus', 'createChangesetResponse_httpStatus' - The response's http status code.
newCreateChangesetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateChangesetResponse
newCreateChangesetResponse pHttpStatus_ =
  CreateChangesetResponse'
    { changeset =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns the changeset details.
createChangesetResponse_changeset :: Lens.Lens' CreateChangesetResponse (Prelude.Maybe ChangesetInfo)
createChangesetResponse_changeset = Lens.lens (\CreateChangesetResponse' {changeset} -> changeset) (\s@CreateChangesetResponse' {} a -> s {changeset = a} :: CreateChangesetResponse)

-- | The response's http status code.
createChangesetResponse_httpStatus :: Lens.Lens' CreateChangesetResponse Prelude.Int
createChangesetResponse_httpStatus = Lens.lens (\CreateChangesetResponse' {httpStatus} -> httpStatus) (\s@CreateChangesetResponse' {} a -> s {httpStatus = a} :: CreateChangesetResponse)

instance Prelude.NFData CreateChangesetResponse where
  rnf CreateChangesetResponse' {..} =
    Prelude.rnf changeset
      `Prelude.seq` Prelude.rnf httpStatus
