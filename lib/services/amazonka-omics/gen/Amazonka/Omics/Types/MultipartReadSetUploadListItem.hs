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
-- Module      : Amazonka.Omics.Types.MultipartReadSetUploadListItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.MultipartReadSetUploadListItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.FileType
import qualified Amazonka.Prelude as Prelude

-- | Part of the response to ListMultipartReadSetUploads, excluding completed
-- and aborted multipart uploads.
--
-- /See:/ 'newMultipartReadSetUploadListItem' smart constructor.
data MultipartReadSetUploadListItem = MultipartReadSetUploadListItem'
  { -- | The description of a read set.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of a read set.
    name :: Prelude.Maybe Prelude.Text,
    -- | Any tags you wish to add to a read set.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The sequence store ID used for the multipart upload.
    sequenceStoreId :: Prelude.Text,
    -- | The ID for the initiated multipart upload.
    uploadId :: Prelude.Text,
    -- | The type of file the read set originated from.
    sourceFileType :: FileType,
    -- | The read set source\'s subject ID.
    subjectId :: Prelude.Text,
    -- | The read set source\'s sample ID.
    sampleId :: Prelude.Text,
    -- | The source of an uploaded part.
    generatedFrom :: Prelude.Text,
    -- | The source\'s reference ARN.
    referenceArn :: Prelude.Text,
    -- | The time stamp for when a direct upload was created.
    creationTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MultipartReadSetUploadListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'multipartReadSetUploadListItem_description' - The description of a read set.
--
-- 'name', 'multipartReadSetUploadListItem_name' - The name of a read set.
--
-- 'tags', 'multipartReadSetUploadListItem_tags' - Any tags you wish to add to a read set.
--
-- 'sequenceStoreId', 'multipartReadSetUploadListItem_sequenceStoreId' - The sequence store ID used for the multipart upload.
--
-- 'uploadId', 'multipartReadSetUploadListItem_uploadId' - The ID for the initiated multipart upload.
--
-- 'sourceFileType', 'multipartReadSetUploadListItem_sourceFileType' - The type of file the read set originated from.
--
-- 'subjectId', 'multipartReadSetUploadListItem_subjectId' - The read set source\'s subject ID.
--
-- 'sampleId', 'multipartReadSetUploadListItem_sampleId' - The read set source\'s sample ID.
--
-- 'generatedFrom', 'multipartReadSetUploadListItem_generatedFrom' - The source of an uploaded part.
--
-- 'referenceArn', 'multipartReadSetUploadListItem_referenceArn' - The source\'s reference ARN.
--
-- 'creationTime', 'multipartReadSetUploadListItem_creationTime' - The time stamp for when a direct upload was created.
newMultipartReadSetUploadListItem ::
  -- | 'sequenceStoreId'
  Prelude.Text ->
  -- | 'uploadId'
  Prelude.Text ->
  -- | 'sourceFileType'
  FileType ->
  -- | 'subjectId'
  Prelude.Text ->
  -- | 'sampleId'
  Prelude.Text ->
  -- | 'generatedFrom'
  Prelude.Text ->
  -- | 'referenceArn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  MultipartReadSetUploadListItem
newMultipartReadSetUploadListItem
  pSequenceStoreId_
  pUploadId_
  pSourceFileType_
  pSubjectId_
  pSampleId_
  pGeneratedFrom_
  pReferenceArn_
  pCreationTime_ =
    MultipartReadSetUploadListItem'
      { description =
          Prelude.Nothing,
        name = Prelude.Nothing,
        tags = Prelude.Nothing,
        sequenceStoreId = pSequenceStoreId_,
        uploadId = pUploadId_,
        sourceFileType = pSourceFileType_,
        subjectId = pSubjectId_,
        sampleId = pSampleId_,
        generatedFrom = pGeneratedFrom_,
        referenceArn = pReferenceArn_,
        creationTime =
          Data._Time Lens.# pCreationTime_
      }

-- | The description of a read set.
multipartReadSetUploadListItem_description :: Lens.Lens' MultipartReadSetUploadListItem (Prelude.Maybe Prelude.Text)
multipartReadSetUploadListItem_description = Lens.lens (\MultipartReadSetUploadListItem' {description} -> description) (\s@MultipartReadSetUploadListItem' {} a -> s {description = a} :: MultipartReadSetUploadListItem)

-- | The name of a read set.
multipartReadSetUploadListItem_name :: Lens.Lens' MultipartReadSetUploadListItem (Prelude.Maybe Prelude.Text)
multipartReadSetUploadListItem_name = Lens.lens (\MultipartReadSetUploadListItem' {name} -> name) (\s@MultipartReadSetUploadListItem' {} a -> s {name = a} :: MultipartReadSetUploadListItem)

-- | Any tags you wish to add to a read set.
multipartReadSetUploadListItem_tags :: Lens.Lens' MultipartReadSetUploadListItem (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
multipartReadSetUploadListItem_tags = Lens.lens (\MultipartReadSetUploadListItem' {tags} -> tags) (\s@MultipartReadSetUploadListItem' {} a -> s {tags = a} :: MultipartReadSetUploadListItem) Prelude.. Lens.mapping Lens.coerced

-- | The sequence store ID used for the multipart upload.
multipartReadSetUploadListItem_sequenceStoreId :: Lens.Lens' MultipartReadSetUploadListItem Prelude.Text
multipartReadSetUploadListItem_sequenceStoreId = Lens.lens (\MultipartReadSetUploadListItem' {sequenceStoreId} -> sequenceStoreId) (\s@MultipartReadSetUploadListItem' {} a -> s {sequenceStoreId = a} :: MultipartReadSetUploadListItem)

-- | The ID for the initiated multipart upload.
multipartReadSetUploadListItem_uploadId :: Lens.Lens' MultipartReadSetUploadListItem Prelude.Text
multipartReadSetUploadListItem_uploadId = Lens.lens (\MultipartReadSetUploadListItem' {uploadId} -> uploadId) (\s@MultipartReadSetUploadListItem' {} a -> s {uploadId = a} :: MultipartReadSetUploadListItem)

-- | The type of file the read set originated from.
multipartReadSetUploadListItem_sourceFileType :: Lens.Lens' MultipartReadSetUploadListItem FileType
multipartReadSetUploadListItem_sourceFileType = Lens.lens (\MultipartReadSetUploadListItem' {sourceFileType} -> sourceFileType) (\s@MultipartReadSetUploadListItem' {} a -> s {sourceFileType = a} :: MultipartReadSetUploadListItem)

-- | The read set source\'s subject ID.
multipartReadSetUploadListItem_subjectId :: Lens.Lens' MultipartReadSetUploadListItem Prelude.Text
multipartReadSetUploadListItem_subjectId = Lens.lens (\MultipartReadSetUploadListItem' {subjectId} -> subjectId) (\s@MultipartReadSetUploadListItem' {} a -> s {subjectId = a} :: MultipartReadSetUploadListItem)

-- | The read set source\'s sample ID.
multipartReadSetUploadListItem_sampleId :: Lens.Lens' MultipartReadSetUploadListItem Prelude.Text
multipartReadSetUploadListItem_sampleId = Lens.lens (\MultipartReadSetUploadListItem' {sampleId} -> sampleId) (\s@MultipartReadSetUploadListItem' {} a -> s {sampleId = a} :: MultipartReadSetUploadListItem)

-- | The source of an uploaded part.
multipartReadSetUploadListItem_generatedFrom :: Lens.Lens' MultipartReadSetUploadListItem Prelude.Text
multipartReadSetUploadListItem_generatedFrom = Lens.lens (\MultipartReadSetUploadListItem' {generatedFrom} -> generatedFrom) (\s@MultipartReadSetUploadListItem' {} a -> s {generatedFrom = a} :: MultipartReadSetUploadListItem)

-- | The source\'s reference ARN.
multipartReadSetUploadListItem_referenceArn :: Lens.Lens' MultipartReadSetUploadListItem Prelude.Text
multipartReadSetUploadListItem_referenceArn = Lens.lens (\MultipartReadSetUploadListItem' {referenceArn} -> referenceArn) (\s@MultipartReadSetUploadListItem' {} a -> s {referenceArn = a} :: MultipartReadSetUploadListItem)

-- | The time stamp for when a direct upload was created.
multipartReadSetUploadListItem_creationTime :: Lens.Lens' MultipartReadSetUploadListItem Prelude.UTCTime
multipartReadSetUploadListItem_creationTime = Lens.lens (\MultipartReadSetUploadListItem' {creationTime} -> creationTime) (\s@MultipartReadSetUploadListItem' {} a -> s {creationTime = a} :: MultipartReadSetUploadListItem) Prelude.. Data._Time

instance Data.FromJSON MultipartReadSetUploadListItem where
  parseJSON =
    Data.withObject
      "MultipartReadSetUploadListItem"
      ( \x ->
          MultipartReadSetUploadListItem'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "sequenceStoreId")
            Prelude.<*> (x Data..: "uploadId")
            Prelude.<*> (x Data..: "sourceFileType")
            Prelude.<*> (x Data..: "subjectId")
            Prelude.<*> (x Data..: "sampleId")
            Prelude.<*> (x Data..: "generatedFrom")
            Prelude.<*> (x Data..: "referenceArn")
            Prelude.<*> (x Data..: "creationTime")
      )

instance
  Prelude.Hashable
    MultipartReadSetUploadListItem
  where
  hashWithSalt
    _salt
    MultipartReadSetUploadListItem' {..} =
      _salt
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` sequenceStoreId
        `Prelude.hashWithSalt` uploadId
        `Prelude.hashWithSalt` sourceFileType
        `Prelude.hashWithSalt` subjectId
        `Prelude.hashWithSalt` sampleId
        `Prelude.hashWithSalt` generatedFrom
        `Prelude.hashWithSalt` referenceArn
        `Prelude.hashWithSalt` creationTime

instance
  Prelude.NFData
    MultipartReadSetUploadListItem
  where
  rnf MultipartReadSetUploadListItem' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf sequenceStoreId
      `Prelude.seq` Prelude.rnf uploadId
      `Prelude.seq` Prelude.rnf sourceFileType
      `Prelude.seq` Prelude.rnf subjectId
      `Prelude.seq` Prelude.rnf sampleId
      `Prelude.seq` Prelude.rnf generatedFrom
      `Prelude.seq` Prelude.rnf referenceArn
      `Prelude.seq` Prelude.rnf creationTime
