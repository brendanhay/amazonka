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
-- Module      : Amazonka.Omics.Types.ImportReadSetSourceItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ImportReadSetSourceItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.FileType
import Amazonka.Omics.Types.ReadSetImportJobItemStatus
import Amazonka.Omics.Types.SourceFiles
import qualified Amazonka.Prelude as Prelude

-- | A source for an import read set job.
--
-- /See:/ 'newImportReadSetSourceItem' smart constructor.
data ImportReadSetSourceItem = ImportReadSetSourceItem'
  { -- | The source\'s description.
    description :: Prelude.Maybe Prelude.Text,
    -- | Where the source originated.
    generatedFrom :: Prelude.Maybe Prelude.Text,
    -- | The source\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The source\'s genome reference ARN.
    referenceArn :: Prelude.Maybe Prelude.Text,
    -- | The source\'s status message.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The source\'s tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The source\'s sample ID.
    sampleId :: Prelude.Text,
    -- | The source\'s file type.
    sourceFileType :: FileType,
    -- | The source files\' location in Amazon S3.
    sourceFiles :: SourceFiles,
    -- | The source\'s status.
    status :: ReadSetImportJobItemStatus,
    -- | The source\'s subject ID.
    subjectId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportReadSetSourceItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'importReadSetSourceItem_description' - The source\'s description.
--
-- 'generatedFrom', 'importReadSetSourceItem_generatedFrom' - Where the source originated.
--
-- 'name', 'importReadSetSourceItem_name' - The source\'s name.
--
-- 'referenceArn', 'importReadSetSourceItem_referenceArn' - The source\'s genome reference ARN.
--
-- 'statusMessage', 'importReadSetSourceItem_statusMessage' - The source\'s status message.
--
-- 'tags', 'importReadSetSourceItem_tags' - The source\'s tags.
--
-- 'sampleId', 'importReadSetSourceItem_sampleId' - The source\'s sample ID.
--
-- 'sourceFileType', 'importReadSetSourceItem_sourceFileType' - The source\'s file type.
--
-- 'sourceFiles', 'importReadSetSourceItem_sourceFiles' - The source files\' location in Amazon S3.
--
-- 'status', 'importReadSetSourceItem_status' - The source\'s status.
--
-- 'subjectId', 'importReadSetSourceItem_subjectId' - The source\'s subject ID.
newImportReadSetSourceItem ::
  -- | 'sampleId'
  Prelude.Text ->
  -- | 'sourceFileType'
  FileType ->
  -- | 'sourceFiles'
  SourceFiles ->
  -- | 'status'
  ReadSetImportJobItemStatus ->
  -- | 'subjectId'
  Prelude.Text ->
  ImportReadSetSourceItem
newImportReadSetSourceItem
  pSampleId_
  pSourceFileType_
  pSourceFiles_
  pStatus_
  pSubjectId_ =
    ImportReadSetSourceItem'
      { description =
          Prelude.Nothing,
        generatedFrom = Prelude.Nothing,
        name = Prelude.Nothing,
        referenceArn = Prelude.Nothing,
        statusMessage = Prelude.Nothing,
        tags = Prelude.Nothing,
        sampleId = pSampleId_,
        sourceFileType = pSourceFileType_,
        sourceFiles = pSourceFiles_,
        status = pStatus_,
        subjectId = pSubjectId_
      }

-- | The source\'s description.
importReadSetSourceItem_description :: Lens.Lens' ImportReadSetSourceItem (Prelude.Maybe Prelude.Text)
importReadSetSourceItem_description = Lens.lens (\ImportReadSetSourceItem' {description} -> description) (\s@ImportReadSetSourceItem' {} a -> s {description = a} :: ImportReadSetSourceItem)

-- | Where the source originated.
importReadSetSourceItem_generatedFrom :: Lens.Lens' ImportReadSetSourceItem (Prelude.Maybe Prelude.Text)
importReadSetSourceItem_generatedFrom = Lens.lens (\ImportReadSetSourceItem' {generatedFrom} -> generatedFrom) (\s@ImportReadSetSourceItem' {} a -> s {generatedFrom = a} :: ImportReadSetSourceItem)

-- | The source\'s name.
importReadSetSourceItem_name :: Lens.Lens' ImportReadSetSourceItem (Prelude.Maybe Prelude.Text)
importReadSetSourceItem_name = Lens.lens (\ImportReadSetSourceItem' {name} -> name) (\s@ImportReadSetSourceItem' {} a -> s {name = a} :: ImportReadSetSourceItem)

-- | The source\'s genome reference ARN.
importReadSetSourceItem_referenceArn :: Lens.Lens' ImportReadSetSourceItem (Prelude.Maybe Prelude.Text)
importReadSetSourceItem_referenceArn = Lens.lens (\ImportReadSetSourceItem' {referenceArn} -> referenceArn) (\s@ImportReadSetSourceItem' {} a -> s {referenceArn = a} :: ImportReadSetSourceItem)

-- | The source\'s status message.
importReadSetSourceItem_statusMessage :: Lens.Lens' ImportReadSetSourceItem (Prelude.Maybe Prelude.Text)
importReadSetSourceItem_statusMessage = Lens.lens (\ImportReadSetSourceItem' {statusMessage} -> statusMessage) (\s@ImportReadSetSourceItem' {} a -> s {statusMessage = a} :: ImportReadSetSourceItem)

-- | The source\'s tags.
importReadSetSourceItem_tags :: Lens.Lens' ImportReadSetSourceItem (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
importReadSetSourceItem_tags = Lens.lens (\ImportReadSetSourceItem' {tags} -> tags) (\s@ImportReadSetSourceItem' {} a -> s {tags = a} :: ImportReadSetSourceItem) Prelude.. Lens.mapping Lens.coerced

-- | The source\'s sample ID.
importReadSetSourceItem_sampleId :: Lens.Lens' ImportReadSetSourceItem Prelude.Text
importReadSetSourceItem_sampleId = Lens.lens (\ImportReadSetSourceItem' {sampleId} -> sampleId) (\s@ImportReadSetSourceItem' {} a -> s {sampleId = a} :: ImportReadSetSourceItem)

-- | The source\'s file type.
importReadSetSourceItem_sourceFileType :: Lens.Lens' ImportReadSetSourceItem FileType
importReadSetSourceItem_sourceFileType = Lens.lens (\ImportReadSetSourceItem' {sourceFileType} -> sourceFileType) (\s@ImportReadSetSourceItem' {} a -> s {sourceFileType = a} :: ImportReadSetSourceItem)

-- | The source files\' location in Amazon S3.
importReadSetSourceItem_sourceFiles :: Lens.Lens' ImportReadSetSourceItem SourceFiles
importReadSetSourceItem_sourceFiles = Lens.lens (\ImportReadSetSourceItem' {sourceFiles} -> sourceFiles) (\s@ImportReadSetSourceItem' {} a -> s {sourceFiles = a} :: ImportReadSetSourceItem)

-- | The source\'s status.
importReadSetSourceItem_status :: Lens.Lens' ImportReadSetSourceItem ReadSetImportJobItemStatus
importReadSetSourceItem_status = Lens.lens (\ImportReadSetSourceItem' {status} -> status) (\s@ImportReadSetSourceItem' {} a -> s {status = a} :: ImportReadSetSourceItem)

-- | The source\'s subject ID.
importReadSetSourceItem_subjectId :: Lens.Lens' ImportReadSetSourceItem Prelude.Text
importReadSetSourceItem_subjectId = Lens.lens (\ImportReadSetSourceItem' {subjectId} -> subjectId) (\s@ImportReadSetSourceItem' {} a -> s {subjectId = a} :: ImportReadSetSourceItem)

instance Data.FromJSON ImportReadSetSourceItem where
  parseJSON =
    Data.withObject
      "ImportReadSetSourceItem"
      ( \x ->
          ImportReadSetSourceItem'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "generatedFrom")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "referenceArn")
            Prelude.<*> (x Data..:? "statusMessage")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "sampleId")
            Prelude.<*> (x Data..: "sourceFileType")
            Prelude.<*> (x Data..: "sourceFiles")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "subjectId")
      )

instance Prelude.Hashable ImportReadSetSourceItem where
  hashWithSalt _salt ImportReadSetSourceItem' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` generatedFrom
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` referenceArn
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` sampleId
      `Prelude.hashWithSalt` sourceFileType
      `Prelude.hashWithSalt` sourceFiles
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` subjectId

instance Prelude.NFData ImportReadSetSourceItem where
  rnf ImportReadSetSourceItem' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf generatedFrom
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf referenceArn
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf sampleId
      `Prelude.seq` Prelude.rnf sourceFileType
      `Prelude.seq` Prelude.rnf sourceFiles
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf subjectId
