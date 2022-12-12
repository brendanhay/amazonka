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
-- Module      : Amazonka.Omics.Types.StartReadSetImportJobSourceItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.StartReadSetImportJobSourceItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.FileType
import Amazonka.Omics.Types.SourceFiles
import qualified Amazonka.Prelude as Prelude

-- | A source for a read set import job.
--
-- /See:/ 'newStartReadSetImportJobSourceItem' smart constructor.
data StartReadSetImportJobSourceItem = StartReadSetImportJobSourceItem'
  { -- | The source\'s description.
    description :: Prelude.Maybe Prelude.Text,
    -- | Where the source originated.
    generatedFrom :: Prelude.Maybe Prelude.Text,
    -- | The source\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The source\'s tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The source\'s reference ARN.
    referenceArn :: Prelude.Text,
    -- | The source\'s sample ID.
    sampleId :: Prelude.Text,
    -- | The source\'s file type.
    sourceFileType :: FileType,
    -- | The source files\' location in Amazon S3.
    sourceFiles :: SourceFiles,
    -- | The source\'s subject ID.
    subjectId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartReadSetImportJobSourceItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'startReadSetImportJobSourceItem_description' - The source\'s description.
--
-- 'generatedFrom', 'startReadSetImportJobSourceItem_generatedFrom' - Where the source originated.
--
-- 'name', 'startReadSetImportJobSourceItem_name' - The source\'s name.
--
-- 'tags', 'startReadSetImportJobSourceItem_tags' - The source\'s tags.
--
-- 'referenceArn', 'startReadSetImportJobSourceItem_referenceArn' - The source\'s reference ARN.
--
-- 'sampleId', 'startReadSetImportJobSourceItem_sampleId' - The source\'s sample ID.
--
-- 'sourceFileType', 'startReadSetImportJobSourceItem_sourceFileType' - The source\'s file type.
--
-- 'sourceFiles', 'startReadSetImportJobSourceItem_sourceFiles' - The source files\' location in Amazon S3.
--
-- 'subjectId', 'startReadSetImportJobSourceItem_subjectId' - The source\'s subject ID.
newStartReadSetImportJobSourceItem ::
  -- | 'referenceArn'
  Prelude.Text ->
  -- | 'sampleId'
  Prelude.Text ->
  -- | 'sourceFileType'
  FileType ->
  -- | 'sourceFiles'
  SourceFiles ->
  -- | 'subjectId'
  Prelude.Text ->
  StartReadSetImportJobSourceItem
newStartReadSetImportJobSourceItem
  pReferenceArn_
  pSampleId_
  pSourceFileType_
  pSourceFiles_
  pSubjectId_ =
    StartReadSetImportJobSourceItem'
      { description =
          Prelude.Nothing,
        generatedFrom = Prelude.Nothing,
        name = Prelude.Nothing,
        tags = Prelude.Nothing,
        referenceArn = pReferenceArn_,
        sampleId = pSampleId_,
        sourceFileType = pSourceFileType_,
        sourceFiles = pSourceFiles_,
        subjectId = pSubjectId_
      }

-- | The source\'s description.
startReadSetImportJobSourceItem_description :: Lens.Lens' StartReadSetImportJobSourceItem (Prelude.Maybe Prelude.Text)
startReadSetImportJobSourceItem_description = Lens.lens (\StartReadSetImportJobSourceItem' {description} -> description) (\s@StartReadSetImportJobSourceItem' {} a -> s {description = a} :: StartReadSetImportJobSourceItem)

-- | Where the source originated.
startReadSetImportJobSourceItem_generatedFrom :: Lens.Lens' StartReadSetImportJobSourceItem (Prelude.Maybe Prelude.Text)
startReadSetImportJobSourceItem_generatedFrom = Lens.lens (\StartReadSetImportJobSourceItem' {generatedFrom} -> generatedFrom) (\s@StartReadSetImportJobSourceItem' {} a -> s {generatedFrom = a} :: StartReadSetImportJobSourceItem)

-- | The source\'s name.
startReadSetImportJobSourceItem_name :: Lens.Lens' StartReadSetImportJobSourceItem (Prelude.Maybe Prelude.Text)
startReadSetImportJobSourceItem_name = Lens.lens (\StartReadSetImportJobSourceItem' {name} -> name) (\s@StartReadSetImportJobSourceItem' {} a -> s {name = a} :: StartReadSetImportJobSourceItem)

-- | The source\'s tags.
startReadSetImportJobSourceItem_tags :: Lens.Lens' StartReadSetImportJobSourceItem (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startReadSetImportJobSourceItem_tags = Lens.lens (\StartReadSetImportJobSourceItem' {tags} -> tags) (\s@StartReadSetImportJobSourceItem' {} a -> s {tags = a} :: StartReadSetImportJobSourceItem) Prelude.. Lens.mapping Lens.coerced

-- | The source\'s reference ARN.
startReadSetImportJobSourceItem_referenceArn :: Lens.Lens' StartReadSetImportJobSourceItem Prelude.Text
startReadSetImportJobSourceItem_referenceArn = Lens.lens (\StartReadSetImportJobSourceItem' {referenceArn} -> referenceArn) (\s@StartReadSetImportJobSourceItem' {} a -> s {referenceArn = a} :: StartReadSetImportJobSourceItem)

-- | The source\'s sample ID.
startReadSetImportJobSourceItem_sampleId :: Lens.Lens' StartReadSetImportJobSourceItem Prelude.Text
startReadSetImportJobSourceItem_sampleId = Lens.lens (\StartReadSetImportJobSourceItem' {sampleId} -> sampleId) (\s@StartReadSetImportJobSourceItem' {} a -> s {sampleId = a} :: StartReadSetImportJobSourceItem)

-- | The source\'s file type.
startReadSetImportJobSourceItem_sourceFileType :: Lens.Lens' StartReadSetImportJobSourceItem FileType
startReadSetImportJobSourceItem_sourceFileType = Lens.lens (\StartReadSetImportJobSourceItem' {sourceFileType} -> sourceFileType) (\s@StartReadSetImportJobSourceItem' {} a -> s {sourceFileType = a} :: StartReadSetImportJobSourceItem)

-- | The source files\' location in Amazon S3.
startReadSetImportJobSourceItem_sourceFiles :: Lens.Lens' StartReadSetImportJobSourceItem SourceFiles
startReadSetImportJobSourceItem_sourceFiles = Lens.lens (\StartReadSetImportJobSourceItem' {sourceFiles} -> sourceFiles) (\s@StartReadSetImportJobSourceItem' {} a -> s {sourceFiles = a} :: StartReadSetImportJobSourceItem)

-- | The source\'s subject ID.
startReadSetImportJobSourceItem_subjectId :: Lens.Lens' StartReadSetImportJobSourceItem Prelude.Text
startReadSetImportJobSourceItem_subjectId = Lens.lens (\StartReadSetImportJobSourceItem' {subjectId} -> subjectId) (\s@StartReadSetImportJobSourceItem' {} a -> s {subjectId = a} :: StartReadSetImportJobSourceItem)

instance
  Prelude.Hashable
    StartReadSetImportJobSourceItem
  where
  hashWithSalt
    _salt
    StartReadSetImportJobSourceItem' {..} =
      _salt `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` generatedFrom
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` referenceArn
        `Prelude.hashWithSalt` sampleId
        `Prelude.hashWithSalt` sourceFileType
        `Prelude.hashWithSalt` sourceFiles
        `Prelude.hashWithSalt` subjectId

instance
  Prelude.NFData
    StartReadSetImportJobSourceItem
  where
  rnf StartReadSetImportJobSourceItem' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf generatedFrom
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf referenceArn
      `Prelude.seq` Prelude.rnf sampleId
      `Prelude.seq` Prelude.rnf sourceFileType
      `Prelude.seq` Prelude.rnf sourceFiles
      `Prelude.seq` Prelude.rnf subjectId

instance Data.ToJSON StartReadSetImportJobSourceItem where
  toJSON StartReadSetImportJobSourceItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("generatedFrom" Data..=) Prelude.<$> generatedFrom,
            ("name" Data..=) Prelude.<$> name,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("referenceArn" Data..= referenceArn),
            Prelude.Just ("sampleId" Data..= sampleId),
            Prelude.Just
              ("sourceFileType" Data..= sourceFileType),
            Prelude.Just ("sourceFiles" Data..= sourceFiles),
            Prelude.Just ("subjectId" Data..= subjectId)
          ]
      )
