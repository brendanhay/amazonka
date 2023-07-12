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
-- Module      : Amazonka.Omics.Types.StartReferenceImportJobSourceItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.StartReferenceImportJobSourceItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A source for a reference import job.
--
-- /See:/ 'newStartReferenceImportJobSourceItem' smart constructor.
data StartReferenceImportJobSourceItem = StartReferenceImportJobSourceItem'
  { -- | The source\'s description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The source\'s tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The source\'s name.
    name :: Prelude.Text,
    -- | The source file\'s location in Amazon S3.
    sourceFile :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartReferenceImportJobSourceItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'startReferenceImportJobSourceItem_description' - The source\'s description.
--
-- 'tags', 'startReferenceImportJobSourceItem_tags' - The source\'s tags.
--
-- 'name', 'startReferenceImportJobSourceItem_name' - The source\'s name.
--
-- 'sourceFile', 'startReferenceImportJobSourceItem_sourceFile' - The source file\'s location in Amazon S3.
newStartReferenceImportJobSourceItem ::
  -- | 'name'
  Prelude.Text ->
  -- | 'sourceFile'
  Prelude.Text ->
  StartReferenceImportJobSourceItem
newStartReferenceImportJobSourceItem
  pName_
  pSourceFile_ =
    StartReferenceImportJobSourceItem'
      { description =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        name = pName_,
        sourceFile = pSourceFile_
      }

-- | The source\'s description.
startReferenceImportJobSourceItem_description :: Lens.Lens' StartReferenceImportJobSourceItem (Prelude.Maybe Prelude.Text)
startReferenceImportJobSourceItem_description = Lens.lens (\StartReferenceImportJobSourceItem' {description} -> description) (\s@StartReferenceImportJobSourceItem' {} a -> s {description = a} :: StartReferenceImportJobSourceItem)

-- | The source\'s tags.
startReferenceImportJobSourceItem_tags :: Lens.Lens' StartReferenceImportJobSourceItem (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
startReferenceImportJobSourceItem_tags = Lens.lens (\StartReferenceImportJobSourceItem' {tags} -> tags) (\s@StartReferenceImportJobSourceItem' {} a -> s {tags = a} :: StartReferenceImportJobSourceItem) Prelude.. Lens.mapping Lens.coerced

-- | The source\'s name.
startReferenceImportJobSourceItem_name :: Lens.Lens' StartReferenceImportJobSourceItem Prelude.Text
startReferenceImportJobSourceItem_name = Lens.lens (\StartReferenceImportJobSourceItem' {name} -> name) (\s@StartReferenceImportJobSourceItem' {} a -> s {name = a} :: StartReferenceImportJobSourceItem)

-- | The source file\'s location in Amazon S3.
startReferenceImportJobSourceItem_sourceFile :: Lens.Lens' StartReferenceImportJobSourceItem Prelude.Text
startReferenceImportJobSourceItem_sourceFile = Lens.lens (\StartReferenceImportJobSourceItem' {sourceFile} -> sourceFile) (\s@StartReferenceImportJobSourceItem' {} a -> s {sourceFile = a} :: StartReferenceImportJobSourceItem)

instance
  Prelude.Hashable
    StartReferenceImportJobSourceItem
  where
  hashWithSalt
    _salt
    StartReferenceImportJobSourceItem' {..} =
      _salt
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` sourceFile

instance
  Prelude.NFData
    StartReferenceImportJobSourceItem
  where
  rnf StartReferenceImportJobSourceItem' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf sourceFile

instance
  Data.ToJSON
    StartReferenceImportJobSourceItem
  where
  toJSON StartReferenceImportJobSourceItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("sourceFile" Data..= sourceFile)
          ]
      )
