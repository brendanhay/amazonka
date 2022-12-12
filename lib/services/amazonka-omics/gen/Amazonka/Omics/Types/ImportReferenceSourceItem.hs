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
-- Module      : Amazonka.Omics.Types.ImportReferenceSourceItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ImportReferenceSourceItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.ReferenceImportJobItemStatus
import qualified Amazonka.Prelude as Prelude

-- | An genome reference source.
--
-- /See:/ 'newImportReferenceSourceItem' smart constructor.
data ImportReferenceSourceItem = ImportReferenceSourceItem'
  { -- | The source\'s description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The source\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The source file\'s location in Amazon S3.
    sourceFile :: Prelude.Maybe Prelude.Text,
    -- | The source\'s status message.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The source\'s tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The source\'s status.
    status :: ReferenceImportJobItemStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportReferenceSourceItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'importReferenceSourceItem_description' - The source\'s description.
--
-- 'name', 'importReferenceSourceItem_name' - The source\'s name.
--
-- 'sourceFile', 'importReferenceSourceItem_sourceFile' - The source file\'s location in Amazon S3.
--
-- 'statusMessage', 'importReferenceSourceItem_statusMessage' - The source\'s status message.
--
-- 'tags', 'importReferenceSourceItem_tags' - The source\'s tags.
--
-- 'status', 'importReferenceSourceItem_status' - The source\'s status.
newImportReferenceSourceItem ::
  -- | 'status'
  ReferenceImportJobItemStatus ->
  ImportReferenceSourceItem
newImportReferenceSourceItem pStatus_ =
  ImportReferenceSourceItem'
    { description =
        Prelude.Nothing,
      name = Prelude.Nothing,
      sourceFile = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      tags = Prelude.Nothing,
      status = pStatus_
    }

-- | The source\'s description.
importReferenceSourceItem_description :: Lens.Lens' ImportReferenceSourceItem (Prelude.Maybe Prelude.Text)
importReferenceSourceItem_description = Lens.lens (\ImportReferenceSourceItem' {description} -> description) (\s@ImportReferenceSourceItem' {} a -> s {description = a} :: ImportReferenceSourceItem)

-- | The source\'s name.
importReferenceSourceItem_name :: Lens.Lens' ImportReferenceSourceItem (Prelude.Maybe Prelude.Text)
importReferenceSourceItem_name = Lens.lens (\ImportReferenceSourceItem' {name} -> name) (\s@ImportReferenceSourceItem' {} a -> s {name = a} :: ImportReferenceSourceItem)

-- | The source file\'s location in Amazon S3.
importReferenceSourceItem_sourceFile :: Lens.Lens' ImportReferenceSourceItem (Prelude.Maybe Prelude.Text)
importReferenceSourceItem_sourceFile = Lens.lens (\ImportReferenceSourceItem' {sourceFile} -> sourceFile) (\s@ImportReferenceSourceItem' {} a -> s {sourceFile = a} :: ImportReferenceSourceItem)

-- | The source\'s status message.
importReferenceSourceItem_statusMessage :: Lens.Lens' ImportReferenceSourceItem (Prelude.Maybe Prelude.Text)
importReferenceSourceItem_statusMessage = Lens.lens (\ImportReferenceSourceItem' {statusMessage} -> statusMessage) (\s@ImportReferenceSourceItem' {} a -> s {statusMessage = a} :: ImportReferenceSourceItem)

-- | The source\'s tags.
importReferenceSourceItem_tags :: Lens.Lens' ImportReferenceSourceItem (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
importReferenceSourceItem_tags = Lens.lens (\ImportReferenceSourceItem' {tags} -> tags) (\s@ImportReferenceSourceItem' {} a -> s {tags = a} :: ImportReferenceSourceItem) Prelude.. Lens.mapping Lens.coerced

-- | The source\'s status.
importReferenceSourceItem_status :: Lens.Lens' ImportReferenceSourceItem ReferenceImportJobItemStatus
importReferenceSourceItem_status = Lens.lens (\ImportReferenceSourceItem' {status} -> status) (\s@ImportReferenceSourceItem' {} a -> s {status = a} :: ImportReferenceSourceItem)

instance Data.FromJSON ImportReferenceSourceItem where
  parseJSON =
    Data.withObject
      "ImportReferenceSourceItem"
      ( \x ->
          ImportReferenceSourceItem'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "sourceFile")
            Prelude.<*> (x Data..:? "statusMessage")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "status")
      )

instance Prelude.Hashable ImportReferenceSourceItem where
  hashWithSalt _salt ImportReferenceSourceItem' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` sourceFile
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` status

instance Prelude.NFData ImportReferenceSourceItem where
  rnf ImportReferenceSourceItem' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf sourceFile
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf status
