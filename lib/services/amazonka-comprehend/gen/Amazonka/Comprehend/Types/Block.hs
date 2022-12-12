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
-- Module      : Amazonka.Comprehend.Types.Block
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.Block where

import Amazonka.Comprehend.Types.BlockType
import Amazonka.Comprehend.Types.Geometry
import Amazonka.Comprehend.Types.RelationshipsListItem
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about each word or line of text in the input document.
--
-- For additional information, see
-- <https://docs.aws.amazon.com/textract/latest/dg/API_Block.html Block> in
-- the Amazon Textract API reference.
--
-- /See:/ 'newBlock' smart constructor.
data Block = Block'
  { -- | The block represents a line of text or one word of text.
    --
    -- -   WORD - A word that\'s detected on a document page. A word is one or
    --     more ISO basic Latin script characters that aren\'t separated by
    --     spaces.
    --
    -- -   LINE - A string of tab-delimited, contiguous words that are detected
    --     on a document page
    blockType :: Prelude.Maybe BlockType,
    -- | Co-ordinates of the rectangle or polygon that contains the text.
    geometry :: Prelude.Maybe Geometry,
    -- | Unique identifier for the block.
    id :: Prelude.Maybe Prelude.Text,
    -- | Page number where the block appears.
    page :: Prelude.Maybe Prelude.Int,
    -- | A list of child blocks of the current block. For example, a LINE object
    -- has child blocks for each WORD block that\'s part of the line of text.
    relationships :: Prelude.Maybe [RelationshipsListItem],
    -- | The word or line of text extracted from the block.
    text :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Block' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blockType', 'block_blockType' - The block represents a line of text or one word of text.
--
-- -   WORD - A word that\'s detected on a document page. A word is one or
--     more ISO basic Latin script characters that aren\'t separated by
--     spaces.
--
-- -   LINE - A string of tab-delimited, contiguous words that are detected
--     on a document page
--
-- 'geometry', 'block_geometry' - Co-ordinates of the rectangle or polygon that contains the text.
--
-- 'id', 'block_id' - Unique identifier for the block.
--
-- 'page', 'block_page' - Page number where the block appears.
--
-- 'relationships', 'block_relationships' - A list of child blocks of the current block. For example, a LINE object
-- has child blocks for each WORD block that\'s part of the line of text.
--
-- 'text', 'block_text' - The word or line of text extracted from the block.
newBlock ::
  Block
newBlock =
  Block'
    { blockType = Prelude.Nothing,
      geometry = Prelude.Nothing,
      id = Prelude.Nothing,
      page = Prelude.Nothing,
      relationships = Prelude.Nothing,
      text = Prelude.Nothing
    }

-- | The block represents a line of text or one word of text.
--
-- -   WORD - A word that\'s detected on a document page. A word is one or
--     more ISO basic Latin script characters that aren\'t separated by
--     spaces.
--
-- -   LINE - A string of tab-delimited, contiguous words that are detected
--     on a document page
block_blockType :: Lens.Lens' Block (Prelude.Maybe BlockType)
block_blockType = Lens.lens (\Block' {blockType} -> blockType) (\s@Block' {} a -> s {blockType = a} :: Block)

-- | Co-ordinates of the rectangle or polygon that contains the text.
block_geometry :: Lens.Lens' Block (Prelude.Maybe Geometry)
block_geometry = Lens.lens (\Block' {geometry} -> geometry) (\s@Block' {} a -> s {geometry = a} :: Block)

-- | Unique identifier for the block.
block_id :: Lens.Lens' Block (Prelude.Maybe Prelude.Text)
block_id = Lens.lens (\Block' {id} -> id) (\s@Block' {} a -> s {id = a} :: Block)

-- | Page number where the block appears.
block_page :: Lens.Lens' Block (Prelude.Maybe Prelude.Int)
block_page = Lens.lens (\Block' {page} -> page) (\s@Block' {} a -> s {page = a} :: Block)

-- | A list of child blocks of the current block. For example, a LINE object
-- has child blocks for each WORD block that\'s part of the line of text.
block_relationships :: Lens.Lens' Block (Prelude.Maybe [RelationshipsListItem])
block_relationships = Lens.lens (\Block' {relationships} -> relationships) (\s@Block' {} a -> s {relationships = a} :: Block) Prelude.. Lens.mapping Lens.coerced

-- | The word or line of text extracted from the block.
block_text :: Lens.Lens' Block (Prelude.Maybe Prelude.Text)
block_text = Lens.lens (\Block' {text} -> text) (\s@Block' {} a -> s {text = a} :: Block)

instance Data.FromJSON Block where
  parseJSON =
    Data.withObject
      "Block"
      ( \x ->
          Block'
            Prelude.<$> (x Data..:? "BlockType")
            Prelude.<*> (x Data..:? "Geometry")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Page")
            Prelude.<*> (x Data..:? "Relationships" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Text")
      )

instance Prelude.Hashable Block where
  hashWithSalt _salt Block' {..} =
    _salt `Prelude.hashWithSalt` blockType
      `Prelude.hashWithSalt` geometry
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` page
      `Prelude.hashWithSalt` relationships
      `Prelude.hashWithSalt` text

instance Prelude.NFData Block where
  rnf Block' {..} =
    Prelude.rnf blockType
      `Prelude.seq` Prelude.rnf geometry
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf page
      `Prelude.seq` Prelude.rnf relationships
      `Prelude.seq` Prelude.rnf text
