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
-- Module      : Amazonka.Comprehend.Types.BlockReference
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.BlockReference where

import Amazonka.Comprehend.Types.ChildBlock
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A reference to a block.
--
-- /See:/ 'newBlockReference' smart constructor.
data BlockReference = BlockReference'
  { -- | Offset of the start of the block within its parent block.
    beginOffset :: Prelude.Maybe Prelude.Int,
    -- | Unique identifier for the block.
    blockId :: Prelude.Maybe Prelude.Text,
    -- | List of child blocks within this block.
    childBlocks :: Prelude.Maybe [ChildBlock],
    -- | Offset of the end of the block within its parent block.
    endOffset :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BlockReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'beginOffset', 'blockReference_beginOffset' - Offset of the start of the block within its parent block.
--
-- 'blockId', 'blockReference_blockId' - Unique identifier for the block.
--
-- 'childBlocks', 'blockReference_childBlocks' - List of child blocks within this block.
--
-- 'endOffset', 'blockReference_endOffset' - Offset of the end of the block within its parent block.
newBlockReference ::
  BlockReference
newBlockReference =
  BlockReference'
    { beginOffset = Prelude.Nothing,
      blockId = Prelude.Nothing,
      childBlocks = Prelude.Nothing,
      endOffset = Prelude.Nothing
    }

-- | Offset of the start of the block within its parent block.
blockReference_beginOffset :: Lens.Lens' BlockReference (Prelude.Maybe Prelude.Int)
blockReference_beginOffset = Lens.lens (\BlockReference' {beginOffset} -> beginOffset) (\s@BlockReference' {} a -> s {beginOffset = a} :: BlockReference)

-- | Unique identifier for the block.
blockReference_blockId :: Lens.Lens' BlockReference (Prelude.Maybe Prelude.Text)
blockReference_blockId = Lens.lens (\BlockReference' {blockId} -> blockId) (\s@BlockReference' {} a -> s {blockId = a} :: BlockReference)

-- | List of child blocks within this block.
blockReference_childBlocks :: Lens.Lens' BlockReference (Prelude.Maybe [ChildBlock])
blockReference_childBlocks = Lens.lens (\BlockReference' {childBlocks} -> childBlocks) (\s@BlockReference' {} a -> s {childBlocks = a} :: BlockReference) Prelude.. Lens.mapping Lens.coerced

-- | Offset of the end of the block within its parent block.
blockReference_endOffset :: Lens.Lens' BlockReference (Prelude.Maybe Prelude.Int)
blockReference_endOffset = Lens.lens (\BlockReference' {endOffset} -> endOffset) (\s@BlockReference' {} a -> s {endOffset = a} :: BlockReference)

instance Data.FromJSON BlockReference where
  parseJSON =
    Data.withObject
      "BlockReference"
      ( \x ->
          BlockReference'
            Prelude.<$> (x Data..:? "BeginOffset")
            Prelude.<*> (x Data..:? "BlockId")
            Prelude.<*> (x Data..:? "ChildBlocks" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "EndOffset")
      )

instance Prelude.Hashable BlockReference where
  hashWithSalt _salt BlockReference' {..} =
    _salt
      `Prelude.hashWithSalt` beginOffset
      `Prelude.hashWithSalt` blockId
      `Prelude.hashWithSalt` childBlocks
      `Prelude.hashWithSalt` endOffset

instance Prelude.NFData BlockReference where
  rnf BlockReference' {..} =
    Prelude.rnf beginOffset
      `Prelude.seq` Prelude.rnf blockId
      `Prelude.seq` Prelude.rnf childBlocks
      `Prelude.seq` Prelude.rnf endOffset
