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
-- Module      : Amazonka.Comprehend.Types.ChildBlock
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.ChildBlock where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Nested block contained within a block.
--
-- /See:/ 'newChildBlock' smart constructor.
data ChildBlock = ChildBlock'
  { -- | Offset of the start of the child block within its parent block.
    beginOffset :: Prelude.Maybe Prelude.Int,
    -- | Unique identifier for the child block.
    childBlockId :: Prelude.Maybe Prelude.Text,
    -- | Offset of the end of the child block within its parent block.
    endOffset :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChildBlock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'beginOffset', 'childBlock_beginOffset' - Offset of the start of the child block within its parent block.
--
-- 'childBlockId', 'childBlock_childBlockId' - Unique identifier for the child block.
--
-- 'endOffset', 'childBlock_endOffset' - Offset of the end of the child block within its parent block.
newChildBlock ::
  ChildBlock
newChildBlock =
  ChildBlock'
    { beginOffset = Prelude.Nothing,
      childBlockId = Prelude.Nothing,
      endOffset = Prelude.Nothing
    }

-- | Offset of the start of the child block within its parent block.
childBlock_beginOffset :: Lens.Lens' ChildBlock (Prelude.Maybe Prelude.Int)
childBlock_beginOffset = Lens.lens (\ChildBlock' {beginOffset} -> beginOffset) (\s@ChildBlock' {} a -> s {beginOffset = a} :: ChildBlock)

-- | Unique identifier for the child block.
childBlock_childBlockId :: Lens.Lens' ChildBlock (Prelude.Maybe Prelude.Text)
childBlock_childBlockId = Lens.lens (\ChildBlock' {childBlockId} -> childBlockId) (\s@ChildBlock' {} a -> s {childBlockId = a} :: ChildBlock)

-- | Offset of the end of the child block within its parent block.
childBlock_endOffset :: Lens.Lens' ChildBlock (Prelude.Maybe Prelude.Int)
childBlock_endOffset = Lens.lens (\ChildBlock' {endOffset} -> endOffset) (\s@ChildBlock' {} a -> s {endOffset = a} :: ChildBlock)

instance Data.FromJSON ChildBlock where
  parseJSON =
    Data.withObject
      "ChildBlock"
      ( \x ->
          ChildBlock'
            Prelude.<$> (x Data..:? "BeginOffset")
            Prelude.<*> (x Data..:? "ChildBlockId")
            Prelude.<*> (x Data..:? "EndOffset")
      )

instance Prelude.Hashable ChildBlock where
  hashWithSalt _salt ChildBlock' {..} =
    _salt `Prelude.hashWithSalt` beginOffset
      `Prelude.hashWithSalt` childBlockId
      `Prelude.hashWithSalt` endOffset

instance Prelude.NFData ChildBlock where
  rnf ChildBlock' {..} =
    Prelude.rnf beginOffset
      `Prelude.seq` Prelude.rnf childBlockId
      `Prelude.seq` Prelude.rnf endOffset
