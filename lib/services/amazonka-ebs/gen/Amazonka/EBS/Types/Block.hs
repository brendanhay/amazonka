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
-- Module      : Amazonka.EBS.Types.Block
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EBS.Types.Block where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A block of data in an Amazon Elastic Block Store snapshot.
--
-- /See:/ 'newBlock' smart constructor.
data Block = Block'
  { -- | The block index.
    blockIndex :: Prelude.Maybe Prelude.Natural,
    -- | The block token for the block index.
    blockToken :: Prelude.Maybe Prelude.Text
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
-- 'blockIndex', 'block_blockIndex' - The block index.
--
-- 'blockToken', 'block_blockToken' - The block token for the block index.
newBlock ::
  Block
newBlock =
  Block'
    { blockIndex = Prelude.Nothing,
      blockToken = Prelude.Nothing
    }

-- | The block index.
block_blockIndex :: Lens.Lens' Block (Prelude.Maybe Prelude.Natural)
block_blockIndex = Lens.lens (\Block' {blockIndex} -> blockIndex) (\s@Block' {} a -> s {blockIndex = a} :: Block)

-- | The block token for the block index.
block_blockToken :: Lens.Lens' Block (Prelude.Maybe Prelude.Text)
block_blockToken = Lens.lens (\Block' {blockToken} -> blockToken) (\s@Block' {} a -> s {blockToken = a} :: Block)

instance Data.FromJSON Block where
  parseJSON =
    Data.withObject
      "Block"
      ( \x ->
          Block'
            Prelude.<$> (x Data..:? "BlockIndex")
            Prelude.<*> (x Data..:? "BlockToken")
      )

instance Prelude.Hashable Block where
  hashWithSalt _salt Block' {..} =
    _salt
      `Prelude.hashWithSalt` blockIndex
      `Prelude.hashWithSalt` blockToken

instance Prelude.NFData Block where
  rnf Block' {..} =
    Prelude.rnf blockIndex
      `Prelude.seq` Prelude.rnf blockToken
