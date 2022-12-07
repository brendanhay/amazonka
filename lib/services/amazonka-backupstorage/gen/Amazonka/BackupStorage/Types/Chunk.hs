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
-- Module      : Amazonka.BackupStorage.Types.Chunk
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.BackupStorage.Types.Chunk where

import Amazonka.BackupStorage.Types.DataChecksumAlgorithm
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Chunk
--
-- /See:/ 'newChunk' smart constructor.
data Chunk = Chunk'
  { -- | Chunk index
    index :: Prelude.Integer,
    -- | Chunk length
    length :: Prelude.Integer,
    -- | Chunk checksum
    checksum :: Prelude.Text,
    -- | Checksum algorithm
    checksumAlgorithm :: DataChecksumAlgorithm,
    -- | Chunk token
    chunkToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Chunk' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'index', 'chunk_index' - Chunk index
--
-- 'length', 'chunk_length' - Chunk length
--
-- 'checksum', 'chunk_checksum' - Chunk checksum
--
-- 'checksumAlgorithm', 'chunk_checksumAlgorithm' - Checksum algorithm
--
-- 'chunkToken', 'chunk_chunkToken' - Chunk token
newChunk ::
  -- | 'index'
  Prelude.Integer ->
  -- | 'length'
  Prelude.Integer ->
  -- | 'checksum'
  Prelude.Text ->
  -- | 'checksumAlgorithm'
  DataChecksumAlgorithm ->
  -- | 'chunkToken'
  Prelude.Text ->
  Chunk
newChunk
  pIndex_
  pLength_
  pChecksum_
  pChecksumAlgorithm_
  pChunkToken_ =
    Chunk'
      { index = pIndex_,
        length = pLength_,
        checksum = pChecksum_,
        checksumAlgorithm = pChecksumAlgorithm_,
        chunkToken = pChunkToken_
      }

-- | Chunk index
chunk_index :: Lens.Lens' Chunk Prelude.Integer
chunk_index = Lens.lens (\Chunk' {index} -> index) (\s@Chunk' {} a -> s {index = a} :: Chunk)

-- | Chunk length
chunk_length :: Lens.Lens' Chunk Prelude.Integer
chunk_length = Lens.lens (\Chunk' {length} -> length) (\s@Chunk' {} a -> s {length = a} :: Chunk)

-- | Chunk checksum
chunk_checksum :: Lens.Lens' Chunk Prelude.Text
chunk_checksum = Lens.lens (\Chunk' {checksum} -> checksum) (\s@Chunk' {} a -> s {checksum = a} :: Chunk)

-- | Checksum algorithm
chunk_checksumAlgorithm :: Lens.Lens' Chunk DataChecksumAlgorithm
chunk_checksumAlgorithm = Lens.lens (\Chunk' {checksumAlgorithm} -> checksumAlgorithm) (\s@Chunk' {} a -> s {checksumAlgorithm = a} :: Chunk)

-- | Chunk token
chunk_chunkToken :: Lens.Lens' Chunk Prelude.Text
chunk_chunkToken = Lens.lens (\Chunk' {chunkToken} -> chunkToken) (\s@Chunk' {} a -> s {chunkToken = a} :: Chunk)

instance Data.FromJSON Chunk where
  parseJSON =
    Data.withObject
      "Chunk"
      ( \x ->
          Chunk'
            Prelude.<$> (x Data..: "Index")
            Prelude.<*> (x Data..: "Length")
            Prelude.<*> (x Data..: "Checksum")
            Prelude.<*> (x Data..: "ChecksumAlgorithm")
            Prelude.<*> (x Data..: "ChunkToken")
      )

instance Prelude.Hashable Chunk where
  hashWithSalt _salt Chunk' {..} =
    _salt `Prelude.hashWithSalt` index
      `Prelude.hashWithSalt` length
      `Prelude.hashWithSalt` checksum
      `Prelude.hashWithSalt` checksumAlgorithm
      `Prelude.hashWithSalt` chunkToken

instance Prelude.NFData Chunk where
  rnf Chunk' {..} =
    Prelude.rnf index
      `Prelude.seq` Prelude.rnf length
      `Prelude.seq` Prelude.rnf checksum
      `Prelude.seq` Prelude.rnf checksumAlgorithm
      `Prelude.seq` Prelude.rnf chunkToken
