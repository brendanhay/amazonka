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
-- Module      : Amazonka.Kinesis.Types.HashKeyRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kinesis.Types.HashKeyRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The range of possible hash key values for the shard, which is a set of
-- ordered contiguous positive integers.
--
-- /See:/ 'newHashKeyRange' smart constructor.
data HashKeyRange = HashKeyRange'
  { -- | The starting hash key of the hash key range.
    startingHashKey :: Prelude.Text,
    -- | The ending hash key of the hash key range.
    endingHashKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HashKeyRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startingHashKey', 'hashKeyRange_startingHashKey' - The starting hash key of the hash key range.
--
-- 'endingHashKey', 'hashKeyRange_endingHashKey' - The ending hash key of the hash key range.
newHashKeyRange ::
  -- | 'startingHashKey'
  Prelude.Text ->
  -- | 'endingHashKey'
  Prelude.Text ->
  HashKeyRange
newHashKeyRange pStartingHashKey_ pEndingHashKey_ =
  HashKeyRange'
    { startingHashKey = pStartingHashKey_,
      endingHashKey = pEndingHashKey_
    }

-- | The starting hash key of the hash key range.
hashKeyRange_startingHashKey :: Lens.Lens' HashKeyRange Prelude.Text
hashKeyRange_startingHashKey = Lens.lens (\HashKeyRange' {startingHashKey} -> startingHashKey) (\s@HashKeyRange' {} a -> s {startingHashKey = a} :: HashKeyRange)

-- | The ending hash key of the hash key range.
hashKeyRange_endingHashKey :: Lens.Lens' HashKeyRange Prelude.Text
hashKeyRange_endingHashKey = Lens.lens (\HashKeyRange' {endingHashKey} -> endingHashKey) (\s@HashKeyRange' {} a -> s {endingHashKey = a} :: HashKeyRange)

instance Data.FromJSON HashKeyRange where
  parseJSON =
    Data.withObject
      "HashKeyRange"
      ( \x ->
          HashKeyRange'
            Prelude.<$> (x Data..: "StartingHashKey")
            Prelude.<*> (x Data..: "EndingHashKey")
      )

instance Prelude.Hashable HashKeyRange where
  hashWithSalt _salt HashKeyRange' {..} =
    _salt
      `Prelude.hashWithSalt` startingHashKey
      `Prelude.hashWithSalt` endingHashKey

instance Prelude.NFData HashKeyRange where
  rnf HashKeyRange' {..} =
    Prelude.rnf startingHashKey `Prelude.seq`
      Prelude.rnf endingHashKey
