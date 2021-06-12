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
-- Module      : Network.AWS.Kinesis.Types.HashKeyRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.HashKeyRange where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The range of possible hash key values for the shard, which is a set of
-- ordered contiguous positive integers.
--
-- /See:/ 'newHashKeyRange' smart constructor.
data HashKeyRange = HashKeyRange'
  { -- | The starting hash key of the hash key range.
    startingHashKey :: Core.Text,
    -- | The ending hash key of the hash key range.
    endingHashKey :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'endingHashKey'
  Core.Text ->
  HashKeyRange
newHashKeyRange pStartingHashKey_ pEndingHashKey_ =
  HashKeyRange'
    { startingHashKey = pStartingHashKey_,
      endingHashKey = pEndingHashKey_
    }

-- | The starting hash key of the hash key range.
hashKeyRange_startingHashKey :: Lens.Lens' HashKeyRange Core.Text
hashKeyRange_startingHashKey = Lens.lens (\HashKeyRange' {startingHashKey} -> startingHashKey) (\s@HashKeyRange' {} a -> s {startingHashKey = a} :: HashKeyRange)

-- | The ending hash key of the hash key range.
hashKeyRange_endingHashKey :: Lens.Lens' HashKeyRange Core.Text
hashKeyRange_endingHashKey = Lens.lens (\HashKeyRange' {endingHashKey} -> endingHashKey) (\s@HashKeyRange' {} a -> s {endingHashKey = a} :: HashKeyRange)

instance Core.FromJSON HashKeyRange where
  parseJSON =
    Core.withObject
      "HashKeyRange"
      ( \x ->
          HashKeyRange'
            Core.<$> (x Core..: "StartingHashKey")
            Core.<*> (x Core..: "EndingHashKey")
      )

instance Core.Hashable HashKeyRange

instance Core.NFData HashKeyRange
