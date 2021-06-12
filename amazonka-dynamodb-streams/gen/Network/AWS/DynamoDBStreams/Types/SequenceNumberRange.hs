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
-- Module      : Network.AWS.DynamoDBStreams.Types.SequenceNumberRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDBStreams.Types.SequenceNumberRange where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The beginning and ending sequence numbers for the stream records
-- contained within a shard.
--
-- /See:/ 'newSequenceNumberRange' smart constructor.
data SequenceNumberRange = SequenceNumberRange'
  { -- | The first sequence number for the stream records contained within a
    -- shard. String contains numeric characters only.
    startingSequenceNumber :: Core.Maybe Core.Text,
    -- | The last sequence number for the stream records contained within a
    -- shard. String contains numeric characters only.
    endingSequenceNumber :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SequenceNumberRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startingSequenceNumber', 'sequenceNumberRange_startingSequenceNumber' - The first sequence number for the stream records contained within a
-- shard. String contains numeric characters only.
--
-- 'endingSequenceNumber', 'sequenceNumberRange_endingSequenceNumber' - The last sequence number for the stream records contained within a
-- shard. String contains numeric characters only.
newSequenceNumberRange ::
  SequenceNumberRange
newSequenceNumberRange =
  SequenceNumberRange'
    { startingSequenceNumber =
        Core.Nothing,
      endingSequenceNumber = Core.Nothing
    }

-- | The first sequence number for the stream records contained within a
-- shard. String contains numeric characters only.
sequenceNumberRange_startingSequenceNumber :: Lens.Lens' SequenceNumberRange (Core.Maybe Core.Text)
sequenceNumberRange_startingSequenceNumber = Lens.lens (\SequenceNumberRange' {startingSequenceNumber} -> startingSequenceNumber) (\s@SequenceNumberRange' {} a -> s {startingSequenceNumber = a} :: SequenceNumberRange)

-- | The last sequence number for the stream records contained within a
-- shard. String contains numeric characters only.
sequenceNumberRange_endingSequenceNumber :: Lens.Lens' SequenceNumberRange (Core.Maybe Core.Text)
sequenceNumberRange_endingSequenceNumber = Lens.lens (\SequenceNumberRange' {endingSequenceNumber} -> endingSequenceNumber) (\s@SequenceNumberRange' {} a -> s {endingSequenceNumber = a} :: SequenceNumberRange)

instance Core.FromJSON SequenceNumberRange where
  parseJSON =
    Core.withObject
      "SequenceNumberRange"
      ( \x ->
          SequenceNumberRange'
            Core.<$> (x Core..:? "StartingSequenceNumber")
            Core.<*> (x Core..:? "EndingSequenceNumber")
      )

instance Core.Hashable SequenceNumberRange

instance Core.NFData SequenceNumberRange
