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
-- Module      : Network.AWS.Kinesis.Types.SequenceNumberRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.SequenceNumberRange where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The range of possible sequence numbers for the shard.
--
-- /See:/ 'newSequenceNumberRange' smart constructor.
data SequenceNumberRange = SequenceNumberRange'
  { -- | The ending sequence number for the range. Shards that are in the OPEN
    -- state have an ending sequence number of @null@.
    endingSequenceNumber :: Core.Maybe Core.Text,
    -- | The starting sequence number for the range.
    startingSequenceNumber :: Core.Text
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
-- 'endingSequenceNumber', 'sequenceNumberRange_endingSequenceNumber' - The ending sequence number for the range. Shards that are in the OPEN
-- state have an ending sequence number of @null@.
--
-- 'startingSequenceNumber', 'sequenceNumberRange_startingSequenceNumber' - The starting sequence number for the range.
newSequenceNumberRange ::
  -- | 'startingSequenceNumber'
  Core.Text ->
  SequenceNumberRange
newSequenceNumberRange pStartingSequenceNumber_ =
  SequenceNumberRange'
    { endingSequenceNumber =
        Core.Nothing,
      startingSequenceNumber = pStartingSequenceNumber_
    }

-- | The ending sequence number for the range. Shards that are in the OPEN
-- state have an ending sequence number of @null@.
sequenceNumberRange_endingSequenceNumber :: Lens.Lens' SequenceNumberRange (Core.Maybe Core.Text)
sequenceNumberRange_endingSequenceNumber = Lens.lens (\SequenceNumberRange' {endingSequenceNumber} -> endingSequenceNumber) (\s@SequenceNumberRange' {} a -> s {endingSequenceNumber = a} :: SequenceNumberRange)

-- | The starting sequence number for the range.
sequenceNumberRange_startingSequenceNumber :: Lens.Lens' SequenceNumberRange Core.Text
sequenceNumberRange_startingSequenceNumber = Lens.lens (\SequenceNumberRange' {startingSequenceNumber} -> startingSequenceNumber) (\s@SequenceNumberRange' {} a -> s {startingSequenceNumber = a} :: SequenceNumberRange)

instance Core.FromJSON SequenceNumberRange where
  parseJSON =
    Core.withObject
      "SequenceNumberRange"
      ( \x ->
          SequenceNumberRange'
            Core.<$> (x Core..:? "EndingSequenceNumber")
            Core.<*> (x Core..: "StartingSequenceNumber")
      )

instance Core.Hashable SequenceNumberRange

instance Core.NFData SequenceNumberRange
