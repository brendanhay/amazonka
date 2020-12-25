{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.SequenceNumberRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.SequenceNumberRange
  ( SequenceNumberRange (..),

    -- * Smart constructor
    mkSequenceNumberRange,

    -- * Lenses
    snrStartingSequenceNumber,
    snrEndingSequenceNumber,
  )
where

import qualified Network.AWS.Kinesis.Types.SequenceNumber as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The range of possible sequence numbers for the shard.
--
-- /See:/ 'mkSequenceNumberRange' smart constructor.
data SequenceNumberRange = SequenceNumberRange'
  { -- | The starting sequence number for the range.
    startingSequenceNumber :: Types.SequenceNumber,
    -- | The ending sequence number for the range. Shards that are in the OPEN state have an ending sequence number of @null@ .
    endingSequenceNumber :: Core.Maybe Types.SequenceNumber
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SequenceNumberRange' value with any optional fields omitted.
mkSequenceNumberRange ::
  -- | 'startingSequenceNumber'
  Types.SequenceNumber ->
  SequenceNumberRange
mkSequenceNumberRange startingSequenceNumber =
  SequenceNumberRange'
    { startingSequenceNumber,
      endingSequenceNumber = Core.Nothing
    }

-- | The starting sequence number for the range.
--
-- /Note:/ Consider using 'startingSequenceNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snrStartingSequenceNumber :: Lens.Lens' SequenceNumberRange Types.SequenceNumber
snrStartingSequenceNumber = Lens.field @"startingSequenceNumber"
{-# DEPRECATED snrStartingSequenceNumber "Use generic-lens or generic-optics with 'startingSequenceNumber' instead." #-}

-- | The ending sequence number for the range. Shards that are in the OPEN state have an ending sequence number of @null@ .
--
-- /Note:/ Consider using 'endingSequenceNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snrEndingSequenceNumber :: Lens.Lens' SequenceNumberRange (Core.Maybe Types.SequenceNumber)
snrEndingSequenceNumber = Lens.field @"endingSequenceNumber"
{-# DEPRECATED snrEndingSequenceNumber "Use generic-lens or generic-optics with 'endingSequenceNumber' instead." #-}

instance Core.FromJSON SequenceNumberRange where
  parseJSON =
    Core.withObject "SequenceNumberRange" Core.$
      \x ->
        SequenceNumberRange'
          Core.<$> (x Core..: "StartingSequenceNumber")
          Core.<*> (x Core..:? "EndingSequenceNumber")
