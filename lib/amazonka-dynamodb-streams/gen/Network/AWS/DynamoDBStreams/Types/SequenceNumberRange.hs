{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDBStreams.Types.SequenceNumberRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDBStreams.Types.SequenceNumberRange
  ( SequenceNumberRange (..)
  -- * Smart constructor
  , mkSequenceNumberRange
  -- * Lenses
  , snrEndingSequenceNumber
  , snrStartingSequenceNumber
  ) where

import qualified Network.AWS.DynamoDBStreams.Types.SequenceNumber as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The beginning and ending sequence numbers for the stream records contained within a shard.
--
-- /See:/ 'mkSequenceNumberRange' smart constructor.
data SequenceNumberRange = SequenceNumberRange'
  { endingSequenceNumber :: Core.Maybe Types.SequenceNumber
    -- ^ The last sequence number for the stream records contained within a shard. String contains numeric characters only.
  , startingSequenceNumber :: Core.Maybe Types.SequenceNumber
    -- ^ The first sequence number for the stream records contained within a shard. String contains numeric characters only.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SequenceNumberRange' value with any optional fields omitted.
mkSequenceNumberRange
    :: SequenceNumberRange
mkSequenceNumberRange
  = SequenceNumberRange'{endingSequenceNumber = Core.Nothing,
                         startingSequenceNumber = Core.Nothing}

-- | The last sequence number for the stream records contained within a shard. String contains numeric characters only.
--
-- /Note:/ Consider using 'endingSequenceNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snrEndingSequenceNumber :: Lens.Lens' SequenceNumberRange (Core.Maybe Types.SequenceNumber)
snrEndingSequenceNumber = Lens.field @"endingSequenceNumber"
{-# INLINEABLE snrEndingSequenceNumber #-}
{-# DEPRECATED endingSequenceNumber "Use generic-lens or generic-optics with 'endingSequenceNumber' instead"  #-}

-- | The first sequence number for the stream records contained within a shard. String contains numeric characters only.
--
-- /Note:/ Consider using 'startingSequenceNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snrStartingSequenceNumber :: Lens.Lens' SequenceNumberRange (Core.Maybe Types.SequenceNumber)
snrStartingSequenceNumber = Lens.field @"startingSequenceNumber"
{-# INLINEABLE snrStartingSequenceNumber #-}
{-# DEPRECATED startingSequenceNumber "Use generic-lens or generic-optics with 'startingSequenceNumber' instead"  #-}

instance Core.FromJSON SequenceNumberRange where
        parseJSON
          = Core.withObject "SequenceNumberRange" Core.$
              \ x ->
                SequenceNumberRange' Core.<$>
                  (x Core..:? "EndingSequenceNumber") Core.<*>
                    x Core..:? "StartingSequenceNumber"
