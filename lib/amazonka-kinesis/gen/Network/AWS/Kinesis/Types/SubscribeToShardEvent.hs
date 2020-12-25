{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.SubscribeToShardEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.SubscribeToShardEvent
  ( SubscribeToShardEvent (..),

    -- * Smart constructor
    mkSubscribeToShardEvent,

    -- * Lenses
    stseRecords,
    stseContinuationSequenceNumber,
    stseMillisBehindLatest,
    stseChildShards,
  )
where

import qualified Network.AWS.Kinesis.Types.ChildShard as Types
import qualified Network.AWS.Kinesis.Types.Record as Types
import qualified Network.AWS.Kinesis.Types.SequenceNumber as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | After you call 'SubscribeToShard' , Kinesis Data Streams sends events of this type over an HTTP/2 connection to your consumer.
--
-- /See:/ 'mkSubscribeToShardEvent' smart constructor.
data SubscribeToShardEvent = SubscribeToShardEvent'
  { -- |
    records :: [Types.Record],
    -- | Use this as @SequenceNumber@ in the next call to 'SubscribeToShard' , with @StartingPosition@ set to @AT_SEQUENCE_NUMBER@ or @AFTER_SEQUENCE_NUMBER@ . Use @ContinuationSequenceNumber@ for checkpointing because it captures your shard progress even when no data is written to the shard.
    continuationSequenceNumber :: Types.SequenceNumber,
    -- | The number of milliseconds the read records are from the tip of the stream, indicating how far behind current time the consumer is. A value of zero indicates that record processing is caught up, and there are no new records to process at this moment.
    millisBehindLatest :: Core.Natural,
    childShards :: Core.Maybe [Types.ChildShard]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'SubscribeToShardEvent' value with any optional fields omitted.
mkSubscribeToShardEvent ::
  -- | 'continuationSequenceNumber'
  Types.SequenceNumber ->
  -- | 'millisBehindLatest'
  Core.Natural ->
  SubscribeToShardEvent
mkSubscribeToShardEvent
  continuationSequenceNumber
  millisBehindLatest =
    SubscribeToShardEvent'
      { records = Core.mempty,
        continuationSequenceNumber,
        millisBehindLatest,
        childShards = Core.Nothing
      }

-- |
--
-- /Note:/ Consider using 'records' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stseRecords :: Lens.Lens' SubscribeToShardEvent [Types.Record]
stseRecords = Lens.field @"records"
{-# DEPRECATED stseRecords "Use generic-lens or generic-optics with 'records' instead." #-}

-- | Use this as @SequenceNumber@ in the next call to 'SubscribeToShard' , with @StartingPosition@ set to @AT_SEQUENCE_NUMBER@ or @AFTER_SEQUENCE_NUMBER@ . Use @ContinuationSequenceNumber@ for checkpointing because it captures your shard progress even when no data is written to the shard.
--
-- /Note:/ Consider using 'continuationSequenceNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stseContinuationSequenceNumber :: Lens.Lens' SubscribeToShardEvent Types.SequenceNumber
stseContinuationSequenceNumber = Lens.field @"continuationSequenceNumber"
{-# DEPRECATED stseContinuationSequenceNumber "Use generic-lens or generic-optics with 'continuationSequenceNumber' instead." #-}

-- | The number of milliseconds the read records are from the tip of the stream, indicating how far behind current time the consumer is. A value of zero indicates that record processing is caught up, and there are no new records to process at this moment.
--
-- /Note:/ Consider using 'millisBehindLatest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stseMillisBehindLatest :: Lens.Lens' SubscribeToShardEvent Core.Natural
stseMillisBehindLatest = Lens.field @"millisBehindLatest"
{-# DEPRECATED stseMillisBehindLatest "Use generic-lens or generic-optics with 'millisBehindLatest' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'childShards' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stseChildShards :: Lens.Lens' SubscribeToShardEvent (Core.Maybe [Types.ChildShard])
stseChildShards = Lens.field @"childShards"
{-# DEPRECATED stseChildShards "Use generic-lens or generic-optics with 'childShards' instead." #-}

instance Core.FromJSON SubscribeToShardEvent where
  parseJSON =
    Core.withObject "SubscribeToShardEvent" Core.$
      \x ->
        SubscribeToShardEvent'
          Core.<$> (x Core..:? "Records" Core..!= Core.mempty)
          Core.<*> (x Core..: "ContinuationSequenceNumber")
          Core.<*> (x Core..: "MillisBehindLatest")
          Core.<*> (x Core..:? "ChildShards")
