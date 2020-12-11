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
    stseChildShards,
    stseRecords,
    stseContinuationSequenceNumber,
    stseMillisBehindLatest,
  )
where

import Network.AWS.Kinesis.Types.ChildShard
import Network.AWS.Kinesis.Types.Record
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | After you call 'SubscribeToShard' , Kinesis Data Streams sends events of this type over an HTTP/2 connection to your consumer.
--
-- /See:/ 'mkSubscribeToShardEvent' smart constructor.
data SubscribeToShardEvent = SubscribeToShardEvent'
  { childShards ::
      Lude.Maybe [ChildShard],
    records :: [Record],
    continuationSequenceNumber :: Lude.Text,
    millisBehindLatest :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SubscribeToShardEvent' with the minimum fields required to make a request.
--
-- * 'childShards' - Undocumented field.
-- * 'continuationSequenceNumber' - Use this as @SequenceNumber@ in the next call to 'SubscribeToShard' , with @StartingPosition@ set to @AT_SEQUENCE_NUMBER@ or @AFTER_SEQUENCE_NUMBER@ . Use @ContinuationSequenceNumber@ for checkpointing because it captures your shard progress even when no data is written to the shard.
-- * 'millisBehindLatest' - The number of milliseconds the read records are from the tip of the stream, indicating how far behind current time the consumer is. A value of zero indicates that record processing is caught up, and there are no new records to process at this moment.
-- * 'records' -
mkSubscribeToShardEvent ::
  -- | 'continuationSequenceNumber'
  Lude.Text ->
  -- | 'millisBehindLatest'
  Lude.Natural ->
  SubscribeToShardEvent
mkSubscribeToShardEvent
  pContinuationSequenceNumber_
  pMillisBehindLatest_ =
    SubscribeToShardEvent'
      { childShards = Lude.Nothing,
        records = Lude.mempty,
        continuationSequenceNumber = pContinuationSequenceNumber_,
        millisBehindLatest = pMillisBehindLatest_
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'childShards' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stseChildShards :: Lens.Lens' SubscribeToShardEvent (Lude.Maybe [ChildShard])
stseChildShards = Lens.lens (childShards :: SubscribeToShardEvent -> Lude.Maybe [ChildShard]) (\s a -> s {childShards = a} :: SubscribeToShardEvent)
{-# DEPRECATED stseChildShards "Use generic-lens or generic-optics with 'childShards' instead." #-}

-- |
--
-- /Note:/ Consider using 'records' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stseRecords :: Lens.Lens' SubscribeToShardEvent [Record]
stseRecords = Lens.lens (records :: SubscribeToShardEvent -> [Record]) (\s a -> s {records = a} :: SubscribeToShardEvent)
{-# DEPRECATED stseRecords "Use generic-lens or generic-optics with 'records' instead." #-}

-- | Use this as @SequenceNumber@ in the next call to 'SubscribeToShard' , with @StartingPosition@ set to @AT_SEQUENCE_NUMBER@ or @AFTER_SEQUENCE_NUMBER@ . Use @ContinuationSequenceNumber@ for checkpointing because it captures your shard progress even when no data is written to the shard.
--
-- /Note:/ Consider using 'continuationSequenceNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stseContinuationSequenceNumber :: Lens.Lens' SubscribeToShardEvent Lude.Text
stseContinuationSequenceNumber = Lens.lens (continuationSequenceNumber :: SubscribeToShardEvent -> Lude.Text) (\s a -> s {continuationSequenceNumber = a} :: SubscribeToShardEvent)
{-# DEPRECATED stseContinuationSequenceNumber "Use generic-lens or generic-optics with 'continuationSequenceNumber' instead." #-}

-- | The number of milliseconds the read records are from the tip of the stream, indicating how far behind current time the consumer is. A value of zero indicates that record processing is caught up, and there are no new records to process at this moment.
--
-- /Note:/ Consider using 'millisBehindLatest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stseMillisBehindLatest :: Lens.Lens' SubscribeToShardEvent Lude.Natural
stseMillisBehindLatest = Lens.lens (millisBehindLatest :: SubscribeToShardEvent -> Lude.Natural) (\s a -> s {millisBehindLatest = a} :: SubscribeToShardEvent)
{-# DEPRECATED stseMillisBehindLatest "Use generic-lens or generic-optics with 'millisBehindLatest' instead." #-}

instance Lude.FromJSON SubscribeToShardEvent where
  parseJSON =
    Lude.withObject
      "SubscribeToShardEvent"
      ( \x ->
          SubscribeToShardEvent'
            Lude.<$> (x Lude..:? "ChildShards" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Records" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "ContinuationSequenceNumber")
            Lude.<*> (x Lude..: "MillisBehindLatest")
      )
