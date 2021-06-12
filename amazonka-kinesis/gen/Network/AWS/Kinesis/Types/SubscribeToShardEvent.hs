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
-- Module      : Network.AWS.Kinesis.Types.SubscribeToShardEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.SubscribeToShardEvent where

import qualified Network.AWS.Core as Core
import Network.AWS.Kinesis.Types.ChildShard
import Network.AWS.Kinesis.Types.Record
import qualified Network.AWS.Lens as Lens

-- | After you call SubscribeToShard, Kinesis Data Streams sends events of
-- this type over an HTTP\/2 connection to your consumer.
--
-- /See:/ 'newSubscribeToShardEvent' smart constructor.
data SubscribeToShardEvent = SubscribeToShardEvent'
  { childShards :: Core.Maybe [ChildShard],
    records :: [Record],
    -- | Use this as @SequenceNumber@ in the next call to SubscribeToShard, with
    -- @StartingPosition@ set to @AT_SEQUENCE_NUMBER@ or
    -- @AFTER_SEQUENCE_NUMBER@. Use @ContinuationSequenceNumber@ for
    -- checkpointing because it captures your shard progress even when no data
    -- is written to the shard.
    continuationSequenceNumber :: Core.Text,
    -- | The number of milliseconds the read records are from the tip of the
    -- stream, indicating how far behind current time the consumer is. A value
    -- of zero indicates that record processing is caught up, and there are no
    -- new records to process at this moment.
    millisBehindLatest :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SubscribeToShardEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'childShards', 'subscribeToShardEvent_childShards' - Undocumented member.
--
-- 'records', 'subscribeToShardEvent_records' -
--
-- 'continuationSequenceNumber', 'subscribeToShardEvent_continuationSequenceNumber' - Use this as @SequenceNumber@ in the next call to SubscribeToShard, with
-- @StartingPosition@ set to @AT_SEQUENCE_NUMBER@ or
-- @AFTER_SEQUENCE_NUMBER@. Use @ContinuationSequenceNumber@ for
-- checkpointing because it captures your shard progress even when no data
-- is written to the shard.
--
-- 'millisBehindLatest', 'subscribeToShardEvent_millisBehindLatest' - The number of milliseconds the read records are from the tip of the
-- stream, indicating how far behind current time the consumer is. A value
-- of zero indicates that record processing is caught up, and there are no
-- new records to process at this moment.
newSubscribeToShardEvent ::
  -- | 'continuationSequenceNumber'
  Core.Text ->
  -- | 'millisBehindLatest'
  Core.Natural ->
  SubscribeToShardEvent
newSubscribeToShardEvent
  pContinuationSequenceNumber_
  pMillisBehindLatest_ =
    SubscribeToShardEvent'
      { childShards = Core.Nothing,
        records = Core.mempty,
        continuationSequenceNumber =
          pContinuationSequenceNumber_,
        millisBehindLatest = pMillisBehindLatest_
      }

-- | Undocumented member.
subscribeToShardEvent_childShards :: Lens.Lens' SubscribeToShardEvent (Core.Maybe [ChildShard])
subscribeToShardEvent_childShards = Lens.lens (\SubscribeToShardEvent' {childShards} -> childShards) (\s@SubscribeToShardEvent' {} a -> s {childShards = a} :: SubscribeToShardEvent) Core.. Lens.mapping Lens._Coerce

-- |
subscribeToShardEvent_records :: Lens.Lens' SubscribeToShardEvent [Record]
subscribeToShardEvent_records = Lens.lens (\SubscribeToShardEvent' {records} -> records) (\s@SubscribeToShardEvent' {} a -> s {records = a} :: SubscribeToShardEvent) Core.. Lens._Coerce

-- | Use this as @SequenceNumber@ in the next call to SubscribeToShard, with
-- @StartingPosition@ set to @AT_SEQUENCE_NUMBER@ or
-- @AFTER_SEQUENCE_NUMBER@. Use @ContinuationSequenceNumber@ for
-- checkpointing because it captures your shard progress even when no data
-- is written to the shard.
subscribeToShardEvent_continuationSequenceNumber :: Lens.Lens' SubscribeToShardEvent Core.Text
subscribeToShardEvent_continuationSequenceNumber = Lens.lens (\SubscribeToShardEvent' {continuationSequenceNumber} -> continuationSequenceNumber) (\s@SubscribeToShardEvent' {} a -> s {continuationSequenceNumber = a} :: SubscribeToShardEvent)

-- | The number of milliseconds the read records are from the tip of the
-- stream, indicating how far behind current time the consumer is. A value
-- of zero indicates that record processing is caught up, and there are no
-- new records to process at this moment.
subscribeToShardEvent_millisBehindLatest :: Lens.Lens' SubscribeToShardEvent Core.Natural
subscribeToShardEvent_millisBehindLatest = Lens.lens (\SubscribeToShardEvent' {millisBehindLatest} -> millisBehindLatest) (\s@SubscribeToShardEvent' {} a -> s {millisBehindLatest = a} :: SubscribeToShardEvent)

instance Core.FromJSON SubscribeToShardEvent where
  parseJSON =
    Core.withObject
      "SubscribeToShardEvent"
      ( \x ->
          SubscribeToShardEvent'
            Core.<$> (x Core..:? "ChildShards" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Records" Core..!= Core.mempty)
            Core.<*> (x Core..: "ContinuationSequenceNumber")
            Core.<*> (x Core..: "MillisBehindLatest")
      )

instance Core.Hashable SubscribeToShardEvent

instance Core.NFData SubscribeToShardEvent
