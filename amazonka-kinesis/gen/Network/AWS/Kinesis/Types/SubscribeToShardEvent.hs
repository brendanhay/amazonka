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
import qualified Network.AWS.Prelude as Prelude

-- | After you call SubscribeToShard, Kinesis Data Streams sends events of
-- this type over an HTTP\/2 connection to your consumer.
--
-- /See:/ 'newSubscribeToShardEvent' smart constructor.
data SubscribeToShardEvent = SubscribeToShardEvent'
  { childShards :: Prelude.Maybe [ChildShard],
    records :: [Record],
    -- | Use this as @SequenceNumber@ in the next call to SubscribeToShard, with
    -- @StartingPosition@ set to @AT_SEQUENCE_NUMBER@ or
    -- @AFTER_SEQUENCE_NUMBER@. Use @ContinuationSequenceNumber@ for
    -- checkpointing because it captures your shard progress even when no data
    -- is written to the shard.
    continuationSequenceNumber :: Prelude.Text,
    -- | The number of milliseconds the read records are from the tip of the
    -- stream, indicating how far behind current time the consumer is. A value
    -- of zero indicates that record processing is caught up, and there are no
    -- new records to process at this moment.
    millisBehindLatest :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'millisBehindLatest'
  Prelude.Natural ->
  SubscribeToShardEvent
newSubscribeToShardEvent
  pContinuationSequenceNumber_
  pMillisBehindLatest_ =
    SubscribeToShardEvent'
      { childShards =
          Prelude.Nothing,
        records = Prelude.mempty,
        continuationSequenceNumber =
          pContinuationSequenceNumber_,
        millisBehindLatest = pMillisBehindLatest_
      }

-- | Undocumented member.
subscribeToShardEvent_childShards :: Lens.Lens' SubscribeToShardEvent (Prelude.Maybe [ChildShard])
subscribeToShardEvent_childShards = Lens.lens (\SubscribeToShardEvent' {childShards} -> childShards) (\s@SubscribeToShardEvent' {} a -> s {childShards = a} :: SubscribeToShardEvent) Prelude.. Lens.mapping Lens._Coerce

-- |
subscribeToShardEvent_records :: Lens.Lens' SubscribeToShardEvent [Record]
subscribeToShardEvent_records = Lens.lens (\SubscribeToShardEvent' {records} -> records) (\s@SubscribeToShardEvent' {} a -> s {records = a} :: SubscribeToShardEvent) Prelude.. Lens._Coerce

-- | Use this as @SequenceNumber@ in the next call to SubscribeToShard, with
-- @StartingPosition@ set to @AT_SEQUENCE_NUMBER@ or
-- @AFTER_SEQUENCE_NUMBER@. Use @ContinuationSequenceNumber@ for
-- checkpointing because it captures your shard progress even when no data
-- is written to the shard.
subscribeToShardEvent_continuationSequenceNumber :: Lens.Lens' SubscribeToShardEvent Prelude.Text
subscribeToShardEvent_continuationSequenceNumber = Lens.lens (\SubscribeToShardEvent' {continuationSequenceNumber} -> continuationSequenceNumber) (\s@SubscribeToShardEvent' {} a -> s {continuationSequenceNumber = a} :: SubscribeToShardEvent)

-- | The number of milliseconds the read records are from the tip of the
-- stream, indicating how far behind current time the consumer is. A value
-- of zero indicates that record processing is caught up, and there are no
-- new records to process at this moment.
subscribeToShardEvent_millisBehindLatest :: Lens.Lens' SubscribeToShardEvent Prelude.Natural
subscribeToShardEvent_millisBehindLatest = Lens.lens (\SubscribeToShardEvent' {millisBehindLatest} -> millisBehindLatest) (\s@SubscribeToShardEvent' {} a -> s {millisBehindLatest = a} :: SubscribeToShardEvent)

instance Core.FromJSON SubscribeToShardEvent where
  parseJSON =
    Core.withObject
      "SubscribeToShardEvent"
      ( \x ->
          SubscribeToShardEvent'
            Prelude.<$> (x Core..:? "ChildShards" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Records" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "ContinuationSequenceNumber")
            Prelude.<*> (x Core..: "MillisBehindLatest")
      )

instance Prelude.Hashable SubscribeToShardEvent

instance Prelude.NFData SubscribeToShardEvent
