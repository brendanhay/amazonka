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
-- Module      : Amazonka.Kinesis.Types.SubscribeToShardEvent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kinesis.Types.SubscribeToShardEvent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kinesis.Types.ChildShard
import Amazonka.Kinesis.Types.Record
import qualified Amazonka.Prelude as Prelude

-- | After you call SubscribeToShard, Kinesis Data Streams sends events of
-- this type over an HTTP\/2 connection to your consumer.
--
-- /See:/ 'newSubscribeToShardEvent' smart constructor.
data SubscribeToShardEvent = SubscribeToShardEvent'
  { -- | The list of the child shards of the current shard, returned only at the
    -- end of the current shard.
    childShards :: Prelude.Maybe [ChildShard],
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
-- 'childShards', 'subscribeToShardEvent_childShards' - The list of the child shards of the current shard, returned only at the
-- end of the current shard.
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

-- | The list of the child shards of the current shard, returned only at the
-- end of the current shard.
subscribeToShardEvent_childShards :: Lens.Lens' SubscribeToShardEvent (Prelude.Maybe [ChildShard])
subscribeToShardEvent_childShards = Lens.lens (\SubscribeToShardEvent' {childShards} -> childShards) (\s@SubscribeToShardEvent' {} a -> s {childShards = a} :: SubscribeToShardEvent) Prelude.. Lens.mapping Lens.coerced

subscribeToShardEvent_records :: Lens.Lens' SubscribeToShardEvent [Record]
subscribeToShardEvent_records = Lens.lens (\SubscribeToShardEvent' {records} -> records) (\s@SubscribeToShardEvent' {} a -> s {records = a} :: SubscribeToShardEvent) Prelude.. Lens.coerced

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

instance Data.FromJSON SubscribeToShardEvent where
  parseJSON =
    Data.withObject
      "SubscribeToShardEvent"
      ( \x ->
          SubscribeToShardEvent'
            Prelude.<$> (x Data..:? "ChildShards" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Records" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "ContinuationSequenceNumber")
            Prelude.<*> (x Data..: "MillisBehindLatest")
      )

instance Prelude.Hashable SubscribeToShardEvent where
  hashWithSalt _salt SubscribeToShardEvent' {..} =
    _salt
      `Prelude.hashWithSalt` childShards
      `Prelude.hashWithSalt` records
      `Prelude.hashWithSalt` continuationSequenceNumber
      `Prelude.hashWithSalt` millisBehindLatest

instance Prelude.NFData SubscribeToShardEvent where
  rnf SubscribeToShardEvent' {..} =
    Prelude.rnf childShards
      `Prelude.seq` Prelude.rnf records
      `Prelude.seq` Prelude.rnf continuationSequenceNumber
      `Prelude.seq` Prelude.rnf millisBehindLatest
