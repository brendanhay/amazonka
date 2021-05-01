{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Kinesis.Types.StartingPosition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.StartingPosition where

import Network.AWS.Kinesis.Types.ShardIteratorType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- |
--
-- /See:/ 'newStartingPosition' smart constructor.
data StartingPosition = StartingPosition'
  { -- | The sequence number of the data record in the shard from which to start
    -- streaming. To specify a sequence number, set @StartingPosition@ to
    -- @AT_SEQUENCE_NUMBER@ or @AFTER_SEQUENCE_NUMBER@.
    sequenceNumber :: Prelude.Maybe Prelude.Text,
    -- | The time stamp of the data record from which to start reading. To
    -- specify a time stamp, set @StartingPosition@ to @Type AT_TIMESTAMP@. A
    -- time stamp is the Unix epoch date with precision in milliseconds. For
    -- example, @2016-04-04T19:58:46.480-00:00@ or @1459799926.480@. If a
    -- record with this exact time stamp does not exist, records will be
    -- streamed from the next (later) record. If the time stamp is older than
    -- the current trim horizon, records will be streamed from the oldest
    -- untrimmed data record (@TRIM_HORIZON@).
    timestamp :: Prelude.Maybe Prelude.POSIX,
    -- | You can set the starting position to one of the following values:
    --
    -- @AT_SEQUENCE_NUMBER@: Start streaming from the position denoted by the
    -- sequence number specified in the @SequenceNumber@ field.
    --
    -- @AFTER_SEQUENCE_NUMBER@: Start streaming right after the position
    -- denoted by the sequence number specified in the @SequenceNumber@ field.
    --
    -- @AT_TIMESTAMP@: Start streaming from the position denoted by the time
    -- stamp specified in the @Timestamp@ field.
    --
    -- @TRIM_HORIZON@: Start streaming at the last untrimmed record in the
    -- shard, which is the oldest data record in the shard.
    --
    -- @LATEST@: Start streaming just after the most recent record in the
    -- shard, so that you always read the most recent data in the shard.
    type' :: ShardIteratorType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StartingPosition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sequenceNumber', 'startingPosition_sequenceNumber' - The sequence number of the data record in the shard from which to start
-- streaming. To specify a sequence number, set @StartingPosition@ to
-- @AT_SEQUENCE_NUMBER@ or @AFTER_SEQUENCE_NUMBER@.
--
-- 'timestamp', 'startingPosition_timestamp' - The time stamp of the data record from which to start reading. To
-- specify a time stamp, set @StartingPosition@ to @Type AT_TIMESTAMP@. A
-- time stamp is the Unix epoch date with precision in milliseconds. For
-- example, @2016-04-04T19:58:46.480-00:00@ or @1459799926.480@. If a
-- record with this exact time stamp does not exist, records will be
-- streamed from the next (later) record. If the time stamp is older than
-- the current trim horizon, records will be streamed from the oldest
-- untrimmed data record (@TRIM_HORIZON@).
--
-- 'type'', 'startingPosition_type' - You can set the starting position to one of the following values:
--
-- @AT_SEQUENCE_NUMBER@: Start streaming from the position denoted by the
-- sequence number specified in the @SequenceNumber@ field.
--
-- @AFTER_SEQUENCE_NUMBER@: Start streaming right after the position
-- denoted by the sequence number specified in the @SequenceNumber@ field.
--
-- @AT_TIMESTAMP@: Start streaming from the position denoted by the time
-- stamp specified in the @Timestamp@ field.
--
-- @TRIM_HORIZON@: Start streaming at the last untrimmed record in the
-- shard, which is the oldest data record in the shard.
--
-- @LATEST@: Start streaming just after the most recent record in the
-- shard, so that you always read the most recent data in the shard.
newStartingPosition ::
  -- | 'type''
  ShardIteratorType ->
  StartingPosition
newStartingPosition pType_ =
  StartingPosition'
    { sequenceNumber = Prelude.Nothing,
      timestamp = Prelude.Nothing,
      type' = pType_
    }

-- | The sequence number of the data record in the shard from which to start
-- streaming. To specify a sequence number, set @StartingPosition@ to
-- @AT_SEQUENCE_NUMBER@ or @AFTER_SEQUENCE_NUMBER@.
startingPosition_sequenceNumber :: Lens.Lens' StartingPosition (Prelude.Maybe Prelude.Text)
startingPosition_sequenceNumber = Lens.lens (\StartingPosition' {sequenceNumber} -> sequenceNumber) (\s@StartingPosition' {} a -> s {sequenceNumber = a} :: StartingPosition)

-- | The time stamp of the data record from which to start reading. To
-- specify a time stamp, set @StartingPosition@ to @Type AT_TIMESTAMP@. A
-- time stamp is the Unix epoch date with precision in milliseconds. For
-- example, @2016-04-04T19:58:46.480-00:00@ or @1459799926.480@. If a
-- record with this exact time stamp does not exist, records will be
-- streamed from the next (later) record. If the time stamp is older than
-- the current trim horizon, records will be streamed from the oldest
-- untrimmed data record (@TRIM_HORIZON@).
startingPosition_timestamp :: Lens.Lens' StartingPosition (Prelude.Maybe Prelude.UTCTime)
startingPosition_timestamp = Lens.lens (\StartingPosition' {timestamp} -> timestamp) (\s@StartingPosition' {} a -> s {timestamp = a} :: StartingPosition) Prelude.. Lens.mapping Prelude._Time

-- | You can set the starting position to one of the following values:
--
-- @AT_SEQUENCE_NUMBER@: Start streaming from the position denoted by the
-- sequence number specified in the @SequenceNumber@ field.
--
-- @AFTER_SEQUENCE_NUMBER@: Start streaming right after the position
-- denoted by the sequence number specified in the @SequenceNumber@ field.
--
-- @AT_TIMESTAMP@: Start streaming from the position denoted by the time
-- stamp specified in the @Timestamp@ field.
--
-- @TRIM_HORIZON@: Start streaming at the last untrimmed record in the
-- shard, which is the oldest data record in the shard.
--
-- @LATEST@: Start streaming just after the most recent record in the
-- shard, so that you always read the most recent data in the shard.
startingPosition_type :: Lens.Lens' StartingPosition ShardIteratorType
startingPosition_type = Lens.lens (\StartingPosition' {type'} -> type') (\s@StartingPosition' {} a -> s {type' = a} :: StartingPosition)

instance Prelude.Hashable StartingPosition

instance Prelude.NFData StartingPosition

instance Prelude.ToJSON StartingPosition where
  toJSON StartingPosition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SequenceNumber" Prelude..=)
              Prelude.<$> sequenceNumber,
            ("Timestamp" Prelude..=) Prelude.<$> timestamp,
            Prelude.Just ("Type" Prelude..= type')
          ]
      )
