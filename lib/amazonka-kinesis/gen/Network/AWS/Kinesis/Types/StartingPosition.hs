{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.StartingPosition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.StartingPosition
  ( StartingPosition (..),

    -- * Smart constructor
    mkStartingPosition,

    -- * Lenses
    spSequenceNumber,
    spType,
    spTimestamp,
  )
where

import Network.AWS.Kinesis.Types.ShardIteratorType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- |
--
-- /See:/ 'mkStartingPosition' smart constructor.
data StartingPosition = StartingPosition'
  { -- | The sequence number of the data record in the shard from which to start streaming. To specify a sequence number, set @StartingPosition@ to @AT_SEQUENCE_NUMBER@ or @AFTER_SEQUENCE_NUMBER@ .
    sequenceNumber :: Lude.Maybe Lude.Text,
    -- | You can set the starting position to one of the following values:
    --
    -- @AT_SEQUENCE_NUMBER@ : Start streaming from the position denoted by the sequence number specified in the @SequenceNumber@ field.
    -- @AFTER_SEQUENCE_NUMBER@ : Start streaming right after the position denoted by the sequence number specified in the @SequenceNumber@ field.
    -- @AT_TIMESTAMP@ : Start streaming from the position denoted by the time stamp specified in the @Timestamp@ field.
    -- @TRIM_HORIZON@ : Start streaming at the last untrimmed record in the shard, which is the oldest data record in the shard.
    -- @LATEST@ : Start streaming just after the most recent record in the shard, so that you always read the most recent data in the shard.
    type' :: ShardIteratorType,
    -- | The time stamp of the data record from which to start reading. To specify a time stamp, set @StartingPosition@ to @Type AT_TIMESTAMP@ . A time stamp is the Unix epoch date with precision in milliseconds. For example, @2016-04-04T19:58:46.480-00:00@ or @1459799926.480@ . If a record with this exact time stamp does not exist, records will be streamed from the next (later) record. If the time stamp is older than the current trim horizon, records will be streamed from the oldest untrimmed data record (@TRIM_HORIZON@ ).
    timestamp :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartingPosition' with the minimum fields required to make a request.
--
-- * 'sequenceNumber' - The sequence number of the data record in the shard from which to start streaming. To specify a sequence number, set @StartingPosition@ to @AT_SEQUENCE_NUMBER@ or @AFTER_SEQUENCE_NUMBER@ .
-- * 'type'' - You can set the starting position to one of the following values:
--
-- @AT_SEQUENCE_NUMBER@ : Start streaming from the position denoted by the sequence number specified in the @SequenceNumber@ field.
-- @AFTER_SEQUENCE_NUMBER@ : Start streaming right after the position denoted by the sequence number specified in the @SequenceNumber@ field.
-- @AT_TIMESTAMP@ : Start streaming from the position denoted by the time stamp specified in the @Timestamp@ field.
-- @TRIM_HORIZON@ : Start streaming at the last untrimmed record in the shard, which is the oldest data record in the shard.
-- @LATEST@ : Start streaming just after the most recent record in the shard, so that you always read the most recent data in the shard.
-- * 'timestamp' - The time stamp of the data record from which to start reading. To specify a time stamp, set @StartingPosition@ to @Type AT_TIMESTAMP@ . A time stamp is the Unix epoch date with precision in milliseconds. For example, @2016-04-04T19:58:46.480-00:00@ or @1459799926.480@ . If a record with this exact time stamp does not exist, records will be streamed from the next (later) record. If the time stamp is older than the current trim horizon, records will be streamed from the oldest untrimmed data record (@TRIM_HORIZON@ ).
mkStartingPosition ::
  -- | 'type''
  ShardIteratorType ->
  StartingPosition
mkStartingPosition pType_ =
  StartingPosition'
    { sequenceNumber = Lude.Nothing,
      type' = pType_,
      timestamp = Lude.Nothing
    }

-- | The sequence number of the data record in the shard from which to start streaming. To specify a sequence number, set @StartingPosition@ to @AT_SEQUENCE_NUMBER@ or @AFTER_SEQUENCE_NUMBER@ .
--
-- /Note:/ Consider using 'sequenceNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spSequenceNumber :: Lens.Lens' StartingPosition (Lude.Maybe Lude.Text)
spSequenceNumber = Lens.lens (sequenceNumber :: StartingPosition -> Lude.Maybe Lude.Text) (\s a -> s {sequenceNumber = a} :: StartingPosition)
{-# DEPRECATED spSequenceNumber "Use generic-lens or generic-optics with 'sequenceNumber' instead." #-}

-- | You can set the starting position to one of the following values:
--
-- @AT_SEQUENCE_NUMBER@ : Start streaming from the position denoted by the sequence number specified in the @SequenceNumber@ field.
-- @AFTER_SEQUENCE_NUMBER@ : Start streaming right after the position denoted by the sequence number specified in the @SequenceNumber@ field.
-- @AT_TIMESTAMP@ : Start streaming from the position denoted by the time stamp specified in the @Timestamp@ field.
-- @TRIM_HORIZON@ : Start streaming at the last untrimmed record in the shard, which is the oldest data record in the shard.
-- @LATEST@ : Start streaming just after the most recent record in the shard, so that you always read the most recent data in the shard.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spType :: Lens.Lens' StartingPosition ShardIteratorType
spType = Lens.lens (type' :: StartingPosition -> ShardIteratorType) (\s a -> s {type' = a} :: StartingPosition)
{-# DEPRECATED spType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The time stamp of the data record from which to start reading. To specify a time stamp, set @StartingPosition@ to @Type AT_TIMESTAMP@ . A time stamp is the Unix epoch date with precision in milliseconds. For example, @2016-04-04T19:58:46.480-00:00@ or @1459799926.480@ . If a record with this exact time stamp does not exist, records will be streamed from the next (later) record. If the time stamp is older than the current trim horizon, records will be streamed from the oldest untrimmed data record (@TRIM_HORIZON@ ).
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spTimestamp :: Lens.Lens' StartingPosition (Lude.Maybe Lude.Timestamp)
spTimestamp = Lens.lens (timestamp :: StartingPosition -> Lude.Maybe Lude.Timestamp) (\s a -> s {timestamp = a} :: StartingPosition)
{-# DEPRECATED spTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Lude.ToJSON StartingPosition where
  toJSON StartingPosition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SequenceNumber" Lude..=) Lude.<$> sequenceNumber,
            Lude.Just ("Type" Lude..= type'),
            ("Timestamp" Lude..=) Lude.<$> timestamp
          ]
      )
