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
    spType,
    spSequenceNumber,
    spTimestamp,
  )
where

import qualified Network.AWS.Kinesis.Types.SequenceNumber as Types
import qualified Network.AWS.Kinesis.Types.ShardIteratorType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- |
--
-- /See:/ 'mkStartingPosition' smart constructor.
data StartingPosition = StartingPosition'
  { -- | You can set the starting position to one of the following values:
    --
    -- @AT_SEQUENCE_NUMBER@ : Start streaming from the position denoted by the sequence number specified in the @SequenceNumber@ field.
    -- @AFTER_SEQUENCE_NUMBER@ : Start streaming right after the position denoted by the sequence number specified in the @SequenceNumber@ field.
    -- @AT_TIMESTAMP@ : Start streaming from the position denoted by the time stamp specified in the @Timestamp@ field.
    -- @TRIM_HORIZON@ : Start streaming at the last untrimmed record in the shard, which is the oldest data record in the shard.
    -- @LATEST@ : Start streaming just after the most recent record in the shard, so that you always read the most recent data in the shard.
    type' :: Types.ShardIteratorType,
    -- | The sequence number of the data record in the shard from which to start streaming. To specify a sequence number, set @StartingPosition@ to @AT_SEQUENCE_NUMBER@ or @AFTER_SEQUENCE_NUMBER@ .
    sequenceNumber :: Core.Maybe Types.SequenceNumber,
    -- | The time stamp of the data record from which to start reading. To specify a time stamp, set @StartingPosition@ to @Type AT_TIMESTAMP@ . A time stamp is the Unix epoch date with precision in milliseconds. For example, @2016-04-04T19:58:46.480-00:00@ or @1459799926.480@ . If a record with this exact time stamp does not exist, records will be streamed from the next (later) record. If the time stamp is older than the current trim horizon, records will be streamed from the oldest untrimmed data record (@TRIM_HORIZON@ ).
    timestamp :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StartingPosition' value with any optional fields omitted.
mkStartingPosition ::
  -- | 'type\''
  Types.ShardIteratorType ->
  StartingPosition
mkStartingPosition type' =
  StartingPosition'
    { type',
      sequenceNumber = Core.Nothing,
      timestamp = Core.Nothing
    }

-- | You can set the starting position to one of the following values:
--
-- @AT_SEQUENCE_NUMBER@ : Start streaming from the position denoted by the sequence number specified in the @SequenceNumber@ field.
-- @AFTER_SEQUENCE_NUMBER@ : Start streaming right after the position denoted by the sequence number specified in the @SequenceNumber@ field.
-- @AT_TIMESTAMP@ : Start streaming from the position denoted by the time stamp specified in the @Timestamp@ field.
-- @TRIM_HORIZON@ : Start streaming at the last untrimmed record in the shard, which is the oldest data record in the shard.
-- @LATEST@ : Start streaming just after the most recent record in the shard, so that you always read the most recent data in the shard.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spType :: Lens.Lens' StartingPosition Types.ShardIteratorType
spType = Lens.field @"type'"
{-# DEPRECATED spType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The sequence number of the data record in the shard from which to start streaming. To specify a sequence number, set @StartingPosition@ to @AT_SEQUENCE_NUMBER@ or @AFTER_SEQUENCE_NUMBER@ .
--
-- /Note:/ Consider using 'sequenceNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spSequenceNumber :: Lens.Lens' StartingPosition (Core.Maybe Types.SequenceNumber)
spSequenceNumber = Lens.field @"sequenceNumber"
{-# DEPRECATED spSequenceNumber "Use generic-lens or generic-optics with 'sequenceNumber' instead." #-}

-- | The time stamp of the data record from which to start reading. To specify a time stamp, set @StartingPosition@ to @Type AT_TIMESTAMP@ . A time stamp is the Unix epoch date with precision in milliseconds. For example, @2016-04-04T19:58:46.480-00:00@ or @1459799926.480@ . If a record with this exact time stamp does not exist, records will be streamed from the next (later) record. If the time stamp is older than the current trim horizon, records will be streamed from the oldest untrimmed data record (@TRIM_HORIZON@ ).
--
-- /Note:/ Consider using 'timestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spTimestamp :: Lens.Lens' StartingPosition (Core.Maybe Core.NominalDiffTime)
spTimestamp = Lens.field @"timestamp"
{-# DEPRECATED spTimestamp "Use generic-lens or generic-optics with 'timestamp' instead." #-}

instance Core.FromJSON StartingPosition where
  toJSON StartingPosition {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Type" Core..= type'),
            ("SequenceNumber" Core..=) Core.<$> sequenceNumber,
            ("Timestamp" Core..=) Core.<$> timestamp
          ]
      )
