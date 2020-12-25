{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.IncreaseStreamRetentionPeriod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Increases the Kinesis data stream's retention period, which is the length of time data records are accessible after they are added to the stream. The maximum value of a stream's retention period is 168 hours (7 days).
--
-- If you choose a longer stream retention period, this operation increases the time period during which records that have not yet expired are accessible. However, it does not make previous, expired data (older than the stream's previous retention period) accessible after the operation has been called. For example, if a stream's retention period is set to 24 hours and is increased to 168 hours, any data that is older than 24 hours remains inaccessible to consumer applications.
module Network.AWS.Kinesis.IncreaseStreamRetentionPeriod
  ( -- * Creating a request
    IncreaseStreamRetentionPeriod (..),
    mkIncreaseStreamRetentionPeriod,

    -- ** Request lenses
    isrpStreamName,
    isrpRetentionPeriodHours,

    -- * Destructuring the response
    IncreaseStreamRetentionPeriodResponse (..),
    mkIncreaseStreamRetentionPeriodResponse,
  )
where

import qualified Network.AWS.Kinesis.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for 'IncreaseStreamRetentionPeriod' .
--
-- /See:/ 'mkIncreaseStreamRetentionPeriod' smart constructor.
data IncreaseStreamRetentionPeriod = IncreaseStreamRetentionPeriod'
  { -- | The name of the stream to modify.
    streamName :: Types.StreamName,
    -- | The new retention period of the stream, in hours. Must be more than the current retention period.
    retentionPeriodHours :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IncreaseStreamRetentionPeriod' value with any optional fields omitted.
mkIncreaseStreamRetentionPeriod ::
  -- | 'streamName'
  Types.StreamName ->
  -- | 'retentionPeriodHours'
  Core.Int ->
  IncreaseStreamRetentionPeriod
mkIncreaseStreamRetentionPeriod streamName retentionPeriodHours =
  IncreaseStreamRetentionPeriod' {streamName, retentionPeriodHours}

-- | The name of the stream to modify.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isrpStreamName :: Lens.Lens' IncreaseStreamRetentionPeriod Types.StreamName
isrpStreamName = Lens.field @"streamName"
{-# DEPRECATED isrpStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

-- | The new retention period of the stream, in hours. Must be more than the current retention period.
--
-- /Note:/ Consider using 'retentionPeriodHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isrpRetentionPeriodHours :: Lens.Lens' IncreaseStreamRetentionPeriod Core.Int
isrpRetentionPeriodHours = Lens.field @"retentionPeriodHours"
{-# DEPRECATED isrpRetentionPeriodHours "Use generic-lens or generic-optics with 'retentionPeriodHours' instead." #-}

instance Core.FromJSON IncreaseStreamRetentionPeriod where
  toJSON IncreaseStreamRetentionPeriod {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StreamName" Core..= streamName),
            Core.Just ("RetentionPeriodHours" Core..= retentionPeriodHours)
          ]
      )

instance Core.AWSRequest IncreaseStreamRetentionPeriod where
  type
    Rs IncreaseStreamRetentionPeriod =
      IncreaseStreamRetentionPeriodResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Kinesis_20131202.IncreaseStreamRetentionPeriod")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveNull IncreaseStreamRetentionPeriodResponse'

-- | /See:/ 'mkIncreaseStreamRetentionPeriodResponse' smart constructor.
data IncreaseStreamRetentionPeriodResponse = IncreaseStreamRetentionPeriodResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'IncreaseStreamRetentionPeriodResponse' value with any optional fields omitted.
mkIncreaseStreamRetentionPeriodResponse ::
  IncreaseStreamRetentionPeriodResponse
mkIncreaseStreamRetentionPeriodResponse =
  IncreaseStreamRetentionPeriodResponse'
