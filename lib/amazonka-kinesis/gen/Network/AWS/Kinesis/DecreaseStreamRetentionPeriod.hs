{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.DecreaseStreamRetentionPeriod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Decreases the Kinesis data stream's retention period, which is the length of time data records are accessible after they are added to the stream. The minimum value of a stream's retention period is 24 hours.
--
-- This operation may result in lost data. For example, if the stream's retention period is 48 hours and is decreased to 24 hours, any data already in the stream that is older than 24 hours is inaccessible.
module Network.AWS.Kinesis.DecreaseStreamRetentionPeriod
  ( -- * Creating a request
    DecreaseStreamRetentionPeriod (..),
    mkDecreaseStreamRetentionPeriod,

    -- ** Request lenses
    dsrpStreamName,
    dsrpRetentionPeriodHours,

    -- * Destructuring the response
    DecreaseStreamRetentionPeriodResponse (..),
    mkDecreaseStreamRetentionPeriodResponse,
  )
where

import qualified Network.AWS.Kinesis.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for 'DecreaseStreamRetentionPeriod' .
--
-- /See:/ 'mkDecreaseStreamRetentionPeriod' smart constructor.
data DecreaseStreamRetentionPeriod = DecreaseStreamRetentionPeriod'
  { -- | The name of the stream to modify.
    streamName :: Types.StreamName,
    -- | The new retention period of the stream, in hours. Must be less than the current retention period.
    retentionPeriodHours :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DecreaseStreamRetentionPeriod' value with any optional fields omitted.
mkDecreaseStreamRetentionPeriod ::
  -- | 'streamName'
  Types.StreamName ->
  -- | 'retentionPeriodHours'
  Core.Int ->
  DecreaseStreamRetentionPeriod
mkDecreaseStreamRetentionPeriod streamName retentionPeriodHours =
  DecreaseStreamRetentionPeriod' {streamName, retentionPeriodHours}

-- | The name of the stream to modify.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrpStreamName :: Lens.Lens' DecreaseStreamRetentionPeriod Types.StreamName
dsrpStreamName = Lens.field @"streamName"
{-# DEPRECATED dsrpStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

-- | The new retention period of the stream, in hours. Must be less than the current retention period.
--
-- /Note:/ Consider using 'retentionPeriodHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrpRetentionPeriodHours :: Lens.Lens' DecreaseStreamRetentionPeriod Core.Int
dsrpRetentionPeriodHours = Lens.field @"retentionPeriodHours"
{-# DEPRECATED dsrpRetentionPeriodHours "Use generic-lens or generic-optics with 'retentionPeriodHours' instead." #-}

instance Core.FromJSON DecreaseStreamRetentionPeriod where
  toJSON DecreaseStreamRetentionPeriod {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StreamName" Core..= streamName),
            Core.Just ("RetentionPeriodHours" Core..= retentionPeriodHours)
          ]
      )

instance Core.AWSRequest DecreaseStreamRetentionPeriod where
  type
    Rs DecreaseStreamRetentionPeriod =
      DecreaseStreamRetentionPeriodResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Kinesis_20131202.DecreaseStreamRetentionPeriod")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveNull DecreaseStreamRetentionPeriodResponse'

-- | /See:/ 'mkDecreaseStreamRetentionPeriodResponse' smart constructor.
data DecreaseStreamRetentionPeriodResponse = DecreaseStreamRetentionPeriodResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DecreaseStreamRetentionPeriodResponse' value with any optional fields omitted.
mkDecreaseStreamRetentionPeriodResponse ::
  DecreaseStreamRetentionPeriodResponse
mkDecreaseStreamRetentionPeriodResponse =
  DecreaseStreamRetentionPeriodResponse'
