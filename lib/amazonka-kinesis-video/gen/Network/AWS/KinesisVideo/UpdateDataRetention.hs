{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideo.UpdateDataRetention
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Increases or decreases the stream's data retention period by the value that you specify. To indicate whether you want to increase or decrease the data retention period, specify the @Operation@ parameter in the request body. In the request, you must specify either the @StreamName@ or the @StreamARN@ .
--
-- This operation requires permission for the @KinesisVideo:UpdateDataRetention@ action.
-- Changing the data retention period affects the data in the stream as follows:
--
--     * If the data retention period is increased, existing data is retained for the new retention period. For example, if the data retention period is increased from one hour to seven hours, all existing data is retained for seven hours.
--
--
--     * If the data retention period is decreased, existing data is retained for the new retention period. For example, if the data retention period is decreased from seven hours to one hour, all existing data is retained for one hour, and any data older than one hour is deleted immediately.
module Network.AWS.KinesisVideo.UpdateDataRetention
  ( -- * Creating a request
    UpdateDataRetention (..),
    mkUpdateDataRetention,

    -- ** Request lenses
    udrCurrentVersion,
    udrOperation,
    udrDataRetentionChangeInHours,
    udrStreamARN,
    udrStreamName,

    -- * Destructuring the response
    UpdateDataRetentionResponse (..),
    mkUpdateDataRetentionResponse,

    -- ** Response lenses
    udrrrsResponseStatus,
  )
where

import qualified Network.AWS.KinesisVideo.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateDataRetention' smart constructor.
data UpdateDataRetention = UpdateDataRetention'
  { -- | The version of the stream whose retention period you want to change. To get the version, call either the @DescribeStream@ or the @ListStreams@ API.
    currentVersion :: Types.CurrentVersion,
    -- | Indicates whether you want to increase or decrease the retention period.
    operation :: Types.UpdateDataRetentionOperation,
    -- | The retention period, in hours. The value you specify replaces the current value. The maximum value for this parameter is 87600 (ten years).
    dataRetentionChangeInHours :: Core.Natural,
    -- | The Amazon Resource Name (ARN) of the stream whose retention period you want to change.
    streamARN :: Core.Maybe Types.StreamARN,
    -- | The name of the stream whose retention period you want to change.
    streamName :: Core.Maybe Types.StreamName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDataRetention' value with any optional fields omitted.
mkUpdateDataRetention ::
  -- | 'currentVersion'
  Types.CurrentVersion ->
  -- | 'operation'
  Types.UpdateDataRetentionOperation ->
  -- | 'dataRetentionChangeInHours'
  Core.Natural ->
  UpdateDataRetention
mkUpdateDataRetention
  currentVersion
  operation
  dataRetentionChangeInHours =
    UpdateDataRetention'
      { currentVersion,
        operation,
        dataRetentionChangeInHours,
        streamARN = Core.Nothing,
        streamName = Core.Nothing
      }

-- | The version of the stream whose retention period you want to change. To get the version, call either the @DescribeStream@ or the @ListStreams@ API.
--
-- /Note:/ Consider using 'currentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrCurrentVersion :: Lens.Lens' UpdateDataRetention Types.CurrentVersion
udrCurrentVersion = Lens.field @"currentVersion"
{-# DEPRECATED udrCurrentVersion "Use generic-lens or generic-optics with 'currentVersion' instead." #-}

-- | Indicates whether you want to increase or decrease the retention period.
--
-- /Note:/ Consider using 'operation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrOperation :: Lens.Lens' UpdateDataRetention Types.UpdateDataRetentionOperation
udrOperation = Lens.field @"operation"
{-# DEPRECATED udrOperation "Use generic-lens or generic-optics with 'operation' instead." #-}

-- | The retention period, in hours. The value you specify replaces the current value. The maximum value for this parameter is 87600 (ten years).
--
-- /Note:/ Consider using 'dataRetentionChangeInHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrDataRetentionChangeInHours :: Lens.Lens' UpdateDataRetention Core.Natural
udrDataRetentionChangeInHours = Lens.field @"dataRetentionChangeInHours"
{-# DEPRECATED udrDataRetentionChangeInHours "Use generic-lens or generic-optics with 'dataRetentionChangeInHours' instead." #-}

-- | The Amazon Resource Name (ARN) of the stream whose retention period you want to change.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrStreamARN :: Lens.Lens' UpdateDataRetention (Core.Maybe Types.StreamARN)
udrStreamARN = Lens.field @"streamARN"
{-# DEPRECATED udrStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | The name of the stream whose retention period you want to change.
--
-- /Note:/ Consider using 'streamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrStreamName :: Lens.Lens' UpdateDataRetention (Core.Maybe Types.StreamName)
udrStreamName = Lens.field @"streamName"
{-# DEPRECATED udrStreamName "Use generic-lens or generic-optics with 'streamName' instead." #-}

instance Core.FromJSON UpdateDataRetention where
  toJSON UpdateDataRetention {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("CurrentVersion" Core..= currentVersion),
            Core.Just ("Operation" Core..= operation),
            Core.Just
              ("DataRetentionChangeInHours" Core..= dataRetentionChangeInHours),
            ("StreamARN" Core..=) Core.<$> streamARN,
            ("StreamName" Core..=) Core.<$> streamName
          ]
      )

instance Core.AWSRequest UpdateDataRetention where
  type Rs UpdateDataRetention = UpdateDataRetentionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/updateDataRetention",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDataRetentionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateDataRetentionResponse' smart constructor.
newtype UpdateDataRetentionResponse = UpdateDataRetentionResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDataRetentionResponse' value with any optional fields omitted.
mkUpdateDataRetentionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateDataRetentionResponse
mkUpdateDataRetentionResponse responseStatus =
  UpdateDataRetentionResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrrrsResponseStatus :: Lens.Lens' UpdateDataRetentionResponse Core.Int
udrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED udrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
