{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.UpdateRealtimeLogConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a real-time log configuration.
--
-- When you update a real-time log configuration, all the parameters are updated with the values provided in the request. You cannot update some parameters independent of others. To update a real-time log configuration:
--
--     * Call @GetRealtimeLogConfig@ to get the current real-time log configuration.
--
--
--     * Locally modify the parameters in the real-time log configuration that you want to update.
--
--
--     * Call this API (@UpdateRealtimeLogConfig@ ) by providing the entire real-time log configuration, including the parameters that you modified and those that you didn’t.
--
--
-- You cannot update a real-time log configuration’s @Name@ or @ARN@ .
module Network.AWS.CloudFront.UpdateRealtimeLogConfig
  ( -- * Creating a request
    UpdateRealtimeLogConfig (..),
    mkUpdateRealtimeLogConfig,

    -- ** Request lenses
    urlcARN,
    urlcEndPoints,
    urlcFields,
    urlcName,
    urlcSamplingRate,

    -- * Destructuring the response
    UpdateRealtimeLogConfigResponse (..),
    mkUpdateRealtimeLogConfigResponse,

    -- ** Response lenses
    urlcrrsRealtimeLogConfig,
    urlcrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateRealtimeLogConfig' smart constructor.
data UpdateRealtimeLogConfig = UpdateRealtimeLogConfig'
  { -- | The Amazon Resource Name (ARN) for this real-time log configuration.
    arn :: Core.Maybe Types.String,
    -- | Contains information about the Amazon Kinesis data stream where you are sending real-time log data.
    endPoints :: Core.Maybe [Types.EndPoint],
    -- | A list of fields to include in each real-time log record.
    --
    -- For more information about fields, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-fields Real-time log configuration fields> in the /Amazon CloudFront Developer Guide/ .
    fields :: Core.Maybe [Types.String],
    -- | The name for this real-time log configuration.
    name :: Core.Maybe Types.String,
    -- | The sampling rate for this real-time log configuration. The sampling rate determines the percentage of viewer requests that are represented in the real-time log data. You must provide an integer between 1 and 100, inclusive.
    samplingRate :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRealtimeLogConfig' value with any optional fields omitted.
mkUpdateRealtimeLogConfig ::
  UpdateRealtimeLogConfig
mkUpdateRealtimeLogConfig =
  UpdateRealtimeLogConfig'
    { arn = Core.Nothing,
      endPoints = Core.Nothing,
      fields = Core.Nothing,
      name = Core.Nothing,
      samplingRate = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) for this real-time log configuration.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urlcARN :: Lens.Lens' UpdateRealtimeLogConfig (Core.Maybe Types.String)
urlcARN = Lens.field @"arn"
{-# DEPRECATED urlcARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Contains information about the Amazon Kinesis data stream where you are sending real-time log data.
--
-- /Note:/ Consider using 'endPoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urlcEndPoints :: Lens.Lens' UpdateRealtimeLogConfig (Core.Maybe [Types.EndPoint])
urlcEndPoints = Lens.field @"endPoints"
{-# DEPRECATED urlcEndPoints "Use generic-lens or generic-optics with 'endPoints' instead." #-}

-- | A list of fields to include in each real-time log record.
--
-- For more information about fields, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-fields Real-time log configuration fields> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'fields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urlcFields :: Lens.Lens' UpdateRealtimeLogConfig (Core.Maybe [Types.String])
urlcFields = Lens.field @"fields"
{-# DEPRECATED urlcFields "Use generic-lens or generic-optics with 'fields' instead." #-}

-- | The name for this real-time log configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urlcName :: Lens.Lens' UpdateRealtimeLogConfig (Core.Maybe Types.String)
urlcName = Lens.field @"name"
{-# DEPRECATED urlcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The sampling rate for this real-time log configuration. The sampling rate determines the percentage of viewer requests that are represented in the real-time log data. You must provide an integer between 1 and 100, inclusive.
--
-- /Note:/ Consider using 'samplingRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urlcSamplingRate :: Lens.Lens' UpdateRealtimeLogConfig (Core.Maybe Core.Integer)
urlcSamplingRate = Lens.field @"samplingRate"
{-# DEPRECATED urlcSamplingRate "Use generic-lens or generic-optics with 'samplingRate' instead." #-}

instance Core.ToXML UpdateRealtimeLogConfig where
  toXML UpdateRealtimeLogConfig {..} =
    Core.toXMLNode "ARN" Core.<$> arn
      Core.<> Core.toXMLNode
        "EndPoints"
        (Core.toXMLList "member" Core.<$> endPoints)
      Core.<> Core.toXMLNode "Fields" (Core.toXMLList "Field" Core.<$> fields)
      Core.<> Core.toXMLNode "Name" Core.<$> name
      Core.<> Core.toXMLNode "SamplingRate" Core.<$> samplingRate
  toXMLDocument =
    Core.mkXMLElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}UpdateRealtimeLogConfigRequest"

instance Core.AWSRequest UpdateRealtimeLogConfig where
  type Rs UpdateRealtimeLogConfig = UpdateRealtimeLogConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath = Core.rawPath "/2020-05-31/realtime-log-config/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toXMLBody x
      }
  response =
    Response.receiveXML
      ( \s h x ->
          UpdateRealtimeLogConfigResponse'
            Core.<$> (x Core..@? "RealtimeLogConfig")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateRealtimeLogConfigResponse' smart constructor.
data UpdateRealtimeLogConfigResponse = UpdateRealtimeLogConfigResponse'
  { -- | A real-time log configuration.
    realtimeLogConfig :: Core.Maybe Types.RealtimeLogConfig,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRealtimeLogConfigResponse' value with any optional fields omitted.
mkUpdateRealtimeLogConfigResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateRealtimeLogConfigResponse
mkUpdateRealtimeLogConfigResponse responseStatus =
  UpdateRealtimeLogConfigResponse'
    { realtimeLogConfig =
        Core.Nothing,
      responseStatus
    }

-- | A real-time log configuration.
--
-- /Note:/ Consider using 'realtimeLogConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urlcrrsRealtimeLogConfig :: Lens.Lens' UpdateRealtimeLogConfigResponse (Core.Maybe Types.RealtimeLogConfig)
urlcrrsRealtimeLogConfig = Lens.field @"realtimeLogConfig"
{-# DEPRECATED urlcrrsRealtimeLogConfig "Use generic-lens or generic-optics with 'realtimeLogConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urlcrrsResponseStatus :: Lens.Lens' UpdateRealtimeLogConfigResponse Core.Int
urlcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED urlcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
