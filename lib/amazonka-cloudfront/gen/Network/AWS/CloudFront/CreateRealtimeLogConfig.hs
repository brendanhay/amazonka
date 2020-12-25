{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.CreateRealtimeLogConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a real-time log configuration.
--
-- After you create a real-time log configuration, you can attach it to one or more cache behaviors to send real-time log data to the specified Amazon Kinesis data stream.
-- For more information about real-time log configurations, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html Real-time logs> in the /Amazon CloudFront Developer Guide/ .
module Network.AWS.CloudFront.CreateRealtimeLogConfig
  ( -- * Creating a request
    CreateRealtimeLogConfig (..),
    mkCreateRealtimeLogConfig,

    -- ** Request lenses
    crlcEndPoints,
    crlcFields,
    crlcName,
    crlcSamplingRate,

    -- * Destructuring the response
    CreateRealtimeLogConfigResponse (..),
    mkCreateRealtimeLogConfigResponse,

    -- ** Response lenses
    crlcrrsRealtimeLogConfig,
    crlcrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateRealtimeLogConfig' smart constructor.
data CreateRealtimeLogConfig = CreateRealtimeLogConfig'
  { -- | Contains information about the Amazon Kinesis data stream where you are sending real-time log data.
    endPoints :: [Types.EndPoint],
    -- | A list of fields to include in each real-time log record.
    --
    -- For more information about fields, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-fields Real-time log configuration fields> in the /Amazon CloudFront Developer Guide/ .
    fields :: [Types.String],
    -- | A unique name to identify this real-time log configuration.
    name :: Types.String,
    -- | The sampling rate for this real-time log configuration. The sampling rate determines the percentage of viewer requests that are represented in the real-time log data. You must provide an integer between 1 and 100, inclusive.
    samplingRate :: Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRealtimeLogConfig' value with any optional fields omitted.
mkCreateRealtimeLogConfig ::
  -- | 'name'
  Types.String ->
  -- | 'samplingRate'
  Core.Integer ->
  CreateRealtimeLogConfig
mkCreateRealtimeLogConfig name samplingRate =
  CreateRealtimeLogConfig'
    { endPoints = Core.mempty,
      fields = Core.mempty,
      name,
      samplingRate
    }

-- | Contains information about the Amazon Kinesis data stream where you are sending real-time log data.
--
-- /Note:/ Consider using 'endPoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crlcEndPoints :: Lens.Lens' CreateRealtimeLogConfig [Types.EndPoint]
crlcEndPoints = Lens.field @"endPoints"
{-# DEPRECATED crlcEndPoints "Use generic-lens or generic-optics with 'endPoints' instead." #-}

-- | A list of fields to include in each real-time log record.
--
-- For more information about fields, see <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/real-time-logs.html#understand-real-time-log-config-fields Real-time log configuration fields> in the /Amazon CloudFront Developer Guide/ .
--
-- /Note:/ Consider using 'fields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crlcFields :: Lens.Lens' CreateRealtimeLogConfig [Types.String]
crlcFields = Lens.field @"fields"
{-# DEPRECATED crlcFields "Use generic-lens or generic-optics with 'fields' instead." #-}

-- | A unique name to identify this real-time log configuration.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crlcName :: Lens.Lens' CreateRealtimeLogConfig Types.String
crlcName = Lens.field @"name"
{-# DEPRECATED crlcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The sampling rate for this real-time log configuration. The sampling rate determines the percentage of viewer requests that are represented in the real-time log data. You must provide an integer between 1 and 100, inclusive.
--
-- /Note:/ Consider using 'samplingRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crlcSamplingRate :: Lens.Lens' CreateRealtimeLogConfig Core.Integer
crlcSamplingRate = Lens.field @"samplingRate"
{-# DEPRECATED crlcSamplingRate "Use generic-lens or generic-optics with 'samplingRate' instead." #-}

instance Core.ToXML CreateRealtimeLogConfig where
  toXML CreateRealtimeLogConfig {..} =
    Core.toXMLNode "EndPoints" (Core.toXMLList "member" endPoints)
      Core.<> Core.toXMLNode "Fields" (Core.toXMLList "Field" fields)
      Core.<> Core.toXMLNode "Name" name
      Core.<> Core.toXMLNode "SamplingRate" samplingRate
  toXMLDocument =
    Core.mkXMLElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}CreateRealtimeLogConfigRequest"

instance Core.AWSRequest CreateRealtimeLogConfig where
  type Rs CreateRealtimeLogConfig = CreateRealtimeLogConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/2020-05-31/realtime-log-config",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toXMLBody x
      }
  response =
    Response.receiveXML
      ( \s h x ->
          CreateRealtimeLogConfigResponse'
            Core.<$> (x Core..@? "RealtimeLogConfig")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateRealtimeLogConfigResponse' smart constructor.
data CreateRealtimeLogConfigResponse = CreateRealtimeLogConfigResponse'
  { -- | A real-time log configuration.
    realtimeLogConfig :: Core.Maybe Types.RealtimeLogConfig,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRealtimeLogConfigResponse' value with any optional fields omitted.
mkCreateRealtimeLogConfigResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateRealtimeLogConfigResponse
mkCreateRealtimeLogConfigResponse responseStatus =
  CreateRealtimeLogConfigResponse'
    { realtimeLogConfig =
        Core.Nothing,
      responseStatus
    }

-- | A real-time log configuration.
--
-- /Note:/ Consider using 'realtimeLogConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crlcrrsRealtimeLogConfig :: Lens.Lens' CreateRealtimeLogConfigResponse (Core.Maybe Types.RealtimeLogConfig)
crlcrrsRealtimeLogConfig = Lens.field @"realtimeLogConfig"
{-# DEPRECATED crlcrrsRealtimeLogConfig "Use generic-lens or generic-optics with 'realtimeLogConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crlcrrsResponseStatus :: Lens.Lens' CreateRealtimeLogConfigResponse Core.Int
crlcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crlcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
