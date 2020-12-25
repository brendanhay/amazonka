{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.GetRealtimeLogConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a real-time log configuration.
--
-- To get a real-time log configuration, you can provide the configuration’s name or its Amazon Resource Name (ARN). You must provide at least one. If you provide both, CloudFront uses the name to identify the real-time log configuration to get.
module Network.AWS.CloudFront.GetRealtimeLogConfig
  ( -- * Creating a request
    GetRealtimeLogConfig (..),
    mkGetRealtimeLogConfig,

    -- ** Request lenses
    grlcARN,
    grlcName,

    -- * Destructuring the response
    GetRealtimeLogConfigResponse (..),
    mkGetRealtimeLogConfigResponse,

    -- ** Response lenses
    grlcrrsRealtimeLogConfig,
    grlcrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetRealtimeLogConfig' smart constructor.
data GetRealtimeLogConfig = GetRealtimeLogConfig'
  { -- | The Amazon Resource Name (ARN) of the real-time log configuration to get.
    arn :: Core.Maybe Types.String,
    -- | The name of the real-time log configuration to get.
    name :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRealtimeLogConfig' value with any optional fields omitted.
mkGetRealtimeLogConfig ::
  GetRealtimeLogConfig
mkGetRealtimeLogConfig =
  GetRealtimeLogConfig' {arn = Core.Nothing, name = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the real-time log configuration to get.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grlcARN :: Lens.Lens' GetRealtimeLogConfig (Core.Maybe Types.String)
grlcARN = Lens.field @"arn"
{-# DEPRECATED grlcARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the real-time log configuration to get.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grlcName :: Lens.Lens' GetRealtimeLogConfig (Core.Maybe Types.String)
grlcName = Lens.field @"name"
{-# DEPRECATED grlcName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.ToXML GetRealtimeLogConfig where
  toXML GetRealtimeLogConfig {..} =
    Core.toXMLNode "ARN" Core.<$> arn
      Core.<> Core.toXMLNode "Name" Core.<$> name
  toXMLDocument =
    Core.mkXMLElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}GetRealtimeLogConfigRequest"

instance Core.AWSRequest GetRealtimeLogConfig where
  type Rs GetRealtimeLogConfig = GetRealtimeLogConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/2020-05-31/get-realtime-log-config/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toXMLBody x
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetRealtimeLogConfigResponse'
            Core.<$> (x Core..@? "RealtimeLogConfig")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetRealtimeLogConfigResponse' smart constructor.
data GetRealtimeLogConfigResponse = GetRealtimeLogConfigResponse'
  { -- | A real-time log configuration.
    realtimeLogConfig :: Core.Maybe Types.RealtimeLogConfig,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRealtimeLogConfigResponse' value with any optional fields omitted.
mkGetRealtimeLogConfigResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetRealtimeLogConfigResponse
mkGetRealtimeLogConfigResponse responseStatus =
  GetRealtimeLogConfigResponse'
    { realtimeLogConfig = Core.Nothing,
      responseStatus
    }

-- | A real-time log configuration.
--
-- /Note:/ Consider using 'realtimeLogConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grlcrrsRealtimeLogConfig :: Lens.Lens' GetRealtimeLogConfigResponse (Core.Maybe Types.RealtimeLogConfig)
grlcrrsRealtimeLogConfig = Lens.field @"realtimeLogConfig"
{-# DEPRECATED grlcrrsRealtimeLogConfig "Use generic-lens or generic-optics with 'realtimeLogConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grlcrrsResponseStatus :: Lens.Lens' GetRealtimeLogConfigResponse Core.Int
grlcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grlcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
