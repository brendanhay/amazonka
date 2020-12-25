{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.GetV2LoggingOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the fine grained logging options.
module Network.AWS.IoT.GetV2LoggingOptions
  ( -- * Creating a request
    GetV2LoggingOptions (..),
    mkGetV2LoggingOptions,

    -- * Destructuring the response
    GetV2LoggingOptionsResponse (..),
    mkGetV2LoggingOptionsResponse,

    -- ** Response lenses
    gvlorrsDefaultLogLevel,
    gvlorrsDisableAllLogs,
    gvlorrsRoleArn,
    gvlorrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetV2LoggingOptions' smart constructor.
data GetV2LoggingOptions = GetV2LoggingOptions'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetV2LoggingOptions' value with any optional fields omitted.
mkGetV2LoggingOptions ::
  GetV2LoggingOptions
mkGetV2LoggingOptions = GetV2LoggingOptions'

instance Core.AWSRequest GetV2LoggingOptions where
  type Rs GetV2LoggingOptions = GetV2LoggingOptionsResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/v2LoggingOptions",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetV2LoggingOptionsResponse'
            Core.<$> (x Core..:? "defaultLogLevel")
            Core.<*> (x Core..:? "disableAllLogs")
            Core.<*> (x Core..:? "roleArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetV2LoggingOptionsResponse' smart constructor.
data GetV2LoggingOptionsResponse = GetV2LoggingOptionsResponse'
  { -- | The default log level.
    defaultLogLevel :: Core.Maybe Types.LogLevel,
    -- | Disables all logs.
    disableAllLogs :: Core.Maybe Core.Bool,
    -- | The IAM role ARN AWS IoT uses to write to your CloudWatch logs.
    roleArn :: Core.Maybe Types.AwsArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetV2LoggingOptionsResponse' value with any optional fields omitted.
mkGetV2LoggingOptionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetV2LoggingOptionsResponse
mkGetV2LoggingOptionsResponse responseStatus =
  GetV2LoggingOptionsResponse'
    { defaultLogLevel = Core.Nothing,
      disableAllLogs = Core.Nothing,
      roleArn = Core.Nothing,
      responseStatus
    }

-- | The default log level.
--
-- /Note:/ Consider using 'defaultLogLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvlorrsDefaultLogLevel :: Lens.Lens' GetV2LoggingOptionsResponse (Core.Maybe Types.LogLevel)
gvlorrsDefaultLogLevel = Lens.field @"defaultLogLevel"
{-# DEPRECATED gvlorrsDefaultLogLevel "Use generic-lens or generic-optics with 'defaultLogLevel' instead." #-}

-- | Disables all logs.
--
-- /Note:/ Consider using 'disableAllLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvlorrsDisableAllLogs :: Lens.Lens' GetV2LoggingOptionsResponse (Core.Maybe Core.Bool)
gvlorrsDisableAllLogs = Lens.field @"disableAllLogs"
{-# DEPRECATED gvlorrsDisableAllLogs "Use generic-lens or generic-optics with 'disableAllLogs' instead." #-}

-- | The IAM role ARN AWS IoT uses to write to your CloudWatch logs.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvlorrsRoleArn :: Lens.Lens' GetV2LoggingOptionsResponse (Core.Maybe Types.AwsArn)
gvlorrsRoleArn = Lens.field @"roleArn"
{-# DEPRECATED gvlorrsRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvlorrsResponseStatus :: Lens.Lens' GetV2LoggingOptionsResponse Core.Int
gvlorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gvlorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
