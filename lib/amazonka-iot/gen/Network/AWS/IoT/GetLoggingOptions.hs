{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.GetLoggingOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the logging options.
--
-- NOTE: use of this command is not recommended. Use @GetV2LoggingOptions@ instead.
module Network.AWS.IoT.GetLoggingOptions
  ( -- * Creating a request
    GetLoggingOptions (..),
    mkGetLoggingOptions,

    -- * Destructuring the response
    GetLoggingOptionsResponse (..),
    mkGetLoggingOptionsResponse,

    -- ** Response lenses
    glorrsLogLevel,
    glorrsRoleArn,
    glorrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the GetLoggingOptions operation.
--
-- /See:/ 'mkGetLoggingOptions' smart constructor.
data GetLoggingOptions = GetLoggingOptions'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetLoggingOptions' value with any optional fields omitted.
mkGetLoggingOptions ::
  GetLoggingOptions
mkGetLoggingOptions = GetLoggingOptions'

instance Core.AWSRequest GetLoggingOptions where
  type Rs GetLoggingOptions = GetLoggingOptionsResponse
  request x@_ =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/loggingOptions",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLoggingOptionsResponse'
            Core.<$> (x Core..:? "logLevel")
            Core.<*> (x Core..:? "roleArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The output from the GetLoggingOptions operation.
--
-- /See:/ 'mkGetLoggingOptionsResponse' smart constructor.
data GetLoggingOptionsResponse = GetLoggingOptionsResponse'
  { -- | The logging level.
    logLevel :: Core.Maybe Types.LogLevel,
    -- | The ARN of the IAM role that grants access.
    roleArn :: Core.Maybe Types.AwsArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetLoggingOptionsResponse' value with any optional fields omitted.
mkGetLoggingOptionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetLoggingOptionsResponse
mkGetLoggingOptionsResponse responseStatus =
  GetLoggingOptionsResponse'
    { logLevel = Core.Nothing,
      roleArn = Core.Nothing,
      responseStatus
    }

-- | The logging level.
--
-- /Note:/ Consider using 'logLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glorrsLogLevel :: Lens.Lens' GetLoggingOptionsResponse (Core.Maybe Types.LogLevel)
glorrsLogLevel = Lens.field @"logLevel"
{-# DEPRECATED glorrsLogLevel "Use generic-lens or generic-optics with 'logLevel' instead." #-}

-- | The ARN of the IAM role that grants access.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glorrsRoleArn :: Lens.Lens' GetLoggingOptionsResponse (Core.Maybe Types.AwsArn)
glorrsRoleArn = Lens.field @"roleArn"
{-# DEPRECATED glorrsRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glorrsResponseStatus :: Lens.Lens' GetLoggingOptionsResponse Core.Int
glorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED glorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
