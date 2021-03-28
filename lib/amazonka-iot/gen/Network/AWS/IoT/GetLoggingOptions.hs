{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetLoggingOptions (..)
    , mkGetLoggingOptions

    -- * Destructuring the response
    , GetLoggingOptionsResponse (..)
    , mkGetLoggingOptionsResponse
    -- ** Response lenses
    , glorrsLogLevel
    , glorrsRoleArn
    , glorrsResponseStatus
    ) where

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
mkGetLoggingOptions
    :: GetLoggingOptions
mkGetLoggingOptions = GetLoggingOptions'

instance Core.ToQuery GetLoggingOptions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetLoggingOptions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetLoggingOptions where
        type Rs GetLoggingOptions = GetLoggingOptionsResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/loggingOptions",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetLoggingOptionsResponse' Core.<$>
                   (x Core..:? "logLevel") Core.<*> x Core..:? "roleArn" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | The output from the GetLoggingOptions operation.
--
-- /See:/ 'mkGetLoggingOptionsResponse' smart constructor.
data GetLoggingOptionsResponse = GetLoggingOptionsResponse'
  { logLevel :: Core.Maybe Types.LogLevel
    -- ^ The logging level.
  , roleArn :: Core.Maybe Types.AwsArn
    -- ^ The ARN of the IAM role that grants access.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetLoggingOptionsResponse' value with any optional fields omitted.
mkGetLoggingOptionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetLoggingOptionsResponse
mkGetLoggingOptionsResponse responseStatus
  = GetLoggingOptionsResponse'{logLevel = Core.Nothing,
                               roleArn = Core.Nothing, responseStatus}

-- | The logging level.
--
-- /Note:/ Consider using 'logLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glorrsLogLevel :: Lens.Lens' GetLoggingOptionsResponse (Core.Maybe Types.LogLevel)
glorrsLogLevel = Lens.field @"logLevel"
{-# INLINEABLE glorrsLogLevel #-}
{-# DEPRECATED logLevel "Use generic-lens or generic-optics with 'logLevel' instead"  #-}

-- | The ARN of the IAM role that grants access.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glorrsRoleArn :: Lens.Lens' GetLoggingOptionsResponse (Core.Maybe Types.AwsArn)
glorrsRoleArn = Lens.field @"roleArn"
{-# INLINEABLE glorrsRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glorrsResponseStatus :: Lens.Lens' GetLoggingOptionsResponse Core.Int
glorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE glorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
