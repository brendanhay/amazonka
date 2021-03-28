{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.SetV2LoggingOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the logging options for the V2 logging service.
module Network.AWS.IoT.SetV2LoggingOptions
    (
    -- * Creating a request
      SetV2LoggingOptions (..)
    , mkSetV2LoggingOptions
    -- ** Request lenses
    , svloDefaultLogLevel
    , svloDisableAllLogs
    , svloRoleArn

    -- * Destructuring the response
    , SetV2LoggingOptionsResponse (..)
    , mkSetV2LoggingOptionsResponse
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSetV2LoggingOptions' smart constructor.
data SetV2LoggingOptions = SetV2LoggingOptions'
  { defaultLogLevel :: Core.Maybe Types.LogLevel
    -- ^ The default logging level.
  , disableAllLogs :: Core.Maybe Core.Bool
    -- ^ If true all logs are disabled. The default is false.
  , roleArn :: Core.Maybe Types.AwsArn
    -- ^ The ARN of the role that allows IoT to write to Cloudwatch logs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetV2LoggingOptions' value with any optional fields omitted.
mkSetV2LoggingOptions
    :: SetV2LoggingOptions
mkSetV2LoggingOptions
  = SetV2LoggingOptions'{defaultLogLevel = Core.Nothing,
                         disableAllLogs = Core.Nothing, roleArn = Core.Nothing}

-- | The default logging level.
--
-- /Note:/ Consider using 'defaultLogLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svloDefaultLogLevel :: Lens.Lens' SetV2LoggingOptions (Core.Maybe Types.LogLevel)
svloDefaultLogLevel = Lens.field @"defaultLogLevel"
{-# INLINEABLE svloDefaultLogLevel #-}
{-# DEPRECATED defaultLogLevel "Use generic-lens or generic-optics with 'defaultLogLevel' instead"  #-}

-- | If true all logs are disabled. The default is false.
--
-- /Note:/ Consider using 'disableAllLogs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svloDisableAllLogs :: Lens.Lens' SetV2LoggingOptions (Core.Maybe Core.Bool)
svloDisableAllLogs = Lens.field @"disableAllLogs"
{-# INLINEABLE svloDisableAllLogs #-}
{-# DEPRECATED disableAllLogs "Use generic-lens or generic-optics with 'disableAllLogs' instead"  #-}

-- | The ARN of the role that allows IoT to write to Cloudwatch logs.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svloRoleArn :: Lens.Lens' SetV2LoggingOptions (Core.Maybe Types.AwsArn)
svloRoleArn = Lens.field @"roleArn"
{-# INLINEABLE svloRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

instance Core.ToQuery SetV2LoggingOptions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SetV2LoggingOptions where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON SetV2LoggingOptions where
        toJSON SetV2LoggingOptions{..}
          = Core.object
              (Core.catMaybes
                 [("defaultLogLevel" Core..=) Core.<$> defaultLogLevel,
                  ("disableAllLogs" Core..=) Core.<$> disableAllLogs,
                  ("roleArn" Core..=) Core.<$> roleArn])

instance Core.AWSRequest SetV2LoggingOptions where
        type Rs SetV2LoggingOptions = SetV2LoggingOptionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/v2LoggingOptions",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull SetV2LoggingOptionsResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSetV2LoggingOptionsResponse' smart constructor.
data SetV2LoggingOptionsResponse = SetV2LoggingOptionsResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetV2LoggingOptionsResponse' value with any optional fields omitted.
mkSetV2LoggingOptionsResponse
    :: SetV2LoggingOptionsResponse
mkSetV2LoggingOptionsResponse = SetV2LoggingOptionsResponse'
