{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.GetLoggingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the 'LoggingConfiguration' for the specified web ACL.
module Network.AWS.WAF.GetLoggingConfiguration
    (
    -- * Creating a request
      GetLoggingConfiguration (..)
    , mkGetLoggingConfiguration
    -- ** Request lenses
    , glcResourceArn

    -- * Destructuring the response
    , GetLoggingConfigurationResponse (..)
    , mkGetLoggingConfigurationResponse
    -- ** Response lenses
    , glcrrsLoggingConfiguration
    , glcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WAF.Types as Types

-- | /See:/ 'mkGetLoggingConfiguration' smart constructor.
newtype GetLoggingConfiguration = GetLoggingConfiguration'
  { resourceArn :: Types.ResourceArn
    -- ^ The Amazon Resource Name (ARN) of the web ACL for which you want to get the 'LoggingConfiguration' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetLoggingConfiguration' value with any optional fields omitted.
mkGetLoggingConfiguration
    :: Types.ResourceArn -- ^ 'resourceArn'
    -> GetLoggingConfiguration
mkGetLoggingConfiguration resourceArn
  = GetLoggingConfiguration'{resourceArn}

-- | The Amazon Resource Name (ARN) of the web ACL for which you want to get the 'LoggingConfiguration' .
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glcResourceArn :: Lens.Lens' GetLoggingConfiguration Types.ResourceArn
glcResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE glcResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

instance Core.ToQuery GetLoggingConfiguration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetLoggingConfiguration where
        toHeaders GetLoggingConfiguration{..}
          = Core.pure
              ("X-Amz-Target", "AWSWAF_20150824.GetLoggingConfiguration")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetLoggingConfiguration where
        toJSON GetLoggingConfiguration{..}
          = Core.object
              (Core.catMaybes [Core.Just ("ResourceArn" Core..= resourceArn)])

instance Core.AWSRequest GetLoggingConfiguration where
        type Rs GetLoggingConfiguration = GetLoggingConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetLoggingConfigurationResponse' Core.<$>
                   (x Core..:? "LoggingConfiguration") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetLoggingConfigurationResponse' smart constructor.
data GetLoggingConfigurationResponse = GetLoggingConfigurationResponse'
  { loggingConfiguration :: Core.Maybe Types.LoggingConfiguration
    -- ^ The 'LoggingConfiguration' for the specified web ACL.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetLoggingConfigurationResponse' value with any optional fields omitted.
mkGetLoggingConfigurationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetLoggingConfigurationResponse
mkGetLoggingConfigurationResponse responseStatus
  = GetLoggingConfigurationResponse'{loggingConfiguration =
                                       Core.Nothing,
                                     responseStatus}

-- | The 'LoggingConfiguration' for the specified web ACL.
--
-- /Note:/ Consider using 'loggingConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glcrrsLoggingConfiguration :: Lens.Lens' GetLoggingConfigurationResponse (Core.Maybe Types.LoggingConfiguration)
glcrrsLoggingConfiguration = Lens.field @"loggingConfiguration"
{-# INLINEABLE glcrrsLoggingConfiguration #-}
{-# DEPRECATED loggingConfiguration "Use generic-lens or generic-optics with 'loggingConfiguration' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glcrrsResponseStatus :: Lens.Lens' GetLoggingConfigurationResponse Core.Int
glcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE glcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
