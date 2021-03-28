{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.UpdateAppImageConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the properties of an AppImageConfig.
module Network.AWS.SageMaker.UpdateAppImageConfig
    (
    -- * Creating a request
      UpdateAppImageConfig (..)
    , mkUpdateAppImageConfig
    -- ** Request lenses
    , uaicAppImageConfigName
    , uaicKernelGatewayImageConfig

    -- * Destructuring the response
    , UpdateAppImageConfigResponse (..)
    , mkUpdateAppImageConfigResponse
    -- ** Response lenses
    , uaicrrsAppImageConfigArn
    , uaicrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkUpdateAppImageConfig' smart constructor.
data UpdateAppImageConfig = UpdateAppImageConfig'
  { appImageConfigName :: Types.AppImageConfigName
    -- ^ The name of the AppImageConfig to update.
  , kernelGatewayImageConfig :: Core.Maybe Types.KernelGatewayImageConfig
    -- ^ The new KernelGateway app to run on the image.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAppImageConfig' value with any optional fields omitted.
mkUpdateAppImageConfig
    :: Types.AppImageConfigName -- ^ 'appImageConfigName'
    -> UpdateAppImageConfig
mkUpdateAppImageConfig appImageConfigName
  = UpdateAppImageConfig'{appImageConfigName,
                          kernelGatewayImageConfig = Core.Nothing}

-- | The name of the AppImageConfig to update.
--
-- /Note:/ Consider using 'appImageConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaicAppImageConfigName :: Lens.Lens' UpdateAppImageConfig Types.AppImageConfigName
uaicAppImageConfigName = Lens.field @"appImageConfigName"
{-# INLINEABLE uaicAppImageConfigName #-}
{-# DEPRECATED appImageConfigName "Use generic-lens or generic-optics with 'appImageConfigName' instead"  #-}

-- | The new KernelGateway app to run on the image.
--
-- /Note:/ Consider using 'kernelGatewayImageConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaicKernelGatewayImageConfig :: Lens.Lens' UpdateAppImageConfig (Core.Maybe Types.KernelGatewayImageConfig)
uaicKernelGatewayImageConfig = Lens.field @"kernelGatewayImageConfig"
{-# INLINEABLE uaicKernelGatewayImageConfig #-}
{-# DEPRECATED kernelGatewayImageConfig "Use generic-lens or generic-optics with 'kernelGatewayImageConfig' instead"  #-}

instance Core.ToQuery UpdateAppImageConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateAppImageConfig where
        toHeaders UpdateAppImageConfig{..}
          = Core.pure ("X-Amz-Target", "SageMaker.UpdateAppImageConfig")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateAppImageConfig where
        toJSON UpdateAppImageConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AppImageConfigName" Core..= appImageConfigName),
                  ("KernelGatewayImageConfig" Core..=) Core.<$>
                    kernelGatewayImageConfig])

instance Core.AWSRequest UpdateAppImageConfig where
        type Rs UpdateAppImageConfig = UpdateAppImageConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateAppImageConfigResponse' Core.<$>
                   (x Core..:? "AppImageConfigArn") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateAppImageConfigResponse' smart constructor.
data UpdateAppImageConfigResponse = UpdateAppImageConfigResponse'
  { appImageConfigArn :: Core.Maybe Types.AppImageConfigArn
    -- ^ The Amazon Resource Name (ARN) for the AppImageConfig.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateAppImageConfigResponse' value with any optional fields omitted.
mkUpdateAppImageConfigResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateAppImageConfigResponse
mkUpdateAppImageConfigResponse responseStatus
  = UpdateAppImageConfigResponse'{appImageConfigArn = Core.Nothing,
                                  responseStatus}

-- | The Amazon Resource Name (ARN) for the AppImageConfig.
--
-- /Note:/ Consider using 'appImageConfigArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaicrrsAppImageConfigArn :: Lens.Lens' UpdateAppImageConfigResponse (Core.Maybe Types.AppImageConfigArn)
uaicrrsAppImageConfigArn = Lens.field @"appImageConfigArn"
{-# INLINEABLE uaicrrsAppImageConfigArn #-}
{-# DEPRECATED appImageConfigArn "Use generic-lens or generic-optics with 'appImageConfigArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaicrrsResponseStatus :: Lens.Lens' UpdateAppImageConfigResponse Core.Int
uaicrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uaicrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
