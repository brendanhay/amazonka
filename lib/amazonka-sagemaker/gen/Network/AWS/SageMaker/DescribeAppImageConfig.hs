{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeAppImageConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an AppImageConfig.
module Network.AWS.SageMaker.DescribeAppImageConfig
    (
    -- * Creating a request
      DescribeAppImageConfig (..)
    , mkDescribeAppImageConfig
    -- ** Request lenses
    , dAppImageConfigName

    -- * Destructuring the response
    , DescribeAppImageConfigResponse (..)
    , mkDescribeAppImageConfigResponse
    -- ** Response lenses
    , daicrrsAppImageConfigArn
    , daicrrsAppImageConfigName
    , daicrrsCreationTime
    , daicrrsKernelGatewayImageConfig
    , daicrrsLastModifiedTime
    , daicrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeAppImageConfig' smart constructor.
newtype DescribeAppImageConfig = DescribeAppImageConfig'
  { appImageConfigName :: Types.AppImageConfigName
    -- ^ The name of the AppImageConfig to describe.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAppImageConfig' value with any optional fields omitted.
mkDescribeAppImageConfig
    :: Types.AppImageConfigName -- ^ 'appImageConfigName'
    -> DescribeAppImageConfig
mkDescribeAppImageConfig appImageConfigName
  = DescribeAppImageConfig'{appImageConfigName}

-- | The name of the AppImageConfig to describe.
--
-- /Note:/ Consider using 'appImageConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAppImageConfigName :: Lens.Lens' DescribeAppImageConfig Types.AppImageConfigName
dAppImageConfigName = Lens.field @"appImageConfigName"
{-# INLINEABLE dAppImageConfigName #-}
{-# DEPRECATED appImageConfigName "Use generic-lens or generic-optics with 'appImageConfigName' instead"  #-}

instance Core.ToQuery DescribeAppImageConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeAppImageConfig where
        toHeaders DescribeAppImageConfig{..}
          = Core.pure ("X-Amz-Target", "SageMaker.DescribeAppImageConfig")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeAppImageConfig where
        toJSON DescribeAppImageConfig{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AppImageConfigName" Core..= appImageConfigName)])

instance Core.AWSRequest DescribeAppImageConfig where
        type Rs DescribeAppImageConfig = DescribeAppImageConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeAppImageConfigResponse' Core.<$>
                   (x Core..:? "AppImageConfigArn") Core.<*>
                     x Core..:? "AppImageConfigName"
                     Core.<*> x Core..:? "CreationTime"
                     Core.<*> x Core..:? "KernelGatewayImageConfig"
                     Core.<*> x Core..:? "LastModifiedTime"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeAppImageConfigResponse' smart constructor.
data DescribeAppImageConfigResponse = DescribeAppImageConfigResponse'
  { appImageConfigArn :: Core.Maybe Types.AppImageConfigArn
    -- ^ The Amazon Resource Name (ARN) of the AppImageConfig.
  , appImageConfigName :: Core.Maybe Types.AppImageConfigName
    -- ^ The name of the AppImageConfig.
  , creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the AppImageConfig was created.
  , kernelGatewayImageConfig :: Core.Maybe Types.KernelGatewayImageConfig
    -- ^ The configuration of a KernelGateway app.
  , lastModifiedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ When the AppImageConfig was last modified.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeAppImageConfigResponse' value with any optional fields omitted.
mkDescribeAppImageConfigResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeAppImageConfigResponse
mkDescribeAppImageConfigResponse responseStatus
  = DescribeAppImageConfigResponse'{appImageConfigArn = Core.Nothing,
                                    appImageConfigName = Core.Nothing, creationTime = Core.Nothing,
                                    kernelGatewayImageConfig = Core.Nothing,
                                    lastModifiedTime = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the AppImageConfig.
--
-- /Note:/ Consider using 'appImageConfigArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daicrrsAppImageConfigArn :: Lens.Lens' DescribeAppImageConfigResponse (Core.Maybe Types.AppImageConfigArn)
daicrrsAppImageConfigArn = Lens.field @"appImageConfigArn"
{-# INLINEABLE daicrrsAppImageConfigArn #-}
{-# DEPRECATED appImageConfigArn "Use generic-lens or generic-optics with 'appImageConfigArn' instead"  #-}

-- | The name of the AppImageConfig.
--
-- /Note:/ Consider using 'appImageConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daicrrsAppImageConfigName :: Lens.Lens' DescribeAppImageConfigResponse (Core.Maybe Types.AppImageConfigName)
daicrrsAppImageConfigName = Lens.field @"appImageConfigName"
{-# INLINEABLE daicrrsAppImageConfigName #-}
{-# DEPRECATED appImageConfigName "Use generic-lens or generic-optics with 'appImageConfigName' instead"  #-}

-- | When the AppImageConfig was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daicrrsCreationTime :: Lens.Lens' DescribeAppImageConfigResponse (Core.Maybe Core.NominalDiffTime)
daicrrsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE daicrrsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The configuration of a KernelGateway app.
--
-- /Note:/ Consider using 'kernelGatewayImageConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daicrrsKernelGatewayImageConfig :: Lens.Lens' DescribeAppImageConfigResponse (Core.Maybe Types.KernelGatewayImageConfig)
daicrrsKernelGatewayImageConfig = Lens.field @"kernelGatewayImageConfig"
{-# INLINEABLE daicrrsKernelGatewayImageConfig #-}
{-# DEPRECATED kernelGatewayImageConfig "Use generic-lens or generic-optics with 'kernelGatewayImageConfig' instead"  #-}

-- | When the AppImageConfig was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daicrrsLastModifiedTime :: Lens.Lens' DescribeAppImageConfigResponse (Core.Maybe Core.NominalDiffTime)
daicrrsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# INLINEABLE daicrrsLastModifiedTime #-}
{-# DEPRECATED lastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daicrrsResponseStatus :: Lens.Lens' DescribeAppImageConfigResponse Core.Int
daicrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE daicrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
