{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DescribeAppImageConfig (..),
    mkDescribeAppImageConfig,

    -- ** Request lenses
    dAppImageConfigName,

    -- * Destructuring the response
    DescribeAppImageConfigResponse (..),
    mkDescribeAppImageConfigResponse,

    -- ** Response lenses
    daicrrsAppImageConfigArn,
    daicrrsAppImageConfigName,
    daicrrsCreationTime,
    daicrrsKernelGatewayImageConfig,
    daicrrsLastModifiedTime,
    daicrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeAppImageConfig' smart constructor.
newtype DescribeAppImageConfig = DescribeAppImageConfig'
  { -- | The name of the AppImageConfig to describe.
    appImageConfigName :: Types.AppImageConfigName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAppImageConfig' value with any optional fields omitted.
mkDescribeAppImageConfig ::
  -- | 'appImageConfigName'
  Types.AppImageConfigName ->
  DescribeAppImageConfig
mkDescribeAppImageConfig appImageConfigName =
  DescribeAppImageConfig' {appImageConfigName}

-- | The name of the AppImageConfig to describe.
--
-- /Note:/ Consider using 'appImageConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAppImageConfigName :: Lens.Lens' DescribeAppImageConfig Types.AppImageConfigName
dAppImageConfigName = Lens.field @"appImageConfigName"
{-# DEPRECATED dAppImageConfigName "Use generic-lens or generic-optics with 'appImageConfigName' instead." #-}

instance Core.FromJSON DescribeAppImageConfig where
  toJSON DescribeAppImageConfig {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("AppImageConfigName" Core..= appImageConfigName)]
      )

instance Core.AWSRequest DescribeAppImageConfig where
  type Rs DescribeAppImageConfig = DescribeAppImageConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.DescribeAppImageConfig")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAppImageConfigResponse'
            Core.<$> (x Core..:? "AppImageConfigArn")
            Core.<*> (x Core..:? "AppImageConfigName")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "KernelGatewayImageConfig")
            Core.<*> (x Core..:? "LastModifiedTime")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeAppImageConfigResponse' smart constructor.
data DescribeAppImageConfigResponse = DescribeAppImageConfigResponse'
  { -- | The Amazon Resource Name (ARN) of the AppImageConfig.
    appImageConfigArn :: Core.Maybe Types.AppImageConfigArn,
    -- | The name of the AppImageConfig.
    appImageConfigName :: Core.Maybe Types.AppImageConfigName,
    -- | When the AppImageConfig was created.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The configuration of a KernelGateway app.
    kernelGatewayImageConfig :: Core.Maybe Types.KernelGatewayImageConfig,
    -- | When the AppImageConfig was last modified.
    lastModifiedTime :: Core.Maybe Core.NominalDiffTime,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeAppImageConfigResponse' value with any optional fields omitted.
mkDescribeAppImageConfigResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeAppImageConfigResponse
mkDescribeAppImageConfigResponse responseStatus =
  DescribeAppImageConfigResponse'
    { appImageConfigArn = Core.Nothing,
      appImageConfigName = Core.Nothing,
      creationTime = Core.Nothing,
      kernelGatewayImageConfig = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the AppImageConfig.
--
-- /Note:/ Consider using 'appImageConfigArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daicrrsAppImageConfigArn :: Lens.Lens' DescribeAppImageConfigResponse (Core.Maybe Types.AppImageConfigArn)
daicrrsAppImageConfigArn = Lens.field @"appImageConfigArn"
{-# DEPRECATED daicrrsAppImageConfigArn "Use generic-lens or generic-optics with 'appImageConfigArn' instead." #-}

-- | The name of the AppImageConfig.
--
-- /Note:/ Consider using 'appImageConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daicrrsAppImageConfigName :: Lens.Lens' DescribeAppImageConfigResponse (Core.Maybe Types.AppImageConfigName)
daicrrsAppImageConfigName = Lens.field @"appImageConfigName"
{-# DEPRECATED daicrrsAppImageConfigName "Use generic-lens or generic-optics with 'appImageConfigName' instead." #-}

-- | When the AppImageConfig was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daicrrsCreationTime :: Lens.Lens' DescribeAppImageConfigResponse (Core.Maybe Core.NominalDiffTime)
daicrrsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED daicrrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The configuration of a KernelGateway app.
--
-- /Note:/ Consider using 'kernelGatewayImageConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daicrrsKernelGatewayImageConfig :: Lens.Lens' DescribeAppImageConfigResponse (Core.Maybe Types.KernelGatewayImageConfig)
daicrrsKernelGatewayImageConfig = Lens.field @"kernelGatewayImageConfig"
{-# DEPRECATED daicrrsKernelGatewayImageConfig "Use generic-lens or generic-optics with 'kernelGatewayImageConfig' instead." #-}

-- | When the AppImageConfig was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daicrrsLastModifiedTime :: Lens.Lens' DescribeAppImageConfigResponse (Core.Maybe Core.NominalDiffTime)
daicrrsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED daicrrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daicrrsResponseStatus :: Lens.Lens' DescribeAppImageConfigResponse Core.Int
daicrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED daicrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
