{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateAppImageConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a configuration for running a SageMaker image as a KernelGateway app. The configuration specifies the Amazon Elastic File System (EFS) storage volume on the image, and a list of the kernels in the image.
module Network.AWS.SageMaker.CreateAppImageConfig
  ( -- * Creating a request
    CreateAppImageConfig (..),
    mkCreateAppImageConfig,

    -- ** Request lenses
    caicAppImageConfigName,
    caicKernelGatewayImageConfig,
    caicTags,

    -- * Destructuring the response
    CreateAppImageConfigResponse (..),
    mkCreateAppImageConfigResponse,

    -- ** Response lenses
    caicrrsAppImageConfigArn,
    caicrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkCreateAppImageConfig' smart constructor.
data CreateAppImageConfig = CreateAppImageConfig'
  { -- | The name of the AppImageConfig. Must be unique to your account.
    appImageConfigName :: Types.AppImageConfigName,
    -- | The KernelGatewayImageConfig.
    kernelGatewayImageConfig :: Core.Maybe Types.KernelGatewayImageConfig,
    -- | A list of tags to apply to the AppImageConfig.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAppImageConfig' value with any optional fields omitted.
mkCreateAppImageConfig ::
  -- | 'appImageConfigName'
  Types.AppImageConfigName ->
  CreateAppImageConfig
mkCreateAppImageConfig appImageConfigName =
  CreateAppImageConfig'
    { appImageConfigName,
      kernelGatewayImageConfig = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name of the AppImageConfig. Must be unique to your account.
--
-- /Note:/ Consider using 'appImageConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caicAppImageConfigName :: Lens.Lens' CreateAppImageConfig Types.AppImageConfigName
caicAppImageConfigName = Lens.field @"appImageConfigName"
{-# DEPRECATED caicAppImageConfigName "Use generic-lens or generic-optics with 'appImageConfigName' instead." #-}

-- | The KernelGatewayImageConfig.
--
-- /Note:/ Consider using 'kernelGatewayImageConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caicKernelGatewayImageConfig :: Lens.Lens' CreateAppImageConfig (Core.Maybe Types.KernelGatewayImageConfig)
caicKernelGatewayImageConfig = Lens.field @"kernelGatewayImageConfig"
{-# DEPRECATED caicKernelGatewayImageConfig "Use generic-lens or generic-optics with 'kernelGatewayImageConfig' instead." #-}

-- | A list of tags to apply to the AppImageConfig.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caicTags :: Lens.Lens' CreateAppImageConfig (Core.Maybe [Types.Tag])
caicTags = Lens.field @"tags"
{-# DEPRECATED caicTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateAppImageConfig where
  toJSON CreateAppImageConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AppImageConfigName" Core..= appImageConfigName),
            ("KernelGatewayImageConfig" Core..=)
              Core.<$> kernelGatewayImageConfig,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateAppImageConfig where
  type Rs CreateAppImageConfig = CreateAppImageConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.CreateAppImageConfig")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAppImageConfigResponse'
            Core.<$> (x Core..:? "AppImageConfigArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateAppImageConfigResponse' smart constructor.
data CreateAppImageConfigResponse = CreateAppImageConfigResponse'
  { -- | The Amazon Resource Name (ARN) of the AppImageConfig.
    appImageConfigArn :: Core.Maybe Types.AppImageConfigArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateAppImageConfigResponse' value with any optional fields omitted.
mkCreateAppImageConfigResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateAppImageConfigResponse
mkCreateAppImageConfigResponse responseStatus =
  CreateAppImageConfigResponse'
    { appImageConfigArn = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the AppImageConfig.
--
-- /Note:/ Consider using 'appImageConfigArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caicrrsAppImageConfigArn :: Lens.Lens' CreateAppImageConfigResponse (Core.Maybe Types.AppImageConfigArn)
caicrrsAppImageConfigArn = Lens.field @"appImageConfigArn"
{-# DEPRECATED caicrrsAppImageConfigArn "Use generic-lens or generic-optics with 'appImageConfigArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caicrrsResponseStatus :: Lens.Lens' CreateAppImageConfigResponse Core.Int
caicrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED caicrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
