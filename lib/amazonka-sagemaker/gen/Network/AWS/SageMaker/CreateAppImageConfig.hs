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
    caicrsAppImageConfigARN,
    caicrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkCreateAppImageConfig' smart constructor.
data CreateAppImageConfig = CreateAppImageConfig'
  { -- | The name of the AppImageConfig. Must be unique to your account.
    appImageConfigName :: Lude.Text,
    -- | The KernelGatewayImageConfig.
    kernelGatewayImageConfig :: Lude.Maybe KernelGatewayImageConfig,
    -- | A list of tags to apply to the AppImageConfig.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAppImageConfig' with the minimum fields required to make a request.
--
-- * 'appImageConfigName' - The name of the AppImageConfig. Must be unique to your account.
-- * 'kernelGatewayImageConfig' - The KernelGatewayImageConfig.
-- * 'tags' - A list of tags to apply to the AppImageConfig.
mkCreateAppImageConfig ::
  -- | 'appImageConfigName'
  Lude.Text ->
  CreateAppImageConfig
mkCreateAppImageConfig pAppImageConfigName_ =
  CreateAppImageConfig'
    { appImageConfigName = pAppImageConfigName_,
      kernelGatewayImageConfig = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The name of the AppImageConfig. Must be unique to your account.
--
-- /Note:/ Consider using 'appImageConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caicAppImageConfigName :: Lens.Lens' CreateAppImageConfig Lude.Text
caicAppImageConfigName = Lens.lens (appImageConfigName :: CreateAppImageConfig -> Lude.Text) (\s a -> s {appImageConfigName = a} :: CreateAppImageConfig)
{-# DEPRECATED caicAppImageConfigName "Use generic-lens or generic-optics with 'appImageConfigName' instead." #-}

-- | The KernelGatewayImageConfig.
--
-- /Note:/ Consider using 'kernelGatewayImageConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caicKernelGatewayImageConfig :: Lens.Lens' CreateAppImageConfig (Lude.Maybe KernelGatewayImageConfig)
caicKernelGatewayImageConfig = Lens.lens (kernelGatewayImageConfig :: CreateAppImageConfig -> Lude.Maybe KernelGatewayImageConfig) (\s a -> s {kernelGatewayImageConfig = a} :: CreateAppImageConfig)
{-# DEPRECATED caicKernelGatewayImageConfig "Use generic-lens or generic-optics with 'kernelGatewayImageConfig' instead." #-}

-- | A list of tags to apply to the AppImageConfig.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caicTags :: Lens.Lens' CreateAppImageConfig (Lude.Maybe [Tag])
caicTags = Lens.lens (tags :: CreateAppImageConfig -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateAppImageConfig)
{-# DEPRECATED caicTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateAppImageConfig where
  type Rs CreateAppImageConfig = CreateAppImageConfigResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateAppImageConfigResponse'
            Lude.<$> (x Lude..?> "AppImageConfigArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateAppImageConfig where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.CreateAppImageConfig" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateAppImageConfig where
  toJSON CreateAppImageConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AppImageConfigName" Lude..= appImageConfigName),
            ("KernelGatewayImageConfig" Lude..=)
              Lude.<$> kernelGatewayImageConfig,
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateAppImageConfig where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateAppImageConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateAppImageConfigResponse' smart constructor.
data CreateAppImageConfigResponse = CreateAppImageConfigResponse'
  { -- | The Amazon Resource Name (ARN) of the AppImageConfig.
    appImageConfigARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAppImageConfigResponse' with the minimum fields required to make a request.
--
-- * 'appImageConfigARN' - The Amazon Resource Name (ARN) of the AppImageConfig.
-- * 'responseStatus' - The response status code.
mkCreateAppImageConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateAppImageConfigResponse
mkCreateAppImageConfigResponse pResponseStatus_ =
  CreateAppImageConfigResponse'
    { appImageConfigARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the AppImageConfig.
--
-- /Note:/ Consider using 'appImageConfigARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caicrsAppImageConfigARN :: Lens.Lens' CreateAppImageConfigResponse (Lude.Maybe Lude.Text)
caicrsAppImageConfigARN = Lens.lens (appImageConfigARN :: CreateAppImageConfigResponse -> Lude.Maybe Lude.Text) (\s a -> s {appImageConfigARN = a} :: CreateAppImageConfigResponse)
{-# DEPRECATED caicrsAppImageConfigARN "Use generic-lens or generic-optics with 'appImageConfigARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caicrsResponseStatus :: Lens.Lens' CreateAppImageConfigResponse Lude.Int
caicrsResponseStatus = Lens.lens (responseStatus :: CreateAppImageConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateAppImageConfigResponse)
{-# DEPRECATED caicrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
