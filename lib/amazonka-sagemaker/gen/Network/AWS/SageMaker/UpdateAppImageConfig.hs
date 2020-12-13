{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    UpdateAppImageConfig (..),
    mkUpdateAppImageConfig,

    -- ** Request lenses
    uaicAppImageConfigName,
    uaicKernelGatewayImageConfig,

    -- * Destructuring the response
    UpdateAppImageConfigResponse (..),
    mkUpdateAppImageConfigResponse,

    -- ** Response lenses
    uaicrsAppImageConfigARN,
    uaicrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkUpdateAppImageConfig' smart constructor.
data UpdateAppImageConfig = UpdateAppImageConfig'
  { -- | The name of the AppImageConfig to update.
    appImageConfigName :: Lude.Text,
    -- | The new KernelGateway app to run on the image.
    kernelGatewayImageConfig :: Lude.Maybe KernelGatewayImageConfig
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAppImageConfig' with the minimum fields required to make a request.
--
-- * 'appImageConfigName' - The name of the AppImageConfig to update.
-- * 'kernelGatewayImageConfig' - The new KernelGateway app to run on the image.
mkUpdateAppImageConfig ::
  -- | 'appImageConfigName'
  Lude.Text ->
  UpdateAppImageConfig
mkUpdateAppImageConfig pAppImageConfigName_ =
  UpdateAppImageConfig'
    { appImageConfigName = pAppImageConfigName_,
      kernelGatewayImageConfig = Lude.Nothing
    }

-- | The name of the AppImageConfig to update.
--
-- /Note:/ Consider using 'appImageConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaicAppImageConfigName :: Lens.Lens' UpdateAppImageConfig Lude.Text
uaicAppImageConfigName = Lens.lens (appImageConfigName :: UpdateAppImageConfig -> Lude.Text) (\s a -> s {appImageConfigName = a} :: UpdateAppImageConfig)
{-# DEPRECATED uaicAppImageConfigName "Use generic-lens or generic-optics with 'appImageConfigName' instead." #-}

-- | The new KernelGateway app to run on the image.
--
-- /Note:/ Consider using 'kernelGatewayImageConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaicKernelGatewayImageConfig :: Lens.Lens' UpdateAppImageConfig (Lude.Maybe KernelGatewayImageConfig)
uaicKernelGatewayImageConfig = Lens.lens (kernelGatewayImageConfig :: UpdateAppImageConfig -> Lude.Maybe KernelGatewayImageConfig) (\s a -> s {kernelGatewayImageConfig = a} :: UpdateAppImageConfig)
{-# DEPRECATED uaicKernelGatewayImageConfig "Use generic-lens or generic-optics with 'kernelGatewayImageConfig' instead." #-}

instance Lude.AWSRequest UpdateAppImageConfig where
  type Rs UpdateAppImageConfig = UpdateAppImageConfigResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateAppImageConfigResponse'
            Lude.<$> (x Lude..?> "AppImageConfigArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateAppImageConfig where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.UpdateAppImageConfig" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateAppImageConfig where
  toJSON UpdateAppImageConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("AppImageConfigName" Lude..= appImageConfigName),
            ("KernelGatewayImageConfig" Lude..=)
              Lude.<$> kernelGatewayImageConfig
          ]
      )

instance Lude.ToPath UpdateAppImageConfig where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateAppImageConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateAppImageConfigResponse' smart constructor.
data UpdateAppImageConfigResponse = UpdateAppImageConfigResponse'
  { -- | The Amazon Resource Name (ARN) for the AppImageConfig.
    appImageConfigARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateAppImageConfigResponse' with the minimum fields required to make a request.
--
-- * 'appImageConfigARN' - The Amazon Resource Name (ARN) for the AppImageConfig.
-- * 'responseStatus' - The response status code.
mkUpdateAppImageConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateAppImageConfigResponse
mkUpdateAppImageConfigResponse pResponseStatus_ =
  UpdateAppImageConfigResponse'
    { appImageConfigARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) for the AppImageConfig.
--
-- /Note:/ Consider using 'appImageConfigARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaicrsAppImageConfigARN :: Lens.Lens' UpdateAppImageConfigResponse (Lude.Maybe Lude.Text)
uaicrsAppImageConfigARN = Lens.lens (appImageConfigARN :: UpdateAppImageConfigResponse -> Lude.Maybe Lude.Text) (\s a -> s {appImageConfigARN = a} :: UpdateAppImageConfigResponse)
{-# DEPRECATED uaicrsAppImageConfigARN "Use generic-lens or generic-optics with 'appImageConfigARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaicrsResponseStatus :: Lens.Lens' UpdateAppImageConfigResponse Lude.Int
uaicrsResponseStatus = Lens.lens (responseStatus :: UpdateAppImageConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateAppImageConfigResponse)
{-# DEPRECATED uaicrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
