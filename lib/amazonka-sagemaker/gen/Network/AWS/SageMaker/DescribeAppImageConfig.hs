{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    daicrsCreationTime,
    daicrsAppImageConfigName,
    daicrsLastModifiedTime,
    daicrsKernelGatewayImageConfig,
    daicrsAppImageConfigARN,
    daicrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDescribeAppImageConfig' smart constructor.
newtype DescribeAppImageConfig = DescribeAppImageConfig'
  { appImageConfigName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAppImageConfig' with the minimum fields required to make a request.
--
-- * 'appImageConfigName' - The name of the AppImageConfig to describe.
mkDescribeAppImageConfig ::
  -- | 'appImageConfigName'
  Lude.Text ->
  DescribeAppImageConfig
mkDescribeAppImageConfig pAppImageConfigName_ =
  DescribeAppImageConfig'
    { appImageConfigName =
        pAppImageConfigName_
    }

-- | The name of the AppImageConfig to describe.
--
-- /Note:/ Consider using 'appImageConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAppImageConfigName :: Lens.Lens' DescribeAppImageConfig Lude.Text
dAppImageConfigName = Lens.lens (appImageConfigName :: DescribeAppImageConfig -> Lude.Text) (\s a -> s {appImageConfigName = a} :: DescribeAppImageConfig)
{-# DEPRECATED dAppImageConfigName "Use generic-lens or generic-optics with 'appImageConfigName' instead." #-}

instance Lude.AWSRequest DescribeAppImageConfig where
  type Rs DescribeAppImageConfig = DescribeAppImageConfigResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeAppImageConfigResponse'
            Lude.<$> (x Lude..?> "CreationTime")
            Lude.<*> (x Lude..?> "AppImageConfigName")
            Lude.<*> (x Lude..?> "LastModifiedTime")
            Lude.<*> (x Lude..?> "KernelGatewayImageConfig")
            Lude.<*> (x Lude..?> "AppImageConfigArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAppImageConfig where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DescribeAppImageConfig" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeAppImageConfig where
  toJSON DescribeAppImageConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("AppImageConfigName" Lude..= appImageConfigName)]
      )

instance Lude.ToPath DescribeAppImageConfig where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAppImageConfig where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeAppImageConfigResponse' smart constructor.
data DescribeAppImageConfigResponse = DescribeAppImageConfigResponse'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    appImageConfigName ::
      Lude.Maybe Lude.Text,
    lastModifiedTime ::
      Lude.Maybe Lude.Timestamp,
    kernelGatewayImageConfig ::
      Lude.Maybe
        KernelGatewayImageConfig,
    appImageConfigARN ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAppImageConfigResponse' with the minimum fields required to make a request.
--
-- * 'appImageConfigARN' - The Amazon Resource Name (ARN) of the AppImageConfig.
-- * 'appImageConfigName' - The name of the AppImageConfig.
-- * 'creationTime' - When the AppImageConfig was created.
-- * 'kernelGatewayImageConfig' - The configuration of a KernelGateway app.
-- * 'lastModifiedTime' - When the AppImageConfig was last modified.
-- * 'responseStatus' - The response status code.
mkDescribeAppImageConfigResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAppImageConfigResponse
mkDescribeAppImageConfigResponse pResponseStatus_ =
  DescribeAppImageConfigResponse'
    { creationTime = Lude.Nothing,
      appImageConfigName = Lude.Nothing,
      lastModifiedTime = Lude.Nothing,
      kernelGatewayImageConfig = Lude.Nothing,
      appImageConfigARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | When the AppImageConfig was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daicrsCreationTime :: Lens.Lens' DescribeAppImageConfigResponse (Lude.Maybe Lude.Timestamp)
daicrsCreationTime = Lens.lens (creationTime :: DescribeAppImageConfigResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: DescribeAppImageConfigResponse)
{-# DEPRECATED daicrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The name of the AppImageConfig.
--
-- /Note:/ Consider using 'appImageConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daicrsAppImageConfigName :: Lens.Lens' DescribeAppImageConfigResponse (Lude.Maybe Lude.Text)
daicrsAppImageConfigName = Lens.lens (appImageConfigName :: DescribeAppImageConfigResponse -> Lude.Maybe Lude.Text) (\s a -> s {appImageConfigName = a} :: DescribeAppImageConfigResponse)
{-# DEPRECATED daicrsAppImageConfigName "Use generic-lens or generic-optics with 'appImageConfigName' instead." #-}

-- | When the AppImageConfig was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daicrsLastModifiedTime :: Lens.Lens' DescribeAppImageConfigResponse (Lude.Maybe Lude.Timestamp)
daicrsLastModifiedTime = Lens.lens (lastModifiedTime :: DescribeAppImageConfigResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: DescribeAppImageConfigResponse)
{-# DEPRECATED daicrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The configuration of a KernelGateway app.
--
-- /Note:/ Consider using 'kernelGatewayImageConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daicrsKernelGatewayImageConfig :: Lens.Lens' DescribeAppImageConfigResponse (Lude.Maybe KernelGatewayImageConfig)
daicrsKernelGatewayImageConfig = Lens.lens (kernelGatewayImageConfig :: DescribeAppImageConfigResponse -> Lude.Maybe KernelGatewayImageConfig) (\s a -> s {kernelGatewayImageConfig = a} :: DescribeAppImageConfigResponse)
{-# DEPRECATED daicrsKernelGatewayImageConfig "Use generic-lens or generic-optics with 'kernelGatewayImageConfig' instead." #-}

-- | The Amazon Resource Name (ARN) of the AppImageConfig.
--
-- /Note:/ Consider using 'appImageConfigARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daicrsAppImageConfigARN :: Lens.Lens' DescribeAppImageConfigResponse (Lude.Maybe Lude.Text)
daicrsAppImageConfigARN = Lens.lens (appImageConfigARN :: DescribeAppImageConfigResponse -> Lude.Maybe Lude.Text) (\s a -> s {appImageConfigARN = a} :: DescribeAppImageConfigResponse)
{-# DEPRECATED daicrsAppImageConfigARN "Use generic-lens or generic-optics with 'appImageConfigARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daicrsResponseStatus :: Lens.Lens' DescribeAppImageConfigResponse Lude.Int
daicrsResponseStatus = Lens.lens (responseStatus :: DescribeAppImageConfigResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAppImageConfigResponse)
{-# DEPRECATED daicrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
