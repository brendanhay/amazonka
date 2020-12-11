-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AppImageConfigDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AppImageConfigDetails
  ( AppImageConfigDetails (..),

    -- * Smart constructor
    mkAppImageConfigDetails,

    -- * Lenses
    aicdCreationTime,
    aicdAppImageConfigName,
    aicdLastModifiedTime,
    aicdKernelGatewayImageConfig,
    aicdAppImageConfigARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.KernelGatewayImageConfig

-- | The configuration for running a SageMaker image as a KernelGateway app.
--
-- /See:/ 'mkAppImageConfigDetails' smart constructor.
data AppImageConfigDetails = AppImageConfigDetails'
  { creationTime ::
      Lude.Maybe Lude.Timestamp,
    appImageConfigName :: Lude.Maybe Lude.Text,
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    kernelGatewayImageConfig ::
      Lude.Maybe KernelGatewayImageConfig,
    appImageConfigARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AppImageConfigDetails' with the minimum fields required to make a request.
--
-- * 'appImageConfigARN' - The Amazon Resource Name (ARN) of the AppImageConfig.
-- * 'appImageConfigName' - The name of the AppImageConfig. Must be unique to your account.
-- * 'creationTime' - When the AppImageConfig was created.
-- * 'kernelGatewayImageConfig' - The configuration for the file system and kernels in the SageMaker image.
-- * 'lastModifiedTime' - When the AppImageConfig was last modified.
mkAppImageConfigDetails ::
  AppImageConfigDetails
mkAppImageConfigDetails =
  AppImageConfigDetails'
    { creationTime = Lude.Nothing,
      appImageConfigName = Lude.Nothing,
      lastModifiedTime = Lude.Nothing,
      kernelGatewayImageConfig = Lude.Nothing,
      appImageConfigARN = Lude.Nothing
    }

-- | When the AppImageConfig was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aicdCreationTime :: Lens.Lens' AppImageConfigDetails (Lude.Maybe Lude.Timestamp)
aicdCreationTime = Lens.lens (creationTime :: AppImageConfigDetails -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: AppImageConfigDetails)
{-# DEPRECATED aicdCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The name of the AppImageConfig. Must be unique to your account.
--
-- /Note:/ Consider using 'appImageConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aicdAppImageConfigName :: Lens.Lens' AppImageConfigDetails (Lude.Maybe Lude.Text)
aicdAppImageConfigName = Lens.lens (appImageConfigName :: AppImageConfigDetails -> Lude.Maybe Lude.Text) (\s a -> s {appImageConfigName = a} :: AppImageConfigDetails)
{-# DEPRECATED aicdAppImageConfigName "Use generic-lens or generic-optics with 'appImageConfigName' instead." #-}

-- | When the AppImageConfig was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aicdLastModifiedTime :: Lens.Lens' AppImageConfigDetails (Lude.Maybe Lude.Timestamp)
aicdLastModifiedTime = Lens.lens (lastModifiedTime :: AppImageConfigDetails -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: AppImageConfigDetails)
{-# DEPRECATED aicdLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The configuration for the file system and kernels in the SageMaker image.
--
-- /Note:/ Consider using 'kernelGatewayImageConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aicdKernelGatewayImageConfig :: Lens.Lens' AppImageConfigDetails (Lude.Maybe KernelGatewayImageConfig)
aicdKernelGatewayImageConfig = Lens.lens (kernelGatewayImageConfig :: AppImageConfigDetails -> Lude.Maybe KernelGatewayImageConfig) (\s a -> s {kernelGatewayImageConfig = a} :: AppImageConfigDetails)
{-# DEPRECATED aicdKernelGatewayImageConfig "Use generic-lens or generic-optics with 'kernelGatewayImageConfig' instead." #-}

-- | The Amazon Resource Name (ARN) of the AppImageConfig.
--
-- /Note:/ Consider using 'appImageConfigARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aicdAppImageConfigARN :: Lens.Lens' AppImageConfigDetails (Lude.Maybe Lude.Text)
aicdAppImageConfigARN = Lens.lens (appImageConfigARN :: AppImageConfigDetails -> Lude.Maybe Lude.Text) (\s a -> s {appImageConfigARN = a} :: AppImageConfigDetails)
{-# DEPRECATED aicdAppImageConfigARN "Use generic-lens or generic-optics with 'appImageConfigARN' instead." #-}

instance Lude.FromJSON AppImageConfigDetails where
  parseJSON =
    Lude.withObject
      "AppImageConfigDetails"
      ( \x ->
          AppImageConfigDetails'
            Lude.<$> (x Lude..:? "CreationTime")
            Lude.<*> (x Lude..:? "AppImageConfigName")
            Lude.<*> (x Lude..:? "LastModifiedTime")
            Lude.<*> (x Lude..:? "KernelGatewayImageConfig")
            Lude.<*> (x Lude..:? "AppImageConfigArn")
      )
