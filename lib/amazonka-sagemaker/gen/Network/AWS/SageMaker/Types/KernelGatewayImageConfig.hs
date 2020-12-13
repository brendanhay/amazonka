{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.KernelGatewayImageConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.KernelGatewayImageConfig
  ( KernelGatewayImageConfig (..),

    -- * Smart constructor
    mkKernelGatewayImageConfig,

    -- * Lenses
    kgicKernelSpecs,
    kgicFileSystemConfig,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.FileSystemConfig
import Network.AWS.SageMaker.Types.KernelSpec

-- | The configuration for the file system and kernels in a SageMaker image running as a KernelGateway app.
--
-- /See:/ 'mkKernelGatewayImageConfig' smart constructor.
data KernelGatewayImageConfig = KernelGatewayImageConfig'
  { -- | The specification of the Jupyter kernels in the image.
    kernelSpecs :: Lude.NonEmpty KernelSpec,
    -- | The Amazon Elastic File System (EFS) storage configuration for a SageMaker image.
    fileSystemConfig :: Lude.Maybe FileSystemConfig
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KernelGatewayImageConfig' with the minimum fields required to make a request.
--
-- * 'kernelSpecs' - The specification of the Jupyter kernels in the image.
-- * 'fileSystemConfig' - The Amazon Elastic File System (EFS) storage configuration for a SageMaker image.
mkKernelGatewayImageConfig ::
  -- | 'kernelSpecs'
  Lude.NonEmpty KernelSpec ->
  KernelGatewayImageConfig
mkKernelGatewayImageConfig pKernelSpecs_ =
  KernelGatewayImageConfig'
    { kernelSpecs = pKernelSpecs_,
      fileSystemConfig = Lude.Nothing
    }

-- | The specification of the Jupyter kernels in the image.
--
-- /Note:/ Consider using 'kernelSpecs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kgicKernelSpecs :: Lens.Lens' KernelGatewayImageConfig (Lude.NonEmpty KernelSpec)
kgicKernelSpecs = Lens.lens (kernelSpecs :: KernelGatewayImageConfig -> Lude.NonEmpty KernelSpec) (\s a -> s {kernelSpecs = a} :: KernelGatewayImageConfig)
{-# DEPRECATED kgicKernelSpecs "Use generic-lens or generic-optics with 'kernelSpecs' instead." #-}

-- | The Amazon Elastic File System (EFS) storage configuration for a SageMaker image.
--
-- /Note:/ Consider using 'fileSystemConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kgicFileSystemConfig :: Lens.Lens' KernelGatewayImageConfig (Lude.Maybe FileSystemConfig)
kgicFileSystemConfig = Lens.lens (fileSystemConfig :: KernelGatewayImageConfig -> Lude.Maybe FileSystemConfig) (\s a -> s {fileSystemConfig = a} :: KernelGatewayImageConfig)
{-# DEPRECATED kgicFileSystemConfig "Use generic-lens or generic-optics with 'fileSystemConfig' instead." #-}

instance Lude.FromJSON KernelGatewayImageConfig where
  parseJSON =
    Lude.withObject
      "KernelGatewayImageConfig"
      ( \x ->
          KernelGatewayImageConfig'
            Lude.<$> (x Lude..: "KernelSpecs") Lude.<*> (x Lude..:? "FileSystemConfig")
      )

instance Lude.ToJSON KernelGatewayImageConfig where
  toJSON KernelGatewayImageConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("KernelSpecs" Lude..= kernelSpecs),
            ("FileSystemConfig" Lude..=) Lude.<$> fileSystemConfig
          ]
      )
