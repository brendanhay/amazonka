{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.ImageScanningConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ImageScanningConfiguration
  ( ImageScanningConfiguration (..),

    -- * Smart constructor
    mkImageScanningConfiguration,

    -- * Lenses
    iscScanOnPush,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The image scanning configuration for a repository.
--
-- /See:/ 'mkImageScanningConfiguration' smart constructor.
newtype ImageScanningConfiguration = ImageScanningConfiguration'
  { scanOnPush ::
      Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImageScanningConfiguration' with the minimum fields required to make a request.
--
-- * 'scanOnPush' - The setting that determines whether images are scanned after being pushed to a repository. If set to @true@ , images will be scanned after being pushed. If this parameter is not specified, it will default to @false@ and images will not be scanned unless a scan is manually started with the 'StartImageScan' API.
mkImageScanningConfiguration ::
  ImageScanningConfiguration
mkImageScanningConfiguration =
  ImageScanningConfiguration' {scanOnPush = Lude.Nothing}

-- | The setting that determines whether images are scanned after being pushed to a repository. If set to @true@ , images will be scanned after being pushed. If this parameter is not specified, it will default to @false@ and images will not be scanned unless a scan is manually started with the 'StartImageScan' API.
--
-- /Note:/ Consider using 'scanOnPush' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscScanOnPush :: Lens.Lens' ImageScanningConfiguration (Lude.Maybe Lude.Bool)
iscScanOnPush = Lens.lens (scanOnPush :: ImageScanningConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {scanOnPush = a} :: ImageScanningConfiguration)
{-# DEPRECATED iscScanOnPush "Use generic-lens or generic-optics with 'scanOnPush' instead." #-}

instance Lude.FromJSON ImageScanningConfiguration where
  parseJSON =
    Lude.withObject
      "ImageScanningConfiguration"
      ( \x ->
          ImageScanningConfiguration' Lude.<$> (x Lude..:? "scanOnPush")
      )

instance Lude.ToJSON ImageScanningConfiguration where
  toJSON ImageScanningConfiguration' {..} =
    Lude.object
      (Lude.catMaybes [("scanOnPush" Lude..=) Lude.<$> scanOnPush])
