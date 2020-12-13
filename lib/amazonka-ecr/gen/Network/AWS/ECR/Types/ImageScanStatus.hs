{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.ImageScanStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ImageScanStatus
  ( ImageScanStatus (..),

    -- * Smart constructor
    mkImageScanStatus,

    -- * Lenses
    issStatus,
    issDescription,
  )
where

import Network.AWS.ECR.Types.ScanStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The current status of an image scan.
--
-- /See:/ 'mkImageScanStatus' smart constructor.
data ImageScanStatus = ImageScanStatus'
  { -- | The current state of an image scan.
    status :: Lude.Maybe ScanStatus,
    -- | The description of the image scan status.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ImageScanStatus' with the minimum fields required to make a request.
--
-- * 'status' - The current state of an image scan.
-- * 'description' - The description of the image scan status.
mkImageScanStatus ::
  ImageScanStatus
mkImageScanStatus =
  ImageScanStatus'
    { status = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The current state of an image scan.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
issStatus :: Lens.Lens' ImageScanStatus (Lude.Maybe ScanStatus)
issStatus = Lens.lens (status :: ImageScanStatus -> Lude.Maybe ScanStatus) (\s a -> s {status = a} :: ImageScanStatus)
{-# DEPRECATED issStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The description of the image scan status.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
issDescription :: Lens.Lens' ImageScanStatus (Lude.Maybe Lude.Text)
issDescription = Lens.lens (description :: ImageScanStatus -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ImageScanStatus)
{-# DEPRECATED issDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON ImageScanStatus where
  parseJSON =
    Lude.withObject
      "ImageScanStatus"
      ( \x ->
          ImageScanStatus'
            Lude.<$> (x Lude..:? "status") Lude.<*> (x Lude..:? "description")
      )
