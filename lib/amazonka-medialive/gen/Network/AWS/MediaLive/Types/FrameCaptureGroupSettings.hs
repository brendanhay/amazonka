-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.FrameCaptureGroupSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FrameCaptureGroupSettings
  ( FrameCaptureGroupSettings (..),

    -- * Smart constructor
    mkFrameCaptureGroupSettings,

    -- * Lenses
    fcgsDestination,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.OutputLocationRef
import qualified Network.AWS.Prelude as Lude

-- | Frame Capture Group Settings
--
-- /See:/ 'mkFrameCaptureGroupSettings' smart constructor.
newtype FrameCaptureGroupSettings = FrameCaptureGroupSettings'
  { destination ::
      OutputLocationRef
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FrameCaptureGroupSettings' with the minimum fields required to make a request.
--
-- * 'destination' - The destination for the frame capture files. Either the URI for an Amazon S3 bucket and object, plus a file name prefix (for example, s3ssl://sportsDelivery/highlights/20180820/curling-) or the URI for a MediaStore container, plus a file name prefix (for example, mediastoressl://sportsDelivery/20180820/curling-). The final file names consist of the prefix from the destination field (for example, "curling-") + name modifier + the counter (5 digits, starting from 00001) + extension (which is always .jpg).  For example, curling-low.00001.jpg
mkFrameCaptureGroupSettings ::
  -- | 'destination'
  OutputLocationRef ->
  FrameCaptureGroupSettings
mkFrameCaptureGroupSettings pDestination_ =
  FrameCaptureGroupSettings' {destination = pDestination_}

-- | The destination for the frame capture files. Either the URI for an Amazon S3 bucket and object, plus a file name prefix (for example, s3ssl://sportsDelivery/highlights/20180820/curling-) or the URI for a MediaStore container, plus a file name prefix (for example, mediastoressl://sportsDelivery/20180820/curling-). The final file names consist of the prefix from the destination field (for example, "curling-") + name modifier + the counter (5 digits, starting from 00001) + extension (which is always .jpg).  For example, curling-low.00001.jpg
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcgsDestination :: Lens.Lens' FrameCaptureGroupSettings OutputLocationRef
fcgsDestination = Lens.lens (destination :: FrameCaptureGroupSettings -> OutputLocationRef) (\s a -> s {destination = a} :: FrameCaptureGroupSettings)
{-# DEPRECATED fcgsDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

instance Lude.FromJSON FrameCaptureGroupSettings where
  parseJSON =
    Lude.withObject
      "FrameCaptureGroupSettings"
      ( \x ->
          FrameCaptureGroupSettings' Lude.<$> (x Lude..: "destination")
      )

instance Lude.ToJSON FrameCaptureGroupSettings where
  toJSON FrameCaptureGroupSettings' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("destination" Lude..= destination)])
