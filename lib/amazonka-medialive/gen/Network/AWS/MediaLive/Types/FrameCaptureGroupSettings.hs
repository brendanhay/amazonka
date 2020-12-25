{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
import qualified Network.AWS.MediaLive.Types.OutputLocationRef as Types
import qualified Network.AWS.Prelude as Core

-- | Frame Capture Group Settings
--
-- /See:/ 'mkFrameCaptureGroupSettings' smart constructor.
newtype FrameCaptureGroupSettings = FrameCaptureGroupSettings'
  { -- | The destination for the frame capture files. Either the URI for an Amazon S3 bucket and object, plus a file name prefix (for example, s3ssl://sportsDelivery/highlights/20180820/curling-) or the URI for a MediaStore container, plus a file name prefix (for example, mediastoressl://sportsDelivery/20180820/curling-). The final file names consist of the prefix from the destination field (for example, "curling-") + name modifier + the counter (5 digits, starting from 00001) + extension (which is always .jpg).  For example, curling-low.00001.jpg
    destination :: Types.OutputLocationRef
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'FrameCaptureGroupSettings' value with any optional fields omitted.
mkFrameCaptureGroupSettings ::
  -- | 'destination'
  Types.OutputLocationRef ->
  FrameCaptureGroupSettings
mkFrameCaptureGroupSettings destination =
  FrameCaptureGroupSettings' {destination}

-- | The destination for the frame capture files. Either the URI for an Amazon S3 bucket and object, plus a file name prefix (for example, s3ssl://sportsDelivery/highlights/20180820/curling-) or the URI for a MediaStore container, plus a file name prefix (for example, mediastoressl://sportsDelivery/20180820/curling-). The final file names consist of the prefix from the destination field (for example, "curling-") + name modifier + the counter (5 digits, starting from 00001) + extension (which is always .jpg).  For example, curling-low.00001.jpg
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcgsDestination :: Lens.Lens' FrameCaptureGroupSettings Types.OutputLocationRef
fcgsDestination = Lens.field @"destination"
{-# DEPRECATED fcgsDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

instance Core.FromJSON FrameCaptureGroupSettings where
  toJSON FrameCaptureGroupSettings {..} =
    Core.object
      (Core.catMaybes [Core.Just ("destination" Core..= destination)])

instance Core.FromJSON FrameCaptureGroupSettings where
  parseJSON =
    Core.withObject "FrameCaptureGroupSettings" Core.$
      \x ->
        FrameCaptureGroupSettings' Core.<$> (x Core..: "destination")
