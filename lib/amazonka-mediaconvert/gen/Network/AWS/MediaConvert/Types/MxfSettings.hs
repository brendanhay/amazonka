{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MxfSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MxfSettings
  ( MxfSettings (..),

    -- * Smart constructor
    mkMxfSettings,

    -- * Lenses
    msAfdSignaling,
    msProfile,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.MxfAfdSignaling as Types
import qualified Network.AWS.MediaConvert.Types.MxfProfile as Types
import qualified Network.AWS.Prelude as Core

-- | MXF settings
--
-- /See:/ 'mkMxfSettings' smart constructor.
data MxfSettings = MxfSettings'
  { -- | Optional. When you have AFD signaling set up in your output video stream, use this setting to choose whether to also include it in the MXF wrapper. Choose Don't copy (NO_COPY) to exclude AFD signaling from the MXF wrapper. Choose Copy from video stream (COPY_FROM_VIDEO) to copy the AFD values from the video stream for this output to the MXF wrapper. Regardless of which option you choose, the AFD values remain in the video stream. Related settings: To set up your output to include or exclude AFD values, see AfdSignaling, under VideoDescription. On the console, find AFD signaling under the output's video encoding settings.
    afdSignaling :: Core.Maybe Types.MxfAfdSignaling,
    -- | Specify the MXF profile, also called shim, for this output. When you choose Auto, MediaConvert chooses a profile based on the video codec and resolution. For a list of codecs supported with each MXF profile, see https://docs.aws.amazon.com/mediaconvert/latest/ug/codecs-supported-with-each-mxf-profile.html. For more information about the automatic selection behavior, see https://docs.aws.amazon.com/mediaconvert/latest/ug/default-automatic-selection-of-mxf-profiles.html.
    profile :: Core.Maybe Types.MxfProfile
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MxfSettings' value with any optional fields omitted.
mkMxfSettings ::
  MxfSettings
mkMxfSettings =
  MxfSettings' {afdSignaling = Core.Nothing, profile = Core.Nothing}

-- | Optional. When you have AFD signaling set up in your output video stream, use this setting to choose whether to also include it in the MXF wrapper. Choose Don't copy (NO_COPY) to exclude AFD signaling from the MXF wrapper. Choose Copy from video stream (COPY_FROM_VIDEO) to copy the AFD values from the video stream for this output to the MXF wrapper. Regardless of which option you choose, the AFD values remain in the video stream. Related settings: To set up your output to include or exclude AFD values, see AfdSignaling, under VideoDescription. On the console, find AFD signaling under the output's video encoding settings.
--
-- /Note:/ Consider using 'afdSignaling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msAfdSignaling :: Lens.Lens' MxfSettings (Core.Maybe Types.MxfAfdSignaling)
msAfdSignaling = Lens.field @"afdSignaling"
{-# DEPRECATED msAfdSignaling "Use generic-lens or generic-optics with 'afdSignaling' instead." #-}

-- | Specify the MXF profile, also called shim, for this output. When you choose Auto, MediaConvert chooses a profile based on the video codec and resolution. For a list of codecs supported with each MXF profile, see https://docs.aws.amazon.com/mediaconvert/latest/ug/codecs-supported-with-each-mxf-profile.html. For more information about the automatic selection behavior, see https://docs.aws.amazon.com/mediaconvert/latest/ug/default-automatic-selection-of-mxf-profiles.html.
--
-- /Note:/ Consider using 'profile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msProfile :: Lens.Lens' MxfSettings (Core.Maybe Types.MxfProfile)
msProfile = Lens.field @"profile"
{-# DEPRECATED msProfile "Use generic-lens or generic-optics with 'profile' instead." #-}

instance Core.FromJSON MxfSettings where
  toJSON MxfSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("afdSignaling" Core..=) Core.<$> afdSignaling,
            ("profile" Core..=) Core.<$> profile
          ]
      )

instance Core.FromJSON MxfSettings where
  parseJSON =
    Core.withObject "MxfSettings" Core.$
      \x ->
        MxfSettings'
          Core.<$> (x Core..:? "afdSignaling") Core.<*> (x Core..:? "profile")
