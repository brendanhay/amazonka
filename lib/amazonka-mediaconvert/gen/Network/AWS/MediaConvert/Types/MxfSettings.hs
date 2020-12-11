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
import Network.AWS.MediaConvert.Types.MxfAfdSignaling
import Network.AWS.MediaConvert.Types.MxfProfile
import qualified Network.AWS.Prelude as Lude

-- | MXF settings
--
-- /See:/ 'mkMxfSettings' smart constructor.
data MxfSettings = MxfSettings'
  { afdSignaling ::
      Lude.Maybe MxfAfdSignaling,
    profile :: Lude.Maybe MxfProfile
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MxfSettings' with the minimum fields required to make a request.
--
-- * 'afdSignaling' - Optional. When you have AFD signaling set up in your output video stream, use this setting to choose whether to also include it in the MXF wrapper. Choose Don't copy (NO_COPY) to exclude AFD signaling from the MXF wrapper. Choose Copy from video stream (COPY_FROM_VIDEO) to copy the AFD values from the video stream for this output to the MXF wrapper. Regardless of which option you choose, the AFD values remain in the video stream. Related settings: To set up your output to include or exclude AFD values, see AfdSignaling, under VideoDescription. On the console, find AFD signaling under the output's video encoding settings.
-- * 'profile' - Specify the MXF profile, also called shim, for this output. When you choose Auto, MediaConvert chooses a profile based on the video codec and resolution. For a list of codecs supported with each MXF profile, see https://docs.aws.amazon.com/mediaconvert/latest/ug/codecs-supported-with-each-mxf-profile.html. For more information about the automatic selection behavior, see https://docs.aws.amazon.com/mediaconvert/latest/ug/default-automatic-selection-of-mxf-profiles.html.
mkMxfSettings ::
  MxfSettings
mkMxfSettings =
  MxfSettings' {afdSignaling = Lude.Nothing, profile = Lude.Nothing}

-- | Optional. When you have AFD signaling set up in your output video stream, use this setting to choose whether to also include it in the MXF wrapper. Choose Don't copy (NO_COPY) to exclude AFD signaling from the MXF wrapper. Choose Copy from video stream (COPY_FROM_VIDEO) to copy the AFD values from the video stream for this output to the MXF wrapper. Regardless of which option you choose, the AFD values remain in the video stream. Related settings: To set up your output to include or exclude AFD values, see AfdSignaling, under VideoDescription. On the console, find AFD signaling under the output's video encoding settings.
--
-- /Note:/ Consider using 'afdSignaling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msAfdSignaling :: Lens.Lens' MxfSettings (Lude.Maybe MxfAfdSignaling)
msAfdSignaling = Lens.lens (afdSignaling :: MxfSettings -> Lude.Maybe MxfAfdSignaling) (\s a -> s {afdSignaling = a} :: MxfSettings)
{-# DEPRECATED msAfdSignaling "Use generic-lens or generic-optics with 'afdSignaling' instead." #-}

-- | Specify the MXF profile, also called shim, for this output. When you choose Auto, MediaConvert chooses a profile based on the video codec and resolution. For a list of codecs supported with each MXF profile, see https://docs.aws.amazon.com/mediaconvert/latest/ug/codecs-supported-with-each-mxf-profile.html. For more information about the automatic selection behavior, see https://docs.aws.amazon.com/mediaconvert/latest/ug/default-automatic-selection-of-mxf-profiles.html.
--
-- /Note:/ Consider using 'profile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msProfile :: Lens.Lens' MxfSettings (Lude.Maybe MxfProfile)
msProfile = Lens.lens (profile :: MxfSettings -> Lude.Maybe MxfProfile) (\s a -> s {profile = a} :: MxfSettings)
{-# DEPRECATED msProfile "Use generic-lens or generic-optics with 'profile' instead." #-}

instance Lude.FromJSON MxfSettings where
  parseJSON =
    Lude.withObject
      "MxfSettings"
      ( \x ->
          MxfSettings'
            Lude.<$> (x Lude..:? "afdSignaling") Lude.<*> (x Lude..:? "profile")
      )

instance Lude.ToJSON MxfSettings where
  toJSON MxfSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("afdSignaling" Lude..=) Lude.<$> afdSignaling,
            ("profile" Lude..=) Lude.<$> profile
          ]
      )
