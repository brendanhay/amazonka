-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DashIsoEncryptionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DashIsoEncryptionSettings
  ( DashIsoEncryptionSettings (..),

    -- * Smart constructor
    mkDashIsoEncryptionSettings,

    -- * Lenses
    diesPlaybackDeviceCompatibility,
    diesSpekeKeyProvider,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.DashIsoPlaybackDeviceCompatibility
import Network.AWS.MediaConvert.Types.SpekeKeyProvider
import qualified Network.AWS.Prelude as Lude

-- | Specifies DRM settings for DASH outputs.
--
-- /See:/ 'mkDashIsoEncryptionSettings' smart constructor.
data DashIsoEncryptionSettings = DashIsoEncryptionSettings'
  { playbackDeviceCompatibility ::
      Lude.Maybe
        DashIsoPlaybackDeviceCompatibility,
    spekeKeyProvider ::
      Lude.Maybe SpekeKeyProvider
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DashIsoEncryptionSettings' with the minimum fields required to make a request.
--
-- * 'playbackDeviceCompatibility' - This setting can improve the compatibility of your output with video players on obsolete devices. It applies only to DASH H.264 outputs with DRM encryption. Choose Unencrypted SEI (UNENCRYPTED_SEI) only to correct problems with playback on older devices. Otherwise, keep the default setting CENC v1 (CENC_V1). If you choose Unencrypted SEI, for that output, the service will exclude the access unit delimiter and will leave the SEI NAL units unencrypted.
-- * 'spekeKeyProvider' - If your output group type is HLS, DASH, or Microsoft Smooth, use these settings when doing DRM encryption with a SPEKE-compliant key provider.  If your output group type is CMAF, use the SpekeKeyProviderCmaf settings instead.
mkDashIsoEncryptionSettings ::
  DashIsoEncryptionSettings
mkDashIsoEncryptionSettings =
  DashIsoEncryptionSettings'
    { playbackDeviceCompatibility =
        Lude.Nothing,
      spekeKeyProvider = Lude.Nothing
    }

-- | This setting can improve the compatibility of your output with video players on obsolete devices. It applies only to DASH H.264 outputs with DRM encryption. Choose Unencrypted SEI (UNENCRYPTED_SEI) only to correct problems with playback on older devices. Otherwise, keep the default setting CENC v1 (CENC_V1). If you choose Unencrypted SEI, for that output, the service will exclude the access unit delimiter and will leave the SEI NAL units unencrypted.
--
-- /Note:/ Consider using 'playbackDeviceCompatibility' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diesPlaybackDeviceCompatibility :: Lens.Lens' DashIsoEncryptionSettings (Lude.Maybe DashIsoPlaybackDeviceCompatibility)
diesPlaybackDeviceCompatibility = Lens.lens (playbackDeviceCompatibility :: DashIsoEncryptionSettings -> Lude.Maybe DashIsoPlaybackDeviceCompatibility) (\s a -> s {playbackDeviceCompatibility = a} :: DashIsoEncryptionSettings)
{-# DEPRECATED diesPlaybackDeviceCompatibility "Use generic-lens or generic-optics with 'playbackDeviceCompatibility' instead." #-}

-- | If your output group type is HLS, DASH, or Microsoft Smooth, use these settings when doing DRM encryption with a SPEKE-compliant key provider.  If your output group type is CMAF, use the SpekeKeyProviderCmaf settings instead.
--
-- /Note:/ Consider using 'spekeKeyProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diesSpekeKeyProvider :: Lens.Lens' DashIsoEncryptionSettings (Lude.Maybe SpekeKeyProvider)
diesSpekeKeyProvider = Lens.lens (spekeKeyProvider :: DashIsoEncryptionSettings -> Lude.Maybe SpekeKeyProvider) (\s a -> s {spekeKeyProvider = a} :: DashIsoEncryptionSettings)
{-# DEPRECATED diesSpekeKeyProvider "Use generic-lens or generic-optics with 'spekeKeyProvider' instead." #-}

instance Lude.FromJSON DashIsoEncryptionSettings where
  parseJSON =
    Lude.withObject
      "DashIsoEncryptionSettings"
      ( \x ->
          DashIsoEncryptionSettings'
            Lude.<$> (x Lude..:? "playbackDeviceCompatibility")
            Lude.<*> (x Lude..:? "spekeKeyProvider")
      )

instance Lude.ToJSON DashIsoEncryptionSettings where
  toJSON DashIsoEncryptionSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("playbackDeviceCompatibility" Lude..=)
              Lude.<$> playbackDeviceCompatibility,
            ("spekeKeyProvider" Lude..=) Lude.<$> spekeKeyProvider
          ]
      )
