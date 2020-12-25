{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
import qualified Network.AWS.MediaConvert.Types.DashIsoPlaybackDeviceCompatibility as Types
import qualified Network.AWS.MediaConvert.Types.SpekeKeyProvider as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies DRM settings for DASH outputs.
--
-- /See:/ 'mkDashIsoEncryptionSettings' smart constructor.
data DashIsoEncryptionSettings = DashIsoEncryptionSettings'
  { -- | This setting can improve the compatibility of your output with video players on obsolete devices. It applies only to DASH H.264 outputs with DRM encryption. Choose Unencrypted SEI (UNENCRYPTED_SEI) only to correct problems with playback on older devices. Otherwise, keep the default setting CENC v1 (CENC_V1). If you choose Unencrypted SEI, for that output, the service will exclude the access unit delimiter and will leave the SEI NAL units unencrypted.
    playbackDeviceCompatibility :: Core.Maybe Types.DashIsoPlaybackDeviceCompatibility,
    -- | If your output group type is HLS, DASH, or Microsoft Smooth, use these settings when doing DRM encryption with a SPEKE-compliant key provider.  If your output group type is CMAF, use the SpekeKeyProviderCmaf settings instead.
    spekeKeyProvider :: Core.Maybe Types.SpekeKeyProvider
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DashIsoEncryptionSettings' value with any optional fields omitted.
mkDashIsoEncryptionSettings ::
  DashIsoEncryptionSettings
mkDashIsoEncryptionSettings =
  DashIsoEncryptionSettings'
    { playbackDeviceCompatibility =
        Core.Nothing,
      spekeKeyProvider = Core.Nothing
    }

-- | This setting can improve the compatibility of your output with video players on obsolete devices. It applies only to DASH H.264 outputs with DRM encryption. Choose Unencrypted SEI (UNENCRYPTED_SEI) only to correct problems with playback on older devices. Otherwise, keep the default setting CENC v1 (CENC_V1). If you choose Unencrypted SEI, for that output, the service will exclude the access unit delimiter and will leave the SEI NAL units unencrypted.
--
-- /Note:/ Consider using 'playbackDeviceCompatibility' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diesPlaybackDeviceCompatibility :: Lens.Lens' DashIsoEncryptionSettings (Core.Maybe Types.DashIsoPlaybackDeviceCompatibility)
diesPlaybackDeviceCompatibility = Lens.field @"playbackDeviceCompatibility"
{-# DEPRECATED diesPlaybackDeviceCompatibility "Use generic-lens or generic-optics with 'playbackDeviceCompatibility' instead." #-}

-- | If your output group type is HLS, DASH, or Microsoft Smooth, use these settings when doing DRM encryption with a SPEKE-compliant key provider.  If your output group type is CMAF, use the SpekeKeyProviderCmaf settings instead.
--
-- /Note:/ Consider using 'spekeKeyProvider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diesSpekeKeyProvider :: Lens.Lens' DashIsoEncryptionSettings (Core.Maybe Types.SpekeKeyProvider)
diesSpekeKeyProvider = Lens.field @"spekeKeyProvider"
{-# DEPRECATED diesSpekeKeyProvider "Use generic-lens or generic-optics with 'spekeKeyProvider' instead." #-}

instance Core.FromJSON DashIsoEncryptionSettings where
  toJSON DashIsoEncryptionSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("playbackDeviceCompatibility" Core..=)
              Core.<$> playbackDeviceCompatibility,
            ("spekeKeyProvider" Core..=) Core.<$> spekeKeyProvider
          ]
      )

instance Core.FromJSON DashIsoEncryptionSettings where
  parseJSON =
    Core.withObject "DashIsoEncryptionSettings" Core.$
      \x ->
        DashIsoEncryptionSettings'
          Core.<$> (x Core..:? "playbackDeviceCompatibility")
          Core.<*> (x Core..:? "spekeKeyProvider")
