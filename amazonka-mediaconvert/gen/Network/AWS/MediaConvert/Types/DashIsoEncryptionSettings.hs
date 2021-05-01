{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DashIsoEncryptionSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DashIsoEncryptionSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.DashIsoPlaybackDeviceCompatibility
import Network.AWS.MediaConvert.Types.SpekeKeyProvider
import qualified Network.AWS.Prelude as Prelude

-- | Specifies DRM settings for DASH outputs.
--
-- /See:/ 'newDashIsoEncryptionSettings' smart constructor.
data DashIsoEncryptionSettings = DashIsoEncryptionSettings'
  { -- | If your output group type is HLS, DASH, or Microsoft Smooth, use these
    -- settings when doing DRM encryption with a SPEKE-compliant key provider.
    -- If your output group type is CMAF, use the SpekeKeyProviderCmaf settings
    -- instead.
    spekeKeyProvider :: Prelude.Maybe SpekeKeyProvider,
    -- | This setting can improve the compatibility of your output with video
    -- players on obsolete devices. It applies only to DASH H.264 outputs with
    -- DRM encryption. Choose Unencrypted SEI (UNENCRYPTED_SEI) only to correct
    -- problems with playback on older devices. Otherwise, keep the default
    -- setting CENC v1 (CENC_V1). If you choose Unencrypted SEI, for that
    -- output, the service will exclude the access unit delimiter and will
    -- leave the SEI NAL units unencrypted.
    playbackDeviceCompatibility :: Prelude.Maybe DashIsoPlaybackDeviceCompatibility
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DashIsoEncryptionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'spekeKeyProvider', 'dashIsoEncryptionSettings_spekeKeyProvider' - If your output group type is HLS, DASH, or Microsoft Smooth, use these
-- settings when doing DRM encryption with a SPEKE-compliant key provider.
-- If your output group type is CMAF, use the SpekeKeyProviderCmaf settings
-- instead.
--
-- 'playbackDeviceCompatibility', 'dashIsoEncryptionSettings_playbackDeviceCompatibility' - This setting can improve the compatibility of your output with video
-- players on obsolete devices. It applies only to DASH H.264 outputs with
-- DRM encryption. Choose Unencrypted SEI (UNENCRYPTED_SEI) only to correct
-- problems with playback on older devices. Otherwise, keep the default
-- setting CENC v1 (CENC_V1). If you choose Unencrypted SEI, for that
-- output, the service will exclude the access unit delimiter and will
-- leave the SEI NAL units unencrypted.
newDashIsoEncryptionSettings ::
  DashIsoEncryptionSettings
newDashIsoEncryptionSettings =
  DashIsoEncryptionSettings'
    { spekeKeyProvider =
        Prelude.Nothing,
      playbackDeviceCompatibility = Prelude.Nothing
    }

-- | If your output group type is HLS, DASH, or Microsoft Smooth, use these
-- settings when doing DRM encryption with a SPEKE-compliant key provider.
-- If your output group type is CMAF, use the SpekeKeyProviderCmaf settings
-- instead.
dashIsoEncryptionSettings_spekeKeyProvider :: Lens.Lens' DashIsoEncryptionSettings (Prelude.Maybe SpekeKeyProvider)
dashIsoEncryptionSettings_spekeKeyProvider = Lens.lens (\DashIsoEncryptionSettings' {spekeKeyProvider} -> spekeKeyProvider) (\s@DashIsoEncryptionSettings' {} a -> s {spekeKeyProvider = a} :: DashIsoEncryptionSettings)

-- | This setting can improve the compatibility of your output with video
-- players on obsolete devices. It applies only to DASH H.264 outputs with
-- DRM encryption. Choose Unencrypted SEI (UNENCRYPTED_SEI) only to correct
-- problems with playback on older devices. Otherwise, keep the default
-- setting CENC v1 (CENC_V1). If you choose Unencrypted SEI, for that
-- output, the service will exclude the access unit delimiter and will
-- leave the SEI NAL units unencrypted.
dashIsoEncryptionSettings_playbackDeviceCompatibility :: Lens.Lens' DashIsoEncryptionSettings (Prelude.Maybe DashIsoPlaybackDeviceCompatibility)
dashIsoEncryptionSettings_playbackDeviceCompatibility = Lens.lens (\DashIsoEncryptionSettings' {playbackDeviceCompatibility} -> playbackDeviceCompatibility) (\s@DashIsoEncryptionSettings' {} a -> s {playbackDeviceCompatibility = a} :: DashIsoEncryptionSettings)

instance Prelude.FromJSON DashIsoEncryptionSettings where
  parseJSON =
    Prelude.withObject
      "DashIsoEncryptionSettings"
      ( \x ->
          DashIsoEncryptionSettings'
            Prelude.<$> (x Prelude..:? "spekeKeyProvider")
            Prelude.<*> (x Prelude..:? "playbackDeviceCompatibility")
      )

instance Prelude.Hashable DashIsoEncryptionSettings

instance Prelude.NFData DashIsoEncryptionSettings

instance Prelude.ToJSON DashIsoEncryptionSettings where
  toJSON DashIsoEncryptionSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("spekeKeyProvider" Prelude..=)
              Prelude.<$> spekeKeyProvider,
            ("playbackDeviceCompatibility" Prelude..=)
              Prelude.<$> playbackDeviceCompatibility
          ]
      )
