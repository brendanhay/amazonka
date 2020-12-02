{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DashIsoEncryptionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DashIsoEncryptionSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.DashIsoPlaybackDeviceCompatibility
import Network.AWS.MediaConvert.Types.SpekeKeyProvider
import Network.AWS.Prelude

-- | Specifies DRM settings for DASH outputs.
--
-- /See:/ 'dashIsoEncryptionSettings' smart constructor.
data DashIsoEncryptionSettings = DashIsoEncryptionSettings'
  { _diesPlaybackDeviceCompatibility ::
      !( Maybe
           DashIsoPlaybackDeviceCompatibility
       ),
    _diesSpekeKeyProvider ::
      !(Maybe SpekeKeyProvider)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DashIsoEncryptionSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diesPlaybackDeviceCompatibility' - This setting can improve the compatibility of your output with video players on obsolete devices. It applies only to DASH H.264 outputs with DRM encryption. Choose Unencrypted SEI (UNENCRYPTED_SEI) only to correct problems with playback on older devices. Otherwise, keep the default setting CENC v1 (CENC_V1). If you choose Unencrypted SEI, for that output, the service will exclude the access unit delimiter and will leave the SEI NAL units unencrypted.
--
-- * 'diesSpekeKeyProvider' - If your output group type is HLS, DASH, or Microsoft Smooth, use these settings when doing DRM encryption with a SPEKE-compliant key provider.  If your output group type is CMAF, use the SpekeKeyProviderCmaf settings instead.
dashIsoEncryptionSettings ::
  DashIsoEncryptionSettings
dashIsoEncryptionSettings =
  DashIsoEncryptionSettings'
    { _diesPlaybackDeviceCompatibility =
        Nothing,
      _diesSpekeKeyProvider = Nothing
    }

-- | This setting can improve the compatibility of your output with video players on obsolete devices. It applies only to DASH H.264 outputs with DRM encryption. Choose Unencrypted SEI (UNENCRYPTED_SEI) only to correct problems with playback on older devices. Otherwise, keep the default setting CENC v1 (CENC_V1). If you choose Unencrypted SEI, for that output, the service will exclude the access unit delimiter and will leave the SEI NAL units unencrypted.
diesPlaybackDeviceCompatibility :: Lens' DashIsoEncryptionSettings (Maybe DashIsoPlaybackDeviceCompatibility)
diesPlaybackDeviceCompatibility = lens _diesPlaybackDeviceCompatibility (\s a -> s {_diesPlaybackDeviceCompatibility = a})

-- | If your output group type is HLS, DASH, or Microsoft Smooth, use these settings when doing DRM encryption with a SPEKE-compliant key provider.  If your output group type is CMAF, use the SpekeKeyProviderCmaf settings instead.
diesSpekeKeyProvider :: Lens' DashIsoEncryptionSettings (Maybe SpekeKeyProvider)
diesSpekeKeyProvider = lens _diesSpekeKeyProvider (\s a -> s {_diesSpekeKeyProvider = a})

instance FromJSON DashIsoEncryptionSettings where
  parseJSON =
    withObject
      "DashIsoEncryptionSettings"
      ( \x ->
          DashIsoEncryptionSettings'
            <$> (x .:? "playbackDeviceCompatibility")
            <*> (x .:? "spekeKeyProvider")
      )

instance Hashable DashIsoEncryptionSettings

instance NFData DashIsoEncryptionSettings

instance ToJSON DashIsoEncryptionSettings where
  toJSON DashIsoEncryptionSettings' {..} =
    object
      ( catMaybes
          [ ("playbackDeviceCompatibility" .=)
              <$> _diesPlaybackDeviceCompatibility,
            ("spekeKeyProvider" .=) <$> _diesSpekeKeyProvider
          ]
      )
