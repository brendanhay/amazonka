{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.NexGuardFileMarkerSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.NexGuardFileMarkerSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.WatermarkingStrength
import Network.AWS.Prelude

-- | For forensic video watermarking, MediaConvert supports Nagra NexGuard File Marker watermarking. MediaConvert supports both PreRelease Content (NGPR/G2) and OTT Streaming workflows.
--
-- /See:/ 'nexGuardFileMarkerSettings' smart constructor.
data NexGuardFileMarkerSettings = NexGuardFileMarkerSettings'
  { _ngfmsStrength ::
      !(Maybe WatermarkingStrength),
    _ngfmsPayload :: !(Maybe Nat),
    _ngfmsPreset :: !(Maybe Text),
    _ngfmsLicense :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NexGuardFileMarkerSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ngfmsStrength' - Optional. Ignore this setting unless Nagra support directs you to specify a value. When you don't specify a value here, the Nagra NexGuard library uses its default value.
--
-- * 'ngfmsPayload' - Specify the payload ID that you want associated with this output. Valid values vary depending on your Nagra NexGuard forensic watermarking workflow. Required when you include Nagra NexGuard File Marker watermarking (NexGuardWatermarkingSettings) in your job. For PreRelease Content (NGPR/G2), specify an integer from 1 through 4,194,303. You must generate a unique ID for each asset you watermark, and keep a record of which ID you have assigned to each asset. Neither Nagra nor MediaConvert keep track of the relationship between output files and your IDs. For OTT Streaming, create two adaptive bitrate (ABR) stacks for each asset. Do this by setting up two output groups. For one output group, set the value of Payload ID (payload) to 0 in every output. For the other output group, set Payload ID (payload) to 1 in every output.
--
-- * 'ngfmsPreset' - Enter one of the watermarking preset strings that Nagra provides you. Required when you include Nagra NexGuard File Marker watermarking (NexGuardWatermarkingSettings) in your job.
--
-- * 'ngfmsLicense' - Use the base64 license string that Nagra provides you. Enter it directly in your JSON job specification or in the console. Required when you include Nagra NexGuard File Marker watermarking (NexGuardWatermarkingSettings) in your job.
nexGuardFileMarkerSettings ::
  NexGuardFileMarkerSettings
nexGuardFileMarkerSettings =
  NexGuardFileMarkerSettings'
    { _ngfmsStrength = Nothing,
      _ngfmsPayload = Nothing,
      _ngfmsPreset = Nothing,
      _ngfmsLicense = Nothing
    }

-- | Optional. Ignore this setting unless Nagra support directs you to specify a value. When you don't specify a value here, the Nagra NexGuard library uses its default value.
ngfmsStrength :: Lens' NexGuardFileMarkerSettings (Maybe WatermarkingStrength)
ngfmsStrength = lens _ngfmsStrength (\s a -> s {_ngfmsStrength = a})

-- | Specify the payload ID that you want associated with this output. Valid values vary depending on your Nagra NexGuard forensic watermarking workflow. Required when you include Nagra NexGuard File Marker watermarking (NexGuardWatermarkingSettings) in your job. For PreRelease Content (NGPR/G2), specify an integer from 1 through 4,194,303. You must generate a unique ID for each asset you watermark, and keep a record of which ID you have assigned to each asset. Neither Nagra nor MediaConvert keep track of the relationship between output files and your IDs. For OTT Streaming, create two adaptive bitrate (ABR) stacks for each asset. Do this by setting up two output groups. For one output group, set the value of Payload ID (payload) to 0 in every output. For the other output group, set Payload ID (payload) to 1 in every output.
ngfmsPayload :: Lens' NexGuardFileMarkerSettings (Maybe Natural)
ngfmsPayload = lens _ngfmsPayload (\s a -> s {_ngfmsPayload = a}) . mapping _Nat

-- | Enter one of the watermarking preset strings that Nagra provides you. Required when you include Nagra NexGuard File Marker watermarking (NexGuardWatermarkingSettings) in your job.
ngfmsPreset :: Lens' NexGuardFileMarkerSettings (Maybe Text)
ngfmsPreset = lens _ngfmsPreset (\s a -> s {_ngfmsPreset = a})

-- | Use the base64 license string that Nagra provides you. Enter it directly in your JSON job specification or in the console. Required when you include Nagra NexGuard File Marker watermarking (NexGuardWatermarkingSettings) in your job.
ngfmsLicense :: Lens' NexGuardFileMarkerSettings (Maybe Text)
ngfmsLicense = lens _ngfmsLicense (\s a -> s {_ngfmsLicense = a})

instance FromJSON NexGuardFileMarkerSettings where
  parseJSON =
    withObject
      "NexGuardFileMarkerSettings"
      ( \x ->
          NexGuardFileMarkerSettings'
            <$> (x .:? "strength")
            <*> (x .:? "payload")
            <*> (x .:? "preset")
            <*> (x .:? "license")
      )

instance Hashable NexGuardFileMarkerSettings

instance NFData NexGuardFileMarkerSettings

instance ToJSON NexGuardFileMarkerSettings where
  toJSON NexGuardFileMarkerSettings' {..} =
    object
      ( catMaybes
          [ ("strength" .=) <$> _ngfmsStrength,
            ("payload" .=) <$> _ngfmsPayload,
            ("preset" .=) <$> _ngfmsPreset,
            ("license" .=) <$> _ngfmsLicense
          ]
      )
