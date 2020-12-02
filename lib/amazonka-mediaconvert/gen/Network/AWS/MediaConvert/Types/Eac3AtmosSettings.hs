{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3AtmosSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3AtmosSettings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.Eac3AtmosBitstreamMode
import Network.AWS.MediaConvert.Types.Eac3AtmosCodingMode
import Network.AWS.MediaConvert.Types.Eac3AtmosDialogueIntelligence
import Network.AWS.MediaConvert.Types.Eac3AtmosDynamicRangeCompressionLine
import Network.AWS.MediaConvert.Types.Eac3AtmosDynamicRangeCompressionRf
import Network.AWS.MediaConvert.Types.Eac3AtmosMeteringMode
import Network.AWS.MediaConvert.Types.Eac3AtmosStereoDownmix
import Network.AWS.MediaConvert.Types.Eac3AtmosSurroundExMode
import Network.AWS.Prelude

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value EAC3_ATMOS.
--
-- /See:/ 'eac3AtmosSettings' smart constructor.
data Eac3AtmosSettings = Eac3AtmosSettings'
  { _easStereoDownmix ::
      !(Maybe Eac3AtmosStereoDownmix),
    _easLoRoCenterMixLevel :: !(Maybe Double),
    _easLtRtCenterMixLevel :: !(Maybe Double),
    _easDynamicRangeCompressionLine ::
      !(Maybe Eac3AtmosDynamicRangeCompressionLine),
    _easLtRtSurroundMixLevel :: !(Maybe Double),
    _easLoRoSurroundMixLevel :: !(Maybe Double),
    _easBitstreamMode :: !(Maybe Eac3AtmosBitstreamMode),
    _easDynamicRangeCompressionRf ::
      !(Maybe Eac3AtmosDynamicRangeCompressionRf),
    _easCodingMode :: !(Maybe Eac3AtmosCodingMode),
    _easSampleRate :: !(Maybe Nat),
    _easSpeechThreshold :: !(Maybe Nat),
    _easBitrate :: !(Maybe Nat),
    _easDialogueIntelligence ::
      !(Maybe Eac3AtmosDialogueIntelligence),
    _easMeteringMode :: !(Maybe Eac3AtmosMeteringMode),
    _easSurroundExMode :: !(Maybe Eac3AtmosSurroundExMode)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Eac3AtmosSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'easStereoDownmix' - Choose how the service does stereo downmixing.
--
-- * 'easLoRoCenterMixLevel' - Specify a value for the following Dolby Atmos setting: Left only/Right only center mix (Lo/Ro center). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3AtmosStereoDownmix). Valid values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, and -6.0.
--
-- * 'easLtRtCenterMixLevel' - Specify a value for the following Dolby Atmos setting: Left total/Right total center mix (Lt/Rt center). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3AtmosStereoDownmix). Valid values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, and -6.0.
--
-- * 'easDynamicRangeCompressionLine' - Specify the absolute peak level for a signal with dynamic range compression.
--
-- * 'easLtRtSurroundMixLevel' - Specify a value for the following Dolby Atmos setting: Left total/Right total surround mix (Lt/Rt surround). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3AtmosStereoDownmix). Valid values: -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel.
--
-- * 'easLoRoSurroundMixLevel' - Specify a value for the following Dolby Atmos setting: Left only/Right only (Lo/Ro surround). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3AtmosStereoDownmix). Valid values: -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel.
--
-- * 'easBitstreamMode' - Specify the bitstream mode for the E-AC-3 stream that the encoder emits. For more information about the EAC3 bitstream mode, see ATSC A/52-2012 (Annex E).
--
-- * 'easDynamicRangeCompressionRf' - Specify how the service limits the audio dynamic range when compressing the audio.
--
-- * 'easCodingMode' - The coding mode for Dolby Digital Plus JOC (Atmos) is always 9.1.6 (CODING_MODE_9_1_6).
--
-- * 'easSampleRate' - This value is always 48000. It represents the sample rate in Hz.
--
-- * 'easSpeechThreshold' - Specify the percentage of audio content that must be speech before the encoder uses the measured speech loudness as the overall program loudness.
--
-- * 'easBitrate' - Specify the average bitrate in bits per second. Valid values: 384k, 448k, 640k, 768k
--
-- * 'easDialogueIntelligence' - Enable Dolby Dialogue Intelligence to adjust loudness based on dialogue analysis.
--
-- * 'easMeteringMode' - Choose how the service meters the loudness of your audio.
--
-- * 'easSurroundExMode' - Specify whether your input audio has an additional center rear surround channel matrix encoded into your left and right surround channels.
eac3AtmosSettings ::
  Eac3AtmosSettings
eac3AtmosSettings =
  Eac3AtmosSettings'
    { _easStereoDownmix = Nothing,
      _easLoRoCenterMixLevel = Nothing,
      _easLtRtCenterMixLevel = Nothing,
      _easDynamicRangeCompressionLine = Nothing,
      _easLtRtSurroundMixLevel = Nothing,
      _easLoRoSurroundMixLevel = Nothing,
      _easBitstreamMode = Nothing,
      _easDynamicRangeCompressionRf = Nothing,
      _easCodingMode = Nothing,
      _easSampleRate = Nothing,
      _easSpeechThreshold = Nothing,
      _easBitrate = Nothing,
      _easDialogueIntelligence = Nothing,
      _easMeteringMode = Nothing,
      _easSurroundExMode = Nothing
    }

-- | Choose how the service does stereo downmixing.
easStereoDownmix :: Lens' Eac3AtmosSettings (Maybe Eac3AtmosStereoDownmix)
easStereoDownmix = lens _easStereoDownmix (\s a -> s {_easStereoDownmix = a})

-- | Specify a value for the following Dolby Atmos setting: Left only/Right only center mix (Lo/Ro center). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3AtmosStereoDownmix). Valid values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, and -6.0.
easLoRoCenterMixLevel :: Lens' Eac3AtmosSettings (Maybe Double)
easLoRoCenterMixLevel = lens _easLoRoCenterMixLevel (\s a -> s {_easLoRoCenterMixLevel = a})

-- | Specify a value for the following Dolby Atmos setting: Left total/Right total center mix (Lt/Rt center). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3AtmosStereoDownmix). Valid values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, and -6.0.
easLtRtCenterMixLevel :: Lens' Eac3AtmosSettings (Maybe Double)
easLtRtCenterMixLevel = lens _easLtRtCenterMixLevel (\s a -> s {_easLtRtCenterMixLevel = a})

-- | Specify the absolute peak level for a signal with dynamic range compression.
easDynamicRangeCompressionLine :: Lens' Eac3AtmosSettings (Maybe Eac3AtmosDynamicRangeCompressionLine)
easDynamicRangeCompressionLine = lens _easDynamicRangeCompressionLine (\s a -> s {_easDynamicRangeCompressionLine = a})

-- | Specify a value for the following Dolby Atmos setting: Left total/Right total surround mix (Lt/Rt surround). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3AtmosStereoDownmix). Valid values: -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel.
easLtRtSurroundMixLevel :: Lens' Eac3AtmosSettings (Maybe Double)
easLtRtSurroundMixLevel = lens _easLtRtSurroundMixLevel (\s a -> s {_easLtRtSurroundMixLevel = a})

-- | Specify a value for the following Dolby Atmos setting: Left only/Right only (Lo/Ro surround). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3AtmosStereoDownmix). Valid values: -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel.
easLoRoSurroundMixLevel :: Lens' Eac3AtmosSettings (Maybe Double)
easLoRoSurroundMixLevel = lens _easLoRoSurroundMixLevel (\s a -> s {_easLoRoSurroundMixLevel = a})

-- | Specify the bitstream mode for the E-AC-3 stream that the encoder emits. For more information about the EAC3 bitstream mode, see ATSC A/52-2012 (Annex E).
easBitstreamMode :: Lens' Eac3AtmosSettings (Maybe Eac3AtmosBitstreamMode)
easBitstreamMode = lens _easBitstreamMode (\s a -> s {_easBitstreamMode = a})

-- | Specify how the service limits the audio dynamic range when compressing the audio.
easDynamicRangeCompressionRf :: Lens' Eac3AtmosSettings (Maybe Eac3AtmosDynamicRangeCompressionRf)
easDynamicRangeCompressionRf = lens _easDynamicRangeCompressionRf (\s a -> s {_easDynamicRangeCompressionRf = a})

-- | The coding mode for Dolby Digital Plus JOC (Atmos) is always 9.1.6 (CODING_MODE_9_1_6).
easCodingMode :: Lens' Eac3AtmosSettings (Maybe Eac3AtmosCodingMode)
easCodingMode = lens _easCodingMode (\s a -> s {_easCodingMode = a})

-- | This value is always 48000. It represents the sample rate in Hz.
easSampleRate :: Lens' Eac3AtmosSettings (Maybe Natural)
easSampleRate = lens _easSampleRate (\s a -> s {_easSampleRate = a}) . mapping _Nat

-- | Specify the percentage of audio content that must be speech before the encoder uses the measured speech loudness as the overall program loudness.
easSpeechThreshold :: Lens' Eac3AtmosSettings (Maybe Natural)
easSpeechThreshold = lens _easSpeechThreshold (\s a -> s {_easSpeechThreshold = a}) . mapping _Nat

-- | Specify the average bitrate in bits per second. Valid values: 384k, 448k, 640k, 768k
easBitrate :: Lens' Eac3AtmosSettings (Maybe Natural)
easBitrate = lens _easBitrate (\s a -> s {_easBitrate = a}) . mapping _Nat

-- | Enable Dolby Dialogue Intelligence to adjust loudness based on dialogue analysis.
easDialogueIntelligence :: Lens' Eac3AtmosSettings (Maybe Eac3AtmosDialogueIntelligence)
easDialogueIntelligence = lens _easDialogueIntelligence (\s a -> s {_easDialogueIntelligence = a})

-- | Choose how the service meters the loudness of your audio.
easMeteringMode :: Lens' Eac3AtmosSettings (Maybe Eac3AtmosMeteringMode)
easMeteringMode = lens _easMeteringMode (\s a -> s {_easMeteringMode = a})

-- | Specify whether your input audio has an additional center rear surround channel matrix encoded into your left and right surround channels.
easSurroundExMode :: Lens' Eac3AtmosSettings (Maybe Eac3AtmosSurroundExMode)
easSurroundExMode = lens _easSurroundExMode (\s a -> s {_easSurroundExMode = a})

instance FromJSON Eac3AtmosSettings where
  parseJSON =
    withObject
      "Eac3AtmosSettings"
      ( \x ->
          Eac3AtmosSettings'
            <$> (x .:? "stereoDownmix")
            <*> (x .:? "loRoCenterMixLevel")
            <*> (x .:? "ltRtCenterMixLevel")
            <*> (x .:? "dynamicRangeCompressionLine")
            <*> (x .:? "ltRtSurroundMixLevel")
            <*> (x .:? "loRoSurroundMixLevel")
            <*> (x .:? "bitstreamMode")
            <*> (x .:? "dynamicRangeCompressionRf")
            <*> (x .:? "codingMode")
            <*> (x .:? "sampleRate")
            <*> (x .:? "speechThreshold")
            <*> (x .:? "bitrate")
            <*> (x .:? "dialogueIntelligence")
            <*> (x .:? "meteringMode")
            <*> (x .:? "surroundExMode")
      )

instance Hashable Eac3AtmosSettings

instance NFData Eac3AtmosSettings

instance ToJSON Eac3AtmosSettings where
  toJSON Eac3AtmosSettings' {..} =
    object
      ( catMaybes
          [ ("stereoDownmix" .=) <$> _easStereoDownmix,
            ("loRoCenterMixLevel" .=) <$> _easLoRoCenterMixLevel,
            ("ltRtCenterMixLevel" .=) <$> _easLtRtCenterMixLevel,
            ("dynamicRangeCompressionLine" .=)
              <$> _easDynamicRangeCompressionLine,
            ("ltRtSurroundMixLevel" .=) <$> _easLtRtSurroundMixLevel,
            ("loRoSurroundMixLevel" .=) <$> _easLoRoSurroundMixLevel,
            ("bitstreamMode" .=) <$> _easBitstreamMode,
            ("dynamicRangeCompressionRf" .=) <$> _easDynamicRangeCompressionRf,
            ("codingMode" .=) <$> _easCodingMode,
            ("sampleRate" .=) <$> _easSampleRate,
            ("speechThreshold" .=) <$> _easSpeechThreshold,
            ("bitrate" .=) <$> _easBitrate,
            ("dialogueIntelligence" .=) <$> _easDialogueIntelligence,
            ("meteringMode" .=) <$> _easMeteringMode,
            ("surroundExMode" .=) <$> _easSurroundExMode
          ]
      )
