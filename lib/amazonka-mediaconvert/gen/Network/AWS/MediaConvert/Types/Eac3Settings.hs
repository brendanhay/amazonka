{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Eac3Settings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Eac3Settings where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types.Eac3AttenuationControl
import Network.AWS.MediaConvert.Types.Eac3BitstreamMode
import Network.AWS.MediaConvert.Types.Eac3CodingMode
import Network.AWS.MediaConvert.Types.Eac3DcFilter
import Network.AWS.MediaConvert.Types.Eac3DynamicRangeCompressionLine
import Network.AWS.MediaConvert.Types.Eac3DynamicRangeCompressionRf
import Network.AWS.MediaConvert.Types.Eac3LfeControl
import Network.AWS.MediaConvert.Types.Eac3LfeFilter
import Network.AWS.MediaConvert.Types.Eac3MetadataControl
import Network.AWS.MediaConvert.Types.Eac3PassthroughControl
import Network.AWS.MediaConvert.Types.Eac3PhaseControl
import Network.AWS.MediaConvert.Types.Eac3StereoDownmix
import Network.AWS.MediaConvert.Types.Eac3SurroundExMode
import Network.AWS.MediaConvert.Types.Eac3SurroundMode
import Network.AWS.Prelude

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings) to the value EAC3.
--
-- /See:/ 'eac3Settings' smart constructor.
data Eac3Settings = Eac3Settings'
  { _esStereoDownmix ::
      !(Maybe Eac3StereoDownmix),
    _esLoRoCenterMixLevel :: !(Maybe Double),
    _esLtRtCenterMixLevel :: !(Maybe Double),
    _esLfeFilter :: !(Maybe Eac3LfeFilter),
    _esDynamicRangeCompressionLine ::
      !(Maybe Eac3DynamicRangeCompressionLine),
    _esLtRtSurroundMixLevel :: !(Maybe Double),
    _esMetadataControl :: !(Maybe Eac3MetadataControl),
    _esLoRoSurroundMixLevel :: !(Maybe Double),
    _esSurroundMode :: !(Maybe Eac3SurroundMode),
    _esAttenuationControl :: !(Maybe Eac3AttenuationControl),
    _esPassthroughControl :: !(Maybe Eac3PassthroughControl),
    _esBitstreamMode :: !(Maybe Eac3BitstreamMode),
    _esLfeControl :: !(Maybe Eac3LfeControl),
    _esDynamicRangeCompressionRf ::
      !(Maybe Eac3DynamicRangeCompressionRf),
    _esCodingMode :: !(Maybe Eac3CodingMode),
    _esSampleRate :: !(Maybe Nat),
    _esDcFilter :: !(Maybe Eac3DcFilter),
    _esBitrate :: !(Maybe Nat),
    _esPhaseControl :: !(Maybe Eac3PhaseControl),
    _esSurroundExMode :: !(Maybe Eac3SurroundExMode),
    _esDialnorm :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Eac3Settings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esStereoDownmix' - Choose how the service does stereo downmixing. This setting only applies if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Stereo downmix (Eac3StereoDownmix).
--
-- * 'esLoRoCenterMixLevel' - Specify a value for the following Dolby Digital Plus setting: Left only/Right only center mix (Lo/Ro center). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3StereoDownmix). Valid values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel. This setting applies only if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Left only/Right only center (loRoCenterMixLevel).
--
-- * 'esLtRtCenterMixLevel' - Specify a value for the following Dolby Digital Plus setting: Left total/Right total center mix (Lt/Rt center). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3StereoDownmix). Valid values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel. This setting applies only if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Left total/Right total center (ltRtCenterMixLevel).
--
-- * 'esLfeFilter' - Applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid with 3_2_LFE coding mode.
--
-- * 'esDynamicRangeCompressionLine' - Specify the absolute peak level for a signal with dynamic range compression.
--
-- * 'esLtRtSurroundMixLevel' - Specify a value for the following Dolby Digital Plus setting: Left total/Right total surround mix (Lt/Rt surround). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3StereoDownmix). Valid values: -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel. This setting applies only if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Left total/Right total surround (ltRtSurroundMixLevel).
--
-- * 'esMetadataControl' - When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.
--
-- * 'esLoRoSurroundMixLevel' - Specify a value for the following Dolby Digital Plus setting: Left only/Right only (Lo/Ro surround). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3StereoDownmix). Valid values: -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel. This setting applies only if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Left only/Right only surround (loRoSurroundMixLevel).
--
-- * 'esSurroundMode' - When encoding 2/0 audio, sets whether Dolby Surround is matrix encoded into the two channels.
--
-- * 'esAttenuationControl' - If set to ATTENUATE_3_DB, applies a 3 dB attenuation to the surround channels. Only used for 3/2 coding mode.
--
-- * 'esPassthroughControl' - When set to WHEN_POSSIBLE, input DD+ audio will be passed through if it is present on the input. this detection is dynamic over the life of the transcode. Inputs that alternate between DD+ and non-DD+ content will have a consistent DD+ output as the system alternates between passthrough and encoding.
--
-- * 'esBitstreamMode' - Specify the bitstream mode for the E-AC-3 stream that the encoder emits. For more information about the EAC3 bitstream mode, see ATSC A/52-2012 (Annex E).
--
-- * 'esLfeControl' - When encoding 3/2 audio, controls whether the LFE channel is enabled
--
-- * 'esDynamicRangeCompressionRf' - Specify how the service limits the audio dynamic range when compressing the audio.
--
-- * 'esCodingMode' - Dolby Digital Plus coding mode. Determines number of channels.
--
-- * 'esSampleRate' - This value is always 48000. It represents the sample rate in Hz.
--
-- * 'esDcFilter' - Activates a DC highpass filter for all input channels.
--
-- * 'esBitrate' - Specify the average bitrate in bits per second. Valid bitrates depend on the coding mode.
--
-- * 'esPhaseControl' - Controls the amount of phase-shift applied to the surround channels. Only used for 3/2 coding mode.
--
-- * 'esSurroundExMode' - When encoding 3/2 audio, sets whether an extra center back surround channel is matrix encoded into the left and right surround channels.
--
-- * 'esDialnorm' - Sets the dialnorm for the output. If blank and input audio is Dolby Digital Plus, dialnorm will be passed through.
eac3Settings ::
  Eac3Settings
eac3Settings =
  Eac3Settings'
    { _esStereoDownmix = Nothing,
      _esLoRoCenterMixLevel = Nothing,
      _esLtRtCenterMixLevel = Nothing,
      _esLfeFilter = Nothing,
      _esDynamicRangeCompressionLine = Nothing,
      _esLtRtSurroundMixLevel = Nothing,
      _esMetadataControl = Nothing,
      _esLoRoSurroundMixLevel = Nothing,
      _esSurroundMode = Nothing,
      _esAttenuationControl = Nothing,
      _esPassthroughControl = Nothing,
      _esBitstreamMode = Nothing,
      _esLfeControl = Nothing,
      _esDynamicRangeCompressionRf = Nothing,
      _esCodingMode = Nothing,
      _esSampleRate = Nothing,
      _esDcFilter = Nothing,
      _esBitrate = Nothing,
      _esPhaseControl = Nothing,
      _esSurroundExMode = Nothing,
      _esDialnorm = Nothing
    }

-- | Choose how the service does stereo downmixing. This setting only applies if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Stereo downmix (Eac3StereoDownmix).
esStereoDownmix :: Lens' Eac3Settings (Maybe Eac3StereoDownmix)
esStereoDownmix = lens _esStereoDownmix (\s a -> s {_esStereoDownmix = a})

-- | Specify a value for the following Dolby Digital Plus setting: Left only/Right only center mix (Lo/Ro center). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3StereoDownmix). Valid values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel. This setting applies only if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Left only/Right only center (loRoCenterMixLevel).
esLoRoCenterMixLevel :: Lens' Eac3Settings (Maybe Double)
esLoRoCenterMixLevel = lens _esLoRoCenterMixLevel (\s a -> s {_esLoRoCenterMixLevel = a})

-- | Specify a value for the following Dolby Digital Plus setting: Left total/Right total center mix (Lt/Rt center). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3StereoDownmix). Valid values: 3.0, 1.5, 0.0, -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel. This setting applies only if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Left total/Right total center (ltRtCenterMixLevel).
esLtRtCenterMixLevel :: Lens' Eac3Settings (Maybe Double)
esLtRtCenterMixLevel = lens _esLtRtCenterMixLevel (\s a -> s {_esLtRtCenterMixLevel = a})

-- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding. Only valid with 3_2_LFE coding mode.
esLfeFilter :: Lens' Eac3Settings (Maybe Eac3LfeFilter)
esLfeFilter = lens _esLfeFilter (\s a -> s {_esLfeFilter = a})

-- | Specify the absolute peak level for a signal with dynamic range compression.
esDynamicRangeCompressionLine :: Lens' Eac3Settings (Maybe Eac3DynamicRangeCompressionLine)
esDynamicRangeCompressionLine = lens _esDynamicRangeCompressionLine (\s a -> s {_esDynamicRangeCompressionLine = a})

-- | Specify a value for the following Dolby Digital Plus setting: Left total/Right total surround mix (Lt/Rt surround). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3StereoDownmix). Valid values: -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel. This setting applies only if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Left total/Right total surround (ltRtSurroundMixLevel).
esLtRtSurroundMixLevel :: Lens' Eac3Settings (Maybe Double)
esLtRtSurroundMixLevel = lens _esLtRtSurroundMixLevel (\s a -> s {_esLtRtSurroundMixLevel = a})

-- | When set to FOLLOW_INPUT, encoder metadata will be sourced from the DD, DD+, or DolbyE decoder that supplied this audio data. If audio was not supplied from one of these streams, then the static metadata settings will be used.
esMetadataControl :: Lens' Eac3Settings (Maybe Eac3MetadataControl)
esMetadataControl = lens _esMetadataControl (\s a -> s {_esMetadataControl = a})

-- | Specify a value for the following Dolby Digital Plus setting: Left only/Right only (Lo/Ro surround). MediaConvert uses this value for downmixing. How the service uses this value depends on the value that you choose for Stereo downmix (Eac3StereoDownmix). Valid values: -1.5, -3.0, -4.5, -6.0, and -60. The value -60 mutes the channel. This setting applies only if you keep the default value of 3/2 - L, R, C, Ls, Rs (CODING_MODE_3_2) for the setting Coding mode (Eac3CodingMode). If you choose a different value for Coding mode, the service ignores Left only/Right only surround (loRoSurroundMixLevel).
esLoRoSurroundMixLevel :: Lens' Eac3Settings (Maybe Double)
esLoRoSurroundMixLevel = lens _esLoRoSurroundMixLevel (\s a -> s {_esLoRoSurroundMixLevel = a})

-- | When encoding 2/0 audio, sets whether Dolby Surround is matrix encoded into the two channels.
esSurroundMode :: Lens' Eac3Settings (Maybe Eac3SurroundMode)
esSurroundMode = lens _esSurroundMode (\s a -> s {_esSurroundMode = a})

-- | If set to ATTENUATE_3_DB, applies a 3 dB attenuation to the surround channels. Only used for 3/2 coding mode.
esAttenuationControl :: Lens' Eac3Settings (Maybe Eac3AttenuationControl)
esAttenuationControl = lens _esAttenuationControl (\s a -> s {_esAttenuationControl = a})

-- | When set to WHEN_POSSIBLE, input DD+ audio will be passed through if it is present on the input. this detection is dynamic over the life of the transcode. Inputs that alternate between DD+ and non-DD+ content will have a consistent DD+ output as the system alternates between passthrough and encoding.
esPassthroughControl :: Lens' Eac3Settings (Maybe Eac3PassthroughControl)
esPassthroughControl = lens _esPassthroughControl (\s a -> s {_esPassthroughControl = a})

-- | Specify the bitstream mode for the E-AC-3 stream that the encoder emits. For more information about the EAC3 bitstream mode, see ATSC A/52-2012 (Annex E).
esBitstreamMode :: Lens' Eac3Settings (Maybe Eac3BitstreamMode)
esBitstreamMode = lens _esBitstreamMode (\s a -> s {_esBitstreamMode = a})

-- | When encoding 3/2 audio, controls whether the LFE channel is enabled
esLfeControl :: Lens' Eac3Settings (Maybe Eac3LfeControl)
esLfeControl = lens _esLfeControl (\s a -> s {_esLfeControl = a})

-- | Specify how the service limits the audio dynamic range when compressing the audio.
esDynamicRangeCompressionRf :: Lens' Eac3Settings (Maybe Eac3DynamicRangeCompressionRf)
esDynamicRangeCompressionRf = lens _esDynamicRangeCompressionRf (\s a -> s {_esDynamicRangeCompressionRf = a})

-- | Dolby Digital Plus coding mode. Determines number of channels.
esCodingMode :: Lens' Eac3Settings (Maybe Eac3CodingMode)
esCodingMode = lens _esCodingMode (\s a -> s {_esCodingMode = a})

-- | This value is always 48000. It represents the sample rate in Hz.
esSampleRate :: Lens' Eac3Settings (Maybe Natural)
esSampleRate = lens _esSampleRate (\s a -> s {_esSampleRate = a}) . mapping _Nat

-- | Activates a DC highpass filter for all input channels.
esDcFilter :: Lens' Eac3Settings (Maybe Eac3DcFilter)
esDcFilter = lens _esDcFilter (\s a -> s {_esDcFilter = a})

-- | Specify the average bitrate in bits per second. Valid bitrates depend on the coding mode.
esBitrate :: Lens' Eac3Settings (Maybe Natural)
esBitrate = lens _esBitrate (\s a -> s {_esBitrate = a}) . mapping _Nat

-- | Controls the amount of phase-shift applied to the surround channels. Only used for 3/2 coding mode.
esPhaseControl :: Lens' Eac3Settings (Maybe Eac3PhaseControl)
esPhaseControl = lens _esPhaseControl (\s a -> s {_esPhaseControl = a})

-- | When encoding 3/2 audio, sets whether an extra center back surround channel is matrix encoded into the left and right surround channels.
esSurroundExMode :: Lens' Eac3Settings (Maybe Eac3SurroundExMode)
esSurroundExMode = lens _esSurroundExMode (\s a -> s {_esSurroundExMode = a})

-- | Sets the dialnorm for the output. If blank and input audio is Dolby Digital Plus, dialnorm will be passed through.
esDialnorm :: Lens' Eac3Settings (Maybe Natural)
esDialnorm = lens _esDialnorm (\s a -> s {_esDialnorm = a}) . mapping _Nat

instance FromJSON Eac3Settings where
  parseJSON =
    withObject
      "Eac3Settings"
      ( \x ->
          Eac3Settings'
            <$> (x .:? "stereoDownmix")
            <*> (x .:? "loRoCenterMixLevel")
            <*> (x .:? "ltRtCenterMixLevel")
            <*> (x .:? "lfeFilter")
            <*> (x .:? "dynamicRangeCompressionLine")
            <*> (x .:? "ltRtSurroundMixLevel")
            <*> (x .:? "metadataControl")
            <*> (x .:? "loRoSurroundMixLevel")
            <*> (x .:? "surroundMode")
            <*> (x .:? "attenuationControl")
            <*> (x .:? "passthroughControl")
            <*> (x .:? "bitstreamMode")
            <*> (x .:? "lfeControl")
            <*> (x .:? "dynamicRangeCompressionRf")
            <*> (x .:? "codingMode")
            <*> (x .:? "sampleRate")
            <*> (x .:? "dcFilter")
            <*> (x .:? "bitrate")
            <*> (x .:? "phaseControl")
            <*> (x .:? "surroundExMode")
            <*> (x .:? "dialnorm")
      )

instance Hashable Eac3Settings

instance NFData Eac3Settings

instance ToJSON Eac3Settings where
  toJSON Eac3Settings' {..} =
    object
      ( catMaybes
          [ ("stereoDownmix" .=) <$> _esStereoDownmix,
            ("loRoCenterMixLevel" .=) <$> _esLoRoCenterMixLevel,
            ("ltRtCenterMixLevel" .=) <$> _esLtRtCenterMixLevel,
            ("lfeFilter" .=) <$> _esLfeFilter,
            ("dynamicRangeCompressionLine" .=)
              <$> _esDynamicRangeCompressionLine,
            ("ltRtSurroundMixLevel" .=) <$> _esLtRtSurroundMixLevel,
            ("metadataControl" .=) <$> _esMetadataControl,
            ("loRoSurroundMixLevel" .=) <$> _esLoRoSurroundMixLevel,
            ("surroundMode" .=) <$> _esSurroundMode,
            ("attenuationControl" .=) <$> _esAttenuationControl,
            ("passthroughControl" .=) <$> _esPassthroughControl,
            ("bitstreamMode" .=) <$> _esBitstreamMode,
            ("lfeControl" .=) <$> _esLfeControl,
            ("dynamicRangeCompressionRf" .=) <$> _esDynamicRangeCompressionRf,
            ("codingMode" .=) <$> _esCodingMode,
            ("sampleRate" .=) <$> _esSampleRate,
            ("dcFilter" .=) <$> _esDcFilter,
            ("bitrate" .=) <$> _esBitrate,
            ("phaseControl" .=) <$> _esPhaseControl,
            ("surroundExMode" .=) <$> _esSurroundExMode,
            ("dialnorm" .=) <$> _esDialnorm
          ]
      )
