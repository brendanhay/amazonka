{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.AudioParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.AudioParameters where

import Network.AWS.ElasticTranscoder.Types.AudioCodecOptions
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Parameters required for transcoding audio.
--
--
--
-- /See:/ 'audioParameters' smart constructor.
data AudioParameters = AudioParameters'
  { _apChannels ::
      !(Maybe Text),
    _apCodec :: !(Maybe Text),
    _apAudioPackingMode :: !(Maybe Text),
    _apSampleRate :: !(Maybe Text),
    _apBitRate :: !(Maybe Text),
    _apCodecOptions :: !(Maybe AudioCodecOptions)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AudioParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apChannels' - The number of audio channels in the output file. The following values are valid: @auto@ , @0@ , @1@ , @2@  One channel carries the information played by a single speaker. For example, a stereo track with two channels sends one channel to the left speaker, and the other channel to the right speaker. The output channels are organized into tracks. If you want Elastic Transcoder to automatically detect the number of audio channels in the input file and use that value for the output file, select @auto@ . The output of a specific channel value and inputs are as follows:     * @auto@ __channel specified, with any input:__ Pass through up to eight input channels.     * @0@ __channels specified, with any input:__ Audio omitted from the output.     * @1@ __channel specified, with at least one input channel:__ Mono sound.     * @2@ __channels specified, with any input:__ Two identical mono channels or stereo. For more information about tracks, see @Audio:AudioPackingMode.@  For more information about how Elastic Transcoder organizes channels and tracks, see @Audio:AudioPackingMode@ .
--
-- * 'apCodec' - The audio codec for the output file. Valid values include @aac@ , @flac@ , @mp2@ , @mp3@ , @pcm@ , and @vorbis@ .
--
-- * 'apAudioPackingMode' - The method of organizing audio channels and tracks. Use @Audio:Channels@ to specify the number of channels in your output, and @Audio:AudioPackingMode@ to specify the number of tracks and their relation to the channels. If you do not specify an @Audio:AudioPackingMode@ , Elastic Transcoder uses @SingleTrack@ . The following values are valid: @SingleTrack@ , @OneChannelPerTrack@ , and @OneChannelPerTrackWithMosTo8Tracks@  When you specify @SingleTrack@ , Elastic Transcoder creates a single track for your output. The track can have up to eight channels. Use @SingleTrack@ for all non-@mxf@ containers. The outputs of @SingleTrack@ for a specific channel value and inputs are as follows:     * @0@ __channels with any input:__ Audio omitted from the output     * @1, 2, or auto @ __channels with no audio input:__ Audio omitted from the output     * @1 @ __channel with any input with audio:__ One track with one channel, downmixed if necessary     * @2 @ __channels with one track with one channel:__ One track with two identical channels     * @2 or auto @ __channels with two tracks with one channel each:__ One track with two channels     * @2 or auto @ __channels with one track with two channels:__ One track with two channels     * @2 @ __channels with one track with multiple channels:__ One track with two channels     * @auto @ __channels with one track with one channel:__ One track with one channel     * @auto @ __channels with one track with multiple channels:__ One track with multiple channels When you specify @OneChannelPerTrack@ , Elastic Transcoder creates a new track for every channel in your output. Your output can have up to eight single-channel tracks. The outputs of @OneChannelPerTrack@ for a specific channel value and inputs are as follows:     * @0 @ __channels with any input:__ Audio omitted from the output     * @1, 2, or auto @ __channels with no audio input:__ Audio omitted from the output     * @1 @ __channel with any input with audio:__ One track with one channel, downmixed if necessary     * @2 @ __channels with one track with one channel:__ Two tracks with one identical channel each     * @2 or auto @ __channels with two tracks with one channel each:__ Two tracks with one channel each     * @2 or auto @ __channels with one track with two channels:__ Two tracks with one channel each     * @2 @ __channels with one track with multiple channels:__ Two tracks with one channel each     * @auto @ __channels with one track with one channel:__ One track with one channel     * @auto @ __channels with one track with multiple channels:__ Up to eight tracks with one channel each When you specify @OneChannelPerTrackWithMosTo8Tracks@ , Elastic Transcoder creates eight single-channel tracks for your output. All tracks that do not contain audio data from an input channel are MOS, or Mit Out Sound, tracks. The outputs of @OneChannelPerTrackWithMosTo8Tracks@ for a specific channel value and inputs are as follows:     * @0 @ __channels with any input:__ Audio omitted from the output     * @1, 2, or auto @ __channels with no audio input:__ Audio omitted from the output     * @1 @ __channel with any input with audio:__ One track with one channel, downmixed if necessary, plus six MOS tracks     * @2 @ __channels with one track with one channel:__ Two tracks with one identical channel each, plus six MOS tracks     * @2 or auto @ __channels with two tracks with one channel each:__ Two tracks with one channel each, plus six MOS tracks     * @2 or auto @ __channels with one track with two channels:__ Two tracks with one channel each, plus six MOS tracks     * @2 @ __channels with one track with multiple channels:__ Two tracks with one channel each, plus six MOS tracks     * @auto @ __channels with one track with one channel:__ One track with one channel, plus seven MOS tracks     * @auto @ __channels with one track with multiple channels:__ Up to eight tracks with one channel each, plus MOS tracks until there are eight tracks in all
--
-- * 'apSampleRate' - The sample rate of the audio stream in the output file, in Hertz. Valid values include: @auto@ , @22050@ , @32000@ , @44100@ , @48000@ , @96000@  If you specify @auto@ , Elastic Transcoder automatically detects the sample rate.
--
-- * 'apBitRate' - The bit rate of the audio stream in the output file, in kilobits/second. Enter an integer between 64 and 320, inclusive.
--
-- * 'apCodecOptions' - If you specified @AAC@ for @Audio:Codec@ , this is the @AAC@ compression profile to use. Valid values include: @auto@ , @AAC-LC@ , @HE-AAC@ , @HE-AACv2@  If you specify @auto@ , Elastic Transcoder chooses a profile based on the bit rate of the output file.
audioParameters ::
  AudioParameters
audioParameters =
  AudioParameters'
    { _apChannels = Nothing,
      _apCodec = Nothing,
      _apAudioPackingMode = Nothing,
      _apSampleRate = Nothing,
      _apBitRate = Nothing,
      _apCodecOptions = Nothing
    }

-- | The number of audio channels in the output file. The following values are valid: @auto@ , @0@ , @1@ , @2@  One channel carries the information played by a single speaker. For example, a stereo track with two channels sends one channel to the left speaker, and the other channel to the right speaker. The output channels are organized into tracks. If you want Elastic Transcoder to automatically detect the number of audio channels in the input file and use that value for the output file, select @auto@ . The output of a specific channel value and inputs are as follows:     * @auto@ __channel specified, with any input:__ Pass through up to eight input channels.     * @0@ __channels specified, with any input:__ Audio omitted from the output.     * @1@ __channel specified, with at least one input channel:__ Mono sound.     * @2@ __channels specified, with any input:__ Two identical mono channels or stereo. For more information about tracks, see @Audio:AudioPackingMode.@  For more information about how Elastic Transcoder organizes channels and tracks, see @Audio:AudioPackingMode@ .
apChannels :: Lens' AudioParameters (Maybe Text)
apChannels = lens _apChannels (\s a -> s {_apChannels = a})

-- | The audio codec for the output file. Valid values include @aac@ , @flac@ , @mp2@ , @mp3@ , @pcm@ , and @vorbis@ .
apCodec :: Lens' AudioParameters (Maybe Text)
apCodec = lens _apCodec (\s a -> s {_apCodec = a})

-- | The method of organizing audio channels and tracks. Use @Audio:Channels@ to specify the number of channels in your output, and @Audio:AudioPackingMode@ to specify the number of tracks and their relation to the channels. If you do not specify an @Audio:AudioPackingMode@ , Elastic Transcoder uses @SingleTrack@ . The following values are valid: @SingleTrack@ , @OneChannelPerTrack@ , and @OneChannelPerTrackWithMosTo8Tracks@  When you specify @SingleTrack@ , Elastic Transcoder creates a single track for your output. The track can have up to eight channels. Use @SingleTrack@ for all non-@mxf@ containers. The outputs of @SingleTrack@ for a specific channel value and inputs are as follows:     * @0@ __channels with any input:__ Audio omitted from the output     * @1, 2, or auto @ __channels with no audio input:__ Audio omitted from the output     * @1 @ __channel with any input with audio:__ One track with one channel, downmixed if necessary     * @2 @ __channels with one track with one channel:__ One track with two identical channels     * @2 or auto @ __channels with two tracks with one channel each:__ One track with two channels     * @2 or auto @ __channels with one track with two channels:__ One track with two channels     * @2 @ __channels with one track with multiple channels:__ One track with two channels     * @auto @ __channels with one track with one channel:__ One track with one channel     * @auto @ __channels with one track with multiple channels:__ One track with multiple channels When you specify @OneChannelPerTrack@ , Elastic Transcoder creates a new track for every channel in your output. Your output can have up to eight single-channel tracks. The outputs of @OneChannelPerTrack@ for a specific channel value and inputs are as follows:     * @0 @ __channels with any input:__ Audio omitted from the output     * @1, 2, or auto @ __channels with no audio input:__ Audio omitted from the output     * @1 @ __channel with any input with audio:__ One track with one channel, downmixed if necessary     * @2 @ __channels with one track with one channel:__ Two tracks with one identical channel each     * @2 or auto @ __channels with two tracks with one channel each:__ Two tracks with one channel each     * @2 or auto @ __channels with one track with two channels:__ Two tracks with one channel each     * @2 @ __channels with one track with multiple channels:__ Two tracks with one channel each     * @auto @ __channels with one track with one channel:__ One track with one channel     * @auto @ __channels with one track with multiple channels:__ Up to eight tracks with one channel each When you specify @OneChannelPerTrackWithMosTo8Tracks@ , Elastic Transcoder creates eight single-channel tracks for your output. All tracks that do not contain audio data from an input channel are MOS, or Mit Out Sound, tracks. The outputs of @OneChannelPerTrackWithMosTo8Tracks@ for a specific channel value and inputs are as follows:     * @0 @ __channels with any input:__ Audio omitted from the output     * @1, 2, or auto @ __channels with no audio input:__ Audio omitted from the output     * @1 @ __channel with any input with audio:__ One track with one channel, downmixed if necessary, plus six MOS tracks     * @2 @ __channels with one track with one channel:__ Two tracks with one identical channel each, plus six MOS tracks     * @2 or auto @ __channels with two tracks with one channel each:__ Two tracks with one channel each, plus six MOS tracks     * @2 or auto @ __channels with one track with two channels:__ Two tracks with one channel each, plus six MOS tracks     * @2 @ __channels with one track with multiple channels:__ Two tracks with one channel each, plus six MOS tracks     * @auto @ __channels with one track with one channel:__ One track with one channel, plus seven MOS tracks     * @auto @ __channels with one track with multiple channels:__ Up to eight tracks with one channel each, plus MOS tracks until there are eight tracks in all
apAudioPackingMode :: Lens' AudioParameters (Maybe Text)
apAudioPackingMode = lens _apAudioPackingMode (\s a -> s {_apAudioPackingMode = a})

-- | The sample rate of the audio stream in the output file, in Hertz. Valid values include: @auto@ , @22050@ , @32000@ , @44100@ , @48000@ , @96000@  If you specify @auto@ , Elastic Transcoder automatically detects the sample rate.
apSampleRate :: Lens' AudioParameters (Maybe Text)
apSampleRate = lens _apSampleRate (\s a -> s {_apSampleRate = a})

-- | The bit rate of the audio stream in the output file, in kilobits/second. Enter an integer between 64 and 320, inclusive.
apBitRate :: Lens' AudioParameters (Maybe Text)
apBitRate = lens _apBitRate (\s a -> s {_apBitRate = a})

-- | If you specified @AAC@ for @Audio:Codec@ , this is the @AAC@ compression profile to use. Valid values include: @auto@ , @AAC-LC@ , @HE-AAC@ , @HE-AACv2@  If you specify @auto@ , Elastic Transcoder chooses a profile based on the bit rate of the output file.
apCodecOptions :: Lens' AudioParameters (Maybe AudioCodecOptions)
apCodecOptions = lens _apCodecOptions (\s a -> s {_apCodecOptions = a})

instance FromJSON AudioParameters where
  parseJSON =
    withObject
      "AudioParameters"
      ( \x ->
          AudioParameters'
            <$> (x .:? "Channels")
            <*> (x .:? "Codec")
            <*> (x .:? "AudioPackingMode")
            <*> (x .:? "SampleRate")
            <*> (x .:? "BitRate")
            <*> (x .:? "CodecOptions")
      )

instance Hashable AudioParameters

instance NFData AudioParameters

instance ToJSON AudioParameters where
  toJSON AudioParameters' {..} =
    object
      ( catMaybes
          [ ("Channels" .=) <$> _apChannels,
            ("Codec" .=) <$> _apCodec,
            ("AudioPackingMode" .=) <$> _apAudioPackingMode,
            ("SampleRate" .=) <$> _apSampleRate,
            ("BitRate" .=) <$> _apBitRate,
            ("CodecOptions" .=) <$> _apCodecOptions
          ]
      )
