-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.AudioParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.AudioParameters
  ( AudioParameters (..),

    -- * Smart constructor
    mkAudioParameters,

    -- * Lenses
    apChannels,
    apCodec,
    apAudioPackingMode,
    apSampleRate,
    apBitRate,
    apCodecOptions,
  )
where

import Network.AWS.ElasticTranscoder.Types.AudioCodecOptions
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Parameters required for transcoding audio.
--
-- /See:/ 'mkAudioParameters' smart constructor.
data AudioParameters = AudioParameters'
  { channels ::
      Lude.Maybe Lude.Text,
    codec :: Lude.Maybe Lude.Text,
    audioPackingMode :: Lude.Maybe Lude.Text,
    sampleRate :: Lude.Maybe Lude.Text,
    bitRate :: Lude.Maybe Lude.Text,
    codecOptions :: Lude.Maybe AudioCodecOptions
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AudioParameters' with the minimum fields required to make a request.
--
-- * 'audioPackingMode' - The method of organizing audio channels and tracks. Use @Audio:Channels@ to specify the number of channels in your output, and @Audio:AudioPackingMode@ to specify the number of tracks and their relation to the channels. If you do not specify an @Audio:AudioPackingMode@ , Elastic Transcoder uses @SingleTrack@ .
--
-- The following values are valid:
-- @SingleTrack@ , @OneChannelPerTrack@ , and @OneChannelPerTrackWithMosTo8Tracks@
-- When you specify @SingleTrack@ , Elastic Transcoder creates a single track for your output. The track can have up to eight channels. Use @SingleTrack@ for all non-@mxf@ containers.
-- The outputs of @SingleTrack@ for a specific channel value and inputs are as follows:
--
--     * @0@ __channels with any input:__ Audio omitted from the output
--
--
--     * @1, 2, or auto @ __channels with no audio input:__ Audio omitted from the output
--
--
--     * @1 @ __channel with any input with audio:__ One track with one channel, downmixed if necessary
--
--
--     * @2 @ __channels with one track with one channel:__ One track with two identical channels
--
--
--     * @2 or auto @ __channels with two tracks with one channel each:__ One track with two channels
--
--
--     * @2 or auto @ __channels with one track with two channels:__ One track with two channels
--
--
--     * @2 @ __channels with one track with multiple channels:__ One track with two channels
--
--
--     * @auto @ __channels with one track with one channel:__ One track with one channel
--
--
--     * @auto @ __channels with one track with multiple channels:__ One track with multiple channels
--
--
-- When you specify @OneChannelPerTrack@ , Elastic Transcoder creates a new track for every channel in your output. Your output can have up to eight single-channel tracks.
-- The outputs of @OneChannelPerTrack@ for a specific channel value and inputs are as follows:
--
--     * @0 @ __channels with any input:__ Audio omitted from the output
--
--
--     * @1, 2, or auto @ __channels with no audio input:__ Audio omitted from the output
--
--
--     * @1 @ __channel with any input with audio:__ One track with one channel, downmixed if necessary
--
--
--     * @2 @ __channels with one track with one channel:__ Two tracks with one identical channel each
--
--
--     * @2 or auto @ __channels with two tracks with one channel each:__ Two tracks with one channel each
--
--
--     * @2 or auto @ __channels with one track with two channels:__ Two tracks with one channel each
--
--
--     * @2 @ __channels with one track with multiple channels:__ Two tracks with one channel each
--
--
--     * @auto @ __channels with one track with one channel:__ One track with one channel
--
--
--     * @auto @ __channels with one track with multiple channels:__ Up to eight tracks with one channel each
--
--
-- When you specify @OneChannelPerTrackWithMosTo8Tracks@ , Elastic Transcoder creates eight single-channel tracks for your output. All tracks that do not contain audio data from an input channel are MOS, or Mit Out Sound, tracks.
-- The outputs of @OneChannelPerTrackWithMosTo8Tracks@ for a specific channel value and inputs are as follows:
--
--     * @0 @ __channels with any input:__ Audio omitted from the output
--
--
--     * @1, 2, or auto @ __channels with no audio input:__ Audio omitted from the output
--
--
--     * @1 @ __channel with any input with audio:__ One track with one channel, downmixed if necessary, plus six MOS tracks
--
--
--     * @2 @ __channels with one track with one channel:__ Two tracks with one identical channel each, plus six MOS tracks
--
--
--     * @2 or auto @ __channels with two tracks with one channel each:__ Two tracks with one channel each, plus six MOS tracks
--
--
--     * @2 or auto @ __channels with one track with two channels:__ Two tracks with one channel each, plus six MOS tracks
--
--
--     * @2 @ __channels with one track with multiple channels:__ Two tracks with one channel each, plus six MOS tracks
--
--
--     * @auto @ __channels with one track with one channel:__ One track with one channel, plus seven MOS tracks
--
--
--     * @auto @ __channels with one track with multiple channels:__ Up to eight tracks with one channel each, plus MOS tracks until there are eight tracks in all
--
--
-- * 'bitRate' - The bit rate of the audio stream in the output file, in kilobits/second. Enter an integer between 64 and 320, inclusive.
-- * 'channels' - The number of audio channels in the output file. The following values are valid:
--
-- @auto@ , @0@ , @1@ , @2@
-- One channel carries the information played by a single speaker. For example, a stereo track with two channels sends one channel to the left speaker, and the other channel to the right speaker. The output channels are organized into tracks. If you want Elastic Transcoder to automatically detect the number of audio channels in the input file and use that value for the output file, select @auto@ .
-- The output of a specific channel value and inputs are as follows:
--
--     * @auto@ __channel specified, with any input:__ Pass through up to eight input channels.
--
--
--     * @0@ __channels specified, with any input:__ Audio omitted from the output.
--
--
--     * @1@ __channel specified, with at least one input channel:__ Mono sound.
--
--
--     * @2@ __channels specified, with any input:__ Two identical mono channels or stereo. For more information about tracks, see @Audio:AudioPackingMode.@
--
--
-- For more information about how Elastic Transcoder organizes channels and tracks, see @Audio:AudioPackingMode@ .
-- * 'codec' - The audio codec for the output file. Valid values include @aac@ , @flac@ , @mp2@ , @mp3@ , @pcm@ , and @vorbis@ .
-- * 'codecOptions' - If you specified @AAC@ for @Audio:Codec@ , this is the @AAC@ compression profile to use. Valid values include:
--
-- @auto@ , @AAC-LC@ , @HE-AAC@ , @HE-AACv2@
-- If you specify @auto@ , Elastic Transcoder chooses a profile based on the bit rate of the output file.
-- * 'sampleRate' - The sample rate of the audio stream in the output file, in Hertz. Valid values include:
--
-- @auto@ , @22050@ , @32000@ , @44100@ , @48000@ , @96000@
-- If you specify @auto@ , Elastic Transcoder automatically detects the sample rate.
mkAudioParameters ::
  AudioParameters
mkAudioParameters =
  AudioParameters'
    { channels = Lude.Nothing,
      codec = Lude.Nothing,
      audioPackingMode = Lude.Nothing,
      sampleRate = Lude.Nothing,
      bitRate = Lude.Nothing,
      codecOptions = Lude.Nothing
    }

-- | The number of audio channels in the output file. The following values are valid:
--
-- @auto@ , @0@ , @1@ , @2@
-- One channel carries the information played by a single speaker. For example, a stereo track with two channels sends one channel to the left speaker, and the other channel to the right speaker. The output channels are organized into tracks. If you want Elastic Transcoder to automatically detect the number of audio channels in the input file and use that value for the output file, select @auto@ .
-- The output of a specific channel value and inputs are as follows:
--
--     * @auto@ __channel specified, with any input:__ Pass through up to eight input channels.
--
--
--     * @0@ __channels specified, with any input:__ Audio omitted from the output.
--
--
--     * @1@ __channel specified, with at least one input channel:__ Mono sound.
--
--
--     * @2@ __channels specified, with any input:__ Two identical mono channels or stereo. For more information about tracks, see @Audio:AudioPackingMode.@
--
--
-- For more information about how Elastic Transcoder organizes channels and tracks, see @Audio:AudioPackingMode@ .
--
-- /Note:/ Consider using 'channels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apChannels :: Lens.Lens' AudioParameters (Lude.Maybe Lude.Text)
apChannels = Lens.lens (channels :: AudioParameters -> Lude.Maybe Lude.Text) (\s a -> s {channels = a} :: AudioParameters)
{-# DEPRECATED apChannels "Use generic-lens or generic-optics with 'channels' instead." #-}

-- | The audio codec for the output file. Valid values include @aac@ , @flac@ , @mp2@ , @mp3@ , @pcm@ , and @vorbis@ .
--
-- /Note:/ Consider using 'codec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apCodec :: Lens.Lens' AudioParameters (Lude.Maybe Lude.Text)
apCodec = Lens.lens (codec :: AudioParameters -> Lude.Maybe Lude.Text) (\s a -> s {codec = a} :: AudioParameters)
{-# DEPRECATED apCodec "Use generic-lens or generic-optics with 'codec' instead." #-}

-- | The method of organizing audio channels and tracks. Use @Audio:Channels@ to specify the number of channels in your output, and @Audio:AudioPackingMode@ to specify the number of tracks and their relation to the channels. If you do not specify an @Audio:AudioPackingMode@ , Elastic Transcoder uses @SingleTrack@ .
--
-- The following values are valid:
-- @SingleTrack@ , @OneChannelPerTrack@ , and @OneChannelPerTrackWithMosTo8Tracks@
-- When you specify @SingleTrack@ , Elastic Transcoder creates a single track for your output. The track can have up to eight channels. Use @SingleTrack@ for all non-@mxf@ containers.
-- The outputs of @SingleTrack@ for a specific channel value and inputs are as follows:
--
--     * @0@ __channels with any input:__ Audio omitted from the output
--
--
--     * @1, 2, or auto @ __channels with no audio input:__ Audio omitted from the output
--
--
--     * @1 @ __channel with any input with audio:__ One track with one channel, downmixed if necessary
--
--
--     * @2 @ __channels with one track with one channel:__ One track with two identical channels
--
--
--     * @2 or auto @ __channels with two tracks with one channel each:__ One track with two channels
--
--
--     * @2 or auto @ __channels with one track with two channels:__ One track with two channels
--
--
--     * @2 @ __channels with one track with multiple channels:__ One track with two channels
--
--
--     * @auto @ __channels with one track with one channel:__ One track with one channel
--
--
--     * @auto @ __channels with one track with multiple channels:__ One track with multiple channels
--
--
-- When you specify @OneChannelPerTrack@ , Elastic Transcoder creates a new track for every channel in your output. Your output can have up to eight single-channel tracks.
-- The outputs of @OneChannelPerTrack@ for a specific channel value and inputs are as follows:
--
--     * @0 @ __channels with any input:__ Audio omitted from the output
--
--
--     * @1, 2, or auto @ __channels with no audio input:__ Audio omitted from the output
--
--
--     * @1 @ __channel with any input with audio:__ One track with one channel, downmixed if necessary
--
--
--     * @2 @ __channels with one track with one channel:__ Two tracks with one identical channel each
--
--
--     * @2 or auto @ __channels with two tracks with one channel each:__ Two tracks with one channel each
--
--
--     * @2 or auto @ __channels with one track with two channels:__ Two tracks with one channel each
--
--
--     * @2 @ __channels with one track with multiple channels:__ Two tracks with one channel each
--
--
--     * @auto @ __channels with one track with one channel:__ One track with one channel
--
--
--     * @auto @ __channels with one track with multiple channels:__ Up to eight tracks with one channel each
--
--
-- When you specify @OneChannelPerTrackWithMosTo8Tracks@ , Elastic Transcoder creates eight single-channel tracks for your output. All tracks that do not contain audio data from an input channel are MOS, or Mit Out Sound, tracks.
-- The outputs of @OneChannelPerTrackWithMosTo8Tracks@ for a specific channel value and inputs are as follows:
--
--     * @0 @ __channels with any input:__ Audio omitted from the output
--
--
--     * @1, 2, or auto @ __channels with no audio input:__ Audio omitted from the output
--
--
--     * @1 @ __channel with any input with audio:__ One track with one channel, downmixed if necessary, plus six MOS tracks
--
--
--     * @2 @ __channels with one track with one channel:__ Two tracks with one identical channel each, plus six MOS tracks
--
--
--     * @2 or auto @ __channels with two tracks with one channel each:__ Two tracks with one channel each, plus six MOS tracks
--
--
--     * @2 or auto @ __channels with one track with two channels:__ Two tracks with one channel each, plus six MOS tracks
--
--
--     * @2 @ __channels with one track with multiple channels:__ Two tracks with one channel each, plus six MOS tracks
--
--
--     * @auto @ __channels with one track with one channel:__ One track with one channel, plus seven MOS tracks
--
--
--     * @auto @ __channels with one track with multiple channels:__ Up to eight tracks with one channel each, plus MOS tracks until there are eight tracks in all
--
--
--
-- /Note:/ Consider using 'audioPackingMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apAudioPackingMode :: Lens.Lens' AudioParameters (Lude.Maybe Lude.Text)
apAudioPackingMode = Lens.lens (audioPackingMode :: AudioParameters -> Lude.Maybe Lude.Text) (\s a -> s {audioPackingMode = a} :: AudioParameters)
{-# DEPRECATED apAudioPackingMode "Use generic-lens or generic-optics with 'audioPackingMode' instead." #-}

-- | The sample rate of the audio stream in the output file, in Hertz. Valid values include:
--
-- @auto@ , @22050@ , @32000@ , @44100@ , @48000@ , @96000@
-- If you specify @auto@ , Elastic Transcoder automatically detects the sample rate.
--
-- /Note:/ Consider using 'sampleRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apSampleRate :: Lens.Lens' AudioParameters (Lude.Maybe Lude.Text)
apSampleRate = Lens.lens (sampleRate :: AudioParameters -> Lude.Maybe Lude.Text) (\s a -> s {sampleRate = a} :: AudioParameters)
{-# DEPRECATED apSampleRate "Use generic-lens or generic-optics with 'sampleRate' instead." #-}

-- | The bit rate of the audio stream in the output file, in kilobits/second. Enter an integer between 64 and 320, inclusive.
--
-- /Note:/ Consider using 'bitRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apBitRate :: Lens.Lens' AudioParameters (Lude.Maybe Lude.Text)
apBitRate = Lens.lens (bitRate :: AudioParameters -> Lude.Maybe Lude.Text) (\s a -> s {bitRate = a} :: AudioParameters)
{-# DEPRECATED apBitRate "Use generic-lens or generic-optics with 'bitRate' instead." #-}

-- | If you specified @AAC@ for @Audio:Codec@ , this is the @AAC@ compression profile to use. Valid values include:
--
-- @auto@ , @AAC-LC@ , @HE-AAC@ , @HE-AACv2@
-- If you specify @auto@ , Elastic Transcoder chooses a profile based on the bit rate of the output file.
--
-- /Note:/ Consider using 'codecOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apCodecOptions :: Lens.Lens' AudioParameters (Lude.Maybe AudioCodecOptions)
apCodecOptions = Lens.lens (codecOptions :: AudioParameters -> Lude.Maybe AudioCodecOptions) (\s a -> s {codecOptions = a} :: AudioParameters)
{-# DEPRECATED apCodecOptions "Use generic-lens or generic-optics with 'codecOptions' instead." #-}

instance Lude.FromJSON AudioParameters where
  parseJSON =
    Lude.withObject
      "AudioParameters"
      ( \x ->
          AudioParameters'
            Lude.<$> (x Lude..:? "Channels")
            Lude.<*> (x Lude..:? "Codec")
            Lude.<*> (x Lude..:? "AudioPackingMode")
            Lude.<*> (x Lude..:? "SampleRate")
            Lude.<*> (x Lude..:? "BitRate")
            Lude.<*> (x Lude..:? "CodecOptions")
      )

instance Lude.ToJSON AudioParameters where
  toJSON AudioParameters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Channels" Lude..=) Lude.<$> channels,
            ("Codec" Lude..=) Lude.<$> codec,
            ("AudioPackingMode" Lude..=) Lude.<$> audioPackingMode,
            ("SampleRate" Lude..=) Lude.<$> sampleRate,
            ("BitRate" Lude..=) Lude.<$> bitRate,
            ("CodecOptions" Lude..=) Lude.<$> codecOptions
          ]
      )
