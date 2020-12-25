{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    apAudioPackingMode,
    apBitRate,
    apChannels,
    apCodec,
    apCodecOptions,
    apSampleRate,
  )
where

import qualified Network.AWS.ElasticTranscoder.Types.AudioCodecOptions as Types
import qualified Network.AWS.ElasticTranscoder.Types.AudioPackingMode as Types
import qualified Network.AWS.ElasticTranscoder.Types.BitRate as Types
import qualified Network.AWS.ElasticTranscoder.Types.Channels as Types
import qualified Network.AWS.ElasticTranscoder.Types.Codec as Types
import qualified Network.AWS.ElasticTranscoder.Types.SampleRate as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Parameters required for transcoding audio.
--
-- /See:/ 'mkAudioParameters' smart constructor.
data AudioParameters = AudioParameters'
  { -- | The method of organizing audio channels and tracks. Use @Audio:Channels@ to specify the number of channels in your output, and @Audio:AudioPackingMode@ to specify the number of tracks and their relation to the channels. If you do not specify an @Audio:AudioPackingMode@ , Elastic Transcoder uses @SingleTrack@ .
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
    audioPackingMode :: Core.Maybe Types.AudioPackingMode,
    -- | The bit rate of the audio stream in the output file, in kilobits/second. Enter an integer between 64 and 320, inclusive.
    bitRate :: Core.Maybe Types.BitRate,
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
    channels :: Core.Maybe Types.Channels,
    -- | The audio codec for the output file. Valid values include @aac@ , @flac@ , @mp2@ , @mp3@ , @pcm@ , and @vorbis@ .
    codec :: Core.Maybe Types.Codec,
    -- | If you specified @AAC@ for @Audio:Codec@ , this is the @AAC@ compression profile to use. Valid values include:
    --
    -- @auto@ , @AAC-LC@ , @HE-AAC@ , @HE-AACv2@
    -- If you specify @auto@ , Elastic Transcoder chooses a profile based on the bit rate of the output file.
    codecOptions :: Core.Maybe Types.AudioCodecOptions,
    -- | The sample rate of the audio stream in the output file, in Hertz. Valid values include:
    --
    -- @auto@ , @22050@ , @32000@ , @44100@ , @48000@ , @96000@
    -- If you specify @auto@ , Elastic Transcoder automatically detects the sample rate.
    sampleRate :: Core.Maybe Types.SampleRate
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AudioParameters' value with any optional fields omitted.
mkAudioParameters ::
  AudioParameters
mkAudioParameters =
  AudioParameters'
    { audioPackingMode = Core.Nothing,
      bitRate = Core.Nothing,
      channels = Core.Nothing,
      codec = Core.Nothing,
      codecOptions = Core.Nothing,
      sampleRate = Core.Nothing
    }

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
apAudioPackingMode :: Lens.Lens' AudioParameters (Core.Maybe Types.AudioPackingMode)
apAudioPackingMode = Lens.field @"audioPackingMode"
{-# DEPRECATED apAudioPackingMode "Use generic-lens or generic-optics with 'audioPackingMode' instead." #-}

-- | The bit rate of the audio stream in the output file, in kilobits/second. Enter an integer between 64 and 320, inclusive.
--
-- /Note:/ Consider using 'bitRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apBitRate :: Lens.Lens' AudioParameters (Core.Maybe Types.BitRate)
apBitRate = Lens.field @"bitRate"
{-# DEPRECATED apBitRate "Use generic-lens or generic-optics with 'bitRate' instead." #-}

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
apChannels :: Lens.Lens' AudioParameters (Core.Maybe Types.Channels)
apChannels = Lens.field @"channels"
{-# DEPRECATED apChannels "Use generic-lens or generic-optics with 'channels' instead." #-}

-- | The audio codec for the output file. Valid values include @aac@ , @flac@ , @mp2@ , @mp3@ , @pcm@ , and @vorbis@ .
--
-- /Note:/ Consider using 'codec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apCodec :: Lens.Lens' AudioParameters (Core.Maybe Types.Codec)
apCodec = Lens.field @"codec"
{-# DEPRECATED apCodec "Use generic-lens or generic-optics with 'codec' instead." #-}

-- | If you specified @AAC@ for @Audio:Codec@ , this is the @AAC@ compression profile to use. Valid values include:
--
-- @auto@ , @AAC-LC@ , @HE-AAC@ , @HE-AACv2@
-- If you specify @auto@ , Elastic Transcoder chooses a profile based on the bit rate of the output file.
--
-- /Note:/ Consider using 'codecOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apCodecOptions :: Lens.Lens' AudioParameters (Core.Maybe Types.AudioCodecOptions)
apCodecOptions = Lens.field @"codecOptions"
{-# DEPRECATED apCodecOptions "Use generic-lens or generic-optics with 'codecOptions' instead." #-}

-- | The sample rate of the audio stream in the output file, in Hertz. Valid values include:
--
-- @auto@ , @22050@ , @32000@ , @44100@ , @48000@ , @96000@
-- If you specify @auto@ , Elastic Transcoder automatically detects the sample rate.
--
-- /Note:/ Consider using 'sampleRate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apSampleRate :: Lens.Lens' AudioParameters (Core.Maybe Types.SampleRate)
apSampleRate = Lens.field @"sampleRate"
{-# DEPRECATED apSampleRate "Use generic-lens or generic-optics with 'sampleRate' instead." #-}

instance Core.FromJSON AudioParameters where
  toJSON AudioParameters {..} =
    Core.object
      ( Core.catMaybes
          [ ("AudioPackingMode" Core..=) Core.<$> audioPackingMode,
            ("BitRate" Core..=) Core.<$> bitRate,
            ("Channels" Core..=) Core.<$> channels,
            ("Codec" Core..=) Core.<$> codec,
            ("CodecOptions" Core..=) Core.<$> codecOptions,
            ("SampleRate" Core..=) Core.<$> sampleRate
          ]
      )

instance Core.FromJSON AudioParameters where
  parseJSON =
    Core.withObject "AudioParameters" Core.$
      \x ->
        AudioParameters'
          Core.<$> (x Core..:? "AudioPackingMode")
          Core.<*> (x Core..:? "BitRate")
          Core.<*> (x Core..:? "Channels")
          Core.<*> (x Core..:? "Codec")
          Core.<*> (x Core..:? "CodecOptions")
          Core.<*> (x Core..:? "SampleRate")
