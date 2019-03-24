{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticTranscoder.Types.Product where

import Network.AWS.ElasticTranscoder.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The file to be used as album art. There can be multiple artworks associated with an audio file, to a maximum of 20.
--
--
-- To remove artwork or leave the artwork empty, you can either set @Artwork@ to null, or set the @Merge Policy@ to "Replace" and use an empty @Artwork@ array.
--
-- To pass through existing artwork unchanged, set the @Merge Policy@ to "Prepend", "Append", or "Fallback", and use an empty @Artwork@ array.
--
--
-- /See:/ 'artwork' smart constructor.
data Artwork = Artwork'
  { _aSizingPolicy   :: !(Maybe Text)
  , _aAlbumArtFormat :: !(Maybe Text)
  , _aMaxHeight      :: !(Maybe Text)
  , _aInputKey       :: !(Maybe Text)
  , _aPaddingPolicy  :: !(Maybe Text)
  , _aEncryption     :: !(Maybe Encryption)
  , _aMaxWidth       :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Artwork' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aSizingPolicy' - Specify one of the following values to control scaling of the output album art:     * @Fit:@ Elastic Transcoder scales the output art so it matches the value that you specified in either @MaxWidth@ or @MaxHeight@ without exceeding the other value.     * @Fill:@ Elastic Transcoder scales the output art so it matches the value that you specified in either @MaxWidth@ or @MaxHeight@ and matches or exceeds the other value. Elastic Transcoder centers the output art and then crops it in the dimension (if any) that exceeds the maximum value.      * @Stretch:@ Elastic Transcoder stretches the output art to match the values that you specified for @MaxWidth@ and @MaxHeight@ . If the relative proportions of the input art and the output art are different, the output art will be distorted.     * @Keep:@ Elastic Transcoder does not scale the output art. If either dimension of the input art exceeds the values that you specified for @MaxWidth@ and @MaxHeight@ , Elastic Transcoder crops the output art.     * @ShrinkToFit:@ Elastic Transcoder scales the output art down so that its dimensions match the values that you specified for at least one of @MaxWidth@ and @MaxHeight@ without exceeding either value. If you specify this option, Elastic Transcoder does not scale the art up.     * @ShrinkToFill@ Elastic Transcoder scales the output art down so that its dimensions match the values that you specified for at least one of @MaxWidth@ and @MaxHeight@ without dropping below either value. If you specify this option, Elastic Transcoder does not scale the art up.
--
-- * 'aAlbumArtFormat' - The format of album art, if any. Valid formats are @.jpg@ and @.png@ .
--
-- * 'aMaxHeight' - The maximum height of the output album art in pixels. If you specify @auto@ , Elastic Transcoder uses 600 as the default value. If you specify a numeric value, enter an even integer between 32 and 3072, inclusive.
--
-- * 'aInputKey' - The name of the file to be used as album art. To determine which Amazon S3 bucket contains the specified file, Elastic Transcoder checks the pipeline specified by @PipelineId@ ; the @InputBucket@ object in that pipeline identifies the bucket. If the file name includes a prefix, for example, @cooking/pie.jpg@ , include the prefix in the key. If the file isn't in the specified bucket, Elastic Transcoder returns an error.
--
-- * 'aPaddingPolicy' - When you set @PaddingPolicy@ to @Pad@ , Elastic Transcoder may add white bars to the top and bottom and/or left and right sides of the output album art to make the total size of the output art match the values that you specified for @MaxWidth@ and @MaxHeight@ .
--
-- * 'aEncryption' - The encryption settings, if any, that you want Elastic Transcoder to apply to your artwork.
--
-- * 'aMaxWidth' - The maximum width of the output album art in pixels. If you specify @auto@ , Elastic Transcoder uses 600 as the default value. If you specify a numeric value, enter an even integer between 32 and 4096, inclusive.
artwork
    :: Artwork
artwork =
  Artwork'
    { _aSizingPolicy = Nothing
    , _aAlbumArtFormat = Nothing
    , _aMaxHeight = Nothing
    , _aInputKey = Nothing
    , _aPaddingPolicy = Nothing
    , _aEncryption = Nothing
    , _aMaxWidth = Nothing
    }


-- | Specify one of the following values to control scaling of the output album art:     * @Fit:@ Elastic Transcoder scales the output art so it matches the value that you specified in either @MaxWidth@ or @MaxHeight@ without exceeding the other value.     * @Fill:@ Elastic Transcoder scales the output art so it matches the value that you specified in either @MaxWidth@ or @MaxHeight@ and matches or exceeds the other value. Elastic Transcoder centers the output art and then crops it in the dimension (if any) that exceeds the maximum value.      * @Stretch:@ Elastic Transcoder stretches the output art to match the values that you specified for @MaxWidth@ and @MaxHeight@ . If the relative proportions of the input art and the output art are different, the output art will be distorted.     * @Keep:@ Elastic Transcoder does not scale the output art. If either dimension of the input art exceeds the values that you specified for @MaxWidth@ and @MaxHeight@ , Elastic Transcoder crops the output art.     * @ShrinkToFit:@ Elastic Transcoder scales the output art down so that its dimensions match the values that you specified for at least one of @MaxWidth@ and @MaxHeight@ without exceeding either value. If you specify this option, Elastic Transcoder does not scale the art up.     * @ShrinkToFill@ Elastic Transcoder scales the output art down so that its dimensions match the values that you specified for at least one of @MaxWidth@ and @MaxHeight@ without dropping below either value. If you specify this option, Elastic Transcoder does not scale the art up.
aSizingPolicy :: Lens' Artwork (Maybe Text)
aSizingPolicy = lens _aSizingPolicy (\ s a -> s{_aSizingPolicy = a})

-- | The format of album art, if any. Valid formats are @.jpg@ and @.png@ .
aAlbumArtFormat :: Lens' Artwork (Maybe Text)
aAlbumArtFormat = lens _aAlbumArtFormat (\ s a -> s{_aAlbumArtFormat = a})

-- | The maximum height of the output album art in pixels. If you specify @auto@ , Elastic Transcoder uses 600 as the default value. If you specify a numeric value, enter an even integer between 32 and 3072, inclusive.
aMaxHeight :: Lens' Artwork (Maybe Text)
aMaxHeight = lens _aMaxHeight (\ s a -> s{_aMaxHeight = a})

-- | The name of the file to be used as album art. To determine which Amazon S3 bucket contains the specified file, Elastic Transcoder checks the pipeline specified by @PipelineId@ ; the @InputBucket@ object in that pipeline identifies the bucket. If the file name includes a prefix, for example, @cooking/pie.jpg@ , include the prefix in the key. If the file isn't in the specified bucket, Elastic Transcoder returns an error.
aInputKey :: Lens' Artwork (Maybe Text)
aInputKey = lens _aInputKey (\ s a -> s{_aInputKey = a})

-- | When you set @PaddingPolicy@ to @Pad@ , Elastic Transcoder may add white bars to the top and bottom and/or left and right sides of the output album art to make the total size of the output art match the values that you specified for @MaxWidth@ and @MaxHeight@ .
aPaddingPolicy :: Lens' Artwork (Maybe Text)
aPaddingPolicy = lens _aPaddingPolicy (\ s a -> s{_aPaddingPolicy = a})

-- | The encryption settings, if any, that you want Elastic Transcoder to apply to your artwork.
aEncryption :: Lens' Artwork (Maybe Encryption)
aEncryption = lens _aEncryption (\ s a -> s{_aEncryption = a})

-- | The maximum width of the output album art in pixels. If you specify @auto@ , Elastic Transcoder uses 600 as the default value. If you specify a numeric value, enter an even integer between 32 and 4096, inclusive.
aMaxWidth :: Lens' Artwork (Maybe Text)
aMaxWidth = lens _aMaxWidth (\ s a -> s{_aMaxWidth = a})

instance FromJSON Artwork where
        parseJSON
          = withObject "Artwork"
              (\ x ->
                 Artwork' <$>
                   (x .:? "SizingPolicy") <*> (x .:? "AlbumArtFormat")
                     <*> (x .:? "MaxHeight")
                     <*> (x .:? "InputKey")
                     <*> (x .:? "PaddingPolicy")
                     <*> (x .:? "Encryption")
                     <*> (x .:? "MaxWidth"))

instance Hashable Artwork where

instance NFData Artwork where

instance ToJSON Artwork where
        toJSON Artwork'{..}
          = object
              (catMaybes
                 [("SizingPolicy" .=) <$> _aSizingPolicy,
                  ("AlbumArtFormat" .=) <$> _aAlbumArtFormat,
                  ("MaxHeight" .=) <$> _aMaxHeight,
                  ("InputKey" .=) <$> _aInputKey,
                  ("PaddingPolicy" .=) <$> _aPaddingPolicy,
                  ("Encryption" .=) <$> _aEncryption,
                  ("MaxWidth" .=) <$> _aMaxWidth])

-- | Options associated with your audio codec.
--
--
--
-- /See:/ 'audioCodecOptions' smart constructor.
data AudioCodecOptions = AudioCodecOptions'
  { _acoSigned   :: !(Maybe Text)
  , _acoBitDepth :: !(Maybe Text)
  , _acoProfile  :: !(Maybe Text)
  , _acoBitOrder :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AudioCodecOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acoSigned' - You can only choose whether an audio sample is signed when you specify @pcm@ for the value of Audio:Codec. Whether audio samples are represented with negative and positive numbers (signed) or only positive numbers (unsigned). The supported value is @Signed@ .
--
-- * 'acoBitDepth' - You can only choose an audio bit depth when you specify @flac@ or @pcm@ for the value of Audio:Codec. The bit depth of a sample is how many bits of information are included in the audio samples. The higher the bit depth, the better the audio, but the larger the file. Valid values are @16@ and @24@ . The most common bit depth is @24@ .
--
-- * 'acoProfile' - You can only choose an audio profile when you specify AAC for the value of Audio:Codec. Specify the AAC profile for the output file. Elastic Transcoder supports the following profiles:     * @auto@ : If you specify @auto@ , Elastic Transcoder selects the profile based on the bit rate selected for the output file.     * @AAC-LC@ : The most common AAC profile. Use for bit rates larger than 64 kbps.     * @HE-AAC@ : Not supported on some older players and devices. Use for bit rates between 40 and 80 kbps.     * @HE-AACv2@ : Not supported on some players and devices. Use for bit rates less than 48 kbps. All outputs in a @Smooth@ playlist must have the same value for @Profile@ .
--
-- * 'acoBitOrder' - You can only choose an audio bit order when you specify @pcm@ for the value of Audio:Codec. The order the bits of a PCM sample are stored in. The supported value is @LittleEndian@ .
audioCodecOptions
    :: AudioCodecOptions
audioCodecOptions =
  AudioCodecOptions'
    { _acoSigned = Nothing
    , _acoBitDepth = Nothing
    , _acoProfile = Nothing
    , _acoBitOrder = Nothing
    }


-- | You can only choose whether an audio sample is signed when you specify @pcm@ for the value of Audio:Codec. Whether audio samples are represented with negative and positive numbers (signed) or only positive numbers (unsigned). The supported value is @Signed@ .
acoSigned :: Lens' AudioCodecOptions (Maybe Text)
acoSigned = lens _acoSigned (\ s a -> s{_acoSigned = a})

-- | You can only choose an audio bit depth when you specify @flac@ or @pcm@ for the value of Audio:Codec. The bit depth of a sample is how many bits of information are included in the audio samples. The higher the bit depth, the better the audio, but the larger the file. Valid values are @16@ and @24@ . The most common bit depth is @24@ .
acoBitDepth :: Lens' AudioCodecOptions (Maybe Text)
acoBitDepth = lens _acoBitDepth (\ s a -> s{_acoBitDepth = a})

-- | You can only choose an audio profile when you specify AAC for the value of Audio:Codec. Specify the AAC profile for the output file. Elastic Transcoder supports the following profiles:     * @auto@ : If you specify @auto@ , Elastic Transcoder selects the profile based on the bit rate selected for the output file.     * @AAC-LC@ : The most common AAC profile. Use for bit rates larger than 64 kbps.     * @HE-AAC@ : Not supported on some older players and devices. Use for bit rates between 40 and 80 kbps.     * @HE-AACv2@ : Not supported on some players and devices. Use for bit rates less than 48 kbps. All outputs in a @Smooth@ playlist must have the same value for @Profile@ .
acoProfile :: Lens' AudioCodecOptions (Maybe Text)
acoProfile = lens _acoProfile (\ s a -> s{_acoProfile = a})

-- | You can only choose an audio bit order when you specify @pcm@ for the value of Audio:Codec. The order the bits of a PCM sample are stored in. The supported value is @LittleEndian@ .
acoBitOrder :: Lens' AudioCodecOptions (Maybe Text)
acoBitOrder = lens _acoBitOrder (\ s a -> s{_acoBitOrder = a})

instance FromJSON AudioCodecOptions where
        parseJSON
          = withObject "AudioCodecOptions"
              (\ x ->
                 AudioCodecOptions' <$>
                   (x .:? "Signed") <*> (x .:? "BitDepth") <*>
                     (x .:? "Profile")
                     <*> (x .:? "BitOrder"))

instance Hashable AudioCodecOptions where

instance NFData AudioCodecOptions where

instance ToJSON AudioCodecOptions where
        toJSON AudioCodecOptions'{..}
          = object
              (catMaybes
                 [("Signed" .=) <$> _acoSigned,
                  ("BitDepth" .=) <$> _acoBitDepth,
                  ("Profile" .=) <$> _acoProfile,
                  ("BitOrder" .=) <$> _acoBitOrder])

-- | Parameters required for transcoding audio.
--
--
--
-- /See:/ 'audioParameters' smart constructor.
data AudioParameters = AudioParameters'
  { _apChannels         :: !(Maybe Text)
  , _apCodec            :: !(Maybe Text)
  , _apAudioPackingMode :: !(Maybe Text)
  , _apSampleRate       :: !(Maybe Text)
  , _apBitRate          :: !(Maybe Text)
  , _apCodecOptions     :: !(Maybe AudioCodecOptions)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


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
audioParameters
    :: AudioParameters
audioParameters =
  AudioParameters'
    { _apChannels = Nothing
    , _apCodec = Nothing
    , _apAudioPackingMode = Nothing
    , _apSampleRate = Nothing
    , _apBitRate = Nothing
    , _apCodecOptions = Nothing
    }


-- | The number of audio channels in the output file. The following values are valid: @auto@ , @0@ , @1@ , @2@  One channel carries the information played by a single speaker. For example, a stereo track with two channels sends one channel to the left speaker, and the other channel to the right speaker. The output channels are organized into tracks. If you want Elastic Transcoder to automatically detect the number of audio channels in the input file and use that value for the output file, select @auto@ . The output of a specific channel value and inputs are as follows:     * @auto@ __channel specified, with any input:__ Pass through up to eight input channels.     * @0@ __channels specified, with any input:__ Audio omitted from the output.     * @1@ __channel specified, with at least one input channel:__ Mono sound.     * @2@ __channels specified, with any input:__ Two identical mono channels or stereo. For more information about tracks, see @Audio:AudioPackingMode.@  For more information about how Elastic Transcoder organizes channels and tracks, see @Audio:AudioPackingMode@ .
apChannels :: Lens' AudioParameters (Maybe Text)
apChannels = lens _apChannels (\ s a -> s{_apChannels = a})

-- | The audio codec for the output file. Valid values include @aac@ , @flac@ , @mp2@ , @mp3@ , @pcm@ , and @vorbis@ .
apCodec :: Lens' AudioParameters (Maybe Text)
apCodec = lens _apCodec (\ s a -> s{_apCodec = a})

-- | The method of organizing audio channels and tracks. Use @Audio:Channels@ to specify the number of channels in your output, and @Audio:AudioPackingMode@ to specify the number of tracks and their relation to the channels. If you do not specify an @Audio:AudioPackingMode@ , Elastic Transcoder uses @SingleTrack@ . The following values are valid: @SingleTrack@ , @OneChannelPerTrack@ , and @OneChannelPerTrackWithMosTo8Tracks@  When you specify @SingleTrack@ , Elastic Transcoder creates a single track for your output. The track can have up to eight channels. Use @SingleTrack@ for all non-@mxf@ containers. The outputs of @SingleTrack@ for a specific channel value and inputs are as follows:     * @0@ __channels with any input:__ Audio omitted from the output     * @1, 2, or auto @ __channels with no audio input:__ Audio omitted from the output     * @1 @ __channel with any input with audio:__ One track with one channel, downmixed if necessary     * @2 @ __channels with one track with one channel:__ One track with two identical channels     * @2 or auto @ __channels with two tracks with one channel each:__ One track with two channels     * @2 or auto @ __channels with one track with two channels:__ One track with two channels     * @2 @ __channels with one track with multiple channels:__ One track with two channels     * @auto @ __channels with one track with one channel:__ One track with one channel     * @auto @ __channels with one track with multiple channels:__ One track with multiple channels When you specify @OneChannelPerTrack@ , Elastic Transcoder creates a new track for every channel in your output. Your output can have up to eight single-channel tracks. The outputs of @OneChannelPerTrack@ for a specific channel value and inputs are as follows:     * @0 @ __channels with any input:__ Audio omitted from the output     * @1, 2, or auto @ __channels with no audio input:__ Audio omitted from the output     * @1 @ __channel with any input with audio:__ One track with one channel, downmixed if necessary     * @2 @ __channels with one track with one channel:__ Two tracks with one identical channel each     * @2 or auto @ __channels with two tracks with one channel each:__ Two tracks with one channel each     * @2 or auto @ __channels with one track with two channels:__ Two tracks with one channel each     * @2 @ __channels with one track with multiple channels:__ Two tracks with one channel each     * @auto @ __channels with one track with one channel:__ One track with one channel     * @auto @ __channels with one track with multiple channels:__ Up to eight tracks with one channel each When you specify @OneChannelPerTrackWithMosTo8Tracks@ , Elastic Transcoder creates eight single-channel tracks for your output. All tracks that do not contain audio data from an input channel are MOS, or Mit Out Sound, tracks. The outputs of @OneChannelPerTrackWithMosTo8Tracks@ for a specific channel value and inputs are as follows:     * @0 @ __channels with any input:__ Audio omitted from the output     * @1, 2, or auto @ __channels with no audio input:__ Audio omitted from the output     * @1 @ __channel with any input with audio:__ One track with one channel, downmixed if necessary, plus six MOS tracks     * @2 @ __channels with one track with one channel:__ Two tracks with one identical channel each, plus six MOS tracks     * @2 or auto @ __channels with two tracks with one channel each:__ Two tracks with one channel each, plus six MOS tracks     * @2 or auto @ __channels with one track with two channels:__ Two tracks with one channel each, plus six MOS tracks     * @2 @ __channels with one track with multiple channels:__ Two tracks with one channel each, plus six MOS tracks     * @auto @ __channels with one track with one channel:__ One track with one channel, plus seven MOS tracks     * @auto @ __channels with one track with multiple channels:__ Up to eight tracks with one channel each, plus MOS tracks until there are eight tracks in all
apAudioPackingMode :: Lens' AudioParameters (Maybe Text)
apAudioPackingMode = lens _apAudioPackingMode (\ s a -> s{_apAudioPackingMode = a})

-- | The sample rate of the audio stream in the output file, in Hertz. Valid values include: @auto@ , @22050@ , @32000@ , @44100@ , @48000@ , @96000@  If you specify @auto@ , Elastic Transcoder automatically detects the sample rate.
apSampleRate :: Lens' AudioParameters (Maybe Text)
apSampleRate = lens _apSampleRate (\ s a -> s{_apSampleRate = a})

-- | The bit rate of the audio stream in the output file, in kilobits/second. Enter an integer between 64 and 320, inclusive.
apBitRate :: Lens' AudioParameters (Maybe Text)
apBitRate = lens _apBitRate (\ s a -> s{_apBitRate = a})

-- | If you specified @AAC@ for @Audio:Codec@ , this is the @AAC@ compression profile to use. Valid values include: @auto@ , @AAC-LC@ , @HE-AAC@ , @HE-AACv2@  If you specify @auto@ , Elastic Transcoder chooses a profile based on the bit rate of the output file.
apCodecOptions :: Lens' AudioParameters (Maybe AudioCodecOptions)
apCodecOptions = lens _apCodecOptions (\ s a -> s{_apCodecOptions = a})

instance FromJSON AudioParameters where
        parseJSON
          = withObject "AudioParameters"
              (\ x ->
                 AudioParameters' <$>
                   (x .:? "Channels") <*> (x .:? "Codec") <*>
                     (x .:? "AudioPackingMode")
                     <*> (x .:? "SampleRate")
                     <*> (x .:? "BitRate")
                     <*> (x .:? "CodecOptions"))

instance Hashable AudioParameters where

instance NFData AudioParameters where

instance ToJSON AudioParameters where
        toJSON AudioParameters'{..}
          = object
              (catMaybes
                 [("Channels" .=) <$> _apChannels,
                  ("Codec" .=) <$> _apCodec,
                  ("AudioPackingMode" .=) <$> _apAudioPackingMode,
                  ("SampleRate" .=) <$> _apSampleRate,
                  ("BitRate" .=) <$> _apBitRate,
                  ("CodecOptions" .=) <$> _apCodecOptions])

-- | The file format of the output captions. If you leave this value blank, Elastic Transcoder returns an error.
--
--
--
-- /See:/ 'captionFormat' smart constructor.
data CaptionFormat = CaptionFormat'
  { _cfPattern    :: !(Maybe Text)
  , _cfFormat     :: !(Maybe Text)
  , _cfEncryption :: !(Maybe Encryption)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CaptionFormat' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfPattern' - The prefix for caption filenames, in the form /description/ -@{language}@ , where:     * /description/ is a description of the video.     * @{language}@ is a literal value that Elastic Transcoder replaces with the two- or three-letter code for the language of the caption in the output file names. If you don't include @{language}@ in the file name pattern, Elastic Transcoder automatically appends "@{language}@ " to the value that you specify for the description. In addition, Elastic Transcoder automatically appends the count to the end of the segment files. For example, suppose you're transcoding into srt format. When you enter "Sydney-{language}-sunrise", and the language of the captions is English (en), the name of the first caption file is be Sydney-en-sunrise00000.srt.
--
-- * 'cfFormat' - The format you specify determines whether Elastic Transcoder generates an embedded or sidecar caption for this output.     * __Valid Embedded Caption Formats:__      * __for FLAC__ : None     * __For MP3__ : None     * __For MP4__ : mov-text     * __For MPEG-TS__ : None     * __For ogg__ : None     * __For webm__ : None     * __Valid Sidecar Caption Formats:__ Elastic Transcoder supports dfxp (first div element only), scc, srt, and webvtt. If you want ttml or smpte-tt compatible captions, specify dfxp as your output format.     * __For FMP4__ : dfxp     * __Non-FMP4 outputs__ : All sidecar types @fmp4@ captions have an extension of @.ismt@
--
-- * 'cfEncryption' - The encryption settings, if any, that you want Elastic Transcoder to apply to your caption formats.
captionFormat
    :: CaptionFormat
captionFormat =
  CaptionFormat'
    {_cfPattern = Nothing, _cfFormat = Nothing, _cfEncryption = Nothing}


-- | The prefix for caption filenames, in the form /description/ -@{language}@ , where:     * /description/ is a description of the video.     * @{language}@ is a literal value that Elastic Transcoder replaces with the two- or three-letter code for the language of the caption in the output file names. If you don't include @{language}@ in the file name pattern, Elastic Transcoder automatically appends "@{language}@ " to the value that you specify for the description. In addition, Elastic Transcoder automatically appends the count to the end of the segment files. For example, suppose you're transcoding into srt format. When you enter "Sydney-{language}-sunrise", and the language of the captions is English (en), the name of the first caption file is be Sydney-en-sunrise00000.srt.
cfPattern :: Lens' CaptionFormat (Maybe Text)
cfPattern = lens _cfPattern (\ s a -> s{_cfPattern = a})

-- | The format you specify determines whether Elastic Transcoder generates an embedded or sidecar caption for this output.     * __Valid Embedded Caption Formats:__      * __for FLAC__ : None     * __For MP3__ : None     * __For MP4__ : mov-text     * __For MPEG-TS__ : None     * __For ogg__ : None     * __For webm__ : None     * __Valid Sidecar Caption Formats:__ Elastic Transcoder supports dfxp (first div element only), scc, srt, and webvtt. If you want ttml or smpte-tt compatible captions, specify dfxp as your output format.     * __For FMP4__ : dfxp     * __Non-FMP4 outputs__ : All sidecar types @fmp4@ captions have an extension of @.ismt@
cfFormat :: Lens' CaptionFormat (Maybe Text)
cfFormat = lens _cfFormat (\ s a -> s{_cfFormat = a})

-- | The encryption settings, if any, that you want Elastic Transcoder to apply to your caption formats.
cfEncryption :: Lens' CaptionFormat (Maybe Encryption)
cfEncryption = lens _cfEncryption (\ s a -> s{_cfEncryption = a})

instance FromJSON CaptionFormat where
        parseJSON
          = withObject "CaptionFormat"
              (\ x ->
                 CaptionFormat' <$>
                   (x .:? "Pattern") <*> (x .:? "Format") <*>
                     (x .:? "Encryption"))

instance Hashable CaptionFormat where

instance NFData CaptionFormat where

instance ToJSON CaptionFormat where
        toJSON CaptionFormat'{..}
          = object
              (catMaybes
                 [("Pattern" .=) <$> _cfPattern,
                  ("Format" .=) <$> _cfFormat,
                  ("Encryption" .=) <$> _cfEncryption])

-- | A source file for the input sidecar captions used during the transcoding process.
--
--
--
-- /See:/ 'captionSource' smart constructor.
data CaptionSource = CaptionSource'
  { _csTimeOffset :: !(Maybe Text)
  , _csEncryption :: !(Maybe Encryption)
  , _csKey        :: !(Maybe Text)
  , _csLanguage   :: !(Maybe Text)
  , _csLabel      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CaptionSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csTimeOffset' - For clip generation or captions that do not start at the same time as the associated video file, the @TimeOffset@ tells Elastic Transcoder how much of the video to encode before including captions. Specify the TimeOffset in the form [+-]SS.sss or [+-]HH:mm:SS.ss.
--
-- * 'csEncryption' - The encryption settings, if any, that Elastic Transcoder needs to decyrpt your caption sources, or that you want Elastic Transcoder to apply to your caption sources.
--
-- * 'csKey' - The name of the sidecar caption file that you want Elastic Transcoder to include in the output file.
--
-- * 'csLanguage' - A string that specifies the language of the caption. If you specified multiple inputs with captions, the caption language must match in order to be included in the output. Specify this as one of:     * 2-character ISO 639-1 code     * 3-character ISO 639-2 code For more information on ISO language codes and language names, see the List of ISO 639-1 codes.
--
-- * 'csLabel' - The label of the caption shown in the player when choosing a language. We recommend that you put the caption language name here, in the language of the captions.
captionSource
    :: CaptionSource
captionSource =
  CaptionSource'
    { _csTimeOffset = Nothing
    , _csEncryption = Nothing
    , _csKey = Nothing
    , _csLanguage = Nothing
    , _csLabel = Nothing
    }


-- | For clip generation or captions that do not start at the same time as the associated video file, the @TimeOffset@ tells Elastic Transcoder how much of the video to encode before including captions. Specify the TimeOffset in the form [+-]SS.sss or [+-]HH:mm:SS.ss.
csTimeOffset :: Lens' CaptionSource (Maybe Text)
csTimeOffset = lens _csTimeOffset (\ s a -> s{_csTimeOffset = a})

-- | The encryption settings, if any, that Elastic Transcoder needs to decyrpt your caption sources, or that you want Elastic Transcoder to apply to your caption sources.
csEncryption :: Lens' CaptionSource (Maybe Encryption)
csEncryption = lens _csEncryption (\ s a -> s{_csEncryption = a})

-- | The name of the sidecar caption file that you want Elastic Transcoder to include in the output file.
csKey :: Lens' CaptionSource (Maybe Text)
csKey = lens _csKey (\ s a -> s{_csKey = a})

-- | A string that specifies the language of the caption. If you specified multiple inputs with captions, the caption language must match in order to be included in the output. Specify this as one of:     * 2-character ISO 639-1 code     * 3-character ISO 639-2 code For more information on ISO language codes and language names, see the List of ISO 639-1 codes.
csLanguage :: Lens' CaptionSource (Maybe Text)
csLanguage = lens _csLanguage (\ s a -> s{_csLanguage = a})

-- | The label of the caption shown in the player when choosing a language. We recommend that you put the caption language name here, in the language of the captions.
csLabel :: Lens' CaptionSource (Maybe Text)
csLabel = lens _csLabel (\ s a -> s{_csLabel = a})

instance FromJSON CaptionSource where
        parseJSON
          = withObject "CaptionSource"
              (\ x ->
                 CaptionSource' <$>
                   (x .:? "TimeOffset") <*> (x .:? "Encryption") <*>
                     (x .:? "Key")
                     <*> (x .:? "Language")
                     <*> (x .:? "Label"))

instance Hashable CaptionSource where

instance NFData CaptionSource where

instance ToJSON CaptionSource where
        toJSON CaptionSource'{..}
          = object
              (catMaybes
                 [("TimeOffset" .=) <$> _csTimeOffset,
                  ("Encryption" .=) <$> _csEncryption,
                  ("Key" .=) <$> _csKey,
                  ("Language" .=) <$> _csLanguage,
                  ("Label" .=) <$> _csLabel])

-- | The captions to be created, if any.
--
--
--
-- /See:/ 'captions' smart constructor.
data Captions = Captions'
  { _cMergePolicy    :: !(Maybe Text)
  , _cCaptionSources :: !(Maybe [CaptionSource])
  , _cCaptionFormats :: !(Maybe [CaptionFormat])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Captions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cMergePolicy' - A policy that determines how Elastic Transcoder handles the existence of multiple captions.     * __MergeOverride:__ Elastic Transcoder transcodes both embedded and sidecar captions into outputs. If captions for a language are embedded in the input file and also appear in a sidecar file, Elastic Transcoder uses the sidecar captions and ignores the embedded captions for that language.     * __MergeRetain:__ Elastic Transcoder transcodes both embedded and sidecar captions into outputs. If captions for a language are embedded in the input file and also appear in a sidecar file, Elastic Transcoder uses the embedded captions and ignores the sidecar captions for that language. If @CaptionSources@ is empty, Elastic Transcoder omits all sidecar captions from the output files.     * __Override:__ Elastic Transcoder transcodes only the sidecar captions that you specify in @CaptionSources@ . @MergePolicy@ cannot be null.
--
-- * 'cCaptionSources' - Source files for the input sidecar captions used during the transcoding process. To omit all sidecar captions, leave @CaptionSources@ blank.
--
-- * 'cCaptionFormats' - The array of file formats for the output captions. If you leave this value blank, Elastic Transcoder returns an error.
captions
    :: Captions
captions =
  Captions'
    { _cMergePolicy = Nothing
    , _cCaptionSources = Nothing
    , _cCaptionFormats = Nothing
    }


-- | A policy that determines how Elastic Transcoder handles the existence of multiple captions.     * __MergeOverride:__ Elastic Transcoder transcodes both embedded and sidecar captions into outputs. If captions for a language are embedded in the input file and also appear in a sidecar file, Elastic Transcoder uses the sidecar captions and ignores the embedded captions for that language.     * __MergeRetain:__ Elastic Transcoder transcodes both embedded and sidecar captions into outputs. If captions for a language are embedded in the input file and also appear in a sidecar file, Elastic Transcoder uses the embedded captions and ignores the sidecar captions for that language. If @CaptionSources@ is empty, Elastic Transcoder omits all sidecar captions from the output files.     * __Override:__ Elastic Transcoder transcodes only the sidecar captions that you specify in @CaptionSources@ . @MergePolicy@ cannot be null.
cMergePolicy :: Lens' Captions (Maybe Text)
cMergePolicy = lens _cMergePolicy (\ s a -> s{_cMergePolicy = a})

-- | Source files for the input sidecar captions used during the transcoding process. To omit all sidecar captions, leave @CaptionSources@ blank.
cCaptionSources :: Lens' Captions [CaptionSource]
cCaptionSources = lens _cCaptionSources (\ s a -> s{_cCaptionSources = a}) . _Default . _Coerce

-- | The array of file formats for the output captions. If you leave this value blank, Elastic Transcoder returns an error.
cCaptionFormats :: Lens' Captions [CaptionFormat]
cCaptionFormats = lens _cCaptionFormats (\ s a -> s{_cCaptionFormats = a}) . _Default . _Coerce

instance FromJSON Captions where
        parseJSON
          = withObject "Captions"
              (\ x ->
                 Captions' <$>
                   (x .:? "MergePolicy") <*>
                     (x .:? "CaptionSources" .!= mempty)
                     <*> (x .:? "CaptionFormats" .!= mempty))

instance Hashable Captions where

instance NFData Captions where

instance ToJSON Captions where
        toJSON Captions'{..}
          = object
              (catMaybes
                 [("MergePolicy" .=) <$> _cMergePolicy,
                  ("CaptionSources" .=) <$> _cCaptionSources,
                  ("CaptionFormats" .=) <$> _cCaptionFormats])

-- | Settings for one clip in a composition. All jobs in a playlist must have the same clip settings.
--
--
--
-- /See:/ 'clip' smart constructor.
newtype Clip = Clip'
  { _cTimeSpan :: Maybe TimeSpan
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Clip' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cTimeSpan' - Settings that determine when a clip begins and how long it lasts.
clip
    :: Clip
clip = Clip' {_cTimeSpan = Nothing}


-- | Settings that determine when a clip begins and how long it lasts.
cTimeSpan :: Lens' Clip (Maybe TimeSpan)
cTimeSpan = lens _cTimeSpan (\ s a -> s{_cTimeSpan = a})

instance FromJSON Clip where
        parseJSON
          = withObject "Clip"
              (\ x -> Clip' <$> (x .:? "TimeSpan"))

instance Hashable Clip where

instance NFData Clip where

instance ToJSON Clip where
        toJSON Clip'{..}
          = object (catMaybes [("TimeSpan" .=) <$> _cTimeSpan])

-- | The @CreateJobOutput@ structure.
--
--
--
-- /See:/ 'createJobOutput' smart constructor.
data CreateJobOutput = CreateJobOutput'
  { _cjoThumbnailPattern    :: !(Maybe Text)
  , _cjoCaptions            :: !(Maybe Captions)
  , _cjoPresetId            :: !(Maybe Text)
  , _cjoComposition         :: !(Maybe [Clip])
  , _cjoAlbumArt            :: !(Maybe JobAlbumArt)
  , _cjoWatermarks          :: !(Maybe [JobWatermark])
  , _cjoEncryption          :: !(Maybe Encryption)
  , _cjoKey                 :: !(Maybe Text)
  , _cjoSegmentDuration     :: !(Maybe Text)
  , _cjoThumbnailEncryption :: !(Maybe Encryption)
  , _cjoRotate              :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateJobOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjoThumbnailPattern' - Whether you want Elastic Transcoder to create thumbnails for your videos and, if so, how you want Elastic Transcoder to name the files. If you don't want Elastic Transcoder to create thumbnails, specify "". If you do want Elastic Transcoder to create thumbnails, specify the information that you want to include in the file name for each thumbnail. You can specify the following values in any sequence:     * __@{count}@ (Required)__ : If you want to create thumbnails, you must include @{count}@ in the @ThumbnailPattern@ object. Wherever you specify @{count}@ , Elastic Transcoder adds a five-digit sequence number (beginning with __00001__ ) to thumbnail file names. The number indicates where a given thumbnail appears in the sequence of thumbnails for a transcoded file.  /Important:/ If you specify a literal value and/or @{resolution}@ but you omit @{count}@ , Elastic Transcoder returns a validation error and does not create the job.     * __Literal values (Optional)__ : You can specify literal values anywhere in the @ThumbnailPattern@ object. For example, you can include them as a file name prefix or as a delimiter between @{resolution}@ and @{count}@ .      * __@{resolution}@ (Optional)__ : If you want Elastic Transcoder to include the resolution in the file name, include @{resolution}@ in the @ThumbnailPattern@ object.  When creating thumbnails, Elastic Transcoder automatically saves the files in the format (.jpg or .png) that appears in the preset that you specified in the @PresetID@ value of @CreateJobOutput@ . Elastic Transcoder also appends the applicable file name extension.
--
-- * 'cjoCaptions' - You can configure Elastic Transcoder to transcode captions, or subtitles, from one format to another. All captions must be in UTF-8. Elastic Transcoder supports two types of captions:     * __Embedded:__ Embedded captions are included in the same file as the audio and video. Elastic Transcoder supports only one embedded caption per language, to a maximum of 300 embedded captions per file. Valid input values include: @CEA-608 (EIA-608@ , first non-empty channel only), @CEA-708 (EIA-708@ , first non-empty channel only), and @mov-text@  Valid outputs include: @mov-text@  Elastic Transcoder supports a maximum of one embedded format per output.     * __Sidecar:__ Sidecar captions are kept in a separate metadata file from the audio and video data. Sidecar captions require a player that is capable of understanding the relationship between the video file and the sidecar file. Elastic Transcoder supports only one sidecar caption per language, to a maximum of 20 sidecar captions per file. Valid input values include: @dfxp@ (first div element only), @ebu-tt@ , @scc@ , @smpt@ , @srt@ , @ttml@ (first div element only), and @webvtt@  Valid outputs include: @dfxp@ (first div element only), @scc@ , @srt@ , and @webvtt@ . If you want ttml or smpte-tt compatible captions, specify dfxp as your output format. Elastic Transcoder does not support OCR (Optical Character Recognition), does not accept pictures as a valid input for captions, and is not available for audio-only transcoding. Elastic Transcoder does not preserve text formatting (for example, italics) during the transcoding process. To remove captions or leave the captions empty, set @Captions@ to null. To pass through existing captions unchanged, set the @MergePolicy@ to @MergeRetain@ , and pass in a null @CaptionSources@ array. For more information on embedded files, see the Subtitles Wikipedia page. For more information on sidecar files, see the Extensible Metadata Platform and Sidecar file Wikipedia pages.
--
-- * 'cjoPresetId' - The @Id@ of the preset to use for this job. The preset determines the audio, video, and thumbnail settings that Elastic Transcoder uses for transcoding.
--
-- * 'cjoComposition' - You can create an output file that contains an excerpt from the input file. This excerpt, called a clip, can come from the beginning, middle, or end of the file. The Composition object contains settings for the clips that make up an output file. For the current release, you can only specify settings for a single clip per output file. The Composition object cannot be null.
--
-- * 'cjoAlbumArt' - Information about the album art that you want Elastic Transcoder to add to the file during transcoding. You can specify up to twenty album artworks for each output. Settings for each artwork must be defined in the job for the current output.
--
-- * 'cjoWatermarks' - Information about the watermarks that you want Elastic Transcoder to add to the video during transcoding. You can specify up to four watermarks for each output. Settings for each watermark must be defined in the preset for the current output.
--
-- * 'cjoEncryption' - You can specify encryption settings for any output files that you want to use for a transcoding job. This includes the output file and any watermarks, thumbnails, album art, or captions that you want to use. You must specify encryption settings for each file individually.
--
-- * 'cjoKey' - The name to assign to the transcoded file. Elastic Transcoder saves the file in the Amazon S3 bucket specified by the @OutputBucket@ object in the pipeline that is specified by the pipeline ID. If a file with the specified name already exists in the output bucket, the job fails.
--
-- * 'cjoSegmentDuration' - /Important:/ (Outputs in Fragmented MP4 or MPEG-TS format only. If you specify a preset in @PresetId@ for which the value of @Container@ is @fmp4@ (Fragmented MP4) or @ts@ (MPEG-TS), @SegmentDuration@ is the target maximum duration of each segment in seconds. For @HLSv3@ format playlists, each media segment is stored in a separate @.ts@ file. For @HLSv4@ and @Smooth@ playlists, all media segments for an output are stored in a single file. Each segment is approximately the length of the @SegmentDuration@ , though individual segments might be shorter or longer. The range of valid values is 1 to 60 seconds. If the duration of the video is not evenly divisible by @SegmentDuration@ , the duration of the last segment is the remainder of total length/SegmentDuration. Elastic Transcoder creates an output-specific playlist for each output @HLS@ output that you specify in OutputKeys. To add an output to the master playlist for this job, include it in the @OutputKeys@ of the associated playlist.
--
-- * 'cjoThumbnailEncryption' - The encryption settings, if any, that you want Elastic Transcoder to apply to your thumbnail.
--
-- * 'cjoRotate' - The number of degrees clockwise by which you want Elastic Transcoder to rotate the output relative to the input. Enter one of the following values: @auto@ , @0@ , @90@ , @180@ , @270@ . The value @auto@ generally works only if the file that you're transcoding contains rotation metadata.
createJobOutput
    :: CreateJobOutput
createJobOutput =
  CreateJobOutput'
    { _cjoThumbnailPattern = Nothing
    , _cjoCaptions = Nothing
    , _cjoPresetId = Nothing
    , _cjoComposition = Nothing
    , _cjoAlbumArt = Nothing
    , _cjoWatermarks = Nothing
    , _cjoEncryption = Nothing
    , _cjoKey = Nothing
    , _cjoSegmentDuration = Nothing
    , _cjoThumbnailEncryption = Nothing
    , _cjoRotate = Nothing
    }


-- | Whether you want Elastic Transcoder to create thumbnails for your videos and, if so, how you want Elastic Transcoder to name the files. If you don't want Elastic Transcoder to create thumbnails, specify "". If you do want Elastic Transcoder to create thumbnails, specify the information that you want to include in the file name for each thumbnail. You can specify the following values in any sequence:     * __@{count}@ (Required)__ : If you want to create thumbnails, you must include @{count}@ in the @ThumbnailPattern@ object. Wherever you specify @{count}@ , Elastic Transcoder adds a five-digit sequence number (beginning with __00001__ ) to thumbnail file names. The number indicates where a given thumbnail appears in the sequence of thumbnails for a transcoded file.  /Important:/ If you specify a literal value and/or @{resolution}@ but you omit @{count}@ , Elastic Transcoder returns a validation error and does not create the job.     * __Literal values (Optional)__ : You can specify literal values anywhere in the @ThumbnailPattern@ object. For example, you can include them as a file name prefix or as a delimiter between @{resolution}@ and @{count}@ .      * __@{resolution}@ (Optional)__ : If you want Elastic Transcoder to include the resolution in the file name, include @{resolution}@ in the @ThumbnailPattern@ object.  When creating thumbnails, Elastic Transcoder automatically saves the files in the format (.jpg or .png) that appears in the preset that you specified in the @PresetID@ value of @CreateJobOutput@ . Elastic Transcoder also appends the applicable file name extension.
cjoThumbnailPattern :: Lens' CreateJobOutput (Maybe Text)
cjoThumbnailPattern = lens _cjoThumbnailPattern (\ s a -> s{_cjoThumbnailPattern = a})

-- | You can configure Elastic Transcoder to transcode captions, or subtitles, from one format to another. All captions must be in UTF-8. Elastic Transcoder supports two types of captions:     * __Embedded:__ Embedded captions are included in the same file as the audio and video. Elastic Transcoder supports only one embedded caption per language, to a maximum of 300 embedded captions per file. Valid input values include: @CEA-608 (EIA-608@ , first non-empty channel only), @CEA-708 (EIA-708@ , first non-empty channel only), and @mov-text@  Valid outputs include: @mov-text@  Elastic Transcoder supports a maximum of one embedded format per output.     * __Sidecar:__ Sidecar captions are kept in a separate metadata file from the audio and video data. Sidecar captions require a player that is capable of understanding the relationship between the video file and the sidecar file. Elastic Transcoder supports only one sidecar caption per language, to a maximum of 20 sidecar captions per file. Valid input values include: @dfxp@ (first div element only), @ebu-tt@ , @scc@ , @smpt@ , @srt@ , @ttml@ (first div element only), and @webvtt@  Valid outputs include: @dfxp@ (first div element only), @scc@ , @srt@ , and @webvtt@ . If you want ttml or smpte-tt compatible captions, specify dfxp as your output format. Elastic Transcoder does not support OCR (Optical Character Recognition), does not accept pictures as a valid input for captions, and is not available for audio-only transcoding. Elastic Transcoder does not preserve text formatting (for example, italics) during the transcoding process. To remove captions or leave the captions empty, set @Captions@ to null. To pass through existing captions unchanged, set the @MergePolicy@ to @MergeRetain@ , and pass in a null @CaptionSources@ array. For more information on embedded files, see the Subtitles Wikipedia page. For more information on sidecar files, see the Extensible Metadata Platform and Sidecar file Wikipedia pages.
cjoCaptions :: Lens' CreateJobOutput (Maybe Captions)
cjoCaptions = lens _cjoCaptions (\ s a -> s{_cjoCaptions = a})

-- | The @Id@ of the preset to use for this job. The preset determines the audio, video, and thumbnail settings that Elastic Transcoder uses for transcoding.
cjoPresetId :: Lens' CreateJobOutput (Maybe Text)
cjoPresetId = lens _cjoPresetId (\ s a -> s{_cjoPresetId = a})

-- | You can create an output file that contains an excerpt from the input file. This excerpt, called a clip, can come from the beginning, middle, or end of the file. The Composition object contains settings for the clips that make up an output file. For the current release, you can only specify settings for a single clip per output file. The Composition object cannot be null.
cjoComposition :: Lens' CreateJobOutput [Clip]
cjoComposition = lens _cjoComposition (\ s a -> s{_cjoComposition = a}) . _Default . _Coerce

-- | Information about the album art that you want Elastic Transcoder to add to the file during transcoding. You can specify up to twenty album artworks for each output. Settings for each artwork must be defined in the job for the current output.
cjoAlbumArt :: Lens' CreateJobOutput (Maybe JobAlbumArt)
cjoAlbumArt = lens _cjoAlbumArt (\ s a -> s{_cjoAlbumArt = a})

-- | Information about the watermarks that you want Elastic Transcoder to add to the video during transcoding. You can specify up to four watermarks for each output. Settings for each watermark must be defined in the preset for the current output.
cjoWatermarks :: Lens' CreateJobOutput [JobWatermark]
cjoWatermarks = lens _cjoWatermarks (\ s a -> s{_cjoWatermarks = a}) . _Default . _Coerce

-- | You can specify encryption settings for any output files that you want to use for a transcoding job. This includes the output file and any watermarks, thumbnails, album art, or captions that you want to use. You must specify encryption settings for each file individually.
cjoEncryption :: Lens' CreateJobOutput (Maybe Encryption)
cjoEncryption = lens _cjoEncryption (\ s a -> s{_cjoEncryption = a})

-- | The name to assign to the transcoded file. Elastic Transcoder saves the file in the Amazon S3 bucket specified by the @OutputBucket@ object in the pipeline that is specified by the pipeline ID. If a file with the specified name already exists in the output bucket, the job fails.
cjoKey :: Lens' CreateJobOutput (Maybe Text)
cjoKey = lens _cjoKey (\ s a -> s{_cjoKey = a})

-- | /Important:/ (Outputs in Fragmented MP4 or MPEG-TS format only. If you specify a preset in @PresetId@ for which the value of @Container@ is @fmp4@ (Fragmented MP4) or @ts@ (MPEG-TS), @SegmentDuration@ is the target maximum duration of each segment in seconds. For @HLSv3@ format playlists, each media segment is stored in a separate @.ts@ file. For @HLSv4@ and @Smooth@ playlists, all media segments for an output are stored in a single file. Each segment is approximately the length of the @SegmentDuration@ , though individual segments might be shorter or longer. The range of valid values is 1 to 60 seconds. If the duration of the video is not evenly divisible by @SegmentDuration@ , the duration of the last segment is the remainder of total length/SegmentDuration. Elastic Transcoder creates an output-specific playlist for each output @HLS@ output that you specify in OutputKeys. To add an output to the master playlist for this job, include it in the @OutputKeys@ of the associated playlist.
cjoSegmentDuration :: Lens' CreateJobOutput (Maybe Text)
cjoSegmentDuration = lens _cjoSegmentDuration (\ s a -> s{_cjoSegmentDuration = a})

-- | The encryption settings, if any, that you want Elastic Transcoder to apply to your thumbnail.
cjoThumbnailEncryption :: Lens' CreateJobOutput (Maybe Encryption)
cjoThumbnailEncryption = lens _cjoThumbnailEncryption (\ s a -> s{_cjoThumbnailEncryption = a})

-- | The number of degrees clockwise by which you want Elastic Transcoder to rotate the output relative to the input. Enter one of the following values: @auto@ , @0@ , @90@ , @180@ , @270@ . The value @auto@ generally works only if the file that you're transcoding contains rotation metadata.
cjoRotate :: Lens' CreateJobOutput (Maybe Text)
cjoRotate = lens _cjoRotate (\ s a -> s{_cjoRotate = a})

instance Hashable CreateJobOutput where

instance NFData CreateJobOutput where

instance ToJSON CreateJobOutput where
        toJSON CreateJobOutput'{..}
          = object
              (catMaybes
                 [("ThumbnailPattern" .=) <$> _cjoThumbnailPattern,
                  ("Captions" .=) <$> _cjoCaptions,
                  ("PresetId" .=) <$> _cjoPresetId,
                  ("Composition" .=) <$> _cjoComposition,
                  ("AlbumArt" .=) <$> _cjoAlbumArt,
                  ("Watermarks" .=) <$> _cjoWatermarks,
                  ("Encryption" .=) <$> _cjoEncryption,
                  ("Key" .=) <$> _cjoKey,
                  ("SegmentDuration" .=) <$> _cjoSegmentDuration,
                  ("ThumbnailEncryption" .=) <$>
                    _cjoThumbnailEncryption,
                  ("Rotate" .=) <$> _cjoRotate])

-- | Information about the master playlist.
--
--
--
-- /See:/ 'createJobPlaylist' smart constructor.
data CreateJobPlaylist = CreateJobPlaylist'
  { _cjpPlayReadyDrm         :: !(Maybe PlayReadyDrm)
  , _cjpFormat               :: !(Maybe Text)
  , _cjpOutputKeys           :: !(Maybe [Text])
  , _cjpName                 :: !(Maybe Text)
  , _cjpHlsContentProtection :: !(Maybe HlsContentProtection)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateJobPlaylist' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cjpPlayReadyDrm' - The DRM settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
--
-- * 'cjpFormat' - The format of the output playlist. Valid formats include @HLSv3@ , @HLSv4@ , and @Smooth@ .
--
-- * 'cjpOutputKeys' - For each output in this job that you want to include in a master playlist, the value of the @Outputs:Key@ object.      * If your output is not @HLS@ or does not have a segment duration set, the name of the output file is a concatenation of @OutputKeyPrefix@ and @Outputs:Key@ : OutputKeyPrefix@Outputs:Key@      * If your output is @HLSv3@ and has a segment duration set, or is not included in a playlist, Elastic Transcoder creates an output playlist file with a file extension of @.m3u8@ , and a series of @.ts@ files that include a five-digit sequential counter beginning with 00000: OutputKeyPrefix@Outputs:Key@ .m3u8 OutputKeyPrefix@Outputs:Key@ 00000.ts     * If your output is @HLSv4@ , has a segment duration set, and is included in an @HLSv4@ playlist, Elastic Transcoder creates an output playlist file with a file extension of @_v4.m3u8@ . If the output is video, Elastic Transcoder also creates an output file with an extension of @_iframe.m3u8@ : OutputKeyPrefix@Outputs:Key@ _v4.m3u8 OutputKeyPrefix@Outputs:Key@ _iframe.m3u8 OutputKeyPrefix@Outputs:Key@ .ts Elastic Transcoder automatically appends the relevant file extension to the file name. If you include a file extension in Output Key, the file name will have two extensions. If you include more than one output in a playlist, any segment duration settings, clip settings, or caption settings must be the same for all outputs in the playlist. For @Smooth@ playlists, the @Audio:Profile@ , @Video:Profile@ , and @Video:FrameRate@ to @Video:KeyframesMaxDist@ ratio must be the same for all outputs.
--
-- * 'cjpName' - The name that you want Elastic Transcoder to assign to the master playlist, for example, nyc-vacation.m3u8. If the name includes a @/@ character, the section of the name before the last @/@ must be identical for all @Name@ objects. If you create more than one master playlist, the values of all @Name@ objects must be unique.
--
-- * 'cjpHlsContentProtection' - The HLS content protection settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
createJobPlaylist
    :: CreateJobPlaylist
createJobPlaylist =
  CreateJobPlaylist'
    { _cjpPlayReadyDrm = Nothing
    , _cjpFormat = Nothing
    , _cjpOutputKeys = Nothing
    , _cjpName = Nothing
    , _cjpHlsContentProtection = Nothing
    }


-- | The DRM settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
cjpPlayReadyDrm :: Lens' CreateJobPlaylist (Maybe PlayReadyDrm)
cjpPlayReadyDrm = lens _cjpPlayReadyDrm (\ s a -> s{_cjpPlayReadyDrm = a})

-- | The format of the output playlist. Valid formats include @HLSv3@ , @HLSv4@ , and @Smooth@ .
cjpFormat :: Lens' CreateJobPlaylist (Maybe Text)
cjpFormat = lens _cjpFormat (\ s a -> s{_cjpFormat = a})

-- | For each output in this job that you want to include in a master playlist, the value of the @Outputs:Key@ object.      * If your output is not @HLS@ or does not have a segment duration set, the name of the output file is a concatenation of @OutputKeyPrefix@ and @Outputs:Key@ : OutputKeyPrefix@Outputs:Key@      * If your output is @HLSv3@ and has a segment duration set, or is not included in a playlist, Elastic Transcoder creates an output playlist file with a file extension of @.m3u8@ , and a series of @.ts@ files that include a five-digit sequential counter beginning with 00000: OutputKeyPrefix@Outputs:Key@ .m3u8 OutputKeyPrefix@Outputs:Key@ 00000.ts     * If your output is @HLSv4@ , has a segment duration set, and is included in an @HLSv4@ playlist, Elastic Transcoder creates an output playlist file with a file extension of @_v4.m3u8@ . If the output is video, Elastic Transcoder also creates an output file with an extension of @_iframe.m3u8@ : OutputKeyPrefix@Outputs:Key@ _v4.m3u8 OutputKeyPrefix@Outputs:Key@ _iframe.m3u8 OutputKeyPrefix@Outputs:Key@ .ts Elastic Transcoder automatically appends the relevant file extension to the file name. If you include a file extension in Output Key, the file name will have two extensions. If you include more than one output in a playlist, any segment duration settings, clip settings, or caption settings must be the same for all outputs in the playlist. For @Smooth@ playlists, the @Audio:Profile@ , @Video:Profile@ , and @Video:FrameRate@ to @Video:KeyframesMaxDist@ ratio must be the same for all outputs.
cjpOutputKeys :: Lens' CreateJobPlaylist [Text]
cjpOutputKeys = lens _cjpOutputKeys (\ s a -> s{_cjpOutputKeys = a}) . _Default . _Coerce

-- | The name that you want Elastic Transcoder to assign to the master playlist, for example, nyc-vacation.m3u8. If the name includes a @/@ character, the section of the name before the last @/@ must be identical for all @Name@ objects. If you create more than one master playlist, the values of all @Name@ objects must be unique.
cjpName :: Lens' CreateJobPlaylist (Maybe Text)
cjpName = lens _cjpName (\ s a -> s{_cjpName = a})

-- | The HLS content protection settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
cjpHlsContentProtection :: Lens' CreateJobPlaylist (Maybe HlsContentProtection)
cjpHlsContentProtection = lens _cjpHlsContentProtection (\ s a -> s{_cjpHlsContentProtection = a})

instance Hashable CreateJobPlaylist where

instance NFData CreateJobPlaylist where

instance ToJSON CreateJobPlaylist where
        toJSON CreateJobPlaylist'{..}
          = object
              (catMaybes
                 [("PlayReadyDrm" .=) <$> _cjpPlayReadyDrm,
                  ("Format" .=) <$> _cjpFormat,
                  ("OutputKeys" .=) <$> _cjpOutputKeys,
                  ("Name" .=) <$> _cjpName,
                  ("HlsContentProtection" .=) <$>
                    _cjpHlsContentProtection])

-- | The detected properties of the input file. Elastic Transcoder identifies these values from the input file.
--
--
--
-- /See:/ 'detectedProperties' smart constructor.
data DetectedProperties = DetectedProperties'
  { _dpHeight         :: !(Maybe Int)
  , _dpFrameRate      :: !(Maybe Text)
  , _dpFileSize       :: !(Maybe Integer)
  , _dpWidth          :: !(Maybe Int)
  , _dpDurationMillis :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetectedProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpHeight' - The detected height of the input file, in pixels.
--
-- * 'dpFrameRate' - The detected frame rate of the input file, in frames per second.
--
-- * 'dpFileSize' - The detected file size of the input file, in bytes.
--
-- * 'dpWidth' - The detected width of the input file, in pixels.
--
-- * 'dpDurationMillis' - The detected duration of the input file, in milliseconds.
detectedProperties
    :: DetectedProperties
detectedProperties =
  DetectedProperties'
    { _dpHeight = Nothing
    , _dpFrameRate = Nothing
    , _dpFileSize = Nothing
    , _dpWidth = Nothing
    , _dpDurationMillis = Nothing
    }


-- | The detected height of the input file, in pixels.
dpHeight :: Lens' DetectedProperties (Maybe Int)
dpHeight = lens _dpHeight (\ s a -> s{_dpHeight = a})

-- | The detected frame rate of the input file, in frames per second.
dpFrameRate :: Lens' DetectedProperties (Maybe Text)
dpFrameRate = lens _dpFrameRate (\ s a -> s{_dpFrameRate = a})

-- | The detected file size of the input file, in bytes.
dpFileSize :: Lens' DetectedProperties (Maybe Integer)
dpFileSize = lens _dpFileSize (\ s a -> s{_dpFileSize = a})

-- | The detected width of the input file, in pixels.
dpWidth :: Lens' DetectedProperties (Maybe Int)
dpWidth = lens _dpWidth (\ s a -> s{_dpWidth = a})

-- | The detected duration of the input file, in milliseconds.
dpDurationMillis :: Lens' DetectedProperties (Maybe Integer)
dpDurationMillis = lens _dpDurationMillis (\ s a -> s{_dpDurationMillis = a})

instance FromJSON DetectedProperties where
        parseJSON
          = withObject "DetectedProperties"
              (\ x ->
                 DetectedProperties' <$>
                   (x .:? "Height") <*> (x .:? "FrameRate") <*>
                     (x .:? "FileSize")
                     <*> (x .:? "Width")
                     <*> (x .:? "DurationMillis"))

instance Hashable DetectedProperties where

instance NFData DetectedProperties where

instance ToJSON DetectedProperties where
        toJSON DetectedProperties'{..}
          = object
              (catMaybes
                 [("Height" .=) <$> _dpHeight,
                  ("FrameRate" .=) <$> _dpFrameRate,
                  ("FileSize" .=) <$> _dpFileSize,
                  ("Width" .=) <$> _dpWidth,
                  ("DurationMillis" .=) <$> _dpDurationMillis])

-- | The encryption settings, if any, that are used for decrypting your input files or encrypting your output files. If your input file is encrypted, you must specify the mode that Elastic Transcoder uses to decrypt your file, otherwise you must specify the mode you want Elastic Transcoder to use to encrypt your output files.
--
--
--
-- /See:/ 'encryption' smart constructor.
data Encryption = Encryption'
  { _eMode                 :: !(Maybe Text)
  , _eKeyMD5               :: !(Maybe Text)
  , _eKey                  :: !(Maybe Text)
  , _eInitializationVector :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Encryption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eMode' - The specific server-side encryption mode that you want Elastic Transcoder to use when decrypting your input files or encrypting your output files. Elastic Transcoder supports the following options:     * __s3:__ Amazon S3 creates and manages the keys used for encrypting your files.     * __s3-aws-kms:__ Amazon S3 calls the Amazon Key Management Service, which creates and manages the keys that are used for encrypting your files. If you specify @s3-aws-kms@ and you don't want to use the default key, you must add the AWS-KMS key that you want to use to your pipeline.     * __aes-cbc-pkcs7:__ A padded cipher-block mode of operation originally used for HLS files.     * __aes-ctr:__ AES Counter Mode.     * __aes-gcm:__ AES Galois Counter Mode, a mode of operation that is an authenticated encryption format, meaning that a file, key, or initialization vector that has been tampered with fails the decryption process. For all three AES options, you must provide the following settings, which must be base64-encoded:     * __Key__      * __Key MD5__      * __Initialization Vector__  /Important:/ For the AES modes, your private encryption keys and your unencrypted data are never stored by AWS; therefore, it is important that you safely manage your encryption keys. If you lose them, you won't be able to unencrypt your data.
--
-- * 'eKeyMD5' - The MD5 digest of the key that you used to encrypt your input file, or that you want Elastic Transcoder to use to encrypt your output file. Elastic Transcoder uses the key digest as a checksum to make sure your key was not corrupted in transit. The key MD5 must be base64-encoded, and it must be exactly 16 bytes long before being base64-encoded.
--
-- * 'eKey' - The data encryption key that you want Elastic Transcoder to use to encrypt your output file, or that was used to encrypt your input file. The key must be base64-encoded and it must be one of the following bit lengths before being base64-encoded: @128@ , @192@ , or @256@ .  The key must also be encrypted by using the Amazon Key Management Service.
--
-- * 'eInitializationVector' - The series of random bits created by a random bit generator, unique for every encryption operation, that you used to encrypt your input files or that you want Elastic Transcoder to use to encrypt your output files. The initialization vector must be base64-encoded, and it must be exactly 16 bytes long before being base64-encoded.
encryption
    :: Encryption
encryption =
  Encryption'
    { _eMode = Nothing
    , _eKeyMD5 = Nothing
    , _eKey = Nothing
    , _eInitializationVector = Nothing
    }


-- | The specific server-side encryption mode that you want Elastic Transcoder to use when decrypting your input files or encrypting your output files. Elastic Transcoder supports the following options:     * __s3:__ Amazon S3 creates and manages the keys used for encrypting your files.     * __s3-aws-kms:__ Amazon S3 calls the Amazon Key Management Service, which creates and manages the keys that are used for encrypting your files. If you specify @s3-aws-kms@ and you don't want to use the default key, you must add the AWS-KMS key that you want to use to your pipeline.     * __aes-cbc-pkcs7:__ A padded cipher-block mode of operation originally used for HLS files.     * __aes-ctr:__ AES Counter Mode.     * __aes-gcm:__ AES Galois Counter Mode, a mode of operation that is an authenticated encryption format, meaning that a file, key, or initialization vector that has been tampered with fails the decryption process. For all three AES options, you must provide the following settings, which must be base64-encoded:     * __Key__      * __Key MD5__      * __Initialization Vector__  /Important:/ For the AES modes, your private encryption keys and your unencrypted data are never stored by AWS; therefore, it is important that you safely manage your encryption keys. If you lose them, you won't be able to unencrypt your data.
eMode :: Lens' Encryption (Maybe Text)
eMode = lens _eMode (\ s a -> s{_eMode = a})

-- | The MD5 digest of the key that you used to encrypt your input file, or that you want Elastic Transcoder to use to encrypt your output file. Elastic Transcoder uses the key digest as a checksum to make sure your key was not corrupted in transit. The key MD5 must be base64-encoded, and it must be exactly 16 bytes long before being base64-encoded.
eKeyMD5 :: Lens' Encryption (Maybe Text)
eKeyMD5 = lens _eKeyMD5 (\ s a -> s{_eKeyMD5 = a})

-- | The data encryption key that you want Elastic Transcoder to use to encrypt your output file, or that was used to encrypt your input file. The key must be base64-encoded and it must be one of the following bit lengths before being base64-encoded: @128@ , @192@ , or @256@ .  The key must also be encrypted by using the Amazon Key Management Service.
eKey :: Lens' Encryption (Maybe Text)
eKey = lens _eKey (\ s a -> s{_eKey = a})

-- | The series of random bits created by a random bit generator, unique for every encryption operation, that you used to encrypt your input files or that you want Elastic Transcoder to use to encrypt your output files. The initialization vector must be base64-encoded, and it must be exactly 16 bytes long before being base64-encoded.
eInitializationVector :: Lens' Encryption (Maybe Text)
eInitializationVector = lens _eInitializationVector (\ s a -> s{_eInitializationVector = a})

instance FromJSON Encryption where
        parseJSON
          = withObject "Encryption"
              (\ x ->
                 Encryption' <$>
                   (x .:? "Mode") <*> (x .:? "KeyMd5") <*> (x .:? "Key")
                     <*> (x .:? "InitializationVector"))

instance Hashable Encryption where

instance NFData Encryption where

instance ToJSON Encryption where
        toJSON Encryption'{..}
          = object
              (catMaybes
                 [("Mode" .=) <$> _eMode, ("KeyMd5" .=) <$> _eKeyMD5,
                  ("Key" .=) <$> _eKey,
                  ("InitializationVector" .=) <$>
                    _eInitializationVector])

-- | The HLS content protection settings, if any, that you want Elastic Transcoder to apply to your output files.
--
--
--
-- /See:/ 'hlsContentProtection' smart constructor.
data HlsContentProtection = HlsContentProtection'
  { _hcpKeyMD5                :: !(Maybe Text)
  , _hcpKeyStoragePolicy      :: !(Maybe Text)
  , _hcpKey                   :: !(Maybe Text)
  , _hcpMethod                :: !(Maybe Text)
  , _hcpInitializationVector  :: !(Maybe Text)
  , _hcpLicenseAcquisitionURL :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'HlsContentProtection' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hcpKeyMD5' - If Elastic Transcoder is generating your key for you, you must leave this field blank. The MD5 digest of the key that you want Elastic Transcoder to use to encrypt your output file, and that you want Elastic Transcoder to use as a checksum to make sure your key was not corrupted in transit. The key MD5 must be base64-encoded, and it must be exactly 16 bytes before being base64- encoded.
--
-- * 'hcpKeyStoragePolicy' - Specify whether you want Elastic Transcoder to write your HLS license key to an Amazon S3 bucket. If you choose @WithVariantPlaylists@ , @LicenseAcquisitionUrl@ must be left blank and Elastic Transcoder writes your data key into the same bucket as the associated playlist.
--
-- * 'hcpKey' - If you want Elastic Transcoder to generate a key for you, leave this field blank. If you choose to supply your own key, you must encrypt the key by using AWS KMS. The key must be base64-encoded, and it must be one of the following bit lengths before being base64-encoded: @128@ , @192@ , or @256@ .
--
-- * 'hcpMethod' - The content protection method for your output. The only valid value is: @aes-128@ . This value is written into the method attribute of the @EXT-X-KEY@ metadata tag in the output playlist.
--
-- * 'hcpInitializationVector' - If Elastic Transcoder is generating your key for you, you must leave this field blank. The series of random bits created by a random bit generator, unique for every encryption operation, that you want Elastic Transcoder to use to encrypt your output files. The initialization vector must be base64-encoded, and it must be exactly 16 bytes before being base64-encoded.
--
-- * 'hcpLicenseAcquisitionURL' - The location of the license key required to decrypt your HLS playlist. The URL must be an absolute path, and is referenced in the URI attribute of the EXT-X-KEY metadata tag in the playlist file.
hlsContentProtection
    :: HlsContentProtection
hlsContentProtection =
  HlsContentProtection'
    { _hcpKeyMD5 = Nothing
    , _hcpKeyStoragePolicy = Nothing
    , _hcpKey = Nothing
    , _hcpMethod = Nothing
    , _hcpInitializationVector = Nothing
    , _hcpLicenseAcquisitionURL = Nothing
    }


-- | If Elastic Transcoder is generating your key for you, you must leave this field blank. The MD5 digest of the key that you want Elastic Transcoder to use to encrypt your output file, and that you want Elastic Transcoder to use as a checksum to make sure your key was not corrupted in transit. The key MD5 must be base64-encoded, and it must be exactly 16 bytes before being base64- encoded.
hcpKeyMD5 :: Lens' HlsContentProtection (Maybe Text)
hcpKeyMD5 = lens _hcpKeyMD5 (\ s a -> s{_hcpKeyMD5 = a})

-- | Specify whether you want Elastic Transcoder to write your HLS license key to an Amazon S3 bucket. If you choose @WithVariantPlaylists@ , @LicenseAcquisitionUrl@ must be left blank and Elastic Transcoder writes your data key into the same bucket as the associated playlist.
hcpKeyStoragePolicy :: Lens' HlsContentProtection (Maybe Text)
hcpKeyStoragePolicy = lens _hcpKeyStoragePolicy (\ s a -> s{_hcpKeyStoragePolicy = a})

-- | If you want Elastic Transcoder to generate a key for you, leave this field blank. If you choose to supply your own key, you must encrypt the key by using AWS KMS. The key must be base64-encoded, and it must be one of the following bit lengths before being base64-encoded: @128@ , @192@ , or @256@ .
hcpKey :: Lens' HlsContentProtection (Maybe Text)
hcpKey = lens _hcpKey (\ s a -> s{_hcpKey = a})

-- | The content protection method for your output. The only valid value is: @aes-128@ . This value is written into the method attribute of the @EXT-X-KEY@ metadata tag in the output playlist.
hcpMethod :: Lens' HlsContentProtection (Maybe Text)
hcpMethod = lens _hcpMethod (\ s a -> s{_hcpMethod = a})

-- | If Elastic Transcoder is generating your key for you, you must leave this field blank. The series of random bits created by a random bit generator, unique for every encryption operation, that you want Elastic Transcoder to use to encrypt your output files. The initialization vector must be base64-encoded, and it must be exactly 16 bytes before being base64-encoded.
hcpInitializationVector :: Lens' HlsContentProtection (Maybe Text)
hcpInitializationVector = lens _hcpInitializationVector (\ s a -> s{_hcpInitializationVector = a})

-- | The location of the license key required to decrypt your HLS playlist. The URL must be an absolute path, and is referenced in the URI attribute of the EXT-X-KEY metadata tag in the playlist file.
hcpLicenseAcquisitionURL :: Lens' HlsContentProtection (Maybe Text)
hcpLicenseAcquisitionURL = lens _hcpLicenseAcquisitionURL (\ s a -> s{_hcpLicenseAcquisitionURL = a})

instance FromJSON HlsContentProtection where
        parseJSON
          = withObject "HlsContentProtection"
              (\ x ->
                 HlsContentProtection' <$>
                   (x .:? "KeyMd5") <*> (x .:? "KeyStoragePolicy") <*>
                     (x .:? "Key")
                     <*> (x .:? "Method")
                     <*> (x .:? "InitializationVector")
                     <*> (x .:? "LicenseAcquisitionUrl"))

instance Hashable HlsContentProtection where

instance NFData HlsContentProtection where

instance ToJSON HlsContentProtection where
        toJSON HlsContentProtection'{..}
          = object
              (catMaybes
                 [("KeyMd5" .=) <$> _hcpKeyMD5,
                  ("KeyStoragePolicy" .=) <$> _hcpKeyStoragePolicy,
                  ("Key" .=) <$> _hcpKey, ("Method" .=) <$> _hcpMethod,
                  ("InitializationVector" .=) <$>
                    _hcpInitializationVector,
                  ("LicenseAcquisitionUrl" .=) <$>
                    _hcpLicenseAcquisitionURL])

-- | The captions to be created, if any.
--
--
--
-- /See:/ 'inputCaptions' smart constructor.
data InputCaptions = InputCaptions'
  { _icMergePolicy    :: !(Maybe Text)
  , _icCaptionSources :: !(Maybe [CaptionSource])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'InputCaptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icMergePolicy' - A policy that determines how Elastic Transcoder handles the existence of multiple captions.     * __MergeOverride:__ Elastic Transcoder transcodes both embedded and sidecar captions into outputs. If captions for a language are embedded in the input file and also appear in a sidecar file, Elastic Transcoder uses the sidecar captions and ignores the embedded captions for that language.     * __MergeRetain:__ Elastic Transcoder transcodes both embedded and sidecar captions into outputs. If captions for a language are embedded in the input file and also appear in a sidecar file, Elastic Transcoder uses the embedded captions and ignores the sidecar captions for that language. If @CaptionSources@ is empty, Elastic Transcoder omits all sidecar captions from the output files.     * __Override:__ Elastic Transcoder transcodes only the sidecar captions that you specify in @CaptionSources@ . @MergePolicy@ cannot be null.
--
-- * 'icCaptionSources' - Source files for the input sidecar captions used during the transcoding process. To omit all sidecar captions, leave @CaptionSources@ blank.
inputCaptions
    :: InputCaptions
inputCaptions =
  InputCaptions' {_icMergePolicy = Nothing, _icCaptionSources = Nothing}


-- | A policy that determines how Elastic Transcoder handles the existence of multiple captions.     * __MergeOverride:__ Elastic Transcoder transcodes both embedded and sidecar captions into outputs. If captions for a language are embedded in the input file and also appear in a sidecar file, Elastic Transcoder uses the sidecar captions and ignores the embedded captions for that language.     * __MergeRetain:__ Elastic Transcoder transcodes both embedded and sidecar captions into outputs. If captions for a language are embedded in the input file and also appear in a sidecar file, Elastic Transcoder uses the embedded captions and ignores the sidecar captions for that language. If @CaptionSources@ is empty, Elastic Transcoder omits all sidecar captions from the output files.     * __Override:__ Elastic Transcoder transcodes only the sidecar captions that you specify in @CaptionSources@ . @MergePolicy@ cannot be null.
icMergePolicy :: Lens' InputCaptions (Maybe Text)
icMergePolicy = lens _icMergePolicy (\ s a -> s{_icMergePolicy = a})

-- | Source files for the input sidecar captions used during the transcoding process. To omit all sidecar captions, leave @CaptionSources@ blank.
icCaptionSources :: Lens' InputCaptions [CaptionSource]
icCaptionSources = lens _icCaptionSources (\ s a -> s{_icCaptionSources = a}) . _Default . _Coerce

instance FromJSON InputCaptions where
        parseJSON
          = withObject "InputCaptions"
              (\ x ->
                 InputCaptions' <$>
                   (x .:? "MergePolicy") <*>
                     (x .:? "CaptionSources" .!= mempty))

instance Hashable InputCaptions where

instance NFData InputCaptions where

instance ToJSON InputCaptions where
        toJSON InputCaptions'{..}
          = object
              (catMaybes
                 [("MergePolicy" .=) <$> _icMergePolicy,
                  ("CaptionSources" .=) <$> _icCaptionSources])

-- | A section of the response body that provides information about the job that is created.
--
--
--
-- /See:/ 'job'' smart constructor.
data Job' = Job''
  { _jStatus          :: !(Maybe Text)
  , _jPipelineId      :: !(Maybe Text)
  , _jARN             :: !(Maybe Text)
  , _jInputs          :: !(Maybe [JobInput])
  , _jInput           :: !(Maybe JobInput)
  , _jUserMetadata    :: !(Maybe (Map Text Text))
  , _jOutputs         :: !(Maybe [JobOutput])
  , _jOutput          :: !(Maybe JobOutput)
  , _jId              :: !(Maybe Text)
  , _jPlaylists       :: !(Maybe [Playlist])
  , _jOutputKeyPrefix :: !(Maybe Text)
  , _jTiming          :: !(Maybe Timing)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Job'' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jStatus' - The status of the job: @Submitted@ , @Progressing@ , @Complete@ , @Canceled@ , or @Error@ .
--
-- * 'jPipelineId' - The @Id@ of the pipeline that you want Elastic Transcoder to use for transcoding. The pipeline determines several settings, including the Amazon S3 bucket from which Elastic Transcoder gets the files to transcode and the bucket into which Elastic Transcoder puts the transcoded files.
--
-- * 'jARN' - The Amazon Resource Name (ARN) for the job.
--
-- * 'jInputs' - Information about the files that you're transcoding. If you specified multiple files for this job, Elastic Transcoder stitches the files together to make one output.
--
-- * 'jInput' - A section of the request or response body that provides information about the file that is being transcoded.
--
-- * 'jUserMetadata' - User-defined metadata that you want to associate with an Elastic Transcoder job. You specify metadata in @key/value@ pairs, and you can add up to 10 @key/value@ pairs per job. Elastic Transcoder does not guarantee that @key/value@ pairs are returned in the same order in which you specify them. Metadata @keys@ and @values@ must use characters from the following list:     * @0-9@      * @A-Z@ and @a-z@      * @Space@      * The following symbols: @_.:/=+-%@@
--
-- * 'jOutputs' - Information about the output files. We recommend that you use the @Outputs@ syntax for all jobs, even when you want Elastic Transcoder to transcode a file into only one format. Do not use both the @Outputs@ and @Output@ syntaxes in the same request. You can create a maximum of 30 outputs per job.  If you specify more than one output for a job, Elastic Transcoder creates the files for each output in the order in which you specify them in the job.
--
-- * 'jOutput' - If you specified one output for a job, information about that output. If you specified multiple outputs for a job, the Output object lists information about the first output. This duplicates the information that is listed for the first output in the Outputs object. /Important:/ Outputs recommended instead. A section of the request or response body that provides information about the transcoded (target) file.
--
-- * 'jId' - The identifier that Elastic Transcoder assigned to the job. You use this value to get settings for the job or to delete the job.
--
-- * 'jPlaylists' - /Important:/ Outputs in Fragmented MP4 or MPEG-TS format only. If you specify a preset in @PresetId@ for which the value of @Container@ is fmp4 (Fragmented MP4) or ts (MPEG-TS), @Playlists@ contains information about the master playlists that you want Elastic Transcoder to create. The maximum number of master playlists in a job is 30.
--
-- * 'jOutputKeyPrefix' - The value, if any, that you want Elastic Transcoder to prepend to the names of all files that this job creates, including output files, thumbnails, and playlists. We recommend that you add a / or some other delimiter to the end of the @OutputKeyPrefix@ .
--
-- * 'jTiming' - Details about the timing of a job.
job'
    :: Job'
job' =
  Job''
    { _jStatus = Nothing
    , _jPipelineId = Nothing
    , _jARN = Nothing
    , _jInputs = Nothing
    , _jInput = Nothing
    , _jUserMetadata = Nothing
    , _jOutputs = Nothing
    , _jOutput = Nothing
    , _jId = Nothing
    , _jPlaylists = Nothing
    , _jOutputKeyPrefix = Nothing
    , _jTiming = Nothing
    }


-- | The status of the job: @Submitted@ , @Progressing@ , @Complete@ , @Canceled@ , or @Error@ .
jStatus :: Lens' Job' (Maybe Text)
jStatus = lens _jStatus (\ s a -> s{_jStatus = a})

-- | The @Id@ of the pipeline that you want Elastic Transcoder to use for transcoding. The pipeline determines several settings, including the Amazon S3 bucket from which Elastic Transcoder gets the files to transcode and the bucket into which Elastic Transcoder puts the transcoded files.
jPipelineId :: Lens' Job' (Maybe Text)
jPipelineId = lens _jPipelineId (\ s a -> s{_jPipelineId = a})

-- | The Amazon Resource Name (ARN) for the job.
jARN :: Lens' Job' (Maybe Text)
jARN = lens _jARN (\ s a -> s{_jARN = a})

-- | Information about the files that you're transcoding. If you specified multiple files for this job, Elastic Transcoder stitches the files together to make one output.
jInputs :: Lens' Job' [JobInput]
jInputs = lens _jInputs (\ s a -> s{_jInputs = a}) . _Default . _Coerce

-- | A section of the request or response body that provides information about the file that is being transcoded.
jInput :: Lens' Job' (Maybe JobInput)
jInput = lens _jInput (\ s a -> s{_jInput = a})

-- | User-defined metadata that you want to associate with an Elastic Transcoder job. You specify metadata in @key/value@ pairs, and you can add up to 10 @key/value@ pairs per job. Elastic Transcoder does not guarantee that @key/value@ pairs are returned in the same order in which you specify them. Metadata @keys@ and @values@ must use characters from the following list:     * @0-9@      * @A-Z@ and @a-z@      * @Space@      * The following symbols: @_.:/=+-%@@
jUserMetadata :: Lens' Job' (HashMap Text Text)
jUserMetadata = lens _jUserMetadata (\ s a -> s{_jUserMetadata = a}) . _Default . _Map

-- | Information about the output files. We recommend that you use the @Outputs@ syntax for all jobs, even when you want Elastic Transcoder to transcode a file into only one format. Do not use both the @Outputs@ and @Output@ syntaxes in the same request. You can create a maximum of 30 outputs per job.  If you specify more than one output for a job, Elastic Transcoder creates the files for each output in the order in which you specify them in the job.
jOutputs :: Lens' Job' [JobOutput]
jOutputs = lens _jOutputs (\ s a -> s{_jOutputs = a}) . _Default . _Coerce

-- | If you specified one output for a job, information about that output. If you specified multiple outputs for a job, the Output object lists information about the first output. This duplicates the information that is listed for the first output in the Outputs object. /Important:/ Outputs recommended instead. A section of the request or response body that provides information about the transcoded (target) file.
jOutput :: Lens' Job' (Maybe JobOutput)
jOutput = lens _jOutput (\ s a -> s{_jOutput = a})

-- | The identifier that Elastic Transcoder assigned to the job. You use this value to get settings for the job or to delete the job.
jId :: Lens' Job' (Maybe Text)
jId = lens _jId (\ s a -> s{_jId = a})

-- | /Important:/ Outputs in Fragmented MP4 or MPEG-TS format only. If you specify a preset in @PresetId@ for which the value of @Container@ is fmp4 (Fragmented MP4) or ts (MPEG-TS), @Playlists@ contains information about the master playlists that you want Elastic Transcoder to create. The maximum number of master playlists in a job is 30.
jPlaylists :: Lens' Job' [Playlist]
jPlaylists = lens _jPlaylists (\ s a -> s{_jPlaylists = a}) . _Default . _Coerce

-- | The value, if any, that you want Elastic Transcoder to prepend to the names of all files that this job creates, including output files, thumbnails, and playlists. We recommend that you add a / or some other delimiter to the end of the @OutputKeyPrefix@ .
jOutputKeyPrefix :: Lens' Job' (Maybe Text)
jOutputKeyPrefix = lens _jOutputKeyPrefix (\ s a -> s{_jOutputKeyPrefix = a})

-- | Details about the timing of a job.
jTiming :: Lens' Job' (Maybe Timing)
jTiming = lens _jTiming (\ s a -> s{_jTiming = a})

instance FromJSON Job' where
        parseJSON
          = withObject "Job'"
              (\ x ->
                 Job'' <$>
                   (x .:? "Status") <*> (x .:? "PipelineId") <*>
                     (x .:? "Arn")
                     <*> (x .:? "Inputs" .!= mempty)
                     <*> (x .:? "Input")
                     <*> (x .:? "UserMetadata" .!= mempty)
                     <*> (x .:? "Outputs" .!= mempty)
                     <*> (x .:? "Output")
                     <*> (x .:? "Id")
                     <*> (x .:? "Playlists" .!= mempty)
                     <*> (x .:? "OutputKeyPrefix")
                     <*> (x .:? "Timing"))

instance Hashable Job' where

instance NFData Job' where

-- | The .jpg or .png file associated with an audio file.
--
--
--
-- /See:/ 'jobAlbumArt' smart constructor.
data JobAlbumArt = JobAlbumArt'
  { _jaaMergePolicy :: !(Maybe Text)
  , _jaaArtwork     :: !(Maybe [Artwork])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JobAlbumArt' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jaaMergePolicy' - A policy that determines how Elastic Transcoder handles the existence of multiple album artwork files.     * @Replace:@ The specified album art replaces any existing album art.     * @Prepend:@ The specified album art is placed in front of any existing album art.     * @Append:@ The specified album art is placed after any existing album art.     * @Fallback:@ If the original input file contains artwork, Elastic Transcoder uses that artwork for the output. If the original input does not contain artwork, Elastic Transcoder uses the specified album art file.
--
-- * 'jaaArtwork' - The file to be used as album art. There can be multiple artworks associated with an audio file, to a maximum of 20. Valid formats are @.jpg@ and @.png@
jobAlbumArt
    :: JobAlbumArt
jobAlbumArt = JobAlbumArt' {_jaaMergePolicy = Nothing, _jaaArtwork = Nothing}


-- | A policy that determines how Elastic Transcoder handles the existence of multiple album artwork files.     * @Replace:@ The specified album art replaces any existing album art.     * @Prepend:@ The specified album art is placed in front of any existing album art.     * @Append:@ The specified album art is placed after any existing album art.     * @Fallback:@ If the original input file contains artwork, Elastic Transcoder uses that artwork for the output. If the original input does not contain artwork, Elastic Transcoder uses the specified album art file.
jaaMergePolicy :: Lens' JobAlbumArt (Maybe Text)
jaaMergePolicy = lens _jaaMergePolicy (\ s a -> s{_jaaMergePolicy = a})

-- | The file to be used as album art. There can be multiple artworks associated with an audio file, to a maximum of 20. Valid formats are @.jpg@ and @.png@
jaaArtwork :: Lens' JobAlbumArt [Artwork]
jaaArtwork = lens _jaaArtwork (\ s a -> s{_jaaArtwork = a}) . _Default . _Coerce

instance FromJSON JobAlbumArt where
        parseJSON
          = withObject "JobAlbumArt"
              (\ x ->
                 JobAlbumArt' <$>
                   (x .:? "MergePolicy") <*>
                     (x .:? "Artwork" .!= mempty))

instance Hashable JobAlbumArt where

instance NFData JobAlbumArt where

instance ToJSON JobAlbumArt where
        toJSON JobAlbumArt'{..}
          = object
              (catMaybes
                 [("MergePolicy" .=) <$> _jaaMergePolicy,
                  ("Artwork" .=) <$> _jaaArtwork])

-- | Information about the file that you're transcoding.
--
--
--
-- /See:/ 'jobInput' smart constructor.
data JobInput = JobInput'
  { _jiFrameRate          :: !(Maybe Text)
  , _jiResolution         :: !(Maybe Text)
  , _jiAspectRatio        :: !(Maybe Text)
  , _jiTimeSpan           :: !(Maybe TimeSpan)
  , _jiEncryption         :: !(Maybe Encryption)
  , _jiKey                :: !(Maybe Text)
  , _jiDetectedProperties :: !(Maybe DetectedProperties)
  , _jiContainer          :: !(Maybe Text)
  , _jiInterlaced         :: !(Maybe Text)
  , _jiInputCaptions      :: !(Maybe InputCaptions)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JobInput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jiFrameRate' - The frame rate of the input file. If you want Elastic Transcoder to automatically detect the frame rate of the input file, specify @auto@ . If you want to specify the frame rate for the input file, enter one of the following values:  @10@ , @15@ , @23.97@ , @24@ , @25@ , @29.97@ , @30@ , @60@  If you specify a value other than @auto@ , Elastic Transcoder disables automatic detection of the frame rate.
--
-- * 'jiResolution' - This value must be @auto@ , which causes Elastic Transcoder to automatically detect the resolution of the input file.
--
-- * 'jiAspectRatio' - The aspect ratio of the input file. If you want Elastic Transcoder to automatically detect the aspect ratio of the input file, specify @auto@ . If you want to specify the aspect ratio for the output file, enter one of the following values:  @1:1@ , @4:3@ , @3:2@ , @16:9@  If you specify a value other than @auto@ , Elastic Transcoder disables automatic detection of the aspect ratio.
--
-- * 'jiTimeSpan' - Settings for clipping an input. Each input can have different clip settings.
--
-- * 'jiEncryption' - The encryption settings, if any, that are used for decrypting your input files. If your input file is encrypted, you must specify the mode that Elastic Transcoder uses to decrypt your file.
--
-- * 'jiKey' - The name of the file to transcode. Elsewhere in the body of the JSON block is the the ID of the pipeline to use for processing the job. The @InputBucket@ object in that pipeline tells Elastic Transcoder which Amazon S3 bucket to get the file from.  If the file name includes a prefix, such as @cooking/lasagna.mpg@ , include the prefix in the key. If the file isn't in the specified bucket, Elastic Transcoder returns an error.
--
-- * 'jiDetectedProperties' - The detected properties of the input file.
--
-- * 'jiContainer' - The container type for the input file. If you want Elastic Transcoder to automatically detect the container type of the input file, specify @auto@ . If you want to specify the container type for the input file, enter one of the following values:  @3gp@ , @aac@ , @asf@ , @avi@ , @divx@ , @flv@ , @m4a@ , @mkv@ , @mov@ , @mp3@ , @mp4@ , @mpeg@ , @mpeg-ps@ , @mpeg-ts@ , @mxf@ , @ogg@ , @vob@ , @wav@ , @webm@
--
-- * 'jiInterlaced' - Whether the input file is interlaced. If you want Elastic Transcoder to automatically detect whether the input file is interlaced, specify @auto@ . If you want to specify whether the input file is interlaced, enter one of the following values: @true@ , @false@  If you specify a value other than @auto@ , Elastic Transcoder disables automatic detection of interlacing.
--
-- * 'jiInputCaptions' - You can configure Elastic Transcoder to transcode captions, or subtitles, from one format to another. All captions must be in UTF-8. Elastic Transcoder supports two types of captions:     * __Embedded:__ Embedded captions are included in the same file as the audio and video. Elastic Transcoder supports only one embedded caption per language, to a maximum of 300 embedded captions per file. Valid input values include: @CEA-608 (EIA-608@ , first non-empty channel only), @CEA-708 (EIA-708@ , first non-empty channel only), and @mov-text@  Valid outputs include: @mov-text@  Elastic Transcoder supports a maximum of one embedded format per output.     * __Sidecar:__ Sidecar captions are kept in a separate metadata file from the audio and video data. Sidecar captions require a player that is capable of understanding the relationship between the video file and the sidecar file. Elastic Transcoder supports only one sidecar caption per language, to a maximum of 20 sidecar captions per file. Valid input values include: @dfxp@ (first div element only), @ebu-tt@ , @scc@ , @smpt@ , @srt@ , @ttml@ (first div element only), and @webvtt@  Valid outputs include: @dfxp@ (first div element only), @scc@ , @srt@ , and @webvtt@ . If you want ttml or smpte-tt compatible captions, specify dfxp as your output format. Elastic Transcoder does not support OCR (Optical Character Recognition), does not accept pictures as a valid input for captions, and is not available for audio-only transcoding. Elastic Transcoder does not preserve text formatting (for example, italics) during the transcoding process. To remove captions or leave the captions empty, set @Captions@ to null. To pass through existing captions unchanged, set the @MergePolicy@ to @MergeRetain@ , and pass in a null @CaptionSources@ array. For more information on embedded files, see the Subtitles Wikipedia page. For more information on sidecar files, see the Extensible Metadata Platform and Sidecar file Wikipedia pages.
jobInput
    :: JobInput
jobInput =
  JobInput'
    { _jiFrameRate = Nothing
    , _jiResolution = Nothing
    , _jiAspectRatio = Nothing
    , _jiTimeSpan = Nothing
    , _jiEncryption = Nothing
    , _jiKey = Nothing
    , _jiDetectedProperties = Nothing
    , _jiContainer = Nothing
    , _jiInterlaced = Nothing
    , _jiInputCaptions = Nothing
    }


-- | The frame rate of the input file. If you want Elastic Transcoder to automatically detect the frame rate of the input file, specify @auto@ . If you want to specify the frame rate for the input file, enter one of the following values:  @10@ , @15@ , @23.97@ , @24@ , @25@ , @29.97@ , @30@ , @60@  If you specify a value other than @auto@ , Elastic Transcoder disables automatic detection of the frame rate.
jiFrameRate :: Lens' JobInput (Maybe Text)
jiFrameRate = lens _jiFrameRate (\ s a -> s{_jiFrameRate = a})

-- | This value must be @auto@ , which causes Elastic Transcoder to automatically detect the resolution of the input file.
jiResolution :: Lens' JobInput (Maybe Text)
jiResolution = lens _jiResolution (\ s a -> s{_jiResolution = a})

-- | The aspect ratio of the input file. If you want Elastic Transcoder to automatically detect the aspect ratio of the input file, specify @auto@ . If you want to specify the aspect ratio for the output file, enter one of the following values:  @1:1@ , @4:3@ , @3:2@ , @16:9@  If you specify a value other than @auto@ , Elastic Transcoder disables automatic detection of the aspect ratio.
jiAspectRatio :: Lens' JobInput (Maybe Text)
jiAspectRatio = lens _jiAspectRatio (\ s a -> s{_jiAspectRatio = a})

-- | Settings for clipping an input. Each input can have different clip settings.
jiTimeSpan :: Lens' JobInput (Maybe TimeSpan)
jiTimeSpan = lens _jiTimeSpan (\ s a -> s{_jiTimeSpan = a})

-- | The encryption settings, if any, that are used for decrypting your input files. If your input file is encrypted, you must specify the mode that Elastic Transcoder uses to decrypt your file.
jiEncryption :: Lens' JobInput (Maybe Encryption)
jiEncryption = lens _jiEncryption (\ s a -> s{_jiEncryption = a})

-- | The name of the file to transcode. Elsewhere in the body of the JSON block is the the ID of the pipeline to use for processing the job. The @InputBucket@ object in that pipeline tells Elastic Transcoder which Amazon S3 bucket to get the file from.  If the file name includes a prefix, such as @cooking/lasagna.mpg@ , include the prefix in the key. If the file isn't in the specified bucket, Elastic Transcoder returns an error.
jiKey :: Lens' JobInput (Maybe Text)
jiKey = lens _jiKey (\ s a -> s{_jiKey = a})

-- | The detected properties of the input file.
jiDetectedProperties :: Lens' JobInput (Maybe DetectedProperties)
jiDetectedProperties = lens _jiDetectedProperties (\ s a -> s{_jiDetectedProperties = a})

-- | The container type for the input file. If you want Elastic Transcoder to automatically detect the container type of the input file, specify @auto@ . If you want to specify the container type for the input file, enter one of the following values:  @3gp@ , @aac@ , @asf@ , @avi@ , @divx@ , @flv@ , @m4a@ , @mkv@ , @mov@ , @mp3@ , @mp4@ , @mpeg@ , @mpeg-ps@ , @mpeg-ts@ , @mxf@ , @ogg@ , @vob@ , @wav@ , @webm@
jiContainer :: Lens' JobInput (Maybe Text)
jiContainer = lens _jiContainer (\ s a -> s{_jiContainer = a})

-- | Whether the input file is interlaced. If you want Elastic Transcoder to automatically detect whether the input file is interlaced, specify @auto@ . If you want to specify whether the input file is interlaced, enter one of the following values: @true@ , @false@  If you specify a value other than @auto@ , Elastic Transcoder disables automatic detection of interlacing.
jiInterlaced :: Lens' JobInput (Maybe Text)
jiInterlaced = lens _jiInterlaced (\ s a -> s{_jiInterlaced = a})

-- | You can configure Elastic Transcoder to transcode captions, or subtitles, from one format to another. All captions must be in UTF-8. Elastic Transcoder supports two types of captions:     * __Embedded:__ Embedded captions are included in the same file as the audio and video. Elastic Transcoder supports only one embedded caption per language, to a maximum of 300 embedded captions per file. Valid input values include: @CEA-608 (EIA-608@ , first non-empty channel only), @CEA-708 (EIA-708@ , first non-empty channel only), and @mov-text@  Valid outputs include: @mov-text@  Elastic Transcoder supports a maximum of one embedded format per output.     * __Sidecar:__ Sidecar captions are kept in a separate metadata file from the audio and video data. Sidecar captions require a player that is capable of understanding the relationship between the video file and the sidecar file. Elastic Transcoder supports only one sidecar caption per language, to a maximum of 20 sidecar captions per file. Valid input values include: @dfxp@ (first div element only), @ebu-tt@ , @scc@ , @smpt@ , @srt@ , @ttml@ (first div element only), and @webvtt@  Valid outputs include: @dfxp@ (first div element only), @scc@ , @srt@ , and @webvtt@ . If you want ttml or smpte-tt compatible captions, specify dfxp as your output format. Elastic Transcoder does not support OCR (Optical Character Recognition), does not accept pictures as a valid input for captions, and is not available for audio-only transcoding. Elastic Transcoder does not preserve text formatting (for example, italics) during the transcoding process. To remove captions or leave the captions empty, set @Captions@ to null. To pass through existing captions unchanged, set the @MergePolicy@ to @MergeRetain@ , and pass in a null @CaptionSources@ array. For more information on embedded files, see the Subtitles Wikipedia page. For more information on sidecar files, see the Extensible Metadata Platform and Sidecar file Wikipedia pages.
jiInputCaptions :: Lens' JobInput (Maybe InputCaptions)
jiInputCaptions = lens _jiInputCaptions (\ s a -> s{_jiInputCaptions = a})

instance FromJSON JobInput where
        parseJSON
          = withObject "JobInput"
              (\ x ->
                 JobInput' <$>
                   (x .:? "FrameRate") <*> (x .:? "Resolution") <*>
                     (x .:? "AspectRatio")
                     <*> (x .:? "TimeSpan")
                     <*> (x .:? "Encryption")
                     <*> (x .:? "Key")
                     <*> (x .:? "DetectedProperties")
                     <*> (x .:? "Container")
                     <*> (x .:? "Interlaced")
                     <*> (x .:? "InputCaptions"))

instance Hashable JobInput where

instance NFData JobInput where

instance ToJSON JobInput where
        toJSON JobInput'{..}
          = object
              (catMaybes
                 [("FrameRate" .=) <$> _jiFrameRate,
                  ("Resolution" .=) <$> _jiResolution,
                  ("AspectRatio" .=) <$> _jiAspectRatio,
                  ("TimeSpan" .=) <$> _jiTimeSpan,
                  ("Encryption" .=) <$> _jiEncryption,
                  ("Key" .=) <$> _jiKey,
                  ("DetectedProperties" .=) <$> _jiDetectedProperties,
                  ("Container" .=) <$> _jiContainer,
                  ("Interlaced" .=) <$> _jiInterlaced,
                  ("InputCaptions" .=) <$> _jiInputCaptions])

-- | /Important:/ Outputs recommended instead.
--
--
-- If you specified one output for a job, information about that output. If you specified multiple outputs for a job, the @Output@ object lists information about the first output. This duplicates the information that is listed for the first output in the @Outputs@ object.
--
--
-- /See:/ 'jobOutput' smart constructor.
data JobOutput = JobOutput'
  { _joAppliedColorSpaceConversion :: !(Maybe Text)
  , _joThumbnailPattern            :: !(Maybe Text)
  , _joStatus                      :: !(Maybe Text)
  , _joHeight                      :: !(Maybe Int)
  , _joFrameRate                   :: !(Maybe Text)
  , _joCaptions                    :: !(Maybe Captions)
  , _joPresetId                    :: !(Maybe Text)
  , _joComposition                 :: !(Maybe [Clip])
  , _joAlbumArt                    :: !(Maybe JobAlbumArt)
  , _joFileSize                    :: !(Maybe Integer)
  , _joWatermarks                  :: !(Maybe [JobWatermark])
  , _joWidth                       :: !(Maybe Int)
  , _joEncryption                  :: !(Maybe Encryption)
  , _joKey                         :: !(Maybe Text)
  , _joStatusDetail                :: !(Maybe Text)
  , _joId                          :: !(Maybe Text)
  , _joSegmentDuration             :: !(Maybe Text)
  , _joDurationMillis              :: !(Maybe Integer)
  , _joThumbnailEncryption         :: !(Maybe Encryption)
  , _joDuration                    :: !(Maybe Integer)
  , _joRotate                      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JobOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'joAppliedColorSpaceConversion' - If Elastic Transcoder used a preset with a @ColorSpaceConversionMode@ to transcode the output file, the @AppliedColorSpaceConversion@ parameter shows the conversion used. If no @ColorSpaceConversionMode@ was defined in the preset, this parameter is not be included in the job response.
--
-- * 'joThumbnailPattern' - Whether you want Elastic Transcoder to create thumbnails for your videos and, if so, how you want Elastic Transcoder to name the files. If you don't want Elastic Transcoder to create thumbnails, specify "". If you do want Elastic Transcoder to create thumbnails, specify the information that you want to include in the file name for each thumbnail. You can specify the following values in any sequence:     * __@{count}@ (Required)__ : If you want to create thumbnails, you must include @{count}@ in the @ThumbnailPattern@ object. Wherever you specify @{count}@ , Elastic Transcoder adds a five-digit sequence number (beginning with __00001__ ) to thumbnail file names. The number indicates where a given thumbnail appears in the sequence of thumbnails for a transcoded file.  /Important:/ If you specify a literal value and/or @{resolution}@ but you omit @{count}@ , Elastic Transcoder returns a validation error and does not create the job.     * __Literal values (Optional)__ : You can specify literal values anywhere in the @ThumbnailPattern@ object. For example, you can include them as a file name prefix or as a delimiter between @{resolution}@ and @{count}@ .      * __@{resolution}@ (Optional)__ : If you want Elastic Transcoder to include the resolution in the file name, include @{resolution}@ in the @ThumbnailPattern@ object.  When creating thumbnails, Elastic Transcoder automatically saves the files in the format (.jpg or .png) that appears in the preset that you specified in the @PresetID@ value of @CreateJobOutput@ . Elastic Transcoder also appends the applicable file name extension.
--
-- * 'joStatus' - The status of one output in a job. If you specified only one output for the job, @Outputs:Status@ is always the same as @Job:Status@ . If you specified more than one output:      * @Job:Status@ and @Outputs:Status@ for all of the outputs is Submitted until Elastic Transcoder starts to process the first output.     * When Elastic Transcoder starts to process the first output, @Outputs:Status@ for that output and @Job:Status@ both change to Progressing. For each output, the value of @Outputs:Status@ remains Submitted until Elastic Transcoder starts to process the output.     * Job:Status remains Progressing until all of the outputs reach a terminal status, either Complete or Error.     * When all of the outputs reach a terminal status, @Job:Status@ changes to Complete only if @Outputs:Status@ for all of the outputs is @Complete@ . If @Outputs:Status@ for one or more outputs is @Error@ , the terminal status for @Job:Status@ is also @Error@ . The value of @Status@ is one of the following: @Submitted@ , @Progressing@ , @Complete@ , @Canceled@ , or @Error@ .
--
-- * 'joHeight' - Height of the output file, in pixels.
--
-- * 'joFrameRate' - Frame rate of the output file, in frames per second.
--
-- * 'joCaptions' - You can configure Elastic Transcoder to transcode captions, or subtitles, from one format to another. All captions must be in UTF-8. Elastic Transcoder supports two types of captions:     * __Embedded:__ Embedded captions are included in the same file as the audio and video. Elastic Transcoder supports only one embedded caption per language, to a maximum of 300 embedded captions per file. Valid input values include: @CEA-608 (EIA-608@ , first non-empty channel only), @CEA-708 (EIA-708@ , first non-empty channel only), and @mov-text@  Valid outputs include: @mov-text@  Elastic Transcoder supports a maximum of one embedded format per output.     * __Sidecar:__ Sidecar captions are kept in a separate metadata file from the audio and video data. Sidecar captions require a player that is capable of understanding the relationship between the video file and the sidecar file. Elastic Transcoder supports only one sidecar caption per language, to a maximum of 20 sidecar captions per file. Valid input values include: @dfxp@ (first div element only), @ebu-tt@ , @scc@ , @smpt@ , @srt@ , @ttml@ (first div element only), and @webvtt@  Valid outputs include: @dfxp@ (first div element only), @scc@ , @srt@ , and @webvtt@ . If you want ttml or smpte-tt compatible captions, specify dfxp as your output format. Elastic Transcoder does not support OCR (Optical Character Recognition), does not accept pictures as a valid input for captions, and is not available for audio-only transcoding. Elastic Transcoder does not preserve text formatting (for example, italics) during the transcoding process. To remove captions or leave the captions empty, set @Captions@ to null. To pass through existing captions unchanged, set the @MergePolicy@ to @MergeRetain@ , and pass in a null @CaptionSources@ array. For more information on embedded files, see the Subtitles Wikipedia page. For more information on sidecar files, see the Extensible Metadata Platform and Sidecar file Wikipedia pages.
--
-- * 'joPresetId' - The value of the @Id@ object for the preset that you want to use for this job. The preset determines the audio, video, and thumbnail settings that Elastic Transcoder uses for transcoding. To use a preset that you created, specify the preset ID that Elastic Transcoder returned in the response when you created the preset. You can also use the Elastic Transcoder system presets, which you can get with @ListPresets@ .
--
-- * 'joComposition' - You can create an output file that contains an excerpt from the input file. This excerpt, called a clip, can come from the beginning, middle, or end of the file. The Composition object contains settings for the clips that make up an output file. For the current release, you can only specify settings for a single clip per output file. The Composition object cannot be null.
--
-- * 'joAlbumArt' - The album art to be associated with the output file, if any.
--
-- * 'joFileSize' - File size of the output file, in bytes.
--
-- * 'joWatermarks' - Information about the watermarks that you want Elastic Transcoder to add to the video during transcoding. You can specify up to four watermarks for each output. Settings for each watermark must be defined in the preset that you specify in @Preset@ for the current output. Watermarks are added to the output video in the sequence in which you list them in the job output
