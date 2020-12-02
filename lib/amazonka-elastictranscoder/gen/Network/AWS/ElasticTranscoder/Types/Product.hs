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
-- * 'eMode' - The specific server-side encryption mode that you want Elastic Transcoder to use when decrypting your input files or encrypting your output files. Elastic Transcoder supports the following options:     * __S3:__ Amazon S3 creates and manages the keys used for encrypting your files.     * __S3-AWS-KMS:__ Amazon S3 calls the Amazon Key Management Service, which creates and manages the keys that are used for encrypting your files. If you specify @S3-AWS-KMS@ and you don't want to use the default key, you must add the AWS-KMS key that you want to use to your pipeline.     * __AES-CBC-PKCS7:__ A padded cipher-block mode of operation originally used for HLS files.     * __AES-CTR:__ AES Counter Mode.     * __AES-GCM:__ AES Galois Counter Mode, a mode of operation that is an authenticated encryption format, meaning that a file, key, or initialization vector that has been tampered with fails the decryption process. For all three AES options, you must provide the following settings, which must be base64-encoded:     * __Key__      * __Key MD5__      * __Initialization Vector__  /Important:/ For the AES modes, your private encryption keys and your unencrypted data are never stored by AWS; therefore, it is important that you safely manage your encryption keys. If you lose them, you won't be able to unencrypt your data.
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


-- | The specific server-side encryption mode that you want Elastic Transcoder to use when decrypting your input files or encrypting your output files. Elastic Transcoder supports the following options:     * __S3:__ Amazon S3 creates and manages the keys used for encrypting your files.     * __S3-AWS-KMS:__ Amazon S3 calls the Amazon Key Management Service, which creates and manages the keys that are used for encrypting your files. If you specify @S3-AWS-KMS@ and you don't want to use the default key, you must add the AWS-KMS key that you want to use to your pipeline.     * __AES-CBC-PKCS7:__ A padded cipher-block mode of operation originally used for HLS files.     * __AES-CTR:__ AES Counter Mode.     * __AES-GCM:__ AES Galois Counter Mode, a mode of operation that is an authenticated encryption format, meaning that a file, key, or initialization vector that has been tampered with fails the decryption process. For all three AES options, you must provide the following settings, which must be base64-encoded:     * __Key__      * __Key MD5__      * __Initialization Vector__  /Important:/ For the AES modes, your private encryption keys and your unencrypted data are never stored by AWS; therefore, it is important that you safely manage your encryption keys. If you lose them, you won't be able to unencrypt your data.
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
-- * 'joWatermarks' - Information about the watermarks that you want Elastic Transcoder to add to the video during transcoding. You can specify up to four watermarks for each output. Settings for each watermark must be defined in the preset that you specify in @Preset@ for the current output. Watermarks are added to the output video in the sequence in which you list them in the job output—the first watermark in the list is added to the output video first, the second watermark in the list is added next, and so on. As a result, if the settings in a preset cause Elastic Transcoder to place all watermarks in the same location, the second watermark that you add covers the first one, the third one covers the second, and the fourth one covers the third.
--
-- * 'joWidth' - Specifies the width of the output file in pixels.
--
-- * 'joEncryption' - The encryption settings, if any, that you want Elastic Transcoder to apply to your output files. If you choose to use encryption, you must specify a mode to use. If you choose not to use encryption, Elastic Transcoder writes an unencrypted file to your Amazon S3 bucket.
--
-- * 'joKey' - The name to assign to the transcoded file. Elastic Transcoder saves the file in the Amazon S3 bucket specified by the @OutputBucket@ object in the pipeline that is specified by the pipeline ID.
--
-- * 'joStatusDetail' - Information that further explains @Status@ .
--
-- * 'joId' - A sequential counter, starting with 1, that identifies an output among the outputs from the current job. In the Output syntax, this value is always 1.
--
-- * 'joSegmentDuration' - /Important:/ (Outputs in Fragmented MP4 or MPEG-TS format only. If you specify a preset in @PresetId@ for which the value of @Container@ is @fmp4@ (Fragmented MP4) or @ts@ (MPEG-TS), @SegmentDuration@ is the target maximum duration of each segment in seconds. For @HLSv3@ format playlists, each media segment is stored in a separate @.ts@ file. For @HLSv4@ , @MPEG-DASH@ , and @Smooth@ playlists, all media segments for an output are stored in a single file. Each segment is approximately the length of the @SegmentDuration@ , though individual segments might be shorter or longer. The range of valid values is 1 to 60 seconds. If the duration of the video is not evenly divisible by @SegmentDuration@ , the duration of the last segment is the remainder of total length/SegmentDuration. Elastic Transcoder creates an output-specific playlist for each output @HLS@ output that you specify in OutputKeys. To add an output to the master playlist for this job, include it in the @OutputKeys@ of the associated playlist.
--
-- * 'joDurationMillis' - Duration of the output file, in milliseconds.
--
-- * 'joThumbnailEncryption' - The encryption settings, if any, that you want Elastic Transcoder to apply to your thumbnail.
--
-- * 'joDuration' - Duration of the output file, in seconds.
--
-- * 'joRotate' - The number of degrees clockwise by which you want Elastic Transcoder to rotate the output relative to the input. Enter one of the following values: @auto@ , @0@ , @90@ , @180@ , @270@  The value @auto@ generally works only if the file that you're transcoding contains rotation metadata.
jobOutput
    :: JobOutput
jobOutput =
  JobOutput'
    { _joAppliedColorSpaceConversion = Nothing
    , _joThumbnailPattern = Nothing
    , _joStatus = Nothing
    , _joHeight = Nothing
    , _joFrameRate = Nothing
    , _joCaptions = Nothing
    , _joPresetId = Nothing
    , _joComposition = Nothing
    , _joAlbumArt = Nothing
    , _joFileSize = Nothing
    , _joWatermarks = Nothing
    , _joWidth = Nothing
    , _joEncryption = Nothing
    , _joKey = Nothing
    , _joStatusDetail = Nothing
    , _joId = Nothing
    , _joSegmentDuration = Nothing
    , _joDurationMillis = Nothing
    , _joThumbnailEncryption = Nothing
    , _joDuration = Nothing
    , _joRotate = Nothing
    }


-- | If Elastic Transcoder used a preset with a @ColorSpaceConversionMode@ to transcode the output file, the @AppliedColorSpaceConversion@ parameter shows the conversion used. If no @ColorSpaceConversionMode@ was defined in the preset, this parameter is not be included in the job response.
joAppliedColorSpaceConversion :: Lens' JobOutput (Maybe Text)
joAppliedColorSpaceConversion = lens _joAppliedColorSpaceConversion (\ s a -> s{_joAppliedColorSpaceConversion = a})

-- | Whether you want Elastic Transcoder to create thumbnails for your videos and, if so, how you want Elastic Transcoder to name the files. If you don't want Elastic Transcoder to create thumbnails, specify "". If you do want Elastic Transcoder to create thumbnails, specify the information that you want to include in the file name for each thumbnail. You can specify the following values in any sequence:     * __@{count}@ (Required)__ : If you want to create thumbnails, you must include @{count}@ in the @ThumbnailPattern@ object. Wherever you specify @{count}@ , Elastic Transcoder adds a five-digit sequence number (beginning with __00001__ ) to thumbnail file names. The number indicates where a given thumbnail appears in the sequence of thumbnails for a transcoded file.  /Important:/ If you specify a literal value and/or @{resolution}@ but you omit @{count}@ , Elastic Transcoder returns a validation error and does not create the job.     * __Literal values (Optional)__ : You can specify literal values anywhere in the @ThumbnailPattern@ object. For example, you can include them as a file name prefix or as a delimiter between @{resolution}@ and @{count}@ .      * __@{resolution}@ (Optional)__ : If you want Elastic Transcoder to include the resolution in the file name, include @{resolution}@ in the @ThumbnailPattern@ object.  When creating thumbnails, Elastic Transcoder automatically saves the files in the format (.jpg or .png) that appears in the preset that you specified in the @PresetID@ value of @CreateJobOutput@ . Elastic Transcoder also appends the applicable file name extension.
joThumbnailPattern :: Lens' JobOutput (Maybe Text)
joThumbnailPattern = lens _joThumbnailPattern (\ s a -> s{_joThumbnailPattern = a})

-- | The status of one output in a job. If you specified only one output for the job, @Outputs:Status@ is always the same as @Job:Status@ . If you specified more than one output:      * @Job:Status@ and @Outputs:Status@ for all of the outputs is Submitted until Elastic Transcoder starts to process the first output.     * When Elastic Transcoder starts to process the first output, @Outputs:Status@ for that output and @Job:Status@ both change to Progressing. For each output, the value of @Outputs:Status@ remains Submitted until Elastic Transcoder starts to process the output.     * Job:Status remains Progressing until all of the outputs reach a terminal status, either Complete or Error.     * When all of the outputs reach a terminal status, @Job:Status@ changes to Complete only if @Outputs:Status@ for all of the outputs is @Complete@ . If @Outputs:Status@ for one or more outputs is @Error@ , the terminal status for @Job:Status@ is also @Error@ . The value of @Status@ is one of the following: @Submitted@ , @Progressing@ , @Complete@ , @Canceled@ , or @Error@ .
joStatus :: Lens' JobOutput (Maybe Text)
joStatus = lens _joStatus (\ s a -> s{_joStatus = a})

-- | Height of the output file, in pixels.
joHeight :: Lens' JobOutput (Maybe Int)
joHeight = lens _joHeight (\ s a -> s{_joHeight = a})

-- | Frame rate of the output file, in frames per second.
joFrameRate :: Lens' JobOutput (Maybe Text)
joFrameRate = lens _joFrameRate (\ s a -> s{_joFrameRate = a})

-- | You can configure Elastic Transcoder to transcode captions, or subtitles, from one format to another. All captions must be in UTF-8. Elastic Transcoder supports two types of captions:     * __Embedded:__ Embedded captions are included in the same file as the audio and video. Elastic Transcoder supports only one embedded caption per language, to a maximum of 300 embedded captions per file. Valid input values include: @CEA-608 (EIA-608@ , first non-empty channel only), @CEA-708 (EIA-708@ , first non-empty channel only), and @mov-text@  Valid outputs include: @mov-text@  Elastic Transcoder supports a maximum of one embedded format per output.     * __Sidecar:__ Sidecar captions are kept in a separate metadata file from the audio and video data. Sidecar captions require a player that is capable of understanding the relationship between the video file and the sidecar file. Elastic Transcoder supports only one sidecar caption per language, to a maximum of 20 sidecar captions per file. Valid input values include: @dfxp@ (first div element only), @ebu-tt@ , @scc@ , @smpt@ , @srt@ , @ttml@ (first div element only), and @webvtt@  Valid outputs include: @dfxp@ (first div element only), @scc@ , @srt@ , and @webvtt@ . If you want ttml or smpte-tt compatible captions, specify dfxp as your output format. Elastic Transcoder does not support OCR (Optical Character Recognition), does not accept pictures as a valid input for captions, and is not available for audio-only transcoding. Elastic Transcoder does not preserve text formatting (for example, italics) during the transcoding process. To remove captions or leave the captions empty, set @Captions@ to null. To pass through existing captions unchanged, set the @MergePolicy@ to @MergeRetain@ , and pass in a null @CaptionSources@ array. For more information on embedded files, see the Subtitles Wikipedia page. For more information on sidecar files, see the Extensible Metadata Platform and Sidecar file Wikipedia pages.
joCaptions :: Lens' JobOutput (Maybe Captions)
joCaptions = lens _joCaptions (\ s a -> s{_joCaptions = a})

-- | The value of the @Id@ object for the preset that you want to use for this job. The preset determines the audio, video, and thumbnail settings that Elastic Transcoder uses for transcoding. To use a preset that you created, specify the preset ID that Elastic Transcoder returned in the response when you created the preset. You can also use the Elastic Transcoder system presets, which you can get with @ListPresets@ .
joPresetId :: Lens' JobOutput (Maybe Text)
joPresetId = lens _joPresetId (\ s a -> s{_joPresetId = a})

-- | You can create an output file that contains an excerpt from the input file. This excerpt, called a clip, can come from the beginning, middle, or end of the file. The Composition object contains settings for the clips that make up an output file. For the current release, you can only specify settings for a single clip per output file. The Composition object cannot be null.
joComposition :: Lens' JobOutput [Clip]
joComposition = lens _joComposition (\ s a -> s{_joComposition = a}) . _Default . _Coerce

-- | The album art to be associated with the output file, if any.
joAlbumArt :: Lens' JobOutput (Maybe JobAlbumArt)
joAlbumArt = lens _joAlbumArt (\ s a -> s{_joAlbumArt = a})

-- | File size of the output file, in bytes.
joFileSize :: Lens' JobOutput (Maybe Integer)
joFileSize = lens _joFileSize (\ s a -> s{_joFileSize = a})

-- | Information about the watermarks that you want Elastic Transcoder to add to the video during transcoding. You can specify up to four watermarks for each output. Settings for each watermark must be defined in the preset that you specify in @Preset@ for the current output. Watermarks are added to the output video in the sequence in which you list them in the job output—the first watermark in the list is added to the output video first, the second watermark in the list is added next, and so on. As a result, if the settings in a preset cause Elastic Transcoder to place all watermarks in the same location, the second watermark that you add covers the first one, the third one covers the second, and the fourth one covers the third.
joWatermarks :: Lens' JobOutput [JobWatermark]
joWatermarks = lens _joWatermarks (\ s a -> s{_joWatermarks = a}) . _Default . _Coerce

-- | Specifies the width of the output file in pixels.
joWidth :: Lens' JobOutput (Maybe Int)
joWidth = lens _joWidth (\ s a -> s{_joWidth = a})

-- | The encryption settings, if any, that you want Elastic Transcoder to apply to your output files. If you choose to use encryption, you must specify a mode to use. If you choose not to use encryption, Elastic Transcoder writes an unencrypted file to your Amazon S3 bucket.
joEncryption :: Lens' JobOutput (Maybe Encryption)
joEncryption = lens _joEncryption (\ s a -> s{_joEncryption = a})

-- | The name to assign to the transcoded file. Elastic Transcoder saves the file in the Amazon S3 bucket specified by the @OutputBucket@ object in the pipeline that is specified by the pipeline ID.
joKey :: Lens' JobOutput (Maybe Text)
joKey = lens _joKey (\ s a -> s{_joKey = a})

-- | Information that further explains @Status@ .
joStatusDetail :: Lens' JobOutput (Maybe Text)
joStatusDetail = lens _joStatusDetail (\ s a -> s{_joStatusDetail = a})

-- | A sequential counter, starting with 1, that identifies an output among the outputs from the current job. In the Output syntax, this value is always 1.
joId :: Lens' JobOutput (Maybe Text)
joId = lens _joId (\ s a -> s{_joId = a})

-- | /Important:/ (Outputs in Fragmented MP4 or MPEG-TS format only. If you specify a preset in @PresetId@ for which the value of @Container@ is @fmp4@ (Fragmented MP4) or @ts@ (MPEG-TS), @SegmentDuration@ is the target maximum duration of each segment in seconds. For @HLSv3@ format playlists, each media segment is stored in a separate @.ts@ file. For @HLSv4@ , @MPEG-DASH@ , and @Smooth@ playlists, all media segments for an output are stored in a single file. Each segment is approximately the length of the @SegmentDuration@ , though individual segments might be shorter or longer. The range of valid values is 1 to 60 seconds. If the duration of the video is not evenly divisible by @SegmentDuration@ , the duration of the last segment is the remainder of total length/SegmentDuration. Elastic Transcoder creates an output-specific playlist for each output @HLS@ output that you specify in OutputKeys. To add an output to the master playlist for this job, include it in the @OutputKeys@ of the associated playlist.
joSegmentDuration :: Lens' JobOutput (Maybe Text)
joSegmentDuration = lens _joSegmentDuration (\ s a -> s{_joSegmentDuration = a})

-- | Duration of the output file, in milliseconds.
joDurationMillis :: Lens' JobOutput (Maybe Integer)
joDurationMillis = lens _joDurationMillis (\ s a -> s{_joDurationMillis = a})

-- | The encryption settings, if any, that you want Elastic Transcoder to apply to your thumbnail.
joThumbnailEncryption :: Lens' JobOutput (Maybe Encryption)
joThumbnailEncryption = lens _joThumbnailEncryption (\ s a -> s{_joThumbnailEncryption = a})

-- | Duration of the output file, in seconds.
joDuration :: Lens' JobOutput (Maybe Integer)
joDuration = lens _joDuration (\ s a -> s{_joDuration = a})

-- | The number of degrees clockwise by which you want Elastic Transcoder to rotate the output relative to the input. Enter one of the following values: @auto@ , @0@ , @90@ , @180@ , @270@  The value @auto@ generally works only if the file that you're transcoding contains rotation metadata.
joRotate :: Lens' JobOutput (Maybe Text)
joRotate = lens _joRotate (\ s a -> s{_joRotate = a})

instance FromJSON JobOutput where
        parseJSON
          = withObject "JobOutput"
              (\ x ->
                 JobOutput' <$>
                   (x .:? "AppliedColorSpaceConversion") <*>
                     (x .:? "ThumbnailPattern")
                     <*> (x .:? "Status")
                     <*> (x .:? "Height")
                     <*> (x .:? "FrameRate")
                     <*> (x .:? "Captions")
                     <*> (x .:? "PresetId")
                     <*> (x .:? "Composition" .!= mempty)
                     <*> (x .:? "AlbumArt")
                     <*> (x .:? "FileSize")
                     <*> (x .:? "Watermarks" .!= mempty)
                     <*> (x .:? "Width")
                     <*> (x .:? "Encryption")
                     <*> (x .:? "Key")
                     <*> (x .:? "StatusDetail")
                     <*> (x .:? "Id")
                     <*> (x .:? "SegmentDuration")
                     <*> (x .:? "DurationMillis")
                     <*> (x .:? "ThumbnailEncryption")
                     <*> (x .:? "Duration")
                     <*> (x .:? "Rotate"))

instance Hashable JobOutput where

instance NFData JobOutput where

-- | Watermarks can be in .png or .jpg format. If you want to display a watermark that is not rectangular, use the .png format, which supports transparency.
--
--
--
-- /See:/ 'jobWatermark' smart constructor.
data JobWatermark = JobWatermark'
  { _jwPresetWatermarkId :: !(Maybe Text)
  , _jwInputKey          :: !(Maybe Text)
  , _jwEncryption        :: !(Maybe Encryption)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'JobWatermark' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jwPresetWatermarkId' - The ID of the watermark settings that Elastic Transcoder uses to add watermarks to the video during transcoding. The settings are in the preset specified by Preset for the current output. In that preset, the value of Watermarks Id tells Elastic Transcoder which settings to use.
--
-- * 'jwInputKey' - The name of the .png or .jpg file that you want to use for the watermark. To determine which Amazon S3 bucket contains the specified file, Elastic Transcoder checks the pipeline specified by @Pipeline@ ; the @Input Bucket@ object in that pipeline identifies the bucket. If the file name includes a prefix, for example, __logos/128x64.png__ , include the prefix in the key. If the file isn't in the specified bucket, Elastic Transcoder returns an error.
--
-- * 'jwEncryption' - The encryption settings, if any, that you want Elastic Transcoder to apply to your watermarks.
jobWatermark
    :: JobWatermark
jobWatermark =
  JobWatermark'
    { _jwPresetWatermarkId = Nothing
    , _jwInputKey = Nothing
    , _jwEncryption = Nothing
    }


-- | The ID of the watermark settings that Elastic Transcoder uses to add watermarks to the video during transcoding. The settings are in the preset specified by Preset for the current output. In that preset, the value of Watermarks Id tells Elastic Transcoder which settings to use.
jwPresetWatermarkId :: Lens' JobWatermark (Maybe Text)
jwPresetWatermarkId = lens _jwPresetWatermarkId (\ s a -> s{_jwPresetWatermarkId = a})

-- | The name of the .png or .jpg file that you want to use for the watermark. To determine which Amazon S3 bucket contains the specified file, Elastic Transcoder checks the pipeline specified by @Pipeline@ ; the @Input Bucket@ object in that pipeline identifies the bucket. If the file name includes a prefix, for example, __logos/128x64.png__ , include the prefix in the key. If the file isn't in the specified bucket, Elastic Transcoder returns an error.
jwInputKey :: Lens' JobWatermark (Maybe Text)
jwInputKey = lens _jwInputKey (\ s a -> s{_jwInputKey = a})

-- | The encryption settings, if any, that you want Elastic Transcoder to apply to your watermarks.
jwEncryption :: Lens' JobWatermark (Maybe Encryption)
jwEncryption = lens _jwEncryption (\ s a -> s{_jwEncryption = a})

instance FromJSON JobWatermark where
        parseJSON
          = withObject "JobWatermark"
              (\ x ->
                 JobWatermark' <$>
                   (x .:? "PresetWatermarkId") <*> (x .:? "InputKey")
                     <*> (x .:? "Encryption"))

instance Hashable JobWatermark where

instance NFData JobWatermark where

instance ToJSON JobWatermark where
        toJSON JobWatermark'{..}
          = object
              (catMaybes
                 [("PresetWatermarkId" .=) <$> _jwPresetWatermarkId,
                  ("InputKey" .=) <$> _jwInputKey,
                  ("Encryption" .=) <$> _jwEncryption])

-- | The Amazon Simple Notification Service (Amazon SNS) topic or topics to notify in order to report job status.
--
--
-- /Important:/ To receive notifications, you must also subscribe to the new topic in the Amazon SNS console.
--
--
-- /See:/ 'notifications' smart constructor.
data Notifications = Notifications'
  { _nError       :: !(Maybe Text)
  , _nWarning     :: !(Maybe Text)
  , _nProgressing :: !(Maybe Text)
  , _nCompleted   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Notifications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nError' - The Amazon SNS topic that you want to notify when Elastic Transcoder encounters an error condition.
--
-- * 'nWarning' - The Amazon SNS topic that you want to notify when Elastic Transcoder encounters a warning condition.
--
-- * 'nProgressing' - The Amazon Simple Notification Service (Amazon SNS) topic that you want to notify when Elastic Transcoder has started to process the job.
--
-- * 'nCompleted' - The Amazon SNS topic that you want to notify when Elastic Transcoder has finished processing the job.
notifications
    :: Notifications
notifications =
  Notifications'
    { _nError = Nothing
    , _nWarning = Nothing
    , _nProgressing = Nothing
    , _nCompleted = Nothing
    }


-- | The Amazon SNS topic that you want to notify when Elastic Transcoder encounters an error condition.
nError :: Lens' Notifications (Maybe Text)
nError = lens _nError (\ s a -> s{_nError = a})

-- | The Amazon SNS topic that you want to notify when Elastic Transcoder encounters a warning condition.
nWarning :: Lens' Notifications (Maybe Text)
nWarning = lens _nWarning (\ s a -> s{_nWarning = a})

-- | The Amazon Simple Notification Service (Amazon SNS) topic that you want to notify when Elastic Transcoder has started to process the job.
nProgressing :: Lens' Notifications (Maybe Text)
nProgressing = lens _nProgressing (\ s a -> s{_nProgressing = a})

-- | The Amazon SNS topic that you want to notify when Elastic Transcoder has finished processing the job.
nCompleted :: Lens' Notifications (Maybe Text)
nCompleted = lens _nCompleted (\ s a -> s{_nCompleted = a})

instance FromJSON Notifications where
        parseJSON
          = withObject "Notifications"
              (\ x ->
                 Notifications' <$>
                   (x .:? "Error") <*> (x .:? "Warning") <*>
                     (x .:? "Progressing")
                     <*> (x .:? "Completed"))

instance Hashable Notifications where

instance NFData Notifications where

instance ToJSON Notifications where
        toJSON Notifications'{..}
          = object
              (catMaybes
                 [("Error" .=) <$> _nError,
                  ("Warning" .=) <$> _nWarning,
                  ("Progressing" .=) <$> _nProgressing,
                  ("Completed" .=) <$> _nCompleted])

-- | The @Permission@ structure.
--
--
--
-- /See:/ 'permission' smart constructor.
data Permission = Permission'
  { _pAccess      :: !(Maybe [Text])
  , _pGranteeType :: !(Maybe Text)
  , _pGrantee     :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Permission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pAccess' - The permission that you want to give to the AWS user that is listed in Grantee. Valid values include:      * @READ@ : The grantee can read the thumbnails and metadata for thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.     * @READ_ACP@ : The grantee can read the object ACL for thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.     * @WRITE_ACP@ : The grantee can write the ACL for the thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.     * @FULL_CONTROL@ : The grantee has READ, READ_ACP, and WRITE_ACP permissions for the thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.
--
-- * 'pGranteeType' - The type of value that appears in the Grantee object:     * @Canonical@ : Either the canonical user ID for an AWS account or an origin access identity for an Amazon CloudFront distribution. /Important:/ A canonical user ID is not the same as an AWS account number.     * @Email@ : The registered email address of an AWS account.     * @Group@ : One of the following predefined Amazon S3 groups: @AllUsers@ , @AuthenticatedUsers@ , or @LogDelivery@ .
--
-- * 'pGrantee' - The AWS user or group that you want to have access to transcoded files and playlists. To identify the user or group, you can specify the canonical user ID for an AWS account, an origin access identity for a CloudFront distribution, the registered email address of an AWS account, or a predefined Amazon S3 group.
permission
    :: Permission
permission =
  Permission' {_pAccess = Nothing, _pGranteeType = Nothing, _pGrantee = Nothing}


-- | The permission that you want to give to the AWS user that is listed in Grantee. Valid values include:      * @READ@ : The grantee can read the thumbnails and metadata for thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.     * @READ_ACP@ : The grantee can read the object ACL for thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.     * @WRITE_ACP@ : The grantee can write the ACL for the thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.     * @FULL_CONTROL@ : The grantee has READ, READ_ACP, and WRITE_ACP permissions for the thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.
pAccess :: Lens' Permission [Text]
pAccess = lens _pAccess (\ s a -> s{_pAccess = a}) . _Default . _Coerce

-- | The type of value that appears in the Grantee object:     * @Canonical@ : Either the canonical user ID for an AWS account or an origin access identity for an Amazon CloudFront distribution. /Important:/ A canonical user ID is not the same as an AWS account number.     * @Email@ : The registered email address of an AWS account.     * @Group@ : One of the following predefined Amazon S3 groups: @AllUsers@ , @AuthenticatedUsers@ , or @LogDelivery@ .
pGranteeType :: Lens' Permission (Maybe Text)
pGranteeType = lens _pGranteeType (\ s a -> s{_pGranteeType = a})

-- | The AWS user or group that you want to have access to transcoded files and playlists. To identify the user or group, you can specify the canonical user ID for an AWS account, an origin access identity for a CloudFront distribution, the registered email address of an AWS account, or a predefined Amazon S3 group.
pGrantee :: Lens' Permission (Maybe Text)
pGrantee = lens _pGrantee (\ s a -> s{_pGrantee = a})

instance FromJSON Permission where
        parseJSON
          = withObject "Permission"
              (\ x ->
                 Permission' <$>
                   (x .:? "Access" .!= mempty) <*> (x .:? "GranteeType")
                     <*> (x .:? "Grantee"))

instance Hashable Permission where

instance NFData Permission where

instance ToJSON Permission where
        toJSON Permission'{..}
          = object
              (catMaybes
                 [("Access" .=) <$> _pAccess,
                  ("GranteeType" .=) <$> _pGranteeType,
                  ("Grantee" .=) <$> _pGrantee])

-- | The pipeline (queue) that is used to manage jobs.
--
--
--
-- /See:/ 'pipeline' smart constructor.
data Pipeline = Pipeline'
  { _pipStatus          :: !(Maybe Text)
  , _pipARN             :: !(Maybe Text)
  , _pipInputBucket     :: !(Maybe Text)
  , _pipContentConfig   :: !(Maybe PipelineOutputConfig)
  , _pipOutputBucket    :: !(Maybe Text)
  , _pipRole            :: !(Maybe Text)
  , _pipName            :: !(Maybe Text)
  , _pipAWSKMSKeyARN    :: !(Maybe Text)
  , _pipId              :: !(Maybe Text)
  , _pipNotifications   :: !(Maybe Notifications)
  , _pipThumbnailConfig :: !(Maybe PipelineOutputConfig)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Pipeline' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pipStatus' - The current status of the pipeline:     * @Active@ : The pipeline is processing jobs.     * @Paused@ : The pipeline is not currently processing jobs.
--
-- * 'pipARN' - The Amazon Resource Name (ARN) for the pipeline.
--
-- * 'pipInputBucket' - The Amazon S3 bucket from which Elastic Transcoder gets media files for transcoding and the graphics files, if any, that you want to use for watermarks.
--
-- * 'pipContentConfig' - Information about the Amazon S3 bucket in which you want Elastic Transcoder to save transcoded files and playlists. Either you specify both @ContentConfig@ and @ThumbnailConfig@ , or you specify @OutputBucket@ .     * __Bucket__ : The Amazon S3 bucket in which you want Elastic Transcoder to save transcoded files and playlists.     * __Permissions__ : A list of the users and/or predefined Amazon S3 groups you want to have access to transcoded files and playlists, and the type of access that you want them to have.      * GranteeType: The type of value that appears in the @Grantee@ object:      * @Canonical@ : Either the canonical user ID for an AWS account or an origin access identity for an Amazon CloudFront distribution.     * @Email@ : The registered email address of an AWS account.     * @Group@ : One of the following predefined Amazon S3 groups: @AllUsers@ , @AuthenticatedUsers@ , or @LogDelivery@ .     * @Grantee@ : The AWS user or group that you want to have access to transcoded files and playlists.     * @Access@ : The permission that you want to give to the AWS user that is listed in @Grantee@ . Valid values include:     * @READ@ : The grantee can read the objects and metadata for objects that Elastic Transcoder adds to the Amazon S3 bucket.     * @READ_ACP@ : The grantee can read the object ACL for objects that Elastic Transcoder adds to the Amazon S3 bucket.     * @WRITE_ACP@ : The grantee can write the ACL for the objects that Elastic Transcoder adds to the Amazon S3 bucket.     * @FULL_CONTROL@ : The grantee has @READ@ , @READ_ACP@ , and @WRITE_ACP@ permissions for the objects that Elastic Transcoder adds to the Amazon S3 bucket.     * __StorageClass__ : The Amazon S3 storage class, Standard or ReducedRedundancy, that you want Elastic Transcoder to assign to the video files and playlists that it stores in your Amazon S3 bucket.
--
-- * 'pipOutputBucket' - The Amazon S3 bucket in which you want Elastic Transcoder to save transcoded files, thumbnails, and playlists. Either you specify this value, or you specify both @ContentConfig@ and @ThumbnailConfig@ .
--
-- * 'pipRole' - The IAM Amazon Resource Name (ARN) for the role that Elastic Transcoder uses to transcode jobs for this pipeline.
--
-- * 'pipName' - The name of the pipeline. We recommend that the name be unique within the AWS account, but uniqueness is not enforced. Constraints: Maximum 40 characters
--
-- * 'pipAWSKMSKeyARN' - The AWS Key Management Service (AWS KMS) key that you want to use with this pipeline. If you use either @S3@ or @S3-AWS-KMS@ as your @Encryption:Mode@ , you don't need to provide a key with your job because a default key, known as an AWS-KMS key, is created for you automatically. You need to provide an AWS-KMS key only if you want to use a non-default AWS-KMS key, or if you are using an @Encryption:Mode@ of @AES-PKCS7@ , @AES-CTR@ , or @AES-GCM@ .
--
-- * 'pipId' - The identifier for the pipeline. You use this value to identify the pipeline in which you want to perform a variety of operations, such as creating a job or a preset.
--
-- * 'pipNotifications' - The Amazon Simple Notification Service (Amazon SNS) topic that you want to notify to report job status. /Important:/ To receive notifications, you must also subscribe to the new topic in the Amazon SNS console.     * __Progressing__ (optional): The Amazon Simple Notification Service (Amazon SNS) topic that you want to notify when Elastic Transcoder has started to process the job.     * __Completed__ (optional): The Amazon SNS topic that you want to notify when Elastic Transcoder has finished processing the job.     * __Warning__ (optional): The Amazon SNS topic that you want to notify when Elastic Transcoder encounters a warning condition.     * __Error__ (optional): The Amazon SNS topic that you want to notify when Elastic Transcoder encounters an error condition.
--
-- * 'pipThumbnailConfig' - Information about the Amazon S3 bucket in which you want Elastic Transcoder to save thumbnail files. Either you specify both @ContentConfig@ and @ThumbnailConfig@ , or you specify @OutputBucket@ .     * @Bucket@ : The Amazon S3 bucket in which you want Elastic Transcoder to save thumbnail files.      * @Permissions@ : A list of the users and/or predefined Amazon S3 groups you want to have access to thumbnail files, and the type of access that you want them to have.      * GranteeType: The type of value that appears in the Grantee object:     * @Canonical@ : Either the canonical user ID for an AWS account or an origin access identity for an Amazon CloudFront distribution. /Important:/ A canonical user ID is not the same as an AWS account number.     * @Email@ : The registered email address of an AWS account.     * @Group@ : One of the following predefined Amazon S3 groups: @AllUsers@ , @AuthenticatedUsers@ , or @LogDelivery@ .     * @Grantee@ : The AWS user or group that you want to have access to thumbnail files.     * Access: The permission that you want to give to the AWS user that is listed in Grantee. Valid values include:      * @READ@ : The grantee can read the thumbnails and metadata for thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.     * @READ_ACP@ : The grantee can read the object ACL for thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.     * @WRITE_ACP@ : The grantee can write the ACL for the thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.     * @FULL_CONTROL@ : The grantee has READ, READ_ACP, and WRITE_ACP permissions for the thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.     * @StorageClass@ : The Amazon S3 storage class, @Standard@ or @ReducedRedundancy@ , that you want Elastic Transcoder to assign to the thumbnails that it stores in your Amazon S3 bucket.
pipeline
    :: Pipeline
pipeline =
  Pipeline'
    { _pipStatus = Nothing
    , _pipARN = Nothing
    , _pipInputBucket = Nothing
    , _pipContentConfig = Nothing
    , _pipOutputBucket = Nothing
    , _pipRole = Nothing
    , _pipName = Nothing
    , _pipAWSKMSKeyARN = Nothing
    , _pipId = Nothing
    , _pipNotifications = Nothing
    , _pipThumbnailConfig = Nothing
    }


-- | The current status of the pipeline:     * @Active@ : The pipeline is processing jobs.     * @Paused@ : The pipeline is not currently processing jobs.
pipStatus :: Lens' Pipeline (Maybe Text)
pipStatus = lens _pipStatus (\ s a -> s{_pipStatus = a})

-- | The Amazon Resource Name (ARN) for the pipeline.
pipARN :: Lens' Pipeline (Maybe Text)
pipARN = lens _pipARN (\ s a -> s{_pipARN = a})

-- | The Amazon S3 bucket from which Elastic Transcoder gets media files for transcoding and the graphics files, if any, that you want to use for watermarks.
pipInputBucket :: Lens' Pipeline (Maybe Text)
pipInputBucket = lens _pipInputBucket (\ s a -> s{_pipInputBucket = a})

-- | Information about the Amazon S3 bucket in which you want Elastic Transcoder to save transcoded files and playlists. Either you specify both @ContentConfig@ and @ThumbnailConfig@ , or you specify @OutputBucket@ .     * __Bucket__ : The Amazon S3 bucket in which you want Elastic Transcoder to save transcoded files and playlists.     * __Permissions__ : A list of the users and/or predefined Amazon S3 groups you want to have access to transcoded files and playlists, and the type of access that you want them to have.      * GranteeType: The type of value that appears in the @Grantee@ object:      * @Canonical@ : Either the canonical user ID for an AWS account or an origin access identity for an Amazon CloudFront distribution.     * @Email@ : The registered email address of an AWS account.     * @Group@ : One of the following predefined Amazon S3 groups: @AllUsers@ , @AuthenticatedUsers@ , or @LogDelivery@ .     * @Grantee@ : The AWS user or group that you want to have access to transcoded files and playlists.     * @Access@ : The permission that you want to give to the AWS user that is listed in @Grantee@ . Valid values include:     * @READ@ : The grantee can read the objects and metadata for objects that Elastic Transcoder adds to the Amazon S3 bucket.     * @READ_ACP@ : The grantee can read the object ACL for objects that Elastic Transcoder adds to the Amazon S3 bucket.     * @WRITE_ACP@ : The grantee can write the ACL for the objects that Elastic Transcoder adds to the Amazon S3 bucket.     * @FULL_CONTROL@ : The grantee has @READ@ , @READ_ACP@ , and @WRITE_ACP@ permissions for the objects that Elastic Transcoder adds to the Amazon S3 bucket.     * __StorageClass__ : The Amazon S3 storage class, Standard or ReducedRedundancy, that you want Elastic Transcoder to assign to the video files and playlists that it stores in your Amazon S3 bucket.
pipContentConfig :: Lens' Pipeline (Maybe PipelineOutputConfig)
pipContentConfig = lens _pipContentConfig (\ s a -> s{_pipContentConfig = a})

-- | The Amazon S3 bucket in which you want Elastic Transcoder to save transcoded files, thumbnails, and playlists. Either you specify this value, or you specify both @ContentConfig@ and @ThumbnailConfig@ .
pipOutputBucket :: Lens' Pipeline (Maybe Text)
pipOutputBucket = lens _pipOutputBucket (\ s a -> s{_pipOutputBucket = a})

-- | The IAM Amazon Resource Name (ARN) for the role that Elastic Transcoder uses to transcode jobs for this pipeline.
pipRole :: Lens' Pipeline (Maybe Text)
pipRole = lens _pipRole (\ s a -> s{_pipRole = a})

-- | The name of the pipeline. We recommend that the name be unique within the AWS account, but uniqueness is not enforced. Constraints: Maximum 40 characters
pipName :: Lens' Pipeline (Maybe Text)
pipName = lens _pipName (\ s a -> s{_pipName = a})

-- | The AWS Key Management Service (AWS KMS) key that you want to use with this pipeline. If you use either @S3@ or @S3-AWS-KMS@ as your @Encryption:Mode@ , you don't need to provide a key with your job because a default key, known as an AWS-KMS key, is created for you automatically. You need to provide an AWS-KMS key only if you want to use a non-default AWS-KMS key, or if you are using an @Encryption:Mode@ of @AES-PKCS7@ , @AES-CTR@ , or @AES-GCM@ .
pipAWSKMSKeyARN :: Lens' Pipeline (Maybe Text)
pipAWSKMSKeyARN = lens _pipAWSKMSKeyARN (\ s a -> s{_pipAWSKMSKeyARN = a})

-- | The identifier for the pipeline. You use this value to identify the pipeline in which you want to perform a variety of operations, such as creating a job or a preset.
pipId :: Lens' Pipeline (Maybe Text)
pipId = lens _pipId (\ s a -> s{_pipId = a})

-- | The Amazon Simple Notification Service (Amazon SNS) topic that you want to notify to report job status. /Important:/ To receive notifications, you must also subscribe to the new topic in the Amazon SNS console.     * __Progressing__ (optional): The Amazon Simple Notification Service (Amazon SNS) topic that you want to notify when Elastic Transcoder has started to process the job.     * __Completed__ (optional): The Amazon SNS topic that you want to notify when Elastic Transcoder has finished processing the job.     * __Warning__ (optional): The Amazon SNS topic that you want to notify when Elastic Transcoder encounters a warning condition.     * __Error__ (optional): The Amazon SNS topic that you want to notify when Elastic Transcoder encounters an error condition.
pipNotifications :: Lens' Pipeline (Maybe Notifications)
pipNotifications = lens _pipNotifications (\ s a -> s{_pipNotifications = a})

-- | Information about the Amazon S3 bucket in which you want Elastic Transcoder to save thumbnail files. Either you specify both @ContentConfig@ and @ThumbnailConfig@ , or you specify @OutputBucket@ .     * @Bucket@ : The Amazon S3 bucket in which you want Elastic Transcoder to save thumbnail files.      * @Permissions@ : A list of the users and/or predefined Amazon S3 groups you want to have access to thumbnail files, and the type of access that you want them to have.      * GranteeType: The type of value that appears in the Grantee object:     * @Canonical@ : Either the canonical user ID for an AWS account or an origin access identity for an Amazon CloudFront distribution. /Important:/ A canonical user ID is not the same as an AWS account number.     * @Email@ : The registered email address of an AWS account.     * @Group@ : One of the following predefined Amazon S3 groups: @AllUsers@ , @AuthenticatedUsers@ , or @LogDelivery@ .     * @Grantee@ : The AWS user or group that you want to have access to thumbnail files.     * Access: The permission that you want to give to the AWS user that is listed in Grantee. Valid values include:      * @READ@ : The grantee can read the thumbnails and metadata for thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.     * @READ_ACP@ : The grantee can read the object ACL for thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.     * @WRITE_ACP@ : The grantee can write the ACL for the thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.     * @FULL_CONTROL@ : The grantee has READ, READ_ACP, and WRITE_ACP permissions for the thumbnails that Elastic Transcoder adds to the Amazon S3 bucket.     * @StorageClass@ : The Amazon S3 storage class, @Standard@ or @ReducedRedundancy@ , that you want Elastic Transcoder to assign to the thumbnails that it stores in your Amazon S3 bucket.
pipThumbnailConfig :: Lens' Pipeline (Maybe PipelineOutputConfig)
pipThumbnailConfig = lens _pipThumbnailConfig (\ s a -> s{_pipThumbnailConfig = a})

instance FromJSON Pipeline where
        parseJSON
          = withObject "Pipeline"
              (\ x ->
                 Pipeline' <$>
                   (x .:? "Status") <*> (x .:? "Arn") <*>
                     (x .:? "InputBucket")
                     <*> (x .:? "ContentConfig")
                     <*> (x .:? "OutputBucket")
                     <*> (x .:? "Role")
                     <*> (x .:? "Name")
                     <*> (x .:? "AwsKmsKeyArn")
                     <*> (x .:? "Id")
                     <*> (x .:? "Notifications")
                     <*> (x .:? "ThumbnailConfig"))

instance Hashable Pipeline where

instance NFData Pipeline where

-- | The @PipelineOutputConfig@ structure.
--
--
--
-- /See:/ 'pipelineOutputConfig' smart constructor.
data PipelineOutputConfig = PipelineOutputConfig'
  { _pocBucket       :: !(Maybe Text)
  , _pocStorageClass :: !(Maybe Text)
  , _pocPermissions  :: !(Maybe [Permission])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PipelineOutputConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pocBucket' - The Amazon S3 bucket in which you want Elastic Transcoder to save the transcoded files. Specify this value when all of the following are true:     * You want to save transcoded files, thumbnails (if any), and playlists (if any) together in one bucket.     * You do not want to specify the users or groups who have access to the transcoded files, thumbnails, and playlists.     * You do not want to specify the permissions that Elastic Transcoder grants to the files.     * You want to associate the transcoded files and thumbnails with the Amazon S3 Standard storage class. If you want to save transcoded files and playlists in one bucket and thumbnails in another bucket, specify which users can access the transcoded files or the permissions the users have, or change the Amazon S3 storage class, omit OutputBucket and specify values for @ContentConfig@ and @ThumbnailConfig@ instead.
--
-- * 'pocStorageClass' - The Amazon S3 storage class, @Standard@ or @ReducedRedundancy@ , that you want Elastic Transcoder to assign to the video files and playlists that it stores in your Amazon S3 bucket.
--
-- * 'pocPermissions' - Optional. The @Permissions@ object specifies which users and/or predefined Amazon S3 groups you want to have access to transcoded files and playlists, and the type of access you want them to have. You can grant permissions to a maximum of 30 users and/or predefined Amazon S3 groups. If you include @Permissions@ , Elastic Transcoder grants only the permissions that you specify. It does not grant full permissions to the owner of the role specified by @Role@ . If you want that user to have full control, you must explicitly grant full control to the user. If you omit @Permissions@ , Elastic Transcoder grants full control over the transcoded files and playlists to the owner of the role specified by @Role@ , and grants no other permissions to any other user or group.
pipelineOutputConfig
    :: PipelineOutputConfig
pipelineOutputConfig =
  PipelineOutputConfig'
    { _pocBucket = Nothing
    , _pocStorageClass = Nothing
    , _pocPermissions = Nothing
    }


-- | The Amazon S3 bucket in which you want Elastic Transcoder to save the transcoded files. Specify this value when all of the following are true:     * You want to save transcoded files, thumbnails (if any), and playlists (if any) together in one bucket.     * You do not want to specify the users or groups who have access to the transcoded files, thumbnails, and playlists.     * You do not want to specify the permissions that Elastic Transcoder grants to the files.     * You want to associate the transcoded files and thumbnails with the Amazon S3 Standard storage class. If you want to save transcoded files and playlists in one bucket and thumbnails in another bucket, specify which users can access the transcoded files or the permissions the users have, or change the Amazon S3 storage class, omit OutputBucket and specify values for @ContentConfig@ and @ThumbnailConfig@ instead.
pocBucket :: Lens' PipelineOutputConfig (Maybe Text)
pocBucket = lens _pocBucket (\ s a -> s{_pocBucket = a})

-- | The Amazon S3 storage class, @Standard@ or @ReducedRedundancy@ , that you want Elastic Transcoder to assign to the video files and playlists that it stores in your Amazon S3 bucket.
pocStorageClass :: Lens' PipelineOutputConfig (Maybe Text)
pocStorageClass = lens _pocStorageClass (\ s a -> s{_pocStorageClass = a})

-- | Optional. The @Permissions@ object specifies which users and/or predefined Amazon S3 groups you want to have access to transcoded files and playlists, and the type of access you want them to have. You can grant permissions to a maximum of 30 users and/or predefined Amazon S3 groups. If you include @Permissions@ , Elastic Transcoder grants only the permissions that you specify. It does not grant full permissions to the owner of the role specified by @Role@ . If you want that user to have full control, you must explicitly grant full control to the user. If you omit @Permissions@ , Elastic Transcoder grants full control over the transcoded files and playlists to the owner of the role specified by @Role@ , and grants no other permissions to any other user or group.
pocPermissions :: Lens' PipelineOutputConfig [Permission]
pocPermissions = lens _pocPermissions (\ s a -> s{_pocPermissions = a}) . _Default . _Coerce

instance FromJSON PipelineOutputConfig where
        parseJSON
          = withObject "PipelineOutputConfig"
              (\ x ->
                 PipelineOutputConfig' <$>
                   (x .:? "Bucket") <*> (x .:? "StorageClass") <*>
                     (x .:? "Permissions" .!= mempty))

instance Hashable PipelineOutputConfig where

instance NFData PipelineOutputConfig where

instance ToJSON PipelineOutputConfig where
        toJSON PipelineOutputConfig'{..}
          = object
              (catMaybes
                 [("Bucket" .=) <$> _pocBucket,
                  ("StorageClass" .=) <$> _pocStorageClass,
                  ("Permissions" .=) <$> _pocPermissions])

-- | The PlayReady DRM settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
--
--
-- PlayReady DRM encrypts your media files using @AES-CTR@ encryption.
--
-- If you use DRM for an @HLSv3@ playlist, your outputs must have a master playlist.
--
--
-- /See:/ 'playReadyDrm' smart constructor.
data PlayReadyDrm = PlayReadyDrm'
  { _prdKeyId                 :: !(Maybe Text)
  , _prdFormat                :: !(Maybe Text)
  , _prdKeyMD5                :: !(Maybe Text)
  , _prdKey                   :: !(Maybe Text)
  , _prdInitializationVector  :: !(Maybe Text)
  , _prdLicenseAcquisitionURL :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PlayReadyDrm' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prdKeyId' - The ID for your DRM key, so that your DRM license provider knows which key to provide. The key ID must be provided in big endian, and Elastic Transcoder converts it to little endian before inserting it into the PlayReady DRM headers. If you are unsure whether your license server provides your key ID in big or little endian, check with your DRM provider.
--
-- * 'prdFormat' - The type of DRM, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
--
-- * 'prdKeyMD5' - The MD5 digest of the key used for DRM on your file, and that you want Elastic Transcoder to use as a checksum to make sure your key was not corrupted in transit. The key MD5 must be base64-encoded, and it must be exactly 16 bytes before being base64-encoded.
--
-- * 'prdKey' - The DRM key for your file, provided by your DRM license provider. The key must be base64-encoded, and it must be one of the following bit lengths before being base64-encoded: @128@ , @192@ , or @256@ .  The key must also be encrypted by using AWS KMS.
--
-- * 'prdInitializationVector' - The series of random bits created by a random bit generator, unique for every encryption operation, that you want Elastic Transcoder to use to encrypt your files. The initialization vector must be base64-encoded, and it must be exactly 8 bytes long before being base64-encoded. If no initialization vector is provided, Elastic Transcoder generates one for you.
--
-- * 'prdLicenseAcquisitionURL' - The location of the license key required to play DRM content. The URL must be an absolute path, and is referenced by the PlayReady header. The PlayReady header is referenced in the protection header of the client manifest for Smooth Streaming outputs, and in the EXT-X-DXDRM and EXT-XDXDRMINFO metadata tags for HLS playlist outputs. An example URL looks like this: @https://www.example.com/exampleKey/@
playReadyDrm
    :: PlayReadyDrm
playReadyDrm =
  PlayReadyDrm'
    { _prdKeyId = Nothing
    , _prdFormat = Nothing
    , _prdKeyMD5 = Nothing
    , _prdKey = Nothing
    , _prdInitializationVector = Nothing
    , _prdLicenseAcquisitionURL = Nothing
    }


-- | The ID for your DRM key, so that your DRM license provider knows which key to provide. The key ID must be provided in big endian, and Elastic Transcoder converts it to little endian before inserting it into the PlayReady DRM headers. If you are unsure whether your license server provides your key ID in big or little endian, check with your DRM provider.
prdKeyId :: Lens' PlayReadyDrm (Maybe Text)
prdKeyId = lens _prdKeyId (\ s a -> s{_prdKeyId = a})

-- | The type of DRM, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
prdFormat :: Lens' PlayReadyDrm (Maybe Text)
prdFormat = lens _prdFormat (\ s a -> s{_prdFormat = a})

-- | The MD5 digest of the key used for DRM on your file, and that you want Elastic Transcoder to use as a checksum to make sure your key was not corrupted in transit. The key MD5 must be base64-encoded, and it must be exactly 16 bytes before being base64-encoded.
prdKeyMD5 :: Lens' PlayReadyDrm (Maybe Text)
prdKeyMD5 = lens _prdKeyMD5 (\ s a -> s{_prdKeyMD5 = a})

-- | The DRM key for your file, provided by your DRM license provider. The key must be base64-encoded, and it must be one of the following bit lengths before being base64-encoded: @128@ , @192@ , or @256@ .  The key must also be encrypted by using AWS KMS.
prdKey :: Lens' PlayReadyDrm (Maybe Text)
prdKey = lens _prdKey (\ s a -> s{_prdKey = a})

-- | The series of random bits created by a random bit generator, unique for every encryption operation, that you want Elastic Transcoder to use to encrypt your files. The initialization vector must be base64-encoded, and it must be exactly 8 bytes long before being base64-encoded. If no initialization vector is provided, Elastic Transcoder generates one for you.
prdInitializationVector :: Lens' PlayReadyDrm (Maybe Text)
prdInitializationVector = lens _prdInitializationVector (\ s a -> s{_prdInitializationVector = a})

-- | The location of the license key required to play DRM content. The URL must be an absolute path, and is referenced by the PlayReady header. The PlayReady header is referenced in the protection header of the client manifest for Smooth Streaming outputs, and in the EXT-X-DXDRM and EXT-XDXDRMINFO metadata tags for HLS playlist outputs. An example URL looks like this: @https://www.example.com/exampleKey/@
prdLicenseAcquisitionURL :: Lens' PlayReadyDrm (Maybe Text)
prdLicenseAcquisitionURL = lens _prdLicenseAcquisitionURL (\ s a -> s{_prdLicenseAcquisitionURL = a})

instance FromJSON PlayReadyDrm where
        parseJSON
          = withObject "PlayReadyDrm"
              (\ x ->
                 PlayReadyDrm' <$>
                   (x .:? "KeyId") <*> (x .:? "Format") <*>
                     (x .:? "KeyMd5")
                     <*> (x .:? "Key")
                     <*> (x .:? "InitializationVector")
                     <*> (x .:? "LicenseAcquisitionUrl"))

instance Hashable PlayReadyDrm where

instance NFData PlayReadyDrm where

instance ToJSON PlayReadyDrm where
        toJSON PlayReadyDrm'{..}
          = object
              (catMaybes
                 [("KeyId" .=) <$> _prdKeyId,
                  ("Format" .=) <$> _prdFormat,
                  ("KeyMd5" .=) <$> _prdKeyMD5, ("Key" .=) <$> _prdKey,
                  ("InitializationVector" .=) <$>
                    _prdInitializationVector,
                  ("LicenseAcquisitionUrl" .=) <$>
                    _prdLicenseAcquisitionURL])

-- | Use Only for Fragmented MP4 or MPEG-TS Outputs. If you specify a preset for which the value of Container is @fmp4@ (Fragmented MP4) or @ts@ (MPEG-TS), Playlists contains information about the master playlists that you want Elastic Transcoder to create. We recommend that you create only one master playlist per output format. The maximum number of master playlists in a job is 30.
--
--
--
-- /See:/ 'playlist' smart constructor.
data Playlist = Playlist'
  { _pStatus               :: !(Maybe Text)
  , _pPlayReadyDrm         :: !(Maybe PlayReadyDrm)
  , _pFormat               :: !(Maybe Text)
  , _pOutputKeys           :: !(Maybe [Text])
  , _pName                 :: !(Maybe Text)
  , _pStatusDetail         :: !(Maybe Text)
  , _pHlsContentProtection :: !(Maybe HlsContentProtection)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Playlist' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pStatus' - The status of the job with which the playlist is associated.
--
-- * 'pPlayReadyDrm' - The DRM settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
--
-- * 'pFormat' - The format of the output playlist. Valid formats include @HLSv3@ , @HLSv4@ , and @Smooth@ .
--
-- * 'pOutputKeys' - For each output in this job that you want to include in a master playlist, the value of the Outputs:Key object.     * If your output is not @HLS@ or does not have a segment duration set, the name of the output file is a concatenation of @OutputKeyPrefix@ and @Outputs:Key@ : OutputKeyPrefix@Outputs:Key@      * If your output is @HLSv3@ and has a segment duration set, or is not included in a playlist, Elastic Transcoder creates an output playlist file with a file extension of @.m3u8@ , and a series of @.ts@ files that include a five-digit sequential counter beginning with 00000: OutputKeyPrefix@Outputs:Key@ .m3u8 OutputKeyPrefix@Outputs:Key@ 00000.ts     * If your output is @HLSv4@ , has a segment duration set, and is included in an @HLSv4@ playlist, Elastic Transcoder creates an output playlist file with a file extension of @_v4.m3u8@ . If the output is video, Elastic Transcoder also creates an output file with an extension of @_iframe.m3u8@ : OutputKeyPrefix@Outputs:Key@ _v4.m3u8 OutputKeyPrefix@Outputs:Key@ _iframe.m3u8 OutputKeyPrefix@Outputs:Key@ .ts Elastic Transcoder automatically appends the relevant file extension to the file name. If you include a file extension in Output Key, the file name will have two extensions. If you include more than one output in a playlist, any segment duration settings, clip settings, or caption settings must be the same for all outputs in the playlist. For @Smooth@ playlists, the @Audio:Profile@ , @Video:Profile@ , and @Video:FrameRate@ to @Video:KeyframesMaxDist@ ratio must be the same for all outputs.
--
-- * 'pName' - The name that you want Elastic Transcoder to assign to the master playlist, for example, nyc-vacation.m3u8. If the name includes a @/@ character, the section of the name before the last @/@ must be identical for all @Name@ objects. If you create more than one master playlist, the values of all @Name@ objects must be unique.
--
-- * 'pStatusDetail' - Information that further explains the status.
--
-- * 'pHlsContentProtection' - The HLS content protection settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
playlist
    :: Playlist
playlist =
  Playlist'
    { _pStatus = Nothing
    , _pPlayReadyDrm = Nothing
    , _pFormat = Nothing
    , _pOutputKeys = Nothing
    , _pName = Nothing
    , _pStatusDetail = Nothing
    , _pHlsContentProtection = Nothing
    }


-- | The status of the job with which the playlist is associated.
pStatus :: Lens' Playlist (Maybe Text)
pStatus = lens _pStatus (\ s a -> s{_pStatus = a})

-- | The DRM settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
pPlayReadyDrm :: Lens' Playlist (Maybe PlayReadyDrm)
pPlayReadyDrm = lens _pPlayReadyDrm (\ s a -> s{_pPlayReadyDrm = a})

-- | The format of the output playlist. Valid formats include @HLSv3@ , @HLSv4@ , and @Smooth@ .
pFormat :: Lens' Playlist (Maybe Text)
pFormat = lens _pFormat (\ s a -> s{_pFormat = a})

-- | For each output in this job that you want to include in a master playlist, the value of the Outputs:Key object.     * If your output is not @HLS@ or does not have a segment duration set, the name of the output file is a concatenation of @OutputKeyPrefix@ and @Outputs:Key@ : OutputKeyPrefix@Outputs:Key@      * If your output is @HLSv3@ and has a segment duration set, or is not included in a playlist, Elastic Transcoder creates an output playlist file with a file extension of @.m3u8@ , and a series of @.ts@ files that include a five-digit sequential counter beginning with 00000: OutputKeyPrefix@Outputs:Key@ .m3u8 OutputKeyPrefix@Outputs:Key@ 00000.ts     * If your output is @HLSv4@ , has a segment duration set, and is included in an @HLSv4@ playlist, Elastic Transcoder creates an output playlist file with a file extension of @_v4.m3u8@ . If the output is video, Elastic Transcoder also creates an output file with an extension of @_iframe.m3u8@ : OutputKeyPrefix@Outputs:Key@ _v4.m3u8 OutputKeyPrefix@Outputs:Key@ _iframe.m3u8 OutputKeyPrefix@Outputs:Key@ .ts Elastic Transcoder automatically appends the relevant file extension to the file name. If you include a file extension in Output Key, the file name will have two extensions. If you include more than one output in a playlist, any segment duration settings, clip settings, or caption settings must be the same for all outputs in the playlist. For @Smooth@ playlists, the @Audio:Profile@ , @Video:Profile@ , and @Video:FrameRate@ to @Video:KeyframesMaxDist@ ratio must be the same for all outputs.
pOutputKeys :: Lens' Playlist [Text]
pOutputKeys = lens _pOutputKeys (\ s a -> s{_pOutputKeys = a}) . _Default . _Coerce

-- | The name that you want Elastic Transcoder to assign to the master playlist, for example, nyc-vacation.m3u8. If the name includes a @/@ character, the section of the name before the last @/@ must be identical for all @Name@ objects. If you create more than one master playlist, the values of all @Name@ objects must be unique.
pName :: Lens' Playlist (Maybe Text)
pName = lens _pName (\ s a -> s{_pName = a})

-- | Information that further explains the status.
pStatusDetail :: Lens' Playlist (Maybe Text)
pStatusDetail = lens _pStatusDetail (\ s a -> s{_pStatusDetail = a})

-- | The HLS content protection settings, if any, that you want Elastic Transcoder to apply to the output files associated with this playlist.
pHlsContentProtection :: Lens' Playlist (Maybe HlsContentProtection)
pHlsContentProtection = lens _pHlsContentProtection (\ s a -> s{_pHlsContentProtection = a})

instance FromJSON Playlist where
        parseJSON
          = withObject "Playlist"
              (\ x ->
                 Playlist' <$>
                   (x .:? "Status") <*> (x .:? "PlayReadyDrm") <*>
                     (x .:? "Format")
                     <*> (x .:? "OutputKeys" .!= mempty)
                     <*> (x .:? "Name")
                     <*> (x .:? "StatusDetail")
                     <*> (x .:? "HlsContentProtection"))

instance Hashable Playlist where

instance NFData Playlist where

-- | Presets are templates that contain most of the settings for transcoding media files from one format to another. Elastic Transcoder includes some default presets for common formats, for example, several iPod and iPhone versions. You can also create your own presets for formats that aren't included among the default presets. You specify which preset you want to use when you create a job.
--
--
--
-- /See:/ 'preset' smart constructor.
data Preset = Preset'
  { _preARN         :: !(Maybe Text)
  , _preVideo       :: !(Maybe VideoParameters)
  , _preThumbnails  :: !(Maybe Thumbnails)
  , _preName        :: !(Maybe Text)
  , _preContainer   :: !(Maybe Text)
  , _preId          :: !(Maybe Text)
  , _preType        :: !(Maybe Text)
  , _preDescription :: !(Maybe Text)
  , _preAudio       :: !(Maybe AudioParameters)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Preset' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'preARN' - The Amazon Resource Name (ARN) for the preset.
--
-- * 'preVideo' - A section of the response body that provides information about the video preset values.
--
-- * 'preThumbnails' - A section of the response body that provides information about the thumbnail preset values, if any.
--
-- * 'preName' - The name of the preset.
--
-- * 'preContainer' - The container type for the output file. Valid values include @flac@ , @flv@ , @fmp4@ , @gif@ , @mp3@ , @mp4@ , @mpg@ , @mxf@ , @oga@ , @ogg@ , @ts@ , and @webm@ .
--
-- * 'preId' - Identifier for the new preset. You use this value to get settings for the preset or to delete it.
--
-- * 'preType' - Whether the preset is a default preset provided by Elastic Transcoder (@System@ ) or a preset that you have defined (@Custom@ ).
--
-- * 'preDescription' - A description of the preset.
--
-- * 'preAudio' - A section of the response body that provides information about the audio preset values.
preset
    :: Preset
preset =
  Preset'
    { _preARN = Nothing
    , _preVideo = Nothing
    , _preThumbnails = Nothing
    , _preName = Nothing
    , _preContainer = Nothing
    , _preId = Nothing
    , _preType = Nothing
    , _preDescription = Nothing
    , _preAudio = Nothing
    }


-- | The Amazon Resource Name (ARN) for the preset.
preARN :: Lens' Preset (Maybe Text)
preARN = lens _preARN (\ s a -> s{_preARN = a})

-- | A section of the response body that provides information about the video preset values.
preVideo :: Lens' Preset (Maybe VideoParameters)
preVideo = lens _preVideo (\ s a -> s{_preVideo = a})

-- | A section of the response body that provides information about the thumbnail preset values, if any.
preThumbnails :: Lens' Preset (Maybe Thumbnails)
preThumbnails = lens _preThumbnails (\ s a -> s{_preThumbnails = a})

-- | The name of the preset.
preName :: Lens' Preset (Maybe Text)
preName = lens _preName (\ s a -> s{_preName = a})

-- | The container type for the output file. Valid values include @flac@ , @flv@ , @fmp4@ , @gif@ , @mp3@ , @mp4@ , @mpg@ , @mxf@ , @oga@ , @ogg@ , @ts@ , and @webm@ .
preContainer :: Lens' Preset (Maybe Text)
preContainer = lens _preContainer (\ s a -> s{_preContainer = a})

-- | Identifier for the new preset. You use this value to get settings for the preset or to delete it.
preId :: Lens' Preset (Maybe Text)
preId = lens _preId (\ s a -> s{_preId = a})

-- | Whether the preset is a default preset provided by Elastic Transcoder (@System@ ) or a preset that you have defined (@Custom@ ).
preType :: Lens' Preset (Maybe Text)
preType = lens _preType (\ s a -> s{_preType = a})

-- | A description of the preset.
preDescription :: Lens' Preset (Maybe Text)
preDescription = lens _preDescription (\ s a -> s{_preDescription = a})

-- | A section of the response body that provides information about the audio preset values.
preAudio :: Lens' Preset (Maybe AudioParameters)
preAudio = lens _preAudio (\ s a -> s{_preAudio = a})

instance FromJSON Preset where
        parseJSON
          = withObject "Preset"
              (\ x ->
                 Preset' <$>
                   (x .:? "Arn") <*> (x .:? "Video") <*>
                     (x .:? "Thumbnails")
                     <*> (x .:? "Name")
                     <*> (x .:? "Container")
                     <*> (x .:? "Id")
                     <*> (x .:? "Type")
                     <*> (x .:? "Description")
                     <*> (x .:? "Audio"))

instance Hashable Preset where

instance NFData Preset where

-- | Settings for the size, location, and opacity of graphics that you want Elastic Transcoder to overlay over videos that are transcoded using this preset. You can specify settings for up to four watermarks. Watermarks appear in the specified size and location, and with the specified opacity for the duration of the transcoded video.
--
--
-- Watermarks can be in .png or .jpg format. If you want to display a watermark that is not rectangular, use the .png format, which supports transparency.
--
-- When you create a job that uses this preset, you specify the .png or .jpg graphics that you want Elastic Transcoder to include in the transcoded videos. You can specify fewer graphics in the job than you specify watermark settings in the preset, which allows you to use the same preset for up to four watermarks that have different dimensions.
--
--
-- /See:/ 'presetWatermark' smart constructor.
data PresetWatermark = PresetWatermark'
  { _pwVerticalAlign    :: !(Maybe Text)
  , _pwSizingPolicy     :: !(Maybe Text)
  , _pwHorizontalOffset :: !(Maybe Text)
  , _pwMaxHeight        :: !(Maybe Text)
  , _pwOpacity          :: !(Maybe Text)
  , _pwVerticalOffset   :: !(Maybe Text)
  , _pwMaxWidth         :: !(Maybe Text)
  , _pwId               :: !(Maybe Text)
  , _pwHorizontalAlign  :: !(Maybe Text)
  , _pwTarget           :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PresetWatermark' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pwVerticalAlign' - The vertical position of the watermark unless you specify a non-zero value for @VerticalOffset@ :      * __Top__ : The top edge of the watermark is aligned with the top border of the video.     * __Bottom__ : The bottom edge of the watermark is aligned with the bottom border of the video.     * __Center__ : The watermark is centered between the top and bottom borders.
--
-- * 'pwSizingPolicy' - A value that controls scaling of the watermark:      * __Fit__ : Elastic Transcoder scales the watermark so it matches the value that you specified in either @MaxWidth@ or @MaxHeight@ without exceeding the other value.     * __Stretch__ : Elastic Transcoder stretches the watermark to match the values that you specified for @MaxWidth@ and @MaxHeight@ . If the relative proportions of the watermark and the values of @MaxWidth@ and @MaxHeight@ are different, the watermark will be distorted.     * __ShrinkToFit__ : Elastic Transcoder scales the watermark down so that its dimensions match the values that you specified for at least one of @MaxWidth@ and @MaxHeight@ without exceeding either value. If you specify this option, Elastic Transcoder does not scale the watermark up.
--
-- * 'pwHorizontalOffset' - The amount by which you want the horizontal position of the watermark to be offset from the position specified by HorizontalAlign:      * number of pixels (px): The minimum value is 0 pixels, and the maximum value is the value of MaxWidth.     * integer percentage (%): The range of valid values is 0 to 100. For example, if you specify Left for @HorizontalAlign@ and 5px for @HorizontalOffset@ , the left side of the watermark appears 5 pixels from the left border of the output video. @HorizontalOffset@ is only valid when the value of @HorizontalAlign@ is @Left@ or @Right@ . If you specify an offset that causes the watermark to extend beyond the left or right border and Elastic Transcoder has not added black bars, the watermark is cropped. If Elastic Transcoder has added black bars, the watermark extends into the black bars. If the watermark extends beyond the black bars, it is cropped. Use the value of @Target@ to specify whether you want to include the black bars that are added by Elastic Transcoder, if any, in the offset calculation.
--
-- * 'pwMaxHeight' - The maximum height of the watermark in one of the following formats:      * number of pixels (px): The minimum value is 16 pixels, and the maximum value is the value of @MaxHeight@ .     * integer percentage (%): The range of valid values is 0 to 100. Use the value of @Target@ to specify whether you want Elastic Transcoder to include the black bars that are added by Elastic Transcoder, if any, in the calculation. If you specify the value in pixels, it must be less than or equal to the value of @MaxHeight@ .
--
-- * 'pwOpacity' - A percentage that indicates how much you want a watermark to obscure the video in the location where it appears. Valid values are 0 (the watermark is invisible) to 100 (the watermark completely obscures the video in the specified location). The datatype of @Opacity@ is float. Elastic Transcoder supports transparent .png graphics. If you use a transparent .png, the transparent portion of the video appears as if you had specified a value of 0 for @Opacity@ . The .jpg file format doesn't support transparency.
--
-- * 'pwVerticalOffset' - @VerticalOffset@  The amount by which you want the vertical position of the watermark to be offset from the position specified by VerticalAlign:     * number of pixels (px): The minimum value is 0 pixels, and the maximum value is the value of @MaxHeight@ .     * integer percentage (%): The range of valid values is 0 to 100. For example, if you specify @Top@ for @VerticalAlign@ and @5px@ for @VerticalOffset@ , the top of the watermark appears 5 pixels from the top border of the output video. @VerticalOffset@ is only valid when the value of VerticalAlign is Top or Bottom. If you specify an offset that causes the watermark to extend beyond the top or bottom border and Elastic Transcoder has not added black bars, the watermark is cropped. If Elastic Transcoder has added black bars, the watermark extends into the black bars. If the watermark extends beyond the black bars, it is cropped. Use the value of @Target@ to specify whether you want Elastic Transcoder to include the black bars that are added by Elastic Transcoder, if any, in the offset calculation.
--
-- * 'pwMaxWidth' - The maximum width of the watermark in one of the following formats:      * number of pixels (px): The minimum value is 16 pixels, and the maximum value is the value of @MaxWidth@ .     * integer percentage (%): The range of valid values is 0 to 100. Use the value of @Target@ to specify whether you want Elastic Transcoder to include the black bars that are added by Elastic Transcoder, if any, in the calculation. If you specify the value in pixels, it must be less than or equal to the value of @MaxWidth@ .
--
-- * 'pwId' - A unique identifier for the settings for one watermark. The value of @Id@ can be up to 40 characters long.
--
-- * 'pwHorizontalAlign' - The horizontal position of the watermark unless you specify a non-zero value for @HorizontalOffset@ :      * __Left__ : The left edge of the watermark is aligned with the left border of the video.     * __Right__ : The right edge of the watermark is aligned with the right border of the video.     * __Center__ : The watermark is centered between the left and right borders.
--
-- * 'pwTarget' - A value that determines how Elastic Transcoder interprets values that you specified for @HorizontalOffset@ , @VerticalOffset@ , @MaxWidth@ , and @MaxHeight@ :     * __Content__ : @HorizontalOffset@ and @VerticalOffset@ values are calculated based on the borders of the video excluding black bars added by Elastic Transcoder, if any. In addition, @MaxWidth@ and @MaxHeight@ , if specified as a percentage, are calculated based on the borders of the video excluding black bars added by Elastic Transcoder, if any.     * __Frame__ : @HorizontalOffset@ and @VerticalOffset@ values are calculated based on the borders of the video including black bars added by Elastic Transcoder, if any. In addition, @MaxWidth@ and @MaxHeight@ , if specified as a percentage, are calculated based on the borders of the video including black bars added by Elastic Transcoder, if any.
presetWatermark
    :: PresetWatermark
presetWatermark =
  PresetWatermark'
    { _pwVerticalAlign = Nothing
    , _pwSizingPolicy = Nothing
    , _pwHorizontalOffset = Nothing
    , _pwMaxHeight = Nothing
    , _pwOpacity = Nothing
    , _pwVerticalOffset = Nothing
    , _pwMaxWidth = Nothing
    , _pwId = Nothing
    , _pwHorizontalAlign = Nothing
    , _pwTarget = Nothing
    }


-- | The vertical position of the watermark unless you specify a non-zero value for @VerticalOffset@ :      * __Top__ : The top edge of the watermark is aligned with the top border of the video.     * __Bottom__ : The bottom edge of the watermark is aligned with the bottom border of the video.     * __Center__ : The watermark is centered between the top and bottom borders.
pwVerticalAlign :: Lens' PresetWatermark (Maybe Text)
pwVerticalAlign = lens _pwVerticalAlign (\ s a -> s{_pwVerticalAlign = a})

-- | A value that controls scaling of the watermark:      * __Fit__ : Elastic Transcoder scales the watermark so it matches the value that you specified in either @MaxWidth@ or @MaxHeight@ without exceeding the other value.     * __Stretch__ : Elastic Transcoder stretches the watermark to match the values that you specified for @MaxWidth@ and @MaxHeight@ . If the relative proportions of the watermark and the values of @MaxWidth@ and @MaxHeight@ are different, the watermark will be distorted.     * __ShrinkToFit__ : Elastic Transcoder scales the watermark down so that its dimensions match the values that you specified for at least one of @MaxWidth@ and @MaxHeight@ without exceeding either value. If you specify this option, Elastic Transcoder does not scale the watermark up.
pwSizingPolicy :: Lens' PresetWatermark (Maybe Text)
pwSizingPolicy = lens _pwSizingPolicy (\ s a -> s{_pwSizingPolicy = a})

-- | The amount by which you want the horizontal position of the watermark to be offset from the position specified by HorizontalAlign:      * number of pixels (px): The minimum value is 0 pixels, and the maximum value is the value of MaxWidth.     * integer percentage (%): The range of valid values is 0 to 100. For example, if you specify Left for @HorizontalAlign@ and 5px for @HorizontalOffset@ , the left side of the watermark appears 5 pixels from the left border of the output video. @HorizontalOffset@ is only valid when the value of @HorizontalAlign@ is @Left@ or @Right@ . If you specify an offset that causes the watermark to extend beyond the left or right border and Elastic Transcoder has not added black bars, the watermark is cropped. If Elastic Transcoder has added black bars, the watermark extends into the black bars. If the watermark extends beyond the black bars, it is cropped. Use the value of @Target@ to specify whether you want to include the black bars that are added by Elastic Transcoder, if any, in the offset calculation.
pwHorizontalOffset :: Lens' PresetWatermark (Maybe Text)
pwHorizontalOffset = lens _pwHorizontalOffset (\ s a -> s{_pwHorizontalOffset = a})

-- | The maximum height of the watermark in one of the following formats:      * number of pixels (px): The minimum value is 16 pixels, and the maximum value is the value of @MaxHeight@ .     * integer percentage (%): The range of valid values is 0 to 100. Use the value of @Target@ to specify whether you want Elastic Transcoder to include the black bars that are added by Elastic Transcoder, if any, in the calculation. If you specify the value in pixels, it must be less than or equal to the value of @MaxHeight@ .
pwMaxHeight :: Lens' PresetWatermark (Maybe Text)
pwMaxHeight = lens _pwMaxHeight (\ s a -> s{_pwMaxHeight = a})

-- | A percentage that indicates how much you want a watermark to obscure the video in the location where it appears. Valid values are 0 (the watermark is invisible) to 100 (the watermark completely obscures the video in the specified location). The datatype of @Opacity@ is float. Elastic Transcoder supports transparent .png graphics. If you use a transparent .png, the transparent portion of the video appears as if you had specified a value of 0 for @Opacity@ . The .jpg file format doesn't support transparency.
pwOpacity :: Lens' PresetWatermark (Maybe Text)
pwOpacity = lens _pwOpacity (\ s a -> s{_pwOpacity = a})

-- | @VerticalOffset@  The amount by which you want the vertical position of the watermark to be offset from the position specified by VerticalAlign:     * number of pixels (px): The minimum value is 0 pixels, and the maximum value is the value of @MaxHeight@ .     * integer percentage (%): The range of valid values is 0 to 100. For example, if you specify @Top@ for @VerticalAlign@ and @5px@ for @VerticalOffset@ , the top of the watermark appears 5 pixels from the top border of the output video. @VerticalOffset@ is only valid when the value of VerticalAlign is Top or Bottom. If you specify an offset that causes the watermark to extend beyond the top or bottom border and Elastic Transcoder has not added black bars, the watermark is cropped. If Elastic Transcoder has added black bars, the watermark extends into the black bars. If the watermark extends beyond the black bars, it is cropped. Use the value of @Target@ to specify whether you want Elastic Transcoder to include the black bars that are added by Elastic Transcoder, if any, in the offset calculation.
pwVerticalOffset :: Lens' PresetWatermark (Maybe Text)
pwVerticalOffset = lens _pwVerticalOffset (\ s a -> s{_pwVerticalOffset = a})

-- | The maximum width of the watermark in one of the following formats:      * number of pixels (px): The minimum value is 16 pixels, and the maximum value is the value of @MaxWidth@ .     * integer percentage (%): The range of valid values is 0 to 100. Use the value of @Target@ to specify whether you want Elastic Transcoder to include the black bars that are added by Elastic Transcoder, if any, in the calculation. If you specify the value in pixels, it must be less than or equal to the value of @MaxWidth@ .
pwMaxWidth :: Lens' PresetWatermark (Maybe Text)
pwMaxWidth = lens _pwMaxWidth (\ s a -> s{_pwMaxWidth = a})

-- | A unique identifier for the settings for one watermark. The value of @Id@ can be up to 40 characters long.
pwId :: Lens' PresetWatermark (Maybe Text)
pwId = lens _pwId (\ s a -> s{_pwId = a})

-- | The horizontal position of the watermark unless you specify a non-zero value for @HorizontalOffset@ :      * __Left__ : The left edge of the watermark is aligned with the left border of the video.     * __Right__ : The right edge of the watermark is aligned with the right border of the video.     * __Center__ : The watermark is centered between the left and right borders.
pwHorizontalAlign :: Lens' PresetWatermark (Maybe Text)
pwHorizontalAlign = lens _pwHorizontalAlign (\ s a -> s{_pwHorizontalAlign = a})

-- | A value that determines how Elastic Transcoder interprets values that you specified for @HorizontalOffset@ , @VerticalOffset@ , @MaxWidth@ , and @MaxHeight@ :     * __Content__ : @HorizontalOffset@ and @VerticalOffset@ values are calculated based on the borders of the video excluding black bars added by Elastic Transcoder, if any. In addition, @MaxWidth@ and @MaxHeight@ , if specified as a percentage, are calculated based on the borders of the video excluding black bars added by Elastic Transcoder, if any.     * __Frame__ : @HorizontalOffset@ and @VerticalOffset@ values are calculated based on the borders of the video including black bars added by Elastic Transcoder, if any. In addition, @MaxWidth@ and @MaxHeight@ , if specified as a percentage, are calculated based on the borders of the video including black bars added by Elastic Transcoder, if any.
pwTarget :: Lens' PresetWatermark (Maybe Text)
pwTarget = lens _pwTarget (\ s a -> s{_pwTarget = a})

instance FromJSON PresetWatermark where
        parseJSON
          = withObject "PresetWatermark"
              (\ x ->
                 PresetWatermark' <$>
                   (x .:? "VerticalAlign") <*> (x .:? "SizingPolicy")
                     <*> (x .:? "HorizontalOffset")
                     <*> (x .:? "MaxHeight")
                     <*> (x .:? "Opacity")
                     <*> (x .:? "VerticalOffset")
                     <*> (x .:? "MaxWidth")
                     <*> (x .:? "Id")
                     <*> (x .:? "HorizontalAlign")
                     <*> (x .:? "Target"))

instance Hashable PresetWatermark where

instance NFData PresetWatermark where

instance ToJSON PresetWatermark where
        toJSON PresetWatermark'{..}
          = object
              (catMaybes
                 [("VerticalAlign" .=) <$> _pwVerticalAlign,
                  ("SizingPolicy" .=) <$> _pwSizingPolicy,
                  ("HorizontalOffset" .=) <$> _pwHorizontalOffset,
                  ("MaxHeight" .=) <$> _pwMaxHeight,
                  ("Opacity" .=) <$> _pwOpacity,
                  ("VerticalOffset" .=) <$> _pwVerticalOffset,
                  ("MaxWidth" .=) <$> _pwMaxWidth, ("Id" .=) <$> _pwId,
                  ("HorizontalAlign" .=) <$> _pwHorizontalAlign,
                  ("Target" .=) <$> _pwTarget])

-- | Thumbnails for videos.
--
--
--
-- /See:/ 'thumbnails' smart constructor.
data Thumbnails = Thumbnails'
  { _tSizingPolicy  :: !(Maybe Text)
  , _tFormat        :: !(Maybe Text)
  , _tMaxHeight     :: !(Maybe Text)
  , _tResolution    :: !(Maybe Text)
  , _tAspectRatio   :: !(Maybe Text)
  , _tPaddingPolicy :: !(Maybe Text)
  , _tInterval      :: !(Maybe Text)
  , _tMaxWidth      :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Thumbnails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tSizingPolicy' - Specify one of the following values to control scaling of thumbnails:     * @Fit@ : Elastic Transcoder scales thumbnails so they match the value that you specified in thumbnail MaxWidth or MaxHeight settings without exceeding the other value.      * @Fill@ : Elastic Transcoder scales thumbnails so they match the value that you specified in thumbnail @MaxWidth@ or @MaxHeight@ settings and matches or exceeds the other value. Elastic Transcoder centers the image in thumbnails and then crops in the dimension (if any) that exceeds the maximum value.     * @Stretch@ : Elastic Transcoder stretches thumbnails to match the values that you specified for thumbnail @MaxWidth@ and @MaxHeight@ settings. If the relative proportions of the input video and thumbnails are different, the thumbnails will be distorted.     * @Keep@ : Elastic Transcoder does not scale thumbnails. If either dimension of the input video exceeds the values that you specified for thumbnail @MaxWidth@ and @MaxHeight@ settings, Elastic Transcoder crops the thumbnails.     * @ShrinkToFit@ : Elastic Transcoder scales thumbnails down so that their dimensions match the values that you specified for at least one of thumbnail @MaxWidth@ and @MaxHeight@ without exceeding either value. If you specify this option, Elastic Transcoder does not scale thumbnails up.     * @ShrinkToFill@ : Elastic Transcoder scales thumbnails down so that their dimensions match the values that you specified for at least one of @MaxWidth@ and @MaxHeight@ without dropping below either value. If you specify this option, Elastic Transcoder does not scale thumbnails up.
--
-- * 'tFormat' - The format of thumbnails, if any. Valid values are @jpg@ and @png@ .  You specify whether you want Elastic Transcoder to create thumbnails when you create a job.
--
-- * 'tMaxHeight' - The maximum height of thumbnails in pixels. If you specify auto, Elastic Transcoder uses 1080 (Full HD) as the default value. If you specify a numeric value, enter an even integer between 32 and 3072.
--
-- * 'tResolution' - /Important:/ To better control resolution and aspect ratio of thumbnails, we recommend that you use the values @MaxWidth@ , @MaxHeight@ , @SizingPolicy@ , and @PaddingPolicy@ instead of @Resolution@ and @AspectRatio@ . The two groups of settings are mutually exclusive. Do not use them together. The width and height of thumbnail files in pixels. Specify a value in the format @/width/ @ x @/height/ @ where both values are even integers. The values cannot exceed the width and height that you specified in the @Video:Resolution@ object.
--
-- * 'tAspectRatio' - /Important:/ To better control resolution and aspect ratio of thumbnails, we recommend that you use the values @MaxWidth@ , @MaxHeight@ , @SizingPolicy@ , and @PaddingPolicy@ instead of @Resolution@ and @AspectRatio@ . The two groups of settings are mutually exclusive. Do not use them together. The aspect ratio of thumbnails. Valid values include: @auto@ , @1:1@ , @4:3@ , @3:2@ , @16:9@  If you specify @auto@ , Elastic Transcoder tries to preserve the aspect ratio of the video in the output file.
--
-- * 'tPaddingPolicy' - When you set @PaddingPolicy@ to @Pad@ , Elastic Transcoder may add black bars to the top and bottom and/or left and right sides of thumbnails to make the total size of the thumbnails match the values that you specified for thumbnail @MaxWidth@ and @MaxHeight@ settings.
--
-- * 'tInterval' - The approximate number of seconds between thumbnails. Specify an integer value.
--
-- * 'tMaxWidth' - The maximum width of thumbnails in pixels. If you specify auto, Elastic Transcoder uses 1920 (Full HD) as the default value. If you specify a numeric value, enter an even integer between 32 and 4096.
thumbnails
    :: Thumbnails
thumbnails =
  Thumbnails'
    { _tSizingPolicy = Nothing
    , _tFormat = Nothing
    , _tMaxHeight = Nothing
    , _tResolution = Nothing
    , _tAspectRatio = Nothing
    , _tPaddingPolicy = Nothing
    , _tInterval = Nothing
    , _tMaxWidth = Nothing
    }


-- | Specify one of the following values to control scaling of thumbnails:     * @Fit@ : Elastic Transcoder scales thumbnails so they match the value that you specified in thumbnail MaxWidth or MaxHeight settings without exceeding the other value.      * @Fill@ : Elastic Transcoder scales thumbnails so they match the value that you specified in thumbnail @MaxWidth@ or @MaxHeight@ settings and matches or exceeds the other value. Elastic Transcoder centers the image in thumbnails and then crops in the dimension (if any) that exceeds the maximum value.     * @Stretch@ : Elastic Transcoder stretches thumbnails to match the values that you specified for thumbnail @MaxWidth@ and @MaxHeight@ settings. If the relative proportions of the input video and thumbnails are different, the thumbnails will be distorted.     * @Keep@ : Elastic Transcoder does not scale thumbnails. If either dimension of the input video exceeds the values that you specified for thumbnail @MaxWidth@ and @MaxHeight@ settings, Elastic Transcoder crops the thumbnails.     * @ShrinkToFit@ : Elastic Transcoder scales thumbnails down so that their dimensions match the values that you specified for at least one of thumbnail @MaxWidth@ and @MaxHeight@ without exceeding either value. If you specify this option, Elastic Transcoder does not scale thumbnails up.     * @ShrinkToFill@ : Elastic Transcoder scales thumbnails down so that their dimensions match the values that you specified for at least one of @MaxWidth@ and @MaxHeight@ without dropping below either value. If you specify this option, Elastic Transcoder does not scale thumbnails up.
tSizingPolicy :: Lens' Thumbnails (Maybe Text)
tSizingPolicy = lens _tSizingPolicy (\ s a -> s{_tSizingPolicy = a})

-- | The format of thumbnails, if any. Valid values are @jpg@ and @png@ .  You specify whether you want Elastic Transcoder to create thumbnails when you create a job.
tFormat :: Lens' Thumbnails (Maybe Text)
tFormat = lens _tFormat (\ s a -> s{_tFormat = a})

-- | The maximum height of thumbnails in pixels. If you specify auto, Elastic Transcoder uses 1080 (Full HD) as the default value. If you specify a numeric value, enter an even integer between 32 and 3072.
tMaxHeight :: Lens' Thumbnails (Maybe Text)
tMaxHeight = lens _tMaxHeight (\ s a -> s{_tMaxHeight = a})

-- | /Important:/ To better control resolution and aspect ratio of thumbnails, we recommend that you use the values @MaxWidth@ , @MaxHeight@ , @SizingPolicy@ , and @PaddingPolicy@ instead of @Resolution@ and @AspectRatio@ . The two groups of settings are mutually exclusive. Do not use them together. The width and height of thumbnail files in pixels. Specify a value in the format @/width/ @ x @/height/ @ where both values are even integers. The values cannot exceed the width and height that you specified in the @Video:Resolution@ object.
tResolution :: Lens' Thumbnails (Maybe Text)
tResolution = lens _tResolution (\ s a -> s{_tResolution = a})

-- | /Important:/ To better control resolution and aspect ratio of thumbnails, we recommend that you use the values @MaxWidth@ , @MaxHeight@ , @SizingPolicy@ , and @PaddingPolicy@ instead of @Resolution@ and @AspectRatio@ . The two groups of settings are mutually exclusive. Do not use them together. The aspect ratio of thumbnails. Valid values include: @auto@ , @1:1@ , @4:3@ , @3:2@ , @16:9@  If you specify @auto@ , Elastic Transcoder tries to preserve the aspect ratio of the video in the output file.
tAspectRatio :: Lens' Thumbnails (Maybe Text)
tAspectRatio = lens _tAspectRatio (\ s a -> s{_tAspectRatio = a})

-- | When you set @PaddingPolicy@ to @Pad@ , Elastic Transcoder may add black bars to the top and bottom and/or left and right sides of thumbnails to make the total size of the thumbnails match the values that you specified for thumbnail @MaxWidth@ and @MaxHeight@ settings.
tPaddingPolicy :: Lens' Thumbnails (Maybe Text)
tPaddingPolicy = lens _tPaddingPolicy (\ s a -> s{_tPaddingPolicy = a})

-- | The approximate number of seconds between thumbnails. Specify an integer value.
tInterval :: Lens' Thumbnails (Maybe Text)
tInterval = lens _tInterval (\ s a -> s{_tInterval = a})

-- | The maximum width of thumbnails in pixels. If you specify auto, Elastic Transcoder uses 1920 (Full HD) as the default value. If you specify a numeric value, enter an even integer between 32 and 4096.
tMaxWidth :: Lens' Thumbnails (Maybe Text)
tMaxWidth = lens _tMaxWidth (\ s a -> s{_tMaxWidth = a})

instance FromJSON Thumbnails where
        parseJSON
          = withObject "Thumbnails"
              (\ x ->
                 Thumbnails' <$>
                   (x .:? "SizingPolicy") <*> (x .:? "Format") <*>
                     (x .:? "MaxHeight")
                     <*> (x .:? "Resolution")
                     <*> (x .:? "AspectRatio")
                     <*> (x .:? "PaddingPolicy")
                     <*> (x .:? "Interval")
                     <*> (x .:? "MaxWidth"))

instance Hashable Thumbnails where

instance NFData Thumbnails where

instance ToJSON Thumbnails where
        toJSON Thumbnails'{..}
          = object
              (catMaybes
                 [("SizingPolicy" .=) <$> _tSizingPolicy,
                  ("Format" .=) <$> _tFormat,
                  ("MaxHeight" .=) <$> _tMaxHeight,
                  ("Resolution" .=) <$> _tResolution,
                  ("AspectRatio" .=) <$> _tAspectRatio,
                  ("PaddingPolicy" .=) <$> _tPaddingPolicy,
                  ("Interval" .=) <$> _tInterval,
                  ("MaxWidth" .=) <$> _tMaxWidth])

-- | Settings that determine when a clip begins and how long it lasts.
--
--
--
-- /See:/ 'timeSpan' smart constructor.
data TimeSpan = TimeSpan'
  { _tsStartTime :: !(Maybe Text)
  , _tsDuration  :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TimeSpan' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tsStartTime' - The place in the input file where you want a clip to start. The format can be either HH:mm:ss.SSS (maximum value: 23:59:59.999; SSS is thousandths of a second) or sssss.SSS (maximum value: 86399.999). If you don't specify a value, Elastic Transcoder starts at the beginning of the input file.
--
-- * 'tsDuration' - The duration of the clip. The format can be either HH:mm:ss.SSS (maximum value: 23:59:59.999; SSS is thousandths of a second) or sssss.SSS (maximum value: 86399.999). If you don't specify a value, Elastic Transcoder creates an output file from StartTime to the end of the file. If you specify a value longer than the duration of the input file, Elastic Transcoder transcodes the file and returns a warning message.
timeSpan
    :: TimeSpan
timeSpan = TimeSpan' {_tsStartTime = Nothing, _tsDuration = Nothing}


-- | The place in the input file where you want a clip to start. The format can be either HH:mm:ss.SSS (maximum value: 23:59:59.999; SSS is thousandths of a second) or sssss.SSS (maximum value: 86399.999). If you don't specify a value, Elastic Transcoder starts at the beginning of the input file.
tsStartTime :: Lens' TimeSpan (Maybe Text)
tsStartTime = lens _tsStartTime (\ s a -> s{_tsStartTime = a})

-- | The duration of the clip. The format can be either HH:mm:ss.SSS (maximum value: 23:59:59.999; SSS is thousandths of a second) or sssss.SSS (maximum value: 86399.999). If you don't specify a value, Elastic Transcoder creates an output file from StartTime to the end of the file. If you specify a value longer than the duration of the input file, Elastic Transcoder transcodes the file and returns a warning message.
tsDuration :: Lens' TimeSpan (Maybe Text)
tsDuration = lens _tsDuration (\ s a -> s{_tsDuration = a})

instance FromJSON TimeSpan where
        parseJSON
          = withObject "TimeSpan"
              (\ x ->
                 TimeSpan' <$>
                   (x .:? "StartTime") <*> (x .:? "Duration"))

instance Hashable TimeSpan where

instance NFData TimeSpan where

instance ToJSON TimeSpan where
        toJSON TimeSpan'{..}
          = object
              (catMaybes
                 [("StartTime" .=) <$> _tsStartTime,
                  ("Duration" .=) <$> _tsDuration])

-- | Details about the timing of a job.
--
--
--
-- /See:/ 'timing' smart constructor.
data Timing = Timing'
  { _tSubmitTimeMillis :: !(Maybe Integer)
  , _tFinishTimeMillis :: !(Maybe Integer)
  , _tStartTimeMillis  :: !(Maybe Integer)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Timing' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tSubmitTimeMillis' - The time the job was submitted to Elastic Transcoder, in epoch milliseconds.
--
-- * 'tFinishTimeMillis' - The time the job finished transcoding, in epoch milliseconds.
--
-- * 'tStartTimeMillis' - The time the job began transcoding, in epoch milliseconds.
timing
    :: Timing
timing =
  Timing'
    { _tSubmitTimeMillis = Nothing
    , _tFinishTimeMillis = Nothing
    , _tStartTimeMillis = Nothing
    }


-- | The time the job was submitted to Elastic Transcoder, in epoch milliseconds.
tSubmitTimeMillis :: Lens' Timing (Maybe Integer)
tSubmitTimeMillis = lens _tSubmitTimeMillis (\ s a -> s{_tSubmitTimeMillis = a})

-- | The time the job finished transcoding, in epoch milliseconds.
tFinishTimeMillis :: Lens' Timing (Maybe Integer)
tFinishTimeMillis = lens _tFinishTimeMillis (\ s a -> s{_tFinishTimeMillis = a})

-- | The time the job began transcoding, in epoch milliseconds.
tStartTimeMillis :: Lens' Timing (Maybe Integer)
tStartTimeMillis = lens _tStartTimeMillis (\ s a -> s{_tStartTimeMillis = a})

instance FromJSON Timing where
        parseJSON
          = withObject "Timing"
              (\ x ->
                 Timing' <$>
                   (x .:? "SubmitTimeMillis") <*>
                     (x .:? "FinishTimeMillis")
                     <*> (x .:? "StartTimeMillis"))

instance Hashable Timing where

instance NFData Timing where

-- | The @VideoParameters@ structure.
--
--
--
-- /See:/ 'videoParameters' smart constructor.
data VideoParameters = VideoParameters'
  { _vpKeyframesMaxDist   :: !(Maybe Text)
  , _vpFrameRate          :: !(Maybe Text)
  , _vpSizingPolicy       :: !(Maybe Text)
  , _vpMaxFrameRate       :: !(Maybe Text)
  , _vpMaxHeight          :: !(Maybe Text)
  , _vpWatermarks         :: !(Maybe [PresetWatermark])
  , _vpDisplayAspectRatio :: !(Maybe Text)
  , _vpResolution         :: !(Maybe Text)
  , _vpCodec              :: !(Maybe Text)
  , _vpAspectRatio        :: !(Maybe Text)
  , _vpPaddingPolicy      :: !(Maybe Text)
  , _vpMaxWidth           :: !(Maybe Text)
  , _vpBitRate            :: !(Maybe Text)
  , _vpFixedGOP           :: !(Maybe Text)
  , _vpCodecOptions       :: !(Maybe (Map Text Text))
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VideoParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vpKeyframesMaxDist' - Applicable only when the value of Video:Codec is one of @H.264@ , @MPEG2@ , or @VP8@ . The maximum number of frames between key frames. Key frames are fully encoded frames; the frames between key frames are encoded based, in part, on the content of the key frames. The value is an integer formatted as a string; valid values are between 1 (every frame is a key frame) and 100000, inclusive. A higher value results in higher compression but may also discernibly decrease video quality. For @Smooth@ outputs, the @FrameRate@ must have a constant ratio to the @KeyframesMaxDist@ . This allows @Smooth@ playlists to switch between different quality levels while the file is being played. For example, an input file can have a @FrameRate@ of 30 with a @KeyframesMaxDist@ of 90. The output file then needs to have a ratio of 1:3. Valid outputs would have @FrameRate@ of 30, 25, and 10, and @KeyframesMaxDist@ of 90, 75, and 30, respectively. Alternately, this can be achieved by setting @FrameRate@ to auto and having the same values for @MaxFrameRate@ and @KeyframesMaxDist@ .
--
-- * 'vpFrameRate' - The frames per second for the video stream in the output file. Valid values include: @auto@ , @10@ , @15@ , @23.97@ , @24@ , @25@ , @29.97@ , @30@ , @60@  If you specify @auto@ , Elastic Transcoder uses the detected frame rate of the input source. If you specify a frame rate, we recommend that you perform the following calculation: @Frame rate = maximum recommended decoding speed in luma samples/second / (width in pixels * height in pixels)@  where:     * /width in pixels/ and /height in pixels/ represent the Resolution of the output video.     * /maximum recommended decoding speed in Luma samples\/second/ is less than or equal to the maximum value listed in the following table, based on the value that you specified for Level. The maximum recommended decoding speed in Luma samples/second for each level is described in the following list (/Level - Decoding speed/ ):     * 1 - 380160     * 1b - 380160     * 1.1 - 76800     * 1.2 - 1536000     * 1.3 - 3041280     * 2 - 3041280     * 2.1 - 5068800     * 2.2 - 5184000     * 3 - 10368000     * 3.1 - 27648000     * 3.2 - 55296000     * 4 - 62914560     * 4.1 - 62914560
--
-- * 'vpSizingPolicy' - Specify one of the following values to control scaling of the output video:     * @Fit@ : Elastic Transcoder scales the output video so it matches the value that you specified in either @MaxWidth@ or @MaxHeight@ without exceeding the other value.     * @Fill@ : Elastic Transcoder scales the output video so it matches the value that you specified in either @MaxWidth@ or @MaxHeight@ and matches or exceeds the other value. Elastic Transcoder centers the output video and then crops it in the dimension (if any) that exceeds the maximum value.     * @Stretch@ : Elastic Transcoder stretches the output video to match the values that you specified for @MaxWidth@ and @MaxHeight@ . If the relative proportions of the input video and the output video are different, the output video will be distorted.     * @Keep@ : Elastic Transcoder does not scale the output video. If either dimension of the input video exceeds the values that you specified for @MaxWidth@ and @MaxHeight@ , Elastic Transcoder crops the output video.     * @ShrinkToFit@ : Elastic Transcoder scales the output video down so that its dimensions match the values that you specified for at least one of @MaxWidth@ and @MaxHeight@ without exceeding either value. If you specify this option, Elastic Transcoder does not scale the video up.     * @ShrinkToFill@ : Elastic Transcoder scales the output video down so that its dimensions match the values that you specified for at least one of @MaxWidth@ and @MaxHeight@ without dropping below either value. If you specify this option, Elastic Transcoder does not scale the video up.
--
-- * 'vpMaxFrameRate' - If you specify @auto@ for @FrameRate@ , Elastic Transcoder uses the frame rate of the input video for the frame rate of the output video. Specify the maximum frame rate that you want Elastic Transcoder to use when the frame rate of the input video is greater than the desired maximum frame rate of the output video. Valid values include: @10@ , @15@ , @23.97@ , @24@ , @25@ , @29.97@ , @30@ , @60@ .
--
-- * 'vpMaxHeight' - The maximum height of the output video in pixels. If you specify @auto@ , Elastic Transcoder uses 1080 (Full HD) as the default value. If you specify a numeric value, enter an even integer between 96 and 3072.
--
-- * 'vpWatermarks' - Settings for the size, location, and opacity of graphics that you want Elastic Transcoder to overlay over videos that are transcoded using this preset. You can specify settings for up to four watermarks. Watermarks appear in the specified size and location, and with the specified opacity for the duration of the transcoded video. Watermarks can be in .png or .jpg format. If you want to display a watermark that is not rectangular, use the .png format, which supports transparency. When you create a job that uses this preset, you specify the .png or .jpg graphics that you want Elastic Transcoder to include in the transcoded videos. You can specify fewer graphics in the job than you specify watermark settings in the preset, which allows you to use the same preset for up to four watermarks that have different dimensions.
--
-- * 'vpDisplayAspectRatio' - The value that Elastic Transcoder adds to the metadata in the output file.
--
-- * 'vpResolution' - /Important:/ To better control resolution and aspect ratio of output videos, we recommend that you use the values @MaxWidth@ , @MaxHeight@ , @SizingPolicy@ , @PaddingPolicy@ , and @DisplayAspectRatio@ instead of @Resolution@ and @AspectRatio@ . The two groups of settings are mutually exclusive. Do not use them together. The width and height of the video in the output file, in pixels. Valid values are @auto@ and /width/ x /height/ :     * @auto@ : Elastic Transcoder attempts to preserve the width and height of the input file, subject to the following rules.     * @/width/ x /height/ @ : The width and height of the output video in pixels. Note the following about specifying the width and height:     * The width must be an even integer between 128 and 4096, inclusive.     * The height must be an even integer between 96 and 3072, inclusive.     * If you specify a resolution that is less than the resolution of the input file, Elastic Transcoder rescales the output file to the lower resolution.     * If you specify a resolution that is greater than the resolution of the input file, Elastic Transcoder rescales the output to the higher resolution.     * We recommend that you specify a resolution for which the product of width and height is less than or equal to the applicable value in the following list (/List - Max width x height value/ ):     * 1 - 25344     * 1b - 25344     * 1.1 - 101376     * 1.2 - 101376     * 1.3 - 101376     * 2 - 101376     * 2.1 - 202752     * 2.2 - 404720     * 3 - 404720     * 3.1 - 921600     * 3.2 - 1310720     * 4 - 2097152     * 4.1 - 2097152
--
-- * 'vpCodec' - The video codec for the output file. Valid values include @gif@ , @H.264@ , @mpeg2@ , @vp8@ , and @vp9@ . You can only specify @vp8@ and @vp9@ when the container type is @webm@ , @gif@ when the container type is @gif@ , and @mpeg2@ when the container type is @mpg@ .
--
-- * 'vpAspectRatio' - /Important:/ To better control resolution and aspect ratio of output videos, we recommend that you use the values @MaxWidth@ , @MaxHeight@ , @SizingPolicy@ , @PaddingPolicy@ , and @DisplayAspectRatio@ instead of @Resolution@ and @AspectRatio@ . The two groups of settings are mutually exclusive. Do not use them together. The display aspect ratio of the video in the output file. Valid values include: @auto@ , @1:1@ , @4:3@ , @3:2@ , @16:9@  If you specify @auto@ , Elastic Transcoder tries to preserve the aspect ratio of the input file. If you specify an aspect ratio for the output file that differs from aspect ratio of the input file, Elastic Transcoder adds pillarboxing (black bars on the sides) or letterboxing (black bars on the top and bottom) to maintain the aspect ratio of the active region of the video.
--
-- * 'vpPaddingPolicy' - When you set @PaddingPolicy@ to @Pad@ , Elastic Transcoder may add black bars to the top and bottom and/or left and right sides of the output video to make the total size of the output video match the values that you specified for @MaxWidth@ and @MaxHeight@ .
--
-- * 'vpMaxWidth' - The maximum width of the output video in pixels. If you specify @auto@ , Elastic Transcoder uses 1920 (Full HD) as the default value. If you specify a numeric value, enter an even integer between 128 and 4096.
--
-- * 'vpBitRate' - The bit rate of the video stream in the output file, in kilobits/second. Valid values depend on the values of @Level@ and @Profile@ . If you specify @auto@ , Elastic Transcoder uses the detected bit rate of the input source. If you specify a value other than @auto@ , we recommend that you specify a value less than or equal to the maximum H.264-compliant value listed for your level and profile: /Level - Maximum video bit rate in kilobits\/second (baseline and main Profile) : maximum video bit rate in kilobits\/second (high Profile)/      * 1 - 64 : 80     * 1b - 128 : 160     * 1.1 - 192 : 240     * 1.2 - 384 : 480     * 1.3 - 768 : 960     * 2 - 2000 : 2500     * 3 - 10000 : 12500     * 3.1 - 14000 : 17500     * 3.2 - 20000 : 25000     * 4 - 20000 : 25000     * 4.1 - 50000 : 62500
--
-- * 'vpFixedGOP' - Applicable only when the value of Video:Codec is one of @H.264@ , @MPEG2@ , or @VP8@ . Whether to use a fixed value for @FixedGOP@ . Valid values are @true@ and @false@ :     * @true@ : Elastic Transcoder uses the value of @KeyframesMaxDist@ for the distance between key frames (the number of frames in a group of pictures, or GOP).     * @false@ : The distance between key frames can vary. /Important:/ @FixedGOP@ must be set to @true@ for @fmp4@ containers.
--
-- * 'vpCodecOptions' - __Profile (H.264/VP8/VP9 Only)__  The H.264 profile that you want to use for the output file. Elastic Transcoder supports the following profiles:     * @baseline@ : The profile most commonly used for videoconferencing and for mobile applications.     * @main@ : The profile used for standard-definition digital TV broadcasts.     * @high@ : The profile used for high-definition digital TV broadcasts and for Blu-ray discs. __Level (H.264 Only)__  The H.264 level that you want to use for the output file. Elastic Transcoder supports the following levels: @1@ , @1b@ , @1.1@ , @1.2@ , @1.3@ , @2@ , @2.1@ , @2.2@ , @3@ , @3.1@ , @3.2@ , @4@ , @4.1@  __MaxReferenceFrames (H.264 Only)__  Applicable only when the value of Video:Codec is H.264. The maximum number of previously decoded frames to use as a reference for decoding future frames. Valid values are integers 0 through 16, but we recommend that you not use a value greater than the following: @Min(Floor(Maximum decoded picture buffer in macroblocks * 256 / (Width in pixels * Height in pixels)), 16)@  where /Width in pixels/ and /Height in pixels/ represent either MaxWidth and MaxHeight, or Resolution. /Maximum decoded picture buffer in macroblocks/ depends on the value of the @Level@ object. See the list below. (A macroblock is a block of pixels measuring 16x16.)      * 1 - 396     * 1b - 396     * 1.1 - 900     * 1.2 - 2376     * 1.3 - 2376     * 2 - 2376     * 2.1 - 4752     * 2.2 - 8100     * 3 - 8100     * 3.1 - 18000     * 3.2 - 20480     * 4 - 32768     * 4.1 - 32768 __MaxBitRate (Optional, H.264/MPEG2/VP8/VP9 only)__  The maximum number of bits per second in a video buffer; the size of the buffer is specified by @BufferSize@ . Specify a value between 16 and 62,500. You can reduce the bandwidth required to stream a video by reducing the maximum bit rate, but this also reduces the quality of the video. __BufferSize (Optional, H.264/MPEG2/VP8/VP9 only)__  The maximum number of bits in any x seconds of the output video. This window is commonly 10 seconds, the standard segment duration when you're using FMP4 or MPEG-TS for the container type of the output video. Specify an integer greater than 0. If you specify @MaxBitRate@ and omit @BufferSize@ , Elastic Transcoder sets @BufferSize@ to 10 times the value of @MaxBitRate@ . __InterlacedMode (Optional, H.264/MPEG2 Only)__  The interlace mode for the output video. Interlaced video is used to double the perceived frame rate for a video by interlacing two fields (one field on every other line, the other field on the other lines) so that the human eye registers multiple pictures per frame. Interlacing reduces the bandwidth required for transmitting a video, but can result in blurred images and flickering. Valid values include @Progressive@ (no interlacing, top to bottom), @TopFirst@ (top field first), @BottomFirst@ (bottom field first), and @Auto@ . If @InterlaceMode@ is not specified, Elastic Transcoder uses @Progressive@ for the output. If @Auto@ is specified, Elastic Transcoder interlaces the output. __ColorSpaceConversionMode (Optional, H.264/MPEG2 Only)__  The color space conversion Elastic Transcoder applies to the output video. Color spaces are the algorithms used by the computer to store information about how to render color. @Bt.601@ is the standard for standard definition video, while @Bt.709@ is the standard for high definition video. Valid values include @None@ , @Bt709toBt601@ , @Bt601toBt709@ , and @Auto@ . If you chose @Auto@ for @ColorSpaceConversionMode@ and your output is interlaced, your frame rate is one of @23.97@ , @24@ , @25@ , @29.97@ , @50@ , or @60@ , your @SegmentDuration@ is null, and you are using one of the resolution changes from the list below, Elastic Transcoder applies the following color space conversions:     * /Standard to HD, 720x480 to 1920x1080/ - Elastic Transcoder applies @Bt601ToBt709@      * /Standard to HD, 720x576 to 1920x1080/ - Elastic Transcoder applies @Bt601ToBt709@      * /HD to Standard, 1920x1080 to 720x480/ - Elastic Transcoder applies @Bt709ToBt601@      * /HD to Standard, 1920x1080 to 720x576/ - Elastic Transcoder applies @Bt709ToBt601@  If you do not specify a @ColorSpaceConversionMode@ , Elastic Transcoder does not change the color space of a file. If you are unsure what @ColorSpaceConversionMode@ was applied to your output file, you can check the @AppliedColorSpaceConversion@ parameter included in your job response. If your job does not have an @AppliedColorSpaceConversion@ in its response, no @ColorSpaceConversionMode@ was applied. __ChromaSubsampling__  The sampling pattern for the chroma (color) channels of the output video. Valid values include @yuv420p@ and @yuv422p@ . @yuv420p@ samples the chroma information of every other horizontal and every other vertical line, @yuv422p@ samples the color information of every horizontal line and every other vertical line. __LoopCount (Gif Only)__  The number of times you want the output gif to loop. Valid values include @Infinite@ and integers between @0@ and @100@ , inclusive.
videoParameters
    :: VideoParameters
videoParameters =
  VideoParameters'
    { _vpKeyframesMaxDist = Nothing
    , _vpFrameRate = Nothing
    , _vpSizingPolicy = Nothing
    , _vpMaxFrameRate = Nothing
    , _vpMaxHeight = Nothing
    , _vpWatermarks = Nothing
    , _vpDisplayAspectRatio = Nothing
    , _vpResolution = Nothing
    , _vpCodec = Nothing
    , _vpAspectRatio = Nothing
    , _vpPaddingPolicy = Nothing
    , _vpMaxWidth = Nothing
    , _vpBitRate = Nothing
    , _vpFixedGOP = Nothing
    , _vpCodecOptions = Nothing
    }


-- | Applicable only when the value of Video:Codec is one of @H.264@ , @MPEG2@ , or @VP8@ . The maximum number of frames between key frames. Key frames are fully encoded frames; the frames between key frames are encoded based, in part, on the content of the key frames. The value is an integer formatted as a string; valid values are between 1 (every frame is a key frame) and 100000, inclusive. A higher value results in higher compression but may also discernibly decrease video quality. For @Smooth@ outputs, the @FrameRate@ must have a constant ratio to the @KeyframesMaxDist@ . This allows @Smooth@ playlists to switch between different quality levels while the file is being played. For example, an input file can have a @FrameRate@ of 30 with a @KeyframesMaxDist@ of 90. The output file then needs to have a ratio of 1:3. Valid outputs would have @FrameRate@ of 30, 25, and 10, and @KeyframesMaxDist@ of 90, 75, and 30, respectively. Alternately, this can be achieved by setting @FrameRate@ to auto and having the same values for @MaxFrameRate@ and @KeyframesMaxDist@ .
vpKeyframesMaxDist :: Lens' VideoParameters (Maybe Text)
vpKeyframesMaxDist = lens _vpKeyframesMaxDist (\ s a -> s{_vpKeyframesMaxDist = a})

-- | The frames per second for the video stream in the output file. Valid values include: @auto@ , @10@ , @15@ , @23.97@ , @24@ , @25@ , @29.97@ , @30@ , @60@  If you specify @auto@ , Elastic Transcoder uses the detected frame rate of the input source. If you specify a frame rate, we recommend that you perform the following calculation: @Frame rate = maximum recommended decoding speed in luma samples/second / (width in pixels * height in pixels)@  where:     * /width in pixels/ and /height in pixels/ represent the Resolution of the output video.     * /maximum recommended decoding speed in Luma samples\/second/ is less than or equal to the maximum value listed in the following table, based on the value that you specified for Level. The maximum recommended decoding speed in Luma samples/second for each level is described in the following list (/Level - Decoding speed/ ):     * 1 - 380160     * 1b - 380160     * 1.1 - 76800     * 1.2 - 1536000     * 1.3 - 3041280     * 2 - 3041280     * 2.1 - 5068800     * 2.2 - 5184000     * 3 - 10368000     * 3.1 - 27648000     * 3.2 - 55296000     * 4 - 62914560     * 4.1 - 62914560
vpFrameRate :: Lens' VideoParameters (Maybe Text)
vpFrameRate = lens _vpFrameRate (\ s a -> s{_vpFrameRate = a})

-- | Specify one of the following values to control scaling of the output video:     * @Fit@ : Elastic Transcoder scales the output video so it matches the value that you specified in either @MaxWidth@ or @MaxHeight@ without exceeding the other value.     * @Fill@ : Elastic Transcoder scales the output video so it matches the value that you specified in either @MaxWidth@ or @MaxHeight@ and matches or exceeds the other value. Elastic Transcoder centers the output video and then crops it in the dimension (if any) that exceeds the maximum value.     * @Stretch@ : Elastic Transcoder stretches the output video to match the values that you specified for @MaxWidth@ and @MaxHeight@ . If the relative proportions of the input video and the output video are different, the output video will be distorted.     * @Keep@ : Elastic Transcoder does not scale the output video. If either dimension of the input video exceeds the values that you specified for @MaxWidth@ and @MaxHeight@ , Elastic Transcoder crops the output video.     * @ShrinkToFit@ : Elastic Transcoder scales the output video down so that its dimensions match the values that you specified for at least one of @MaxWidth@ and @MaxHeight@ without exceeding either value. If you specify this option, Elastic Transcoder does not scale the video up.     * @ShrinkToFill@ : Elastic Transcoder scales the output video down so that its dimensions match the values that you specified for at least one of @MaxWidth@ and @MaxHeight@ without dropping below either value. If you specify this option, Elastic Transcoder does not scale the video up.
vpSizingPolicy :: Lens' VideoParameters (Maybe Text)
vpSizingPolicy = lens _vpSizingPolicy (\ s a -> s{_vpSizingPolicy = a})

-- | If you specify @auto@ for @FrameRate@ , Elastic Transcoder uses the frame rate of the input video for the frame rate of the output video. Specify the maximum frame rate that you want Elastic Transcoder to use when the frame rate of the input video is greater than the desired maximum frame rate of the output video. Valid values include: @10@ , @15@ , @23.97@ , @24@ , @25@ , @29.97@ , @30@ , @60@ .
vpMaxFrameRate :: Lens' VideoParameters (Maybe Text)
vpMaxFrameRate = lens _vpMaxFrameRate (\ s a -> s{_vpMaxFrameRate = a})

-- | The maximum height of the output video in pixels. If you specify @auto@ , Elastic Transcoder uses 1080 (Full HD) as the default value. If you specify a numeric value, enter an even integer between 96 and 3072.
vpMaxHeight :: Lens' VideoParameters (Maybe Text)
vpMaxHeight = lens _vpMaxHeight (\ s a -> s{_vpMaxHeight = a})

-- | Settings for the size, location, and opacity of graphics that you want Elastic Transcoder to overlay over videos that are transcoded using this preset. You can specify settings for up to four watermarks. Watermarks appear in the specified size and location, and with the specified opacity for the duration of the transcoded video. Watermarks can be in .png or .jpg format. If you want to display a watermark that is not rectangular, use the .png format, which supports transparency. When you create a job that uses this preset, you specify the .png or .jpg graphics that you want Elastic Transcoder to include in the transcoded videos. You can specify fewer graphics in the job than you specify watermark settings in the preset, which allows you to use the same preset for up to four watermarks that have different dimensions.
vpWatermarks :: Lens' VideoParameters [PresetWatermark]
vpWatermarks = lens _vpWatermarks (\ s a -> s{_vpWatermarks = a}) . _Default . _Coerce

-- | The value that Elastic Transcoder adds to the metadata in the output file.
vpDisplayAspectRatio :: Lens' VideoParameters (Maybe Text)
vpDisplayAspectRatio = lens _vpDisplayAspectRatio (\ s a -> s{_vpDisplayAspectRatio = a})

-- | /Important:/ To better control resolution and aspect ratio of output videos, we recommend that you use the values @MaxWidth@ , @MaxHeight@ , @SizingPolicy@ , @PaddingPolicy@ , and @DisplayAspectRatio@ instead of @Resolution@ and @AspectRatio@ . The two groups of settings are mutually exclusive. Do not use them together. The width and height of the video in the output file, in pixels. Valid values are @auto@ and /width/ x /height/ :     * @auto@ : Elastic Transcoder attempts to preserve the width and height of the input file, subject to the following rules.     * @/width/ x /height/ @ : The width and height of the output video in pixels. Note the following about specifying the width and height:     * The width must be an even integer between 128 and 4096, inclusive.     * The height must be an even integer between 96 and 3072, inclusive.     * If you specify a resolution that is less than the resolution of the input file, Elastic Transcoder rescales the output file to the lower resolution.     * If you specify a resolution that is greater than the resolution of the input file, Elastic Transcoder rescales the output to the higher resolution.     * We recommend that you specify a resolution for which the product of width and height is less than or equal to the applicable value in the following list (/List - Max width x height value/ ):     * 1 - 25344     * 1b - 25344     * 1.1 - 101376     * 1.2 - 101376     * 1.3 - 101376     * 2 - 101376     * 2.1 - 202752     * 2.2 - 404720     * 3 - 404720     * 3.1 - 921600     * 3.2 - 1310720     * 4 - 2097152     * 4.1 - 2097152
vpResolution :: Lens' VideoParameters (Maybe Text)
vpResolution = lens _vpResolution (\ s a -> s{_vpResolution = a})

-- | The video codec for the output file. Valid values include @gif@ , @H.264@ , @mpeg2@ , @vp8@ , and @vp9@ . You can only specify @vp8@ and @vp9@ when the container type is @webm@ , @gif@ when the container type is @gif@ , and @mpeg2@ when the container type is @mpg@ .
vpCodec :: Lens' VideoParameters (Maybe Text)
vpCodec = lens _vpCodec (\ s a -> s{_vpCodec = a})

-- | /Important:/ To better control resolution and aspect ratio of output videos, we recommend that you use the values @MaxWidth@ , @MaxHeight@ , @SizingPolicy@ , @PaddingPolicy@ , and @DisplayAspectRatio@ instead of @Resolution@ and @AspectRatio@ . The two groups of settings are mutually exclusive. Do not use them together. The display aspect ratio of the video in the output file. Valid values include: @auto@ , @1:1@ , @4:3@ , @3:2@ , @16:9@  If you specify @auto@ , Elastic Transcoder tries to preserve the aspect ratio of the input file. If you specify an aspect ratio for the output file that differs from aspect ratio of the input file, Elastic Transcoder adds pillarboxing (black bars on the sides) or letterboxing (black bars on the top and bottom) to maintain the aspect ratio of the active region of the video.
vpAspectRatio :: Lens' VideoParameters (Maybe Text)
vpAspectRatio = lens _vpAspectRatio (\ s a -> s{_vpAspectRatio = a})

-- | When you set @PaddingPolicy@ to @Pad@ , Elastic Transcoder may add black bars to the top and bottom and/or left and right sides of the output video to make the total size of the output video match the values that you specified for @MaxWidth@ and @MaxHeight@ .
vpPaddingPolicy :: Lens' VideoParameters (Maybe Text)
vpPaddingPolicy = lens _vpPaddingPolicy (\ s a -> s{_vpPaddingPolicy = a})

-- | The maximum width of the output video in pixels. If you specify @auto@ , Elastic Transcoder uses 1920 (Full HD) as the default value. If you specify a numeric value, enter an even integer between 128 and 4096.
vpMaxWidth :: Lens' VideoParameters (Maybe Text)
vpMaxWidth = lens _vpMaxWidth (\ s a -> s{_vpMaxWidth = a})

-- | The bit rate of the video stream in the output file, in kilobits/second. Valid values depend on the values of @Level@ and @Profile@ . If you specify @auto@ , Elastic Transcoder uses the detected bit rate of the input source. If you specify a value other than @auto@ , we recommend that you specify a value less than or equal to the maximum H.264-compliant value listed for your level and profile: /Level - Maximum video bit rate in kilobits\/second (baseline and main Profile) : maximum video bit rate in kilobits\/second (high Profile)/      * 1 - 64 : 80     * 1b - 128 : 160     * 1.1 - 192 : 240     * 1.2 - 384 : 480     * 1.3 - 768 : 960     * 2 - 2000 : 2500     * 3 - 10000 : 12500     * 3.1 - 14000 : 17500     * 3.2 - 20000 : 25000     * 4 - 20000 : 25000     * 4.1 - 50000 : 62500
vpBitRate :: Lens' VideoParameters (Maybe Text)
vpBitRate = lens _vpBitRate (\ s a -> s{_vpBitRate = a})

-- | Applicable only when the value of Video:Codec is one of @H.264@ , @MPEG2@ , or @VP8@ . Whether to use a fixed value for @FixedGOP@ . Valid values are @true@ and @false@ :     * @true@ : Elastic Transcoder uses the value of @KeyframesMaxDist@ for the distance between key frames (the number of frames in a group of pictures, or GOP).     * @false@ : The distance between key frames can vary. /Important:/ @FixedGOP@ must be set to @true@ for @fmp4@ containers.
vpFixedGOP :: Lens' VideoParameters (Maybe Text)
vpFixedGOP = lens _vpFixedGOP (\ s a -> s{_vpFixedGOP = a})

-- | __Profile (H.264/VP8/VP9 Only)__  The H.264 profile that you want to use for the output file. Elastic Transcoder supports the following profiles:     * @baseline@ : The profile most commonly used for videoconferencing and for mobile applications.     * @main@ : The profile used for standard-definition digital TV broadcasts.     * @high@ : The profile used for high-definition digital TV broadcasts and for Blu-ray discs. __Level (H.264 Only)__  The H.264 level that you want to use for the output file. Elastic Transcoder supports the following levels: @1@ , @1b@ , @1.1@ , @1.2@ , @1.3@ , @2@ , @2.1@ , @2.2@ , @3@ , @3.1@ , @3.2@ , @4@ , @4.1@  __MaxReferenceFrames (H.264 Only)__  Applicable only when the value of Video:Codec is H.264. The maximum number of previously decoded frames to use as a reference for decoding future frames. Valid values are integers 0 through 16, but we recommend that you not use a value greater than the following: @Min(Floor(Maximum decoded picture buffer in macroblocks * 256 / (Width in pixels * Height in pixels)), 16)@  where /Width in pixels/ and /Height in pixels/ represent either MaxWidth and MaxHeight, or Resolution. /Maximum decoded picture buffer in macroblocks/ depends on the value of the @Level@ object. See the list below. (A macroblock is a block of pixels measuring 16x16.)      * 1 - 396     * 1b - 396     * 1.1 - 900     * 1.2 - 2376     * 1.3 - 2376     * 2 - 2376     * 2.1 - 4752     * 2.2 - 8100     * 3 - 8100     * 3.1 - 18000     * 3.2 - 20480     * 4 - 32768     * 4.1 - 32768 __MaxBitRate (Optional, H.264/MPEG2/VP8/VP9 only)__  The maximum number of bits per second in a video buffer; the size of the buffer is specified by @BufferSize@ . Specify a value between 16 and 62,500. You can reduce the bandwidth required to stream a video by reducing the maximum bit rate, but this also reduces the quality of the video. __BufferSize (Optional, H.264/MPEG2/VP8/VP9 only)__  The maximum number of bits in any x seconds of the output video. This window is commonly 10 seconds, the standard segment duration when you're using FMP4 or MPEG-TS for the container type of the output video. Specify an integer greater than 0. If you specify @MaxBitRate@ and omit @BufferSize@ , Elastic Transcoder sets @BufferSize@ to 10 times the value of @MaxBitRate@ . __InterlacedMode (Optional, H.264/MPEG2 Only)__  The interlace mode for the output video. Interlaced video is used to double the perceived frame rate for a video by interlacing two fields (one field on every other line, the other field on the other lines) so that the human eye registers multiple pictures per frame. Interlacing reduces the bandwidth required for transmitting a video, but can result in blurred images and flickering. Valid values include @Progressive@ (no interlacing, top to bottom), @TopFirst@ (top field first), @BottomFirst@ (bottom field first), and @Auto@ . If @InterlaceMode@ is not specified, Elastic Transcoder uses @Progressive@ for the output. If @Auto@ is specified, Elastic Transcoder interlaces the output. __ColorSpaceConversionMode (Optional, H.264/MPEG2 Only)__  The color space conversion Elastic Transcoder applies to the output video. Color spaces are the algorithms used by the computer to store information about how to render color. @Bt.601@ is the standard for standard definition video, while @Bt.709@ is the standard for high definition video. Valid values include @None@ , @Bt709toBt601@ , @Bt601toBt709@ , and @Auto@ . If you chose @Auto@ for @ColorSpaceConversionMode@ and your output is interlaced, your frame rate is one of @23.97@ , @24@ , @25@ , @29.97@ , @50@ , or @60@ , your @SegmentDuration@ is null, and you are using one of the resolution changes from the list below, Elastic Transcoder applies the following color space conversions:     * /Standard to HD, 720x480 to 1920x1080/ - Elastic Transcoder applies @Bt601ToBt709@      * /Standard to HD, 720x576 to 1920x1080/ - Elastic Transcoder applies @Bt601ToBt709@      * /HD to Standard, 1920x1080 to 720x480/ - Elastic Transcoder applies @Bt709ToBt601@      * /HD to Standard, 1920x1080 to 720x576/ - Elastic Transcoder applies @Bt709ToBt601@  If you do not specify a @ColorSpaceConversionMode@ , Elastic Transcoder does not change the color space of a file. If you are unsure what @ColorSpaceConversionMode@ was applied to your output file, you can check the @AppliedColorSpaceConversion@ parameter included in your job response. If your job does not have an @AppliedColorSpaceConversion@ in its response, no @ColorSpaceConversionMode@ was applied. __ChromaSubsampling__  The sampling pattern for the chroma (color) channels of the output video. Valid values include @yuv420p@ and @yuv422p@ . @yuv420p@ samples the chroma information of every other horizontal and every other vertical line, @yuv422p@ samples the color information of every horizontal line and every other vertical line. __LoopCount (Gif Only)__  The number of times you want the output gif to loop. Valid values include @Infinite@ and integers between @0@ and @100@ , inclusive.
vpCodecOptions :: Lens' VideoParameters (HashMap Text Text)
vpCodecOptions = lens _vpCodecOptions (\ s a -> s{_vpCodecOptions = a}) . _Default . _Map

instance FromJSON VideoParameters where
        parseJSON
          = withObject "VideoParameters"
              (\ x ->
                 VideoParameters' <$>
                   (x .:? "KeyframesMaxDist") <*> (x .:? "FrameRate")
                     <*> (x .:? "SizingPolicy")
                     <*> (x .:? "MaxFrameRate")
                     <*> (x .:? "MaxHeight")
                     <*> (x .:? "Watermarks" .!= mempty)
                     <*> (x .:? "DisplayAspectRatio")
                     <*> (x .:? "Resolution")
                     <*> (x .:? "Codec")
                     <*> (x .:? "AspectRatio")
                     <*> (x .:? "PaddingPolicy")
                     <*> (x .:? "MaxWidth")
                     <*> (x .:? "BitRate")
                     <*> (x .:? "FixedGOP")
                     <*> (x .:? "CodecOptions" .!= mempty))

instance Hashable VideoParameters where

instance NFData VideoParameters where

instance ToJSON VideoParameters where
        toJSON VideoParameters'{..}
          = object
              (catMaybes
                 [("KeyframesMaxDist" .=) <$> _vpKeyframesMaxDist,
                  ("FrameRate" .=) <$> _vpFrameRate,
                  ("SizingPolicy" .=) <$> _vpSizingPolicy,
                  ("MaxFrameRate" .=) <$> _vpMaxFrameRate,
                  ("MaxHeight" .=) <$> _vpMaxHeight,
                  ("Watermarks" .=) <$> _vpWatermarks,
                  ("DisplayAspectRatio" .=) <$> _vpDisplayAspectRatio,
                  ("Resolution" .=) <$> _vpResolution,
                  ("Codec" .=) <$> _vpCodec,
                  ("AspectRatio" .=) <$> _vpAspectRatio,
                  ("PaddingPolicy" .=) <$> _vpPaddingPolicy,
                  ("MaxWidth" .=) <$> _vpMaxWidth,
                  ("BitRate" .=) <$> _vpBitRate,
                  ("FixedGOP" .=) <$> _vpFixedGOP,
                  ("CodecOptions" .=) <$> _vpCodecOptions])

-- | Elastic Transcoder returns a warning if the resources used by your pipeline are not in the same region as the pipeline.
--
--
-- Using resources in the same region, such as your Amazon S3 buckets, Amazon SNS notification topics, and AWS KMS key, reduces processing time and prevents cross-regional charges.
--
--
-- /See:/ 'warning' smart constructor.
data Warning = Warning'
  { _wCode    :: !(Maybe Text)
  , _wMessage :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Warning' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wCode' - The code of the cross-regional warning.
--
-- * 'wMessage' - The message explaining what resources are in a different region from the pipeline.
warning
    :: Warning
warning = Warning' {_wCode = Nothing, _wMessage = Nothing}


-- | The code of the cross-regional warning.
wCode :: Lens' Warning (Maybe Text)
wCode = lens _wCode (\ s a -> s{_wCode = a})

-- | The message explaining what resources are in a different region from the pipeline.
wMessage :: Lens' Warning (Maybe Text)
wMessage = lens _wMessage (\ s a -> s{_wMessage = a})

instance FromJSON Warning where
        parseJSON
          = withObject "Warning"
              (\ x ->
                 Warning' <$> (x .:? "Code") <*> (x .:? "Message"))

instance Hashable Warning where

instance NFData Warning where
