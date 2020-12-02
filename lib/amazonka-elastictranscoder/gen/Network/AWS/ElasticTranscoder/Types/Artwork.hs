{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Artwork
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.Artwork where

import Network.AWS.ElasticTranscoder.Types.Encryption
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
  { _aSizingPolicy :: !(Maybe Text),
    _aAlbumArtFormat :: !(Maybe Text),
    _aMaxHeight :: !(Maybe Text),
    _aInputKey :: !(Maybe Text),
    _aPaddingPolicy :: !(Maybe Text),
    _aEncryption :: !(Maybe Encryption),
    _aMaxWidth :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

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
artwork ::
  Artwork
artwork =
  Artwork'
    { _aSizingPolicy = Nothing,
      _aAlbumArtFormat = Nothing,
      _aMaxHeight = Nothing,
      _aInputKey = Nothing,
      _aPaddingPolicy = Nothing,
      _aEncryption = Nothing,
      _aMaxWidth = Nothing
    }

-- | Specify one of the following values to control scaling of the output album art:     * @Fit:@ Elastic Transcoder scales the output art so it matches the value that you specified in either @MaxWidth@ or @MaxHeight@ without exceeding the other value.     * @Fill:@ Elastic Transcoder scales the output art so it matches the value that you specified in either @MaxWidth@ or @MaxHeight@ and matches or exceeds the other value. Elastic Transcoder centers the output art and then crops it in the dimension (if any) that exceeds the maximum value.      * @Stretch:@ Elastic Transcoder stretches the output art to match the values that you specified for @MaxWidth@ and @MaxHeight@ . If the relative proportions of the input art and the output art are different, the output art will be distorted.     * @Keep:@ Elastic Transcoder does not scale the output art. If either dimension of the input art exceeds the values that you specified for @MaxWidth@ and @MaxHeight@ , Elastic Transcoder crops the output art.     * @ShrinkToFit:@ Elastic Transcoder scales the output art down so that its dimensions match the values that you specified for at least one of @MaxWidth@ and @MaxHeight@ without exceeding either value. If you specify this option, Elastic Transcoder does not scale the art up.     * @ShrinkToFill@ Elastic Transcoder scales the output art down so that its dimensions match the values that you specified for at least one of @MaxWidth@ and @MaxHeight@ without dropping below either value. If you specify this option, Elastic Transcoder does not scale the art up.
aSizingPolicy :: Lens' Artwork (Maybe Text)
aSizingPolicy = lens _aSizingPolicy (\s a -> s {_aSizingPolicy = a})

-- | The format of album art, if any. Valid formats are @.jpg@ and @.png@ .
aAlbumArtFormat :: Lens' Artwork (Maybe Text)
aAlbumArtFormat = lens _aAlbumArtFormat (\s a -> s {_aAlbumArtFormat = a})

-- | The maximum height of the output album art in pixels. If you specify @auto@ , Elastic Transcoder uses 600 as the default value. If you specify a numeric value, enter an even integer between 32 and 3072, inclusive.
aMaxHeight :: Lens' Artwork (Maybe Text)
aMaxHeight = lens _aMaxHeight (\s a -> s {_aMaxHeight = a})

-- | The name of the file to be used as album art. To determine which Amazon S3 bucket contains the specified file, Elastic Transcoder checks the pipeline specified by @PipelineId@ ; the @InputBucket@ object in that pipeline identifies the bucket. If the file name includes a prefix, for example, @cooking/pie.jpg@ , include the prefix in the key. If the file isn't in the specified bucket, Elastic Transcoder returns an error.
aInputKey :: Lens' Artwork (Maybe Text)
aInputKey = lens _aInputKey (\s a -> s {_aInputKey = a})

-- | When you set @PaddingPolicy@ to @Pad@ , Elastic Transcoder may add white bars to the top and bottom and/or left and right sides of the output album art to make the total size of the output art match the values that you specified for @MaxWidth@ and @MaxHeight@ .
aPaddingPolicy :: Lens' Artwork (Maybe Text)
aPaddingPolicy = lens _aPaddingPolicy (\s a -> s {_aPaddingPolicy = a})

-- | The encryption settings, if any, that you want Elastic Transcoder to apply to your artwork.
aEncryption :: Lens' Artwork (Maybe Encryption)
aEncryption = lens _aEncryption (\s a -> s {_aEncryption = a})

-- | The maximum width of the output album art in pixels. If you specify @auto@ , Elastic Transcoder uses 600 as the default value. If you specify a numeric value, enter an even integer between 32 and 4096, inclusive.
aMaxWidth :: Lens' Artwork (Maybe Text)
aMaxWidth = lens _aMaxWidth (\s a -> s {_aMaxWidth = a})

instance FromJSON Artwork where
  parseJSON =
    withObject
      "Artwork"
      ( \x ->
          Artwork'
            <$> (x .:? "SizingPolicy")
            <*> (x .:? "AlbumArtFormat")
            <*> (x .:? "MaxHeight")
            <*> (x .:? "InputKey")
            <*> (x .:? "PaddingPolicy")
            <*> (x .:? "Encryption")
            <*> (x .:? "MaxWidth")
      )

instance Hashable Artwork

instance NFData Artwork

instance ToJSON Artwork where
  toJSON Artwork' {..} =
    object
      ( catMaybes
          [ ("SizingPolicy" .=) <$> _aSizingPolicy,
            ("AlbumArtFormat" .=) <$> _aAlbumArtFormat,
            ("MaxHeight" .=) <$> _aMaxHeight,
            ("InputKey" .=) <$> _aInputKey,
            ("PaddingPolicy" .=) <$> _aPaddingPolicy,
            ("Encryption" .=) <$> _aEncryption,
            ("MaxWidth" .=) <$> _aMaxWidth
          ]
      )
