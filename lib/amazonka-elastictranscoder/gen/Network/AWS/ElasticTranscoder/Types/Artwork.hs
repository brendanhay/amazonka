-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Artwork
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.Artwork
  ( Artwork (..),

    -- * Smart constructor
    mkArtwork,

    -- * Lenses
    aSizingPolicy,
    aAlbumArtFormat,
    aMaxHeight,
    aInputKey,
    aPaddingPolicy,
    aEncryption,
    aMaxWidth,
  )
where

import Network.AWS.ElasticTranscoder.Types.Encryption
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The file to be used as album art. There can be multiple artworks associated with an audio file, to a maximum of 20.
--
-- To remove artwork or leave the artwork empty, you can either set @Artwork@ to null, or set the @Merge Policy@ to "Replace" and use an empty @Artwork@ array.
-- To pass through existing artwork unchanged, set the @Merge Policy@ to "Prepend", "Append", or "Fallback", and use an empty @Artwork@ array.
--
-- /See:/ 'mkArtwork' smart constructor.
data Artwork = Artwork'
  { sizingPolicy :: Lude.Maybe Lude.Text,
    albumArtFormat :: Lude.Maybe Lude.Text,
    maxHeight :: Lude.Maybe Lude.Text,
    inputKey :: Lude.Maybe Lude.Text,
    paddingPolicy :: Lude.Maybe Lude.Text,
    encryption :: Lude.Maybe Encryption,
    maxWidth :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Artwork' with the minimum fields required to make a request.
--
-- * 'albumArtFormat' - The format of album art, if any. Valid formats are @.jpg@ and @.png@ .
-- * 'encryption' - The encryption settings, if any, that you want Elastic Transcoder to apply to your artwork.
-- * 'inputKey' - The name of the file to be used as album art. To determine which Amazon S3 bucket contains the specified file, Elastic Transcoder checks the pipeline specified by @PipelineId@ ; the @InputBucket@ object in that pipeline identifies the bucket.
--
-- If the file name includes a prefix, for example, @cooking/pie.jpg@ , include the prefix in the key. If the file isn't in the specified bucket, Elastic Transcoder returns an error.
-- * 'maxHeight' - The maximum height of the output album art in pixels. If you specify @auto@ , Elastic Transcoder uses 600 as the default value. If you specify a numeric value, enter an even integer between 32 and 3072, inclusive.
-- * 'maxWidth' - The maximum width of the output album art in pixels. If you specify @auto@ , Elastic Transcoder uses 600 as the default value. If you specify a numeric value, enter an even integer between 32 and 4096, inclusive.
-- * 'paddingPolicy' - When you set @PaddingPolicy@ to @Pad@ , Elastic Transcoder may add white bars to the top and bottom and/or left and right sides of the output album art to make the total size of the output art match the values that you specified for @MaxWidth@ and @MaxHeight@ .
-- * 'sizingPolicy' - Specify one of the following values to control scaling of the output album art:
--
--
--     * @Fit:@ Elastic Transcoder scales the output art so it matches the value that you specified in either @MaxWidth@ or @MaxHeight@ without exceeding the other value.
--
--
--     * @Fill:@ Elastic Transcoder scales the output art so it matches the value that you specified in either @MaxWidth@ or @MaxHeight@ and matches or exceeds the other value. Elastic Transcoder centers the output art and then crops it in the dimension (if any) that exceeds the maximum value.
--
--
--     * @Stretch:@ Elastic Transcoder stretches the output art to match the values that you specified for @MaxWidth@ and @MaxHeight@ . If the relative proportions of the input art and the output art are different, the output art will be distorted.
--
--
--     * @Keep:@ Elastic Transcoder does not scale the output art. If either dimension of the input art exceeds the values that you specified for @MaxWidth@ and @MaxHeight@ , Elastic Transcoder crops the output art.
--
--
--     * @ShrinkToFit:@ Elastic Transcoder scales the output art down so that its dimensions match the values that you specified for at least one of @MaxWidth@ and @MaxHeight@ without exceeding either value. If you specify this option, Elastic Transcoder does not scale the art up.
--
--
--     * @ShrinkToFill@ Elastic Transcoder scales the output art down so that its dimensions match the values that you specified for at least one of @MaxWidth@ and @MaxHeight@ without dropping below either value. If you specify this option, Elastic Transcoder does not scale the art up.
mkArtwork ::
  Artwork
mkArtwork =
  Artwork'
    { sizingPolicy = Lude.Nothing,
      albumArtFormat = Lude.Nothing,
      maxHeight = Lude.Nothing,
      inputKey = Lude.Nothing,
      paddingPolicy = Lude.Nothing,
      encryption = Lude.Nothing,
      maxWidth = Lude.Nothing
    }

-- | Specify one of the following values to control scaling of the output album art:
--
--
--     * @Fit:@ Elastic Transcoder scales the output art so it matches the value that you specified in either @MaxWidth@ or @MaxHeight@ without exceeding the other value.
--
--
--     * @Fill:@ Elastic Transcoder scales the output art so it matches the value that you specified in either @MaxWidth@ or @MaxHeight@ and matches or exceeds the other value. Elastic Transcoder centers the output art and then crops it in the dimension (if any) that exceeds the maximum value.
--
--
--     * @Stretch:@ Elastic Transcoder stretches the output art to match the values that you specified for @MaxWidth@ and @MaxHeight@ . If the relative proportions of the input art and the output art are different, the output art will be distorted.
--
--
--     * @Keep:@ Elastic Transcoder does not scale the output art. If either dimension of the input art exceeds the values that you specified for @MaxWidth@ and @MaxHeight@ , Elastic Transcoder crops the output art.
--
--
--     * @ShrinkToFit:@ Elastic Transcoder scales the output art down so that its dimensions match the values that you specified for at least one of @MaxWidth@ and @MaxHeight@ without exceeding either value. If you specify this option, Elastic Transcoder does not scale the art up.
--
--
--     * @ShrinkToFill@ Elastic Transcoder scales the output art down so that its dimensions match the values that you specified for at least one of @MaxWidth@ and @MaxHeight@ without dropping below either value. If you specify this option, Elastic Transcoder does not scale the art up.
--
--
--
-- /Note:/ Consider using 'sizingPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aSizingPolicy :: Lens.Lens' Artwork (Lude.Maybe Lude.Text)
aSizingPolicy = Lens.lens (sizingPolicy :: Artwork -> Lude.Maybe Lude.Text) (\s a -> s {sizingPolicy = a} :: Artwork)
{-# DEPRECATED aSizingPolicy "Use generic-lens or generic-optics with 'sizingPolicy' instead." #-}

-- | The format of album art, if any. Valid formats are @.jpg@ and @.png@ .
--
-- /Note:/ Consider using 'albumArtFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAlbumArtFormat :: Lens.Lens' Artwork (Lude.Maybe Lude.Text)
aAlbumArtFormat = Lens.lens (albumArtFormat :: Artwork -> Lude.Maybe Lude.Text) (\s a -> s {albumArtFormat = a} :: Artwork)
{-# DEPRECATED aAlbumArtFormat "Use generic-lens or generic-optics with 'albumArtFormat' instead." #-}

-- | The maximum height of the output album art in pixels. If you specify @auto@ , Elastic Transcoder uses 600 as the default value. If you specify a numeric value, enter an even integer between 32 and 3072, inclusive.
--
-- /Note:/ Consider using 'maxHeight' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aMaxHeight :: Lens.Lens' Artwork (Lude.Maybe Lude.Text)
aMaxHeight = Lens.lens (maxHeight :: Artwork -> Lude.Maybe Lude.Text) (\s a -> s {maxHeight = a} :: Artwork)
{-# DEPRECATED aMaxHeight "Use generic-lens or generic-optics with 'maxHeight' instead." #-}

-- | The name of the file to be used as album art. To determine which Amazon S3 bucket contains the specified file, Elastic Transcoder checks the pipeline specified by @PipelineId@ ; the @InputBucket@ object in that pipeline identifies the bucket.
--
-- If the file name includes a prefix, for example, @cooking/pie.jpg@ , include the prefix in the key. If the file isn't in the specified bucket, Elastic Transcoder returns an error.
--
-- /Note:/ Consider using 'inputKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aInputKey :: Lens.Lens' Artwork (Lude.Maybe Lude.Text)
aInputKey = Lens.lens (inputKey :: Artwork -> Lude.Maybe Lude.Text) (\s a -> s {inputKey = a} :: Artwork)
{-# DEPRECATED aInputKey "Use generic-lens or generic-optics with 'inputKey' instead." #-}

-- | When you set @PaddingPolicy@ to @Pad@ , Elastic Transcoder may add white bars to the top and bottom and/or left and right sides of the output album art to make the total size of the output art match the values that you specified for @MaxWidth@ and @MaxHeight@ .
--
-- /Note:/ Consider using 'paddingPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPaddingPolicy :: Lens.Lens' Artwork (Lude.Maybe Lude.Text)
aPaddingPolicy = Lens.lens (paddingPolicy :: Artwork -> Lude.Maybe Lude.Text) (\s a -> s {paddingPolicy = a} :: Artwork)
{-# DEPRECATED aPaddingPolicy "Use generic-lens or generic-optics with 'paddingPolicy' instead." #-}

-- | The encryption settings, if any, that you want Elastic Transcoder to apply to your artwork.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aEncryption :: Lens.Lens' Artwork (Lude.Maybe Encryption)
aEncryption = Lens.lens (encryption :: Artwork -> Lude.Maybe Encryption) (\s a -> s {encryption = a} :: Artwork)
{-# DEPRECATED aEncryption "Use generic-lens or generic-optics with 'encryption' instead." #-}

-- | The maximum width of the output album art in pixels. If you specify @auto@ , Elastic Transcoder uses 600 as the default value. If you specify a numeric value, enter an even integer between 32 and 4096, inclusive.
--
-- /Note:/ Consider using 'maxWidth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aMaxWidth :: Lens.Lens' Artwork (Lude.Maybe Lude.Text)
aMaxWidth = Lens.lens (maxWidth :: Artwork -> Lude.Maybe Lude.Text) (\s a -> s {maxWidth = a} :: Artwork)
{-# DEPRECATED aMaxWidth "Use generic-lens or generic-optics with 'maxWidth' instead." #-}

instance Lude.FromJSON Artwork where
  parseJSON =
    Lude.withObject
      "Artwork"
      ( \x ->
          Artwork'
            Lude.<$> (x Lude..:? "SizingPolicy")
            Lude.<*> (x Lude..:? "AlbumArtFormat")
            Lude.<*> (x Lude..:? "MaxHeight")
            Lude.<*> (x Lude..:? "InputKey")
            Lude.<*> (x Lude..:? "PaddingPolicy")
            Lude.<*> (x Lude..:? "Encryption")
            Lude.<*> (x Lude..:? "MaxWidth")
      )

instance Lude.ToJSON Artwork where
  toJSON Artwork' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SizingPolicy" Lude..=) Lude.<$> sizingPolicy,
            ("AlbumArtFormat" Lude..=) Lude.<$> albumArtFormat,
            ("MaxHeight" Lude..=) Lude.<$> maxHeight,
            ("InputKey" Lude..=) Lude.<$> inputKey,
            ("PaddingPolicy" Lude..=) Lude.<$> paddingPolicy,
            ("Encryption" Lude..=) Lude.<$> encryption,
            ("MaxWidth" Lude..=) Lude.<$> maxWidth
          ]
      )
