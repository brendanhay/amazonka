{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    aAlbumArtFormat,
    aEncryption,
    aInputKey,
    aMaxHeight,
    aMaxWidth,
    aPaddingPolicy,
    aSizingPolicy,
  )
where

import qualified Network.AWS.ElasticTranscoder.Types.AlbumArtFormat as Types
import qualified Network.AWS.ElasticTranscoder.Types.Encryption as Types
import qualified Network.AWS.ElasticTranscoder.Types.MaxHeight as Types
import qualified Network.AWS.ElasticTranscoder.Types.MaxWidth as Types
import qualified Network.AWS.ElasticTranscoder.Types.PaddingPolicy as Types
import qualified Network.AWS.ElasticTranscoder.Types.SizingPolicy as Types
import qualified Network.AWS.ElasticTranscoder.Types.WatermarkKey as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The file to be used as album art. There can be multiple artworks associated with an audio file, to a maximum of 20.
--
-- To remove artwork or leave the artwork empty, you can either set @Artwork@ to null, or set the @Merge Policy@ to "Replace" and use an empty @Artwork@ array.
-- To pass through existing artwork unchanged, set the @Merge Policy@ to "Prepend", "Append", or "Fallback", and use an empty @Artwork@ array.
--
-- /See:/ 'mkArtwork' smart constructor.
data Artwork = Artwork'
  { -- | The format of album art, if any. Valid formats are @.jpg@ and @.png@ .
    albumArtFormat :: Core.Maybe Types.AlbumArtFormat,
    -- | The encryption settings, if any, that you want Elastic Transcoder to apply to your artwork.
    encryption :: Core.Maybe Types.Encryption,
    -- | The name of the file to be used as album art. To determine which Amazon S3 bucket contains the specified file, Elastic Transcoder checks the pipeline specified by @PipelineId@ ; the @InputBucket@ object in that pipeline identifies the bucket.
    --
    -- If the file name includes a prefix, for example, @cooking/pie.jpg@ , include the prefix in the key. If the file isn't in the specified bucket, Elastic Transcoder returns an error.
    inputKey :: Core.Maybe Types.WatermarkKey,
    -- | The maximum height of the output album art in pixels. If you specify @auto@ , Elastic Transcoder uses 600 as the default value. If you specify a numeric value, enter an even integer between 32 and 3072, inclusive.
    maxHeight :: Core.Maybe Types.MaxHeight,
    -- | The maximum width of the output album art in pixels. If you specify @auto@ , Elastic Transcoder uses 600 as the default value. If you specify a numeric value, enter an even integer between 32 and 4096, inclusive.
    maxWidth :: Core.Maybe Types.MaxWidth,
    -- | When you set @PaddingPolicy@ to @Pad@ , Elastic Transcoder may add white bars to the top and bottom and/or left and right sides of the output album art to make the total size of the output art match the values that you specified for @MaxWidth@ and @MaxHeight@ .
    paddingPolicy :: Core.Maybe Types.PaddingPolicy,
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
    sizingPolicy :: Core.Maybe Types.SizingPolicy
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Artwork' value with any optional fields omitted.
mkArtwork ::
  Artwork
mkArtwork =
  Artwork'
    { albumArtFormat = Core.Nothing,
      encryption = Core.Nothing,
      inputKey = Core.Nothing,
      maxHeight = Core.Nothing,
      maxWidth = Core.Nothing,
      paddingPolicy = Core.Nothing,
      sizingPolicy = Core.Nothing
    }

-- | The format of album art, if any. Valid formats are @.jpg@ and @.png@ .
--
-- /Note:/ Consider using 'albumArtFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAlbumArtFormat :: Lens.Lens' Artwork (Core.Maybe Types.AlbumArtFormat)
aAlbumArtFormat = Lens.field @"albumArtFormat"
{-# DEPRECATED aAlbumArtFormat "Use generic-lens or generic-optics with 'albumArtFormat' instead." #-}

-- | The encryption settings, if any, that you want Elastic Transcoder to apply to your artwork.
--
-- /Note:/ Consider using 'encryption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aEncryption :: Lens.Lens' Artwork (Core.Maybe Types.Encryption)
aEncryption = Lens.field @"encryption"
{-# DEPRECATED aEncryption "Use generic-lens or generic-optics with 'encryption' instead." #-}

-- | The name of the file to be used as album art. To determine which Amazon S3 bucket contains the specified file, Elastic Transcoder checks the pipeline specified by @PipelineId@ ; the @InputBucket@ object in that pipeline identifies the bucket.
--
-- If the file name includes a prefix, for example, @cooking/pie.jpg@ , include the prefix in the key. If the file isn't in the specified bucket, Elastic Transcoder returns an error.
--
-- /Note:/ Consider using 'inputKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aInputKey :: Lens.Lens' Artwork (Core.Maybe Types.WatermarkKey)
aInputKey = Lens.field @"inputKey"
{-# DEPRECATED aInputKey "Use generic-lens or generic-optics with 'inputKey' instead." #-}

-- | The maximum height of the output album art in pixels. If you specify @auto@ , Elastic Transcoder uses 600 as the default value. If you specify a numeric value, enter an even integer between 32 and 3072, inclusive.
--
-- /Note:/ Consider using 'maxHeight' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aMaxHeight :: Lens.Lens' Artwork (Core.Maybe Types.MaxHeight)
aMaxHeight = Lens.field @"maxHeight"
{-# DEPRECATED aMaxHeight "Use generic-lens or generic-optics with 'maxHeight' instead." #-}

-- | The maximum width of the output album art in pixels. If you specify @auto@ , Elastic Transcoder uses 600 as the default value. If you specify a numeric value, enter an even integer between 32 and 4096, inclusive.
--
-- /Note:/ Consider using 'maxWidth' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aMaxWidth :: Lens.Lens' Artwork (Core.Maybe Types.MaxWidth)
aMaxWidth = Lens.field @"maxWidth"
{-# DEPRECATED aMaxWidth "Use generic-lens or generic-optics with 'maxWidth' instead." #-}

-- | When you set @PaddingPolicy@ to @Pad@ , Elastic Transcoder may add white bars to the top and bottom and/or left and right sides of the output album art to make the total size of the output art match the values that you specified for @MaxWidth@ and @MaxHeight@ .
--
-- /Note:/ Consider using 'paddingPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPaddingPolicy :: Lens.Lens' Artwork (Core.Maybe Types.PaddingPolicy)
aPaddingPolicy = Lens.field @"paddingPolicy"
{-# DEPRECATED aPaddingPolicy "Use generic-lens or generic-optics with 'paddingPolicy' instead." #-}

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
aSizingPolicy :: Lens.Lens' Artwork (Core.Maybe Types.SizingPolicy)
aSizingPolicy = Lens.field @"sizingPolicy"
{-# DEPRECATED aSizingPolicy "Use generic-lens or generic-optics with 'sizingPolicy' instead." #-}

instance Core.FromJSON Artwork where
  toJSON Artwork {..} =
    Core.object
      ( Core.catMaybes
          [ ("AlbumArtFormat" Core..=) Core.<$> albumArtFormat,
            ("Encryption" Core..=) Core.<$> encryption,
            ("InputKey" Core..=) Core.<$> inputKey,
            ("MaxHeight" Core..=) Core.<$> maxHeight,
            ("MaxWidth" Core..=) Core.<$> maxWidth,
            ("PaddingPolicy" Core..=) Core.<$> paddingPolicy,
            ("SizingPolicy" Core..=) Core.<$> sizingPolicy
          ]
      )

instance Core.FromJSON Artwork where
  parseJSON =
    Core.withObject "Artwork" Core.$
      \x ->
        Artwork'
          Core.<$> (x Core..:? "AlbumArtFormat")
          Core.<*> (x Core..:? "Encryption")
          Core.<*> (x Core..:? "InputKey")
          Core.<*> (x Core..:? "MaxHeight")
          Core.<*> (x Core..:? "MaxWidth")
          Core.<*> (x Core..:? "PaddingPolicy")
          Core.<*> (x Core..:? "SizingPolicy")
