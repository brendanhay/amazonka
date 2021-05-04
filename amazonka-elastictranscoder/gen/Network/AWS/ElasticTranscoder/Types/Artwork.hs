{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Artwork
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.Artwork where

import Network.AWS.ElasticTranscoder.Types.Encryption
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The file to be used as album art. There can be multiple artworks
-- associated with an audio file, to a maximum of 20.
--
-- To remove artwork or leave the artwork empty, you can either set
-- @Artwork@ to null, or set the @Merge Policy@ to \"Replace\" and use an
-- empty @Artwork@ array.
--
-- To pass through existing artwork unchanged, set the @Merge Policy@ to
-- \"Prepend\", \"Append\", or \"Fallback\", and use an empty @Artwork@
-- array.
--
-- /See:/ 'newArtwork' smart constructor.
data Artwork = Artwork'
  { -- | The name of the file to be used as album art. To determine which Amazon
    -- S3 bucket contains the specified file, Elastic Transcoder checks the
    -- pipeline specified by @PipelineId@; the @InputBucket@ object in that
    -- pipeline identifies the bucket.
    --
    -- If the file name includes a prefix, for example, @cooking\/pie.jpg@,
    -- include the prefix in the key. If the file isn\'t in the specified
    -- bucket, Elastic Transcoder returns an error.
    inputKey :: Prelude.Maybe Prelude.Text,
    -- | The format of album art, if any. Valid formats are @.jpg@ and @.png@.
    albumArtFormat :: Prelude.Maybe Prelude.Text,
    -- | Specify one of the following values to control scaling of the output
    -- album art:
    --
    -- -   @Fit:@ Elastic Transcoder scales the output art so it matches the
    --     value that you specified in either @MaxWidth@ or @MaxHeight@ without
    --     exceeding the other value.
    --
    -- -   @Fill:@ Elastic Transcoder scales the output art so it matches the
    --     value that you specified in either @MaxWidth@ or @MaxHeight@ and
    --     matches or exceeds the other value. Elastic Transcoder centers the
    --     output art and then crops it in the dimension (if any) that exceeds
    --     the maximum value.
    --
    -- -   @Stretch:@ Elastic Transcoder stretches the output art to match the
    --     values that you specified for @MaxWidth@ and @MaxHeight@. If the
    --     relative proportions of the input art and the output art are
    --     different, the output art will be distorted.
    --
    -- -   @Keep:@ Elastic Transcoder does not scale the output art. If either
    --     dimension of the input art exceeds the values that you specified for
    --     @MaxWidth@ and @MaxHeight@, Elastic Transcoder crops the output art.
    --
    -- -   @ShrinkToFit:@ Elastic Transcoder scales the output art down so that
    --     its dimensions match the values that you specified for at least one
    --     of @MaxWidth@ and @MaxHeight@ without exceeding either value. If you
    --     specify this option, Elastic Transcoder does not scale the art up.
    --
    -- -   @ShrinkToFill@ Elastic Transcoder scales the output art down so that
    --     its dimensions match the values that you specified for at least one
    --     of @MaxWidth@ and @MaxHeight@ without dropping below either value.
    --     If you specify this option, Elastic Transcoder does not scale the
    --     art up.
    sizingPolicy :: Prelude.Maybe Prelude.Text,
    -- | The encryption settings, if any, that you want Elastic Transcoder to
    -- apply to your artwork.
    encryption :: Prelude.Maybe Encryption,
    -- | When you set @PaddingPolicy@ to @Pad@, Elastic Transcoder may add white
    -- bars to the top and bottom and\/or left and right sides of the output
    -- album art to make the total size of the output art match the values that
    -- you specified for @MaxWidth@ and @MaxHeight@.
    paddingPolicy :: Prelude.Maybe Prelude.Text,
    -- | The maximum height of the output album art in pixels. If you specify
    -- @auto@, Elastic Transcoder uses 600 as the default value. If you specify
    -- a numeric value, enter an even integer between 32 and 3072, inclusive.
    maxHeight :: Prelude.Maybe Prelude.Text,
    -- | The maximum width of the output album art in pixels. If you specify
    -- @auto@, Elastic Transcoder uses 600 as the default value. If you specify
    -- a numeric value, enter an even integer between 32 and 4096, inclusive.
    maxWidth :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Artwork' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputKey', 'artwork_inputKey' - The name of the file to be used as album art. To determine which Amazon
-- S3 bucket contains the specified file, Elastic Transcoder checks the
-- pipeline specified by @PipelineId@; the @InputBucket@ object in that
-- pipeline identifies the bucket.
--
-- If the file name includes a prefix, for example, @cooking\/pie.jpg@,
-- include the prefix in the key. If the file isn\'t in the specified
-- bucket, Elastic Transcoder returns an error.
--
-- 'albumArtFormat', 'artwork_albumArtFormat' - The format of album art, if any. Valid formats are @.jpg@ and @.png@.
--
-- 'sizingPolicy', 'artwork_sizingPolicy' - Specify one of the following values to control scaling of the output
-- album art:
--
-- -   @Fit:@ Elastic Transcoder scales the output art so it matches the
--     value that you specified in either @MaxWidth@ or @MaxHeight@ without
--     exceeding the other value.
--
-- -   @Fill:@ Elastic Transcoder scales the output art so it matches the
--     value that you specified in either @MaxWidth@ or @MaxHeight@ and
--     matches or exceeds the other value. Elastic Transcoder centers the
--     output art and then crops it in the dimension (if any) that exceeds
--     the maximum value.
--
-- -   @Stretch:@ Elastic Transcoder stretches the output art to match the
--     values that you specified for @MaxWidth@ and @MaxHeight@. If the
--     relative proportions of the input art and the output art are
--     different, the output art will be distorted.
--
-- -   @Keep:@ Elastic Transcoder does not scale the output art. If either
--     dimension of the input art exceeds the values that you specified for
--     @MaxWidth@ and @MaxHeight@, Elastic Transcoder crops the output art.
--
-- -   @ShrinkToFit:@ Elastic Transcoder scales the output art down so that
--     its dimensions match the values that you specified for at least one
--     of @MaxWidth@ and @MaxHeight@ without exceeding either value. If you
--     specify this option, Elastic Transcoder does not scale the art up.
--
-- -   @ShrinkToFill@ Elastic Transcoder scales the output art down so that
--     its dimensions match the values that you specified for at least one
--     of @MaxWidth@ and @MaxHeight@ without dropping below either value.
--     If you specify this option, Elastic Transcoder does not scale the
--     art up.
--
-- 'encryption', 'artwork_encryption' - The encryption settings, if any, that you want Elastic Transcoder to
-- apply to your artwork.
--
-- 'paddingPolicy', 'artwork_paddingPolicy' - When you set @PaddingPolicy@ to @Pad@, Elastic Transcoder may add white
-- bars to the top and bottom and\/or left and right sides of the output
-- album art to make the total size of the output art match the values that
-- you specified for @MaxWidth@ and @MaxHeight@.
--
-- 'maxHeight', 'artwork_maxHeight' - The maximum height of the output album art in pixels. If you specify
-- @auto@, Elastic Transcoder uses 600 as the default value. If you specify
-- a numeric value, enter an even integer between 32 and 3072, inclusive.
--
-- 'maxWidth', 'artwork_maxWidth' - The maximum width of the output album art in pixels. If you specify
-- @auto@, Elastic Transcoder uses 600 as the default value. If you specify
-- a numeric value, enter an even integer between 32 and 4096, inclusive.
newArtwork ::
  Artwork
newArtwork =
  Artwork'
    { inputKey = Prelude.Nothing,
      albumArtFormat = Prelude.Nothing,
      sizingPolicy = Prelude.Nothing,
      encryption = Prelude.Nothing,
      paddingPolicy = Prelude.Nothing,
      maxHeight = Prelude.Nothing,
      maxWidth = Prelude.Nothing
    }

-- | The name of the file to be used as album art. To determine which Amazon
-- S3 bucket contains the specified file, Elastic Transcoder checks the
-- pipeline specified by @PipelineId@; the @InputBucket@ object in that
-- pipeline identifies the bucket.
--
-- If the file name includes a prefix, for example, @cooking\/pie.jpg@,
-- include the prefix in the key. If the file isn\'t in the specified
-- bucket, Elastic Transcoder returns an error.
artwork_inputKey :: Lens.Lens' Artwork (Prelude.Maybe Prelude.Text)
artwork_inputKey = Lens.lens (\Artwork' {inputKey} -> inputKey) (\s@Artwork' {} a -> s {inputKey = a} :: Artwork)

-- | The format of album art, if any. Valid formats are @.jpg@ and @.png@.
artwork_albumArtFormat :: Lens.Lens' Artwork (Prelude.Maybe Prelude.Text)
artwork_albumArtFormat = Lens.lens (\Artwork' {albumArtFormat} -> albumArtFormat) (\s@Artwork' {} a -> s {albumArtFormat = a} :: Artwork)

-- | Specify one of the following values to control scaling of the output
-- album art:
--
-- -   @Fit:@ Elastic Transcoder scales the output art so it matches the
--     value that you specified in either @MaxWidth@ or @MaxHeight@ without
--     exceeding the other value.
--
-- -   @Fill:@ Elastic Transcoder scales the output art so it matches the
--     value that you specified in either @MaxWidth@ or @MaxHeight@ and
--     matches or exceeds the other value. Elastic Transcoder centers the
--     output art and then crops it in the dimension (if any) that exceeds
--     the maximum value.
--
-- -   @Stretch:@ Elastic Transcoder stretches the output art to match the
--     values that you specified for @MaxWidth@ and @MaxHeight@. If the
--     relative proportions of the input art and the output art are
--     different, the output art will be distorted.
--
-- -   @Keep:@ Elastic Transcoder does not scale the output art. If either
--     dimension of the input art exceeds the values that you specified for
--     @MaxWidth@ and @MaxHeight@, Elastic Transcoder crops the output art.
--
-- -   @ShrinkToFit:@ Elastic Transcoder scales the output art down so that
--     its dimensions match the values that you specified for at least one
--     of @MaxWidth@ and @MaxHeight@ without exceeding either value. If you
--     specify this option, Elastic Transcoder does not scale the art up.
--
-- -   @ShrinkToFill@ Elastic Transcoder scales the output art down so that
--     its dimensions match the values that you specified for at least one
--     of @MaxWidth@ and @MaxHeight@ without dropping below either value.
--     If you specify this option, Elastic Transcoder does not scale the
--     art up.
artwork_sizingPolicy :: Lens.Lens' Artwork (Prelude.Maybe Prelude.Text)
artwork_sizingPolicy = Lens.lens (\Artwork' {sizingPolicy} -> sizingPolicy) (\s@Artwork' {} a -> s {sizingPolicy = a} :: Artwork)

-- | The encryption settings, if any, that you want Elastic Transcoder to
-- apply to your artwork.
artwork_encryption :: Lens.Lens' Artwork (Prelude.Maybe Encryption)
artwork_encryption = Lens.lens (\Artwork' {encryption} -> encryption) (\s@Artwork' {} a -> s {encryption = a} :: Artwork)

-- | When you set @PaddingPolicy@ to @Pad@, Elastic Transcoder may add white
-- bars to the top and bottom and\/or left and right sides of the output
-- album art to make the total size of the output art match the values that
-- you specified for @MaxWidth@ and @MaxHeight@.
artwork_paddingPolicy :: Lens.Lens' Artwork (Prelude.Maybe Prelude.Text)
artwork_paddingPolicy = Lens.lens (\Artwork' {paddingPolicy} -> paddingPolicy) (\s@Artwork' {} a -> s {paddingPolicy = a} :: Artwork)

-- | The maximum height of the output album art in pixels. If you specify
-- @auto@, Elastic Transcoder uses 600 as the default value. If you specify
-- a numeric value, enter an even integer between 32 and 3072, inclusive.
artwork_maxHeight :: Lens.Lens' Artwork (Prelude.Maybe Prelude.Text)
artwork_maxHeight = Lens.lens (\Artwork' {maxHeight} -> maxHeight) (\s@Artwork' {} a -> s {maxHeight = a} :: Artwork)

-- | The maximum width of the output album art in pixels. If you specify
-- @auto@, Elastic Transcoder uses 600 as the default value. If you specify
-- a numeric value, enter an even integer between 32 and 4096, inclusive.
artwork_maxWidth :: Lens.Lens' Artwork (Prelude.Maybe Prelude.Text)
artwork_maxWidth = Lens.lens (\Artwork' {maxWidth} -> maxWidth) (\s@Artwork' {} a -> s {maxWidth = a} :: Artwork)

instance Prelude.FromJSON Artwork where
  parseJSON =
    Prelude.withObject
      "Artwork"
      ( \x ->
          Artwork'
            Prelude.<$> (x Prelude..:? "InputKey")
            Prelude.<*> (x Prelude..:? "AlbumArtFormat")
            Prelude.<*> (x Prelude..:? "SizingPolicy")
            Prelude.<*> (x Prelude..:? "Encryption")
            Prelude.<*> (x Prelude..:? "PaddingPolicy")
            Prelude.<*> (x Prelude..:? "MaxHeight")
            Prelude.<*> (x Prelude..:? "MaxWidth")
      )

instance Prelude.Hashable Artwork

instance Prelude.NFData Artwork

instance Prelude.ToJSON Artwork where
  toJSON Artwork' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("InputKey" Prelude..=) Prelude.<$> inputKey,
            ("AlbumArtFormat" Prelude..=)
              Prelude.<$> albumArtFormat,
            ("SizingPolicy" Prelude..=) Prelude.<$> sizingPolicy,
            ("Encryption" Prelude..=) Prelude.<$> encryption,
            ("PaddingPolicy" Prelude..=)
              Prelude.<$> paddingPolicy,
            ("MaxHeight" Prelude..=) Prelude.<$> maxHeight,
            ("MaxWidth" Prelude..=) Prelude.<$> maxWidth
          ]
      )
