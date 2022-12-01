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
-- Module      : Amazonka.ElasticTranscoder.Types.Artwork
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticTranscoder.Types.Artwork where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ElasticTranscoder.Types.Encryption
import qualified Amazonka.Prelude as Prelude

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
  { -- | The format of album art, if any. Valid formats are @.jpg@ and @.png@.
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
    -- | When you set @PaddingPolicy@ to @Pad@, Elastic Transcoder may add white
    -- bars to the top and bottom and\/or left and right sides of the output
    -- album art to make the total size of the output art match the values that
    -- you specified for @MaxWidth@ and @MaxHeight@.
    paddingPolicy :: Prelude.Maybe Prelude.Text,
    -- | The name of the file to be used as album art. To determine which Amazon
    -- S3 bucket contains the specified file, Elastic Transcoder checks the
    -- pipeline specified by @PipelineId@; the @InputBucket@ object in that
    -- pipeline identifies the bucket.
    --
    -- If the file name includes a prefix, for example, @cooking\/pie.jpg@,
    -- include the prefix in the key. If the file isn\'t in the specified
    -- bucket, Elastic Transcoder returns an error.
    inputKey :: Prelude.Maybe Prelude.Text,
    -- | The encryption settings, if any, that you want Elastic Transcoder to
    -- apply to your artwork.
    encryption :: Prelude.Maybe Encryption,
    -- | The maximum height of the output album art in pixels. If you specify
    -- @auto@, Elastic Transcoder uses 600 as the default value. If you specify
    -- a numeric value, enter an even integer between 32 and 3072, inclusive.
    maxHeight :: Prelude.Maybe Prelude.Text,
    -- | The maximum width of the output album art in pixels. If you specify
    -- @auto@, Elastic Transcoder uses 600 as the default value. If you specify
    -- a numeric value, enter an even integer between 32 and 4096, inclusive.
    maxWidth :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Artwork' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'paddingPolicy', 'artwork_paddingPolicy' - When you set @PaddingPolicy@ to @Pad@, Elastic Transcoder may add white
-- bars to the top and bottom and\/or left and right sides of the output
-- album art to make the total size of the output art match the values that
-- you specified for @MaxWidth@ and @MaxHeight@.
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
-- 'encryption', 'artwork_encryption' - The encryption settings, if any, that you want Elastic Transcoder to
-- apply to your artwork.
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
    { albumArtFormat = Prelude.Nothing,
      sizingPolicy = Prelude.Nothing,
      paddingPolicy = Prelude.Nothing,
      inputKey = Prelude.Nothing,
      encryption = Prelude.Nothing,
      maxHeight = Prelude.Nothing,
      maxWidth = Prelude.Nothing
    }

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

-- | When you set @PaddingPolicy@ to @Pad@, Elastic Transcoder may add white
-- bars to the top and bottom and\/or left and right sides of the output
-- album art to make the total size of the output art match the values that
-- you specified for @MaxWidth@ and @MaxHeight@.
artwork_paddingPolicy :: Lens.Lens' Artwork (Prelude.Maybe Prelude.Text)
artwork_paddingPolicy = Lens.lens (\Artwork' {paddingPolicy} -> paddingPolicy) (\s@Artwork' {} a -> s {paddingPolicy = a} :: Artwork)

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

-- | The encryption settings, if any, that you want Elastic Transcoder to
-- apply to your artwork.
artwork_encryption :: Lens.Lens' Artwork (Prelude.Maybe Encryption)
artwork_encryption = Lens.lens (\Artwork' {encryption} -> encryption) (\s@Artwork' {} a -> s {encryption = a} :: Artwork)

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

instance Core.FromJSON Artwork where
  parseJSON =
    Core.withObject
      "Artwork"
      ( \x ->
          Artwork'
            Prelude.<$> (x Core..:? "AlbumArtFormat")
            Prelude.<*> (x Core..:? "SizingPolicy")
            Prelude.<*> (x Core..:? "PaddingPolicy")
            Prelude.<*> (x Core..:? "InputKey")
            Prelude.<*> (x Core..:? "Encryption")
            Prelude.<*> (x Core..:? "MaxHeight")
            Prelude.<*> (x Core..:? "MaxWidth")
      )

instance Prelude.Hashable Artwork where
  hashWithSalt _salt Artwork' {..} =
    _salt `Prelude.hashWithSalt` albumArtFormat
      `Prelude.hashWithSalt` sizingPolicy
      `Prelude.hashWithSalt` paddingPolicy
      `Prelude.hashWithSalt` inputKey
      `Prelude.hashWithSalt` encryption
      `Prelude.hashWithSalt` maxHeight
      `Prelude.hashWithSalt` maxWidth

instance Prelude.NFData Artwork where
  rnf Artwork' {..} =
    Prelude.rnf albumArtFormat
      `Prelude.seq` Prelude.rnf sizingPolicy
      `Prelude.seq` Prelude.rnf paddingPolicy
      `Prelude.seq` Prelude.rnf inputKey
      `Prelude.seq` Prelude.rnf encryption
      `Prelude.seq` Prelude.rnf maxHeight
      `Prelude.seq` Prelude.rnf maxWidth

instance Core.ToJSON Artwork where
  toJSON Artwork' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AlbumArtFormat" Core..=)
              Prelude.<$> albumArtFormat,
            ("SizingPolicy" Core..=) Prelude.<$> sizingPolicy,
            ("PaddingPolicy" Core..=) Prelude.<$> paddingPolicy,
            ("InputKey" Core..=) Prelude.<$> inputKey,
            ("Encryption" Core..=) Prelude.<$> encryption,
            ("MaxHeight" Core..=) Prelude.<$> maxHeight,
            ("MaxWidth" Core..=) Prelude.<$> maxWidth
          ]
      )
