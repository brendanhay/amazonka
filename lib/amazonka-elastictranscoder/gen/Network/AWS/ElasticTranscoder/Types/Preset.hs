{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Preset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.Preset
  ( Preset (..),

    -- * Smart constructor
    mkPreset,

    -- * Lenses
    pgARN,
    pgVideo,
    pgThumbnails,
    pgName,
    pgContainer,
    pgId,
    pgType,
    pgDescription,
    pgAudio,
  )
where

import Network.AWS.ElasticTranscoder.Types.AudioParameters
import Network.AWS.ElasticTranscoder.Types.Thumbnails
import Network.AWS.ElasticTranscoder.Types.VideoParameters
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Presets are templates that contain most of the settings for transcoding media files from one format to another. Elastic Transcoder includes some default presets for common formats, for example, several iPod and iPhone versions. You can also create your own presets for formats that aren't included among the default presets. You specify which preset you want to use when you create a job.
--
-- /See:/ 'mkPreset' smart constructor.
data Preset = Preset'
  { -- | The Amazon Resource Name (ARN) for the preset.
    arn :: Lude.Maybe Lude.Text,
    -- | A section of the response body that provides information about the video preset values.
    video :: Lude.Maybe VideoParameters,
    -- | A section of the response body that provides information about the thumbnail preset values, if any.
    thumbnails :: Lude.Maybe Thumbnails,
    -- | The name of the preset.
    name :: Lude.Maybe Lude.Text,
    -- | The container type for the output file. Valid values include @flac@ , @flv@ , @fmp4@ , @gif@ , @mp3@ , @mp4@ , @mpg@ , @mxf@ , @oga@ , @ogg@ , @ts@ , and @webm@ .
    container :: Lude.Maybe Lude.Text,
    -- | Identifier for the new preset. You use this value to get settings for the preset or to delete it.
    id :: Lude.Maybe Lude.Text,
    -- | Whether the preset is a default preset provided by Elastic Transcoder (@System@ ) or a preset that you have defined (@Custom@ ).
    type' :: Lude.Maybe Lude.Text,
    -- | A description of the preset.
    description :: Lude.Maybe Lude.Text,
    -- | A section of the response body that provides information about the audio preset values.
    audio :: Lude.Maybe AudioParameters
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Preset' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) for the preset.
-- * 'video' - A section of the response body that provides information about the video preset values.
-- * 'thumbnails' - A section of the response body that provides information about the thumbnail preset values, if any.
-- * 'name' - The name of the preset.
-- * 'container' - The container type for the output file. Valid values include @flac@ , @flv@ , @fmp4@ , @gif@ , @mp3@ , @mp4@ , @mpg@ , @mxf@ , @oga@ , @ogg@ , @ts@ , and @webm@ .
-- * 'id' - Identifier for the new preset. You use this value to get settings for the preset or to delete it.
-- * 'type'' - Whether the preset is a default preset provided by Elastic Transcoder (@System@ ) or a preset that you have defined (@Custom@ ).
-- * 'description' - A description of the preset.
-- * 'audio' - A section of the response body that provides information about the audio preset values.
mkPreset ::
  Preset
mkPreset =
  Preset'
    { arn = Lude.Nothing,
      video = Lude.Nothing,
      thumbnails = Lude.Nothing,
      name = Lude.Nothing,
      container = Lude.Nothing,
      id = Lude.Nothing,
      type' = Lude.Nothing,
      description = Lude.Nothing,
      audio = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) for the preset.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgARN :: Lens.Lens' Preset (Lude.Maybe Lude.Text)
pgARN = Lens.lens (arn :: Preset -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Preset)
{-# DEPRECATED pgARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | A section of the response body that provides information about the video preset values.
--
-- /Note:/ Consider using 'video' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgVideo :: Lens.Lens' Preset (Lude.Maybe VideoParameters)
pgVideo = Lens.lens (video :: Preset -> Lude.Maybe VideoParameters) (\s a -> s {video = a} :: Preset)
{-# DEPRECATED pgVideo "Use generic-lens or generic-optics with 'video' instead." #-}

-- | A section of the response body that provides information about the thumbnail preset values, if any.
--
-- /Note:/ Consider using 'thumbnails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgThumbnails :: Lens.Lens' Preset (Lude.Maybe Thumbnails)
pgThumbnails = Lens.lens (thumbnails :: Preset -> Lude.Maybe Thumbnails) (\s a -> s {thumbnails = a} :: Preset)
{-# DEPRECATED pgThumbnails "Use generic-lens or generic-optics with 'thumbnails' instead." #-}

-- | The name of the preset.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgName :: Lens.Lens' Preset (Lude.Maybe Lude.Text)
pgName = Lens.lens (name :: Preset -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Preset)
{-# DEPRECATED pgName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The container type for the output file. Valid values include @flac@ , @flv@ , @fmp4@ , @gif@ , @mp3@ , @mp4@ , @mpg@ , @mxf@ , @oga@ , @ogg@ , @ts@ , and @webm@ .
--
-- /Note:/ Consider using 'container' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgContainer :: Lens.Lens' Preset (Lude.Maybe Lude.Text)
pgContainer = Lens.lens (container :: Preset -> Lude.Maybe Lude.Text) (\s a -> s {container = a} :: Preset)
{-# DEPRECATED pgContainer "Use generic-lens or generic-optics with 'container' instead." #-}

-- | Identifier for the new preset. You use this value to get settings for the preset or to delete it.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgId :: Lens.Lens' Preset (Lude.Maybe Lude.Text)
pgId = Lens.lens (id :: Preset -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Preset)
{-# DEPRECATED pgId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Whether the preset is a default preset provided by Elastic Transcoder (@System@ ) or a preset that you have defined (@Custom@ ).
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgType :: Lens.Lens' Preset (Lude.Maybe Lude.Text)
pgType = Lens.lens (type' :: Preset -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: Preset)
{-# DEPRECATED pgType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A description of the preset.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgDescription :: Lens.Lens' Preset (Lude.Maybe Lude.Text)
pgDescription = Lens.lens (description :: Preset -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Preset)
{-# DEPRECATED pgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A section of the response body that provides information about the audio preset values.
--
-- /Note:/ Consider using 'audio' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgAudio :: Lens.Lens' Preset (Lude.Maybe AudioParameters)
pgAudio = Lens.lens (audio :: Preset -> Lude.Maybe AudioParameters) (\s a -> s {audio = a} :: Preset)
{-# DEPRECATED pgAudio "Use generic-lens or generic-optics with 'audio' instead." #-}

instance Lude.FromJSON Preset where
  parseJSON =
    Lude.withObject
      "Preset"
      ( \x ->
          Preset'
            Lude.<$> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "Video")
            Lude.<*> (x Lude..:? "Thumbnails")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Container")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "Audio")
      )
