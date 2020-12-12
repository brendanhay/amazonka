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
    preARN,
    preVideo,
    preThumbnails,
    preName,
    preContainer,
    preId,
    preType,
    preDescription,
    preAudio,
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
  { arn :: Lude.Maybe Lude.Text,
    video :: Lude.Maybe VideoParameters,
    thumbnails :: Lude.Maybe Thumbnails,
    name :: Lude.Maybe Lude.Text,
    container :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    audio :: Lude.Maybe AudioParameters
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Preset' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) for the preset.
-- * 'audio' - A section of the response body that provides information about the audio preset values.
-- * 'container' - The container type for the output file. Valid values include @flac@ , @flv@ , @fmp4@ , @gif@ , @mp3@ , @mp4@ , @mpg@ , @mxf@ , @oga@ , @ogg@ , @ts@ , and @webm@ .
-- * 'description' - A description of the preset.
-- * 'id' - Identifier for the new preset. You use this value to get settings for the preset or to delete it.
-- * 'name' - The name of the preset.
-- * 'thumbnails' - A section of the response body that provides information about the thumbnail preset values, if any.
-- * 'type'' - Whether the preset is a default preset provided by Elastic Transcoder (@System@ ) or a preset that you have defined (@Custom@ ).
-- * 'video' - A section of the response body that provides information about the video preset values.
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
preARN :: Lens.Lens' Preset (Lude.Maybe Lude.Text)
preARN = Lens.lens (arn :: Preset -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Preset)
{-# DEPRECATED preARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | A section of the response body that provides information about the video preset values.
--
-- /Note:/ Consider using 'video' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preVideo :: Lens.Lens' Preset (Lude.Maybe VideoParameters)
preVideo = Lens.lens (video :: Preset -> Lude.Maybe VideoParameters) (\s a -> s {video = a} :: Preset)
{-# DEPRECATED preVideo "Use generic-lens or generic-optics with 'video' instead." #-}

-- | A section of the response body that provides information about the thumbnail preset values, if any.
--
-- /Note:/ Consider using 'thumbnails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preThumbnails :: Lens.Lens' Preset (Lude.Maybe Thumbnails)
preThumbnails = Lens.lens (thumbnails :: Preset -> Lude.Maybe Thumbnails) (\s a -> s {thumbnails = a} :: Preset)
{-# DEPRECATED preThumbnails "Use generic-lens or generic-optics with 'thumbnails' instead." #-}

-- | The name of the preset.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preName :: Lens.Lens' Preset (Lude.Maybe Lude.Text)
preName = Lens.lens (name :: Preset -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Preset)
{-# DEPRECATED preName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The container type for the output file. Valid values include @flac@ , @flv@ , @fmp4@ , @gif@ , @mp3@ , @mp4@ , @mpg@ , @mxf@ , @oga@ , @ogg@ , @ts@ , and @webm@ .
--
-- /Note:/ Consider using 'container' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preContainer :: Lens.Lens' Preset (Lude.Maybe Lude.Text)
preContainer = Lens.lens (container :: Preset -> Lude.Maybe Lude.Text) (\s a -> s {container = a} :: Preset)
{-# DEPRECATED preContainer "Use generic-lens or generic-optics with 'container' instead." #-}

-- | Identifier for the new preset. You use this value to get settings for the preset or to delete it.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preId :: Lens.Lens' Preset (Lude.Maybe Lude.Text)
preId = Lens.lens (id :: Preset -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Preset)
{-# DEPRECATED preId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Whether the preset is a default preset provided by Elastic Transcoder (@System@ ) or a preset that you have defined (@Custom@ ).
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preType :: Lens.Lens' Preset (Lude.Maybe Lude.Text)
preType = Lens.lens (type' :: Preset -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: Preset)
{-# DEPRECATED preType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A description of the preset.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preDescription :: Lens.Lens' Preset (Lude.Maybe Lude.Text)
preDescription = Lens.lens (description :: Preset -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Preset)
{-# DEPRECATED preDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A section of the response body that provides information about the audio preset values.
--
-- /Note:/ Consider using 'audio' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
preAudio :: Lens.Lens' Preset (Lude.Maybe AudioParameters)
preAudio = Lens.lens (audio :: Preset -> Lude.Maybe AudioParameters) (\s a -> s {audio = a} :: Preset)
{-# DEPRECATED preAudio "Use generic-lens or generic-optics with 'audio' instead." #-}

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
