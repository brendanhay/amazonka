{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Preset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticTranscoder.Types.Preset
  ( Preset (..)
  -- * Smart constructor
  , mkPreset
  -- * Lenses
  , pgArn
  , pgAudio
  , pgContainer
  , pgDescription
  , pgId
  , pgName
  , pgThumbnails
  , pgType
  , pgVideo
  ) where

import qualified Network.AWS.ElasticTranscoder.Types.AudioParameters as Types
import qualified Network.AWS.ElasticTranscoder.Types.Description as Types
import qualified Network.AWS.ElasticTranscoder.Types.Id as Types
import qualified Network.AWS.ElasticTranscoder.Types.Name as Types
import qualified Network.AWS.ElasticTranscoder.Types.PresetContainer as Types
import qualified Network.AWS.ElasticTranscoder.Types.PresetType as Types
import qualified Network.AWS.ElasticTranscoder.Types.Thumbnails as Types
import qualified Network.AWS.ElasticTranscoder.Types.VideoParameters as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Presets are templates that contain most of the settings for transcoding media files from one format to another. Elastic Transcoder includes some default presets for common formats, for example, several iPod and iPhone versions. You can also create your own presets for formats that aren't included among the default presets. You specify which preset you want to use when you create a job.
--
-- /See:/ 'mkPreset' smart constructor.
data Preset = Preset'
  { arn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) for the preset.
  , audio :: Core.Maybe Types.AudioParameters
    -- ^ A section of the response body that provides information about the audio preset values.
  , container :: Core.Maybe Types.PresetContainer
    -- ^ The container type for the output file. Valid values include @flac@ , @flv@ , @fmp4@ , @gif@ , @mp3@ , @mp4@ , @mpg@ , @mxf@ , @oga@ , @ogg@ , @ts@ , and @webm@ .
  , description :: Core.Maybe Types.Description
    -- ^ A description of the preset.
  , id :: Core.Maybe Types.Id
    -- ^ Identifier for the new preset. You use this value to get settings for the preset or to delete it.
  , name :: Core.Maybe Types.Name
    -- ^ The name of the preset.
  , thumbnails :: Core.Maybe Types.Thumbnails
    -- ^ A section of the response body that provides information about the thumbnail preset values, if any.
  , type' :: Core.Maybe Types.PresetType
    -- ^ Whether the preset is a default preset provided by Elastic Transcoder (@System@ ) or a preset that you have defined (@Custom@ ).
  , video :: Core.Maybe Types.VideoParameters
    -- ^ A section of the response body that provides information about the video preset values.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Preset' value with any optional fields omitted.
mkPreset
    :: Preset
mkPreset
  = Preset'{arn = Core.Nothing, audio = Core.Nothing,
            container = Core.Nothing, description = Core.Nothing,
            id = Core.Nothing, name = Core.Nothing, thumbnails = Core.Nothing,
            type' = Core.Nothing, video = Core.Nothing}

-- | The Amazon Resource Name (ARN) for the preset.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgArn :: Lens.Lens' Preset (Core.Maybe Core.Text)
pgArn = Lens.field @"arn"
{-# INLINEABLE pgArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | A section of the response body that provides information about the audio preset values.
--
-- /Note:/ Consider using 'audio' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgAudio :: Lens.Lens' Preset (Core.Maybe Types.AudioParameters)
pgAudio = Lens.field @"audio"
{-# INLINEABLE pgAudio #-}
{-# DEPRECATED audio "Use generic-lens or generic-optics with 'audio' instead"  #-}

-- | The container type for the output file. Valid values include @flac@ , @flv@ , @fmp4@ , @gif@ , @mp3@ , @mp4@ , @mpg@ , @mxf@ , @oga@ , @ogg@ , @ts@ , and @webm@ .
--
-- /Note:/ Consider using 'container' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgContainer :: Lens.Lens' Preset (Core.Maybe Types.PresetContainer)
pgContainer = Lens.field @"container"
{-# INLINEABLE pgContainer #-}
{-# DEPRECATED container "Use generic-lens or generic-optics with 'container' instead"  #-}

-- | A description of the preset.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgDescription :: Lens.Lens' Preset (Core.Maybe Types.Description)
pgDescription = Lens.field @"description"
{-# INLINEABLE pgDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Identifier for the new preset. You use this value to get settings for the preset or to delete it.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgId :: Lens.Lens' Preset (Core.Maybe Types.Id)
pgId = Lens.field @"id"
{-# INLINEABLE pgId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The name of the preset.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgName :: Lens.Lens' Preset (Core.Maybe Types.Name)
pgName = Lens.field @"name"
{-# INLINEABLE pgName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A section of the response body that provides information about the thumbnail preset values, if any.
--
-- /Note:/ Consider using 'thumbnails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgThumbnails :: Lens.Lens' Preset (Core.Maybe Types.Thumbnails)
pgThumbnails = Lens.field @"thumbnails"
{-# INLINEABLE pgThumbnails #-}
{-# DEPRECATED thumbnails "Use generic-lens or generic-optics with 'thumbnails' instead"  #-}

-- | Whether the preset is a default preset provided by Elastic Transcoder (@System@ ) or a preset that you have defined (@Custom@ ).
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgType :: Lens.Lens' Preset (Core.Maybe Types.PresetType)
pgType = Lens.field @"type'"
{-# INLINEABLE pgType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | A section of the response body that provides information about the video preset values.
--
-- /Note:/ Consider using 'video' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pgVideo :: Lens.Lens' Preset (Core.Maybe Types.VideoParameters)
pgVideo = Lens.field @"video"
{-# INLINEABLE pgVideo #-}
{-# DEPRECATED video "Use generic-lens or generic-optics with 'video' instead"  #-}

instance Core.FromJSON Preset where
        parseJSON
          = Core.withObject "Preset" Core.$
              \ x ->
                Preset' Core.<$>
                  (x Core..:? "Arn") Core.<*> x Core..:? "Audio" Core.<*>
                    x Core..:? "Container"
                    Core.<*> x Core..:? "Description"
                    Core.<*> x Core..:? "Id"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "Thumbnails"
                    Core.<*> x Core..:? "Type"
                    Core.<*> x Core..:? "Video"
