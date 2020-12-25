{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.CreatePreset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The CreatePreset operation creates a preset with settings that you specify.
--
-- /Important:/ Elastic Transcoder checks the CreatePreset settings to ensure that they meet Elastic Transcoder requirements and to determine whether they comply with H.264 standards. If your settings are not valid for Elastic Transcoder, Elastic Transcoder returns an HTTP 400 response (@ValidationException@ ) and does not create the preset. If the settings are valid for Elastic Transcoder but aren't strictly compliant with the H.264 standard, Elastic Transcoder creates the preset and returns a warning message in the response. This helps you determine whether your settings comply with the H.264 standard while giving you greater flexibility with respect to the video that Elastic Transcoder produces.
-- Elastic Transcoder uses the H.264 video-compression format. For more information, see the International Telecommunication Union publication /Recommendation ITU-T H.264: Advanced video coding for generic audiovisual services/ .
module Network.AWS.ElasticTranscoder.CreatePreset
  ( -- * Creating a request
    CreatePreset (..),
    mkCreatePreset,

    -- ** Request lenses
    cpName,
    cpContainer,
    cpAudio,
    cpDescription,
    cpThumbnails,
    cpVideo,

    -- * Destructuring the response
    CreatePresetResponse (..),
    mkCreatePresetResponse,

    -- ** Response lenses
    cprrsPreset,
    cprrsWarning,
    cprrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticTranscoder.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The @CreatePresetRequest@ structure.
--
-- /See:/ 'mkCreatePreset' smart constructor.
data CreatePreset = CreatePreset'
  { -- | The name of the preset. We recommend that the name be unique within the AWS account, but uniqueness is not enforced.
    name :: Types.Name,
    -- | The container type for the output file. Valid values include @flac@ , @flv@ , @fmp4@ , @gif@ , @mp3@ , @mp4@ , @mpg@ , @mxf@ , @oga@ , @ogg@ , @ts@ , and @webm@ .
    container :: Types.Container,
    -- | A section of the request body that specifies the audio parameters.
    audio :: Core.Maybe Types.AudioParameters,
    -- | A description of the preset.
    description :: Core.Maybe Types.Description,
    -- | A section of the request body that specifies the thumbnail parameters, if any.
    thumbnails :: Core.Maybe Types.Thumbnails,
    -- | A section of the request body that specifies the video parameters.
    video :: Core.Maybe Types.VideoParameters
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePreset' value with any optional fields omitted.
mkCreatePreset ::
  -- | 'name'
  Types.Name ->
  -- | 'container'
  Types.Container ->
  CreatePreset
mkCreatePreset name container =
  CreatePreset'
    { name,
      container,
      audio = Core.Nothing,
      description = Core.Nothing,
      thumbnails = Core.Nothing,
      video = Core.Nothing
    }

-- | The name of the preset. We recommend that the name be unique within the AWS account, but uniqueness is not enforced.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpName :: Lens.Lens' CreatePreset Types.Name
cpName = Lens.field @"name"
{-# DEPRECATED cpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The container type for the output file. Valid values include @flac@ , @flv@ , @fmp4@ , @gif@ , @mp3@ , @mp4@ , @mpg@ , @mxf@ , @oga@ , @ogg@ , @ts@ , and @webm@ .
--
-- /Note:/ Consider using 'container' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpContainer :: Lens.Lens' CreatePreset Types.Container
cpContainer = Lens.field @"container"
{-# DEPRECATED cpContainer "Use generic-lens or generic-optics with 'container' instead." #-}

-- | A section of the request body that specifies the audio parameters.
--
-- /Note:/ Consider using 'audio' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpAudio :: Lens.Lens' CreatePreset (Core.Maybe Types.AudioParameters)
cpAudio = Lens.field @"audio"
{-# DEPRECATED cpAudio "Use generic-lens or generic-optics with 'audio' instead." #-}

-- | A description of the preset.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpDescription :: Lens.Lens' CreatePreset (Core.Maybe Types.Description)
cpDescription = Lens.field @"description"
{-# DEPRECATED cpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A section of the request body that specifies the thumbnail parameters, if any.
--
-- /Note:/ Consider using 'thumbnails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpThumbnails :: Lens.Lens' CreatePreset (Core.Maybe Types.Thumbnails)
cpThumbnails = Lens.field @"thumbnails"
{-# DEPRECATED cpThumbnails "Use generic-lens or generic-optics with 'thumbnails' instead." #-}

-- | A section of the request body that specifies the video parameters.
--
-- /Note:/ Consider using 'video' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpVideo :: Lens.Lens' CreatePreset (Core.Maybe Types.VideoParameters)
cpVideo = Lens.field @"video"
{-# DEPRECATED cpVideo "Use generic-lens or generic-optics with 'video' instead." #-}

instance Core.FromJSON CreatePreset where
  toJSON CreatePreset {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("Container" Core..= container),
            ("Audio" Core..=) Core.<$> audio,
            ("Description" Core..=) Core.<$> description,
            ("Thumbnails" Core..=) Core.<$> thumbnails,
            ("Video" Core..=) Core.<$> video
          ]
      )

instance Core.AWSRequest CreatePreset where
  type Rs CreatePreset = CreatePresetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/2012-09-25/presets",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePresetResponse'
            Core.<$> (x Core..:? "Preset")
            Core.<*> (x Core..:? "Warning")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The @CreatePresetResponse@ structure.
--
-- /See:/ 'mkCreatePresetResponse' smart constructor.
data CreatePresetResponse = CreatePresetResponse'
  { -- | A section of the response body that provides information about the preset that is created.
    preset :: Core.Maybe Types.Preset,
    -- | If the preset settings don't comply with the standards for the video codec but Elastic Transcoder created the preset, this message explains the reason the preset settings don't meet the standard. Elastic Transcoder created the preset because the settings might produce acceptable output.
    warning :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePresetResponse' value with any optional fields omitted.
mkCreatePresetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreatePresetResponse
mkCreatePresetResponse responseStatus =
  CreatePresetResponse'
    { preset = Core.Nothing,
      warning = Core.Nothing,
      responseStatus
    }

-- | A section of the response body that provides information about the preset that is created.
--
-- /Note:/ Consider using 'preset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsPreset :: Lens.Lens' CreatePresetResponse (Core.Maybe Types.Preset)
cprrsPreset = Lens.field @"preset"
{-# DEPRECATED cprrsPreset "Use generic-lens or generic-optics with 'preset' instead." #-}

-- | If the preset settings don't comply with the standards for the video codec but Elastic Transcoder created the preset, this message explains the reason the preset settings don't meet the standard. Elastic Transcoder created the preset because the settings might produce acceptable output.
--
-- /Note:/ Consider using 'warning' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsWarning :: Lens.Lens' CreatePresetResponse (Core.Maybe Types.String)
cprrsWarning = Lens.field @"warning"
{-# DEPRECATED cprrsWarning "Use generic-lens or generic-optics with 'warning' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprrsResponseStatus :: Lens.Lens' CreatePresetResponse Core.Int
cprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
