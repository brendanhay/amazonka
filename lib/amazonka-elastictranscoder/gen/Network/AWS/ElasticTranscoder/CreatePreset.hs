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
    cpVideo,
    cpThumbnails,
    cpName,
    cpContainer,
    cpDescription,
    cpAudio,

    -- * Destructuring the response
    CreatePresetResponse (..),
    mkCreatePresetResponse,

    -- ** Response lenses
    cprsWarning,
    cprsPreset,
    cprsResponseStatus,
  )
where

import Network.AWS.ElasticTranscoder.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The @CreatePresetRequest@ structure.
--
-- /See:/ 'mkCreatePreset' smart constructor.
data CreatePreset = CreatePreset'
  { -- | A section of the request body that specifies the video parameters.
    video :: Lude.Maybe VideoParameters,
    -- | A section of the request body that specifies the thumbnail parameters, if any.
    thumbnails :: Lude.Maybe Thumbnails,
    -- | The name of the preset. We recommend that the name be unique within the AWS account, but uniqueness is not enforced.
    name :: Lude.Text,
    -- | The container type for the output file. Valid values include @flac@ , @flv@ , @fmp4@ , @gif@ , @mp3@ , @mp4@ , @mpg@ , @mxf@ , @oga@ , @ogg@ , @ts@ , and @webm@ .
    container :: Lude.Text,
    -- | A description of the preset.
    description :: Lude.Maybe Lude.Text,
    -- | A section of the request body that specifies the audio parameters.
    audio :: Lude.Maybe AudioParameters
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePreset' with the minimum fields required to make a request.
--
-- * 'video' - A section of the request body that specifies the video parameters.
-- * 'thumbnails' - A section of the request body that specifies the thumbnail parameters, if any.
-- * 'name' - The name of the preset. We recommend that the name be unique within the AWS account, but uniqueness is not enforced.
-- * 'container' - The container type for the output file. Valid values include @flac@ , @flv@ , @fmp4@ , @gif@ , @mp3@ , @mp4@ , @mpg@ , @mxf@ , @oga@ , @ogg@ , @ts@ , and @webm@ .
-- * 'description' - A description of the preset.
-- * 'audio' - A section of the request body that specifies the audio parameters.
mkCreatePreset ::
  -- | 'name'
  Lude.Text ->
  -- | 'container'
  Lude.Text ->
  CreatePreset
mkCreatePreset pName_ pContainer_ =
  CreatePreset'
    { video = Lude.Nothing,
      thumbnails = Lude.Nothing,
      name = pName_,
      container = pContainer_,
      description = Lude.Nothing,
      audio = Lude.Nothing
    }

-- | A section of the request body that specifies the video parameters.
--
-- /Note:/ Consider using 'video' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpVideo :: Lens.Lens' CreatePreset (Lude.Maybe VideoParameters)
cpVideo = Lens.lens (video :: CreatePreset -> Lude.Maybe VideoParameters) (\s a -> s {video = a} :: CreatePreset)
{-# DEPRECATED cpVideo "Use generic-lens or generic-optics with 'video' instead." #-}

-- | A section of the request body that specifies the thumbnail parameters, if any.
--
-- /Note:/ Consider using 'thumbnails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpThumbnails :: Lens.Lens' CreatePreset (Lude.Maybe Thumbnails)
cpThumbnails = Lens.lens (thumbnails :: CreatePreset -> Lude.Maybe Thumbnails) (\s a -> s {thumbnails = a} :: CreatePreset)
{-# DEPRECATED cpThumbnails "Use generic-lens or generic-optics with 'thumbnails' instead." #-}

-- | The name of the preset. We recommend that the name be unique within the AWS account, but uniqueness is not enforced.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpName :: Lens.Lens' CreatePreset Lude.Text
cpName = Lens.lens (name :: CreatePreset -> Lude.Text) (\s a -> s {name = a} :: CreatePreset)
{-# DEPRECATED cpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The container type for the output file. Valid values include @flac@ , @flv@ , @fmp4@ , @gif@ , @mp3@ , @mp4@ , @mpg@ , @mxf@ , @oga@ , @ogg@ , @ts@ , and @webm@ .
--
-- /Note:/ Consider using 'container' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpContainer :: Lens.Lens' CreatePreset Lude.Text
cpContainer = Lens.lens (container :: CreatePreset -> Lude.Text) (\s a -> s {container = a} :: CreatePreset)
{-# DEPRECATED cpContainer "Use generic-lens or generic-optics with 'container' instead." #-}

-- | A description of the preset.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpDescription :: Lens.Lens' CreatePreset (Lude.Maybe Lude.Text)
cpDescription = Lens.lens (description :: CreatePreset -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreatePreset)
{-# DEPRECATED cpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A section of the request body that specifies the audio parameters.
--
-- /Note:/ Consider using 'audio' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpAudio :: Lens.Lens' CreatePreset (Lude.Maybe AudioParameters)
cpAudio = Lens.lens (audio :: CreatePreset -> Lude.Maybe AudioParameters) (\s a -> s {audio = a} :: CreatePreset)
{-# DEPRECATED cpAudio "Use generic-lens or generic-optics with 'audio' instead." #-}

instance Lude.AWSRequest CreatePreset where
  type Rs CreatePreset = CreatePresetResponse
  request = Req.postJSON elasticTranscoderService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreatePresetResponse'
            Lude.<$> (x Lude..?> "Warning")
            Lude.<*> (x Lude..?> "Preset")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreatePreset where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreatePreset where
  toJSON CreatePreset' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Video" Lude..=) Lude.<$> video,
            ("Thumbnails" Lude..=) Lude.<$> thumbnails,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("Container" Lude..= container),
            ("Description" Lude..=) Lude.<$> description,
            ("Audio" Lude..=) Lude.<$> audio
          ]
      )

instance Lude.ToPath CreatePreset where
  toPath = Lude.const "/2012-09-25/presets"

instance Lude.ToQuery CreatePreset where
  toQuery = Lude.const Lude.mempty

-- | The @CreatePresetResponse@ structure.
--
-- /See:/ 'mkCreatePresetResponse' smart constructor.
data CreatePresetResponse = CreatePresetResponse'
  { -- | If the preset settings don't comply with the standards for the video codec but Elastic Transcoder created the preset, this message explains the reason the preset settings don't meet the standard. Elastic Transcoder created the preset because the settings might produce acceptable output.
    warning :: Lude.Maybe Lude.Text,
    -- | A section of the response body that provides information about the preset that is created.
    preset :: Lude.Maybe Preset,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePresetResponse' with the minimum fields required to make a request.
--
-- * 'warning' - If the preset settings don't comply with the standards for the video codec but Elastic Transcoder created the preset, this message explains the reason the preset settings don't meet the standard. Elastic Transcoder created the preset because the settings might produce acceptable output.
-- * 'preset' - A section of the response body that provides information about the preset that is created.
-- * 'responseStatus' - The response status code.
mkCreatePresetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreatePresetResponse
mkCreatePresetResponse pResponseStatus_ =
  CreatePresetResponse'
    { warning = Lude.Nothing,
      preset = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the preset settings don't comply with the standards for the video codec but Elastic Transcoder created the preset, this message explains the reason the preset settings don't meet the standard. Elastic Transcoder created the preset because the settings might produce acceptable output.
--
-- /Note:/ Consider using 'warning' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsWarning :: Lens.Lens' CreatePresetResponse (Lude.Maybe Lude.Text)
cprsWarning = Lens.lens (warning :: CreatePresetResponse -> Lude.Maybe Lude.Text) (\s a -> s {warning = a} :: CreatePresetResponse)
{-# DEPRECATED cprsWarning "Use generic-lens or generic-optics with 'warning' instead." #-}

-- | A section of the response body that provides information about the preset that is created.
--
-- /Note:/ Consider using 'preset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsPreset :: Lens.Lens' CreatePresetResponse (Lude.Maybe Preset)
cprsPreset = Lens.lens (preset :: CreatePresetResponse -> Lude.Maybe Preset) (\s a -> s {preset = a} :: CreatePresetResponse)
{-# DEPRECATED cprsPreset "Use generic-lens or generic-optics with 'preset' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cprsResponseStatus :: Lens.Lens' CreatePresetResponse Lude.Int
cprsResponseStatus = Lens.lens (responseStatus :: CreatePresetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreatePresetResponse)
{-# DEPRECATED cprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
