{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.CreatePreset
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The CreatePreset operation creates a preset with settings that you
-- specify.
--
-- Elastic Transcoder checks the CreatePreset settings to ensure that they
-- meet Elastic Transcoder requirements and to determine whether they
-- comply with H.264 standards. If your settings are not valid for Elastic
-- Transcoder, Elastic Transcoder returns an HTTP 400 response
-- (@ValidationException@) and does not create the preset. If the settings
-- are valid for Elastic Transcoder but aren\'t strictly compliant with the
-- H.264 standard, Elastic Transcoder creates the preset and returns a
-- warning message in the response. This helps you determine whether your
-- settings comply with the H.264 standard while giving you greater
-- flexibility with respect to the video that Elastic Transcoder produces.
--
-- Elastic Transcoder uses the H.264 video-compression format. For more
-- information, see the International Telecommunication Union publication
-- /Recommendation ITU-T H.264: Advanced video coding for generic
-- audiovisual services/.
module Network.AWS.ElasticTranscoder.CreatePreset
  ( -- * Creating a Request
    CreatePreset (..),
    newCreatePreset,

    -- * Request Lenses
    createPreset_thumbnails,
    createPreset_video,
    createPreset_audio,
    createPreset_description,
    createPreset_name,
    createPreset_container,

    -- * Destructuring the Response
    CreatePresetResponse (..),
    newCreatePresetResponse,

    -- * Response Lenses
    createPresetResponse_preset,
    createPresetResponse_warning,
    createPresetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticTranscoder.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The @CreatePresetRequest@ structure.
--
-- /See:/ 'newCreatePreset' smart constructor.
data CreatePreset = CreatePreset'
  { -- | A section of the request body that specifies the thumbnail parameters,
    -- if any.
    thumbnails :: Core.Maybe Thumbnails,
    -- | A section of the request body that specifies the video parameters.
    video :: Core.Maybe VideoParameters,
    -- | A section of the request body that specifies the audio parameters.
    audio :: Core.Maybe AudioParameters,
    -- | A description of the preset.
    description :: Core.Maybe Core.Text,
    -- | The name of the preset. We recommend that the name be unique within the
    -- AWS account, but uniqueness is not enforced.
    name :: Core.Text,
    -- | The container type for the output file. Valid values include @flac@,
    -- @flv@, @fmp4@, @gif@, @mp3@, @mp4@, @mpg@, @mxf@, @oga@, @ogg@, @ts@,
    -- and @webm@.
    container :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreatePreset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thumbnails', 'createPreset_thumbnails' - A section of the request body that specifies the thumbnail parameters,
-- if any.
--
-- 'video', 'createPreset_video' - A section of the request body that specifies the video parameters.
--
-- 'audio', 'createPreset_audio' - A section of the request body that specifies the audio parameters.
--
-- 'description', 'createPreset_description' - A description of the preset.
--
-- 'name', 'createPreset_name' - The name of the preset. We recommend that the name be unique within the
-- AWS account, but uniqueness is not enforced.
--
-- 'container', 'createPreset_container' - The container type for the output file. Valid values include @flac@,
-- @flv@, @fmp4@, @gif@, @mp3@, @mp4@, @mpg@, @mxf@, @oga@, @ogg@, @ts@,
-- and @webm@.
newCreatePreset ::
  -- | 'name'
  Core.Text ->
  -- | 'container'
  Core.Text ->
  CreatePreset
newCreatePreset pName_ pContainer_ =
  CreatePreset'
    { thumbnails = Core.Nothing,
      video = Core.Nothing,
      audio = Core.Nothing,
      description = Core.Nothing,
      name = pName_,
      container = pContainer_
    }

-- | A section of the request body that specifies the thumbnail parameters,
-- if any.
createPreset_thumbnails :: Lens.Lens' CreatePreset (Core.Maybe Thumbnails)
createPreset_thumbnails = Lens.lens (\CreatePreset' {thumbnails} -> thumbnails) (\s@CreatePreset' {} a -> s {thumbnails = a} :: CreatePreset)

-- | A section of the request body that specifies the video parameters.
createPreset_video :: Lens.Lens' CreatePreset (Core.Maybe VideoParameters)
createPreset_video = Lens.lens (\CreatePreset' {video} -> video) (\s@CreatePreset' {} a -> s {video = a} :: CreatePreset)

-- | A section of the request body that specifies the audio parameters.
createPreset_audio :: Lens.Lens' CreatePreset (Core.Maybe AudioParameters)
createPreset_audio = Lens.lens (\CreatePreset' {audio} -> audio) (\s@CreatePreset' {} a -> s {audio = a} :: CreatePreset)

-- | A description of the preset.
createPreset_description :: Lens.Lens' CreatePreset (Core.Maybe Core.Text)
createPreset_description = Lens.lens (\CreatePreset' {description} -> description) (\s@CreatePreset' {} a -> s {description = a} :: CreatePreset)

-- | The name of the preset. We recommend that the name be unique within the
-- AWS account, but uniqueness is not enforced.
createPreset_name :: Lens.Lens' CreatePreset Core.Text
createPreset_name = Lens.lens (\CreatePreset' {name} -> name) (\s@CreatePreset' {} a -> s {name = a} :: CreatePreset)

-- | The container type for the output file. Valid values include @flac@,
-- @flv@, @fmp4@, @gif@, @mp3@, @mp4@, @mpg@, @mxf@, @oga@, @ogg@, @ts@,
-- and @webm@.
createPreset_container :: Lens.Lens' CreatePreset Core.Text
createPreset_container = Lens.lens (\CreatePreset' {container} -> container) (\s@CreatePreset' {} a -> s {container = a} :: CreatePreset)

instance Core.AWSRequest CreatePreset where
  type AWSResponse CreatePreset = CreatePresetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePresetResponse'
            Core.<$> (x Core..?> "Preset")
            Core.<*> (x Core..?> "Warning")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreatePreset

instance Core.NFData CreatePreset

instance Core.ToHeaders CreatePreset where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON CreatePreset where
  toJSON CreatePreset' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Thumbnails" Core..=) Core.<$> thumbnails,
            ("Video" Core..=) Core.<$> video,
            ("Audio" Core..=) Core.<$> audio,
            ("Description" Core..=) Core.<$> description,
            Core.Just ("Name" Core..= name),
            Core.Just ("Container" Core..= container)
          ]
      )

instance Core.ToPath CreatePreset where
  toPath = Core.const "/2012-09-25/presets"

instance Core.ToQuery CreatePreset where
  toQuery = Core.const Core.mempty

-- | The @CreatePresetResponse@ structure.
--
-- /See:/ 'newCreatePresetResponse' smart constructor.
data CreatePresetResponse = CreatePresetResponse'
  { -- | A section of the response body that provides information about the
    -- preset that is created.
    preset :: Core.Maybe Preset,
    -- | If the preset settings don\'t comply with the standards for the video
    -- codec but Elastic Transcoder created the preset, this message explains
    -- the reason the preset settings don\'t meet the standard. Elastic
    -- Transcoder created the preset because the settings might produce
    -- acceptable output.
    warning :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreatePresetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'preset', 'createPresetResponse_preset' - A section of the response body that provides information about the
-- preset that is created.
--
-- 'warning', 'createPresetResponse_warning' - If the preset settings don\'t comply with the standards for the video
-- codec but Elastic Transcoder created the preset, this message explains
-- the reason the preset settings don\'t meet the standard. Elastic
-- Transcoder created the preset because the settings might produce
-- acceptable output.
--
-- 'httpStatus', 'createPresetResponse_httpStatus' - The response's http status code.
newCreatePresetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreatePresetResponse
newCreatePresetResponse pHttpStatus_ =
  CreatePresetResponse'
    { preset = Core.Nothing,
      warning = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A section of the response body that provides information about the
-- preset that is created.
createPresetResponse_preset :: Lens.Lens' CreatePresetResponse (Core.Maybe Preset)
createPresetResponse_preset = Lens.lens (\CreatePresetResponse' {preset} -> preset) (\s@CreatePresetResponse' {} a -> s {preset = a} :: CreatePresetResponse)

-- | If the preset settings don\'t comply with the standards for the video
-- codec but Elastic Transcoder created the preset, this message explains
-- the reason the preset settings don\'t meet the standard. Elastic
-- Transcoder created the preset because the settings might produce
-- acceptable output.
createPresetResponse_warning :: Lens.Lens' CreatePresetResponse (Core.Maybe Core.Text)
createPresetResponse_warning = Lens.lens (\CreatePresetResponse' {warning} -> warning) (\s@CreatePresetResponse' {} a -> s {warning = a} :: CreatePresetResponse)

-- | The response's http status code.
createPresetResponse_httpStatus :: Lens.Lens' CreatePresetResponse Core.Int
createPresetResponse_httpStatus = Lens.lens (\CreatePresetResponse' {httpStatus} -> httpStatus) (\s@CreatePresetResponse' {} a -> s {httpStatus = a} :: CreatePresetResponse)

instance Core.NFData CreatePresetResponse
