{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.ElasticTranscoder.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The @CreatePresetRequest@ structure.
--
-- /See:/ 'newCreatePreset' smart constructor.
data CreatePreset = CreatePreset'
  { -- | A section of the request body that specifies the thumbnail parameters,
    -- if any.
    thumbnails :: Prelude.Maybe Thumbnails,
    -- | A section of the request body that specifies the video parameters.
    video :: Prelude.Maybe VideoParameters,
    -- | A section of the request body that specifies the audio parameters.
    audio :: Prelude.Maybe AudioParameters,
    -- | A description of the preset.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the preset. We recommend that the name be unique within the
    -- AWS account, but uniqueness is not enforced.
    name :: Prelude.Text,
    -- | The container type for the output file. Valid values include @flac@,
    -- @flv@, @fmp4@, @gif@, @mp3@, @mp4@, @mpg@, @mxf@, @oga@, @ogg@, @ts@,
    -- and @webm@.
    container :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'container'
  Prelude.Text ->
  CreatePreset
newCreatePreset pName_ pContainer_ =
  CreatePreset'
    { thumbnails = Prelude.Nothing,
      video = Prelude.Nothing,
      audio = Prelude.Nothing,
      description = Prelude.Nothing,
      name = pName_,
      container = pContainer_
    }

-- | A section of the request body that specifies the thumbnail parameters,
-- if any.
createPreset_thumbnails :: Lens.Lens' CreatePreset (Prelude.Maybe Thumbnails)
createPreset_thumbnails = Lens.lens (\CreatePreset' {thumbnails} -> thumbnails) (\s@CreatePreset' {} a -> s {thumbnails = a} :: CreatePreset)

-- | A section of the request body that specifies the video parameters.
createPreset_video :: Lens.Lens' CreatePreset (Prelude.Maybe VideoParameters)
createPreset_video = Lens.lens (\CreatePreset' {video} -> video) (\s@CreatePreset' {} a -> s {video = a} :: CreatePreset)

-- | A section of the request body that specifies the audio parameters.
createPreset_audio :: Lens.Lens' CreatePreset (Prelude.Maybe AudioParameters)
createPreset_audio = Lens.lens (\CreatePreset' {audio} -> audio) (\s@CreatePreset' {} a -> s {audio = a} :: CreatePreset)

-- | A description of the preset.
createPreset_description :: Lens.Lens' CreatePreset (Prelude.Maybe Prelude.Text)
createPreset_description = Lens.lens (\CreatePreset' {description} -> description) (\s@CreatePreset' {} a -> s {description = a} :: CreatePreset)

-- | The name of the preset. We recommend that the name be unique within the
-- AWS account, but uniqueness is not enforced.
createPreset_name :: Lens.Lens' CreatePreset Prelude.Text
createPreset_name = Lens.lens (\CreatePreset' {name} -> name) (\s@CreatePreset' {} a -> s {name = a} :: CreatePreset)

-- | The container type for the output file. Valid values include @flac@,
-- @flv@, @fmp4@, @gif@, @mp3@, @mp4@, @mpg@, @mxf@, @oga@, @ogg@, @ts@,
-- and @webm@.
createPreset_container :: Lens.Lens' CreatePreset Prelude.Text
createPreset_container = Lens.lens (\CreatePreset' {container} -> container) (\s@CreatePreset' {} a -> s {container = a} :: CreatePreset)

instance Prelude.AWSRequest CreatePreset where
  type Rs CreatePreset = CreatePresetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePresetResponse'
            Prelude.<$> (x Prelude..?> "Preset")
            Prelude.<*> (x Prelude..?> "Warning")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePreset

instance Prelude.NFData CreatePreset

instance Prelude.ToHeaders CreatePreset where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON CreatePreset where
  toJSON CreatePreset' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Thumbnails" Prelude..=) Prelude.<$> thumbnails,
            ("Video" Prelude..=) Prelude.<$> video,
            ("Audio" Prelude..=) Prelude.<$> audio,
            ("Description" Prelude..=) Prelude.<$> description,
            Prelude.Just ("Name" Prelude..= name),
            Prelude.Just ("Container" Prelude..= container)
          ]
      )

instance Prelude.ToPath CreatePreset where
  toPath = Prelude.const "/2012-09-25/presets"

instance Prelude.ToQuery CreatePreset where
  toQuery = Prelude.const Prelude.mempty

-- | The @CreatePresetResponse@ structure.
--
-- /See:/ 'newCreatePresetResponse' smart constructor.
data CreatePresetResponse = CreatePresetResponse'
  { -- | A section of the response body that provides information about the
    -- preset that is created.
    preset :: Prelude.Maybe Preset,
    -- | If the preset settings don\'t comply with the standards for the video
    -- codec but Elastic Transcoder created the preset, this message explains
    -- the reason the preset settings don\'t meet the standard. Elastic
    -- Transcoder created the preset because the settings might produce
    -- acceptable output.
    warning :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CreatePresetResponse
newCreatePresetResponse pHttpStatus_ =
  CreatePresetResponse'
    { preset = Prelude.Nothing,
      warning = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A section of the response body that provides information about the
-- preset that is created.
createPresetResponse_preset :: Lens.Lens' CreatePresetResponse (Prelude.Maybe Preset)
createPresetResponse_preset = Lens.lens (\CreatePresetResponse' {preset} -> preset) (\s@CreatePresetResponse' {} a -> s {preset = a} :: CreatePresetResponse)

-- | If the preset settings don\'t comply with the standards for the video
-- codec but Elastic Transcoder created the preset, this message explains
-- the reason the preset settings don\'t meet the standard. Elastic
-- Transcoder created the preset because the settings might produce
-- acceptable output.
createPresetResponse_warning :: Lens.Lens' CreatePresetResponse (Prelude.Maybe Prelude.Text)
createPresetResponse_warning = Lens.lens (\CreatePresetResponse' {warning} -> warning) (\s@CreatePresetResponse' {} a -> s {warning = a} :: CreatePresetResponse)

-- | The response's http status code.
createPresetResponse_httpStatus :: Lens.Lens' CreatePresetResponse Prelude.Int
createPresetResponse_httpStatus = Lens.lens (\CreatePresetResponse' {httpStatus} -> httpStatus) (\s@CreatePresetResponse' {} a -> s {httpStatus = a} :: CreatePresetResponse)

instance Prelude.NFData CreatePresetResponse
