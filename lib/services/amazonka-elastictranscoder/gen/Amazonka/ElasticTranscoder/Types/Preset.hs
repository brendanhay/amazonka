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
-- Module      : Amazonka.ElasticTranscoder.Types.Preset
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticTranscoder.Types.Preset where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ElasticTranscoder.Types.AudioParameters
import Amazonka.ElasticTranscoder.Types.Thumbnails
import Amazonka.ElasticTranscoder.Types.VideoParameters
import qualified Amazonka.Prelude as Prelude

-- | Presets are templates that contain most of the settings for transcoding
-- media files from one format to another. Elastic Transcoder includes some
-- default presets for common formats, for example, several iPod and iPhone
-- versions. You can also create your own presets for formats that aren\'t
-- included among the default presets. You specify which preset you want to
-- use when you create a job.
--
-- /See:/ 'newPreset' smart constructor.
data Preset = Preset'
  { -- | The name of the preset.
    name :: Prelude.Maybe Prelude.Text,
    -- | Whether the preset is a default preset provided by Elastic Transcoder
    -- (@System@) or a preset that you have defined (@Custom@).
    type' :: Prelude.Maybe Prelude.Text,
    -- | A section of the response body that provides information about the audio
    -- preset values.
    audio :: Prelude.Maybe AudioParameters,
    -- | The Amazon Resource Name (ARN) for the preset.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Identifier for the new preset. You use this value to get settings for
    -- the preset or to delete it.
    id :: Prelude.Maybe Prelude.Text,
    -- | A description of the preset.
    description :: Prelude.Maybe Prelude.Text,
    -- | The container type for the output file. Valid values include @flac@,
    -- @flv@, @fmp4@, @gif@, @mp3@, @mp4@, @mpg@, @mxf@, @oga@, @ogg@, @ts@,
    -- and @webm@.
    container :: Prelude.Maybe Prelude.Text,
    -- | A section of the response body that provides information about the
    -- thumbnail preset values, if any.
    thumbnails :: Prelude.Maybe Thumbnails,
    -- | A section of the response body that provides information about the video
    -- preset values.
    video :: Prelude.Maybe VideoParameters
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Preset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'preset_name' - The name of the preset.
--
-- 'type'', 'preset_type' - Whether the preset is a default preset provided by Elastic Transcoder
-- (@System@) or a preset that you have defined (@Custom@).
--
-- 'audio', 'preset_audio' - A section of the response body that provides information about the audio
-- preset values.
--
-- 'arn', 'preset_arn' - The Amazon Resource Name (ARN) for the preset.
--
-- 'id', 'preset_id' - Identifier for the new preset. You use this value to get settings for
-- the preset or to delete it.
--
-- 'description', 'preset_description' - A description of the preset.
--
-- 'container', 'preset_container' - The container type for the output file. Valid values include @flac@,
-- @flv@, @fmp4@, @gif@, @mp3@, @mp4@, @mpg@, @mxf@, @oga@, @ogg@, @ts@,
-- and @webm@.
--
-- 'thumbnails', 'preset_thumbnails' - A section of the response body that provides information about the
-- thumbnail preset values, if any.
--
-- 'video', 'preset_video' - A section of the response body that provides information about the video
-- preset values.
newPreset ::
  Preset
newPreset =
  Preset'
    { name = Prelude.Nothing,
      type' = Prelude.Nothing,
      audio = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing,
      container = Prelude.Nothing,
      thumbnails = Prelude.Nothing,
      video = Prelude.Nothing
    }

-- | The name of the preset.
preset_name :: Lens.Lens' Preset (Prelude.Maybe Prelude.Text)
preset_name = Lens.lens (\Preset' {name} -> name) (\s@Preset' {} a -> s {name = a} :: Preset)

-- | Whether the preset is a default preset provided by Elastic Transcoder
-- (@System@) or a preset that you have defined (@Custom@).
preset_type :: Lens.Lens' Preset (Prelude.Maybe Prelude.Text)
preset_type = Lens.lens (\Preset' {type'} -> type') (\s@Preset' {} a -> s {type' = a} :: Preset)

-- | A section of the response body that provides information about the audio
-- preset values.
preset_audio :: Lens.Lens' Preset (Prelude.Maybe AudioParameters)
preset_audio = Lens.lens (\Preset' {audio} -> audio) (\s@Preset' {} a -> s {audio = a} :: Preset)

-- | The Amazon Resource Name (ARN) for the preset.
preset_arn :: Lens.Lens' Preset (Prelude.Maybe Prelude.Text)
preset_arn = Lens.lens (\Preset' {arn} -> arn) (\s@Preset' {} a -> s {arn = a} :: Preset)

-- | Identifier for the new preset. You use this value to get settings for
-- the preset or to delete it.
preset_id :: Lens.Lens' Preset (Prelude.Maybe Prelude.Text)
preset_id = Lens.lens (\Preset' {id} -> id) (\s@Preset' {} a -> s {id = a} :: Preset)

-- | A description of the preset.
preset_description :: Lens.Lens' Preset (Prelude.Maybe Prelude.Text)
preset_description = Lens.lens (\Preset' {description} -> description) (\s@Preset' {} a -> s {description = a} :: Preset)

-- | The container type for the output file. Valid values include @flac@,
-- @flv@, @fmp4@, @gif@, @mp3@, @mp4@, @mpg@, @mxf@, @oga@, @ogg@, @ts@,
-- and @webm@.
preset_container :: Lens.Lens' Preset (Prelude.Maybe Prelude.Text)
preset_container = Lens.lens (\Preset' {container} -> container) (\s@Preset' {} a -> s {container = a} :: Preset)

-- | A section of the response body that provides information about the
-- thumbnail preset values, if any.
preset_thumbnails :: Lens.Lens' Preset (Prelude.Maybe Thumbnails)
preset_thumbnails = Lens.lens (\Preset' {thumbnails} -> thumbnails) (\s@Preset' {} a -> s {thumbnails = a} :: Preset)

-- | A section of the response body that provides information about the video
-- preset values.
preset_video :: Lens.Lens' Preset (Prelude.Maybe VideoParameters)
preset_video = Lens.lens (\Preset' {video} -> video) (\s@Preset' {} a -> s {video = a} :: Preset)

instance Core.FromJSON Preset where
  parseJSON =
    Core.withObject
      "Preset"
      ( \x ->
          Preset'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "Audio")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "Container")
            Prelude.<*> (x Core..:? "Thumbnails")
            Prelude.<*> (x Core..:? "Video")
      )

instance Prelude.Hashable Preset where
  hashWithSalt _salt Preset' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` audio
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` container
      `Prelude.hashWithSalt` thumbnails
      `Prelude.hashWithSalt` video

instance Prelude.NFData Preset where
  rnf Preset' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf audio
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf container
      `Prelude.seq` Prelude.rnf thumbnails
      `Prelude.seq` Prelude.rnf video
