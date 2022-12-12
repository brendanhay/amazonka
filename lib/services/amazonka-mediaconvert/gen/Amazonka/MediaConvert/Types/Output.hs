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
-- Module      : Amazonka.MediaConvert.Types.Output
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Output where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.AudioDescription
import Amazonka.MediaConvert.Types.CaptionDescription
import Amazonka.MediaConvert.Types.ContainerSettings
import Amazonka.MediaConvert.Types.OutputSettings
import Amazonka.MediaConvert.Types.VideoDescription
import qualified Amazonka.Prelude as Prelude

-- | Each output in your job is a collection of settings that describes how
-- you want MediaConvert to encode a single output file or stream. For more
-- information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/create-outputs.html.
--
-- /See:/ 'newOutput' smart constructor.
data Output = Output'
  { -- | (AudioDescriptions) contains groups of audio encoding settings organized
    -- by audio codec. Include one instance of (AudioDescriptions) per output.
    -- (AudioDescriptions) can contain multiple groups of encoding settings.
    audioDescriptions :: Prelude.Maybe [AudioDescription],
    -- | (CaptionDescriptions) contains groups of captions settings. For each
    -- output that has captions, include one instance of (CaptionDescriptions).
    -- (CaptionDescriptions) can contain multiple groups of captions settings.
    captionDescriptions :: Prelude.Maybe [CaptionDescription],
    -- | Container specific settings.
    containerSettings :: Prelude.Maybe ContainerSettings,
    -- | Use Extension (Extension) to specify the file extension for outputs in
    -- File output groups. If you do not specify a value, the service will use
    -- default extensions by container type as follows * MPEG-2 transport
    -- stream, m2ts * Quicktime, mov * MXF container, mxf * MPEG-4 container,
    -- mp4 * WebM container, webm * No Container, the service will use codec
    -- extensions (e.g. AAC, H265, H265, AC3)
    extension :: Prelude.Maybe Prelude.Text,
    -- | Use Name modifier (NameModifier) to have the service add a string to the
    -- end of each output filename. You specify the base filename as part of
    -- your destination URI. When you create multiple outputs in the same
    -- output group, Name modifier (NameModifier) is required. Name modifier
    -- also accepts format identifiers. For DASH ISO outputs, if you use the
    -- format identifiers $Number$ or $Time$ in one output, you must use them
    -- in the same way in all outputs of the output group.
    nameModifier :: Prelude.Maybe Prelude.Text,
    -- | Specific settings for this type of output.
    outputSettings :: Prelude.Maybe OutputSettings,
    -- | Use Preset (Preset) to specify a preset for your transcoding settings.
    -- Provide the system or custom preset name. You can specify either Preset
    -- (Preset) or Container settings (ContainerSettings), but not both.
    preset :: Prelude.Maybe Prelude.Text,
    -- | VideoDescription contains a group of video encoding settings. The
    -- specific video settings depend on the video codec that you choose for
    -- the property codec. Include one instance of VideoDescription per output.
    videoDescription :: Prelude.Maybe VideoDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Output' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'audioDescriptions', 'output_audioDescriptions' - (AudioDescriptions) contains groups of audio encoding settings organized
-- by audio codec. Include one instance of (AudioDescriptions) per output.
-- (AudioDescriptions) can contain multiple groups of encoding settings.
--
-- 'captionDescriptions', 'output_captionDescriptions' - (CaptionDescriptions) contains groups of captions settings. For each
-- output that has captions, include one instance of (CaptionDescriptions).
-- (CaptionDescriptions) can contain multiple groups of captions settings.
--
-- 'containerSettings', 'output_containerSettings' - Container specific settings.
--
-- 'extension', 'output_extension' - Use Extension (Extension) to specify the file extension for outputs in
-- File output groups. If you do not specify a value, the service will use
-- default extensions by container type as follows * MPEG-2 transport
-- stream, m2ts * Quicktime, mov * MXF container, mxf * MPEG-4 container,
-- mp4 * WebM container, webm * No Container, the service will use codec
-- extensions (e.g. AAC, H265, H265, AC3)
--
-- 'nameModifier', 'output_nameModifier' - Use Name modifier (NameModifier) to have the service add a string to the
-- end of each output filename. You specify the base filename as part of
-- your destination URI. When you create multiple outputs in the same
-- output group, Name modifier (NameModifier) is required. Name modifier
-- also accepts format identifiers. For DASH ISO outputs, if you use the
-- format identifiers $Number$ or $Time$ in one output, you must use them
-- in the same way in all outputs of the output group.
--
-- 'outputSettings', 'output_outputSettings' - Specific settings for this type of output.
--
-- 'preset', 'output_preset' - Use Preset (Preset) to specify a preset for your transcoding settings.
-- Provide the system or custom preset name. You can specify either Preset
-- (Preset) or Container settings (ContainerSettings), but not both.
--
-- 'videoDescription', 'output_videoDescription' - VideoDescription contains a group of video encoding settings. The
-- specific video settings depend on the video codec that you choose for
-- the property codec. Include one instance of VideoDescription per output.
newOutput ::
  Output
newOutput =
  Output'
    { audioDescriptions = Prelude.Nothing,
      captionDescriptions = Prelude.Nothing,
      containerSettings = Prelude.Nothing,
      extension = Prelude.Nothing,
      nameModifier = Prelude.Nothing,
      outputSettings = Prelude.Nothing,
      preset = Prelude.Nothing,
      videoDescription = Prelude.Nothing
    }

-- | (AudioDescriptions) contains groups of audio encoding settings organized
-- by audio codec. Include one instance of (AudioDescriptions) per output.
-- (AudioDescriptions) can contain multiple groups of encoding settings.
output_audioDescriptions :: Lens.Lens' Output (Prelude.Maybe [AudioDescription])
output_audioDescriptions = Lens.lens (\Output' {audioDescriptions} -> audioDescriptions) (\s@Output' {} a -> s {audioDescriptions = a} :: Output) Prelude.. Lens.mapping Lens.coerced

-- | (CaptionDescriptions) contains groups of captions settings. For each
-- output that has captions, include one instance of (CaptionDescriptions).
-- (CaptionDescriptions) can contain multiple groups of captions settings.
output_captionDescriptions :: Lens.Lens' Output (Prelude.Maybe [CaptionDescription])
output_captionDescriptions = Lens.lens (\Output' {captionDescriptions} -> captionDescriptions) (\s@Output' {} a -> s {captionDescriptions = a} :: Output) Prelude.. Lens.mapping Lens.coerced

-- | Container specific settings.
output_containerSettings :: Lens.Lens' Output (Prelude.Maybe ContainerSettings)
output_containerSettings = Lens.lens (\Output' {containerSettings} -> containerSettings) (\s@Output' {} a -> s {containerSettings = a} :: Output)

-- | Use Extension (Extension) to specify the file extension for outputs in
-- File output groups. If you do not specify a value, the service will use
-- default extensions by container type as follows * MPEG-2 transport
-- stream, m2ts * Quicktime, mov * MXF container, mxf * MPEG-4 container,
-- mp4 * WebM container, webm * No Container, the service will use codec
-- extensions (e.g. AAC, H265, H265, AC3)
output_extension :: Lens.Lens' Output (Prelude.Maybe Prelude.Text)
output_extension = Lens.lens (\Output' {extension} -> extension) (\s@Output' {} a -> s {extension = a} :: Output)

-- | Use Name modifier (NameModifier) to have the service add a string to the
-- end of each output filename. You specify the base filename as part of
-- your destination URI. When you create multiple outputs in the same
-- output group, Name modifier (NameModifier) is required. Name modifier
-- also accepts format identifiers. For DASH ISO outputs, if you use the
-- format identifiers $Number$ or $Time$ in one output, you must use them
-- in the same way in all outputs of the output group.
output_nameModifier :: Lens.Lens' Output (Prelude.Maybe Prelude.Text)
output_nameModifier = Lens.lens (\Output' {nameModifier} -> nameModifier) (\s@Output' {} a -> s {nameModifier = a} :: Output)

-- | Specific settings for this type of output.
output_outputSettings :: Lens.Lens' Output (Prelude.Maybe OutputSettings)
output_outputSettings = Lens.lens (\Output' {outputSettings} -> outputSettings) (\s@Output' {} a -> s {outputSettings = a} :: Output)

-- | Use Preset (Preset) to specify a preset for your transcoding settings.
-- Provide the system or custom preset name. You can specify either Preset
-- (Preset) or Container settings (ContainerSettings), but not both.
output_preset :: Lens.Lens' Output (Prelude.Maybe Prelude.Text)
output_preset = Lens.lens (\Output' {preset} -> preset) (\s@Output' {} a -> s {preset = a} :: Output)

-- | VideoDescription contains a group of video encoding settings. The
-- specific video settings depend on the video codec that you choose for
-- the property codec. Include one instance of VideoDescription per output.
output_videoDescription :: Lens.Lens' Output (Prelude.Maybe VideoDescription)
output_videoDescription = Lens.lens (\Output' {videoDescription} -> videoDescription) (\s@Output' {} a -> s {videoDescription = a} :: Output)

instance Data.FromJSON Output where
  parseJSON =
    Data.withObject
      "Output"
      ( \x ->
          Output'
            Prelude.<$> ( x Data..:? "audioDescriptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "captionDescriptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "containerSettings")
            Prelude.<*> (x Data..:? "extension")
            Prelude.<*> (x Data..:? "nameModifier")
            Prelude.<*> (x Data..:? "outputSettings")
            Prelude.<*> (x Data..:? "preset")
            Prelude.<*> (x Data..:? "videoDescription")
      )

instance Prelude.Hashable Output where
  hashWithSalt _salt Output' {..} =
    _salt `Prelude.hashWithSalt` audioDescriptions
      `Prelude.hashWithSalt` captionDescriptions
      `Prelude.hashWithSalt` containerSettings
      `Prelude.hashWithSalt` extension
      `Prelude.hashWithSalt` nameModifier
      `Prelude.hashWithSalt` outputSettings
      `Prelude.hashWithSalt` preset
      `Prelude.hashWithSalt` videoDescription

instance Prelude.NFData Output where
  rnf Output' {..} =
    Prelude.rnf audioDescriptions
      `Prelude.seq` Prelude.rnf captionDescriptions
      `Prelude.seq` Prelude.rnf containerSettings
      `Prelude.seq` Prelude.rnf extension
      `Prelude.seq` Prelude.rnf nameModifier
      `Prelude.seq` Prelude.rnf outputSettings
      `Prelude.seq` Prelude.rnf preset
      `Prelude.seq` Prelude.rnf videoDescription

instance Data.ToJSON Output where
  toJSON Output' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("audioDescriptions" Data..=)
              Prelude.<$> audioDescriptions,
            ("captionDescriptions" Data..=)
              Prelude.<$> captionDescriptions,
            ("containerSettings" Data..=)
              Prelude.<$> containerSettings,
            ("extension" Data..=) Prelude.<$> extension,
            ("nameModifier" Data..=) Prelude.<$> nameModifier,
            ("outputSettings" Data..=)
              Prelude.<$> outputSettings,
            ("preset" Data..=) Prelude.<$> preset,
            ("videoDescription" Data..=)
              Prelude.<$> videoDescription
          ]
      )
