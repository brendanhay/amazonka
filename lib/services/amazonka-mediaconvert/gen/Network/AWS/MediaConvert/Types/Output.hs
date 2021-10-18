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
-- Module      : Network.AWS.MediaConvert.Types.Output
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Output where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.AudioDescription
import Network.AWS.MediaConvert.Types.CaptionDescription
import Network.AWS.MediaConvert.Types.ContainerSettings
import Network.AWS.MediaConvert.Types.OutputSettings
import Network.AWS.MediaConvert.Types.VideoDescription
import qualified Network.AWS.Prelude as Prelude

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
    -- | Use Preset (Preset) to specify a preset for your transcoding settings.
    -- Provide the system or custom preset name. You can specify either Preset
    -- (Preset) or Container settings (ContainerSettings), but not both.
    preset :: Prelude.Maybe Prelude.Text,
    -- | Container specific settings.
    containerSettings :: Prelude.Maybe ContainerSettings,
    -- | VideoDescription contains a group of video encoding settings. The
    -- specific video settings depend on the video codec that you choose for
    -- the property codec. Include one instance of VideoDescription per output.
    videoDescription :: Prelude.Maybe VideoDescription,
    -- | Use Extension (Extension) to specify the file extension for outputs in
    -- File output groups. If you do not specify a value, the service will use
    -- default extensions by container type as follows * MPEG-2 transport
    -- stream, m2ts * Quicktime, mov * MXF container, mxf * MPEG-4 container,
    -- mp4 * WebM container, webm * No Container, the service will use codec
    -- extensions (e.g. AAC, H265, H265, AC3)
    extension :: Prelude.Maybe Prelude.Text,
    -- | (CaptionDescriptions) contains groups of captions settings. For each
    -- output that has captions, include one instance of (CaptionDescriptions).
    -- (CaptionDescriptions) can contain multiple groups of captions settings.
    captionDescriptions :: Prelude.Maybe [CaptionDescription],
    -- | Use Name modifier (NameModifier) to have the service add a string to the
    -- end of each output filename. You specify the base filename as part of
    -- your destination URI. When you create multiple outputs in the same
    -- output group, Name modifier (NameModifier) is required. Name modifier
    -- also accepts format identifiers. For DASH ISO outputs, if you use the
    -- format identifiers $Number$ or $Time$ in one output, you must use them
    -- in the same way in all outputs of the output group.
    nameModifier :: Prelude.Maybe Prelude.Text,
    -- | Specific settings for this type of output.
    outputSettings :: Prelude.Maybe OutputSettings
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
-- 'preset', 'output_preset' - Use Preset (Preset) to specify a preset for your transcoding settings.
-- Provide the system or custom preset name. You can specify either Preset
-- (Preset) or Container settings (ContainerSettings), but not both.
--
-- 'containerSettings', 'output_containerSettings' - Container specific settings.
--
-- 'videoDescription', 'output_videoDescription' - VideoDescription contains a group of video encoding settings. The
-- specific video settings depend on the video codec that you choose for
-- the property codec. Include one instance of VideoDescription per output.
--
-- 'extension', 'output_extension' - Use Extension (Extension) to specify the file extension for outputs in
-- File output groups. If you do not specify a value, the service will use
-- default extensions by container type as follows * MPEG-2 transport
-- stream, m2ts * Quicktime, mov * MXF container, mxf * MPEG-4 container,
-- mp4 * WebM container, webm * No Container, the service will use codec
-- extensions (e.g. AAC, H265, H265, AC3)
--
-- 'captionDescriptions', 'output_captionDescriptions' - (CaptionDescriptions) contains groups of captions settings. For each
-- output that has captions, include one instance of (CaptionDescriptions).
-- (CaptionDescriptions) can contain multiple groups of captions settings.
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
newOutput ::
  Output
newOutput =
  Output'
    { audioDescriptions = Prelude.Nothing,
      preset = Prelude.Nothing,
      containerSettings = Prelude.Nothing,
      videoDescription = Prelude.Nothing,
      extension = Prelude.Nothing,
      captionDescriptions = Prelude.Nothing,
      nameModifier = Prelude.Nothing,
      outputSettings = Prelude.Nothing
    }

-- | (AudioDescriptions) contains groups of audio encoding settings organized
-- by audio codec. Include one instance of (AudioDescriptions) per output.
-- (AudioDescriptions) can contain multiple groups of encoding settings.
output_audioDescriptions :: Lens.Lens' Output (Prelude.Maybe [AudioDescription])
output_audioDescriptions = Lens.lens (\Output' {audioDescriptions} -> audioDescriptions) (\s@Output' {} a -> s {audioDescriptions = a} :: Output) Prelude.. Lens.mapping Lens._Coerce

-- | Use Preset (Preset) to specify a preset for your transcoding settings.
-- Provide the system or custom preset name. You can specify either Preset
-- (Preset) or Container settings (ContainerSettings), but not both.
output_preset :: Lens.Lens' Output (Prelude.Maybe Prelude.Text)
output_preset = Lens.lens (\Output' {preset} -> preset) (\s@Output' {} a -> s {preset = a} :: Output)

-- | Container specific settings.
output_containerSettings :: Lens.Lens' Output (Prelude.Maybe ContainerSettings)
output_containerSettings = Lens.lens (\Output' {containerSettings} -> containerSettings) (\s@Output' {} a -> s {containerSettings = a} :: Output)

-- | VideoDescription contains a group of video encoding settings. The
-- specific video settings depend on the video codec that you choose for
-- the property codec. Include one instance of VideoDescription per output.
output_videoDescription :: Lens.Lens' Output (Prelude.Maybe VideoDescription)
output_videoDescription = Lens.lens (\Output' {videoDescription} -> videoDescription) (\s@Output' {} a -> s {videoDescription = a} :: Output)

-- | Use Extension (Extension) to specify the file extension for outputs in
-- File output groups. If you do not specify a value, the service will use
-- default extensions by container type as follows * MPEG-2 transport
-- stream, m2ts * Quicktime, mov * MXF container, mxf * MPEG-4 container,
-- mp4 * WebM container, webm * No Container, the service will use codec
-- extensions (e.g. AAC, H265, H265, AC3)
output_extension :: Lens.Lens' Output (Prelude.Maybe Prelude.Text)
output_extension = Lens.lens (\Output' {extension} -> extension) (\s@Output' {} a -> s {extension = a} :: Output)

-- | (CaptionDescriptions) contains groups of captions settings. For each
-- output that has captions, include one instance of (CaptionDescriptions).
-- (CaptionDescriptions) can contain multiple groups of captions settings.
output_captionDescriptions :: Lens.Lens' Output (Prelude.Maybe [CaptionDescription])
output_captionDescriptions = Lens.lens (\Output' {captionDescriptions} -> captionDescriptions) (\s@Output' {} a -> s {captionDescriptions = a} :: Output) Prelude.. Lens.mapping Lens._Coerce

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

instance Core.FromJSON Output where
  parseJSON =
    Core.withObject
      "Output"
      ( \x ->
          Output'
            Prelude.<$> ( x Core..:? "audioDescriptions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "preset")
            Prelude.<*> (x Core..:? "containerSettings")
            Prelude.<*> (x Core..:? "videoDescription")
            Prelude.<*> (x Core..:? "extension")
            Prelude.<*> ( x Core..:? "captionDescriptions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "nameModifier")
            Prelude.<*> (x Core..:? "outputSettings")
      )

instance Prelude.Hashable Output

instance Prelude.NFData Output

instance Core.ToJSON Output where
  toJSON Output' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("audioDescriptions" Core..=)
              Prelude.<$> audioDescriptions,
            ("preset" Core..=) Prelude.<$> preset,
            ("containerSettings" Core..=)
              Prelude.<$> containerSettings,
            ("videoDescription" Core..=)
              Prelude.<$> videoDescription,
            ("extension" Core..=) Prelude.<$> extension,
            ("captionDescriptions" Core..=)
              Prelude.<$> captionDescriptions,
            ("nameModifier" Core..=) Prelude.<$> nameModifier,
            ("outputSettings" Core..=)
              Prelude.<$> outputSettings
          ]
      )
