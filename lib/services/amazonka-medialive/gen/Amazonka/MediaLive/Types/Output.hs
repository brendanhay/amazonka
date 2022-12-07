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
-- Module      : Amazonka.MediaLive.Types.Output
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Output where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.OutputSettings
import qualified Amazonka.Prelude as Prelude

-- | Output settings. There can be multiple outputs within a group.
--
-- /See:/ 'newOutput' smart constructor.
data Output = Output'
  { -- | The name of the VideoDescription used as the source for this output.
    videoDescriptionName :: Prelude.Maybe Prelude.Text,
    -- | The names of the CaptionDescriptions used as caption sources for this
    -- output.
    captionDescriptionNames :: Prelude.Maybe [Prelude.Text],
    -- | The name used to identify an output.
    outputName :: Prelude.Maybe Prelude.Text,
    -- | The names of the AudioDescriptions used as audio sources for this
    -- output.
    audioDescriptionNames :: Prelude.Maybe [Prelude.Text],
    -- | Output type-specific settings.
    outputSettings :: OutputSettings
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
-- 'videoDescriptionName', 'output_videoDescriptionName' - The name of the VideoDescription used as the source for this output.
--
-- 'captionDescriptionNames', 'output_captionDescriptionNames' - The names of the CaptionDescriptions used as caption sources for this
-- output.
--
-- 'outputName', 'output_outputName' - The name used to identify an output.
--
-- 'audioDescriptionNames', 'output_audioDescriptionNames' - The names of the AudioDescriptions used as audio sources for this
-- output.
--
-- 'outputSettings', 'output_outputSettings' - Output type-specific settings.
newOutput ::
  -- | 'outputSettings'
  OutputSettings ->
  Output
newOutput pOutputSettings_ =
  Output'
    { videoDescriptionName = Prelude.Nothing,
      captionDescriptionNames = Prelude.Nothing,
      outputName = Prelude.Nothing,
      audioDescriptionNames = Prelude.Nothing,
      outputSettings = pOutputSettings_
    }

-- | The name of the VideoDescription used as the source for this output.
output_videoDescriptionName :: Lens.Lens' Output (Prelude.Maybe Prelude.Text)
output_videoDescriptionName = Lens.lens (\Output' {videoDescriptionName} -> videoDescriptionName) (\s@Output' {} a -> s {videoDescriptionName = a} :: Output)

-- | The names of the CaptionDescriptions used as caption sources for this
-- output.
output_captionDescriptionNames :: Lens.Lens' Output (Prelude.Maybe [Prelude.Text])
output_captionDescriptionNames = Lens.lens (\Output' {captionDescriptionNames} -> captionDescriptionNames) (\s@Output' {} a -> s {captionDescriptionNames = a} :: Output) Prelude.. Lens.mapping Lens.coerced

-- | The name used to identify an output.
output_outputName :: Lens.Lens' Output (Prelude.Maybe Prelude.Text)
output_outputName = Lens.lens (\Output' {outputName} -> outputName) (\s@Output' {} a -> s {outputName = a} :: Output)

-- | The names of the AudioDescriptions used as audio sources for this
-- output.
output_audioDescriptionNames :: Lens.Lens' Output (Prelude.Maybe [Prelude.Text])
output_audioDescriptionNames = Lens.lens (\Output' {audioDescriptionNames} -> audioDescriptionNames) (\s@Output' {} a -> s {audioDescriptionNames = a} :: Output) Prelude.. Lens.mapping Lens.coerced

-- | Output type-specific settings.
output_outputSettings :: Lens.Lens' Output OutputSettings
output_outputSettings = Lens.lens (\Output' {outputSettings} -> outputSettings) (\s@Output' {} a -> s {outputSettings = a} :: Output)

instance Data.FromJSON Output where
  parseJSON =
    Data.withObject
      "Output"
      ( \x ->
          Output'
            Prelude.<$> (x Data..:? "videoDescriptionName")
            Prelude.<*> ( x Data..:? "captionDescriptionNames"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "outputName")
            Prelude.<*> ( x Data..:? "audioDescriptionNames"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "outputSettings")
      )

instance Prelude.Hashable Output where
  hashWithSalt _salt Output' {..} =
    _salt `Prelude.hashWithSalt` videoDescriptionName
      `Prelude.hashWithSalt` captionDescriptionNames
      `Prelude.hashWithSalt` outputName
      `Prelude.hashWithSalt` audioDescriptionNames
      `Prelude.hashWithSalt` outputSettings

instance Prelude.NFData Output where
  rnf Output' {..} =
    Prelude.rnf videoDescriptionName
      `Prelude.seq` Prelude.rnf captionDescriptionNames
      `Prelude.seq` Prelude.rnf outputName
      `Prelude.seq` Prelude.rnf audioDescriptionNames
      `Prelude.seq` Prelude.rnf outputSettings

instance Data.ToJSON Output where
  toJSON Output' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("videoDescriptionName" Data..=)
              Prelude.<$> videoDescriptionName,
            ("captionDescriptionNames" Data..=)
              Prelude.<$> captionDescriptionNames,
            ("outputName" Data..=) Prelude.<$> outputName,
            ("audioDescriptionNames" Data..=)
              Prelude.<$> audioDescriptionNames,
            Prelude.Just
              ("outputSettings" Data..= outputSettings)
          ]
      )
