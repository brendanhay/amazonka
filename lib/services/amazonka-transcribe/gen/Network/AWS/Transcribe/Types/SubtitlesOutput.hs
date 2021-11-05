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
-- Module      : Amazonka.Transcribe.Types.SubtitlesOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.SubtitlesOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transcribe.Types.SubtitleFormat

-- | Specify the output format for your subtitle file.
--
-- /See:/ 'newSubtitlesOutput' smart constructor.
data SubtitlesOutput = SubtitlesOutput'
  { -- | Specify the output format for your subtitle file; if you select both SRT
    -- and VTT formats, two output files are genereated.
    formats :: Prelude.Maybe [SubtitleFormat],
    -- | Choose the output location for your subtitle file. This location must be
    -- an S3 bucket.
    subtitleFileUris :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SubtitlesOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'formats', 'subtitlesOutput_formats' - Specify the output format for your subtitle file; if you select both SRT
-- and VTT formats, two output files are genereated.
--
-- 'subtitleFileUris', 'subtitlesOutput_subtitleFileUris' - Choose the output location for your subtitle file. This location must be
-- an S3 bucket.
newSubtitlesOutput ::
  SubtitlesOutput
newSubtitlesOutput =
  SubtitlesOutput'
    { formats = Prelude.Nothing,
      subtitleFileUris = Prelude.Nothing
    }

-- | Specify the output format for your subtitle file; if you select both SRT
-- and VTT formats, two output files are genereated.
subtitlesOutput_formats :: Lens.Lens' SubtitlesOutput (Prelude.Maybe [SubtitleFormat])
subtitlesOutput_formats = Lens.lens (\SubtitlesOutput' {formats} -> formats) (\s@SubtitlesOutput' {} a -> s {formats = a} :: SubtitlesOutput) Prelude.. Lens.mapping Lens.coerced

-- | Choose the output location for your subtitle file. This location must be
-- an S3 bucket.
subtitlesOutput_subtitleFileUris :: Lens.Lens' SubtitlesOutput (Prelude.Maybe [Prelude.Text])
subtitlesOutput_subtitleFileUris = Lens.lens (\SubtitlesOutput' {subtitleFileUris} -> subtitleFileUris) (\s@SubtitlesOutput' {} a -> s {subtitleFileUris = a} :: SubtitlesOutput) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON SubtitlesOutput where
  parseJSON =
    Core.withObject
      "SubtitlesOutput"
      ( \x ->
          SubtitlesOutput'
            Prelude.<$> (x Core..:? "Formats" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "SubtitleFileUris"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable SubtitlesOutput

instance Prelude.NFData SubtitlesOutput
