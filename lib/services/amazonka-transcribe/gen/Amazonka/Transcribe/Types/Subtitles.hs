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
-- Module      : Amazonka.Transcribe.Types.Subtitles
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.Subtitles where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transcribe.Types.SubtitleFormat

-- | Generate subtitles for your media file with your transcription request.
--
-- You can choose a start index of 0 or 1, and you can specify either
-- WebVTT or SubRip (or both) as your output format.
--
-- Note that your subtitle files are placed in the same location as your
-- transcription output.
--
-- /See:/ 'newSubtitles' smart constructor.
data Subtitles = Subtitles'
  { -- | Specify the output format for your subtitle file; if you select both
    -- WebVTT (@vtt@) and SubRip (@srt@) formats, two output files are
    -- generated.
    formats :: Prelude.Maybe [SubtitleFormat],
    -- | Specify the starting value that is assigned to the first subtitle
    -- segment.
    --
    -- The default start index for Amazon Transcribe is @0@, which differs from
    -- the more widely used standard of @1@. If you\'re uncertain which value
    -- to use, we recommend choosing @1@, as this may improve compatibility
    -- with other services.
    outputStartIndex :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Subtitles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'formats', 'subtitles_formats' - Specify the output format for your subtitle file; if you select both
-- WebVTT (@vtt@) and SubRip (@srt@) formats, two output files are
-- generated.
--
-- 'outputStartIndex', 'subtitles_outputStartIndex' - Specify the starting value that is assigned to the first subtitle
-- segment.
--
-- The default start index for Amazon Transcribe is @0@, which differs from
-- the more widely used standard of @1@. If you\'re uncertain which value
-- to use, we recommend choosing @1@, as this may improve compatibility
-- with other services.
newSubtitles ::
  Subtitles
newSubtitles =
  Subtitles'
    { formats = Prelude.Nothing,
      outputStartIndex = Prelude.Nothing
    }

-- | Specify the output format for your subtitle file; if you select both
-- WebVTT (@vtt@) and SubRip (@srt@) formats, two output files are
-- generated.
subtitles_formats :: Lens.Lens' Subtitles (Prelude.Maybe [SubtitleFormat])
subtitles_formats = Lens.lens (\Subtitles' {formats} -> formats) (\s@Subtitles' {} a -> s {formats = a} :: Subtitles) Prelude.. Lens.mapping Lens.coerced

-- | Specify the starting value that is assigned to the first subtitle
-- segment.
--
-- The default start index for Amazon Transcribe is @0@, which differs from
-- the more widely used standard of @1@. If you\'re uncertain which value
-- to use, we recommend choosing @1@, as this may improve compatibility
-- with other services.
subtitles_outputStartIndex :: Lens.Lens' Subtitles (Prelude.Maybe Prelude.Natural)
subtitles_outputStartIndex = Lens.lens (\Subtitles' {outputStartIndex} -> outputStartIndex) (\s@Subtitles' {} a -> s {outputStartIndex = a} :: Subtitles)

instance Prelude.Hashable Subtitles where
  hashWithSalt _salt Subtitles' {..} =
    _salt
      `Prelude.hashWithSalt` formats
      `Prelude.hashWithSalt` outputStartIndex

instance Prelude.NFData Subtitles where
  rnf Subtitles' {..} =
    Prelude.rnf formats
      `Prelude.seq` Prelude.rnf outputStartIndex

instance Data.ToJSON Subtitles where
  toJSON Subtitles' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Formats" Data..=) Prelude.<$> formats,
            ("OutputStartIndex" Data..=)
              Prelude.<$> outputStartIndex
          ]
      )
