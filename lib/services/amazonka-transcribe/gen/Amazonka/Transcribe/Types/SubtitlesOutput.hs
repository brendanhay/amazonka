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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.SubtitlesOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transcribe.Types.SubtitleFormat

-- | Provides information about your subtitle file, including format, start
-- index, and Amazon S3 location.
--
-- /See:/ 'newSubtitlesOutput' smart constructor.
data SubtitlesOutput = SubtitlesOutput'
  { -- | Provides the start index value for your subtitle files. If you did not
    -- specify a value in your request, the default value of @0@ is used.
    outputStartIndex :: Prelude.Maybe Prelude.Natural,
    -- | Provides the format of your subtitle files. If your request included
    -- both WebVTT (@vtt@) and SubRip (@srt@) formats, both formats are shown.
    formats :: Prelude.Maybe [SubtitleFormat],
    -- | The Amazon S3 location of your transcript. You can use this URI to
    -- access or download your subtitle file. Your subtitle file is stored in
    -- the same location as your transcript. If you specified both WebVTT and
    -- SubRip subtitle formats, two URIs are provided.
    --
    -- If you included @OutputBucketName@ in your transcription job request,
    -- this is the URI of that bucket. If you also included @OutputKey@ in your
    -- request, your output is located in the path you specified in your
    -- request.
    --
    -- If you didn\'t include @OutputBucketName@ in your transcription job
    -- request, your subtitle file is stored in a service-managed bucket, and
    -- @TranscriptFileUri@ provides you with a temporary URI you can use for
    -- secure access to your subtitle file.
    --
    -- Temporary URIs for service-managed Amazon S3 buckets are only valid for
    -- 15 minutes. If you get an @AccesDenied@ error, you can get a new
    -- temporary URI by running a @GetTranscriptionJob@ or
    -- @ListTranscriptionJob@ request.
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
-- 'outputStartIndex', 'subtitlesOutput_outputStartIndex' - Provides the start index value for your subtitle files. If you did not
-- specify a value in your request, the default value of @0@ is used.
--
-- 'formats', 'subtitlesOutput_formats' - Provides the format of your subtitle files. If your request included
-- both WebVTT (@vtt@) and SubRip (@srt@) formats, both formats are shown.
--
-- 'subtitleFileUris', 'subtitlesOutput_subtitleFileUris' - The Amazon S3 location of your transcript. You can use this URI to
-- access or download your subtitle file. Your subtitle file is stored in
-- the same location as your transcript. If you specified both WebVTT and
-- SubRip subtitle formats, two URIs are provided.
--
-- If you included @OutputBucketName@ in your transcription job request,
-- this is the URI of that bucket. If you also included @OutputKey@ in your
-- request, your output is located in the path you specified in your
-- request.
--
-- If you didn\'t include @OutputBucketName@ in your transcription job
-- request, your subtitle file is stored in a service-managed bucket, and
-- @TranscriptFileUri@ provides you with a temporary URI you can use for
-- secure access to your subtitle file.
--
-- Temporary URIs for service-managed Amazon S3 buckets are only valid for
-- 15 minutes. If you get an @AccesDenied@ error, you can get a new
-- temporary URI by running a @GetTranscriptionJob@ or
-- @ListTranscriptionJob@ request.
newSubtitlesOutput ::
  SubtitlesOutput
newSubtitlesOutput =
  SubtitlesOutput'
    { outputStartIndex =
        Prelude.Nothing,
      formats = Prelude.Nothing,
      subtitleFileUris = Prelude.Nothing
    }

-- | Provides the start index value for your subtitle files. If you did not
-- specify a value in your request, the default value of @0@ is used.
subtitlesOutput_outputStartIndex :: Lens.Lens' SubtitlesOutput (Prelude.Maybe Prelude.Natural)
subtitlesOutput_outputStartIndex = Lens.lens (\SubtitlesOutput' {outputStartIndex} -> outputStartIndex) (\s@SubtitlesOutput' {} a -> s {outputStartIndex = a} :: SubtitlesOutput)

-- | Provides the format of your subtitle files. If your request included
-- both WebVTT (@vtt@) and SubRip (@srt@) formats, both formats are shown.
subtitlesOutput_formats :: Lens.Lens' SubtitlesOutput (Prelude.Maybe [SubtitleFormat])
subtitlesOutput_formats = Lens.lens (\SubtitlesOutput' {formats} -> formats) (\s@SubtitlesOutput' {} a -> s {formats = a} :: SubtitlesOutput) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon S3 location of your transcript. You can use this URI to
-- access or download your subtitle file. Your subtitle file is stored in
-- the same location as your transcript. If you specified both WebVTT and
-- SubRip subtitle formats, two URIs are provided.
--
-- If you included @OutputBucketName@ in your transcription job request,
-- this is the URI of that bucket. If you also included @OutputKey@ in your
-- request, your output is located in the path you specified in your
-- request.
--
-- If you didn\'t include @OutputBucketName@ in your transcription job
-- request, your subtitle file is stored in a service-managed bucket, and
-- @TranscriptFileUri@ provides you with a temporary URI you can use for
-- secure access to your subtitle file.
--
-- Temporary URIs for service-managed Amazon S3 buckets are only valid for
-- 15 minutes. If you get an @AccesDenied@ error, you can get a new
-- temporary URI by running a @GetTranscriptionJob@ or
-- @ListTranscriptionJob@ request.
subtitlesOutput_subtitleFileUris :: Lens.Lens' SubtitlesOutput (Prelude.Maybe [Prelude.Text])
subtitlesOutput_subtitleFileUris = Lens.lens (\SubtitlesOutput' {subtitleFileUris} -> subtitleFileUris) (\s@SubtitlesOutput' {} a -> s {subtitleFileUris = a} :: SubtitlesOutput) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON SubtitlesOutput where
  parseJSON =
    Core.withObject
      "SubtitlesOutput"
      ( \x ->
          SubtitlesOutput'
            Prelude.<$> (x Core..:? "OutputStartIndex")
            Prelude.<*> (x Core..:? "Formats" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "SubtitleFileUris"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable SubtitlesOutput where
  hashWithSalt _salt SubtitlesOutput' {..} =
    _salt `Prelude.hashWithSalt` outputStartIndex
      `Prelude.hashWithSalt` formats
      `Prelude.hashWithSalt` subtitleFileUris

instance Prelude.NFData SubtitlesOutput where
  rnf SubtitlesOutput' {..} =
    Prelude.rnf outputStartIndex
      `Prelude.seq` Prelude.rnf formats
      `Prelude.seq` Prelude.rnf subtitleFileUris
