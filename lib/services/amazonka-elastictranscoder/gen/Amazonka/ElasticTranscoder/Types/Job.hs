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
-- Module      : Amazonka.ElasticTranscoder.Types.Job
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticTranscoder.Types.Job where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticTranscoder.Types.JobInput
import Amazonka.ElasticTranscoder.Types.JobOutput
import Amazonka.ElasticTranscoder.Types.Playlist
import Amazonka.ElasticTranscoder.Types.Timing
import qualified Amazonka.Prelude as Prelude

-- | A section of the response body that provides information about the job
-- that is created.
--
-- /See:/ 'newJob' smart constructor.
data Job = Job'
  { -- | The Amazon Resource Name (ARN) for the job.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The identifier that Elastic Transcoder assigned to the job. You use this
    -- value to get settings for the job or to delete the job.
    id :: Prelude.Maybe Prelude.Text,
    -- | A section of the request or response body that provides information
    -- about the file that is being transcoded.
    input :: Prelude.Maybe JobInput,
    -- | Information about the files that you\'re transcoding. If you specified
    -- multiple files for this job, Elastic Transcoder stitches the files
    -- together to make one output.
    inputs :: Prelude.Maybe [JobInput],
    -- | If you specified one output for a job, information about that output. If
    -- you specified multiple outputs for a job, the Output object lists
    -- information about the first output. This duplicates the information that
    -- is listed for the first output in the Outputs object.
    --
    -- Outputs recommended instead.
    --
    -- A section of the request or response body that provides information
    -- about the transcoded (target) file.
    output :: Prelude.Maybe JobOutput,
    -- | The value, if any, that you want Elastic Transcoder to prepend to the
    -- names of all files that this job creates, including output files,
    -- thumbnails, and playlists. We recommend that you add a \/ or some other
    -- delimiter to the end of the @OutputKeyPrefix@.
    outputKeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | Information about the output files. We recommend that you use the
    -- @Outputs@ syntax for all jobs, even when you want Elastic Transcoder to
    -- transcode a file into only one format. Do not use both the @Outputs@ and
    -- @Output@ syntaxes in the same request. You can create a maximum of 30
    -- outputs per job.
    --
    -- If you specify more than one output for a job, Elastic Transcoder
    -- creates the files for each output in the order in which you specify them
    -- in the job.
    outputs :: Prelude.Maybe [JobOutput],
    -- | The @Id@ of the pipeline that you want Elastic Transcoder to use for
    -- transcoding. The pipeline determines several settings, including the
    -- Amazon S3 bucket from which Elastic Transcoder gets the files to
    -- transcode and the bucket into which Elastic Transcoder puts the
    -- transcoded files.
    pipelineId :: Prelude.Maybe Prelude.Text,
    -- | Outputs in Fragmented MP4 or MPEG-TS format only.
    --
    -- If you specify a preset in @PresetId@ for which the value of @Container@
    -- is fmp4 (Fragmented MP4) or ts (MPEG-TS), @Playlists@ contains
    -- information about the master playlists that you want Elastic Transcoder
    -- to create.
    --
    -- The maximum number of master playlists in a job is 30.
    playlists :: Prelude.Maybe [Playlist],
    -- | The status of the job: @Submitted@, @Progressing@, @Complete@,
    -- @Canceled@, or @Error@.
    status :: Prelude.Maybe Prelude.Text,
    -- | Details about the timing of a job.
    timing :: Prelude.Maybe Timing,
    -- | User-defined metadata that you want to associate with an Elastic
    -- Transcoder job. You specify metadata in @key\/value@ pairs, and you can
    -- add up to 10 @key\/value@ pairs per job. Elastic Transcoder does not
    -- guarantee that @key\/value@ pairs are returned in the same order in
    -- which you specify them.
    --
    -- Metadata @keys@ and @values@ must use characters from the following
    -- list:
    --
    -- -   @0-9@
    --
    -- -   @A-Z@ and @a-z@
    --
    -- -   @Space@
    --
    -- -   The following symbols: @_.:\/=+-%\@@
    userMetadata :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Job' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'job_arn' - The Amazon Resource Name (ARN) for the job.
--
-- 'id', 'job_id' - The identifier that Elastic Transcoder assigned to the job. You use this
-- value to get settings for the job or to delete the job.
--
-- 'input', 'job_input' - A section of the request or response body that provides information
-- about the file that is being transcoded.
--
-- 'inputs', 'job_inputs' - Information about the files that you\'re transcoding. If you specified
-- multiple files for this job, Elastic Transcoder stitches the files
-- together to make one output.
--
-- 'output', 'job_output' - If you specified one output for a job, information about that output. If
-- you specified multiple outputs for a job, the Output object lists
-- information about the first output. This duplicates the information that
-- is listed for the first output in the Outputs object.
--
-- Outputs recommended instead.
--
-- A section of the request or response body that provides information
-- about the transcoded (target) file.
--
-- 'outputKeyPrefix', 'job_outputKeyPrefix' - The value, if any, that you want Elastic Transcoder to prepend to the
-- names of all files that this job creates, including output files,
-- thumbnails, and playlists. We recommend that you add a \/ or some other
-- delimiter to the end of the @OutputKeyPrefix@.
--
-- 'outputs', 'job_outputs' - Information about the output files. We recommend that you use the
-- @Outputs@ syntax for all jobs, even when you want Elastic Transcoder to
-- transcode a file into only one format. Do not use both the @Outputs@ and
-- @Output@ syntaxes in the same request. You can create a maximum of 30
-- outputs per job.
--
-- If you specify more than one output for a job, Elastic Transcoder
-- creates the files for each output in the order in which you specify them
-- in the job.
--
-- 'pipelineId', 'job_pipelineId' - The @Id@ of the pipeline that you want Elastic Transcoder to use for
-- transcoding. The pipeline determines several settings, including the
-- Amazon S3 bucket from which Elastic Transcoder gets the files to
-- transcode and the bucket into which Elastic Transcoder puts the
-- transcoded files.
--
-- 'playlists', 'job_playlists' - Outputs in Fragmented MP4 or MPEG-TS format only.
--
-- If you specify a preset in @PresetId@ for which the value of @Container@
-- is fmp4 (Fragmented MP4) or ts (MPEG-TS), @Playlists@ contains
-- information about the master playlists that you want Elastic Transcoder
-- to create.
--
-- The maximum number of master playlists in a job is 30.
--
-- 'status', 'job_status' - The status of the job: @Submitted@, @Progressing@, @Complete@,
-- @Canceled@, or @Error@.
--
-- 'timing', 'job_timing' - Details about the timing of a job.
--
-- 'userMetadata', 'job_userMetadata' - User-defined metadata that you want to associate with an Elastic
-- Transcoder job. You specify metadata in @key\/value@ pairs, and you can
-- add up to 10 @key\/value@ pairs per job. Elastic Transcoder does not
-- guarantee that @key\/value@ pairs are returned in the same order in
-- which you specify them.
--
-- Metadata @keys@ and @values@ must use characters from the following
-- list:
--
-- -   @0-9@
--
-- -   @A-Z@ and @a-z@
--
-- -   @Space@
--
-- -   The following symbols: @_.:\/=+-%\@@
newJob ::
  Job
newJob =
  Job'
    { arn = Prelude.Nothing,
      id = Prelude.Nothing,
      input = Prelude.Nothing,
      inputs = Prelude.Nothing,
      output = Prelude.Nothing,
      outputKeyPrefix = Prelude.Nothing,
      outputs = Prelude.Nothing,
      pipelineId = Prelude.Nothing,
      playlists = Prelude.Nothing,
      status = Prelude.Nothing,
      timing = Prelude.Nothing,
      userMetadata = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) for the job.
job_arn :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_arn = Lens.lens (\Job' {arn} -> arn) (\s@Job' {} a -> s {arn = a} :: Job)

-- | The identifier that Elastic Transcoder assigned to the job. You use this
-- value to get settings for the job or to delete the job.
job_id :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_id = Lens.lens (\Job' {id} -> id) (\s@Job' {} a -> s {id = a} :: Job)

-- | A section of the request or response body that provides information
-- about the file that is being transcoded.
job_input :: Lens.Lens' Job (Prelude.Maybe JobInput)
job_input = Lens.lens (\Job' {input} -> input) (\s@Job' {} a -> s {input = a} :: Job)

-- | Information about the files that you\'re transcoding. If you specified
-- multiple files for this job, Elastic Transcoder stitches the files
-- together to make one output.
job_inputs :: Lens.Lens' Job (Prelude.Maybe [JobInput])
job_inputs = Lens.lens (\Job' {inputs} -> inputs) (\s@Job' {} a -> s {inputs = a} :: Job) Prelude.. Lens.mapping Lens.coerced

-- | If you specified one output for a job, information about that output. If
-- you specified multiple outputs for a job, the Output object lists
-- information about the first output. This duplicates the information that
-- is listed for the first output in the Outputs object.
--
-- Outputs recommended instead.
--
-- A section of the request or response body that provides information
-- about the transcoded (target) file.
job_output :: Lens.Lens' Job (Prelude.Maybe JobOutput)
job_output = Lens.lens (\Job' {output} -> output) (\s@Job' {} a -> s {output = a} :: Job)

-- | The value, if any, that you want Elastic Transcoder to prepend to the
-- names of all files that this job creates, including output files,
-- thumbnails, and playlists. We recommend that you add a \/ or some other
-- delimiter to the end of the @OutputKeyPrefix@.
job_outputKeyPrefix :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_outputKeyPrefix = Lens.lens (\Job' {outputKeyPrefix} -> outputKeyPrefix) (\s@Job' {} a -> s {outputKeyPrefix = a} :: Job)

-- | Information about the output files. We recommend that you use the
-- @Outputs@ syntax for all jobs, even when you want Elastic Transcoder to
-- transcode a file into only one format. Do not use both the @Outputs@ and
-- @Output@ syntaxes in the same request. You can create a maximum of 30
-- outputs per job.
--
-- If you specify more than one output for a job, Elastic Transcoder
-- creates the files for each output in the order in which you specify them
-- in the job.
job_outputs :: Lens.Lens' Job (Prelude.Maybe [JobOutput])
job_outputs = Lens.lens (\Job' {outputs} -> outputs) (\s@Job' {} a -> s {outputs = a} :: Job) Prelude.. Lens.mapping Lens.coerced

-- | The @Id@ of the pipeline that you want Elastic Transcoder to use for
-- transcoding. The pipeline determines several settings, including the
-- Amazon S3 bucket from which Elastic Transcoder gets the files to
-- transcode and the bucket into which Elastic Transcoder puts the
-- transcoded files.
job_pipelineId :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_pipelineId = Lens.lens (\Job' {pipelineId} -> pipelineId) (\s@Job' {} a -> s {pipelineId = a} :: Job)

-- | Outputs in Fragmented MP4 or MPEG-TS format only.
--
-- If you specify a preset in @PresetId@ for which the value of @Container@
-- is fmp4 (Fragmented MP4) or ts (MPEG-TS), @Playlists@ contains
-- information about the master playlists that you want Elastic Transcoder
-- to create.
--
-- The maximum number of master playlists in a job is 30.
job_playlists :: Lens.Lens' Job (Prelude.Maybe [Playlist])
job_playlists = Lens.lens (\Job' {playlists} -> playlists) (\s@Job' {} a -> s {playlists = a} :: Job) Prelude.. Lens.mapping Lens.coerced

-- | The status of the job: @Submitted@, @Progressing@, @Complete@,
-- @Canceled@, or @Error@.
job_status :: Lens.Lens' Job (Prelude.Maybe Prelude.Text)
job_status = Lens.lens (\Job' {status} -> status) (\s@Job' {} a -> s {status = a} :: Job)

-- | Details about the timing of a job.
job_timing :: Lens.Lens' Job (Prelude.Maybe Timing)
job_timing = Lens.lens (\Job' {timing} -> timing) (\s@Job' {} a -> s {timing = a} :: Job)

-- | User-defined metadata that you want to associate with an Elastic
-- Transcoder job. You specify metadata in @key\/value@ pairs, and you can
-- add up to 10 @key\/value@ pairs per job. Elastic Transcoder does not
-- guarantee that @key\/value@ pairs are returned in the same order in
-- which you specify them.
--
-- Metadata @keys@ and @values@ must use characters from the following
-- list:
--
-- -   @0-9@
--
-- -   @A-Z@ and @a-z@
--
-- -   @Space@
--
-- -   The following symbols: @_.:\/=+-%\@@
job_userMetadata :: Lens.Lens' Job (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
job_userMetadata = Lens.lens (\Job' {userMetadata} -> userMetadata) (\s@Job' {} a -> s {userMetadata = a} :: Job) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Job where
  parseJSON =
    Data.withObject
      "Job"
      ( \x ->
          Job'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Input")
            Prelude.<*> (x Data..:? "Inputs" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Output")
            Prelude.<*> (x Data..:? "OutputKeyPrefix")
            Prelude.<*> (x Data..:? "Outputs" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "PipelineId")
            Prelude.<*> (x Data..:? "Playlists" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Timing")
            Prelude.<*> (x Data..:? "UserMetadata" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Job where
  hashWithSalt _salt Job' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` input
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` output
      `Prelude.hashWithSalt` outputKeyPrefix
      `Prelude.hashWithSalt` outputs
      `Prelude.hashWithSalt` pipelineId
      `Prelude.hashWithSalt` playlists
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` timing
      `Prelude.hashWithSalt` userMetadata

instance Prelude.NFData Job where
  rnf Job' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf input
      `Prelude.seq` Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf output
      `Prelude.seq` Prelude.rnf outputKeyPrefix
      `Prelude.seq` Prelude.rnf outputs
      `Prelude.seq` Prelude.rnf pipelineId
      `Prelude.seq` Prelude.rnf playlists
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf timing
      `Prelude.seq` Prelude.rnf userMetadata
