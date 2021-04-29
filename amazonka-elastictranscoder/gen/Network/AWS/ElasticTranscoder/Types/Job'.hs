{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ElasticTranscoder.Types.Job'
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.Job' where

import Network.AWS.ElasticTranscoder.Types.JobInput
import Network.AWS.ElasticTranscoder.Types.JobOutput
import Network.AWS.ElasticTranscoder.Types.Playlist
import Network.AWS.ElasticTranscoder.Types.Timing
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A section of the response body that provides information about the job
-- that is created.
--
-- /See:/ 'newJob'' smart constructor.
data Job' = Job''
  { -- | The @Id@ of the pipeline that you want Elastic Transcoder to use for
    -- transcoding. The pipeline determines several settings, including the
    -- Amazon S3 bucket from which Elastic Transcoder gets the files to
    -- transcode and the bucket into which Elastic Transcoder puts the
    -- transcoded files.
    pipelineId :: Prelude.Maybe Prelude.Text,
    -- | The status of the job: @Submitted@, @Progressing@, @Complete@,
    -- @Canceled@, or @Error@.
    status :: Prelude.Maybe Prelude.Text,
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
    -- | A section of the request or response body that provides information
    -- about the file that is being transcoded.
    input :: Prelude.Maybe JobInput,
    -- | The value, if any, that you want Elastic Transcoder to prepend to the
    -- names of all files that this job creates, including output files,
    -- thumbnails, and playlists. We recommend that you add a \/ or some other
    -- delimiter to the end of the @OutputKeyPrefix@.
    outputKeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the job.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The identifier that Elastic Transcoder assigned to the job. You use this
    -- value to get settings for the job or to delete the job.
    id :: Prelude.Maybe Prelude.Text,
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
    userMetadata :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Details about the timing of a job.
    timing :: Prelude.Maybe Timing,
    -- | Information about the files that you\'re transcoding. If you specified
    -- multiple files for this job, Elastic Transcoder stitches the files
    -- together to make one output.
    inputs :: Prelude.Maybe [JobInput],
    -- | Outputs in Fragmented MP4 or MPEG-TS format only.
    --
    -- If you specify a preset in @PresetId@ for which the value of @Container@
    -- is fmp4 (Fragmented MP4) or ts (MPEG-TS), @Playlists@ contains
    -- information about the master playlists that you want Elastic Transcoder
    -- to create.
    --
    -- The maximum number of master playlists in a job is 30.
    playlists :: Prelude.Maybe [Playlist]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Job'' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineId', 'job'_pipelineId' - The @Id@ of the pipeline that you want Elastic Transcoder to use for
-- transcoding. The pipeline determines several settings, including the
-- Amazon S3 bucket from which Elastic Transcoder gets the files to
-- transcode and the bucket into which Elastic Transcoder puts the
-- transcoded files.
--
-- 'status', 'job'_status' - The status of the job: @Submitted@, @Progressing@, @Complete@,
-- @Canceled@, or @Error@.
--
-- 'outputs', 'job'_outputs' - Information about the output files. We recommend that you use the
-- @Outputs@ syntax for all jobs, even when you want Elastic Transcoder to
-- transcode a file into only one format. Do not use both the @Outputs@ and
-- @Output@ syntaxes in the same request. You can create a maximum of 30
-- outputs per job.
--
-- If you specify more than one output for a job, Elastic Transcoder
-- creates the files for each output in the order in which you specify them
-- in the job.
--
-- 'input', 'job'_input' - A section of the request or response body that provides information
-- about the file that is being transcoded.
--
-- 'outputKeyPrefix', 'job'_outputKeyPrefix' - The value, if any, that you want Elastic Transcoder to prepend to the
-- names of all files that this job creates, including output files,
-- thumbnails, and playlists. We recommend that you add a \/ or some other
-- delimiter to the end of the @OutputKeyPrefix@.
--
-- 'arn', 'job'_arn' - The Amazon Resource Name (ARN) for the job.
--
-- 'id', 'job'_id' - The identifier that Elastic Transcoder assigned to the job. You use this
-- value to get settings for the job or to delete the job.
--
-- 'output', 'job'_output' - If you specified one output for a job, information about that output. If
-- you specified multiple outputs for a job, the Output object lists
-- information about the first output. This duplicates the information that
-- is listed for the first output in the Outputs object.
--
-- Outputs recommended instead.
--
-- A section of the request or response body that provides information
-- about the transcoded (target) file.
--
-- 'userMetadata', 'job'_userMetadata' - User-defined metadata that you want to associate with an Elastic
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
--
-- 'timing', 'job'_timing' - Details about the timing of a job.
--
-- 'inputs', 'job'_inputs' - Information about the files that you\'re transcoding. If you specified
-- multiple files for this job, Elastic Transcoder stitches the files
-- together to make one output.
--
-- 'playlists', 'job'_playlists' - Outputs in Fragmented MP4 or MPEG-TS format only.
--
-- If you specify a preset in @PresetId@ for which the value of @Container@
-- is fmp4 (Fragmented MP4) or ts (MPEG-TS), @Playlists@ contains
-- information about the master playlists that you want Elastic Transcoder
-- to create.
--
-- The maximum number of master playlists in a job is 30.
newJob' ::
  Job'
newJob' =
  Job''
    { pipelineId = Prelude.Nothing,
      status = Prelude.Nothing,
      outputs = Prelude.Nothing,
      input = Prelude.Nothing,
      outputKeyPrefix = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      output = Prelude.Nothing,
      userMetadata = Prelude.Nothing,
      timing = Prelude.Nothing,
      inputs = Prelude.Nothing,
      playlists = Prelude.Nothing
    }

-- | The @Id@ of the pipeline that you want Elastic Transcoder to use for
-- transcoding. The pipeline determines several settings, including the
-- Amazon S3 bucket from which Elastic Transcoder gets the files to
-- transcode and the bucket into which Elastic Transcoder puts the
-- transcoded files.
job'_pipelineId :: Lens.Lens' Job' (Prelude.Maybe Prelude.Text)
job'_pipelineId = Lens.lens (\Job'' {pipelineId} -> pipelineId) (\s@Job'' {} a -> s {pipelineId = a} :: Job')

-- | The status of the job: @Submitted@, @Progressing@, @Complete@,
-- @Canceled@, or @Error@.
job'_status :: Lens.Lens' Job' (Prelude.Maybe Prelude.Text)
job'_status = Lens.lens (\Job'' {status} -> status) (\s@Job'' {} a -> s {status = a} :: Job')

-- | Information about the output files. We recommend that you use the
-- @Outputs@ syntax for all jobs, even when you want Elastic Transcoder to
-- transcode a file into only one format. Do not use both the @Outputs@ and
-- @Output@ syntaxes in the same request. You can create a maximum of 30
-- outputs per job.
--
-- If you specify more than one output for a job, Elastic Transcoder
-- creates the files for each output in the order in which you specify them
-- in the job.
job'_outputs :: Lens.Lens' Job' (Prelude.Maybe [JobOutput])
job'_outputs = Lens.lens (\Job'' {outputs} -> outputs) (\s@Job'' {} a -> s {outputs = a} :: Job') Prelude.. Lens.mapping Prelude._Coerce

-- | A section of the request or response body that provides information
-- about the file that is being transcoded.
job'_input :: Lens.Lens' Job' (Prelude.Maybe JobInput)
job'_input = Lens.lens (\Job'' {input} -> input) (\s@Job'' {} a -> s {input = a} :: Job')

-- | The value, if any, that you want Elastic Transcoder to prepend to the
-- names of all files that this job creates, including output files,
-- thumbnails, and playlists. We recommend that you add a \/ or some other
-- delimiter to the end of the @OutputKeyPrefix@.
job'_outputKeyPrefix :: Lens.Lens' Job' (Prelude.Maybe Prelude.Text)
job'_outputKeyPrefix = Lens.lens (\Job'' {outputKeyPrefix} -> outputKeyPrefix) (\s@Job'' {} a -> s {outputKeyPrefix = a} :: Job')

-- | The Amazon Resource Name (ARN) for the job.
job'_arn :: Lens.Lens' Job' (Prelude.Maybe Prelude.Text)
job'_arn = Lens.lens (\Job'' {arn} -> arn) (\s@Job'' {} a -> s {arn = a} :: Job')

-- | The identifier that Elastic Transcoder assigned to the job. You use this
-- value to get settings for the job or to delete the job.
job'_id :: Lens.Lens' Job' (Prelude.Maybe Prelude.Text)
job'_id = Lens.lens (\Job'' {id} -> id) (\s@Job'' {} a -> s {id = a} :: Job')

-- | If you specified one output for a job, information about that output. If
-- you specified multiple outputs for a job, the Output object lists
-- information about the first output. This duplicates the information that
-- is listed for the first output in the Outputs object.
--
-- Outputs recommended instead.
--
-- A section of the request or response body that provides information
-- about the transcoded (target) file.
job'_output :: Lens.Lens' Job' (Prelude.Maybe JobOutput)
job'_output = Lens.lens (\Job'' {output} -> output) (\s@Job'' {} a -> s {output = a} :: Job')

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
job'_userMetadata :: Lens.Lens' Job' (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
job'_userMetadata = Lens.lens (\Job'' {userMetadata} -> userMetadata) (\s@Job'' {} a -> s {userMetadata = a} :: Job') Prelude.. Lens.mapping Prelude._Coerce

-- | Details about the timing of a job.
job'_timing :: Lens.Lens' Job' (Prelude.Maybe Timing)
job'_timing = Lens.lens (\Job'' {timing} -> timing) (\s@Job'' {} a -> s {timing = a} :: Job')

-- | Information about the files that you\'re transcoding. If you specified
-- multiple files for this job, Elastic Transcoder stitches the files
-- together to make one output.
job'_inputs :: Lens.Lens' Job' (Prelude.Maybe [JobInput])
job'_inputs = Lens.lens (\Job'' {inputs} -> inputs) (\s@Job'' {} a -> s {inputs = a} :: Job') Prelude.. Lens.mapping Prelude._Coerce

-- | Outputs in Fragmented MP4 or MPEG-TS format only.
--
-- If you specify a preset in @PresetId@ for which the value of @Container@
-- is fmp4 (Fragmented MP4) or ts (MPEG-TS), @Playlists@ contains
-- information about the master playlists that you want Elastic Transcoder
-- to create.
--
-- The maximum number of master playlists in a job is 30.
job'_playlists :: Lens.Lens' Job' (Prelude.Maybe [Playlist])
job'_playlists = Lens.lens (\Job'' {playlists} -> playlists) (\s@Job'' {} a -> s {playlists = a} :: Job') Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON Job' where
  parseJSON =
    Prelude.withObject
      "Job'"
      ( \x ->
          Job''
            Prelude.<$> (x Prelude..:? "PipelineId")
            Prelude.<*> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "Outputs" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "Input")
            Prelude.<*> (x Prelude..:? "OutputKeyPrefix")
            Prelude.<*> (x Prelude..:? "Arn")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "Output")
            Prelude.<*> ( x Prelude..:? "UserMetadata"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "Timing")
            Prelude.<*> (x Prelude..:? "Inputs" Prelude..!= Prelude.mempty)
            Prelude.<*> ( x Prelude..:? "Playlists"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Job'

instance Prelude.NFData Job'
