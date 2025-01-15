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
-- Module      : Amazonka.ElasticTranscoder.CreateJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- When you create a job, Elastic Transcoder returns JSON data that
-- includes the values that you specified plus information about the job
-- that is created.
--
-- If you have specified more than one output for your jobs (for example,
-- one output for the Kindle Fire and another output for the Apple iPhone
-- 4s), you currently must use the Elastic Transcoder API to list the jobs
-- (as opposed to the AWS Console).
module Amazonka.ElasticTranscoder.CreateJob
  ( -- * Creating a Request
    CreateJob (..),
    newCreateJob,

    -- * Request Lenses
    createJob_input,
    createJob_inputs,
    createJob_output,
    createJob_outputKeyPrefix,
    createJob_outputs,
    createJob_playlists,
    createJob_userMetadata,
    createJob_pipelineId,

    -- * Destructuring the Response
    CreateJobResponse (..),
    newCreateJobResponse,

    -- * Response Lenses
    createJobResponse_job,
    createJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticTranscoder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The @CreateJobRequest@ structure.
--
-- /See:/ 'newCreateJob' smart constructor.
data CreateJob = CreateJob'
  { -- | A section of the request body that provides information about the file
    -- that is being transcoded.
    input :: Prelude.Maybe JobInput,
    -- | A section of the request body that provides information about the files
    -- that are being transcoded.
    inputs :: Prelude.Maybe [JobInput],
    -- | A section of the request body that provides information about the
    -- transcoded (target) file. We strongly recommend that you use the
    -- @Outputs@ syntax instead of the @Output@ syntax.
    output :: Prelude.Maybe CreateJobOutput,
    -- | The value, if any, that you want Elastic Transcoder to prepend to the
    -- names of all files that this job creates, including output files,
    -- thumbnails, and playlists.
    outputKeyPrefix :: Prelude.Maybe Prelude.Text,
    -- | A section of the request body that provides information about the
    -- transcoded (target) files. We recommend that you use the @Outputs@
    -- syntax instead of the @Output@ syntax.
    outputs :: Prelude.Maybe [CreateJobOutput],
    -- | If you specify a preset in @PresetId@ for which the value of @Container@
    -- is fmp4 (Fragmented MP4) or ts (MPEG-TS), Playlists contains information
    -- about the master playlists that you want Elastic Transcoder to create.
    --
    -- The maximum number of master playlists in a job is 30.
    playlists :: Prelude.Maybe [CreateJobPlaylist],
    -- | User-defined metadata that you want to associate with an Elastic
    -- Transcoder job. You specify metadata in @key\/value@ pairs, and you can
    -- add up to 10 @key\/value@ pairs per job. Elastic Transcoder does not
    -- guarantee that @key\/value@ pairs are returned in the same order in
    -- which you specify them.
    userMetadata :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The @Id@ of the pipeline that you want Elastic Transcoder to use for
    -- transcoding. The pipeline determines several settings, including the
    -- Amazon S3 bucket from which Elastic Transcoder gets the files to
    -- transcode and the bucket into which Elastic Transcoder puts the
    -- transcoded files.
    pipelineId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'input', 'createJob_input' - A section of the request body that provides information about the file
-- that is being transcoded.
--
-- 'inputs', 'createJob_inputs' - A section of the request body that provides information about the files
-- that are being transcoded.
--
-- 'output', 'createJob_output' - A section of the request body that provides information about the
-- transcoded (target) file. We strongly recommend that you use the
-- @Outputs@ syntax instead of the @Output@ syntax.
--
-- 'outputKeyPrefix', 'createJob_outputKeyPrefix' - The value, if any, that you want Elastic Transcoder to prepend to the
-- names of all files that this job creates, including output files,
-- thumbnails, and playlists.
--
-- 'outputs', 'createJob_outputs' - A section of the request body that provides information about the
-- transcoded (target) files. We recommend that you use the @Outputs@
-- syntax instead of the @Output@ syntax.
--
-- 'playlists', 'createJob_playlists' - If you specify a preset in @PresetId@ for which the value of @Container@
-- is fmp4 (Fragmented MP4) or ts (MPEG-TS), Playlists contains information
-- about the master playlists that you want Elastic Transcoder to create.
--
-- The maximum number of master playlists in a job is 30.
--
-- 'userMetadata', 'createJob_userMetadata' - User-defined metadata that you want to associate with an Elastic
-- Transcoder job. You specify metadata in @key\/value@ pairs, and you can
-- add up to 10 @key\/value@ pairs per job. Elastic Transcoder does not
-- guarantee that @key\/value@ pairs are returned in the same order in
-- which you specify them.
--
-- 'pipelineId', 'createJob_pipelineId' - The @Id@ of the pipeline that you want Elastic Transcoder to use for
-- transcoding. The pipeline determines several settings, including the
-- Amazon S3 bucket from which Elastic Transcoder gets the files to
-- transcode and the bucket into which Elastic Transcoder puts the
-- transcoded files.
newCreateJob ::
  -- | 'pipelineId'
  Prelude.Text ->
  CreateJob
newCreateJob pPipelineId_ =
  CreateJob'
    { input = Prelude.Nothing,
      inputs = Prelude.Nothing,
      output = Prelude.Nothing,
      outputKeyPrefix = Prelude.Nothing,
      outputs = Prelude.Nothing,
      playlists = Prelude.Nothing,
      userMetadata = Prelude.Nothing,
      pipelineId = pPipelineId_
    }

-- | A section of the request body that provides information about the file
-- that is being transcoded.
createJob_input :: Lens.Lens' CreateJob (Prelude.Maybe JobInput)
createJob_input = Lens.lens (\CreateJob' {input} -> input) (\s@CreateJob' {} a -> s {input = a} :: CreateJob)

-- | A section of the request body that provides information about the files
-- that are being transcoded.
createJob_inputs :: Lens.Lens' CreateJob (Prelude.Maybe [JobInput])
createJob_inputs = Lens.lens (\CreateJob' {inputs} -> inputs) (\s@CreateJob' {} a -> s {inputs = a} :: CreateJob) Prelude.. Lens.mapping Lens.coerced

-- | A section of the request body that provides information about the
-- transcoded (target) file. We strongly recommend that you use the
-- @Outputs@ syntax instead of the @Output@ syntax.
createJob_output :: Lens.Lens' CreateJob (Prelude.Maybe CreateJobOutput)
createJob_output = Lens.lens (\CreateJob' {output} -> output) (\s@CreateJob' {} a -> s {output = a} :: CreateJob)

-- | The value, if any, that you want Elastic Transcoder to prepend to the
-- names of all files that this job creates, including output files,
-- thumbnails, and playlists.
createJob_outputKeyPrefix :: Lens.Lens' CreateJob (Prelude.Maybe Prelude.Text)
createJob_outputKeyPrefix = Lens.lens (\CreateJob' {outputKeyPrefix} -> outputKeyPrefix) (\s@CreateJob' {} a -> s {outputKeyPrefix = a} :: CreateJob)

-- | A section of the request body that provides information about the
-- transcoded (target) files. We recommend that you use the @Outputs@
-- syntax instead of the @Output@ syntax.
createJob_outputs :: Lens.Lens' CreateJob (Prelude.Maybe [CreateJobOutput])
createJob_outputs = Lens.lens (\CreateJob' {outputs} -> outputs) (\s@CreateJob' {} a -> s {outputs = a} :: CreateJob) Prelude.. Lens.mapping Lens.coerced

-- | If you specify a preset in @PresetId@ for which the value of @Container@
-- is fmp4 (Fragmented MP4) or ts (MPEG-TS), Playlists contains information
-- about the master playlists that you want Elastic Transcoder to create.
--
-- The maximum number of master playlists in a job is 30.
createJob_playlists :: Lens.Lens' CreateJob (Prelude.Maybe [CreateJobPlaylist])
createJob_playlists = Lens.lens (\CreateJob' {playlists} -> playlists) (\s@CreateJob' {} a -> s {playlists = a} :: CreateJob) Prelude.. Lens.mapping Lens.coerced

-- | User-defined metadata that you want to associate with an Elastic
-- Transcoder job. You specify metadata in @key\/value@ pairs, and you can
-- add up to 10 @key\/value@ pairs per job. Elastic Transcoder does not
-- guarantee that @key\/value@ pairs are returned in the same order in
-- which you specify them.
createJob_userMetadata :: Lens.Lens' CreateJob (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createJob_userMetadata = Lens.lens (\CreateJob' {userMetadata} -> userMetadata) (\s@CreateJob' {} a -> s {userMetadata = a} :: CreateJob) Prelude.. Lens.mapping Lens.coerced

-- | The @Id@ of the pipeline that you want Elastic Transcoder to use for
-- transcoding. The pipeline determines several settings, including the
-- Amazon S3 bucket from which Elastic Transcoder gets the files to
-- transcode and the bucket into which Elastic Transcoder puts the
-- transcoded files.
createJob_pipelineId :: Lens.Lens' CreateJob Prelude.Text
createJob_pipelineId = Lens.lens (\CreateJob' {pipelineId} -> pipelineId) (\s@CreateJob' {} a -> s {pipelineId = a} :: CreateJob)

instance Core.AWSRequest CreateJob where
  type AWSResponse CreateJob = CreateJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateJobResponse'
            Prelude.<$> (x Data..?> "Job")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateJob where
  hashWithSalt _salt CreateJob' {..} =
    _salt
      `Prelude.hashWithSalt` input
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` output
      `Prelude.hashWithSalt` outputKeyPrefix
      `Prelude.hashWithSalt` outputs
      `Prelude.hashWithSalt` playlists
      `Prelude.hashWithSalt` userMetadata
      `Prelude.hashWithSalt` pipelineId

instance Prelude.NFData CreateJob where
  rnf CreateJob' {..} =
    Prelude.rnf input `Prelude.seq`
      Prelude.rnf inputs `Prelude.seq`
        Prelude.rnf output `Prelude.seq`
          Prelude.rnf outputKeyPrefix `Prelude.seq`
            Prelude.rnf outputs `Prelude.seq`
              Prelude.rnf playlists `Prelude.seq`
                Prelude.rnf userMetadata `Prelude.seq`
                  Prelude.rnf pipelineId

instance Data.ToHeaders CreateJob where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateJob where
  toJSON CreateJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Input" Data..=) Prelude.<$> input,
            ("Inputs" Data..=) Prelude.<$> inputs,
            ("Output" Data..=) Prelude.<$> output,
            ("OutputKeyPrefix" Data..=)
              Prelude.<$> outputKeyPrefix,
            ("Outputs" Data..=) Prelude.<$> outputs,
            ("Playlists" Data..=) Prelude.<$> playlists,
            ("UserMetadata" Data..=) Prelude.<$> userMetadata,
            Prelude.Just ("PipelineId" Data..= pipelineId)
          ]
      )

instance Data.ToPath CreateJob where
  toPath = Prelude.const "/2012-09-25/jobs"

instance Data.ToQuery CreateJob where
  toQuery = Prelude.const Prelude.mempty

-- | The CreateJobResponse structure.
--
-- /See:/ 'newCreateJobResponse' smart constructor.
data CreateJobResponse = CreateJobResponse'
  { -- | A section of the response body that provides information about the job
    -- that is created.
    job :: Prelude.Maybe Job,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'job', 'createJobResponse_job' - A section of the response body that provides information about the job
-- that is created.
--
-- 'httpStatus', 'createJobResponse_httpStatus' - The response's http status code.
newCreateJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateJobResponse
newCreateJobResponse pHttpStatus_ =
  CreateJobResponse'
    { job = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A section of the response body that provides information about the job
-- that is created.
createJobResponse_job :: Lens.Lens' CreateJobResponse (Prelude.Maybe Job)
createJobResponse_job = Lens.lens (\CreateJobResponse' {job} -> job) (\s@CreateJobResponse' {} a -> s {job = a} :: CreateJobResponse)

-- | The response's http status code.
createJobResponse_httpStatus :: Lens.Lens' CreateJobResponse Prelude.Int
createJobResponse_httpStatus = Lens.lens (\CreateJobResponse' {httpStatus} -> httpStatus) (\s@CreateJobResponse' {} a -> s {httpStatus = a} :: CreateJobResponse)

instance Prelude.NFData CreateJobResponse where
  rnf CreateJobResponse' {..} =
    Prelude.rnf job `Prelude.seq`
      Prelude.rnf httpStatus
