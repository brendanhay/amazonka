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
-- Module      : Network.AWS.ElasticTranscoder.CreateJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.ElasticTranscoder.CreateJob
  ( -- * Creating a Request
    CreateJob (..),
    newCreateJob,

    -- * Request Lenses
    createJob_outputs,
    createJob_input,
    createJob_outputKeyPrefix,
    createJob_output,
    createJob_userMetadata,
    createJob_inputs,
    createJob_playlists,
    createJob_pipelineId,

    -- * Destructuring the Response
    CreateJobResponse (..),
    newCreateJobResponse,

    -- * Response Lenses
    createJobResponse_job,
    createJobResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticTranscoder.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The @CreateJobRequest@ structure.
--
-- /See:/ 'newCreateJob' smart constructor.
data CreateJob = CreateJob'
  { -- | A section of the request body that provides information about the
    -- transcoded (target) files. We recommend that you use the @Outputs@
    -- syntax instead of the @Output@ syntax.
    outputs :: Core.Maybe [CreateJobOutput],
    -- | A section of the request body that provides information about the file
    -- that is being transcoded.
    input :: Core.Maybe JobInput,
    -- | The value, if any, that you want Elastic Transcoder to prepend to the
    -- names of all files that this job creates, including output files,
    -- thumbnails, and playlists.
    outputKeyPrefix :: Core.Maybe Core.Text,
    -- | A section of the request body that provides information about the
    -- transcoded (target) file. We strongly recommend that you use the
    -- @Outputs@ syntax instead of the @Output@ syntax.
    output :: Core.Maybe CreateJobOutput,
    -- | User-defined metadata that you want to associate with an Elastic
    -- Transcoder job. You specify metadata in @key\/value@ pairs, and you can
    -- add up to 10 @key\/value@ pairs per job. Elastic Transcoder does not
    -- guarantee that @key\/value@ pairs are returned in the same order in
    -- which you specify them.
    userMetadata :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | A section of the request body that provides information about the files
    -- that are being transcoded.
    inputs :: Core.Maybe [JobInput],
    -- | If you specify a preset in @PresetId@ for which the value of @Container@
    -- is fmp4 (Fragmented MP4) or ts (MPEG-TS), Playlists contains information
    -- about the master playlists that you want Elastic Transcoder to create.
    --
    -- The maximum number of master playlists in a job is 30.
    playlists :: Core.Maybe [CreateJobPlaylist],
    -- | The @Id@ of the pipeline that you want Elastic Transcoder to use for
    -- transcoding. The pipeline determines several settings, including the
    -- Amazon S3 bucket from which Elastic Transcoder gets the files to
    -- transcode and the bucket into which Elastic Transcoder puts the
    -- transcoded files.
    pipelineId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputs', 'createJob_outputs' - A section of the request body that provides information about the
-- transcoded (target) files. We recommend that you use the @Outputs@
-- syntax instead of the @Output@ syntax.
--
-- 'input', 'createJob_input' - A section of the request body that provides information about the file
-- that is being transcoded.
--
-- 'outputKeyPrefix', 'createJob_outputKeyPrefix' - The value, if any, that you want Elastic Transcoder to prepend to the
-- names of all files that this job creates, including output files,
-- thumbnails, and playlists.
--
-- 'output', 'createJob_output' - A section of the request body that provides information about the
-- transcoded (target) file. We strongly recommend that you use the
-- @Outputs@ syntax instead of the @Output@ syntax.
--
-- 'userMetadata', 'createJob_userMetadata' - User-defined metadata that you want to associate with an Elastic
-- Transcoder job. You specify metadata in @key\/value@ pairs, and you can
-- add up to 10 @key\/value@ pairs per job. Elastic Transcoder does not
-- guarantee that @key\/value@ pairs are returned in the same order in
-- which you specify them.
--
-- 'inputs', 'createJob_inputs' - A section of the request body that provides information about the files
-- that are being transcoded.
--
-- 'playlists', 'createJob_playlists' - If you specify a preset in @PresetId@ for which the value of @Container@
-- is fmp4 (Fragmented MP4) or ts (MPEG-TS), Playlists contains information
-- about the master playlists that you want Elastic Transcoder to create.
--
-- The maximum number of master playlists in a job is 30.
--
-- 'pipelineId', 'createJob_pipelineId' - The @Id@ of the pipeline that you want Elastic Transcoder to use for
-- transcoding. The pipeline determines several settings, including the
-- Amazon S3 bucket from which Elastic Transcoder gets the files to
-- transcode and the bucket into which Elastic Transcoder puts the
-- transcoded files.
newCreateJob ::
  -- | 'pipelineId'
  Core.Text ->
  CreateJob
newCreateJob pPipelineId_ =
  CreateJob'
    { outputs = Core.Nothing,
      input = Core.Nothing,
      outputKeyPrefix = Core.Nothing,
      output = Core.Nothing,
      userMetadata = Core.Nothing,
      inputs = Core.Nothing,
      playlists = Core.Nothing,
      pipelineId = pPipelineId_
    }

-- | A section of the request body that provides information about the
-- transcoded (target) files. We recommend that you use the @Outputs@
-- syntax instead of the @Output@ syntax.
createJob_outputs :: Lens.Lens' CreateJob (Core.Maybe [CreateJobOutput])
createJob_outputs = Lens.lens (\CreateJob' {outputs} -> outputs) (\s@CreateJob' {} a -> s {outputs = a} :: CreateJob) Core.. Lens.mapping Lens._Coerce

-- | A section of the request body that provides information about the file
-- that is being transcoded.
createJob_input :: Lens.Lens' CreateJob (Core.Maybe JobInput)
createJob_input = Lens.lens (\CreateJob' {input} -> input) (\s@CreateJob' {} a -> s {input = a} :: CreateJob)

-- | The value, if any, that you want Elastic Transcoder to prepend to the
-- names of all files that this job creates, including output files,
-- thumbnails, and playlists.
createJob_outputKeyPrefix :: Lens.Lens' CreateJob (Core.Maybe Core.Text)
createJob_outputKeyPrefix = Lens.lens (\CreateJob' {outputKeyPrefix} -> outputKeyPrefix) (\s@CreateJob' {} a -> s {outputKeyPrefix = a} :: CreateJob)

-- | A section of the request body that provides information about the
-- transcoded (target) file. We strongly recommend that you use the
-- @Outputs@ syntax instead of the @Output@ syntax.
createJob_output :: Lens.Lens' CreateJob (Core.Maybe CreateJobOutput)
createJob_output = Lens.lens (\CreateJob' {output} -> output) (\s@CreateJob' {} a -> s {output = a} :: CreateJob)

-- | User-defined metadata that you want to associate with an Elastic
-- Transcoder job. You specify metadata in @key\/value@ pairs, and you can
-- add up to 10 @key\/value@ pairs per job. Elastic Transcoder does not
-- guarantee that @key\/value@ pairs are returned in the same order in
-- which you specify them.
createJob_userMetadata :: Lens.Lens' CreateJob (Core.Maybe (Core.HashMap Core.Text Core.Text))
createJob_userMetadata = Lens.lens (\CreateJob' {userMetadata} -> userMetadata) (\s@CreateJob' {} a -> s {userMetadata = a} :: CreateJob) Core.. Lens.mapping Lens._Coerce

-- | A section of the request body that provides information about the files
-- that are being transcoded.
createJob_inputs :: Lens.Lens' CreateJob (Core.Maybe [JobInput])
createJob_inputs = Lens.lens (\CreateJob' {inputs} -> inputs) (\s@CreateJob' {} a -> s {inputs = a} :: CreateJob) Core.. Lens.mapping Lens._Coerce

-- | If you specify a preset in @PresetId@ for which the value of @Container@
-- is fmp4 (Fragmented MP4) or ts (MPEG-TS), Playlists contains information
-- about the master playlists that you want Elastic Transcoder to create.
--
-- The maximum number of master playlists in a job is 30.
createJob_playlists :: Lens.Lens' CreateJob (Core.Maybe [CreateJobPlaylist])
createJob_playlists = Lens.lens (\CreateJob' {playlists} -> playlists) (\s@CreateJob' {} a -> s {playlists = a} :: CreateJob) Core.. Lens.mapping Lens._Coerce

-- | The @Id@ of the pipeline that you want Elastic Transcoder to use for
-- transcoding. The pipeline determines several settings, including the
-- Amazon S3 bucket from which Elastic Transcoder gets the files to
-- transcode and the bucket into which Elastic Transcoder puts the
-- transcoded files.
createJob_pipelineId :: Lens.Lens' CreateJob Core.Text
createJob_pipelineId = Lens.lens (\CreateJob' {pipelineId} -> pipelineId) (\s@CreateJob' {} a -> s {pipelineId = a} :: CreateJob)

instance Core.AWSRequest CreateJob where
  type AWSResponse CreateJob = CreateJobResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateJobResponse'
            Core.<$> (x Core..?> "Job")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateJob

instance Core.NFData CreateJob

instance Core.ToHeaders CreateJob where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON CreateJob where
  toJSON CreateJob' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Outputs" Core..=) Core.<$> outputs,
            ("Input" Core..=) Core.<$> input,
            ("OutputKeyPrefix" Core..=) Core.<$> outputKeyPrefix,
            ("Output" Core..=) Core.<$> output,
            ("UserMetadata" Core..=) Core.<$> userMetadata,
            ("Inputs" Core..=) Core.<$> inputs,
            ("Playlists" Core..=) Core.<$> playlists,
            Core.Just ("PipelineId" Core..= pipelineId)
          ]
      )

instance Core.ToPath CreateJob where
  toPath = Core.const "/2012-09-25/jobs"

instance Core.ToQuery CreateJob where
  toQuery = Core.const Core.mempty

-- | The CreateJobResponse structure.
--
-- /See:/ 'newCreateJobResponse' smart constructor.
data CreateJobResponse = CreateJobResponse'
  { -- | A section of the response body that provides information about the job
    -- that is created.
    job :: Core.Maybe Job',
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateJobResponse
newCreateJobResponse pHttpStatus_ =
  CreateJobResponse'
    { job = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A section of the response body that provides information about the job
-- that is created.
createJobResponse_job :: Lens.Lens' CreateJobResponse (Core.Maybe Job')
createJobResponse_job = Lens.lens (\CreateJobResponse' {job} -> job) (\s@CreateJobResponse' {} a -> s {job = a} :: CreateJobResponse)

-- | The response's http status code.
createJobResponse_httpStatus :: Lens.Lens' CreateJobResponse Core.Int
createJobResponse_httpStatus = Lens.lens (\CreateJobResponse' {httpStatus} -> httpStatus) (\s@CreateJobResponse' {} a -> s {httpStatus = a} :: CreateJobResponse)

instance Core.NFData CreateJobResponse
