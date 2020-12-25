{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.CreateJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- When you create a job, Elastic Transcoder returns JSON data that includes the values that you specified plus information about the job that is created.
--
-- If you have specified more than one output for your jobs (for example, one output for the Kindle Fire and another output for the Apple iPhone 4s), you currently must use the Elastic Transcoder API to list the jobs (as opposed to the AWS Console).
module Network.AWS.ElasticTranscoder.CreateJob
  ( -- * Creating a request
    CreateJob (..),
    mkCreateJob,

    -- ** Request lenses
    cjPipelineId,
    cjInput,
    cjInputs,
    cjOutput,
    cjOutputKeyPrefix,
    cjOutputs,
    cjPlaylists,
    cjUserMetadata,

    -- * Destructuring the response
    CreateJobResponse (..),
    mkCreateJobResponse,

    -- ** Response lenses
    cjrrsJob,
    cjrrsResponseStatus,
  )
where

import qualified Network.AWS.ElasticTranscoder.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The @CreateJobRequest@ structure.
--
-- /See:/ 'mkCreateJob' smart constructor.
data CreateJob = CreateJob'
  { -- | The @Id@ of the pipeline that you want Elastic Transcoder to use for transcoding. The pipeline determines several settings, including the Amazon S3 bucket from which Elastic Transcoder gets the files to transcode and the bucket into which Elastic Transcoder puts the transcoded files.
    pipelineId :: Types.PipelineId,
    -- | A section of the request body that provides information about the file that is being transcoded.
    input :: Core.Maybe Types.JobInput,
    -- | A section of the request body that provides information about the files that are being transcoded.
    inputs :: Core.Maybe [Types.JobInput],
    -- | A section of the request body that provides information about the transcoded (target) file. We strongly recommend that you use the @Outputs@ syntax instead of the @Output@ syntax.
    output :: Core.Maybe Types.CreateJobOutput,
    -- | The value, if any, that you want Elastic Transcoder to prepend to the names of all files that this job creates, including output files, thumbnails, and playlists.
    outputKeyPrefix :: Core.Maybe Types.OutputKeyPrefix,
    -- | A section of the request body that provides information about the transcoded (target) files. We recommend that you use the @Outputs@ syntax instead of the @Output@ syntax.
    outputs :: Core.Maybe [Types.CreateJobOutput],
    -- | If you specify a preset in @PresetId@ for which the value of @Container@ is fmp4 (Fragmented MP4) or ts (MPEG-TS), Playlists contains information about the master playlists that you want Elastic Transcoder to create.
    --
    -- The maximum number of master playlists in a job is 30.
    playlists :: Core.Maybe [Types.CreateJobPlaylist],
    -- | User-defined metadata that you want to associate with an Elastic Transcoder job. You specify metadata in @key/value@ pairs, and you can add up to 10 @key/value@ pairs per job. Elastic Transcoder does not guarantee that @key/value@ pairs are returned in the same order in which you specify them.
    userMetadata :: Core.Maybe (Core.HashMap Types.String Types.String)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateJob' value with any optional fields omitted.
mkCreateJob ::
  -- | 'pipelineId'
  Types.PipelineId ->
  CreateJob
mkCreateJob pipelineId =
  CreateJob'
    { pipelineId,
      input = Core.Nothing,
      inputs = Core.Nothing,
      output = Core.Nothing,
      outputKeyPrefix = Core.Nothing,
      outputs = Core.Nothing,
      playlists = Core.Nothing,
      userMetadata = Core.Nothing
    }

-- | The @Id@ of the pipeline that you want Elastic Transcoder to use for transcoding. The pipeline determines several settings, including the Amazon S3 bucket from which Elastic Transcoder gets the files to transcode and the bucket into which Elastic Transcoder puts the transcoded files.
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjPipelineId :: Lens.Lens' CreateJob Types.PipelineId
cjPipelineId = Lens.field @"pipelineId"
{-# DEPRECATED cjPipelineId "Use generic-lens or generic-optics with 'pipelineId' instead." #-}

-- | A section of the request body that provides information about the file that is being transcoded.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjInput :: Lens.Lens' CreateJob (Core.Maybe Types.JobInput)
cjInput = Lens.field @"input"
{-# DEPRECATED cjInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | A section of the request body that provides information about the files that are being transcoded.
--
-- /Note:/ Consider using 'inputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjInputs :: Lens.Lens' CreateJob (Core.Maybe [Types.JobInput])
cjInputs = Lens.field @"inputs"
{-# DEPRECATED cjInputs "Use generic-lens or generic-optics with 'inputs' instead." #-}

-- | A section of the request body that provides information about the transcoded (target) file. We strongly recommend that you use the @Outputs@ syntax instead of the @Output@ syntax.
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjOutput :: Lens.Lens' CreateJob (Core.Maybe Types.CreateJobOutput)
cjOutput = Lens.field @"output"
{-# DEPRECATED cjOutput "Use generic-lens or generic-optics with 'output' instead." #-}

-- | The value, if any, that you want Elastic Transcoder to prepend to the names of all files that this job creates, including output files, thumbnails, and playlists.
--
-- /Note:/ Consider using 'outputKeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjOutputKeyPrefix :: Lens.Lens' CreateJob (Core.Maybe Types.OutputKeyPrefix)
cjOutputKeyPrefix = Lens.field @"outputKeyPrefix"
{-# DEPRECATED cjOutputKeyPrefix "Use generic-lens or generic-optics with 'outputKeyPrefix' instead." #-}

-- | A section of the request body that provides information about the transcoded (target) files. We recommend that you use the @Outputs@ syntax instead of the @Output@ syntax.
--
-- /Note:/ Consider using 'outputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjOutputs :: Lens.Lens' CreateJob (Core.Maybe [Types.CreateJobOutput])
cjOutputs = Lens.field @"outputs"
{-# DEPRECATED cjOutputs "Use generic-lens or generic-optics with 'outputs' instead." #-}

-- | If you specify a preset in @PresetId@ for which the value of @Container@ is fmp4 (Fragmented MP4) or ts (MPEG-TS), Playlists contains information about the master playlists that you want Elastic Transcoder to create.
--
-- The maximum number of master playlists in a job is 30.
--
-- /Note:/ Consider using 'playlists' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjPlaylists :: Lens.Lens' CreateJob (Core.Maybe [Types.CreateJobPlaylist])
cjPlaylists = Lens.field @"playlists"
{-# DEPRECATED cjPlaylists "Use generic-lens or generic-optics with 'playlists' instead." #-}

-- | User-defined metadata that you want to associate with an Elastic Transcoder job. You specify metadata in @key/value@ pairs, and you can add up to 10 @key/value@ pairs per job. Elastic Transcoder does not guarantee that @key/value@ pairs are returned in the same order in which you specify them.
--
-- /Note:/ Consider using 'userMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjUserMetadata :: Lens.Lens' CreateJob (Core.Maybe (Core.HashMap Types.String Types.String))
cjUserMetadata = Lens.field @"userMetadata"
{-# DEPRECATED cjUserMetadata "Use generic-lens or generic-optics with 'userMetadata' instead." #-}

instance Core.FromJSON CreateJob where
  toJSON CreateJob {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("PipelineId" Core..= pipelineId),
            ("Input" Core..=) Core.<$> input,
            ("Inputs" Core..=) Core.<$> inputs,
            ("Output" Core..=) Core.<$> output,
            ("OutputKeyPrefix" Core..=) Core.<$> outputKeyPrefix,
            ("Outputs" Core..=) Core.<$> outputs,
            ("Playlists" Core..=) Core.<$> playlists,
            ("UserMetadata" Core..=) Core.<$> userMetadata
          ]
      )

instance Core.AWSRequest CreateJob where
  type Rs CreateJob = CreateJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/2012-09-25/jobs",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateJobResponse'
            Core.<$> (x Core..:? "Job") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The CreateJobResponse structure.
--
-- /See:/ 'mkCreateJobResponse' smart constructor.
data CreateJobResponse = CreateJobResponse'
  { -- | A section of the response body that provides information about the job that is created.
    job :: Core.Maybe Types.Job',
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateJobResponse' value with any optional fields omitted.
mkCreateJobResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateJobResponse
mkCreateJobResponse responseStatus =
  CreateJobResponse' {job = Core.Nothing, responseStatus}

-- | A section of the response body that provides information about the job that is created.
--
-- /Note:/ Consider using 'job' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrrsJob :: Lens.Lens' CreateJobResponse (Core.Maybe Types.Job')
cjrrsJob = Lens.field @"job"
{-# DEPRECATED cjrrsJob "Use generic-lens or generic-optics with 'job' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrrsResponseStatus :: Lens.Lens' CreateJobResponse Core.Int
cjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
