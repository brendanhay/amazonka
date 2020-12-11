{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    cjInputs,
    cjInput,
    cjUserMetadata,
    cjOutputs,
    cjOutput,
    cjPlaylists,
    cjOutputKeyPrefix,
    cjPipelineId,

    -- * Destructuring the response
    CreateJobResponse (..),
    mkCreateJobResponse,

    -- ** Response lenses
    cjrsJob,
    cjrsResponseStatus,
  )
where

import Network.AWS.ElasticTranscoder.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The @CreateJobRequest@ structure.
--
-- /See:/ 'mkCreateJob' smart constructor.
data CreateJob = CreateJob'
  { inputs :: Lude.Maybe [JobInput],
    input :: Lude.Maybe JobInput,
    userMetadata :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    outputs :: Lude.Maybe [CreateJobOutput],
    output :: Lude.Maybe CreateJobOutput,
    playlists :: Lude.Maybe [CreateJobPlaylist],
    outputKeyPrefix :: Lude.Maybe Lude.Text,
    pipelineId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateJob' with the minimum fields required to make a request.
--
-- * 'input' - A section of the request body that provides information about the file that is being transcoded.
-- * 'inputs' - A section of the request body that provides information about the files that are being transcoded.
-- * 'output' - A section of the request body that provides information about the transcoded (target) file. We strongly recommend that you use the @Outputs@ syntax instead of the @Output@ syntax.
-- * 'outputKeyPrefix' - The value, if any, that you want Elastic Transcoder to prepend to the names of all files that this job creates, including output files, thumbnails, and playlists.
-- * 'outputs' - A section of the request body that provides information about the transcoded (target) files. We recommend that you use the @Outputs@ syntax instead of the @Output@ syntax.
-- * 'pipelineId' - The @Id@ of the pipeline that you want Elastic Transcoder to use for transcoding. The pipeline determines several settings, including the Amazon S3 bucket from which Elastic Transcoder gets the files to transcode and the bucket into which Elastic Transcoder puts the transcoded files.
-- * 'playlists' - If you specify a preset in @PresetId@ for which the value of @Container@ is fmp4 (Fragmented MP4) or ts (MPEG-TS), Playlists contains information about the master playlists that you want Elastic Transcoder to create.
--
-- The maximum number of master playlists in a job is 30.
-- * 'userMetadata' - User-defined metadata that you want to associate with an Elastic Transcoder job. You specify metadata in @key/value@ pairs, and you can add up to 10 @key/value@ pairs per job. Elastic Transcoder does not guarantee that @key/value@ pairs are returned in the same order in which you specify them.
mkCreateJob ::
  -- | 'pipelineId'
  Lude.Text ->
  CreateJob
mkCreateJob pPipelineId_ =
  CreateJob'
    { inputs = Lude.Nothing,
      input = Lude.Nothing,
      userMetadata = Lude.Nothing,
      outputs = Lude.Nothing,
      output = Lude.Nothing,
      playlists = Lude.Nothing,
      outputKeyPrefix = Lude.Nothing,
      pipelineId = pPipelineId_
    }

-- | A section of the request body that provides information about the files that are being transcoded.
--
-- /Note:/ Consider using 'inputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjInputs :: Lens.Lens' CreateJob (Lude.Maybe [JobInput])
cjInputs = Lens.lens (inputs :: CreateJob -> Lude.Maybe [JobInput]) (\s a -> s {inputs = a} :: CreateJob)
{-# DEPRECATED cjInputs "Use generic-lens or generic-optics with 'inputs' instead." #-}

-- | A section of the request body that provides information about the file that is being transcoded.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjInput :: Lens.Lens' CreateJob (Lude.Maybe JobInput)
cjInput = Lens.lens (input :: CreateJob -> Lude.Maybe JobInput) (\s a -> s {input = a} :: CreateJob)
{-# DEPRECATED cjInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | User-defined metadata that you want to associate with an Elastic Transcoder job. You specify metadata in @key/value@ pairs, and you can add up to 10 @key/value@ pairs per job. Elastic Transcoder does not guarantee that @key/value@ pairs are returned in the same order in which you specify them.
--
-- /Note:/ Consider using 'userMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjUserMetadata :: Lens.Lens' CreateJob (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cjUserMetadata = Lens.lens (userMetadata :: CreateJob -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {userMetadata = a} :: CreateJob)
{-# DEPRECATED cjUserMetadata "Use generic-lens or generic-optics with 'userMetadata' instead." #-}

-- | A section of the request body that provides information about the transcoded (target) files. We recommend that you use the @Outputs@ syntax instead of the @Output@ syntax.
--
-- /Note:/ Consider using 'outputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjOutputs :: Lens.Lens' CreateJob (Lude.Maybe [CreateJobOutput])
cjOutputs = Lens.lens (outputs :: CreateJob -> Lude.Maybe [CreateJobOutput]) (\s a -> s {outputs = a} :: CreateJob)
{-# DEPRECATED cjOutputs "Use generic-lens or generic-optics with 'outputs' instead." #-}

-- | A section of the request body that provides information about the transcoded (target) file. We strongly recommend that you use the @Outputs@ syntax instead of the @Output@ syntax.
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjOutput :: Lens.Lens' CreateJob (Lude.Maybe CreateJobOutput)
cjOutput = Lens.lens (output :: CreateJob -> Lude.Maybe CreateJobOutput) (\s a -> s {output = a} :: CreateJob)
{-# DEPRECATED cjOutput "Use generic-lens or generic-optics with 'output' instead." #-}

-- | If you specify a preset in @PresetId@ for which the value of @Container@ is fmp4 (Fragmented MP4) or ts (MPEG-TS), Playlists contains information about the master playlists that you want Elastic Transcoder to create.
--
-- The maximum number of master playlists in a job is 30.
--
-- /Note:/ Consider using 'playlists' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjPlaylists :: Lens.Lens' CreateJob (Lude.Maybe [CreateJobPlaylist])
cjPlaylists = Lens.lens (playlists :: CreateJob -> Lude.Maybe [CreateJobPlaylist]) (\s a -> s {playlists = a} :: CreateJob)
{-# DEPRECATED cjPlaylists "Use generic-lens or generic-optics with 'playlists' instead." #-}

-- | The value, if any, that you want Elastic Transcoder to prepend to the names of all files that this job creates, including output files, thumbnails, and playlists.
--
-- /Note:/ Consider using 'outputKeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjOutputKeyPrefix :: Lens.Lens' CreateJob (Lude.Maybe Lude.Text)
cjOutputKeyPrefix = Lens.lens (outputKeyPrefix :: CreateJob -> Lude.Maybe Lude.Text) (\s a -> s {outputKeyPrefix = a} :: CreateJob)
{-# DEPRECATED cjOutputKeyPrefix "Use generic-lens or generic-optics with 'outputKeyPrefix' instead." #-}

-- | The @Id@ of the pipeline that you want Elastic Transcoder to use for transcoding. The pipeline determines several settings, including the Amazon S3 bucket from which Elastic Transcoder gets the files to transcode and the bucket into which Elastic Transcoder puts the transcoded files.
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjPipelineId :: Lens.Lens' CreateJob Lude.Text
cjPipelineId = Lens.lens (pipelineId :: CreateJob -> Lude.Text) (\s a -> s {pipelineId = a} :: CreateJob)
{-# DEPRECATED cjPipelineId "Use generic-lens or generic-optics with 'pipelineId' instead." #-}

instance Lude.AWSRequest CreateJob where
  type Rs CreateJob = CreateJobResponse
  request = Req.postJSON elasticTranscoderService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateJobResponse'
            Lude.<$> (x Lude..?> "Job") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateJob where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateJob where
  toJSON CreateJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Inputs" Lude..=) Lude.<$> inputs,
            ("Input" Lude..=) Lude.<$> input,
            ("UserMetadata" Lude..=) Lude.<$> userMetadata,
            ("Outputs" Lude..=) Lude.<$> outputs,
            ("Output" Lude..=) Lude.<$> output,
            ("Playlists" Lude..=) Lude.<$> playlists,
            ("OutputKeyPrefix" Lude..=) Lude.<$> outputKeyPrefix,
            Lude.Just ("PipelineId" Lude..= pipelineId)
          ]
      )

instance Lude.ToPath CreateJob where
  toPath = Lude.const "/2012-09-25/jobs"

instance Lude.ToQuery CreateJob where
  toQuery = Lude.const Lude.mempty

-- | The CreateJobResponse structure.
--
-- /See:/ 'mkCreateJobResponse' smart constructor.
data CreateJobResponse = CreateJobResponse'
  { job :: Lude.Maybe Job',
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateJobResponse' with the minimum fields required to make a request.
--
-- * 'job' - A section of the response body that provides information about the job that is created.
-- * 'responseStatus' - The response status code.
mkCreateJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateJobResponse
mkCreateJobResponse pResponseStatus_ =
  CreateJobResponse'
    { job = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A section of the response body that provides information about the job that is created.
--
-- /Note:/ Consider using 'job' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrsJob :: Lens.Lens' CreateJobResponse (Lude.Maybe Job')
cjrsJob = Lens.lens (job :: CreateJobResponse -> Lude.Maybe Job') (\s a -> s {job = a} :: CreateJobResponse)
{-# DEPRECATED cjrsJob "Use generic-lens or generic-optics with 'job' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrsResponseStatus :: Lens.Lens' CreateJobResponse Lude.Int
cjrsResponseStatus = Lens.lens (responseStatus :: CreateJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateJobResponse)
{-# DEPRECATED cjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
