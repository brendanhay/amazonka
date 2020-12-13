{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Job'
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticTranscoder.Types.Job'
  ( Job' (..),

    -- * Smart constructor
    mkJob',

    -- * Lenses
    jStatus,
    jPipelineId,
    jARN,
    jInputs,
    jInput,
    jUserMetadata,
    jOutputs,
    jOutput,
    jId,
    jPlaylists,
    jOutputKeyPrefix,
    jTiming,
  )
where

import Network.AWS.ElasticTranscoder.Types.JobInput
import Network.AWS.ElasticTranscoder.Types.JobOutput
import Network.AWS.ElasticTranscoder.Types.Playlist
import Network.AWS.ElasticTranscoder.Types.Timing
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A section of the response body that provides information about the job that is created.
--
-- /See:/ 'mkJob'' smart constructor.
data Job' = Job''
  { -- | The status of the job: @Submitted@ , @Progressing@ , @Complete@ , @Canceled@ , or @Error@ .
    status :: Lude.Maybe Lude.Text,
    -- | The @Id@ of the pipeline that you want Elastic Transcoder to use for transcoding. The pipeline determines several settings, including the Amazon S3 bucket from which Elastic Transcoder gets the files to transcode and the bucket into which Elastic Transcoder puts the transcoded files.
    pipelineId :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) for the job.
    arn :: Lude.Maybe Lude.Text,
    -- | Information about the files that you're transcoding. If you specified multiple files for this job, Elastic Transcoder stitches the files together to make one output.
    inputs :: Lude.Maybe [JobInput],
    -- | A section of the request or response body that provides information about the file that is being transcoded.
    input :: Lude.Maybe JobInput,
    -- | User-defined metadata that you want to associate with an Elastic Transcoder job. You specify metadata in @key/value@ pairs, and you can add up to 10 @key/value@ pairs per job. Elastic Transcoder does not guarantee that @key/value@ pairs are returned in the same order in which you specify them.
    --
    -- Metadata @keys@ and @values@ must use characters from the following list:
    --
    --     * @0-9@
    --
    --
    --     * @A-Z@ and @a-z@
    --
    --
    --     * @Space@
    --
    --
    --     * The following symbols: @_.:/=+-%@@
    userMetadata :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | Information about the output files. We recommend that you use the @Outputs@ syntax for all jobs, even when you want Elastic Transcoder to transcode a file into only one format. Do not use both the @Outputs@ and @Output@ syntaxes in the same request. You can create a maximum of 30 outputs per job.
    --
    -- If you specify more than one output for a job, Elastic Transcoder creates the files for each output in the order in which you specify them in the job.
    outputs :: Lude.Maybe [JobOutput],
    -- | If you specified one output for a job, information about that output. If you specified multiple outputs for a job, the Output object lists information about the first output. This duplicates the information that is listed for the first output in the Outputs object.
    --
    -- /Important:/ Outputs recommended instead.
    -- A section of the request or response body that provides information about the transcoded (target) file.
    output :: Lude.Maybe JobOutput,
    -- | The identifier that Elastic Transcoder assigned to the job. You use this value to get settings for the job or to delete the job.
    id :: Lude.Maybe Lude.Text,
    -- | /Important:/ Outputs in Fragmented MP4 or MPEG-TS format only.
    --
    -- If you specify a preset in @PresetId@ for which the value of @Container@ is fmp4 (Fragmented MP4) or ts (MPEG-TS), @Playlists@ contains information about the master playlists that you want Elastic Transcoder to create.
    -- The maximum number of master playlists in a job is 30.
    playlists :: Lude.Maybe [Playlist],
    -- | The value, if any, that you want Elastic Transcoder to prepend to the names of all files that this job creates, including output files, thumbnails, and playlists. We recommend that you add a / or some other delimiter to the end of the @OutputKeyPrefix@ .
    outputKeyPrefix :: Lude.Maybe Lude.Text,
    -- | Details about the timing of a job.
    timing :: Lude.Maybe Timing
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Job'' with the minimum fields required to make a request.
--
-- * 'status' - The status of the job: @Submitted@ , @Progressing@ , @Complete@ , @Canceled@ , or @Error@ .
-- * 'pipelineId' - The @Id@ of the pipeline that you want Elastic Transcoder to use for transcoding. The pipeline determines several settings, including the Amazon S3 bucket from which Elastic Transcoder gets the files to transcode and the bucket into which Elastic Transcoder puts the transcoded files.
-- * 'arn' - The Amazon Resource Name (ARN) for the job.
-- * 'inputs' - Information about the files that you're transcoding. If you specified multiple files for this job, Elastic Transcoder stitches the files together to make one output.
-- * 'input' - A section of the request or response body that provides information about the file that is being transcoded.
-- * 'userMetadata' - User-defined metadata that you want to associate with an Elastic Transcoder job. You specify metadata in @key/value@ pairs, and you can add up to 10 @key/value@ pairs per job. Elastic Transcoder does not guarantee that @key/value@ pairs are returned in the same order in which you specify them.
--
-- Metadata @keys@ and @values@ must use characters from the following list:
--
--     * @0-9@
--
--
--     * @A-Z@ and @a-z@
--
--
--     * @Space@
--
--
--     * The following symbols: @_.:/=+-%@@
--
--
-- * 'outputs' - Information about the output files. We recommend that you use the @Outputs@ syntax for all jobs, even when you want Elastic Transcoder to transcode a file into only one format. Do not use both the @Outputs@ and @Output@ syntaxes in the same request. You can create a maximum of 30 outputs per job.
--
-- If you specify more than one output for a job, Elastic Transcoder creates the files for each output in the order in which you specify them in the job.
-- * 'output' - If you specified one output for a job, information about that output. If you specified multiple outputs for a job, the Output object lists information about the first output. This duplicates the information that is listed for the first output in the Outputs object.
--
-- /Important:/ Outputs recommended instead.
-- A section of the request or response body that provides information about the transcoded (target) file.
-- * 'id' - The identifier that Elastic Transcoder assigned to the job. You use this value to get settings for the job or to delete the job.
-- * 'playlists' - /Important:/ Outputs in Fragmented MP4 or MPEG-TS format only.
--
-- If you specify a preset in @PresetId@ for which the value of @Container@ is fmp4 (Fragmented MP4) or ts (MPEG-TS), @Playlists@ contains information about the master playlists that you want Elastic Transcoder to create.
-- The maximum number of master playlists in a job is 30.
-- * 'outputKeyPrefix' - The value, if any, that you want Elastic Transcoder to prepend to the names of all files that this job creates, including output files, thumbnails, and playlists. We recommend that you add a / or some other delimiter to the end of the @OutputKeyPrefix@ .
-- * 'timing' - Details about the timing of a job.
mkJob' ::
  Job'
mkJob' =
  Job''
    { status = Lude.Nothing,
      pipelineId = Lude.Nothing,
      arn = Lude.Nothing,
      inputs = Lude.Nothing,
      input = Lude.Nothing,
      userMetadata = Lude.Nothing,
      outputs = Lude.Nothing,
      output = Lude.Nothing,
      id = Lude.Nothing,
      playlists = Lude.Nothing,
      outputKeyPrefix = Lude.Nothing,
      timing = Lude.Nothing
    }

-- | The status of the job: @Submitted@ , @Progressing@ , @Complete@ , @Canceled@ , or @Error@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jStatus :: Lens.Lens' Job' (Lude.Maybe Lude.Text)
jStatus = Lens.lens (status :: Job' -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: Job')
{-# DEPRECATED jStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The @Id@ of the pipeline that you want Elastic Transcoder to use for transcoding. The pipeline determines several settings, including the Amazon S3 bucket from which Elastic Transcoder gets the files to transcode and the bucket into which Elastic Transcoder puts the transcoded files.
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jPipelineId :: Lens.Lens' Job' (Lude.Maybe Lude.Text)
jPipelineId = Lens.lens (pipelineId :: Job' -> Lude.Maybe Lude.Text) (\s a -> s {pipelineId = a} :: Job')
{-# DEPRECATED jPipelineId "Use generic-lens or generic-optics with 'pipelineId' instead." #-}

-- | The Amazon Resource Name (ARN) for the job.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jARN :: Lens.Lens' Job' (Lude.Maybe Lude.Text)
jARN = Lens.lens (arn :: Job' -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Job')
{-# DEPRECATED jARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Information about the files that you're transcoding. If you specified multiple files for this job, Elastic Transcoder stitches the files together to make one output.
--
-- /Note:/ Consider using 'inputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jInputs :: Lens.Lens' Job' (Lude.Maybe [JobInput])
jInputs = Lens.lens (inputs :: Job' -> Lude.Maybe [JobInput]) (\s a -> s {inputs = a} :: Job')
{-# DEPRECATED jInputs "Use generic-lens or generic-optics with 'inputs' instead." #-}

-- | A section of the request or response body that provides information about the file that is being transcoded.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jInput :: Lens.Lens' Job' (Lude.Maybe JobInput)
jInput = Lens.lens (input :: Job' -> Lude.Maybe JobInput) (\s a -> s {input = a} :: Job')
{-# DEPRECATED jInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | User-defined metadata that you want to associate with an Elastic Transcoder job. You specify metadata in @key/value@ pairs, and you can add up to 10 @key/value@ pairs per job. Elastic Transcoder does not guarantee that @key/value@ pairs are returned in the same order in which you specify them.
--
-- Metadata @keys@ and @values@ must use characters from the following list:
--
--     * @0-9@
--
--
--     * @A-Z@ and @a-z@
--
--
--     * @Space@
--
--
--     * The following symbols: @_.:/=+-%@@
--
--
--
-- /Note:/ Consider using 'userMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jUserMetadata :: Lens.Lens' Job' (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
jUserMetadata = Lens.lens (userMetadata :: Job' -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {userMetadata = a} :: Job')
{-# DEPRECATED jUserMetadata "Use generic-lens or generic-optics with 'userMetadata' instead." #-}

-- | Information about the output files. We recommend that you use the @Outputs@ syntax for all jobs, even when you want Elastic Transcoder to transcode a file into only one format. Do not use both the @Outputs@ and @Output@ syntaxes in the same request. You can create a maximum of 30 outputs per job.
--
-- If you specify more than one output for a job, Elastic Transcoder creates the files for each output in the order in which you specify them in the job.
--
-- /Note:/ Consider using 'outputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jOutputs :: Lens.Lens' Job' (Lude.Maybe [JobOutput])
jOutputs = Lens.lens (outputs :: Job' -> Lude.Maybe [JobOutput]) (\s a -> s {outputs = a} :: Job')
{-# DEPRECATED jOutputs "Use generic-lens or generic-optics with 'outputs' instead." #-}

-- | If you specified one output for a job, information about that output. If you specified multiple outputs for a job, the Output object lists information about the first output. This duplicates the information that is listed for the first output in the Outputs object.
--
-- /Important:/ Outputs recommended instead.
-- A section of the request or response body that provides information about the transcoded (target) file.
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jOutput :: Lens.Lens' Job' (Lude.Maybe JobOutput)
jOutput = Lens.lens (output :: Job' -> Lude.Maybe JobOutput) (\s a -> s {output = a} :: Job')
{-# DEPRECATED jOutput "Use generic-lens or generic-optics with 'output' instead." #-}

-- | The identifier that Elastic Transcoder assigned to the job. You use this value to get settings for the job or to delete the job.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jId :: Lens.Lens' Job' (Lude.Maybe Lude.Text)
jId = Lens.lens (id :: Job' -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Job')
{-# DEPRECATED jId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | /Important:/ Outputs in Fragmented MP4 or MPEG-TS format only.
--
-- If you specify a preset in @PresetId@ for which the value of @Container@ is fmp4 (Fragmented MP4) or ts (MPEG-TS), @Playlists@ contains information about the master playlists that you want Elastic Transcoder to create.
-- The maximum number of master playlists in a job is 30.
--
-- /Note:/ Consider using 'playlists' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jPlaylists :: Lens.Lens' Job' (Lude.Maybe [Playlist])
jPlaylists = Lens.lens (playlists :: Job' -> Lude.Maybe [Playlist]) (\s a -> s {playlists = a} :: Job')
{-# DEPRECATED jPlaylists "Use generic-lens or generic-optics with 'playlists' instead." #-}

-- | The value, if any, that you want Elastic Transcoder to prepend to the names of all files that this job creates, including output files, thumbnails, and playlists. We recommend that you add a / or some other delimiter to the end of the @OutputKeyPrefix@ .
--
-- /Note:/ Consider using 'outputKeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jOutputKeyPrefix :: Lens.Lens' Job' (Lude.Maybe Lude.Text)
jOutputKeyPrefix = Lens.lens (outputKeyPrefix :: Job' -> Lude.Maybe Lude.Text) (\s a -> s {outputKeyPrefix = a} :: Job')
{-# DEPRECATED jOutputKeyPrefix "Use generic-lens or generic-optics with 'outputKeyPrefix' instead." #-}

-- | Details about the timing of a job.
--
-- /Note:/ Consider using 'timing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jTiming :: Lens.Lens' Job' (Lude.Maybe Timing)
jTiming = Lens.lens (timing :: Job' -> Lude.Maybe Timing) (\s a -> s {timing = a} :: Job')
{-# DEPRECATED jTiming "Use generic-lens or generic-optics with 'timing' instead." #-}

instance Lude.FromJSON Job' where
  parseJSON =
    Lude.withObject
      "Job'"
      ( \x ->
          Job''
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "PipelineId")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "Inputs" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Input")
            Lude.<*> (x Lude..:? "UserMetadata" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Outputs" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Output")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "Playlists" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "OutputKeyPrefix")
            Lude.<*> (x Lude..:? "Timing")
      )
