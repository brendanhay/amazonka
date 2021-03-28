{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.Types.Job'
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticTranscoder.Types.Job'
  ( Job' (..)
  -- * Smart constructor
  , mkJob'
  -- * Lenses
  , jArn
  , jId
  , jInput
  , jInputs
  , jOutput
  , jOutputKeyPrefix
  , jOutputs
  , jPipelineId
  , jPlaylists
  , jStatus
  , jTiming
  , jUserMetadata
  ) where

import qualified Network.AWS.ElasticTranscoder.Types.Id as Types
import qualified Network.AWS.ElasticTranscoder.Types.JobInput as Types
import qualified Network.AWS.ElasticTranscoder.Types.JobOutput as Types
import qualified Network.AWS.ElasticTranscoder.Types.OutputKeyPrefix as Types
import qualified Network.AWS.ElasticTranscoder.Types.PipelineId as Types
import qualified Network.AWS.ElasticTranscoder.Types.Playlist as Types
import qualified Network.AWS.ElasticTranscoder.Types.Status as Types
import qualified Network.AWS.ElasticTranscoder.Types.Timing as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A section of the response body that provides information about the job that is created.
--
-- /See:/ 'mkJob'' smart constructor.
data Job' = Job''
  { arn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) for the job.
  , id :: Core.Maybe Types.Id
    -- ^ The identifier that Elastic Transcoder assigned to the job. You use this value to get settings for the job or to delete the job.
  , input :: Core.Maybe Types.JobInput
    -- ^ A section of the request or response body that provides information about the file that is being transcoded.
  , inputs :: Core.Maybe [Types.JobInput]
    -- ^ Information about the files that you're transcoding. If you specified multiple files for this job, Elastic Transcoder stitches the files together to make one output.
  , output :: Core.Maybe Types.JobOutput
    -- ^ If you specified one output for a job, information about that output. If you specified multiple outputs for a job, the Output object lists information about the first output. This duplicates the information that is listed for the first output in the Outputs object.
--
-- /Important:/ Outputs recommended instead.
-- A section of the request or response body that provides information about the transcoded (target) file. 
  , outputKeyPrefix :: Core.Maybe Types.OutputKeyPrefix
    -- ^ The value, if any, that you want Elastic Transcoder to prepend to the names of all files that this job creates, including output files, thumbnails, and playlists. We recommend that you add a / or some other delimiter to the end of the @OutputKeyPrefix@ .
  , outputs :: Core.Maybe [Types.JobOutput]
    -- ^ Information about the output files. We recommend that you use the @Outputs@ syntax for all jobs, even when you want Elastic Transcoder to transcode a file into only one format. Do not use both the @Outputs@ and @Output@ syntaxes in the same request. You can create a maximum of 30 outputs per job. 
--
-- If you specify more than one output for a job, Elastic Transcoder creates the files for each output in the order in which you specify them in the job.
  , pipelineId :: Core.Maybe Types.PipelineId
    -- ^ The @Id@ of the pipeline that you want Elastic Transcoder to use for transcoding. The pipeline determines several settings, including the Amazon S3 bucket from which Elastic Transcoder gets the files to transcode and the bucket into which Elastic Transcoder puts the transcoded files. 
  , playlists :: Core.Maybe [Types.Playlist]
    -- ^ /Important:/ Outputs in Fragmented MP4 or MPEG-TS format only.
--
-- If you specify a preset in @PresetId@ for which the value of @Container@ is fmp4 (Fragmented MP4) or ts (MPEG-TS), @Playlists@ contains information about the master playlists that you want Elastic Transcoder to create.
-- The maximum number of master playlists in a job is 30.
  , status :: Core.Maybe Types.Status
    -- ^ The status of the job: @Submitted@ , @Progressing@ , @Complete@ , @Canceled@ , or @Error@ . 
  , timing :: Core.Maybe Types.Timing
    -- ^ Details about the timing of a job.
  , userMetadata :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ User-defined metadata that you want to associate with an Elastic Transcoder job. You specify metadata in @key/value@ pairs, and you can add up to 10 @key/value@ pairs per job. Elastic Transcoder does not guarantee that @key/value@ pairs are returned in the same order in which you specify them.
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Job'' value with any optional fields omitted.
mkJob'
    :: Job'
mkJob'
  = Job''{arn = Core.Nothing, id = Core.Nothing,
          input = Core.Nothing, inputs = Core.Nothing, output = Core.Nothing,
          outputKeyPrefix = Core.Nothing, outputs = Core.Nothing,
          pipelineId = Core.Nothing, playlists = Core.Nothing,
          status = Core.Nothing, timing = Core.Nothing,
          userMetadata = Core.Nothing}

-- | The Amazon Resource Name (ARN) for the job.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jArn :: Lens.Lens' Job' (Core.Maybe Core.Text)
jArn = Lens.field @"arn"
{-# INLINEABLE jArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The identifier that Elastic Transcoder assigned to the job. You use this value to get settings for the job or to delete the job.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jId :: Lens.Lens' Job' (Core.Maybe Types.Id)
jId = Lens.field @"id"
{-# INLINEABLE jId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | A section of the request or response body that provides information about the file that is being transcoded.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jInput :: Lens.Lens' Job' (Core.Maybe Types.JobInput)
jInput = Lens.field @"input"
{-# INLINEABLE jInput #-}
{-# DEPRECATED input "Use generic-lens or generic-optics with 'input' instead"  #-}

-- | Information about the files that you're transcoding. If you specified multiple files for this job, Elastic Transcoder stitches the files together to make one output.
--
-- /Note:/ Consider using 'inputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jInputs :: Lens.Lens' Job' (Core.Maybe [Types.JobInput])
jInputs = Lens.field @"inputs"
{-# INLINEABLE jInputs #-}
{-# DEPRECATED inputs "Use generic-lens or generic-optics with 'inputs' instead"  #-}

-- | If you specified one output for a job, information about that output. If you specified multiple outputs for a job, the Output object lists information about the first output. This duplicates the information that is listed for the first output in the Outputs object.
--
-- /Important:/ Outputs recommended instead.
-- A section of the request or response body that provides information about the transcoded (target) file. 
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jOutput :: Lens.Lens' Job' (Core.Maybe Types.JobOutput)
jOutput = Lens.field @"output"
{-# INLINEABLE jOutput #-}
{-# DEPRECATED output "Use generic-lens or generic-optics with 'output' instead"  #-}

-- | The value, if any, that you want Elastic Transcoder to prepend to the names of all files that this job creates, including output files, thumbnails, and playlists. We recommend that you add a / or some other delimiter to the end of the @OutputKeyPrefix@ .
--
-- /Note:/ Consider using 'outputKeyPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jOutputKeyPrefix :: Lens.Lens' Job' (Core.Maybe Types.OutputKeyPrefix)
jOutputKeyPrefix = Lens.field @"outputKeyPrefix"
{-# INLINEABLE jOutputKeyPrefix #-}
{-# DEPRECATED outputKeyPrefix "Use generic-lens or generic-optics with 'outputKeyPrefix' instead"  #-}

-- | Information about the output files. We recommend that you use the @Outputs@ syntax for all jobs, even when you want Elastic Transcoder to transcode a file into only one format. Do not use both the @Outputs@ and @Output@ syntaxes in the same request. You can create a maximum of 30 outputs per job. 
--
-- If you specify more than one output for a job, Elastic Transcoder creates the files for each output in the order in which you specify them in the job.
--
-- /Note:/ Consider using 'outputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jOutputs :: Lens.Lens' Job' (Core.Maybe [Types.JobOutput])
jOutputs = Lens.field @"outputs"
{-# INLINEABLE jOutputs #-}
{-# DEPRECATED outputs "Use generic-lens or generic-optics with 'outputs' instead"  #-}

-- | The @Id@ of the pipeline that you want Elastic Transcoder to use for transcoding. The pipeline determines several settings, including the Amazon S3 bucket from which Elastic Transcoder gets the files to transcode and the bucket into which Elastic Transcoder puts the transcoded files. 
--
-- /Note:/ Consider using 'pipelineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jPipelineId :: Lens.Lens' Job' (Core.Maybe Types.PipelineId)
jPipelineId = Lens.field @"pipelineId"
{-# INLINEABLE jPipelineId #-}
{-# DEPRECATED pipelineId "Use generic-lens or generic-optics with 'pipelineId' instead"  #-}

-- | /Important:/ Outputs in Fragmented MP4 or MPEG-TS format only.
--
-- If you specify a preset in @PresetId@ for which the value of @Container@ is fmp4 (Fragmented MP4) or ts (MPEG-TS), @Playlists@ contains information about the master playlists that you want Elastic Transcoder to create.
-- The maximum number of master playlists in a job is 30.
--
-- /Note:/ Consider using 'playlists' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jPlaylists :: Lens.Lens' Job' (Core.Maybe [Types.Playlist])
jPlaylists = Lens.field @"playlists"
{-# INLINEABLE jPlaylists #-}
{-# DEPRECATED playlists "Use generic-lens or generic-optics with 'playlists' instead"  #-}

-- | The status of the job: @Submitted@ , @Progressing@ , @Complete@ , @Canceled@ , or @Error@ . 
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jStatus :: Lens.Lens' Job' (Core.Maybe Types.Status)
jStatus = Lens.field @"status"
{-# INLINEABLE jStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | Details about the timing of a job.
--
-- /Note:/ Consider using 'timing' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jTiming :: Lens.Lens' Job' (Core.Maybe Types.Timing)
jTiming = Lens.field @"timing"
{-# INLINEABLE jTiming #-}
{-# DEPRECATED timing "Use generic-lens or generic-optics with 'timing' instead"  #-}

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
jUserMetadata :: Lens.Lens' Job' (Core.Maybe (Core.HashMap Core.Text Core.Text))
jUserMetadata = Lens.field @"userMetadata"
{-# INLINEABLE jUserMetadata #-}
{-# DEPRECATED userMetadata "Use generic-lens or generic-optics with 'userMetadata' instead"  #-}

instance Core.FromJSON Job' where
        parseJSON
          = Core.withObject "Job'" Core.$
              \ x ->
                Job'' Core.<$>
                  (x Core..:? "Arn") Core.<*> x Core..:? "Id" Core.<*>
                    x Core..:? "Input"
                    Core.<*> x Core..:? "Inputs"
                    Core.<*> x Core..:? "Output"
                    Core.<*> x Core..:? "OutputKeyPrefix"
                    Core.<*> x Core..:? "Outputs"
                    Core.<*> x Core..:? "PipelineId"
                    Core.<*> x Core..:? "Playlists"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "Timing"
                    Core.<*> x Core..:? "UserMetadata"
