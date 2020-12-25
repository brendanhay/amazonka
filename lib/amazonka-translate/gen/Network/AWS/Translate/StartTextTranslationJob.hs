{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.StartTextTranslationJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous batch translation job. Batch translation jobs can be used to translate large volumes of text across multiple documents at once. For more information, see 'async' .
--
-- Batch translation jobs can be described with the 'DescribeTextTranslationJob' operation, listed with the 'ListTextTranslationJobs' operation, and stopped with the 'StopTextTranslationJob' operation.
module Network.AWS.Translate.StartTextTranslationJob
  ( -- * Creating a request
    StartTextTranslationJob (..),
    mkStartTextTranslationJob,

    -- ** Request lenses
    sttjInputDataConfig,
    sttjOutputDataConfig,
    sttjDataAccessRoleArn,
    sttjSourceLanguageCode,
    sttjTargetLanguageCodes,
    sttjClientToken,
    sttjJobName,
    sttjParallelDataNames,
    sttjTerminologyNames,

    -- * Destructuring the response
    StartTextTranslationJobResponse (..),
    mkStartTextTranslationJobResponse,

    -- ** Response lenses
    srsJobId,
    srsJobStatus,
    srsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Translate.Types as Types

-- | /See:/ 'mkStartTextTranslationJob' smart constructor.
data StartTextTranslationJob = StartTextTranslationJob'
  { -- | Specifies the format and S3 location of the input documents for the translation job.
    inputDataConfig :: Types.InputDataConfig,
    -- | Specifies the S3 folder to which your job output will be saved.
    outputDataConfig :: Types.OutputDataConfig,
    -- | The Amazon Resource Name (ARN) of an AWS Identity Access and Management (IAM) role that grants Amazon Translate read access to your input data. For more nformation, see 'identity-and-access-management' .
    dataAccessRoleArn :: Types.IamRoleArn,
    -- | The language code of the input language. For a list of language codes, see 'what-is-languages' .
    --
    -- Amazon Translate does not automatically detect a source language during batch translation jobs.
    sourceLanguageCode :: Types.LanguageCodeString,
    -- | The language code of the output language.
    targetLanguageCodes :: Core.NonEmpty Types.LanguageCodeString,
    -- | A unique identifier for the request. This token is auto-generated when using the Amazon Translate SDK.
    clientToken :: Types.ClientTokenString,
    -- | The name of the batch translation job to be performed.
    jobName :: Core.Maybe Types.JobName,
    -- | The names of the parallel data resources to use in the batch translation job. For a list of available parallel data resources, use the 'ListParallelData' operation.
    parallelDataNames :: Core.Maybe [Types.ResourceName],
    -- | The name of the terminology to use in the batch translation job. For a list of available terminologies, use the 'ListTerminologies' operation.
    terminologyNames :: Core.Maybe [Types.ResourceName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartTextTranslationJob' value with any optional fields omitted.
mkStartTextTranslationJob ::
  -- | 'inputDataConfig'
  Types.InputDataConfig ->
  -- | 'outputDataConfig'
  Types.OutputDataConfig ->
  -- | 'dataAccessRoleArn'
  Types.IamRoleArn ->
  -- | 'sourceLanguageCode'
  Types.LanguageCodeString ->
  -- | 'targetLanguageCodes'
  Core.NonEmpty Types.LanguageCodeString ->
  -- | 'clientToken'
  Types.ClientTokenString ->
  StartTextTranslationJob
mkStartTextTranslationJob
  inputDataConfig
  outputDataConfig
  dataAccessRoleArn
  sourceLanguageCode
  targetLanguageCodes
  clientToken =
    StartTextTranslationJob'
      { inputDataConfig,
        outputDataConfig,
        dataAccessRoleArn,
        sourceLanguageCode,
        targetLanguageCodes,
        clientToken,
        jobName = Core.Nothing,
        parallelDataNames = Core.Nothing,
        terminologyNames = Core.Nothing
      }

-- | Specifies the format and S3 location of the input documents for the translation job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sttjInputDataConfig :: Lens.Lens' StartTextTranslationJob Types.InputDataConfig
sttjInputDataConfig = Lens.field @"inputDataConfig"
{-# DEPRECATED sttjInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | Specifies the S3 folder to which your job output will be saved.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sttjOutputDataConfig :: Lens.Lens' StartTextTranslationJob Types.OutputDataConfig
sttjOutputDataConfig = Lens.field @"outputDataConfig"
{-# DEPRECATED sttjOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | The Amazon Resource Name (ARN) of an AWS Identity Access and Management (IAM) role that grants Amazon Translate read access to your input data. For more nformation, see 'identity-and-access-management' .
--
-- /Note:/ Consider using 'dataAccessRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sttjDataAccessRoleArn :: Lens.Lens' StartTextTranslationJob Types.IamRoleArn
sttjDataAccessRoleArn = Lens.field @"dataAccessRoleArn"
{-# DEPRECATED sttjDataAccessRoleArn "Use generic-lens or generic-optics with 'dataAccessRoleArn' instead." #-}

-- | The language code of the input language. For a list of language codes, see 'what-is-languages' .
--
-- Amazon Translate does not automatically detect a source language during batch translation jobs.
--
-- /Note:/ Consider using 'sourceLanguageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sttjSourceLanguageCode :: Lens.Lens' StartTextTranslationJob Types.LanguageCodeString
sttjSourceLanguageCode = Lens.field @"sourceLanguageCode"
{-# DEPRECATED sttjSourceLanguageCode "Use generic-lens or generic-optics with 'sourceLanguageCode' instead." #-}

-- | The language code of the output language.
--
-- /Note:/ Consider using 'targetLanguageCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sttjTargetLanguageCodes :: Lens.Lens' StartTextTranslationJob (Core.NonEmpty Types.LanguageCodeString)
sttjTargetLanguageCodes = Lens.field @"targetLanguageCodes"
{-# DEPRECATED sttjTargetLanguageCodes "Use generic-lens or generic-optics with 'targetLanguageCodes' instead." #-}

-- | A unique identifier for the request. This token is auto-generated when using the Amazon Translate SDK.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sttjClientToken :: Lens.Lens' StartTextTranslationJob Types.ClientTokenString
sttjClientToken = Lens.field @"clientToken"
{-# DEPRECATED sttjClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The name of the batch translation job to be performed.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sttjJobName :: Lens.Lens' StartTextTranslationJob (Core.Maybe Types.JobName)
sttjJobName = Lens.field @"jobName"
{-# DEPRECATED sttjJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The names of the parallel data resources to use in the batch translation job. For a list of available parallel data resources, use the 'ListParallelData' operation.
--
-- /Note:/ Consider using 'parallelDataNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sttjParallelDataNames :: Lens.Lens' StartTextTranslationJob (Core.Maybe [Types.ResourceName])
sttjParallelDataNames = Lens.field @"parallelDataNames"
{-# DEPRECATED sttjParallelDataNames "Use generic-lens or generic-optics with 'parallelDataNames' instead." #-}

-- | The name of the terminology to use in the batch translation job. For a list of available terminologies, use the 'ListTerminologies' operation.
--
-- /Note:/ Consider using 'terminologyNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sttjTerminologyNames :: Lens.Lens' StartTextTranslationJob (Core.Maybe [Types.ResourceName])
sttjTerminologyNames = Lens.field @"terminologyNames"
{-# DEPRECATED sttjTerminologyNames "Use generic-lens or generic-optics with 'terminologyNames' instead." #-}

instance Core.FromJSON StartTextTranslationJob where
  toJSON StartTextTranslationJob {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("InputDataConfig" Core..= inputDataConfig),
            Core.Just ("OutputDataConfig" Core..= outputDataConfig),
            Core.Just ("DataAccessRoleArn" Core..= dataAccessRoleArn),
            Core.Just ("SourceLanguageCode" Core..= sourceLanguageCode),
            Core.Just ("TargetLanguageCodes" Core..= targetLanguageCodes),
            Core.Just ("ClientToken" Core..= clientToken),
            ("JobName" Core..=) Core.<$> jobName,
            ("ParallelDataNames" Core..=) Core.<$> parallelDataNames,
            ("TerminologyNames" Core..=) Core.<$> terminologyNames
          ]
      )

instance Core.AWSRequest StartTextTranslationJob where
  type Rs StartTextTranslationJob = StartTextTranslationJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSShineFrontendService_20170701.StartTextTranslationJob"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartTextTranslationJobResponse'
            Core.<$> (x Core..:? "JobId")
            Core.<*> (x Core..:? "JobStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartTextTranslationJobResponse' smart constructor.
data StartTextTranslationJobResponse = StartTextTranslationJobResponse'
  { -- | The identifier generated for the job. To get the status of a job, use this ID with the 'DescribeTextTranslationJob' operation.
    jobId :: Core.Maybe Types.JobId,
    -- | The status of the job. Possible values include:
    --
    --
    --     * @SUBMITTED@ - The job has been received and is queued for processing.
    --
    --
    --     * @IN_PROGRESS@ - Amazon Translate is processing the job.
    --
    --
    --     * @COMPLETED@ - The job was successfully completed and the output is available.
    --
    --
    --     * @COMPLETED_WITH_ERROR@ - The job was completed with errors. The errors can be analyzed in the job's output.
    --
    --
    --     * @FAILED@ - The job did not complete. To get details, use the 'DescribeTextTranslationJob' operation.
    --
    --
    --     * @STOP_REQUESTED@ - The user who started the job has requested that it be stopped.
    --
    --
    --     * @STOPPED@ - The job has been stopped.
    jobStatus :: Core.Maybe Types.JobStatus,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartTextTranslationJobResponse' value with any optional fields omitted.
mkStartTextTranslationJobResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartTextTranslationJobResponse
mkStartTextTranslationJobResponse responseStatus =
  StartTextTranslationJobResponse'
    { jobId = Core.Nothing,
      jobStatus = Core.Nothing,
      responseStatus
    }

-- | The identifier generated for the job. To get the status of a job, use this ID with the 'DescribeTextTranslationJob' operation.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsJobId :: Lens.Lens' StartTextTranslationJobResponse (Core.Maybe Types.JobId)
srsJobId = Lens.field @"jobId"
{-# DEPRECATED srsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The status of the job. Possible values include:
--
--
--     * @SUBMITTED@ - The job has been received and is queued for processing.
--
--
--     * @IN_PROGRESS@ - Amazon Translate is processing the job.
--
--
--     * @COMPLETED@ - The job was successfully completed and the output is available.
--
--
--     * @COMPLETED_WITH_ERROR@ - The job was completed with errors. The errors can be analyzed in the job's output.
--
--
--     * @FAILED@ - The job did not complete. To get details, use the 'DescribeTextTranslationJob' operation.
--
--
--     * @STOP_REQUESTED@ - The user who started the job has requested that it be stopped.
--
--
--     * @STOPPED@ - The job has been stopped.
--
--
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsJobStatus :: Lens.Lens' StartTextTranslationJobResponse (Core.Maybe Types.JobStatus)
srsJobStatus = Lens.field @"jobStatus"
{-# DEPRECATED srsJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StartTextTranslationJobResponse Core.Int
srsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
