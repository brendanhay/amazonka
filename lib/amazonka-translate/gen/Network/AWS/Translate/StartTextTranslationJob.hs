{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    sttjJobName,
    sttjParallelDataNames,
    sttjTerminologyNames,
    sttjInputDataConfig,
    sttjOutputDataConfig,
    sttjDataAccessRoleARN,
    sttjSourceLanguageCode,
    sttjTargetLanguageCodes,
    sttjClientToken,

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
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Translate.Types

-- | /See:/ 'mkStartTextTranslationJob' smart constructor.
data StartTextTranslationJob = StartTextTranslationJob'
  { jobName ::
      Lude.Maybe Lude.Text,
    parallelDataNames :: Lude.Maybe [Lude.Text],
    terminologyNames :: Lude.Maybe [Lude.Text],
    inputDataConfig :: InputDataConfig,
    outputDataConfig :: OutputDataConfig,
    dataAccessRoleARN :: Lude.Text,
    sourceLanguageCode :: Lude.Text,
    targetLanguageCodes ::
      Lude.NonEmpty Lude.Text,
    clientToken :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartTextTranslationJob' with the minimum fields required to make a request.
--
-- * 'clientToken' - A unique identifier for the request. This token is auto-generated when using the Amazon Translate SDK.
-- * 'dataAccessRoleARN' - The Amazon Resource Name (ARN) of an AWS Identity Access and Management (IAM) role that grants Amazon Translate read access to your input data. For more nformation, see 'identity-and-access-management' .
-- * 'inputDataConfig' - Specifies the format and S3 location of the input documents for the translation job.
-- * 'jobName' - The name of the batch translation job to be performed.
-- * 'outputDataConfig' - Specifies the S3 folder to which your job output will be saved.
-- * 'parallelDataNames' - The names of the parallel data resources to use in the batch translation job. For a list of available parallel data resources, use the 'ListParallelData' operation.
-- * 'sourceLanguageCode' - The language code of the input language. For a list of language codes, see 'what-is-languages' .
--
-- Amazon Translate does not automatically detect a source language during batch translation jobs.
-- * 'targetLanguageCodes' - The language code of the output language.
-- * 'terminologyNames' - The name of the terminology to use in the batch translation job. For a list of available terminologies, use the 'ListTerminologies' operation.
mkStartTextTranslationJob ::
  -- | 'inputDataConfig'
  InputDataConfig ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  -- | 'dataAccessRoleARN'
  Lude.Text ->
  -- | 'sourceLanguageCode'
  Lude.Text ->
  -- | 'targetLanguageCodes'
  Lude.NonEmpty Lude.Text ->
  -- | 'clientToken'
  Lude.Text ->
  StartTextTranslationJob
mkStartTextTranslationJob
  pInputDataConfig_
  pOutputDataConfig_
  pDataAccessRoleARN_
  pSourceLanguageCode_
  pTargetLanguageCodes_
  pClientToken_ =
    StartTextTranslationJob'
      { jobName = Lude.Nothing,
        parallelDataNames = Lude.Nothing,
        terminologyNames = Lude.Nothing,
        inputDataConfig = pInputDataConfig_,
        outputDataConfig = pOutputDataConfig_,
        dataAccessRoleARN = pDataAccessRoleARN_,
        sourceLanguageCode = pSourceLanguageCode_,
        targetLanguageCodes = pTargetLanguageCodes_,
        clientToken = pClientToken_
      }

-- | The name of the batch translation job to be performed.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sttjJobName :: Lens.Lens' StartTextTranslationJob (Lude.Maybe Lude.Text)
sttjJobName = Lens.lens (jobName :: StartTextTranslationJob -> Lude.Maybe Lude.Text) (\s a -> s {jobName = a} :: StartTextTranslationJob)
{-# DEPRECATED sttjJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | The names of the parallel data resources to use in the batch translation job. For a list of available parallel data resources, use the 'ListParallelData' operation.
--
-- /Note:/ Consider using 'parallelDataNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sttjParallelDataNames :: Lens.Lens' StartTextTranslationJob (Lude.Maybe [Lude.Text])
sttjParallelDataNames = Lens.lens (parallelDataNames :: StartTextTranslationJob -> Lude.Maybe [Lude.Text]) (\s a -> s {parallelDataNames = a} :: StartTextTranslationJob)
{-# DEPRECATED sttjParallelDataNames "Use generic-lens or generic-optics with 'parallelDataNames' instead." #-}

-- | The name of the terminology to use in the batch translation job. For a list of available terminologies, use the 'ListTerminologies' operation.
--
-- /Note:/ Consider using 'terminologyNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sttjTerminologyNames :: Lens.Lens' StartTextTranslationJob (Lude.Maybe [Lude.Text])
sttjTerminologyNames = Lens.lens (terminologyNames :: StartTextTranslationJob -> Lude.Maybe [Lude.Text]) (\s a -> s {terminologyNames = a} :: StartTextTranslationJob)
{-# DEPRECATED sttjTerminologyNames "Use generic-lens or generic-optics with 'terminologyNames' instead." #-}

-- | Specifies the format and S3 location of the input documents for the translation job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sttjInputDataConfig :: Lens.Lens' StartTextTranslationJob InputDataConfig
sttjInputDataConfig = Lens.lens (inputDataConfig :: StartTextTranslationJob -> InputDataConfig) (\s a -> s {inputDataConfig = a} :: StartTextTranslationJob)
{-# DEPRECATED sttjInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | Specifies the S3 folder to which your job output will be saved.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sttjOutputDataConfig :: Lens.Lens' StartTextTranslationJob OutputDataConfig
sttjOutputDataConfig = Lens.lens (outputDataConfig :: StartTextTranslationJob -> OutputDataConfig) (\s a -> s {outputDataConfig = a} :: StartTextTranslationJob)
{-# DEPRECATED sttjOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | The Amazon Resource Name (ARN) of an AWS Identity Access and Management (IAM) role that grants Amazon Translate read access to your input data. For more nformation, see 'identity-and-access-management' .
--
-- /Note:/ Consider using 'dataAccessRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sttjDataAccessRoleARN :: Lens.Lens' StartTextTranslationJob Lude.Text
sttjDataAccessRoleARN = Lens.lens (dataAccessRoleARN :: StartTextTranslationJob -> Lude.Text) (\s a -> s {dataAccessRoleARN = a} :: StartTextTranslationJob)
{-# DEPRECATED sttjDataAccessRoleARN "Use generic-lens or generic-optics with 'dataAccessRoleARN' instead." #-}

-- | The language code of the input language. For a list of language codes, see 'what-is-languages' .
--
-- Amazon Translate does not automatically detect a source language during batch translation jobs.
--
-- /Note:/ Consider using 'sourceLanguageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sttjSourceLanguageCode :: Lens.Lens' StartTextTranslationJob Lude.Text
sttjSourceLanguageCode = Lens.lens (sourceLanguageCode :: StartTextTranslationJob -> Lude.Text) (\s a -> s {sourceLanguageCode = a} :: StartTextTranslationJob)
{-# DEPRECATED sttjSourceLanguageCode "Use generic-lens or generic-optics with 'sourceLanguageCode' instead." #-}

-- | The language code of the output language.
--
-- /Note:/ Consider using 'targetLanguageCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sttjTargetLanguageCodes :: Lens.Lens' StartTextTranslationJob (Lude.NonEmpty Lude.Text)
sttjTargetLanguageCodes = Lens.lens (targetLanguageCodes :: StartTextTranslationJob -> Lude.NonEmpty Lude.Text) (\s a -> s {targetLanguageCodes = a} :: StartTextTranslationJob)
{-# DEPRECATED sttjTargetLanguageCodes "Use generic-lens or generic-optics with 'targetLanguageCodes' instead." #-}

-- | A unique identifier for the request. This token is auto-generated when using the Amazon Translate SDK.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sttjClientToken :: Lens.Lens' StartTextTranslationJob Lude.Text
sttjClientToken = Lens.lens (clientToken :: StartTextTranslationJob -> Lude.Text) (\s a -> s {clientToken = a} :: StartTextTranslationJob)
{-# DEPRECATED sttjClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

instance Lude.AWSRequest StartTextTranslationJob where
  type Rs StartTextTranslationJob = StartTextTranslationJobResponse
  request = Req.postJSON translateService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartTextTranslationJobResponse'
            Lude.<$> (x Lude..?> "JobId")
            Lude.<*> (x Lude..?> "JobStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartTextTranslationJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSShineFrontendService_20170701.StartTextTranslationJob" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartTextTranslationJob where
  toJSON StartTextTranslationJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("JobName" Lude..=) Lude.<$> jobName,
            ("ParallelDataNames" Lude..=) Lude.<$> parallelDataNames,
            ("TerminologyNames" Lude..=) Lude.<$> terminologyNames,
            Lude.Just ("InputDataConfig" Lude..= inputDataConfig),
            Lude.Just ("OutputDataConfig" Lude..= outputDataConfig),
            Lude.Just ("DataAccessRoleArn" Lude..= dataAccessRoleARN),
            Lude.Just ("SourceLanguageCode" Lude..= sourceLanguageCode),
            Lude.Just ("TargetLanguageCodes" Lude..= targetLanguageCodes),
            Lude.Just ("ClientToken" Lude..= clientToken)
          ]
      )

instance Lude.ToPath StartTextTranslationJob where
  toPath = Lude.const "/"

instance Lude.ToQuery StartTextTranslationJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartTextTranslationJobResponse' smart constructor.
data StartTextTranslationJobResponse = StartTextTranslationJobResponse'
  { jobId ::
      Lude.Maybe Lude.Text,
    jobStatus ::
      Lude.Maybe JobStatus,
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

-- | Creates a value of 'StartTextTranslationJobResponse' with the minimum fields required to make a request.
--
-- * 'jobId' - The identifier generated for the job. To get the status of a job, use this ID with the 'DescribeTextTranslationJob' operation.
-- * 'jobStatus' - The status of the job. Possible values include:
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
-- * 'responseStatus' - The response status code.
mkStartTextTranslationJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartTextTranslationJobResponse
mkStartTextTranslationJobResponse pResponseStatus_ =
  StartTextTranslationJobResponse'
    { jobId = Lude.Nothing,
      jobStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier generated for the job. To get the status of a job, use this ID with the 'DescribeTextTranslationJob' operation.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsJobId :: Lens.Lens' StartTextTranslationJobResponse (Lude.Maybe Lude.Text)
srsJobId = Lens.lens (jobId :: StartTextTranslationJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: StartTextTranslationJobResponse)
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
srsJobStatus :: Lens.Lens' StartTextTranslationJobResponse (Lude.Maybe JobStatus)
srsJobStatus = Lens.lens (jobStatus :: StartTextTranslationJobResponse -> Lude.Maybe JobStatus) (\s a -> s {jobStatus = a} :: StartTextTranslationJobResponse)
{-# DEPRECATED srsJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StartTextTranslationJobResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: StartTextTranslationJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartTextTranslationJobResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
