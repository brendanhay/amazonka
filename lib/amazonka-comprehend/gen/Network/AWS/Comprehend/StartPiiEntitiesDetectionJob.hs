{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.StartPiiEntitiesDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous PII entity detection job for a collection of documents.
module Network.AWS.Comprehend.StartPiiEntitiesDetectionJob
  ( -- * Creating a request
    StartPiiEntitiesDetectionJob (..),
    mkStartPiiEntitiesDetectionJob,

    -- ** Request lenses
    spedjJobName,
    spedjRedactionConfig,
    spedjClientRequestToken,
    spedjInputDataConfig,
    spedjOutputDataConfig,
    spedjMode,
    spedjDataAccessRoleARN,
    spedjLanguageCode,

    -- * Destructuring the response
    StartPiiEntitiesDetectionJobResponse (..),
    mkStartPiiEntitiesDetectionJobResponse,

    -- ** Response lenses
    spedjrsJobId,
    spedjrsJobStatus,
    spedjrsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartPiiEntitiesDetectionJob' smart constructor.
data StartPiiEntitiesDetectionJob = StartPiiEntitiesDetectionJob'
  { jobName ::
      Lude.Maybe Lude.Text,
    redactionConfig ::
      Lude.Maybe RedactionConfig,
    clientRequestToken ::
      Lude.Maybe Lude.Text,
    inputDataConfig ::
      InputDataConfig,
    outputDataConfig ::
      OutputDataConfig,
    mode :: PiiEntitiesDetectionMode,
    dataAccessRoleARN :: Lude.Text,
    languageCode :: LanguageCode
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartPiiEntitiesDetectionJob' with the minimum fields required to make a request.
--
-- * 'clientRequestToken' - A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
-- * 'dataAccessRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data.
-- * 'inputDataConfig' - The input properties for a PII entities detection job.
-- * 'jobName' - The identifier of the job.
-- * 'languageCode' - The language of the input documents.
-- * 'mode' - Specifies whether the output provides the locations (offsets) of PII entities or a file in which PII entities are redacted.
-- * 'outputDataConfig' - Provides conﬁguration parameters for the output of PII entity detection jobs.
-- * 'redactionConfig' - Provides configuration parameters for PII entity redaction.
--
-- This parameter is required if you set the @Mode@ parameter to @ONLY_REDACTION@ . In that case, you must provide a @RedactionConfig@ definition that includes the @PiiEntityTypes@ parameter.
mkStartPiiEntitiesDetectionJob ::
  -- | 'inputDataConfig'
  InputDataConfig ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  -- | 'mode'
  PiiEntitiesDetectionMode ->
  -- | 'dataAccessRoleARN'
  Lude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  StartPiiEntitiesDetectionJob
mkStartPiiEntitiesDetectionJob
  pInputDataConfig_
  pOutputDataConfig_
  pMode_
  pDataAccessRoleARN_
  pLanguageCode_ =
    StartPiiEntitiesDetectionJob'
      { jobName = Lude.Nothing,
        redactionConfig = Lude.Nothing,
        clientRequestToken = Lude.Nothing,
        inputDataConfig = pInputDataConfig_,
        outputDataConfig = pOutputDataConfig_,
        mode = pMode_,
        dataAccessRoleARN = pDataAccessRoleARN_,
        languageCode = pLanguageCode_
      }

-- | The identifier of the job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjJobName :: Lens.Lens' StartPiiEntitiesDetectionJob (Lude.Maybe Lude.Text)
spedjJobName = Lens.lens (jobName :: StartPiiEntitiesDetectionJob -> Lude.Maybe Lude.Text) (\s a -> s {jobName = a} :: StartPiiEntitiesDetectionJob)
{-# DEPRECATED spedjJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | Provides configuration parameters for PII entity redaction.
--
-- This parameter is required if you set the @Mode@ parameter to @ONLY_REDACTION@ . In that case, you must provide a @RedactionConfig@ definition that includes the @PiiEntityTypes@ parameter.
--
-- /Note:/ Consider using 'redactionConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjRedactionConfig :: Lens.Lens' StartPiiEntitiesDetectionJob (Lude.Maybe RedactionConfig)
spedjRedactionConfig = Lens.lens (redactionConfig :: StartPiiEntitiesDetectionJob -> Lude.Maybe RedactionConfig) (\s a -> s {redactionConfig = a} :: StartPiiEntitiesDetectionJob)
{-# DEPRECATED spedjRedactionConfig "Use generic-lens or generic-optics with 'redactionConfig' instead." #-}

-- | A unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjClientRequestToken :: Lens.Lens' StartPiiEntitiesDetectionJob (Lude.Maybe Lude.Text)
spedjClientRequestToken = Lens.lens (clientRequestToken :: StartPiiEntitiesDetectionJob -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: StartPiiEntitiesDetectionJob)
{-# DEPRECATED spedjClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | The input properties for a PII entities detection job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjInputDataConfig :: Lens.Lens' StartPiiEntitiesDetectionJob InputDataConfig
spedjInputDataConfig = Lens.lens (inputDataConfig :: StartPiiEntitiesDetectionJob -> InputDataConfig) (\s a -> s {inputDataConfig = a} :: StartPiiEntitiesDetectionJob)
{-# DEPRECATED spedjInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | Provides conﬁguration parameters for the output of PII entity detection jobs.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjOutputDataConfig :: Lens.Lens' StartPiiEntitiesDetectionJob OutputDataConfig
spedjOutputDataConfig = Lens.lens (outputDataConfig :: StartPiiEntitiesDetectionJob -> OutputDataConfig) (\s a -> s {outputDataConfig = a} :: StartPiiEntitiesDetectionJob)
{-# DEPRECATED spedjOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | Specifies whether the output provides the locations (offsets) of PII entities or a file in which PII entities are redacted.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjMode :: Lens.Lens' StartPiiEntitiesDetectionJob PiiEntitiesDetectionMode
spedjMode = Lens.lens (mode :: StartPiiEntitiesDetectionJob -> PiiEntitiesDetectionMode) (\s a -> s {mode = a} :: StartPiiEntitiesDetectionJob)
{-# DEPRECATED spedjMode "Use generic-lens or generic-optics with 'mode' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- /Note:/ Consider using 'dataAccessRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjDataAccessRoleARN :: Lens.Lens' StartPiiEntitiesDetectionJob Lude.Text
spedjDataAccessRoleARN = Lens.lens (dataAccessRoleARN :: StartPiiEntitiesDetectionJob -> Lude.Text) (\s a -> s {dataAccessRoleARN = a} :: StartPiiEntitiesDetectionJob)
{-# DEPRECATED spedjDataAccessRoleARN "Use generic-lens or generic-optics with 'dataAccessRoleARN' instead." #-}

-- | The language of the input documents.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjLanguageCode :: Lens.Lens' StartPiiEntitiesDetectionJob LanguageCode
spedjLanguageCode = Lens.lens (languageCode :: StartPiiEntitiesDetectionJob -> LanguageCode) (\s a -> s {languageCode = a} :: StartPiiEntitiesDetectionJob)
{-# DEPRECATED spedjLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

instance Lude.AWSRequest StartPiiEntitiesDetectionJob where
  type
    Rs StartPiiEntitiesDetectionJob =
      StartPiiEntitiesDetectionJobResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartPiiEntitiesDetectionJobResponse'
            Lude.<$> (x Lude..?> "JobId")
            Lude.<*> (x Lude..?> "JobStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartPiiEntitiesDetectionJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Comprehend_20171127.StartPiiEntitiesDetectionJob" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartPiiEntitiesDetectionJob where
  toJSON StartPiiEntitiesDetectionJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("JobName" Lude..=) Lude.<$> jobName,
            ("RedactionConfig" Lude..=) Lude.<$> redactionConfig,
            ("ClientRequestToken" Lude..=) Lude.<$> clientRequestToken,
            Lude.Just ("InputDataConfig" Lude..= inputDataConfig),
            Lude.Just ("OutputDataConfig" Lude..= outputDataConfig),
            Lude.Just ("Mode" Lude..= mode),
            Lude.Just ("DataAccessRoleArn" Lude..= dataAccessRoleARN),
            Lude.Just ("LanguageCode" Lude..= languageCode)
          ]
      )

instance Lude.ToPath StartPiiEntitiesDetectionJob where
  toPath = Lude.const "/"

instance Lude.ToQuery StartPiiEntitiesDetectionJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartPiiEntitiesDetectionJobResponse' smart constructor.
data StartPiiEntitiesDetectionJobResponse = StartPiiEntitiesDetectionJobResponse'
  { jobId ::
      Lude.Maybe
        Lude.Text,
    jobStatus ::
      Lude.Maybe
        JobStatus,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartPiiEntitiesDetectionJobResponse' with the minimum fields required to make a request.
--
-- * 'jobId' - The identifier generated for the job.
-- * 'jobStatus' - The status of the job.
-- * 'responseStatus' - The response status code.
mkStartPiiEntitiesDetectionJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartPiiEntitiesDetectionJobResponse
mkStartPiiEntitiesDetectionJobResponse pResponseStatus_ =
  StartPiiEntitiesDetectionJobResponse'
    { jobId = Lude.Nothing,
      jobStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier generated for the job.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjrsJobId :: Lens.Lens' StartPiiEntitiesDetectionJobResponse (Lude.Maybe Lude.Text)
spedjrsJobId = Lens.lens (jobId :: StartPiiEntitiesDetectionJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: StartPiiEntitiesDetectionJobResponse)
{-# DEPRECATED spedjrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The status of the job.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjrsJobStatus :: Lens.Lens' StartPiiEntitiesDetectionJobResponse (Lude.Maybe JobStatus)
spedjrsJobStatus = Lens.lens (jobStatus :: StartPiiEntitiesDetectionJobResponse -> Lude.Maybe JobStatus) (\s a -> s {jobStatus = a} :: StartPiiEntitiesDetectionJobResponse)
{-# DEPRECATED spedjrsJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spedjrsResponseStatus :: Lens.Lens' StartPiiEntitiesDetectionJobResponse Lude.Int
spedjrsResponseStatus = Lens.lens (responseStatus :: StartPiiEntitiesDetectionJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartPiiEntitiesDetectionJobResponse)
{-# DEPRECATED spedjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
