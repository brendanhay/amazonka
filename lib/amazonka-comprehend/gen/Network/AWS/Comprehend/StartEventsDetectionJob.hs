{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.StartEventsDetectionJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an asynchronous event detection job for a collection of documents.
module Network.AWS.Comprehend.StartEventsDetectionJob
  ( -- * Creating a request
    StartEventsDetectionJob (..),
    mkStartEventsDetectionJob,

    -- ** Request lenses
    sJobName,
    sClientRequestToken,
    sInputDataConfig,
    sOutputDataConfig,
    sDataAccessRoleARN,
    sLanguageCode,
    sTargetEventTypes,

    -- * Destructuring the response
    StartEventsDetectionJobResponse (..),
    mkStartEventsDetectionJobResponse,

    -- ** Response lenses
    starsJobId,
    starsJobStatus,
    starsResponseStatus,
  )
where

import Network.AWS.Comprehend.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartEventsDetectionJob' smart constructor.
data StartEventsDetectionJob = StartEventsDetectionJob'
  { jobName ::
      Lude.Maybe Lude.Text,
    clientRequestToken :: Lude.Maybe Lude.Text,
    inputDataConfig :: InputDataConfig,
    outputDataConfig :: OutputDataConfig,
    dataAccessRoleARN :: Lude.Text,
    languageCode :: LanguageCode,
    targetEventTypes :: Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartEventsDetectionJob' with the minimum fields required to make a request.
--
-- * 'clientRequestToken' - An unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
-- * 'dataAccessRoleARN' - The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data.
-- * 'inputDataConfig' - Specifies the format and location of the input data for the job.
-- * 'jobName' - The identifier of the events detection job.
-- * 'languageCode' - The language code of the input documents.
-- * 'outputDataConfig' - Specifies where to send the output files.
-- * 'targetEventTypes' - The types of events to detect in the input documents.
mkStartEventsDetectionJob ::
  -- | 'inputDataConfig'
  InputDataConfig ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  -- | 'dataAccessRoleARN'
  Lude.Text ->
  -- | 'languageCode'
  LanguageCode ->
  -- | 'targetEventTypes'
  Lude.NonEmpty Lude.Text ->
  StartEventsDetectionJob
mkStartEventsDetectionJob
  pInputDataConfig_
  pOutputDataConfig_
  pDataAccessRoleARN_
  pLanguageCode_
  pTargetEventTypes_ =
    StartEventsDetectionJob'
      { jobName = Lude.Nothing,
        clientRequestToken = Lude.Nothing,
        inputDataConfig = pInputDataConfig_,
        outputDataConfig = pOutputDataConfig_,
        dataAccessRoleARN = pDataAccessRoleARN_,
        languageCode = pLanguageCode_,
        targetEventTypes = pTargetEventTypes_
      }

-- | The identifier of the events detection job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sJobName :: Lens.Lens' StartEventsDetectionJob (Lude.Maybe Lude.Text)
sJobName = Lens.lens (jobName :: StartEventsDetectionJob -> Lude.Maybe Lude.Text) (\s a -> s {jobName = a} :: StartEventsDetectionJob)
{-# DEPRECATED sJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | An unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
--
-- /Note:/ Consider using 'clientRequestToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sClientRequestToken :: Lens.Lens' StartEventsDetectionJob (Lude.Maybe Lude.Text)
sClientRequestToken = Lens.lens (clientRequestToken :: StartEventsDetectionJob -> Lude.Maybe Lude.Text) (\s a -> s {clientRequestToken = a} :: StartEventsDetectionJob)
{-# DEPRECATED sClientRequestToken "Use generic-lens or generic-optics with 'clientRequestToken' instead." #-}

-- | Specifies the format and location of the input data for the job.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sInputDataConfig :: Lens.Lens' StartEventsDetectionJob InputDataConfig
sInputDataConfig = Lens.lens (inputDataConfig :: StartEventsDetectionJob -> InputDataConfig) (\s a -> s {inputDataConfig = a} :: StartEventsDetectionJob)
{-# DEPRECATED sInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | Specifies where to send the output files.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sOutputDataConfig :: Lens.Lens' StartEventsDetectionJob OutputDataConfig
sOutputDataConfig = Lens.lens (outputDataConfig :: StartEventsDetectionJob -> OutputDataConfig) (\s a -> s {outputDataConfig = a} :: StartEventsDetectionJob)
{-# DEPRECATED sOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that grants Amazon Comprehend read access to your input data.
--
-- /Note:/ Consider using 'dataAccessRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDataAccessRoleARN :: Lens.Lens' StartEventsDetectionJob Lude.Text
sDataAccessRoleARN = Lens.lens (dataAccessRoleARN :: StartEventsDetectionJob -> Lude.Text) (\s a -> s {dataAccessRoleARN = a} :: StartEventsDetectionJob)
{-# DEPRECATED sDataAccessRoleARN "Use generic-lens or generic-optics with 'dataAccessRoleARN' instead." #-}

-- | The language code of the input documents.
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sLanguageCode :: Lens.Lens' StartEventsDetectionJob LanguageCode
sLanguageCode = Lens.lens (languageCode :: StartEventsDetectionJob -> LanguageCode) (\s a -> s {languageCode = a} :: StartEventsDetectionJob)
{-# DEPRECATED sLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The types of events to detect in the input documents.
--
-- /Note:/ Consider using 'targetEventTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTargetEventTypes :: Lens.Lens' StartEventsDetectionJob (Lude.NonEmpty Lude.Text)
sTargetEventTypes = Lens.lens (targetEventTypes :: StartEventsDetectionJob -> Lude.NonEmpty Lude.Text) (\s a -> s {targetEventTypes = a} :: StartEventsDetectionJob)
{-# DEPRECATED sTargetEventTypes "Use generic-lens or generic-optics with 'targetEventTypes' instead." #-}

instance Lude.AWSRequest StartEventsDetectionJob where
  type Rs StartEventsDetectionJob = StartEventsDetectionJobResponse
  request = Req.postJSON comprehendService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartEventsDetectionJobResponse'
            Lude.<$> (x Lude..?> "JobId")
            Lude.<*> (x Lude..?> "JobStatus")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartEventsDetectionJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Comprehend_20171127.StartEventsDetectionJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartEventsDetectionJob where
  toJSON StartEventsDetectionJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("JobName" Lude..=) Lude.<$> jobName,
            ("ClientRequestToken" Lude..=) Lude.<$> clientRequestToken,
            Lude.Just ("InputDataConfig" Lude..= inputDataConfig),
            Lude.Just ("OutputDataConfig" Lude..= outputDataConfig),
            Lude.Just ("DataAccessRoleArn" Lude..= dataAccessRoleARN),
            Lude.Just ("LanguageCode" Lude..= languageCode),
            Lude.Just ("TargetEventTypes" Lude..= targetEventTypes)
          ]
      )

instance Lude.ToPath StartEventsDetectionJob where
  toPath = Lude.const "/"

instance Lude.ToQuery StartEventsDetectionJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartEventsDetectionJobResponse' smart constructor.
data StartEventsDetectionJobResponse = StartEventsDetectionJobResponse'
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

-- | Creates a value of 'StartEventsDetectionJobResponse' with the minimum fields required to make a request.
--
-- * 'jobId' - An unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
-- * 'jobStatus' - The status of the events detection job.
-- * 'responseStatus' - The response status code.
mkStartEventsDetectionJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartEventsDetectionJobResponse
mkStartEventsDetectionJobResponse pResponseStatus_ =
  StartEventsDetectionJobResponse'
    { jobId = Lude.Nothing,
      jobStatus = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An unique identifier for the request. If you don't set the client request token, Amazon Comprehend generates one.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
starsJobId :: Lens.Lens' StartEventsDetectionJobResponse (Lude.Maybe Lude.Text)
starsJobId = Lens.lens (jobId :: StartEventsDetectionJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: StartEventsDetectionJobResponse)
{-# DEPRECATED starsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | The status of the events detection job.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
starsJobStatus :: Lens.Lens' StartEventsDetectionJobResponse (Lude.Maybe JobStatus)
starsJobStatus = Lens.lens (jobStatus :: StartEventsDetectionJobResponse -> Lude.Maybe JobStatus) (\s a -> s {jobStatus = a} :: StartEventsDetectionJobResponse)
{-# DEPRECATED starsJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
starsResponseStatus :: Lens.Lens' StartEventsDetectionJobResponse Lude.Int
starsResponseStatus = Lens.lens (responseStatus :: StartEventsDetectionJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartEventsDetectionJobResponse)
{-# DEPRECATED starsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
