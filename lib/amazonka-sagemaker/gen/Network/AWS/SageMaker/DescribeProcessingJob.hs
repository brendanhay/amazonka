{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeProcessingJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of a processing job.
module Network.AWS.SageMaker.DescribeProcessingJob
  ( -- * Creating a request
    DescribeProcessingJob (..),
    mkDescribeProcessingJob,

    -- ** Request lenses
    dpjProcessingJobName,

    -- * Destructuring the response
    DescribeProcessingJobResponse (..),
    mkDescribeProcessingJobResponse,

    -- ** Response lenses
    dpjrsFailureReason,
    dpjrsMonitoringScheduleARN,
    dpjrsEnvironment,
    dpjrsStoppingCondition,
    dpjrsExperimentConfig,
    dpjrsLastModifiedTime,
    dpjrsProcessingInputs,
    dpjrsNetworkConfig,
    dpjrsAutoMLJobARN,
    dpjrsTrainingJobARN,
    dpjrsExitMessage,
    dpjrsProcessingOutputConfig,
    dpjrsProcessingStartTime,
    dpjrsProcessingEndTime,
    dpjrsRoleARN,
    dpjrsResponseStatus,
    dpjrsProcessingJobName,
    dpjrsProcessingResources,
    dpjrsAppSpecification,
    dpjrsProcessingJobARN,
    dpjrsProcessingJobStatus,
    dpjrsCreationTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDescribeProcessingJob' smart constructor.
newtype DescribeProcessingJob = DescribeProcessingJob'
  { processingJobName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeProcessingJob' with the minimum fields required to make a request.
--
-- * 'processingJobName' - The name of the processing job. The name must be unique within an AWS Region in the AWS account.
mkDescribeProcessingJob ::
  -- | 'processingJobName'
  Lude.Text ->
  DescribeProcessingJob
mkDescribeProcessingJob pProcessingJobName_ =
  DescribeProcessingJob' {processingJobName = pProcessingJobName_}

-- | The name of the processing job. The name must be unique within an AWS Region in the AWS account.
--
-- /Note:/ Consider using 'processingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjProcessingJobName :: Lens.Lens' DescribeProcessingJob Lude.Text
dpjProcessingJobName = Lens.lens (processingJobName :: DescribeProcessingJob -> Lude.Text) (\s a -> s {processingJobName = a} :: DescribeProcessingJob)
{-# DEPRECATED dpjProcessingJobName "Use generic-lens or generic-optics with 'processingJobName' instead." #-}

instance Lude.AWSRequest DescribeProcessingJob where
  type Rs DescribeProcessingJob = DescribeProcessingJobResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeProcessingJobResponse'
            Lude.<$> (x Lude..?> "FailureReason")
            Lude.<*> (x Lude..?> "MonitoringScheduleArn")
            Lude.<*> (x Lude..?> "Environment" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "StoppingCondition")
            Lude.<*> (x Lude..?> "ExperimentConfig")
            Lude.<*> (x Lude..?> "LastModifiedTime")
            Lude.<*> (x Lude..?> "ProcessingInputs" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NetworkConfig")
            Lude.<*> (x Lude..?> "AutoMLJobArn")
            Lude.<*> (x Lude..?> "TrainingJobArn")
            Lude.<*> (x Lude..?> "ExitMessage")
            Lude.<*> (x Lude..?> "ProcessingOutputConfig")
            Lude.<*> (x Lude..?> "ProcessingStartTime")
            Lude.<*> (x Lude..?> "ProcessingEndTime")
            Lude.<*> (x Lude..?> "RoleArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "ProcessingJobName")
            Lude.<*> (x Lude..:> "ProcessingResources")
            Lude.<*> (x Lude..:> "AppSpecification")
            Lude.<*> (x Lude..:> "ProcessingJobArn")
            Lude.<*> (x Lude..:> "ProcessingJobStatus")
            Lude.<*> (x Lude..:> "CreationTime")
      )

instance Lude.ToHeaders DescribeProcessingJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DescribeProcessingJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeProcessingJob where
  toJSON DescribeProcessingJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ProcessingJobName" Lude..= processingJobName)]
      )

instance Lude.ToPath DescribeProcessingJob where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeProcessingJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeProcessingJobResponse' smart constructor.
data DescribeProcessingJobResponse = DescribeProcessingJobResponse'
  { failureReason ::
      Lude.Maybe Lude.Text,
    monitoringScheduleARN ::
      Lude.Maybe Lude.Text,
    environment ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Lude.Text)
        ),
    stoppingCondition ::
      Lude.Maybe
        ProcessingStoppingCondition,
    experimentConfig ::
      Lude.Maybe ExperimentConfig,
    lastModifiedTime ::
      Lude.Maybe Lude.Timestamp,
    processingInputs ::
      Lude.Maybe [ProcessingInput],
    networkConfig ::
      Lude.Maybe NetworkConfig,
    autoMLJobARN ::
      Lude.Maybe Lude.Text,
    trainingJobARN ::
      Lude.Maybe Lude.Text,
    exitMessage ::
      Lude.Maybe Lude.Text,
    processingOutputConfig ::
      Lude.Maybe
        ProcessingOutputConfig,
    processingStartTime ::
      Lude.Maybe Lude.Timestamp,
    processingEndTime ::
      Lude.Maybe Lude.Timestamp,
    roleARN :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    processingJobName :: Lude.Text,
    processingResources ::
      ProcessingResources,
    appSpecification ::
      AppSpecification,
    processingJobARN :: Lude.Text,
    processingJobStatus ::
      ProcessingJobStatus,
    creationTime :: Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeProcessingJobResponse' with the minimum fields required to make a request.
--
-- * 'appSpecification' - Configures the processing job to run a specified container image.
-- * 'autoMLJobARN' - The ARN of an AutoML job associated with this processing job.
-- * 'creationTime' - The time at which the processing job was created.
-- * 'environment' - The environment variables set in the Docker container.
-- * 'exitMessage' - An optional string, up to one KB in size, that contains metadata from the processing container when the processing job exits.
-- * 'experimentConfig' - The configuration information used to create an experiment.
-- * 'failureReason' - A string, up to one KB in size, that contains the reason a processing job failed, if it failed.
-- * 'lastModifiedTime' - The time at which the processing job was last modified.
-- * 'monitoringScheduleARN' - The ARN of a monitoring schedule for an endpoint associated with this processing job.
-- * 'networkConfig' - Networking options for a processing job.
-- * 'processingEndTime' - The time at which the processing job completed.
-- * 'processingInputs' - The inputs for a processing job.
-- * 'processingJobARN' - The Amazon Resource Name (ARN) of the processing job.
-- * 'processingJobName' - The name of the processing job. The name must be unique within an AWS Region in the AWS account.
-- * 'processingJobStatus' - Provides the status of a processing job.
-- * 'processingOutputConfig' - Output configuration for the processing job.
-- * 'processingResources' - Identifies the resources, ML compute instances, and ML storage volumes to deploy for a processing job. In distributed training, you specify more than one instance.
-- * 'processingStartTime' - The time at which the processing job started.
-- * 'responseStatus' - The response status code.
-- * 'roleARN' - The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.
-- * 'stoppingCondition' - The time limit for how long the processing job is allowed to run.
-- * 'trainingJobARN' - The ARN of a training job associated with this processing job.
mkDescribeProcessingJobResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'processingJobName'
  Lude.Text ->
  -- | 'processingResources'
  ProcessingResources ->
  -- | 'appSpecification'
  AppSpecification ->
  -- | 'processingJobARN'
  Lude.Text ->
  -- | 'processingJobStatus'
  ProcessingJobStatus ->
  -- | 'creationTime'
  Lude.Timestamp ->
  DescribeProcessingJobResponse
mkDescribeProcessingJobResponse
  pResponseStatus_
  pProcessingJobName_
  pProcessingResources_
  pAppSpecification_
  pProcessingJobARN_
  pProcessingJobStatus_
  pCreationTime_ =
    DescribeProcessingJobResponse'
      { failureReason = Lude.Nothing,
        monitoringScheduleARN = Lude.Nothing,
        environment = Lude.Nothing,
        stoppingCondition = Lude.Nothing,
        experimentConfig = Lude.Nothing,
        lastModifiedTime = Lude.Nothing,
        processingInputs = Lude.Nothing,
        networkConfig = Lude.Nothing,
        autoMLJobARN = Lude.Nothing,
        trainingJobARN = Lude.Nothing,
        exitMessage = Lude.Nothing,
        processingOutputConfig = Lude.Nothing,
        processingStartTime = Lude.Nothing,
        processingEndTime = Lude.Nothing,
        roleARN = Lude.Nothing,
        responseStatus = pResponseStatus_,
        processingJobName = pProcessingJobName_,
        processingResources = pProcessingResources_,
        appSpecification = pAppSpecification_,
        processingJobARN = pProcessingJobARN_,
        processingJobStatus = pProcessingJobStatus_,
        creationTime = pCreationTime_
      }

-- | A string, up to one KB in size, that contains the reason a processing job failed, if it failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrsFailureReason :: Lens.Lens' DescribeProcessingJobResponse (Lude.Maybe Lude.Text)
dpjrsFailureReason = Lens.lens (failureReason :: DescribeProcessingJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: DescribeProcessingJobResponse)
{-# DEPRECATED dpjrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The ARN of a monitoring schedule for an endpoint associated with this processing job.
--
-- /Note:/ Consider using 'monitoringScheduleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrsMonitoringScheduleARN :: Lens.Lens' DescribeProcessingJobResponse (Lude.Maybe Lude.Text)
dpjrsMonitoringScheduleARN = Lens.lens (monitoringScheduleARN :: DescribeProcessingJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {monitoringScheduleARN = a} :: DescribeProcessingJobResponse)
{-# DEPRECATED dpjrsMonitoringScheduleARN "Use generic-lens or generic-optics with 'monitoringScheduleARN' instead." #-}

-- | The environment variables set in the Docker container.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrsEnvironment :: Lens.Lens' DescribeProcessingJobResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
dpjrsEnvironment = Lens.lens (environment :: DescribeProcessingJobResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {environment = a} :: DescribeProcessingJobResponse)
{-# DEPRECATED dpjrsEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | The time limit for how long the processing job is allowed to run.
--
-- /Note:/ Consider using 'stoppingCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrsStoppingCondition :: Lens.Lens' DescribeProcessingJobResponse (Lude.Maybe ProcessingStoppingCondition)
dpjrsStoppingCondition = Lens.lens (stoppingCondition :: DescribeProcessingJobResponse -> Lude.Maybe ProcessingStoppingCondition) (\s a -> s {stoppingCondition = a} :: DescribeProcessingJobResponse)
{-# DEPRECATED dpjrsStoppingCondition "Use generic-lens or generic-optics with 'stoppingCondition' instead." #-}

-- | The configuration information used to create an experiment.
--
-- /Note:/ Consider using 'experimentConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrsExperimentConfig :: Lens.Lens' DescribeProcessingJobResponse (Lude.Maybe ExperimentConfig)
dpjrsExperimentConfig = Lens.lens (experimentConfig :: DescribeProcessingJobResponse -> Lude.Maybe ExperimentConfig) (\s a -> s {experimentConfig = a} :: DescribeProcessingJobResponse)
{-# DEPRECATED dpjrsExperimentConfig "Use generic-lens or generic-optics with 'experimentConfig' instead." #-}

-- | The time at which the processing job was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrsLastModifiedTime :: Lens.Lens' DescribeProcessingJobResponse (Lude.Maybe Lude.Timestamp)
dpjrsLastModifiedTime = Lens.lens (lastModifiedTime :: DescribeProcessingJobResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: DescribeProcessingJobResponse)
{-# DEPRECATED dpjrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The inputs for a processing job.
--
-- /Note:/ Consider using 'processingInputs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrsProcessingInputs :: Lens.Lens' DescribeProcessingJobResponse (Lude.Maybe [ProcessingInput])
dpjrsProcessingInputs = Lens.lens (processingInputs :: DescribeProcessingJobResponse -> Lude.Maybe [ProcessingInput]) (\s a -> s {processingInputs = a} :: DescribeProcessingJobResponse)
{-# DEPRECATED dpjrsProcessingInputs "Use generic-lens or generic-optics with 'processingInputs' instead." #-}

-- | Networking options for a processing job.
--
-- /Note:/ Consider using 'networkConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrsNetworkConfig :: Lens.Lens' DescribeProcessingJobResponse (Lude.Maybe NetworkConfig)
dpjrsNetworkConfig = Lens.lens (networkConfig :: DescribeProcessingJobResponse -> Lude.Maybe NetworkConfig) (\s a -> s {networkConfig = a} :: DescribeProcessingJobResponse)
{-# DEPRECATED dpjrsNetworkConfig "Use generic-lens or generic-optics with 'networkConfig' instead." #-}

-- | The ARN of an AutoML job associated with this processing job.
--
-- /Note:/ Consider using 'autoMLJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrsAutoMLJobARN :: Lens.Lens' DescribeProcessingJobResponse (Lude.Maybe Lude.Text)
dpjrsAutoMLJobARN = Lens.lens (autoMLJobARN :: DescribeProcessingJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {autoMLJobARN = a} :: DescribeProcessingJobResponse)
{-# DEPRECATED dpjrsAutoMLJobARN "Use generic-lens or generic-optics with 'autoMLJobARN' instead." #-}

-- | The ARN of a training job associated with this processing job.
--
-- /Note:/ Consider using 'trainingJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrsTrainingJobARN :: Lens.Lens' DescribeProcessingJobResponse (Lude.Maybe Lude.Text)
dpjrsTrainingJobARN = Lens.lens (trainingJobARN :: DescribeProcessingJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {trainingJobARN = a} :: DescribeProcessingJobResponse)
{-# DEPRECATED dpjrsTrainingJobARN "Use generic-lens or generic-optics with 'trainingJobARN' instead." #-}

-- | An optional string, up to one KB in size, that contains metadata from the processing container when the processing job exits.
--
-- /Note:/ Consider using 'exitMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrsExitMessage :: Lens.Lens' DescribeProcessingJobResponse (Lude.Maybe Lude.Text)
dpjrsExitMessage = Lens.lens (exitMessage :: DescribeProcessingJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {exitMessage = a} :: DescribeProcessingJobResponse)
{-# DEPRECATED dpjrsExitMessage "Use generic-lens or generic-optics with 'exitMessage' instead." #-}

-- | Output configuration for the processing job.
--
-- /Note:/ Consider using 'processingOutputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrsProcessingOutputConfig :: Lens.Lens' DescribeProcessingJobResponse (Lude.Maybe ProcessingOutputConfig)
dpjrsProcessingOutputConfig = Lens.lens (processingOutputConfig :: DescribeProcessingJobResponse -> Lude.Maybe ProcessingOutputConfig) (\s a -> s {processingOutputConfig = a} :: DescribeProcessingJobResponse)
{-# DEPRECATED dpjrsProcessingOutputConfig "Use generic-lens or generic-optics with 'processingOutputConfig' instead." #-}

-- | The time at which the processing job started.
--
-- /Note:/ Consider using 'processingStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrsProcessingStartTime :: Lens.Lens' DescribeProcessingJobResponse (Lude.Maybe Lude.Timestamp)
dpjrsProcessingStartTime = Lens.lens (processingStartTime :: DescribeProcessingJobResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {processingStartTime = a} :: DescribeProcessingJobResponse)
{-# DEPRECATED dpjrsProcessingStartTime "Use generic-lens or generic-optics with 'processingStartTime' instead." #-}

-- | The time at which the processing job completed.
--
-- /Note:/ Consider using 'processingEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrsProcessingEndTime :: Lens.Lens' DescribeProcessingJobResponse (Lude.Maybe Lude.Timestamp)
dpjrsProcessingEndTime = Lens.lens (processingEndTime :: DescribeProcessingJobResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {processingEndTime = a} :: DescribeProcessingJobResponse)
{-# DEPRECATED dpjrsProcessingEndTime "Use generic-lens or generic-optics with 'processingEndTime' instead." #-}

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker can assume to perform tasks on your behalf.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrsRoleARN :: Lens.Lens' DescribeProcessingJobResponse (Lude.Maybe Lude.Text)
dpjrsRoleARN = Lens.lens (roleARN :: DescribeProcessingJobResponse -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: DescribeProcessingJobResponse)
{-# DEPRECATED dpjrsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrsResponseStatus :: Lens.Lens' DescribeProcessingJobResponse Lude.Int
dpjrsResponseStatus = Lens.lens (responseStatus :: DescribeProcessingJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeProcessingJobResponse)
{-# DEPRECATED dpjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The name of the processing job. The name must be unique within an AWS Region in the AWS account.
--
-- /Note:/ Consider using 'processingJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrsProcessingJobName :: Lens.Lens' DescribeProcessingJobResponse Lude.Text
dpjrsProcessingJobName = Lens.lens (processingJobName :: DescribeProcessingJobResponse -> Lude.Text) (\s a -> s {processingJobName = a} :: DescribeProcessingJobResponse)
{-# DEPRECATED dpjrsProcessingJobName "Use generic-lens or generic-optics with 'processingJobName' instead." #-}

-- | Identifies the resources, ML compute instances, and ML storage volumes to deploy for a processing job. In distributed training, you specify more than one instance.
--
-- /Note:/ Consider using 'processingResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrsProcessingResources :: Lens.Lens' DescribeProcessingJobResponse ProcessingResources
dpjrsProcessingResources = Lens.lens (processingResources :: DescribeProcessingJobResponse -> ProcessingResources) (\s a -> s {processingResources = a} :: DescribeProcessingJobResponse)
{-# DEPRECATED dpjrsProcessingResources "Use generic-lens or generic-optics with 'processingResources' instead." #-}

-- | Configures the processing job to run a specified container image.
--
-- /Note:/ Consider using 'appSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrsAppSpecification :: Lens.Lens' DescribeProcessingJobResponse AppSpecification
dpjrsAppSpecification = Lens.lens (appSpecification :: DescribeProcessingJobResponse -> AppSpecification) (\s a -> s {appSpecification = a} :: DescribeProcessingJobResponse)
{-# DEPRECATED dpjrsAppSpecification "Use generic-lens or generic-optics with 'appSpecification' instead." #-}

-- | The Amazon Resource Name (ARN) of the processing job.
--
-- /Note:/ Consider using 'processingJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrsProcessingJobARN :: Lens.Lens' DescribeProcessingJobResponse Lude.Text
dpjrsProcessingJobARN = Lens.lens (processingJobARN :: DescribeProcessingJobResponse -> Lude.Text) (\s a -> s {processingJobARN = a} :: DescribeProcessingJobResponse)
{-# DEPRECATED dpjrsProcessingJobARN "Use generic-lens or generic-optics with 'processingJobARN' instead." #-}

-- | Provides the status of a processing job.
--
-- /Note:/ Consider using 'processingJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrsProcessingJobStatus :: Lens.Lens' DescribeProcessingJobResponse ProcessingJobStatus
dpjrsProcessingJobStatus = Lens.lens (processingJobStatus :: DescribeProcessingJobResponse -> ProcessingJobStatus) (\s a -> s {processingJobStatus = a} :: DescribeProcessingJobResponse)
{-# DEPRECATED dpjrsProcessingJobStatus "Use generic-lens or generic-optics with 'processingJobStatus' instead." #-}

-- | The time at which the processing job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpjrsCreationTime :: Lens.Lens' DescribeProcessingJobResponse Lude.Timestamp
dpjrsCreationTime = Lens.lens (creationTime :: DescribeProcessingJobResponse -> Lude.Timestamp) (\s a -> s {creationTime = a} :: DescribeProcessingJobResponse)
{-# DEPRECATED dpjrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}
