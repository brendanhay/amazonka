{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeCompilationJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a model compilation job.
--
-- To create a model compilation job, use 'CreateCompilationJob' . To get information about multiple model compilation jobs, use 'ListCompilationJobs' .
module Network.AWS.SageMaker.DescribeCompilationJob
  ( -- * Creating a request
    DescribeCompilationJob (..),
    mkDescribeCompilationJob,

    -- ** Request lenses
    dcjCompilationJobName,

    -- * Destructuring the response
    DescribeCompilationJobResponse (..),
    mkDescribeCompilationJobResponse,

    -- ** Response lenses
    dcjrsCompilationStartTime,
    dcjrsCreationTime,
    dcjrsFailureReason,
    dcjrsModelArtifacts,
    dcjrsStoppingCondition,
    dcjrsLastModifiedTime,
    dcjrsCompilationJobName,
    dcjrsInputConfig,
    dcjrsCompilationJobStatus,
    dcjrsOutputConfig,
    dcjrsCompilationEndTime,
    dcjrsCompilationJobARN,
    dcjrsRoleARN,
    dcjrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDescribeCompilationJob' smart constructor.
newtype DescribeCompilationJob = DescribeCompilationJob'
  { -- | The name of the model compilation job that you want information about.
    compilationJobName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCompilationJob' with the minimum fields required to make a request.
--
-- * 'compilationJobName' - The name of the model compilation job that you want information about.
mkDescribeCompilationJob ::
  -- | 'compilationJobName'
  Lude.Text ->
  DescribeCompilationJob
mkDescribeCompilationJob pCompilationJobName_ =
  DescribeCompilationJob'
    { compilationJobName =
        pCompilationJobName_
    }

-- | The name of the model compilation job that you want information about.
--
-- /Note:/ Consider using 'compilationJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjCompilationJobName :: Lens.Lens' DescribeCompilationJob Lude.Text
dcjCompilationJobName = Lens.lens (compilationJobName :: DescribeCompilationJob -> Lude.Text) (\s a -> s {compilationJobName = a} :: DescribeCompilationJob)
{-# DEPRECATED dcjCompilationJobName "Use generic-lens or generic-optics with 'compilationJobName' instead." #-}

instance Lude.AWSRequest DescribeCompilationJob where
  type Rs DescribeCompilationJob = DescribeCompilationJobResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeCompilationJobResponse'
            Lude.<$> (x Lude..?> "CompilationStartTime")
            Lude.<*> (x Lude..:> "CreationTime")
            Lude.<*> (x Lude..:> "FailureReason")
            Lude.<*> (x Lude..:> "ModelArtifacts")
            Lude.<*> (x Lude..:> "StoppingCondition")
            Lude.<*> (x Lude..:> "LastModifiedTime")
            Lude.<*> (x Lude..:> "CompilationJobName")
            Lude.<*> (x Lude..:> "InputConfig")
            Lude.<*> (x Lude..:> "CompilationJobStatus")
            Lude.<*> (x Lude..:> "OutputConfig")
            Lude.<*> (x Lude..?> "CompilationEndTime")
            Lude.<*> (x Lude..:> "CompilationJobArn")
            Lude.<*> (x Lude..:> "RoleArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeCompilationJob where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DescribeCompilationJob" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeCompilationJob where
  toJSON DescribeCompilationJob' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("CompilationJobName" Lude..= compilationJobName)]
      )

instance Lude.ToPath DescribeCompilationJob where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeCompilationJob where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeCompilationJobResponse' smart constructor.
data DescribeCompilationJobResponse = DescribeCompilationJobResponse'
  { -- | The time when the model compilation job started the @CompilationJob@ instances.
    --
    -- You are billed for the time between this timestamp and the timestamp in the 'DescribeCompilationJobResponse$CompilationEndTime' field. In Amazon CloudWatch Logs, the start time might be later than this time. That's because it takes time to download the compilation job, which depends on the size of the compilation job container.
    compilationStartTime :: Lude.Maybe Lude.Timestamp,
    -- | The time that the model compilation job was created.
    creationTime :: Lude.Timestamp,
    -- | If a model compilation job failed, the reason it failed.
    failureReason :: Lude.Text,
    -- | Information about the location in Amazon S3 that has been configured for storing the model artifacts used in the compilation job.
    modelArtifacts :: ModelArtifacts,
    -- | Specifies a limit to how long a model compilation job can run. When the job reaches the time limit, Amazon SageMaker ends the compilation job. Use this API to cap model training costs.
    stoppingCondition :: StoppingCondition,
    -- | The time that the status of the model compilation job was last modified.
    lastModifiedTime :: Lude.Timestamp,
    -- | The name of the model compilation job.
    compilationJobName :: Lude.Text,
    -- | Information about the location in Amazon S3 of the input model artifacts, the name and shape of the expected data inputs, and the framework in which the model was trained.
    inputConfig :: InputConfig,
    -- | The status of the model compilation job.
    compilationJobStatus :: CompilationJobStatus,
    -- | Information about the output location for the compiled model and the target device that the model runs on.
    outputConfig :: OutputConfig,
    -- | The time when the model compilation job on a compilation job instance ended. For a successful or stopped job, this is when the job's model artifacts have finished uploading. For a failed job, this is when Amazon SageMaker detected that the job failed.
    compilationEndTime :: Lude.Maybe Lude.Timestamp,
    -- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker assumes to perform the model compilation job.
    compilationJobARN :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the model compilation job.
    roleARN :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCompilationJobResponse' with the minimum fields required to make a request.
--
-- * 'compilationStartTime' - The time when the model compilation job started the @CompilationJob@ instances.
--
-- You are billed for the time between this timestamp and the timestamp in the 'DescribeCompilationJobResponse$CompilationEndTime' field. In Amazon CloudWatch Logs, the start time might be later than this time. That's because it takes time to download the compilation job, which depends on the size of the compilation job container.
-- * 'creationTime' - The time that the model compilation job was created.
-- * 'failureReason' - If a model compilation job failed, the reason it failed.
-- * 'modelArtifacts' - Information about the location in Amazon S3 that has been configured for storing the model artifacts used in the compilation job.
-- * 'stoppingCondition' - Specifies a limit to how long a model compilation job can run. When the job reaches the time limit, Amazon SageMaker ends the compilation job. Use this API to cap model training costs.
-- * 'lastModifiedTime' - The time that the status of the model compilation job was last modified.
-- * 'compilationJobName' - The name of the model compilation job.
-- * 'inputConfig' - Information about the location in Amazon S3 of the input model artifacts, the name and shape of the expected data inputs, and the framework in which the model was trained.
-- * 'compilationJobStatus' - The status of the model compilation job.
-- * 'outputConfig' - Information about the output location for the compiled model and the target device that the model runs on.
-- * 'compilationEndTime' - The time when the model compilation job on a compilation job instance ended. For a successful or stopped job, this is when the job's model artifacts have finished uploading. For a failed job, this is when Amazon SageMaker detected that the job failed.
-- * 'compilationJobARN' - The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker assumes to perform the model compilation job.
-- * 'roleARN' - The Amazon Resource Name (ARN) of the model compilation job.
-- * 'responseStatus' - The response status code.
mkDescribeCompilationJobResponse ::
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'failureReason'
  Lude.Text ->
  -- | 'modelArtifacts'
  ModelArtifacts ->
  -- | 'stoppingCondition'
  StoppingCondition ->
  -- | 'lastModifiedTime'
  Lude.Timestamp ->
  -- | 'compilationJobName'
  Lude.Text ->
  -- | 'inputConfig'
  InputConfig ->
  -- | 'compilationJobStatus'
  CompilationJobStatus ->
  -- | 'outputConfig'
  OutputConfig ->
  -- | 'compilationJobARN'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribeCompilationJobResponse
mkDescribeCompilationJobResponse
  pCreationTime_
  pFailureReason_
  pModelArtifacts_
  pStoppingCondition_
  pLastModifiedTime_
  pCompilationJobName_
  pInputConfig_
  pCompilationJobStatus_
  pOutputConfig_
  pCompilationJobARN_
  pRoleARN_
  pResponseStatus_ =
    DescribeCompilationJobResponse'
      { compilationStartTime =
          Lude.Nothing,
        creationTime = pCreationTime_,
        failureReason = pFailureReason_,
        modelArtifacts = pModelArtifacts_,
        stoppingCondition = pStoppingCondition_,
        lastModifiedTime = pLastModifiedTime_,
        compilationJobName = pCompilationJobName_,
        inputConfig = pInputConfig_,
        compilationJobStatus = pCompilationJobStatus_,
        outputConfig = pOutputConfig_,
        compilationEndTime = Lude.Nothing,
        compilationJobARN = pCompilationJobARN_,
        roleARN = pRoleARN_,
        responseStatus = pResponseStatus_
      }

-- | The time when the model compilation job started the @CompilationJob@ instances.
--
-- You are billed for the time between this timestamp and the timestamp in the 'DescribeCompilationJobResponse$CompilationEndTime' field. In Amazon CloudWatch Logs, the start time might be later than this time. That's because it takes time to download the compilation job, which depends on the size of the compilation job container.
--
-- /Note:/ Consider using 'compilationStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjrsCompilationStartTime :: Lens.Lens' DescribeCompilationJobResponse (Lude.Maybe Lude.Timestamp)
dcjrsCompilationStartTime = Lens.lens (compilationStartTime :: DescribeCompilationJobResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {compilationStartTime = a} :: DescribeCompilationJobResponse)
{-# DEPRECATED dcjrsCompilationStartTime "Use generic-lens or generic-optics with 'compilationStartTime' instead." #-}

-- | The time that the model compilation job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjrsCreationTime :: Lens.Lens' DescribeCompilationJobResponse Lude.Timestamp
dcjrsCreationTime = Lens.lens (creationTime :: DescribeCompilationJobResponse -> Lude.Timestamp) (\s a -> s {creationTime = a} :: DescribeCompilationJobResponse)
{-# DEPRECATED dcjrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | If a model compilation job failed, the reason it failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjrsFailureReason :: Lens.Lens' DescribeCompilationJobResponse Lude.Text
dcjrsFailureReason = Lens.lens (failureReason :: DescribeCompilationJobResponse -> Lude.Text) (\s a -> s {failureReason = a} :: DescribeCompilationJobResponse)
{-# DEPRECATED dcjrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | Information about the location in Amazon S3 that has been configured for storing the model artifacts used in the compilation job.
--
-- /Note:/ Consider using 'modelArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjrsModelArtifacts :: Lens.Lens' DescribeCompilationJobResponse ModelArtifacts
dcjrsModelArtifacts = Lens.lens (modelArtifacts :: DescribeCompilationJobResponse -> ModelArtifacts) (\s a -> s {modelArtifacts = a} :: DescribeCompilationJobResponse)
{-# DEPRECATED dcjrsModelArtifacts "Use generic-lens or generic-optics with 'modelArtifacts' instead." #-}

-- | Specifies a limit to how long a model compilation job can run. When the job reaches the time limit, Amazon SageMaker ends the compilation job. Use this API to cap model training costs.
--
-- /Note:/ Consider using 'stoppingCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjrsStoppingCondition :: Lens.Lens' DescribeCompilationJobResponse StoppingCondition
dcjrsStoppingCondition = Lens.lens (stoppingCondition :: DescribeCompilationJobResponse -> StoppingCondition) (\s a -> s {stoppingCondition = a} :: DescribeCompilationJobResponse)
{-# DEPRECATED dcjrsStoppingCondition "Use generic-lens or generic-optics with 'stoppingCondition' instead." #-}

-- | The time that the status of the model compilation job was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjrsLastModifiedTime :: Lens.Lens' DescribeCompilationJobResponse Lude.Timestamp
dcjrsLastModifiedTime = Lens.lens (lastModifiedTime :: DescribeCompilationJobResponse -> Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: DescribeCompilationJobResponse)
{-# DEPRECATED dcjrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The name of the model compilation job.
--
-- /Note:/ Consider using 'compilationJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjrsCompilationJobName :: Lens.Lens' DescribeCompilationJobResponse Lude.Text
dcjrsCompilationJobName = Lens.lens (compilationJobName :: DescribeCompilationJobResponse -> Lude.Text) (\s a -> s {compilationJobName = a} :: DescribeCompilationJobResponse)
{-# DEPRECATED dcjrsCompilationJobName "Use generic-lens or generic-optics with 'compilationJobName' instead." #-}

-- | Information about the location in Amazon S3 of the input model artifacts, the name and shape of the expected data inputs, and the framework in which the model was trained.
--
-- /Note:/ Consider using 'inputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjrsInputConfig :: Lens.Lens' DescribeCompilationJobResponse InputConfig
dcjrsInputConfig = Lens.lens (inputConfig :: DescribeCompilationJobResponse -> InputConfig) (\s a -> s {inputConfig = a} :: DescribeCompilationJobResponse)
{-# DEPRECATED dcjrsInputConfig "Use generic-lens or generic-optics with 'inputConfig' instead." #-}

-- | The status of the model compilation job.
--
-- /Note:/ Consider using 'compilationJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjrsCompilationJobStatus :: Lens.Lens' DescribeCompilationJobResponse CompilationJobStatus
dcjrsCompilationJobStatus = Lens.lens (compilationJobStatus :: DescribeCompilationJobResponse -> CompilationJobStatus) (\s a -> s {compilationJobStatus = a} :: DescribeCompilationJobResponse)
{-# DEPRECATED dcjrsCompilationJobStatus "Use generic-lens or generic-optics with 'compilationJobStatus' instead." #-}

-- | Information about the output location for the compiled model and the target device that the model runs on.
--
-- /Note:/ Consider using 'outputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjrsOutputConfig :: Lens.Lens' DescribeCompilationJobResponse OutputConfig
dcjrsOutputConfig = Lens.lens (outputConfig :: DescribeCompilationJobResponse -> OutputConfig) (\s a -> s {outputConfig = a} :: DescribeCompilationJobResponse)
{-# DEPRECATED dcjrsOutputConfig "Use generic-lens or generic-optics with 'outputConfig' instead." #-}

-- | The time when the model compilation job on a compilation job instance ended. For a successful or stopped job, this is when the job's model artifacts have finished uploading. For a failed job, this is when Amazon SageMaker detected that the job failed.
--
-- /Note:/ Consider using 'compilationEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjrsCompilationEndTime :: Lens.Lens' DescribeCompilationJobResponse (Lude.Maybe Lude.Timestamp)
dcjrsCompilationEndTime = Lens.lens (compilationEndTime :: DescribeCompilationJobResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {compilationEndTime = a} :: DescribeCompilationJobResponse)
{-# DEPRECATED dcjrsCompilationEndTime "Use generic-lens or generic-optics with 'compilationEndTime' instead." #-}

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker assumes to perform the model compilation job.
--
-- /Note:/ Consider using 'compilationJobARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjrsCompilationJobARN :: Lens.Lens' DescribeCompilationJobResponse Lude.Text
dcjrsCompilationJobARN = Lens.lens (compilationJobARN :: DescribeCompilationJobResponse -> Lude.Text) (\s a -> s {compilationJobARN = a} :: DescribeCompilationJobResponse)
{-# DEPRECATED dcjrsCompilationJobARN "Use generic-lens or generic-optics with 'compilationJobARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the model compilation job.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjrsRoleARN :: Lens.Lens' DescribeCompilationJobResponse Lude.Text
dcjrsRoleARN = Lens.lens (roleARN :: DescribeCompilationJobResponse -> Lude.Text) (\s a -> s {roleARN = a} :: DescribeCompilationJobResponse)
{-# DEPRECATED dcjrsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjrsResponseStatus :: Lens.Lens' DescribeCompilationJobResponse Lude.Int
dcjrsResponseStatus = Lens.lens (responseStatus :: DescribeCompilationJobResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeCompilationJobResponse)
{-# DEPRECATED dcjrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
