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
    dcjrrsCompilationJobName,
    dcjrrsCompilationJobArn,
    dcjrrsCompilationJobStatus,
    dcjrrsStoppingCondition,
    dcjrrsCreationTime,
    dcjrrsLastModifiedTime,
    dcjrrsFailureReason,
    dcjrrsModelArtifacts,
    dcjrrsRoleArn,
    dcjrrsInputConfig,
    dcjrrsOutputConfig,
    dcjrrsCompilationEndTime,
    dcjrrsCompilationStartTime,
    dcjrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeCompilationJob' smart constructor.
newtype DescribeCompilationJob = DescribeCompilationJob'
  { -- | The name of the model compilation job that you want information about.
    compilationJobName :: Types.EntityName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCompilationJob' value with any optional fields omitted.
mkDescribeCompilationJob ::
  -- | 'compilationJobName'
  Types.EntityName ->
  DescribeCompilationJob
mkDescribeCompilationJob compilationJobName =
  DescribeCompilationJob' {compilationJobName}

-- | The name of the model compilation job that you want information about.
--
-- /Note:/ Consider using 'compilationJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjCompilationJobName :: Lens.Lens' DescribeCompilationJob Types.EntityName
dcjCompilationJobName = Lens.field @"compilationJobName"
{-# DEPRECATED dcjCompilationJobName "Use generic-lens or generic-optics with 'compilationJobName' instead." #-}

instance Core.FromJSON DescribeCompilationJob where
  toJSON DescribeCompilationJob {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("CompilationJobName" Core..= compilationJobName)]
      )

instance Core.AWSRequest DescribeCompilationJob where
  type Rs DescribeCompilationJob = DescribeCompilationJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.DescribeCompilationJob")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCompilationJobResponse'
            Core.<$> (x Core..: "CompilationJobName")
            Core.<*> (x Core..: "CompilationJobArn")
            Core.<*> (x Core..: "CompilationJobStatus")
            Core.<*> (x Core..: "StoppingCondition")
            Core.<*> (x Core..: "CreationTime")
            Core.<*> (x Core..: "LastModifiedTime")
            Core.<*> (x Core..: "FailureReason")
            Core.<*> (x Core..: "ModelArtifacts")
            Core.<*> (x Core..: "RoleArn")
            Core.<*> (x Core..: "InputConfig")
            Core.<*> (x Core..: "OutputConfig")
            Core.<*> (x Core..:? "CompilationEndTime")
            Core.<*> (x Core..:? "CompilationStartTime")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeCompilationJobResponse' smart constructor.
data DescribeCompilationJobResponse = DescribeCompilationJobResponse'
  { -- | The name of the model compilation job.
    compilationJobName :: Types.EntityName,
    -- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker assumes to perform the model compilation job.
    compilationJobArn :: Types.CompilationJobArn,
    -- | The status of the model compilation job.
    compilationJobStatus :: Types.CompilationJobStatus,
    -- | Specifies a limit to how long a model compilation job can run. When the job reaches the time limit, Amazon SageMaker ends the compilation job. Use this API to cap model training costs.
    stoppingCondition :: Types.StoppingCondition,
    -- | The time that the model compilation job was created.
    creationTime :: Core.NominalDiffTime,
    -- | The time that the status of the model compilation job was last modified.
    lastModifiedTime :: Core.NominalDiffTime,
    -- | If a model compilation job failed, the reason it failed.
    failureReason :: Types.FailureReason,
    -- | Information about the location in Amazon S3 that has been configured for storing the model artifacts used in the compilation job.
    modelArtifacts :: Types.ModelArtifacts,
    -- | The Amazon Resource Name (ARN) of the model compilation job.
    roleArn :: Types.RoleArn,
    -- | Information about the location in Amazon S3 of the input model artifacts, the name and shape of the expected data inputs, and the framework in which the model was trained.
    inputConfig :: Types.InputConfig,
    -- | Information about the output location for the compiled model and the target device that the model runs on.
    outputConfig :: Types.OutputConfig,
    -- | The time when the model compilation job on a compilation job instance ended. For a successful or stopped job, this is when the job's model artifacts have finished uploading. For a failed job, this is when Amazon SageMaker detected that the job failed.
    compilationEndTime :: Core.Maybe Core.NominalDiffTime,
    -- | The time when the model compilation job started the @CompilationJob@ instances.
    --
    -- You are billed for the time between this timestamp and the timestamp in the 'DescribeCompilationJobResponse$CompilationEndTime' field. In Amazon CloudWatch Logs, the start time might be later than this time. That's because it takes time to download the compilation job, which depends on the size of the compilation job container.
    compilationStartTime :: Core.Maybe Core.NominalDiffTime,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeCompilationJobResponse' value with any optional fields omitted.
mkDescribeCompilationJobResponse ::
  -- | 'compilationJobName'
  Types.EntityName ->
  -- | 'compilationJobArn'
  Types.CompilationJobArn ->
  -- | 'compilationJobStatus'
  Types.CompilationJobStatus ->
  -- | 'stoppingCondition'
  Types.StoppingCondition ->
  -- | 'creationTime'
  Core.NominalDiffTime ->
  -- | 'lastModifiedTime'
  Core.NominalDiffTime ->
  -- | 'failureReason'
  Types.FailureReason ->
  -- | 'modelArtifacts'
  Types.ModelArtifacts ->
  -- | 'roleArn'
  Types.RoleArn ->
  -- | 'inputConfig'
  Types.InputConfig ->
  -- | 'outputConfig'
  Types.OutputConfig ->
  -- | 'responseStatus'
  Core.Int ->
  DescribeCompilationJobResponse
mkDescribeCompilationJobResponse
  compilationJobName
  compilationJobArn
  compilationJobStatus
  stoppingCondition
  creationTime
  lastModifiedTime
  failureReason
  modelArtifacts
  roleArn
  inputConfig
  outputConfig
  responseStatus =
    DescribeCompilationJobResponse'
      { compilationJobName,
        compilationJobArn,
        compilationJobStatus,
        stoppingCondition,
        creationTime,
        lastModifiedTime,
        failureReason,
        modelArtifacts,
        roleArn,
        inputConfig,
        outputConfig,
        compilationEndTime = Core.Nothing,
        compilationStartTime = Core.Nothing,
        responseStatus
      }

-- | The name of the model compilation job.
--
-- /Note:/ Consider using 'compilationJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjrrsCompilationJobName :: Lens.Lens' DescribeCompilationJobResponse Types.EntityName
dcjrrsCompilationJobName = Lens.field @"compilationJobName"
{-# DEPRECATED dcjrrsCompilationJobName "Use generic-lens or generic-optics with 'compilationJobName' instead." #-}

-- | The Amazon Resource Name (ARN) of an IAM role that Amazon SageMaker assumes to perform the model compilation job.
--
-- /Note:/ Consider using 'compilationJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjrrsCompilationJobArn :: Lens.Lens' DescribeCompilationJobResponse Types.CompilationJobArn
dcjrrsCompilationJobArn = Lens.field @"compilationJobArn"
{-# DEPRECATED dcjrrsCompilationJobArn "Use generic-lens or generic-optics with 'compilationJobArn' instead." #-}

-- | The status of the model compilation job.
--
-- /Note:/ Consider using 'compilationJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjrrsCompilationJobStatus :: Lens.Lens' DescribeCompilationJobResponse Types.CompilationJobStatus
dcjrrsCompilationJobStatus = Lens.field @"compilationJobStatus"
{-# DEPRECATED dcjrrsCompilationJobStatus "Use generic-lens or generic-optics with 'compilationJobStatus' instead." #-}

-- | Specifies a limit to how long a model compilation job can run. When the job reaches the time limit, Amazon SageMaker ends the compilation job. Use this API to cap model training costs.
--
-- /Note:/ Consider using 'stoppingCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjrrsStoppingCondition :: Lens.Lens' DescribeCompilationJobResponse Types.StoppingCondition
dcjrrsStoppingCondition = Lens.field @"stoppingCondition"
{-# DEPRECATED dcjrrsStoppingCondition "Use generic-lens or generic-optics with 'stoppingCondition' instead." #-}

-- | The time that the model compilation job was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjrrsCreationTime :: Lens.Lens' DescribeCompilationJobResponse Core.NominalDiffTime
dcjrrsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED dcjrrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The time that the status of the model compilation job was last modified.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjrrsLastModifiedTime :: Lens.Lens' DescribeCompilationJobResponse Core.NominalDiffTime
dcjrrsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED dcjrrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | If a model compilation job failed, the reason it failed.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjrrsFailureReason :: Lens.Lens' DescribeCompilationJobResponse Types.FailureReason
dcjrrsFailureReason = Lens.field @"failureReason"
{-# DEPRECATED dcjrrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | Information about the location in Amazon S3 that has been configured for storing the model artifacts used in the compilation job.
--
-- /Note:/ Consider using 'modelArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjrrsModelArtifacts :: Lens.Lens' DescribeCompilationJobResponse Types.ModelArtifacts
dcjrrsModelArtifacts = Lens.field @"modelArtifacts"
{-# DEPRECATED dcjrrsModelArtifacts "Use generic-lens or generic-optics with 'modelArtifacts' instead." #-}

-- | The Amazon Resource Name (ARN) of the model compilation job.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjrrsRoleArn :: Lens.Lens' DescribeCompilationJobResponse Types.RoleArn
dcjrrsRoleArn = Lens.field @"roleArn"
{-# DEPRECATED dcjrrsRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | Information about the location in Amazon S3 of the input model artifacts, the name and shape of the expected data inputs, and the framework in which the model was trained.
--
-- /Note:/ Consider using 'inputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjrrsInputConfig :: Lens.Lens' DescribeCompilationJobResponse Types.InputConfig
dcjrrsInputConfig = Lens.field @"inputConfig"
{-# DEPRECATED dcjrrsInputConfig "Use generic-lens or generic-optics with 'inputConfig' instead." #-}

-- | Information about the output location for the compiled model and the target device that the model runs on.
--
-- /Note:/ Consider using 'outputConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjrrsOutputConfig :: Lens.Lens' DescribeCompilationJobResponse Types.OutputConfig
dcjrrsOutputConfig = Lens.field @"outputConfig"
{-# DEPRECATED dcjrrsOutputConfig "Use generic-lens or generic-optics with 'outputConfig' instead." #-}

-- | The time when the model compilation job on a compilation job instance ended. For a successful or stopped job, this is when the job's model artifacts have finished uploading. For a failed job, this is when Amazon SageMaker detected that the job failed.
--
-- /Note:/ Consider using 'compilationEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjrrsCompilationEndTime :: Lens.Lens' DescribeCompilationJobResponse (Core.Maybe Core.NominalDiffTime)
dcjrrsCompilationEndTime = Lens.field @"compilationEndTime"
{-# DEPRECATED dcjrrsCompilationEndTime "Use generic-lens or generic-optics with 'compilationEndTime' instead." #-}

-- | The time when the model compilation job started the @CompilationJob@ instances.
--
-- You are billed for the time between this timestamp and the timestamp in the 'DescribeCompilationJobResponse$CompilationEndTime' field. In Amazon CloudWatch Logs, the start time might be later than this time. That's because it takes time to download the compilation job, which depends on the size of the compilation job container.
--
-- /Note:/ Consider using 'compilationStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjrrsCompilationStartTime :: Lens.Lens' DescribeCompilationJobResponse (Core.Maybe Core.NominalDiffTime)
dcjrrsCompilationStartTime = Lens.field @"compilationStartTime"
{-# DEPRECATED dcjrrsCompilationStartTime "Use generic-lens or generic-optics with 'compilationStartTime' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcjrrsResponseStatus :: Lens.Lens' DescribeCompilationJobResponse Core.Int
dcjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dcjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
