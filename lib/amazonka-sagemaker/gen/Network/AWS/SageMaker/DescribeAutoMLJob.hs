{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeAutoMLJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about an Amazon SageMaker job.
module Network.AWS.SageMaker.DescribeAutoMLJob
  ( -- * Creating a request
    DescribeAutoMLJob (..),
    mkDescribeAutoMLJob,

    -- ** Request lenses
    damljAutoMLJobName,

    -- * Destructuring the response
    DescribeAutoMLJobResponse (..),
    mkDescribeAutoMLJobResponse,

    -- ** Response lenses
    damljrrsAutoMLJobName,
    damljrrsAutoMLJobArn,
    damljrrsInputDataConfig,
    damljrrsOutputDataConfig,
    damljrrsRoleArn,
    damljrrsCreationTime,
    damljrrsLastModifiedTime,
    damljrrsAutoMLJobStatus,
    damljrrsAutoMLJobSecondaryStatus,
    damljrrsAutoMLJobArtifacts,
    damljrrsAutoMLJobConfig,
    damljrrsAutoMLJobObjective,
    damljrrsBestCandidate,
    damljrrsEndTime,
    damljrrsFailureReason,
    damljrrsGenerateCandidateDefinitionsOnly,
    damljrrsProblemType,
    damljrrsResolvedAttributes,
    damljrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeAutoMLJob' smart constructor.
newtype DescribeAutoMLJob = DescribeAutoMLJob'
  { -- | Request information about a job using that job's unique name.
    autoMLJobName :: Types.AutoMLJobName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAutoMLJob' value with any optional fields omitted.
mkDescribeAutoMLJob ::
  -- | 'autoMLJobName'
  Types.AutoMLJobName ->
  DescribeAutoMLJob
mkDescribeAutoMLJob autoMLJobName =
  DescribeAutoMLJob' {autoMLJobName}

-- | Request information about a job using that job's unique name.
--
-- /Note:/ Consider using 'autoMLJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljAutoMLJobName :: Lens.Lens' DescribeAutoMLJob Types.AutoMLJobName
damljAutoMLJobName = Lens.field @"autoMLJobName"
{-# DEPRECATED damljAutoMLJobName "Use generic-lens or generic-optics with 'autoMLJobName' instead." #-}

instance Core.FromJSON DescribeAutoMLJob where
  toJSON DescribeAutoMLJob {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("AutoMLJobName" Core..= autoMLJobName)]
      )

instance Core.AWSRequest DescribeAutoMLJob where
  type Rs DescribeAutoMLJob = DescribeAutoMLJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.DescribeAutoMLJob")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAutoMLJobResponse'
            Core.<$> (x Core..: "AutoMLJobName")
            Core.<*> (x Core..: "AutoMLJobArn")
            Core.<*> (x Core..: "InputDataConfig")
            Core.<*> (x Core..: "OutputDataConfig")
            Core.<*> (x Core..: "RoleArn")
            Core.<*> (x Core..: "CreationTime")
            Core.<*> (x Core..: "LastModifiedTime")
            Core.<*> (x Core..: "AutoMLJobStatus")
            Core.<*> (x Core..: "AutoMLJobSecondaryStatus")
            Core.<*> (x Core..:? "AutoMLJobArtifacts")
            Core.<*> (x Core..:? "AutoMLJobConfig")
            Core.<*> (x Core..:? "AutoMLJobObjective")
            Core.<*> (x Core..:? "BestCandidate")
            Core.<*> (x Core..:? "EndTime")
            Core.<*> (x Core..:? "FailureReason")
            Core.<*> (x Core..:? "GenerateCandidateDefinitionsOnly")
            Core.<*> (x Core..:? "ProblemType")
            Core.<*> (x Core..:? "ResolvedAttributes")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeAutoMLJobResponse' smart constructor.
data DescribeAutoMLJobResponse = DescribeAutoMLJobResponse'
  { -- | Returns the name of a job.
    autoMLJobName :: Types.AutoMLJobName,
    -- | Returns the job's ARN.
    autoMLJobArn :: Types.AutoMLJobArn,
    -- | Returns the job's input data config.
    inputDataConfig :: Core.NonEmpty Types.AutoMLChannel,
    -- | Returns the job's output data config.
    outputDataConfig :: Types.AutoMLOutputDataConfig,
    -- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that has read permission to the input data location and write permission to the output data location in Amazon S3.
    roleArn :: Types.RoleArn,
    -- | Returns the job's creation time.
    creationTime :: Core.NominalDiffTime,
    -- | Returns the job's last modified time.
    lastModifiedTime :: Core.NominalDiffTime,
    -- | Returns the job's AutoMLJobStatus.
    autoMLJobStatus :: Types.AutoMLJobStatus,
    -- | Returns the job's AutoMLJobSecondaryStatus.
    autoMLJobSecondaryStatus :: Types.AutoMLJobSecondaryStatus,
    -- | Returns information on the job's artifacts found in AutoMLJobArtifacts.
    autoMLJobArtifacts :: Core.Maybe Types.AutoMLJobArtifacts,
    -- | Returns the job's config.
    autoMLJobConfig :: Core.Maybe Types.AutoMLJobConfig,
    -- | Returns the job's objective.
    autoMLJobObjective :: Core.Maybe Types.AutoMLJobObjective,
    -- | Returns the job's BestCandidate.
    bestCandidate :: Core.Maybe Types.AutoMLCandidate,
    -- | Returns the job's end time.
    endTime :: Core.Maybe Core.NominalDiffTime,
    -- | Returns the job's FailureReason.
    failureReason :: Core.Maybe Types.FailureReason,
    -- | Returns the job's output from GenerateCandidateDefinitionsOnly.
    generateCandidateDefinitionsOnly :: Core.Maybe Core.Bool,
    -- | Returns the job's problem type.
    problemType :: Core.Maybe Types.ProblemType,
    -- | This contains ProblemType, AutoMLJobObjective and CompletionCriteria. They're auto-inferred values, if not provided by you. If you do provide them, then they'll be the same as provided.
    resolvedAttributes :: Core.Maybe Types.ResolvedAttributes,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeAutoMLJobResponse' value with any optional fields omitted.
mkDescribeAutoMLJobResponse ::
  -- | 'autoMLJobName'
  Types.AutoMLJobName ->
  -- | 'autoMLJobArn'
  Types.AutoMLJobArn ->
  -- | 'inputDataConfig'
  Core.NonEmpty Types.AutoMLChannel ->
  -- | 'outputDataConfig'
  Types.AutoMLOutputDataConfig ->
  -- | 'roleArn'
  Types.RoleArn ->
  -- | 'creationTime'
  Core.NominalDiffTime ->
  -- | 'lastModifiedTime'
  Core.NominalDiffTime ->
  -- | 'autoMLJobStatus'
  Types.AutoMLJobStatus ->
  -- | 'autoMLJobSecondaryStatus'
  Types.AutoMLJobSecondaryStatus ->
  -- | 'responseStatus'
  Core.Int ->
  DescribeAutoMLJobResponse
mkDescribeAutoMLJobResponse
  autoMLJobName
  autoMLJobArn
  inputDataConfig
  outputDataConfig
  roleArn
  creationTime
  lastModifiedTime
  autoMLJobStatus
  autoMLJobSecondaryStatus
  responseStatus =
    DescribeAutoMLJobResponse'
      { autoMLJobName,
        autoMLJobArn,
        inputDataConfig,
        outputDataConfig,
        roleArn,
        creationTime,
        lastModifiedTime,
        autoMLJobStatus,
        autoMLJobSecondaryStatus,
        autoMLJobArtifacts = Core.Nothing,
        autoMLJobConfig = Core.Nothing,
        autoMLJobObjective = Core.Nothing,
        bestCandidate = Core.Nothing,
        endTime = Core.Nothing,
        failureReason = Core.Nothing,
        generateCandidateDefinitionsOnly = Core.Nothing,
        problemType = Core.Nothing,
        resolvedAttributes = Core.Nothing,
        responseStatus
      }

-- | Returns the name of a job.
--
-- /Note:/ Consider using 'autoMLJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsAutoMLJobName :: Lens.Lens' DescribeAutoMLJobResponse Types.AutoMLJobName
damljrrsAutoMLJobName = Lens.field @"autoMLJobName"
{-# DEPRECATED damljrrsAutoMLJobName "Use generic-lens or generic-optics with 'autoMLJobName' instead." #-}

-- | Returns the job's ARN.
--
-- /Note:/ Consider using 'autoMLJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsAutoMLJobArn :: Lens.Lens' DescribeAutoMLJobResponse Types.AutoMLJobArn
damljrrsAutoMLJobArn = Lens.field @"autoMLJobArn"
{-# DEPRECATED damljrrsAutoMLJobArn "Use generic-lens or generic-optics with 'autoMLJobArn' instead." #-}

-- | Returns the job's input data config.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsInputDataConfig :: Lens.Lens' DescribeAutoMLJobResponse (Core.NonEmpty Types.AutoMLChannel)
damljrrsInputDataConfig = Lens.field @"inputDataConfig"
{-# DEPRECATED damljrrsInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | Returns the job's output data config.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsOutputDataConfig :: Lens.Lens' DescribeAutoMLJobResponse Types.AutoMLOutputDataConfig
damljrrsOutputDataConfig = Lens.field @"outputDataConfig"
{-# DEPRECATED damljrrsOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that has read permission to the input data location and write permission to the output data location in Amazon S3.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsRoleArn :: Lens.Lens' DescribeAutoMLJobResponse Types.RoleArn
damljrrsRoleArn = Lens.field @"roleArn"
{-# DEPRECATED damljrrsRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | Returns the job's creation time.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsCreationTime :: Lens.Lens' DescribeAutoMLJobResponse Core.NominalDiffTime
damljrrsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED damljrrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Returns the job's last modified time.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsLastModifiedTime :: Lens.Lens' DescribeAutoMLJobResponse Core.NominalDiffTime
damljrrsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# DEPRECATED damljrrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | Returns the job's AutoMLJobStatus.
--
-- /Note:/ Consider using 'autoMLJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsAutoMLJobStatus :: Lens.Lens' DescribeAutoMLJobResponse Types.AutoMLJobStatus
damljrrsAutoMLJobStatus = Lens.field @"autoMLJobStatus"
{-# DEPRECATED damljrrsAutoMLJobStatus "Use generic-lens or generic-optics with 'autoMLJobStatus' instead." #-}

-- | Returns the job's AutoMLJobSecondaryStatus.
--
-- /Note:/ Consider using 'autoMLJobSecondaryStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsAutoMLJobSecondaryStatus :: Lens.Lens' DescribeAutoMLJobResponse Types.AutoMLJobSecondaryStatus
damljrrsAutoMLJobSecondaryStatus = Lens.field @"autoMLJobSecondaryStatus"
{-# DEPRECATED damljrrsAutoMLJobSecondaryStatus "Use generic-lens or generic-optics with 'autoMLJobSecondaryStatus' instead." #-}

-- | Returns information on the job's artifacts found in AutoMLJobArtifacts.
--
-- /Note:/ Consider using 'autoMLJobArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsAutoMLJobArtifacts :: Lens.Lens' DescribeAutoMLJobResponse (Core.Maybe Types.AutoMLJobArtifacts)
damljrrsAutoMLJobArtifacts = Lens.field @"autoMLJobArtifacts"
{-# DEPRECATED damljrrsAutoMLJobArtifacts "Use generic-lens or generic-optics with 'autoMLJobArtifacts' instead." #-}

-- | Returns the job's config.
--
-- /Note:/ Consider using 'autoMLJobConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsAutoMLJobConfig :: Lens.Lens' DescribeAutoMLJobResponse (Core.Maybe Types.AutoMLJobConfig)
damljrrsAutoMLJobConfig = Lens.field @"autoMLJobConfig"
{-# DEPRECATED damljrrsAutoMLJobConfig "Use generic-lens or generic-optics with 'autoMLJobConfig' instead." #-}

-- | Returns the job's objective.
--
-- /Note:/ Consider using 'autoMLJobObjective' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsAutoMLJobObjective :: Lens.Lens' DescribeAutoMLJobResponse (Core.Maybe Types.AutoMLJobObjective)
damljrrsAutoMLJobObjective = Lens.field @"autoMLJobObjective"
{-# DEPRECATED damljrrsAutoMLJobObjective "Use generic-lens or generic-optics with 'autoMLJobObjective' instead." #-}

-- | Returns the job's BestCandidate.
--
-- /Note:/ Consider using 'bestCandidate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsBestCandidate :: Lens.Lens' DescribeAutoMLJobResponse (Core.Maybe Types.AutoMLCandidate)
damljrrsBestCandidate = Lens.field @"bestCandidate"
{-# DEPRECATED damljrrsBestCandidate "Use generic-lens or generic-optics with 'bestCandidate' instead." #-}

-- | Returns the job's end time.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsEndTime :: Lens.Lens' DescribeAutoMLJobResponse (Core.Maybe Core.NominalDiffTime)
damljrrsEndTime = Lens.field @"endTime"
{-# DEPRECATED damljrrsEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | Returns the job's FailureReason.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsFailureReason :: Lens.Lens' DescribeAutoMLJobResponse (Core.Maybe Types.FailureReason)
damljrrsFailureReason = Lens.field @"failureReason"
{-# DEPRECATED damljrrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | Returns the job's output from GenerateCandidateDefinitionsOnly.
--
-- /Note:/ Consider using 'generateCandidateDefinitionsOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsGenerateCandidateDefinitionsOnly :: Lens.Lens' DescribeAutoMLJobResponse (Core.Maybe Core.Bool)
damljrrsGenerateCandidateDefinitionsOnly = Lens.field @"generateCandidateDefinitionsOnly"
{-# DEPRECATED damljrrsGenerateCandidateDefinitionsOnly "Use generic-lens or generic-optics with 'generateCandidateDefinitionsOnly' instead." #-}

-- | Returns the job's problem type.
--
-- /Note:/ Consider using 'problemType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsProblemType :: Lens.Lens' DescribeAutoMLJobResponse (Core.Maybe Types.ProblemType)
damljrrsProblemType = Lens.field @"problemType"
{-# DEPRECATED damljrrsProblemType "Use generic-lens or generic-optics with 'problemType' instead." #-}

-- | This contains ProblemType, AutoMLJobObjective and CompletionCriteria. They're auto-inferred values, if not provided by you. If you do provide them, then they'll be the same as provided.
--
-- /Note:/ Consider using 'resolvedAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsResolvedAttributes :: Lens.Lens' DescribeAutoMLJobResponse (Core.Maybe Types.ResolvedAttributes)
damljrrsResolvedAttributes = Lens.field @"resolvedAttributes"
{-# DEPRECATED damljrrsResolvedAttributes "Use generic-lens or generic-optics with 'resolvedAttributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsResponseStatus :: Lens.Lens' DescribeAutoMLJobResponse Core.Int
damljrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED damljrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
