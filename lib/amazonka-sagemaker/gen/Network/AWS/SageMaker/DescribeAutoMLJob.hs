{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeAutoMLJob (..)
    , mkDescribeAutoMLJob
    -- ** Request lenses
    , damljAutoMLJobName

    -- * Destructuring the response
    , DescribeAutoMLJobResponse (..)
    , mkDescribeAutoMLJobResponse
    -- ** Response lenses
    , damljrrsAutoMLJobName
    , damljrrsAutoMLJobArn
    , damljrrsInputDataConfig
    , damljrrsOutputDataConfig
    , damljrrsRoleArn
    , damljrrsCreationTime
    , damljrrsLastModifiedTime
    , damljrrsAutoMLJobStatus
    , damljrrsAutoMLJobSecondaryStatus
    , damljrrsAutoMLJobArtifacts
    , damljrrsAutoMLJobConfig
    , damljrrsAutoMLJobObjective
    , damljrrsBestCandidate
    , damljrrsEndTime
    , damljrrsFailureReason
    , damljrrsGenerateCandidateDefinitionsOnly
    , damljrrsProblemType
    , damljrrsResolvedAttributes
    , damljrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeAutoMLJob' smart constructor.
newtype DescribeAutoMLJob = DescribeAutoMLJob'
  { autoMLJobName :: Types.AutoMLJobName
    -- ^ Request information about a job using that job's unique name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAutoMLJob' value with any optional fields omitted.
mkDescribeAutoMLJob
    :: Types.AutoMLJobName -- ^ 'autoMLJobName'
    -> DescribeAutoMLJob
mkDescribeAutoMLJob autoMLJobName
  = DescribeAutoMLJob'{autoMLJobName}

-- | Request information about a job using that job's unique name.
--
-- /Note:/ Consider using 'autoMLJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljAutoMLJobName :: Lens.Lens' DescribeAutoMLJob Types.AutoMLJobName
damljAutoMLJobName = Lens.field @"autoMLJobName"
{-# INLINEABLE damljAutoMLJobName #-}
{-# DEPRECATED autoMLJobName "Use generic-lens or generic-optics with 'autoMLJobName' instead"  #-}

instance Core.ToQuery DescribeAutoMLJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeAutoMLJob where
        toHeaders DescribeAutoMLJob{..}
          = Core.pure ("X-Amz-Target", "SageMaker.DescribeAutoMLJob") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeAutoMLJob where
        toJSON DescribeAutoMLJob{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AutoMLJobName" Core..= autoMLJobName)])

instance Core.AWSRequest DescribeAutoMLJob where
        type Rs DescribeAutoMLJob = DescribeAutoMLJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeAutoMLJobResponse' Core.<$>
                   (x Core..: "AutoMLJobName") Core.<*> x Core..: "AutoMLJobArn"
                     Core.<*> x Core..: "InputDataConfig"
                     Core.<*> x Core..: "OutputDataConfig"
                     Core.<*> x Core..: "RoleArn"
                     Core.<*> x Core..: "CreationTime"
                     Core.<*> x Core..: "LastModifiedTime"
                     Core.<*> x Core..: "AutoMLJobStatus"
                     Core.<*> x Core..: "AutoMLJobSecondaryStatus"
                     Core.<*> x Core..:? "AutoMLJobArtifacts"
                     Core.<*> x Core..:? "AutoMLJobConfig"
                     Core.<*> x Core..:? "AutoMLJobObjective"
                     Core.<*> x Core..:? "BestCandidate"
                     Core.<*> x Core..:? "EndTime"
                     Core.<*> x Core..:? "FailureReason"
                     Core.<*> x Core..:? "GenerateCandidateDefinitionsOnly"
                     Core.<*> x Core..:? "ProblemType"
                     Core.<*> x Core..:? "ResolvedAttributes"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeAutoMLJobResponse' smart constructor.
data DescribeAutoMLJobResponse = DescribeAutoMLJobResponse'
  { autoMLJobName :: Types.AutoMLJobName
    -- ^ Returns the name of a job.
  , autoMLJobArn :: Types.AutoMLJobArn
    -- ^ Returns the job's ARN.
  , inputDataConfig :: Core.NonEmpty Types.AutoMLChannel
    -- ^ Returns the job's input data config.
  , outputDataConfig :: Types.AutoMLOutputDataConfig
    -- ^ Returns the job's output data config.
  , roleArn :: Types.RoleArn
    -- ^ The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that has read permission to the input data location and write permission to the output data location in Amazon S3.
  , creationTime :: Core.NominalDiffTime
    -- ^ Returns the job's creation time.
  , lastModifiedTime :: Core.NominalDiffTime
    -- ^ Returns the job's last modified time.
  , autoMLJobStatus :: Types.AutoMLJobStatus
    -- ^ Returns the job's AutoMLJobStatus.
  , autoMLJobSecondaryStatus :: Types.AutoMLJobSecondaryStatus
    -- ^ Returns the job's AutoMLJobSecondaryStatus.
  , autoMLJobArtifacts :: Core.Maybe Types.AutoMLJobArtifacts
    -- ^ Returns information on the job's artifacts found in AutoMLJobArtifacts.
  , autoMLJobConfig :: Core.Maybe Types.AutoMLJobConfig
    -- ^ Returns the job's config.
  , autoMLJobObjective :: Core.Maybe Types.AutoMLJobObjective
    -- ^ Returns the job's objective.
  , bestCandidate :: Core.Maybe Types.AutoMLCandidate
    -- ^ Returns the job's BestCandidate.
  , endTime :: Core.Maybe Core.NominalDiffTime
    -- ^ Returns the job's end time.
  , failureReason :: Core.Maybe Types.FailureReason
    -- ^ Returns the job's FailureReason.
  , generateCandidateDefinitionsOnly :: Core.Maybe Core.Bool
    -- ^ Returns the job's output from GenerateCandidateDefinitionsOnly.
  , problemType :: Core.Maybe Types.ProblemType
    -- ^ Returns the job's problem type.
  , resolvedAttributes :: Core.Maybe Types.ResolvedAttributes
    -- ^ This contains ProblemType, AutoMLJobObjective and CompletionCriteria. They're auto-inferred values, if not provided by you. If you do provide them, then they'll be the same as provided.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeAutoMLJobResponse' value with any optional fields omitted.
mkDescribeAutoMLJobResponse
    :: Types.AutoMLJobName -- ^ 'autoMLJobName'
    -> Types.AutoMLJobArn -- ^ 'autoMLJobArn'
    -> Core.NonEmpty Types.AutoMLChannel -- ^ 'inputDataConfig'
    -> Types.AutoMLOutputDataConfig -- ^ 'outputDataConfig'
    -> Types.RoleArn -- ^ 'roleArn'
    -> Core.NominalDiffTime -- ^ 'creationTime'
    -> Core.NominalDiffTime -- ^ 'lastModifiedTime'
    -> Types.AutoMLJobStatus -- ^ 'autoMLJobStatus'
    -> Types.AutoMLJobSecondaryStatus -- ^ 'autoMLJobSecondaryStatus'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribeAutoMLJobResponse
mkDescribeAutoMLJobResponse autoMLJobName autoMLJobArn
  inputDataConfig outputDataConfig roleArn creationTime
  lastModifiedTime autoMLJobStatus autoMLJobSecondaryStatus
  responseStatus
  = DescribeAutoMLJobResponse'{autoMLJobName, autoMLJobArn,
                               inputDataConfig, outputDataConfig, roleArn, creationTime,
                               lastModifiedTime, autoMLJobStatus, autoMLJobSecondaryStatus,
                               autoMLJobArtifacts = Core.Nothing, autoMLJobConfig = Core.Nothing,
                               autoMLJobObjective = Core.Nothing, bestCandidate = Core.Nothing,
                               endTime = Core.Nothing, failureReason = Core.Nothing,
                               generateCandidateDefinitionsOnly = Core.Nothing,
                               problemType = Core.Nothing, resolvedAttributes = Core.Nothing,
                               responseStatus}

-- | Returns the name of a job.
--
-- /Note:/ Consider using 'autoMLJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsAutoMLJobName :: Lens.Lens' DescribeAutoMLJobResponse Types.AutoMLJobName
damljrrsAutoMLJobName = Lens.field @"autoMLJobName"
{-# INLINEABLE damljrrsAutoMLJobName #-}
{-# DEPRECATED autoMLJobName "Use generic-lens or generic-optics with 'autoMLJobName' instead"  #-}

-- | Returns the job's ARN.
--
-- /Note:/ Consider using 'autoMLJobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsAutoMLJobArn :: Lens.Lens' DescribeAutoMLJobResponse Types.AutoMLJobArn
damljrrsAutoMLJobArn = Lens.field @"autoMLJobArn"
{-# INLINEABLE damljrrsAutoMLJobArn #-}
{-# DEPRECATED autoMLJobArn "Use generic-lens or generic-optics with 'autoMLJobArn' instead"  #-}

-- | Returns the job's input data config.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsInputDataConfig :: Lens.Lens' DescribeAutoMLJobResponse (Core.NonEmpty Types.AutoMLChannel)
damljrrsInputDataConfig = Lens.field @"inputDataConfig"
{-# INLINEABLE damljrrsInputDataConfig #-}
{-# DEPRECATED inputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead"  #-}

-- | Returns the job's output data config.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsOutputDataConfig :: Lens.Lens' DescribeAutoMLJobResponse Types.AutoMLOutputDataConfig
damljrrsOutputDataConfig = Lens.field @"outputDataConfig"
{-# INLINEABLE damljrrsOutputDataConfig #-}
{-# DEPRECATED outputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead"  #-}

-- | The Amazon Resource Name (ARN) of the AWS Identity and Access Management (IAM) role that has read permission to the input data location and write permission to the output data location in Amazon S3.
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsRoleArn :: Lens.Lens' DescribeAutoMLJobResponse Types.RoleArn
damljrrsRoleArn = Lens.field @"roleArn"
{-# INLINEABLE damljrrsRoleArn #-}
{-# DEPRECATED roleArn "Use generic-lens or generic-optics with 'roleArn' instead"  #-}

-- | Returns the job's creation time.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsCreationTime :: Lens.Lens' DescribeAutoMLJobResponse Core.NominalDiffTime
damljrrsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE damljrrsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | Returns the job's last modified time.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsLastModifiedTime :: Lens.Lens' DescribeAutoMLJobResponse Core.NominalDiffTime
damljrrsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# INLINEABLE damljrrsLastModifiedTime #-}
{-# DEPRECATED lastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead"  #-}

-- | Returns the job's AutoMLJobStatus.
--
-- /Note:/ Consider using 'autoMLJobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsAutoMLJobStatus :: Lens.Lens' DescribeAutoMLJobResponse Types.AutoMLJobStatus
damljrrsAutoMLJobStatus = Lens.field @"autoMLJobStatus"
{-# INLINEABLE damljrrsAutoMLJobStatus #-}
{-# DEPRECATED autoMLJobStatus "Use generic-lens or generic-optics with 'autoMLJobStatus' instead"  #-}

-- | Returns the job's AutoMLJobSecondaryStatus.
--
-- /Note:/ Consider using 'autoMLJobSecondaryStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsAutoMLJobSecondaryStatus :: Lens.Lens' DescribeAutoMLJobResponse Types.AutoMLJobSecondaryStatus
damljrrsAutoMLJobSecondaryStatus = Lens.field @"autoMLJobSecondaryStatus"
{-# INLINEABLE damljrrsAutoMLJobSecondaryStatus #-}
{-# DEPRECATED autoMLJobSecondaryStatus "Use generic-lens or generic-optics with 'autoMLJobSecondaryStatus' instead"  #-}

-- | Returns information on the job's artifacts found in AutoMLJobArtifacts.
--
-- /Note:/ Consider using 'autoMLJobArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsAutoMLJobArtifacts :: Lens.Lens' DescribeAutoMLJobResponse (Core.Maybe Types.AutoMLJobArtifacts)
damljrrsAutoMLJobArtifacts = Lens.field @"autoMLJobArtifacts"
{-# INLINEABLE damljrrsAutoMLJobArtifacts #-}
{-# DEPRECATED autoMLJobArtifacts "Use generic-lens or generic-optics with 'autoMLJobArtifacts' instead"  #-}

-- | Returns the job's config.
--
-- /Note:/ Consider using 'autoMLJobConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsAutoMLJobConfig :: Lens.Lens' DescribeAutoMLJobResponse (Core.Maybe Types.AutoMLJobConfig)
damljrrsAutoMLJobConfig = Lens.field @"autoMLJobConfig"
{-# INLINEABLE damljrrsAutoMLJobConfig #-}
{-# DEPRECATED autoMLJobConfig "Use generic-lens or generic-optics with 'autoMLJobConfig' instead"  #-}

-- | Returns the job's objective.
--
-- /Note:/ Consider using 'autoMLJobObjective' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsAutoMLJobObjective :: Lens.Lens' DescribeAutoMLJobResponse (Core.Maybe Types.AutoMLJobObjective)
damljrrsAutoMLJobObjective = Lens.field @"autoMLJobObjective"
{-# INLINEABLE damljrrsAutoMLJobObjective #-}
{-# DEPRECATED autoMLJobObjective "Use generic-lens or generic-optics with 'autoMLJobObjective' instead"  #-}

-- | Returns the job's BestCandidate.
--
-- /Note:/ Consider using 'bestCandidate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsBestCandidate :: Lens.Lens' DescribeAutoMLJobResponse (Core.Maybe Types.AutoMLCandidate)
damljrrsBestCandidate = Lens.field @"bestCandidate"
{-# INLINEABLE damljrrsBestCandidate #-}
{-# DEPRECATED bestCandidate "Use generic-lens or generic-optics with 'bestCandidate' instead"  #-}

-- | Returns the job's end time.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsEndTime :: Lens.Lens' DescribeAutoMLJobResponse (Core.Maybe Core.NominalDiffTime)
damljrrsEndTime = Lens.field @"endTime"
{-# INLINEABLE damljrrsEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | Returns the job's FailureReason.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsFailureReason :: Lens.Lens' DescribeAutoMLJobResponse (Core.Maybe Types.FailureReason)
damljrrsFailureReason = Lens.field @"failureReason"
{-# INLINEABLE damljrrsFailureReason #-}
{-# DEPRECATED failureReason "Use generic-lens or generic-optics with 'failureReason' instead"  #-}

-- | Returns the job's output from GenerateCandidateDefinitionsOnly.
--
-- /Note:/ Consider using 'generateCandidateDefinitionsOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsGenerateCandidateDefinitionsOnly :: Lens.Lens' DescribeAutoMLJobResponse (Core.Maybe Core.Bool)
damljrrsGenerateCandidateDefinitionsOnly = Lens.field @"generateCandidateDefinitionsOnly"
{-# INLINEABLE damljrrsGenerateCandidateDefinitionsOnly #-}
{-# DEPRECATED generateCandidateDefinitionsOnly "Use generic-lens or generic-optics with 'generateCandidateDefinitionsOnly' instead"  #-}

-- | Returns the job's problem type.
--
-- /Note:/ Consider using 'problemType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsProblemType :: Lens.Lens' DescribeAutoMLJobResponse (Core.Maybe Types.ProblemType)
damljrrsProblemType = Lens.field @"problemType"
{-# INLINEABLE damljrrsProblemType #-}
{-# DEPRECATED problemType "Use generic-lens or generic-optics with 'problemType' instead"  #-}

-- | This contains ProblemType, AutoMLJobObjective and CompletionCriteria. They're auto-inferred values, if not provided by you. If you do provide them, then they'll be the same as provided.
--
-- /Note:/ Consider using 'resolvedAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsResolvedAttributes :: Lens.Lens' DescribeAutoMLJobResponse (Core.Maybe Types.ResolvedAttributes)
damljrrsResolvedAttributes = Lens.field @"resolvedAttributes"
{-# INLINEABLE damljrrsResolvedAttributes #-}
{-# DEPRECATED resolvedAttributes "Use generic-lens or generic-optics with 'resolvedAttributes' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
damljrrsResponseStatus :: Lens.Lens' DescribeAutoMLJobResponse Core.Int
damljrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE damljrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
