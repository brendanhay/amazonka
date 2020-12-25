{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.StartReplicationTaskAssessmentRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a new premigration assessment run for one or more individual assessments of a migration task.
--
-- The assessments that you can specify depend on the source and target database engine and the migration type defined for the given task. To run this operation, your migration task must already be created. After you run this operation, you can review the status of each individual assessment. You can also run the migration task manually after the assessment run and its individual assessments complete.
module Network.AWS.DMS.StartReplicationTaskAssessmentRun
  ( -- * Creating a request
    StartReplicationTaskAssessmentRun (..),
    mkStartReplicationTaskAssessmentRun,

    -- ** Request lenses
    srtarReplicationTaskArn,
    srtarServiceAccessRoleArn,
    srtarResultLocationBucket,
    srtarAssessmentRunName,
    srtarExclude,
    srtarIncludeOnly,
    srtarResultEncryptionMode,
    srtarResultKmsKeyArn,
    srtarResultLocationFolder,

    -- * Destructuring the response
    StartReplicationTaskAssessmentRunResponse (..),
    mkStartReplicationTaskAssessmentRunResponse,

    -- ** Response lenses
    srtarrrsReplicationTaskAssessmentRun,
    srtarrrsResponseStatus,
  )
where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkStartReplicationTaskAssessmentRun' smart constructor.
data StartReplicationTaskAssessmentRun = StartReplicationTaskAssessmentRun'
  { -- | Amazon Resource Name (ARN) of the migration task associated with the premigration assessment run that you want to start.
    replicationTaskArn :: Types.String,
    -- | ARN of a service role needed to start the assessment run.
    serviceAccessRoleArn :: Types.String,
    -- | Amazon S3 bucket where you want AWS DMS to store the results of this assessment run.
    resultLocationBucket :: Types.String,
    -- | Unique name to identify the assessment run.
    assessmentRunName :: Types.String,
    -- | Space-separated list of names for specific individual assessments that you want to exclude. These names come from the default list of individual assessments that AWS DMS supports for the associated migration task. This task is specified by @ReplicationTaskArn@ .
    exclude :: Core.Maybe [Types.String],
    -- | Space-separated list of names for specific individual assessments that you want to include. These names come from the default list of individual assessments that AWS DMS supports for the associated migration task. This task is specified by @ReplicationTaskArn@ .
    includeOnly :: Core.Maybe [Types.String],
    -- | Encryption mode that you can specify to encrypt the results of this assessment run. If you don't specify this request parameter, AWS DMS stores the assessment run results without encryption. You can specify one of the options following:
    --
    --
    --     * @"SSE_S3"@ – The server-side encryption provided as a default by Amazon S3.
    --
    --
    --     * @"SSE_KMS"@ – AWS Key Management Service (AWS KMS) encryption. This encryption can use either a custom KMS encryption key that you specify or the default KMS encryption key that DMS provides.
    resultEncryptionMode :: Core.Maybe Types.String,
    -- | ARN of a custom KMS encryption key that you specify when you set @ResultEncryptionMode@ to @"SSE_KMS@ ".
    resultKmsKeyArn :: Core.Maybe Types.String,
    -- | Folder within an Amazon S3 bucket where you want AWS DMS to store the results of this assessment run.
    resultLocationFolder :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartReplicationTaskAssessmentRun' value with any optional fields omitted.
mkStartReplicationTaskAssessmentRun ::
  -- | 'replicationTaskArn'
  Types.String ->
  -- | 'serviceAccessRoleArn'
  Types.String ->
  -- | 'resultLocationBucket'
  Types.String ->
  -- | 'assessmentRunName'
  Types.String ->
  StartReplicationTaskAssessmentRun
mkStartReplicationTaskAssessmentRun
  replicationTaskArn
  serviceAccessRoleArn
  resultLocationBucket
  assessmentRunName =
    StartReplicationTaskAssessmentRun'
      { replicationTaskArn,
        serviceAccessRoleArn,
        resultLocationBucket,
        assessmentRunName,
        exclude = Core.Nothing,
        includeOnly = Core.Nothing,
        resultEncryptionMode = Core.Nothing,
        resultKmsKeyArn = Core.Nothing,
        resultLocationFolder = Core.Nothing
      }

-- | Amazon Resource Name (ARN) of the migration task associated with the premigration assessment run that you want to start.
--
-- /Note:/ Consider using 'replicationTaskArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtarReplicationTaskArn :: Lens.Lens' StartReplicationTaskAssessmentRun Types.String
srtarReplicationTaskArn = Lens.field @"replicationTaskArn"
{-# DEPRECATED srtarReplicationTaskArn "Use generic-lens or generic-optics with 'replicationTaskArn' instead." #-}

-- | ARN of a service role needed to start the assessment run.
--
-- /Note:/ Consider using 'serviceAccessRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtarServiceAccessRoleArn :: Lens.Lens' StartReplicationTaskAssessmentRun Types.String
srtarServiceAccessRoleArn = Lens.field @"serviceAccessRoleArn"
{-# DEPRECATED srtarServiceAccessRoleArn "Use generic-lens or generic-optics with 'serviceAccessRoleArn' instead." #-}

-- | Amazon S3 bucket where you want AWS DMS to store the results of this assessment run.
--
-- /Note:/ Consider using 'resultLocationBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtarResultLocationBucket :: Lens.Lens' StartReplicationTaskAssessmentRun Types.String
srtarResultLocationBucket = Lens.field @"resultLocationBucket"
{-# DEPRECATED srtarResultLocationBucket "Use generic-lens or generic-optics with 'resultLocationBucket' instead." #-}

-- | Unique name to identify the assessment run.
--
-- /Note:/ Consider using 'assessmentRunName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtarAssessmentRunName :: Lens.Lens' StartReplicationTaskAssessmentRun Types.String
srtarAssessmentRunName = Lens.field @"assessmentRunName"
{-# DEPRECATED srtarAssessmentRunName "Use generic-lens or generic-optics with 'assessmentRunName' instead." #-}

-- | Space-separated list of names for specific individual assessments that you want to exclude. These names come from the default list of individual assessments that AWS DMS supports for the associated migration task. This task is specified by @ReplicationTaskArn@ .
--
-- /Note:/ Consider using 'exclude' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtarExclude :: Lens.Lens' StartReplicationTaskAssessmentRun (Core.Maybe [Types.String])
srtarExclude = Lens.field @"exclude"
{-# DEPRECATED srtarExclude "Use generic-lens or generic-optics with 'exclude' instead." #-}

-- | Space-separated list of names for specific individual assessments that you want to include. These names come from the default list of individual assessments that AWS DMS supports for the associated migration task. This task is specified by @ReplicationTaskArn@ .
--
-- /Note:/ Consider using 'includeOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtarIncludeOnly :: Lens.Lens' StartReplicationTaskAssessmentRun (Core.Maybe [Types.String])
srtarIncludeOnly = Lens.field @"includeOnly"
{-# DEPRECATED srtarIncludeOnly "Use generic-lens or generic-optics with 'includeOnly' instead." #-}

-- | Encryption mode that you can specify to encrypt the results of this assessment run. If you don't specify this request parameter, AWS DMS stores the assessment run results without encryption. You can specify one of the options following:
--
--
--     * @"SSE_S3"@ – The server-side encryption provided as a default by Amazon S3.
--
--
--     * @"SSE_KMS"@ – AWS Key Management Service (AWS KMS) encryption. This encryption can use either a custom KMS encryption key that you specify or the default KMS encryption key that DMS provides.
--
--
--
-- /Note:/ Consider using 'resultEncryptionMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtarResultEncryptionMode :: Lens.Lens' StartReplicationTaskAssessmentRun (Core.Maybe Types.String)
srtarResultEncryptionMode = Lens.field @"resultEncryptionMode"
{-# DEPRECATED srtarResultEncryptionMode "Use generic-lens or generic-optics with 'resultEncryptionMode' instead." #-}

-- | ARN of a custom KMS encryption key that you specify when you set @ResultEncryptionMode@ to @"SSE_KMS@ ".
--
-- /Note:/ Consider using 'resultKmsKeyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtarResultKmsKeyArn :: Lens.Lens' StartReplicationTaskAssessmentRun (Core.Maybe Types.String)
srtarResultKmsKeyArn = Lens.field @"resultKmsKeyArn"
{-# DEPRECATED srtarResultKmsKeyArn "Use generic-lens or generic-optics with 'resultKmsKeyArn' instead." #-}

-- | Folder within an Amazon S3 bucket where you want AWS DMS to store the results of this assessment run.
--
-- /Note:/ Consider using 'resultLocationFolder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtarResultLocationFolder :: Lens.Lens' StartReplicationTaskAssessmentRun (Core.Maybe Types.String)
srtarResultLocationFolder = Lens.field @"resultLocationFolder"
{-# DEPRECATED srtarResultLocationFolder "Use generic-lens or generic-optics with 'resultLocationFolder' instead." #-}

instance Core.FromJSON StartReplicationTaskAssessmentRun where
  toJSON StartReplicationTaskAssessmentRun {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ReplicationTaskArn" Core..= replicationTaskArn),
            Core.Just ("ServiceAccessRoleArn" Core..= serviceAccessRoleArn),
            Core.Just ("ResultLocationBucket" Core..= resultLocationBucket),
            Core.Just ("AssessmentRunName" Core..= assessmentRunName),
            ("Exclude" Core..=) Core.<$> exclude,
            ("IncludeOnly" Core..=) Core.<$> includeOnly,
            ("ResultEncryptionMode" Core..=) Core.<$> resultEncryptionMode,
            ("ResultKmsKeyArn" Core..=) Core.<$> resultKmsKeyArn,
            ("ResultLocationFolder" Core..=) Core.<$> resultLocationFolder
          ]
      )

instance Core.AWSRequest StartReplicationTaskAssessmentRun where
  type
    Rs StartReplicationTaskAssessmentRun =
      StartReplicationTaskAssessmentRunResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonDMSv20160101.StartReplicationTaskAssessmentRun"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartReplicationTaskAssessmentRunResponse'
            Core.<$> (x Core..:? "ReplicationTaskAssessmentRun")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- |
--
-- /See:/ 'mkStartReplicationTaskAssessmentRunResponse' smart constructor.
data StartReplicationTaskAssessmentRunResponse = StartReplicationTaskAssessmentRunResponse'
  { -- | The premigration assessment run that was started.
    replicationTaskAssessmentRun :: Core.Maybe Types.ReplicationTaskAssessmentRun,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StartReplicationTaskAssessmentRunResponse' value with any optional fields omitted.
mkStartReplicationTaskAssessmentRunResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartReplicationTaskAssessmentRunResponse
mkStartReplicationTaskAssessmentRunResponse responseStatus =
  StartReplicationTaskAssessmentRunResponse'
    { replicationTaskAssessmentRun =
        Core.Nothing,
      responseStatus
    }

-- | The premigration assessment run that was started.
--
-- /Note:/ Consider using 'replicationTaskAssessmentRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtarrrsReplicationTaskAssessmentRun :: Lens.Lens' StartReplicationTaskAssessmentRunResponse (Core.Maybe Types.ReplicationTaskAssessmentRun)
srtarrrsReplicationTaskAssessmentRun = Lens.field @"replicationTaskAssessmentRun"
{-# DEPRECATED srtarrrsReplicationTaskAssessmentRun "Use generic-lens or generic-optics with 'replicationTaskAssessmentRun' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtarrrsResponseStatus :: Lens.Lens' StartReplicationTaskAssessmentRunResponse Core.Int
srtarrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED srtarrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
