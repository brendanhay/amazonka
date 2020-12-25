{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ImportExport.UpdateJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You use this operation to change the parameters specified in the original manifest file by supplying a new manifest file. The manifest file attached to this request replaces the original manifest file. You can only use the operation after a CreateJob request but before the data transfer starts and you can only use it on jobs you own.
module Network.AWS.ImportExport.UpdateJob
  ( -- * Creating a request
    UpdateJob (..),
    mkUpdateJob,

    -- ** Request lenses
    ujJobId,
    ujManifest,
    ujJobType,
    ujValidateOnly,
    ujAPIVersion,

    -- * Destructuring the response
    UpdateJobResponse (..),
    mkUpdateJobResponse,

    -- ** Response lenses
    ujrrsArtifactList,
    ujrrsSuccess,
    ujrrsWarningMessage,
    ujrrsResponseStatus,
  )
where

import qualified Network.AWS.ImportExport.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input structure for the UpateJob operation.
--
-- /See:/ 'mkUpdateJob' smart constructor.
data UpdateJob = UpdateJob'
  { jobId :: Types.JobId,
    manifest :: Types.Manifest,
    jobType :: Types.JobType,
    validateOnly :: Core.Bool,
    aPIVersion :: Core.Maybe Types.APIVersion
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateJob' value with any optional fields omitted.
mkUpdateJob ::
  -- | 'jobId'
  Types.JobId ->
  -- | 'manifest'
  Types.Manifest ->
  -- | 'jobType'
  Types.JobType ->
  -- | 'validateOnly'
  Core.Bool ->
  UpdateJob
mkUpdateJob jobId manifest jobType validateOnly =
  UpdateJob'
    { jobId,
      manifest,
      jobType,
      validateOnly,
      aPIVersion = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujJobId :: Lens.Lens' UpdateJob Types.JobId
ujJobId = Lens.field @"jobId"
{-# DEPRECATED ujJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'manifest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujManifest :: Lens.Lens' UpdateJob Types.Manifest
ujManifest = Lens.field @"manifest"
{-# DEPRECATED ujManifest "Use generic-lens or generic-optics with 'manifest' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'jobType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujJobType :: Lens.Lens' UpdateJob Types.JobType
ujJobType = Lens.field @"jobType"
{-# DEPRECATED ujJobType "Use generic-lens or generic-optics with 'jobType' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'validateOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujValidateOnly :: Lens.Lens' UpdateJob Core.Bool
ujValidateOnly = Lens.field @"validateOnly"
{-# DEPRECATED ujValidateOnly "Use generic-lens or generic-optics with 'validateOnly' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'aPIVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujAPIVersion :: Lens.Lens' UpdateJob (Core.Maybe Types.APIVersion)
ujAPIVersion = Lens.field @"aPIVersion"
{-# DEPRECATED ujAPIVersion "Use generic-lens or generic-optics with 'aPIVersion' instead." #-}

instance Core.AWSRequest UpdateJob where
  type Rs UpdateJob = UpdateJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Operation=UpdateJob", "")
                Core.<> (Core.pure ("Action", "UpdateJob"))
                Core.<> (Core.pure ("Version", "2010-06-01"))
                Core.<> (Core.toQueryValue "JobId" jobId)
                Core.<> (Core.toQueryValue "Manifest" manifest)
                Core.<> (Core.toQueryValue "JobType" jobType)
                Core.<> (Core.toQueryValue "ValidateOnly" validateOnly)
                Core.<> (Core.toQueryValue "APIVersion" Core.<$> aPIVersion)
            )
      }
  response =
    Response.receiveXMLWrapper
      "UpdateJobResult"
      ( \s h x ->
          UpdateJobResponse'
            Core.<$> (x Core..@? "ArtifactList" Core..<@> Core.parseXMLList "member")
            Core.<*> (x Core..@? "Success")
            Core.<*> (x Core..@? "WarningMessage")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Output structure for the UpateJob operation.
--
-- /See:/ 'mkUpdateJobResponse' smart constructor.
data UpdateJobResponse = UpdateJobResponse'
  { artifactList :: Core.Maybe [Types.Artifact],
    success :: Core.Maybe Core.Bool,
    warningMessage :: Core.Maybe Types.WarningMessage,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateJobResponse' value with any optional fields omitted.
mkUpdateJobResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateJobResponse
mkUpdateJobResponse responseStatus =
  UpdateJobResponse'
    { artifactList = Core.Nothing,
      success = Core.Nothing,
      warningMessage = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'artifactList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujrrsArtifactList :: Lens.Lens' UpdateJobResponse (Core.Maybe [Types.Artifact])
ujrrsArtifactList = Lens.field @"artifactList"
{-# DEPRECATED ujrrsArtifactList "Use generic-lens or generic-optics with 'artifactList' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'success' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujrrsSuccess :: Lens.Lens' UpdateJobResponse (Core.Maybe Core.Bool)
ujrrsSuccess = Lens.field @"success"
{-# DEPRECATED ujrrsSuccess "Use generic-lens or generic-optics with 'success' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'warningMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujrrsWarningMessage :: Lens.Lens' UpdateJobResponse (Core.Maybe Types.WarningMessage)
ujrrsWarningMessage = Lens.field @"warningMessage"
{-# DEPRECATED ujrrsWarningMessage "Use generic-lens or generic-optics with 'warningMessage' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ujrrsResponseStatus :: Lens.Lens' UpdateJobResponse Core.Int
ujrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ujrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
