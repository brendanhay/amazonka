{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ImportExport.CreateJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation initiates the process of scheduling an upload or download of your data. You include in the request a manifest that describes the data transfer specifics. The response to the request includes a job ID, which you can use in other operations, a signature that you use to identify your storage device, and the address where you should ship your storage device.
module Network.AWS.ImportExport.CreateJob
  ( -- * Creating a request
    CreateJob (..),
    mkCreateJob,

    -- ** Request lenses
    cjJobType,
    cjManifest,
    cjValidateOnly,
    cjAPIVersion,
    cjManifestAddendum,

    -- * Destructuring the response
    CreateJobResponse (..),
    mkCreateJobResponse,

    -- ** Response lenses
    cjrrsArtifactList,
    cjrrsJobId,
    cjrrsJobType,
    cjrrsSignature,
    cjrrsSignatureFileContents,
    cjrrsWarningMessage,
    cjrrsResponseStatus,
  )
where

import qualified Network.AWS.ImportExport.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input structure for the CreateJob operation.
--
-- /See:/ 'mkCreateJob' smart constructor.
data CreateJob = CreateJob'
  { jobType :: Types.JobType,
    manifest :: Types.Manifest,
    validateOnly :: Core.Bool,
    aPIVersion :: Core.Maybe Types.APIVersion,
    manifestAddendum :: Core.Maybe Types.ManifestAddendum
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateJob' value with any optional fields omitted.
mkCreateJob ::
  -- | 'jobType'
  Types.JobType ->
  -- | 'manifest'
  Types.Manifest ->
  -- | 'validateOnly'
  Core.Bool ->
  CreateJob
mkCreateJob jobType manifest validateOnly =
  CreateJob'
    { jobType,
      manifest,
      validateOnly,
      aPIVersion = Core.Nothing,
      manifestAddendum = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'jobType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjJobType :: Lens.Lens' CreateJob Types.JobType
cjJobType = Lens.field @"jobType"
{-# DEPRECATED cjJobType "Use generic-lens or generic-optics with 'jobType' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'manifest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjManifest :: Lens.Lens' CreateJob Types.Manifest
cjManifest = Lens.field @"manifest"
{-# DEPRECATED cjManifest "Use generic-lens or generic-optics with 'manifest' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'validateOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjValidateOnly :: Lens.Lens' CreateJob Core.Bool
cjValidateOnly = Lens.field @"validateOnly"
{-# DEPRECATED cjValidateOnly "Use generic-lens or generic-optics with 'validateOnly' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'aPIVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjAPIVersion :: Lens.Lens' CreateJob (Core.Maybe Types.APIVersion)
cjAPIVersion = Lens.field @"aPIVersion"
{-# DEPRECATED cjAPIVersion "Use generic-lens or generic-optics with 'aPIVersion' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'manifestAddendum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjManifestAddendum :: Lens.Lens' CreateJob (Core.Maybe Types.ManifestAddendum)
cjManifestAddendum = Lens.field @"manifestAddendum"
{-# DEPRECATED cjManifestAddendum "Use generic-lens or generic-optics with 'manifestAddendum' instead." #-}

instance Core.AWSRequest CreateJob where
  type Rs CreateJob = CreateJobResponse
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
            ( Core.pure ("Operation=CreateJob", "")
                Core.<> (Core.pure ("Action", "CreateJob"))
                Core.<> (Core.pure ("Version", "2010-06-01"))
                Core.<> (Core.toQueryValue "JobType" jobType)
                Core.<> (Core.toQueryValue "Manifest" manifest)
                Core.<> (Core.toQueryValue "ValidateOnly" validateOnly)
                Core.<> (Core.toQueryValue "APIVersion" Core.<$> aPIVersion)
                Core.<> (Core.toQueryValue "ManifestAddendum" Core.<$> manifestAddendum)
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateJobResult"
      ( \s h x ->
          CreateJobResponse'
            Core.<$> (x Core..@? "ArtifactList" Core..<@> Core.parseXMLList "member")
            Core.<*> (x Core..@? "JobId")
            Core.<*> (x Core..@? "JobType")
            Core.<*> (x Core..@? "Signature")
            Core.<*> (x Core..@? "SignatureFileContents")
            Core.<*> (x Core..@? "WarningMessage")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Output structure for the CreateJob operation.
--
-- /See:/ 'mkCreateJobResponse' smart constructor.
data CreateJobResponse = CreateJobResponse'
  { artifactList :: Core.Maybe [Types.Artifact],
    jobId :: Core.Maybe Types.JobId,
    jobType :: Core.Maybe Types.JobType,
    signature :: Core.Maybe Types.Signature,
    signatureFileContents :: Core.Maybe Types.SignatureFileContents,
    warningMessage :: Core.Maybe Types.WarningMessage,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateJobResponse' value with any optional fields omitted.
mkCreateJobResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateJobResponse
mkCreateJobResponse responseStatus =
  CreateJobResponse'
    { artifactList = Core.Nothing,
      jobId = Core.Nothing,
      jobType = Core.Nothing,
      signature = Core.Nothing,
      signatureFileContents = Core.Nothing,
      warningMessage = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'artifactList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrrsArtifactList :: Lens.Lens' CreateJobResponse (Core.Maybe [Types.Artifact])
cjrrsArtifactList = Lens.field @"artifactList"
{-# DEPRECATED cjrrsArtifactList "Use generic-lens or generic-optics with 'artifactList' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrrsJobId :: Lens.Lens' CreateJobResponse (Core.Maybe Types.JobId)
cjrrsJobId = Lens.field @"jobId"
{-# DEPRECATED cjrrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'jobType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrrsJobType :: Lens.Lens' CreateJobResponse (Core.Maybe Types.JobType)
cjrrsJobType = Lens.field @"jobType"
{-# DEPRECATED cjrrsJobType "Use generic-lens or generic-optics with 'jobType' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'signature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrrsSignature :: Lens.Lens' CreateJobResponse (Core.Maybe Types.Signature)
cjrrsSignature = Lens.field @"signature"
{-# DEPRECATED cjrrsSignature "Use generic-lens or generic-optics with 'signature' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'signatureFileContents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrrsSignatureFileContents :: Lens.Lens' CreateJobResponse (Core.Maybe Types.SignatureFileContents)
cjrrsSignatureFileContents = Lens.field @"signatureFileContents"
{-# DEPRECATED cjrrsSignatureFileContents "Use generic-lens or generic-optics with 'signatureFileContents' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'warningMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrrsWarningMessage :: Lens.Lens' CreateJobResponse (Core.Maybe Types.WarningMessage)
cjrrsWarningMessage = Lens.field @"warningMessage"
{-# DEPRECATED cjrrsWarningMessage "Use generic-lens or generic-optics with 'warningMessage' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrrsResponseStatus :: Lens.Lens' CreateJobResponse Core.Int
cjrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cjrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
