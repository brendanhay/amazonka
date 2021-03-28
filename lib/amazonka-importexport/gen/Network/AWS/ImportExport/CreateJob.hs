{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateJob (..)
    , mkCreateJob
    -- ** Request lenses
    , cjJobType
    , cjManifest
    , cjValidateOnly
    , cjAPIVersion
    , cjManifestAddendum

    -- * Destructuring the response
    , CreateJobResponse (..)
    , mkCreateJobResponse
    -- ** Response lenses
    , cjrrsArtifactList
    , cjrrsJobId
    , cjrrsJobType
    , cjrrsSignature
    , cjrrsSignatureFileContents
    , cjrrsWarningMessage
    , cjrrsResponseStatus
    ) where

import qualified Network.AWS.ImportExport.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input structure for the CreateJob operation.
--
-- /See:/ 'mkCreateJob' smart constructor.
data CreateJob = CreateJob'
  { jobType :: Types.JobType
  , manifest :: Types.Manifest
  , validateOnly :: Core.Bool
  , aPIVersion :: Core.Maybe Types.APIVersion
  , manifestAddendum :: Core.Maybe Types.ManifestAddendum
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateJob' value with any optional fields omitted.
mkCreateJob
    :: Types.JobType -- ^ 'jobType'
    -> Types.Manifest -- ^ 'manifest'
    -> Core.Bool -- ^ 'validateOnly'
    -> CreateJob
mkCreateJob jobType manifest validateOnly
  = CreateJob'{jobType, manifest, validateOnly,
               aPIVersion = Core.Nothing, manifestAddendum = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'jobType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjJobType :: Lens.Lens' CreateJob Types.JobType
cjJobType = Lens.field @"jobType"
{-# INLINEABLE cjJobType #-}
{-# DEPRECATED jobType "Use generic-lens or generic-optics with 'jobType' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'manifest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjManifest :: Lens.Lens' CreateJob Types.Manifest
cjManifest = Lens.field @"manifest"
{-# INLINEABLE cjManifest #-}
{-# DEPRECATED manifest "Use generic-lens or generic-optics with 'manifest' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'validateOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjValidateOnly :: Lens.Lens' CreateJob Core.Bool
cjValidateOnly = Lens.field @"validateOnly"
{-# INLINEABLE cjValidateOnly #-}
{-# DEPRECATED validateOnly "Use generic-lens or generic-optics with 'validateOnly' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'aPIVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjAPIVersion :: Lens.Lens' CreateJob (Core.Maybe Types.APIVersion)
cjAPIVersion = Lens.field @"aPIVersion"
{-# INLINEABLE cjAPIVersion #-}
{-# DEPRECATED aPIVersion "Use generic-lens or generic-optics with 'aPIVersion' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'manifestAddendum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjManifestAddendum :: Lens.Lens' CreateJob (Core.Maybe Types.ManifestAddendum)
cjManifestAddendum = Lens.field @"manifestAddendum"
{-# INLINEABLE cjManifestAddendum #-}
{-# DEPRECATED manifestAddendum "Use generic-lens or generic-optics with 'manifestAddendum' instead"  #-}

instance Core.ToQuery CreateJob where
        toQuery CreateJob{..}
          = Core.toQueryPair "Operation=CreateJob" ("" :: Core.Text) Core.<>
              Core.toQueryPair "Action" ("CreateJob" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-06-01" :: Core.Text)
              Core.<> Core.toQueryPair "JobType" jobType
              Core.<> Core.toQueryPair "Manifest" manifest
              Core.<> Core.toQueryPair "ValidateOnly" validateOnly
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "APIVersion") aPIVersion
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ManifestAddendum")
                manifestAddendum

instance Core.ToHeaders CreateJob where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateJob where
        type Rs CreateJob = CreateJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "CreateJobResult"
              (\ s h x ->
                 CreateJobResponse' Core.<$>
                   (x Core..@? "ArtifactList" Core..<@> Core.parseXMLList "member")
                     Core.<*> x Core..@? "JobId"
                     Core.<*> x Core..@? "JobType"
                     Core.<*> x Core..@? "Signature"
                     Core.<*> x Core..@? "SignatureFileContents"
                     Core.<*> x Core..@? "WarningMessage"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Output structure for the CreateJob operation.
--
-- /See:/ 'mkCreateJobResponse' smart constructor.
data CreateJobResponse = CreateJobResponse'
  { artifactList :: Core.Maybe [Types.Artifact]
  , jobId :: Core.Maybe Types.JobId
  , jobType :: Core.Maybe Types.JobType
  , signature :: Core.Maybe Types.Signature
  , signatureFileContents :: Core.Maybe Types.SignatureFileContents
  , warningMessage :: Core.Maybe Types.WarningMessage
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateJobResponse' value with any optional fields omitted.
mkCreateJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateJobResponse
mkCreateJobResponse responseStatus
  = CreateJobResponse'{artifactList = Core.Nothing,
                       jobId = Core.Nothing, jobType = Core.Nothing,
                       signature = Core.Nothing, signatureFileContents = Core.Nothing,
                       warningMessage = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'artifactList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrrsArtifactList :: Lens.Lens' CreateJobResponse (Core.Maybe [Types.Artifact])
cjrrsArtifactList = Lens.field @"artifactList"
{-# INLINEABLE cjrrsArtifactList #-}
{-# DEPRECATED artifactList "Use generic-lens or generic-optics with 'artifactList' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrrsJobId :: Lens.Lens' CreateJobResponse (Core.Maybe Types.JobId)
cjrrsJobId = Lens.field @"jobId"
{-# INLINEABLE cjrrsJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'jobType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrrsJobType :: Lens.Lens' CreateJobResponse (Core.Maybe Types.JobType)
cjrrsJobType = Lens.field @"jobType"
{-# INLINEABLE cjrrsJobType #-}
{-# DEPRECATED jobType "Use generic-lens or generic-optics with 'jobType' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'signature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrrsSignature :: Lens.Lens' CreateJobResponse (Core.Maybe Types.Signature)
cjrrsSignature = Lens.field @"signature"
{-# INLINEABLE cjrrsSignature #-}
{-# DEPRECATED signature "Use generic-lens or generic-optics with 'signature' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'signatureFileContents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrrsSignatureFileContents :: Lens.Lens' CreateJobResponse (Core.Maybe Types.SignatureFileContents)
cjrrsSignatureFileContents = Lens.field @"signatureFileContents"
{-# INLINEABLE cjrrsSignatureFileContents #-}
{-# DEPRECATED signatureFileContents "Use generic-lens or generic-optics with 'signatureFileContents' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'warningMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrrsWarningMessage :: Lens.Lens' CreateJobResponse (Core.Maybe Types.WarningMessage)
cjrrsWarningMessage = Lens.field @"warningMessage"
{-# INLINEABLE cjrrsWarningMessage #-}
{-# DEPRECATED warningMessage "Use generic-lens or generic-optics with 'warningMessage' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cjrrsResponseStatus :: Lens.Lens' CreateJobResponse Core.Int
cjrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cjrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
