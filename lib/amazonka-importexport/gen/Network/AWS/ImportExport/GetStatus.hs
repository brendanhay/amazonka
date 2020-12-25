{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ImportExport.GetStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns information about a job, including where the job is in the processing pipeline, the status of the results, and the signature value associated with the job. You can only return information about jobs you own.
module Network.AWS.ImportExport.GetStatus
  ( -- * Creating a request
    GetStatus (..),
    mkGetStatus,

    -- ** Request lenses
    gsJobId,
    gsAPIVersion,

    -- * Destructuring the response
    GetStatusResponse (..),
    mkGetStatusResponse,

    -- ** Response lenses
    gsrrsArtifactList,
    gsrrsCarrier,
    gsrrsCreationDate,
    gsrrsCurrentManifest,
    gsrrsErrorCount,
    gsrrsJobId,
    gsrrsJobType,
    gsrrsLocationCode,
    gsrrsLocationMessage,
    gsrrsLogBucket,
    gsrrsLogKey,
    gsrrsProgressCode,
    gsrrsProgressMessage,
    gsrrsSignature,
    gsrrsSignatureFileContents,
    gsrrsTrackingNumber,
    gsrrsResponseStatus,
  )
where

import qualified Network.AWS.ImportExport.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input structure for the GetStatus operation.
--
-- /See:/ 'mkGetStatus' smart constructor.
data GetStatus = GetStatus'
  { jobId :: Types.JobId,
    aPIVersion :: Core.Maybe Types.APIVersion
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetStatus' value with any optional fields omitted.
mkGetStatus ::
  -- | 'jobId'
  Types.JobId ->
  GetStatus
mkGetStatus jobId = GetStatus' {jobId, aPIVersion = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsJobId :: Lens.Lens' GetStatus Types.JobId
gsJobId = Lens.field @"jobId"
{-# DEPRECATED gsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'aPIVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsAPIVersion :: Lens.Lens' GetStatus (Core.Maybe Types.APIVersion)
gsAPIVersion = Lens.field @"aPIVersion"
{-# DEPRECATED gsAPIVersion "Use generic-lens or generic-optics with 'aPIVersion' instead." #-}

instance Core.AWSRequest GetStatus where
  type Rs GetStatus = GetStatusResponse
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
            ( Core.pure ("Operation=GetStatus", "")
                Core.<> (Core.pure ("Action", "GetStatus"))
                Core.<> (Core.pure ("Version", "2010-06-01"))
                Core.<> (Core.toQueryValue "JobId" jobId)
                Core.<> (Core.toQueryValue "APIVersion" Core.<$> aPIVersion)
            )
      }
  response =
    Response.receiveXMLWrapper
      "GetStatusResult"
      ( \s h x ->
          GetStatusResponse'
            Core.<$> (x Core..@? "ArtifactList" Core..<@> Core.parseXMLList "member")
            Core.<*> (x Core..@? "Carrier")
            Core.<*> (x Core..@? "CreationDate")
            Core.<*> (x Core..@? "CurrentManifest")
            Core.<*> (x Core..@? "ErrorCount")
            Core.<*> (x Core..@? "JobId")
            Core.<*> (x Core..@? "JobType")
            Core.<*> (x Core..@? "LocationCode")
            Core.<*> (x Core..@? "LocationMessage")
            Core.<*> (x Core..@? "LogBucket")
            Core.<*> (x Core..@? "LogKey")
            Core.<*> (x Core..@? "ProgressCode")
            Core.<*> (x Core..@? "ProgressMessage")
            Core.<*> (x Core..@? "Signature")
            Core.<*> (x Core..@? "SignatureFileContents")
            Core.<*> (x Core..@? "TrackingNumber")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Output structure for the GetStatus operation.
--
-- /See:/ 'mkGetStatusResponse' smart constructor.
data GetStatusResponse = GetStatusResponse'
  { artifactList :: Core.Maybe [Types.Artifact],
    carrier :: Core.Maybe Types.Carrier,
    creationDate :: Core.Maybe Core.UTCTime,
    currentManifest :: Core.Maybe Types.CurrentManifest,
    errorCount :: Core.Maybe Core.Int,
    jobId :: Core.Maybe Types.JobId,
    jobType :: Core.Maybe Types.JobType,
    locationCode :: Core.Maybe Types.LocationCode,
    locationMessage :: Core.Maybe Types.LocationMessage,
    logBucket :: Core.Maybe Types.LogBucket,
    logKey :: Core.Maybe Types.LogKey,
    progressCode :: Core.Maybe Types.ProgressCode,
    progressMessage :: Core.Maybe Types.ProgressMessage,
    signature :: Core.Maybe Types.Signature,
    signatureFileContents :: Core.Maybe Types.Signature,
    trackingNumber :: Core.Maybe Types.TrackingNumber,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetStatusResponse' value with any optional fields omitted.
mkGetStatusResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetStatusResponse
mkGetStatusResponse responseStatus =
  GetStatusResponse'
    { artifactList = Core.Nothing,
      carrier = Core.Nothing,
      creationDate = Core.Nothing,
      currentManifest = Core.Nothing,
      errorCount = Core.Nothing,
      jobId = Core.Nothing,
      jobType = Core.Nothing,
      locationCode = Core.Nothing,
      locationMessage = Core.Nothing,
      logBucket = Core.Nothing,
      logKey = Core.Nothing,
      progressCode = Core.Nothing,
      progressMessage = Core.Nothing,
      signature = Core.Nothing,
      signatureFileContents = Core.Nothing,
      trackingNumber = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'artifactList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsArtifactList :: Lens.Lens' GetStatusResponse (Core.Maybe [Types.Artifact])
gsrrsArtifactList = Lens.field @"artifactList"
{-# DEPRECATED gsrrsArtifactList "Use generic-lens or generic-optics with 'artifactList' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'carrier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsCarrier :: Lens.Lens' GetStatusResponse (Core.Maybe Types.Carrier)
gsrrsCarrier = Lens.field @"carrier"
{-# DEPRECATED gsrrsCarrier "Use generic-lens or generic-optics with 'carrier' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsCreationDate :: Lens.Lens' GetStatusResponse (Core.Maybe Core.UTCTime)
gsrrsCreationDate = Lens.field @"creationDate"
{-# DEPRECATED gsrrsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'currentManifest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsCurrentManifest :: Lens.Lens' GetStatusResponse (Core.Maybe Types.CurrentManifest)
gsrrsCurrentManifest = Lens.field @"currentManifest"
{-# DEPRECATED gsrrsCurrentManifest "Use generic-lens or generic-optics with 'currentManifest' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'errorCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsErrorCount :: Lens.Lens' GetStatusResponse (Core.Maybe Core.Int)
gsrrsErrorCount = Lens.field @"errorCount"
{-# DEPRECATED gsrrsErrorCount "Use generic-lens or generic-optics with 'errorCount' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsJobId :: Lens.Lens' GetStatusResponse (Core.Maybe Types.JobId)
gsrrsJobId = Lens.field @"jobId"
{-# DEPRECATED gsrrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'jobType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsJobType :: Lens.Lens' GetStatusResponse (Core.Maybe Types.JobType)
gsrrsJobType = Lens.field @"jobType"
{-# DEPRECATED gsrrsJobType "Use generic-lens or generic-optics with 'jobType' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'locationCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsLocationCode :: Lens.Lens' GetStatusResponse (Core.Maybe Types.LocationCode)
gsrrsLocationCode = Lens.field @"locationCode"
{-# DEPRECATED gsrrsLocationCode "Use generic-lens or generic-optics with 'locationCode' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'locationMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsLocationMessage :: Lens.Lens' GetStatusResponse (Core.Maybe Types.LocationMessage)
gsrrsLocationMessage = Lens.field @"locationMessage"
{-# DEPRECATED gsrrsLocationMessage "Use generic-lens or generic-optics with 'locationMessage' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'logBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsLogBucket :: Lens.Lens' GetStatusResponse (Core.Maybe Types.LogBucket)
gsrrsLogBucket = Lens.field @"logBucket"
{-# DEPRECATED gsrrsLogBucket "Use generic-lens or generic-optics with 'logBucket' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'logKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsLogKey :: Lens.Lens' GetStatusResponse (Core.Maybe Types.LogKey)
gsrrsLogKey = Lens.field @"logKey"
{-# DEPRECATED gsrrsLogKey "Use generic-lens or generic-optics with 'logKey' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'progressCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsProgressCode :: Lens.Lens' GetStatusResponse (Core.Maybe Types.ProgressCode)
gsrrsProgressCode = Lens.field @"progressCode"
{-# DEPRECATED gsrrsProgressCode "Use generic-lens or generic-optics with 'progressCode' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'progressMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsProgressMessage :: Lens.Lens' GetStatusResponse (Core.Maybe Types.ProgressMessage)
gsrrsProgressMessage = Lens.field @"progressMessage"
{-# DEPRECATED gsrrsProgressMessage "Use generic-lens or generic-optics with 'progressMessage' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'signature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsSignature :: Lens.Lens' GetStatusResponse (Core.Maybe Types.Signature)
gsrrsSignature = Lens.field @"signature"
{-# DEPRECATED gsrrsSignature "Use generic-lens or generic-optics with 'signature' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'signatureFileContents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsSignatureFileContents :: Lens.Lens' GetStatusResponse (Core.Maybe Types.Signature)
gsrrsSignatureFileContents = Lens.field @"signatureFileContents"
{-# DEPRECATED gsrrsSignatureFileContents "Use generic-lens or generic-optics with 'signatureFileContents' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'trackingNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsTrackingNumber :: Lens.Lens' GetStatusResponse (Core.Maybe Types.TrackingNumber)
gsrrsTrackingNumber = Lens.field @"trackingNumber"
{-# DEPRECATED gsrrsTrackingNumber "Use generic-lens or generic-optics with 'trackingNumber' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsResponseStatus :: Lens.Lens' GetStatusResponse Core.Int
gsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
