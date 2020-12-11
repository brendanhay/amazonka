{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    gsAPIVersion,
    gsJobId,

    -- * Destructuring the response
    GetStatusResponse (..),
    mkGetStatusResponse,

    -- ** Response lenses
    gsrsCarrier,
    gsrsTrackingNumber,
    gsrsSignature,
    gsrsJobType,
    gsrsJobId,
    gsrsSignatureFileContents,
    gsrsErrorCount,
    gsrsCurrentManifest,
    gsrsArtifactList,
    gsrsLogBucket,
    gsrsCreationDate,
    gsrsProgressCode,
    gsrsLocationCode,
    gsrsLogKey,
    gsrsLocationMessage,
    gsrsProgressMessage,
    gsrsResponseStatus,
  )
where

import Network.AWS.ImportExport.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Input structure for the GetStatus operation.
--
-- /See:/ 'mkGetStatus' smart constructor.
data GetStatus = GetStatus'
  { apiVersion :: Lude.Maybe Lude.Text,
    jobId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetStatus' with the minimum fields required to make a request.
--
-- * 'apiVersion' - Undocumented field.
-- * 'jobId' - Undocumented field.
mkGetStatus ::
  -- | 'jobId'
  Lude.Text ->
  GetStatus
mkGetStatus pJobId_ =
  GetStatus' {apiVersion = Lude.Nothing, jobId = pJobId_}

-- | Undocumented field.
--
-- /Note:/ Consider using 'apiVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsAPIVersion :: Lens.Lens' GetStatus (Lude.Maybe Lude.Text)
gsAPIVersion = Lens.lens (apiVersion :: GetStatus -> Lude.Maybe Lude.Text) (\s a -> s {apiVersion = a} :: GetStatus)
{-# DEPRECATED gsAPIVersion "Use generic-lens or generic-optics with 'apiVersion' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsJobId :: Lens.Lens' GetStatus Lude.Text
gsJobId = Lens.lens (jobId :: GetStatus -> Lude.Text) (\s a -> s {jobId = a} :: GetStatus)
{-# DEPRECATED gsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

instance Lude.AWSRequest GetStatus where
  type Rs GetStatus = GetStatusResponse
  request = Req.postQuery importExportService
  response =
    Res.receiveXMLWrapper
      "GetStatusResult"
      ( \s h x ->
          GetStatusResponse'
            Lude.<$> (x Lude..@? "Carrier")
            Lude.<*> (x Lude..@? "TrackingNumber")
            Lude.<*> (x Lude..@? "Signature")
            Lude.<*> (x Lude..@? "JobType")
            Lude.<*> (x Lude..@? "JobId")
            Lude.<*> (x Lude..@? "SignatureFileContents")
            Lude.<*> (x Lude..@? "ErrorCount")
            Lude.<*> (x Lude..@? "CurrentManifest")
            Lude.<*> ( x Lude..@? "ArtifactList" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "LogBucket")
            Lude.<*> (x Lude..@? "CreationDate")
            Lude.<*> (x Lude..@? "ProgressCode")
            Lude.<*> (x Lude..@? "LocationCode")
            Lude.<*> (x Lude..@? "LogKey")
            Lude.<*> (x Lude..@? "LocationMessage")
            Lude.<*> (x Lude..@? "ProgressMessage")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetStatus where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery GetStatus where
  toQuery GetStatus' {..} =
    Lude.mconcat
      [ "Operation=GetStatus",
        "Action" Lude.=: ("GetStatus" :: Lude.ByteString),
        "Version" Lude.=: ("2010-06-01" :: Lude.ByteString),
        "APIVersion" Lude.=: apiVersion,
        "JobId" Lude.=: jobId
      ]

-- | Output structure for the GetStatus operation.
--
-- /See:/ 'mkGetStatusResponse' smart constructor.
data GetStatusResponse = GetStatusResponse'
  { carrier ::
      Lude.Maybe Lude.Text,
    trackingNumber :: Lude.Maybe Lude.Text,
    signature :: Lude.Maybe Lude.Text,
    jobType :: Lude.Maybe JobType,
    jobId :: Lude.Maybe Lude.Text,
    signatureFileContents :: Lude.Maybe Lude.Text,
    errorCount :: Lude.Maybe Lude.Int,
    currentManifest :: Lude.Maybe Lude.Text,
    artifactList :: Lude.Maybe [Artifact],
    logBucket :: Lude.Maybe Lude.Text,
    creationDate :: Lude.Maybe Lude.ISO8601,
    progressCode :: Lude.Maybe Lude.Text,
    locationCode :: Lude.Maybe Lude.Text,
    logKey :: Lude.Maybe Lude.Text,
    locationMessage :: Lude.Maybe Lude.Text,
    progressMessage :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetStatusResponse' with the minimum fields required to make a request.
--
-- * 'artifactList' - Undocumented field.
-- * 'carrier' - Undocumented field.
-- * 'creationDate' - Undocumented field.
-- * 'currentManifest' - Undocumented field.
-- * 'errorCount' - Undocumented field.
-- * 'jobId' - Undocumented field.
-- * 'jobType' - Undocumented field.
-- * 'locationCode' - Undocumented field.
-- * 'locationMessage' - Undocumented field.
-- * 'logBucket' - Undocumented field.
-- * 'logKey' - Undocumented field.
-- * 'progressCode' - Undocumented field.
-- * 'progressMessage' - Undocumented field.
-- * 'responseStatus' - The response status code.
-- * 'signature' - Undocumented field.
-- * 'signatureFileContents' - Undocumented field.
-- * 'trackingNumber' - Undocumented field.
mkGetStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetStatusResponse
mkGetStatusResponse pResponseStatus_ =
  GetStatusResponse'
    { carrier = Lude.Nothing,
      trackingNumber = Lude.Nothing,
      signature = Lude.Nothing,
      jobType = Lude.Nothing,
      jobId = Lude.Nothing,
      signatureFileContents = Lude.Nothing,
      errorCount = Lude.Nothing,
      currentManifest = Lude.Nothing,
      artifactList = Lude.Nothing,
      logBucket = Lude.Nothing,
      creationDate = Lude.Nothing,
      progressCode = Lude.Nothing,
      locationCode = Lude.Nothing,
      logKey = Lude.Nothing,
      locationMessage = Lude.Nothing,
      progressMessage = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'carrier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsCarrier :: Lens.Lens' GetStatusResponse (Lude.Maybe Lude.Text)
gsrsCarrier = Lens.lens (carrier :: GetStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {carrier = a} :: GetStatusResponse)
{-# DEPRECATED gsrsCarrier "Use generic-lens or generic-optics with 'carrier' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'trackingNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsTrackingNumber :: Lens.Lens' GetStatusResponse (Lude.Maybe Lude.Text)
gsrsTrackingNumber = Lens.lens (trackingNumber :: GetStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {trackingNumber = a} :: GetStatusResponse)
{-# DEPRECATED gsrsTrackingNumber "Use generic-lens or generic-optics with 'trackingNumber' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'signature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsSignature :: Lens.Lens' GetStatusResponse (Lude.Maybe Lude.Text)
gsrsSignature = Lens.lens (signature :: GetStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {signature = a} :: GetStatusResponse)
{-# DEPRECATED gsrsSignature "Use generic-lens or generic-optics with 'signature' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'jobType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsJobType :: Lens.Lens' GetStatusResponse (Lude.Maybe JobType)
gsrsJobType = Lens.lens (jobType :: GetStatusResponse -> Lude.Maybe JobType) (\s a -> s {jobType = a} :: GetStatusResponse)
{-# DEPRECATED gsrsJobType "Use generic-lens or generic-optics with 'jobType' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsJobId :: Lens.Lens' GetStatusResponse (Lude.Maybe Lude.Text)
gsrsJobId = Lens.lens (jobId :: GetStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobId = a} :: GetStatusResponse)
{-# DEPRECATED gsrsJobId "Use generic-lens or generic-optics with 'jobId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'signatureFileContents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsSignatureFileContents :: Lens.Lens' GetStatusResponse (Lude.Maybe Lude.Text)
gsrsSignatureFileContents = Lens.lens (signatureFileContents :: GetStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {signatureFileContents = a} :: GetStatusResponse)
{-# DEPRECATED gsrsSignatureFileContents "Use generic-lens or generic-optics with 'signatureFileContents' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'errorCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsErrorCount :: Lens.Lens' GetStatusResponse (Lude.Maybe Lude.Int)
gsrsErrorCount = Lens.lens (errorCount :: GetStatusResponse -> Lude.Maybe Lude.Int) (\s a -> s {errorCount = a} :: GetStatusResponse)
{-# DEPRECATED gsrsErrorCount "Use generic-lens or generic-optics with 'errorCount' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'currentManifest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsCurrentManifest :: Lens.Lens' GetStatusResponse (Lude.Maybe Lude.Text)
gsrsCurrentManifest = Lens.lens (currentManifest :: GetStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {currentManifest = a} :: GetStatusResponse)
{-# DEPRECATED gsrsCurrentManifest "Use generic-lens or generic-optics with 'currentManifest' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'artifactList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsArtifactList :: Lens.Lens' GetStatusResponse (Lude.Maybe [Artifact])
gsrsArtifactList = Lens.lens (artifactList :: GetStatusResponse -> Lude.Maybe [Artifact]) (\s a -> s {artifactList = a} :: GetStatusResponse)
{-# DEPRECATED gsrsArtifactList "Use generic-lens or generic-optics with 'artifactList' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'logBucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsLogBucket :: Lens.Lens' GetStatusResponse (Lude.Maybe Lude.Text)
gsrsLogBucket = Lens.lens (logBucket :: GetStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {logBucket = a} :: GetStatusResponse)
{-# DEPRECATED gsrsLogBucket "Use generic-lens or generic-optics with 'logBucket' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsCreationDate :: Lens.Lens' GetStatusResponse (Lude.Maybe Lude.ISO8601)
gsrsCreationDate = Lens.lens (creationDate :: GetStatusResponse -> Lude.Maybe Lude.ISO8601) (\s a -> s {creationDate = a} :: GetStatusResponse)
{-# DEPRECATED gsrsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'progressCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsProgressCode :: Lens.Lens' GetStatusResponse (Lude.Maybe Lude.Text)
gsrsProgressCode = Lens.lens (progressCode :: GetStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {progressCode = a} :: GetStatusResponse)
{-# DEPRECATED gsrsProgressCode "Use generic-lens or generic-optics with 'progressCode' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'locationCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsLocationCode :: Lens.Lens' GetStatusResponse (Lude.Maybe Lude.Text)
gsrsLocationCode = Lens.lens (locationCode :: GetStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {locationCode = a} :: GetStatusResponse)
{-# DEPRECATED gsrsLocationCode "Use generic-lens or generic-optics with 'locationCode' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'logKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsLogKey :: Lens.Lens' GetStatusResponse (Lude.Maybe Lude.Text)
gsrsLogKey = Lens.lens (logKey :: GetStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {logKey = a} :: GetStatusResponse)
{-# DEPRECATED gsrsLogKey "Use generic-lens or generic-optics with 'logKey' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'locationMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsLocationMessage :: Lens.Lens' GetStatusResponse (Lude.Maybe Lude.Text)
gsrsLocationMessage = Lens.lens (locationMessage :: GetStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {locationMessage = a} :: GetStatusResponse)
{-# DEPRECATED gsrsLocationMessage "Use generic-lens or generic-optics with 'locationMessage' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'progressMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsProgressMessage :: Lens.Lens' GetStatusResponse (Lude.Maybe Lude.Text)
gsrsProgressMessage = Lens.lens (progressMessage :: GetStatusResponse -> Lude.Maybe Lude.Text) (\s a -> s {progressMessage = a} :: GetStatusResponse)
{-# DEPRECATED gsrsProgressMessage "Use generic-lens or generic-optics with 'progressMessage' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrsResponseStatus :: Lens.Lens' GetStatusResponse Lude.Int
gsrsResponseStatus = Lens.lens (responseStatus :: GetStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetStatusResponse)
{-# DEPRECATED gsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
