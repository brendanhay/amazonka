{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ImportExport.GetStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation returns information about a job, including where the job
-- is in the processing pipeline, the status of the results, and the
-- signature value associated with the job. You can only return information
-- about jobs you own.
module Network.AWS.ImportExport.GetStatus
  ( -- * Creating a Request
    GetStatus (..),
    newGetStatus,

    -- * Request Lenses
    getStatus_aPIVersion,
    getStatus_jobId,

    -- * Destructuring the Response
    GetStatusResponse (..),
    newGetStatusResponse,

    -- * Response Lenses
    getStatusResponse_trackingNumber,
    getStatusResponse_currentManifest,
    getStatusResponse_errorCount,
    getStatusResponse_creationDate,
    getStatusResponse_logBucket,
    getStatusResponse_jobType,
    getStatusResponse_artifactList,
    getStatusResponse_signature,
    getStatusResponse_carrier,
    getStatusResponse_progressMessage,
    getStatusResponse_locationMessage,
    getStatusResponse_logKey,
    getStatusResponse_signatureFileContents,
    getStatusResponse_progressCode,
    getStatusResponse_locationCode,
    getStatusResponse_jobId,
    getStatusResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ImportExport.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input structure for the GetStatus operation.
--
-- /See:/ 'newGetStatus' smart constructor.
data GetStatus = GetStatus'
  { aPIVersion :: Core.Maybe Core.Text,
    jobId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aPIVersion', 'getStatus_aPIVersion' - Undocumented member.
--
-- 'jobId', 'getStatus_jobId' - Undocumented member.
newGetStatus ::
  -- | 'jobId'
  Core.Text ->
  GetStatus
newGetStatus pJobId_ =
  GetStatus'
    { aPIVersion = Core.Nothing,
      jobId = pJobId_
    }

-- | Undocumented member.
getStatus_aPIVersion :: Lens.Lens' GetStatus (Core.Maybe Core.Text)
getStatus_aPIVersion = Lens.lens (\GetStatus' {aPIVersion} -> aPIVersion) (\s@GetStatus' {} a -> s {aPIVersion = a} :: GetStatus)

-- | Undocumented member.
getStatus_jobId :: Lens.Lens' GetStatus Core.Text
getStatus_jobId = Lens.lens (\GetStatus' {jobId} -> jobId) (\s@GetStatus' {} a -> s {jobId = a} :: GetStatus)

instance Core.AWSRequest GetStatus where
  type AWSResponse GetStatus = GetStatusResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetStatusResult"
      ( \s h x ->
          GetStatusResponse'
            Core.<$> (x Core..@? "TrackingNumber")
            Core.<*> (x Core..@? "CurrentManifest")
            Core.<*> (x Core..@? "ErrorCount")
            Core.<*> (x Core..@? "CreationDate")
            Core.<*> (x Core..@? "LogBucket")
            Core.<*> (x Core..@? "JobType")
            Core.<*> ( x Core..@? "ArtifactList" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (x Core..@? "Signature")
            Core.<*> (x Core..@? "Carrier")
            Core.<*> (x Core..@? "ProgressMessage")
            Core.<*> (x Core..@? "LocationMessage")
            Core.<*> (x Core..@? "LogKey")
            Core.<*> (x Core..@? "SignatureFileContents")
            Core.<*> (x Core..@? "ProgressCode")
            Core.<*> (x Core..@? "LocationCode")
            Core.<*> (x Core..@? "JobId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetStatus

instance Core.NFData GetStatus

instance Core.ToHeaders GetStatus where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetStatus where
  toPath = Core.const "/"

instance Core.ToQuery GetStatus where
  toQuery GetStatus' {..} =
    Core.mconcat
      [ "Operation=GetStatus",
        "Action" Core.=: ("GetStatus" :: Core.ByteString),
        "Version" Core.=: ("2010-06-01" :: Core.ByteString),
        "APIVersion" Core.=: aPIVersion,
        "JobId" Core.=: jobId
      ]

-- | Output structure for the GetStatus operation.
--
-- /See:/ 'newGetStatusResponse' smart constructor.
data GetStatusResponse = GetStatusResponse'
  { trackingNumber :: Core.Maybe Core.Text,
    currentManifest :: Core.Maybe Core.Text,
    errorCount :: Core.Maybe Core.Int,
    creationDate :: Core.Maybe Core.ISO8601,
    logBucket :: Core.Maybe Core.Text,
    jobType :: Core.Maybe JobType,
    artifactList :: Core.Maybe [Artifact],
    signature :: Core.Maybe Core.Text,
    carrier :: Core.Maybe Core.Text,
    progressMessage :: Core.Maybe Core.Text,
    locationMessage :: Core.Maybe Core.Text,
    logKey :: Core.Maybe Core.Text,
    signatureFileContents :: Core.Maybe Core.Text,
    progressCode :: Core.Maybe Core.Text,
    locationCode :: Core.Maybe Core.Text,
    jobId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trackingNumber', 'getStatusResponse_trackingNumber' - Undocumented member.
--
-- 'currentManifest', 'getStatusResponse_currentManifest' - Undocumented member.
--
-- 'errorCount', 'getStatusResponse_errorCount' - Undocumented member.
--
-- 'creationDate', 'getStatusResponse_creationDate' - Undocumented member.
--
-- 'logBucket', 'getStatusResponse_logBucket' - Undocumented member.
--
-- 'jobType', 'getStatusResponse_jobType' - Undocumented member.
--
-- 'artifactList', 'getStatusResponse_artifactList' - Undocumented member.
--
-- 'signature', 'getStatusResponse_signature' - Undocumented member.
--
-- 'carrier', 'getStatusResponse_carrier' - Undocumented member.
--
-- 'progressMessage', 'getStatusResponse_progressMessage' - Undocumented member.
--
-- 'locationMessage', 'getStatusResponse_locationMessage' - Undocumented member.
--
-- 'logKey', 'getStatusResponse_logKey' - Undocumented member.
--
-- 'signatureFileContents', 'getStatusResponse_signatureFileContents' - Undocumented member.
--
-- 'progressCode', 'getStatusResponse_progressCode' - Undocumented member.
--
-- 'locationCode', 'getStatusResponse_locationCode' - Undocumented member.
--
-- 'jobId', 'getStatusResponse_jobId' - Undocumented member.
--
-- 'httpStatus', 'getStatusResponse_httpStatus' - The response's http status code.
newGetStatusResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetStatusResponse
newGetStatusResponse pHttpStatus_ =
  GetStatusResponse'
    { trackingNumber = Core.Nothing,
      currentManifest = Core.Nothing,
      errorCount = Core.Nothing,
      creationDate = Core.Nothing,
      logBucket = Core.Nothing,
      jobType = Core.Nothing,
      artifactList = Core.Nothing,
      signature = Core.Nothing,
      carrier = Core.Nothing,
      progressMessage = Core.Nothing,
      locationMessage = Core.Nothing,
      logKey = Core.Nothing,
      signatureFileContents = Core.Nothing,
      progressCode = Core.Nothing,
      locationCode = Core.Nothing,
      jobId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getStatusResponse_trackingNumber :: Lens.Lens' GetStatusResponse (Core.Maybe Core.Text)
getStatusResponse_trackingNumber = Lens.lens (\GetStatusResponse' {trackingNumber} -> trackingNumber) (\s@GetStatusResponse' {} a -> s {trackingNumber = a} :: GetStatusResponse)

-- | Undocumented member.
getStatusResponse_currentManifest :: Lens.Lens' GetStatusResponse (Core.Maybe Core.Text)
getStatusResponse_currentManifest = Lens.lens (\GetStatusResponse' {currentManifest} -> currentManifest) (\s@GetStatusResponse' {} a -> s {currentManifest = a} :: GetStatusResponse)

-- | Undocumented member.
getStatusResponse_errorCount :: Lens.Lens' GetStatusResponse (Core.Maybe Core.Int)
getStatusResponse_errorCount = Lens.lens (\GetStatusResponse' {errorCount} -> errorCount) (\s@GetStatusResponse' {} a -> s {errorCount = a} :: GetStatusResponse)

-- | Undocumented member.
getStatusResponse_creationDate :: Lens.Lens' GetStatusResponse (Core.Maybe Core.UTCTime)
getStatusResponse_creationDate = Lens.lens (\GetStatusResponse' {creationDate} -> creationDate) (\s@GetStatusResponse' {} a -> s {creationDate = a} :: GetStatusResponse) Core.. Lens.mapping Core._Time

-- | Undocumented member.
getStatusResponse_logBucket :: Lens.Lens' GetStatusResponse (Core.Maybe Core.Text)
getStatusResponse_logBucket = Lens.lens (\GetStatusResponse' {logBucket} -> logBucket) (\s@GetStatusResponse' {} a -> s {logBucket = a} :: GetStatusResponse)

-- | Undocumented member.
getStatusResponse_jobType :: Lens.Lens' GetStatusResponse (Core.Maybe JobType)
getStatusResponse_jobType = Lens.lens (\GetStatusResponse' {jobType} -> jobType) (\s@GetStatusResponse' {} a -> s {jobType = a} :: GetStatusResponse)

-- | Undocumented member.
getStatusResponse_artifactList :: Lens.Lens' GetStatusResponse (Core.Maybe [Artifact])
getStatusResponse_artifactList = Lens.lens (\GetStatusResponse' {artifactList} -> artifactList) (\s@GetStatusResponse' {} a -> s {artifactList = a} :: GetStatusResponse) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
getStatusResponse_signature :: Lens.Lens' GetStatusResponse (Core.Maybe Core.Text)
getStatusResponse_signature = Lens.lens (\GetStatusResponse' {signature} -> signature) (\s@GetStatusResponse' {} a -> s {signature = a} :: GetStatusResponse)

-- | Undocumented member.
getStatusResponse_carrier :: Lens.Lens' GetStatusResponse (Core.Maybe Core.Text)
getStatusResponse_carrier = Lens.lens (\GetStatusResponse' {carrier} -> carrier) (\s@GetStatusResponse' {} a -> s {carrier = a} :: GetStatusResponse)

-- | Undocumented member.
getStatusResponse_progressMessage :: Lens.Lens' GetStatusResponse (Core.Maybe Core.Text)
getStatusResponse_progressMessage = Lens.lens (\GetStatusResponse' {progressMessage} -> progressMessage) (\s@GetStatusResponse' {} a -> s {progressMessage = a} :: GetStatusResponse)

-- | Undocumented member.
getStatusResponse_locationMessage :: Lens.Lens' GetStatusResponse (Core.Maybe Core.Text)
getStatusResponse_locationMessage = Lens.lens (\GetStatusResponse' {locationMessage} -> locationMessage) (\s@GetStatusResponse' {} a -> s {locationMessage = a} :: GetStatusResponse)

-- | Undocumented member.
getStatusResponse_logKey :: Lens.Lens' GetStatusResponse (Core.Maybe Core.Text)
getStatusResponse_logKey = Lens.lens (\GetStatusResponse' {logKey} -> logKey) (\s@GetStatusResponse' {} a -> s {logKey = a} :: GetStatusResponse)

-- | Undocumented member.
getStatusResponse_signatureFileContents :: Lens.Lens' GetStatusResponse (Core.Maybe Core.Text)
getStatusResponse_signatureFileContents = Lens.lens (\GetStatusResponse' {signatureFileContents} -> signatureFileContents) (\s@GetStatusResponse' {} a -> s {signatureFileContents = a} :: GetStatusResponse)

-- | Undocumented member.
getStatusResponse_progressCode :: Lens.Lens' GetStatusResponse (Core.Maybe Core.Text)
getStatusResponse_progressCode = Lens.lens (\GetStatusResponse' {progressCode} -> progressCode) (\s@GetStatusResponse' {} a -> s {progressCode = a} :: GetStatusResponse)

-- | Undocumented member.
getStatusResponse_locationCode :: Lens.Lens' GetStatusResponse (Core.Maybe Core.Text)
getStatusResponse_locationCode = Lens.lens (\GetStatusResponse' {locationCode} -> locationCode) (\s@GetStatusResponse' {} a -> s {locationCode = a} :: GetStatusResponse)

-- | Undocumented member.
getStatusResponse_jobId :: Lens.Lens' GetStatusResponse (Core.Maybe Core.Text)
getStatusResponse_jobId = Lens.lens (\GetStatusResponse' {jobId} -> jobId) (\s@GetStatusResponse' {} a -> s {jobId = a} :: GetStatusResponse)

-- | The response's http status code.
getStatusResponse_httpStatus :: Lens.Lens' GetStatusResponse Core.Int
getStatusResponse_httpStatus = Lens.lens (\GetStatusResponse' {httpStatus} -> httpStatus) (\s@GetStatusResponse' {} a -> s {httpStatus = a} :: GetStatusResponse)

instance Core.NFData GetStatusResponse
