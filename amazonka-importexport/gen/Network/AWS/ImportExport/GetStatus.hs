{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.ImportExport.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input structure for the GetStatus operation.
--
-- /See:/ 'newGetStatus' smart constructor.
data GetStatus = GetStatus'
  { aPIVersion :: Prelude.Maybe Prelude.Text,
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  GetStatus
newGetStatus pJobId_ =
  GetStatus'
    { aPIVersion = Prelude.Nothing,
      jobId = pJobId_
    }

-- | Undocumented member.
getStatus_aPIVersion :: Lens.Lens' GetStatus (Prelude.Maybe Prelude.Text)
getStatus_aPIVersion = Lens.lens (\GetStatus' {aPIVersion} -> aPIVersion) (\s@GetStatus' {} a -> s {aPIVersion = a} :: GetStatus)

-- | Undocumented member.
getStatus_jobId :: Lens.Lens' GetStatus Prelude.Text
getStatus_jobId = Lens.lens (\GetStatus' {jobId} -> jobId) (\s@GetStatus' {} a -> s {jobId = a} :: GetStatus)

instance Prelude.AWSRequest GetStatus where
  type Rs GetStatus = GetStatusResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetStatusResult"
      ( \s h x ->
          GetStatusResponse'
            Prelude.<$> (x Prelude..@? "TrackingNumber")
            Prelude.<*> (x Prelude..@? "CurrentManifest")
            Prelude.<*> (x Prelude..@? "ErrorCount")
            Prelude.<*> (x Prelude..@? "CreationDate")
            Prelude.<*> (x Prelude..@? "LogBucket")
            Prelude.<*> (x Prelude..@? "JobType")
            Prelude.<*> ( x Prelude..@? "ArtifactList"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                        )
            Prelude.<*> (x Prelude..@? "Signature")
            Prelude.<*> (x Prelude..@? "Carrier")
            Prelude.<*> (x Prelude..@? "ProgressMessage")
            Prelude.<*> (x Prelude..@? "LocationMessage")
            Prelude.<*> (x Prelude..@? "LogKey")
            Prelude.<*> (x Prelude..@? "SignatureFileContents")
            Prelude.<*> (x Prelude..@? "ProgressCode")
            Prelude.<*> (x Prelude..@? "LocationCode")
            Prelude.<*> (x Prelude..@? "JobId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetStatus

instance Prelude.NFData GetStatus

instance Prelude.ToHeaders GetStatus where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath GetStatus where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetStatus where
  toQuery GetStatus' {..} =
    Prelude.mconcat
      [ "Operation=GetStatus",
        "Action"
          Prelude.=: ("GetStatus" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-06-01" :: Prelude.ByteString),
        "APIVersion" Prelude.=: aPIVersion,
        "JobId" Prelude.=: jobId
      ]

-- | Output structure for the GetStatus operation.
--
-- /See:/ 'newGetStatusResponse' smart constructor.
data GetStatusResponse = GetStatusResponse'
  { trackingNumber :: Prelude.Maybe Prelude.Text,
    currentManifest :: Prelude.Maybe Prelude.Text,
    errorCount :: Prelude.Maybe Prelude.Int,
    creationDate :: Prelude.Maybe Prelude.ISO8601,
    logBucket :: Prelude.Maybe Prelude.Text,
    jobType :: Prelude.Maybe JobType,
    artifactList :: Prelude.Maybe [Artifact],
    signature :: Prelude.Maybe Prelude.Text,
    carrier :: Prelude.Maybe Prelude.Text,
    progressMessage :: Prelude.Maybe Prelude.Text,
    locationMessage :: Prelude.Maybe Prelude.Text,
    logKey :: Prelude.Maybe Prelude.Text,
    signatureFileContents :: Prelude.Maybe Prelude.Text,
    progressCode :: Prelude.Maybe Prelude.Text,
    locationCode :: Prelude.Maybe Prelude.Text,
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetStatusResponse
newGetStatusResponse pHttpStatus_ =
  GetStatusResponse'
    { trackingNumber =
        Prelude.Nothing,
      currentManifest = Prelude.Nothing,
      errorCount = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      logBucket = Prelude.Nothing,
      jobType = Prelude.Nothing,
      artifactList = Prelude.Nothing,
      signature = Prelude.Nothing,
      carrier = Prelude.Nothing,
      progressMessage = Prelude.Nothing,
      locationMessage = Prelude.Nothing,
      logKey = Prelude.Nothing,
      signatureFileContents = Prelude.Nothing,
      progressCode = Prelude.Nothing,
      locationCode = Prelude.Nothing,
      jobId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getStatusResponse_trackingNumber :: Lens.Lens' GetStatusResponse (Prelude.Maybe Prelude.Text)
getStatusResponse_trackingNumber = Lens.lens (\GetStatusResponse' {trackingNumber} -> trackingNumber) (\s@GetStatusResponse' {} a -> s {trackingNumber = a} :: GetStatusResponse)

-- | Undocumented member.
getStatusResponse_currentManifest :: Lens.Lens' GetStatusResponse (Prelude.Maybe Prelude.Text)
getStatusResponse_currentManifest = Lens.lens (\GetStatusResponse' {currentManifest} -> currentManifest) (\s@GetStatusResponse' {} a -> s {currentManifest = a} :: GetStatusResponse)

-- | Undocumented member.
getStatusResponse_errorCount :: Lens.Lens' GetStatusResponse (Prelude.Maybe Prelude.Int)
getStatusResponse_errorCount = Lens.lens (\GetStatusResponse' {errorCount} -> errorCount) (\s@GetStatusResponse' {} a -> s {errorCount = a} :: GetStatusResponse)

-- | Undocumented member.
getStatusResponse_creationDate :: Lens.Lens' GetStatusResponse (Prelude.Maybe Prelude.UTCTime)
getStatusResponse_creationDate = Lens.lens (\GetStatusResponse' {creationDate} -> creationDate) (\s@GetStatusResponse' {} a -> s {creationDate = a} :: GetStatusResponse) Prelude.. Lens.mapping Prelude._Time

-- | Undocumented member.
getStatusResponse_logBucket :: Lens.Lens' GetStatusResponse (Prelude.Maybe Prelude.Text)
getStatusResponse_logBucket = Lens.lens (\GetStatusResponse' {logBucket} -> logBucket) (\s@GetStatusResponse' {} a -> s {logBucket = a} :: GetStatusResponse)

-- | Undocumented member.
getStatusResponse_jobType :: Lens.Lens' GetStatusResponse (Prelude.Maybe JobType)
getStatusResponse_jobType = Lens.lens (\GetStatusResponse' {jobType} -> jobType) (\s@GetStatusResponse' {} a -> s {jobType = a} :: GetStatusResponse)

-- | Undocumented member.
getStatusResponse_artifactList :: Lens.Lens' GetStatusResponse (Prelude.Maybe [Artifact])
getStatusResponse_artifactList = Lens.lens (\GetStatusResponse' {artifactList} -> artifactList) (\s@GetStatusResponse' {} a -> s {artifactList = a} :: GetStatusResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | Undocumented member.
getStatusResponse_signature :: Lens.Lens' GetStatusResponse (Prelude.Maybe Prelude.Text)
getStatusResponse_signature = Lens.lens (\GetStatusResponse' {signature} -> signature) (\s@GetStatusResponse' {} a -> s {signature = a} :: GetStatusResponse)

-- | Undocumented member.
getStatusResponse_carrier :: Lens.Lens' GetStatusResponse (Prelude.Maybe Prelude.Text)
getStatusResponse_carrier = Lens.lens (\GetStatusResponse' {carrier} -> carrier) (\s@GetStatusResponse' {} a -> s {carrier = a} :: GetStatusResponse)

-- | Undocumented member.
getStatusResponse_progressMessage :: Lens.Lens' GetStatusResponse (Prelude.Maybe Prelude.Text)
getStatusResponse_progressMessage = Lens.lens (\GetStatusResponse' {progressMessage} -> progressMessage) (\s@GetStatusResponse' {} a -> s {progressMessage = a} :: GetStatusResponse)

-- | Undocumented member.
getStatusResponse_locationMessage :: Lens.Lens' GetStatusResponse (Prelude.Maybe Prelude.Text)
getStatusResponse_locationMessage = Lens.lens (\GetStatusResponse' {locationMessage} -> locationMessage) (\s@GetStatusResponse' {} a -> s {locationMessage = a} :: GetStatusResponse)

-- | Undocumented member.
getStatusResponse_logKey :: Lens.Lens' GetStatusResponse (Prelude.Maybe Prelude.Text)
getStatusResponse_logKey = Lens.lens (\GetStatusResponse' {logKey} -> logKey) (\s@GetStatusResponse' {} a -> s {logKey = a} :: GetStatusResponse)

-- | Undocumented member.
getStatusResponse_signatureFileContents :: Lens.Lens' GetStatusResponse (Prelude.Maybe Prelude.Text)
getStatusResponse_signatureFileContents = Lens.lens (\GetStatusResponse' {signatureFileContents} -> signatureFileContents) (\s@GetStatusResponse' {} a -> s {signatureFileContents = a} :: GetStatusResponse)

-- | Undocumented member.
getStatusResponse_progressCode :: Lens.Lens' GetStatusResponse (Prelude.Maybe Prelude.Text)
getStatusResponse_progressCode = Lens.lens (\GetStatusResponse' {progressCode} -> progressCode) (\s@GetStatusResponse' {} a -> s {progressCode = a} :: GetStatusResponse)

-- | Undocumented member.
getStatusResponse_locationCode :: Lens.Lens' GetStatusResponse (Prelude.Maybe Prelude.Text)
getStatusResponse_locationCode = Lens.lens (\GetStatusResponse' {locationCode} -> locationCode) (\s@GetStatusResponse' {} a -> s {locationCode = a} :: GetStatusResponse)

-- | Undocumented member.
getStatusResponse_jobId :: Lens.Lens' GetStatusResponse (Prelude.Maybe Prelude.Text)
getStatusResponse_jobId = Lens.lens (\GetStatusResponse' {jobId} -> jobId) (\s@GetStatusResponse' {} a -> s {jobId = a} :: GetStatusResponse)

-- | The response's http status code.
getStatusResponse_httpStatus :: Lens.Lens' GetStatusResponse Prelude.Int
getStatusResponse_httpStatus = Lens.lens (\GetStatusResponse' {httpStatus} -> httpStatus) (\s@GetStatusResponse' {} a -> s {httpStatus = a} :: GetStatusResponse)

instance Prelude.NFData GetStatusResponse
