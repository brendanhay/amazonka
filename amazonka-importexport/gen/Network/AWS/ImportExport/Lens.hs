{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ImportExport.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ImportExport.Lens
  ( -- * Operations

    -- ** CancelJob
    cancelJob_aPIVersion,
    cancelJob_jobId,
    cancelJobResponse_success,
    cancelJobResponse_httpStatus,

    -- ** UpdateJob
    updateJob_aPIVersion,
    updateJob_jobId,
    updateJob_manifest,
    updateJob_jobType,
    updateJob_validateOnly,
    updateJobResponse_warningMessage,
    updateJobResponse_artifactList,
    updateJobResponse_success,
    updateJobResponse_httpStatus,

    -- ** GetStatus
    getStatus_aPIVersion,
    getStatus_jobId,
    getStatusResponse_trackingNumber,
    getStatusResponse_currentManifest,
    getStatusResponse_errorCount,
    getStatusResponse_creationDate,
    getStatusResponse_logBucket,
    getStatusResponse_jobType,
    getStatusResponse_signature,
    getStatusResponse_carrier,
    getStatusResponse_artifactList,
    getStatusResponse_progressMessage,
    getStatusResponse_locationMessage,
    getStatusResponse_logKey,
    getStatusResponse_signatureFileContents,
    getStatusResponse_locationCode,
    getStatusResponse_progressCode,
    getStatusResponse_jobId,
    getStatusResponse_httpStatus,

    -- ** GetShippingLabel
    getShippingLabel_company,
    getShippingLabel_phoneNumber,
    getShippingLabel_postalCode,
    getShippingLabel_street1,
    getShippingLabel_aPIVersion,
    getShippingLabel_city,
    getShippingLabel_name,
    getShippingLabel_street2,
    getShippingLabel_country,
    getShippingLabel_stateOrProvince,
    getShippingLabel_street3,
    getShippingLabel_jobIds,
    getShippingLabelResponse_warning,
    getShippingLabelResponse_shippingLabelURL,
    getShippingLabelResponse_httpStatus,

    -- ** ListJobs
    listJobs_maxJobs,
    listJobs_aPIVersion,
    listJobs_marker,
    listJobsResponse_isTruncated,
    listJobsResponse_jobs,
    listJobsResponse_httpStatus,

    -- ** CreateJob
    createJob_aPIVersion,
    createJob_manifestAddendum,
    createJob_jobType,
    createJob_manifest,
    createJob_validateOnly,
    createJobResponse_warningMessage,
    createJobResponse_jobType,
    createJobResponse_signature,
    createJobResponse_artifactList,
    createJobResponse_signatureFileContents,
    createJobResponse_jobId,
    createJobResponse_httpStatus,

    -- * Types

    -- ** Artifact
    artifact_description,
    artifact_url,

    -- ** Job
    job_jobType,
    job_jobId,
    job_isCanceled,
    job_creationDate,
  )
where

import Network.AWS.ImportExport.CancelJob
import Network.AWS.ImportExport.CreateJob
import Network.AWS.ImportExport.GetShippingLabel
import Network.AWS.ImportExport.GetStatus
import Network.AWS.ImportExport.ListJobs
import Network.AWS.ImportExport.Types.Artifact
import Network.AWS.ImportExport.Types.Job
import Network.AWS.ImportExport.UpdateJob
