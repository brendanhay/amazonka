{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ImportExport.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImportExport.Lens
  ( -- * Operations

    -- ** CancelJob
    cancelJob_aPIVersion,
    cancelJob_jobId,
    cancelJobResponse_success,
    cancelJobResponse_httpStatus,

    -- ** CreateJob
    createJob_aPIVersion,
    createJob_manifestAddendum,
    createJob_jobType,
    createJob_manifest,
    createJob_validateOnly,
    createJobResponse_artifactList,
    createJobResponse_jobId,
    createJobResponse_jobType,
    createJobResponse_signature,
    createJobResponse_signatureFileContents,
    createJobResponse_warningMessage,
    createJobResponse_httpStatus,

    -- ** GetShippingLabel
    getShippingLabel_aPIVersion,
    getShippingLabel_city,
    getShippingLabel_company,
    getShippingLabel_country,
    getShippingLabel_name,
    getShippingLabel_phoneNumber,
    getShippingLabel_postalCode,
    getShippingLabel_stateOrProvince,
    getShippingLabel_street1,
    getShippingLabel_street2,
    getShippingLabel_street3,
    getShippingLabel_jobIds,
    getShippingLabelResponse_shippingLabelURL,
    getShippingLabelResponse_warning,
    getShippingLabelResponse_httpStatus,

    -- ** GetStatus
    getStatus_aPIVersion,
    getStatus_jobId,
    getStatusResponse_artifactList,
    getStatusResponse_carrier,
    getStatusResponse_creationDate,
    getStatusResponse_currentManifest,
    getStatusResponse_errorCount,
    getStatusResponse_jobId,
    getStatusResponse_jobType,
    getStatusResponse_locationCode,
    getStatusResponse_locationMessage,
    getStatusResponse_logBucket,
    getStatusResponse_logKey,
    getStatusResponse_progressCode,
    getStatusResponse_progressMessage,
    getStatusResponse_signature,
    getStatusResponse_signatureFileContents,
    getStatusResponse_trackingNumber,
    getStatusResponse_httpStatus,

    -- ** ListJobs
    listJobs_aPIVersion,
    listJobs_marker,
    listJobs_maxJobs,
    listJobsResponse_isTruncated,
    listJobsResponse_jobs,
    listJobsResponse_httpStatus,

    -- ** UpdateJob
    updateJob_aPIVersion,
    updateJob_jobId,
    updateJob_manifest,
    updateJob_jobType,
    updateJob_validateOnly,
    updateJobResponse_artifactList,
    updateJobResponse_success,
    updateJobResponse_warningMessage,
    updateJobResponse_httpStatus,

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

import Amazonka.ImportExport.CancelJob
import Amazonka.ImportExport.CreateJob
import Amazonka.ImportExport.GetShippingLabel
import Amazonka.ImportExport.GetStatus
import Amazonka.ImportExport.ListJobs
import Amazonka.ImportExport.Types.Artifact
import Amazonka.ImportExport.Types.Job
import Amazonka.ImportExport.UpdateJob
