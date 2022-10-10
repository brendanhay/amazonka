{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.ImportExport
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2010-06-01@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS Import\/Export Service AWS Import\/Export accelerates transferring
-- large amounts of data between the AWS cloud and portable storage devices
-- that you mail to us. AWS Import\/Export transfers data directly onto and
-- off of your storage devices using Amazon\'s high-speed internal network
-- and bypassing the Internet. For large data sets, AWS Import\/Export is
-- often faster than Internet transfer and more cost effective than
-- upgrading your connectivity.
module Amazonka.ImportExport
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidAddressException
    _InvalidAddressException,

    -- ** MissingManifestFieldException
    _MissingManifestFieldException,

    -- ** BucketPermissionException
    _BucketPermissionException,

    -- ** MissingParameterException
    _MissingParameterException,

    -- ** UnableToCancelJobIdException
    _UnableToCancelJobIdException,

    -- ** MissingCustomsException
    _MissingCustomsException,

    -- ** InvalidAccessKeyIdException
    _InvalidAccessKeyIdException,

    -- ** InvalidVersionException
    _InvalidVersionException,

    -- ** MalformedManifestException
    _MalformedManifestException,

    -- ** UnableToUpdateJobIdException
    _UnableToUpdateJobIdException,

    -- ** InvalidManifestFieldException
    _InvalidManifestFieldException,

    -- ** NoSuchBucketException
    _NoSuchBucketException,

    -- ** CanceledJobIdException
    _CanceledJobIdException,

    -- ** InvalidJobIdException
    _InvalidJobIdException,

    -- ** InvalidCustomsException
    _InvalidCustomsException,

    -- ** CreateJobQuotaExceededException
    _CreateJobQuotaExceededException,

    -- ** MultipleRegionsException
    _MultipleRegionsException,

    -- ** ExpiredJobIdException
    _ExpiredJobIdException,

    -- ** InvalidFileSystemException
    _InvalidFileSystemException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** CancelJob
    CancelJob (CancelJob'),
    newCancelJob,
    CancelJobResponse (CancelJobResponse'),
    newCancelJobResponse,

    -- ** CreateJob
    CreateJob (CreateJob'),
    newCreateJob,
    CreateJobResponse (CreateJobResponse'),
    newCreateJobResponse,

    -- ** GetShippingLabel
    GetShippingLabel (GetShippingLabel'),
    newGetShippingLabel,
    GetShippingLabelResponse (GetShippingLabelResponse'),
    newGetShippingLabelResponse,

    -- ** GetStatus
    GetStatus (GetStatus'),
    newGetStatus,
    GetStatusResponse (GetStatusResponse'),
    newGetStatusResponse,

    -- ** ListJobs (Paginated)
    ListJobs (ListJobs'),
    newListJobs,
    ListJobsResponse (ListJobsResponse'),
    newListJobsResponse,

    -- ** UpdateJob
    UpdateJob (UpdateJob'),
    newUpdateJob,
    UpdateJobResponse (UpdateJobResponse'),
    newUpdateJobResponse,

    -- * Types

    -- ** JobType
    JobType (..),

    -- ** Artifact
    Artifact (Artifact'),
    newArtifact,

    -- ** Job
    Job (Job'),
    newJob,
  )
where

import Amazonka.ImportExport.CancelJob
import Amazonka.ImportExport.CreateJob
import Amazonka.ImportExport.GetShippingLabel
import Amazonka.ImportExport.GetStatus
import Amazonka.ImportExport.Lens
import Amazonka.ImportExport.ListJobs
import Amazonka.ImportExport.Types
import Amazonka.ImportExport.UpdateJob
import Amazonka.ImportExport.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ImportExport'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
