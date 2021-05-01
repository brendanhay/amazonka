{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTJobsData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS IoT Jobs is a service that allows you to define a set of jobs —
-- remote operations that are sent to and executed on one or more devices
-- connected to AWS IoT. For example, you can define a job that instructs a
-- set of devices to download and install application or firmware updates,
-- reboot, rotate certificates, or perform remote troubleshooting
-- operations.
--
-- To create a job, you make a job document which is a description of the
-- remote operations to be performed, and you specify a list of targets
-- that should perform the operations. The targets can be individual
-- things, thing groups or both.
--
-- AWS IoT Jobs sends a message to inform the targets that a job is
-- available. The target starts the execution of the job by downloading the
-- job document, performing the operations it specifies, and reporting its
-- progress to AWS IoT. The Jobs service provides commands to track the
-- progress of a job on a specific target and for all the targets of the
-- job
module Network.AWS.IoTJobsData
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** TerminalStateException
    _TerminalStateException,

    -- ** CertificateValidationException
    _CertificateValidationException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** InvalidRequestException
    _InvalidRequestException,

    -- ** InvalidStateTransitionException
    _InvalidStateTransitionException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetPendingJobExecutions
    GetPendingJobExecutions (GetPendingJobExecutions'),
    newGetPendingJobExecutions,
    GetPendingJobExecutionsResponse (GetPendingJobExecutionsResponse'),
    newGetPendingJobExecutionsResponse,

    -- ** UpdateJobExecution
    UpdateJobExecution (UpdateJobExecution'),
    newUpdateJobExecution,
    UpdateJobExecutionResponse (UpdateJobExecutionResponse'),
    newUpdateJobExecutionResponse,

    -- ** DescribeJobExecution
    DescribeJobExecution (DescribeJobExecution'),
    newDescribeJobExecution,
    DescribeJobExecutionResponse (DescribeJobExecutionResponse'),
    newDescribeJobExecutionResponse,

    -- ** StartNextPendingJobExecution
    StartNextPendingJobExecution (StartNextPendingJobExecution'),
    newStartNextPendingJobExecution,
    StartNextPendingJobExecutionResponse (StartNextPendingJobExecutionResponse'),
    newStartNextPendingJobExecutionResponse,

    -- * Types

    -- ** JobExecutionStatus
    JobExecutionStatus (..),

    -- ** JobExecution
    JobExecution (JobExecution'),
    newJobExecution,

    -- ** JobExecutionState
    JobExecutionState (JobExecutionState'),
    newJobExecutionState,

    -- ** JobExecutionSummary
    JobExecutionSummary (JobExecutionSummary'),
    newJobExecutionSummary,
  )
where

import Network.AWS.IoTJobsData.DescribeJobExecution
import Network.AWS.IoTJobsData.GetPendingJobExecutions
import Network.AWS.IoTJobsData.Lens
import Network.AWS.IoTJobsData.StartNextPendingJobExecution
import Network.AWS.IoTJobsData.Types
import Network.AWS.IoTJobsData.UpdateJobExecution
import Network.AWS.IoTJobsData.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'IoTJobsData'.

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
