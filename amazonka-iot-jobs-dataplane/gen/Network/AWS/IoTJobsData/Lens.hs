{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTJobsData.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTJobsData.Lens
  ( -- * Operations

    -- ** GetPendingJobExecutions
    getPendingJobExecutions_thingName,
    getPendingJobExecutionsResponse_inProgressJobs,
    getPendingJobExecutionsResponse_queuedJobs,
    getPendingJobExecutionsResponse_httpStatus,

    -- ** UpdateJobExecution
    updateJobExecution_expectedVersion,
    updateJobExecution_includeJobDocument,
    updateJobExecution_includeJobExecutionState,
    updateJobExecution_statusDetails,
    updateJobExecution_stepTimeoutInMinutes,
    updateJobExecution_executionNumber,
    updateJobExecution_jobId,
    updateJobExecution_thingName,
    updateJobExecution_status,
    updateJobExecutionResponse_executionState,
    updateJobExecutionResponse_jobDocument,
    updateJobExecutionResponse_httpStatus,

    -- ** DescribeJobExecution
    describeJobExecution_includeJobDocument,
    describeJobExecution_executionNumber,
    describeJobExecution_jobId,
    describeJobExecution_thingName,
    describeJobExecutionResponse_execution,
    describeJobExecutionResponse_httpStatus,

    -- ** StartNextPendingJobExecution
    startNextPendingJobExecution_statusDetails,
    startNextPendingJobExecution_stepTimeoutInMinutes,
    startNextPendingJobExecution_thingName,
    startNextPendingJobExecutionResponse_execution,
    startNextPendingJobExecutionResponse_httpStatus,

    -- * Types

    -- ** JobExecution
    jobExecution_startedAt,
    jobExecution_status,
    jobExecution_statusDetails,
    jobExecution_thingName,
    jobExecution_queuedAt,
    jobExecution_versionNumber,
    jobExecution_executionNumber,
    jobExecution_jobDocument,
    jobExecution_approximateSecondsBeforeTimedOut,
    jobExecution_lastUpdatedAt,
    jobExecution_jobId,

    -- ** JobExecutionState
    jobExecutionState_status,
    jobExecutionState_statusDetails,
    jobExecutionState_versionNumber,

    -- ** JobExecutionSummary
    jobExecutionSummary_startedAt,
    jobExecutionSummary_queuedAt,
    jobExecutionSummary_versionNumber,
    jobExecutionSummary_executionNumber,
    jobExecutionSummary_lastUpdatedAt,
    jobExecutionSummary_jobId,
  )
where

import Network.AWS.IoTJobsData.DescribeJobExecution
import Network.AWS.IoTJobsData.GetPendingJobExecutions
import Network.AWS.IoTJobsData.StartNextPendingJobExecution
import Network.AWS.IoTJobsData.Types.JobExecution
import Network.AWS.IoTJobsData.Types.JobExecutionState
import Network.AWS.IoTJobsData.Types.JobExecutionSummary
import Network.AWS.IoTJobsData.UpdateJobExecution
