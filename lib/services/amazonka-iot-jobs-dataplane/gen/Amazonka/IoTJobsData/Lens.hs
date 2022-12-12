{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTJobsData.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTJobsData.Lens
  ( -- * Operations

    -- ** DescribeJobExecution
    describeJobExecution_executionNumber,
    describeJobExecution_includeJobDocument,
    describeJobExecution_jobId,
    describeJobExecution_thingName,
    describeJobExecutionResponse_execution,
    describeJobExecutionResponse_httpStatus,

    -- ** GetPendingJobExecutions
    getPendingJobExecutions_thingName,
    getPendingJobExecutionsResponse_inProgressJobs,
    getPendingJobExecutionsResponse_queuedJobs,
    getPendingJobExecutionsResponse_httpStatus,

    -- ** StartNextPendingJobExecution
    startNextPendingJobExecution_statusDetails,
    startNextPendingJobExecution_stepTimeoutInMinutes,
    startNextPendingJobExecution_thingName,
    startNextPendingJobExecutionResponse_execution,
    startNextPendingJobExecutionResponse_httpStatus,

    -- ** UpdateJobExecution
    updateJobExecution_executionNumber,
    updateJobExecution_expectedVersion,
    updateJobExecution_includeJobDocument,
    updateJobExecution_includeJobExecutionState,
    updateJobExecution_statusDetails,
    updateJobExecution_stepTimeoutInMinutes,
    updateJobExecution_jobId,
    updateJobExecution_thingName,
    updateJobExecution_status,
    updateJobExecutionResponse_executionState,
    updateJobExecutionResponse_jobDocument,
    updateJobExecutionResponse_httpStatus,

    -- * Types

    -- ** JobExecution
    jobExecution_approximateSecondsBeforeTimedOut,
    jobExecution_executionNumber,
    jobExecution_jobDocument,
    jobExecution_jobId,
    jobExecution_lastUpdatedAt,
    jobExecution_queuedAt,
    jobExecution_startedAt,
    jobExecution_status,
    jobExecution_statusDetails,
    jobExecution_thingName,
    jobExecution_versionNumber,

    -- ** JobExecutionState
    jobExecutionState_status,
    jobExecutionState_statusDetails,
    jobExecutionState_versionNumber,

    -- ** JobExecutionSummary
    jobExecutionSummary_executionNumber,
    jobExecutionSummary_jobId,
    jobExecutionSummary_lastUpdatedAt,
    jobExecutionSummary_queuedAt,
    jobExecutionSummary_startedAt,
    jobExecutionSummary_versionNumber,
  )
where

import Amazonka.IoTJobsData.DescribeJobExecution
import Amazonka.IoTJobsData.GetPendingJobExecutions
import Amazonka.IoTJobsData.StartNextPendingJobExecution
import Amazonka.IoTJobsData.Types.JobExecution
import Amazonka.IoTJobsData.Types.JobExecutionState
import Amazonka.IoTJobsData.Types.JobExecutionSummary
import Amazonka.IoTJobsData.UpdateJobExecution
