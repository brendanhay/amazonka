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
    updateJobExecution_includeJobExecutionState,
    updateJobExecution_executionNumber,
    updateJobExecution_statusDetails,
    updateJobExecution_stepTimeoutInMinutes,
    updateJobExecution_includeJobDocument,
    updateJobExecution_expectedVersion,
    updateJobExecution_jobId,
    updateJobExecution_thingName,
    updateJobExecution_status,
    updateJobExecutionResponse_jobDocument,
    updateJobExecutionResponse_executionState,
    updateJobExecutionResponse_httpStatus,

    -- * Types

    -- ** JobExecution
    jobExecution_thingName,
    jobExecution_executionNumber,
    jobExecution_jobDocument,
    jobExecution_lastUpdatedAt,
    jobExecution_statusDetails,
    jobExecution_jobId,
    jobExecution_status,
    jobExecution_startedAt,
    jobExecution_versionNumber,
    jobExecution_queuedAt,
    jobExecution_approximateSecondsBeforeTimedOut,

    -- ** JobExecutionState
    jobExecutionState_statusDetails,
    jobExecutionState_status,
    jobExecutionState_versionNumber,

    -- ** JobExecutionSummary
    jobExecutionSummary_executionNumber,
    jobExecutionSummary_lastUpdatedAt,
    jobExecutionSummary_jobId,
    jobExecutionSummary_startedAt,
    jobExecutionSummary_versionNumber,
    jobExecutionSummary_queuedAt,
  )
where

import Amazonka.IoTJobsData.DescribeJobExecution
import Amazonka.IoTJobsData.GetPendingJobExecutions
import Amazonka.IoTJobsData.StartNextPendingJobExecution
import Amazonka.IoTJobsData.Types.JobExecution
import Amazonka.IoTJobsData.Types.JobExecutionState
import Amazonka.IoTJobsData.Types.JobExecutionSummary
import Amazonka.IoTJobsData.UpdateJobExecution
