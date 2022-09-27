{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EMrServerLess.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMrServerLess.Lens
  ( -- * Operations

    -- ** CancelJobRun
    cancelJobRun_applicationId,
    cancelJobRun_jobRunId,
    cancelJobRunResponse_httpStatus,
    cancelJobRunResponse_applicationId,
    cancelJobRunResponse_jobRunId,

    -- ** CreateApplication
    createApplication_tags,
    createApplication_name,
    createApplication_autoStopConfiguration,
    createApplication_initialCapacity,
    createApplication_networkConfiguration,
    createApplication_autoStartConfiguration,
    createApplication_maximumCapacity,
    createApplication_releaseLabel,
    createApplication_type,
    createApplication_clientToken,
    createApplicationResponse_name,
    createApplicationResponse_httpStatus,
    createApplicationResponse_applicationId,
    createApplicationResponse_arn,

    -- ** DeleteApplication
    deleteApplication_applicationId,
    deleteApplicationResponse_httpStatus,

    -- ** GetApplication
    getApplication_applicationId,
    getApplicationResponse_httpStatus,
    getApplicationResponse_application,

    -- ** GetJobRun
    getJobRun_applicationId,
    getJobRun_jobRunId,
    getJobRunResponse_httpStatus,
    getJobRunResponse_jobRun,

    -- ** ListApplications
    listApplications_nextToken,
    listApplications_maxResults,
    listApplications_states,
    listApplicationsResponse_nextToken,
    listApplicationsResponse_httpStatus,
    listApplicationsResponse_applications,

    -- ** ListJobRuns
    listJobRuns_nextToken,
    listJobRuns_maxResults,
    listJobRuns_createdAtAfter,
    listJobRuns_createdAtBefore,
    listJobRuns_states,
    listJobRuns_applicationId,
    listJobRunsResponse_nextToken,
    listJobRunsResponse_httpStatus,
    listJobRunsResponse_jobRuns,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** StartApplication
    startApplication_applicationId,
    startApplicationResponse_httpStatus,

    -- ** StartJobRun
    startJobRun_tags,
    startJobRun_name,
    startJobRun_jobDriver,
    startJobRun_configurationOverrides,
    startJobRun_executionTimeoutMinutes,
    startJobRun_applicationId,
    startJobRun_clientToken,
    startJobRun_executionRoleArn,
    startJobRunResponse_httpStatus,
    startJobRunResponse_applicationId,
    startJobRunResponse_jobRunId,
    startJobRunResponse_arn,

    -- ** StopApplication
    stopApplication_applicationId,
    stopApplicationResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateApplication
    updateApplication_autoStopConfiguration,
    updateApplication_initialCapacity,
    updateApplication_networkConfiguration,
    updateApplication_autoStartConfiguration,
    updateApplication_maximumCapacity,
    updateApplication_applicationId,
    updateApplication_clientToken,
    updateApplicationResponse_httpStatus,
    updateApplicationResponse_application,

    -- * Types

    -- ** Application
    application_tags,
    application_name,
    application_autoStopConfiguration,
    application_stateDetails,
    application_initialCapacity,
    application_networkConfiguration,
    application_autoStartConfiguration,
    application_maximumCapacity,
    application_applicationId,
    application_arn,
    application_releaseLabel,
    application_type,
    application_state,
    application_createdAt,
    application_updatedAt,

    -- ** ApplicationSummary
    applicationSummary_name,
    applicationSummary_stateDetails,
    applicationSummary_id,
    applicationSummary_arn,
    applicationSummary_releaseLabel,
    applicationSummary_type,
    applicationSummary_state,
    applicationSummary_createdAt,
    applicationSummary_updatedAt,

    -- ** AutoStartConfig
    autoStartConfig_enabled,

    -- ** AutoStopConfig
    autoStopConfig_idleTimeoutMinutes,
    autoStopConfig_enabled,

    -- ** Configuration
    configuration_properties,
    configuration_configurations,
    configuration_classification,

    -- ** ConfigurationOverrides
    configurationOverrides_applicationConfiguration,
    configurationOverrides_monitoringConfiguration,

    -- ** Hive
    hive_initQueryFile,
    hive_parameters,
    hive_query,

    -- ** InitialCapacityConfig
    initialCapacityConfig_workerConfiguration,
    initialCapacityConfig_workerCount,

    -- ** JobDriver
    jobDriver_hive,
    jobDriver_sparkSubmit,

    -- ** JobRun
    jobRun_tags,
    jobRun_name,
    jobRun_networkConfiguration,
    jobRun_configurationOverrides,
    jobRun_totalExecutionDurationSeconds,
    jobRun_totalResourceUtilization,
    jobRun_applicationId,
    jobRun_jobRunId,
    jobRun_arn,
    jobRun_createdBy,
    jobRun_createdAt,
    jobRun_updatedAt,
    jobRun_executionRole,
    jobRun_state,
    jobRun_stateDetails,
    jobRun_releaseLabel,
    jobRun_jobDriver,

    -- ** JobRunSummary
    jobRunSummary_name,
    jobRunSummary_type,
    jobRunSummary_applicationId,
    jobRunSummary_id,
    jobRunSummary_arn,
    jobRunSummary_createdBy,
    jobRunSummary_createdAt,
    jobRunSummary_updatedAt,
    jobRunSummary_executionRole,
    jobRunSummary_state,
    jobRunSummary_stateDetails,
    jobRunSummary_releaseLabel,

    -- ** ManagedPersistenceMonitoringConfiguration
    managedPersistenceMonitoringConfiguration_encryptionKeyArn,
    managedPersistenceMonitoringConfiguration_enabled,

    -- ** MaximumAllowedResources
    maximumAllowedResources_disk,
    maximumAllowedResources_cpu,
    maximumAllowedResources_memory,

    -- ** MonitoringConfiguration
    monitoringConfiguration_managedPersistenceMonitoringConfiguration,
    monitoringConfiguration_s3MonitoringConfiguration,

    -- ** NetworkConfiguration
    networkConfiguration_securityGroupIds,
    networkConfiguration_subnetIds,

    -- ** S3MonitoringConfiguration
    s3MonitoringConfiguration_encryptionKeyArn,
    s3MonitoringConfiguration_logUri,

    -- ** SparkSubmit
    sparkSubmit_entryPointArguments,
    sparkSubmit_sparkSubmitParameters,
    sparkSubmit_entryPoint,

    -- ** TotalResourceUtilization
    totalResourceUtilization_storageGBHour,
    totalResourceUtilization_memoryGBHour,
    totalResourceUtilization_vCPUHour,

    -- ** WorkerResourceConfig
    workerResourceConfig_disk,
    workerResourceConfig_cpu,
    workerResourceConfig_memory,
  )
where

import Amazonka.EMrServerLess.CancelJobRun
import Amazonka.EMrServerLess.CreateApplication
import Amazonka.EMrServerLess.DeleteApplication
import Amazonka.EMrServerLess.GetApplication
import Amazonka.EMrServerLess.GetJobRun
import Amazonka.EMrServerLess.ListApplications
import Amazonka.EMrServerLess.ListJobRuns
import Amazonka.EMrServerLess.ListTagsForResource
import Amazonka.EMrServerLess.StartApplication
import Amazonka.EMrServerLess.StartJobRun
import Amazonka.EMrServerLess.StopApplication
import Amazonka.EMrServerLess.TagResource
import Amazonka.EMrServerLess.Types.Application
import Amazonka.EMrServerLess.Types.ApplicationSummary
import Amazonka.EMrServerLess.Types.AutoStartConfig
import Amazonka.EMrServerLess.Types.AutoStopConfig
import Amazonka.EMrServerLess.Types.Configuration
import Amazonka.EMrServerLess.Types.ConfigurationOverrides
import Amazonka.EMrServerLess.Types.Hive
import Amazonka.EMrServerLess.Types.InitialCapacityConfig
import Amazonka.EMrServerLess.Types.JobDriver
import Amazonka.EMrServerLess.Types.JobRun
import Amazonka.EMrServerLess.Types.JobRunSummary
import Amazonka.EMrServerLess.Types.ManagedPersistenceMonitoringConfiguration
import Amazonka.EMrServerLess.Types.MaximumAllowedResources
import Amazonka.EMrServerLess.Types.MonitoringConfiguration
import Amazonka.EMrServerLess.Types.NetworkConfiguration
import Amazonka.EMrServerLess.Types.S3MonitoringConfiguration
import Amazonka.EMrServerLess.Types.SparkSubmit
import Amazonka.EMrServerLess.Types.TotalResourceUtilization
import Amazonka.EMrServerLess.Types.WorkerResourceConfig
import Amazonka.EMrServerLess.UntagResource
import Amazonka.EMrServerLess.UpdateApplication
