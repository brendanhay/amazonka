{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EMRServerless.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRServerless.Lens
  ( -- * Operations

    -- ** CancelJobRun
    cancelJobRun_applicationId,
    cancelJobRun_jobRunId,
    cancelJobRunResponse_httpStatus,
    cancelJobRunResponse_applicationId,
    cancelJobRunResponse_jobRunId,

    -- ** CreateApplication
    createApplication_architecture,
    createApplication_autoStartConfiguration,
    createApplication_autoStopConfiguration,
    createApplication_imageConfiguration,
    createApplication_initialCapacity,
    createApplication_maximumCapacity,
    createApplication_name,
    createApplication_networkConfiguration,
    createApplication_tags,
    createApplication_workerTypeSpecifications,
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

    -- ** GetDashboardForJobRun
    getDashboardForJobRun_applicationId,
    getDashboardForJobRun_jobRunId,
    getDashboardForJobRunResponse_url,
    getDashboardForJobRunResponse_httpStatus,

    -- ** GetJobRun
    getJobRun_applicationId,
    getJobRun_jobRunId,
    getJobRunResponse_httpStatus,
    getJobRunResponse_jobRun,

    -- ** ListApplications
    listApplications_maxResults,
    listApplications_nextToken,
    listApplications_states,
    listApplicationsResponse_nextToken,
    listApplicationsResponse_httpStatus,
    listApplicationsResponse_applications,

    -- ** ListJobRuns
    listJobRuns_createdAtAfter,
    listJobRuns_createdAtBefore,
    listJobRuns_maxResults,
    listJobRuns_nextToken,
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
    startJobRun_configurationOverrides,
    startJobRun_executionTimeoutMinutes,
    startJobRun_jobDriver,
    startJobRun_name,
    startJobRun_tags,
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
    updateApplication_architecture,
    updateApplication_autoStartConfiguration,
    updateApplication_autoStopConfiguration,
    updateApplication_imageConfiguration,
    updateApplication_initialCapacity,
    updateApplication_maximumCapacity,
    updateApplication_networkConfiguration,
    updateApplication_workerTypeSpecifications,
    updateApplication_applicationId,
    updateApplication_clientToken,
    updateApplicationResponse_httpStatus,
    updateApplicationResponse_application,

    -- * Types

    -- ** Application
    application_architecture,
    application_autoStartConfiguration,
    application_autoStopConfiguration,
    application_imageConfiguration,
    application_initialCapacity,
    application_maximumCapacity,
    application_name,
    application_networkConfiguration,
    application_stateDetails,
    application_tags,
    application_workerTypeSpecifications,
    application_applicationId,
    application_arn,
    application_releaseLabel,
    application_type,
    application_state,
    application_createdAt,
    application_updatedAt,

    -- ** ApplicationSummary
    applicationSummary_architecture,
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
    autoStopConfig_enabled,
    autoStopConfig_idleTimeoutMinutes,

    -- ** Configuration
    configuration_configurations,
    configuration_properties,
    configuration_classification,

    -- ** ConfigurationOverrides
    configurationOverrides_applicationConfiguration,
    configurationOverrides_monitoringConfiguration,

    -- ** Hive
    hive_initQueryFile,
    hive_parameters,
    hive_query,

    -- ** ImageConfiguration
    imageConfiguration_resolvedImageDigest,
    imageConfiguration_imageUri,

    -- ** ImageConfigurationInput
    imageConfigurationInput_imageUri,

    -- ** InitialCapacityConfig
    initialCapacityConfig_workerConfiguration,
    initialCapacityConfig_workerCount,

    -- ** JobDriver
    jobDriver_hive,
    jobDriver_sparkSubmit,

    -- ** JobRun
    jobRun_configurationOverrides,
    jobRun_name,
    jobRun_networkConfiguration,
    jobRun_tags,
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
    managedPersistenceMonitoringConfiguration_enabled,
    managedPersistenceMonitoringConfiguration_encryptionKeyArn,

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
    totalResourceUtilization_memoryGBHour,
    totalResourceUtilization_storageGBHour,
    totalResourceUtilization_vCPUHour,

    -- ** WorkerResourceConfig
    workerResourceConfig_disk,
    workerResourceConfig_cpu,
    workerResourceConfig_memory,

    -- ** WorkerTypeSpecification
    workerTypeSpecification_imageConfiguration,

    -- ** WorkerTypeSpecificationInput
    workerTypeSpecificationInput_imageConfiguration,
  )
where

import Amazonka.EMRServerless.CancelJobRun
import Amazonka.EMRServerless.CreateApplication
import Amazonka.EMRServerless.DeleteApplication
import Amazonka.EMRServerless.GetApplication
import Amazonka.EMRServerless.GetDashboardForJobRun
import Amazonka.EMRServerless.GetJobRun
import Amazonka.EMRServerless.ListApplications
import Amazonka.EMRServerless.ListJobRuns
import Amazonka.EMRServerless.ListTagsForResource
import Amazonka.EMRServerless.StartApplication
import Amazonka.EMRServerless.StartJobRun
import Amazonka.EMRServerless.StopApplication
import Amazonka.EMRServerless.TagResource
import Amazonka.EMRServerless.Types.Application
import Amazonka.EMRServerless.Types.ApplicationSummary
import Amazonka.EMRServerless.Types.AutoStartConfig
import Amazonka.EMRServerless.Types.AutoStopConfig
import Amazonka.EMRServerless.Types.Configuration
import Amazonka.EMRServerless.Types.ConfigurationOverrides
import Amazonka.EMRServerless.Types.Hive
import Amazonka.EMRServerless.Types.ImageConfiguration
import Amazonka.EMRServerless.Types.ImageConfigurationInput
import Amazonka.EMRServerless.Types.InitialCapacityConfig
import Amazonka.EMRServerless.Types.JobDriver
import Amazonka.EMRServerless.Types.JobRun
import Amazonka.EMRServerless.Types.JobRunSummary
import Amazonka.EMRServerless.Types.ManagedPersistenceMonitoringConfiguration
import Amazonka.EMRServerless.Types.MaximumAllowedResources
import Amazonka.EMRServerless.Types.MonitoringConfiguration
import Amazonka.EMRServerless.Types.NetworkConfiguration
import Amazonka.EMRServerless.Types.S3MonitoringConfiguration
import Amazonka.EMRServerless.Types.SparkSubmit
import Amazonka.EMRServerless.Types.TotalResourceUtilization
import Amazonka.EMRServerless.Types.WorkerResourceConfig
import Amazonka.EMRServerless.Types.WorkerTypeSpecification
import Amazonka.EMRServerless.Types.WorkerTypeSpecificationInput
import Amazonka.EMRServerless.UntagResource
import Amazonka.EMRServerless.UpdateApplication
