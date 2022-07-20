{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DeviceFarm.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Lens
  ( -- * Operations

    -- ** CreateDevicePool
    createDevicePool_description,
    createDevicePool_maxDevices,
    createDevicePool_projectArn,
    createDevicePool_name,
    createDevicePool_rules,
    createDevicePoolResponse_devicePool,
    createDevicePoolResponse_httpStatus,

    -- ** CreateInstanceProfile
    createInstanceProfile_excludeAppPackagesFromCleanup,
    createInstanceProfile_description,
    createInstanceProfile_packageCleanup,
    createInstanceProfile_rebootAfterUse,
    createInstanceProfile_name,
    createInstanceProfileResponse_instanceProfile,
    createInstanceProfileResponse_httpStatus,

    -- ** CreateNetworkProfile
    createNetworkProfile_uplinkDelayMs,
    createNetworkProfile_type,
    createNetworkProfile_uplinkJitterMs,
    createNetworkProfile_downlinkBandwidthBits,
    createNetworkProfile_uplinkLossPercent,
    createNetworkProfile_uplinkBandwidthBits,
    createNetworkProfile_description,
    createNetworkProfile_downlinkLossPercent,
    createNetworkProfile_downlinkDelayMs,
    createNetworkProfile_downlinkJitterMs,
    createNetworkProfile_projectArn,
    createNetworkProfile_name,
    createNetworkProfileResponse_networkProfile,
    createNetworkProfileResponse_httpStatus,

    -- ** CreateProject
    createProject_defaultJobTimeoutMinutes,
    createProject_name,
    createProjectResponse_project,
    createProjectResponse_httpStatus,

    -- ** CreateRemoteAccessSession
    createRemoteAccessSession_name,
    createRemoteAccessSession_clientId,
    createRemoteAccessSession_remoteDebugEnabled,
    createRemoteAccessSession_remoteRecordAppArn,
    createRemoteAccessSession_remoteRecordEnabled,
    createRemoteAccessSession_configuration,
    createRemoteAccessSession_sshPublicKey,
    createRemoteAccessSession_instanceArn,
    createRemoteAccessSession_interactionMode,
    createRemoteAccessSession_skipAppResign,
    createRemoteAccessSession_projectArn,
    createRemoteAccessSession_deviceArn,
    createRemoteAccessSessionResponse_remoteAccessSession,
    createRemoteAccessSessionResponse_httpStatus,

    -- ** CreateTestGridProject
    createTestGridProject_vpcConfig,
    createTestGridProject_description,
    createTestGridProject_name,
    createTestGridProjectResponse_testGridProject,
    createTestGridProjectResponse_httpStatus,

    -- ** CreateTestGridUrl
    createTestGridUrl_projectArn,
    createTestGridUrl_expiresInSeconds,
    createTestGridUrlResponse_url,
    createTestGridUrlResponse_expires,
    createTestGridUrlResponse_httpStatus,

    -- ** CreateUpload
    createUpload_contentType,
    createUpload_projectArn,
    createUpload_name,
    createUpload_type,
    createUploadResponse_upload,
    createUploadResponse_httpStatus,

    -- ** CreateVPCEConfiguration
    createVPCEConfiguration_vpceConfigurationDescription,
    createVPCEConfiguration_vpceConfigurationName,
    createVPCEConfiguration_vpceServiceName,
    createVPCEConfiguration_serviceDnsName,
    createVPCEConfigurationResponse_vpceConfiguration,
    createVPCEConfigurationResponse_httpStatus,

    -- ** DeleteDevicePool
    deleteDevicePool_arn,
    deleteDevicePoolResponse_httpStatus,

    -- ** DeleteInstanceProfile
    deleteInstanceProfile_arn,
    deleteInstanceProfileResponse_httpStatus,

    -- ** DeleteNetworkProfile
    deleteNetworkProfile_arn,
    deleteNetworkProfileResponse_httpStatus,

    -- ** DeleteProject
    deleteProject_arn,
    deleteProjectResponse_httpStatus,

    -- ** DeleteRemoteAccessSession
    deleteRemoteAccessSession_arn,
    deleteRemoteAccessSessionResponse_httpStatus,

    -- ** DeleteRun
    deleteRun_arn,
    deleteRunResponse_httpStatus,

    -- ** DeleteTestGridProject
    deleteTestGridProject_projectArn,
    deleteTestGridProjectResponse_httpStatus,

    -- ** DeleteUpload
    deleteUpload_arn,
    deleteUploadResponse_httpStatus,

    -- ** DeleteVPCEConfiguration
    deleteVPCEConfiguration_arn,
    deleteVPCEConfigurationResponse_httpStatus,

    -- ** GetAccountSettings
    getAccountSettingsResponse_accountSettings,
    getAccountSettingsResponse_httpStatus,

    -- ** GetDevice
    getDevice_arn,
    getDeviceResponse_device,
    getDeviceResponse_httpStatus,

    -- ** GetDeviceInstance
    getDeviceInstance_arn,
    getDeviceInstanceResponse_deviceInstance,
    getDeviceInstanceResponse_httpStatus,

    -- ** GetDevicePool
    getDevicePool_arn,
    getDevicePoolResponse_devicePool,
    getDevicePoolResponse_httpStatus,

    -- ** GetDevicePoolCompatibility
    getDevicePoolCompatibility_configuration,
    getDevicePoolCompatibility_testType,
    getDevicePoolCompatibility_appArn,
    getDevicePoolCompatibility_test,
    getDevicePoolCompatibility_devicePoolArn,
    getDevicePoolCompatibilityResponse_incompatibleDevices,
    getDevicePoolCompatibilityResponse_compatibleDevices,
    getDevicePoolCompatibilityResponse_httpStatus,

    -- ** GetInstanceProfile
    getInstanceProfile_arn,
    getInstanceProfileResponse_instanceProfile,
    getInstanceProfileResponse_httpStatus,

    -- ** GetJob
    getJob_arn,
    getJobResponse_job,
    getJobResponse_httpStatus,

    -- ** GetNetworkProfile
    getNetworkProfile_arn,
    getNetworkProfileResponse_networkProfile,
    getNetworkProfileResponse_httpStatus,

    -- ** GetOfferingStatus
    getOfferingStatus_nextToken,
    getOfferingStatusResponse_nextToken,
    getOfferingStatusResponse_nextPeriod,
    getOfferingStatusResponse_current,
    getOfferingStatusResponse_httpStatus,

    -- ** GetProject
    getProject_arn,
    getProjectResponse_project,
    getProjectResponse_httpStatus,

    -- ** GetRemoteAccessSession
    getRemoteAccessSession_arn,
    getRemoteAccessSessionResponse_remoteAccessSession,
    getRemoteAccessSessionResponse_httpStatus,

    -- ** GetRun
    getRun_arn,
    getRunResponse_run,
    getRunResponse_httpStatus,

    -- ** GetSuite
    getSuite_arn,
    getSuiteResponse_suite,
    getSuiteResponse_httpStatus,

    -- ** GetTest
    getTest_arn,
    getTestResponse_test,
    getTestResponse_httpStatus,

    -- ** GetTestGridProject
    getTestGridProject_projectArn,
    getTestGridProjectResponse_testGridProject,
    getTestGridProjectResponse_httpStatus,

    -- ** GetTestGridSession
    getTestGridSession_sessionArn,
    getTestGridSession_sessionId,
    getTestGridSession_projectArn,
    getTestGridSessionResponse_testGridSession,
    getTestGridSessionResponse_httpStatus,

    -- ** GetUpload
    getUpload_arn,
    getUploadResponse_upload,
    getUploadResponse_httpStatus,

    -- ** GetVPCEConfiguration
    getVPCEConfiguration_arn,
    getVPCEConfigurationResponse_vpceConfiguration,
    getVPCEConfigurationResponse_httpStatus,

    -- ** InstallToRemoteAccessSession
    installToRemoteAccessSession_remoteAccessSessionArn,
    installToRemoteAccessSession_appArn,
    installToRemoteAccessSessionResponse_appUpload,
    installToRemoteAccessSessionResponse_httpStatus,

    -- ** ListArtifacts
    listArtifacts_nextToken,
    listArtifacts_arn,
    listArtifacts_type,
    listArtifactsResponse_nextToken,
    listArtifactsResponse_artifacts,
    listArtifactsResponse_httpStatus,

    -- ** ListDeviceInstances
    listDeviceInstances_nextToken,
    listDeviceInstances_maxResults,
    listDeviceInstancesResponse_nextToken,
    listDeviceInstancesResponse_deviceInstances,
    listDeviceInstancesResponse_httpStatus,

    -- ** ListDevicePools
    listDevicePools_nextToken,
    listDevicePools_type,
    listDevicePools_arn,
    listDevicePoolsResponse_nextToken,
    listDevicePoolsResponse_devicePools,
    listDevicePoolsResponse_httpStatus,

    -- ** ListDevices
    listDevices_nextToken,
    listDevices_filters,
    listDevices_arn,
    listDevicesResponse_devices,
    listDevicesResponse_nextToken,
    listDevicesResponse_httpStatus,

    -- ** ListInstanceProfiles
    listInstanceProfiles_nextToken,
    listInstanceProfiles_maxResults,
    listInstanceProfilesResponse_nextToken,
    listInstanceProfilesResponse_instanceProfiles,
    listInstanceProfilesResponse_httpStatus,

    -- ** ListJobs
    listJobs_nextToken,
    listJobs_arn,
    listJobsResponse_nextToken,
    listJobsResponse_jobs,
    listJobsResponse_httpStatus,

    -- ** ListNetworkProfiles
    listNetworkProfiles_nextToken,
    listNetworkProfiles_type,
    listNetworkProfiles_arn,
    listNetworkProfilesResponse_nextToken,
    listNetworkProfilesResponse_networkProfiles,
    listNetworkProfilesResponse_httpStatus,

    -- ** ListOfferingPromotions
    listOfferingPromotions_nextToken,
    listOfferingPromotionsResponse_nextToken,
    listOfferingPromotionsResponse_offeringPromotions,
    listOfferingPromotionsResponse_httpStatus,

    -- ** ListOfferingTransactions
    listOfferingTransactions_nextToken,
    listOfferingTransactionsResponse_nextToken,
    listOfferingTransactionsResponse_offeringTransactions,
    listOfferingTransactionsResponse_httpStatus,

    -- ** ListOfferings
    listOfferings_nextToken,
    listOfferingsResponse_nextToken,
    listOfferingsResponse_offerings,
    listOfferingsResponse_httpStatus,

    -- ** ListProjects
    listProjects_nextToken,
    listProjects_arn,
    listProjectsResponse_projects,
    listProjectsResponse_nextToken,
    listProjectsResponse_httpStatus,

    -- ** ListRemoteAccessSessions
    listRemoteAccessSessions_nextToken,
    listRemoteAccessSessions_arn,
    listRemoteAccessSessionsResponse_nextToken,
    listRemoteAccessSessionsResponse_remoteAccessSessions,
    listRemoteAccessSessionsResponse_httpStatus,

    -- ** ListRuns
    listRuns_nextToken,
    listRuns_arn,
    listRunsResponse_nextToken,
    listRunsResponse_runs,
    listRunsResponse_httpStatus,

    -- ** ListSamples
    listSamples_nextToken,
    listSamples_arn,
    listSamplesResponse_nextToken,
    listSamplesResponse_samples,
    listSamplesResponse_httpStatus,

    -- ** ListSuites
    listSuites_nextToken,
    listSuites_arn,
    listSuitesResponse_suites,
    listSuitesResponse_nextToken,
    listSuitesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTestGridProjects
    listTestGridProjects_nextToken,
    listTestGridProjects_maxResult,
    listTestGridProjectsResponse_nextToken,
    listTestGridProjectsResponse_testGridProjects,
    listTestGridProjectsResponse_httpStatus,

    -- ** ListTestGridSessionActions
    listTestGridSessionActions_nextToken,
    listTestGridSessionActions_maxResult,
    listTestGridSessionActions_sessionArn,
    listTestGridSessionActionsResponse_nextToken,
    listTestGridSessionActionsResponse_actions,
    listTestGridSessionActionsResponse_httpStatus,

    -- ** ListTestGridSessionArtifacts
    listTestGridSessionArtifacts_nextToken,
    listTestGridSessionArtifacts_type,
    listTestGridSessionArtifacts_maxResult,
    listTestGridSessionArtifacts_sessionArn,
    listTestGridSessionArtifactsResponse_nextToken,
    listTestGridSessionArtifactsResponse_artifacts,
    listTestGridSessionArtifactsResponse_httpStatus,

    -- ** ListTestGridSessions
    listTestGridSessions_nextToken,
    listTestGridSessions_maxResult,
    listTestGridSessions_status,
    listTestGridSessions_creationTimeBefore,
    listTestGridSessions_endTimeBefore,
    listTestGridSessions_endTimeAfter,
    listTestGridSessions_creationTimeAfter,
    listTestGridSessions_projectArn,
    listTestGridSessionsResponse_nextToken,
    listTestGridSessionsResponse_testGridSessions,
    listTestGridSessionsResponse_httpStatus,

    -- ** ListTests
    listTests_nextToken,
    listTests_arn,
    listTestsResponse_nextToken,
    listTestsResponse_tests,
    listTestsResponse_httpStatus,

    -- ** ListUniqueProblems
    listUniqueProblems_nextToken,
    listUniqueProblems_arn,
    listUniqueProblemsResponse_nextToken,
    listUniqueProblemsResponse_uniqueProblems,
    listUniqueProblemsResponse_httpStatus,

    -- ** ListUploads
    listUploads_nextToken,
    listUploads_type,
    listUploads_arn,
    listUploadsResponse_uploads,
    listUploadsResponse_nextToken,
    listUploadsResponse_httpStatus,

    -- ** ListVPCEConfigurations
    listVPCEConfigurations_nextToken,
    listVPCEConfigurations_maxResults,
    listVPCEConfigurationsResponse_nextToken,
    listVPCEConfigurationsResponse_vpceConfigurations,
    listVPCEConfigurationsResponse_httpStatus,

    -- ** PurchaseOffering
    purchaseOffering_offeringPromotionId,
    purchaseOffering_offeringId,
    purchaseOffering_quantity,
    purchaseOfferingResponse_offeringTransaction,
    purchaseOfferingResponse_httpStatus,

    -- ** RenewOffering
    renewOffering_offeringId,
    renewOffering_quantity,
    renewOfferingResponse_offeringTransaction,
    renewOfferingResponse_httpStatus,

    -- ** ScheduleRun
    scheduleRun_name,
    scheduleRun_deviceSelectionConfiguration,
    scheduleRun_configuration,
    scheduleRun_executionConfiguration,
    scheduleRun_appArn,
    scheduleRun_devicePoolArn,
    scheduleRun_projectArn,
    scheduleRun_test,
    scheduleRunResponse_run,
    scheduleRunResponse_httpStatus,

    -- ** StopJob
    stopJob_arn,
    stopJobResponse_job,
    stopJobResponse_httpStatus,

    -- ** StopRemoteAccessSession
    stopRemoteAccessSession_arn,
    stopRemoteAccessSessionResponse_remoteAccessSession,
    stopRemoteAccessSessionResponse_httpStatus,

    -- ** StopRun
    stopRun_arn,
    stopRunResponse_run,
    stopRunResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateDeviceInstance
    updateDeviceInstance_profileArn,
    updateDeviceInstance_labels,
    updateDeviceInstance_arn,
    updateDeviceInstanceResponse_deviceInstance,
    updateDeviceInstanceResponse_httpStatus,

    -- ** UpdateDevicePool
    updateDevicePool_name,
    updateDevicePool_rules,
    updateDevicePool_clearMaxDevices,
    updateDevicePool_description,
    updateDevicePool_maxDevices,
    updateDevicePool_arn,
    updateDevicePoolResponse_devicePool,
    updateDevicePoolResponse_httpStatus,

    -- ** UpdateInstanceProfile
    updateInstanceProfile_name,
    updateInstanceProfile_excludeAppPackagesFromCleanup,
    updateInstanceProfile_description,
    updateInstanceProfile_packageCleanup,
    updateInstanceProfile_rebootAfterUse,
    updateInstanceProfile_arn,
    updateInstanceProfileResponse_instanceProfile,
    updateInstanceProfileResponse_httpStatus,

    -- ** UpdateNetworkProfile
    updateNetworkProfile_uplinkDelayMs,
    updateNetworkProfile_name,
    updateNetworkProfile_type,
    updateNetworkProfile_uplinkJitterMs,
    updateNetworkProfile_downlinkBandwidthBits,
    updateNetworkProfile_uplinkLossPercent,
    updateNetworkProfile_uplinkBandwidthBits,
    updateNetworkProfile_description,
    updateNetworkProfile_downlinkLossPercent,
    updateNetworkProfile_downlinkDelayMs,
    updateNetworkProfile_downlinkJitterMs,
    updateNetworkProfile_arn,
    updateNetworkProfileResponse_networkProfile,
    updateNetworkProfileResponse_httpStatus,

    -- ** UpdateProject
    updateProject_name,
    updateProject_defaultJobTimeoutMinutes,
    updateProject_arn,
    updateProjectResponse_project,
    updateProjectResponse_httpStatus,

    -- ** UpdateTestGridProject
    updateTestGridProject_name,
    updateTestGridProject_vpcConfig,
    updateTestGridProject_description,
    updateTestGridProject_projectArn,
    updateTestGridProjectResponse_testGridProject,
    updateTestGridProjectResponse_httpStatus,

    -- ** UpdateUpload
    updateUpload_name,
    updateUpload_editContent,
    updateUpload_contentType,
    updateUpload_arn,
    updateUploadResponse_upload,
    updateUploadResponse_httpStatus,

    -- ** UpdateVPCEConfiguration
    updateVPCEConfiguration_vpceServiceName,
    updateVPCEConfiguration_vpceConfigurationName,
    updateVPCEConfiguration_vpceConfigurationDescription,
    updateVPCEConfiguration_serviceDnsName,
    updateVPCEConfiguration_arn,
    updateVPCEConfigurationResponse_vpceConfiguration,
    updateVPCEConfigurationResponse_httpStatus,

    -- * Types

    -- ** AccountSettings
    accountSettings_unmeteredRemoteAccessDevices,
    accountSettings_trialMinutes,
    accountSettings_defaultJobTimeoutMinutes,
    accountSettings_maxSlots,
    accountSettings_unmeteredDevices,
    accountSettings_awsAccountNumber,
    accountSettings_maxJobTimeoutMinutes,
    accountSettings_skipAppResign,

    -- ** Artifact
    artifact_name,
    artifact_type,
    artifact_extension,
    artifact_arn,
    artifact_url,

    -- ** CPU
    cpu_frequency,
    cpu_clock,
    cpu_architecture,

    -- ** Counters
    counters_failed,
    counters_total,
    counters_warned,
    counters_errored,
    counters_skipped,
    counters_passed,
    counters_stopped,

    -- ** CreateRemoteAccessSessionConfiguration
    createRemoteAccessSessionConfiguration_vpceConfigurationArns,
    createRemoteAccessSessionConfiguration_billingMethod,

    -- ** CustomerArtifactPaths
    customerArtifactPaths_androidPaths,
    customerArtifactPaths_deviceHostPaths,
    customerArtifactPaths_iosPaths,

    -- ** Device
    device_instances,
    device_os,
    device_name,
    device_model,
    device_fleetType,
    device_formFactor,
    device_remoteDebugEnabled,
    device_memory,
    device_cpu,
    device_remoteAccessEnabled,
    device_arn,
    device_heapSize,
    device_carrier,
    device_platform,
    device_availability,
    device_fleetName,
    device_manufacturer,
    device_modelId,
    device_resolution,
    device_image,
    device_radio,

    -- ** DeviceFilter
    deviceFilter_attribute,
    deviceFilter_operator,
    deviceFilter_values,

    -- ** DeviceInstance
    deviceInstance_arn,
    deviceInstance_status,
    deviceInstance_instanceProfile,
    deviceInstance_labels,
    deviceInstance_deviceArn,
    deviceInstance_udid,

    -- ** DeviceMinutes
    deviceMinutes_unmetered,
    deviceMinutes_total,
    deviceMinutes_metered,

    -- ** DevicePool
    devicePool_name,
    devicePool_type,
    devicePool_rules,
    devicePool_arn,
    devicePool_description,
    devicePool_maxDevices,

    -- ** DevicePoolCompatibilityResult
    devicePoolCompatibilityResult_compatible,
    devicePoolCompatibilityResult_device,
    devicePoolCompatibilityResult_incompatibilityMessages,

    -- ** DeviceSelectionConfiguration
    deviceSelectionConfiguration_filters,
    deviceSelectionConfiguration_maxDevices,

    -- ** DeviceSelectionResult
    deviceSelectionResult_filters,
    deviceSelectionResult_maxDevices,
    deviceSelectionResult_matchedDevicesCount,

    -- ** ExecutionConfiguration
    executionConfiguration_jobTimeoutMinutes,
    executionConfiguration_videoCapture,
    executionConfiguration_accountsCleanup,
    executionConfiguration_appPackagesCleanup,
    executionConfiguration_skipAppResign,

    -- ** IncompatibilityMessage
    incompatibilityMessage_message,
    incompatibilityMessage_type,

    -- ** InstanceProfile
    instanceProfile_name,
    instanceProfile_excludeAppPackagesFromCleanup,
    instanceProfile_arn,
    instanceProfile_description,
    instanceProfile_packageCleanup,
    instanceProfile_rebootAfterUse,

    -- ** Job
    job_message,
    job_name,
    job_type,
    job_started,
    job_created,
    job_deviceMinutes,
    job_device,
    job_arn,
    job_videoCapture,
    job_videoEndpoint,
    job_status,
    job_counters,
    job_instanceArn,
    job_result,
    job_stopped,

    -- ** Location
    location_latitude,
    location_longitude,

    -- ** MonetaryAmount
    monetaryAmount_currencyCode,
    monetaryAmount_amount,

    -- ** NetworkProfile
    networkProfile_uplinkDelayMs,
    networkProfile_name,
    networkProfile_type,
    networkProfile_uplinkJitterMs,
    networkProfile_downlinkBandwidthBits,
    networkProfile_uplinkLossPercent,
    networkProfile_uplinkBandwidthBits,
    networkProfile_arn,
    networkProfile_description,
    networkProfile_downlinkLossPercent,
    networkProfile_downlinkDelayMs,
    networkProfile_downlinkJitterMs,

    -- ** Offering
    offering_type,
    offering_recurringCharges,
    offering_description,
    offering_platform,
    offering_id,

    -- ** OfferingPromotion
    offeringPromotion_description,
    offeringPromotion_id,

    -- ** OfferingStatus
    offeringStatus_effectiveOn,
    offeringStatus_quantity,
    offeringStatus_type,
    offeringStatus_offering,

    -- ** OfferingTransaction
    offeringTransaction_createdOn,
    offeringTransaction_offeringPromotionId,
    offeringTransaction_transactionId,
    offeringTransaction_offeringStatus,
    offeringTransaction_cost,

    -- ** Problem
    problem_message,
    problem_suite,
    problem_device,
    problem_run,
    problem_job,
    problem_test,
    problem_result,

    -- ** ProblemDetail
    problemDetail_name,
    problemDetail_arn,

    -- ** Project
    project_name,
    project_created,
    project_arn,
    project_defaultJobTimeoutMinutes,

    -- ** Radios
    radios_gps,
    radios_wifi,
    radios_bluetooth,
    radios_nfc,

    -- ** RecurringCharge
    recurringCharge_frequency,
    recurringCharge_cost,

    -- ** RemoteAccessSession
    remoteAccessSession_deviceUdid,
    remoteAccessSession_message,
    remoteAccessSession_name,
    remoteAccessSession_started,
    remoteAccessSession_clientId,
    remoteAccessSession_remoteDebugEnabled,
    remoteAccessSession_remoteRecordAppArn,
    remoteAccessSession_created,
    remoteAccessSession_remoteRecordEnabled,
    remoteAccessSession_deviceMinutes,
    remoteAccessSession_device,
    remoteAccessSession_arn,
    remoteAccessSession_status,
    remoteAccessSession_billingMethod,
    remoteAccessSession_instanceArn,
    remoteAccessSession_result,
    remoteAccessSession_endpoint,
    remoteAccessSession_interactionMode,
    remoteAccessSession_stopped,
    remoteAccessSession_hostAddress,
    remoteAccessSession_skipAppResign,

    -- ** Resolution
    resolution_width,
    resolution_height,

    -- ** Rule
    rule_attribute,
    rule_operator,
    rule_value,

    -- ** Run
    run_appUpload,
    run_message,
    run_jobTimeoutMinutes,
    run_name,
    run_type,
    run_started,
    run_customerArtifactPaths,
    run_created,
    run_deviceMinutes,
    run_locale,
    run_resultCode,
    run_networkProfile,
    run_seed,
    run_radios,
    run_arn,
    run_totalJobs,
    run_status,
    run_platform,
    run_billingMethod,
    run_counters,
    run_webUrl,
    run_eventCount,
    run_location,
    run_deviceSelectionResult,
    run_parsingResultUrl,
    run_completedJobs,
    run_testSpecArn,
    run_result,
    run_stopped,
    run_devicePoolArn,
    run_skipAppResign,

    -- ** Sample
    sample_type,
    sample_arn,
    sample_url,

    -- ** ScheduleRunConfiguration
    scheduleRunConfiguration_customerArtifactPaths,
    scheduleRunConfiguration_vpceConfigurationArns,
    scheduleRunConfiguration_locale,
    scheduleRunConfiguration_radios,
    scheduleRunConfiguration_billingMethod,
    scheduleRunConfiguration_location,
    scheduleRunConfiguration_extraDataPackageArn,
    scheduleRunConfiguration_auxiliaryApps,
    scheduleRunConfiguration_networkProfileArn,

    -- ** ScheduleRunTest
    scheduleRunTest_filter,
    scheduleRunTest_testSpecArn,
    scheduleRunTest_testPackageArn,
    scheduleRunTest_parameters,
    scheduleRunTest_type,

    -- ** Suite
    suite_message,
    suite_name,
    suite_type,
    suite_started,
    suite_created,
    suite_deviceMinutes,
    suite_arn,
    suite_status,
    suite_counters,
    suite_result,
    suite_stopped,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Test
    test_message,
    test_name,
    test_type,
    test_started,
    test_created,
    test_deviceMinutes,
    test_arn,
    test_status,
    test_counters,
    test_result,
    test_stopped,

    -- ** TestGridProject
    testGridProject_name,
    testGridProject_vpcConfig,
    testGridProject_created,
    testGridProject_arn,
    testGridProject_description,

    -- ** TestGridSession
    testGridSession_seleniumProperties,
    testGridSession_ended,
    testGridSession_created,
    testGridSession_arn,
    testGridSession_status,
    testGridSession_billingMinutes,

    -- ** TestGridSessionAction
    testGridSessionAction_requestMethod,
    testGridSessionAction_started,
    testGridSessionAction_duration,
    testGridSessionAction_action,
    testGridSessionAction_statusCode,

    -- ** TestGridSessionArtifact
    testGridSessionArtifact_type,
    testGridSessionArtifact_filename,
    testGridSessionArtifact_url,

    -- ** TestGridVpcConfig
    testGridVpcConfig_securityGroupIds,
    testGridVpcConfig_subnetIds,
    testGridVpcConfig_vpcId,

    -- ** TrialMinutes
    trialMinutes_total,
    trialMinutes_remaining,

    -- ** UniqueProblem
    uniqueProblem_message,
    uniqueProblem_problems,

    -- ** Upload
    upload_message,
    upload_name,
    upload_type,
    upload_created,
    upload_metadata,
    upload_arn,
    upload_url,
    upload_status,
    upload_category,
    upload_contentType,

    -- ** VPCEConfiguration
    vPCEConfiguration_arn,
    vPCEConfiguration_vpceServiceName,
    vPCEConfiguration_vpceConfigurationName,
    vPCEConfiguration_vpceConfigurationDescription,
    vPCEConfiguration_serviceDnsName,
  )
where

import Amazonka.DeviceFarm.CreateDevicePool
import Amazonka.DeviceFarm.CreateInstanceProfile
import Amazonka.DeviceFarm.CreateNetworkProfile
import Amazonka.DeviceFarm.CreateProject
import Amazonka.DeviceFarm.CreateRemoteAccessSession
import Amazonka.DeviceFarm.CreateTestGridProject
import Amazonka.DeviceFarm.CreateTestGridUrl
import Amazonka.DeviceFarm.CreateUpload
import Amazonka.DeviceFarm.CreateVPCEConfiguration
import Amazonka.DeviceFarm.DeleteDevicePool
import Amazonka.DeviceFarm.DeleteInstanceProfile
import Amazonka.DeviceFarm.DeleteNetworkProfile
import Amazonka.DeviceFarm.DeleteProject
import Amazonka.DeviceFarm.DeleteRemoteAccessSession
import Amazonka.DeviceFarm.DeleteRun
import Amazonka.DeviceFarm.DeleteTestGridProject
import Amazonka.DeviceFarm.DeleteUpload
import Amazonka.DeviceFarm.DeleteVPCEConfiguration
import Amazonka.DeviceFarm.GetAccountSettings
import Amazonka.DeviceFarm.GetDevice
import Amazonka.DeviceFarm.GetDeviceInstance
import Amazonka.DeviceFarm.GetDevicePool
import Amazonka.DeviceFarm.GetDevicePoolCompatibility
import Amazonka.DeviceFarm.GetInstanceProfile
import Amazonka.DeviceFarm.GetJob
import Amazonka.DeviceFarm.GetNetworkProfile
import Amazonka.DeviceFarm.GetOfferingStatus
import Amazonka.DeviceFarm.GetProject
import Amazonka.DeviceFarm.GetRemoteAccessSession
import Amazonka.DeviceFarm.GetRun
import Amazonka.DeviceFarm.GetSuite
import Amazonka.DeviceFarm.GetTest
import Amazonka.DeviceFarm.GetTestGridProject
import Amazonka.DeviceFarm.GetTestGridSession
import Amazonka.DeviceFarm.GetUpload
import Amazonka.DeviceFarm.GetVPCEConfiguration
import Amazonka.DeviceFarm.InstallToRemoteAccessSession
import Amazonka.DeviceFarm.ListArtifacts
import Amazonka.DeviceFarm.ListDeviceInstances
import Amazonka.DeviceFarm.ListDevicePools
import Amazonka.DeviceFarm.ListDevices
import Amazonka.DeviceFarm.ListInstanceProfiles
import Amazonka.DeviceFarm.ListJobs
import Amazonka.DeviceFarm.ListNetworkProfiles
import Amazonka.DeviceFarm.ListOfferingPromotions
import Amazonka.DeviceFarm.ListOfferingTransactions
import Amazonka.DeviceFarm.ListOfferings
import Amazonka.DeviceFarm.ListProjects
import Amazonka.DeviceFarm.ListRemoteAccessSessions
import Amazonka.DeviceFarm.ListRuns
import Amazonka.DeviceFarm.ListSamples
import Amazonka.DeviceFarm.ListSuites
import Amazonka.DeviceFarm.ListTagsForResource
import Amazonka.DeviceFarm.ListTestGridProjects
import Amazonka.DeviceFarm.ListTestGridSessionActions
import Amazonka.DeviceFarm.ListTestGridSessionArtifacts
import Amazonka.DeviceFarm.ListTestGridSessions
import Amazonka.DeviceFarm.ListTests
import Amazonka.DeviceFarm.ListUniqueProblems
import Amazonka.DeviceFarm.ListUploads
import Amazonka.DeviceFarm.ListVPCEConfigurations
import Amazonka.DeviceFarm.PurchaseOffering
import Amazonka.DeviceFarm.RenewOffering
import Amazonka.DeviceFarm.ScheduleRun
import Amazonka.DeviceFarm.StopJob
import Amazonka.DeviceFarm.StopRemoteAccessSession
import Amazonka.DeviceFarm.StopRun
import Amazonka.DeviceFarm.TagResource
import Amazonka.DeviceFarm.Types.AccountSettings
import Amazonka.DeviceFarm.Types.Artifact
import Amazonka.DeviceFarm.Types.CPU
import Amazonka.DeviceFarm.Types.Counters
import Amazonka.DeviceFarm.Types.CreateRemoteAccessSessionConfiguration
import Amazonka.DeviceFarm.Types.CustomerArtifactPaths
import Amazonka.DeviceFarm.Types.Device
import Amazonka.DeviceFarm.Types.DeviceFilter
import Amazonka.DeviceFarm.Types.DeviceInstance
import Amazonka.DeviceFarm.Types.DeviceMinutes
import Amazonka.DeviceFarm.Types.DevicePool
import Amazonka.DeviceFarm.Types.DevicePoolCompatibilityResult
import Amazonka.DeviceFarm.Types.DeviceSelectionConfiguration
import Amazonka.DeviceFarm.Types.DeviceSelectionResult
import Amazonka.DeviceFarm.Types.ExecutionConfiguration
import Amazonka.DeviceFarm.Types.IncompatibilityMessage
import Amazonka.DeviceFarm.Types.InstanceProfile
import Amazonka.DeviceFarm.Types.Job
import Amazonka.DeviceFarm.Types.Location
import Amazonka.DeviceFarm.Types.MonetaryAmount
import Amazonka.DeviceFarm.Types.NetworkProfile
import Amazonka.DeviceFarm.Types.Offering
import Amazonka.DeviceFarm.Types.OfferingPromotion
import Amazonka.DeviceFarm.Types.OfferingStatus
import Amazonka.DeviceFarm.Types.OfferingTransaction
import Amazonka.DeviceFarm.Types.Problem
import Amazonka.DeviceFarm.Types.ProblemDetail
import Amazonka.DeviceFarm.Types.Project
import Amazonka.DeviceFarm.Types.Radios
import Amazonka.DeviceFarm.Types.RecurringCharge
import Amazonka.DeviceFarm.Types.RemoteAccessSession
import Amazonka.DeviceFarm.Types.Resolution
import Amazonka.DeviceFarm.Types.Rule
import Amazonka.DeviceFarm.Types.Run
import Amazonka.DeviceFarm.Types.Sample
import Amazonka.DeviceFarm.Types.ScheduleRunConfiguration
import Amazonka.DeviceFarm.Types.ScheduleRunTest
import Amazonka.DeviceFarm.Types.Suite
import Amazonka.DeviceFarm.Types.Tag
import Amazonka.DeviceFarm.Types.Test
import Amazonka.DeviceFarm.Types.TestGridProject
import Amazonka.DeviceFarm.Types.TestGridSession
import Amazonka.DeviceFarm.Types.TestGridSessionAction
import Amazonka.DeviceFarm.Types.TestGridSessionArtifact
import Amazonka.DeviceFarm.Types.TestGridVpcConfig
import Amazonka.DeviceFarm.Types.TrialMinutes
import Amazonka.DeviceFarm.Types.UniqueProblem
import Amazonka.DeviceFarm.Types.Upload
import Amazonka.DeviceFarm.Types.VPCEConfiguration
import Amazonka.DeviceFarm.UntagResource
import Amazonka.DeviceFarm.UpdateDeviceInstance
import Amazonka.DeviceFarm.UpdateDevicePool
import Amazonka.DeviceFarm.UpdateInstanceProfile
import Amazonka.DeviceFarm.UpdateNetworkProfile
import Amazonka.DeviceFarm.UpdateProject
import Amazonka.DeviceFarm.UpdateTestGridProject
import Amazonka.DeviceFarm.UpdateUpload
import Amazonka.DeviceFarm.UpdateVPCEConfiguration
