{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DeviceFarm.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    createInstanceProfile_description,
    createInstanceProfile_excludeAppPackagesFromCleanup,
    createInstanceProfile_packageCleanup,
    createInstanceProfile_rebootAfterUse,
    createInstanceProfile_name,
    createInstanceProfileResponse_instanceProfile,
    createInstanceProfileResponse_httpStatus,

    -- ** CreateNetworkProfile
    createNetworkProfile_description,
    createNetworkProfile_downlinkBandwidthBits,
    createNetworkProfile_downlinkDelayMs,
    createNetworkProfile_downlinkJitterMs,
    createNetworkProfile_downlinkLossPercent,
    createNetworkProfile_type,
    createNetworkProfile_uplinkBandwidthBits,
    createNetworkProfile_uplinkDelayMs,
    createNetworkProfile_uplinkJitterMs,
    createNetworkProfile_uplinkLossPercent,
    createNetworkProfile_projectArn,
    createNetworkProfile_name,
    createNetworkProfileResponse_networkProfile,
    createNetworkProfileResponse_httpStatus,

    -- ** CreateProject
    createProject_defaultJobTimeoutMinutes,
    createProject_vpcConfig,
    createProject_name,
    createProjectResponse_project,
    createProjectResponse_httpStatus,

    -- ** CreateRemoteAccessSession
    createRemoteAccessSession_clientId,
    createRemoteAccessSession_configuration,
    createRemoteAccessSession_instanceArn,
    createRemoteAccessSession_interactionMode,
    createRemoteAccessSession_name,
    createRemoteAccessSession_remoteDebugEnabled,
    createRemoteAccessSession_remoteRecordAppArn,
    createRemoteAccessSession_remoteRecordEnabled,
    createRemoteAccessSession_skipAppResign,
    createRemoteAccessSession_sshPublicKey,
    createRemoteAccessSession_projectArn,
    createRemoteAccessSession_deviceArn,
    createRemoteAccessSessionResponse_remoteAccessSession,
    createRemoteAccessSessionResponse_httpStatus,

    -- ** CreateTestGridProject
    createTestGridProject_description,
    createTestGridProject_vpcConfig,
    createTestGridProject_name,
    createTestGridProjectResponse_testGridProject,
    createTestGridProjectResponse_httpStatus,

    -- ** CreateTestGridUrl
    createTestGridUrl_projectArn,
    createTestGridUrl_expiresInSeconds,
    createTestGridUrlResponse_expires,
    createTestGridUrlResponse_url,
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
    getDevicePoolCompatibility_appArn,
    getDevicePoolCompatibility_configuration,
    getDevicePoolCompatibility_test,
    getDevicePoolCompatibility_testType,
    getDevicePoolCompatibility_devicePoolArn,
    getDevicePoolCompatibilityResponse_compatibleDevices,
    getDevicePoolCompatibilityResponse_incompatibleDevices,
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
    getOfferingStatusResponse_current,
    getOfferingStatusResponse_nextPeriod,
    getOfferingStatusResponse_nextToken,
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
    getTestGridSession_projectArn,
    getTestGridSession_sessionArn,
    getTestGridSession_sessionId,
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
    listArtifactsResponse_artifacts,
    listArtifactsResponse_nextToken,
    listArtifactsResponse_httpStatus,

    -- ** ListDeviceInstances
    listDeviceInstances_maxResults,
    listDeviceInstances_nextToken,
    listDeviceInstancesResponse_deviceInstances,
    listDeviceInstancesResponse_nextToken,
    listDeviceInstancesResponse_httpStatus,

    -- ** ListDevicePools
    listDevicePools_nextToken,
    listDevicePools_type,
    listDevicePools_arn,
    listDevicePoolsResponse_devicePools,
    listDevicePoolsResponse_nextToken,
    listDevicePoolsResponse_httpStatus,

    -- ** ListDevices
    listDevices_arn,
    listDevices_filters,
    listDevices_nextToken,
    listDevicesResponse_devices,
    listDevicesResponse_nextToken,
    listDevicesResponse_httpStatus,

    -- ** ListInstanceProfiles
    listInstanceProfiles_maxResults,
    listInstanceProfiles_nextToken,
    listInstanceProfilesResponse_instanceProfiles,
    listInstanceProfilesResponse_nextToken,
    listInstanceProfilesResponse_httpStatus,

    -- ** ListJobs
    listJobs_nextToken,
    listJobs_arn,
    listJobsResponse_jobs,
    listJobsResponse_nextToken,
    listJobsResponse_httpStatus,

    -- ** ListNetworkProfiles
    listNetworkProfiles_nextToken,
    listNetworkProfiles_type,
    listNetworkProfiles_arn,
    listNetworkProfilesResponse_networkProfiles,
    listNetworkProfilesResponse_nextToken,
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
    listProjects_arn,
    listProjects_nextToken,
    listProjectsResponse_nextToken,
    listProjectsResponse_projects,
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
    listSuitesResponse_nextToken,
    listSuitesResponse_suites,
    listSuitesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListTestGridProjects
    listTestGridProjects_maxResult,
    listTestGridProjects_nextToken,
    listTestGridProjectsResponse_nextToken,
    listTestGridProjectsResponse_testGridProjects,
    listTestGridProjectsResponse_httpStatus,

    -- ** ListTestGridSessionActions
    listTestGridSessionActions_maxResult,
    listTestGridSessionActions_nextToken,
    listTestGridSessionActions_sessionArn,
    listTestGridSessionActionsResponse_actions,
    listTestGridSessionActionsResponse_nextToken,
    listTestGridSessionActionsResponse_httpStatus,

    -- ** ListTestGridSessionArtifacts
    listTestGridSessionArtifacts_maxResult,
    listTestGridSessionArtifacts_nextToken,
    listTestGridSessionArtifacts_type,
    listTestGridSessionArtifacts_sessionArn,
    listTestGridSessionArtifactsResponse_artifacts,
    listTestGridSessionArtifactsResponse_nextToken,
    listTestGridSessionArtifactsResponse_httpStatus,

    -- ** ListTestGridSessions
    listTestGridSessions_creationTimeAfter,
    listTestGridSessions_creationTimeBefore,
    listTestGridSessions_endTimeAfter,
    listTestGridSessions_endTimeBefore,
    listTestGridSessions_maxResult,
    listTestGridSessions_nextToken,
    listTestGridSessions_status,
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
    listUploadsResponse_nextToken,
    listUploadsResponse_uploads,
    listUploadsResponse_httpStatus,

    -- ** ListVPCEConfigurations
    listVPCEConfigurations_maxResults,
    listVPCEConfigurations_nextToken,
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
    scheduleRun_appArn,
    scheduleRun_configuration,
    scheduleRun_devicePoolArn,
    scheduleRun_deviceSelectionConfiguration,
    scheduleRun_executionConfiguration,
    scheduleRun_name,
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
    updateDeviceInstance_labels,
    updateDeviceInstance_profileArn,
    updateDeviceInstance_arn,
    updateDeviceInstanceResponse_deviceInstance,
    updateDeviceInstanceResponse_httpStatus,

    -- ** UpdateDevicePool
    updateDevicePool_clearMaxDevices,
    updateDevicePool_description,
    updateDevicePool_maxDevices,
    updateDevicePool_name,
    updateDevicePool_rules,
    updateDevicePool_arn,
    updateDevicePoolResponse_devicePool,
    updateDevicePoolResponse_httpStatus,

    -- ** UpdateInstanceProfile
    updateInstanceProfile_description,
    updateInstanceProfile_excludeAppPackagesFromCleanup,
    updateInstanceProfile_name,
    updateInstanceProfile_packageCleanup,
    updateInstanceProfile_rebootAfterUse,
    updateInstanceProfile_arn,
    updateInstanceProfileResponse_instanceProfile,
    updateInstanceProfileResponse_httpStatus,

    -- ** UpdateNetworkProfile
    updateNetworkProfile_description,
    updateNetworkProfile_downlinkBandwidthBits,
    updateNetworkProfile_downlinkDelayMs,
    updateNetworkProfile_downlinkJitterMs,
    updateNetworkProfile_downlinkLossPercent,
    updateNetworkProfile_name,
    updateNetworkProfile_type,
    updateNetworkProfile_uplinkBandwidthBits,
    updateNetworkProfile_uplinkDelayMs,
    updateNetworkProfile_uplinkJitterMs,
    updateNetworkProfile_uplinkLossPercent,
    updateNetworkProfile_arn,
    updateNetworkProfileResponse_networkProfile,
    updateNetworkProfileResponse_httpStatus,

    -- ** UpdateProject
    updateProject_defaultJobTimeoutMinutes,
    updateProject_name,
    updateProject_vpcConfig,
    updateProject_arn,
    updateProjectResponse_project,
    updateProjectResponse_httpStatus,

    -- ** UpdateTestGridProject
    updateTestGridProject_description,
    updateTestGridProject_name,
    updateTestGridProject_vpcConfig,
    updateTestGridProject_projectArn,
    updateTestGridProjectResponse_testGridProject,
    updateTestGridProjectResponse_httpStatus,

    -- ** UpdateUpload
    updateUpload_contentType,
    updateUpload_editContent,
    updateUpload_name,
    updateUpload_arn,
    updateUploadResponse_upload,
    updateUploadResponse_httpStatus,

    -- ** UpdateVPCEConfiguration
    updateVPCEConfiguration_serviceDnsName,
    updateVPCEConfiguration_vpceConfigurationDescription,
    updateVPCEConfiguration_vpceConfigurationName,
    updateVPCEConfiguration_vpceServiceName,
    updateVPCEConfiguration_arn,
    updateVPCEConfigurationResponse_vpceConfiguration,
    updateVPCEConfigurationResponse_httpStatus,

    -- * Types

    -- ** AccountSettings
    accountSettings_awsAccountNumber,
    accountSettings_defaultJobTimeoutMinutes,
    accountSettings_maxJobTimeoutMinutes,
    accountSettings_maxSlots,
    accountSettings_skipAppResign,
    accountSettings_trialMinutes,
    accountSettings_unmeteredDevices,
    accountSettings_unmeteredRemoteAccessDevices,

    -- ** Artifact
    artifact_arn,
    artifact_extension,
    artifact_name,
    artifact_type,
    artifact_url,

    -- ** CPU
    cpu_architecture,
    cpu_clock,
    cpu_frequency,

    -- ** Counters
    counters_errored,
    counters_failed,
    counters_passed,
    counters_skipped,
    counters_stopped,
    counters_total,
    counters_warned,

    -- ** CreateRemoteAccessSessionConfiguration
    createRemoteAccessSessionConfiguration_billingMethod,
    createRemoteAccessSessionConfiguration_vpceConfigurationArns,

    -- ** CustomerArtifactPaths
    customerArtifactPaths_androidPaths,
    customerArtifactPaths_deviceHostPaths,
    customerArtifactPaths_iosPaths,

    -- ** Device
    device_arn,
    device_availability,
    device_carrier,
    device_cpu,
    device_fleetName,
    device_fleetType,
    device_formFactor,
    device_heapSize,
    device_image,
    device_instances,
    device_manufacturer,
    device_memory,
    device_model,
    device_modelId,
    device_name,
    device_os,
    device_platform,
    device_radio,
    device_remoteAccessEnabled,
    device_remoteDebugEnabled,
    device_resolution,

    -- ** DeviceFilter
    deviceFilter_attribute,
    deviceFilter_operator,
    deviceFilter_values,

    -- ** DeviceInstance
    deviceInstance_arn,
    deviceInstance_deviceArn,
    deviceInstance_instanceProfile,
    deviceInstance_labels,
    deviceInstance_status,
    deviceInstance_udid,

    -- ** DeviceMinutes
    deviceMinutes_metered,
    deviceMinutes_total,
    deviceMinutes_unmetered,

    -- ** DevicePool
    devicePool_arn,
    devicePool_description,
    devicePool_maxDevices,
    devicePool_name,
    devicePool_rules,
    devicePool_type,

    -- ** DevicePoolCompatibilityResult
    devicePoolCompatibilityResult_compatible,
    devicePoolCompatibilityResult_device,
    devicePoolCompatibilityResult_incompatibilityMessages,

    -- ** DeviceSelectionConfiguration
    deviceSelectionConfiguration_filters,
    deviceSelectionConfiguration_maxDevices,

    -- ** DeviceSelectionResult
    deviceSelectionResult_filters,
    deviceSelectionResult_matchedDevicesCount,
    deviceSelectionResult_maxDevices,

    -- ** ExecutionConfiguration
    executionConfiguration_accountsCleanup,
    executionConfiguration_appPackagesCleanup,
    executionConfiguration_jobTimeoutMinutes,
    executionConfiguration_skipAppResign,
    executionConfiguration_videoCapture,

    -- ** IncompatibilityMessage
    incompatibilityMessage_message,
    incompatibilityMessage_type,

    -- ** InstanceProfile
    instanceProfile_arn,
    instanceProfile_description,
    instanceProfile_excludeAppPackagesFromCleanup,
    instanceProfile_name,
    instanceProfile_packageCleanup,
    instanceProfile_rebootAfterUse,

    -- ** Job
    job_arn,
    job_counters,
    job_created,
    job_device,
    job_deviceMinutes,
    job_instanceArn,
    job_message,
    job_name,
    job_result,
    job_started,
    job_status,
    job_stopped,
    job_type,
    job_videoCapture,
    job_videoEndpoint,

    -- ** Location
    location_latitude,
    location_longitude,

    -- ** MonetaryAmount
    monetaryAmount_amount,
    monetaryAmount_currencyCode,

    -- ** NetworkProfile
    networkProfile_arn,
    networkProfile_description,
    networkProfile_downlinkBandwidthBits,
    networkProfile_downlinkDelayMs,
    networkProfile_downlinkJitterMs,
    networkProfile_downlinkLossPercent,
    networkProfile_name,
    networkProfile_type,
    networkProfile_uplinkBandwidthBits,
    networkProfile_uplinkDelayMs,
    networkProfile_uplinkJitterMs,
    networkProfile_uplinkLossPercent,

    -- ** Offering
    offering_description,
    offering_id,
    offering_platform,
    offering_recurringCharges,
    offering_type,

    -- ** OfferingPromotion
    offeringPromotion_description,
    offeringPromotion_id,

    -- ** OfferingStatus
    offeringStatus_effectiveOn,
    offeringStatus_offering,
    offeringStatus_quantity,
    offeringStatus_type,

    -- ** OfferingTransaction
    offeringTransaction_cost,
    offeringTransaction_createdOn,
    offeringTransaction_offeringPromotionId,
    offeringTransaction_offeringStatus,
    offeringTransaction_transactionId,

    -- ** Problem
    problem_device,
    problem_job,
    problem_message,
    problem_result,
    problem_run,
    problem_suite,
    problem_test,

    -- ** ProblemDetail
    problemDetail_arn,
    problemDetail_name,

    -- ** Project
    project_arn,
    project_created,
    project_defaultJobTimeoutMinutes,
    project_name,
    project_vpcConfig,

    -- ** Radios
    radios_bluetooth,
    radios_gps,
    radios_nfc,
    radios_wifi,

    -- ** RecurringCharge
    recurringCharge_cost,
    recurringCharge_frequency,

    -- ** RemoteAccessSession
    remoteAccessSession_arn,
    remoteAccessSession_billingMethod,
    remoteAccessSession_clientId,
    remoteAccessSession_created,
    remoteAccessSession_device,
    remoteAccessSession_deviceMinutes,
    remoteAccessSession_deviceUdid,
    remoteAccessSession_endpoint,
    remoteAccessSession_hostAddress,
    remoteAccessSession_instanceArn,
    remoteAccessSession_interactionMode,
    remoteAccessSession_message,
    remoteAccessSession_name,
    remoteAccessSession_remoteDebugEnabled,
    remoteAccessSession_remoteRecordAppArn,
    remoteAccessSession_remoteRecordEnabled,
    remoteAccessSession_result,
    remoteAccessSession_skipAppResign,
    remoteAccessSession_started,
    remoteAccessSession_status,
    remoteAccessSession_stopped,
    remoteAccessSession_vpcConfig,

    -- ** Resolution
    resolution_height,
    resolution_width,

    -- ** Rule
    rule_attribute,
    rule_operator,
    rule_value,

    -- ** Run
    run_appUpload,
    run_arn,
    run_billingMethod,
    run_completedJobs,
    run_counters,
    run_created,
    run_customerArtifactPaths,
    run_deviceMinutes,
    run_devicePoolArn,
    run_deviceSelectionResult,
    run_eventCount,
    run_jobTimeoutMinutes,
    run_locale,
    run_location,
    run_message,
    run_name,
    run_networkProfile,
    run_parsingResultUrl,
    run_platform,
    run_radios,
    run_result,
    run_resultCode,
    run_seed,
    run_skipAppResign,
    run_started,
    run_status,
    run_stopped,
    run_testSpecArn,
    run_totalJobs,
    run_type,
    run_vpcConfig,
    run_webUrl,

    -- ** Sample
    sample_arn,
    sample_type,
    sample_url,

    -- ** ScheduleRunConfiguration
    scheduleRunConfiguration_auxiliaryApps,
    scheduleRunConfiguration_billingMethod,
    scheduleRunConfiguration_customerArtifactPaths,
    scheduleRunConfiguration_extraDataPackageArn,
    scheduleRunConfiguration_locale,
    scheduleRunConfiguration_location,
    scheduleRunConfiguration_networkProfileArn,
    scheduleRunConfiguration_radios,
    scheduleRunConfiguration_vpceConfigurationArns,

    -- ** ScheduleRunTest
    scheduleRunTest_filter,
    scheduleRunTest_parameters,
    scheduleRunTest_testPackageArn,
    scheduleRunTest_testSpecArn,
    scheduleRunTest_type,

    -- ** Suite
    suite_arn,
    suite_counters,
    suite_created,
    suite_deviceMinutes,
    suite_message,
    suite_name,
    suite_result,
    suite_started,
    suite_status,
    suite_stopped,
    suite_type,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Test
    test_arn,
    test_counters,
    test_created,
    test_deviceMinutes,
    test_message,
    test_name,
    test_result,
    test_started,
    test_status,
    test_stopped,
    test_type,

    -- ** TestGridProject
    testGridProject_arn,
    testGridProject_created,
    testGridProject_description,
    testGridProject_name,
    testGridProject_vpcConfig,

    -- ** TestGridSession
    testGridSession_arn,
    testGridSession_billingMinutes,
    testGridSession_created,
    testGridSession_ended,
    testGridSession_seleniumProperties,
    testGridSession_status,

    -- ** TestGridSessionAction
    testGridSessionAction_action,
    testGridSessionAction_duration,
    testGridSessionAction_requestMethod,
    testGridSessionAction_started,
    testGridSessionAction_statusCode,

    -- ** TestGridSessionArtifact
    testGridSessionArtifact_filename,
    testGridSessionArtifact_type,
    testGridSessionArtifact_url,

    -- ** TestGridVpcConfig
    testGridVpcConfig_securityGroupIds,
    testGridVpcConfig_subnetIds,
    testGridVpcConfig_vpcId,

    -- ** TrialMinutes
    trialMinutes_remaining,
    trialMinutes_total,

    -- ** UniqueProblem
    uniqueProblem_message,
    uniqueProblem_problems,

    -- ** Upload
    upload_arn,
    upload_category,
    upload_contentType,
    upload_created,
    upload_message,
    upload_metadata,
    upload_name,
    upload_status,
    upload_type,
    upload_url,

    -- ** VPCEConfiguration
    vPCEConfiguration_arn,
    vPCEConfiguration_serviceDnsName,
    vPCEConfiguration_vpceConfigurationDescription,
    vPCEConfiguration_vpceConfigurationName,
    vPCEConfiguration_vpceServiceName,

    -- ** VpcConfig
    vpcConfig_securityGroupIds,
    vpcConfig_subnetIds,
    vpcConfig_vpcId,
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
import Amazonka.DeviceFarm.Types.VpcConfig
import Amazonka.DeviceFarm.UntagResource
import Amazonka.DeviceFarm.UpdateDeviceInstance
import Amazonka.DeviceFarm.UpdateDevicePool
import Amazonka.DeviceFarm.UpdateInstanceProfile
import Amazonka.DeviceFarm.UpdateNetworkProfile
import Amazonka.DeviceFarm.UpdateProject
import Amazonka.DeviceFarm.UpdateTestGridProject
import Amazonka.DeviceFarm.UpdateUpload
import Amazonka.DeviceFarm.UpdateVPCEConfiguration
