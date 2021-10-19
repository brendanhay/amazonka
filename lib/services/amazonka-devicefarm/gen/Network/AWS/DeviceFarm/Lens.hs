{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Lens
  ( -- * Operations

    -- ** ListProjects
    listProjects_arn,
    listProjects_nextToken,
    listProjectsResponse_nextToken,
    listProjectsResponse_projects,
    listProjectsResponse_httpStatus,

    -- ** DeleteProject
    deleteProject_arn,
    deleteProjectResponse_httpStatus,

    -- ** UpdateProject
    updateProject_name,
    updateProject_defaultJobTimeoutMinutes,
    updateProject_arn,
    updateProjectResponse_project,
    updateProjectResponse_httpStatus,

    -- ** UpdateNetworkProfile
    updateNetworkProfile_uplinkJitterMs,
    updateNetworkProfile_uplinkLossPercent,
    updateNetworkProfile_downlinkJitterMs,
    updateNetworkProfile_name,
    updateNetworkProfile_downlinkLossPercent,
    updateNetworkProfile_type,
    updateNetworkProfile_uplinkDelayMs,
    updateNetworkProfile_uplinkBandwidthBits,
    updateNetworkProfile_description,
    updateNetworkProfile_downlinkDelayMs,
    updateNetworkProfile_downlinkBandwidthBits,
    updateNetworkProfile_arn,
    updateNetworkProfileResponse_networkProfile,
    updateNetworkProfileResponse_httpStatus,

    -- ** DeleteNetworkProfile
    deleteNetworkProfile_arn,
    deleteNetworkProfileResponse_httpStatus,

    -- ** GetDevicePoolCompatibility
    getDevicePoolCompatibility_test,
    getDevicePoolCompatibility_appArn,
    getDevicePoolCompatibility_configuration,
    getDevicePoolCompatibility_testType,
    getDevicePoolCompatibility_devicePoolArn,
    getDevicePoolCompatibilityResponse_incompatibleDevices,
    getDevicePoolCompatibilityResponse_compatibleDevices,
    getDevicePoolCompatibilityResponse_httpStatus,

    -- ** InstallToRemoteAccessSession
    installToRemoteAccessSession_remoteAccessSessionArn,
    installToRemoteAccessSession_appArn,
    installToRemoteAccessSessionResponse_appUpload,
    installToRemoteAccessSessionResponse_httpStatus,

    -- ** ListTests
    listTests_nextToken,
    listTests_arn,
    listTestsResponse_tests,
    listTestsResponse_nextToken,
    listTestsResponse_httpStatus,

    -- ** ListArtifacts
    listArtifacts_nextToken,
    listArtifacts_arn,
    listArtifacts_type,
    listArtifactsResponse_artifacts,
    listArtifactsResponse_nextToken,
    listArtifactsResponse_httpStatus,

    -- ** ListTestGridSessionActions
    listTestGridSessionActions_maxResult,
    listTestGridSessionActions_nextToken,
    listTestGridSessionActions_sessionArn,
    listTestGridSessionActionsResponse_actions,
    listTestGridSessionActionsResponse_nextToken,
    listTestGridSessionActionsResponse_httpStatus,

    -- ** CreateUpload
    createUpload_contentType,
    createUpload_projectArn,
    createUpload_name,
    createUpload_type,
    createUploadResponse_upload,
    createUploadResponse_httpStatus,

    -- ** GetDeviceInstance
    getDeviceInstance_arn,
    getDeviceInstanceResponse_deviceInstance,
    getDeviceInstanceResponse_httpStatus,

    -- ** StopJob
    stopJob_arn,
    stopJobResponse_job,
    stopJobResponse_httpStatus,

    -- ** DeleteRemoteAccessSession
    deleteRemoteAccessSession_arn,
    deleteRemoteAccessSessionResponse_httpStatus,

    -- ** ListTestGridSessionArtifacts
    listTestGridSessionArtifacts_maxResult,
    listTestGridSessionArtifacts_nextToken,
    listTestGridSessionArtifacts_type,
    listTestGridSessionArtifacts_sessionArn,
    listTestGridSessionArtifactsResponse_artifacts,
    listTestGridSessionArtifactsResponse_nextToken,
    listTestGridSessionArtifactsResponse_httpStatus,

    -- ** ListTestGridProjects
    listTestGridProjects_maxResult,
    listTestGridProjects_nextToken,
    listTestGridProjectsResponse_testGridProjects,
    listTestGridProjectsResponse_nextToken,
    listTestGridProjectsResponse_httpStatus,

    -- ** DeleteUpload
    deleteUpload_arn,
    deleteUploadResponse_httpStatus,

    -- ** UpdateUpload
    updateUpload_editContent,
    updateUpload_name,
    updateUpload_contentType,
    updateUpload_arn,
    updateUploadResponse_upload,
    updateUploadResponse_httpStatus,

    -- ** DeleteTestGridProject
    deleteTestGridProject_projectArn,
    deleteTestGridProjectResponse_httpStatus,

    -- ** UpdateTestGridProject
    updateTestGridProject_name,
    updateTestGridProject_vpcConfig,
    updateTestGridProject_description,
    updateTestGridProject_projectArn,
    updateTestGridProjectResponse_testGridProject,
    updateTestGridProjectResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** GetDevicePool
    getDevicePool_arn,
    getDevicePoolResponse_devicePool,
    getDevicePoolResponse_httpStatus,

    -- ** ListDevicePools
    listDevicePools_nextToken,
    listDevicePools_type,
    listDevicePools_arn,
    listDevicePoolsResponse_devicePools,
    listDevicePoolsResponse_nextToken,
    listDevicePoolsResponse_httpStatus,

    -- ** UpdateDevicePool
    updateDevicePool_rules,
    updateDevicePool_clearMaxDevices,
    updateDevicePool_name,
    updateDevicePool_maxDevices,
    updateDevicePool_description,
    updateDevicePool_arn,
    updateDevicePoolResponse_devicePool,
    updateDevicePoolResponse_httpStatus,

    -- ** DeleteDevicePool
    deleteDevicePool_arn,
    deleteDevicePoolResponse_httpStatus,

    -- ** GetUpload
    getUpload_arn,
    getUploadResponse_upload,
    getUploadResponse_httpStatus,

    -- ** ListOfferingTransactions
    listOfferingTransactions_nextToken,
    listOfferingTransactionsResponse_offeringTransactions,
    listOfferingTransactionsResponse_nextToken,
    listOfferingTransactionsResponse_httpStatus,

    -- ** CreateDevicePool
    createDevicePool_maxDevices,
    createDevicePool_description,
    createDevicePool_projectArn,
    createDevicePool_name,
    createDevicePool_rules,
    createDevicePoolResponse_devicePool,
    createDevicePoolResponse_httpStatus,

    -- ** DeleteRun
    deleteRun_arn,
    deleteRunResponse_httpStatus,

    -- ** ListRuns
    listRuns_nextToken,
    listRuns_arn,
    listRunsResponse_runs,
    listRunsResponse_nextToken,
    listRunsResponse_httpStatus,

    -- ** GetTest
    getTest_arn,
    getTestResponse_test,
    getTestResponse_httpStatus,

    -- ** UpdateDeviceInstance
    updateDeviceInstance_profileArn,
    updateDeviceInstance_labels,
    updateDeviceInstance_arn,
    updateDeviceInstanceResponse_deviceInstance,
    updateDeviceInstanceResponse_httpStatus,

    -- ** GetNetworkProfile
    getNetworkProfile_arn,
    getNetworkProfileResponse_networkProfile,
    getNetworkProfileResponse_httpStatus,

    -- ** RenewOffering
    renewOffering_offeringId,
    renewOffering_quantity,
    renewOfferingResponse_offeringTransaction,
    renewOfferingResponse_httpStatus,

    -- ** DeleteInstanceProfile
    deleteInstanceProfile_arn,
    deleteInstanceProfileResponse_httpStatus,

    -- ** UpdateInstanceProfile
    updateInstanceProfile_rebootAfterUse,
    updateInstanceProfile_name,
    updateInstanceProfile_packageCleanup,
    updateInstanceProfile_excludeAppPackagesFromCleanup,
    updateInstanceProfile_description,
    updateInstanceProfile_arn,
    updateInstanceProfileResponse_instanceProfile,
    updateInstanceProfileResponse_httpStatus,

    -- ** CreateInstanceProfile
    createInstanceProfile_rebootAfterUse,
    createInstanceProfile_packageCleanup,
    createInstanceProfile_excludeAppPackagesFromCleanup,
    createInstanceProfile_description,
    createInstanceProfile_name,
    createInstanceProfileResponse_instanceProfile,
    createInstanceProfileResponse_httpStatus,

    -- ** GetDevice
    getDevice_arn,
    getDeviceResponse_device,
    getDeviceResponse_httpStatus,

    -- ** ListJobs
    listJobs_nextToken,
    listJobs_arn,
    listJobsResponse_jobs,
    listJobsResponse_nextToken,
    listJobsResponse_httpStatus,

    -- ** GetTestGridSession
    getTestGridSession_sessionArn,
    getTestGridSession_projectArn,
    getTestGridSession_sessionId,
    getTestGridSessionResponse_testGridSession,
    getTestGridSessionResponse_httpStatus,

    -- ** GetVPCEConfiguration
    getVPCEConfiguration_arn,
    getVPCEConfigurationResponse_vpceConfiguration,
    getVPCEConfigurationResponse_httpStatus,

    -- ** StopRemoteAccessSession
    stopRemoteAccessSession_arn,
    stopRemoteAccessSessionResponse_remoteAccessSession,
    stopRemoteAccessSessionResponse_httpStatus,

    -- ** CreateNetworkProfile
    createNetworkProfile_uplinkJitterMs,
    createNetworkProfile_uplinkLossPercent,
    createNetworkProfile_downlinkJitterMs,
    createNetworkProfile_downlinkLossPercent,
    createNetworkProfile_type,
    createNetworkProfile_uplinkDelayMs,
    createNetworkProfile_uplinkBandwidthBits,
    createNetworkProfile_description,
    createNetworkProfile_downlinkDelayMs,
    createNetworkProfile_downlinkBandwidthBits,
    createNetworkProfile_projectArn,
    createNetworkProfile_name,
    createNetworkProfileResponse_networkProfile,
    createNetworkProfileResponse_httpStatus,

    -- ** DeleteVPCEConfiguration
    deleteVPCEConfiguration_arn,
    deleteVPCEConfigurationResponse_httpStatus,

    -- ** UpdateVPCEConfiguration
    updateVPCEConfiguration_vpceServiceName,
    updateVPCEConfiguration_vpceConfigurationName,
    updateVPCEConfiguration_serviceDnsName,
    updateVPCEConfiguration_vpceConfigurationDescription,
    updateVPCEConfiguration_arn,
    updateVPCEConfigurationResponse_vpceConfiguration,
    updateVPCEConfigurationResponse_httpStatus,

    -- ** GetJob
    getJob_arn,
    getJobResponse_job,
    getJobResponse_httpStatus,

    -- ** GetInstanceProfile
    getInstanceProfile_arn,
    getInstanceProfileResponse_instanceProfile,
    getInstanceProfileResponse_httpStatus,

    -- ** ListNetworkProfiles
    listNetworkProfiles_nextToken,
    listNetworkProfiles_type,
    listNetworkProfiles_arn,
    listNetworkProfilesResponse_networkProfiles,
    listNetworkProfilesResponse_nextToken,
    listNetworkProfilesResponse_httpStatus,

    -- ** CreateVPCEConfiguration
    createVPCEConfiguration_vpceConfigurationDescription,
    createVPCEConfiguration_vpceConfigurationName,
    createVPCEConfiguration_vpceServiceName,
    createVPCEConfiguration_serviceDnsName,
    createVPCEConfigurationResponse_vpceConfiguration,
    createVPCEConfigurationResponse_httpStatus,

    -- ** ScheduleRun
    scheduleRun_executionConfiguration,
    scheduleRun_deviceSelectionConfiguration,
    scheduleRun_appArn,
    scheduleRun_name,
    scheduleRun_configuration,
    scheduleRun_devicePoolArn,
    scheduleRun_projectArn,
    scheduleRun_test,
    scheduleRunResponse_run,
    scheduleRunResponse_httpStatus,

    -- ** CreateTestGridProject
    createTestGridProject_vpcConfig,
    createTestGridProject_description,
    createTestGridProject_name,
    createTestGridProjectResponse_testGridProject,
    createTestGridProjectResponse_httpStatus,

    -- ** GetRun
    getRun_arn,
    getRunResponse_run,
    getRunResponse_httpStatus,

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

    -- ** ListRemoteAccessSessions
    listRemoteAccessSessions_nextToken,
    listRemoteAccessSessions_arn,
    listRemoteAccessSessionsResponse_nextToken,
    listRemoteAccessSessionsResponse_remoteAccessSessions,
    listRemoteAccessSessionsResponse_httpStatus,

    -- ** GetAccountSettings
    getAccountSettingsResponse_accountSettings,
    getAccountSettingsResponse_httpStatus,

    -- ** CreateRemoteAccessSession
    createRemoteAccessSession_clientId,
    createRemoteAccessSession_skipAppResign,
    createRemoteAccessSession_instanceArn,
    createRemoteAccessSession_remoteRecordEnabled,
    createRemoteAccessSession_remoteRecordAppArn,
    createRemoteAccessSession_sshPublicKey,
    createRemoteAccessSession_name,
    createRemoteAccessSession_remoteDebugEnabled,
    createRemoteAccessSession_configuration,
    createRemoteAccessSession_interactionMode,
    createRemoteAccessSession_projectArn,
    createRemoteAccessSession_deviceArn,
    createRemoteAccessSessionResponse_remoteAccessSession,
    createRemoteAccessSessionResponse_httpStatus,

    -- ** ListOfferingPromotions
    listOfferingPromotions_nextToken,
    listOfferingPromotionsResponse_nextToken,
    listOfferingPromotionsResponse_offeringPromotions,
    listOfferingPromotionsResponse_httpStatus,

    -- ** GetOfferingStatus
    getOfferingStatus_nextToken,
    getOfferingStatusResponse_nextPeriod,
    getOfferingStatusResponse_current,
    getOfferingStatusResponse_nextToken,
    getOfferingStatusResponse_httpStatus,

    -- ** ListUploads
    listUploads_nextToken,
    listUploads_type,
    listUploads_arn,
    listUploadsResponse_nextToken,
    listUploadsResponse_uploads,
    listUploadsResponse_httpStatus,

    -- ** GetTestGridProject
    getTestGridProject_projectArn,
    getTestGridProjectResponse_testGridProject,
    getTestGridProjectResponse_httpStatus,

    -- ** GetSuite
    getSuite_arn,
    getSuiteResponse_suite,
    getSuiteResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** GetRemoteAccessSession
    getRemoteAccessSession_arn,
    getRemoteAccessSessionResponse_remoteAccessSession,
    getRemoteAccessSessionResponse_httpStatus,

    -- ** ListDeviceInstances
    listDeviceInstances_nextToken,
    listDeviceInstances_maxResults,
    listDeviceInstancesResponse_nextToken,
    listDeviceInstancesResponse_deviceInstances,
    listDeviceInstancesResponse_httpStatus,

    -- ** PurchaseOffering
    purchaseOffering_offeringPromotionId,
    purchaseOffering_offeringId,
    purchaseOffering_quantity,
    purchaseOfferingResponse_offeringTransaction,
    purchaseOfferingResponse_httpStatus,

    -- ** ListInstanceProfiles
    listInstanceProfiles_nextToken,
    listInstanceProfiles_maxResults,
    listInstanceProfilesResponse_nextToken,
    listInstanceProfilesResponse_instanceProfiles,
    listInstanceProfilesResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** GetProject
    getProject_arn,
    getProjectResponse_project,
    getProjectResponse_httpStatus,

    -- ** ListUniqueProblems
    listUniqueProblems_nextToken,
    listUniqueProblems_arn,
    listUniqueProblemsResponse_nextToken,
    listUniqueProblemsResponse_uniqueProblems,
    listUniqueProblemsResponse_httpStatus,

    -- ** ListVPCEConfigurations
    listVPCEConfigurations_nextToken,
    listVPCEConfigurations_maxResults,
    listVPCEConfigurationsResponse_nextToken,
    listVPCEConfigurationsResponse_vpceConfigurations,
    listVPCEConfigurationsResponse_httpStatus,

    -- ** StopRun
    stopRun_arn,
    stopRunResponse_run,
    stopRunResponse_httpStatus,

    -- ** ListDevices
    listDevices_arn,
    listDevices_filters,
    listDevices_nextToken,
    listDevicesResponse_nextToken,
    listDevicesResponse_devices,
    listDevicesResponse_httpStatus,

    -- ** CreateProject
    createProject_defaultJobTimeoutMinutes,
    createProject_name,
    createProjectResponse_project,
    createProjectResponse_httpStatus,

    -- ** ListTestGridSessions
    listTestGridSessions_status,
    listTestGridSessions_maxResult,
    listTestGridSessions_creationTimeAfter,
    listTestGridSessions_endTimeBefore,
    listTestGridSessions_endTimeAfter,
    listTestGridSessions_nextToken,
    listTestGridSessions_creationTimeBefore,
    listTestGridSessions_projectArn,
    listTestGridSessionsResponse_nextToken,
    listTestGridSessionsResponse_testGridSessions,
    listTestGridSessionsResponse_httpStatus,

    -- ** CreateTestGridUrl
    createTestGridUrl_projectArn,
    createTestGridUrl_expiresInSeconds,
    createTestGridUrlResponse_expires,
    createTestGridUrlResponse_url,
    createTestGridUrlResponse_httpStatus,

    -- ** ListOfferings
    listOfferings_nextToken,
    listOfferingsResponse_nextToken,
    listOfferingsResponse_offerings,
    listOfferingsResponse_httpStatus,

    -- * Types

    -- ** AccountSettings
    accountSettings_skipAppResign,
    accountSettings_awsAccountNumber,
    accountSettings_maxJobTimeoutMinutes,
    accountSettings_maxSlots,
    accountSettings_trialMinutes,
    accountSettings_unmeteredDevices,
    accountSettings_unmeteredRemoteAccessDevices,
    accountSettings_defaultJobTimeoutMinutes,

    -- ** Artifact
    artifact_arn,
    artifact_url,
    artifact_extension,
    artifact_name,
    artifact_type,

    -- ** CPU
    cpu_frequency,
    cpu_clock,
    cpu_architecture,

    -- ** Counters
    counters_passed,
    counters_skipped,
    counters_warned,
    counters_stopped,
    counters_total,
    counters_failed,
    counters_errored,

    -- ** CreateRemoteAccessSessionConfiguration
    createRemoteAccessSessionConfiguration_billingMethod,
    createRemoteAccessSessionConfiguration_vpceConfigurationArns,

    -- ** CustomerArtifactPaths
    customerArtifactPaths_androidPaths,
    customerArtifactPaths_deviceHostPaths,
    customerArtifactPaths_iosPaths,

    -- ** Device
    device_carrier,
    device_image,
    device_manufacturer,
    device_platform,
    device_modelId,
    device_remoteAccessEnabled,
    device_arn,
    device_formFactor,
    device_fleetType,
    device_resolution,
    device_availability,
    device_memory,
    device_radio,
    device_os,
    device_name,
    device_model,
    device_instances,
    device_remoteDebugEnabled,
    device_cpu,
    device_heapSize,
    device_fleetName,

    -- ** DeviceFilter
    deviceFilter_attribute,
    deviceFilter_operator,
    deviceFilter_values,

    -- ** DeviceInstance
    deviceInstance_status,
    deviceInstance_udid,
    deviceInstance_instanceProfile,
    deviceInstance_arn,
    deviceInstance_deviceArn,
    deviceInstance_labels,

    -- ** DeviceMinutes
    deviceMinutes_metered,
    deviceMinutes_total,
    deviceMinutes_unmetered,

    -- ** DevicePool
    devicePool_arn,
    devicePool_rules,
    devicePool_name,
    devicePool_maxDevices,
    devicePool_type,
    devicePool_description,

    -- ** DevicePoolCompatibilityResult
    devicePoolCompatibilityResult_device,
    devicePoolCompatibilityResult_compatible,
    devicePoolCompatibilityResult_incompatibilityMessages,

    -- ** DeviceSelectionConfiguration
    deviceSelectionConfiguration_filters,
    deviceSelectionConfiguration_maxDevices,

    -- ** DeviceSelectionResult
    deviceSelectionResult_matchedDevicesCount,
    deviceSelectionResult_filters,
    deviceSelectionResult_maxDevices,

    -- ** ExecutionConfiguration
    executionConfiguration_skipAppResign,
    executionConfiguration_accountsCleanup,
    executionConfiguration_appPackagesCleanup,
    executionConfiguration_jobTimeoutMinutes,
    executionConfiguration_videoCapture,

    -- ** IncompatibilityMessage
    incompatibilityMessage_type,
    incompatibilityMessage_message,

    -- ** InstanceProfile
    instanceProfile_arn,
    instanceProfile_rebootAfterUse,
    instanceProfile_name,
    instanceProfile_packageCleanup,
    instanceProfile_excludeAppPackagesFromCleanup,
    instanceProfile_description,

    -- ** Job
    job_instanceArn,
    job_status,
    job_counters,
    job_arn,
    job_created,
    job_device,
    job_stopped,
    job_result,
    job_name,
    job_videoEndpoint,
    job_deviceMinutes,
    job_videoCapture,
    job_type,
    job_message,
    job_started,

    -- ** Location
    location_latitude,
    location_longitude,

    -- ** MonetaryAmount
    monetaryAmount_amount,
    monetaryAmount_currencyCode,

    -- ** NetworkProfile
    networkProfile_uplinkJitterMs,
    networkProfile_arn,
    networkProfile_uplinkLossPercent,
    networkProfile_downlinkJitterMs,
    networkProfile_name,
    networkProfile_downlinkLossPercent,
    networkProfile_type,
    networkProfile_uplinkDelayMs,
    networkProfile_uplinkBandwidthBits,
    networkProfile_description,
    networkProfile_downlinkDelayMs,
    networkProfile_downlinkBandwidthBits,

    -- ** Offering
    offering_platform,
    offering_id,
    offering_recurringCharges,
    offering_type,
    offering_description,

    -- ** OfferingPromotion
    offeringPromotion_id,
    offeringPromotion_description,

    -- ** OfferingStatus
    offeringStatus_effectiveOn,
    offeringStatus_offering,
    offeringStatus_quantity,
    offeringStatus_type,

    -- ** OfferingTransaction
    offeringTransaction_offeringStatus,
    offeringTransaction_cost,
    offeringTransaction_transactionId,
    offeringTransaction_offeringPromotionId,
    offeringTransaction_createdOn,

    -- ** Problem
    problem_device,
    problem_test,
    problem_result,
    problem_run,
    problem_job,
    problem_message,
    problem_suite,

    -- ** ProblemDetail
    problemDetail_arn,
    problemDetail_name,

    -- ** Project
    project_arn,
    project_created,
    project_name,
    project_defaultJobTimeoutMinutes,

    -- ** Radios
    radios_nfc,
    radios_gps,
    radios_bluetooth,
    radios_wifi,

    -- ** RecurringCharge
    recurringCharge_frequency,
    recurringCharge_cost,

    -- ** RemoteAccessSession
    remoteAccessSession_billingMethod,
    remoteAccessSession_clientId,
    remoteAccessSession_deviceUdid,
    remoteAccessSession_skipAppResign,
    remoteAccessSession_instanceArn,
    remoteAccessSession_status,
    remoteAccessSession_remoteRecordEnabled,
    remoteAccessSession_arn,
    remoteAccessSession_remoteRecordAppArn,
    remoteAccessSession_created,
    remoteAccessSession_device,
    remoteAccessSession_stopped,
    remoteAccessSession_result,
    remoteAccessSession_name,
    remoteAccessSession_deviceMinutes,
    remoteAccessSession_remoteDebugEnabled,
    remoteAccessSession_endpoint,
    remoteAccessSession_message,
    remoteAccessSession_hostAddress,
    remoteAccessSession_interactionMode,
    remoteAccessSession_started,

    -- ** Resolution
    resolution_height,
    resolution_width,

    -- ** Rule
    rule_attribute,
    rule_operator,
    rule_value,

    -- ** Run
    run_billingMethod,
    run_skipAppResign,
    run_status,
    run_customerArtifactPaths,
    run_eventCount,
    run_counters,
    run_platform,
    run_seed,
    run_radios,
    run_arn,
    run_location,
    run_created,
    run_locale,
    run_testSpecArn,
    run_stopped,
    run_result,
    run_jobTimeoutMinutes,
    run_completedJobs,
    run_resultCode,
    run_name,
    run_appUpload,
    run_parsingResultUrl,
    run_networkProfile,
    run_deviceMinutes,
    run_type,
    run_message,
    run_webUrl,
    run_totalJobs,
    run_devicePoolArn,
    run_started,
    run_deviceSelectionResult,

    -- ** Sample
    sample_arn,
    sample_url,
    sample_type,

    -- ** ScheduleRunConfiguration
    scheduleRunConfiguration_billingMethod,
    scheduleRunConfiguration_customerArtifactPaths,
    scheduleRunConfiguration_radios,
    scheduleRunConfiguration_location,
    scheduleRunConfiguration_locale,
    scheduleRunConfiguration_networkProfileArn,
    scheduleRunConfiguration_extraDataPackageArn,
    scheduleRunConfiguration_auxiliaryApps,
    scheduleRunConfiguration_vpceConfigurationArns,

    -- ** ScheduleRunTest
    scheduleRunTest_testSpecArn,
    scheduleRunTest_testPackageArn,
    scheduleRunTest_parameters,
    scheduleRunTest_filter,
    scheduleRunTest_type,

    -- ** Suite
    suite_status,
    suite_counters,
    suite_arn,
    suite_created,
    suite_stopped,
    suite_result,
    suite_name,
    suite_deviceMinutes,
    suite_type,
    suite_message,
    suite_started,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Test
    test_status,
    test_counters,
    test_arn,
    test_created,
    test_stopped,
    test_result,
    test_name,
    test_deviceMinutes,
    test_type,
    test_message,
    test_started,

    -- ** TestGridProject
    testGridProject_arn,
    testGridProject_created,
    testGridProject_name,
    testGridProject_vpcConfig,
    testGridProject_description,

    -- ** TestGridSession
    testGridSession_status,
    testGridSession_arn,
    testGridSession_created,
    testGridSession_billingMinutes,
    testGridSession_ended,
    testGridSession_seleniumProperties,

    -- ** TestGridSessionAction
    testGridSessionAction_action,
    testGridSessionAction_duration,
    testGridSessionAction_requestMethod,
    testGridSessionAction_started,
    testGridSessionAction_statusCode,

    -- ** TestGridSessionArtifact
    testGridSessionArtifact_url,
    testGridSessionArtifact_type,
    testGridSessionArtifact_filename,

    -- ** TestGridVpcConfig
    testGridVpcConfig_securityGroupIds,
    testGridVpcConfig_subnetIds,
    testGridVpcConfig_vpcId,

    -- ** TrialMinutes
    trialMinutes_remaining,
    trialMinutes_total,

    -- ** UniqueProblem
    uniqueProblem_problems,
    uniqueProblem_message,

    -- ** Upload
    upload_status,
    upload_arn,
    upload_created,
    upload_category,
    upload_url,
    upload_name,
    upload_metadata,
    upload_type,
    upload_message,
    upload_contentType,

    -- ** VPCEConfiguration
    vPCEConfiguration_vpceServiceName,
    vPCEConfiguration_arn,
    vPCEConfiguration_vpceConfigurationName,
    vPCEConfiguration_serviceDnsName,
    vPCEConfiguration_vpceConfigurationDescription,
  )
where

import Network.AWS.DeviceFarm.CreateDevicePool
import Network.AWS.DeviceFarm.CreateInstanceProfile
import Network.AWS.DeviceFarm.CreateNetworkProfile
import Network.AWS.DeviceFarm.CreateProject
import Network.AWS.DeviceFarm.CreateRemoteAccessSession
import Network.AWS.DeviceFarm.CreateTestGridProject
import Network.AWS.DeviceFarm.CreateTestGridUrl
import Network.AWS.DeviceFarm.CreateUpload
import Network.AWS.DeviceFarm.CreateVPCEConfiguration
import Network.AWS.DeviceFarm.DeleteDevicePool
import Network.AWS.DeviceFarm.DeleteInstanceProfile
import Network.AWS.DeviceFarm.DeleteNetworkProfile
import Network.AWS.DeviceFarm.DeleteProject
import Network.AWS.DeviceFarm.DeleteRemoteAccessSession
import Network.AWS.DeviceFarm.DeleteRun
import Network.AWS.DeviceFarm.DeleteTestGridProject
import Network.AWS.DeviceFarm.DeleteUpload
import Network.AWS.DeviceFarm.DeleteVPCEConfiguration
import Network.AWS.DeviceFarm.GetAccountSettings
import Network.AWS.DeviceFarm.GetDevice
import Network.AWS.DeviceFarm.GetDeviceInstance
import Network.AWS.DeviceFarm.GetDevicePool
import Network.AWS.DeviceFarm.GetDevicePoolCompatibility
import Network.AWS.DeviceFarm.GetInstanceProfile
import Network.AWS.DeviceFarm.GetJob
import Network.AWS.DeviceFarm.GetNetworkProfile
import Network.AWS.DeviceFarm.GetOfferingStatus
import Network.AWS.DeviceFarm.GetProject
import Network.AWS.DeviceFarm.GetRemoteAccessSession
import Network.AWS.DeviceFarm.GetRun
import Network.AWS.DeviceFarm.GetSuite
import Network.AWS.DeviceFarm.GetTest
import Network.AWS.DeviceFarm.GetTestGridProject
import Network.AWS.DeviceFarm.GetTestGridSession
import Network.AWS.DeviceFarm.GetUpload
import Network.AWS.DeviceFarm.GetVPCEConfiguration
import Network.AWS.DeviceFarm.InstallToRemoteAccessSession
import Network.AWS.DeviceFarm.ListArtifacts
import Network.AWS.DeviceFarm.ListDeviceInstances
import Network.AWS.DeviceFarm.ListDevicePools
import Network.AWS.DeviceFarm.ListDevices
import Network.AWS.DeviceFarm.ListInstanceProfiles
import Network.AWS.DeviceFarm.ListJobs
import Network.AWS.DeviceFarm.ListNetworkProfiles
import Network.AWS.DeviceFarm.ListOfferingPromotions
import Network.AWS.DeviceFarm.ListOfferingTransactions
import Network.AWS.DeviceFarm.ListOfferings
import Network.AWS.DeviceFarm.ListProjects
import Network.AWS.DeviceFarm.ListRemoteAccessSessions
import Network.AWS.DeviceFarm.ListRuns
import Network.AWS.DeviceFarm.ListSamples
import Network.AWS.DeviceFarm.ListSuites
import Network.AWS.DeviceFarm.ListTagsForResource
import Network.AWS.DeviceFarm.ListTestGridProjects
import Network.AWS.DeviceFarm.ListTestGridSessionActions
import Network.AWS.DeviceFarm.ListTestGridSessionArtifacts
import Network.AWS.DeviceFarm.ListTestGridSessions
import Network.AWS.DeviceFarm.ListTests
import Network.AWS.DeviceFarm.ListUniqueProblems
import Network.AWS.DeviceFarm.ListUploads
import Network.AWS.DeviceFarm.ListVPCEConfigurations
import Network.AWS.DeviceFarm.PurchaseOffering
import Network.AWS.DeviceFarm.RenewOffering
import Network.AWS.DeviceFarm.ScheduleRun
import Network.AWS.DeviceFarm.StopJob
import Network.AWS.DeviceFarm.StopRemoteAccessSession
import Network.AWS.DeviceFarm.StopRun
import Network.AWS.DeviceFarm.TagResource
import Network.AWS.DeviceFarm.Types.AccountSettings
import Network.AWS.DeviceFarm.Types.Artifact
import Network.AWS.DeviceFarm.Types.CPU
import Network.AWS.DeviceFarm.Types.Counters
import Network.AWS.DeviceFarm.Types.CreateRemoteAccessSessionConfiguration
import Network.AWS.DeviceFarm.Types.CustomerArtifactPaths
import Network.AWS.DeviceFarm.Types.Device
import Network.AWS.DeviceFarm.Types.DeviceFilter
import Network.AWS.DeviceFarm.Types.DeviceInstance
import Network.AWS.DeviceFarm.Types.DeviceMinutes
import Network.AWS.DeviceFarm.Types.DevicePool
import Network.AWS.DeviceFarm.Types.DevicePoolCompatibilityResult
import Network.AWS.DeviceFarm.Types.DeviceSelectionConfiguration
import Network.AWS.DeviceFarm.Types.DeviceSelectionResult
import Network.AWS.DeviceFarm.Types.ExecutionConfiguration
import Network.AWS.DeviceFarm.Types.IncompatibilityMessage
import Network.AWS.DeviceFarm.Types.InstanceProfile
import Network.AWS.DeviceFarm.Types.Job
import Network.AWS.DeviceFarm.Types.Location
import Network.AWS.DeviceFarm.Types.MonetaryAmount
import Network.AWS.DeviceFarm.Types.NetworkProfile
import Network.AWS.DeviceFarm.Types.Offering
import Network.AWS.DeviceFarm.Types.OfferingPromotion
import Network.AWS.DeviceFarm.Types.OfferingStatus
import Network.AWS.DeviceFarm.Types.OfferingTransaction
import Network.AWS.DeviceFarm.Types.Problem
import Network.AWS.DeviceFarm.Types.ProblemDetail
import Network.AWS.DeviceFarm.Types.Project
import Network.AWS.DeviceFarm.Types.Radios
import Network.AWS.DeviceFarm.Types.RecurringCharge
import Network.AWS.DeviceFarm.Types.RemoteAccessSession
import Network.AWS.DeviceFarm.Types.Resolution
import Network.AWS.DeviceFarm.Types.Rule
import Network.AWS.DeviceFarm.Types.Run
import Network.AWS.DeviceFarm.Types.Sample
import Network.AWS.DeviceFarm.Types.ScheduleRunConfiguration
import Network.AWS.DeviceFarm.Types.ScheduleRunTest
import Network.AWS.DeviceFarm.Types.Suite
import Network.AWS.DeviceFarm.Types.Tag
import Network.AWS.DeviceFarm.Types.Test
import Network.AWS.DeviceFarm.Types.TestGridProject
import Network.AWS.DeviceFarm.Types.TestGridSession
import Network.AWS.DeviceFarm.Types.TestGridSessionAction
import Network.AWS.DeviceFarm.Types.TestGridSessionArtifact
import Network.AWS.DeviceFarm.Types.TestGridVpcConfig
import Network.AWS.DeviceFarm.Types.TrialMinutes
import Network.AWS.DeviceFarm.Types.UniqueProblem
import Network.AWS.DeviceFarm.Types.Upload
import Network.AWS.DeviceFarm.Types.VPCEConfiguration
import Network.AWS.DeviceFarm.UntagResource
import Network.AWS.DeviceFarm.UpdateDeviceInstance
import Network.AWS.DeviceFarm.UpdateDevicePool
import Network.AWS.DeviceFarm.UpdateInstanceProfile
import Network.AWS.DeviceFarm.UpdateNetworkProfile
import Network.AWS.DeviceFarm.UpdateProject
import Network.AWS.DeviceFarm.UpdateTestGridProject
import Network.AWS.DeviceFarm.UpdateUpload
import Network.AWS.DeviceFarm.UpdateVPCEConfiguration
