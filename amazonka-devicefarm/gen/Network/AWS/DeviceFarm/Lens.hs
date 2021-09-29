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

    -- ** CreateUpload
    createUpload_contentType,
    createUpload_projectArn,
    createUpload_name,
    createUpload_type,
    createUploadResponse_upload,
    createUploadResponse_httpStatus,

    -- ** CreateTestGridProject
    createTestGridProject_vpcConfig,
    createTestGridProject_description,
    createTestGridProject_name,
    createTestGridProjectResponse_testGridProject,
    createTestGridProjectResponse_httpStatus,

    -- ** ListTestGridSessionArtifacts
    listTestGridSessionArtifacts_nextToken,
    listTestGridSessionArtifacts_maxResult,
    listTestGridSessionArtifacts_type,
    listTestGridSessionArtifacts_sessionArn,
    listTestGridSessionArtifactsResponse_nextToken,
    listTestGridSessionArtifactsResponse_artifacts,
    listTestGridSessionArtifactsResponse_httpStatus,

    -- ** ListTestGridSessionActions
    listTestGridSessionActions_nextToken,
    listTestGridSessionActions_maxResult,
    listTestGridSessionActions_sessionArn,
    listTestGridSessionActionsResponse_nextToken,
    listTestGridSessionActionsResponse_actions,
    listTestGridSessionActionsResponse_httpStatus,

    -- ** ListSamples
    listSamples_nextToken,
    listSamples_arn,
    listSamplesResponse_nextToken,
    listSamplesResponse_samples,
    listSamplesResponse_httpStatus,

    -- ** ScheduleRun
    scheduleRun_devicePoolArn,
    scheduleRun_configuration,
    scheduleRun_deviceSelectionConfiguration,
    scheduleRun_executionConfiguration,
    scheduleRun_name,
    scheduleRun_appArn,
    scheduleRun_projectArn,
    scheduleRun_test,
    scheduleRunResponse_run,
    scheduleRunResponse_httpStatus,

    -- ** ListProjects
    listProjects_nextToken,
    listProjects_arn,
    listProjectsResponse_nextToken,
    listProjectsResponse_projects,
    listProjectsResponse_httpStatus,

    -- ** GetDevicePoolCompatibility
    getDevicePoolCompatibility_testType,
    getDevicePoolCompatibility_configuration,
    getDevicePoolCompatibility_appArn,
    getDevicePoolCompatibility_test,
    getDevicePoolCompatibility_devicePoolArn,
    getDevicePoolCompatibilityResponse_incompatibleDevices,
    getDevicePoolCompatibilityResponse_compatibleDevices,
    getDevicePoolCompatibilityResponse_httpStatus,

    -- ** CreateVPCEConfiguration
    createVPCEConfiguration_vpceConfigurationDescription,
    createVPCEConfiguration_vpceConfigurationName,
    createVPCEConfiguration_vpceServiceName,
    createVPCEConfiguration_serviceDnsName,
    createVPCEConfigurationResponse_vpceConfiguration,
    createVPCEConfigurationResponse_httpStatus,

    -- ** UpdateNetworkProfile
    updateNetworkProfile_uplinkJitterMs,
    updateNetworkProfile_downlinkBandwidthBits,
    updateNetworkProfile_downlinkDelayMs,
    updateNetworkProfile_downlinkJitterMs,
    updateNetworkProfile_uplinkLossPercent,
    updateNetworkProfile_downlinkLossPercent,
    updateNetworkProfile_name,
    updateNetworkProfile_uplinkBandwidthBits,
    updateNetworkProfile_uplinkDelayMs,
    updateNetworkProfile_description,
    updateNetworkProfile_type,
    updateNetworkProfile_arn,
    updateNetworkProfileResponse_networkProfile,
    updateNetworkProfileResponse_httpStatus,

    -- ** ListNetworkProfiles
    listNetworkProfiles_nextToken,
    listNetworkProfiles_type,
    listNetworkProfiles_arn,
    listNetworkProfilesResponse_nextToken,
    listNetworkProfilesResponse_networkProfiles,
    listNetworkProfilesResponse_httpStatus,

    -- ** DeleteNetworkProfile
    deleteNetworkProfile_arn,
    deleteNetworkProfileResponse_httpStatus,

    -- ** InstallToRemoteAccessSession
    installToRemoteAccessSession_remoteAccessSessionArn,
    installToRemoteAccessSession_appArn,
    installToRemoteAccessSessionResponse_appUpload,
    installToRemoteAccessSessionResponse_httpStatus,

    -- ** StopRun
    stopRun_arn,
    stopRunResponse_run,
    stopRunResponse_httpStatus,

    -- ** ListOfferings
    listOfferings_nextToken,
    listOfferingsResponse_nextToken,
    listOfferingsResponse_offerings,
    listOfferingsResponse_httpStatus,

    -- ** CreateProject
    createProject_defaultJobTimeoutMinutes,
    createProject_name,
    createProjectResponse_project,
    createProjectResponse_httpStatus,

    -- ** GetJob
    getJob_arn,
    getJobResponse_job,
    getJobResponse_httpStatus,

    -- ** ListDevices
    listDevices_nextToken,
    listDevices_arn,
    listDevices_filters,
    listDevicesResponse_nextToken,
    listDevicesResponse_devices,
    listDevicesResponse_httpStatus,

    -- ** CreateTestGridUrl
    createTestGridUrl_projectArn,
    createTestGridUrl_expiresInSeconds,
    createTestGridUrlResponse_url,
    createTestGridUrlResponse_expires,
    createTestGridUrlResponse_httpStatus,

    -- ** CreateNetworkProfile
    createNetworkProfile_uplinkJitterMs,
    createNetworkProfile_downlinkBandwidthBits,
    createNetworkProfile_downlinkDelayMs,
    createNetworkProfile_downlinkJitterMs,
    createNetworkProfile_uplinkLossPercent,
    createNetworkProfile_downlinkLossPercent,
    createNetworkProfile_uplinkBandwidthBits,
    createNetworkProfile_uplinkDelayMs,
    createNetworkProfile_description,
    createNetworkProfile_type,
    createNetworkProfile_projectArn,
    createNetworkProfile_name,
    createNetworkProfileResponse_networkProfile,
    createNetworkProfileResponse_httpStatus,

    -- ** GetDevice
    getDevice_arn,
    getDeviceResponse_device,
    getDeviceResponse_httpStatus,

    -- ** CreateInstanceProfile
    createInstanceProfile_excludeAppPackagesFromCleanup,
    createInstanceProfile_description,
    createInstanceProfile_rebootAfterUse,
    createInstanceProfile_packageCleanup,
    createInstanceProfile_name,
    createInstanceProfileResponse_instanceProfile,
    createInstanceProfileResponse_httpStatus,

    -- ** StopRemoteAccessSession
    stopRemoteAccessSession_arn,
    stopRemoteAccessSessionResponse_remoteAccessSession,
    stopRemoteAccessSessionResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceARN,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** ListRuns
    listRuns_nextToken,
    listRuns_arn,
    listRunsResponse_nextToken,
    listRunsResponse_runs,
    listRunsResponse_httpStatus,

    -- ** UpdateDeviceInstance
    updateDeviceInstance_labels,
    updateDeviceInstance_profileArn,
    updateDeviceInstance_arn,
    updateDeviceInstanceResponse_deviceInstance,
    updateDeviceInstanceResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceARN,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** ListDeviceInstances
    listDeviceInstances_nextToken,
    listDeviceInstances_maxResults,
    listDeviceInstancesResponse_nextToken,
    listDeviceInstancesResponse_deviceInstances,
    listDeviceInstancesResponse_httpStatus,

    -- ** CreateDevicePool
    createDevicePool_maxDevices,
    createDevicePool_description,
    createDevicePool_projectArn,
    createDevicePool_name,
    createDevicePool_rules,
    createDevicePoolResponse_devicePool,
    createDevicePoolResponse_httpStatus,

    -- ** ListDevicePools
    listDevicePools_nextToken,
    listDevicePools_type,
    listDevicePools_arn,
    listDevicePoolsResponse_nextToken,
    listDevicePoolsResponse_devicePools,
    listDevicePoolsResponse_httpStatus,

    -- ** GetTestGridProject
    getTestGridProject_projectArn,
    getTestGridProjectResponse_testGridProject,
    getTestGridProjectResponse_httpStatus,

    -- ** GetUpload
    getUpload_arn,
    getUploadResponse_upload,
    getUploadResponse_httpStatus,

    -- ** UpdateUpload
    updateUpload_contentType,
    updateUpload_editContent,
    updateUpload_name,
    updateUpload_arn,
    updateUploadResponse_upload,
    updateUploadResponse_httpStatus,

    -- ** GetOfferingStatus
    getOfferingStatus_nextToken,
    getOfferingStatusResponse_nextToken,
    getOfferingStatusResponse_nextPeriod,
    getOfferingStatusResponse_current,
    getOfferingStatusResponse_httpStatus,

    -- ** DeleteUpload
    deleteUpload_arn,
    deleteUploadResponse_httpStatus,

    -- ** CreateRemoteAccessSession
    createRemoteAccessSession_clientId,
    createRemoteAccessSession_interactionMode,
    createRemoteAccessSession_configuration,
    createRemoteAccessSession_name,
    createRemoteAccessSession_remoteRecordEnabled,
    createRemoteAccessSession_instanceArn,
    createRemoteAccessSession_skipAppResign,
    createRemoteAccessSession_sshPublicKey,
    createRemoteAccessSession_remoteDebugEnabled,
    createRemoteAccessSession_remoteRecordAppArn,
    createRemoteAccessSession_projectArn,
    createRemoteAccessSession_deviceArn,
    createRemoteAccessSessionResponse_remoteAccessSession,
    createRemoteAccessSessionResponse_httpStatus,

    -- ** ListTestGridProjects
    listTestGridProjects_nextToken,
    listTestGridProjects_maxResult,
    listTestGridProjectsResponse_nextToken,
    listTestGridProjectsResponse_testGridProjects,
    listTestGridProjectsResponse_httpStatus,

    -- ** ListUploads
    listUploads_nextToken,
    listUploads_type,
    listUploads_arn,
    listUploadsResponse_nextToken,
    listUploadsResponse_uploads,
    listUploadsResponse_httpStatus,

    -- ** ListTests
    listTests_nextToken,
    listTests_arn,
    listTestsResponse_nextToken,
    listTestsResponse_tests,
    listTestsResponse_httpStatus,

    -- ** ListRemoteAccessSessions
    listRemoteAccessSessions_nextToken,
    listRemoteAccessSessions_arn,
    listRemoteAccessSessionsResponse_nextToken,
    listRemoteAccessSessionsResponse_remoteAccessSessions,
    listRemoteAccessSessionsResponse_httpStatus,

    -- ** GetRun
    getRun_arn,
    getRunResponse_run,
    getRunResponse_httpStatus,

    -- ** DeleteRemoteAccessSession
    deleteRemoteAccessSession_arn,
    deleteRemoteAccessSessionResponse_httpStatus,

    -- ** GetDeviceInstance
    getDeviceInstance_arn,
    getDeviceInstanceResponse_deviceInstance,
    getDeviceInstanceResponse_httpStatus,

    -- ** ListSuites
    listSuites_nextToken,
    listSuites_arn,
    listSuitesResponse_nextToken,
    listSuitesResponse_suites,
    listSuitesResponse_httpStatus,

    -- ** ListArtifacts
    listArtifacts_nextToken,
    listArtifacts_arn,
    listArtifacts_type,
    listArtifactsResponse_nextToken,
    listArtifactsResponse_artifacts,
    listArtifactsResponse_httpStatus,

    -- ** StopJob
    stopJob_arn,
    stopJobResponse_job,
    stopJobResponse_httpStatus,

    -- ** UpdateProject
    updateProject_name,
    updateProject_defaultJobTimeoutMinutes,
    updateProject_arn,
    updateProjectResponse_project,
    updateProjectResponse_httpStatus,

    -- ** DeleteProject
    deleteProject_arn,
    deleteProjectResponse_httpStatus,

    -- ** GetInstanceProfile
    getInstanceProfile_arn,
    getInstanceProfileResponse_instanceProfile,
    getInstanceProfileResponse_httpStatus,

    -- ** UpdateVPCEConfiguration
    updateVPCEConfiguration_vpceConfigurationName,
    updateVPCEConfiguration_vpceConfigurationDescription,
    updateVPCEConfiguration_serviceDnsName,
    updateVPCEConfiguration_vpceServiceName,
    updateVPCEConfiguration_arn,
    updateVPCEConfigurationResponse_vpceConfiguration,
    updateVPCEConfigurationResponse_httpStatus,

    -- ** ListVPCEConfigurations
    listVPCEConfigurations_nextToken,
    listVPCEConfigurations_maxResults,
    listVPCEConfigurationsResponse_nextToken,
    listVPCEConfigurationsResponse_vpceConfigurations,
    listVPCEConfigurationsResponse_httpStatus,

    -- ** ListTestGridSessions
    listTestGridSessions_nextToken,
    listTestGridSessions_status,
    listTestGridSessions_creationTimeBefore,
    listTestGridSessions_maxResult,
    listTestGridSessions_endTimeAfter,
    listTestGridSessions_endTimeBefore,
    listTestGridSessions_creationTimeAfter,
    listTestGridSessions_projectArn,
    listTestGridSessionsResponse_nextToken,
    listTestGridSessionsResponse_testGridSessions,
    listTestGridSessionsResponse_httpStatus,

    -- ** DeleteVPCEConfiguration
    deleteVPCEConfiguration_arn,
    deleteVPCEConfigurationResponse_httpStatus,

    -- ** ListUniqueProblems
    listUniqueProblems_nextToken,
    listUniqueProblems_arn,
    listUniqueProblemsResponse_nextToken,
    listUniqueProblemsResponse_uniqueProblems,
    listUniqueProblemsResponse_httpStatus,

    -- ** GetVPCEConfiguration
    getVPCEConfiguration_arn,
    getVPCEConfigurationResponse_vpceConfiguration,
    getVPCEConfigurationResponse_httpStatus,

    -- ** GetTestGridSession
    getTestGridSession_sessionArn,
    getTestGridSession_sessionId,
    getTestGridSession_projectArn,
    getTestGridSessionResponse_testGridSession,
    getTestGridSessionResponse_httpStatus,

    -- ** ListJobs
    listJobs_nextToken,
    listJobs_arn,
    listJobsResponse_nextToken,
    listJobsResponse_jobs,
    listJobsResponse_httpStatus,

    -- ** GetNetworkProfile
    getNetworkProfile_arn,
    getNetworkProfileResponse_networkProfile,
    getNetworkProfileResponse_httpStatus,

    -- ** GetProject
    getProject_arn,
    getProjectResponse_project,
    getProjectResponse_httpStatus,

    -- ** UpdateInstanceProfile
    updateInstanceProfile_excludeAppPackagesFromCleanup,
    updateInstanceProfile_name,
    updateInstanceProfile_description,
    updateInstanceProfile_rebootAfterUse,
    updateInstanceProfile_packageCleanup,
    updateInstanceProfile_arn,
    updateInstanceProfileResponse_instanceProfile,
    updateInstanceProfileResponse_httpStatus,

    -- ** ListInstanceProfiles
    listInstanceProfiles_nextToken,
    listInstanceProfiles_maxResults,
    listInstanceProfilesResponse_nextToken,
    listInstanceProfilesResponse_instanceProfiles,
    listInstanceProfilesResponse_httpStatus,

    -- ** DeleteInstanceProfile
    deleteInstanceProfile_arn,
    deleteInstanceProfileResponse_httpStatus,

    -- ** RenewOffering
    renewOffering_offeringId,
    renewOffering_quantity,
    renewOfferingResponse_offeringTransaction,
    renewOfferingResponse_httpStatus,

    -- ** PurchaseOffering
    purchaseOffering_offeringPromotionId,
    purchaseOffering_offeringId,
    purchaseOffering_quantity,
    purchaseOfferingResponse_offeringTransaction,
    purchaseOfferingResponse_httpStatus,

    -- ** DeleteRun
    deleteRun_arn,
    deleteRunResponse_httpStatus,

    -- ** GetRemoteAccessSession
    getRemoteAccessSession_arn,
    getRemoteAccessSessionResponse_remoteAccessSession,
    getRemoteAccessSessionResponse_httpStatus,

    -- ** GetSuite
    getSuite_arn,
    getSuiteResponse_suite,
    getSuiteResponse_httpStatus,

    -- ** GetTest
    getTest_arn,
    getTestResponse_test,
    getTestResponse_httpStatus,

    -- ** ListOfferingTransactions
    listOfferingTransactions_nextToken,
    listOfferingTransactionsResponse_offeringTransactions,
    listOfferingTransactionsResponse_nextToken,
    listOfferingTransactionsResponse_httpStatus,

    -- ** DeleteDevicePool
    deleteDevicePool_arn,
    deleteDevicePoolResponse_httpStatus,

    -- ** UpdateDevicePool
    updateDevicePool_clearMaxDevices,
    updateDevicePool_rules,
    updateDevicePool_maxDevices,
    updateDevicePool_name,
    updateDevicePool_description,
    updateDevicePool_arn,
    updateDevicePoolResponse_devicePool,
    updateDevicePoolResponse_httpStatus,

    -- ** GetDevicePool
    getDevicePool_arn,
    getDevicePoolResponse_devicePool,
    getDevicePoolResponse_httpStatus,

    -- ** DeleteTestGridProject
    deleteTestGridProject_projectArn,
    deleteTestGridProjectResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceARN,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** UpdateTestGridProject
    updateTestGridProject_vpcConfig,
    updateTestGridProject_name,
    updateTestGridProject_description,
    updateTestGridProject_projectArn,
    updateTestGridProjectResponse_testGridProject,
    updateTestGridProjectResponse_httpStatus,

    -- ** GetAccountSettings
    getAccountSettingsResponse_accountSettings,
    getAccountSettingsResponse_httpStatus,

    -- ** ListOfferingPromotions
    listOfferingPromotions_nextToken,
    listOfferingPromotionsResponse_nextToken,
    listOfferingPromotionsResponse_offeringPromotions,
    listOfferingPromotionsResponse_httpStatus,

    -- * Types

    -- ** AccountSettings
    accountSettings_awsAccountNumber,
    accountSettings_maxSlots,
    accountSettings_trialMinutes,
    accountSettings_skipAppResign,
    accountSettings_maxJobTimeoutMinutes,
    accountSettings_unmeteredDevices,
    accountSettings_unmeteredRemoteAccessDevices,
    accountSettings_defaultJobTimeoutMinutes,

    -- ** Artifact
    artifact_arn,
    artifact_name,
    artifact_extension,
    artifact_url,
    artifact_type,

    -- ** CPU
    cpu_architecture,
    cpu_frequency,
    cpu_clock,

    -- ** Counters
    counters_errored,
    counters_warned,
    counters_passed,
    counters_total,
    counters_stopped,
    counters_failed,
    counters_skipped,

    -- ** CreateRemoteAccessSessionConfiguration
    createRemoteAccessSessionConfiguration_billingMethod,
    createRemoteAccessSessionConfiguration_vpceConfigurationArns,

    -- ** CustomerArtifactPaths
    customerArtifactPaths_deviceHostPaths,
    customerArtifactPaths_iosPaths,
    customerArtifactPaths_androidPaths,

    -- ** Device
    device_platform,
    device_manufacturer,
    device_model,
    device_memory,
    device_availability,
    device_fleetName,
    device_fleetType,
    device_formFactor,
    device_arn,
    device_remoteAccessEnabled,
    device_instances,
    device_name,
    device_image,
    device_carrier,
    device_os,
    device_heapSize,
    device_radio,
    device_resolution,
    device_cpu,
    device_remoteDebugEnabled,
    device_modelId,

    -- ** DeviceFilter
    deviceFilter_attribute,
    deviceFilter_operator,
    deviceFilter_values,

    -- ** DeviceInstance
    deviceInstance_status,
    deviceInstance_udid,
    deviceInstance_deviceArn,
    deviceInstance_arn,
    deviceInstance_labels,
    deviceInstance_instanceProfile,

    -- ** DeviceMinutes
    deviceMinutes_unmetered,
    deviceMinutes_metered,
    deviceMinutes_total,

    -- ** DevicePool
    devicePool_arn,
    devicePool_rules,
    devicePool_maxDevices,
    devicePool_name,
    devicePool_description,
    devicePool_type,

    -- ** DevicePoolCompatibilityResult
    devicePoolCompatibilityResult_incompatibilityMessages,
    devicePoolCompatibilityResult_compatible,
    devicePoolCompatibilityResult_device,

    -- ** DeviceSelectionConfiguration
    deviceSelectionConfiguration_filters,
    deviceSelectionConfiguration_maxDevices,

    -- ** DeviceSelectionResult
    deviceSelectionResult_maxDevices,
    deviceSelectionResult_filters,
    deviceSelectionResult_matchedDevicesCount,

    -- ** ExecutionConfiguration
    executionConfiguration_appPackagesCleanup,
    executionConfiguration_videoCapture,
    executionConfiguration_skipAppResign,
    executionConfiguration_jobTimeoutMinutes,
    executionConfiguration_accountsCleanup,

    -- ** IncompatibilityMessage
    incompatibilityMessage_message,
    incompatibilityMessage_type,

    -- ** InstanceProfile
    instanceProfile_excludeAppPackagesFromCleanup,
    instanceProfile_arn,
    instanceProfile_name,
    instanceProfile_description,
    instanceProfile_rebootAfterUse,
    instanceProfile_packageCleanup,

    -- ** Job
    job_counters,
    job_status,
    job_started,
    job_result,
    job_message,
    job_device,
    job_arn,
    job_videoCapture,
    job_videoEndpoint,
    job_name,
    job_instanceArn,
    job_stopped,
    job_type,
    job_created,
    job_deviceMinutes,

    -- ** Location
    location_latitude,
    location_longitude,

    -- ** MonetaryAmount
    monetaryAmount_amount,
    monetaryAmount_currencyCode,

    -- ** NetworkProfile
    networkProfile_uplinkJitterMs,
    networkProfile_downlinkBandwidthBits,
    networkProfile_downlinkDelayMs,
    networkProfile_downlinkJitterMs,
    networkProfile_uplinkLossPercent,
    networkProfile_arn,
    networkProfile_downlinkLossPercent,
    networkProfile_name,
    networkProfile_uplinkBandwidthBits,
    networkProfile_uplinkDelayMs,
    networkProfile_description,
    networkProfile_type,

    -- ** Offering
    offering_platform,
    offering_id,
    offering_description,
    offering_type,
    offering_recurringCharges,

    -- ** OfferingPromotion
    offeringPromotion_id,
    offeringPromotion_description,

    -- ** OfferingStatus
    offeringStatus_quantity,
    offeringStatus_offering,
    offeringStatus_effectiveOn,
    offeringStatus_type,

    -- ** OfferingTransaction
    offeringTransaction_offeringStatus,
    offeringTransaction_createdOn,
    offeringTransaction_cost,
    offeringTransaction_transactionId,
    offeringTransaction_offeringPromotionId,

    -- ** Problem
    problem_job,
    problem_result,
    problem_message,
    problem_device,
    problem_run,
    problem_suite,
    problem_test,

    -- ** ProblemDetail
    problemDetail_arn,
    problemDetail_name,

    -- ** Project
    project_arn,
    project_name,
    project_created,
    project_defaultJobTimeoutMinutes,

    -- ** Radios
    radios_gps,
    radios_wifi,
    radios_bluetooth,
    radios_nfc,

    -- ** RecurringCharge
    recurringCharge_cost,
    recurringCharge_frequency,

    -- ** RemoteAccessSession
    remoteAccessSession_deviceUdid,
    remoteAccessSession_status,
    remoteAccessSession_clientId,
    remoteAccessSession_interactionMode,
    remoteAccessSession_started,
    remoteAccessSession_result,
    remoteAccessSession_message,
    remoteAccessSession_device,
    remoteAccessSession_arn,
    remoteAccessSession_name,
    remoteAccessSession_remoteRecordEnabled,
    remoteAccessSession_instanceArn,
    remoteAccessSession_billingMethod,
    remoteAccessSession_skipAppResign,
    remoteAccessSession_stopped,
    remoteAccessSession_hostAddress,
    remoteAccessSession_endpoint,
    remoteAccessSession_remoteDebugEnabled,
    remoteAccessSession_created,
    remoteAccessSession_deviceMinutes,
    remoteAccessSession_remoteRecordAppArn,

    -- ** Resolution
    resolution_height,
    resolution_width,

    -- ** Rule
    rule_operator,
    rule_attribute,
    rule_value,

    -- ** Run
    run_platform,
    run_counters,
    run_seed,
    run_eventCount,
    run_status,
    run_started,
    run_deviceSelectionResult,
    run_result,
    run_devicePoolArn,
    run_testSpecArn,
    run_message,
    run_locale,
    run_networkProfile,
    run_arn,
    run_radios,
    run_appUpload,
    run_name,
    run_billingMethod,
    run_resultCode,
    run_skipAppResign,
    run_customerArtifactPaths,
    run_completedJobs,
    run_jobTimeoutMinutes,
    run_stopped,
    run_totalJobs,
    run_webUrl,
    run_type,
    run_created,
    run_deviceMinutes,
    run_location,
    run_parsingResultUrl,

    -- ** Sample
    sample_arn,
    sample_url,
    sample_type,

    -- ** ScheduleRunConfiguration
    scheduleRunConfiguration_auxiliaryApps,
    scheduleRunConfiguration_locale,
    scheduleRunConfiguration_radios,
    scheduleRunConfiguration_billingMethod,
    scheduleRunConfiguration_customerArtifactPaths,
    scheduleRunConfiguration_vpceConfigurationArns,
    scheduleRunConfiguration_networkProfileArn,
    scheduleRunConfiguration_extraDataPackageArn,
    scheduleRunConfiguration_location,

    -- ** ScheduleRunTest
    scheduleRunTest_testPackageArn,
    scheduleRunTest_testSpecArn,
    scheduleRunTest_filter,
    scheduleRunTest_parameters,
    scheduleRunTest_type,

    -- ** Suite
    suite_counters,
    suite_status,
    suite_started,
    suite_result,
    suite_message,
    suite_arn,
    suite_name,
    suite_stopped,
    suite_type,
    suite_created,
    suite_deviceMinutes,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** Test
    test_counters,
    test_status,
    test_started,
    test_result,
    test_message,
    test_arn,
    test_name,
    test_stopped,
    test_type,
    test_created,
    test_deviceMinutes,

    -- ** TestGridProject
    testGridProject_vpcConfig,
    testGridProject_arn,
    testGridProject_name,
    testGridProject_description,
    testGridProject_created,

    -- ** TestGridSession
    testGridSession_status,
    testGridSession_arn,
    testGridSession_seleniumProperties,
    testGridSession_ended,
    testGridSession_billingMinutes,
    testGridSession_created,

    -- ** TestGridSessionAction
    testGridSessionAction_started,
    testGridSessionAction_duration,
    testGridSessionAction_statusCode,
    testGridSessionAction_action,
    testGridSessionAction_requestMethod,

    -- ** TestGridSessionArtifact
    testGridSessionArtifact_filename,
    testGridSessionArtifact_url,
    testGridSessionArtifact_type,

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
    upload_status,
    upload_contentType,
    upload_message,
    upload_category,
    upload_arn,
    upload_metadata,
    upload_name,
    upload_url,
    upload_type,
    upload_created,

    -- ** VPCEConfiguration
    vPCEConfiguration_vpceConfigurationName,
    vPCEConfiguration_vpceConfigurationDescription,
    vPCEConfiguration_arn,
    vPCEConfiguration_serviceDnsName,
    vPCEConfiguration_vpceServiceName,
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
