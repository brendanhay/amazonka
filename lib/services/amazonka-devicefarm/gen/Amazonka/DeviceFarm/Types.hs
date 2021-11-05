{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DeviceFarm.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _NotEligibleException,
    _CannotDeleteException,
    _IdempotencyException,
    _TooManyTagsException,
    _ArgumentException,
    _NotFoundException,
    _InternalServiceException,
    _TagPolicyException,
    _TagOperationException,
    _ServiceAccountException,
    _InvalidOperationException,
    _LimitExceededException,

    -- * ArtifactCategory
    ArtifactCategory (..),

    -- * ArtifactType
    ArtifactType (..),

    -- * BillingMethod
    BillingMethod (..),

    -- * CurrencyCode
    CurrencyCode (..),

    -- * DeviceAttribute
    DeviceAttribute (..),

    -- * DeviceAvailability
    DeviceAvailability (..),

    -- * DeviceFilterAttribute
    DeviceFilterAttribute (..),

    -- * DeviceFormFactor
    DeviceFormFactor (..),

    -- * DevicePlatform
    DevicePlatform (..),

    -- * DevicePoolType
    DevicePoolType (..),

    -- * ExecutionResult
    ExecutionResult (..),

    -- * ExecutionResultCode
    ExecutionResultCode (..),

    -- * ExecutionStatus
    ExecutionStatus (..),

    -- * InstanceStatus
    InstanceStatus (..),

    -- * InteractionMode
    InteractionMode (..),

    -- * NetworkProfileType
    NetworkProfileType (..),

    -- * OfferingTransactionType
    OfferingTransactionType (..),

    -- * OfferingType
    OfferingType (..),

    -- * RecurringChargeFrequency
    RecurringChargeFrequency (..),

    -- * RuleOperator
    RuleOperator (..),

    -- * SampleType
    SampleType (..),

    -- * TestGridSessionArtifactCategory
    TestGridSessionArtifactCategory (..),

    -- * TestGridSessionArtifactType
    TestGridSessionArtifactType (..),

    -- * TestGridSessionStatus
    TestGridSessionStatus (..),

    -- * TestType
    TestType (..),

    -- * UploadCategory
    UploadCategory (..),

    -- * UploadStatus
    UploadStatus (..),

    -- * UploadType
    UploadType (..),

    -- * AccountSettings
    AccountSettings (..),
    newAccountSettings,
    accountSettings_skipAppResign,
    accountSettings_awsAccountNumber,
    accountSettings_maxJobTimeoutMinutes,
    accountSettings_maxSlots,
    accountSettings_trialMinutes,
    accountSettings_unmeteredDevices,
    accountSettings_unmeteredRemoteAccessDevices,
    accountSettings_defaultJobTimeoutMinutes,

    -- * Artifact
    Artifact (..),
    newArtifact,
    artifact_arn,
    artifact_url,
    artifact_extension,
    artifact_name,
    artifact_type,

    -- * CPU
    CPU (..),
    newCPU,
    cpu_frequency,
    cpu_clock,
    cpu_architecture,

    -- * Counters
    Counters (..),
    newCounters,
    counters_passed,
    counters_skipped,
    counters_warned,
    counters_stopped,
    counters_total,
    counters_failed,
    counters_errored,

    -- * CreateRemoteAccessSessionConfiguration
    CreateRemoteAccessSessionConfiguration (..),
    newCreateRemoteAccessSessionConfiguration,
    createRemoteAccessSessionConfiguration_billingMethod,
    createRemoteAccessSessionConfiguration_vpceConfigurationArns,

    -- * CustomerArtifactPaths
    CustomerArtifactPaths (..),
    newCustomerArtifactPaths,
    customerArtifactPaths_androidPaths,
    customerArtifactPaths_deviceHostPaths,
    customerArtifactPaths_iosPaths,

    -- * Device
    Device (..),
    newDevice,
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

    -- * DeviceFilter
    DeviceFilter (..),
    newDeviceFilter,
    deviceFilter_attribute,
    deviceFilter_operator,
    deviceFilter_values,

    -- * DeviceInstance
    DeviceInstance (..),
    newDeviceInstance,
    deviceInstance_status,
    deviceInstance_udid,
    deviceInstance_instanceProfile,
    deviceInstance_arn,
    deviceInstance_deviceArn,
    deviceInstance_labels,

    -- * DeviceMinutes
    DeviceMinutes (..),
    newDeviceMinutes,
    deviceMinutes_metered,
    deviceMinutes_total,
    deviceMinutes_unmetered,

    -- * DevicePool
    DevicePool (..),
    newDevicePool,
    devicePool_arn,
    devicePool_rules,
    devicePool_name,
    devicePool_maxDevices,
    devicePool_type,
    devicePool_description,

    -- * DevicePoolCompatibilityResult
    DevicePoolCompatibilityResult (..),
    newDevicePoolCompatibilityResult,
    devicePoolCompatibilityResult_device,
    devicePoolCompatibilityResult_compatible,
    devicePoolCompatibilityResult_incompatibilityMessages,

    -- * DeviceSelectionConfiguration
    DeviceSelectionConfiguration (..),
    newDeviceSelectionConfiguration,
    deviceSelectionConfiguration_filters,
    deviceSelectionConfiguration_maxDevices,

    -- * DeviceSelectionResult
    DeviceSelectionResult (..),
    newDeviceSelectionResult,
    deviceSelectionResult_matchedDevicesCount,
    deviceSelectionResult_filters,
    deviceSelectionResult_maxDevices,

    -- * ExecutionConfiguration
    ExecutionConfiguration (..),
    newExecutionConfiguration,
    executionConfiguration_skipAppResign,
    executionConfiguration_accountsCleanup,
    executionConfiguration_appPackagesCleanup,
    executionConfiguration_jobTimeoutMinutes,
    executionConfiguration_videoCapture,

    -- * IncompatibilityMessage
    IncompatibilityMessage (..),
    newIncompatibilityMessage,
    incompatibilityMessage_type,
    incompatibilityMessage_message,

    -- * InstanceProfile
    InstanceProfile (..),
    newInstanceProfile,
    instanceProfile_arn,
    instanceProfile_rebootAfterUse,
    instanceProfile_name,
    instanceProfile_packageCleanup,
    instanceProfile_excludeAppPackagesFromCleanup,
    instanceProfile_description,

    -- * Job
    Job (..),
    newJob,
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

    -- * Location
    Location (..),
    newLocation,
    location_latitude,
    location_longitude,

    -- * MonetaryAmount
    MonetaryAmount (..),
    newMonetaryAmount,
    monetaryAmount_amount,
    monetaryAmount_currencyCode,

    -- * NetworkProfile
    NetworkProfile (..),
    newNetworkProfile,
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

    -- * Offering
    Offering (..),
    newOffering,
    offering_platform,
    offering_id,
    offering_recurringCharges,
    offering_type,
    offering_description,

    -- * OfferingPromotion
    OfferingPromotion (..),
    newOfferingPromotion,
    offeringPromotion_id,
    offeringPromotion_description,

    -- * OfferingStatus
    OfferingStatus (..),
    newOfferingStatus,
    offeringStatus_effectiveOn,
    offeringStatus_offering,
    offeringStatus_quantity,
    offeringStatus_type,

    -- * OfferingTransaction
    OfferingTransaction (..),
    newOfferingTransaction,
    offeringTransaction_offeringStatus,
    offeringTransaction_cost,
    offeringTransaction_transactionId,
    offeringTransaction_offeringPromotionId,
    offeringTransaction_createdOn,

    -- * Problem
    Problem (..),
    newProblem,
    problem_device,
    problem_test,
    problem_result,
    problem_run,
    problem_job,
    problem_message,
    problem_suite,

    -- * ProblemDetail
    ProblemDetail (..),
    newProblemDetail,
    problemDetail_arn,
    problemDetail_name,

    -- * Project
    Project (..),
    newProject,
    project_arn,
    project_created,
    project_name,
    project_defaultJobTimeoutMinutes,

    -- * Radios
    Radios (..),
    newRadios,
    radios_nfc,
    radios_gps,
    radios_bluetooth,
    radios_wifi,

    -- * RecurringCharge
    RecurringCharge (..),
    newRecurringCharge,
    recurringCharge_frequency,
    recurringCharge_cost,

    -- * RemoteAccessSession
    RemoteAccessSession (..),
    newRemoteAccessSession,
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

    -- * Resolution
    Resolution (..),
    newResolution,
    resolution_height,
    resolution_width,

    -- * Rule
    Rule (..),
    newRule,
    rule_attribute,
    rule_operator,
    rule_value,

    -- * Run
    Run (..),
    newRun,
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

    -- * Sample
    Sample (..),
    newSample,
    sample_arn,
    sample_url,
    sample_type,

    -- * ScheduleRunConfiguration
    ScheduleRunConfiguration (..),
    newScheduleRunConfiguration,
    scheduleRunConfiguration_billingMethod,
    scheduleRunConfiguration_customerArtifactPaths,
    scheduleRunConfiguration_radios,
    scheduleRunConfiguration_location,
    scheduleRunConfiguration_locale,
    scheduleRunConfiguration_networkProfileArn,
    scheduleRunConfiguration_extraDataPackageArn,
    scheduleRunConfiguration_auxiliaryApps,
    scheduleRunConfiguration_vpceConfigurationArns,

    -- * ScheduleRunTest
    ScheduleRunTest (..),
    newScheduleRunTest,
    scheduleRunTest_testSpecArn,
    scheduleRunTest_testPackageArn,
    scheduleRunTest_parameters,
    scheduleRunTest_filter,
    scheduleRunTest_type,

    -- * Suite
    Suite (..),
    newSuite,
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

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * Test
    Test (..),
    newTest,
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

    -- * TestGridProject
    TestGridProject (..),
    newTestGridProject,
    testGridProject_arn,
    testGridProject_created,
    testGridProject_name,
    testGridProject_vpcConfig,
    testGridProject_description,

    -- * TestGridSession
    TestGridSession (..),
    newTestGridSession,
    testGridSession_status,
    testGridSession_arn,
    testGridSession_created,
    testGridSession_billingMinutes,
    testGridSession_ended,
    testGridSession_seleniumProperties,

    -- * TestGridSessionAction
    TestGridSessionAction (..),
    newTestGridSessionAction,
    testGridSessionAction_action,
    testGridSessionAction_duration,
    testGridSessionAction_requestMethod,
    testGridSessionAction_started,
    testGridSessionAction_statusCode,

    -- * TestGridSessionArtifact
    TestGridSessionArtifact (..),
    newTestGridSessionArtifact,
    testGridSessionArtifact_url,
    testGridSessionArtifact_type,
    testGridSessionArtifact_filename,

    -- * TestGridVpcConfig
    TestGridVpcConfig (..),
    newTestGridVpcConfig,
    testGridVpcConfig_securityGroupIds,
    testGridVpcConfig_subnetIds,
    testGridVpcConfig_vpcId,

    -- * TrialMinutes
    TrialMinutes (..),
    newTrialMinutes,
    trialMinutes_remaining,
    trialMinutes_total,

    -- * UniqueProblem
    UniqueProblem (..),
    newUniqueProblem,
    uniqueProblem_problems,
    uniqueProblem_message,

    -- * Upload
    Upload (..),
    newUpload,
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

    -- * VPCEConfiguration
    VPCEConfiguration (..),
    newVPCEConfiguration,
    vPCEConfiguration_vpceServiceName,
    vPCEConfiguration_arn,
    vPCEConfiguration_vpceConfigurationName,
    vPCEConfiguration_serviceDnsName,
    vPCEConfiguration_vpceConfigurationDescription,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.DeviceFarm.Types.AccountSettings
import Amazonka.DeviceFarm.Types.Artifact
import Amazonka.DeviceFarm.Types.ArtifactCategory
import Amazonka.DeviceFarm.Types.ArtifactType
import Amazonka.DeviceFarm.Types.BillingMethod
import Amazonka.DeviceFarm.Types.CPU
import Amazonka.DeviceFarm.Types.Counters
import Amazonka.DeviceFarm.Types.CreateRemoteAccessSessionConfiguration
import Amazonka.DeviceFarm.Types.CurrencyCode
import Amazonka.DeviceFarm.Types.CustomerArtifactPaths
import Amazonka.DeviceFarm.Types.Device
import Amazonka.DeviceFarm.Types.DeviceAttribute
import Amazonka.DeviceFarm.Types.DeviceAvailability
import Amazonka.DeviceFarm.Types.DeviceFilter
import Amazonka.DeviceFarm.Types.DeviceFilterAttribute
import Amazonka.DeviceFarm.Types.DeviceFormFactor
import Amazonka.DeviceFarm.Types.DeviceInstance
import Amazonka.DeviceFarm.Types.DeviceMinutes
import Amazonka.DeviceFarm.Types.DevicePlatform
import Amazonka.DeviceFarm.Types.DevicePool
import Amazonka.DeviceFarm.Types.DevicePoolCompatibilityResult
import Amazonka.DeviceFarm.Types.DevicePoolType
import Amazonka.DeviceFarm.Types.DeviceSelectionConfiguration
import Amazonka.DeviceFarm.Types.DeviceSelectionResult
import Amazonka.DeviceFarm.Types.ExecutionConfiguration
import Amazonka.DeviceFarm.Types.ExecutionResult
import Amazonka.DeviceFarm.Types.ExecutionResultCode
import Amazonka.DeviceFarm.Types.ExecutionStatus
import Amazonka.DeviceFarm.Types.IncompatibilityMessage
import Amazonka.DeviceFarm.Types.InstanceProfile
import Amazonka.DeviceFarm.Types.InstanceStatus
import Amazonka.DeviceFarm.Types.InteractionMode
import Amazonka.DeviceFarm.Types.Job
import Amazonka.DeviceFarm.Types.Location
import Amazonka.DeviceFarm.Types.MonetaryAmount
import Amazonka.DeviceFarm.Types.NetworkProfile
import Amazonka.DeviceFarm.Types.NetworkProfileType
import Amazonka.DeviceFarm.Types.Offering
import Amazonka.DeviceFarm.Types.OfferingPromotion
import Amazonka.DeviceFarm.Types.OfferingStatus
import Amazonka.DeviceFarm.Types.OfferingTransaction
import Amazonka.DeviceFarm.Types.OfferingTransactionType
import Amazonka.DeviceFarm.Types.OfferingType
import Amazonka.DeviceFarm.Types.Problem
import Amazonka.DeviceFarm.Types.ProblemDetail
import Amazonka.DeviceFarm.Types.Project
import Amazonka.DeviceFarm.Types.Radios
import Amazonka.DeviceFarm.Types.RecurringCharge
import Amazonka.DeviceFarm.Types.RecurringChargeFrequency
import Amazonka.DeviceFarm.Types.RemoteAccessSession
import Amazonka.DeviceFarm.Types.Resolution
import Amazonka.DeviceFarm.Types.Rule
import Amazonka.DeviceFarm.Types.RuleOperator
import Amazonka.DeviceFarm.Types.Run
import Amazonka.DeviceFarm.Types.Sample
import Amazonka.DeviceFarm.Types.SampleType
import Amazonka.DeviceFarm.Types.ScheduleRunConfiguration
import Amazonka.DeviceFarm.Types.ScheduleRunTest
import Amazonka.DeviceFarm.Types.Suite
import Amazonka.DeviceFarm.Types.Tag
import Amazonka.DeviceFarm.Types.Test
import Amazonka.DeviceFarm.Types.TestGridProject
import Amazonka.DeviceFarm.Types.TestGridSession
import Amazonka.DeviceFarm.Types.TestGridSessionAction
import Amazonka.DeviceFarm.Types.TestGridSessionArtifact
import Amazonka.DeviceFarm.Types.TestGridSessionArtifactCategory
import Amazonka.DeviceFarm.Types.TestGridSessionArtifactType
import Amazonka.DeviceFarm.Types.TestGridSessionStatus
import Amazonka.DeviceFarm.Types.TestGridVpcConfig
import Amazonka.DeviceFarm.Types.TestType
import Amazonka.DeviceFarm.Types.TrialMinutes
import Amazonka.DeviceFarm.Types.UniqueProblem
import Amazonka.DeviceFarm.Types.Upload
import Amazonka.DeviceFarm.Types.UploadCategory
import Amazonka.DeviceFarm.Types.UploadStatus
import Amazonka.DeviceFarm.Types.UploadType
import Amazonka.DeviceFarm.Types.VPCEConfiguration
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2015-06-23@ of the Amazon Device Farm SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev = "DeviceFarm",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "devicefarm",
      Core._serviceSigningName = "devicefarm",
      Core._serviceVersion = "2015-06-23",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "DeviceFarm",
      Core._serviceRetry = retry
    }
  where
    retry =
      Core.Exponential
        { Core._retryBase = 5.0e-2,
          Core._retryGrowth = 2,
          Core._retryAttempts = 5,
          Core._retryCheck = check
        }
    check e
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Prelude.otherwise = Prelude.Nothing

-- | Exception gets thrown when a user is not eligible to perform the
-- specified transaction.
_NotEligibleException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotEligibleException =
  Core._MatchServiceError
    defaultService
    "NotEligibleException"

-- | The requested object could not be deleted.
_CannotDeleteException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CannotDeleteException =
  Core._MatchServiceError
    defaultService
    "CannotDeleteException"

-- | An entity with the same name already exists.
_IdempotencyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IdempotencyException =
  Core._MatchServiceError
    defaultService
    "IdempotencyException"

-- | The list of tags on the repository is over the limit. The maximum number
-- of tags that can be applied to a repository is 50.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | An invalid argument was specified.
_ArgumentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ArgumentException =
  Core._MatchServiceError
    defaultService
    "ArgumentException"

-- | The specified entity was not found.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"

-- | An internal exception was raised in the service. Contact
-- <mailto:aws-devicefarm-support@amazon.com aws-devicefarm-support\@amazon.com>
-- if you see this error.
_InternalServiceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServiceException =
  Core._MatchServiceError
    defaultService
    "InternalServiceException"

-- | The request doesn\'t comply with the AWS Identity and Access Management
-- (IAM) tag policy. Correct your request and then retry it.
_TagPolicyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagPolicyException =
  Core._MatchServiceError
    defaultService
    "TagPolicyException"

-- | The operation was not successful. Try again.
_TagOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagOperationException =
  Core._MatchServiceError
    defaultService
    "TagOperationException"

-- | There was a problem with the service account.
_ServiceAccountException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ServiceAccountException =
  Core._MatchServiceError
    defaultService
    "ServiceAccountException"

-- | There was an error with the update request, or you do not have
-- sufficient permissions to update this VPC endpoint configuration.
_InvalidOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InvalidOperationException =
  Core._MatchServiceError
    defaultService
    "InvalidOperationException"

-- | A limit was exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"
