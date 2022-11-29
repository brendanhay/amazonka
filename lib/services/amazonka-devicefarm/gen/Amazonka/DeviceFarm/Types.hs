{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DeviceFarm.Types
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ServiceAccountException,
    _InvalidOperationException,
    _TooManyTagsException,
    _NotFoundException,
    _ArgumentException,
    _IdempotencyException,
    _CannotDeleteException,
    _LimitExceededException,
    _TagOperationException,
    _InternalServiceException,
    _NotEligibleException,
    _TagPolicyException,

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
    accountSettings_unmeteredRemoteAccessDevices,
    accountSettings_trialMinutes,
    accountSettings_defaultJobTimeoutMinutes,
    accountSettings_maxSlots,
    accountSettings_unmeteredDevices,
    accountSettings_awsAccountNumber,
    accountSettings_maxJobTimeoutMinutes,
    accountSettings_skipAppResign,

    -- * Artifact
    Artifact (..),
    newArtifact,
    artifact_name,
    artifact_type,
    artifact_extension,
    artifact_arn,
    artifact_url,

    -- * CPU
    CPU (..),
    newCPU,
    cpu_frequency,
    cpu_clock,
    cpu_architecture,

    -- * Counters
    Counters (..),
    newCounters,
    counters_failed,
    counters_total,
    counters_warned,
    counters_errored,
    counters_skipped,
    counters_passed,
    counters_stopped,

    -- * CreateRemoteAccessSessionConfiguration
    CreateRemoteAccessSessionConfiguration (..),
    newCreateRemoteAccessSessionConfiguration,
    createRemoteAccessSessionConfiguration_vpceConfigurationArns,
    createRemoteAccessSessionConfiguration_billingMethod,

    -- * CustomerArtifactPaths
    CustomerArtifactPaths (..),
    newCustomerArtifactPaths,
    customerArtifactPaths_androidPaths,
    customerArtifactPaths_deviceHostPaths,
    customerArtifactPaths_iosPaths,

    -- * Device
    Device (..),
    newDevice,
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

    -- * DeviceFilter
    DeviceFilter (..),
    newDeviceFilter,
    deviceFilter_attribute,
    deviceFilter_operator,
    deviceFilter_values,

    -- * DeviceInstance
    DeviceInstance (..),
    newDeviceInstance,
    deviceInstance_arn,
    deviceInstance_status,
    deviceInstance_instanceProfile,
    deviceInstance_labels,
    deviceInstance_deviceArn,
    deviceInstance_udid,

    -- * DeviceMinutes
    DeviceMinutes (..),
    newDeviceMinutes,
    deviceMinutes_unmetered,
    deviceMinutes_total,
    deviceMinutes_metered,

    -- * DevicePool
    DevicePool (..),
    newDevicePool,
    devicePool_name,
    devicePool_type,
    devicePool_rules,
    devicePool_arn,
    devicePool_description,
    devicePool_maxDevices,

    -- * DevicePoolCompatibilityResult
    DevicePoolCompatibilityResult (..),
    newDevicePoolCompatibilityResult,
    devicePoolCompatibilityResult_compatible,
    devicePoolCompatibilityResult_device,
    devicePoolCompatibilityResult_incompatibilityMessages,

    -- * DeviceSelectionConfiguration
    DeviceSelectionConfiguration (..),
    newDeviceSelectionConfiguration,
    deviceSelectionConfiguration_filters,
    deviceSelectionConfiguration_maxDevices,

    -- * DeviceSelectionResult
    DeviceSelectionResult (..),
    newDeviceSelectionResult,
    deviceSelectionResult_filters,
    deviceSelectionResult_maxDevices,
    deviceSelectionResult_matchedDevicesCount,

    -- * ExecutionConfiguration
    ExecutionConfiguration (..),
    newExecutionConfiguration,
    executionConfiguration_jobTimeoutMinutes,
    executionConfiguration_videoCapture,
    executionConfiguration_accountsCleanup,
    executionConfiguration_appPackagesCleanup,
    executionConfiguration_skipAppResign,

    -- * IncompatibilityMessage
    IncompatibilityMessage (..),
    newIncompatibilityMessage,
    incompatibilityMessage_message,
    incompatibilityMessage_type,

    -- * InstanceProfile
    InstanceProfile (..),
    newInstanceProfile,
    instanceProfile_name,
    instanceProfile_excludeAppPackagesFromCleanup,
    instanceProfile_arn,
    instanceProfile_description,
    instanceProfile_packageCleanup,
    instanceProfile_rebootAfterUse,

    -- * Job
    Job (..),
    newJob,
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

    -- * Location
    Location (..),
    newLocation,
    location_latitude,
    location_longitude,

    -- * MonetaryAmount
    MonetaryAmount (..),
    newMonetaryAmount,
    monetaryAmount_currencyCode,
    monetaryAmount_amount,

    -- * NetworkProfile
    NetworkProfile (..),
    newNetworkProfile,
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

    -- * Offering
    Offering (..),
    newOffering,
    offering_type,
    offering_recurringCharges,
    offering_description,
    offering_platform,
    offering_id,

    -- * OfferingPromotion
    OfferingPromotion (..),
    newOfferingPromotion,
    offeringPromotion_description,
    offeringPromotion_id,

    -- * OfferingStatus
    OfferingStatus (..),
    newOfferingStatus,
    offeringStatus_effectiveOn,
    offeringStatus_quantity,
    offeringStatus_type,
    offeringStatus_offering,

    -- * OfferingTransaction
    OfferingTransaction (..),
    newOfferingTransaction,
    offeringTransaction_createdOn,
    offeringTransaction_offeringPromotionId,
    offeringTransaction_transactionId,
    offeringTransaction_offeringStatus,
    offeringTransaction_cost,

    -- * Problem
    Problem (..),
    newProblem,
    problem_message,
    problem_suite,
    problem_device,
    problem_run,
    problem_job,
    problem_test,
    problem_result,

    -- * ProblemDetail
    ProblemDetail (..),
    newProblemDetail,
    problemDetail_name,
    problemDetail_arn,

    -- * Project
    Project (..),
    newProject,
    project_name,
    project_vpcConfig,
    project_created,
    project_arn,
    project_defaultJobTimeoutMinutes,

    -- * Radios
    Radios (..),
    newRadios,
    radios_gps,
    radios_wifi,
    radios_bluetooth,
    radios_nfc,

    -- * RecurringCharge
    RecurringCharge (..),
    newRecurringCharge,
    recurringCharge_frequency,
    recurringCharge_cost,

    -- * RemoteAccessSession
    RemoteAccessSession (..),
    newRemoteAccessSession,
    remoteAccessSession_deviceUdid,
    remoteAccessSession_message,
    remoteAccessSession_name,
    remoteAccessSession_started,
    remoteAccessSession_clientId,
    remoteAccessSession_remoteDebugEnabled,
    remoteAccessSession_remoteRecordAppArn,
    remoteAccessSession_vpcConfig,
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

    -- * Resolution
    Resolution (..),
    newResolution,
    resolution_width,
    resolution_height,

    -- * Rule
    Rule (..),
    newRule,
    rule_attribute,
    rule_operator,
    rule_value,

    -- * Run
    Run (..),
    newRun,
    run_appUpload,
    run_message,
    run_jobTimeoutMinutes,
    run_name,
    run_type,
    run_started,
    run_customerArtifactPaths,
    run_vpcConfig,
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

    -- * Sample
    Sample (..),
    newSample,
    sample_type,
    sample_arn,
    sample_url,

    -- * ScheduleRunConfiguration
    ScheduleRunConfiguration (..),
    newScheduleRunConfiguration,
    scheduleRunConfiguration_customerArtifactPaths,
    scheduleRunConfiguration_vpceConfigurationArns,
    scheduleRunConfiguration_locale,
    scheduleRunConfiguration_radios,
    scheduleRunConfiguration_billingMethod,
    scheduleRunConfiguration_location,
    scheduleRunConfiguration_extraDataPackageArn,
    scheduleRunConfiguration_auxiliaryApps,
    scheduleRunConfiguration_networkProfileArn,

    -- * ScheduleRunTest
    ScheduleRunTest (..),
    newScheduleRunTest,
    scheduleRunTest_filter,
    scheduleRunTest_testSpecArn,
    scheduleRunTest_testPackageArn,
    scheduleRunTest_parameters,
    scheduleRunTest_type,

    -- * Suite
    Suite (..),
    newSuite,
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

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * Test
    Test (..),
    newTest,
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

    -- * TestGridProject
    TestGridProject (..),
    newTestGridProject,
    testGridProject_name,
    testGridProject_vpcConfig,
    testGridProject_created,
    testGridProject_arn,
    testGridProject_description,

    -- * TestGridSession
    TestGridSession (..),
    newTestGridSession,
    testGridSession_seleniumProperties,
    testGridSession_ended,
    testGridSession_created,
    testGridSession_arn,
    testGridSession_status,
    testGridSession_billingMinutes,

    -- * TestGridSessionAction
    TestGridSessionAction (..),
    newTestGridSessionAction,
    testGridSessionAction_requestMethod,
    testGridSessionAction_started,
    testGridSessionAction_duration,
    testGridSessionAction_action,
    testGridSessionAction_statusCode,

    -- * TestGridSessionArtifact
    TestGridSessionArtifact (..),
    newTestGridSessionArtifact,
    testGridSessionArtifact_type,
    testGridSessionArtifact_filename,
    testGridSessionArtifact_url,

    -- * TestGridVpcConfig
    TestGridVpcConfig (..),
    newTestGridVpcConfig,
    testGridVpcConfig_securityGroupIds,
    testGridVpcConfig_subnetIds,
    testGridVpcConfig_vpcId,

    -- * TrialMinutes
    TrialMinutes (..),
    newTrialMinutes,
    trialMinutes_total,
    trialMinutes_remaining,

    -- * UniqueProblem
    UniqueProblem (..),
    newUniqueProblem,
    uniqueProblem_message,
    uniqueProblem_problems,

    -- * Upload
    Upload (..),
    newUpload,
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

    -- * VPCEConfiguration
    VPCEConfiguration (..),
    newVPCEConfiguration,
    vPCEConfiguration_arn,
    vPCEConfiguration_vpceServiceName,
    vPCEConfiguration_vpceConfigurationName,
    vPCEConfiguration_vpceConfigurationDescription,
    vPCEConfiguration_serviceDnsName,

    -- * VpcConfig
    VpcConfig (..),
    newVpcConfig,
    vpcConfig_securityGroupIds,
    vpcConfig_subnetIds,
    vpcConfig_vpcId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
import Amazonka.DeviceFarm.Types.VpcConfig
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2015-06-23@ of the Amazon Device Farm SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "DeviceFarm",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "devicefarm",
      Core.signingName = "devicefarm",
      Core.version = "2015-06-23",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "DeviceFarm",
      Core.retry = retry
    }
  where
    retry =
      Core.Exponential
        { Core.base = 5.0e-2,
          Core.growth = 2,
          Core.attempts = 5,
          Core.check = check
        }
    check e
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Prelude.otherwise = Prelude.Nothing

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

-- | The list of tags on the repository is over the limit. The maximum number
-- of tags that can be applied to a repository is 50.
_TooManyTagsException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | The specified entity was not found.
_NotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"

-- | An invalid argument was specified.
_ArgumentException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ArgumentException =
  Core._MatchServiceError
    defaultService
    "ArgumentException"

-- | An entity with the same name already exists.
_IdempotencyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_IdempotencyException =
  Core._MatchServiceError
    defaultService
    "IdempotencyException"

-- | The requested object could not be deleted.
_CannotDeleteException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_CannotDeleteException =
  Core._MatchServiceError
    defaultService
    "CannotDeleteException"

-- | A limit was exceeded.
_LimitExceededException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | The operation was not successful. Try again.
_TagOperationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagOperationException =
  Core._MatchServiceError
    defaultService
    "TagOperationException"

-- | An internal exception was raised in the service. Contact
-- <mailto:aws-devicefarm-support@amazon.com aws-devicefarm-support\@amazon.com>
-- if you see this error.
_InternalServiceException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServiceException =
  Core._MatchServiceError
    defaultService
    "InternalServiceException"

-- | Exception gets thrown when a user is not eligible to perform the
-- specified transaction.
_NotEligibleException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_NotEligibleException =
  Core._MatchServiceError
    defaultService
    "NotEligibleException"

-- | The request doesn\'t comply with the AWS Identity and Access Management
-- (IAM) tag policy. Correct your request and then retry it.
_TagPolicyException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_TagPolicyException =
  Core._MatchServiceError
    defaultService
    "TagPolicyException"
