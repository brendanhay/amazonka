{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DeviceFarm.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ArgumentException,
    _CannotDeleteException,
    _IdempotencyException,
    _InternalServiceException,
    _InvalidOperationException,
    _LimitExceededException,
    _NotEligibleException,
    _NotFoundException,
    _ServiceAccountException,
    _TagOperationException,
    _TagPolicyException,
    _TooManyTagsException,

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
    accountSettings_awsAccountNumber,
    accountSettings_defaultJobTimeoutMinutes,
    accountSettings_maxJobTimeoutMinutes,
    accountSettings_maxSlots,
    accountSettings_skipAppResign,
    accountSettings_trialMinutes,
    accountSettings_unmeteredDevices,
    accountSettings_unmeteredRemoteAccessDevices,

    -- * Artifact
    Artifact (..),
    newArtifact,
    artifact_arn,
    artifact_extension,
    artifact_name,
    artifact_type,
    artifact_url,

    -- * CPU
    CPU (..),
    newCPU,
    cpu_architecture,
    cpu_clock,
    cpu_frequency,

    -- * Counters
    Counters (..),
    newCounters,
    counters_errored,
    counters_failed,
    counters_passed,
    counters_skipped,
    counters_stopped,
    counters_total,
    counters_warned,

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
    deviceInstance_deviceArn,
    deviceInstance_instanceProfile,
    deviceInstance_labels,
    deviceInstance_status,
    deviceInstance_udid,

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
    devicePool_description,
    devicePool_maxDevices,
    devicePool_name,
    devicePool_rules,
    devicePool_type,

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
    deviceSelectionResult_matchedDevicesCount,
    deviceSelectionResult_maxDevices,

    -- * ExecutionConfiguration
    ExecutionConfiguration (..),
    newExecutionConfiguration,
    executionConfiguration_accountsCleanup,
    executionConfiguration_appPackagesCleanup,
    executionConfiguration_jobTimeoutMinutes,
    executionConfiguration_skipAppResign,
    executionConfiguration_videoCapture,

    -- * IncompatibilityMessage
    IncompatibilityMessage (..),
    newIncompatibilityMessage,
    incompatibilityMessage_message,
    incompatibilityMessage_type,

    -- * InstanceProfile
    InstanceProfile (..),
    newInstanceProfile,
    instanceProfile_arn,
    instanceProfile_description,
    instanceProfile_excludeAppPackagesFromCleanup,
    instanceProfile_name,
    instanceProfile_packageCleanup,
    instanceProfile_rebootAfterUse,

    -- * Job
    Job (..),
    newJob,
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

    -- * Offering
    Offering (..),
    newOffering,
    offering_description,
    offering_id,
    offering_platform,
    offering_recurringCharges,
    offering_type,

    -- * OfferingPromotion
    OfferingPromotion (..),
    newOfferingPromotion,
    offeringPromotion_description,
    offeringPromotion_id,

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
    offeringTransaction_cost,
    offeringTransaction_createdOn,
    offeringTransaction_offeringPromotionId,
    offeringTransaction_offeringStatus,
    offeringTransaction_transactionId,

    -- * Problem
    Problem (..),
    newProblem,
    problem_device,
    problem_job,
    problem_message,
    problem_result,
    problem_run,
    problem_suite,
    problem_test,

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
    project_defaultJobTimeoutMinutes,
    project_name,
    project_vpcConfig,

    -- * Radios
    Radios (..),
    newRadios,
    radios_bluetooth,
    radios_gps,
    radios_nfc,
    radios_wifi,

    -- * RecurringCharge
    RecurringCharge (..),
    newRecurringCharge,
    recurringCharge_cost,
    recurringCharge_frequency,

    -- * RemoteAccessSession
    RemoteAccessSession (..),
    newRemoteAccessSession,
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

    -- * Sample
    Sample (..),
    newSample,
    sample_arn,
    sample_type,
    sample_url,

    -- * ScheduleRunConfiguration
    ScheduleRunConfiguration (..),
    newScheduleRunConfiguration,
    scheduleRunConfiguration_auxiliaryApps,
    scheduleRunConfiguration_billingMethod,
    scheduleRunConfiguration_customerArtifactPaths,
    scheduleRunConfiguration_extraDataPackageArn,
    scheduleRunConfiguration_locale,
    scheduleRunConfiguration_location,
    scheduleRunConfiguration_networkProfileArn,
    scheduleRunConfiguration_radios,
    scheduleRunConfiguration_vpceConfigurationArns,

    -- * ScheduleRunTest
    ScheduleRunTest (..),
    newScheduleRunTest,
    scheduleRunTest_filter,
    scheduleRunTest_parameters,
    scheduleRunTest_testPackageArn,
    scheduleRunTest_testSpecArn,
    scheduleRunTest_type,

    -- * Suite
    Suite (..),
    newSuite,
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

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * Test
    Test (..),
    newTest,
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

    -- * TestGridProject
    TestGridProject (..),
    newTestGridProject,
    testGridProject_arn,
    testGridProject_created,
    testGridProject_description,
    testGridProject_name,
    testGridProject_vpcConfig,

    -- * TestGridSession
    TestGridSession (..),
    newTestGridSession,
    testGridSession_arn,
    testGridSession_billingMinutes,
    testGridSession_created,
    testGridSession_ended,
    testGridSession_seleniumProperties,
    testGridSession_status,

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
    testGridSessionArtifact_filename,
    testGridSessionArtifact_type,
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
    trialMinutes_remaining,
    trialMinutes_total,

    -- * UniqueProblem
    UniqueProblem (..),
    newUniqueProblem,
    uniqueProblem_message,
    uniqueProblem_problems,

    -- * Upload
    Upload (..),
    newUpload,
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

    -- * VPCEConfiguration
    VPCEConfiguration (..),
    newVPCEConfiguration,
    vPCEConfiguration_arn,
    vPCEConfiguration_serviceDnsName,
    vPCEConfiguration_vpceConfigurationDescription,
    vPCEConfiguration_vpceConfigurationName,
    vPCEConfiguration_vpceServiceName,

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
      | Lens.has (Core.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Core.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has (Core.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has (Core.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has
          ( Core.hasCode "RequestThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has (Core.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has
          ( Core.hasCode "ThrottledException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has
          ( Core.hasCode "Throttling"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Lens.has
          ( Core.hasCode "ThrottlingException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Core.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Core.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Core.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Prelude.otherwise = Prelude.Nothing

-- | An invalid argument was specified.
_ArgumentException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ArgumentException =
  Core._MatchServiceError
    defaultService
    "ArgumentException"

-- | The requested object could not be deleted.
_CannotDeleteException :: Core.AsError a => Lens.Fold a Core.ServiceError
_CannotDeleteException =
  Core._MatchServiceError
    defaultService
    "CannotDeleteException"

-- | An entity with the same name already exists.
_IdempotencyException :: Core.AsError a => Lens.Fold a Core.ServiceError
_IdempotencyException =
  Core._MatchServiceError
    defaultService
    "IdempotencyException"

-- | An internal exception was raised in the service. Contact
-- <mailto:aws-devicefarm-support@amazon.com aws-devicefarm-support\@amazon.com>
-- if you see this error.
_InternalServiceException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InternalServiceException =
  Core._MatchServiceError
    defaultService
    "InternalServiceException"

-- | There was an error with the update request, or you do not have
-- sufficient permissions to update this VPC endpoint configuration.
_InvalidOperationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_InvalidOperationException =
  Core._MatchServiceError
    defaultService
    "InvalidOperationException"

-- | A limit was exceeded.
_LimitExceededException :: Core.AsError a => Lens.Fold a Core.ServiceError
_LimitExceededException =
  Core._MatchServiceError
    defaultService
    "LimitExceededException"

-- | Exception gets thrown when a user is not eligible to perform the
-- specified transaction.
_NotEligibleException :: Core.AsError a => Lens.Fold a Core.ServiceError
_NotEligibleException =
  Core._MatchServiceError
    defaultService
    "NotEligibleException"

-- | The specified entity was not found.
_NotFoundException :: Core.AsError a => Lens.Fold a Core.ServiceError
_NotFoundException =
  Core._MatchServiceError
    defaultService
    "NotFoundException"

-- | There was a problem with the service account.
_ServiceAccountException :: Core.AsError a => Lens.Fold a Core.ServiceError
_ServiceAccountException =
  Core._MatchServiceError
    defaultService
    "ServiceAccountException"

-- | The operation was not successful. Try again.
_TagOperationException :: Core.AsError a => Lens.Fold a Core.ServiceError
_TagOperationException =
  Core._MatchServiceError
    defaultService
    "TagOperationException"

-- | The request doesn\'t comply with the AWS Identity and Access Management
-- (IAM) tag policy. Correct your request and then retry it.
_TagPolicyException :: Core.AsError a => Lens.Fold a Core.ServiceError
_TagPolicyException =
  Core._MatchServiceError
    defaultService
    "TagPolicyException"

-- | The list of tags on the repository is over the limit. The maximum number
-- of tags that can be applied to a repository is 50.
_TooManyTagsException :: Core.AsError a => Lens.Fold a Core.ServiceError
_TooManyTagsException =
  Core._MatchServiceError
    defaultService
    "TooManyTagsException"
