{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _NotEligibleException,
    _ArgumentException,
    _NotFoundException,
    _TooManyTagsException,
    _InvalidOperationException,
    _ServiceAccountException,
    _TagOperationException,
    _IdempotencyException,
    _InternalServiceException,
    _TagPolicyException,
    _CannotDeleteException,
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
    accountSettings_awsAccountNumber,
    accountSettings_maxSlots,
    accountSettings_trialMinutes,
    accountSettings_skipAppResign,
    accountSettings_maxJobTimeoutMinutes,
    accountSettings_defaultJobTimeoutMinutes,
    accountSettings_unmeteredDevices,
    accountSettings_unmeteredRemoteAccessDevices,

    -- * Artifact
    Artifact (..),
    newArtifact,
    artifact_arn,
    artifact_name,
    artifact_extension,
    artifact_url,
    artifact_type,

    -- * CPU
    CPU (..),
    newCPU,
    cpu_architecture,
    cpu_frequency,
    cpu_clock,

    -- * Counters
    Counters (..),
    newCounters,
    counters_errored,
    counters_warned,
    counters_passed,
    counters_total,
    counters_stopped,
    counters_failed,
    counters_skipped,

    -- * CreateRemoteAccessSessionConfiguration
    CreateRemoteAccessSessionConfiguration (..),
    newCreateRemoteAccessSessionConfiguration,
    createRemoteAccessSessionConfiguration_billingMethod,
    createRemoteAccessSessionConfiguration_vpceConfigurationArns,

    -- * CustomerArtifactPaths
    CustomerArtifactPaths (..),
    newCustomerArtifactPaths,
    customerArtifactPaths_deviceHostPaths,
    customerArtifactPaths_iosPaths,
    customerArtifactPaths_androidPaths,

    -- * Device
    Device (..),
    newDevice,
    device_manufacturer,
    device_platform,
    device_model,
    device_fleetName,
    device_memory,
    device_availability,
    device_fleetType,
    device_formFactor,
    device_remoteAccessEnabled,
    device_arn,
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

    -- * DeviceFilter
    DeviceFilter (..),
    newDeviceFilter,
    deviceFilter_operator,
    deviceFilter_values,
    deviceFilter_attribute,

    -- * DeviceInstance
    DeviceInstance (..),
    newDeviceInstance,
    deviceInstance_udid,
    deviceInstance_status,
    deviceInstance_deviceArn,
    deviceInstance_arn,
    deviceInstance_labels,
    deviceInstance_instanceProfile,

    -- * DeviceMinutes
    DeviceMinutes (..),
    newDeviceMinutes,
    deviceMinutes_unmetered,
    deviceMinutes_metered,
    deviceMinutes_total,

    -- * DevicePool
    DevicePool (..),
    newDevicePool,
    devicePool_rules,
    devicePool_arn,
    devicePool_name,
    devicePool_maxDevices,
    devicePool_description,
    devicePool_type,

    -- * DevicePoolCompatibilityResult
    DevicePoolCompatibilityResult (..),
    newDevicePoolCompatibilityResult,
    devicePoolCompatibilityResult_incompatibilityMessages,
    devicePoolCompatibilityResult_compatible,
    devicePoolCompatibilityResult_device,

    -- * DeviceSelectionConfiguration
    DeviceSelectionConfiguration (..),
    newDeviceSelectionConfiguration,
    deviceSelectionConfiguration_filters,
    deviceSelectionConfiguration_maxDevices,

    -- * DeviceSelectionResult
    DeviceSelectionResult (..),
    newDeviceSelectionResult,
    deviceSelectionResult_maxDevices,
    deviceSelectionResult_filters,
    deviceSelectionResult_matchedDevicesCount,

    -- * ExecutionConfiguration
    ExecutionConfiguration (..),
    newExecutionConfiguration,
    executionConfiguration_appPackagesCleanup,
    executionConfiguration_videoCapture,
    executionConfiguration_skipAppResign,
    executionConfiguration_jobTimeoutMinutes,
    executionConfiguration_accountsCleanup,

    -- * IncompatibilityMessage
    IncompatibilityMessage (..),
    newIncompatibilityMessage,
    incompatibilityMessage_message,
    incompatibilityMessage_type,

    -- * InstanceProfile
    InstanceProfile (..),
    newInstanceProfile,
    instanceProfile_excludeAppPackagesFromCleanup,
    instanceProfile_arn,
    instanceProfile_name,
    instanceProfile_description,
    instanceProfile_rebootAfterUse,
    instanceProfile_packageCleanup,

    -- * Job
    Job (..),
    newJob,
    job_counters,
    job_status,
    job_result,
    job_started,
    job_message,
    job_device,
    job_videoCapture,
    job_arn,
    job_videoEndpoint,
    job_name,
    job_instanceArn,
    job_stopped,
    job_created,
    job_type,
    job_deviceMinutes,

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
    networkProfile_downlinkDelayMs,
    networkProfile_downlinkBandwidthBits,
    networkProfile_downlinkJitterMs,
    networkProfile_uplinkLossPercent,
    networkProfile_arn,
    networkProfile_downlinkLossPercent,
    networkProfile_name,
    networkProfile_description,
    networkProfile_uplinkDelayMs,
    networkProfile_uplinkBandwidthBits,
    networkProfile_type,

    -- * Offering
    Offering (..),
    newOffering,
    offering_platform,
    offering_id,
    offering_description,
    offering_recurringCharges,
    offering_type,

    -- * OfferingPromotion
    OfferingPromotion (..),
    newOfferingPromotion,
    offeringPromotion_id,
    offeringPromotion_description,

    -- * OfferingStatus
    OfferingStatus (..),
    newOfferingStatus,
    offeringStatus_quantity,
    offeringStatus_offering,
    offeringStatus_effectiveOn,
    offeringStatus_type,

    -- * OfferingTransaction
    OfferingTransaction (..),
    newOfferingTransaction,
    offeringTransaction_offeringStatus,
    offeringTransaction_createdOn,
    offeringTransaction_cost,
    offeringTransaction_transactionId,
    offeringTransaction_offeringPromotionId,

    -- * Problem
    Problem (..),
    newProblem,
    problem_job,
    problem_result,
    problem_message,
    problem_device,
    problem_run,
    problem_test,
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
    project_name,
    project_created,
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
    recurringCharge_cost,
    recurringCharge_frequency,

    -- * RemoteAccessSession
    RemoteAccessSession (..),
    newRemoteAccessSession,
    remoteAccessSession_deviceUdid,
    remoteAccessSession_clientId,
    remoteAccessSession_status,
    remoteAccessSession_result,
    remoteAccessSession_interactionMode,
    remoteAccessSession_started,
    remoteAccessSession_message,
    remoteAccessSession_device,
    remoteAccessSession_arn,
    remoteAccessSession_name,
    remoteAccessSession_instanceArn,
    remoteAccessSession_billingMethod,
    remoteAccessSession_remoteRecordEnabled,
    remoteAccessSession_skipAppResign,
    remoteAccessSession_stopped,
    remoteAccessSession_hostAddress,
    remoteAccessSession_endpoint,
    remoteAccessSession_created,
    remoteAccessSession_remoteDebugEnabled,
    remoteAccessSession_remoteRecordAppArn,
    remoteAccessSession_deviceMinutes,

    -- * Resolution
    Resolution (..),
    newResolution,
    resolution_height,
    resolution_width,

    -- * Rule
    Rule (..),
    newRule,
    rule_operator,
    rule_attribute,
    rule_value,

    -- * Run
    Run (..),
    newRun,
    run_seed,
    run_eventCount,
    run_counters,
    run_platform,
    run_status,
    run_result,
    run_devicePoolArn,
    run_deviceSelectionResult,
    run_started,
    run_testSpecArn,
    run_message,
    run_locale,
    run_arn,
    run_networkProfile,
    run_appUpload,
    run_radios,
    run_name,
    run_billingMethod,
    run_customerArtifactPaths,
    run_resultCode,
    run_skipAppResign,
    run_completedJobs,
    run_stopped,
    run_jobTimeoutMinutes,
    run_totalJobs,
    run_webUrl,
    run_created,
    run_type,
    run_deviceMinutes,
    run_location,
    run_parsingResultUrl,

    -- * Sample
    Sample (..),
    newSample,
    sample_arn,
    sample_url,
    sample_type,

    -- * ScheduleRunConfiguration
    ScheduleRunConfiguration (..),
    newScheduleRunConfiguration,
    scheduleRunConfiguration_locale,
    scheduleRunConfiguration_auxiliaryApps,
    scheduleRunConfiguration_radios,
    scheduleRunConfiguration_billingMethod,
    scheduleRunConfiguration_customerArtifactPaths,
    scheduleRunConfiguration_vpceConfigurationArns,
    scheduleRunConfiguration_networkProfileArn,
    scheduleRunConfiguration_location,
    scheduleRunConfiguration_extraDataPackageArn,

    -- * ScheduleRunTest
    ScheduleRunTest (..),
    newScheduleRunTest,
    scheduleRunTest_testPackageArn,
    scheduleRunTest_testSpecArn,
    scheduleRunTest_filter,
    scheduleRunTest_parameters,
    scheduleRunTest_type,

    -- * Suite
    Suite (..),
    newSuite,
    suite_counters,
    suite_status,
    suite_result,
    suite_started,
    suite_message,
    suite_arn,
    suite_name,
    suite_stopped,
    suite_created,
    suite_type,
    suite_deviceMinutes,

    -- * Tag
    Tag (..),
    newTag,
    tag_key,
    tag_value,

    -- * Test
    Test (..),
    newTest,
    test_counters,
    test_status,
    test_result,
    test_started,
    test_message,
    test_arn,
    test_name,
    test_stopped,
    test_created,
    test_type,
    test_deviceMinutes,

    -- * TestGridProject
    TestGridProject (..),
    newTestGridProject,
    testGridProject_arn,
    testGridProject_name,
    testGridProject_description,
    testGridProject_created,

    -- * TestGridSession
    TestGridSession (..),
    newTestGridSession,
    testGridSession_status,
    testGridSession_arn,
    testGridSession_seleniumProperties,
    testGridSession_billingMinutes,
    testGridSession_ended,
    testGridSession_created,

    -- * TestGridSessionAction
    TestGridSessionAction (..),
    newTestGridSessionAction,
    testGridSessionAction_started,
    testGridSessionAction_duration,
    testGridSessionAction_statusCode,
    testGridSessionAction_action,
    testGridSessionAction_requestMethod,

    -- * TestGridSessionArtifact
    TestGridSessionArtifact (..),
    newTestGridSessionArtifact,
    testGridSessionArtifact_filename,
    testGridSessionArtifact_url,
    testGridSessionArtifact_type,

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
    upload_status,
    upload_contentType,
    upload_message,
    upload_category,
    upload_metadata,
    upload_arn,
    upload_name,
    upload_url,
    upload_created,
    upload_type,

    -- * VPCEConfiguration
    VPCEConfiguration (..),
    newVPCEConfiguration,
    vPCEConfiguration_vpceConfigurationName,
    vPCEConfiguration_vpceConfigurationDescription,
    vPCEConfiguration_arn,
    vPCEConfiguration_serviceDnsName,
    vPCEConfiguration_vpceServiceName,
  )
where

import Network.AWS.DeviceFarm.Types.AccountSettings
import Network.AWS.DeviceFarm.Types.Artifact
import Network.AWS.DeviceFarm.Types.ArtifactCategory
import Network.AWS.DeviceFarm.Types.ArtifactType
import Network.AWS.DeviceFarm.Types.BillingMethod
import Network.AWS.DeviceFarm.Types.CPU
import Network.AWS.DeviceFarm.Types.Counters
import Network.AWS.DeviceFarm.Types.CreateRemoteAccessSessionConfiguration
import Network.AWS.DeviceFarm.Types.CurrencyCode
import Network.AWS.DeviceFarm.Types.CustomerArtifactPaths
import Network.AWS.DeviceFarm.Types.Device
import Network.AWS.DeviceFarm.Types.DeviceAttribute
import Network.AWS.DeviceFarm.Types.DeviceAvailability
import Network.AWS.DeviceFarm.Types.DeviceFilter
import Network.AWS.DeviceFarm.Types.DeviceFilterAttribute
import Network.AWS.DeviceFarm.Types.DeviceFormFactor
import Network.AWS.DeviceFarm.Types.DeviceInstance
import Network.AWS.DeviceFarm.Types.DeviceMinutes
import Network.AWS.DeviceFarm.Types.DevicePlatform
import Network.AWS.DeviceFarm.Types.DevicePool
import Network.AWS.DeviceFarm.Types.DevicePoolCompatibilityResult
import Network.AWS.DeviceFarm.Types.DevicePoolType
import Network.AWS.DeviceFarm.Types.DeviceSelectionConfiguration
import Network.AWS.DeviceFarm.Types.DeviceSelectionResult
import Network.AWS.DeviceFarm.Types.ExecutionConfiguration
import Network.AWS.DeviceFarm.Types.ExecutionResult
import Network.AWS.DeviceFarm.Types.ExecutionResultCode
import Network.AWS.DeviceFarm.Types.ExecutionStatus
import Network.AWS.DeviceFarm.Types.IncompatibilityMessage
import Network.AWS.DeviceFarm.Types.InstanceProfile
import Network.AWS.DeviceFarm.Types.InstanceStatus
import Network.AWS.DeviceFarm.Types.InteractionMode
import Network.AWS.DeviceFarm.Types.Job
import Network.AWS.DeviceFarm.Types.Location
import Network.AWS.DeviceFarm.Types.MonetaryAmount
import Network.AWS.DeviceFarm.Types.NetworkProfile
import Network.AWS.DeviceFarm.Types.NetworkProfileType
import Network.AWS.DeviceFarm.Types.Offering
import Network.AWS.DeviceFarm.Types.OfferingPromotion
import Network.AWS.DeviceFarm.Types.OfferingStatus
import Network.AWS.DeviceFarm.Types.OfferingTransaction
import Network.AWS.DeviceFarm.Types.OfferingTransactionType
import Network.AWS.DeviceFarm.Types.OfferingType
import Network.AWS.DeviceFarm.Types.Problem
import Network.AWS.DeviceFarm.Types.ProblemDetail
import Network.AWS.DeviceFarm.Types.Project
import Network.AWS.DeviceFarm.Types.Radios
import Network.AWS.DeviceFarm.Types.RecurringCharge
import Network.AWS.DeviceFarm.Types.RecurringChargeFrequency
import Network.AWS.DeviceFarm.Types.RemoteAccessSession
import Network.AWS.DeviceFarm.Types.Resolution
import Network.AWS.DeviceFarm.Types.Rule
import Network.AWS.DeviceFarm.Types.RuleOperator
import Network.AWS.DeviceFarm.Types.Run
import Network.AWS.DeviceFarm.Types.Sample
import Network.AWS.DeviceFarm.Types.SampleType
import Network.AWS.DeviceFarm.Types.ScheduleRunConfiguration
import Network.AWS.DeviceFarm.Types.ScheduleRunTest
import Network.AWS.DeviceFarm.Types.Suite
import Network.AWS.DeviceFarm.Types.Tag
import Network.AWS.DeviceFarm.Types.Test
import Network.AWS.DeviceFarm.Types.TestGridProject
import Network.AWS.DeviceFarm.Types.TestGridSession
import Network.AWS.DeviceFarm.Types.TestGridSessionAction
import Network.AWS.DeviceFarm.Types.TestGridSessionArtifact
import Network.AWS.DeviceFarm.Types.TestGridSessionArtifactCategory
import Network.AWS.DeviceFarm.Types.TestGridSessionArtifactType
import Network.AWS.DeviceFarm.Types.TestGridSessionStatus
import Network.AWS.DeviceFarm.Types.TestType
import Network.AWS.DeviceFarm.Types.TrialMinutes
import Network.AWS.DeviceFarm.Types.UniqueProblem
import Network.AWS.DeviceFarm.Types.Upload
import Network.AWS.DeviceFarm.Types.UploadCategory
import Network.AWS.DeviceFarm.Types.UploadStatus
import Network.AWS.DeviceFarm.Types.UploadType
import Network.AWS.DeviceFarm.Types.VPCEConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Sign.V4 as Sign

-- | API version @2015-06-23@ of the Amazon Device Farm SDK configuration.
defaultService :: Prelude.Service
defaultService =
  Prelude.Service
    { Prelude._svcAbbrev = "DeviceFarm",
      Prelude._svcSigner = Sign.v4,
      Prelude._svcEndpointPrefix = "devicefarm",
      Prelude._svcSigningName = "devicefarm",
      Prelude._svcVersion = "2015-06-23",
      Prelude._svcEndpoint =
        Prelude.defaultEndpoint defaultService,
      Prelude._svcTimeout = Prelude.Just 70,
      Prelude._svcCheck = Prelude.statusSuccess,
      Prelude._svcError =
        Prelude.parseJSONError "DeviceFarm",
      Prelude._svcRetry = retry
    }
  where
    retry =
      Prelude.Exponential
        { Prelude._retryBase = 5.0e-2,
          Prelude._retryGrowth = 2,
          Prelude._retryAttempts = 5,
          Prelude._retryCheck = check
        }
    check e
      | Lens.has (Prelude.hasStatus 504) e =
        Prelude.Just "gateway_timeout"
      | Lens.has
          ( Prelude.hasCode
              "ProvisionedThroughputExceededException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throughput_exceeded"
      | Lens.has (Prelude.hasStatus 503) e =
        Prelude.Just "service_unavailable"
      | Lens.has (Prelude.hasStatus 502) e =
        Prelude.Just "bad_gateway"
      | Lens.has (Prelude.hasStatus 429) e =
        Prelude.Just "too_many_requests"
      | Lens.has
          ( Prelude.hasCode "RequestThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "request_throttled_exception"
      | Lens.has
          ( Prelude.hasCode "ThrottledException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttled_exception"
      | Lens.has (Prelude.hasStatus 509) e =
        Prelude.Just "limit_exceeded"
      | Lens.has (Prelude.hasStatus 500) e =
        Prelude.Just "general_server_error"
      | Lens.has
          ( Prelude.hasCode "ThrottlingException"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling_exception"
      | Lens.has
          ( Prelude.hasCode "Throttling"
              Prelude.. Prelude.hasStatus 400
          )
          e =
        Prelude.Just "throttling"
      | Prelude.otherwise = Prelude.Nothing

-- | Exception gets thrown when a user is not eligible to perform the
-- specified transaction.
_NotEligibleException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NotEligibleException =
  Prelude._MatchServiceError
    defaultService
    "NotEligibleException"

-- | An invalid argument was specified.
_ArgumentException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ArgumentException =
  Prelude._MatchServiceError
    defaultService
    "ArgumentException"

-- | The specified entity was not found.
_NotFoundException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_NotFoundException =
  Prelude._MatchServiceError
    defaultService
    "NotFoundException"

-- | The list of tags on the repository is over the limit. The maximum number
-- of tags that can be applied to a repository is 50.
_TooManyTagsException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TooManyTagsException =
  Prelude._MatchServiceError
    defaultService
    "TooManyTagsException"

-- | There was an error with the update request, or you do not have
-- sufficient permissions to update this VPC endpoint configuration.
_InvalidOperationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InvalidOperationException =
  Prelude._MatchServiceError
    defaultService
    "InvalidOperationException"

-- | There was a problem with the service account.
_ServiceAccountException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_ServiceAccountException =
  Prelude._MatchServiceError
    defaultService
    "ServiceAccountException"

-- | The operation was not successful. Try again.
_TagOperationException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TagOperationException =
  Prelude._MatchServiceError
    defaultService
    "TagOperationException"

-- | An entity with the same name already exists.
_IdempotencyException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_IdempotencyException =
  Prelude._MatchServiceError
    defaultService
    "IdempotencyException"

-- | An internal exception was raised in the service. Contact
-- <mailto:aws-devicefarm-support@amazon.com aws-devicefarm-support\@amazon.com>
-- if you see this error.
_InternalServiceException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_InternalServiceException =
  Prelude._MatchServiceError
    defaultService
    "InternalServiceException"

-- | The request doesn\'t comply with the AWS Identity and Access Management
-- (IAM) tag policy. Correct your request and then retry it.
_TagPolicyException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_TagPolicyException =
  Prelude._MatchServiceError
    defaultService
    "TagPolicyException"

-- | The requested object could not be deleted.
_CannotDeleteException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_CannotDeleteException =
  Prelude._MatchServiceError
    defaultService
    "CannotDeleteException"

-- | A limit was exceeded.
_LimitExceededException :: Prelude.AsError a => Lens.Getting (Prelude.First Prelude.ServiceError) a Prelude.ServiceError
_LimitExceededException =
  Prelude._MatchServiceError
    defaultService
    "LimitExceededException"
