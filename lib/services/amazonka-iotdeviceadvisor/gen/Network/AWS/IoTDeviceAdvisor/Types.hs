{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTDeviceAdvisor.Types
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTDeviceAdvisor.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ValidationException,
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,

    -- * Status
    Status (..),

    -- * SuiteRunStatus
    SuiteRunStatus (..),

    -- * DeviceUnderTest
    DeviceUnderTest (..),
    newDeviceUnderTest,
    deviceUnderTest_certificateArn,
    deviceUnderTest_thingArn,

    -- * GroupResult
    GroupResult (..),
    newGroupResult,
    groupResult_tests,
    groupResult_groupId,
    groupResult_groupName,

    -- * SuiteDefinitionConfiguration
    SuiteDefinitionConfiguration (..),
    newSuiteDefinitionConfiguration,
    suiteDefinitionConfiguration_suiteDefinitionName,
    suiteDefinitionConfiguration_intendedForQualification,
    suiteDefinitionConfiguration_devicePermissionRoleArn,
    suiteDefinitionConfiguration_devices,
    suiteDefinitionConfiguration_rootGroup,

    -- * SuiteDefinitionInformation
    SuiteDefinitionInformation (..),
    newSuiteDefinitionInformation,
    suiteDefinitionInformation_createdAt,
    suiteDefinitionInformation_defaultDevices,
    suiteDefinitionInformation_suiteDefinitionId,
    suiteDefinitionInformation_suiteDefinitionName,
    suiteDefinitionInformation_intendedForQualification,

    -- * SuiteRunConfiguration
    SuiteRunConfiguration (..),
    newSuiteRunConfiguration,
    suiteRunConfiguration_primaryDevice,
    suiteRunConfiguration_selectedTestList,

    -- * SuiteRunInformation
    SuiteRunInformation (..),
    newSuiteRunInformation,
    suiteRunInformation_status,
    suiteRunInformation_createdAt,
    suiteRunInformation_passed,
    suiteRunInformation_suiteDefinitionId,
    suiteRunInformation_suiteDefinitionVersion,
    suiteRunInformation_startedAt,
    suiteRunInformation_suiteDefinitionName,
    suiteRunInformation_endAt,
    suiteRunInformation_suiteRunId,
    suiteRunInformation_failed,

    -- * TestCaseRun
    TestCaseRun (..),
    newTestCaseRun,
    testCaseRun_status,
    testCaseRun_logUrl,
    testCaseRun_startTime,
    testCaseRun_testCaseRunId,
    testCaseRun_warnings,
    testCaseRun_endTime,
    testCaseRun_testCaseDefinitionId,
    testCaseRun_failure,
    testCaseRun_testCaseDefinitionName,

    -- * TestResult
    TestResult (..),
    newTestResult,
    testResult_groups,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTDeviceAdvisor.Types.DeviceUnderTest
import Amazonka.IoTDeviceAdvisor.Types.GroupResult
import Amazonka.IoTDeviceAdvisor.Types.Status
import Amazonka.IoTDeviceAdvisor.Types.SuiteDefinitionConfiguration
import Amazonka.IoTDeviceAdvisor.Types.SuiteDefinitionInformation
import Amazonka.IoTDeviceAdvisor.Types.SuiteRunConfiguration
import Amazonka.IoTDeviceAdvisor.Types.SuiteRunInformation
import Amazonka.IoTDeviceAdvisor.Types.SuiteRunStatus
import Amazonka.IoTDeviceAdvisor.Types.TestCaseRun
import Amazonka.IoTDeviceAdvisor.Types.TestResult
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-09-18@ of the Amazon IoT Core Device Advisor SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core._serviceAbbrev =
        "IoTDeviceAdvisor",
      Core._serviceSigner = Sign.v4,
      Core._serviceEndpointPrefix = "api.iotdeviceadvisor",
      Core._serviceSigningName = "iotdeviceadvisor",
      Core._serviceVersion = "2020-09-18",
      Core._serviceEndpoint =
        Core.defaultEndpoint defaultService,
      Core._serviceTimeout = Prelude.Just 70,
      Core._serviceCheck = Core.statusSuccess,
      Core._serviceError =
        Core.parseJSONError "IoTDeviceAdvisor",
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

-- | Sends invalid request exception.
_ValidationException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400

-- | Sends Conflict Exception.
_ConflictException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 400

-- | Sends Internal Failure Exception.
_InternalServerException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | Sends Resource Not Found Exception.
_ResourceNotFoundException :: Core.AsError a => Lens.Getting (Prelude.First Core.ServiceError) a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404
