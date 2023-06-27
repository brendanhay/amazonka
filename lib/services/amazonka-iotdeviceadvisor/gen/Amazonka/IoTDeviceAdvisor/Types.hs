{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTDeviceAdvisor.Types
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTDeviceAdvisor.Types
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    _ConflictException,
    _InternalServerException,
    _ResourceNotFoundException,
    _ValidationException,

    -- * AuthenticationMethod
    AuthenticationMethod (..),

    -- * Protocol
    Protocol (..),

    -- * Status
    Status (..),

    -- * SuiteRunStatus
    SuiteRunStatus (..),

    -- * TestCaseScenarioStatus
    TestCaseScenarioStatus (..),

    -- * TestCaseScenarioType
    TestCaseScenarioType (..),

    -- * DeviceUnderTest
    DeviceUnderTest (..),
    newDeviceUnderTest,
    deviceUnderTest_certificateArn,
    deviceUnderTest_deviceRoleArn,
    deviceUnderTest_thingArn,

    -- * GroupResult
    GroupResult (..),
    newGroupResult,
    groupResult_groupId,
    groupResult_groupName,
    groupResult_tests,

    -- * SuiteDefinitionConfiguration
    SuiteDefinitionConfiguration (..),
    newSuiteDefinitionConfiguration,
    suiteDefinitionConfiguration_devices,
    suiteDefinitionConfiguration_intendedForQualification,
    suiteDefinitionConfiguration_isLongDurationTest,
    suiteDefinitionConfiguration_protocol,
    suiteDefinitionConfiguration_suiteDefinitionName,
    suiteDefinitionConfiguration_rootGroup,
    suiteDefinitionConfiguration_devicePermissionRoleArn,

    -- * SuiteDefinitionInformation
    SuiteDefinitionInformation (..),
    newSuiteDefinitionInformation,
    suiteDefinitionInformation_createdAt,
    suiteDefinitionInformation_defaultDevices,
    suiteDefinitionInformation_intendedForQualification,
    suiteDefinitionInformation_isLongDurationTest,
    suiteDefinitionInformation_protocol,
    suiteDefinitionInformation_suiteDefinitionId,
    suiteDefinitionInformation_suiteDefinitionName,

    -- * SuiteRunConfiguration
    SuiteRunConfiguration (..),
    newSuiteRunConfiguration,
    suiteRunConfiguration_parallelRun,
    suiteRunConfiguration_selectedTestList,
    suiteRunConfiguration_primaryDevice,

    -- * SuiteRunInformation
    SuiteRunInformation (..),
    newSuiteRunInformation,
    suiteRunInformation_createdAt,
    suiteRunInformation_endAt,
    suiteRunInformation_failed,
    suiteRunInformation_passed,
    suiteRunInformation_startedAt,
    suiteRunInformation_status,
    suiteRunInformation_suiteDefinitionId,
    suiteRunInformation_suiteDefinitionName,
    suiteRunInformation_suiteDefinitionVersion,
    suiteRunInformation_suiteRunId,

    -- * TestCaseRun
    TestCaseRun (..),
    newTestCaseRun,
    testCaseRun_endTime,
    testCaseRun_failure,
    testCaseRun_logUrl,
    testCaseRun_startTime,
    testCaseRun_status,
    testCaseRun_testCaseDefinitionId,
    testCaseRun_testCaseDefinitionName,
    testCaseRun_testCaseRunId,
    testCaseRun_testScenarios,
    testCaseRun_warnings,

    -- * TestCaseScenario
    TestCaseScenario (..),
    newTestCaseScenario,
    testCaseScenario_failure,
    testCaseScenario_status,
    testCaseScenario_systemMessage,
    testCaseScenario_testCaseScenarioId,
    testCaseScenario_testCaseScenarioType,

    -- * TestResult
    TestResult (..),
    newTestResult,
    testResult_groups,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTDeviceAdvisor.Types.AuthenticationMethod
import Amazonka.IoTDeviceAdvisor.Types.DeviceUnderTest
import Amazonka.IoTDeviceAdvisor.Types.GroupResult
import Amazonka.IoTDeviceAdvisor.Types.Protocol
import Amazonka.IoTDeviceAdvisor.Types.Status
import Amazonka.IoTDeviceAdvisor.Types.SuiteDefinitionConfiguration
import Amazonka.IoTDeviceAdvisor.Types.SuiteDefinitionInformation
import Amazonka.IoTDeviceAdvisor.Types.SuiteRunConfiguration
import Amazonka.IoTDeviceAdvisor.Types.SuiteRunInformation
import Amazonka.IoTDeviceAdvisor.Types.SuiteRunStatus
import Amazonka.IoTDeviceAdvisor.Types.TestCaseRun
import Amazonka.IoTDeviceAdvisor.Types.TestCaseScenario
import Amazonka.IoTDeviceAdvisor.Types.TestCaseScenarioStatus
import Amazonka.IoTDeviceAdvisor.Types.TestCaseScenarioType
import Amazonka.IoTDeviceAdvisor.Types.TestResult
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Sign.V4 as Sign

-- | API version @2020-09-18@ of the Amazon IoT Core Device Advisor SDK configuration.
defaultService :: Core.Service
defaultService =
  Core.Service
    { Core.abbrev = "IoTDeviceAdvisor",
      Core.signer = Sign.v4,
      Core.endpointPrefix = "api.iotdeviceadvisor",
      Core.signingName = "iotdeviceadvisor",
      Core.version = "2020-09-18",
      Core.s3AddressingStyle = Core.S3AddressingStyleAuto,
      Core.endpoint = Core.defaultEndpoint defaultService,
      Core.timeout = Prelude.Just 70,
      Core.check = Core.statusSuccess,
      Core.error = Core.parseJSONError "IoTDeviceAdvisor",
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

-- | Sends a Conflict Exception.
_ConflictException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ConflictException =
  Core._MatchServiceError
    defaultService
    "ConflictException"
    Prelude.. Core.hasStatus 400

-- | Sends an Internal Failure exception.
_InternalServerException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_InternalServerException =
  Core._MatchServiceError
    defaultService
    "InternalServerException"
    Prelude.. Core.hasStatus 500

-- | Sends a Resource Not Found exception.
_ResourceNotFoundException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ResourceNotFoundException =
  Core._MatchServiceError
    defaultService
    "ResourceNotFoundException"
    Prelude.. Core.hasStatus 404

-- | Sends a validation exception.
_ValidationException :: (Core.AsError a) => Lens.Fold a Core.ServiceError
_ValidationException =
  Core._MatchServiceError
    defaultService
    "ValidationException"
    Prelude.. Core.hasStatus 400
