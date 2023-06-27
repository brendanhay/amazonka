{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTDeviceAdvisor.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTDeviceAdvisor.Lens
  ( -- * Operations

    -- ** CreateSuiteDefinition
    createSuiteDefinition_tags,
    createSuiteDefinition_suiteDefinitionConfiguration,
    createSuiteDefinitionResponse_createdAt,
    createSuiteDefinitionResponse_suiteDefinitionArn,
    createSuiteDefinitionResponse_suiteDefinitionId,
    createSuiteDefinitionResponse_suiteDefinitionName,
    createSuiteDefinitionResponse_httpStatus,

    -- ** DeleteSuiteDefinition
    deleteSuiteDefinition_suiteDefinitionId,
    deleteSuiteDefinitionResponse_httpStatus,

    -- ** GetEndpoint
    getEndpoint_authenticationMethod,
    getEndpoint_certificateArn,
    getEndpoint_deviceRoleArn,
    getEndpoint_thingArn,
    getEndpointResponse_endpoint,
    getEndpointResponse_httpStatus,

    -- ** GetSuiteDefinition
    getSuiteDefinition_suiteDefinitionVersion,
    getSuiteDefinition_suiteDefinitionId,
    getSuiteDefinitionResponse_createdAt,
    getSuiteDefinitionResponse_lastModifiedAt,
    getSuiteDefinitionResponse_latestVersion,
    getSuiteDefinitionResponse_suiteDefinitionArn,
    getSuiteDefinitionResponse_suiteDefinitionConfiguration,
    getSuiteDefinitionResponse_suiteDefinitionId,
    getSuiteDefinitionResponse_suiteDefinitionVersion,
    getSuiteDefinitionResponse_tags,
    getSuiteDefinitionResponse_httpStatus,

    -- ** GetSuiteRun
    getSuiteRun_suiteDefinitionId,
    getSuiteRun_suiteRunId,
    getSuiteRunResponse_endTime,
    getSuiteRunResponse_errorReason,
    getSuiteRunResponse_startTime,
    getSuiteRunResponse_status,
    getSuiteRunResponse_suiteDefinitionId,
    getSuiteRunResponse_suiteDefinitionVersion,
    getSuiteRunResponse_suiteRunArn,
    getSuiteRunResponse_suiteRunConfiguration,
    getSuiteRunResponse_suiteRunId,
    getSuiteRunResponse_tags,
    getSuiteRunResponse_testResult,
    getSuiteRunResponse_httpStatus,

    -- ** GetSuiteRunReport
    getSuiteRunReport_suiteDefinitionId,
    getSuiteRunReport_suiteRunId,
    getSuiteRunReportResponse_qualificationReportDownloadUrl,
    getSuiteRunReportResponse_httpStatus,

    -- ** ListSuiteDefinitions
    listSuiteDefinitions_maxResults,
    listSuiteDefinitions_nextToken,
    listSuiteDefinitionsResponse_nextToken,
    listSuiteDefinitionsResponse_suiteDefinitionInformationList,
    listSuiteDefinitionsResponse_httpStatus,

    -- ** ListSuiteRuns
    listSuiteRuns_maxResults,
    listSuiteRuns_nextToken,
    listSuiteRuns_suiteDefinitionId,
    listSuiteRuns_suiteDefinitionVersion,
    listSuiteRunsResponse_nextToken,
    listSuiteRunsResponse_suiteRunsList,
    listSuiteRunsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** StartSuiteRun
    startSuiteRun_suiteDefinitionVersion,
    startSuiteRun_tags,
    startSuiteRun_suiteDefinitionId,
    startSuiteRun_suiteRunConfiguration,
    startSuiteRunResponse_createdAt,
    startSuiteRunResponse_endpoint,
    startSuiteRunResponse_suiteRunArn,
    startSuiteRunResponse_suiteRunId,
    startSuiteRunResponse_httpStatus,

    -- ** StopSuiteRun
    stopSuiteRun_suiteDefinitionId,
    stopSuiteRun_suiteRunId,
    stopSuiteRunResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateSuiteDefinition
    updateSuiteDefinition_suiteDefinitionId,
    updateSuiteDefinition_suiteDefinitionConfiguration,
    updateSuiteDefinitionResponse_createdAt,
    updateSuiteDefinitionResponse_lastUpdatedAt,
    updateSuiteDefinitionResponse_suiteDefinitionArn,
    updateSuiteDefinitionResponse_suiteDefinitionId,
    updateSuiteDefinitionResponse_suiteDefinitionName,
    updateSuiteDefinitionResponse_suiteDefinitionVersion,
    updateSuiteDefinitionResponse_httpStatus,

    -- * Types

    -- ** DeviceUnderTest
    deviceUnderTest_certificateArn,
    deviceUnderTest_deviceRoleArn,
    deviceUnderTest_thingArn,

    -- ** GroupResult
    groupResult_groupId,
    groupResult_groupName,
    groupResult_tests,

    -- ** SuiteDefinitionConfiguration
    suiteDefinitionConfiguration_devices,
    suiteDefinitionConfiguration_intendedForQualification,
    suiteDefinitionConfiguration_isLongDurationTest,
    suiteDefinitionConfiguration_protocol,
    suiteDefinitionConfiguration_suiteDefinitionName,
    suiteDefinitionConfiguration_rootGroup,
    suiteDefinitionConfiguration_devicePermissionRoleArn,

    -- ** SuiteDefinitionInformation
    suiteDefinitionInformation_createdAt,
    suiteDefinitionInformation_defaultDevices,
    suiteDefinitionInformation_intendedForQualification,
    suiteDefinitionInformation_isLongDurationTest,
    suiteDefinitionInformation_protocol,
    suiteDefinitionInformation_suiteDefinitionId,
    suiteDefinitionInformation_suiteDefinitionName,

    -- ** SuiteRunConfiguration
    suiteRunConfiguration_parallelRun,
    suiteRunConfiguration_selectedTestList,
    suiteRunConfiguration_primaryDevice,

    -- ** SuiteRunInformation
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

    -- ** TestCaseRun
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

    -- ** TestCaseScenario
    testCaseScenario_failure,
    testCaseScenario_status,
    testCaseScenario_systemMessage,
    testCaseScenario_testCaseScenarioId,
    testCaseScenario_testCaseScenarioType,

    -- ** TestResult
    testResult_groups,
  )
where

import Amazonka.IoTDeviceAdvisor.CreateSuiteDefinition
import Amazonka.IoTDeviceAdvisor.DeleteSuiteDefinition
import Amazonka.IoTDeviceAdvisor.GetEndpoint
import Amazonka.IoTDeviceAdvisor.GetSuiteDefinition
import Amazonka.IoTDeviceAdvisor.GetSuiteRun
import Amazonka.IoTDeviceAdvisor.GetSuiteRunReport
import Amazonka.IoTDeviceAdvisor.ListSuiteDefinitions
import Amazonka.IoTDeviceAdvisor.ListSuiteRuns
import Amazonka.IoTDeviceAdvisor.ListTagsForResource
import Amazonka.IoTDeviceAdvisor.StartSuiteRun
import Amazonka.IoTDeviceAdvisor.StopSuiteRun
import Amazonka.IoTDeviceAdvisor.TagResource
import Amazonka.IoTDeviceAdvisor.Types.DeviceUnderTest
import Amazonka.IoTDeviceAdvisor.Types.GroupResult
import Amazonka.IoTDeviceAdvisor.Types.SuiteDefinitionConfiguration
import Amazonka.IoTDeviceAdvisor.Types.SuiteDefinitionInformation
import Amazonka.IoTDeviceAdvisor.Types.SuiteRunConfiguration
import Amazonka.IoTDeviceAdvisor.Types.SuiteRunInformation
import Amazonka.IoTDeviceAdvisor.Types.TestCaseRun
import Amazonka.IoTDeviceAdvisor.Types.TestCaseScenario
import Amazonka.IoTDeviceAdvisor.Types.TestResult
import Amazonka.IoTDeviceAdvisor.UntagResource
import Amazonka.IoTDeviceAdvisor.UpdateSuiteDefinition
