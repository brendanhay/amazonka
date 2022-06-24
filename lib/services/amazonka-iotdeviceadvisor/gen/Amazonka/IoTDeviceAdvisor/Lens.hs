{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoTDeviceAdvisor.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTDeviceAdvisor.Lens
  ( -- * Operations

    -- ** CreateSuiteDefinition
    createSuiteDefinition_tags,
    createSuiteDefinition_suiteDefinitionConfiguration,
    createSuiteDefinitionResponse_suiteDefinitionName,
    createSuiteDefinitionResponse_suiteDefinitionArn,
    createSuiteDefinitionResponse_suiteDefinitionId,
    createSuiteDefinitionResponse_createdAt,
    createSuiteDefinitionResponse_httpStatus,

    -- ** DeleteSuiteDefinition
    deleteSuiteDefinition_suiteDefinitionId,
    deleteSuiteDefinitionResponse_httpStatus,

    -- ** GetSuiteDefinition
    getSuiteDefinition_suiteDefinitionVersion,
    getSuiteDefinition_suiteDefinitionId,
    getSuiteDefinitionResponse_tags,
    getSuiteDefinitionResponse_latestVersion,
    getSuiteDefinitionResponse_suiteDefinitionConfiguration,
    getSuiteDefinitionResponse_suiteDefinitionArn,
    getSuiteDefinitionResponse_suiteDefinitionVersion,
    getSuiteDefinitionResponse_suiteDefinitionId,
    getSuiteDefinitionResponse_createdAt,
    getSuiteDefinitionResponse_lastModifiedAt,
    getSuiteDefinitionResponse_httpStatus,

    -- ** GetSuiteRun
    getSuiteRun_suiteDefinitionId,
    getSuiteRun_suiteRunId,
    getSuiteRunResponse_tags,
    getSuiteRunResponse_errorReason,
    getSuiteRunResponse_suiteRunArn,
    getSuiteRunResponse_suiteRunConfiguration,
    getSuiteRunResponse_status,
    getSuiteRunResponse_endTime,
    getSuiteRunResponse_suiteDefinitionVersion,
    getSuiteRunResponse_testResult,
    getSuiteRunResponse_suiteDefinitionId,
    getSuiteRunResponse_startTime,
    getSuiteRunResponse_suiteRunId,
    getSuiteRunResponse_httpStatus,

    -- ** GetSuiteRunReport
    getSuiteRunReport_suiteDefinitionId,
    getSuiteRunReport_suiteRunId,
    getSuiteRunReportResponse_qualificationReportDownloadUrl,
    getSuiteRunReportResponse_httpStatus,

    -- ** ListSuiteDefinitions
    listSuiteDefinitions_nextToken,
    listSuiteDefinitions_maxResults,
    listSuiteDefinitionsResponse_suiteDefinitionInformationList,
    listSuiteDefinitionsResponse_nextToken,
    listSuiteDefinitionsResponse_httpStatus,

    -- ** ListSuiteRuns
    listSuiteRuns_nextToken,
    listSuiteRuns_maxResults,
    listSuiteRuns_suiteDefinitionVersion,
    listSuiteRuns_suiteDefinitionId,
    listSuiteRunsResponse_suiteRunsList,
    listSuiteRunsResponse_nextToken,
    listSuiteRunsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** StartSuiteRun
    startSuiteRun_tags,
    startSuiteRun_suiteRunConfiguration,
    startSuiteRun_suiteDefinitionVersion,
    startSuiteRun_suiteDefinitionId,
    startSuiteRunResponse_suiteRunArn,
    startSuiteRunResponse_createdAt,
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
    updateSuiteDefinition_suiteDefinitionConfiguration,
    updateSuiteDefinition_suiteDefinitionId,
    updateSuiteDefinitionResponse_lastUpdatedAt,
    updateSuiteDefinitionResponse_suiteDefinitionName,
    updateSuiteDefinitionResponse_suiteDefinitionArn,
    updateSuiteDefinitionResponse_suiteDefinitionVersion,
    updateSuiteDefinitionResponse_suiteDefinitionId,
    updateSuiteDefinitionResponse_createdAt,
    updateSuiteDefinitionResponse_httpStatus,

    -- * Types

    -- ** DeviceUnderTest
    deviceUnderTest_thingArn,
    deviceUnderTest_certificateArn,

    -- ** GroupResult
    groupResult_tests,
    groupResult_groupName,
    groupResult_groupId,

    -- ** SuiteDefinitionConfiguration
    suiteDefinitionConfiguration_rootGroup,
    suiteDefinitionConfiguration_devices,
    suiteDefinitionConfiguration_suiteDefinitionName,
    suiteDefinitionConfiguration_intendedForQualification,
    suiteDefinitionConfiguration_devicePermissionRoleArn,

    -- ** SuiteDefinitionInformation
    suiteDefinitionInformation_suiteDefinitionName,
    suiteDefinitionInformation_defaultDevices,
    suiteDefinitionInformation_intendedForQualification,
    suiteDefinitionInformation_suiteDefinitionId,
    suiteDefinitionInformation_createdAt,

    -- ** SuiteRunConfiguration
    suiteRunConfiguration_selectedTestList,
    suiteRunConfiguration_primaryDevice,

    -- ** SuiteRunInformation
    suiteRunInformation_failed,
    suiteRunInformation_endAt,
    suiteRunInformation_status,
    suiteRunInformation_suiteDefinitionName,
    suiteRunInformation_startedAt,
    suiteRunInformation_suiteDefinitionVersion,
    suiteRunInformation_passed,
    suiteRunInformation_suiteDefinitionId,
    suiteRunInformation_createdAt,
    suiteRunInformation_suiteRunId,

    -- ** TestCaseRun
    testCaseRun_testCaseDefinitionId,
    testCaseRun_logUrl,
    testCaseRun_status,
    testCaseRun_endTime,
    testCaseRun_failure,
    testCaseRun_testCaseDefinitionName,
    testCaseRun_warnings,
    testCaseRun_testCaseRunId,
    testCaseRun_startTime,

    -- ** TestResult
    testResult_groups,
  )
where

import Amazonka.IoTDeviceAdvisor.CreateSuiteDefinition
import Amazonka.IoTDeviceAdvisor.DeleteSuiteDefinition
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
import Amazonka.IoTDeviceAdvisor.Types.TestResult
import Amazonka.IoTDeviceAdvisor.UntagResource
import Amazonka.IoTDeviceAdvisor.UpdateSuiteDefinition
