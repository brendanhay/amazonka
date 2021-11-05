{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTDeviceAdvisor.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTDeviceAdvisor.Lens
  ( -- * Operations

    -- ** GetSuiteRunReport
    getSuiteRunReport_suiteDefinitionId,
    getSuiteRunReport_suiteRunId,
    getSuiteRunReportResponse_qualificationReportDownloadUrl,
    getSuiteRunReportResponse_httpStatus,

    -- ** StartSuiteRun
    startSuiteRun_suiteRunConfiguration,
    startSuiteRun_suiteDefinitionVersion,
    startSuiteRun_tags,
    startSuiteRun_suiteDefinitionId,
    startSuiteRunResponse_createdAt,
    startSuiteRunResponse_suiteRunArn,
    startSuiteRunResponse_suiteRunId,
    startSuiteRunResponse_httpStatus,

    -- ** ListSuiteDefinitions
    listSuiteDefinitions_nextToken,
    listSuiteDefinitions_maxResults,
    listSuiteDefinitionsResponse_suiteDefinitionInformationList,
    listSuiteDefinitionsResponse_nextToken,
    listSuiteDefinitionsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** DeleteSuiteDefinition
    deleteSuiteDefinition_suiteDefinitionId,
    deleteSuiteDefinitionResponse_httpStatus,

    -- ** UpdateSuiteDefinition
    updateSuiteDefinition_suiteDefinitionConfiguration,
    updateSuiteDefinition_suiteDefinitionId,
    updateSuiteDefinitionResponse_lastUpdatedAt,
    updateSuiteDefinitionResponse_createdAt,
    updateSuiteDefinitionResponse_suiteDefinitionArn,
    updateSuiteDefinitionResponse_suiteDefinitionId,
    updateSuiteDefinitionResponse_suiteDefinitionVersion,
    updateSuiteDefinitionResponse_suiteDefinitionName,
    updateSuiteDefinitionResponse_httpStatus,

    -- ** CreateSuiteDefinition
    createSuiteDefinition_suiteDefinitionConfiguration,
    createSuiteDefinition_tags,
    createSuiteDefinitionResponse_createdAt,
    createSuiteDefinitionResponse_suiteDefinitionArn,
    createSuiteDefinitionResponse_suiteDefinitionId,
    createSuiteDefinitionResponse_suiteDefinitionName,
    createSuiteDefinitionResponse_httpStatus,

    -- ** StopSuiteRun
    stopSuiteRun_suiteDefinitionId,
    stopSuiteRun_suiteRunId,
    stopSuiteRunResponse_httpStatus,

    -- ** GetSuiteDefinition
    getSuiteDefinition_suiteDefinitionVersion,
    getSuiteDefinition_suiteDefinitionId,
    getSuiteDefinitionResponse_createdAt,
    getSuiteDefinitionResponse_suiteDefinitionConfiguration,
    getSuiteDefinitionResponse_suiteDefinitionArn,
    getSuiteDefinitionResponse_lastModifiedAt,
    getSuiteDefinitionResponse_suiteDefinitionId,
    getSuiteDefinitionResponse_suiteDefinitionVersion,
    getSuiteDefinitionResponse_latestVersion,
    getSuiteDefinitionResponse_tags,
    getSuiteDefinitionResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** ListSuiteRuns
    listSuiteRuns_suiteDefinitionId,
    listSuiteRuns_suiteDefinitionVersion,
    listSuiteRuns_nextToken,
    listSuiteRuns_maxResults,
    listSuiteRunsResponse_nextToken,
    listSuiteRunsResponse_suiteRunsList,
    listSuiteRunsResponse_httpStatus,

    -- ** GetSuiteRun
    getSuiteRun_suiteDefinitionId,
    getSuiteRun_suiteRunId,
    getSuiteRunResponse_status,
    getSuiteRunResponse_suiteRunConfiguration,
    getSuiteRunResponse_startTime,
    getSuiteRunResponse_suiteDefinitionId,
    getSuiteRunResponse_suiteDefinitionVersion,
    getSuiteRunResponse_errorReason,
    getSuiteRunResponse_testResult,
    getSuiteRunResponse_endTime,
    getSuiteRunResponse_suiteRunArn,
    getSuiteRunResponse_suiteRunId,
    getSuiteRunResponse_tags,
    getSuiteRunResponse_httpStatus,

    -- * Types

    -- ** DeviceUnderTest
    deviceUnderTest_certificateArn,
    deviceUnderTest_thingArn,

    -- ** GroupResult
    groupResult_tests,
    groupResult_groupId,
    groupResult_groupName,

    -- ** SuiteDefinitionConfiguration
    suiteDefinitionConfiguration_suiteDefinitionName,
    suiteDefinitionConfiguration_intendedForQualification,
    suiteDefinitionConfiguration_devicePermissionRoleArn,
    suiteDefinitionConfiguration_devices,
    suiteDefinitionConfiguration_rootGroup,

    -- ** SuiteDefinitionInformation
    suiteDefinitionInformation_createdAt,
    suiteDefinitionInformation_defaultDevices,
    suiteDefinitionInformation_suiteDefinitionId,
    suiteDefinitionInformation_suiteDefinitionName,
    suiteDefinitionInformation_intendedForQualification,

    -- ** SuiteRunConfiguration
    suiteRunConfiguration_primaryDevice,
    suiteRunConfiguration_selectedTestList,

    -- ** SuiteRunInformation
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

    -- ** TestCaseRun
    testCaseRun_status,
    testCaseRun_logUrl,
    testCaseRun_startTime,
    testCaseRun_testCaseRunId,
    testCaseRun_warnings,
    testCaseRun_endTime,
    testCaseRun_testCaseDefinitionId,
    testCaseRun_failure,
    testCaseRun_testCaseDefinitionName,

    -- ** TestResult
    testResult_groups,
  )
where

import Network.AWS.IoTDeviceAdvisor.CreateSuiteDefinition
import Network.AWS.IoTDeviceAdvisor.DeleteSuiteDefinition
import Network.AWS.IoTDeviceAdvisor.GetSuiteDefinition
import Network.AWS.IoTDeviceAdvisor.GetSuiteRun
import Network.AWS.IoTDeviceAdvisor.GetSuiteRunReport
import Network.AWS.IoTDeviceAdvisor.ListSuiteDefinitions
import Network.AWS.IoTDeviceAdvisor.ListSuiteRuns
import Network.AWS.IoTDeviceAdvisor.ListTagsForResource
import Network.AWS.IoTDeviceAdvisor.StartSuiteRun
import Network.AWS.IoTDeviceAdvisor.StopSuiteRun
import Network.AWS.IoTDeviceAdvisor.TagResource
import Network.AWS.IoTDeviceAdvisor.Types.DeviceUnderTest
import Network.AWS.IoTDeviceAdvisor.Types.GroupResult
import Network.AWS.IoTDeviceAdvisor.Types.SuiteDefinitionConfiguration
import Network.AWS.IoTDeviceAdvisor.Types.SuiteDefinitionInformation
import Network.AWS.IoTDeviceAdvisor.Types.SuiteRunConfiguration
import Network.AWS.IoTDeviceAdvisor.Types.SuiteRunInformation
import Network.AWS.IoTDeviceAdvisor.Types.TestCaseRun
import Network.AWS.IoTDeviceAdvisor.Types.TestResult
import Network.AWS.IoTDeviceAdvisor.UntagResource
import Network.AWS.IoTDeviceAdvisor.UpdateSuiteDefinition
