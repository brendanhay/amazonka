{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.IoTDeviceAdvisor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-09-18@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- AWS IoT Core Device Advisor is a cloud-based, fully managed test
-- capability for validating IoT devices during device software
-- development. Device Advisor provides pre-built tests that you can use to
-- validate IoT devices for reliable and secure connectivity with AWS IoT
-- Core before deploying devices to production. By using Device Advisor,
-- you can confirm that your devices can connect to AWS IoT Core, follow
-- security best practices and, if applicable, receive software updates
-- from IoT Device Management. You can also download signed qualification
-- reports to submit to the AWS Partner Network to get your device
-- qualified for the AWS Partner Device Catalog without the need to send
-- your device in and wait for it to be tested.
module Network.AWS.IoTDeviceAdvisor
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** ConflictException
    _ConflictException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** GetSuiteRunReport
    GetSuiteRunReport (GetSuiteRunReport'),
    newGetSuiteRunReport,
    GetSuiteRunReportResponse (GetSuiteRunReportResponse'),
    newGetSuiteRunReportResponse,

    -- ** StartSuiteRun
    StartSuiteRun (StartSuiteRun'),
    newStartSuiteRun,
    StartSuiteRunResponse (StartSuiteRunResponse'),
    newStartSuiteRunResponse,

    -- ** ListSuiteDefinitions
    ListSuiteDefinitions (ListSuiteDefinitions'),
    newListSuiteDefinitions,
    ListSuiteDefinitionsResponse (ListSuiteDefinitionsResponse'),
    newListSuiteDefinitionsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** DeleteSuiteDefinition
    DeleteSuiteDefinition (DeleteSuiteDefinition'),
    newDeleteSuiteDefinition,
    DeleteSuiteDefinitionResponse (DeleteSuiteDefinitionResponse'),
    newDeleteSuiteDefinitionResponse,

    -- ** UpdateSuiteDefinition
    UpdateSuiteDefinition (UpdateSuiteDefinition'),
    newUpdateSuiteDefinition,
    UpdateSuiteDefinitionResponse (UpdateSuiteDefinitionResponse'),
    newUpdateSuiteDefinitionResponse,

    -- ** CreateSuiteDefinition
    CreateSuiteDefinition (CreateSuiteDefinition'),
    newCreateSuiteDefinition,
    CreateSuiteDefinitionResponse (CreateSuiteDefinitionResponse'),
    newCreateSuiteDefinitionResponse,

    -- ** StopSuiteRun
    StopSuiteRun (StopSuiteRun'),
    newStopSuiteRun,
    StopSuiteRunResponse (StopSuiteRunResponse'),
    newStopSuiteRunResponse,

    -- ** GetSuiteDefinition
    GetSuiteDefinition (GetSuiteDefinition'),
    newGetSuiteDefinition,
    GetSuiteDefinitionResponse (GetSuiteDefinitionResponse'),
    newGetSuiteDefinitionResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** ListSuiteRuns
    ListSuiteRuns (ListSuiteRuns'),
    newListSuiteRuns,
    ListSuiteRunsResponse (ListSuiteRunsResponse'),
    newListSuiteRunsResponse,

    -- ** GetSuiteRun
    GetSuiteRun (GetSuiteRun'),
    newGetSuiteRun,
    GetSuiteRunResponse (GetSuiteRunResponse'),
    newGetSuiteRunResponse,

    -- * Types

    -- ** Status
    Status (..),

    -- ** SuiteRunStatus
    SuiteRunStatus (..),

    -- ** DeviceUnderTest
    DeviceUnderTest (DeviceUnderTest'),
    newDeviceUnderTest,

    -- ** GroupResult
    GroupResult (GroupResult'),
    newGroupResult,

    -- ** SuiteDefinitionConfiguration
    SuiteDefinitionConfiguration (SuiteDefinitionConfiguration'),
    newSuiteDefinitionConfiguration,

    -- ** SuiteDefinitionInformation
    SuiteDefinitionInformation (SuiteDefinitionInformation'),
    newSuiteDefinitionInformation,

    -- ** SuiteRunConfiguration
    SuiteRunConfiguration (SuiteRunConfiguration'),
    newSuiteRunConfiguration,

    -- ** SuiteRunInformation
    SuiteRunInformation (SuiteRunInformation'),
    newSuiteRunInformation,

    -- ** TestCaseRun
    TestCaseRun (TestCaseRun'),
    newTestCaseRun,

    -- ** TestResult
    TestResult (TestResult'),
    newTestResult,
  )
where

import Network.AWS.IoTDeviceAdvisor.CreateSuiteDefinition
import Network.AWS.IoTDeviceAdvisor.DeleteSuiteDefinition
import Network.AWS.IoTDeviceAdvisor.GetSuiteDefinition
import Network.AWS.IoTDeviceAdvisor.GetSuiteRun
import Network.AWS.IoTDeviceAdvisor.GetSuiteRunReport
import Network.AWS.IoTDeviceAdvisor.Lens
import Network.AWS.IoTDeviceAdvisor.ListSuiteDefinitions
import Network.AWS.IoTDeviceAdvisor.ListSuiteRuns
import Network.AWS.IoTDeviceAdvisor.ListTagsForResource
import Network.AWS.IoTDeviceAdvisor.StartSuiteRun
import Network.AWS.IoTDeviceAdvisor.StopSuiteRun
import Network.AWS.IoTDeviceAdvisor.TagResource
import Network.AWS.IoTDeviceAdvisor.Types
import Network.AWS.IoTDeviceAdvisor.UntagResource
import Network.AWS.IoTDeviceAdvisor.UpdateSuiteDefinition
import Network.AWS.IoTDeviceAdvisor.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'IoTDeviceAdvisor'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
