-- Module      : Test.AWS.Gen.ElasticBeanstalk
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

module Test.AWS.Gen.ElasticBeanstalk where

import Data.Proxy
import Test.AWS.Fixture
import Test.Tasty
import Network.AWS.ElasticBeanstalk

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ testDescribeApplications $
--             describeApplications
--
--         , testCreateApplicationVersion $
--             createApplicationVersion
--
--         , testUpdateEnvironment $
--             updateEnvironment
--
--         , testTerminateEnvironment $
--             terminateEnvironment
--
--         , testDescribeEvents $
--             describeEvents
--
--         , testRequestEnvironmentInfo $
--             requestEnvironmentInfo
--
--         , testRetrieveEnvironmentInfo $
--             retrieveEnvironmentInfo
--
--         , testDeleteApplication $
--             deleteApplication
--
--         , testUpdateApplication $
--             updateApplication
--
--         , testCreateApplication $
--             createApplication
--
--         , testAbortEnvironmentUpdate $
--             abortEnvironmentUpdate
--
--         , testDeleteApplicationVersion $
--             deleteApplicationVersion
--
--         , testUpdateApplicationVersion $
--             updateApplicationVersion
--
--         , testDescribeEnvironmentResources $
--             describeEnvironmentResources
--
--         , testDeleteConfigurationTemplate $
--             deleteConfigurationTemplate
--
--         , testUpdateConfigurationTemplate $
--             updateConfigurationTemplate
--
--         , testRebuildEnvironment $
--             rebuildEnvironment
--
--         , testDeleteEnvironmentConfiguration $
--             deleteEnvironmentConfiguration
--
--         , testCreateConfigurationTemplate $
--             createConfigurationTemplate
--
--         , testListAvailableSolutionStacks $
--             listAvailableSolutionStacks
--
--         , testSwapEnvironmentCNAMEs $
--             swapEnvironmentCNAMEs
--
--         , testDescribeConfigurationOptions $
--             describeConfigurationOptions
--
--         , testDescribeConfigurationSettings $
--             describeConfigurationSettings
--
--         , testCreateStorageLocation $
--             createStorageLocation
--
--         , testDescribeEnvironments $
--             describeEnvironments
--
--         , testRestartAppServer $
--             restartAppServer
--
--         , testValidateConfigurationSettings $
--             validateConfigurationSettings
--
--         , testDescribeApplicationVersions $
--             describeApplicationVersions
--
--         , testCheckDNSAvailability $
--             checkDNSAvailability
--
--         , testCreateEnvironment $
--             createEnvironment
--
--           ]

--     , testGroup "response"
--         [ testDescribeApplicationsResponse $
--             describeApplicationsResponse
--
--         , testCreateApplicationVersionResponse $
--             applicationVersionDescriptionMessage
--
--         , testUpdateEnvironmentResponse $
--             environmentDescription
--
--         , testTerminateEnvironmentResponse $
--             environmentDescription
--
--         , testDescribeEventsResponse $
--             describeEventsResponse
--
--         , testRequestEnvironmentInfoResponse $
--             requestEnvironmentInfoResponse
--
--         , testRetrieveEnvironmentInfoResponse $
--             retrieveEnvironmentInfoResponse
--
--         , testDeleteApplicationResponse $
--             deleteApplicationResponse
--
--         , testUpdateApplicationResponse $
--             applicationDescriptionMessage
--
--         , testCreateApplicationResponse $
--             applicationDescriptionMessage
--
--         , testAbortEnvironmentUpdateResponse $
--             abortEnvironmentUpdateResponse
--
--         , testDeleteApplicationVersionResponse $
--             deleteApplicationVersionResponse
--
--         , testUpdateApplicationVersionResponse $
--             applicationVersionDescriptionMessage
--
--         , testDescribeEnvironmentResourcesResponse $
--             describeEnvironmentResourcesResponse
--
--         , testDeleteConfigurationTemplateResponse $
--             deleteConfigurationTemplateResponse
--
--         , testUpdateConfigurationTemplateResponse $
--             configurationSettingsDescription
--
--         , testRebuildEnvironmentResponse $
--             rebuildEnvironmentResponse
--
--         , testDeleteEnvironmentConfigurationResponse $
--             deleteEnvironmentConfigurationResponse
--
--         , testCreateConfigurationTemplateResponse $
--             configurationSettingsDescription
--
--         , testListAvailableSolutionStacksResponse $
--             listAvailableSolutionStacksResponse
--
--         , testSwapEnvironmentCNAMEsResponse $
--             swapEnvironmentCNAMEsResponse
--
--         , testDescribeConfigurationOptionsResponse $
--             describeConfigurationOptionsResponse
--
--         , testDescribeConfigurationSettingsResponse $
--             describeConfigurationSettingsResponse
--
--         , testCreateStorageLocationResponse $
--             createStorageLocationResponse
--
--         , testDescribeEnvironmentsResponse $
--             describeEnvironmentsResponse
--
--         , testRestartAppServerResponse $
--             restartAppServerResponse
--
--         , testValidateConfigurationSettingsResponse $
--             validateConfigurationSettingsResponse
--
--         , testDescribeApplicationVersionsResponse $
--             describeApplicationVersionsResponse
--
--         , testCheckDNSAvailabilityResponse $
--             checkDNSAvailabilityResponse
--
--         , testCreateEnvironmentResponse $
--             environmentDescription
--
--           ]
--     ]

-- Requests

testDescribeApplications :: DescribeApplications -> TestTree
testDescribeApplications = undefined

testCreateApplicationVersion :: CreateApplicationVersion -> TestTree
testCreateApplicationVersion = undefined

testUpdateEnvironment :: UpdateEnvironment -> TestTree
testUpdateEnvironment = undefined

testTerminateEnvironment :: TerminateEnvironment -> TestTree
testTerminateEnvironment = undefined

testDescribeEvents :: DescribeEvents -> TestTree
testDescribeEvents = undefined

testRequestEnvironmentInfo :: RequestEnvironmentInfo -> TestTree
testRequestEnvironmentInfo = undefined

testRetrieveEnvironmentInfo :: RetrieveEnvironmentInfo -> TestTree
testRetrieveEnvironmentInfo = undefined

testDeleteApplication :: DeleteApplication -> TestTree
testDeleteApplication = undefined

testUpdateApplication :: UpdateApplication -> TestTree
testUpdateApplication = undefined

testCreateApplication :: CreateApplication -> TestTree
testCreateApplication = undefined

testAbortEnvironmentUpdate :: AbortEnvironmentUpdate -> TestTree
testAbortEnvironmentUpdate = undefined

testDeleteApplicationVersion :: DeleteApplicationVersion -> TestTree
testDeleteApplicationVersion = undefined

testUpdateApplicationVersion :: UpdateApplicationVersion -> TestTree
testUpdateApplicationVersion = undefined

testDescribeEnvironmentResources :: DescribeEnvironmentResources -> TestTree
testDescribeEnvironmentResources = undefined

testDeleteConfigurationTemplate :: DeleteConfigurationTemplate -> TestTree
testDeleteConfigurationTemplate = undefined

testUpdateConfigurationTemplate :: UpdateConfigurationTemplate -> TestTree
testUpdateConfigurationTemplate = undefined

testRebuildEnvironment :: RebuildEnvironment -> TestTree
testRebuildEnvironment = undefined

testDeleteEnvironmentConfiguration :: DeleteEnvironmentConfiguration -> TestTree
testDeleteEnvironmentConfiguration = undefined

testCreateConfigurationTemplate :: CreateConfigurationTemplate -> TestTree
testCreateConfigurationTemplate = undefined

testListAvailableSolutionStacks :: ListAvailableSolutionStacks -> TestTree
testListAvailableSolutionStacks = undefined

testSwapEnvironmentCNAMEs :: SwapEnvironmentCNAMEs -> TestTree
testSwapEnvironmentCNAMEs = undefined

testDescribeConfigurationOptions :: DescribeConfigurationOptions -> TestTree
testDescribeConfigurationOptions = undefined

testDescribeConfigurationSettings :: DescribeConfigurationSettings -> TestTree
testDescribeConfigurationSettings = undefined

testCreateStorageLocation :: CreateStorageLocation -> TestTree
testCreateStorageLocation = undefined

testDescribeEnvironments :: DescribeEnvironments -> TestTree
testDescribeEnvironments = undefined

testRestartAppServer :: RestartAppServer -> TestTree
testRestartAppServer = undefined

testValidateConfigurationSettings :: ValidateConfigurationSettings -> TestTree
testValidateConfigurationSettings = undefined

testDescribeApplicationVersions :: DescribeApplicationVersions -> TestTree
testDescribeApplicationVersions = undefined

testCheckDNSAvailability :: CheckDNSAvailability -> TestTree
testCheckDNSAvailability = undefined

testCreateEnvironment :: CreateEnvironment -> TestTree
testCreateEnvironment = undefined

-- Responses

testDescribeApplicationsResponse :: DescribeApplicationsResponse -> TestTree
testDescribeApplicationsResponse = resp
    "DescribeApplicationsResponse"
    "fixture/DescribeApplicationsResponse"
    (Proxy :: Proxy DescribeApplications)

testCreateApplicationVersionResponse :: ApplicationVersionDescriptionMessage -> TestTree
testCreateApplicationVersionResponse = resp
    "CreateApplicationVersionResponse"
    "fixture/CreateApplicationVersionResponse"
    (Proxy :: Proxy CreateApplicationVersion)

testUpdateEnvironmentResponse :: EnvironmentDescription -> TestTree
testUpdateEnvironmentResponse = resp
    "UpdateEnvironmentResponse"
    "fixture/UpdateEnvironmentResponse"
    (Proxy :: Proxy UpdateEnvironment)

testTerminateEnvironmentResponse :: EnvironmentDescription -> TestTree
testTerminateEnvironmentResponse = resp
    "TerminateEnvironmentResponse"
    "fixture/TerminateEnvironmentResponse"
    (Proxy :: Proxy TerminateEnvironment)

testDescribeEventsResponse :: DescribeEventsResponse -> TestTree
testDescribeEventsResponse = resp
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse"
    (Proxy :: Proxy DescribeEvents)

testRequestEnvironmentInfoResponse :: RequestEnvironmentInfoResponse -> TestTree
testRequestEnvironmentInfoResponse = resp
    "RequestEnvironmentInfoResponse"
    "fixture/RequestEnvironmentInfoResponse"
    (Proxy :: Proxy RequestEnvironmentInfo)

testRetrieveEnvironmentInfoResponse :: RetrieveEnvironmentInfoResponse -> TestTree
testRetrieveEnvironmentInfoResponse = resp
    "RetrieveEnvironmentInfoResponse"
    "fixture/RetrieveEnvironmentInfoResponse"
    (Proxy :: Proxy RetrieveEnvironmentInfo)

testDeleteApplicationResponse :: DeleteApplicationResponse -> TestTree
testDeleteApplicationResponse = resp
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse"
    (Proxy :: Proxy DeleteApplication)

testUpdateApplicationResponse :: ApplicationDescriptionMessage -> TestTree
testUpdateApplicationResponse = resp
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse"
    (Proxy :: Proxy UpdateApplication)

testCreateApplicationResponse :: ApplicationDescriptionMessage -> TestTree
testCreateApplicationResponse = resp
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse"
    (Proxy :: Proxy CreateApplication)

testAbortEnvironmentUpdateResponse :: AbortEnvironmentUpdateResponse -> TestTree
testAbortEnvironmentUpdateResponse = resp
    "AbortEnvironmentUpdateResponse"
    "fixture/AbortEnvironmentUpdateResponse"
    (Proxy :: Proxy AbortEnvironmentUpdate)

testDeleteApplicationVersionResponse :: DeleteApplicationVersionResponse -> TestTree
testDeleteApplicationVersionResponse = resp
    "DeleteApplicationVersionResponse"
    "fixture/DeleteApplicationVersionResponse"
    (Proxy :: Proxy DeleteApplicationVersion)

testUpdateApplicationVersionResponse :: ApplicationVersionDescriptionMessage -> TestTree
testUpdateApplicationVersionResponse = resp
    "UpdateApplicationVersionResponse"
    "fixture/UpdateApplicationVersionResponse"
    (Proxy :: Proxy UpdateApplicationVersion)

testDescribeEnvironmentResourcesResponse :: DescribeEnvironmentResourcesResponse -> TestTree
testDescribeEnvironmentResourcesResponse = resp
    "DescribeEnvironmentResourcesResponse"
    "fixture/DescribeEnvironmentResourcesResponse"
    (Proxy :: Proxy DescribeEnvironmentResources)

testDeleteConfigurationTemplateResponse :: DeleteConfigurationTemplateResponse -> TestTree
testDeleteConfigurationTemplateResponse = resp
    "DeleteConfigurationTemplateResponse"
    "fixture/DeleteConfigurationTemplateResponse"
    (Proxy :: Proxy DeleteConfigurationTemplate)

testUpdateConfigurationTemplateResponse :: ConfigurationSettingsDescription -> TestTree
testUpdateConfigurationTemplateResponse = resp
    "UpdateConfigurationTemplateResponse"
    "fixture/UpdateConfigurationTemplateResponse"
    (Proxy :: Proxy UpdateConfigurationTemplate)

testRebuildEnvironmentResponse :: RebuildEnvironmentResponse -> TestTree
testRebuildEnvironmentResponse = resp
    "RebuildEnvironmentResponse"
    "fixture/RebuildEnvironmentResponse"
    (Proxy :: Proxy RebuildEnvironment)

testDeleteEnvironmentConfigurationResponse :: DeleteEnvironmentConfigurationResponse -> TestTree
testDeleteEnvironmentConfigurationResponse = resp
    "DeleteEnvironmentConfigurationResponse"
    "fixture/DeleteEnvironmentConfigurationResponse"
    (Proxy :: Proxy DeleteEnvironmentConfiguration)

testCreateConfigurationTemplateResponse :: ConfigurationSettingsDescription -> TestTree
testCreateConfigurationTemplateResponse = resp
    "CreateConfigurationTemplateResponse"
    "fixture/CreateConfigurationTemplateResponse"
    (Proxy :: Proxy CreateConfigurationTemplate)

testListAvailableSolutionStacksResponse :: ListAvailableSolutionStacksResponse -> TestTree
testListAvailableSolutionStacksResponse = resp
    "ListAvailableSolutionStacksResponse"
    "fixture/ListAvailableSolutionStacksResponse"
    (Proxy :: Proxy ListAvailableSolutionStacks)

testSwapEnvironmentCNAMEsResponse :: SwapEnvironmentCNAMEsResponse -> TestTree
testSwapEnvironmentCNAMEsResponse = resp
    "SwapEnvironmentCNAMEsResponse"
    "fixture/SwapEnvironmentCNAMEsResponse"
    (Proxy :: Proxy SwapEnvironmentCNAMEs)

testDescribeConfigurationOptionsResponse :: DescribeConfigurationOptionsResponse -> TestTree
testDescribeConfigurationOptionsResponse = resp
    "DescribeConfigurationOptionsResponse"
    "fixture/DescribeConfigurationOptionsResponse"
    (Proxy :: Proxy DescribeConfigurationOptions)

testDescribeConfigurationSettingsResponse :: DescribeConfigurationSettingsResponse -> TestTree
testDescribeConfigurationSettingsResponse = resp
    "DescribeConfigurationSettingsResponse"
    "fixture/DescribeConfigurationSettingsResponse"
    (Proxy :: Proxy DescribeConfigurationSettings)

testCreateStorageLocationResponse :: CreateStorageLocationResponse -> TestTree
testCreateStorageLocationResponse = resp
    "CreateStorageLocationResponse"
    "fixture/CreateStorageLocationResponse"
    (Proxy :: Proxy CreateStorageLocation)

testDescribeEnvironmentsResponse :: DescribeEnvironmentsResponse -> TestTree
testDescribeEnvironmentsResponse = resp
    "DescribeEnvironmentsResponse"
    "fixture/DescribeEnvironmentsResponse"
    (Proxy :: Proxy DescribeEnvironments)

testRestartAppServerResponse :: RestartAppServerResponse -> TestTree
testRestartAppServerResponse = resp
    "RestartAppServerResponse"
    "fixture/RestartAppServerResponse"
    (Proxy :: Proxy RestartAppServer)

testValidateConfigurationSettingsResponse :: ValidateConfigurationSettingsResponse -> TestTree
testValidateConfigurationSettingsResponse = resp
    "ValidateConfigurationSettingsResponse"
    "fixture/ValidateConfigurationSettingsResponse"
    (Proxy :: Proxy ValidateConfigurationSettings)

testDescribeApplicationVersionsResponse :: DescribeApplicationVersionsResponse -> TestTree
testDescribeApplicationVersionsResponse = resp
    "DescribeApplicationVersionsResponse"
    "fixture/DescribeApplicationVersionsResponse"
    (Proxy :: Proxy DescribeApplicationVersions)

testCheckDNSAvailabilityResponse :: CheckDNSAvailabilityResponse -> TestTree
testCheckDNSAvailabilityResponse = resp
    "CheckDNSAvailabilityResponse"
    "fixture/CheckDNSAvailabilityResponse"
    (Proxy :: Proxy CheckDNSAvailability)

testCreateEnvironmentResponse :: EnvironmentDescription -> TestTree
testCreateEnvironmentResponse = resp
    "CreateEnvironmentResponse"
    "fixture/CreateEnvironmentResponse"
    (Proxy :: Proxy CreateEnvironment)
