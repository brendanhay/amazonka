{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ElasticBeanstalk
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.ElasticBeanstalk where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.ElasticBeanstalk
import Test.AWS.ElasticBeanstalk.Internal

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
--         , testUpdateEnvironment $
--             updateEnvironment
--
--         , testTerminateEnvironment $
--             terminateEnvironment
--
--         , testCreateApplicationVersion $
--             createApplicationVersion
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
--         , testDescribeInstancesHealth $
--             describeInstancesHealth
--
--         , testCreateApplication $
--             createApplication
--
--         , testComposeEnvironments $
--             composeEnvironments
--
--         , testAbortEnvironmentUpdate $
--             abortEnvironmentUpdate
--
--         , testDeleteConfigurationTemplate $
--             deleteConfigurationTemplate
--
--         , testUpdateConfigurationTemplate $
--             updateConfigurationTemplate
--
--         , testDescribeEnvironmentResources $
--             describeEnvironmentResources
--
--         , testDescribeEnvironmentManagedActionHistory $
--             describeEnvironmentManagedActionHistory
--
--         , testDeleteApplicationVersion $
--             deleteApplicationVersion
--
--         , testUpdateApplicationVersion $
--             updateApplicationVersion
--
--         , testCreateConfigurationTemplate $
--             createConfigurationTemplate
--
--         , testDescribeEnvironmentHealth $
--             describeEnvironmentHealth
--
--         , testRebuildEnvironment $
--             rebuildEnvironment
--
--         , testDeleteEnvironmentConfiguration $
--             deleteEnvironmentConfiguration
--
--         , testSwapEnvironmentCNAMEs $
--             swapEnvironmentCNAMEs
--
--         , testListAvailableSolutionStacks $
--             listAvailableSolutionStacks
--
--         , testApplyEnvironmentManagedAction $
--             applyEnvironmentManagedAction
--
--         , testDescribeConfigurationOptions $
--             describeConfigurationOptions
--
--         , testCreateStorageLocation $
--             createStorageLocation
--
--         , testDescribeEnvironmentManagedActions $
--             describeEnvironmentManagedActions
--
--         , testDescribeConfigurationSettings $
--             describeConfigurationSettings
--
--         , testValidateConfigurationSettings $
--             validateConfigurationSettings
--
--         , testRestartAppServer $
--             restartAppServer
--
--         , testDescribeEnvironments $
--             describeEnvironments
--
--         , testCheckDNSAvailability $
--             checkDNSAvailability
--
--         , testDescribeApplicationVersions $
--             describeApplicationVersions
--
--         , testCreateEnvironment $
--             createEnvironment
--
--           ]

--     , testGroup "response"
--         [ testDescribeApplicationsResponse $
--             describeApplicationsResponse
--
--         , testUpdateEnvironmentResponse $
--             environmentDescription
--
--         , testTerminateEnvironmentResponse $
--             environmentDescription
--
--         , testCreateApplicationVersionResponse $
--             applicationVersionDescriptionMessage
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
--         , testDescribeInstancesHealthResponse $
--             describeInstancesHealthResponse
--
--         , testCreateApplicationResponse $
--             applicationDescriptionMessage
--
--         , testComposeEnvironmentsResponse $
--             environmentDescriptionsMessage
--
--         , testAbortEnvironmentUpdateResponse $
--             abortEnvironmentUpdateResponse
--
--         , testDeleteConfigurationTemplateResponse $
--             deleteConfigurationTemplateResponse
--
--         , testUpdateConfigurationTemplateResponse $
--             configurationSettingsDescription
--
--         , testDescribeEnvironmentResourcesResponse $
--             describeEnvironmentResourcesResponse
--
--         , testDescribeEnvironmentManagedActionHistoryResponse $
--             describeEnvironmentManagedActionHistoryResponse
--
--         , testDeleteApplicationVersionResponse $
--             deleteApplicationVersionResponse
--
--         , testUpdateApplicationVersionResponse $
--             applicationVersionDescriptionMessage
--
--         , testCreateConfigurationTemplateResponse $
--             configurationSettingsDescription
--
--         , testDescribeEnvironmentHealthResponse $
--             describeEnvironmentHealthResponse
--
--         , testRebuildEnvironmentResponse $
--             rebuildEnvironmentResponse
--
--         , testDeleteEnvironmentConfigurationResponse $
--             deleteEnvironmentConfigurationResponse
--
--         , testSwapEnvironmentCNAMEsResponse $
--             swapEnvironmentCNAMEsResponse
--
--         , testListAvailableSolutionStacksResponse $
--             listAvailableSolutionStacksResponse
--
--         , testApplyEnvironmentManagedActionResponse $
--             applyEnvironmentManagedActionResponse
--
--         , testDescribeConfigurationOptionsResponse $
--             describeConfigurationOptionsResponse
--
--         , testCreateStorageLocationResponse $
--             createStorageLocationResponse
--
--         , testDescribeEnvironmentManagedActionsResponse $
--             describeEnvironmentManagedActionsResponse
--
--         , testDescribeConfigurationSettingsResponse $
--             describeConfigurationSettingsResponse
--
--         , testValidateConfigurationSettingsResponse $
--             validateConfigurationSettingsResponse
--
--         , testRestartAppServerResponse $
--             restartAppServerResponse
--
--         , testDescribeEnvironmentsResponse $
--             environmentDescriptionsMessage
--
--         , testCheckDNSAvailabilityResponse $
--             checkDNSAvailabilityResponse
--
--         , testDescribeApplicationVersionsResponse $
--             describeApplicationVersionsResponse
--
--         , testCreateEnvironmentResponse $
--             environmentDescription
--
--           ]
--     ]

-- Requests

testDescribeApplications :: DescribeApplications -> TestTree
testDescribeApplications = req
    "DescribeApplications"
    "fixture/DescribeApplications.yaml"

testUpdateEnvironment :: UpdateEnvironment -> TestTree
testUpdateEnvironment = req
    "UpdateEnvironment"
    "fixture/UpdateEnvironment.yaml"

testTerminateEnvironment :: TerminateEnvironment -> TestTree
testTerminateEnvironment = req
    "TerminateEnvironment"
    "fixture/TerminateEnvironment.yaml"

testCreateApplicationVersion :: CreateApplicationVersion -> TestTree
testCreateApplicationVersion = req
    "CreateApplicationVersion"
    "fixture/CreateApplicationVersion.yaml"

testDescribeEvents :: DescribeEvents -> TestTree
testDescribeEvents = req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

testRequestEnvironmentInfo :: RequestEnvironmentInfo -> TestTree
testRequestEnvironmentInfo = req
    "RequestEnvironmentInfo"
    "fixture/RequestEnvironmentInfo.yaml"

testRetrieveEnvironmentInfo :: RetrieveEnvironmentInfo -> TestTree
testRetrieveEnvironmentInfo = req
    "RetrieveEnvironmentInfo"
    "fixture/RetrieveEnvironmentInfo.yaml"

testDeleteApplication :: DeleteApplication -> TestTree
testDeleteApplication = req
    "DeleteApplication"
    "fixture/DeleteApplication.yaml"

testUpdateApplication :: UpdateApplication -> TestTree
testUpdateApplication = req
    "UpdateApplication"
    "fixture/UpdateApplication.yaml"

testDescribeInstancesHealth :: DescribeInstancesHealth -> TestTree
testDescribeInstancesHealth = req
    "DescribeInstancesHealth"
    "fixture/DescribeInstancesHealth.yaml"

testCreateApplication :: CreateApplication -> TestTree
testCreateApplication = req
    "CreateApplication"
    "fixture/CreateApplication.yaml"

testComposeEnvironments :: ComposeEnvironments -> TestTree
testComposeEnvironments = req
    "ComposeEnvironments"
    "fixture/ComposeEnvironments.yaml"

testAbortEnvironmentUpdate :: AbortEnvironmentUpdate -> TestTree
testAbortEnvironmentUpdate = req
    "AbortEnvironmentUpdate"
    "fixture/AbortEnvironmentUpdate.yaml"

testDeleteConfigurationTemplate :: DeleteConfigurationTemplate -> TestTree
testDeleteConfigurationTemplate = req
    "DeleteConfigurationTemplate"
    "fixture/DeleteConfigurationTemplate.yaml"

testUpdateConfigurationTemplate :: UpdateConfigurationTemplate -> TestTree
testUpdateConfigurationTemplate = req
    "UpdateConfigurationTemplate"
    "fixture/UpdateConfigurationTemplate.yaml"

testDescribeEnvironmentResources :: DescribeEnvironmentResources -> TestTree
testDescribeEnvironmentResources = req
    "DescribeEnvironmentResources"
    "fixture/DescribeEnvironmentResources.yaml"

testDescribeEnvironmentManagedActionHistory :: DescribeEnvironmentManagedActionHistory -> TestTree
testDescribeEnvironmentManagedActionHistory = req
    "DescribeEnvironmentManagedActionHistory"
    "fixture/DescribeEnvironmentManagedActionHistory.yaml"

testDeleteApplicationVersion :: DeleteApplicationVersion -> TestTree
testDeleteApplicationVersion = req
    "DeleteApplicationVersion"
    "fixture/DeleteApplicationVersion.yaml"

testUpdateApplicationVersion :: UpdateApplicationVersion -> TestTree
testUpdateApplicationVersion = req
    "UpdateApplicationVersion"
    "fixture/UpdateApplicationVersion.yaml"

testCreateConfigurationTemplate :: CreateConfigurationTemplate -> TestTree
testCreateConfigurationTemplate = req
    "CreateConfigurationTemplate"
    "fixture/CreateConfigurationTemplate.yaml"

testDescribeEnvironmentHealth :: DescribeEnvironmentHealth -> TestTree
testDescribeEnvironmentHealth = req
    "DescribeEnvironmentHealth"
    "fixture/DescribeEnvironmentHealth.yaml"

testRebuildEnvironment :: RebuildEnvironment -> TestTree
testRebuildEnvironment = req
    "RebuildEnvironment"
    "fixture/RebuildEnvironment.yaml"

testDeleteEnvironmentConfiguration :: DeleteEnvironmentConfiguration -> TestTree
testDeleteEnvironmentConfiguration = req
    "DeleteEnvironmentConfiguration"
    "fixture/DeleteEnvironmentConfiguration.yaml"

testSwapEnvironmentCNAMEs :: SwapEnvironmentCNAMEs -> TestTree
testSwapEnvironmentCNAMEs = req
    "SwapEnvironmentCNAMEs"
    "fixture/SwapEnvironmentCNAMEs.yaml"

testListAvailableSolutionStacks :: ListAvailableSolutionStacks -> TestTree
testListAvailableSolutionStacks = req
    "ListAvailableSolutionStacks"
    "fixture/ListAvailableSolutionStacks.yaml"

testApplyEnvironmentManagedAction :: ApplyEnvironmentManagedAction -> TestTree
testApplyEnvironmentManagedAction = req
    "ApplyEnvironmentManagedAction"
    "fixture/ApplyEnvironmentManagedAction.yaml"

testDescribeConfigurationOptions :: DescribeConfigurationOptions -> TestTree
testDescribeConfigurationOptions = req
    "DescribeConfigurationOptions"
    "fixture/DescribeConfigurationOptions.yaml"

testCreateStorageLocation :: CreateStorageLocation -> TestTree
testCreateStorageLocation = req
    "CreateStorageLocation"
    "fixture/CreateStorageLocation.yaml"

testDescribeEnvironmentManagedActions :: DescribeEnvironmentManagedActions -> TestTree
testDescribeEnvironmentManagedActions = req
    "DescribeEnvironmentManagedActions"
    "fixture/DescribeEnvironmentManagedActions.yaml"

testDescribeConfigurationSettings :: DescribeConfigurationSettings -> TestTree
testDescribeConfigurationSettings = req
    "DescribeConfigurationSettings"
    "fixture/DescribeConfigurationSettings.yaml"

testValidateConfigurationSettings :: ValidateConfigurationSettings -> TestTree
testValidateConfigurationSettings = req
    "ValidateConfigurationSettings"
    "fixture/ValidateConfigurationSettings.yaml"

testRestartAppServer :: RestartAppServer -> TestTree
testRestartAppServer = req
    "RestartAppServer"
    "fixture/RestartAppServer.yaml"

testDescribeEnvironments :: DescribeEnvironments -> TestTree
testDescribeEnvironments = req
    "DescribeEnvironments"
    "fixture/DescribeEnvironments.yaml"

testCheckDNSAvailability :: CheckDNSAvailability -> TestTree
testCheckDNSAvailability = req
    "CheckDNSAvailability"
    "fixture/CheckDNSAvailability.yaml"

testDescribeApplicationVersions :: DescribeApplicationVersions -> TestTree
testDescribeApplicationVersions = req
    "DescribeApplicationVersions"
    "fixture/DescribeApplicationVersions.yaml"

testCreateEnvironment :: CreateEnvironment -> TestTree
testCreateEnvironment = req
    "CreateEnvironment"
    "fixture/CreateEnvironment.yaml"

-- Responses

testDescribeApplicationsResponse :: DescribeApplicationsResponse -> TestTree
testDescribeApplicationsResponse = res
    "DescribeApplicationsResponse"
    "fixture/DescribeApplicationsResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy DescribeApplications)

testUpdateEnvironmentResponse :: EnvironmentDescription -> TestTree
testUpdateEnvironmentResponse = res
    "UpdateEnvironmentResponse"
    "fixture/UpdateEnvironmentResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy UpdateEnvironment)

testTerminateEnvironmentResponse :: EnvironmentDescription -> TestTree
testTerminateEnvironmentResponse = res
    "TerminateEnvironmentResponse"
    "fixture/TerminateEnvironmentResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy TerminateEnvironment)

testCreateApplicationVersionResponse :: ApplicationVersionDescriptionMessage -> TestTree
testCreateApplicationVersionResponse = res
    "CreateApplicationVersionResponse"
    "fixture/CreateApplicationVersionResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy CreateApplicationVersion)

testDescribeEventsResponse :: DescribeEventsResponse -> TestTree
testDescribeEventsResponse = res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy DescribeEvents)

testRequestEnvironmentInfoResponse :: RequestEnvironmentInfoResponse -> TestTree
testRequestEnvironmentInfoResponse = res
    "RequestEnvironmentInfoResponse"
    "fixture/RequestEnvironmentInfoResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy RequestEnvironmentInfo)

testRetrieveEnvironmentInfoResponse :: RetrieveEnvironmentInfoResponse -> TestTree
testRetrieveEnvironmentInfoResponse = res
    "RetrieveEnvironmentInfoResponse"
    "fixture/RetrieveEnvironmentInfoResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy RetrieveEnvironmentInfo)

testDeleteApplicationResponse :: DeleteApplicationResponse -> TestTree
testDeleteApplicationResponse = res
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy DeleteApplication)

testUpdateApplicationResponse :: ApplicationDescriptionMessage -> TestTree
testUpdateApplicationResponse = res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy UpdateApplication)

testDescribeInstancesHealthResponse :: DescribeInstancesHealthResponse -> TestTree
testDescribeInstancesHealthResponse = res
    "DescribeInstancesHealthResponse"
    "fixture/DescribeInstancesHealthResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy DescribeInstancesHealth)

testCreateApplicationResponse :: ApplicationDescriptionMessage -> TestTree
testCreateApplicationResponse = res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy CreateApplication)

testComposeEnvironmentsResponse :: EnvironmentDescriptionsMessage -> TestTree
testComposeEnvironmentsResponse = res
    "ComposeEnvironmentsResponse"
    "fixture/ComposeEnvironmentsResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy ComposeEnvironments)

testAbortEnvironmentUpdateResponse :: AbortEnvironmentUpdateResponse -> TestTree
testAbortEnvironmentUpdateResponse = res
    "AbortEnvironmentUpdateResponse"
    "fixture/AbortEnvironmentUpdateResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy AbortEnvironmentUpdate)

testDeleteConfigurationTemplateResponse :: DeleteConfigurationTemplateResponse -> TestTree
testDeleteConfigurationTemplateResponse = res
    "DeleteConfigurationTemplateResponse"
    "fixture/DeleteConfigurationTemplateResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy DeleteConfigurationTemplate)

testUpdateConfigurationTemplateResponse :: ConfigurationSettingsDescription -> TestTree
testUpdateConfigurationTemplateResponse = res
    "UpdateConfigurationTemplateResponse"
    "fixture/UpdateConfigurationTemplateResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy UpdateConfigurationTemplate)

testDescribeEnvironmentResourcesResponse :: DescribeEnvironmentResourcesResponse -> TestTree
testDescribeEnvironmentResourcesResponse = res
    "DescribeEnvironmentResourcesResponse"
    "fixture/DescribeEnvironmentResourcesResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy DescribeEnvironmentResources)

testDescribeEnvironmentManagedActionHistoryResponse :: DescribeEnvironmentManagedActionHistoryResponse -> TestTree
testDescribeEnvironmentManagedActionHistoryResponse = res
    "DescribeEnvironmentManagedActionHistoryResponse"
    "fixture/DescribeEnvironmentManagedActionHistoryResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy DescribeEnvironmentManagedActionHistory)

testDeleteApplicationVersionResponse :: DeleteApplicationVersionResponse -> TestTree
testDeleteApplicationVersionResponse = res
    "DeleteApplicationVersionResponse"
    "fixture/DeleteApplicationVersionResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy DeleteApplicationVersion)

testUpdateApplicationVersionResponse :: ApplicationVersionDescriptionMessage -> TestTree
testUpdateApplicationVersionResponse = res
    "UpdateApplicationVersionResponse"
    "fixture/UpdateApplicationVersionResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy UpdateApplicationVersion)

testCreateConfigurationTemplateResponse :: ConfigurationSettingsDescription -> TestTree
testCreateConfigurationTemplateResponse = res
    "CreateConfigurationTemplateResponse"
    "fixture/CreateConfigurationTemplateResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy CreateConfigurationTemplate)

testDescribeEnvironmentHealthResponse :: DescribeEnvironmentHealthResponse -> TestTree
testDescribeEnvironmentHealthResponse = res
    "DescribeEnvironmentHealthResponse"
    "fixture/DescribeEnvironmentHealthResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy DescribeEnvironmentHealth)

testRebuildEnvironmentResponse :: RebuildEnvironmentResponse -> TestTree
testRebuildEnvironmentResponse = res
    "RebuildEnvironmentResponse"
    "fixture/RebuildEnvironmentResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy RebuildEnvironment)

testDeleteEnvironmentConfigurationResponse :: DeleteEnvironmentConfigurationResponse -> TestTree
testDeleteEnvironmentConfigurationResponse = res
    "DeleteEnvironmentConfigurationResponse"
    "fixture/DeleteEnvironmentConfigurationResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy DeleteEnvironmentConfiguration)

testSwapEnvironmentCNAMEsResponse :: SwapEnvironmentCNAMEsResponse -> TestTree
testSwapEnvironmentCNAMEsResponse = res
    "SwapEnvironmentCNAMEsResponse"
    "fixture/SwapEnvironmentCNAMEsResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy SwapEnvironmentCNAMEs)

testListAvailableSolutionStacksResponse :: ListAvailableSolutionStacksResponse -> TestTree
testListAvailableSolutionStacksResponse = res
    "ListAvailableSolutionStacksResponse"
    "fixture/ListAvailableSolutionStacksResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy ListAvailableSolutionStacks)

testApplyEnvironmentManagedActionResponse :: ApplyEnvironmentManagedActionResponse -> TestTree
testApplyEnvironmentManagedActionResponse = res
    "ApplyEnvironmentManagedActionResponse"
    "fixture/ApplyEnvironmentManagedActionResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy ApplyEnvironmentManagedAction)

testDescribeConfigurationOptionsResponse :: DescribeConfigurationOptionsResponse -> TestTree
testDescribeConfigurationOptionsResponse = res
    "DescribeConfigurationOptionsResponse"
    "fixture/DescribeConfigurationOptionsResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy DescribeConfigurationOptions)

testCreateStorageLocationResponse :: CreateStorageLocationResponse -> TestTree
testCreateStorageLocationResponse = res
    "CreateStorageLocationResponse"
    "fixture/CreateStorageLocationResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy CreateStorageLocation)

testDescribeEnvironmentManagedActionsResponse :: DescribeEnvironmentManagedActionsResponse -> TestTree
testDescribeEnvironmentManagedActionsResponse = res
    "DescribeEnvironmentManagedActionsResponse"
    "fixture/DescribeEnvironmentManagedActionsResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy DescribeEnvironmentManagedActions)

testDescribeConfigurationSettingsResponse :: DescribeConfigurationSettingsResponse -> TestTree
testDescribeConfigurationSettingsResponse = res
    "DescribeConfigurationSettingsResponse"
    "fixture/DescribeConfigurationSettingsResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy DescribeConfigurationSettings)

testValidateConfigurationSettingsResponse :: ValidateConfigurationSettingsResponse -> TestTree
testValidateConfigurationSettingsResponse = res
    "ValidateConfigurationSettingsResponse"
    "fixture/ValidateConfigurationSettingsResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy ValidateConfigurationSettings)

testRestartAppServerResponse :: RestartAppServerResponse -> TestTree
testRestartAppServerResponse = res
    "RestartAppServerResponse"
    "fixture/RestartAppServerResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy RestartAppServer)

testDescribeEnvironmentsResponse :: EnvironmentDescriptionsMessage -> TestTree
testDescribeEnvironmentsResponse = res
    "DescribeEnvironmentsResponse"
    "fixture/DescribeEnvironmentsResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy DescribeEnvironments)

testCheckDNSAvailabilityResponse :: CheckDNSAvailabilityResponse -> TestTree
testCheckDNSAvailabilityResponse = res
    "CheckDNSAvailabilityResponse"
    "fixture/CheckDNSAvailabilityResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy CheckDNSAvailability)

testDescribeApplicationVersionsResponse :: DescribeApplicationVersionsResponse -> TestTree
testDescribeApplicationVersionsResponse = res
    "DescribeApplicationVersionsResponse"
    "fixture/DescribeApplicationVersionsResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy DescribeApplicationVersions)

testCreateEnvironmentResponse :: EnvironmentDescription -> TestTree
testCreateEnvironmentResponse = res
    "CreateEnvironmentResponse"
    "fixture/CreateEnvironmentResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy CreateEnvironment)
