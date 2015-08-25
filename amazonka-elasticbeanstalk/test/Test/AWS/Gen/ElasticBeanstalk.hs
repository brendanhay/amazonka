{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ElasticBeanstalk
-- Copyright   : (c) 2013-2015 Brendan Hay
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
--         , testDescribeInstancesHealth $
--             describeInstancesHealth
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
--         , testDescribeEnvironmentHealth $
--             describeEnvironmentHealth
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
--         , testDescribeInstancesHealthResponse $
--             describeInstancesHealthResponse
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
--         , testDescribeEnvironmentHealthResponse $
--             describeEnvironmentHealthResponse
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
testDescribeApplications = req
    "DescribeApplications"
    "fixture/DescribeApplications"

testCreateApplicationVersion :: CreateApplicationVersion -> TestTree
testCreateApplicationVersion = req
    "CreateApplicationVersion"
    "fixture/CreateApplicationVersion"

testUpdateEnvironment :: UpdateEnvironment -> TestTree
testUpdateEnvironment = req
    "UpdateEnvironment"
    "fixture/UpdateEnvironment"

testTerminateEnvironment :: TerminateEnvironment -> TestTree
testTerminateEnvironment = req
    "TerminateEnvironment"
    "fixture/TerminateEnvironment"

testDescribeEvents :: DescribeEvents -> TestTree
testDescribeEvents = req
    "DescribeEvents"
    "fixture/DescribeEvents"

testRequestEnvironmentInfo :: RequestEnvironmentInfo -> TestTree
testRequestEnvironmentInfo = req
    "RequestEnvironmentInfo"
    "fixture/RequestEnvironmentInfo"

testRetrieveEnvironmentInfo :: RetrieveEnvironmentInfo -> TestTree
testRetrieveEnvironmentInfo = req
    "RetrieveEnvironmentInfo"
    "fixture/RetrieveEnvironmentInfo"

testDeleteApplication :: DeleteApplication -> TestTree
testDeleteApplication = req
    "DeleteApplication"
    "fixture/DeleteApplication"

testUpdateApplication :: UpdateApplication -> TestTree
testUpdateApplication = req
    "UpdateApplication"
    "fixture/UpdateApplication"

testDescribeInstancesHealth :: DescribeInstancesHealth -> TestTree
testDescribeInstancesHealth = req
    "DescribeInstancesHealth"
    "fixture/DescribeInstancesHealth"

testCreateApplication :: CreateApplication -> TestTree
testCreateApplication = req
    "CreateApplication"
    "fixture/CreateApplication"

testAbortEnvironmentUpdate :: AbortEnvironmentUpdate -> TestTree
testAbortEnvironmentUpdate = req
    "AbortEnvironmentUpdate"
    "fixture/AbortEnvironmentUpdate"

testDeleteApplicationVersion :: DeleteApplicationVersion -> TestTree
testDeleteApplicationVersion = req
    "DeleteApplicationVersion"
    "fixture/DeleteApplicationVersion"

testUpdateApplicationVersion :: UpdateApplicationVersion -> TestTree
testUpdateApplicationVersion = req
    "UpdateApplicationVersion"
    "fixture/UpdateApplicationVersion"

testDescribeEnvironmentResources :: DescribeEnvironmentResources -> TestTree
testDescribeEnvironmentResources = req
    "DescribeEnvironmentResources"
    "fixture/DescribeEnvironmentResources"

testDeleteConfigurationTemplate :: DeleteConfigurationTemplate -> TestTree
testDeleteConfigurationTemplate = req
    "DeleteConfigurationTemplate"
    "fixture/DeleteConfigurationTemplate"

testUpdateConfigurationTemplate :: UpdateConfigurationTemplate -> TestTree
testUpdateConfigurationTemplate = req
    "UpdateConfigurationTemplate"
    "fixture/UpdateConfigurationTemplate"

testDescribeEnvironmentHealth :: DescribeEnvironmentHealth -> TestTree
testDescribeEnvironmentHealth = req
    "DescribeEnvironmentHealth"
    "fixture/DescribeEnvironmentHealth"

testRebuildEnvironment :: RebuildEnvironment -> TestTree
testRebuildEnvironment = req
    "RebuildEnvironment"
    "fixture/RebuildEnvironment"

testDeleteEnvironmentConfiguration :: DeleteEnvironmentConfiguration -> TestTree
testDeleteEnvironmentConfiguration = req
    "DeleteEnvironmentConfiguration"
    "fixture/DeleteEnvironmentConfiguration"

testCreateConfigurationTemplate :: CreateConfigurationTemplate -> TestTree
testCreateConfigurationTemplate = req
    "CreateConfigurationTemplate"
    "fixture/CreateConfigurationTemplate"

testListAvailableSolutionStacks :: ListAvailableSolutionStacks -> TestTree
testListAvailableSolutionStacks = req
    "ListAvailableSolutionStacks"
    "fixture/ListAvailableSolutionStacks"

testSwapEnvironmentCNAMEs :: SwapEnvironmentCNAMEs -> TestTree
testSwapEnvironmentCNAMEs = req
    "SwapEnvironmentCNAMEs"
    "fixture/SwapEnvironmentCNAMEs"

testDescribeConfigurationOptions :: DescribeConfigurationOptions -> TestTree
testDescribeConfigurationOptions = req
    "DescribeConfigurationOptions"
    "fixture/DescribeConfigurationOptions"

testDescribeConfigurationSettings :: DescribeConfigurationSettings -> TestTree
testDescribeConfigurationSettings = req
    "DescribeConfigurationSettings"
    "fixture/DescribeConfigurationSettings"

testCreateStorageLocation :: CreateStorageLocation -> TestTree
testCreateStorageLocation = req
    "CreateStorageLocation"
    "fixture/CreateStorageLocation"

testDescribeEnvironments :: DescribeEnvironments -> TestTree
testDescribeEnvironments = req
    "DescribeEnvironments"
    "fixture/DescribeEnvironments"

testRestartAppServer :: RestartAppServer -> TestTree
testRestartAppServer = req
    "RestartAppServer"
    "fixture/RestartAppServer"

testValidateConfigurationSettings :: ValidateConfigurationSettings -> TestTree
testValidateConfigurationSettings = req
    "ValidateConfigurationSettings"
    "fixture/ValidateConfigurationSettings"

testDescribeApplicationVersions :: DescribeApplicationVersions -> TestTree
testDescribeApplicationVersions = req
    "DescribeApplicationVersions"
    "fixture/DescribeApplicationVersions"

testCheckDNSAvailability :: CheckDNSAvailability -> TestTree
testCheckDNSAvailability = req
    "CheckDNSAvailability"
    "fixture/CheckDNSAvailability"

testCreateEnvironment :: CreateEnvironment -> TestTree
testCreateEnvironment = req
    "CreateEnvironment"
    "fixture/CreateEnvironment"

-- Responses

testDescribeApplicationsResponse :: DescribeApplicationsResponse -> TestTree
testDescribeApplicationsResponse = res
    "DescribeApplicationsResponse"
    "fixture/DescribeApplicationsResponse"
    elasticBeanstalk
    (Proxy :: Proxy DescribeApplications)

testCreateApplicationVersionResponse :: ApplicationVersionDescriptionMessage -> TestTree
testCreateApplicationVersionResponse = res
    "CreateApplicationVersionResponse"
    "fixture/CreateApplicationVersionResponse"
    elasticBeanstalk
    (Proxy :: Proxy CreateApplicationVersion)

testUpdateEnvironmentResponse :: EnvironmentDescription -> TestTree
testUpdateEnvironmentResponse = res
    "UpdateEnvironmentResponse"
    "fixture/UpdateEnvironmentResponse"
    elasticBeanstalk
    (Proxy :: Proxy UpdateEnvironment)

testTerminateEnvironmentResponse :: EnvironmentDescription -> TestTree
testTerminateEnvironmentResponse = res
    "TerminateEnvironmentResponse"
    "fixture/TerminateEnvironmentResponse"
    elasticBeanstalk
    (Proxy :: Proxy TerminateEnvironment)

testDescribeEventsResponse :: DescribeEventsResponse -> TestTree
testDescribeEventsResponse = res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse"
    elasticBeanstalk
    (Proxy :: Proxy DescribeEvents)

testRequestEnvironmentInfoResponse :: RequestEnvironmentInfoResponse -> TestTree
testRequestEnvironmentInfoResponse = res
    "RequestEnvironmentInfoResponse"
    "fixture/RequestEnvironmentInfoResponse"
    elasticBeanstalk
    (Proxy :: Proxy RequestEnvironmentInfo)

testRetrieveEnvironmentInfoResponse :: RetrieveEnvironmentInfoResponse -> TestTree
testRetrieveEnvironmentInfoResponse = res
    "RetrieveEnvironmentInfoResponse"
    "fixture/RetrieveEnvironmentInfoResponse"
    elasticBeanstalk
    (Proxy :: Proxy RetrieveEnvironmentInfo)

testDeleteApplicationResponse :: DeleteApplicationResponse -> TestTree
testDeleteApplicationResponse = res
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse"
    elasticBeanstalk
    (Proxy :: Proxy DeleteApplication)

testUpdateApplicationResponse :: ApplicationDescriptionMessage -> TestTree
testUpdateApplicationResponse = res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse"
    elasticBeanstalk
    (Proxy :: Proxy UpdateApplication)

testDescribeInstancesHealthResponse :: DescribeInstancesHealthResponse -> TestTree
testDescribeInstancesHealthResponse = res
    "DescribeInstancesHealthResponse"
    "fixture/DescribeInstancesHealthResponse"
    elasticBeanstalk
    (Proxy :: Proxy DescribeInstancesHealth)

testCreateApplicationResponse :: ApplicationDescriptionMessage -> TestTree
testCreateApplicationResponse = res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse"
    elasticBeanstalk
    (Proxy :: Proxy CreateApplication)

testAbortEnvironmentUpdateResponse :: AbortEnvironmentUpdateResponse -> TestTree
testAbortEnvironmentUpdateResponse = res
    "AbortEnvironmentUpdateResponse"
    "fixture/AbortEnvironmentUpdateResponse"
    elasticBeanstalk
    (Proxy :: Proxy AbortEnvironmentUpdate)

testDeleteApplicationVersionResponse :: DeleteApplicationVersionResponse -> TestTree
testDeleteApplicationVersionResponse = res
    "DeleteApplicationVersionResponse"
    "fixture/DeleteApplicationVersionResponse"
    elasticBeanstalk
    (Proxy :: Proxy DeleteApplicationVersion)

testUpdateApplicationVersionResponse :: ApplicationVersionDescriptionMessage -> TestTree
testUpdateApplicationVersionResponse = res
    "UpdateApplicationVersionResponse"
    "fixture/UpdateApplicationVersionResponse"
    elasticBeanstalk
    (Proxy :: Proxy UpdateApplicationVersion)

testDescribeEnvironmentResourcesResponse :: DescribeEnvironmentResourcesResponse -> TestTree
testDescribeEnvironmentResourcesResponse = res
    "DescribeEnvironmentResourcesResponse"
    "fixture/DescribeEnvironmentResourcesResponse"
    elasticBeanstalk
    (Proxy :: Proxy DescribeEnvironmentResources)

testDeleteConfigurationTemplateResponse :: DeleteConfigurationTemplateResponse -> TestTree
testDeleteConfigurationTemplateResponse = res
    "DeleteConfigurationTemplateResponse"
    "fixture/DeleteConfigurationTemplateResponse"
    elasticBeanstalk
    (Proxy :: Proxy DeleteConfigurationTemplate)

testUpdateConfigurationTemplateResponse :: ConfigurationSettingsDescription -> TestTree
testUpdateConfigurationTemplateResponse = res
    "UpdateConfigurationTemplateResponse"
    "fixture/UpdateConfigurationTemplateResponse"
    elasticBeanstalk
    (Proxy :: Proxy UpdateConfigurationTemplate)

testDescribeEnvironmentHealthResponse :: DescribeEnvironmentHealthResponse -> TestTree
testDescribeEnvironmentHealthResponse = res
    "DescribeEnvironmentHealthResponse"
    "fixture/DescribeEnvironmentHealthResponse"
    elasticBeanstalk
    (Proxy :: Proxy DescribeEnvironmentHealth)

testRebuildEnvironmentResponse :: RebuildEnvironmentResponse -> TestTree
testRebuildEnvironmentResponse = res
    "RebuildEnvironmentResponse"
    "fixture/RebuildEnvironmentResponse"
    elasticBeanstalk
    (Proxy :: Proxy RebuildEnvironment)

testDeleteEnvironmentConfigurationResponse :: DeleteEnvironmentConfigurationResponse -> TestTree
testDeleteEnvironmentConfigurationResponse = res
    "DeleteEnvironmentConfigurationResponse"
    "fixture/DeleteEnvironmentConfigurationResponse"
    elasticBeanstalk
    (Proxy :: Proxy DeleteEnvironmentConfiguration)

testCreateConfigurationTemplateResponse :: ConfigurationSettingsDescription -> TestTree
testCreateConfigurationTemplateResponse = res
    "CreateConfigurationTemplateResponse"
    "fixture/CreateConfigurationTemplateResponse"
    elasticBeanstalk
    (Proxy :: Proxy CreateConfigurationTemplate)

testListAvailableSolutionStacksResponse :: ListAvailableSolutionStacksResponse -> TestTree
testListAvailableSolutionStacksResponse = res
    "ListAvailableSolutionStacksResponse"
    "fixture/ListAvailableSolutionStacksResponse"
    elasticBeanstalk
    (Proxy :: Proxy ListAvailableSolutionStacks)

testSwapEnvironmentCNAMEsResponse :: SwapEnvironmentCNAMEsResponse -> TestTree
testSwapEnvironmentCNAMEsResponse = res
    "SwapEnvironmentCNAMEsResponse"
    "fixture/SwapEnvironmentCNAMEsResponse"
    elasticBeanstalk
    (Proxy :: Proxy SwapEnvironmentCNAMEs)

testDescribeConfigurationOptionsResponse :: DescribeConfigurationOptionsResponse -> TestTree
testDescribeConfigurationOptionsResponse = res
    "DescribeConfigurationOptionsResponse"
    "fixture/DescribeConfigurationOptionsResponse"
    elasticBeanstalk
    (Proxy :: Proxy DescribeConfigurationOptions)

testDescribeConfigurationSettingsResponse :: DescribeConfigurationSettingsResponse -> TestTree
testDescribeConfigurationSettingsResponse = res
    "DescribeConfigurationSettingsResponse"
    "fixture/DescribeConfigurationSettingsResponse"
    elasticBeanstalk
    (Proxy :: Proxy DescribeConfigurationSettings)

testCreateStorageLocationResponse :: CreateStorageLocationResponse -> TestTree
testCreateStorageLocationResponse = res
    "CreateStorageLocationResponse"
    "fixture/CreateStorageLocationResponse"
    elasticBeanstalk
    (Proxy :: Proxy CreateStorageLocation)

testDescribeEnvironmentsResponse :: DescribeEnvironmentsResponse -> TestTree
testDescribeEnvironmentsResponse = res
    "DescribeEnvironmentsResponse"
    "fixture/DescribeEnvironmentsResponse"
    elasticBeanstalk
    (Proxy :: Proxy DescribeEnvironments)

testRestartAppServerResponse :: RestartAppServerResponse -> TestTree
testRestartAppServerResponse = res
    "RestartAppServerResponse"
    "fixture/RestartAppServerResponse"
    elasticBeanstalk
    (Proxy :: Proxy RestartAppServer)

testValidateConfigurationSettingsResponse :: ValidateConfigurationSettingsResponse -> TestTree
testValidateConfigurationSettingsResponse = res
    "ValidateConfigurationSettingsResponse"
    "fixture/ValidateConfigurationSettingsResponse"
    elasticBeanstalk
    (Proxy :: Proxy ValidateConfigurationSettings)

testDescribeApplicationVersionsResponse :: DescribeApplicationVersionsResponse -> TestTree
testDescribeApplicationVersionsResponse = res
    "DescribeApplicationVersionsResponse"
    "fixture/DescribeApplicationVersionsResponse"
    elasticBeanstalk
    (Proxy :: Proxy DescribeApplicationVersions)

testCheckDNSAvailabilityResponse :: CheckDNSAvailabilityResponse -> TestTree
testCheckDNSAvailabilityResponse = res
    "CheckDNSAvailabilityResponse"
    "fixture/CheckDNSAvailabilityResponse"
    elasticBeanstalk
    (Proxy :: Proxy CheckDNSAvailability)

testCreateEnvironmentResponse :: EnvironmentDescription -> TestTree
testCreateEnvironmentResponse = res
    "CreateEnvironmentResponse"
    "fixture/CreateEnvironmentResponse"
    elasticBeanstalk
    (Proxy :: Proxy CreateEnvironment)
