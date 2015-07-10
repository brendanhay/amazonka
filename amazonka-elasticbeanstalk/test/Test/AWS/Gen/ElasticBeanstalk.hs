{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ElasticBeanstalk
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
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
    (Proxy :: Proxy DescribeApplications)

testCreateApplicationVersionResponse :: ApplicationVersionDescriptionMessage -> TestTree
testCreateApplicationVersionResponse = res
    "CreateApplicationVersionResponse"
    "fixture/CreateApplicationVersionResponse"
    (Proxy :: Proxy CreateApplicationVersion)

testUpdateEnvironmentResponse :: EnvironmentDescription -> TestTree
testUpdateEnvironmentResponse = res
    "UpdateEnvironmentResponse"
    "fixture/UpdateEnvironmentResponse"
    (Proxy :: Proxy UpdateEnvironment)

testTerminateEnvironmentResponse :: EnvironmentDescription -> TestTree
testTerminateEnvironmentResponse = res
    "TerminateEnvironmentResponse"
    "fixture/TerminateEnvironmentResponse"
    (Proxy :: Proxy TerminateEnvironment)

testDescribeEventsResponse :: DescribeEventsResponse -> TestTree
testDescribeEventsResponse = res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse"
    (Proxy :: Proxy DescribeEvents)

testRequestEnvironmentInfoResponse :: RequestEnvironmentInfoResponse -> TestTree
testRequestEnvironmentInfoResponse = res
    "RequestEnvironmentInfoResponse"
    "fixture/RequestEnvironmentInfoResponse"
    (Proxy :: Proxy RequestEnvironmentInfo)

testRetrieveEnvironmentInfoResponse :: RetrieveEnvironmentInfoResponse -> TestTree
testRetrieveEnvironmentInfoResponse = res
    "RetrieveEnvironmentInfoResponse"
    "fixture/RetrieveEnvironmentInfoResponse"
    (Proxy :: Proxy RetrieveEnvironmentInfo)

testDeleteApplicationResponse :: DeleteApplicationResponse -> TestTree
testDeleteApplicationResponse = res
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse"
    (Proxy :: Proxy DeleteApplication)

testUpdateApplicationResponse :: ApplicationDescriptionMessage -> TestTree
testUpdateApplicationResponse = res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse"
    (Proxy :: Proxy UpdateApplication)

testCreateApplicationResponse :: ApplicationDescriptionMessage -> TestTree
testCreateApplicationResponse = res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse"
    (Proxy :: Proxy CreateApplication)

testAbortEnvironmentUpdateResponse :: AbortEnvironmentUpdateResponse -> TestTree
testAbortEnvironmentUpdateResponse = res
    "AbortEnvironmentUpdateResponse"
    "fixture/AbortEnvironmentUpdateResponse"
    (Proxy :: Proxy AbortEnvironmentUpdate)

testDeleteApplicationVersionResponse :: DeleteApplicationVersionResponse -> TestTree
testDeleteApplicationVersionResponse = res
    "DeleteApplicationVersionResponse"
    "fixture/DeleteApplicationVersionResponse"
    (Proxy :: Proxy DeleteApplicationVersion)

testUpdateApplicationVersionResponse :: ApplicationVersionDescriptionMessage -> TestTree
testUpdateApplicationVersionResponse = res
    "UpdateApplicationVersionResponse"
    "fixture/UpdateApplicationVersionResponse"
    (Proxy :: Proxy UpdateApplicationVersion)

testDescribeEnvironmentResourcesResponse :: DescribeEnvironmentResourcesResponse -> TestTree
testDescribeEnvironmentResourcesResponse = res
    "DescribeEnvironmentResourcesResponse"
    "fixture/DescribeEnvironmentResourcesResponse"
    (Proxy :: Proxy DescribeEnvironmentResources)

testDeleteConfigurationTemplateResponse :: DeleteConfigurationTemplateResponse -> TestTree
testDeleteConfigurationTemplateResponse = res
    "DeleteConfigurationTemplateResponse"
    "fixture/DeleteConfigurationTemplateResponse"
    (Proxy :: Proxy DeleteConfigurationTemplate)

testUpdateConfigurationTemplateResponse :: ConfigurationSettingsDescription -> TestTree
testUpdateConfigurationTemplateResponse = res
    "UpdateConfigurationTemplateResponse"
    "fixture/UpdateConfigurationTemplateResponse"
    (Proxy :: Proxy UpdateConfigurationTemplate)

testRebuildEnvironmentResponse :: RebuildEnvironmentResponse -> TestTree
testRebuildEnvironmentResponse = res
    "RebuildEnvironmentResponse"
    "fixture/RebuildEnvironmentResponse"
    (Proxy :: Proxy RebuildEnvironment)

testDeleteEnvironmentConfigurationResponse :: DeleteEnvironmentConfigurationResponse -> TestTree
testDeleteEnvironmentConfigurationResponse = res
    "DeleteEnvironmentConfigurationResponse"
    "fixture/DeleteEnvironmentConfigurationResponse"
    (Proxy :: Proxy DeleteEnvironmentConfiguration)

testCreateConfigurationTemplateResponse :: ConfigurationSettingsDescription -> TestTree
testCreateConfigurationTemplateResponse = res
    "CreateConfigurationTemplateResponse"
    "fixture/CreateConfigurationTemplateResponse"
    (Proxy :: Proxy CreateConfigurationTemplate)

testListAvailableSolutionStacksResponse :: ListAvailableSolutionStacksResponse -> TestTree
testListAvailableSolutionStacksResponse = res
    "ListAvailableSolutionStacksResponse"
    "fixture/ListAvailableSolutionStacksResponse"
    (Proxy :: Proxy ListAvailableSolutionStacks)

testSwapEnvironmentCNAMEsResponse :: SwapEnvironmentCNAMEsResponse -> TestTree
testSwapEnvironmentCNAMEsResponse = res
    "SwapEnvironmentCNAMEsResponse"
    "fixture/SwapEnvironmentCNAMEsResponse"
    (Proxy :: Proxy SwapEnvironmentCNAMEs)

testDescribeConfigurationOptionsResponse :: DescribeConfigurationOptionsResponse -> TestTree
testDescribeConfigurationOptionsResponse = res
    "DescribeConfigurationOptionsResponse"
    "fixture/DescribeConfigurationOptionsResponse"
    (Proxy :: Proxy DescribeConfigurationOptions)

testDescribeConfigurationSettingsResponse :: DescribeConfigurationSettingsResponse -> TestTree
testDescribeConfigurationSettingsResponse = res
    "DescribeConfigurationSettingsResponse"
    "fixture/DescribeConfigurationSettingsResponse"
    (Proxy :: Proxy DescribeConfigurationSettings)

testCreateStorageLocationResponse :: CreateStorageLocationResponse -> TestTree
testCreateStorageLocationResponse = res
    "CreateStorageLocationResponse"
    "fixture/CreateStorageLocationResponse"
    (Proxy :: Proxy CreateStorageLocation)

testDescribeEnvironmentsResponse :: DescribeEnvironmentsResponse -> TestTree
testDescribeEnvironmentsResponse = res
    "DescribeEnvironmentsResponse"
    "fixture/DescribeEnvironmentsResponse"
    (Proxy :: Proxy DescribeEnvironments)

testRestartAppServerResponse :: RestartAppServerResponse -> TestTree
testRestartAppServerResponse = res
    "RestartAppServerResponse"
    "fixture/RestartAppServerResponse"
    (Proxy :: Proxy RestartAppServer)

testValidateConfigurationSettingsResponse :: ValidateConfigurationSettingsResponse -> TestTree
testValidateConfigurationSettingsResponse = res
    "ValidateConfigurationSettingsResponse"
    "fixture/ValidateConfigurationSettingsResponse"
    (Proxy :: Proxy ValidateConfigurationSettings)

testDescribeApplicationVersionsResponse :: DescribeApplicationVersionsResponse -> TestTree
testDescribeApplicationVersionsResponse = res
    "DescribeApplicationVersionsResponse"
    "fixture/DescribeApplicationVersionsResponse"
    (Proxy :: Proxy DescribeApplicationVersions)

testCheckDNSAvailabilityResponse :: CheckDNSAvailabilityResponse -> TestTree
testCheckDNSAvailabilityResponse = res
    "CheckDNSAvailabilityResponse"
    "fixture/CheckDNSAvailabilityResponse"
    (Proxy :: Proxy CheckDNSAvailability)

testCreateEnvironmentResponse :: EnvironmentDescription -> TestTree
testCreateEnvironmentResponse = res
    "CreateEnvironmentResponse"
    "fixture/CreateEnvironmentResponse"
    (Proxy :: Proxy CreateEnvironment)

instance Out AbortEnvironmentUpdate
instance Out AbortEnvironmentUpdateResponse
instance Out ApplicationDescription
instance Out ApplicationDescriptionMessage
instance Out ApplicationVersionDescription
instance Out ApplicationVersionDescriptionMessage
instance Out AutoScalingGroup
instance Out CheckDNSAvailability
instance Out CheckDNSAvailabilityResponse
instance Out ConfigurationDeploymentStatus
instance Out ConfigurationOptionDescription
instance Out ConfigurationOptionSetting
instance Out ConfigurationOptionValueType
instance Out ConfigurationSettingsDescription
instance Out CreateApplication
instance Out CreateApplicationVersion
instance Out CreateConfigurationTemplate
instance Out CreateEnvironment
instance Out CreateStorageLocation
instance Out CreateStorageLocationResponse
instance Out DeleteApplication
instance Out DeleteApplicationResponse
instance Out DeleteApplicationVersion
instance Out DeleteApplicationVersionResponse
instance Out DeleteConfigurationTemplate
instance Out DeleteConfigurationTemplateResponse
instance Out DeleteEnvironmentConfiguration
instance Out DeleteEnvironmentConfigurationResponse
instance Out DescribeApplicationVersions
instance Out DescribeApplicationVersionsResponse
instance Out DescribeApplications
instance Out DescribeApplicationsResponse
instance Out DescribeConfigurationOptions
instance Out DescribeConfigurationOptionsResponse
instance Out DescribeConfigurationSettings
instance Out DescribeConfigurationSettingsResponse
instance Out DescribeEnvironmentResources
instance Out DescribeEnvironmentResourcesResponse
instance Out DescribeEnvironments
instance Out DescribeEnvironmentsResponse
instance Out DescribeEvents
instance Out DescribeEventsResponse
instance Out EnvironmentDescription
instance Out EnvironmentHealth
instance Out EnvironmentInfoDescription
instance Out EnvironmentInfoType
instance Out EnvironmentResourceDescription
instance Out EnvironmentResourcesDescription
instance Out EnvironmentStatus
instance Out EnvironmentTier
instance Out EventDescription
instance Out EventSeverity
instance Out Instance
instance Out LaunchConfiguration
instance Out ListAvailableSolutionStacks
instance Out ListAvailableSolutionStacksResponse
instance Out Listener
instance Out LoadBalancer
instance Out LoadBalancerDescription
instance Out OptionRestrictionRegex
instance Out OptionSpecification
instance Out Queue
instance Out RebuildEnvironment
instance Out RebuildEnvironmentResponse
instance Out RequestEnvironmentInfo
instance Out RequestEnvironmentInfoResponse
instance Out RestartAppServer
instance Out RestartAppServerResponse
instance Out RetrieveEnvironmentInfo
instance Out RetrieveEnvironmentInfoResponse
instance Out S3Location
instance Out SolutionStackDescription
instance Out SourceConfiguration
instance Out SwapEnvironmentCNAMEs
instance Out SwapEnvironmentCNAMEsResponse
instance Out Tag
instance Out TerminateEnvironment
instance Out Trigger
instance Out UpdateApplication
instance Out UpdateApplicationVersion
instance Out UpdateConfigurationTemplate
instance Out UpdateEnvironment
instance Out ValidateConfigurationSettings
instance Out ValidateConfigurationSettingsResponse
instance Out ValidationMessage
instance Out ValidationSeverity
