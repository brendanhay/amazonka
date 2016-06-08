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
--         [ requestDescribeApplications $
--             describeApplications
--
--         , requestUpdateEnvironment $
--             updateEnvironment
--
--         , requestTerminateEnvironment $
--             terminateEnvironment
--
--         , requestCreateApplicationVersion $
--             createApplicationVersion
--
--         , requestDescribeEvents $
--             describeEvents
--
--         , requestRequestEnvironmentInfo $
--             requestEnvironmentInfo
--
--         , requestRetrieveEnvironmentInfo $
--             retrieveEnvironmentInfo
--
--         , requestDeleteApplication $
--             deleteApplication
--
--         , requestUpdateApplication $
--             updateApplication
--
--         , requestDescribeInstancesHealth $
--             describeInstancesHealth
--
--         , requestCreateApplication $
--             createApplication
--
--         , requestComposeEnvironments $
--             composeEnvironments
--
--         , requestAbortEnvironmentUpdate $
--             abortEnvironmentUpdate
--
--         , requestDeleteConfigurationTemplate $
--             deleteConfigurationTemplate
--
--         , requestUpdateConfigurationTemplate $
--             updateConfigurationTemplate
--
--         , requestDescribeEnvironmentResources $
--             describeEnvironmentResources
--
--         , requestDescribeEnvironmentManagedActionHistory $
--             describeEnvironmentManagedActionHistory
--
--         , requestDeleteApplicationVersion $
--             deleteApplicationVersion
--
--         , requestUpdateApplicationVersion $
--             updateApplicationVersion
--
--         , requestCreateConfigurationTemplate $
--             createConfigurationTemplate
--
--         , requestDescribeEnvironmentHealth $
--             describeEnvironmentHealth
--
--         , requestRebuildEnvironment $
--             rebuildEnvironment
--
--         , requestDeleteEnvironmentConfiguration $
--             deleteEnvironmentConfiguration
--
--         , requestSwapEnvironmentCNAMEs $
--             swapEnvironmentCNAMEs
--
--         , requestListAvailableSolutionStacks $
--             listAvailableSolutionStacks
--
--         , requestApplyEnvironmentManagedAction $
--             applyEnvironmentManagedAction
--
--         , requestDescribeConfigurationOptions $
--             describeConfigurationOptions
--
--         , requestCreateStorageLocation $
--             createStorageLocation
--
--         , requestDescribeEnvironmentManagedActions $
--             describeEnvironmentManagedActions
--
--         , requestDescribeConfigurationSettings $
--             describeConfigurationSettings
--
--         , requestValidateConfigurationSettings $
--             validateConfigurationSettings
--
--         , requestRestartAppServer $
--             restartAppServer
--
--         , requestDescribeEnvironments $
--             describeEnvironments
--
--         , requestCheckDNSAvailability $
--             checkDNSAvailability
--
--         , requestDescribeApplicationVersions $
--             describeApplicationVersions
--
--         , requestCreateEnvironment $
--             createEnvironment
--
--           ]

--     , testGroup "response"
--         [ responseDescribeApplications $
--             describeApplicationsResponse
--
--         , responseUpdateEnvironment $
--             environmentDescription
--
--         , responseTerminateEnvironment $
--             environmentDescription
--
--         , responseCreateApplicationVersion $
--             applicationVersionDescriptionMessage
--
--         , responseDescribeEvents $
--             describeEventsResponse
--
--         , responseRequestEnvironmentInfo $
--             requestEnvironmentInfoResponse
--
--         , responseRetrieveEnvironmentInfo $
--             retrieveEnvironmentInfoResponse
--
--         , responseDeleteApplication $
--             deleteApplicationResponse
--
--         , responseUpdateApplication $
--             applicationDescriptionMessage
--
--         , responseDescribeInstancesHealth $
--             describeInstancesHealthResponse
--
--         , responseCreateApplication $
--             applicationDescriptionMessage
--
--         , responseComposeEnvironments $
--             environmentDescriptionsMessage
--
--         , responseAbortEnvironmentUpdate $
--             abortEnvironmentUpdateResponse
--
--         , responseDeleteConfigurationTemplate $
--             deleteConfigurationTemplateResponse
--
--         , responseUpdateConfigurationTemplate $
--             configurationSettingsDescription
--
--         , responseDescribeEnvironmentResources $
--             describeEnvironmentResourcesResponse
--
--         , responseDescribeEnvironmentManagedActionHistory $
--             describeEnvironmentManagedActionHistoryResponse
--
--         , responseDeleteApplicationVersion $
--             deleteApplicationVersionResponse
--
--         , responseUpdateApplicationVersion $
--             applicationVersionDescriptionMessage
--
--         , responseCreateConfigurationTemplate $
--             configurationSettingsDescription
--
--         , responseDescribeEnvironmentHealth $
--             describeEnvironmentHealthResponse
--
--         , responseRebuildEnvironment $
--             rebuildEnvironmentResponse
--
--         , responseDeleteEnvironmentConfiguration $
--             deleteEnvironmentConfigurationResponse
--
--         , responseSwapEnvironmentCNAMEs $
--             swapEnvironmentCNAMEsResponse
--
--         , responseListAvailableSolutionStacks $
--             listAvailableSolutionStacksResponse
--
--         , responseApplyEnvironmentManagedAction $
--             applyEnvironmentManagedActionResponse
--
--         , responseDescribeConfigurationOptions $
--             describeConfigurationOptionsResponse
--
--         , responseCreateStorageLocation $
--             createStorageLocationResponse
--
--         , responseDescribeEnvironmentManagedActions $
--             describeEnvironmentManagedActionsResponse
--
--         , responseDescribeConfigurationSettings $
--             describeConfigurationSettingsResponse
--
--         , responseValidateConfigurationSettings $
--             validateConfigurationSettingsResponse
--
--         , responseRestartAppServer $
--             restartAppServerResponse
--
--         , responseDescribeEnvironments $
--             environmentDescriptionsMessage
--
--         , responseCheckDNSAvailability $
--             checkDNSAvailabilityResponse
--
--         , responseDescribeApplicationVersions $
--             describeApplicationVersionsResponse
--
--         , responseCreateEnvironment $
--             environmentDescription
--
--           ]
--     ]

-- Requests

requestDescribeApplications :: DescribeApplications -> TestTree
requestDescribeApplications = req
    "DescribeApplications"
    "fixture/DescribeApplications.yaml"

requestUpdateEnvironment :: UpdateEnvironment -> TestTree
requestUpdateEnvironment = req
    "UpdateEnvironment"
    "fixture/UpdateEnvironment.yaml"

requestTerminateEnvironment :: TerminateEnvironment -> TestTree
requestTerminateEnvironment = req
    "TerminateEnvironment"
    "fixture/TerminateEnvironment.yaml"

requestCreateApplicationVersion :: CreateApplicationVersion -> TestTree
requestCreateApplicationVersion = req
    "CreateApplicationVersion"
    "fixture/CreateApplicationVersion.yaml"

requestDescribeEvents :: DescribeEvents -> TestTree
requestDescribeEvents = req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

requestRequestEnvironmentInfo :: RequestEnvironmentInfo -> TestTree
requestRequestEnvironmentInfo = req
    "RequestEnvironmentInfo"
    "fixture/RequestEnvironmentInfo.yaml"

requestRetrieveEnvironmentInfo :: RetrieveEnvironmentInfo -> TestTree
requestRetrieveEnvironmentInfo = req
    "RetrieveEnvironmentInfo"
    "fixture/RetrieveEnvironmentInfo.yaml"

requestDeleteApplication :: DeleteApplication -> TestTree
requestDeleteApplication = req
    "DeleteApplication"
    "fixture/DeleteApplication.yaml"

requestUpdateApplication :: UpdateApplication -> TestTree
requestUpdateApplication = req
    "UpdateApplication"
    "fixture/UpdateApplication.yaml"

requestDescribeInstancesHealth :: DescribeInstancesHealth -> TestTree
requestDescribeInstancesHealth = req
    "DescribeInstancesHealth"
    "fixture/DescribeInstancesHealth.yaml"

requestCreateApplication :: CreateApplication -> TestTree
requestCreateApplication = req
    "CreateApplication"
    "fixture/CreateApplication.yaml"

requestComposeEnvironments :: ComposeEnvironments -> TestTree
requestComposeEnvironments = req
    "ComposeEnvironments"
    "fixture/ComposeEnvironments.yaml"

requestAbortEnvironmentUpdate :: AbortEnvironmentUpdate -> TestTree
requestAbortEnvironmentUpdate = req
    "AbortEnvironmentUpdate"
    "fixture/AbortEnvironmentUpdate.yaml"

requestDeleteConfigurationTemplate :: DeleteConfigurationTemplate -> TestTree
requestDeleteConfigurationTemplate = req
    "DeleteConfigurationTemplate"
    "fixture/DeleteConfigurationTemplate.yaml"

requestUpdateConfigurationTemplate :: UpdateConfigurationTemplate -> TestTree
requestUpdateConfigurationTemplate = req
    "UpdateConfigurationTemplate"
    "fixture/UpdateConfigurationTemplate.yaml"

requestDescribeEnvironmentResources :: DescribeEnvironmentResources -> TestTree
requestDescribeEnvironmentResources = req
    "DescribeEnvironmentResources"
    "fixture/DescribeEnvironmentResources.yaml"

requestDescribeEnvironmentManagedActionHistory :: DescribeEnvironmentManagedActionHistory -> TestTree
requestDescribeEnvironmentManagedActionHistory = req
    "DescribeEnvironmentManagedActionHistory"
    "fixture/DescribeEnvironmentManagedActionHistory.yaml"

requestDeleteApplicationVersion :: DeleteApplicationVersion -> TestTree
requestDeleteApplicationVersion = req
    "DeleteApplicationVersion"
    "fixture/DeleteApplicationVersion.yaml"

requestUpdateApplicationVersion :: UpdateApplicationVersion -> TestTree
requestUpdateApplicationVersion = req
    "UpdateApplicationVersion"
    "fixture/UpdateApplicationVersion.yaml"

requestCreateConfigurationTemplate :: CreateConfigurationTemplate -> TestTree
requestCreateConfigurationTemplate = req
    "CreateConfigurationTemplate"
    "fixture/CreateConfigurationTemplate.yaml"

requestDescribeEnvironmentHealth :: DescribeEnvironmentHealth -> TestTree
requestDescribeEnvironmentHealth = req
    "DescribeEnvironmentHealth"
    "fixture/DescribeEnvironmentHealth.yaml"

requestRebuildEnvironment :: RebuildEnvironment -> TestTree
requestRebuildEnvironment = req
    "RebuildEnvironment"
    "fixture/RebuildEnvironment.yaml"

requestDeleteEnvironmentConfiguration :: DeleteEnvironmentConfiguration -> TestTree
requestDeleteEnvironmentConfiguration = req
    "DeleteEnvironmentConfiguration"
    "fixture/DeleteEnvironmentConfiguration.yaml"

requestSwapEnvironmentCNAMEs :: SwapEnvironmentCNAMEs -> TestTree
requestSwapEnvironmentCNAMEs = req
    "SwapEnvironmentCNAMEs"
    "fixture/SwapEnvironmentCNAMEs.yaml"

requestListAvailableSolutionStacks :: ListAvailableSolutionStacks -> TestTree
requestListAvailableSolutionStacks = req
    "ListAvailableSolutionStacks"
    "fixture/ListAvailableSolutionStacks.yaml"

requestApplyEnvironmentManagedAction :: ApplyEnvironmentManagedAction -> TestTree
requestApplyEnvironmentManagedAction = req
    "ApplyEnvironmentManagedAction"
    "fixture/ApplyEnvironmentManagedAction.yaml"

requestDescribeConfigurationOptions :: DescribeConfigurationOptions -> TestTree
requestDescribeConfigurationOptions = req
    "DescribeConfigurationOptions"
    "fixture/DescribeConfigurationOptions.yaml"

requestCreateStorageLocation :: CreateStorageLocation -> TestTree
requestCreateStorageLocation = req
    "CreateStorageLocation"
    "fixture/CreateStorageLocation.yaml"

requestDescribeEnvironmentManagedActions :: DescribeEnvironmentManagedActions -> TestTree
requestDescribeEnvironmentManagedActions = req
    "DescribeEnvironmentManagedActions"
    "fixture/DescribeEnvironmentManagedActions.yaml"

requestDescribeConfigurationSettings :: DescribeConfigurationSettings -> TestTree
requestDescribeConfigurationSettings = req
    "DescribeConfigurationSettings"
    "fixture/DescribeConfigurationSettings.yaml"

requestValidateConfigurationSettings :: ValidateConfigurationSettings -> TestTree
requestValidateConfigurationSettings = req
    "ValidateConfigurationSettings"
    "fixture/ValidateConfigurationSettings.yaml"

requestRestartAppServer :: RestartAppServer -> TestTree
requestRestartAppServer = req
    "RestartAppServer"
    "fixture/RestartAppServer.yaml"

requestDescribeEnvironments :: DescribeEnvironments -> TestTree
requestDescribeEnvironments = req
    "DescribeEnvironments"
    "fixture/DescribeEnvironments.yaml"

requestCheckDNSAvailability :: CheckDNSAvailability -> TestTree
requestCheckDNSAvailability = req
    "CheckDNSAvailability"
    "fixture/CheckDNSAvailability.yaml"

requestDescribeApplicationVersions :: DescribeApplicationVersions -> TestTree
requestDescribeApplicationVersions = req
    "DescribeApplicationVersions"
    "fixture/DescribeApplicationVersions.yaml"

requestCreateEnvironment :: CreateEnvironment -> TestTree
requestCreateEnvironment = req
    "CreateEnvironment"
    "fixture/CreateEnvironment.yaml"

-- Responses

responseDescribeApplications :: DescribeApplicationsResponse -> TestTree
responseDescribeApplications = res
    "DescribeApplicationsResponse"
    "fixture/DescribeApplicationsResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy DescribeApplications)

responseUpdateEnvironment :: EnvironmentDescription -> TestTree
responseUpdateEnvironment = res
    "UpdateEnvironmentResponse"
    "fixture/UpdateEnvironmentResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy UpdateEnvironment)

responseTerminateEnvironment :: EnvironmentDescription -> TestTree
responseTerminateEnvironment = res
    "TerminateEnvironmentResponse"
    "fixture/TerminateEnvironmentResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy TerminateEnvironment)

responseCreateApplicationVersion :: ApplicationVersionDescriptionMessage -> TestTree
responseCreateApplicationVersion = res
    "CreateApplicationVersionResponse"
    "fixture/CreateApplicationVersionResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy CreateApplicationVersion)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents = res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy DescribeEvents)

responseRequestEnvironmentInfo :: RequestEnvironmentInfoResponse -> TestTree
responseRequestEnvironmentInfo = res
    "RequestEnvironmentInfoResponse"
    "fixture/RequestEnvironmentInfoResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy RequestEnvironmentInfo)

responseRetrieveEnvironmentInfo :: RetrieveEnvironmentInfoResponse -> TestTree
responseRetrieveEnvironmentInfo = res
    "RetrieveEnvironmentInfoResponse"
    "fixture/RetrieveEnvironmentInfoResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy RetrieveEnvironmentInfo)

responseDeleteApplication :: DeleteApplicationResponse -> TestTree
responseDeleteApplication = res
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy DeleteApplication)

responseUpdateApplication :: ApplicationDescriptionMessage -> TestTree
responseUpdateApplication = res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy UpdateApplication)

responseDescribeInstancesHealth :: DescribeInstancesHealthResponse -> TestTree
responseDescribeInstancesHealth = res
    "DescribeInstancesHealthResponse"
    "fixture/DescribeInstancesHealthResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy DescribeInstancesHealth)

responseCreateApplication :: ApplicationDescriptionMessage -> TestTree
responseCreateApplication = res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy CreateApplication)

responseComposeEnvironments :: EnvironmentDescriptionsMessage -> TestTree
responseComposeEnvironments = res
    "ComposeEnvironmentsResponse"
    "fixture/ComposeEnvironmentsResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy ComposeEnvironments)

responseAbortEnvironmentUpdate :: AbortEnvironmentUpdateResponse -> TestTree
responseAbortEnvironmentUpdate = res
    "AbortEnvironmentUpdateResponse"
    "fixture/AbortEnvironmentUpdateResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy AbortEnvironmentUpdate)

responseDeleteConfigurationTemplate :: DeleteConfigurationTemplateResponse -> TestTree
responseDeleteConfigurationTemplate = res
    "DeleteConfigurationTemplateResponse"
    "fixture/DeleteConfigurationTemplateResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy DeleteConfigurationTemplate)

responseUpdateConfigurationTemplate :: ConfigurationSettingsDescription -> TestTree
responseUpdateConfigurationTemplate = res
    "UpdateConfigurationTemplateResponse"
    "fixture/UpdateConfigurationTemplateResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy UpdateConfigurationTemplate)

responseDescribeEnvironmentResources :: DescribeEnvironmentResourcesResponse -> TestTree
responseDescribeEnvironmentResources = res
    "DescribeEnvironmentResourcesResponse"
    "fixture/DescribeEnvironmentResourcesResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy DescribeEnvironmentResources)

responseDescribeEnvironmentManagedActionHistory :: DescribeEnvironmentManagedActionHistoryResponse -> TestTree
responseDescribeEnvironmentManagedActionHistory = res
    "DescribeEnvironmentManagedActionHistoryResponse"
    "fixture/DescribeEnvironmentManagedActionHistoryResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy DescribeEnvironmentManagedActionHistory)

responseDeleteApplicationVersion :: DeleteApplicationVersionResponse -> TestTree
responseDeleteApplicationVersion = res
    "DeleteApplicationVersionResponse"
    "fixture/DeleteApplicationVersionResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy DeleteApplicationVersion)

responseUpdateApplicationVersion :: ApplicationVersionDescriptionMessage -> TestTree
responseUpdateApplicationVersion = res
    "UpdateApplicationVersionResponse"
    "fixture/UpdateApplicationVersionResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy UpdateApplicationVersion)

responseCreateConfigurationTemplate :: ConfigurationSettingsDescription -> TestTree
responseCreateConfigurationTemplate = res
    "CreateConfigurationTemplateResponse"
    "fixture/CreateConfigurationTemplateResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy CreateConfigurationTemplate)

responseDescribeEnvironmentHealth :: DescribeEnvironmentHealthResponse -> TestTree
responseDescribeEnvironmentHealth = res
    "DescribeEnvironmentHealthResponse"
    "fixture/DescribeEnvironmentHealthResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy DescribeEnvironmentHealth)

responseRebuildEnvironment :: RebuildEnvironmentResponse -> TestTree
responseRebuildEnvironment = res
    "RebuildEnvironmentResponse"
    "fixture/RebuildEnvironmentResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy RebuildEnvironment)

responseDeleteEnvironmentConfiguration :: DeleteEnvironmentConfigurationResponse -> TestTree
responseDeleteEnvironmentConfiguration = res
    "DeleteEnvironmentConfigurationResponse"
    "fixture/DeleteEnvironmentConfigurationResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy DeleteEnvironmentConfiguration)

responseSwapEnvironmentCNAMEs :: SwapEnvironmentCNAMEsResponse -> TestTree
responseSwapEnvironmentCNAMEs = res
    "SwapEnvironmentCNAMEsResponse"
    "fixture/SwapEnvironmentCNAMEsResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy SwapEnvironmentCNAMEs)

responseListAvailableSolutionStacks :: ListAvailableSolutionStacksResponse -> TestTree
responseListAvailableSolutionStacks = res
    "ListAvailableSolutionStacksResponse"
    "fixture/ListAvailableSolutionStacksResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy ListAvailableSolutionStacks)

responseApplyEnvironmentManagedAction :: ApplyEnvironmentManagedActionResponse -> TestTree
responseApplyEnvironmentManagedAction = res
    "ApplyEnvironmentManagedActionResponse"
    "fixture/ApplyEnvironmentManagedActionResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy ApplyEnvironmentManagedAction)

responseDescribeConfigurationOptions :: DescribeConfigurationOptionsResponse -> TestTree
responseDescribeConfigurationOptions = res
    "DescribeConfigurationOptionsResponse"
    "fixture/DescribeConfigurationOptionsResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy DescribeConfigurationOptions)

responseCreateStorageLocation :: CreateStorageLocationResponse -> TestTree
responseCreateStorageLocation = res
    "CreateStorageLocationResponse"
    "fixture/CreateStorageLocationResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy CreateStorageLocation)

responseDescribeEnvironmentManagedActions :: DescribeEnvironmentManagedActionsResponse -> TestTree
responseDescribeEnvironmentManagedActions = res
    "DescribeEnvironmentManagedActionsResponse"
    "fixture/DescribeEnvironmentManagedActionsResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy DescribeEnvironmentManagedActions)

responseDescribeConfigurationSettings :: DescribeConfigurationSettingsResponse -> TestTree
responseDescribeConfigurationSettings = res
    "DescribeConfigurationSettingsResponse"
    "fixture/DescribeConfigurationSettingsResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy DescribeConfigurationSettings)

responseValidateConfigurationSettings :: ValidateConfigurationSettingsResponse -> TestTree
responseValidateConfigurationSettings = res
    "ValidateConfigurationSettingsResponse"
    "fixture/ValidateConfigurationSettingsResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy ValidateConfigurationSettings)

responseRestartAppServer :: RestartAppServerResponse -> TestTree
responseRestartAppServer = res
    "RestartAppServerResponse"
    "fixture/RestartAppServerResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy RestartAppServer)

responseDescribeEnvironments :: EnvironmentDescriptionsMessage -> TestTree
responseDescribeEnvironments = res
    "DescribeEnvironmentsResponse"
    "fixture/DescribeEnvironmentsResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy DescribeEnvironments)

responseCheckDNSAvailability :: CheckDNSAvailabilityResponse -> TestTree
responseCheckDNSAvailability = res
    "CheckDNSAvailabilityResponse"
    "fixture/CheckDNSAvailabilityResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy CheckDNSAvailability)

responseDescribeApplicationVersions :: DescribeApplicationVersionsResponse -> TestTree
responseDescribeApplicationVersions = res
    "DescribeApplicationVersionsResponse"
    "fixture/DescribeApplicationVersionsResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy DescribeApplicationVersions)

responseCreateEnvironment :: EnvironmentDescription -> TestTree
responseCreateEnvironment = res
    "CreateEnvironmentResponse"
    "fixture/CreateEnvironmentResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy CreateEnvironment)
