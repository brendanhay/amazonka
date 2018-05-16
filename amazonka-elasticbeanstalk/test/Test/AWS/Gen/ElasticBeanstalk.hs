{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ElasticBeanstalk
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.ElasticBeanstalk where

import Data.Proxy
import Network.AWS.ElasticBeanstalk
import Test.AWS.ElasticBeanstalk.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

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
--         , requestListPlatformVersions $
--             listPlatformVersions
--
--         , requestDeletePlatformVersion $
--             deletePlatformVersion
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
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestRetrieveEnvironmentInfo $
--             retrieveEnvironmentInfo
--
--         , requestDescribePlatformVersion $
--             describePlatformVersion
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
--         , requestUpdateTagsForResource $
--             updateTagsForResource
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
--         , requestUpdateApplicationResourceLifecycle $
--             updateApplicationResourceLifecycle
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
--         , requestDescribeAccountAttributes $
--             describeAccountAttributes
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
--         , requestCreatePlatformVersion $
--             createPlatformVersion
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
--         , responseListPlatformVersions $
--             listPlatformVersionsResponse
--
--         , responseDeletePlatformVersion $
--             deletePlatformVersionResponse
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
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseRetrieveEnvironmentInfo $
--             retrieveEnvironmentInfoResponse
--
--         , responseDescribePlatformVersion $
--             describePlatformVersionResponse
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
--         , responseUpdateTagsForResource $
--             updateTagsForResourceResponse
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
--         , responseUpdateApplicationResourceLifecycle $
--             updateApplicationResourceLifecycleResponse
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
--         , responseDescribeAccountAttributes $
--             describeAccountAttributesResponse
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
--         , responseCreatePlatformVersion $
--             createPlatformVersionResponse
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

requestListPlatformVersions :: ListPlatformVersions -> TestTree
requestListPlatformVersions = req
    "ListPlatformVersions"
    "fixture/ListPlatformVersions.yaml"

requestDeletePlatformVersion :: DeletePlatformVersion -> TestTree
requestDeletePlatformVersion = req
    "DeletePlatformVersion"
    "fixture/DeletePlatformVersion.yaml"

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

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestRetrieveEnvironmentInfo :: RetrieveEnvironmentInfo -> TestTree
requestRetrieveEnvironmentInfo = req
    "RetrieveEnvironmentInfo"
    "fixture/RetrieveEnvironmentInfo.yaml"

requestDescribePlatformVersion :: DescribePlatformVersion -> TestTree
requestDescribePlatformVersion = req
    "DescribePlatformVersion"
    "fixture/DescribePlatformVersion.yaml"

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

requestUpdateTagsForResource :: UpdateTagsForResource -> TestTree
requestUpdateTagsForResource = req
    "UpdateTagsForResource"
    "fixture/UpdateTagsForResource.yaml"

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

requestUpdateApplicationResourceLifecycle :: UpdateApplicationResourceLifecycle -> TestTree
requestUpdateApplicationResourceLifecycle = req
    "UpdateApplicationResourceLifecycle"
    "fixture/UpdateApplicationResourceLifecycle.yaml"

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

requestDescribeAccountAttributes :: DescribeAccountAttributes -> TestTree
requestDescribeAccountAttributes = req
    "DescribeAccountAttributes"
    "fixture/DescribeAccountAttributes.yaml"

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

requestCreatePlatformVersion :: CreatePlatformVersion -> TestTree
requestCreatePlatformVersion = req
    "CreatePlatformVersion"
    "fixture/CreatePlatformVersion.yaml"

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

responseListPlatformVersions :: ListPlatformVersionsResponse -> TestTree
responseListPlatformVersions = res
    "ListPlatformVersionsResponse"
    "fixture/ListPlatformVersionsResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy ListPlatformVersions)

responseDeletePlatformVersion :: DeletePlatformVersionResponse -> TestTree
responseDeletePlatformVersion = res
    "DeletePlatformVersionResponse"
    "fixture/DeletePlatformVersionResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy DeletePlatformVersion)

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

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy ListTagsForResource)

responseRetrieveEnvironmentInfo :: RetrieveEnvironmentInfoResponse -> TestTree
responseRetrieveEnvironmentInfo = res
    "RetrieveEnvironmentInfoResponse"
    "fixture/RetrieveEnvironmentInfoResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy RetrieveEnvironmentInfo)

responseDescribePlatformVersion :: DescribePlatformVersionResponse -> TestTree
responseDescribePlatformVersion = res
    "DescribePlatformVersionResponse"
    "fixture/DescribePlatformVersionResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy DescribePlatformVersion)

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

responseUpdateTagsForResource :: UpdateTagsForResourceResponse -> TestTree
responseUpdateTagsForResource = res
    "UpdateTagsForResourceResponse"
    "fixture/UpdateTagsForResourceResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy UpdateTagsForResource)

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

responseUpdateApplicationResourceLifecycle :: UpdateApplicationResourceLifecycleResponse -> TestTree
responseUpdateApplicationResourceLifecycle = res
    "UpdateApplicationResourceLifecycleResponse"
    "fixture/UpdateApplicationResourceLifecycleResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy UpdateApplicationResourceLifecycle)

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

responseDescribeAccountAttributes :: DescribeAccountAttributesResponse -> TestTree
responseDescribeAccountAttributes = res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy DescribeAccountAttributes)

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

responseCreatePlatformVersion :: CreatePlatformVersionResponse -> TestTree
responseCreatePlatformVersion = res
    "CreatePlatformVersionResponse"
    "fixture/CreatePlatformVersionResponse.proto"
    elasticBeanstalk
    (Proxy :: Proxy CreatePlatformVersion)
