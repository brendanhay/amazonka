{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ElasticBeanstalk
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--             mkDescribeApplications
--
--         , requestUpdateEnvironment $
--             mkUpdateEnvironment
--
--         , requestTerminateEnvironment $
--             mkTerminateEnvironment
--
--         , requestListPlatformVersions $
--             mkListPlatformVersions
--
--         , requestDeletePlatformVersion $
--             mkDeletePlatformVersion
--
--         , requestCreateApplicationVersion $
--             mkCreateApplicationVersion
--
--         , requestListPlatformBranches $
--             mkListPlatformBranches
--
--         , requestDescribeEvents $
--             mkDescribeEvents
--
--         , requestRequestEnvironmentInfo $
--             mkRequestEnvironmentInfo
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestRetrieveEnvironmentInfo $
--             mkRetrieveEnvironmentInfo
--
--         , requestDescribePlatformVersion $
--             mkDescribePlatformVersion
--
--         , requestDeleteApplication $
--             mkDeleteApplication
--
--         , requestUpdateApplication $
--             mkUpdateApplication
--
--         , requestDescribeInstancesHealth $
--             mkDescribeInstancesHealth
--
--         , requestCreateApplication $
--             mkCreateApplication
--
--         , requestComposeEnvironments $
--             mkComposeEnvironments
--
--         , requestAbortEnvironmentUpdate $
--             mkAbortEnvironmentUpdate
--
--         , requestDeleteConfigurationTemplate $
--             mkDeleteConfigurationTemplate
--
--         , requestUpdateConfigurationTemplate $
--             mkUpdateConfigurationTemplate
--
--         , requestUpdateTagsForResource $
--             mkUpdateTagsForResource
--
--         , requestDescribeEnvironmentResources $
--             mkDescribeEnvironmentResources
--
--         , requestDescribeEnvironmentManagedActionHistory $
--             mkDescribeEnvironmentManagedActionHistory
--
--         , requestDeleteApplicationVersion $
--             mkDeleteApplicationVersion
--
--         , requestUpdateApplicationVersion $
--             mkUpdateApplicationVersion
--
--         , requestCreateConfigurationTemplate $
--             mkCreateConfigurationTemplate
--
--         , requestDescribeEnvironmentHealth $
--             mkDescribeEnvironmentHealth
--
--         , requestRebuildEnvironment $
--             mkRebuildEnvironment
--
--         , requestDeleteEnvironmentConfiguration $
--             mkDeleteEnvironmentConfiguration
--
--         , requestUpdateApplicationResourceLifecycle $
--             mkUpdateApplicationResourceLifecycle
--
--         , requestSwapEnvironmentCNAMEs $
--             mkSwapEnvironmentCNAMEs
--
--         , requestListAvailableSolutionStacks $
--             mkListAvailableSolutionStacks
--
--         , requestApplyEnvironmentManagedAction $
--             mkApplyEnvironmentManagedAction
--
--         , requestDescribeConfigurationOptions $
--             mkDescribeConfigurationOptions
--
--         , requestDisassociateEnvironmentOperationsRole $
--             mkDisassociateEnvironmentOperationsRole
--
--         , requestCreateStorageLocation $
--             mkCreateStorageLocation
--
--         , requestDescribeEnvironmentManagedActions $
--             mkDescribeEnvironmentManagedActions
--
--         , requestDescribeConfigurationSettings $
--             mkDescribeConfigurationSettings
--
--         , requestValidateConfigurationSettings $
--             mkValidateConfigurationSettings
--
--         , requestDescribeAccountAttributes $
--             mkDescribeAccountAttributes
--
--         , requestAssociateEnvironmentOperationsRole $
--             mkAssociateEnvironmentOperationsRole
--
--         , requestRestartAppServer $
--             mkRestartAppServer
--
--         , requestDescribeEnvironments $
--             mkDescribeEnvironments
--
--         , requestCheckDNSAvailability $
--             mkCheckDNSAvailability
--
--         , requestDescribeApplicationVersions $
--             mkDescribeApplicationVersions
--
--         , requestCreateEnvironment $
--             mkCreateEnvironment
--
--         , requestCreatePlatformVersion $
--             mkCreatePlatformVersion
--
--           ]

--     , testGroup "response"
--         [ responseDescribeApplications $
--             mkDescribeApplicationsResponse
--
--         , responseUpdateEnvironment $
--             mkEnvironmentDescription
--
--         , responseTerminateEnvironment $
--             mkEnvironmentDescription
--
--         , responseListPlatformVersions $
--             mkListPlatformVersionsResponse
--
--         , responseDeletePlatformVersion $
--             mkDeletePlatformVersionResponse
--
--         , responseCreateApplicationVersion $
--             mkApplicationVersionDescriptionMessage
--
--         , responseListPlatformBranches $
--             mkListPlatformBranchesResponse
--
--         , responseDescribeEvents $
--             mkDescribeEventsResponse
--
--         , responseRequestEnvironmentInfo $
--             mkRequestEnvironmentInfoResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseRetrieveEnvironmentInfo $
--             mkRetrieveEnvironmentInfoResponse
--
--         , responseDescribePlatformVersion $
--             mkDescribePlatformVersionResponse
--
--         , responseDeleteApplication $
--             mkDeleteApplicationResponse
--
--         , responseUpdateApplication $
--             mkApplicationDescriptionMessage
--
--         , responseDescribeInstancesHealth $
--             mkDescribeInstancesHealthResponse
--
--         , responseCreateApplication $
--             mkApplicationDescriptionMessage
--
--         , responseComposeEnvironments $
--             mkEnvironmentDescriptionsMessage
--
--         , responseAbortEnvironmentUpdate $
--             mkAbortEnvironmentUpdateResponse
--
--         , responseDeleteConfigurationTemplate $
--             mkDeleteConfigurationTemplateResponse
--
--         , responseUpdateConfigurationTemplate $
--             mkConfigurationSettingsDescription
--
--         , responseUpdateTagsForResource $
--             mkUpdateTagsForResourceResponse
--
--         , responseDescribeEnvironmentResources $
--             mkDescribeEnvironmentResourcesResponse
--
--         , responseDescribeEnvironmentManagedActionHistory $
--             mkDescribeEnvironmentManagedActionHistoryResponse
--
--         , responseDeleteApplicationVersion $
--             mkDeleteApplicationVersionResponse
--
--         , responseUpdateApplicationVersion $
--             mkApplicationVersionDescriptionMessage
--
--         , responseCreateConfigurationTemplate $
--             mkConfigurationSettingsDescription
--
--         , responseDescribeEnvironmentHealth $
--             mkDescribeEnvironmentHealthResponse
--
--         , responseRebuildEnvironment $
--             mkRebuildEnvironmentResponse
--
--         , responseDeleteEnvironmentConfiguration $
--             mkDeleteEnvironmentConfigurationResponse
--
--         , responseUpdateApplicationResourceLifecycle $
--             mkUpdateApplicationResourceLifecycleResponse
--
--         , responseSwapEnvironmentCNAMEs $
--             mkSwapEnvironmentCNAMEsResponse
--
--         , responseListAvailableSolutionStacks $
--             mkListAvailableSolutionStacksResponse
--
--         , responseApplyEnvironmentManagedAction $
--             mkApplyEnvironmentManagedActionResponse
--
--         , responseDescribeConfigurationOptions $
--             mkDescribeConfigurationOptionsResponse
--
--         , responseDisassociateEnvironmentOperationsRole $
--             mkDisassociateEnvironmentOperationsRoleResponse
--
--         , responseCreateStorageLocation $
--             mkCreateStorageLocationResponse
--
--         , responseDescribeEnvironmentManagedActions $
--             mkDescribeEnvironmentManagedActionsResponse
--
--         , responseDescribeConfigurationSettings $
--             mkDescribeConfigurationSettingsResponse
--
--         , responseValidateConfigurationSettings $
--             mkValidateConfigurationSettingsResponse
--
--         , responseDescribeAccountAttributes $
--             mkDescribeAccountAttributesResponse
--
--         , responseAssociateEnvironmentOperationsRole $
--             mkAssociateEnvironmentOperationsRoleResponse
--
--         , responseRestartAppServer $
--             mkRestartAppServerResponse
--
--         , responseDescribeEnvironments $
--             mkEnvironmentDescriptionsMessage
--
--         , responseCheckDNSAvailability $
--             mkCheckDNSAvailabilityResponse
--
--         , responseDescribeApplicationVersions $
--             mkDescribeApplicationVersionsResponse
--
--         , responseCreateEnvironment $
--             mkEnvironmentDescription
--
--         , responseCreatePlatformVersion $
--             mkCreatePlatformVersionResponse
--
--           ]
--     ]

-- Requests

requestDescribeApplications :: DescribeApplications -> TestTree
requestDescribeApplications =
  req
    "DescribeApplications"
    "fixture/DescribeApplications.yaml"

requestUpdateEnvironment :: UpdateEnvironment -> TestTree
requestUpdateEnvironment =
  req
    "UpdateEnvironment"
    "fixture/UpdateEnvironment.yaml"

requestTerminateEnvironment :: TerminateEnvironment -> TestTree
requestTerminateEnvironment =
  req
    "TerminateEnvironment"
    "fixture/TerminateEnvironment.yaml"

requestListPlatformVersions :: ListPlatformVersions -> TestTree
requestListPlatformVersions =
  req
    "ListPlatformVersions"
    "fixture/ListPlatformVersions.yaml"

requestDeletePlatformVersion :: DeletePlatformVersion -> TestTree
requestDeletePlatformVersion =
  req
    "DeletePlatformVersion"
    "fixture/DeletePlatformVersion.yaml"

requestCreateApplicationVersion :: CreateApplicationVersion -> TestTree
requestCreateApplicationVersion =
  req
    "CreateApplicationVersion"
    "fixture/CreateApplicationVersion.yaml"

requestListPlatformBranches :: ListPlatformBranches -> TestTree
requestListPlatformBranches =
  req
    "ListPlatformBranches"
    "fixture/ListPlatformBranches.yaml"

requestDescribeEvents :: DescribeEvents -> TestTree
requestDescribeEvents =
  req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

requestRequestEnvironmentInfo :: RequestEnvironmentInfo -> TestTree
requestRequestEnvironmentInfo =
  req
    "RequestEnvironmentInfo"
    "fixture/RequestEnvironmentInfo.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestRetrieveEnvironmentInfo :: RetrieveEnvironmentInfo -> TestTree
requestRetrieveEnvironmentInfo =
  req
    "RetrieveEnvironmentInfo"
    "fixture/RetrieveEnvironmentInfo.yaml"

requestDescribePlatformVersion :: DescribePlatformVersion -> TestTree
requestDescribePlatformVersion =
  req
    "DescribePlatformVersion"
    "fixture/DescribePlatformVersion.yaml"

requestDeleteApplication :: DeleteApplication -> TestTree
requestDeleteApplication =
  req
    "DeleteApplication"
    "fixture/DeleteApplication.yaml"

requestUpdateApplication :: UpdateApplication -> TestTree
requestUpdateApplication =
  req
    "UpdateApplication"
    "fixture/UpdateApplication.yaml"

requestDescribeInstancesHealth :: DescribeInstancesHealth -> TestTree
requestDescribeInstancesHealth =
  req
    "DescribeInstancesHealth"
    "fixture/DescribeInstancesHealth.yaml"

requestCreateApplication :: CreateApplication -> TestTree
requestCreateApplication =
  req
    "CreateApplication"
    "fixture/CreateApplication.yaml"

requestComposeEnvironments :: ComposeEnvironments -> TestTree
requestComposeEnvironments =
  req
    "ComposeEnvironments"
    "fixture/ComposeEnvironments.yaml"

requestAbortEnvironmentUpdate :: AbortEnvironmentUpdate -> TestTree
requestAbortEnvironmentUpdate =
  req
    "AbortEnvironmentUpdate"
    "fixture/AbortEnvironmentUpdate.yaml"

requestDeleteConfigurationTemplate :: DeleteConfigurationTemplate -> TestTree
requestDeleteConfigurationTemplate =
  req
    "DeleteConfigurationTemplate"
    "fixture/DeleteConfigurationTemplate.yaml"

requestUpdateConfigurationTemplate :: UpdateConfigurationTemplate -> TestTree
requestUpdateConfigurationTemplate =
  req
    "UpdateConfigurationTemplate"
    "fixture/UpdateConfigurationTemplate.yaml"

requestUpdateTagsForResource :: UpdateTagsForResource -> TestTree
requestUpdateTagsForResource =
  req
    "UpdateTagsForResource"
    "fixture/UpdateTagsForResource.yaml"

requestDescribeEnvironmentResources :: DescribeEnvironmentResources -> TestTree
requestDescribeEnvironmentResources =
  req
    "DescribeEnvironmentResources"
    "fixture/DescribeEnvironmentResources.yaml"

requestDescribeEnvironmentManagedActionHistory :: DescribeEnvironmentManagedActionHistory -> TestTree
requestDescribeEnvironmentManagedActionHistory =
  req
    "DescribeEnvironmentManagedActionHistory"
    "fixture/DescribeEnvironmentManagedActionHistory.yaml"

requestDeleteApplicationVersion :: DeleteApplicationVersion -> TestTree
requestDeleteApplicationVersion =
  req
    "DeleteApplicationVersion"
    "fixture/DeleteApplicationVersion.yaml"

requestUpdateApplicationVersion :: UpdateApplicationVersion -> TestTree
requestUpdateApplicationVersion =
  req
    "UpdateApplicationVersion"
    "fixture/UpdateApplicationVersion.yaml"

requestCreateConfigurationTemplate :: CreateConfigurationTemplate -> TestTree
requestCreateConfigurationTemplate =
  req
    "CreateConfigurationTemplate"
    "fixture/CreateConfigurationTemplate.yaml"

requestDescribeEnvironmentHealth :: DescribeEnvironmentHealth -> TestTree
requestDescribeEnvironmentHealth =
  req
    "DescribeEnvironmentHealth"
    "fixture/DescribeEnvironmentHealth.yaml"

requestRebuildEnvironment :: RebuildEnvironment -> TestTree
requestRebuildEnvironment =
  req
    "RebuildEnvironment"
    "fixture/RebuildEnvironment.yaml"

requestDeleteEnvironmentConfiguration :: DeleteEnvironmentConfiguration -> TestTree
requestDeleteEnvironmentConfiguration =
  req
    "DeleteEnvironmentConfiguration"
    "fixture/DeleteEnvironmentConfiguration.yaml"

requestUpdateApplicationResourceLifecycle :: UpdateApplicationResourceLifecycle -> TestTree
requestUpdateApplicationResourceLifecycle =
  req
    "UpdateApplicationResourceLifecycle"
    "fixture/UpdateApplicationResourceLifecycle.yaml"

requestSwapEnvironmentCNAMEs :: SwapEnvironmentCNAMEs -> TestTree
requestSwapEnvironmentCNAMEs =
  req
    "SwapEnvironmentCNAMEs"
    "fixture/SwapEnvironmentCNAMEs.yaml"

requestListAvailableSolutionStacks :: ListAvailableSolutionStacks -> TestTree
requestListAvailableSolutionStacks =
  req
    "ListAvailableSolutionStacks"
    "fixture/ListAvailableSolutionStacks.yaml"

requestApplyEnvironmentManagedAction :: ApplyEnvironmentManagedAction -> TestTree
requestApplyEnvironmentManagedAction =
  req
    "ApplyEnvironmentManagedAction"
    "fixture/ApplyEnvironmentManagedAction.yaml"

requestDescribeConfigurationOptions :: DescribeConfigurationOptions -> TestTree
requestDescribeConfigurationOptions =
  req
    "DescribeConfigurationOptions"
    "fixture/DescribeConfigurationOptions.yaml"

requestDisassociateEnvironmentOperationsRole :: DisassociateEnvironmentOperationsRole -> TestTree
requestDisassociateEnvironmentOperationsRole =
  req
    "DisassociateEnvironmentOperationsRole"
    "fixture/DisassociateEnvironmentOperationsRole.yaml"

requestCreateStorageLocation :: CreateStorageLocation -> TestTree
requestCreateStorageLocation =
  req
    "CreateStorageLocation"
    "fixture/CreateStorageLocation.yaml"

requestDescribeEnvironmentManagedActions :: DescribeEnvironmentManagedActions -> TestTree
requestDescribeEnvironmentManagedActions =
  req
    "DescribeEnvironmentManagedActions"
    "fixture/DescribeEnvironmentManagedActions.yaml"

requestDescribeConfigurationSettings :: DescribeConfigurationSettings -> TestTree
requestDescribeConfigurationSettings =
  req
    "DescribeConfigurationSettings"
    "fixture/DescribeConfigurationSettings.yaml"

requestValidateConfigurationSettings :: ValidateConfigurationSettings -> TestTree
requestValidateConfigurationSettings =
  req
    "ValidateConfigurationSettings"
    "fixture/ValidateConfigurationSettings.yaml"

requestDescribeAccountAttributes :: DescribeAccountAttributes -> TestTree
requestDescribeAccountAttributes =
  req
    "DescribeAccountAttributes"
    "fixture/DescribeAccountAttributes.yaml"

requestAssociateEnvironmentOperationsRole :: AssociateEnvironmentOperationsRole -> TestTree
requestAssociateEnvironmentOperationsRole =
  req
    "AssociateEnvironmentOperationsRole"
    "fixture/AssociateEnvironmentOperationsRole.yaml"

requestRestartAppServer :: RestartAppServer -> TestTree
requestRestartAppServer =
  req
    "RestartAppServer"
    "fixture/RestartAppServer.yaml"

requestDescribeEnvironments :: DescribeEnvironments -> TestTree
requestDescribeEnvironments =
  req
    "DescribeEnvironments"
    "fixture/DescribeEnvironments.yaml"

requestCheckDNSAvailability :: CheckDNSAvailability -> TestTree
requestCheckDNSAvailability =
  req
    "CheckDNSAvailability"
    "fixture/CheckDNSAvailability.yaml"

requestDescribeApplicationVersions :: DescribeApplicationVersions -> TestTree
requestDescribeApplicationVersions =
  req
    "DescribeApplicationVersions"
    "fixture/DescribeApplicationVersions.yaml"

requestCreateEnvironment :: CreateEnvironment -> TestTree
requestCreateEnvironment =
  req
    "CreateEnvironment"
    "fixture/CreateEnvironment.yaml"

requestCreatePlatformVersion :: CreatePlatformVersion -> TestTree
requestCreatePlatformVersion =
  req
    "CreatePlatformVersion"
    "fixture/CreatePlatformVersion.yaml"

-- Responses

responseDescribeApplications :: DescribeApplicationsResponse -> TestTree
responseDescribeApplications =
  res
    "DescribeApplicationsResponse"
    "fixture/DescribeApplicationsResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy DescribeApplications)

responseUpdateEnvironment :: EnvironmentDescription -> TestTree
responseUpdateEnvironment =
  res
    "UpdateEnvironmentResponse"
    "fixture/UpdateEnvironmentResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy UpdateEnvironment)

responseTerminateEnvironment :: EnvironmentDescription -> TestTree
responseTerminateEnvironment =
  res
    "TerminateEnvironmentResponse"
    "fixture/TerminateEnvironmentResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy TerminateEnvironment)

responseListPlatformVersions :: ListPlatformVersionsResponse -> TestTree
responseListPlatformVersions =
  res
    "ListPlatformVersionsResponse"
    "fixture/ListPlatformVersionsResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy ListPlatformVersions)

responseDeletePlatformVersion :: DeletePlatformVersionResponse -> TestTree
responseDeletePlatformVersion =
  res
    "DeletePlatformVersionResponse"
    "fixture/DeletePlatformVersionResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy DeletePlatformVersion)

responseCreateApplicationVersion :: ApplicationVersionDescriptionMessage -> TestTree
responseCreateApplicationVersion =
  res
    "CreateApplicationVersionResponse"
    "fixture/CreateApplicationVersionResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy CreateApplicationVersion)

responseListPlatformBranches :: ListPlatformBranchesResponse -> TestTree
responseListPlatformBranches =
  res
    "ListPlatformBranchesResponse"
    "fixture/ListPlatformBranchesResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy ListPlatformBranches)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents =
  res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy DescribeEvents)

responseRequestEnvironmentInfo :: RequestEnvironmentInfoResponse -> TestTree
responseRequestEnvironmentInfo =
  res
    "RequestEnvironmentInfoResponse"
    "fixture/RequestEnvironmentInfoResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy RequestEnvironmentInfo)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy ListTagsForResource)

responseRetrieveEnvironmentInfo :: RetrieveEnvironmentInfoResponse -> TestTree
responseRetrieveEnvironmentInfo =
  res
    "RetrieveEnvironmentInfoResponse"
    "fixture/RetrieveEnvironmentInfoResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy RetrieveEnvironmentInfo)

responseDescribePlatformVersion :: DescribePlatformVersionResponse -> TestTree
responseDescribePlatformVersion =
  res
    "DescribePlatformVersionResponse"
    "fixture/DescribePlatformVersionResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy DescribePlatformVersion)

responseDeleteApplication :: DeleteApplicationResponse -> TestTree
responseDeleteApplication =
  res
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy DeleteApplication)

responseUpdateApplication :: ApplicationDescriptionMessage -> TestTree
responseUpdateApplication =
  res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy UpdateApplication)

responseDescribeInstancesHealth :: DescribeInstancesHealthResponse -> TestTree
responseDescribeInstancesHealth =
  res
    "DescribeInstancesHealthResponse"
    "fixture/DescribeInstancesHealthResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy DescribeInstancesHealth)

responseCreateApplication :: ApplicationDescriptionMessage -> TestTree
responseCreateApplication =
  res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy CreateApplication)

responseComposeEnvironments :: EnvironmentDescriptionsMessage -> TestTree
responseComposeEnvironments =
  res
    "ComposeEnvironmentsResponse"
    "fixture/ComposeEnvironmentsResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy ComposeEnvironments)

responseAbortEnvironmentUpdate :: AbortEnvironmentUpdateResponse -> TestTree
responseAbortEnvironmentUpdate =
  res
    "AbortEnvironmentUpdateResponse"
    "fixture/AbortEnvironmentUpdateResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy AbortEnvironmentUpdate)

responseDeleteConfigurationTemplate :: DeleteConfigurationTemplateResponse -> TestTree
responseDeleteConfigurationTemplate =
  res
    "DeleteConfigurationTemplateResponse"
    "fixture/DeleteConfigurationTemplateResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy DeleteConfigurationTemplate)

responseUpdateConfigurationTemplate :: ConfigurationSettingsDescription -> TestTree
responseUpdateConfigurationTemplate =
  res
    "UpdateConfigurationTemplateResponse"
    "fixture/UpdateConfigurationTemplateResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy UpdateConfigurationTemplate)

responseUpdateTagsForResource :: UpdateTagsForResourceResponse -> TestTree
responseUpdateTagsForResource =
  res
    "UpdateTagsForResourceResponse"
    "fixture/UpdateTagsForResourceResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy UpdateTagsForResource)

responseDescribeEnvironmentResources :: DescribeEnvironmentResourcesResponse -> TestTree
responseDescribeEnvironmentResources =
  res
    "DescribeEnvironmentResourcesResponse"
    "fixture/DescribeEnvironmentResourcesResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy DescribeEnvironmentResources)

responseDescribeEnvironmentManagedActionHistory :: DescribeEnvironmentManagedActionHistoryResponse -> TestTree
responseDescribeEnvironmentManagedActionHistory =
  res
    "DescribeEnvironmentManagedActionHistoryResponse"
    "fixture/DescribeEnvironmentManagedActionHistoryResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy DescribeEnvironmentManagedActionHistory)

responseDeleteApplicationVersion :: DeleteApplicationVersionResponse -> TestTree
responseDeleteApplicationVersion =
  res
    "DeleteApplicationVersionResponse"
    "fixture/DeleteApplicationVersionResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy DeleteApplicationVersion)

responseUpdateApplicationVersion :: ApplicationVersionDescriptionMessage -> TestTree
responseUpdateApplicationVersion =
  res
    "UpdateApplicationVersionResponse"
    "fixture/UpdateApplicationVersionResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy UpdateApplicationVersion)

responseCreateConfigurationTemplate :: ConfigurationSettingsDescription -> TestTree
responseCreateConfigurationTemplate =
  res
    "CreateConfigurationTemplateResponse"
    "fixture/CreateConfigurationTemplateResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy CreateConfigurationTemplate)

responseDescribeEnvironmentHealth :: DescribeEnvironmentHealthResponse -> TestTree
responseDescribeEnvironmentHealth =
  res
    "DescribeEnvironmentHealthResponse"
    "fixture/DescribeEnvironmentHealthResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy DescribeEnvironmentHealth)

responseRebuildEnvironment :: RebuildEnvironmentResponse -> TestTree
responseRebuildEnvironment =
  res
    "RebuildEnvironmentResponse"
    "fixture/RebuildEnvironmentResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy RebuildEnvironment)

responseDeleteEnvironmentConfiguration :: DeleteEnvironmentConfigurationResponse -> TestTree
responseDeleteEnvironmentConfiguration =
  res
    "DeleteEnvironmentConfigurationResponse"
    "fixture/DeleteEnvironmentConfigurationResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy DeleteEnvironmentConfiguration)

responseUpdateApplicationResourceLifecycle :: UpdateApplicationResourceLifecycleResponse -> TestTree
responseUpdateApplicationResourceLifecycle =
  res
    "UpdateApplicationResourceLifecycleResponse"
    "fixture/UpdateApplicationResourceLifecycleResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy UpdateApplicationResourceLifecycle)

responseSwapEnvironmentCNAMEs :: SwapEnvironmentCNAMEsResponse -> TestTree
responseSwapEnvironmentCNAMEs =
  res
    "SwapEnvironmentCNAMEsResponse"
    "fixture/SwapEnvironmentCNAMEsResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy SwapEnvironmentCNAMEs)

responseListAvailableSolutionStacks :: ListAvailableSolutionStacksResponse -> TestTree
responseListAvailableSolutionStacks =
  res
    "ListAvailableSolutionStacksResponse"
    "fixture/ListAvailableSolutionStacksResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy ListAvailableSolutionStacks)

responseApplyEnvironmentManagedAction :: ApplyEnvironmentManagedActionResponse -> TestTree
responseApplyEnvironmentManagedAction =
  res
    "ApplyEnvironmentManagedActionResponse"
    "fixture/ApplyEnvironmentManagedActionResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy ApplyEnvironmentManagedAction)

responseDescribeConfigurationOptions :: DescribeConfigurationOptionsResponse -> TestTree
responseDescribeConfigurationOptions =
  res
    "DescribeConfigurationOptionsResponse"
    "fixture/DescribeConfigurationOptionsResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy DescribeConfigurationOptions)

responseDisassociateEnvironmentOperationsRole :: DisassociateEnvironmentOperationsRoleResponse -> TestTree
responseDisassociateEnvironmentOperationsRole =
  res
    "DisassociateEnvironmentOperationsRoleResponse"
    "fixture/DisassociateEnvironmentOperationsRoleResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy DisassociateEnvironmentOperationsRole)

responseCreateStorageLocation :: CreateStorageLocationResponse -> TestTree
responseCreateStorageLocation =
  res
    "CreateStorageLocationResponse"
    "fixture/CreateStorageLocationResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy CreateStorageLocation)

responseDescribeEnvironmentManagedActions :: DescribeEnvironmentManagedActionsResponse -> TestTree
responseDescribeEnvironmentManagedActions =
  res
    "DescribeEnvironmentManagedActionsResponse"
    "fixture/DescribeEnvironmentManagedActionsResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy DescribeEnvironmentManagedActions)

responseDescribeConfigurationSettings :: DescribeConfigurationSettingsResponse -> TestTree
responseDescribeConfigurationSettings =
  res
    "DescribeConfigurationSettingsResponse"
    "fixture/DescribeConfigurationSettingsResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy DescribeConfigurationSettings)

responseValidateConfigurationSettings :: ValidateConfigurationSettingsResponse -> TestTree
responseValidateConfigurationSettings =
  res
    "ValidateConfigurationSettingsResponse"
    "fixture/ValidateConfigurationSettingsResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy ValidateConfigurationSettings)

responseDescribeAccountAttributes :: DescribeAccountAttributesResponse -> TestTree
responseDescribeAccountAttributes =
  res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy DescribeAccountAttributes)

responseAssociateEnvironmentOperationsRole :: AssociateEnvironmentOperationsRoleResponse -> TestTree
responseAssociateEnvironmentOperationsRole =
  res
    "AssociateEnvironmentOperationsRoleResponse"
    "fixture/AssociateEnvironmentOperationsRoleResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy AssociateEnvironmentOperationsRole)

responseRestartAppServer :: RestartAppServerResponse -> TestTree
responseRestartAppServer =
  res
    "RestartAppServerResponse"
    "fixture/RestartAppServerResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy RestartAppServer)

responseDescribeEnvironments :: EnvironmentDescriptionsMessage -> TestTree
responseDescribeEnvironments =
  res
    "DescribeEnvironmentsResponse"
    "fixture/DescribeEnvironmentsResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy DescribeEnvironments)

responseCheckDNSAvailability :: CheckDNSAvailabilityResponse -> TestTree
responseCheckDNSAvailability =
  res
    "CheckDNSAvailabilityResponse"
    "fixture/CheckDNSAvailabilityResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy CheckDNSAvailability)

responseDescribeApplicationVersions :: DescribeApplicationVersionsResponse -> TestTree
responseDescribeApplicationVersions =
  res
    "DescribeApplicationVersionsResponse"
    "fixture/DescribeApplicationVersionsResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy DescribeApplicationVersions)

responseCreateEnvironment :: EnvironmentDescription -> TestTree
responseCreateEnvironment =
  res
    "CreateEnvironmentResponse"
    "fixture/CreateEnvironmentResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy CreateEnvironment)

responseCreatePlatformVersion :: CreatePlatformVersionResponse -> TestTree
responseCreatePlatformVersion =
  res
    "CreatePlatformVersionResponse"
    "fixture/CreatePlatformVersionResponse.proto"
    elasticBeanstalkService
    (Proxy :: Proxy CreatePlatformVersion)
