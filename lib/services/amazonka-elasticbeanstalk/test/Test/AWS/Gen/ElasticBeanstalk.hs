{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ElasticBeanstalk
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--             newDescribeApplications
--
--         , requestUpdateEnvironment $
--             newUpdateEnvironment
--
--         , requestTerminateEnvironment $
--             newTerminateEnvironment
--
--         , requestListPlatformVersions $
--             newListPlatformVersions
--
--         , requestDeletePlatformVersion $
--             newDeletePlatformVersion
--
--         , requestCreateApplicationVersion $
--             newCreateApplicationVersion
--
--         , requestListPlatformBranches $
--             newListPlatformBranches
--
--         , requestDescribeEvents $
--             newDescribeEvents
--
--         , requestRequestEnvironmentInfo $
--             newRequestEnvironmentInfo
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestRetrieveEnvironmentInfo $
--             newRetrieveEnvironmentInfo
--
--         , requestDescribePlatformVersion $
--             newDescribePlatformVersion
--
--         , requestDeleteApplication $
--             newDeleteApplication
--
--         , requestUpdateApplication $
--             newUpdateApplication
--
--         , requestDescribeInstancesHealth $
--             newDescribeInstancesHealth
--
--         , requestCreateApplication $
--             newCreateApplication
--
--         , requestComposeEnvironments $
--             newComposeEnvironments
--
--         , requestAbortEnvironmentUpdate $
--             newAbortEnvironmentUpdate
--
--         , requestDeleteConfigurationTemplate $
--             newDeleteConfigurationTemplate
--
--         , requestUpdateConfigurationTemplate $
--             newUpdateConfigurationTemplate
--
--         , requestUpdateTagsForResource $
--             newUpdateTagsForResource
--
--         , requestDescribeEnvironmentResources $
--             newDescribeEnvironmentResources
--
--         , requestDescribeEnvironmentManagedActionHistory $
--             newDescribeEnvironmentManagedActionHistory
--
--         , requestDeleteApplicationVersion $
--             newDeleteApplicationVersion
--
--         , requestUpdateApplicationVersion $
--             newUpdateApplicationVersion
--
--         , requestCreateConfigurationTemplate $
--             newCreateConfigurationTemplate
--
--         , requestDescribeEnvironmentHealth $
--             newDescribeEnvironmentHealth
--
--         , requestRebuildEnvironment $
--             newRebuildEnvironment
--
--         , requestDeleteEnvironmentConfiguration $
--             newDeleteEnvironmentConfiguration
--
--         , requestUpdateApplicationResourceLifecycle $
--             newUpdateApplicationResourceLifecycle
--
--         , requestSwapEnvironmentCNAMEs $
--             newSwapEnvironmentCNAMEs
--
--         , requestListAvailableSolutionStacks $
--             newListAvailableSolutionStacks
--
--         , requestApplyEnvironmentManagedAction $
--             newApplyEnvironmentManagedAction
--
--         , requestDescribeConfigurationOptions $
--             newDescribeConfigurationOptions
--
--         , requestDisassociateEnvironmentOperationsRole $
--             newDisassociateEnvironmentOperationsRole
--
--         , requestCreateStorageLocation $
--             newCreateStorageLocation
--
--         , requestDescribeEnvironmentManagedActions $
--             newDescribeEnvironmentManagedActions
--
--         , requestDescribeConfigurationSettings $
--             newDescribeConfigurationSettings
--
--         , requestValidateConfigurationSettings $
--             newValidateConfigurationSettings
--
--         , requestDescribeAccountAttributes $
--             newDescribeAccountAttributes
--
--         , requestAssociateEnvironmentOperationsRole $
--             newAssociateEnvironmentOperationsRole
--
--         , requestRestartAppServer $
--             newRestartAppServer
--
--         , requestDescribeEnvironments $
--             newDescribeEnvironments
--
--         , requestCheckDNSAvailability $
--             newCheckDNSAvailability
--
--         , requestDescribeApplicationVersions $
--             newDescribeApplicationVersions
--
--         , requestCreateEnvironment $
--             newCreateEnvironment
--
--         , requestCreatePlatformVersion $
--             newCreatePlatformVersion
--
--           ]

--     , testGroup "response"
--         [ responseDescribeApplications $
--             newDescribeApplicationsResponse
--
--         , responseUpdateEnvironment $
--             newEnvironmentDescription
--
--         , responseTerminateEnvironment $
--             newEnvironmentDescription
--
--         , responseListPlatformVersions $
--             newListPlatformVersionsResponse
--
--         , responseDeletePlatformVersion $
--             newDeletePlatformVersionResponse
--
--         , responseCreateApplicationVersion $
--             newApplicationVersionDescriptionMessage
--
--         , responseListPlatformBranches $
--             newListPlatformBranchesResponse
--
--         , responseDescribeEvents $
--             newDescribeEventsResponse
--
--         , responseRequestEnvironmentInfo $
--             newRequestEnvironmentInfoResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseRetrieveEnvironmentInfo $
--             newRetrieveEnvironmentInfoResponse
--
--         , responseDescribePlatformVersion $
--             newDescribePlatformVersionResponse
--
--         , responseDeleteApplication $
--             newDeleteApplicationResponse
--
--         , responseUpdateApplication $
--             newApplicationDescriptionMessage
--
--         , responseDescribeInstancesHealth $
--             newDescribeInstancesHealthResponse
--
--         , responseCreateApplication $
--             newApplicationDescriptionMessage
--
--         , responseComposeEnvironments $
--             newEnvironmentDescriptionsMessage
--
--         , responseAbortEnvironmentUpdate $
--             newAbortEnvironmentUpdateResponse
--
--         , responseDeleteConfigurationTemplate $
--             newDeleteConfigurationTemplateResponse
--
--         , responseUpdateConfigurationTemplate $
--             newConfigurationSettingsDescription
--
--         , responseUpdateTagsForResource $
--             newUpdateTagsForResourceResponse
--
--         , responseDescribeEnvironmentResources $
--             newDescribeEnvironmentResourcesResponse
--
--         , responseDescribeEnvironmentManagedActionHistory $
--             newDescribeEnvironmentManagedActionHistoryResponse
--
--         , responseDeleteApplicationVersion $
--             newDeleteApplicationVersionResponse
--
--         , responseUpdateApplicationVersion $
--             newApplicationVersionDescriptionMessage
--
--         , responseCreateConfigurationTemplate $
--             newConfigurationSettingsDescription
--
--         , responseDescribeEnvironmentHealth $
--             newDescribeEnvironmentHealthResponse
--
--         , responseRebuildEnvironment $
--             newRebuildEnvironmentResponse
--
--         , responseDeleteEnvironmentConfiguration $
--             newDeleteEnvironmentConfigurationResponse
--
--         , responseUpdateApplicationResourceLifecycle $
--             newUpdateApplicationResourceLifecycleResponse
--
--         , responseSwapEnvironmentCNAMEs $
--             newSwapEnvironmentCNAMEsResponse
--
--         , responseListAvailableSolutionStacks $
--             newListAvailableSolutionStacksResponse
--
--         , responseApplyEnvironmentManagedAction $
--             newApplyEnvironmentManagedActionResponse
--
--         , responseDescribeConfigurationOptions $
--             newDescribeConfigurationOptionsResponse
--
--         , responseDisassociateEnvironmentOperationsRole $
--             newDisassociateEnvironmentOperationsRoleResponse
--
--         , responseCreateStorageLocation $
--             newCreateStorageLocationResponse
--
--         , responseDescribeEnvironmentManagedActions $
--             newDescribeEnvironmentManagedActionsResponse
--
--         , responseDescribeConfigurationSettings $
--             newDescribeConfigurationSettingsResponse
--
--         , responseValidateConfigurationSettings $
--             newValidateConfigurationSettingsResponse
--
--         , responseDescribeAccountAttributes $
--             newDescribeAccountAttributesResponse
--
--         , responseAssociateEnvironmentOperationsRole $
--             newAssociateEnvironmentOperationsRoleResponse
--
--         , responseRestartAppServer $
--             newRestartAppServerResponse
--
--         , responseDescribeEnvironments $
--             newEnvironmentDescriptionsMessage
--
--         , responseCheckDNSAvailability $
--             newCheckDNSAvailabilityResponse
--
--         , responseDescribeApplicationVersions $
--             newDescribeApplicationVersionsResponse
--
--         , responseCreateEnvironment $
--             newEnvironmentDescription
--
--         , responseCreatePlatformVersion $
--             newCreatePlatformVersionResponse
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
    defaultService
    (Proxy :: Proxy DescribeApplications)

responseUpdateEnvironment :: EnvironmentDescription -> TestTree
responseUpdateEnvironment =
  res
    "UpdateEnvironmentResponse"
    "fixture/UpdateEnvironmentResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateEnvironment)

responseTerminateEnvironment :: EnvironmentDescription -> TestTree
responseTerminateEnvironment =
  res
    "TerminateEnvironmentResponse"
    "fixture/TerminateEnvironmentResponse.proto"
    defaultService
    (Proxy :: Proxy TerminateEnvironment)

responseListPlatformVersions :: ListPlatformVersionsResponse -> TestTree
responseListPlatformVersions =
  res
    "ListPlatformVersionsResponse"
    "fixture/ListPlatformVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPlatformVersions)

responseDeletePlatformVersion :: DeletePlatformVersionResponse -> TestTree
responseDeletePlatformVersion =
  res
    "DeletePlatformVersionResponse"
    "fixture/DeletePlatformVersionResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePlatformVersion)

responseCreateApplicationVersion :: ApplicationVersionDescriptionMessage -> TestTree
responseCreateApplicationVersion =
  res
    "CreateApplicationVersionResponse"
    "fixture/CreateApplicationVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateApplicationVersion)

responseListPlatformBranches :: ListPlatformBranchesResponse -> TestTree
responseListPlatformBranches =
  res
    "ListPlatformBranchesResponse"
    "fixture/ListPlatformBranchesResponse.proto"
    defaultService
    (Proxy :: Proxy ListPlatformBranches)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents =
  res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEvents)

responseRequestEnvironmentInfo :: RequestEnvironmentInfoResponse -> TestTree
responseRequestEnvironmentInfo =
  res
    "RequestEnvironmentInfoResponse"
    "fixture/RequestEnvironmentInfoResponse.proto"
    defaultService
    (Proxy :: Proxy RequestEnvironmentInfo)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseRetrieveEnvironmentInfo :: RetrieveEnvironmentInfoResponse -> TestTree
responseRetrieveEnvironmentInfo =
  res
    "RetrieveEnvironmentInfoResponse"
    "fixture/RetrieveEnvironmentInfoResponse.proto"
    defaultService
    (Proxy :: Proxy RetrieveEnvironmentInfo)

responseDescribePlatformVersion :: DescribePlatformVersionResponse -> TestTree
responseDescribePlatformVersion =
  res
    "DescribePlatformVersionResponse"
    "fixture/DescribePlatformVersionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePlatformVersion)

responseDeleteApplication :: DeleteApplicationResponse -> TestTree
responseDeleteApplication =
  res
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApplication)

responseUpdateApplication :: ApplicationDescriptionMessage -> TestTree
responseUpdateApplication =
  res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApplication)

responseDescribeInstancesHealth :: DescribeInstancesHealthResponse -> TestTree
responseDescribeInstancesHealth =
  res
    "DescribeInstancesHealthResponse"
    "fixture/DescribeInstancesHealthResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstancesHealth)

responseCreateApplication :: ApplicationDescriptionMessage -> TestTree
responseCreateApplication =
  res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateApplication)

responseComposeEnvironments :: EnvironmentDescriptionsMessage -> TestTree
responseComposeEnvironments =
  res
    "ComposeEnvironmentsResponse"
    "fixture/ComposeEnvironmentsResponse.proto"
    defaultService
    (Proxy :: Proxy ComposeEnvironments)

responseAbortEnvironmentUpdate :: AbortEnvironmentUpdateResponse -> TestTree
responseAbortEnvironmentUpdate =
  res
    "AbortEnvironmentUpdateResponse"
    "fixture/AbortEnvironmentUpdateResponse.proto"
    defaultService
    (Proxy :: Proxy AbortEnvironmentUpdate)

responseDeleteConfigurationTemplate :: DeleteConfigurationTemplateResponse -> TestTree
responseDeleteConfigurationTemplate =
  res
    "DeleteConfigurationTemplateResponse"
    "fixture/DeleteConfigurationTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConfigurationTemplate)

responseUpdateConfigurationTemplate :: ConfigurationSettingsDescription -> TestTree
responseUpdateConfigurationTemplate =
  res
    "UpdateConfigurationTemplateResponse"
    "fixture/UpdateConfigurationTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateConfigurationTemplate)

responseUpdateTagsForResource :: UpdateTagsForResourceResponse -> TestTree
responseUpdateTagsForResource =
  res
    "UpdateTagsForResourceResponse"
    "fixture/UpdateTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateTagsForResource)

responseDescribeEnvironmentResources :: DescribeEnvironmentResourcesResponse -> TestTree
responseDescribeEnvironmentResources =
  res
    "DescribeEnvironmentResourcesResponse"
    "fixture/DescribeEnvironmentResourcesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEnvironmentResources)

responseDescribeEnvironmentManagedActionHistory :: DescribeEnvironmentManagedActionHistoryResponse -> TestTree
responseDescribeEnvironmentManagedActionHistory =
  res
    "DescribeEnvironmentManagedActionHistoryResponse"
    "fixture/DescribeEnvironmentManagedActionHistoryResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEnvironmentManagedActionHistory)

responseDeleteApplicationVersion :: DeleteApplicationVersionResponse -> TestTree
responseDeleteApplicationVersion =
  res
    "DeleteApplicationVersionResponse"
    "fixture/DeleteApplicationVersionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApplicationVersion)

responseUpdateApplicationVersion :: ApplicationVersionDescriptionMessage -> TestTree
responseUpdateApplicationVersion =
  res
    "UpdateApplicationVersionResponse"
    "fixture/UpdateApplicationVersionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApplicationVersion)

responseCreateConfigurationTemplate :: ConfigurationSettingsDescription -> TestTree
responseCreateConfigurationTemplate =
  res
    "CreateConfigurationTemplateResponse"
    "fixture/CreateConfigurationTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateConfigurationTemplate)

responseDescribeEnvironmentHealth :: DescribeEnvironmentHealthResponse -> TestTree
responseDescribeEnvironmentHealth =
  res
    "DescribeEnvironmentHealthResponse"
    "fixture/DescribeEnvironmentHealthResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEnvironmentHealth)

responseRebuildEnvironment :: RebuildEnvironmentResponse -> TestTree
responseRebuildEnvironment =
  res
    "RebuildEnvironmentResponse"
    "fixture/RebuildEnvironmentResponse.proto"
    defaultService
    (Proxy :: Proxy RebuildEnvironment)

responseDeleteEnvironmentConfiguration :: DeleteEnvironmentConfigurationResponse -> TestTree
responseDeleteEnvironmentConfiguration =
  res
    "DeleteEnvironmentConfigurationResponse"
    "fixture/DeleteEnvironmentConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEnvironmentConfiguration)

responseUpdateApplicationResourceLifecycle :: UpdateApplicationResourceLifecycleResponse -> TestTree
responseUpdateApplicationResourceLifecycle =
  res
    "UpdateApplicationResourceLifecycleResponse"
    "fixture/UpdateApplicationResourceLifecycleResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApplicationResourceLifecycle)

responseSwapEnvironmentCNAMEs :: SwapEnvironmentCNAMEsResponse -> TestTree
responseSwapEnvironmentCNAMEs =
  res
    "SwapEnvironmentCNAMEsResponse"
    "fixture/SwapEnvironmentCNAMEsResponse.proto"
    defaultService
    (Proxy :: Proxy SwapEnvironmentCNAMEs)

responseListAvailableSolutionStacks :: ListAvailableSolutionStacksResponse -> TestTree
responseListAvailableSolutionStacks =
  res
    "ListAvailableSolutionStacksResponse"
    "fixture/ListAvailableSolutionStacksResponse.proto"
    defaultService
    (Proxy :: Proxy ListAvailableSolutionStacks)

responseApplyEnvironmentManagedAction :: ApplyEnvironmentManagedActionResponse -> TestTree
responseApplyEnvironmentManagedAction =
  res
    "ApplyEnvironmentManagedActionResponse"
    "fixture/ApplyEnvironmentManagedActionResponse.proto"
    defaultService
    (Proxy :: Proxy ApplyEnvironmentManagedAction)

responseDescribeConfigurationOptions :: DescribeConfigurationOptionsResponse -> TestTree
responseDescribeConfigurationOptions =
  res
    "DescribeConfigurationOptionsResponse"
    "fixture/DescribeConfigurationOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConfigurationOptions)

responseDisassociateEnvironmentOperationsRole :: DisassociateEnvironmentOperationsRoleResponse -> TestTree
responseDisassociateEnvironmentOperationsRole =
  res
    "DisassociateEnvironmentOperationsRoleResponse"
    "fixture/DisassociateEnvironmentOperationsRoleResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateEnvironmentOperationsRole)

responseCreateStorageLocation :: CreateStorageLocationResponse -> TestTree
responseCreateStorageLocation =
  res
    "CreateStorageLocationResponse"
    "fixture/CreateStorageLocationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateStorageLocation)

responseDescribeEnvironmentManagedActions :: DescribeEnvironmentManagedActionsResponse -> TestTree
responseDescribeEnvironmentManagedActions =
  res
    "DescribeEnvironmentManagedActionsResponse"
    "fixture/DescribeEnvironmentManagedActionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEnvironmentManagedActions)

responseDescribeConfigurationSettings :: DescribeConfigurationSettingsResponse -> TestTree
responseDescribeConfigurationSettings =
  res
    "DescribeConfigurationSettingsResponse"
    "fixture/DescribeConfigurationSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConfigurationSettings)

responseValidateConfigurationSettings :: ValidateConfigurationSettingsResponse -> TestTree
responseValidateConfigurationSettings =
  res
    "ValidateConfigurationSettingsResponse"
    "fixture/ValidateConfigurationSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy ValidateConfigurationSettings)

responseDescribeAccountAttributes :: DescribeAccountAttributesResponse -> TestTree
responseDescribeAccountAttributes =
  res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAccountAttributes)

responseAssociateEnvironmentOperationsRole :: AssociateEnvironmentOperationsRoleResponse -> TestTree
responseAssociateEnvironmentOperationsRole =
  res
    "AssociateEnvironmentOperationsRoleResponse"
    "fixture/AssociateEnvironmentOperationsRoleResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateEnvironmentOperationsRole)

responseRestartAppServer :: RestartAppServerResponse -> TestTree
responseRestartAppServer =
  res
    "RestartAppServerResponse"
    "fixture/RestartAppServerResponse.proto"
    defaultService
    (Proxy :: Proxy RestartAppServer)

responseDescribeEnvironments :: EnvironmentDescriptionsMessage -> TestTree
responseDescribeEnvironments =
  res
    "DescribeEnvironmentsResponse"
    "fixture/DescribeEnvironmentsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEnvironments)

responseCheckDNSAvailability :: CheckDNSAvailabilityResponse -> TestTree
responseCheckDNSAvailability =
  res
    "CheckDNSAvailabilityResponse"
    "fixture/CheckDNSAvailabilityResponse.proto"
    defaultService
    (Proxy :: Proxy CheckDNSAvailability)

responseDescribeApplicationVersions :: DescribeApplicationVersionsResponse -> TestTree
responseDescribeApplicationVersions =
  res
    "DescribeApplicationVersionsResponse"
    "fixture/DescribeApplicationVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeApplicationVersions)

responseCreateEnvironment :: EnvironmentDescription -> TestTree
responseCreateEnvironment =
  res
    "CreateEnvironmentResponse"
    "fixture/CreateEnvironmentResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEnvironment)

responseCreatePlatformVersion :: CreatePlatformVersionResponse -> TestTree
responseCreatePlatformVersion =
  res
    "CreatePlatformVersionResponse"
    "fixture/CreatePlatformVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePlatformVersion)
