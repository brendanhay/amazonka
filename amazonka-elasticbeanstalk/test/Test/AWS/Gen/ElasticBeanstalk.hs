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
--         [ requestSwapEnvironmentCNAMEs $
--             newSwapEnvironmentCNAMEs
--
--         , requestListPlatformBranches $
--             newListPlatformBranches
--
--         , requestListAvailableSolutionStacks $
--             newListAvailableSolutionStacks
--
--         , requestDescribeEnvironmentHealth $
--             newDescribeEnvironmentHealth
--
--         , requestCreateConfigurationTemplate $
--             newCreateConfigurationTemplate
--
--         , requestDescribeApplications $
--             newDescribeApplications
--
--         , requestListPlatformVersions $
--             newListPlatformVersions
--
--         , requestCreateApplicationVersion $
--             newCreateApplicationVersion
--
--         , requestTerminateEnvironment $
--             newTerminateEnvironment
--
--         , requestDescribeEnvironmentResources $
--             newDescribeEnvironmentResources
--
--         , requestUpdateApplicationVersion $
--             newUpdateApplicationVersion
--
--         , requestCreatePlatformVersion $
--             newCreatePlatformVersion
--
--         , requestDeleteApplicationVersion $
--             newDeleteApplicationVersion
--
--         , requestCreateEnvironment $
--             newCreateEnvironment
--
--         , requestCreateApplication $
--             newCreateApplication
--
--         , requestComposeEnvironments $
--             newComposeEnvironments
--
--         , requestCheckDNSAvailability $
--             newCheckDNSAvailability
--
--         , requestDescribeAccountAttributes $
--             newDescribeAccountAttributes
--
--         , requestValidateConfigurationSettings $
--             newValidateConfigurationSettings
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
--         , requestDescribeConfigurationOptions $
--             newDescribeConfigurationOptions
--
--         , requestRetrieveEnvironmentInfo $
--             newRetrieveEnvironmentInfo
--
--         , requestRequestEnvironmentInfo $
--             newRequestEnvironmentInfo
--
--         , requestDescribeEvents $
--             newDescribeEvents
--
--         , requestApplyEnvironmentManagedAction $
--             newApplyEnvironmentManagedAction
--
--         , requestUpdateApplicationResourceLifecycle $
--             newUpdateApplicationResourceLifecycle
--
--         , requestRebuildEnvironment $
--             newRebuildEnvironment
--
--         , requestUpdateEnvironment $
--             newUpdateEnvironment
--
--         , requestDeletePlatformVersion $
--             newDeletePlatformVersion
--
--         , requestDeleteEnvironmentConfiguration $
--             newDeleteEnvironmentConfiguration
--
--         , requestDescribeEnvironmentManagedActionHistory $
--             newDescribeEnvironmentManagedActionHistory
--
--         , requestUpdateConfigurationTemplate $
--             newUpdateConfigurationTemplate
--
--         , requestDeleteConfigurationTemplate $
--             newDeleteConfigurationTemplate
--
--         , requestUpdateTagsForResource $
--             newUpdateTagsForResource
--
--         , requestDescribeApplicationVersions $
--             newDescribeApplicationVersions
--
--         , requestAbortEnvironmentUpdate $
--             newAbortEnvironmentUpdate
--
--         , requestDescribeEnvironments $
--             newDescribeEnvironments
--
--         , requestRestartAppServer $
--             newRestartAppServer
--
--         , requestAssociateEnvironmentOperationsRole $
--             newAssociateEnvironmentOperationsRole
--
--         , requestDeleteApplication $
--             newDeleteApplication
--
--         , requestDescribeInstancesHealth $
--             newDescribeInstancesHealth
--
--         , requestUpdateApplication $
--             newUpdateApplication
--
--         , requestDescribePlatformVersion $
--             newDescribePlatformVersion
--
--         , requestDisassociateEnvironmentOperationsRole $
--             newDisassociateEnvironmentOperationsRole
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--           ]

--     , testGroup "response"
--         [ responseSwapEnvironmentCNAMEs $
--             newSwapEnvironmentCNAMEsResponse
--
--         , responseListPlatformBranches $
--             newListPlatformBranchesResponse
--
--         , responseListAvailableSolutionStacks $
--             newListAvailableSolutionStacksResponse
--
--         , responseDescribeEnvironmentHealth $
--             newDescribeEnvironmentHealthResponse
--
--         , responseCreateConfigurationTemplate $
--             newConfigurationSettingsDescription
--
--         , responseDescribeApplications $
--             newDescribeApplicationsResponse
--
--         , responseListPlatformVersions $
--             newListPlatformVersionsResponse
--
--         , responseCreateApplicationVersion $
--             newApplicationVersionDescriptionMessage
--
--         , responseTerminateEnvironment $
--             newEnvironmentDescription
--
--         , responseDescribeEnvironmentResources $
--             newDescribeEnvironmentResourcesResponse
--
--         , responseUpdateApplicationVersion $
--             newApplicationVersionDescriptionMessage
--
--         , responseCreatePlatformVersion $
--             newCreatePlatformVersionResponse
--
--         , responseDeleteApplicationVersion $
--             newDeleteApplicationVersionResponse
--
--         , responseCreateEnvironment $
--             newEnvironmentDescription
--
--         , responseCreateApplication $
--             newApplicationDescriptionMessage
--
--         , responseComposeEnvironments $
--             newEnvironmentDescriptionsMessage
--
--         , responseCheckDNSAvailability $
--             newCheckDNSAvailabilityResponse
--
--         , responseDescribeAccountAttributes $
--             newDescribeAccountAttributesResponse
--
--         , responseValidateConfigurationSettings $
--             newValidateConfigurationSettingsResponse
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
--         , responseDescribeConfigurationOptions $
--             newDescribeConfigurationOptionsResponse
--
--         , responseRetrieveEnvironmentInfo $
--             newRetrieveEnvironmentInfoResponse
--
--         , responseRequestEnvironmentInfo $
--             newRequestEnvironmentInfoResponse
--
--         , responseDescribeEvents $
--             newDescribeEventsResponse
--
--         , responseApplyEnvironmentManagedAction $
--             newApplyEnvironmentManagedActionResponse
--
--         , responseUpdateApplicationResourceLifecycle $
--             newUpdateApplicationResourceLifecycleResponse
--
--         , responseRebuildEnvironment $
--             newRebuildEnvironmentResponse
--
--         , responseUpdateEnvironment $
--             newEnvironmentDescription
--
--         , responseDeletePlatformVersion $
--             newDeletePlatformVersionResponse
--
--         , responseDeleteEnvironmentConfiguration $
--             newDeleteEnvironmentConfigurationResponse
--
--         , responseDescribeEnvironmentManagedActionHistory $
--             newDescribeEnvironmentManagedActionHistoryResponse
--
--         , responseUpdateConfigurationTemplate $
--             newConfigurationSettingsDescription
--
--         , responseDeleteConfigurationTemplate $
--             newDeleteConfigurationTemplateResponse
--
--         , responseUpdateTagsForResource $
--             newUpdateTagsForResourceResponse
--
--         , responseDescribeApplicationVersions $
--             newDescribeApplicationVersionsResponse
--
--         , responseAbortEnvironmentUpdate $
--             newAbortEnvironmentUpdateResponse
--
--         , responseDescribeEnvironments $
--             newEnvironmentDescriptionsMessage
--
--         , responseRestartAppServer $
--             newRestartAppServerResponse
--
--         , responseAssociateEnvironmentOperationsRole $
--             newAssociateEnvironmentOperationsRoleResponse
--
--         , responseDeleteApplication $
--             newDeleteApplicationResponse
--
--         , responseDescribeInstancesHealth $
--             newDescribeInstancesHealthResponse
--
--         , responseUpdateApplication $
--             newApplicationDescriptionMessage
--
--         , responseDescribePlatformVersion $
--             newDescribePlatformVersionResponse
--
--         , responseDisassociateEnvironmentOperationsRole $
--             newDisassociateEnvironmentOperationsRoleResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--           ]
--     ]

-- Requests

requestSwapEnvironmentCNAMEs :: SwapEnvironmentCNAMEs -> TestTree
requestSwapEnvironmentCNAMEs =
  req
    "SwapEnvironmentCNAMEs"
    "fixture/SwapEnvironmentCNAMEs.yaml"

requestListPlatformBranches :: ListPlatformBranches -> TestTree
requestListPlatformBranches =
  req
    "ListPlatformBranches"
    "fixture/ListPlatformBranches.yaml"

requestListAvailableSolutionStacks :: ListAvailableSolutionStacks -> TestTree
requestListAvailableSolutionStacks =
  req
    "ListAvailableSolutionStacks"
    "fixture/ListAvailableSolutionStacks.yaml"

requestDescribeEnvironmentHealth :: DescribeEnvironmentHealth -> TestTree
requestDescribeEnvironmentHealth =
  req
    "DescribeEnvironmentHealth"
    "fixture/DescribeEnvironmentHealth.yaml"

requestCreateConfigurationTemplate :: CreateConfigurationTemplate -> TestTree
requestCreateConfigurationTemplate =
  req
    "CreateConfigurationTemplate"
    "fixture/CreateConfigurationTemplate.yaml"

requestDescribeApplications :: DescribeApplications -> TestTree
requestDescribeApplications =
  req
    "DescribeApplications"
    "fixture/DescribeApplications.yaml"

requestListPlatformVersions :: ListPlatformVersions -> TestTree
requestListPlatformVersions =
  req
    "ListPlatformVersions"
    "fixture/ListPlatformVersions.yaml"

requestCreateApplicationVersion :: CreateApplicationVersion -> TestTree
requestCreateApplicationVersion =
  req
    "CreateApplicationVersion"
    "fixture/CreateApplicationVersion.yaml"

requestTerminateEnvironment :: TerminateEnvironment -> TestTree
requestTerminateEnvironment =
  req
    "TerminateEnvironment"
    "fixture/TerminateEnvironment.yaml"

requestDescribeEnvironmentResources :: DescribeEnvironmentResources -> TestTree
requestDescribeEnvironmentResources =
  req
    "DescribeEnvironmentResources"
    "fixture/DescribeEnvironmentResources.yaml"

requestUpdateApplicationVersion :: UpdateApplicationVersion -> TestTree
requestUpdateApplicationVersion =
  req
    "UpdateApplicationVersion"
    "fixture/UpdateApplicationVersion.yaml"

requestCreatePlatformVersion :: CreatePlatformVersion -> TestTree
requestCreatePlatformVersion =
  req
    "CreatePlatformVersion"
    "fixture/CreatePlatformVersion.yaml"

requestDeleteApplicationVersion :: DeleteApplicationVersion -> TestTree
requestDeleteApplicationVersion =
  req
    "DeleteApplicationVersion"
    "fixture/DeleteApplicationVersion.yaml"

requestCreateEnvironment :: CreateEnvironment -> TestTree
requestCreateEnvironment =
  req
    "CreateEnvironment"
    "fixture/CreateEnvironment.yaml"

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

requestCheckDNSAvailability :: CheckDNSAvailability -> TestTree
requestCheckDNSAvailability =
  req
    "CheckDNSAvailability"
    "fixture/CheckDNSAvailability.yaml"

requestDescribeAccountAttributes :: DescribeAccountAttributes -> TestTree
requestDescribeAccountAttributes =
  req
    "DescribeAccountAttributes"
    "fixture/DescribeAccountAttributes.yaml"

requestValidateConfigurationSettings :: ValidateConfigurationSettings -> TestTree
requestValidateConfigurationSettings =
  req
    "ValidateConfigurationSettings"
    "fixture/ValidateConfigurationSettings.yaml"

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

requestDescribeConfigurationOptions :: DescribeConfigurationOptions -> TestTree
requestDescribeConfigurationOptions =
  req
    "DescribeConfigurationOptions"
    "fixture/DescribeConfigurationOptions.yaml"

requestRetrieveEnvironmentInfo :: RetrieveEnvironmentInfo -> TestTree
requestRetrieveEnvironmentInfo =
  req
    "RetrieveEnvironmentInfo"
    "fixture/RetrieveEnvironmentInfo.yaml"

requestRequestEnvironmentInfo :: RequestEnvironmentInfo -> TestTree
requestRequestEnvironmentInfo =
  req
    "RequestEnvironmentInfo"
    "fixture/RequestEnvironmentInfo.yaml"

requestDescribeEvents :: DescribeEvents -> TestTree
requestDescribeEvents =
  req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

requestApplyEnvironmentManagedAction :: ApplyEnvironmentManagedAction -> TestTree
requestApplyEnvironmentManagedAction =
  req
    "ApplyEnvironmentManagedAction"
    "fixture/ApplyEnvironmentManagedAction.yaml"

requestUpdateApplicationResourceLifecycle :: UpdateApplicationResourceLifecycle -> TestTree
requestUpdateApplicationResourceLifecycle =
  req
    "UpdateApplicationResourceLifecycle"
    "fixture/UpdateApplicationResourceLifecycle.yaml"

requestRebuildEnvironment :: RebuildEnvironment -> TestTree
requestRebuildEnvironment =
  req
    "RebuildEnvironment"
    "fixture/RebuildEnvironment.yaml"

requestUpdateEnvironment :: UpdateEnvironment -> TestTree
requestUpdateEnvironment =
  req
    "UpdateEnvironment"
    "fixture/UpdateEnvironment.yaml"

requestDeletePlatformVersion :: DeletePlatformVersion -> TestTree
requestDeletePlatformVersion =
  req
    "DeletePlatformVersion"
    "fixture/DeletePlatformVersion.yaml"

requestDeleteEnvironmentConfiguration :: DeleteEnvironmentConfiguration -> TestTree
requestDeleteEnvironmentConfiguration =
  req
    "DeleteEnvironmentConfiguration"
    "fixture/DeleteEnvironmentConfiguration.yaml"

requestDescribeEnvironmentManagedActionHistory :: DescribeEnvironmentManagedActionHistory -> TestTree
requestDescribeEnvironmentManagedActionHistory =
  req
    "DescribeEnvironmentManagedActionHistory"
    "fixture/DescribeEnvironmentManagedActionHistory.yaml"

requestUpdateConfigurationTemplate :: UpdateConfigurationTemplate -> TestTree
requestUpdateConfigurationTemplate =
  req
    "UpdateConfigurationTemplate"
    "fixture/UpdateConfigurationTemplate.yaml"

requestDeleteConfigurationTemplate :: DeleteConfigurationTemplate -> TestTree
requestDeleteConfigurationTemplate =
  req
    "DeleteConfigurationTemplate"
    "fixture/DeleteConfigurationTemplate.yaml"

requestUpdateTagsForResource :: UpdateTagsForResource -> TestTree
requestUpdateTagsForResource =
  req
    "UpdateTagsForResource"
    "fixture/UpdateTagsForResource.yaml"

requestDescribeApplicationVersions :: DescribeApplicationVersions -> TestTree
requestDescribeApplicationVersions =
  req
    "DescribeApplicationVersions"
    "fixture/DescribeApplicationVersions.yaml"

requestAbortEnvironmentUpdate :: AbortEnvironmentUpdate -> TestTree
requestAbortEnvironmentUpdate =
  req
    "AbortEnvironmentUpdate"
    "fixture/AbortEnvironmentUpdate.yaml"

requestDescribeEnvironments :: DescribeEnvironments -> TestTree
requestDescribeEnvironments =
  req
    "DescribeEnvironments"
    "fixture/DescribeEnvironments.yaml"

requestRestartAppServer :: RestartAppServer -> TestTree
requestRestartAppServer =
  req
    "RestartAppServer"
    "fixture/RestartAppServer.yaml"

requestAssociateEnvironmentOperationsRole :: AssociateEnvironmentOperationsRole -> TestTree
requestAssociateEnvironmentOperationsRole =
  req
    "AssociateEnvironmentOperationsRole"
    "fixture/AssociateEnvironmentOperationsRole.yaml"

requestDeleteApplication :: DeleteApplication -> TestTree
requestDeleteApplication =
  req
    "DeleteApplication"
    "fixture/DeleteApplication.yaml"

requestDescribeInstancesHealth :: DescribeInstancesHealth -> TestTree
requestDescribeInstancesHealth =
  req
    "DescribeInstancesHealth"
    "fixture/DescribeInstancesHealth.yaml"

requestUpdateApplication :: UpdateApplication -> TestTree
requestUpdateApplication =
  req
    "UpdateApplication"
    "fixture/UpdateApplication.yaml"

requestDescribePlatformVersion :: DescribePlatformVersion -> TestTree
requestDescribePlatformVersion =
  req
    "DescribePlatformVersion"
    "fixture/DescribePlatformVersion.yaml"

requestDisassociateEnvironmentOperationsRole :: DisassociateEnvironmentOperationsRole -> TestTree
requestDisassociateEnvironmentOperationsRole =
  req
    "DisassociateEnvironmentOperationsRole"
    "fixture/DisassociateEnvironmentOperationsRole.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

-- Responses

responseSwapEnvironmentCNAMEs :: SwapEnvironmentCNAMEsResponse -> TestTree
responseSwapEnvironmentCNAMEs =
  res
    "SwapEnvironmentCNAMEsResponse"
    "fixture/SwapEnvironmentCNAMEsResponse.proto"
    defaultService
    (Proxy :: Proxy SwapEnvironmentCNAMEs)

responseListPlatformBranches :: ListPlatformBranchesResponse -> TestTree
responseListPlatformBranches =
  res
    "ListPlatformBranchesResponse"
    "fixture/ListPlatformBranchesResponse.proto"
    defaultService
    (Proxy :: Proxy ListPlatformBranches)

responseListAvailableSolutionStacks :: ListAvailableSolutionStacksResponse -> TestTree
responseListAvailableSolutionStacks =
  res
    "ListAvailableSolutionStacksResponse"
    "fixture/ListAvailableSolutionStacksResponse.proto"
    defaultService
    (Proxy :: Proxy ListAvailableSolutionStacks)

responseDescribeEnvironmentHealth :: DescribeEnvironmentHealthResponse -> TestTree
responseDescribeEnvironmentHealth =
  res
    "DescribeEnvironmentHealthResponse"
    "fixture/DescribeEnvironmentHealthResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEnvironmentHealth)

responseCreateConfigurationTemplate :: ConfigurationSettingsDescription -> TestTree
responseCreateConfigurationTemplate =
  res
    "CreateConfigurationTemplateResponse"
    "fixture/CreateConfigurationTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy CreateConfigurationTemplate)

responseDescribeApplications :: DescribeApplicationsResponse -> TestTree
responseDescribeApplications =
  res
    "DescribeApplicationsResponse"
    "fixture/DescribeApplicationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeApplications)

responseListPlatformVersions :: ListPlatformVersionsResponse -> TestTree
responseListPlatformVersions =
  res
    "ListPlatformVersionsResponse"
    "fixture/ListPlatformVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPlatformVersions)

responseCreateApplicationVersion :: ApplicationVersionDescriptionMessage -> TestTree
responseCreateApplicationVersion =
  res
    "CreateApplicationVersionResponse"
    "fixture/CreateApplicationVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateApplicationVersion)

responseTerminateEnvironment :: EnvironmentDescription -> TestTree
responseTerminateEnvironment =
  res
    "TerminateEnvironmentResponse"
    "fixture/TerminateEnvironmentResponse.proto"
    defaultService
    (Proxy :: Proxy TerminateEnvironment)

responseDescribeEnvironmentResources :: DescribeEnvironmentResourcesResponse -> TestTree
responseDescribeEnvironmentResources =
  res
    "DescribeEnvironmentResourcesResponse"
    "fixture/DescribeEnvironmentResourcesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEnvironmentResources)

responseUpdateApplicationVersion :: ApplicationVersionDescriptionMessage -> TestTree
responseUpdateApplicationVersion =
  res
    "UpdateApplicationVersionResponse"
    "fixture/UpdateApplicationVersionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApplicationVersion)

responseCreatePlatformVersion :: CreatePlatformVersionResponse -> TestTree
responseCreatePlatformVersion =
  res
    "CreatePlatformVersionResponse"
    "fixture/CreatePlatformVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePlatformVersion)

responseDeleteApplicationVersion :: DeleteApplicationVersionResponse -> TestTree
responseDeleteApplicationVersion =
  res
    "DeleteApplicationVersionResponse"
    "fixture/DeleteApplicationVersionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApplicationVersion)

responseCreateEnvironment :: EnvironmentDescription -> TestTree
responseCreateEnvironment =
  res
    "CreateEnvironmentResponse"
    "fixture/CreateEnvironmentResponse.proto"
    defaultService
    (Proxy :: Proxy CreateEnvironment)

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

responseCheckDNSAvailability :: CheckDNSAvailabilityResponse -> TestTree
responseCheckDNSAvailability =
  res
    "CheckDNSAvailabilityResponse"
    "fixture/CheckDNSAvailabilityResponse.proto"
    defaultService
    (Proxy :: Proxy CheckDNSAvailability)

responseDescribeAccountAttributes :: DescribeAccountAttributesResponse -> TestTree
responseDescribeAccountAttributes =
  res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAccountAttributes)

responseValidateConfigurationSettings :: ValidateConfigurationSettingsResponse -> TestTree
responseValidateConfigurationSettings =
  res
    "ValidateConfigurationSettingsResponse"
    "fixture/ValidateConfigurationSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy ValidateConfigurationSettings)

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

responseDescribeConfigurationOptions :: DescribeConfigurationOptionsResponse -> TestTree
responseDescribeConfigurationOptions =
  res
    "DescribeConfigurationOptionsResponse"
    "fixture/DescribeConfigurationOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConfigurationOptions)

responseRetrieveEnvironmentInfo :: RetrieveEnvironmentInfoResponse -> TestTree
responseRetrieveEnvironmentInfo =
  res
    "RetrieveEnvironmentInfoResponse"
    "fixture/RetrieveEnvironmentInfoResponse.proto"
    defaultService
    (Proxy :: Proxy RetrieveEnvironmentInfo)

responseRequestEnvironmentInfo :: RequestEnvironmentInfoResponse -> TestTree
responseRequestEnvironmentInfo =
  res
    "RequestEnvironmentInfoResponse"
    "fixture/RequestEnvironmentInfoResponse.proto"
    defaultService
    (Proxy :: Proxy RequestEnvironmentInfo)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents =
  res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEvents)

responseApplyEnvironmentManagedAction :: ApplyEnvironmentManagedActionResponse -> TestTree
responseApplyEnvironmentManagedAction =
  res
    "ApplyEnvironmentManagedActionResponse"
    "fixture/ApplyEnvironmentManagedActionResponse.proto"
    defaultService
    (Proxy :: Proxy ApplyEnvironmentManagedAction)

responseUpdateApplicationResourceLifecycle :: UpdateApplicationResourceLifecycleResponse -> TestTree
responseUpdateApplicationResourceLifecycle =
  res
    "UpdateApplicationResourceLifecycleResponse"
    "fixture/UpdateApplicationResourceLifecycleResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApplicationResourceLifecycle)

responseRebuildEnvironment :: RebuildEnvironmentResponse -> TestTree
responseRebuildEnvironment =
  res
    "RebuildEnvironmentResponse"
    "fixture/RebuildEnvironmentResponse.proto"
    defaultService
    (Proxy :: Proxy RebuildEnvironment)

responseUpdateEnvironment :: EnvironmentDescription -> TestTree
responseUpdateEnvironment =
  res
    "UpdateEnvironmentResponse"
    "fixture/UpdateEnvironmentResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateEnvironment)

responseDeletePlatformVersion :: DeletePlatformVersionResponse -> TestTree
responseDeletePlatformVersion =
  res
    "DeletePlatformVersionResponse"
    "fixture/DeletePlatformVersionResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePlatformVersion)

responseDeleteEnvironmentConfiguration :: DeleteEnvironmentConfigurationResponse -> TestTree
responseDeleteEnvironmentConfiguration =
  res
    "DeleteEnvironmentConfigurationResponse"
    "fixture/DeleteEnvironmentConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEnvironmentConfiguration)

responseDescribeEnvironmentManagedActionHistory :: DescribeEnvironmentManagedActionHistoryResponse -> TestTree
responseDescribeEnvironmentManagedActionHistory =
  res
    "DescribeEnvironmentManagedActionHistoryResponse"
    "fixture/DescribeEnvironmentManagedActionHistoryResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEnvironmentManagedActionHistory)

responseUpdateConfigurationTemplate :: ConfigurationSettingsDescription -> TestTree
responseUpdateConfigurationTemplate =
  res
    "UpdateConfigurationTemplateResponse"
    "fixture/UpdateConfigurationTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateConfigurationTemplate)

responseDeleteConfigurationTemplate :: DeleteConfigurationTemplateResponse -> TestTree
responseDeleteConfigurationTemplate =
  res
    "DeleteConfigurationTemplateResponse"
    "fixture/DeleteConfigurationTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConfigurationTemplate)

responseUpdateTagsForResource :: UpdateTagsForResourceResponse -> TestTree
responseUpdateTagsForResource =
  res
    "UpdateTagsForResourceResponse"
    "fixture/UpdateTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateTagsForResource)

responseDescribeApplicationVersions :: DescribeApplicationVersionsResponse -> TestTree
responseDescribeApplicationVersions =
  res
    "DescribeApplicationVersionsResponse"
    "fixture/DescribeApplicationVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeApplicationVersions)

responseAbortEnvironmentUpdate :: AbortEnvironmentUpdateResponse -> TestTree
responseAbortEnvironmentUpdate =
  res
    "AbortEnvironmentUpdateResponse"
    "fixture/AbortEnvironmentUpdateResponse.proto"
    defaultService
    (Proxy :: Proxy AbortEnvironmentUpdate)

responseDescribeEnvironments :: EnvironmentDescriptionsMessage -> TestTree
responseDescribeEnvironments =
  res
    "DescribeEnvironmentsResponse"
    "fixture/DescribeEnvironmentsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEnvironments)

responseRestartAppServer :: RestartAppServerResponse -> TestTree
responseRestartAppServer =
  res
    "RestartAppServerResponse"
    "fixture/RestartAppServerResponse.proto"
    defaultService
    (Proxy :: Proxy RestartAppServer)

responseAssociateEnvironmentOperationsRole :: AssociateEnvironmentOperationsRoleResponse -> TestTree
responseAssociateEnvironmentOperationsRole =
  res
    "AssociateEnvironmentOperationsRoleResponse"
    "fixture/AssociateEnvironmentOperationsRoleResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateEnvironmentOperationsRole)

responseDeleteApplication :: DeleteApplicationResponse -> TestTree
responseDeleteApplication =
  res
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApplication)

responseDescribeInstancesHealth :: DescribeInstancesHealthResponse -> TestTree
responseDescribeInstancesHealth =
  res
    "DescribeInstancesHealthResponse"
    "fixture/DescribeInstancesHealthResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeInstancesHealth)

responseUpdateApplication :: ApplicationDescriptionMessage -> TestTree
responseUpdateApplication =
  res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateApplication)

responseDescribePlatformVersion :: DescribePlatformVersionResponse -> TestTree
responseDescribePlatformVersion =
  res
    "DescribePlatformVersionResponse"
    "fixture/DescribePlatformVersionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePlatformVersion)

responseDisassociateEnvironmentOperationsRole :: DisassociateEnvironmentOperationsRoleResponse -> TestTree
responseDisassociateEnvironmentOperationsRole =
  res
    "DisassociateEnvironmentOperationsRoleResponse"
    "fixture/DisassociateEnvironmentOperationsRoleResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateEnvironmentOperationsRole)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)
