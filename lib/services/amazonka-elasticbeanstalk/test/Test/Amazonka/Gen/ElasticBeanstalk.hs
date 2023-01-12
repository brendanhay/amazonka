{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ElasticBeanstalk
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ElasticBeanstalk where

import Amazonka.ElasticBeanstalk
import qualified Data.Proxy as Proxy
import Test.Amazonka.ElasticBeanstalk.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAbortEnvironmentUpdate $
--             newAbortEnvironmentUpdate
--
--         , requestApplyEnvironmentManagedAction $
--             newApplyEnvironmentManagedAction
--
--         , requestAssociateEnvironmentOperationsRole $
--             newAssociateEnvironmentOperationsRole
--
--         , requestCheckDNSAvailability $
--             newCheckDNSAvailability
--
--         , requestComposeEnvironments $
--             newComposeEnvironments
--
--         , requestCreateApplication $
--             newCreateApplication
--
--         , requestCreateApplicationVersion $
--             newCreateApplicationVersion
--
--         , requestCreateConfigurationTemplate $
--             newCreateConfigurationTemplate
--
--         , requestCreateEnvironment $
--             newCreateEnvironment
--
--         , requestCreatePlatformVersion $
--             newCreatePlatformVersion
--
--         , requestCreateStorageLocation $
--             newCreateStorageLocation
--
--         , requestDeleteApplication $
--             newDeleteApplication
--
--         , requestDeleteApplicationVersion $
--             newDeleteApplicationVersion
--
--         , requestDeleteConfigurationTemplate $
--             newDeleteConfigurationTemplate
--
--         , requestDeleteEnvironmentConfiguration $
--             newDeleteEnvironmentConfiguration
--
--         , requestDeletePlatformVersion $
--             newDeletePlatformVersion
--
--         , requestDescribeAccountAttributes $
--             newDescribeAccountAttributes
--
--         , requestDescribeApplicationVersions $
--             newDescribeApplicationVersions
--
--         , requestDescribeApplications $
--             newDescribeApplications
--
--         , requestDescribeConfigurationOptions $
--             newDescribeConfigurationOptions
--
--         , requestDescribeConfigurationSettings $
--             newDescribeConfigurationSettings
--
--         , requestDescribeEnvironmentHealth $
--             newDescribeEnvironmentHealth
--
--         , requestDescribeEnvironmentManagedActionHistory $
--             newDescribeEnvironmentManagedActionHistory
--
--         , requestDescribeEnvironmentManagedActions $
--             newDescribeEnvironmentManagedActions
--
--         , requestDescribeEnvironmentResources $
--             newDescribeEnvironmentResources
--
--         , requestDescribeEnvironments $
--             newDescribeEnvironments
--
--         , requestDescribeEvents $
--             newDescribeEvents
--
--         , requestDescribeInstancesHealth $
--             newDescribeInstancesHealth
--
--         , requestDescribePlatformVersion $
--             newDescribePlatformVersion
--
--         , requestDisassociateEnvironmentOperationsRole $
--             newDisassociateEnvironmentOperationsRole
--
--         , requestListAvailableSolutionStacks $
--             newListAvailableSolutionStacks
--
--         , requestListPlatformBranches $
--             newListPlatformBranches
--
--         , requestListPlatformVersions $
--             newListPlatformVersions
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestRebuildEnvironment $
--             newRebuildEnvironment
--
--         , requestRequestEnvironmentInfo $
--             newRequestEnvironmentInfo
--
--         , requestRestartAppServer $
--             newRestartAppServer
--
--         , requestRetrieveEnvironmentInfo $
--             newRetrieveEnvironmentInfo
--
--         , requestSwapEnvironmentCNAMEs $
--             newSwapEnvironmentCNAMEs
--
--         , requestTerminateEnvironment $
--             newTerminateEnvironment
--
--         , requestUpdateApplication $
--             newUpdateApplication
--
--         , requestUpdateApplicationResourceLifecycle $
--             newUpdateApplicationResourceLifecycle
--
--         , requestUpdateApplicationVersion $
--             newUpdateApplicationVersion
--
--         , requestUpdateConfigurationTemplate $
--             newUpdateConfigurationTemplate
--
--         , requestUpdateEnvironment $
--             newUpdateEnvironment
--
--         , requestUpdateTagsForResource $
--             newUpdateTagsForResource
--
--         , requestValidateConfigurationSettings $
--             newValidateConfigurationSettings
--
--           ]

--     , testGroup "response"
--         [ responseAbortEnvironmentUpdate $
--             newAbortEnvironmentUpdateResponse
--
--         , responseApplyEnvironmentManagedAction $
--             newApplyEnvironmentManagedActionResponse
--
--         , responseAssociateEnvironmentOperationsRole $
--             newAssociateEnvironmentOperationsRoleResponse
--
--         , responseCheckDNSAvailability $
--             newCheckDNSAvailabilityResponse
--
--         , responseComposeEnvironments $
--             newEnvironmentDescriptionsMessage
--
--         , responseCreateApplication $
--             newApplicationDescriptionMessage
--
--         , responseCreateApplicationVersion $
--             newApplicationVersionDescriptionMessage
--
--         , responseCreateConfigurationTemplate $
--             newConfigurationSettingsDescription
--
--         , responseCreateEnvironment $
--             newEnvironmentDescription
--
--         , responseCreatePlatformVersion $
--             newCreatePlatformVersionResponse
--
--         , responseCreateStorageLocation $
--             newCreateStorageLocationResponse
--
--         , responseDeleteApplication $
--             newDeleteApplicationResponse
--
--         , responseDeleteApplicationVersion $
--             newDeleteApplicationVersionResponse
--
--         , responseDeleteConfigurationTemplate $
--             newDeleteConfigurationTemplateResponse
--
--         , responseDeleteEnvironmentConfiguration $
--             newDeleteEnvironmentConfigurationResponse
--
--         , responseDeletePlatformVersion $
--             newDeletePlatformVersionResponse
--
--         , responseDescribeAccountAttributes $
--             newDescribeAccountAttributesResponse
--
--         , responseDescribeApplicationVersions $
--             newDescribeApplicationVersionsResponse
--
--         , responseDescribeApplications $
--             newDescribeApplicationsResponse
--
--         , responseDescribeConfigurationOptions $
--             newDescribeConfigurationOptionsResponse
--
--         , responseDescribeConfigurationSettings $
--             newDescribeConfigurationSettingsResponse
--
--         , responseDescribeEnvironmentHealth $
--             newDescribeEnvironmentHealthResponse
--
--         , responseDescribeEnvironmentManagedActionHistory $
--             newDescribeEnvironmentManagedActionHistoryResponse
--
--         , responseDescribeEnvironmentManagedActions $
--             newDescribeEnvironmentManagedActionsResponse
--
--         , responseDescribeEnvironmentResources $
--             newDescribeEnvironmentResourcesResponse
--
--         , responseDescribeEnvironments $
--             newEnvironmentDescriptionsMessage
--
--         , responseDescribeEvents $
--             newDescribeEventsResponse
--
--         , responseDescribeInstancesHealth $
--             newDescribeInstancesHealthResponse
--
--         , responseDescribePlatformVersion $
--             newDescribePlatformVersionResponse
--
--         , responseDisassociateEnvironmentOperationsRole $
--             newDisassociateEnvironmentOperationsRoleResponse
--
--         , responseListAvailableSolutionStacks $
--             newListAvailableSolutionStacksResponse
--
--         , responseListPlatformBranches $
--             newListPlatformBranchesResponse
--
--         , responseListPlatformVersions $
--             newListPlatformVersionsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseRebuildEnvironment $
--             newRebuildEnvironmentResponse
--
--         , responseRequestEnvironmentInfo $
--             newRequestEnvironmentInfoResponse
--
--         , responseRestartAppServer $
--             newRestartAppServerResponse
--
--         , responseRetrieveEnvironmentInfo $
--             newRetrieveEnvironmentInfoResponse
--
--         , responseSwapEnvironmentCNAMEs $
--             newSwapEnvironmentCNAMEsResponse
--
--         , responseTerminateEnvironment $
--             newEnvironmentDescription
--
--         , responseUpdateApplication $
--             newApplicationDescriptionMessage
--
--         , responseUpdateApplicationResourceLifecycle $
--             newUpdateApplicationResourceLifecycleResponse
--
--         , responseUpdateApplicationVersion $
--             newApplicationVersionDescriptionMessage
--
--         , responseUpdateConfigurationTemplate $
--             newConfigurationSettingsDescription
--
--         , responseUpdateEnvironment $
--             newEnvironmentDescription
--
--         , responseUpdateTagsForResource $
--             newUpdateTagsForResourceResponse
--
--         , responseValidateConfigurationSettings $
--             newValidateConfigurationSettingsResponse
--
--           ]
--     ]

-- Requests

requestAbortEnvironmentUpdate :: AbortEnvironmentUpdate -> TestTree
requestAbortEnvironmentUpdate =
  req
    "AbortEnvironmentUpdate"
    "fixture/AbortEnvironmentUpdate.yaml"

requestApplyEnvironmentManagedAction :: ApplyEnvironmentManagedAction -> TestTree
requestApplyEnvironmentManagedAction =
  req
    "ApplyEnvironmentManagedAction"
    "fixture/ApplyEnvironmentManagedAction.yaml"

requestAssociateEnvironmentOperationsRole :: AssociateEnvironmentOperationsRole -> TestTree
requestAssociateEnvironmentOperationsRole =
  req
    "AssociateEnvironmentOperationsRole"
    "fixture/AssociateEnvironmentOperationsRole.yaml"

requestCheckDNSAvailability :: CheckDNSAvailability -> TestTree
requestCheckDNSAvailability =
  req
    "CheckDNSAvailability"
    "fixture/CheckDNSAvailability.yaml"

requestComposeEnvironments :: ComposeEnvironments -> TestTree
requestComposeEnvironments =
  req
    "ComposeEnvironments"
    "fixture/ComposeEnvironments.yaml"

requestCreateApplication :: CreateApplication -> TestTree
requestCreateApplication =
  req
    "CreateApplication"
    "fixture/CreateApplication.yaml"

requestCreateApplicationVersion :: CreateApplicationVersion -> TestTree
requestCreateApplicationVersion =
  req
    "CreateApplicationVersion"
    "fixture/CreateApplicationVersion.yaml"

requestCreateConfigurationTemplate :: CreateConfigurationTemplate -> TestTree
requestCreateConfigurationTemplate =
  req
    "CreateConfigurationTemplate"
    "fixture/CreateConfigurationTemplate.yaml"

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

requestCreateStorageLocation :: CreateStorageLocation -> TestTree
requestCreateStorageLocation =
  req
    "CreateStorageLocation"
    "fixture/CreateStorageLocation.yaml"

requestDeleteApplication :: DeleteApplication -> TestTree
requestDeleteApplication =
  req
    "DeleteApplication"
    "fixture/DeleteApplication.yaml"

requestDeleteApplicationVersion :: DeleteApplicationVersion -> TestTree
requestDeleteApplicationVersion =
  req
    "DeleteApplicationVersion"
    "fixture/DeleteApplicationVersion.yaml"

requestDeleteConfigurationTemplate :: DeleteConfigurationTemplate -> TestTree
requestDeleteConfigurationTemplate =
  req
    "DeleteConfigurationTemplate"
    "fixture/DeleteConfigurationTemplate.yaml"

requestDeleteEnvironmentConfiguration :: DeleteEnvironmentConfiguration -> TestTree
requestDeleteEnvironmentConfiguration =
  req
    "DeleteEnvironmentConfiguration"
    "fixture/DeleteEnvironmentConfiguration.yaml"

requestDeletePlatformVersion :: DeletePlatformVersion -> TestTree
requestDeletePlatformVersion =
  req
    "DeletePlatformVersion"
    "fixture/DeletePlatformVersion.yaml"

requestDescribeAccountAttributes :: DescribeAccountAttributes -> TestTree
requestDescribeAccountAttributes =
  req
    "DescribeAccountAttributes"
    "fixture/DescribeAccountAttributes.yaml"

requestDescribeApplicationVersions :: DescribeApplicationVersions -> TestTree
requestDescribeApplicationVersions =
  req
    "DescribeApplicationVersions"
    "fixture/DescribeApplicationVersions.yaml"

requestDescribeApplications :: DescribeApplications -> TestTree
requestDescribeApplications =
  req
    "DescribeApplications"
    "fixture/DescribeApplications.yaml"

requestDescribeConfigurationOptions :: DescribeConfigurationOptions -> TestTree
requestDescribeConfigurationOptions =
  req
    "DescribeConfigurationOptions"
    "fixture/DescribeConfigurationOptions.yaml"

requestDescribeConfigurationSettings :: DescribeConfigurationSettings -> TestTree
requestDescribeConfigurationSettings =
  req
    "DescribeConfigurationSettings"
    "fixture/DescribeConfigurationSettings.yaml"

requestDescribeEnvironmentHealth :: DescribeEnvironmentHealth -> TestTree
requestDescribeEnvironmentHealth =
  req
    "DescribeEnvironmentHealth"
    "fixture/DescribeEnvironmentHealth.yaml"

requestDescribeEnvironmentManagedActionHistory :: DescribeEnvironmentManagedActionHistory -> TestTree
requestDescribeEnvironmentManagedActionHistory =
  req
    "DescribeEnvironmentManagedActionHistory"
    "fixture/DescribeEnvironmentManagedActionHistory.yaml"

requestDescribeEnvironmentManagedActions :: DescribeEnvironmentManagedActions -> TestTree
requestDescribeEnvironmentManagedActions =
  req
    "DescribeEnvironmentManagedActions"
    "fixture/DescribeEnvironmentManagedActions.yaml"

requestDescribeEnvironmentResources :: DescribeEnvironmentResources -> TestTree
requestDescribeEnvironmentResources =
  req
    "DescribeEnvironmentResources"
    "fixture/DescribeEnvironmentResources.yaml"

requestDescribeEnvironments :: DescribeEnvironments -> TestTree
requestDescribeEnvironments =
  req
    "DescribeEnvironments"
    "fixture/DescribeEnvironments.yaml"

requestDescribeEvents :: DescribeEvents -> TestTree
requestDescribeEvents =
  req
    "DescribeEvents"
    "fixture/DescribeEvents.yaml"

requestDescribeInstancesHealth :: DescribeInstancesHealth -> TestTree
requestDescribeInstancesHealth =
  req
    "DescribeInstancesHealth"
    "fixture/DescribeInstancesHealth.yaml"

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

requestListAvailableSolutionStacks :: ListAvailableSolutionStacks -> TestTree
requestListAvailableSolutionStacks =
  req
    "ListAvailableSolutionStacks"
    "fixture/ListAvailableSolutionStacks.yaml"

requestListPlatformBranches :: ListPlatformBranches -> TestTree
requestListPlatformBranches =
  req
    "ListPlatformBranches"
    "fixture/ListPlatformBranches.yaml"

requestListPlatformVersions :: ListPlatformVersions -> TestTree
requestListPlatformVersions =
  req
    "ListPlatformVersions"
    "fixture/ListPlatformVersions.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestRebuildEnvironment :: RebuildEnvironment -> TestTree
requestRebuildEnvironment =
  req
    "RebuildEnvironment"
    "fixture/RebuildEnvironment.yaml"

requestRequestEnvironmentInfo :: RequestEnvironmentInfo -> TestTree
requestRequestEnvironmentInfo =
  req
    "RequestEnvironmentInfo"
    "fixture/RequestEnvironmentInfo.yaml"

requestRestartAppServer :: RestartAppServer -> TestTree
requestRestartAppServer =
  req
    "RestartAppServer"
    "fixture/RestartAppServer.yaml"

requestRetrieveEnvironmentInfo :: RetrieveEnvironmentInfo -> TestTree
requestRetrieveEnvironmentInfo =
  req
    "RetrieveEnvironmentInfo"
    "fixture/RetrieveEnvironmentInfo.yaml"

requestSwapEnvironmentCNAMEs :: SwapEnvironmentCNAMEs -> TestTree
requestSwapEnvironmentCNAMEs =
  req
    "SwapEnvironmentCNAMEs"
    "fixture/SwapEnvironmentCNAMEs.yaml"

requestTerminateEnvironment :: TerminateEnvironment -> TestTree
requestTerminateEnvironment =
  req
    "TerminateEnvironment"
    "fixture/TerminateEnvironment.yaml"

requestUpdateApplication :: UpdateApplication -> TestTree
requestUpdateApplication =
  req
    "UpdateApplication"
    "fixture/UpdateApplication.yaml"

requestUpdateApplicationResourceLifecycle :: UpdateApplicationResourceLifecycle -> TestTree
requestUpdateApplicationResourceLifecycle =
  req
    "UpdateApplicationResourceLifecycle"
    "fixture/UpdateApplicationResourceLifecycle.yaml"

requestUpdateApplicationVersion :: UpdateApplicationVersion -> TestTree
requestUpdateApplicationVersion =
  req
    "UpdateApplicationVersion"
    "fixture/UpdateApplicationVersion.yaml"

requestUpdateConfigurationTemplate :: UpdateConfigurationTemplate -> TestTree
requestUpdateConfigurationTemplate =
  req
    "UpdateConfigurationTemplate"
    "fixture/UpdateConfigurationTemplate.yaml"

requestUpdateEnvironment :: UpdateEnvironment -> TestTree
requestUpdateEnvironment =
  req
    "UpdateEnvironment"
    "fixture/UpdateEnvironment.yaml"

requestUpdateTagsForResource :: UpdateTagsForResource -> TestTree
requestUpdateTagsForResource =
  req
    "UpdateTagsForResource"
    "fixture/UpdateTagsForResource.yaml"

requestValidateConfigurationSettings :: ValidateConfigurationSettings -> TestTree
requestValidateConfigurationSettings =
  req
    "ValidateConfigurationSettings"
    "fixture/ValidateConfigurationSettings.yaml"

-- Responses

responseAbortEnvironmentUpdate :: AbortEnvironmentUpdateResponse -> TestTree
responseAbortEnvironmentUpdate =
  res
    "AbortEnvironmentUpdateResponse"
    "fixture/AbortEnvironmentUpdateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AbortEnvironmentUpdate)

responseApplyEnvironmentManagedAction :: ApplyEnvironmentManagedActionResponse -> TestTree
responseApplyEnvironmentManagedAction =
  res
    "ApplyEnvironmentManagedActionResponse"
    "fixture/ApplyEnvironmentManagedActionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ApplyEnvironmentManagedAction)

responseAssociateEnvironmentOperationsRole :: AssociateEnvironmentOperationsRoleResponse -> TestTree
responseAssociateEnvironmentOperationsRole =
  res
    "AssociateEnvironmentOperationsRoleResponse"
    "fixture/AssociateEnvironmentOperationsRoleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateEnvironmentOperationsRole)

responseCheckDNSAvailability :: CheckDNSAvailabilityResponse -> TestTree
responseCheckDNSAvailability =
  res
    "CheckDNSAvailabilityResponse"
    "fixture/CheckDNSAvailabilityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CheckDNSAvailability)

responseComposeEnvironments :: EnvironmentDescriptionsMessage -> TestTree
responseComposeEnvironments =
  res
    "ComposeEnvironmentsResponse"
    "fixture/ComposeEnvironmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ComposeEnvironments)

responseCreateApplication :: ApplicationDescriptionMessage -> TestTree
responseCreateApplication =
  res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApplication)

responseCreateApplicationVersion :: ApplicationVersionDescriptionMessage -> TestTree
responseCreateApplicationVersion =
  res
    "CreateApplicationVersionResponse"
    "fixture/CreateApplicationVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApplicationVersion)

responseCreateConfigurationTemplate :: ConfigurationSettingsDescription -> TestTree
responseCreateConfigurationTemplate =
  res
    "CreateConfigurationTemplateResponse"
    "fixture/CreateConfigurationTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConfigurationTemplate)

responseCreateEnvironment :: EnvironmentDescription -> TestTree
responseCreateEnvironment =
  res
    "CreateEnvironmentResponse"
    "fixture/CreateEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEnvironment)

responseCreatePlatformVersion :: CreatePlatformVersionResponse -> TestTree
responseCreatePlatformVersion =
  res
    "CreatePlatformVersionResponse"
    "fixture/CreatePlatformVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePlatformVersion)

responseCreateStorageLocation :: CreateStorageLocationResponse -> TestTree
responseCreateStorageLocation =
  res
    "CreateStorageLocationResponse"
    "fixture/CreateStorageLocationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStorageLocation)

responseDeleteApplication :: DeleteApplicationResponse -> TestTree
responseDeleteApplication =
  res
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApplication)

responseDeleteApplicationVersion :: DeleteApplicationVersionResponse -> TestTree
responseDeleteApplicationVersion =
  res
    "DeleteApplicationVersionResponse"
    "fixture/DeleteApplicationVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApplicationVersion)

responseDeleteConfigurationTemplate :: DeleteConfigurationTemplateResponse -> TestTree
responseDeleteConfigurationTemplate =
  res
    "DeleteConfigurationTemplateResponse"
    "fixture/DeleteConfigurationTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConfigurationTemplate)

responseDeleteEnvironmentConfiguration :: DeleteEnvironmentConfigurationResponse -> TestTree
responseDeleteEnvironmentConfiguration =
  res
    "DeleteEnvironmentConfigurationResponse"
    "fixture/DeleteEnvironmentConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEnvironmentConfiguration)

responseDeletePlatformVersion :: DeletePlatformVersionResponse -> TestTree
responseDeletePlatformVersion =
  res
    "DeletePlatformVersionResponse"
    "fixture/DeletePlatformVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePlatformVersion)

responseDescribeAccountAttributes :: DescribeAccountAttributesResponse -> TestTree
responseDescribeAccountAttributes =
  res
    "DescribeAccountAttributesResponse"
    "fixture/DescribeAccountAttributesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccountAttributes)

responseDescribeApplicationVersions :: DescribeApplicationVersionsResponse -> TestTree
responseDescribeApplicationVersions =
  res
    "DescribeApplicationVersionsResponse"
    "fixture/DescribeApplicationVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeApplicationVersions)

responseDescribeApplications :: DescribeApplicationsResponse -> TestTree
responseDescribeApplications =
  res
    "DescribeApplicationsResponse"
    "fixture/DescribeApplicationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeApplications)

responseDescribeConfigurationOptions :: DescribeConfigurationOptionsResponse -> TestTree
responseDescribeConfigurationOptions =
  res
    "DescribeConfigurationOptionsResponse"
    "fixture/DescribeConfigurationOptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConfigurationOptions)

responseDescribeConfigurationSettings :: DescribeConfigurationSettingsResponse -> TestTree
responseDescribeConfigurationSettings =
  res
    "DescribeConfigurationSettingsResponse"
    "fixture/DescribeConfigurationSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeConfigurationSettings)

responseDescribeEnvironmentHealth :: DescribeEnvironmentHealthResponse -> TestTree
responseDescribeEnvironmentHealth =
  res
    "DescribeEnvironmentHealthResponse"
    "fixture/DescribeEnvironmentHealthResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEnvironmentHealth)

responseDescribeEnvironmentManagedActionHistory :: DescribeEnvironmentManagedActionHistoryResponse -> TestTree
responseDescribeEnvironmentManagedActionHistory =
  res
    "DescribeEnvironmentManagedActionHistoryResponse"
    "fixture/DescribeEnvironmentManagedActionHistoryResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEnvironmentManagedActionHistory)

responseDescribeEnvironmentManagedActions :: DescribeEnvironmentManagedActionsResponse -> TestTree
responseDescribeEnvironmentManagedActions =
  res
    "DescribeEnvironmentManagedActionsResponse"
    "fixture/DescribeEnvironmentManagedActionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEnvironmentManagedActions)

responseDescribeEnvironmentResources :: DescribeEnvironmentResourcesResponse -> TestTree
responseDescribeEnvironmentResources =
  res
    "DescribeEnvironmentResourcesResponse"
    "fixture/DescribeEnvironmentResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEnvironmentResources)

responseDescribeEnvironments :: EnvironmentDescriptionsMessage -> TestTree
responseDescribeEnvironments =
  res
    "DescribeEnvironmentsResponse"
    "fixture/DescribeEnvironmentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEnvironments)

responseDescribeEvents :: DescribeEventsResponse -> TestTree
responseDescribeEvents =
  res
    "DescribeEventsResponse"
    "fixture/DescribeEventsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEvents)

responseDescribeInstancesHealth :: DescribeInstancesHealthResponse -> TestTree
responseDescribeInstancesHealth =
  res
    "DescribeInstancesHealthResponse"
    "fixture/DescribeInstancesHealthResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInstancesHealth)

responseDescribePlatformVersion :: DescribePlatformVersionResponse -> TestTree
responseDescribePlatformVersion =
  res
    "DescribePlatformVersionResponse"
    "fixture/DescribePlatformVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePlatformVersion)

responseDisassociateEnvironmentOperationsRole :: DisassociateEnvironmentOperationsRoleResponse -> TestTree
responseDisassociateEnvironmentOperationsRole =
  res
    "DisassociateEnvironmentOperationsRoleResponse"
    "fixture/DisassociateEnvironmentOperationsRoleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateEnvironmentOperationsRole)

responseListAvailableSolutionStacks :: ListAvailableSolutionStacksResponse -> TestTree
responseListAvailableSolutionStacks =
  res
    "ListAvailableSolutionStacksResponse"
    "fixture/ListAvailableSolutionStacksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAvailableSolutionStacks)

responseListPlatformBranches :: ListPlatformBranchesResponse -> TestTree
responseListPlatformBranches =
  res
    "ListPlatformBranchesResponse"
    "fixture/ListPlatformBranchesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPlatformBranches)

responseListPlatformVersions :: ListPlatformVersionsResponse -> TestTree
responseListPlatformVersions =
  res
    "ListPlatformVersionsResponse"
    "fixture/ListPlatformVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPlatformVersions)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseRebuildEnvironment :: RebuildEnvironmentResponse -> TestTree
responseRebuildEnvironment =
  res
    "RebuildEnvironmentResponse"
    "fixture/RebuildEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RebuildEnvironment)

responseRequestEnvironmentInfo :: RequestEnvironmentInfoResponse -> TestTree
responseRequestEnvironmentInfo =
  res
    "RequestEnvironmentInfoResponse"
    "fixture/RequestEnvironmentInfoResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RequestEnvironmentInfo)

responseRestartAppServer :: RestartAppServerResponse -> TestTree
responseRestartAppServer =
  res
    "RestartAppServerResponse"
    "fixture/RestartAppServerResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RestartAppServer)

responseRetrieveEnvironmentInfo :: RetrieveEnvironmentInfoResponse -> TestTree
responseRetrieveEnvironmentInfo =
  res
    "RetrieveEnvironmentInfoResponse"
    "fixture/RetrieveEnvironmentInfoResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RetrieveEnvironmentInfo)

responseSwapEnvironmentCNAMEs :: SwapEnvironmentCNAMEsResponse -> TestTree
responseSwapEnvironmentCNAMEs =
  res
    "SwapEnvironmentCNAMEsResponse"
    "fixture/SwapEnvironmentCNAMEsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SwapEnvironmentCNAMEs)

responseTerminateEnvironment :: EnvironmentDescription -> TestTree
responseTerminateEnvironment =
  res
    "TerminateEnvironmentResponse"
    "fixture/TerminateEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TerminateEnvironment)

responseUpdateApplication :: ApplicationDescriptionMessage -> TestTree
responseUpdateApplication =
  res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApplication)

responseUpdateApplicationResourceLifecycle :: UpdateApplicationResourceLifecycleResponse -> TestTree
responseUpdateApplicationResourceLifecycle =
  res
    "UpdateApplicationResourceLifecycleResponse"
    "fixture/UpdateApplicationResourceLifecycleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApplicationResourceLifecycle)

responseUpdateApplicationVersion :: ApplicationVersionDescriptionMessage -> TestTree
responseUpdateApplicationVersion =
  res
    "UpdateApplicationVersionResponse"
    "fixture/UpdateApplicationVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApplicationVersion)

responseUpdateConfigurationTemplate :: ConfigurationSettingsDescription -> TestTree
responseUpdateConfigurationTemplate =
  res
    "UpdateConfigurationTemplateResponse"
    "fixture/UpdateConfigurationTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConfigurationTemplate)

responseUpdateEnvironment :: EnvironmentDescription -> TestTree
responseUpdateEnvironment =
  res
    "UpdateEnvironmentResponse"
    "fixture/UpdateEnvironmentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEnvironment)

responseUpdateTagsForResource :: UpdateTagsForResourceResponse -> TestTree
responseUpdateTagsForResource =
  res
    "UpdateTagsForResourceResponse"
    "fixture/UpdateTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateTagsForResource)

responseValidateConfigurationSettings :: ValidateConfigurationSettingsResponse -> TestTree
responseValidateConfigurationSettings =
  res
    "ValidateConfigurationSettingsResponse"
    "fixture/ValidateConfigurationSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ValidateConfigurationSettings)
