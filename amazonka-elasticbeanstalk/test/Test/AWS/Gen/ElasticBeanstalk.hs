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
--         [ requestListPlatformBranches $
--             newListPlatformBranches
--
--         , requestSwapEnvironmentCNAMEs $
--             newSwapEnvironmentCNAMEs
--
--         , requestListAvailableSolutionStacks $
--             newListAvailableSolutionStacks
--
--         , requestListPlatformVersions $
--             newListPlatformVersions
--
--         , requestDescribeApplications $
--             newDescribeApplications
--
--         , requestCreateApplicationVersion $
--             newCreateApplicationVersion
--
--         , requestDescribeEnvironmentHealth $
--             newDescribeEnvironmentHealth
--
--         , requestCreateConfigurationTemplate $
--             newCreateConfigurationTemplate
--
--         , requestTerminateEnvironment $
--             newTerminateEnvironment
--
--         , requestCreateEnvironment $
--             newCreateEnvironment
--
--         , requestCreatePlatformVersion $
--             newCreatePlatformVersion
--
--         , requestDescribeEnvironmentResources $
--             newDescribeEnvironmentResources
--
--         , requestUpdateApplicationVersion $
--             newUpdateApplicationVersion
--
--         , requestDeleteApplicationVersion $
--             newDeleteApplicationVersion
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
--         , requestDescribeAccountAttributes $
--             newDescribeAccountAttributes
--
--         , requestValidateConfigurationSettings $
--             newValidateConfigurationSettings
--
--         , requestDescribeEnvironmentManagedActions $
--             newDescribeEnvironmentManagedActions
--
--         , requestCreateStorageLocation $
--             newCreateStorageLocation
--
--         , requestDescribeConfigurationSettings $
--             newDescribeConfigurationSettings
--
--         , requestRetrieveEnvironmentInfo $
--             newRetrieveEnvironmentInfo
--
--         , requestDescribeConfigurationOptions $
--             newDescribeConfigurationOptions
--
--         , requestDescribeEvents $
--             newDescribeEvents
--
--         , requestRequestEnvironmentInfo $
--             newRequestEnvironmentInfo
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
--         , requestDeleteEnvironmentConfiguration $
--             newDeleteEnvironmentConfiguration
--
--         , requestDeletePlatformVersion $
--             newDeletePlatformVersion
--
--         , requestUpdateEnvironment $
--             newUpdateEnvironment
--
--         , requestUpdateTagsForResource $
--             newUpdateTagsForResource
--
--         , requestUpdateConfigurationTemplate $
--             newUpdateConfigurationTemplate
--
--         , requestDescribeEnvironmentManagedActionHistory $
--             newDescribeEnvironmentManagedActionHistory
--
--         , requestDeleteConfigurationTemplate $
--             newDeleteConfigurationTemplate
--
--         , requestDescribeApplicationVersions $
--             newDescribeApplicationVersions
--
--         , requestAbortEnvironmentUpdate $
--             newAbortEnvironmentUpdate
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
--         , requestRestartAppServer $
--             newRestartAppServer
--
--         , requestDescribeEnvironments $
--             newDescribeEnvironments
--
--         , requestAssociateEnvironmentOperationsRole $
--             newAssociateEnvironmentOperationsRole
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
--         [ responseListPlatformBranches $
--             newListPlatformBranchesResponse
--
--         , responseSwapEnvironmentCNAMEs $
--             newSwapEnvironmentCNAMEsResponse
--
--         , responseListAvailableSolutionStacks $
--             newListAvailableSolutionStacksResponse
--
--         , responseListPlatformVersions $
--             newListPlatformVersionsResponse
--
--         , responseDescribeApplications $
--             newDescribeApplicationsResponse
--
--         , responseCreateApplicationVersion $
--             newApplicationVersionDescriptionMessage
--
--         , responseDescribeEnvironmentHealth $
--             newDescribeEnvironmentHealthResponse
--
--         , responseCreateConfigurationTemplate $
--             newConfigurationSettingsDescription
--
--         , responseTerminateEnvironment $
--             newEnvironmentDescription
--
--         , responseCreateEnvironment $
--             newEnvironmentDescription
--
--         , responseCreatePlatformVersion $
--             newCreatePlatformVersionResponse
--
--         , responseDescribeEnvironmentResources $
--             newDescribeEnvironmentResourcesResponse
--
--         , responseUpdateApplicationVersion $
--             newApplicationVersionDescriptionMessage
--
--         , responseDeleteApplicationVersion $
--             newDeleteApplicationVersionResponse
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
--         , responseDescribeAccountAttributes $
--             newDescribeAccountAttributesResponse
--
--         , responseValidateConfigurationSettings $
--             newValidateConfigurationSettingsResponse
--
--         , responseDescribeEnvironmentManagedActions $
--             newDescribeEnvironmentManagedActionsResponse
--
--         , responseCreateStorageLocation $
--             newCreateStorageLocationResponse
--
--         , responseDescribeConfigurationSettings $
--             newDescribeConfigurationSettingsResponse
--
--         , responseRetrieveEnvironmentInfo $
--             newRetrieveEnvironmentInfoResponse
--
--         , responseDescribeConfigurationOptions $
--             newDescribeConfigurationOptionsResponse
--
--         , responseDescribeEvents $
--             newDescribeEventsResponse
--
--         , responseRequestEnvironmentInfo $
--             newRequestEnvironmentInfoResponse
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
--         , responseDeleteEnvironmentConfiguration $
--             newDeleteEnvironmentConfigurationResponse
--
--         , responseDeletePlatformVersion $
--             newDeletePlatformVersionResponse
--
--         , responseUpdateEnvironment $
--             newEnvironmentDescription
--
--         , responseUpdateTagsForResource $
--             newUpdateTagsForResourceResponse
--
--         , responseUpdateConfigurationTemplate $
--             newConfigurationSettingsDescription
--
--         , responseDescribeEnvironmentManagedActionHistory $
--             newDescribeEnvironmentManagedActionHistoryResponse
--
--         , responseDeleteConfigurationTemplate $
--             newDeleteConfigurationTemplateResponse
--
--         , responseDescribeApplicationVersions $
--             newDescribeApplicationVersionsResponse
--
--         , responseAbortEnvironmentUpdate $
--             newAbortEnvironmentUpdateResponse
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
--         , responseRestartAppServer $
--             newRestartAppServerResponse
--
--         , responseDescribeEnvironments $
--             newEnvironmentDescriptionsMessage
--
--         , responseAssociateEnvironmentOperationsRole $
--             newAssociateEnvironmentOperationsRoleResponse
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

requestListPlatformBranches :: ListPlatformBranches -> TestTree
requestListPlatformBranches =
  req
    "ListPlatformBranches"
    "fixture/ListPlatformBranches.yaml"

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

requestListPlatformVersions :: ListPlatformVersions -> TestTree
requestListPlatformVersions =
  req
    "ListPlatformVersions"
    "fixture/ListPlatformVersions.yaml"

requestDescribeApplications :: DescribeApplications -> TestTree
requestDescribeApplications =
  req
    "DescribeApplications"
    "fixture/DescribeApplications.yaml"

requestCreateApplicationVersion :: CreateApplicationVersion -> TestTree
requestCreateApplicationVersion =
  req
    "CreateApplicationVersion"
    "fixture/CreateApplicationVersion.yaml"

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

requestTerminateEnvironment :: TerminateEnvironment -> TestTree
requestTerminateEnvironment =
  req
    "TerminateEnvironment"
    "fixture/TerminateEnvironment.yaml"

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

requestDeleteApplicationVersion :: DeleteApplicationVersion -> TestTree
requestDeleteApplicationVersion =
  req
    "DeleteApplicationVersion"
    "fixture/DeleteApplicationVersion.yaml"

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

requestDescribeEnvironmentManagedActions :: DescribeEnvironmentManagedActions -> TestTree
requestDescribeEnvironmentManagedActions =
  req
    "DescribeEnvironmentManagedActions"
    "fixture/DescribeEnvironmentManagedActions.yaml"

requestCreateStorageLocation :: CreateStorageLocation -> TestTree
requestCreateStorageLocation =
  req
    "CreateStorageLocation"
    "fixture/CreateStorageLocation.yaml"

requestDescribeConfigurationSettings :: DescribeConfigurationSettings -> TestTree
requestDescribeConfigurationSettings =
  req
    "DescribeConfigurationSettings"
    "fixture/DescribeConfigurationSettings.yaml"

requestRetrieveEnvironmentInfo :: RetrieveEnvironmentInfo -> TestTree
requestRetrieveEnvironmentInfo =
  req
    "RetrieveEnvironmentInfo"
    "fixture/RetrieveEnvironmentInfo.yaml"

requestDescribeConfigurationOptions :: DescribeConfigurationOptions -> TestTree
requestDescribeConfigurationOptions =
  req
    "DescribeConfigurationOptions"
    "fixture/DescribeConfigurationOptions.yaml"

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

requestUpdateConfigurationTemplate :: UpdateConfigurationTemplate -> TestTree
requestUpdateConfigurationTemplate =
  req
    "UpdateConfigurationTemplate"
    "fixture/UpdateConfigurationTemplate.yaml"

requestDescribeEnvironmentManagedActionHistory :: DescribeEnvironmentManagedActionHistory -> TestTree
requestDescribeEnvironmentManagedActionHistory =
  req
    "DescribeEnvironmentManagedActionHistory"
    "fixture/DescribeEnvironmentManagedActionHistory.yaml"

requestDeleteConfigurationTemplate :: DeleteConfigurationTemplate -> TestTree
requestDeleteConfigurationTemplate =
  req
    "DeleteConfigurationTemplate"
    "fixture/DeleteConfigurationTemplate.yaml"

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

requestAssociateEnvironmentOperationsRole :: AssociateEnvironmentOperationsRole -> TestTree
requestAssociateEnvironmentOperationsRole =
  req
    "AssociateEnvironmentOperationsRole"
    "fixture/AssociateEnvironmentOperationsRole.yaml"

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

responseListPlatformBranches :: ListPlatformBranchesResponse -> TestTree
responseListPlatformBranches =
  res
    "ListPlatformBranchesResponse"
    "fixture/ListPlatformBranchesResponse.proto"
    defaultService
    (Proxy :: Proxy ListPlatformBranches)

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

responseListPlatformVersions :: ListPlatformVersionsResponse -> TestTree
responseListPlatformVersions =
  res
    "ListPlatformVersionsResponse"
    "fixture/ListPlatformVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPlatformVersions)

responseDescribeApplications :: DescribeApplicationsResponse -> TestTree
responseDescribeApplications =
  res
    "DescribeApplicationsResponse"
    "fixture/DescribeApplicationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeApplications)

responseCreateApplicationVersion :: ApplicationVersionDescriptionMessage -> TestTree
responseCreateApplicationVersion =
  res
    "CreateApplicationVersionResponse"
    "fixture/CreateApplicationVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateApplicationVersion)

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

responseTerminateEnvironment :: EnvironmentDescription -> TestTree
responseTerminateEnvironment =
  res
    "TerminateEnvironmentResponse"
    "fixture/TerminateEnvironmentResponse.proto"
    defaultService
    (Proxy :: Proxy TerminateEnvironment)

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

responseDeleteApplicationVersion :: DeleteApplicationVersionResponse -> TestTree
responseDeleteApplicationVersion =
  res
    "DeleteApplicationVersionResponse"
    "fixture/DeleteApplicationVersionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteApplicationVersion)

responseCheckDNSAvailability :: CheckDNSAvailabilityResponse -> TestTree
responseCheckDNSAvailability =
  res
    "CheckDNSAvailabilityResponse"
    "fixture/CheckDNSAvailabilityResponse.proto"
    defaultService
    (Proxy :: Proxy CheckDNSAvailability)

responseComposeEnvironments :: EnvironmentDescriptionsMessage -> TestTree
responseComposeEnvironments =
  res
    "ComposeEnvironmentsResponse"
    "fixture/ComposeEnvironmentsResponse.proto"
    defaultService
    (Proxy :: Proxy ComposeEnvironments)

responseCreateApplication :: ApplicationDescriptionMessage -> TestTree
responseCreateApplication =
  res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateApplication)

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

responseDescribeEnvironmentManagedActions :: DescribeEnvironmentManagedActionsResponse -> TestTree
responseDescribeEnvironmentManagedActions =
  res
    "DescribeEnvironmentManagedActionsResponse"
    "fixture/DescribeEnvironmentManagedActionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEnvironmentManagedActions)

responseCreateStorageLocation :: CreateStorageLocationResponse -> TestTree
responseCreateStorageLocation =
  res
    "CreateStorageLocationResponse"
    "fixture/CreateStorageLocationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateStorageLocation)

responseDescribeConfigurationSettings :: DescribeConfigurationSettingsResponse -> TestTree
responseDescribeConfigurationSettings =
  res
    "DescribeConfigurationSettingsResponse"
    "fixture/DescribeConfigurationSettingsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConfigurationSettings)

responseRetrieveEnvironmentInfo :: RetrieveEnvironmentInfoResponse -> TestTree
responseRetrieveEnvironmentInfo =
  res
    "RetrieveEnvironmentInfoResponse"
    "fixture/RetrieveEnvironmentInfoResponse.proto"
    defaultService
    (Proxy :: Proxy RetrieveEnvironmentInfo)

responseDescribeConfigurationOptions :: DescribeConfigurationOptionsResponse -> TestTree
responseDescribeConfigurationOptions =
  res
    "DescribeConfigurationOptionsResponse"
    "fixture/DescribeConfigurationOptionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeConfigurationOptions)

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

responseDeleteEnvironmentConfiguration :: DeleteEnvironmentConfigurationResponse -> TestTree
responseDeleteEnvironmentConfiguration =
  res
    "DeleteEnvironmentConfigurationResponse"
    "fixture/DeleteEnvironmentConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteEnvironmentConfiguration)

responseDeletePlatformVersion :: DeletePlatformVersionResponse -> TestTree
responseDeletePlatformVersion =
  res
    "DeletePlatformVersionResponse"
    "fixture/DeletePlatformVersionResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePlatformVersion)

responseUpdateEnvironment :: EnvironmentDescription -> TestTree
responseUpdateEnvironment =
  res
    "UpdateEnvironmentResponse"
    "fixture/UpdateEnvironmentResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateEnvironment)

responseUpdateTagsForResource :: UpdateTagsForResourceResponse -> TestTree
responseUpdateTagsForResource =
  res
    "UpdateTagsForResourceResponse"
    "fixture/UpdateTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateTagsForResource)

responseUpdateConfigurationTemplate :: ConfigurationSettingsDescription -> TestTree
responseUpdateConfigurationTemplate =
  res
    "UpdateConfigurationTemplateResponse"
    "fixture/UpdateConfigurationTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateConfigurationTemplate)

responseDescribeEnvironmentManagedActionHistory :: DescribeEnvironmentManagedActionHistoryResponse -> TestTree
responseDescribeEnvironmentManagedActionHistory =
  res
    "DescribeEnvironmentManagedActionHistoryResponse"
    "fixture/DescribeEnvironmentManagedActionHistoryResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEnvironmentManagedActionHistory)

responseDeleteConfigurationTemplate :: DeleteConfigurationTemplateResponse -> TestTree
responseDeleteConfigurationTemplate =
  res
    "DeleteConfigurationTemplateResponse"
    "fixture/DeleteConfigurationTemplateResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConfigurationTemplate)

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

responseAssociateEnvironmentOperationsRole :: AssociateEnvironmentOperationsRoleResponse -> TestTree
responseAssociateEnvironmentOperationsRole =
  res
    "AssociateEnvironmentOperationsRoleResponse"
    "fixture/AssociateEnvironmentOperationsRoleResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateEnvironmentOperationsRole)

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
