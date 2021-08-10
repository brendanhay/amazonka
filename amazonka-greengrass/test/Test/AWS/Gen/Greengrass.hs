{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Greengrass
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Greengrass where

import Data.Proxy
import Network.AWS.Greengrass
import Test.AWS.Fixture
import Test.AWS.Greengrass.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDeleteSubscriptionDefinition $
--             newDeleteSubscriptionDefinition
--
--         , requestUpdateCoreDefinition $
--             newUpdateCoreDefinition
--
--         , requestUpdateSubscriptionDefinition $
--             newUpdateSubscriptionDefinition
--
--         , requestAssociateServiceRoleToAccount $
--             newAssociateServiceRoleToAccount
--
--         , requestDeleteCoreDefinition $
--             newDeleteCoreDefinition
--
--         , requestAssociateRoleToGroup $
--             newAssociateRoleToGroup
--
--         , requestGetGroupCertificateConfiguration $
--             newGetGroupCertificateConfiguration
--
--         , requestListFunctionDefinitionVersions $
--             newListFunctionDefinitionVersions
--
--         , requestStartBulkDeployment $
--             newStartBulkDeployment
--
--         , requestCreateFunctionDefinitionVersion $
--             newCreateFunctionDefinitionVersion
--
--         , requestGetFunctionDefinition $
--             newGetFunctionDefinition
--
--         , requestUpdateThingRuntimeConfiguration $
--             newUpdateThingRuntimeConfiguration
--
--         , requestStopBulkDeployment $
--             newStopBulkDeployment
--
--         , requestListGroups $
--             newListGroups
--
--         , requestDeleteResourceDefinition $
--             newDeleteResourceDefinition
--
--         , requestGetThingRuntimeConfiguration $
--             newGetThingRuntimeConfiguration
--
--         , requestListDeviceDefinitionVersions $
--             newListDeviceDefinitionVersions
--
--         , requestUpdateResourceDefinition $
--             newUpdateResourceDefinition
--
--         , requestListResourceDefinitions $
--             newListResourceDefinitions
--
--         , requestGetDeviceDefinition $
--             newGetDeviceDefinition
--
--         , requestCreateResourceDefinition $
--             newCreateResourceDefinition
--
--         , requestGetResourceDefinitionVersion $
--             newGetResourceDefinitionVersion
--
--         , requestCreateDeviceDefinitionVersion $
--             newCreateDeviceDefinitionVersion
--
--         , requestGetGroupVersion $
--             newGetGroupVersion
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestCreateGroup $
--             newCreateGroup
--
--         , requestUpdateGroupCertificateConfiguration $
--             newUpdateGroupCertificateConfiguration
--
--         , requestDeleteLoggerDefinition $
--             newDeleteLoggerDefinition
--
--         , requestListBulkDeployments $
--             newListBulkDeployments
--
--         , requestCreateConnectorDefinitionVersion $
--             newCreateConnectorDefinitionVersion
--
--         , requestCreateSubscriptionDefinitionVersion $
--             newCreateSubscriptionDefinitionVersion
--
--         , requestUpdateLoggerDefinition $
--             newUpdateLoggerDefinition
--
--         , requestListLoggerDefinitions $
--             newListLoggerDefinitions
--
--         , requestCreateCoreDefinitionVersion $
--             newCreateCoreDefinitionVersion
--
--         , requestListDeployments $
--             newListDeployments
--
--         , requestTagResource $
--             newTagResource
--
--         , requestGetConnectivityInfo $
--             newGetConnectivityInfo
--
--         , requestListSubscriptionDefinitionVersions $
--             newListSubscriptionDefinitionVersions
--
--         , requestListCoreDefinitionVersions $
--             newListCoreDefinitionVersions
--
--         , requestListConnectorDefinitionVersions $
--             newListConnectorDefinitionVersions
--
--         , requestCreateSoftwareUpdateJob $
--             newCreateSoftwareUpdateJob
--
--         , requestCreateCoreDefinition $
--             newCreateCoreDefinition
--
--         , requestCreateConnectorDefinition $
--             newCreateConnectorDefinition
--
--         , requestGetAssociatedRole $
--             newGetAssociatedRole
--
--         , requestUpdateConnectivityInfo $
--             newUpdateConnectivityInfo
--
--         , requestCreateSubscriptionDefinition $
--             newCreateSubscriptionDefinition
--
--         , requestDisassociateRoleFromGroup $
--             newDisassociateRoleFromGroup
--
--         , requestListCoreDefinitions $
--             newListCoreDefinitions
--
--         , requestListConnectorDefinitions $
--             newListConnectorDefinitions
--
--         , requestCreateGroupCertificateAuthority $
--             newCreateGroupCertificateAuthority
--
--         , requestListGroupCertificateAuthorities $
--             newListGroupCertificateAuthorities
--
--         , requestDeleteConnectorDefinition $
--             newDeleteConnectorDefinition
--
--         , requestGetLoggerDefinition $
--             newGetLoggerDefinition
--
--         , requestUpdateConnectorDefinition $
--             newUpdateConnectorDefinition
--
--         , requestCreateLoggerDefinitionVersion $
--             newCreateLoggerDefinitionVersion
--
--         , requestResetDeployments $
--             newResetDeployments
--
--         , requestListSubscriptionDefinitions $
--             newListSubscriptionDefinitions
--
--         , requestListGroupVersions $
--             newListGroupVersions
--
--         , requestDeleteDeviceDefinition $
--             newDeleteDeviceDefinition
--
--         , requestUpdateDeviceDefinition $
--             newUpdateDeviceDefinition
--
--         , requestListDeviceDefinitions $
--             newListDeviceDefinitions
--
--         , requestDisassociateServiceRoleFromAccount $
--             newDisassociateServiceRoleFromAccount
--
--         , requestListResourceDefinitionVersions $
--             newListResourceDefinitionVersions
--
--         , requestGetResourceDefinition $
--             newGetResourceDefinition
--
--         , requestGetGroup $
--             newGetGroup
--
--         , requestCreateDeviceDefinition $
--             newCreateDeviceDefinition
--
--         , requestGetDeviceDefinitionVersion $
--             newGetDeviceDefinitionVersion
--
--         , requestCreateResourceDefinitionVersion $
--             newCreateResourceDefinitionVersion
--
--         , requestCreateGroupVersion $
--             newCreateGroupVersion
--
--         , requestListBulkDeploymentDetailedReports $
--             newListBulkDeploymentDetailedReports
--
--         , requestUpdateGroup $
--             newUpdateGroup
--
--         , requestDeleteGroup $
--             newDeleteGroup
--
--         , requestDeleteFunctionDefinition $
--             newDeleteFunctionDefinition
--
--         , requestUpdateFunctionDefinition $
--             newUpdateFunctionDefinition
--
--         , requestListFunctionDefinitions $
--             newListFunctionDefinitions
--
--         , requestGetFunctionDefinitionVersion $
--             newGetFunctionDefinitionVersion
--
--         , requestGetBulkDeploymentStatus $
--             newGetBulkDeploymentStatus
--
--         , requestGetDeploymentStatus $
--             newGetDeploymentStatus
--
--         , requestCreateFunctionDefinition $
--             newCreateFunctionDefinition
--
--         , requestGetSubscriptionDefinition $
--             newGetSubscriptionDefinition
--
--         , requestGetConnectorDefinition $
--             newGetConnectorDefinition
--
--         , requestGetCoreDefinition $
--             newGetCoreDefinition
--
--         , requestGetGroupCertificateAuthority $
--             newGetGroupCertificateAuthority
--
--         , requestGetServiceRoleForAccount $
--             newGetServiceRoleForAccount
--
--         , requestGetLoggerDefinitionVersion $
--             newGetLoggerDefinitionVersion
--
--         , requestCreateLoggerDefinition $
--             newCreateLoggerDefinition
--
--         , requestCreateDeployment $
--             newCreateDeployment
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestGetSubscriptionDefinitionVersion $
--             newGetSubscriptionDefinitionVersion
--
--         , requestListLoggerDefinitionVersions $
--             newListLoggerDefinitionVersions
--
--         , requestGetCoreDefinitionVersion $
--             newGetCoreDefinitionVersion
--
--         , requestGetConnectorDefinitionVersion $
--             newGetConnectorDefinitionVersion
--
--           ]

--     , testGroup "response"
--         [ responseDeleteSubscriptionDefinition $
--             newDeleteSubscriptionDefinitionResponse
--
--         , responseUpdateCoreDefinition $
--             newUpdateCoreDefinitionResponse
--
--         , responseUpdateSubscriptionDefinition $
--             newUpdateSubscriptionDefinitionResponse
--
--         , responseAssociateServiceRoleToAccount $
--             newAssociateServiceRoleToAccountResponse
--
--         , responseDeleteCoreDefinition $
--             newDeleteCoreDefinitionResponse
--
--         , responseAssociateRoleToGroup $
--             newAssociateRoleToGroupResponse
--
--         , responseGetGroupCertificateConfiguration $
--             newGetGroupCertificateConfigurationResponse
--
--         , responseListFunctionDefinitionVersions $
--             newListFunctionDefinitionVersionsResponse
--
--         , responseStartBulkDeployment $
--             newStartBulkDeploymentResponse
--
--         , responseCreateFunctionDefinitionVersion $
--             newCreateFunctionDefinitionVersionResponse
--
--         , responseGetFunctionDefinition $
--             newGetFunctionDefinitionResponse
--
--         , responseUpdateThingRuntimeConfiguration $
--             newUpdateThingRuntimeConfigurationResponse
--
--         , responseStopBulkDeployment $
--             newStopBulkDeploymentResponse
--
--         , responseListGroups $
--             newListGroupsResponse
--
--         , responseDeleteResourceDefinition $
--             newDeleteResourceDefinitionResponse
--
--         , responseGetThingRuntimeConfiguration $
--             newGetThingRuntimeConfigurationResponse
--
--         , responseListDeviceDefinitionVersions $
--             newListDeviceDefinitionVersionsResponse
--
--         , responseUpdateResourceDefinition $
--             newUpdateResourceDefinitionResponse
--
--         , responseListResourceDefinitions $
--             newListResourceDefinitionsResponse
--
--         , responseGetDeviceDefinition $
--             newGetDeviceDefinitionResponse
--
--         , responseCreateResourceDefinition $
--             newCreateResourceDefinitionResponse
--
--         , responseGetResourceDefinitionVersion $
--             newGetResourceDefinitionVersionResponse
--
--         , responseCreateDeviceDefinitionVersion $
--             newCreateDeviceDefinitionVersionResponse
--
--         , responseGetGroupVersion $
--             newGetGroupVersionResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseCreateGroup $
--             newCreateGroupResponse
--
--         , responseUpdateGroupCertificateConfiguration $
--             newUpdateGroupCertificateConfigurationResponse
--
--         , responseDeleteLoggerDefinition $
--             newDeleteLoggerDefinitionResponse
--
--         , responseListBulkDeployments $
--             newListBulkDeploymentsResponse
--
--         , responseCreateConnectorDefinitionVersion $
--             newCreateConnectorDefinitionVersionResponse
--
--         , responseCreateSubscriptionDefinitionVersion $
--             newCreateSubscriptionDefinitionVersionResponse
--
--         , responseUpdateLoggerDefinition $
--             newUpdateLoggerDefinitionResponse
--
--         , responseListLoggerDefinitions $
--             newListLoggerDefinitionsResponse
--
--         , responseCreateCoreDefinitionVersion $
--             newCreateCoreDefinitionVersionResponse
--
--         , responseListDeployments $
--             newListDeploymentsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseGetConnectivityInfo $
--             newGetConnectivityInfoResponse
--
--         , responseListSubscriptionDefinitionVersions $
--             newListSubscriptionDefinitionVersionsResponse
--
--         , responseListCoreDefinitionVersions $
--             newListCoreDefinitionVersionsResponse
--
--         , responseListConnectorDefinitionVersions $
--             newListConnectorDefinitionVersionsResponse
--
--         , responseCreateSoftwareUpdateJob $
--             newCreateSoftwareUpdateJobResponse
--
--         , responseCreateCoreDefinition $
--             newCreateCoreDefinitionResponse
--
--         , responseCreateConnectorDefinition $
--             newCreateConnectorDefinitionResponse
--
--         , responseGetAssociatedRole $
--             newGetAssociatedRoleResponse
--
--         , responseUpdateConnectivityInfo $
--             newUpdateConnectivityInfoResponse
--
--         , responseCreateSubscriptionDefinition $
--             newCreateSubscriptionDefinitionResponse
--
--         , responseDisassociateRoleFromGroup $
--             newDisassociateRoleFromGroupResponse
--
--         , responseListCoreDefinitions $
--             newListCoreDefinitionsResponse
--
--         , responseListConnectorDefinitions $
--             newListConnectorDefinitionsResponse
--
--         , responseCreateGroupCertificateAuthority $
--             newCreateGroupCertificateAuthorityResponse
--
--         , responseListGroupCertificateAuthorities $
--             newListGroupCertificateAuthoritiesResponse
--
--         , responseDeleteConnectorDefinition $
--             newDeleteConnectorDefinitionResponse
--
--         , responseGetLoggerDefinition $
--             newGetLoggerDefinitionResponse
--
--         , responseUpdateConnectorDefinition $
--             newUpdateConnectorDefinitionResponse
--
--         , responseCreateLoggerDefinitionVersion $
--             newCreateLoggerDefinitionVersionResponse
--
--         , responseResetDeployments $
--             newResetDeploymentsResponse
--
--         , responseListSubscriptionDefinitions $
--             newListSubscriptionDefinitionsResponse
--
--         , responseListGroupVersions $
--             newListGroupVersionsResponse
--
--         , responseDeleteDeviceDefinition $
--             newDeleteDeviceDefinitionResponse
--
--         , responseUpdateDeviceDefinition $
--             newUpdateDeviceDefinitionResponse
--
--         , responseListDeviceDefinitions $
--             newListDeviceDefinitionsResponse
--
--         , responseDisassociateServiceRoleFromAccount $
--             newDisassociateServiceRoleFromAccountResponse
--
--         , responseListResourceDefinitionVersions $
--             newListResourceDefinitionVersionsResponse
--
--         , responseGetResourceDefinition $
--             newGetResourceDefinitionResponse
--
--         , responseGetGroup $
--             newGetGroupResponse
--
--         , responseCreateDeviceDefinition $
--             newCreateDeviceDefinitionResponse
--
--         , responseGetDeviceDefinitionVersion $
--             newGetDeviceDefinitionVersionResponse
--
--         , responseCreateResourceDefinitionVersion $
--             newCreateResourceDefinitionVersionResponse
--
--         , responseCreateGroupVersion $
--             newCreateGroupVersionResponse
--
--         , responseListBulkDeploymentDetailedReports $
--             newListBulkDeploymentDetailedReportsResponse
--
--         , responseUpdateGroup $
--             newUpdateGroupResponse
--
--         , responseDeleteGroup $
--             newDeleteGroupResponse
--
--         , responseDeleteFunctionDefinition $
--             newDeleteFunctionDefinitionResponse
--
--         , responseUpdateFunctionDefinition $
--             newUpdateFunctionDefinitionResponse
--
--         , responseListFunctionDefinitions $
--             newListFunctionDefinitionsResponse
--
--         , responseGetFunctionDefinitionVersion $
--             newGetFunctionDefinitionVersionResponse
--
--         , responseGetBulkDeploymentStatus $
--             newGetBulkDeploymentStatusResponse
--
--         , responseGetDeploymentStatus $
--             newGetDeploymentStatusResponse
--
--         , responseCreateFunctionDefinition $
--             newCreateFunctionDefinitionResponse
--
--         , responseGetSubscriptionDefinition $
--             newGetSubscriptionDefinitionResponse
--
--         , responseGetConnectorDefinition $
--             newGetConnectorDefinitionResponse
--
--         , responseGetCoreDefinition $
--             newGetCoreDefinitionResponse
--
--         , responseGetGroupCertificateAuthority $
--             newGetGroupCertificateAuthorityResponse
--
--         , responseGetServiceRoleForAccount $
--             newGetServiceRoleForAccountResponse
--
--         , responseGetLoggerDefinitionVersion $
--             newGetLoggerDefinitionVersionResponse
--
--         , responseCreateLoggerDefinition $
--             newCreateLoggerDefinitionResponse
--
--         , responseCreateDeployment $
--             newCreateDeploymentResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseGetSubscriptionDefinitionVersion $
--             newGetSubscriptionDefinitionVersionResponse
--
--         , responseListLoggerDefinitionVersions $
--             newListLoggerDefinitionVersionsResponse
--
--         , responseGetCoreDefinitionVersion $
--             newGetCoreDefinitionVersionResponse
--
--         , responseGetConnectorDefinitionVersion $
--             newGetConnectorDefinitionVersionResponse
--
--           ]
--     ]

-- Requests

requestDeleteSubscriptionDefinition :: DeleteSubscriptionDefinition -> TestTree
requestDeleteSubscriptionDefinition =
  req
    "DeleteSubscriptionDefinition"
    "fixture/DeleteSubscriptionDefinition.yaml"

requestUpdateCoreDefinition :: UpdateCoreDefinition -> TestTree
requestUpdateCoreDefinition =
  req
    "UpdateCoreDefinition"
    "fixture/UpdateCoreDefinition.yaml"

requestUpdateSubscriptionDefinition :: UpdateSubscriptionDefinition -> TestTree
requestUpdateSubscriptionDefinition =
  req
    "UpdateSubscriptionDefinition"
    "fixture/UpdateSubscriptionDefinition.yaml"

requestAssociateServiceRoleToAccount :: AssociateServiceRoleToAccount -> TestTree
requestAssociateServiceRoleToAccount =
  req
    "AssociateServiceRoleToAccount"
    "fixture/AssociateServiceRoleToAccount.yaml"

requestDeleteCoreDefinition :: DeleteCoreDefinition -> TestTree
requestDeleteCoreDefinition =
  req
    "DeleteCoreDefinition"
    "fixture/DeleteCoreDefinition.yaml"

requestAssociateRoleToGroup :: AssociateRoleToGroup -> TestTree
requestAssociateRoleToGroup =
  req
    "AssociateRoleToGroup"
    "fixture/AssociateRoleToGroup.yaml"

requestGetGroupCertificateConfiguration :: GetGroupCertificateConfiguration -> TestTree
requestGetGroupCertificateConfiguration =
  req
    "GetGroupCertificateConfiguration"
    "fixture/GetGroupCertificateConfiguration.yaml"

requestListFunctionDefinitionVersions :: ListFunctionDefinitionVersions -> TestTree
requestListFunctionDefinitionVersions =
  req
    "ListFunctionDefinitionVersions"
    "fixture/ListFunctionDefinitionVersions.yaml"

requestStartBulkDeployment :: StartBulkDeployment -> TestTree
requestStartBulkDeployment =
  req
    "StartBulkDeployment"
    "fixture/StartBulkDeployment.yaml"

requestCreateFunctionDefinitionVersion :: CreateFunctionDefinitionVersion -> TestTree
requestCreateFunctionDefinitionVersion =
  req
    "CreateFunctionDefinitionVersion"
    "fixture/CreateFunctionDefinitionVersion.yaml"

requestGetFunctionDefinition :: GetFunctionDefinition -> TestTree
requestGetFunctionDefinition =
  req
    "GetFunctionDefinition"
    "fixture/GetFunctionDefinition.yaml"

requestUpdateThingRuntimeConfiguration :: UpdateThingRuntimeConfiguration -> TestTree
requestUpdateThingRuntimeConfiguration =
  req
    "UpdateThingRuntimeConfiguration"
    "fixture/UpdateThingRuntimeConfiguration.yaml"

requestStopBulkDeployment :: StopBulkDeployment -> TestTree
requestStopBulkDeployment =
  req
    "StopBulkDeployment"
    "fixture/StopBulkDeployment.yaml"

requestListGroups :: ListGroups -> TestTree
requestListGroups =
  req
    "ListGroups"
    "fixture/ListGroups.yaml"

requestDeleteResourceDefinition :: DeleteResourceDefinition -> TestTree
requestDeleteResourceDefinition =
  req
    "DeleteResourceDefinition"
    "fixture/DeleteResourceDefinition.yaml"

requestGetThingRuntimeConfiguration :: GetThingRuntimeConfiguration -> TestTree
requestGetThingRuntimeConfiguration =
  req
    "GetThingRuntimeConfiguration"
    "fixture/GetThingRuntimeConfiguration.yaml"

requestListDeviceDefinitionVersions :: ListDeviceDefinitionVersions -> TestTree
requestListDeviceDefinitionVersions =
  req
    "ListDeviceDefinitionVersions"
    "fixture/ListDeviceDefinitionVersions.yaml"

requestUpdateResourceDefinition :: UpdateResourceDefinition -> TestTree
requestUpdateResourceDefinition =
  req
    "UpdateResourceDefinition"
    "fixture/UpdateResourceDefinition.yaml"

requestListResourceDefinitions :: ListResourceDefinitions -> TestTree
requestListResourceDefinitions =
  req
    "ListResourceDefinitions"
    "fixture/ListResourceDefinitions.yaml"

requestGetDeviceDefinition :: GetDeviceDefinition -> TestTree
requestGetDeviceDefinition =
  req
    "GetDeviceDefinition"
    "fixture/GetDeviceDefinition.yaml"

requestCreateResourceDefinition :: CreateResourceDefinition -> TestTree
requestCreateResourceDefinition =
  req
    "CreateResourceDefinition"
    "fixture/CreateResourceDefinition.yaml"

requestGetResourceDefinitionVersion :: GetResourceDefinitionVersion -> TestTree
requestGetResourceDefinitionVersion =
  req
    "GetResourceDefinitionVersion"
    "fixture/GetResourceDefinitionVersion.yaml"

requestCreateDeviceDefinitionVersion :: CreateDeviceDefinitionVersion -> TestTree
requestCreateDeviceDefinitionVersion =
  req
    "CreateDeviceDefinitionVersion"
    "fixture/CreateDeviceDefinitionVersion.yaml"

requestGetGroupVersion :: GetGroupVersion -> TestTree
requestGetGroupVersion =
  req
    "GetGroupVersion"
    "fixture/GetGroupVersion.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestCreateGroup :: CreateGroup -> TestTree
requestCreateGroup =
  req
    "CreateGroup"
    "fixture/CreateGroup.yaml"

requestUpdateGroupCertificateConfiguration :: UpdateGroupCertificateConfiguration -> TestTree
requestUpdateGroupCertificateConfiguration =
  req
    "UpdateGroupCertificateConfiguration"
    "fixture/UpdateGroupCertificateConfiguration.yaml"

requestDeleteLoggerDefinition :: DeleteLoggerDefinition -> TestTree
requestDeleteLoggerDefinition =
  req
    "DeleteLoggerDefinition"
    "fixture/DeleteLoggerDefinition.yaml"

requestListBulkDeployments :: ListBulkDeployments -> TestTree
requestListBulkDeployments =
  req
    "ListBulkDeployments"
    "fixture/ListBulkDeployments.yaml"

requestCreateConnectorDefinitionVersion :: CreateConnectorDefinitionVersion -> TestTree
requestCreateConnectorDefinitionVersion =
  req
    "CreateConnectorDefinitionVersion"
    "fixture/CreateConnectorDefinitionVersion.yaml"

requestCreateSubscriptionDefinitionVersion :: CreateSubscriptionDefinitionVersion -> TestTree
requestCreateSubscriptionDefinitionVersion =
  req
    "CreateSubscriptionDefinitionVersion"
    "fixture/CreateSubscriptionDefinitionVersion.yaml"

requestUpdateLoggerDefinition :: UpdateLoggerDefinition -> TestTree
requestUpdateLoggerDefinition =
  req
    "UpdateLoggerDefinition"
    "fixture/UpdateLoggerDefinition.yaml"

requestListLoggerDefinitions :: ListLoggerDefinitions -> TestTree
requestListLoggerDefinitions =
  req
    "ListLoggerDefinitions"
    "fixture/ListLoggerDefinitions.yaml"

requestCreateCoreDefinitionVersion :: CreateCoreDefinitionVersion -> TestTree
requestCreateCoreDefinitionVersion =
  req
    "CreateCoreDefinitionVersion"
    "fixture/CreateCoreDefinitionVersion.yaml"

requestListDeployments :: ListDeployments -> TestTree
requestListDeployments =
  req
    "ListDeployments"
    "fixture/ListDeployments.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestGetConnectivityInfo :: GetConnectivityInfo -> TestTree
requestGetConnectivityInfo =
  req
    "GetConnectivityInfo"
    "fixture/GetConnectivityInfo.yaml"

requestListSubscriptionDefinitionVersions :: ListSubscriptionDefinitionVersions -> TestTree
requestListSubscriptionDefinitionVersions =
  req
    "ListSubscriptionDefinitionVersions"
    "fixture/ListSubscriptionDefinitionVersions.yaml"

requestListCoreDefinitionVersions :: ListCoreDefinitionVersions -> TestTree
requestListCoreDefinitionVersions =
  req
    "ListCoreDefinitionVersions"
    "fixture/ListCoreDefinitionVersions.yaml"

requestListConnectorDefinitionVersions :: ListConnectorDefinitionVersions -> TestTree
requestListConnectorDefinitionVersions =
  req
    "ListConnectorDefinitionVersions"
    "fixture/ListConnectorDefinitionVersions.yaml"

requestCreateSoftwareUpdateJob :: CreateSoftwareUpdateJob -> TestTree
requestCreateSoftwareUpdateJob =
  req
    "CreateSoftwareUpdateJob"
    "fixture/CreateSoftwareUpdateJob.yaml"

requestCreateCoreDefinition :: CreateCoreDefinition -> TestTree
requestCreateCoreDefinition =
  req
    "CreateCoreDefinition"
    "fixture/CreateCoreDefinition.yaml"

requestCreateConnectorDefinition :: CreateConnectorDefinition -> TestTree
requestCreateConnectorDefinition =
  req
    "CreateConnectorDefinition"
    "fixture/CreateConnectorDefinition.yaml"

requestGetAssociatedRole :: GetAssociatedRole -> TestTree
requestGetAssociatedRole =
  req
    "GetAssociatedRole"
    "fixture/GetAssociatedRole.yaml"

requestUpdateConnectivityInfo :: UpdateConnectivityInfo -> TestTree
requestUpdateConnectivityInfo =
  req
    "UpdateConnectivityInfo"
    "fixture/UpdateConnectivityInfo.yaml"

requestCreateSubscriptionDefinition :: CreateSubscriptionDefinition -> TestTree
requestCreateSubscriptionDefinition =
  req
    "CreateSubscriptionDefinition"
    "fixture/CreateSubscriptionDefinition.yaml"

requestDisassociateRoleFromGroup :: DisassociateRoleFromGroup -> TestTree
requestDisassociateRoleFromGroup =
  req
    "DisassociateRoleFromGroup"
    "fixture/DisassociateRoleFromGroup.yaml"

requestListCoreDefinitions :: ListCoreDefinitions -> TestTree
requestListCoreDefinitions =
  req
    "ListCoreDefinitions"
    "fixture/ListCoreDefinitions.yaml"

requestListConnectorDefinitions :: ListConnectorDefinitions -> TestTree
requestListConnectorDefinitions =
  req
    "ListConnectorDefinitions"
    "fixture/ListConnectorDefinitions.yaml"

requestCreateGroupCertificateAuthority :: CreateGroupCertificateAuthority -> TestTree
requestCreateGroupCertificateAuthority =
  req
    "CreateGroupCertificateAuthority"
    "fixture/CreateGroupCertificateAuthority.yaml"

requestListGroupCertificateAuthorities :: ListGroupCertificateAuthorities -> TestTree
requestListGroupCertificateAuthorities =
  req
    "ListGroupCertificateAuthorities"
    "fixture/ListGroupCertificateAuthorities.yaml"

requestDeleteConnectorDefinition :: DeleteConnectorDefinition -> TestTree
requestDeleteConnectorDefinition =
  req
    "DeleteConnectorDefinition"
    "fixture/DeleteConnectorDefinition.yaml"

requestGetLoggerDefinition :: GetLoggerDefinition -> TestTree
requestGetLoggerDefinition =
  req
    "GetLoggerDefinition"
    "fixture/GetLoggerDefinition.yaml"

requestUpdateConnectorDefinition :: UpdateConnectorDefinition -> TestTree
requestUpdateConnectorDefinition =
  req
    "UpdateConnectorDefinition"
    "fixture/UpdateConnectorDefinition.yaml"

requestCreateLoggerDefinitionVersion :: CreateLoggerDefinitionVersion -> TestTree
requestCreateLoggerDefinitionVersion =
  req
    "CreateLoggerDefinitionVersion"
    "fixture/CreateLoggerDefinitionVersion.yaml"

requestResetDeployments :: ResetDeployments -> TestTree
requestResetDeployments =
  req
    "ResetDeployments"
    "fixture/ResetDeployments.yaml"

requestListSubscriptionDefinitions :: ListSubscriptionDefinitions -> TestTree
requestListSubscriptionDefinitions =
  req
    "ListSubscriptionDefinitions"
    "fixture/ListSubscriptionDefinitions.yaml"

requestListGroupVersions :: ListGroupVersions -> TestTree
requestListGroupVersions =
  req
    "ListGroupVersions"
    "fixture/ListGroupVersions.yaml"

requestDeleteDeviceDefinition :: DeleteDeviceDefinition -> TestTree
requestDeleteDeviceDefinition =
  req
    "DeleteDeviceDefinition"
    "fixture/DeleteDeviceDefinition.yaml"

requestUpdateDeviceDefinition :: UpdateDeviceDefinition -> TestTree
requestUpdateDeviceDefinition =
  req
    "UpdateDeviceDefinition"
    "fixture/UpdateDeviceDefinition.yaml"

requestListDeviceDefinitions :: ListDeviceDefinitions -> TestTree
requestListDeviceDefinitions =
  req
    "ListDeviceDefinitions"
    "fixture/ListDeviceDefinitions.yaml"

requestDisassociateServiceRoleFromAccount :: DisassociateServiceRoleFromAccount -> TestTree
requestDisassociateServiceRoleFromAccount =
  req
    "DisassociateServiceRoleFromAccount"
    "fixture/DisassociateServiceRoleFromAccount.yaml"

requestListResourceDefinitionVersions :: ListResourceDefinitionVersions -> TestTree
requestListResourceDefinitionVersions =
  req
    "ListResourceDefinitionVersions"
    "fixture/ListResourceDefinitionVersions.yaml"

requestGetResourceDefinition :: GetResourceDefinition -> TestTree
requestGetResourceDefinition =
  req
    "GetResourceDefinition"
    "fixture/GetResourceDefinition.yaml"

requestGetGroup :: GetGroup -> TestTree
requestGetGroup =
  req
    "GetGroup"
    "fixture/GetGroup.yaml"

requestCreateDeviceDefinition :: CreateDeviceDefinition -> TestTree
requestCreateDeviceDefinition =
  req
    "CreateDeviceDefinition"
    "fixture/CreateDeviceDefinition.yaml"

requestGetDeviceDefinitionVersion :: GetDeviceDefinitionVersion -> TestTree
requestGetDeviceDefinitionVersion =
  req
    "GetDeviceDefinitionVersion"
    "fixture/GetDeviceDefinitionVersion.yaml"

requestCreateResourceDefinitionVersion :: CreateResourceDefinitionVersion -> TestTree
requestCreateResourceDefinitionVersion =
  req
    "CreateResourceDefinitionVersion"
    "fixture/CreateResourceDefinitionVersion.yaml"

requestCreateGroupVersion :: CreateGroupVersion -> TestTree
requestCreateGroupVersion =
  req
    "CreateGroupVersion"
    "fixture/CreateGroupVersion.yaml"

requestListBulkDeploymentDetailedReports :: ListBulkDeploymentDetailedReports -> TestTree
requestListBulkDeploymentDetailedReports =
  req
    "ListBulkDeploymentDetailedReports"
    "fixture/ListBulkDeploymentDetailedReports.yaml"

requestUpdateGroup :: UpdateGroup -> TestTree
requestUpdateGroup =
  req
    "UpdateGroup"
    "fixture/UpdateGroup.yaml"

requestDeleteGroup :: DeleteGroup -> TestTree
requestDeleteGroup =
  req
    "DeleteGroup"
    "fixture/DeleteGroup.yaml"

requestDeleteFunctionDefinition :: DeleteFunctionDefinition -> TestTree
requestDeleteFunctionDefinition =
  req
    "DeleteFunctionDefinition"
    "fixture/DeleteFunctionDefinition.yaml"

requestUpdateFunctionDefinition :: UpdateFunctionDefinition -> TestTree
requestUpdateFunctionDefinition =
  req
    "UpdateFunctionDefinition"
    "fixture/UpdateFunctionDefinition.yaml"

requestListFunctionDefinitions :: ListFunctionDefinitions -> TestTree
requestListFunctionDefinitions =
  req
    "ListFunctionDefinitions"
    "fixture/ListFunctionDefinitions.yaml"

requestGetFunctionDefinitionVersion :: GetFunctionDefinitionVersion -> TestTree
requestGetFunctionDefinitionVersion =
  req
    "GetFunctionDefinitionVersion"
    "fixture/GetFunctionDefinitionVersion.yaml"

requestGetBulkDeploymentStatus :: GetBulkDeploymentStatus -> TestTree
requestGetBulkDeploymentStatus =
  req
    "GetBulkDeploymentStatus"
    "fixture/GetBulkDeploymentStatus.yaml"

requestGetDeploymentStatus :: GetDeploymentStatus -> TestTree
requestGetDeploymentStatus =
  req
    "GetDeploymentStatus"
    "fixture/GetDeploymentStatus.yaml"

requestCreateFunctionDefinition :: CreateFunctionDefinition -> TestTree
requestCreateFunctionDefinition =
  req
    "CreateFunctionDefinition"
    "fixture/CreateFunctionDefinition.yaml"

requestGetSubscriptionDefinition :: GetSubscriptionDefinition -> TestTree
requestGetSubscriptionDefinition =
  req
    "GetSubscriptionDefinition"
    "fixture/GetSubscriptionDefinition.yaml"

requestGetConnectorDefinition :: GetConnectorDefinition -> TestTree
requestGetConnectorDefinition =
  req
    "GetConnectorDefinition"
    "fixture/GetConnectorDefinition.yaml"

requestGetCoreDefinition :: GetCoreDefinition -> TestTree
requestGetCoreDefinition =
  req
    "GetCoreDefinition"
    "fixture/GetCoreDefinition.yaml"

requestGetGroupCertificateAuthority :: GetGroupCertificateAuthority -> TestTree
requestGetGroupCertificateAuthority =
  req
    "GetGroupCertificateAuthority"
    "fixture/GetGroupCertificateAuthority.yaml"

requestGetServiceRoleForAccount :: GetServiceRoleForAccount -> TestTree
requestGetServiceRoleForAccount =
  req
    "GetServiceRoleForAccount"
    "fixture/GetServiceRoleForAccount.yaml"

requestGetLoggerDefinitionVersion :: GetLoggerDefinitionVersion -> TestTree
requestGetLoggerDefinitionVersion =
  req
    "GetLoggerDefinitionVersion"
    "fixture/GetLoggerDefinitionVersion.yaml"

requestCreateLoggerDefinition :: CreateLoggerDefinition -> TestTree
requestCreateLoggerDefinition =
  req
    "CreateLoggerDefinition"
    "fixture/CreateLoggerDefinition.yaml"

requestCreateDeployment :: CreateDeployment -> TestTree
requestCreateDeployment =
  req
    "CreateDeployment"
    "fixture/CreateDeployment.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetSubscriptionDefinitionVersion :: GetSubscriptionDefinitionVersion -> TestTree
requestGetSubscriptionDefinitionVersion =
  req
    "GetSubscriptionDefinitionVersion"
    "fixture/GetSubscriptionDefinitionVersion.yaml"

requestListLoggerDefinitionVersions :: ListLoggerDefinitionVersions -> TestTree
requestListLoggerDefinitionVersions =
  req
    "ListLoggerDefinitionVersions"
    "fixture/ListLoggerDefinitionVersions.yaml"

requestGetCoreDefinitionVersion :: GetCoreDefinitionVersion -> TestTree
requestGetCoreDefinitionVersion =
  req
    "GetCoreDefinitionVersion"
    "fixture/GetCoreDefinitionVersion.yaml"

requestGetConnectorDefinitionVersion :: GetConnectorDefinitionVersion -> TestTree
requestGetConnectorDefinitionVersion =
  req
    "GetConnectorDefinitionVersion"
    "fixture/GetConnectorDefinitionVersion.yaml"

-- Responses

responseDeleteSubscriptionDefinition :: DeleteSubscriptionDefinitionResponse -> TestTree
responseDeleteSubscriptionDefinition =
  res
    "DeleteSubscriptionDefinitionResponse"
    "fixture/DeleteSubscriptionDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSubscriptionDefinition)

responseUpdateCoreDefinition :: UpdateCoreDefinitionResponse -> TestTree
responseUpdateCoreDefinition =
  res
    "UpdateCoreDefinitionResponse"
    "fixture/UpdateCoreDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateCoreDefinition)

responseUpdateSubscriptionDefinition :: UpdateSubscriptionDefinitionResponse -> TestTree
responseUpdateSubscriptionDefinition =
  res
    "UpdateSubscriptionDefinitionResponse"
    "fixture/UpdateSubscriptionDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateSubscriptionDefinition)

responseAssociateServiceRoleToAccount :: AssociateServiceRoleToAccountResponse -> TestTree
responseAssociateServiceRoleToAccount =
  res
    "AssociateServiceRoleToAccountResponse"
    "fixture/AssociateServiceRoleToAccountResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateServiceRoleToAccount)

responseDeleteCoreDefinition :: DeleteCoreDefinitionResponse -> TestTree
responseDeleteCoreDefinition =
  res
    "DeleteCoreDefinitionResponse"
    "fixture/DeleteCoreDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCoreDefinition)

responseAssociateRoleToGroup :: AssociateRoleToGroupResponse -> TestTree
responseAssociateRoleToGroup =
  res
    "AssociateRoleToGroupResponse"
    "fixture/AssociateRoleToGroupResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateRoleToGroup)

responseGetGroupCertificateConfiguration :: GetGroupCertificateConfigurationResponse -> TestTree
responseGetGroupCertificateConfiguration =
  res
    "GetGroupCertificateConfigurationResponse"
    "fixture/GetGroupCertificateConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetGroupCertificateConfiguration)

responseListFunctionDefinitionVersions :: ListFunctionDefinitionVersionsResponse -> TestTree
responseListFunctionDefinitionVersions =
  res
    "ListFunctionDefinitionVersionsResponse"
    "fixture/ListFunctionDefinitionVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListFunctionDefinitionVersions)

responseStartBulkDeployment :: StartBulkDeploymentResponse -> TestTree
responseStartBulkDeployment =
  res
    "StartBulkDeploymentResponse"
    "fixture/StartBulkDeploymentResponse.proto"
    defaultService
    (Proxy :: Proxy StartBulkDeployment)

responseCreateFunctionDefinitionVersion :: CreateFunctionDefinitionVersionResponse -> TestTree
responseCreateFunctionDefinitionVersion =
  res
    "CreateFunctionDefinitionVersionResponse"
    "fixture/CreateFunctionDefinitionVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateFunctionDefinitionVersion)

responseGetFunctionDefinition :: GetFunctionDefinitionResponse -> TestTree
responseGetFunctionDefinition =
  res
    "GetFunctionDefinitionResponse"
    "fixture/GetFunctionDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy GetFunctionDefinition)

responseUpdateThingRuntimeConfiguration :: UpdateThingRuntimeConfigurationResponse -> TestTree
responseUpdateThingRuntimeConfiguration =
  res
    "UpdateThingRuntimeConfigurationResponse"
    "fixture/UpdateThingRuntimeConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateThingRuntimeConfiguration)

responseStopBulkDeployment :: StopBulkDeploymentResponse -> TestTree
responseStopBulkDeployment =
  res
    "StopBulkDeploymentResponse"
    "fixture/StopBulkDeploymentResponse.proto"
    defaultService
    (Proxy :: Proxy StopBulkDeployment)

responseListGroups :: ListGroupsResponse -> TestTree
responseListGroups =
  res
    "ListGroupsResponse"
    "fixture/ListGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListGroups)

responseDeleteResourceDefinition :: DeleteResourceDefinitionResponse -> TestTree
responseDeleteResourceDefinition =
  res
    "DeleteResourceDefinitionResponse"
    "fixture/DeleteResourceDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteResourceDefinition)

responseGetThingRuntimeConfiguration :: GetThingRuntimeConfigurationResponse -> TestTree
responseGetThingRuntimeConfiguration =
  res
    "GetThingRuntimeConfigurationResponse"
    "fixture/GetThingRuntimeConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetThingRuntimeConfiguration)

responseListDeviceDefinitionVersions :: ListDeviceDefinitionVersionsResponse -> TestTree
responseListDeviceDefinitionVersions =
  res
    "ListDeviceDefinitionVersionsResponse"
    "fixture/ListDeviceDefinitionVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDeviceDefinitionVersions)

responseUpdateResourceDefinition :: UpdateResourceDefinitionResponse -> TestTree
responseUpdateResourceDefinition =
  res
    "UpdateResourceDefinitionResponse"
    "fixture/UpdateResourceDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateResourceDefinition)

responseListResourceDefinitions :: ListResourceDefinitionsResponse -> TestTree
responseListResourceDefinitions =
  res
    "ListResourceDefinitionsResponse"
    "fixture/ListResourceDefinitionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListResourceDefinitions)

responseGetDeviceDefinition :: GetDeviceDefinitionResponse -> TestTree
responseGetDeviceDefinition =
  res
    "GetDeviceDefinitionResponse"
    "fixture/GetDeviceDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy GetDeviceDefinition)

responseCreateResourceDefinition :: CreateResourceDefinitionResponse -> TestTree
responseCreateResourceDefinition =
  res
    "CreateResourceDefinitionResponse"
    "fixture/CreateResourceDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateResourceDefinition)

responseGetResourceDefinitionVersion :: GetResourceDefinitionVersionResponse -> TestTree
responseGetResourceDefinitionVersion =
  res
    "GetResourceDefinitionVersionResponse"
    "fixture/GetResourceDefinitionVersionResponse.proto"
    defaultService
    (Proxy :: Proxy GetResourceDefinitionVersion)

responseCreateDeviceDefinitionVersion :: CreateDeviceDefinitionVersionResponse -> TestTree
responseCreateDeviceDefinitionVersion =
  res
    "CreateDeviceDefinitionVersionResponse"
    "fixture/CreateDeviceDefinitionVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDeviceDefinitionVersion)

responseGetGroupVersion :: GetGroupVersionResponse -> TestTree
responseGetGroupVersion =
  res
    "GetGroupVersionResponse"
    "fixture/GetGroupVersionResponse.proto"
    defaultService
    (Proxy :: Proxy GetGroupVersion)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseCreateGroup :: CreateGroupResponse -> TestTree
responseCreateGroup =
  res
    "CreateGroupResponse"
    "fixture/CreateGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateGroup)

responseUpdateGroupCertificateConfiguration :: UpdateGroupCertificateConfigurationResponse -> TestTree
responseUpdateGroupCertificateConfiguration =
  res
    "UpdateGroupCertificateConfigurationResponse"
    "fixture/UpdateGroupCertificateConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGroupCertificateConfiguration)

responseDeleteLoggerDefinition :: DeleteLoggerDefinitionResponse -> TestTree
responseDeleteLoggerDefinition =
  res
    "DeleteLoggerDefinitionResponse"
    "fixture/DeleteLoggerDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLoggerDefinition)

responseListBulkDeployments :: ListBulkDeploymentsResponse -> TestTree
responseListBulkDeployments =
  res
    "ListBulkDeploymentsResponse"
    "fixture/ListBulkDeploymentsResponse.proto"
    defaultService
    (Proxy :: Proxy ListBulkDeployments)

responseCreateConnectorDefinitionVersion :: CreateConnectorDefinitionVersionResponse -> TestTree
responseCreateConnectorDefinitionVersion =
  res
    "CreateConnectorDefinitionVersionResponse"
    "fixture/CreateConnectorDefinitionVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateConnectorDefinitionVersion)

responseCreateSubscriptionDefinitionVersion :: CreateSubscriptionDefinitionVersionResponse -> TestTree
responseCreateSubscriptionDefinitionVersion =
  res
    "CreateSubscriptionDefinitionVersionResponse"
    "fixture/CreateSubscriptionDefinitionVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSubscriptionDefinitionVersion)

responseUpdateLoggerDefinition :: UpdateLoggerDefinitionResponse -> TestTree
responseUpdateLoggerDefinition =
  res
    "UpdateLoggerDefinitionResponse"
    "fixture/UpdateLoggerDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateLoggerDefinition)

responseListLoggerDefinitions :: ListLoggerDefinitionsResponse -> TestTree
responseListLoggerDefinitions =
  res
    "ListLoggerDefinitionsResponse"
    "fixture/ListLoggerDefinitionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListLoggerDefinitions)

responseCreateCoreDefinitionVersion :: CreateCoreDefinitionVersionResponse -> TestTree
responseCreateCoreDefinitionVersion =
  res
    "CreateCoreDefinitionVersionResponse"
    "fixture/CreateCoreDefinitionVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCoreDefinitionVersion)

responseListDeployments :: ListDeploymentsResponse -> TestTree
responseListDeployments =
  res
    "ListDeploymentsResponse"
    "fixture/ListDeploymentsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDeployments)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseGetConnectivityInfo :: GetConnectivityInfoResponse -> TestTree
responseGetConnectivityInfo =
  res
    "GetConnectivityInfoResponse"
    "fixture/GetConnectivityInfoResponse.proto"
    defaultService
    (Proxy :: Proxy GetConnectivityInfo)

responseListSubscriptionDefinitionVersions :: ListSubscriptionDefinitionVersionsResponse -> TestTree
responseListSubscriptionDefinitionVersions =
  res
    "ListSubscriptionDefinitionVersionsResponse"
    "fixture/ListSubscriptionDefinitionVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSubscriptionDefinitionVersions)

responseListCoreDefinitionVersions :: ListCoreDefinitionVersionsResponse -> TestTree
responseListCoreDefinitionVersions =
  res
    "ListCoreDefinitionVersionsResponse"
    "fixture/ListCoreDefinitionVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListCoreDefinitionVersions)

responseListConnectorDefinitionVersions :: ListConnectorDefinitionVersionsResponse -> TestTree
responseListConnectorDefinitionVersions =
  res
    "ListConnectorDefinitionVersionsResponse"
    "fixture/ListConnectorDefinitionVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListConnectorDefinitionVersions)

responseCreateSoftwareUpdateJob :: CreateSoftwareUpdateJobResponse -> TestTree
responseCreateSoftwareUpdateJob =
  res
    "CreateSoftwareUpdateJobResponse"
    "fixture/CreateSoftwareUpdateJobResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSoftwareUpdateJob)

responseCreateCoreDefinition :: CreateCoreDefinitionResponse -> TestTree
responseCreateCoreDefinition =
  res
    "CreateCoreDefinitionResponse"
    "fixture/CreateCoreDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCoreDefinition)

responseCreateConnectorDefinition :: CreateConnectorDefinitionResponse -> TestTree
responseCreateConnectorDefinition =
  res
    "CreateConnectorDefinitionResponse"
    "fixture/CreateConnectorDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateConnectorDefinition)

responseGetAssociatedRole :: GetAssociatedRoleResponse -> TestTree
responseGetAssociatedRole =
  res
    "GetAssociatedRoleResponse"
    "fixture/GetAssociatedRoleResponse.proto"
    defaultService
    (Proxy :: Proxy GetAssociatedRole)

responseUpdateConnectivityInfo :: UpdateConnectivityInfoResponse -> TestTree
responseUpdateConnectivityInfo =
  res
    "UpdateConnectivityInfoResponse"
    "fixture/UpdateConnectivityInfoResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateConnectivityInfo)

responseCreateSubscriptionDefinition :: CreateSubscriptionDefinitionResponse -> TestTree
responseCreateSubscriptionDefinition =
  res
    "CreateSubscriptionDefinitionResponse"
    "fixture/CreateSubscriptionDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSubscriptionDefinition)

responseDisassociateRoleFromGroup :: DisassociateRoleFromGroupResponse -> TestTree
responseDisassociateRoleFromGroup =
  res
    "DisassociateRoleFromGroupResponse"
    "fixture/DisassociateRoleFromGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateRoleFromGroup)

responseListCoreDefinitions :: ListCoreDefinitionsResponse -> TestTree
responseListCoreDefinitions =
  res
    "ListCoreDefinitionsResponse"
    "fixture/ListCoreDefinitionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListCoreDefinitions)

responseListConnectorDefinitions :: ListConnectorDefinitionsResponse -> TestTree
responseListConnectorDefinitions =
  res
    "ListConnectorDefinitionsResponse"
    "fixture/ListConnectorDefinitionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListConnectorDefinitions)

responseCreateGroupCertificateAuthority :: CreateGroupCertificateAuthorityResponse -> TestTree
responseCreateGroupCertificateAuthority =
  res
    "CreateGroupCertificateAuthorityResponse"
    "fixture/CreateGroupCertificateAuthorityResponse.proto"
    defaultService
    (Proxy :: Proxy CreateGroupCertificateAuthority)

responseListGroupCertificateAuthorities :: ListGroupCertificateAuthoritiesResponse -> TestTree
responseListGroupCertificateAuthorities =
  res
    "ListGroupCertificateAuthoritiesResponse"
    "fixture/ListGroupCertificateAuthoritiesResponse.proto"
    defaultService
    (Proxy :: Proxy ListGroupCertificateAuthorities)

responseDeleteConnectorDefinition :: DeleteConnectorDefinitionResponse -> TestTree
responseDeleteConnectorDefinition =
  res
    "DeleteConnectorDefinitionResponse"
    "fixture/DeleteConnectorDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConnectorDefinition)

responseGetLoggerDefinition :: GetLoggerDefinitionResponse -> TestTree
responseGetLoggerDefinition =
  res
    "GetLoggerDefinitionResponse"
    "fixture/GetLoggerDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy GetLoggerDefinition)

responseUpdateConnectorDefinition :: UpdateConnectorDefinitionResponse -> TestTree
responseUpdateConnectorDefinition =
  res
    "UpdateConnectorDefinitionResponse"
    "fixture/UpdateConnectorDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateConnectorDefinition)

responseCreateLoggerDefinitionVersion :: CreateLoggerDefinitionVersionResponse -> TestTree
responseCreateLoggerDefinitionVersion =
  res
    "CreateLoggerDefinitionVersionResponse"
    "fixture/CreateLoggerDefinitionVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLoggerDefinitionVersion)

responseResetDeployments :: ResetDeploymentsResponse -> TestTree
responseResetDeployments =
  res
    "ResetDeploymentsResponse"
    "fixture/ResetDeploymentsResponse.proto"
    defaultService
    (Proxy :: Proxy ResetDeployments)

responseListSubscriptionDefinitions :: ListSubscriptionDefinitionsResponse -> TestTree
responseListSubscriptionDefinitions =
  res
    "ListSubscriptionDefinitionsResponse"
    "fixture/ListSubscriptionDefinitionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSubscriptionDefinitions)

responseListGroupVersions :: ListGroupVersionsResponse -> TestTree
responseListGroupVersions =
  res
    "ListGroupVersionsResponse"
    "fixture/ListGroupVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListGroupVersions)

responseDeleteDeviceDefinition :: DeleteDeviceDefinitionResponse -> TestTree
responseDeleteDeviceDefinition =
  res
    "DeleteDeviceDefinitionResponse"
    "fixture/DeleteDeviceDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDeviceDefinition)

responseUpdateDeviceDefinition :: UpdateDeviceDefinitionResponse -> TestTree
responseUpdateDeviceDefinition =
  res
    "UpdateDeviceDefinitionResponse"
    "fixture/UpdateDeviceDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDeviceDefinition)

responseListDeviceDefinitions :: ListDeviceDefinitionsResponse -> TestTree
responseListDeviceDefinitions =
  res
    "ListDeviceDefinitionsResponse"
    "fixture/ListDeviceDefinitionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDeviceDefinitions)

responseDisassociateServiceRoleFromAccount :: DisassociateServiceRoleFromAccountResponse -> TestTree
responseDisassociateServiceRoleFromAccount =
  res
    "DisassociateServiceRoleFromAccountResponse"
    "fixture/DisassociateServiceRoleFromAccountResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateServiceRoleFromAccount)

responseListResourceDefinitionVersions :: ListResourceDefinitionVersionsResponse -> TestTree
responseListResourceDefinitionVersions =
  res
    "ListResourceDefinitionVersionsResponse"
    "fixture/ListResourceDefinitionVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListResourceDefinitionVersions)

responseGetResourceDefinition :: GetResourceDefinitionResponse -> TestTree
responseGetResourceDefinition =
  res
    "GetResourceDefinitionResponse"
    "fixture/GetResourceDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy GetResourceDefinition)

responseGetGroup :: GetGroupResponse -> TestTree
responseGetGroup =
  res
    "GetGroupResponse"
    "fixture/GetGroupResponse.proto"
    defaultService
    (Proxy :: Proxy GetGroup)

responseCreateDeviceDefinition :: CreateDeviceDefinitionResponse -> TestTree
responseCreateDeviceDefinition =
  res
    "CreateDeviceDefinitionResponse"
    "fixture/CreateDeviceDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDeviceDefinition)

responseGetDeviceDefinitionVersion :: GetDeviceDefinitionVersionResponse -> TestTree
responseGetDeviceDefinitionVersion =
  res
    "GetDeviceDefinitionVersionResponse"
    "fixture/GetDeviceDefinitionVersionResponse.proto"
    defaultService
    (Proxy :: Proxy GetDeviceDefinitionVersion)

responseCreateResourceDefinitionVersion :: CreateResourceDefinitionVersionResponse -> TestTree
responseCreateResourceDefinitionVersion =
  res
    "CreateResourceDefinitionVersionResponse"
    "fixture/CreateResourceDefinitionVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateResourceDefinitionVersion)

responseCreateGroupVersion :: CreateGroupVersionResponse -> TestTree
responseCreateGroupVersion =
  res
    "CreateGroupVersionResponse"
    "fixture/CreateGroupVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateGroupVersion)

responseListBulkDeploymentDetailedReports :: ListBulkDeploymentDetailedReportsResponse -> TestTree
responseListBulkDeploymentDetailedReports =
  res
    "ListBulkDeploymentDetailedReportsResponse"
    "fixture/ListBulkDeploymentDetailedReportsResponse.proto"
    defaultService
    (Proxy :: Proxy ListBulkDeploymentDetailedReports)

responseUpdateGroup :: UpdateGroupResponse -> TestTree
responseUpdateGroup =
  res
    "UpdateGroupResponse"
    "fixture/UpdateGroupResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateGroup)

responseDeleteGroup :: DeleteGroupResponse -> TestTree
responseDeleteGroup =
  res
    "DeleteGroupResponse"
    "fixture/DeleteGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteGroup)

responseDeleteFunctionDefinition :: DeleteFunctionDefinitionResponse -> TestTree
responseDeleteFunctionDefinition =
  res
    "DeleteFunctionDefinitionResponse"
    "fixture/DeleteFunctionDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFunctionDefinition)

responseUpdateFunctionDefinition :: UpdateFunctionDefinitionResponse -> TestTree
responseUpdateFunctionDefinition =
  res
    "UpdateFunctionDefinitionResponse"
    "fixture/UpdateFunctionDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateFunctionDefinition)

responseListFunctionDefinitions :: ListFunctionDefinitionsResponse -> TestTree
responseListFunctionDefinitions =
  res
    "ListFunctionDefinitionsResponse"
    "fixture/ListFunctionDefinitionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListFunctionDefinitions)

responseGetFunctionDefinitionVersion :: GetFunctionDefinitionVersionResponse -> TestTree
responseGetFunctionDefinitionVersion =
  res
    "GetFunctionDefinitionVersionResponse"
    "fixture/GetFunctionDefinitionVersionResponse.proto"
    defaultService
    (Proxy :: Proxy GetFunctionDefinitionVersion)

responseGetBulkDeploymentStatus :: GetBulkDeploymentStatusResponse -> TestTree
responseGetBulkDeploymentStatus =
  res
    "GetBulkDeploymentStatusResponse"
    "fixture/GetBulkDeploymentStatusResponse.proto"
    defaultService
    (Proxy :: Proxy GetBulkDeploymentStatus)

responseGetDeploymentStatus :: GetDeploymentStatusResponse -> TestTree
responseGetDeploymentStatus =
  res
    "GetDeploymentStatusResponse"
    "fixture/GetDeploymentStatusResponse.proto"
    defaultService
    (Proxy :: Proxy GetDeploymentStatus)

responseCreateFunctionDefinition :: CreateFunctionDefinitionResponse -> TestTree
responseCreateFunctionDefinition =
  res
    "CreateFunctionDefinitionResponse"
    "fixture/CreateFunctionDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateFunctionDefinition)

responseGetSubscriptionDefinition :: GetSubscriptionDefinitionResponse -> TestTree
responseGetSubscriptionDefinition =
  res
    "GetSubscriptionDefinitionResponse"
    "fixture/GetSubscriptionDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy GetSubscriptionDefinition)

responseGetConnectorDefinition :: GetConnectorDefinitionResponse -> TestTree
responseGetConnectorDefinition =
  res
    "GetConnectorDefinitionResponse"
    "fixture/GetConnectorDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy GetConnectorDefinition)

responseGetCoreDefinition :: GetCoreDefinitionResponse -> TestTree
responseGetCoreDefinition =
  res
    "GetCoreDefinitionResponse"
    "fixture/GetCoreDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy GetCoreDefinition)

responseGetGroupCertificateAuthority :: GetGroupCertificateAuthorityResponse -> TestTree
responseGetGroupCertificateAuthority =
  res
    "GetGroupCertificateAuthorityResponse"
    "fixture/GetGroupCertificateAuthorityResponse.proto"
    defaultService
    (Proxy :: Proxy GetGroupCertificateAuthority)

responseGetServiceRoleForAccount :: GetServiceRoleForAccountResponse -> TestTree
responseGetServiceRoleForAccount =
  res
    "GetServiceRoleForAccountResponse"
    "fixture/GetServiceRoleForAccountResponse.proto"
    defaultService
    (Proxy :: Proxy GetServiceRoleForAccount)

responseGetLoggerDefinitionVersion :: GetLoggerDefinitionVersionResponse -> TestTree
responseGetLoggerDefinitionVersion =
  res
    "GetLoggerDefinitionVersionResponse"
    "fixture/GetLoggerDefinitionVersionResponse.proto"
    defaultService
    (Proxy :: Proxy GetLoggerDefinitionVersion)

responseCreateLoggerDefinition :: CreateLoggerDefinitionResponse -> TestTree
responseCreateLoggerDefinition =
  res
    "CreateLoggerDefinitionResponse"
    "fixture/CreateLoggerDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateLoggerDefinition)

responseCreateDeployment :: CreateDeploymentResponse -> TestTree
responseCreateDeployment =
  res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDeployment)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseGetSubscriptionDefinitionVersion :: GetSubscriptionDefinitionVersionResponse -> TestTree
responseGetSubscriptionDefinitionVersion =
  res
    "GetSubscriptionDefinitionVersionResponse"
    "fixture/GetSubscriptionDefinitionVersionResponse.proto"
    defaultService
    (Proxy :: Proxy GetSubscriptionDefinitionVersion)

responseListLoggerDefinitionVersions :: ListLoggerDefinitionVersionsResponse -> TestTree
responseListLoggerDefinitionVersions =
  res
    "ListLoggerDefinitionVersionsResponse"
    "fixture/ListLoggerDefinitionVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListLoggerDefinitionVersions)

responseGetCoreDefinitionVersion :: GetCoreDefinitionVersionResponse -> TestTree
responseGetCoreDefinitionVersion =
  res
    "GetCoreDefinitionVersionResponse"
    "fixture/GetCoreDefinitionVersionResponse.proto"
    defaultService
    (Proxy :: Proxy GetCoreDefinitionVersion)

responseGetConnectorDefinitionVersion :: GetConnectorDefinitionVersionResponse -> TestTree
responseGetConnectorDefinitionVersion =
  res
    "GetConnectorDefinitionVersionResponse"
    "fixture/GetConnectorDefinitionVersionResponse.proto"
    defaultService
    (Proxy :: Proxy GetConnectorDefinitionVersion)
