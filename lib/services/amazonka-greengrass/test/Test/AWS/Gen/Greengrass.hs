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
--         [ requestDeleteCoreDefinition $
--             newDeleteCoreDefinition
--
--         , requestUpdateCoreDefinition $
--             newUpdateCoreDefinition
--
--         , requestDeleteSubscriptionDefinition $
--             newDeleteSubscriptionDefinition
--
--         , requestUpdateSubscriptionDefinition $
--             newUpdateSubscriptionDefinition
--
--         , requestAssociateServiceRoleToAccount $
--             newAssociateServiceRoleToAccount
--
--         , requestGetGroupCertificateConfiguration $
--             newGetGroupCertificateConfiguration
--
--         , requestAssociateRoleToGroup $
--             newAssociateRoleToGroup
--
--         , requestListFunctionDefinitionVersions $
--             newListFunctionDefinitionVersions
--
--         , requestStopBulkDeployment $
--             newStopBulkDeployment
--
--         , requestCreateFunctionDefinitionVersion $
--             newCreateFunctionDefinitionVersion
--
--         , requestUpdateThingRuntimeConfiguration $
--             newUpdateThingRuntimeConfiguration
--
--         , requestGetFunctionDefinition $
--             newGetFunctionDefinition
--
--         , requestStartBulkDeployment $
--             newStartBulkDeployment
--
--         , requestGetThingRuntimeConfiguration $
--             newGetThingRuntimeConfiguration
--
--         , requestListResourceDefinitions $
--             newListResourceDefinitions
--
--         , requestListDeviceDefinitionVersions $
--             newListDeviceDefinitionVersions
--
--         , requestListGroups $
--             newListGroups
--
--         , requestDeleteResourceDefinition $
--             newDeleteResourceDefinition
--
--         , requestUpdateResourceDefinition $
--             newUpdateResourceDefinition
--
--         , requestGetGroupVersion $
--             newGetGroupVersion
--
--         , requestCreateDeviceDefinitionVersion $
--             newCreateDeviceDefinitionVersion
--
--         , requestCreateResourceDefinition $
--             newCreateResourceDefinition
--
--         , requestGetDeviceDefinition $
--             newGetDeviceDefinition
--
--         , requestCreateGroup $
--             newCreateGroup
--
--         , requestUpdateGroupCertificateConfiguration $
--             newUpdateGroupCertificateConfiguration
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestGetResourceDefinitionVersion $
--             newGetResourceDefinitionVersion
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListLoggerDefinitions $
--             newListLoggerDefinitions
--
--         , requestDeleteLoggerDefinition $
--             newDeleteLoggerDefinition
--
--         , requestListDeployments $
--             newListDeployments
--
--         , requestCreateSubscriptionDefinitionVersion $
--             newCreateSubscriptionDefinitionVersion
--
--         , requestCreateCoreDefinitionVersion $
--             newCreateCoreDefinitionVersion
--
--         , requestCreateConnectorDefinitionVersion $
--             newCreateConnectorDefinitionVersion
--
--         , requestListBulkDeployments $
--             newListBulkDeployments
--
--         , requestUpdateLoggerDefinition $
--             newUpdateLoggerDefinition
--
--         , requestCreateSoftwareUpdateJob $
--             newCreateSoftwareUpdateJob
--
--         , requestListSubscriptionDefinitionVersions $
--             newListSubscriptionDefinitionVersions
--
--         , requestGetConnectivityInfo $
--             newGetConnectivityInfo
--
--         , requestListConnectorDefinitionVersions $
--             newListConnectorDefinitionVersions
--
--         , requestListCoreDefinitionVersions $
--             newListCoreDefinitionVersions
--
--         , requestGetAssociatedRole $
--             newGetAssociatedRole
--
--         , requestCreateCoreDefinition $
--             newCreateCoreDefinition
--
--         , requestUpdateConnectivityInfo $
--             newUpdateConnectivityInfo
--
--         , requestCreateSubscriptionDefinition $
--             newCreateSubscriptionDefinition
--
--         , requestCreateConnectorDefinition $
--             newCreateConnectorDefinition
--
--         , requestListConnectorDefinitions $
--             newListConnectorDefinitions
--
--         , requestGetLoggerDefinition $
--             newGetLoggerDefinition
--
--         , requestDeleteConnectorDefinition $
--             newDeleteConnectorDefinition
--
--         , requestCreateGroupCertificateAuthority $
--             newCreateGroupCertificateAuthority
--
--         , requestListGroupCertificateAuthorities $
--             newListGroupCertificateAuthorities
--
--         , requestDisassociateRoleFromGroup $
--             newDisassociateRoleFromGroup
--
--         , requestListCoreDefinitions $
--             newListCoreDefinitions
--
--         , requestUpdateConnectorDefinition $
--             newUpdateConnectorDefinition
--
--         , requestListSubscriptionDefinitions $
--             newListSubscriptionDefinitions
--
--         , requestCreateLoggerDefinitionVersion $
--             newCreateLoggerDefinitionVersion
--
--         , requestResetDeployments $
--             newResetDeployments
--
--         , requestDeleteDeviceDefinition $
--             newDeleteDeviceDefinition
--
--         , requestDisassociateServiceRoleFromAccount $
--             newDisassociateServiceRoleFromAccount
--
--         , requestListDeviceDefinitions $
--             newListDeviceDefinitions
--
--         , requestListGroupVersions $
--             newListGroupVersions
--
--         , requestUpdateDeviceDefinition $
--             newUpdateDeviceDefinition
--
--         , requestListResourceDefinitionVersions $
--             newListResourceDefinitionVersions
--
--         , requestCreateDeviceDefinition $
--             newCreateDeviceDefinition
--
--         , requestGetResourceDefinition $
--             newGetResourceDefinition
--
--         , requestCreateResourceDefinitionVersion $
--             newCreateResourceDefinitionVersion
--
--         , requestCreateGroupVersion $
--             newCreateGroupVersion
--
--         , requestGetDeviceDefinitionVersion $
--             newGetDeviceDefinitionVersion
--
--         , requestGetGroup $
--             newGetGroup
--
--         , requestDeleteGroup $
--             newDeleteGroup
--
--         , requestListFunctionDefinitions $
--             newListFunctionDefinitions
--
--         , requestDeleteFunctionDefinition $
--             newDeleteFunctionDefinition
--
--         , requestUpdateFunctionDefinition $
--             newUpdateFunctionDefinition
--
--         , requestListBulkDeploymentDetailedReports $
--             newListBulkDeploymentDetailedReports
--
--         , requestUpdateGroup $
--             newUpdateGroup
--
--         , requestGetDeploymentStatus $
--             newGetDeploymentStatus
--
--         , requestGetFunctionDefinitionVersion $
--             newGetFunctionDefinitionVersion
--
--         , requestGetBulkDeploymentStatus $
--             newGetBulkDeploymentStatus
--
--         , requestCreateFunctionDefinition $
--             newCreateFunctionDefinition
--
--         , requestGetConnectorDefinition $
--             newGetConnectorDefinition
--
--         , requestGetSubscriptionDefinition $
--             newGetSubscriptionDefinition
--
--         , requestGetCoreDefinition $
--             newGetCoreDefinition
--
--         , requestGetServiceRoleForAccount $
--             newGetServiceRoleForAccount
--
--         , requestCreateDeployment $
--             newCreateDeployment
--
--         , requestGetLoggerDefinitionVersion $
--             newGetLoggerDefinitionVersion
--
--         , requestCreateLoggerDefinition $
--             newCreateLoggerDefinition
--
--         , requestGetGroupCertificateAuthority $
--             newGetGroupCertificateAuthority
--
--         , requestGetConnectorDefinitionVersion $
--             newGetConnectorDefinitionVersion
--
--         , requestListLoggerDefinitionVersions $
--             newListLoggerDefinitionVersions
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestGetSubscriptionDefinitionVersion $
--             newGetSubscriptionDefinitionVersion
--
--         , requestGetCoreDefinitionVersion $
--             newGetCoreDefinitionVersion
--
--           ]

--     , testGroup "response"
--         [ responseDeleteCoreDefinition $
--             newDeleteCoreDefinitionResponse
--
--         , responseUpdateCoreDefinition $
--             newUpdateCoreDefinitionResponse
--
--         , responseDeleteSubscriptionDefinition $
--             newDeleteSubscriptionDefinitionResponse
--
--         , responseUpdateSubscriptionDefinition $
--             newUpdateSubscriptionDefinitionResponse
--
--         , responseAssociateServiceRoleToAccount $
--             newAssociateServiceRoleToAccountResponse
--
--         , responseGetGroupCertificateConfiguration $
--             newGetGroupCertificateConfigurationResponse
--
--         , responseAssociateRoleToGroup $
--             newAssociateRoleToGroupResponse
--
--         , responseListFunctionDefinitionVersions $
--             newListFunctionDefinitionVersionsResponse
--
--         , responseStopBulkDeployment $
--             newStopBulkDeploymentResponse
--
--         , responseCreateFunctionDefinitionVersion $
--             newCreateFunctionDefinitionVersionResponse
--
--         , responseUpdateThingRuntimeConfiguration $
--             newUpdateThingRuntimeConfigurationResponse
--
--         , responseGetFunctionDefinition $
--             newGetFunctionDefinitionResponse
--
--         , responseStartBulkDeployment $
--             newStartBulkDeploymentResponse
--
--         , responseGetThingRuntimeConfiguration $
--             newGetThingRuntimeConfigurationResponse
--
--         , responseListResourceDefinitions $
--             newListResourceDefinitionsResponse
--
--         , responseListDeviceDefinitionVersions $
--             newListDeviceDefinitionVersionsResponse
--
--         , responseListGroups $
--             newListGroupsResponse
--
--         , responseDeleteResourceDefinition $
--             newDeleteResourceDefinitionResponse
--
--         , responseUpdateResourceDefinition $
--             newUpdateResourceDefinitionResponse
--
--         , responseGetGroupVersion $
--             newGetGroupVersionResponse
--
--         , responseCreateDeviceDefinitionVersion $
--             newCreateDeviceDefinitionVersionResponse
--
--         , responseCreateResourceDefinition $
--             newCreateResourceDefinitionResponse
--
--         , responseGetDeviceDefinition $
--             newGetDeviceDefinitionResponse
--
--         , responseCreateGroup $
--             newCreateGroupResponse
--
--         , responseUpdateGroupCertificateConfiguration $
--             newUpdateGroupCertificateConfigurationResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseGetResourceDefinitionVersion $
--             newGetResourceDefinitionVersionResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListLoggerDefinitions $
--             newListLoggerDefinitionsResponse
--
--         , responseDeleteLoggerDefinition $
--             newDeleteLoggerDefinitionResponse
--
--         , responseListDeployments $
--             newListDeploymentsResponse
--
--         , responseCreateSubscriptionDefinitionVersion $
--             newCreateSubscriptionDefinitionVersionResponse
--
--         , responseCreateCoreDefinitionVersion $
--             newCreateCoreDefinitionVersionResponse
--
--         , responseCreateConnectorDefinitionVersion $
--             newCreateConnectorDefinitionVersionResponse
--
--         , responseListBulkDeployments $
--             newListBulkDeploymentsResponse
--
--         , responseUpdateLoggerDefinition $
--             newUpdateLoggerDefinitionResponse
--
--         , responseCreateSoftwareUpdateJob $
--             newCreateSoftwareUpdateJobResponse
--
--         , responseListSubscriptionDefinitionVersions $
--             newListSubscriptionDefinitionVersionsResponse
--
--         , responseGetConnectivityInfo $
--             newGetConnectivityInfoResponse
--
--         , responseListConnectorDefinitionVersions $
--             newListConnectorDefinitionVersionsResponse
--
--         , responseListCoreDefinitionVersions $
--             newListCoreDefinitionVersionsResponse
--
--         , responseGetAssociatedRole $
--             newGetAssociatedRoleResponse
--
--         , responseCreateCoreDefinition $
--             newCreateCoreDefinitionResponse
--
--         , responseUpdateConnectivityInfo $
--             newUpdateConnectivityInfoResponse
--
--         , responseCreateSubscriptionDefinition $
--             newCreateSubscriptionDefinitionResponse
--
--         , responseCreateConnectorDefinition $
--             newCreateConnectorDefinitionResponse
--
--         , responseListConnectorDefinitions $
--             newListConnectorDefinitionsResponse
--
--         , responseGetLoggerDefinition $
--             newGetLoggerDefinitionResponse
--
--         , responseDeleteConnectorDefinition $
--             newDeleteConnectorDefinitionResponse
--
--         , responseCreateGroupCertificateAuthority $
--             newCreateGroupCertificateAuthorityResponse
--
--         , responseListGroupCertificateAuthorities $
--             newListGroupCertificateAuthoritiesResponse
--
--         , responseDisassociateRoleFromGroup $
--             newDisassociateRoleFromGroupResponse
--
--         , responseListCoreDefinitions $
--             newListCoreDefinitionsResponse
--
--         , responseUpdateConnectorDefinition $
--             newUpdateConnectorDefinitionResponse
--
--         , responseListSubscriptionDefinitions $
--             newListSubscriptionDefinitionsResponse
--
--         , responseCreateLoggerDefinitionVersion $
--             newCreateLoggerDefinitionVersionResponse
--
--         , responseResetDeployments $
--             newResetDeploymentsResponse
--
--         , responseDeleteDeviceDefinition $
--             newDeleteDeviceDefinitionResponse
--
--         , responseDisassociateServiceRoleFromAccount $
--             newDisassociateServiceRoleFromAccountResponse
--
--         , responseListDeviceDefinitions $
--             newListDeviceDefinitionsResponse
--
--         , responseListGroupVersions $
--             newListGroupVersionsResponse
--
--         , responseUpdateDeviceDefinition $
--             newUpdateDeviceDefinitionResponse
--
--         , responseListResourceDefinitionVersions $
--             newListResourceDefinitionVersionsResponse
--
--         , responseCreateDeviceDefinition $
--             newCreateDeviceDefinitionResponse
--
--         , responseGetResourceDefinition $
--             newGetResourceDefinitionResponse
--
--         , responseCreateResourceDefinitionVersion $
--             newCreateResourceDefinitionVersionResponse
--
--         , responseCreateGroupVersion $
--             newCreateGroupVersionResponse
--
--         , responseGetDeviceDefinitionVersion $
--             newGetDeviceDefinitionVersionResponse
--
--         , responseGetGroup $
--             newGetGroupResponse
--
--         , responseDeleteGroup $
--             newDeleteGroupResponse
--
--         , responseListFunctionDefinitions $
--             newListFunctionDefinitionsResponse
--
--         , responseDeleteFunctionDefinition $
--             newDeleteFunctionDefinitionResponse
--
--         , responseUpdateFunctionDefinition $
--             newUpdateFunctionDefinitionResponse
--
--         , responseListBulkDeploymentDetailedReports $
--             newListBulkDeploymentDetailedReportsResponse
--
--         , responseUpdateGroup $
--             newUpdateGroupResponse
--
--         , responseGetDeploymentStatus $
--             newGetDeploymentStatusResponse
--
--         , responseGetFunctionDefinitionVersion $
--             newGetFunctionDefinitionVersionResponse
--
--         , responseGetBulkDeploymentStatus $
--             newGetBulkDeploymentStatusResponse
--
--         , responseCreateFunctionDefinition $
--             newCreateFunctionDefinitionResponse
--
--         , responseGetConnectorDefinition $
--             newGetConnectorDefinitionResponse
--
--         , responseGetSubscriptionDefinition $
--             newGetSubscriptionDefinitionResponse
--
--         , responseGetCoreDefinition $
--             newGetCoreDefinitionResponse
--
--         , responseGetServiceRoleForAccount $
--             newGetServiceRoleForAccountResponse
--
--         , responseCreateDeployment $
--             newCreateDeploymentResponse
--
--         , responseGetLoggerDefinitionVersion $
--             newGetLoggerDefinitionVersionResponse
--
--         , responseCreateLoggerDefinition $
--             newCreateLoggerDefinitionResponse
--
--         , responseGetGroupCertificateAuthority $
--             newGetGroupCertificateAuthorityResponse
--
--         , responseGetConnectorDefinitionVersion $
--             newGetConnectorDefinitionVersionResponse
--
--         , responseListLoggerDefinitionVersions $
--             newListLoggerDefinitionVersionsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseGetSubscriptionDefinitionVersion $
--             newGetSubscriptionDefinitionVersionResponse
--
--         , responseGetCoreDefinitionVersion $
--             newGetCoreDefinitionVersionResponse
--
--           ]
--     ]

-- Requests

requestDeleteCoreDefinition :: DeleteCoreDefinition -> TestTree
requestDeleteCoreDefinition =
  req
    "DeleteCoreDefinition"
    "fixture/DeleteCoreDefinition.yaml"

requestUpdateCoreDefinition :: UpdateCoreDefinition -> TestTree
requestUpdateCoreDefinition =
  req
    "UpdateCoreDefinition"
    "fixture/UpdateCoreDefinition.yaml"

requestDeleteSubscriptionDefinition :: DeleteSubscriptionDefinition -> TestTree
requestDeleteSubscriptionDefinition =
  req
    "DeleteSubscriptionDefinition"
    "fixture/DeleteSubscriptionDefinition.yaml"

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

requestGetGroupCertificateConfiguration :: GetGroupCertificateConfiguration -> TestTree
requestGetGroupCertificateConfiguration =
  req
    "GetGroupCertificateConfiguration"
    "fixture/GetGroupCertificateConfiguration.yaml"

requestAssociateRoleToGroup :: AssociateRoleToGroup -> TestTree
requestAssociateRoleToGroup =
  req
    "AssociateRoleToGroup"
    "fixture/AssociateRoleToGroup.yaml"

requestListFunctionDefinitionVersions :: ListFunctionDefinitionVersions -> TestTree
requestListFunctionDefinitionVersions =
  req
    "ListFunctionDefinitionVersions"
    "fixture/ListFunctionDefinitionVersions.yaml"

requestStopBulkDeployment :: StopBulkDeployment -> TestTree
requestStopBulkDeployment =
  req
    "StopBulkDeployment"
    "fixture/StopBulkDeployment.yaml"

requestCreateFunctionDefinitionVersion :: CreateFunctionDefinitionVersion -> TestTree
requestCreateFunctionDefinitionVersion =
  req
    "CreateFunctionDefinitionVersion"
    "fixture/CreateFunctionDefinitionVersion.yaml"

requestUpdateThingRuntimeConfiguration :: UpdateThingRuntimeConfiguration -> TestTree
requestUpdateThingRuntimeConfiguration =
  req
    "UpdateThingRuntimeConfiguration"
    "fixture/UpdateThingRuntimeConfiguration.yaml"

requestGetFunctionDefinition :: GetFunctionDefinition -> TestTree
requestGetFunctionDefinition =
  req
    "GetFunctionDefinition"
    "fixture/GetFunctionDefinition.yaml"

requestStartBulkDeployment :: StartBulkDeployment -> TestTree
requestStartBulkDeployment =
  req
    "StartBulkDeployment"
    "fixture/StartBulkDeployment.yaml"

requestGetThingRuntimeConfiguration :: GetThingRuntimeConfiguration -> TestTree
requestGetThingRuntimeConfiguration =
  req
    "GetThingRuntimeConfiguration"
    "fixture/GetThingRuntimeConfiguration.yaml"

requestListResourceDefinitions :: ListResourceDefinitions -> TestTree
requestListResourceDefinitions =
  req
    "ListResourceDefinitions"
    "fixture/ListResourceDefinitions.yaml"

requestListDeviceDefinitionVersions :: ListDeviceDefinitionVersions -> TestTree
requestListDeviceDefinitionVersions =
  req
    "ListDeviceDefinitionVersions"
    "fixture/ListDeviceDefinitionVersions.yaml"

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

requestUpdateResourceDefinition :: UpdateResourceDefinition -> TestTree
requestUpdateResourceDefinition =
  req
    "UpdateResourceDefinition"
    "fixture/UpdateResourceDefinition.yaml"

requestGetGroupVersion :: GetGroupVersion -> TestTree
requestGetGroupVersion =
  req
    "GetGroupVersion"
    "fixture/GetGroupVersion.yaml"

requestCreateDeviceDefinitionVersion :: CreateDeviceDefinitionVersion -> TestTree
requestCreateDeviceDefinitionVersion =
  req
    "CreateDeviceDefinitionVersion"
    "fixture/CreateDeviceDefinitionVersion.yaml"

requestCreateResourceDefinition :: CreateResourceDefinition -> TestTree
requestCreateResourceDefinition =
  req
    "CreateResourceDefinition"
    "fixture/CreateResourceDefinition.yaml"

requestGetDeviceDefinition :: GetDeviceDefinition -> TestTree
requestGetDeviceDefinition =
  req
    "GetDeviceDefinition"
    "fixture/GetDeviceDefinition.yaml"

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

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestGetResourceDefinitionVersion :: GetResourceDefinitionVersion -> TestTree
requestGetResourceDefinitionVersion =
  req
    "GetResourceDefinitionVersion"
    "fixture/GetResourceDefinitionVersion.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListLoggerDefinitions :: ListLoggerDefinitions -> TestTree
requestListLoggerDefinitions =
  req
    "ListLoggerDefinitions"
    "fixture/ListLoggerDefinitions.yaml"

requestDeleteLoggerDefinition :: DeleteLoggerDefinition -> TestTree
requestDeleteLoggerDefinition =
  req
    "DeleteLoggerDefinition"
    "fixture/DeleteLoggerDefinition.yaml"

requestListDeployments :: ListDeployments -> TestTree
requestListDeployments =
  req
    "ListDeployments"
    "fixture/ListDeployments.yaml"

requestCreateSubscriptionDefinitionVersion :: CreateSubscriptionDefinitionVersion -> TestTree
requestCreateSubscriptionDefinitionVersion =
  req
    "CreateSubscriptionDefinitionVersion"
    "fixture/CreateSubscriptionDefinitionVersion.yaml"

requestCreateCoreDefinitionVersion :: CreateCoreDefinitionVersion -> TestTree
requestCreateCoreDefinitionVersion =
  req
    "CreateCoreDefinitionVersion"
    "fixture/CreateCoreDefinitionVersion.yaml"

requestCreateConnectorDefinitionVersion :: CreateConnectorDefinitionVersion -> TestTree
requestCreateConnectorDefinitionVersion =
  req
    "CreateConnectorDefinitionVersion"
    "fixture/CreateConnectorDefinitionVersion.yaml"

requestListBulkDeployments :: ListBulkDeployments -> TestTree
requestListBulkDeployments =
  req
    "ListBulkDeployments"
    "fixture/ListBulkDeployments.yaml"

requestUpdateLoggerDefinition :: UpdateLoggerDefinition -> TestTree
requestUpdateLoggerDefinition =
  req
    "UpdateLoggerDefinition"
    "fixture/UpdateLoggerDefinition.yaml"

requestCreateSoftwareUpdateJob :: CreateSoftwareUpdateJob -> TestTree
requestCreateSoftwareUpdateJob =
  req
    "CreateSoftwareUpdateJob"
    "fixture/CreateSoftwareUpdateJob.yaml"

requestListSubscriptionDefinitionVersions :: ListSubscriptionDefinitionVersions -> TestTree
requestListSubscriptionDefinitionVersions =
  req
    "ListSubscriptionDefinitionVersions"
    "fixture/ListSubscriptionDefinitionVersions.yaml"

requestGetConnectivityInfo :: GetConnectivityInfo -> TestTree
requestGetConnectivityInfo =
  req
    "GetConnectivityInfo"
    "fixture/GetConnectivityInfo.yaml"

requestListConnectorDefinitionVersions :: ListConnectorDefinitionVersions -> TestTree
requestListConnectorDefinitionVersions =
  req
    "ListConnectorDefinitionVersions"
    "fixture/ListConnectorDefinitionVersions.yaml"

requestListCoreDefinitionVersions :: ListCoreDefinitionVersions -> TestTree
requestListCoreDefinitionVersions =
  req
    "ListCoreDefinitionVersions"
    "fixture/ListCoreDefinitionVersions.yaml"

requestGetAssociatedRole :: GetAssociatedRole -> TestTree
requestGetAssociatedRole =
  req
    "GetAssociatedRole"
    "fixture/GetAssociatedRole.yaml"

requestCreateCoreDefinition :: CreateCoreDefinition -> TestTree
requestCreateCoreDefinition =
  req
    "CreateCoreDefinition"
    "fixture/CreateCoreDefinition.yaml"

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

requestCreateConnectorDefinition :: CreateConnectorDefinition -> TestTree
requestCreateConnectorDefinition =
  req
    "CreateConnectorDefinition"
    "fixture/CreateConnectorDefinition.yaml"

requestListConnectorDefinitions :: ListConnectorDefinitions -> TestTree
requestListConnectorDefinitions =
  req
    "ListConnectorDefinitions"
    "fixture/ListConnectorDefinitions.yaml"

requestGetLoggerDefinition :: GetLoggerDefinition -> TestTree
requestGetLoggerDefinition =
  req
    "GetLoggerDefinition"
    "fixture/GetLoggerDefinition.yaml"

requestDeleteConnectorDefinition :: DeleteConnectorDefinition -> TestTree
requestDeleteConnectorDefinition =
  req
    "DeleteConnectorDefinition"
    "fixture/DeleteConnectorDefinition.yaml"

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

requestUpdateConnectorDefinition :: UpdateConnectorDefinition -> TestTree
requestUpdateConnectorDefinition =
  req
    "UpdateConnectorDefinition"
    "fixture/UpdateConnectorDefinition.yaml"

requestListSubscriptionDefinitions :: ListSubscriptionDefinitions -> TestTree
requestListSubscriptionDefinitions =
  req
    "ListSubscriptionDefinitions"
    "fixture/ListSubscriptionDefinitions.yaml"

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

requestDeleteDeviceDefinition :: DeleteDeviceDefinition -> TestTree
requestDeleteDeviceDefinition =
  req
    "DeleteDeviceDefinition"
    "fixture/DeleteDeviceDefinition.yaml"

requestDisassociateServiceRoleFromAccount :: DisassociateServiceRoleFromAccount -> TestTree
requestDisassociateServiceRoleFromAccount =
  req
    "DisassociateServiceRoleFromAccount"
    "fixture/DisassociateServiceRoleFromAccount.yaml"

requestListDeviceDefinitions :: ListDeviceDefinitions -> TestTree
requestListDeviceDefinitions =
  req
    "ListDeviceDefinitions"
    "fixture/ListDeviceDefinitions.yaml"

requestListGroupVersions :: ListGroupVersions -> TestTree
requestListGroupVersions =
  req
    "ListGroupVersions"
    "fixture/ListGroupVersions.yaml"

requestUpdateDeviceDefinition :: UpdateDeviceDefinition -> TestTree
requestUpdateDeviceDefinition =
  req
    "UpdateDeviceDefinition"
    "fixture/UpdateDeviceDefinition.yaml"

requestListResourceDefinitionVersions :: ListResourceDefinitionVersions -> TestTree
requestListResourceDefinitionVersions =
  req
    "ListResourceDefinitionVersions"
    "fixture/ListResourceDefinitionVersions.yaml"

requestCreateDeviceDefinition :: CreateDeviceDefinition -> TestTree
requestCreateDeviceDefinition =
  req
    "CreateDeviceDefinition"
    "fixture/CreateDeviceDefinition.yaml"

requestGetResourceDefinition :: GetResourceDefinition -> TestTree
requestGetResourceDefinition =
  req
    "GetResourceDefinition"
    "fixture/GetResourceDefinition.yaml"

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

requestGetDeviceDefinitionVersion :: GetDeviceDefinitionVersion -> TestTree
requestGetDeviceDefinitionVersion =
  req
    "GetDeviceDefinitionVersion"
    "fixture/GetDeviceDefinitionVersion.yaml"

requestGetGroup :: GetGroup -> TestTree
requestGetGroup =
  req
    "GetGroup"
    "fixture/GetGroup.yaml"

requestDeleteGroup :: DeleteGroup -> TestTree
requestDeleteGroup =
  req
    "DeleteGroup"
    "fixture/DeleteGroup.yaml"

requestListFunctionDefinitions :: ListFunctionDefinitions -> TestTree
requestListFunctionDefinitions =
  req
    "ListFunctionDefinitions"
    "fixture/ListFunctionDefinitions.yaml"

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

requestGetDeploymentStatus :: GetDeploymentStatus -> TestTree
requestGetDeploymentStatus =
  req
    "GetDeploymentStatus"
    "fixture/GetDeploymentStatus.yaml"

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

requestCreateFunctionDefinition :: CreateFunctionDefinition -> TestTree
requestCreateFunctionDefinition =
  req
    "CreateFunctionDefinition"
    "fixture/CreateFunctionDefinition.yaml"

requestGetConnectorDefinition :: GetConnectorDefinition -> TestTree
requestGetConnectorDefinition =
  req
    "GetConnectorDefinition"
    "fixture/GetConnectorDefinition.yaml"

requestGetSubscriptionDefinition :: GetSubscriptionDefinition -> TestTree
requestGetSubscriptionDefinition =
  req
    "GetSubscriptionDefinition"
    "fixture/GetSubscriptionDefinition.yaml"

requestGetCoreDefinition :: GetCoreDefinition -> TestTree
requestGetCoreDefinition =
  req
    "GetCoreDefinition"
    "fixture/GetCoreDefinition.yaml"

requestGetServiceRoleForAccount :: GetServiceRoleForAccount -> TestTree
requestGetServiceRoleForAccount =
  req
    "GetServiceRoleForAccount"
    "fixture/GetServiceRoleForAccount.yaml"

requestCreateDeployment :: CreateDeployment -> TestTree
requestCreateDeployment =
  req
    "CreateDeployment"
    "fixture/CreateDeployment.yaml"

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

requestGetGroupCertificateAuthority :: GetGroupCertificateAuthority -> TestTree
requestGetGroupCertificateAuthority =
  req
    "GetGroupCertificateAuthority"
    "fixture/GetGroupCertificateAuthority.yaml"

requestGetConnectorDefinitionVersion :: GetConnectorDefinitionVersion -> TestTree
requestGetConnectorDefinitionVersion =
  req
    "GetConnectorDefinitionVersion"
    "fixture/GetConnectorDefinitionVersion.yaml"

requestListLoggerDefinitionVersions :: ListLoggerDefinitionVersions -> TestTree
requestListLoggerDefinitionVersions =
  req
    "ListLoggerDefinitionVersions"
    "fixture/ListLoggerDefinitionVersions.yaml"

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

requestGetCoreDefinitionVersion :: GetCoreDefinitionVersion -> TestTree
requestGetCoreDefinitionVersion =
  req
    "GetCoreDefinitionVersion"
    "fixture/GetCoreDefinitionVersion.yaml"

-- Responses

responseDeleteCoreDefinition :: DeleteCoreDefinitionResponse -> TestTree
responseDeleteCoreDefinition =
  res
    "DeleteCoreDefinitionResponse"
    "fixture/DeleteCoreDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteCoreDefinition)

responseUpdateCoreDefinition :: UpdateCoreDefinitionResponse -> TestTree
responseUpdateCoreDefinition =
  res
    "UpdateCoreDefinitionResponse"
    "fixture/UpdateCoreDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateCoreDefinition)

responseDeleteSubscriptionDefinition :: DeleteSubscriptionDefinitionResponse -> TestTree
responseDeleteSubscriptionDefinition =
  res
    "DeleteSubscriptionDefinitionResponse"
    "fixture/DeleteSubscriptionDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteSubscriptionDefinition)

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

responseGetGroupCertificateConfiguration :: GetGroupCertificateConfigurationResponse -> TestTree
responseGetGroupCertificateConfiguration =
  res
    "GetGroupCertificateConfigurationResponse"
    "fixture/GetGroupCertificateConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetGroupCertificateConfiguration)

responseAssociateRoleToGroup :: AssociateRoleToGroupResponse -> TestTree
responseAssociateRoleToGroup =
  res
    "AssociateRoleToGroupResponse"
    "fixture/AssociateRoleToGroupResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateRoleToGroup)

responseListFunctionDefinitionVersions :: ListFunctionDefinitionVersionsResponse -> TestTree
responseListFunctionDefinitionVersions =
  res
    "ListFunctionDefinitionVersionsResponse"
    "fixture/ListFunctionDefinitionVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListFunctionDefinitionVersions)

responseStopBulkDeployment :: StopBulkDeploymentResponse -> TestTree
responseStopBulkDeployment =
  res
    "StopBulkDeploymentResponse"
    "fixture/StopBulkDeploymentResponse.proto"
    defaultService
    (Proxy :: Proxy StopBulkDeployment)

responseCreateFunctionDefinitionVersion :: CreateFunctionDefinitionVersionResponse -> TestTree
responseCreateFunctionDefinitionVersion =
  res
    "CreateFunctionDefinitionVersionResponse"
    "fixture/CreateFunctionDefinitionVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateFunctionDefinitionVersion)

responseUpdateThingRuntimeConfiguration :: UpdateThingRuntimeConfigurationResponse -> TestTree
responseUpdateThingRuntimeConfiguration =
  res
    "UpdateThingRuntimeConfigurationResponse"
    "fixture/UpdateThingRuntimeConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateThingRuntimeConfiguration)

responseGetFunctionDefinition :: GetFunctionDefinitionResponse -> TestTree
responseGetFunctionDefinition =
  res
    "GetFunctionDefinitionResponse"
    "fixture/GetFunctionDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy GetFunctionDefinition)

responseStartBulkDeployment :: StartBulkDeploymentResponse -> TestTree
responseStartBulkDeployment =
  res
    "StartBulkDeploymentResponse"
    "fixture/StartBulkDeploymentResponse.proto"
    defaultService
    (Proxy :: Proxy StartBulkDeployment)

responseGetThingRuntimeConfiguration :: GetThingRuntimeConfigurationResponse -> TestTree
responseGetThingRuntimeConfiguration =
  res
    "GetThingRuntimeConfigurationResponse"
    "fixture/GetThingRuntimeConfigurationResponse.proto"
    defaultService
    (Proxy :: Proxy GetThingRuntimeConfiguration)

responseListResourceDefinitions :: ListResourceDefinitionsResponse -> TestTree
responseListResourceDefinitions =
  res
    "ListResourceDefinitionsResponse"
    "fixture/ListResourceDefinitionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListResourceDefinitions)

responseListDeviceDefinitionVersions :: ListDeviceDefinitionVersionsResponse -> TestTree
responseListDeviceDefinitionVersions =
  res
    "ListDeviceDefinitionVersionsResponse"
    "fixture/ListDeviceDefinitionVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDeviceDefinitionVersions)

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

responseUpdateResourceDefinition :: UpdateResourceDefinitionResponse -> TestTree
responseUpdateResourceDefinition =
  res
    "UpdateResourceDefinitionResponse"
    "fixture/UpdateResourceDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateResourceDefinition)

responseGetGroupVersion :: GetGroupVersionResponse -> TestTree
responseGetGroupVersion =
  res
    "GetGroupVersionResponse"
    "fixture/GetGroupVersionResponse.proto"
    defaultService
    (Proxy :: Proxy GetGroupVersion)

responseCreateDeviceDefinitionVersion :: CreateDeviceDefinitionVersionResponse -> TestTree
responseCreateDeviceDefinitionVersion =
  res
    "CreateDeviceDefinitionVersionResponse"
    "fixture/CreateDeviceDefinitionVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDeviceDefinitionVersion)

responseCreateResourceDefinition :: CreateResourceDefinitionResponse -> TestTree
responseCreateResourceDefinition =
  res
    "CreateResourceDefinitionResponse"
    "fixture/CreateResourceDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateResourceDefinition)

responseGetDeviceDefinition :: GetDeviceDefinitionResponse -> TestTree
responseGetDeviceDefinition =
  res
    "GetDeviceDefinitionResponse"
    "fixture/GetDeviceDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy GetDeviceDefinition)

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

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseGetResourceDefinitionVersion :: GetResourceDefinitionVersionResponse -> TestTree
responseGetResourceDefinitionVersion =
  res
    "GetResourceDefinitionVersionResponse"
    "fixture/GetResourceDefinitionVersionResponse.proto"
    defaultService
    (Proxy :: Proxy GetResourceDefinitionVersion)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseListLoggerDefinitions :: ListLoggerDefinitionsResponse -> TestTree
responseListLoggerDefinitions =
  res
    "ListLoggerDefinitionsResponse"
    "fixture/ListLoggerDefinitionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListLoggerDefinitions)

responseDeleteLoggerDefinition :: DeleteLoggerDefinitionResponse -> TestTree
responseDeleteLoggerDefinition =
  res
    "DeleteLoggerDefinitionResponse"
    "fixture/DeleteLoggerDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteLoggerDefinition)

responseListDeployments :: ListDeploymentsResponse -> TestTree
responseListDeployments =
  res
    "ListDeploymentsResponse"
    "fixture/ListDeploymentsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDeployments)

responseCreateSubscriptionDefinitionVersion :: CreateSubscriptionDefinitionVersionResponse -> TestTree
responseCreateSubscriptionDefinitionVersion =
  res
    "CreateSubscriptionDefinitionVersionResponse"
    "fixture/CreateSubscriptionDefinitionVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSubscriptionDefinitionVersion)

responseCreateCoreDefinitionVersion :: CreateCoreDefinitionVersionResponse -> TestTree
responseCreateCoreDefinitionVersion =
  res
    "CreateCoreDefinitionVersionResponse"
    "fixture/CreateCoreDefinitionVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCoreDefinitionVersion)

responseCreateConnectorDefinitionVersion :: CreateConnectorDefinitionVersionResponse -> TestTree
responseCreateConnectorDefinitionVersion =
  res
    "CreateConnectorDefinitionVersionResponse"
    "fixture/CreateConnectorDefinitionVersionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateConnectorDefinitionVersion)

responseListBulkDeployments :: ListBulkDeploymentsResponse -> TestTree
responseListBulkDeployments =
  res
    "ListBulkDeploymentsResponse"
    "fixture/ListBulkDeploymentsResponse.proto"
    defaultService
    (Proxy :: Proxy ListBulkDeployments)

responseUpdateLoggerDefinition :: UpdateLoggerDefinitionResponse -> TestTree
responseUpdateLoggerDefinition =
  res
    "UpdateLoggerDefinitionResponse"
    "fixture/UpdateLoggerDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateLoggerDefinition)

responseCreateSoftwareUpdateJob :: CreateSoftwareUpdateJobResponse -> TestTree
responseCreateSoftwareUpdateJob =
  res
    "CreateSoftwareUpdateJobResponse"
    "fixture/CreateSoftwareUpdateJobResponse.proto"
    defaultService
    (Proxy :: Proxy CreateSoftwareUpdateJob)

responseListSubscriptionDefinitionVersions :: ListSubscriptionDefinitionVersionsResponse -> TestTree
responseListSubscriptionDefinitionVersions =
  res
    "ListSubscriptionDefinitionVersionsResponse"
    "fixture/ListSubscriptionDefinitionVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSubscriptionDefinitionVersions)

responseGetConnectivityInfo :: GetConnectivityInfoResponse -> TestTree
responseGetConnectivityInfo =
  res
    "GetConnectivityInfoResponse"
    "fixture/GetConnectivityInfoResponse.proto"
    defaultService
    (Proxy :: Proxy GetConnectivityInfo)

responseListConnectorDefinitionVersions :: ListConnectorDefinitionVersionsResponse -> TestTree
responseListConnectorDefinitionVersions =
  res
    "ListConnectorDefinitionVersionsResponse"
    "fixture/ListConnectorDefinitionVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListConnectorDefinitionVersions)

responseListCoreDefinitionVersions :: ListCoreDefinitionVersionsResponse -> TestTree
responseListCoreDefinitionVersions =
  res
    "ListCoreDefinitionVersionsResponse"
    "fixture/ListCoreDefinitionVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListCoreDefinitionVersions)

responseGetAssociatedRole :: GetAssociatedRoleResponse -> TestTree
responseGetAssociatedRole =
  res
    "GetAssociatedRoleResponse"
    "fixture/GetAssociatedRoleResponse.proto"
    defaultService
    (Proxy :: Proxy GetAssociatedRole)

responseCreateCoreDefinition :: CreateCoreDefinitionResponse -> TestTree
responseCreateCoreDefinition =
  res
    "CreateCoreDefinitionResponse"
    "fixture/CreateCoreDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateCoreDefinition)

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

responseCreateConnectorDefinition :: CreateConnectorDefinitionResponse -> TestTree
responseCreateConnectorDefinition =
  res
    "CreateConnectorDefinitionResponse"
    "fixture/CreateConnectorDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateConnectorDefinition)

responseListConnectorDefinitions :: ListConnectorDefinitionsResponse -> TestTree
responseListConnectorDefinitions =
  res
    "ListConnectorDefinitionsResponse"
    "fixture/ListConnectorDefinitionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListConnectorDefinitions)

responseGetLoggerDefinition :: GetLoggerDefinitionResponse -> TestTree
responseGetLoggerDefinition =
  res
    "GetLoggerDefinitionResponse"
    "fixture/GetLoggerDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy GetLoggerDefinition)

responseDeleteConnectorDefinition :: DeleteConnectorDefinitionResponse -> TestTree
responseDeleteConnectorDefinition =
  res
    "DeleteConnectorDefinitionResponse"
    "fixture/DeleteConnectorDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteConnectorDefinition)

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

responseUpdateConnectorDefinition :: UpdateConnectorDefinitionResponse -> TestTree
responseUpdateConnectorDefinition =
  res
    "UpdateConnectorDefinitionResponse"
    "fixture/UpdateConnectorDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateConnectorDefinition)

responseListSubscriptionDefinitions :: ListSubscriptionDefinitionsResponse -> TestTree
responseListSubscriptionDefinitions =
  res
    "ListSubscriptionDefinitionsResponse"
    "fixture/ListSubscriptionDefinitionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListSubscriptionDefinitions)

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

responseDeleteDeviceDefinition :: DeleteDeviceDefinitionResponse -> TestTree
responseDeleteDeviceDefinition =
  res
    "DeleteDeviceDefinitionResponse"
    "fixture/DeleteDeviceDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDeviceDefinition)

responseDisassociateServiceRoleFromAccount :: DisassociateServiceRoleFromAccountResponse -> TestTree
responseDisassociateServiceRoleFromAccount =
  res
    "DisassociateServiceRoleFromAccountResponse"
    "fixture/DisassociateServiceRoleFromAccountResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateServiceRoleFromAccount)

responseListDeviceDefinitions :: ListDeviceDefinitionsResponse -> TestTree
responseListDeviceDefinitions =
  res
    "ListDeviceDefinitionsResponse"
    "fixture/ListDeviceDefinitionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDeviceDefinitions)

responseListGroupVersions :: ListGroupVersionsResponse -> TestTree
responseListGroupVersions =
  res
    "ListGroupVersionsResponse"
    "fixture/ListGroupVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListGroupVersions)

responseUpdateDeviceDefinition :: UpdateDeviceDefinitionResponse -> TestTree
responseUpdateDeviceDefinition =
  res
    "UpdateDeviceDefinitionResponse"
    "fixture/UpdateDeviceDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDeviceDefinition)

responseListResourceDefinitionVersions :: ListResourceDefinitionVersionsResponse -> TestTree
responseListResourceDefinitionVersions =
  res
    "ListResourceDefinitionVersionsResponse"
    "fixture/ListResourceDefinitionVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListResourceDefinitionVersions)

responseCreateDeviceDefinition :: CreateDeviceDefinitionResponse -> TestTree
responseCreateDeviceDefinition =
  res
    "CreateDeviceDefinitionResponse"
    "fixture/CreateDeviceDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDeviceDefinition)

responseGetResourceDefinition :: GetResourceDefinitionResponse -> TestTree
responseGetResourceDefinition =
  res
    "GetResourceDefinitionResponse"
    "fixture/GetResourceDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy GetResourceDefinition)

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

responseGetDeviceDefinitionVersion :: GetDeviceDefinitionVersionResponse -> TestTree
responseGetDeviceDefinitionVersion =
  res
    "GetDeviceDefinitionVersionResponse"
    "fixture/GetDeviceDefinitionVersionResponse.proto"
    defaultService
    (Proxy :: Proxy GetDeviceDefinitionVersion)

responseGetGroup :: GetGroupResponse -> TestTree
responseGetGroup =
  res
    "GetGroupResponse"
    "fixture/GetGroupResponse.proto"
    defaultService
    (Proxy :: Proxy GetGroup)

responseDeleteGroup :: DeleteGroupResponse -> TestTree
responseDeleteGroup =
  res
    "DeleteGroupResponse"
    "fixture/DeleteGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteGroup)

responseListFunctionDefinitions :: ListFunctionDefinitionsResponse -> TestTree
responseListFunctionDefinitions =
  res
    "ListFunctionDefinitionsResponse"
    "fixture/ListFunctionDefinitionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListFunctionDefinitions)

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

responseGetDeploymentStatus :: GetDeploymentStatusResponse -> TestTree
responseGetDeploymentStatus =
  res
    "GetDeploymentStatusResponse"
    "fixture/GetDeploymentStatusResponse.proto"
    defaultService
    (Proxy :: Proxy GetDeploymentStatus)

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

responseCreateFunctionDefinition :: CreateFunctionDefinitionResponse -> TestTree
responseCreateFunctionDefinition =
  res
    "CreateFunctionDefinitionResponse"
    "fixture/CreateFunctionDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateFunctionDefinition)

responseGetConnectorDefinition :: GetConnectorDefinitionResponse -> TestTree
responseGetConnectorDefinition =
  res
    "GetConnectorDefinitionResponse"
    "fixture/GetConnectorDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy GetConnectorDefinition)

responseGetSubscriptionDefinition :: GetSubscriptionDefinitionResponse -> TestTree
responseGetSubscriptionDefinition =
  res
    "GetSubscriptionDefinitionResponse"
    "fixture/GetSubscriptionDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy GetSubscriptionDefinition)

responseGetCoreDefinition :: GetCoreDefinitionResponse -> TestTree
responseGetCoreDefinition =
  res
    "GetCoreDefinitionResponse"
    "fixture/GetCoreDefinitionResponse.proto"
    defaultService
    (Proxy :: Proxy GetCoreDefinition)

responseGetServiceRoleForAccount :: GetServiceRoleForAccountResponse -> TestTree
responseGetServiceRoleForAccount =
  res
    "GetServiceRoleForAccountResponse"
    "fixture/GetServiceRoleForAccountResponse.proto"
    defaultService
    (Proxy :: Proxy GetServiceRoleForAccount)

responseCreateDeployment :: CreateDeploymentResponse -> TestTree
responseCreateDeployment =
  res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDeployment)

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

responseGetGroupCertificateAuthority :: GetGroupCertificateAuthorityResponse -> TestTree
responseGetGroupCertificateAuthority =
  res
    "GetGroupCertificateAuthorityResponse"
    "fixture/GetGroupCertificateAuthorityResponse.proto"
    defaultService
    (Proxy :: Proxy GetGroupCertificateAuthority)

responseGetConnectorDefinitionVersion :: GetConnectorDefinitionVersionResponse -> TestTree
responseGetConnectorDefinitionVersion =
  res
    "GetConnectorDefinitionVersionResponse"
    "fixture/GetConnectorDefinitionVersionResponse.proto"
    defaultService
    (Proxy :: Proxy GetConnectorDefinitionVersion)

responseListLoggerDefinitionVersions :: ListLoggerDefinitionVersionsResponse -> TestTree
responseListLoggerDefinitionVersions =
  res
    "ListLoggerDefinitionVersionsResponse"
    "fixture/ListLoggerDefinitionVersionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListLoggerDefinitionVersions)

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

responseGetCoreDefinitionVersion :: GetCoreDefinitionVersionResponse -> TestTree
responseGetCoreDefinitionVersion =
  res
    "GetCoreDefinitionVersionResponse"
    "fixture/GetCoreDefinitionVersionResponse.proto"
    defaultService
    (Proxy :: Proxy GetCoreDefinitionVersion)
