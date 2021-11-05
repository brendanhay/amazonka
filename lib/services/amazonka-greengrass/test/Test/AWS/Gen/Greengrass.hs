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

import Amazonka.Greengrass
import qualified Data.Proxy as Proxy
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
--         [ requestGetGroupCertificateConfiguration $
--             newGetGroupCertificateConfiguration
--
--         , requestListGroupVersions $
--             newListGroupVersions
--
--         , requestListFunctionDefinitionVersions $
--             newListFunctionDefinitionVersions
--
--         , requestListDeviceDefinitions $
--             newListDeviceDefinitions
--
--         , requestAssociateRoleToGroup $
--             newAssociateRoleToGroup
--
--         , requestUpdateCoreDefinition $
--             newUpdateCoreDefinition
--
--         , requestDeleteCoreDefinition $
--             newDeleteCoreDefinition
--
--         , requestGetLoggerDefinition $
--             newGetLoggerDefinition
--
--         , requestListGroupCertificateAuthorities $
--             newListGroupCertificateAuthorities
--
--         , requestDisassociateRoleFromGroup $
--             newDisassociateRoleFromGroup
--
--         , requestUpdateSubscriptionDefinition $
--             newUpdateSubscriptionDefinition
--
--         , requestDeleteSubscriptionDefinition $
--             newDeleteSubscriptionDefinition
--
--         , requestListCoreDefinitions $
--             newListCoreDefinitions
--
--         , requestListSubscriptionDefinitions $
--             newListSubscriptionDefinitions
--
--         , requestCreateGroupCertificateAuthority $
--             newCreateGroupCertificateAuthority
--
--         , requestDeleteConnectorDefinition $
--             newDeleteConnectorDefinition
--
--         , requestUpdateConnectorDefinition $
--             newUpdateConnectorDefinition
--
--         , requestCreateLoggerDefinitionVersion $
--             newCreateLoggerDefinitionVersion
--
--         , requestCreateCoreDefinition $
--             newCreateCoreDefinition
--
--         , requestGetConnectorDefinitionVersion $
--             newGetConnectorDefinitionVersion
--
--         , requestUpdateConnectivityInfo $
--             newUpdateConnectivityInfo
--
--         , requestCreateSubscriptionDefinition $
--             newCreateSubscriptionDefinition
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestGetGroupCertificateAuthority $
--             newGetGroupCertificateAuthority
--
--         , requestGetLoggerDefinitionVersion $
--             newGetLoggerDefinitionVersion
--
--         , requestGetServiceRoleForAccount $
--             newGetServiceRoleForAccount
--
--         , requestListConnectorDefinitionVersions $
--             newListConnectorDefinitionVersions
--
--         , requestCreateSoftwareUpdateJob $
--             newCreateSoftwareUpdateJob
--
--         , requestCreateLoggerDefinition $
--             newCreateLoggerDefinition
--
--         , requestGetConnectivityInfo $
--             newGetConnectivityInfo
--
--         , requestCreateDeployment $
--             newCreateDeployment
--
--         , requestDeleteLoggerDefinition $
--             newDeleteLoggerDefinition
--
--         , requestUpdateLoggerDefinition $
--             newUpdateLoggerDefinition
--
--         , requestGetSubscriptionDefinition $
--             newGetSubscriptionDefinition
--
--         , requestGetCoreDefinition $
--             newGetCoreDefinition
--
--         , requestCreateConnectorDefinitionVersion $
--             newCreateConnectorDefinitionVersion
--
--         , requestGetDeploymentStatus $
--             newGetDeploymentStatus
--
--         , requestGetBulkDeploymentStatus $
--             newGetBulkDeploymentStatus
--
--         , requestCreateResourceDefinition $
--             newCreateResourceDefinition
--
--         , requestGetResourceDefinitionVersion $
--             newGetResourceDefinitionVersion
--
--         , requestUpdateFunctionDefinition $
--             newUpdateFunctionDefinition
--
--         , requestDeleteFunctionDefinition $
--             newDeleteFunctionDefinition
--
--         , requestListResourceDefinitions $
--             newListResourceDefinitions
--
--         , requestStopBulkDeployment $
--             newStopBulkDeployment
--
--         , requestCreateResourceDefinitionVersion $
--             newCreateResourceDefinitionVersion
--
--         , requestGetResourceDefinition $
--             newGetResourceDefinition
--
--         , requestListResourceDefinitionVersions $
--             newListResourceDefinitionVersions
--
--         , requestDisassociateServiceRoleFromAccount $
--             newDisassociateServiceRoleFromAccount
--
--         , requestDeleteDeviceDefinition $
--             newDeleteDeviceDefinition
--
--         , requestUpdateDeviceDefinition $
--             newUpdateDeviceDefinition
--
--         , requestAssociateServiceRoleToAccount $
--             newAssociateServiceRoleToAccount
--
--         , requestResetDeployments $
--             newResetDeployments
--
--         , requestListConnectorDefinitions $
--             newListConnectorDefinitions
--
--         , requestGetSubscriptionDefinitionVersion $
--             newGetSubscriptionDefinitionVersion
--
--         , requestGetAssociatedRole $
--             newGetAssociatedRole
--
--         , requestListLoggerDefinitionVersions $
--             newListLoggerDefinitionVersions
--
--         , requestCreateConnectorDefinition $
--             newCreateConnectorDefinition
--
--         , requestGetCoreDefinitionVersion $
--             newGetCoreDefinitionVersion
--
--         , requestListSubscriptionDefinitionVersions $
--             newListSubscriptionDefinitionVersions
--
--         , requestListCoreDefinitionVersions $
--             newListCoreDefinitionVersions
--
--         , requestCreateCoreDefinitionVersion $
--             newCreateCoreDefinitionVersion
--
--         , requestListBulkDeployments $
--             newListBulkDeployments
--
--         , requestListDeployments $
--             newListDeployments
--
--         , requestGetConnectorDefinition $
--             newGetConnectorDefinition
--
--         , requestListLoggerDefinitions $
--             newListLoggerDefinitions
--
--         , requestTagResource $
--             newTagResource
--
--         , requestCreateSubscriptionDefinitionVersion $
--             newCreateSubscriptionDefinitionVersion
--
--         , requestGetGroupVersion $
--             newGetGroupVersion
--
--         , requestUpdateGroupCertificateConfiguration $
--             newUpdateGroupCertificateConfiguration
--
--         , requestGetFunctionDefinitionVersion $
--             newGetFunctionDefinitionVersion
--
--         , requestGetDeviceDefinition $
--             newGetDeviceDefinition
--
--         , requestCreateGroup $
--             newCreateGroup
--
--         , requestCreateFunctionDefinition $
--             newCreateFunctionDefinition
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestCreateDeviceDefinitionVersion $
--             newCreateDeviceDefinitionVersion
--
--         , requestDeleteGroup $
--             newDeleteGroup
--
--         , requestUpdateGroup $
--             newUpdateGroup
--
--         , requestListGroups $
--             newListGroups
--
--         , requestListBulkDeploymentDetailedReports $
--             newListBulkDeploymentDetailedReports
--
--         , requestGetThingRuntimeConfiguration $
--             newGetThingRuntimeConfiguration
--
--         , requestDeleteResourceDefinition $
--             newDeleteResourceDefinition
--
--         , requestUpdateResourceDefinition $
--             newUpdateResourceDefinition
--
--         , requestListDeviceDefinitionVersions $
--             newListDeviceDefinitionVersions
--
--         , requestListFunctionDefinitions $
--             newListFunctionDefinitions
--
--         , requestGetFunctionDefinition $
--             newGetFunctionDefinition
--
--         , requestGetGroup $
--             newGetGroup
--
--         , requestCreateDeviceDefinition $
--             newCreateDeviceDefinition
--
--         , requestCreateGroupVersion $
--             newCreateGroupVersion
--
--         , requestCreateFunctionDefinitionVersion $
--             newCreateFunctionDefinitionVersion
--
--         , requestStartBulkDeployment $
--             newStartBulkDeployment
--
--         , requestUpdateThingRuntimeConfiguration $
--             newUpdateThingRuntimeConfiguration
--
--         , requestGetDeviceDefinitionVersion $
--             newGetDeviceDefinitionVersion
--
--           ]

--     , testGroup "response"
--         [ responseGetGroupCertificateConfiguration $
--             newGetGroupCertificateConfigurationResponse
--
--         , responseListGroupVersions $
--             newListGroupVersionsResponse
--
--         , responseListFunctionDefinitionVersions $
--             newListFunctionDefinitionVersionsResponse
--
--         , responseListDeviceDefinitions $
--             newListDeviceDefinitionsResponse
--
--         , responseAssociateRoleToGroup $
--             newAssociateRoleToGroupResponse
--
--         , responseUpdateCoreDefinition $
--             newUpdateCoreDefinitionResponse
--
--         , responseDeleteCoreDefinition $
--             newDeleteCoreDefinitionResponse
--
--         , responseGetLoggerDefinition $
--             newGetLoggerDefinitionResponse
--
--         , responseListGroupCertificateAuthorities $
--             newListGroupCertificateAuthoritiesResponse
--
--         , responseDisassociateRoleFromGroup $
--             newDisassociateRoleFromGroupResponse
--
--         , responseUpdateSubscriptionDefinition $
--             newUpdateSubscriptionDefinitionResponse
--
--         , responseDeleteSubscriptionDefinition $
--             newDeleteSubscriptionDefinitionResponse
--
--         , responseListCoreDefinitions $
--             newListCoreDefinitionsResponse
--
--         , responseListSubscriptionDefinitions $
--             newListSubscriptionDefinitionsResponse
--
--         , responseCreateGroupCertificateAuthority $
--             newCreateGroupCertificateAuthorityResponse
--
--         , responseDeleteConnectorDefinition $
--             newDeleteConnectorDefinitionResponse
--
--         , responseUpdateConnectorDefinition $
--             newUpdateConnectorDefinitionResponse
--
--         , responseCreateLoggerDefinitionVersion $
--             newCreateLoggerDefinitionVersionResponse
--
--         , responseCreateCoreDefinition $
--             newCreateCoreDefinitionResponse
--
--         , responseGetConnectorDefinitionVersion $
--             newGetConnectorDefinitionVersionResponse
--
--         , responseUpdateConnectivityInfo $
--             newUpdateConnectivityInfoResponse
--
--         , responseCreateSubscriptionDefinition $
--             newCreateSubscriptionDefinitionResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseGetGroupCertificateAuthority $
--             newGetGroupCertificateAuthorityResponse
--
--         , responseGetLoggerDefinitionVersion $
--             newGetLoggerDefinitionVersionResponse
--
--         , responseGetServiceRoleForAccount $
--             newGetServiceRoleForAccountResponse
--
--         , responseListConnectorDefinitionVersions $
--             newListConnectorDefinitionVersionsResponse
--
--         , responseCreateSoftwareUpdateJob $
--             newCreateSoftwareUpdateJobResponse
--
--         , responseCreateLoggerDefinition $
--             newCreateLoggerDefinitionResponse
--
--         , responseGetConnectivityInfo $
--             newGetConnectivityInfoResponse
--
--         , responseCreateDeployment $
--             newCreateDeploymentResponse
--
--         , responseDeleteLoggerDefinition $
--             newDeleteLoggerDefinitionResponse
--
--         , responseUpdateLoggerDefinition $
--             newUpdateLoggerDefinitionResponse
--
--         , responseGetSubscriptionDefinition $
--             newGetSubscriptionDefinitionResponse
--
--         , responseGetCoreDefinition $
--             newGetCoreDefinitionResponse
--
--         , responseCreateConnectorDefinitionVersion $
--             newCreateConnectorDefinitionVersionResponse
--
--         , responseGetDeploymentStatus $
--             newGetDeploymentStatusResponse
--
--         , responseGetBulkDeploymentStatus $
--             newGetBulkDeploymentStatusResponse
--
--         , responseCreateResourceDefinition $
--             newCreateResourceDefinitionResponse
--
--         , responseGetResourceDefinitionVersion $
--             newGetResourceDefinitionVersionResponse
--
--         , responseUpdateFunctionDefinition $
--             newUpdateFunctionDefinitionResponse
--
--         , responseDeleteFunctionDefinition $
--             newDeleteFunctionDefinitionResponse
--
--         , responseListResourceDefinitions $
--             newListResourceDefinitionsResponse
--
--         , responseStopBulkDeployment $
--             newStopBulkDeploymentResponse
--
--         , responseCreateResourceDefinitionVersion $
--             newCreateResourceDefinitionVersionResponse
--
--         , responseGetResourceDefinition $
--             newGetResourceDefinitionResponse
--
--         , responseListResourceDefinitionVersions $
--             newListResourceDefinitionVersionsResponse
--
--         , responseDisassociateServiceRoleFromAccount $
--             newDisassociateServiceRoleFromAccountResponse
--
--         , responseDeleteDeviceDefinition $
--             newDeleteDeviceDefinitionResponse
--
--         , responseUpdateDeviceDefinition $
--             newUpdateDeviceDefinitionResponse
--
--         , responseAssociateServiceRoleToAccount $
--             newAssociateServiceRoleToAccountResponse
--
--         , responseResetDeployments $
--             newResetDeploymentsResponse
--
--         , responseListConnectorDefinitions $
--             newListConnectorDefinitionsResponse
--
--         , responseGetSubscriptionDefinitionVersion $
--             newGetSubscriptionDefinitionVersionResponse
--
--         , responseGetAssociatedRole $
--             newGetAssociatedRoleResponse
--
--         , responseListLoggerDefinitionVersions $
--             newListLoggerDefinitionVersionsResponse
--
--         , responseCreateConnectorDefinition $
--             newCreateConnectorDefinitionResponse
--
--         , responseGetCoreDefinitionVersion $
--             newGetCoreDefinitionVersionResponse
--
--         , responseListSubscriptionDefinitionVersions $
--             newListSubscriptionDefinitionVersionsResponse
--
--         , responseListCoreDefinitionVersions $
--             newListCoreDefinitionVersionsResponse
--
--         , responseCreateCoreDefinitionVersion $
--             newCreateCoreDefinitionVersionResponse
--
--         , responseListBulkDeployments $
--             newListBulkDeploymentsResponse
--
--         , responseListDeployments $
--             newListDeploymentsResponse
--
--         , responseGetConnectorDefinition $
--             newGetConnectorDefinitionResponse
--
--         , responseListLoggerDefinitions $
--             newListLoggerDefinitionsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseCreateSubscriptionDefinitionVersion $
--             newCreateSubscriptionDefinitionVersionResponse
--
--         , responseGetGroupVersion $
--             newGetGroupVersionResponse
--
--         , responseUpdateGroupCertificateConfiguration $
--             newUpdateGroupCertificateConfigurationResponse
--
--         , responseGetFunctionDefinitionVersion $
--             newGetFunctionDefinitionVersionResponse
--
--         , responseGetDeviceDefinition $
--             newGetDeviceDefinitionResponse
--
--         , responseCreateGroup $
--             newCreateGroupResponse
--
--         , responseCreateFunctionDefinition $
--             newCreateFunctionDefinitionResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseCreateDeviceDefinitionVersion $
--             newCreateDeviceDefinitionVersionResponse
--
--         , responseDeleteGroup $
--             newDeleteGroupResponse
--
--         , responseUpdateGroup $
--             newUpdateGroupResponse
--
--         , responseListGroups $
--             newListGroupsResponse
--
--         , responseListBulkDeploymentDetailedReports $
--             newListBulkDeploymentDetailedReportsResponse
--
--         , responseGetThingRuntimeConfiguration $
--             newGetThingRuntimeConfigurationResponse
--
--         , responseDeleteResourceDefinition $
--             newDeleteResourceDefinitionResponse
--
--         , responseUpdateResourceDefinition $
--             newUpdateResourceDefinitionResponse
--
--         , responseListDeviceDefinitionVersions $
--             newListDeviceDefinitionVersionsResponse
--
--         , responseListFunctionDefinitions $
--             newListFunctionDefinitionsResponse
--
--         , responseGetFunctionDefinition $
--             newGetFunctionDefinitionResponse
--
--         , responseGetGroup $
--             newGetGroupResponse
--
--         , responseCreateDeviceDefinition $
--             newCreateDeviceDefinitionResponse
--
--         , responseCreateGroupVersion $
--             newCreateGroupVersionResponse
--
--         , responseCreateFunctionDefinitionVersion $
--             newCreateFunctionDefinitionVersionResponse
--
--         , responseStartBulkDeployment $
--             newStartBulkDeploymentResponse
--
--         , responseUpdateThingRuntimeConfiguration $
--             newUpdateThingRuntimeConfigurationResponse
--
--         , responseGetDeviceDefinitionVersion $
--             newGetDeviceDefinitionVersionResponse
--
--           ]
--     ]

-- Requests

requestGetGroupCertificateConfiguration :: GetGroupCertificateConfiguration -> TestTree
requestGetGroupCertificateConfiguration =
  req
    "GetGroupCertificateConfiguration"
    "fixture/GetGroupCertificateConfiguration.yaml"

requestListGroupVersions :: ListGroupVersions -> TestTree
requestListGroupVersions =
  req
    "ListGroupVersions"
    "fixture/ListGroupVersions.yaml"

requestListFunctionDefinitionVersions :: ListFunctionDefinitionVersions -> TestTree
requestListFunctionDefinitionVersions =
  req
    "ListFunctionDefinitionVersions"
    "fixture/ListFunctionDefinitionVersions.yaml"

requestListDeviceDefinitions :: ListDeviceDefinitions -> TestTree
requestListDeviceDefinitions =
  req
    "ListDeviceDefinitions"
    "fixture/ListDeviceDefinitions.yaml"

requestAssociateRoleToGroup :: AssociateRoleToGroup -> TestTree
requestAssociateRoleToGroup =
  req
    "AssociateRoleToGroup"
    "fixture/AssociateRoleToGroup.yaml"

requestUpdateCoreDefinition :: UpdateCoreDefinition -> TestTree
requestUpdateCoreDefinition =
  req
    "UpdateCoreDefinition"
    "fixture/UpdateCoreDefinition.yaml"

requestDeleteCoreDefinition :: DeleteCoreDefinition -> TestTree
requestDeleteCoreDefinition =
  req
    "DeleteCoreDefinition"
    "fixture/DeleteCoreDefinition.yaml"

requestGetLoggerDefinition :: GetLoggerDefinition -> TestTree
requestGetLoggerDefinition =
  req
    "GetLoggerDefinition"
    "fixture/GetLoggerDefinition.yaml"

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

requestUpdateSubscriptionDefinition :: UpdateSubscriptionDefinition -> TestTree
requestUpdateSubscriptionDefinition =
  req
    "UpdateSubscriptionDefinition"
    "fixture/UpdateSubscriptionDefinition.yaml"

requestDeleteSubscriptionDefinition :: DeleteSubscriptionDefinition -> TestTree
requestDeleteSubscriptionDefinition =
  req
    "DeleteSubscriptionDefinition"
    "fixture/DeleteSubscriptionDefinition.yaml"

requestListCoreDefinitions :: ListCoreDefinitions -> TestTree
requestListCoreDefinitions =
  req
    "ListCoreDefinitions"
    "fixture/ListCoreDefinitions.yaml"

requestListSubscriptionDefinitions :: ListSubscriptionDefinitions -> TestTree
requestListSubscriptionDefinitions =
  req
    "ListSubscriptionDefinitions"
    "fixture/ListSubscriptionDefinitions.yaml"

requestCreateGroupCertificateAuthority :: CreateGroupCertificateAuthority -> TestTree
requestCreateGroupCertificateAuthority =
  req
    "CreateGroupCertificateAuthority"
    "fixture/CreateGroupCertificateAuthority.yaml"

requestDeleteConnectorDefinition :: DeleteConnectorDefinition -> TestTree
requestDeleteConnectorDefinition =
  req
    "DeleteConnectorDefinition"
    "fixture/DeleteConnectorDefinition.yaml"

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

requestCreateCoreDefinition :: CreateCoreDefinition -> TestTree
requestCreateCoreDefinition =
  req
    "CreateCoreDefinition"
    "fixture/CreateCoreDefinition.yaml"

requestGetConnectorDefinitionVersion :: GetConnectorDefinitionVersion -> TestTree
requestGetConnectorDefinitionVersion =
  req
    "GetConnectorDefinitionVersion"
    "fixture/GetConnectorDefinitionVersion.yaml"

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

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestGetGroupCertificateAuthority :: GetGroupCertificateAuthority -> TestTree
requestGetGroupCertificateAuthority =
  req
    "GetGroupCertificateAuthority"
    "fixture/GetGroupCertificateAuthority.yaml"

requestGetLoggerDefinitionVersion :: GetLoggerDefinitionVersion -> TestTree
requestGetLoggerDefinitionVersion =
  req
    "GetLoggerDefinitionVersion"
    "fixture/GetLoggerDefinitionVersion.yaml"

requestGetServiceRoleForAccount :: GetServiceRoleForAccount -> TestTree
requestGetServiceRoleForAccount =
  req
    "GetServiceRoleForAccount"
    "fixture/GetServiceRoleForAccount.yaml"

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

requestCreateLoggerDefinition :: CreateLoggerDefinition -> TestTree
requestCreateLoggerDefinition =
  req
    "CreateLoggerDefinition"
    "fixture/CreateLoggerDefinition.yaml"

requestGetConnectivityInfo :: GetConnectivityInfo -> TestTree
requestGetConnectivityInfo =
  req
    "GetConnectivityInfo"
    "fixture/GetConnectivityInfo.yaml"

requestCreateDeployment :: CreateDeployment -> TestTree
requestCreateDeployment =
  req
    "CreateDeployment"
    "fixture/CreateDeployment.yaml"

requestDeleteLoggerDefinition :: DeleteLoggerDefinition -> TestTree
requestDeleteLoggerDefinition =
  req
    "DeleteLoggerDefinition"
    "fixture/DeleteLoggerDefinition.yaml"

requestUpdateLoggerDefinition :: UpdateLoggerDefinition -> TestTree
requestUpdateLoggerDefinition =
  req
    "UpdateLoggerDefinition"
    "fixture/UpdateLoggerDefinition.yaml"

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

requestCreateConnectorDefinitionVersion :: CreateConnectorDefinitionVersion -> TestTree
requestCreateConnectorDefinitionVersion =
  req
    "CreateConnectorDefinitionVersion"
    "fixture/CreateConnectorDefinitionVersion.yaml"

requestGetDeploymentStatus :: GetDeploymentStatus -> TestTree
requestGetDeploymentStatus =
  req
    "GetDeploymentStatus"
    "fixture/GetDeploymentStatus.yaml"

requestGetBulkDeploymentStatus :: GetBulkDeploymentStatus -> TestTree
requestGetBulkDeploymentStatus =
  req
    "GetBulkDeploymentStatus"
    "fixture/GetBulkDeploymentStatus.yaml"

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

requestUpdateFunctionDefinition :: UpdateFunctionDefinition -> TestTree
requestUpdateFunctionDefinition =
  req
    "UpdateFunctionDefinition"
    "fixture/UpdateFunctionDefinition.yaml"

requestDeleteFunctionDefinition :: DeleteFunctionDefinition -> TestTree
requestDeleteFunctionDefinition =
  req
    "DeleteFunctionDefinition"
    "fixture/DeleteFunctionDefinition.yaml"

requestListResourceDefinitions :: ListResourceDefinitions -> TestTree
requestListResourceDefinitions =
  req
    "ListResourceDefinitions"
    "fixture/ListResourceDefinitions.yaml"

requestStopBulkDeployment :: StopBulkDeployment -> TestTree
requestStopBulkDeployment =
  req
    "StopBulkDeployment"
    "fixture/StopBulkDeployment.yaml"

requestCreateResourceDefinitionVersion :: CreateResourceDefinitionVersion -> TestTree
requestCreateResourceDefinitionVersion =
  req
    "CreateResourceDefinitionVersion"
    "fixture/CreateResourceDefinitionVersion.yaml"

requestGetResourceDefinition :: GetResourceDefinition -> TestTree
requestGetResourceDefinition =
  req
    "GetResourceDefinition"
    "fixture/GetResourceDefinition.yaml"

requestListResourceDefinitionVersions :: ListResourceDefinitionVersions -> TestTree
requestListResourceDefinitionVersions =
  req
    "ListResourceDefinitionVersions"
    "fixture/ListResourceDefinitionVersions.yaml"

requestDisassociateServiceRoleFromAccount :: DisassociateServiceRoleFromAccount -> TestTree
requestDisassociateServiceRoleFromAccount =
  req
    "DisassociateServiceRoleFromAccount"
    "fixture/DisassociateServiceRoleFromAccount.yaml"

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

requestAssociateServiceRoleToAccount :: AssociateServiceRoleToAccount -> TestTree
requestAssociateServiceRoleToAccount =
  req
    "AssociateServiceRoleToAccount"
    "fixture/AssociateServiceRoleToAccount.yaml"

requestResetDeployments :: ResetDeployments -> TestTree
requestResetDeployments =
  req
    "ResetDeployments"
    "fixture/ResetDeployments.yaml"

requestListConnectorDefinitions :: ListConnectorDefinitions -> TestTree
requestListConnectorDefinitions =
  req
    "ListConnectorDefinitions"
    "fixture/ListConnectorDefinitions.yaml"

requestGetSubscriptionDefinitionVersion :: GetSubscriptionDefinitionVersion -> TestTree
requestGetSubscriptionDefinitionVersion =
  req
    "GetSubscriptionDefinitionVersion"
    "fixture/GetSubscriptionDefinitionVersion.yaml"

requestGetAssociatedRole :: GetAssociatedRole -> TestTree
requestGetAssociatedRole =
  req
    "GetAssociatedRole"
    "fixture/GetAssociatedRole.yaml"

requestListLoggerDefinitionVersions :: ListLoggerDefinitionVersions -> TestTree
requestListLoggerDefinitionVersions =
  req
    "ListLoggerDefinitionVersions"
    "fixture/ListLoggerDefinitionVersions.yaml"

requestCreateConnectorDefinition :: CreateConnectorDefinition -> TestTree
requestCreateConnectorDefinition =
  req
    "CreateConnectorDefinition"
    "fixture/CreateConnectorDefinition.yaml"

requestGetCoreDefinitionVersion :: GetCoreDefinitionVersion -> TestTree
requestGetCoreDefinitionVersion =
  req
    "GetCoreDefinitionVersion"
    "fixture/GetCoreDefinitionVersion.yaml"

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

requestCreateCoreDefinitionVersion :: CreateCoreDefinitionVersion -> TestTree
requestCreateCoreDefinitionVersion =
  req
    "CreateCoreDefinitionVersion"
    "fixture/CreateCoreDefinitionVersion.yaml"

requestListBulkDeployments :: ListBulkDeployments -> TestTree
requestListBulkDeployments =
  req
    "ListBulkDeployments"
    "fixture/ListBulkDeployments.yaml"

requestListDeployments :: ListDeployments -> TestTree
requestListDeployments =
  req
    "ListDeployments"
    "fixture/ListDeployments.yaml"

requestGetConnectorDefinition :: GetConnectorDefinition -> TestTree
requestGetConnectorDefinition =
  req
    "GetConnectorDefinition"
    "fixture/GetConnectorDefinition.yaml"

requestListLoggerDefinitions :: ListLoggerDefinitions -> TestTree
requestListLoggerDefinitions =
  req
    "ListLoggerDefinitions"
    "fixture/ListLoggerDefinitions.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestCreateSubscriptionDefinitionVersion :: CreateSubscriptionDefinitionVersion -> TestTree
requestCreateSubscriptionDefinitionVersion =
  req
    "CreateSubscriptionDefinitionVersion"
    "fixture/CreateSubscriptionDefinitionVersion.yaml"

requestGetGroupVersion :: GetGroupVersion -> TestTree
requestGetGroupVersion =
  req
    "GetGroupVersion"
    "fixture/GetGroupVersion.yaml"

requestUpdateGroupCertificateConfiguration :: UpdateGroupCertificateConfiguration -> TestTree
requestUpdateGroupCertificateConfiguration =
  req
    "UpdateGroupCertificateConfiguration"
    "fixture/UpdateGroupCertificateConfiguration.yaml"

requestGetFunctionDefinitionVersion :: GetFunctionDefinitionVersion -> TestTree
requestGetFunctionDefinitionVersion =
  req
    "GetFunctionDefinitionVersion"
    "fixture/GetFunctionDefinitionVersion.yaml"

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

requestCreateFunctionDefinition :: CreateFunctionDefinition -> TestTree
requestCreateFunctionDefinition =
  req
    "CreateFunctionDefinition"
    "fixture/CreateFunctionDefinition.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestCreateDeviceDefinitionVersion :: CreateDeviceDefinitionVersion -> TestTree
requestCreateDeviceDefinitionVersion =
  req
    "CreateDeviceDefinitionVersion"
    "fixture/CreateDeviceDefinitionVersion.yaml"

requestDeleteGroup :: DeleteGroup -> TestTree
requestDeleteGroup =
  req
    "DeleteGroup"
    "fixture/DeleteGroup.yaml"

requestUpdateGroup :: UpdateGroup -> TestTree
requestUpdateGroup =
  req
    "UpdateGroup"
    "fixture/UpdateGroup.yaml"

requestListGroups :: ListGroups -> TestTree
requestListGroups =
  req
    "ListGroups"
    "fixture/ListGroups.yaml"

requestListBulkDeploymentDetailedReports :: ListBulkDeploymentDetailedReports -> TestTree
requestListBulkDeploymentDetailedReports =
  req
    "ListBulkDeploymentDetailedReports"
    "fixture/ListBulkDeploymentDetailedReports.yaml"

requestGetThingRuntimeConfiguration :: GetThingRuntimeConfiguration -> TestTree
requestGetThingRuntimeConfiguration =
  req
    "GetThingRuntimeConfiguration"
    "fixture/GetThingRuntimeConfiguration.yaml"

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

requestListDeviceDefinitionVersions :: ListDeviceDefinitionVersions -> TestTree
requestListDeviceDefinitionVersions =
  req
    "ListDeviceDefinitionVersions"
    "fixture/ListDeviceDefinitionVersions.yaml"

requestListFunctionDefinitions :: ListFunctionDefinitions -> TestTree
requestListFunctionDefinitions =
  req
    "ListFunctionDefinitions"
    "fixture/ListFunctionDefinitions.yaml"

requestGetFunctionDefinition :: GetFunctionDefinition -> TestTree
requestGetFunctionDefinition =
  req
    "GetFunctionDefinition"
    "fixture/GetFunctionDefinition.yaml"

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

requestCreateGroupVersion :: CreateGroupVersion -> TestTree
requestCreateGroupVersion =
  req
    "CreateGroupVersion"
    "fixture/CreateGroupVersion.yaml"

requestCreateFunctionDefinitionVersion :: CreateFunctionDefinitionVersion -> TestTree
requestCreateFunctionDefinitionVersion =
  req
    "CreateFunctionDefinitionVersion"
    "fixture/CreateFunctionDefinitionVersion.yaml"

requestStartBulkDeployment :: StartBulkDeployment -> TestTree
requestStartBulkDeployment =
  req
    "StartBulkDeployment"
    "fixture/StartBulkDeployment.yaml"

requestUpdateThingRuntimeConfiguration :: UpdateThingRuntimeConfiguration -> TestTree
requestUpdateThingRuntimeConfiguration =
  req
    "UpdateThingRuntimeConfiguration"
    "fixture/UpdateThingRuntimeConfiguration.yaml"

requestGetDeviceDefinitionVersion :: GetDeviceDefinitionVersion -> TestTree
requestGetDeviceDefinitionVersion =
  req
    "GetDeviceDefinitionVersion"
    "fixture/GetDeviceDefinitionVersion.yaml"

-- Responses

responseGetGroupCertificateConfiguration :: GetGroupCertificateConfigurationResponse -> TestTree
responseGetGroupCertificateConfiguration =
  res
    "GetGroupCertificateConfigurationResponse"
    "fixture/GetGroupCertificateConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGroupCertificateConfiguration)

responseListGroupVersions :: ListGroupVersionsResponse -> TestTree
responseListGroupVersions =
  res
    "ListGroupVersionsResponse"
    "fixture/ListGroupVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGroupVersions)

responseListFunctionDefinitionVersions :: ListFunctionDefinitionVersionsResponse -> TestTree
responseListFunctionDefinitionVersions =
  res
    "ListFunctionDefinitionVersionsResponse"
    "fixture/ListFunctionDefinitionVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFunctionDefinitionVersions)

responseListDeviceDefinitions :: ListDeviceDefinitionsResponse -> TestTree
responseListDeviceDefinitions =
  res
    "ListDeviceDefinitionsResponse"
    "fixture/ListDeviceDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDeviceDefinitions)

responseAssociateRoleToGroup :: AssociateRoleToGroupResponse -> TestTree
responseAssociateRoleToGroup =
  res
    "AssociateRoleToGroupResponse"
    "fixture/AssociateRoleToGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateRoleToGroup)

responseUpdateCoreDefinition :: UpdateCoreDefinitionResponse -> TestTree
responseUpdateCoreDefinition =
  res
    "UpdateCoreDefinitionResponse"
    "fixture/UpdateCoreDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCoreDefinition)

responseDeleteCoreDefinition :: DeleteCoreDefinitionResponse -> TestTree
responseDeleteCoreDefinition =
  res
    "DeleteCoreDefinitionResponse"
    "fixture/DeleteCoreDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCoreDefinition)

responseGetLoggerDefinition :: GetLoggerDefinitionResponse -> TestTree
responseGetLoggerDefinition =
  res
    "GetLoggerDefinitionResponse"
    "fixture/GetLoggerDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLoggerDefinition)

responseListGroupCertificateAuthorities :: ListGroupCertificateAuthoritiesResponse -> TestTree
responseListGroupCertificateAuthorities =
  res
    "ListGroupCertificateAuthoritiesResponse"
    "fixture/ListGroupCertificateAuthoritiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGroupCertificateAuthorities)

responseDisassociateRoleFromGroup :: DisassociateRoleFromGroupResponse -> TestTree
responseDisassociateRoleFromGroup =
  res
    "DisassociateRoleFromGroupResponse"
    "fixture/DisassociateRoleFromGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateRoleFromGroup)

responseUpdateSubscriptionDefinition :: UpdateSubscriptionDefinitionResponse -> TestTree
responseUpdateSubscriptionDefinition =
  res
    "UpdateSubscriptionDefinitionResponse"
    "fixture/UpdateSubscriptionDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSubscriptionDefinition)

responseDeleteSubscriptionDefinition :: DeleteSubscriptionDefinitionResponse -> TestTree
responseDeleteSubscriptionDefinition =
  res
    "DeleteSubscriptionDefinitionResponse"
    "fixture/DeleteSubscriptionDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSubscriptionDefinition)

responseListCoreDefinitions :: ListCoreDefinitionsResponse -> TestTree
responseListCoreDefinitions =
  res
    "ListCoreDefinitionsResponse"
    "fixture/ListCoreDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCoreDefinitions)

responseListSubscriptionDefinitions :: ListSubscriptionDefinitionsResponse -> TestTree
responseListSubscriptionDefinitions =
  res
    "ListSubscriptionDefinitionsResponse"
    "fixture/ListSubscriptionDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSubscriptionDefinitions)

responseCreateGroupCertificateAuthority :: CreateGroupCertificateAuthorityResponse -> TestTree
responseCreateGroupCertificateAuthority =
  res
    "CreateGroupCertificateAuthorityResponse"
    "fixture/CreateGroupCertificateAuthorityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGroupCertificateAuthority)

responseDeleteConnectorDefinition :: DeleteConnectorDefinitionResponse -> TestTree
responseDeleteConnectorDefinition =
  res
    "DeleteConnectorDefinitionResponse"
    "fixture/DeleteConnectorDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConnectorDefinition)

responseUpdateConnectorDefinition :: UpdateConnectorDefinitionResponse -> TestTree
responseUpdateConnectorDefinition =
  res
    "UpdateConnectorDefinitionResponse"
    "fixture/UpdateConnectorDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConnectorDefinition)

responseCreateLoggerDefinitionVersion :: CreateLoggerDefinitionVersionResponse -> TestTree
responseCreateLoggerDefinitionVersion =
  res
    "CreateLoggerDefinitionVersionResponse"
    "fixture/CreateLoggerDefinitionVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLoggerDefinitionVersion)

responseCreateCoreDefinition :: CreateCoreDefinitionResponse -> TestTree
responseCreateCoreDefinition =
  res
    "CreateCoreDefinitionResponse"
    "fixture/CreateCoreDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCoreDefinition)

responseGetConnectorDefinitionVersion :: GetConnectorDefinitionVersionResponse -> TestTree
responseGetConnectorDefinitionVersion =
  res
    "GetConnectorDefinitionVersionResponse"
    "fixture/GetConnectorDefinitionVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConnectorDefinitionVersion)

responseUpdateConnectivityInfo :: UpdateConnectivityInfoResponse -> TestTree
responseUpdateConnectivityInfo =
  res
    "UpdateConnectivityInfoResponse"
    "fixture/UpdateConnectivityInfoResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConnectivityInfo)

responseCreateSubscriptionDefinition :: CreateSubscriptionDefinitionResponse -> TestTree
responseCreateSubscriptionDefinition =
  res
    "CreateSubscriptionDefinitionResponse"
    "fixture/CreateSubscriptionDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSubscriptionDefinition)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseGetGroupCertificateAuthority :: GetGroupCertificateAuthorityResponse -> TestTree
responseGetGroupCertificateAuthority =
  res
    "GetGroupCertificateAuthorityResponse"
    "fixture/GetGroupCertificateAuthorityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGroupCertificateAuthority)

responseGetLoggerDefinitionVersion :: GetLoggerDefinitionVersionResponse -> TestTree
responseGetLoggerDefinitionVersion =
  res
    "GetLoggerDefinitionVersionResponse"
    "fixture/GetLoggerDefinitionVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLoggerDefinitionVersion)

responseGetServiceRoleForAccount :: GetServiceRoleForAccountResponse -> TestTree
responseGetServiceRoleForAccount =
  res
    "GetServiceRoleForAccountResponse"
    "fixture/GetServiceRoleForAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetServiceRoleForAccount)

responseListConnectorDefinitionVersions :: ListConnectorDefinitionVersionsResponse -> TestTree
responseListConnectorDefinitionVersions =
  res
    "ListConnectorDefinitionVersionsResponse"
    "fixture/ListConnectorDefinitionVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConnectorDefinitionVersions)

responseCreateSoftwareUpdateJob :: CreateSoftwareUpdateJobResponse -> TestTree
responseCreateSoftwareUpdateJob =
  res
    "CreateSoftwareUpdateJobResponse"
    "fixture/CreateSoftwareUpdateJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSoftwareUpdateJob)

responseCreateLoggerDefinition :: CreateLoggerDefinitionResponse -> TestTree
responseCreateLoggerDefinition =
  res
    "CreateLoggerDefinitionResponse"
    "fixture/CreateLoggerDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLoggerDefinition)

responseGetConnectivityInfo :: GetConnectivityInfoResponse -> TestTree
responseGetConnectivityInfo =
  res
    "GetConnectivityInfoResponse"
    "fixture/GetConnectivityInfoResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConnectivityInfo)

responseCreateDeployment :: CreateDeploymentResponse -> TestTree
responseCreateDeployment =
  res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDeployment)

responseDeleteLoggerDefinition :: DeleteLoggerDefinitionResponse -> TestTree
responseDeleteLoggerDefinition =
  res
    "DeleteLoggerDefinitionResponse"
    "fixture/DeleteLoggerDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLoggerDefinition)

responseUpdateLoggerDefinition :: UpdateLoggerDefinitionResponse -> TestTree
responseUpdateLoggerDefinition =
  res
    "UpdateLoggerDefinitionResponse"
    "fixture/UpdateLoggerDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLoggerDefinition)

responseGetSubscriptionDefinition :: GetSubscriptionDefinitionResponse -> TestTree
responseGetSubscriptionDefinition =
  res
    "GetSubscriptionDefinitionResponse"
    "fixture/GetSubscriptionDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSubscriptionDefinition)

responseGetCoreDefinition :: GetCoreDefinitionResponse -> TestTree
responseGetCoreDefinition =
  res
    "GetCoreDefinitionResponse"
    "fixture/GetCoreDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCoreDefinition)

responseCreateConnectorDefinitionVersion :: CreateConnectorDefinitionVersionResponse -> TestTree
responseCreateConnectorDefinitionVersion =
  res
    "CreateConnectorDefinitionVersionResponse"
    "fixture/CreateConnectorDefinitionVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConnectorDefinitionVersion)

responseGetDeploymentStatus :: GetDeploymentStatusResponse -> TestTree
responseGetDeploymentStatus =
  res
    "GetDeploymentStatusResponse"
    "fixture/GetDeploymentStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeploymentStatus)

responseGetBulkDeploymentStatus :: GetBulkDeploymentStatusResponse -> TestTree
responseGetBulkDeploymentStatus =
  res
    "GetBulkDeploymentStatusResponse"
    "fixture/GetBulkDeploymentStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBulkDeploymentStatus)

responseCreateResourceDefinition :: CreateResourceDefinitionResponse -> TestTree
responseCreateResourceDefinition =
  res
    "CreateResourceDefinitionResponse"
    "fixture/CreateResourceDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateResourceDefinition)

responseGetResourceDefinitionVersion :: GetResourceDefinitionVersionResponse -> TestTree
responseGetResourceDefinitionVersion =
  res
    "GetResourceDefinitionVersionResponse"
    "fixture/GetResourceDefinitionVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourceDefinitionVersion)

responseUpdateFunctionDefinition :: UpdateFunctionDefinitionResponse -> TestTree
responseUpdateFunctionDefinition =
  res
    "UpdateFunctionDefinitionResponse"
    "fixture/UpdateFunctionDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFunctionDefinition)

responseDeleteFunctionDefinition :: DeleteFunctionDefinitionResponse -> TestTree
responseDeleteFunctionDefinition =
  res
    "DeleteFunctionDefinitionResponse"
    "fixture/DeleteFunctionDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFunctionDefinition)

responseListResourceDefinitions :: ListResourceDefinitionsResponse -> TestTree
responseListResourceDefinitions =
  res
    "ListResourceDefinitionsResponse"
    "fixture/ListResourceDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourceDefinitions)

responseStopBulkDeployment :: StopBulkDeploymentResponse -> TestTree
responseStopBulkDeployment =
  res
    "StopBulkDeploymentResponse"
    "fixture/StopBulkDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopBulkDeployment)

responseCreateResourceDefinitionVersion :: CreateResourceDefinitionVersionResponse -> TestTree
responseCreateResourceDefinitionVersion =
  res
    "CreateResourceDefinitionVersionResponse"
    "fixture/CreateResourceDefinitionVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateResourceDefinitionVersion)

responseGetResourceDefinition :: GetResourceDefinitionResponse -> TestTree
responseGetResourceDefinition =
  res
    "GetResourceDefinitionResponse"
    "fixture/GetResourceDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourceDefinition)

responseListResourceDefinitionVersions :: ListResourceDefinitionVersionsResponse -> TestTree
responseListResourceDefinitionVersions =
  res
    "ListResourceDefinitionVersionsResponse"
    "fixture/ListResourceDefinitionVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourceDefinitionVersions)

responseDisassociateServiceRoleFromAccount :: DisassociateServiceRoleFromAccountResponse -> TestTree
responseDisassociateServiceRoleFromAccount =
  res
    "DisassociateServiceRoleFromAccountResponse"
    "fixture/DisassociateServiceRoleFromAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateServiceRoleFromAccount)

responseDeleteDeviceDefinition :: DeleteDeviceDefinitionResponse -> TestTree
responseDeleteDeviceDefinition =
  res
    "DeleteDeviceDefinitionResponse"
    "fixture/DeleteDeviceDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDeviceDefinition)

responseUpdateDeviceDefinition :: UpdateDeviceDefinitionResponse -> TestTree
responseUpdateDeviceDefinition =
  res
    "UpdateDeviceDefinitionResponse"
    "fixture/UpdateDeviceDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDeviceDefinition)

responseAssociateServiceRoleToAccount :: AssociateServiceRoleToAccountResponse -> TestTree
responseAssociateServiceRoleToAccount =
  res
    "AssociateServiceRoleToAccountResponse"
    "fixture/AssociateServiceRoleToAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateServiceRoleToAccount)

responseResetDeployments :: ResetDeploymentsResponse -> TestTree
responseResetDeployments =
  res
    "ResetDeploymentsResponse"
    "fixture/ResetDeploymentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetDeployments)

responseListConnectorDefinitions :: ListConnectorDefinitionsResponse -> TestTree
responseListConnectorDefinitions =
  res
    "ListConnectorDefinitionsResponse"
    "fixture/ListConnectorDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConnectorDefinitions)

responseGetSubscriptionDefinitionVersion :: GetSubscriptionDefinitionVersionResponse -> TestTree
responseGetSubscriptionDefinitionVersion =
  res
    "GetSubscriptionDefinitionVersionResponse"
    "fixture/GetSubscriptionDefinitionVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSubscriptionDefinitionVersion)

responseGetAssociatedRole :: GetAssociatedRoleResponse -> TestTree
responseGetAssociatedRole =
  res
    "GetAssociatedRoleResponse"
    "fixture/GetAssociatedRoleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAssociatedRole)

responseListLoggerDefinitionVersions :: ListLoggerDefinitionVersionsResponse -> TestTree
responseListLoggerDefinitionVersions =
  res
    "ListLoggerDefinitionVersionsResponse"
    "fixture/ListLoggerDefinitionVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLoggerDefinitionVersions)

responseCreateConnectorDefinition :: CreateConnectorDefinitionResponse -> TestTree
responseCreateConnectorDefinition =
  res
    "CreateConnectorDefinitionResponse"
    "fixture/CreateConnectorDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConnectorDefinition)

responseGetCoreDefinitionVersion :: GetCoreDefinitionVersionResponse -> TestTree
responseGetCoreDefinitionVersion =
  res
    "GetCoreDefinitionVersionResponse"
    "fixture/GetCoreDefinitionVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCoreDefinitionVersion)

responseListSubscriptionDefinitionVersions :: ListSubscriptionDefinitionVersionsResponse -> TestTree
responseListSubscriptionDefinitionVersions =
  res
    "ListSubscriptionDefinitionVersionsResponse"
    "fixture/ListSubscriptionDefinitionVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSubscriptionDefinitionVersions)

responseListCoreDefinitionVersions :: ListCoreDefinitionVersionsResponse -> TestTree
responseListCoreDefinitionVersions =
  res
    "ListCoreDefinitionVersionsResponse"
    "fixture/ListCoreDefinitionVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCoreDefinitionVersions)

responseCreateCoreDefinitionVersion :: CreateCoreDefinitionVersionResponse -> TestTree
responseCreateCoreDefinitionVersion =
  res
    "CreateCoreDefinitionVersionResponse"
    "fixture/CreateCoreDefinitionVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCoreDefinitionVersion)

responseListBulkDeployments :: ListBulkDeploymentsResponse -> TestTree
responseListBulkDeployments =
  res
    "ListBulkDeploymentsResponse"
    "fixture/ListBulkDeploymentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBulkDeployments)

responseListDeployments :: ListDeploymentsResponse -> TestTree
responseListDeployments =
  res
    "ListDeploymentsResponse"
    "fixture/ListDeploymentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDeployments)

responseGetConnectorDefinition :: GetConnectorDefinitionResponse -> TestTree
responseGetConnectorDefinition =
  res
    "GetConnectorDefinitionResponse"
    "fixture/GetConnectorDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConnectorDefinition)

responseListLoggerDefinitions :: ListLoggerDefinitionsResponse -> TestTree
responseListLoggerDefinitions =
  res
    "ListLoggerDefinitionsResponse"
    "fixture/ListLoggerDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLoggerDefinitions)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseCreateSubscriptionDefinitionVersion :: CreateSubscriptionDefinitionVersionResponse -> TestTree
responseCreateSubscriptionDefinitionVersion =
  res
    "CreateSubscriptionDefinitionVersionResponse"
    "fixture/CreateSubscriptionDefinitionVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSubscriptionDefinitionVersion)

responseGetGroupVersion :: GetGroupVersionResponse -> TestTree
responseGetGroupVersion =
  res
    "GetGroupVersionResponse"
    "fixture/GetGroupVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGroupVersion)

responseUpdateGroupCertificateConfiguration :: UpdateGroupCertificateConfigurationResponse -> TestTree
responseUpdateGroupCertificateConfiguration =
  res
    "UpdateGroupCertificateConfigurationResponse"
    "fixture/UpdateGroupCertificateConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGroupCertificateConfiguration)

responseGetFunctionDefinitionVersion :: GetFunctionDefinitionVersionResponse -> TestTree
responseGetFunctionDefinitionVersion =
  res
    "GetFunctionDefinitionVersionResponse"
    "fixture/GetFunctionDefinitionVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFunctionDefinitionVersion)

responseGetDeviceDefinition :: GetDeviceDefinitionResponse -> TestTree
responseGetDeviceDefinition =
  res
    "GetDeviceDefinitionResponse"
    "fixture/GetDeviceDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeviceDefinition)

responseCreateGroup :: CreateGroupResponse -> TestTree
responseCreateGroup =
  res
    "CreateGroupResponse"
    "fixture/CreateGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGroup)

responseCreateFunctionDefinition :: CreateFunctionDefinitionResponse -> TestTree
responseCreateFunctionDefinition =
  res
    "CreateFunctionDefinitionResponse"
    "fixture/CreateFunctionDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFunctionDefinition)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseCreateDeviceDefinitionVersion :: CreateDeviceDefinitionVersionResponse -> TestTree
responseCreateDeviceDefinitionVersion =
  res
    "CreateDeviceDefinitionVersionResponse"
    "fixture/CreateDeviceDefinitionVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDeviceDefinitionVersion)

responseDeleteGroup :: DeleteGroupResponse -> TestTree
responseDeleteGroup =
  res
    "DeleteGroupResponse"
    "fixture/DeleteGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGroup)

responseUpdateGroup :: UpdateGroupResponse -> TestTree
responseUpdateGroup =
  res
    "UpdateGroupResponse"
    "fixture/UpdateGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGroup)

responseListGroups :: ListGroupsResponse -> TestTree
responseListGroups =
  res
    "ListGroupsResponse"
    "fixture/ListGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGroups)

responseListBulkDeploymentDetailedReports :: ListBulkDeploymentDetailedReportsResponse -> TestTree
responseListBulkDeploymentDetailedReports =
  res
    "ListBulkDeploymentDetailedReportsResponse"
    "fixture/ListBulkDeploymentDetailedReportsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBulkDeploymentDetailedReports)

responseGetThingRuntimeConfiguration :: GetThingRuntimeConfigurationResponse -> TestTree
responseGetThingRuntimeConfiguration =
  res
    "GetThingRuntimeConfigurationResponse"
    "fixture/GetThingRuntimeConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetThingRuntimeConfiguration)

responseDeleteResourceDefinition :: DeleteResourceDefinitionResponse -> TestTree
responseDeleteResourceDefinition =
  res
    "DeleteResourceDefinitionResponse"
    "fixture/DeleteResourceDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourceDefinition)

responseUpdateResourceDefinition :: UpdateResourceDefinitionResponse -> TestTree
responseUpdateResourceDefinition =
  res
    "UpdateResourceDefinitionResponse"
    "fixture/UpdateResourceDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResourceDefinition)

responseListDeviceDefinitionVersions :: ListDeviceDefinitionVersionsResponse -> TestTree
responseListDeviceDefinitionVersions =
  res
    "ListDeviceDefinitionVersionsResponse"
    "fixture/ListDeviceDefinitionVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDeviceDefinitionVersions)

responseListFunctionDefinitions :: ListFunctionDefinitionsResponse -> TestTree
responseListFunctionDefinitions =
  res
    "ListFunctionDefinitionsResponse"
    "fixture/ListFunctionDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFunctionDefinitions)

responseGetFunctionDefinition :: GetFunctionDefinitionResponse -> TestTree
responseGetFunctionDefinition =
  res
    "GetFunctionDefinitionResponse"
    "fixture/GetFunctionDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFunctionDefinition)

responseGetGroup :: GetGroupResponse -> TestTree
responseGetGroup =
  res
    "GetGroupResponse"
    "fixture/GetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGroup)

responseCreateDeviceDefinition :: CreateDeviceDefinitionResponse -> TestTree
responseCreateDeviceDefinition =
  res
    "CreateDeviceDefinitionResponse"
    "fixture/CreateDeviceDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDeviceDefinition)

responseCreateGroupVersion :: CreateGroupVersionResponse -> TestTree
responseCreateGroupVersion =
  res
    "CreateGroupVersionResponse"
    "fixture/CreateGroupVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGroupVersion)

responseCreateFunctionDefinitionVersion :: CreateFunctionDefinitionVersionResponse -> TestTree
responseCreateFunctionDefinitionVersion =
  res
    "CreateFunctionDefinitionVersionResponse"
    "fixture/CreateFunctionDefinitionVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFunctionDefinitionVersion)

responseStartBulkDeployment :: StartBulkDeploymentResponse -> TestTree
responseStartBulkDeployment =
  res
    "StartBulkDeploymentResponse"
    "fixture/StartBulkDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartBulkDeployment)

responseUpdateThingRuntimeConfiguration :: UpdateThingRuntimeConfigurationResponse -> TestTree
responseUpdateThingRuntimeConfiguration =
  res
    "UpdateThingRuntimeConfigurationResponse"
    "fixture/UpdateThingRuntimeConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateThingRuntimeConfiguration)

responseGetDeviceDefinitionVersion :: GetDeviceDefinitionVersionResponse -> TestTree
responseGetDeviceDefinitionVersion =
  res
    "GetDeviceDefinitionVersionResponse"
    "fixture/GetDeviceDefinitionVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeviceDefinitionVersion)
