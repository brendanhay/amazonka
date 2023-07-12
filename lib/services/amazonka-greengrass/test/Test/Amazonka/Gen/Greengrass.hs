{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Greengrass
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Greengrass where

import Amazonka.Greengrass
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Greengrass.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateRoleToGroup $
--             newAssociateRoleToGroup
--
--         , requestAssociateServiceRoleToAccount $
--             newAssociateServiceRoleToAccount
--
--         , requestCreateConnectorDefinition $
--             newCreateConnectorDefinition
--
--         , requestCreateConnectorDefinitionVersion $
--             newCreateConnectorDefinitionVersion
--
--         , requestCreateCoreDefinition $
--             newCreateCoreDefinition
--
--         , requestCreateCoreDefinitionVersion $
--             newCreateCoreDefinitionVersion
--
--         , requestCreateDeployment $
--             newCreateDeployment
--
--         , requestCreateDeviceDefinition $
--             newCreateDeviceDefinition
--
--         , requestCreateDeviceDefinitionVersion $
--             newCreateDeviceDefinitionVersion
--
--         , requestCreateFunctionDefinition $
--             newCreateFunctionDefinition
--
--         , requestCreateFunctionDefinitionVersion $
--             newCreateFunctionDefinitionVersion
--
--         , requestCreateGroup $
--             newCreateGroup
--
--         , requestCreateGroupCertificateAuthority $
--             newCreateGroupCertificateAuthority
--
--         , requestCreateGroupVersion $
--             newCreateGroupVersion
--
--         , requestCreateLoggerDefinition $
--             newCreateLoggerDefinition
--
--         , requestCreateLoggerDefinitionVersion $
--             newCreateLoggerDefinitionVersion
--
--         , requestCreateResourceDefinition $
--             newCreateResourceDefinition
--
--         , requestCreateResourceDefinitionVersion $
--             newCreateResourceDefinitionVersion
--
--         , requestCreateSoftwareUpdateJob $
--             newCreateSoftwareUpdateJob
--
--         , requestCreateSubscriptionDefinition $
--             newCreateSubscriptionDefinition
--
--         , requestCreateSubscriptionDefinitionVersion $
--             newCreateSubscriptionDefinitionVersion
--
--         , requestDeleteConnectorDefinition $
--             newDeleteConnectorDefinition
--
--         , requestDeleteCoreDefinition $
--             newDeleteCoreDefinition
--
--         , requestDeleteDeviceDefinition $
--             newDeleteDeviceDefinition
--
--         , requestDeleteFunctionDefinition $
--             newDeleteFunctionDefinition
--
--         , requestDeleteGroup $
--             newDeleteGroup
--
--         , requestDeleteLoggerDefinition $
--             newDeleteLoggerDefinition
--
--         , requestDeleteResourceDefinition $
--             newDeleteResourceDefinition
--
--         , requestDeleteSubscriptionDefinition $
--             newDeleteSubscriptionDefinition
--
--         , requestDisassociateRoleFromGroup $
--             newDisassociateRoleFromGroup
--
--         , requestDisassociateServiceRoleFromAccount $
--             newDisassociateServiceRoleFromAccount
--
--         , requestGetAssociatedRole $
--             newGetAssociatedRole
--
--         , requestGetBulkDeploymentStatus $
--             newGetBulkDeploymentStatus
--
--         , requestGetConnectivityInfo $
--             newGetConnectivityInfo
--
--         , requestGetConnectorDefinition $
--             newGetConnectorDefinition
--
--         , requestGetConnectorDefinitionVersion $
--             newGetConnectorDefinitionVersion
--
--         , requestGetCoreDefinition $
--             newGetCoreDefinition
--
--         , requestGetCoreDefinitionVersion $
--             newGetCoreDefinitionVersion
--
--         , requestGetDeploymentStatus $
--             newGetDeploymentStatus
--
--         , requestGetDeviceDefinition $
--             newGetDeviceDefinition
--
--         , requestGetDeviceDefinitionVersion $
--             newGetDeviceDefinitionVersion
--
--         , requestGetFunctionDefinition $
--             newGetFunctionDefinition
--
--         , requestGetFunctionDefinitionVersion $
--             newGetFunctionDefinitionVersion
--
--         , requestGetGroup $
--             newGetGroup
--
--         , requestGetGroupCertificateAuthority $
--             newGetGroupCertificateAuthority
--
--         , requestGetGroupCertificateConfiguration $
--             newGetGroupCertificateConfiguration
--
--         , requestGetGroupVersion $
--             newGetGroupVersion
--
--         , requestGetLoggerDefinition $
--             newGetLoggerDefinition
--
--         , requestGetLoggerDefinitionVersion $
--             newGetLoggerDefinitionVersion
--
--         , requestGetResourceDefinition $
--             newGetResourceDefinition
--
--         , requestGetResourceDefinitionVersion $
--             newGetResourceDefinitionVersion
--
--         , requestGetServiceRoleForAccount $
--             newGetServiceRoleForAccount
--
--         , requestGetSubscriptionDefinition $
--             newGetSubscriptionDefinition
--
--         , requestGetSubscriptionDefinitionVersion $
--             newGetSubscriptionDefinitionVersion
--
--         , requestGetThingRuntimeConfiguration $
--             newGetThingRuntimeConfiguration
--
--         , requestListBulkDeploymentDetailedReports $
--             newListBulkDeploymentDetailedReports
--
--         , requestListBulkDeployments $
--             newListBulkDeployments
--
--         , requestListConnectorDefinitionVersions $
--             newListConnectorDefinitionVersions
--
--         , requestListConnectorDefinitions $
--             newListConnectorDefinitions
--
--         , requestListCoreDefinitionVersions $
--             newListCoreDefinitionVersions
--
--         , requestListCoreDefinitions $
--             newListCoreDefinitions
--
--         , requestListDeployments $
--             newListDeployments
--
--         , requestListDeviceDefinitionVersions $
--             newListDeviceDefinitionVersions
--
--         , requestListDeviceDefinitions $
--             newListDeviceDefinitions
--
--         , requestListFunctionDefinitionVersions $
--             newListFunctionDefinitionVersions
--
--         , requestListFunctionDefinitions $
--             newListFunctionDefinitions
--
--         , requestListGroupCertificateAuthorities $
--             newListGroupCertificateAuthorities
--
--         , requestListGroupVersions $
--             newListGroupVersions
--
--         , requestListGroups $
--             newListGroups
--
--         , requestListLoggerDefinitionVersions $
--             newListLoggerDefinitionVersions
--
--         , requestListLoggerDefinitions $
--             newListLoggerDefinitions
--
--         , requestListResourceDefinitionVersions $
--             newListResourceDefinitionVersions
--
--         , requestListResourceDefinitions $
--             newListResourceDefinitions
--
--         , requestListSubscriptionDefinitionVersions $
--             newListSubscriptionDefinitionVersions
--
--         , requestListSubscriptionDefinitions $
--             newListSubscriptionDefinitions
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestResetDeployments $
--             newResetDeployments
--
--         , requestStartBulkDeployment $
--             newStartBulkDeployment
--
--         , requestStopBulkDeployment $
--             newStopBulkDeployment
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateConnectivityInfo $
--             newUpdateConnectivityInfo
--
--         , requestUpdateConnectorDefinition $
--             newUpdateConnectorDefinition
--
--         , requestUpdateCoreDefinition $
--             newUpdateCoreDefinition
--
--         , requestUpdateDeviceDefinition $
--             newUpdateDeviceDefinition
--
--         , requestUpdateFunctionDefinition $
--             newUpdateFunctionDefinition
--
--         , requestUpdateGroup $
--             newUpdateGroup
--
--         , requestUpdateGroupCertificateConfiguration $
--             newUpdateGroupCertificateConfiguration
--
--         , requestUpdateLoggerDefinition $
--             newUpdateLoggerDefinition
--
--         , requestUpdateResourceDefinition $
--             newUpdateResourceDefinition
--
--         , requestUpdateSubscriptionDefinition $
--             newUpdateSubscriptionDefinition
--
--         , requestUpdateThingRuntimeConfiguration $
--             newUpdateThingRuntimeConfiguration
--
--           ]

--     , testGroup "response"
--         [ responseAssociateRoleToGroup $
--             newAssociateRoleToGroupResponse
--
--         , responseAssociateServiceRoleToAccount $
--             newAssociateServiceRoleToAccountResponse
--
--         , responseCreateConnectorDefinition $
--             newCreateConnectorDefinitionResponse
--
--         , responseCreateConnectorDefinitionVersion $
--             newCreateConnectorDefinitionVersionResponse
--
--         , responseCreateCoreDefinition $
--             newCreateCoreDefinitionResponse
--
--         , responseCreateCoreDefinitionVersion $
--             newCreateCoreDefinitionVersionResponse
--
--         , responseCreateDeployment $
--             newCreateDeploymentResponse
--
--         , responseCreateDeviceDefinition $
--             newCreateDeviceDefinitionResponse
--
--         , responseCreateDeviceDefinitionVersion $
--             newCreateDeviceDefinitionVersionResponse
--
--         , responseCreateFunctionDefinition $
--             newCreateFunctionDefinitionResponse
--
--         , responseCreateFunctionDefinitionVersion $
--             newCreateFunctionDefinitionVersionResponse
--
--         , responseCreateGroup $
--             newCreateGroupResponse
--
--         , responseCreateGroupCertificateAuthority $
--             newCreateGroupCertificateAuthorityResponse
--
--         , responseCreateGroupVersion $
--             newCreateGroupVersionResponse
--
--         , responseCreateLoggerDefinition $
--             newCreateLoggerDefinitionResponse
--
--         , responseCreateLoggerDefinitionVersion $
--             newCreateLoggerDefinitionVersionResponse
--
--         , responseCreateResourceDefinition $
--             newCreateResourceDefinitionResponse
--
--         , responseCreateResourceDefinitionVersion $
--             newCreateResourceDefinitionVersionResponse
--
--         , responseCreateSoftwareUpdateJob $
--             newCreateSoftwareUpdateJobResponse
--
--         , responseCreateSubscriptionDefinition $
--             newCreateSubscriptionDefinitionResponse
--
--         , responseCreateSubscriptionDefinitionVersion $
--             newCreateSubscriptionDefinitionVersionResponse
--
--         , responseDeleteConnectorDefinition $
--             newDeleteConnectorDefinitionResponse
--
--         , responseDeleteCoreDefinition $
--             newDeleteCoreDefinitionResponse
--
--         , responseDeleteDeviceDefinition $
--             newDeleteDeviceDefinitionResponse
--
--         , responseDeleteFunctionDefinition $
--             newDeleteFunctionDefinitionResponse
--
--         , responseDeleteGroup $
--             newDeleteGroupResponse
--
--         , responseDeleteLoggerDefinition $
--             newDeleteLoggerDefinitionResponse
--
--         , responseDeleteResourceDefinition $
--             newDeleteResourceDefinitionResponse
--
--         , responseDeleteSubscriptionDefinition $
--             newDeleteSubscriptionDefinitionResponse
--
--         , responseDisassociateRoleFromGroup $
--             newDisassociateRoleFromGroupResponse
--
--         , responseDisassociateServiceRoleFromAccount $
--             newDisassociateServiceRoleFromAccountResponse
--
--         , responseGetAssociatedRole $
--             newGetAssociatedRoleResponse
--
--         , responseGetBulkDeploymentStatus $
--             newGetBulkDeploymentStatusResponse
--
--         , responseGetConnectivityInfo $
--             newGetConnectivityInfoResponse
--
--         , responseGetConnectorDefinition $
--             newGetConnectorDefinitionResponse
--
--         , responseGetConnectorDefinitionVersion $
--             newGetConnectorDefinitionVersionResponse
--
--         , responseGetCoreDefinition $
--             newGetCoreDefinitionResponse
--
--         , responseGetCoreDefinitionVersion $
--             newGetCoreDefinitionVersionResponse
--
--         , responseGetDeploymentStatus $
--             newGetDeploymentStatusResponse
--
--         , responseGetDeviceDefinition $
--             newGetDeviceDefinitionResponse
--
--         , responseGetDeviceDefinitionVersion $
--             newGetDeviceDefinitionVersionResponse
--
--         , responseGetFunctionDefinition $
--             newGetFunctionDefinitionResponse
--
--         , responseGetFunctionDefinitionVersion $
--             newGetFunctionDefinitionVersionResponse
--
--         , responseGetGroup $
--             newGetGroupResponse
--
--         , responseGetGroupCertificateAuthority $
--             newGetGroupCertificateAuthorityResponse
--
--         , responseGetGroupCertificateConfiguration $
--             newGetGroupCertificateConfigurationResponse
--
--         , responseGetGroupVersion $
--             newGetGroupVersionResponse
--
--         , responseGetLoggerDefinition $
--             newGetLoggerDefinitionResponse
--
--         , responseGetLoggerDefinitionVersion $
--             newGetLoggerDefinitionVersionResponse
--
--         , responseGetResourceDefinition $
--             newGetResourceDefinitionResponse
--
--         , responseGetResourceDefinitionVersion $
--             newGetResourceDefinitionVersionResponse
--
--         , responseGetServiceRoleForAccount $
--             newGetServiceRoleForAccountResponse
--
--         , responseGetSubscriptionDefinition $
--             newGetSubscriptionDefinitionResponse
--
--         , responseGetSubscriptionDefinitionVersion $
--             newGetSubscriptionDefinitionVersionResponse
--
--         , responseGetThingRuntimeConfiguration $
--             newGetThingRuntimeConfigurationResponse
--
--         , responseListBulkDeploymentDetailedReports $
--             newListBulkDeploymentDetailedReportsResponse
--
--         , responseListBulkDeployments $
--             newListBulkDeploymentsResponse
--
--         , responseListConnectorDefinitionVersions $
--             newListConnectorDefinitionVersionsResponse
--
--         , responseListConnectorDefinitions $
--             newListConnectorDefinitionsResponse
--
--         , responseListCoreDefinitionVersions $
--             newListCoreDefinitionVersionsResponse
--
--         , responseListCoreDefinitions $
--             newListCoreDefinitionsResponse
--
--         , responseListDeployments $
--             newListDeploymentsResponse
--
--         , responseListDeviceDefinitionVersions $
--             newListDeviceDefinitionVersionsResponse
--
--         , responseListDeviceDefinitions $
--             newListDeviceDefinitionsResponse
--
--         , responseListFunctionDefinitionVersions $
--             newListFunctionDefinitionVersionsResponse
--
--         , responseListFunctionDefinitions $
--             newListFunctionDefinitionsResponse
--
--         , responseListGroupCertificateAuthorities $
--             newListGroupCertificateAuthoritiesResponse
--
--         , responseListGroupVersions $
--             newListGroupVersionsResponse
--
--         , responseListGroups $
--             newListGroupsResponse
--
--         , responseListLoggerDefinitionVersions $
--             newListLoggerDefinitionVersionsResponse
--
--         , responseListLoggerDefinitions $
--             newListLoggerDefinitionsResponse
--
--         , responseListResourceDefinitionVersions $
--             newListResourceDefinitionVersionsResponse
--
--         , responseListResourceDefinitions $
--             newListResourceDefinitionsResponse
--
--         , responseListSubscriptionDefinitionVersions $
--             newListSubscriptionDefinitionVersionsResponse
--
--         , responseListSubscriptionDefinitions $
--             newListSubscriptionDefinitionsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseResetDeployments $
--             newResetDeploymentsResponse
--
--         , responseStartBulkDeployment $
--             newStartBulkDeploymentResponse
--
--         , responseStopBulkDeployment $
--             newStopBulkDeploymentResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateConnectivityInfo $
--             newUpdateConnectivityInfoResponse
--
--         , responseUpdateConnectorDefinition $
--             newUpdateConnectorDefinitionResponse
--
--         , responseUpdateCoreDefinition $
--             newUpdateCoreDefinitionResponse
--
--         , responseUpdateDeviceDefinition $
--             newUpdateDeviceDefinitionResponse
--
--         , responseUpdateFunctionDefinition $
--             newUpdateFunctionDefinitionResponse
--
--         , responseUpdateGroup $
--             newUpdateGroupResponse
--
--         , responseUpdateGroupCertificateConfiguration $
--             newUpdateGroupCertificateConfigurationResponse
--
--         , responseUpdateLoggerDefinition $
--             newUpdateLoggerDefinitionResponse
--
--         , responseUpdateResourceDefinition $
--             newUpdateResourceDefinitionResponse
--
--         , responseUpdateSubscriptionDefinition $
--             newUpdateSubscriptionDefinitionResponse
--
--         , responseUpdateThingRuntimeConfiguration $
--             newUpdateThingRuntimeConfigurationResponse
--
--           ]
--     ]

-- Requests

requestAssociateRoleToGroup :: AssociateRoleToGroup -> TestTree
requestAssociateRoleToGroup =
  req
    "AssociateRoleToGroup"
    "fixture/AssociateRoleToGroup.yaml"

requestAssociateServiceRoleToAccount :: AssociateServiceRoleToAccount -> TestTree
requestAssociateServiceRoleToAccount =
  req
    "AssociateServiceRoleToAccount"
    "fixture/AssociateServiceRoleToAccount.yaml"

requestCreateConnectorDefinition :: CreateConnectorDefinition -> TestTree
requestCreateConnectorDefinition =
  req
    "CreateConnectorDefinition"
    "fixture/CreateConnectorDefinition.yaml"

requestCreateConnectorDefinitionVersion :: CreateConnectorDefinitionVersion -> TestTree
requestCreateConnectorDefinitionVersion =
  req
    "CreateConnectorDefinitionVersion"
    "fixture/CreateConnectorDefinitionVersion.yaml"

requestCreateCoreDefinition :: CreateCoreDefinition -> TestTree
requestCreateCoreDefinition =
  req
    "CreateCoreDefinition"
    "fixture/CreateCoreDefinition.yaml"

requestCreateCoreDefinitionVersion :: CreateCoreDefinitionVersion -> TestTree
requestCreateCoreDefinitionVersion =
  req
    "CreateCoreDefinitionVersion"
    "fixture/CreateCoreDefinitionVersion.yaml"

requestCreateDeployment :: CreateDeployment -> TestTree
requestCreateDeployment =
  req
    "CreateDeployment"
    "fixture/CreateDeployment.yaml"

requestCreateDeviceDefinition :: CreateDeviceDefinition -> TestTree
requestCreateDeviceDefinition =
  req
    "CreateDeviceDefinition"
    "fixture/CreateDeviceDefinition.yaml"

requestCreateDeviceDefinitionVersion :: CreateDeviceDefinitionVersion -> TestTree
requestCreateDeviceDefinitionVersion =
  req
    "CreateDeviceDefinitionVersion"
    "fixture/CreateDeviceDefinitionVersion.yaml"

requestCreateFunctionDefinition :: CreateFunctionDefinition -> TestTree
requestCreateFunctionDefinition =
  req
    "CreateFunctionDefinition"
    "fixture/CreateFunctionDefinition.yaml"

requestCreateFunctionDefinitionVersion :: CreateFunctionDefinitionVersion -> TestTree
requestCreateFunctionDefinitionVersion =
  req
    "CreateFunctionDefinitionVersion"
    "fixture/CreateFunctionDefinitionVersion.yaml"

requestCreateGroup :: CreateGroup -> TestTree
requestCreateGroup =
  req
    "CreateGroup"
    "fixture/CreateGroup.yaml"

requestCreateGroupCertificateAuthority :: CreateGroupCertificateAuthority -> TestTree
requestCreateGroupCertificateAuthority =
  req
    "CreateGroupCertificateAuthority"
    "fixture/CreateGroupCertificateAuthority.yaml"

requestCreateGroupVersion :: CreateGroupVersion -> TestTree
requestCreateGroupVersion =
  req
    "CreateGroupVersion"
    "fixture/CreateGroupVersion.yaml"

requestCreateLoggerDefinition :: CreateLoggerDefinition -> TestTree
requestCreateLoggerDefinition =
  req
    "CreateLoggerDefinition"
    "fixture/CreateLoggerDefinition.yaml"

requestCreateLoggerDefinitionVersion :: CreateLoggerDefinitionVersion -> TestTree
requestCreateLoggerDefinitionVersion =
  req
    "CreateLoggerDefinitionVersion"
    "fixture/CreateLoggerDefinitionVersion.yaml"

requestCreateResourceDefinition :: CreateResourceDefinition -> TestTree
requestCreateResourceDefinition =
  req
    "CreateResourceDefinition"
    "fixture/CreateResourceDefinition.yaml"

requestCreateResourceDefinitionVersion :: CreateResourceDefinitionVersion -> TestTree
requestCreateResourceDefinitionVersion =
  req
    "CreateResourceDefinitionVersion"
    "fixture/CreateResourceDefinitionVersion.yaml"

requestCreateSoftwareUpdateJob :: CreateSoftwareUpdateJob -> TestTree
requestCreateSoftwareUpdateJob =
  req
    "CreateSoftwareUpdateJob"
    "fixture/CreateSoftwareUpdateJob.yaml"

requestCreateSubscriptionDefinition :: CreateSubscriptionDefinition -> TestTree
requestCreateSubscriptionDefinition =
  req
    "CreateSubscriptionDefinition"
    "fixture/CreateSubscriptionDefinition.yaml"

requestCreateSubscriptionDefinitionVersion :: CreateSubscriptionDefinitionVersion -> TestTree
requestCreateSubscriptionDefinitionVersion =
  req
    "CreateSubscriptionDefinitionVersion"
    "fixture/CreateSubscriptionDefinitionVersion.yaml"

requestDeleteConnectorDefinition :: DeleteConnectorDefinition -> TestTree
requestDeleteConnectorDefinition =
  req
    "DeleteConnectorDefinition"
    "fixture/DeleteConnectorDefinition.yaml"

requestDeleteCoreDefinition :: DeleteCoreDefinition -> TestTree
requestDeleteCoreDefinition =
  req
    "DeleteCoreDefinition"
    "fixture/DeleteCoreDefinition.yaml"

requestDeleteDeviceDefinition :: DeleteDeviceDefinition -> TestTree
requestDeleteDeviceDefinition =
  req
    "DeleteDeviceDefinition"
    "fixture/DeleteDeviceDefinition.yaml"

requestDeleteFunctionDefinition :: DeleteFunctionDefinition -> TestTree
requestDeleteFunctionDefinition =
  req
    "DeleteFunctionDefinition"
    "fixture/DeleteFunctionDefinition.yaml"

requestDeleteGroup :: DeleteGroup -> TestTree
requestDeleteGroup =
  req
    "DeleteGroup"
    "fixture/DeleteGroup.yaml"

requestDeleteLoggerDefinition :: DeleteLoggerDefinition -> TestTree
requestDeleteLoggerDefinition =
  req
    "DeleteLoggerDefinition"
    "fixture/DeleteLoggerDefinition.yaml"

requestDeleteResourceDefinition :: DeleteResourceDefinition -> TestTree
requestDeleteResourceDefinition =
  req
    "DeleteResourceDefinition"
    "fixture/DeleteResourceDefinition.yaml"

requestDeleteSubscriptionDefinition :: DeleteSubscriptionDefinition -> TestTree
requestDeleteSubscriptionDefinition =
  req
    "DeleteSubscriptionDefinition"
    "fixture/DeleteSubscriptionDefinition.yaml"

requestDisassociateRoleFromGroup :: DisassociateRoleFromGroup -> TestTree
requestDisassociateRoleFromGroup =
  req
    "DisassociateRoleFromGroup"
    "fixture/DisassociateRoleFromGroup.yaml"

requestDisassociateServiceRoleFromAccount :: DisassociateServiceRoleFromAccount -> TestTree
requestDisassociateServiceRoleFromAccount =
  req
    "DisassociateServiceRoleFromAccount"
    "fixture/DisassociateServiceRoleFromAccount.yaml"

requestGetAssociatedRole :: GetAssociatedRole -> TestTree
requestGetAssociatedRole =
  req
    "GetAssociatedRole"
    "fixture/GetAssociatedRole.yaml"

requestGetBulkDeploymentStatus :: GetBulkDeploymentStatus -> TestTree
requestGetBulkDeploymentStatus =
  req
    "GetBulkDeploymentStatus"
    "fixture/GetBulkDeploymentStatus.yaml"

requestGetConnectivityInfo :: GetConnectivityInfo -> TestTree
requestGetConnectivityInfo =
  req
    "GetConnectivityInfo"
    "fixture/GetConnectivityInfo.yaml"

requestGetConnectorDefinition :: GetConnectorDefinition -> TestTree
requestGetConnectorDefinition =
  req
    "GetConnectorDefinition"
    "fixture/GetConnectorDefinition.yaml"

requestGetConnectorDefinitionVersion :: GetConnectorDefinitionVersion -> TestTree
requestGetConnectorDefinitionVersion =
  req
    "GetConnectorDefinitionVersion"
    "fixture/GetConnectorDefinitionVersion.yaml"

requestGetCoreDefinition :: GetCoreDefinition -> TestTree
requestGetCoreDefinition =
  req
    "GetCoreDefinition"
    "fixture/GetCoreDefinition.yaml"

requestGetCoreDefinitionVersion :: GetCoreDefinitionVersion -> TestTree
requestGetCoreDefinitionVersion =
  req
    "GetCoreDefinitionVersion"
    "fixture/GetCoreDefinitionVersion.yaml"

requestGetDeploymentStatus :: GetDeploymentStatus -> TestTree
requestGetDeploymentStatus =
  req
    "GetDeploymentStatus"
    "fixture/GetDeploymentStatus.yaml"

requestGetDeviceDefinition :: GetDeviceDefinition -> TestTree
requestGetDeviceDefinition =
  req
    "GetDeviceDefinition"
    "fixture/GetDeviceDefinition.yaml"

requestGetDeviceDefinitionVersion :: GetDeviceDefinitionVersion -> TestTree
requestGetDeviceDefinitionVersion =
  req
    "GetDeviceDefinitionVersion"
    "fixture/GetDeviceDefinitionVersion.yaml"

requestGetFunctionDefinition :: GetFunctionDefinition -> TestTree
requestGetFunctionDefinition =
  req
    "GetFunctionDefinition"
    "fixture/GetFunctionDefinition.yaml"

requestGetFunctionDefinitionVersion :: GetFunctionDefinitionVersion -> TestTree
requestGetFunctionDefinitionVersion =
  req
    "GetFunctionDefinitionVersion"
    "fixture/GetFunctionDefinitionVersion.yaml"

requestGetGroup :: GetGroup -> TestTree
requestGetGroup =
  req
    "GetGroup"
    "fixture/GetGroup.yaml"

requestGetGroupCertificateAuthority :: GetGroupCertificateAuthority -> TestTree
requestGetGroupCertificateAuthority =
  req
    "GetGroupCertificateAuthority"
    "fixture/GetGroupCertificateAuthority.yaml"

requestGetGroupCertificateConfiguration :: GetGroupCertificateConfiguration -> TestTree
requestGetGroupCertificateConfiguration =
  req
    "GetGroupCertificateConfiguration"
    "fixture/GetGroupCertificateConfiguration.yaml"

requestGetGroupVersion :: GetGroupVersion -> TestTree
requestGetGroupVersion =
  req
    "GetGroupVersion"
    "fixture/GetGroupVersion.yaml"

requestGetLoggerDefinition :: GetLoggerDefinition -> TestTree
requestGetLoggerDefinition =
  req
    "GetLoggerDefinition"
    "fixture/GetLoggerDefinition.yaml"

requestGetLoggerDefinitionVersion :: GetLoggerDefinitionVersion -> TestTree
requestGetLoggerDefinitionVersion =
  req
    "GetLoggerDefinitionVersion"
    "fixture/GetLoggerDefinitionVersion.yaml"

requestGetResourceDefinition :: GetResourceDefinition -> TestTree
requestGetResourceDefinition =
  req
    "GetResourceDefinition"
    "fixture/GetResourceDefinition.yaml"

requestGetResourceDefinitionVersion :: GetResourceDefinitionVersion -> TestTree
requestGetResourceDefinitionVersion =
  req
    "GetResourceDefinitionVersion"
    "fixture/GetResourceDefinitionVersion.yaml"

requestGetServiceRoleForAccount :: GetServiceRoleForAccount -> TestTree
requestGetServiceRoleForAccount =
  req
    "GetServiceRoleForAccount"
    "fixture/GetServiceRoleForAccount.yaml"

requestGetSubscriptionDefinition :: GetSubscriptionDefinition -> TestTree
requestGetSubscriptionDefinition =
  req
    "GetSubscriptionDefinition"
    "fixture/GetSubscriptionDefinition.yaml"

requestGetSubscriptionDefinitionVersion :: GetSubscriptionDefinitionVersion -> TestTree
requestGetSubscriptionDefinitionVersion =
  req
    "GetSubscriptionDefinitionVersion"
    "fixture/GetSubscriptionDefinitionVersion.yaml"

requestGetThingRuntimeConfiguration :: GetThingRuntimeConfiguration -> TestTree
requestGetThingRuntimeConfiguration =
  req
    "GetThingRuntimeConfiguration"
    "fixture/GetThingRuntimeConfiguration.yaml"

requestListBulkDeploymentDetailedReports :: ListBulkDeploymentDetailedReports -> TestTree
requestListBulkDeploymentDetailedReports =
  req
    "ListBulkDeploymentDetailedReports"
    "fixture/ListBulkDeploymentDetailedReports.yaml"

requestListBulkDeployments :: ListBulkDeployments -> TestTree
requestListBulkDeployments =
  req
    "ListBulkDeployments"
    "fixture/ListBulkDeployments.yaml"

requestListConnectorDefinitionVersions :: ListConnectorDefinitionVersions -> TestTree
requestListConnectorDefinitionVersions =
  req
    "ListConnectorDefinitionVersions"
    "fixture/ListConnectorDefinitionVersions.yaml"

requestListConnectorDefinitions :: ListConnectorDefinitions -> TestTree
requestListConnectorDefinitions =
  req
    "ListConnectorDefinitions"
    "fixture/ListConnectorDefinitions.yaml"

requestListCoreDefinitionVersions :: ListCoreDefinitionVersions -> TestTree
requestListCoreDefinitionVersions =
  req
    "ListCoreDefinitionVersions"
    "fixture/ListCoreDefinitionVersions.yaml"

requestListCoreDefinitions :: ListCoreDefinitions -> TestTree
requestListCoreDefinitions =
  req
    "ListCoreDefinitions"
    "fixture/ListCoreDefinitions.yaml"

requestListDeployments :: ListDeployments -> TestTree
requestListDeployments =
  req
    "ListDeployments"
    "fixture/ListDeployments.yaml"

requestListDeviceDefinitionVersions :: ListDeviceDefinitionVersions -> TestTree
requestListDeviceDefinitionVersions =
  req
    "ListDeviceDefinitionVersions"
    "fixture/ListDeviceDefinitionVersions.yaml"

requestListDeviceDefinitions :: ListDeviceDefinitions -> TestTree
requestListDeviceDefinitions =
  req
    "ListDeviceDefinitions"
    "fixture/ListDeviceDefinitions.yaml"

requestListFunctionDefinitionVersions :: ListFunctionDefinitionVersions -> TestTree
requestListFunctionDefinitionVersions =
  req
    "ListFunctionDefinitionVersions"
    "fixture/ListFunctionDefinitionVersions.yaml"

requestListFunctionDefinitions :: ListFunctionDefinitions -> TestTree
requestListFunctionDefinitions =
  req
    "ListFunctionDefinitions"
    "fixture/ListFunctionDefinitions.yaml"

requestListGroupCertificateAuthorities :: ListGroupCertificateAuthorities -> TestTree
requestListGroupCertificateAuthorities =
  req
    "ListGroupCertificateAuthorities"
    "fixture/ListGroupCertificateAuthorities.yaml"

requestListGroupVersions :: ListGroupVersions -> TestTree
requestListGroupVersions =
  req
    "ListGroupVersions"
    "fixture/ListGroupVersions.yaml"

requestListGroups :: ListGroups -> TestTree
requestListGroups =
  req
    "ListGroups"
    "fixture/ListGroups.yaml"

requestListLoggerDefinitionVersions :: ListLoggerDefinitionVersions -> TestTree
requestListLoggerDefinitionVersions =
  req
    "ListLoggerDefinitionVersions"
    "fixture/ListLoggerDefinitionVersions.yaml"

requestListLoggerDefinitions :: ListLoggerDefinitions -> TestTree
requestListLoggerDefinitions =
  req
    "ListLoggerDefinitions"
    "fixture/ListLoggerDefinitions.yaml"

requestListResourceDefinitionVersions :: ListResourceDefinitionVersions -> TestTree
requestListResourceDefinitionVersions =
  req
    "ListResourceDefinitionVersions"
    "fixture/ListResourceDefinitionVersions.yaml"

requestListResourceDefinitions :: ListResourceDefinitions -> TestTree
requestListResourceDefinitions =
  req
    "ListResourceDefinitions"
    "fixture/ListResourceDefinitions.yaml"

requestListSubscriptionDefinitionVersions :: ListSubscriptionDefinitionVersions -> TestTree
requestListSubscriptionDefinitionVersions =
  req
    "ListSubscriptionDefinitionVersions"
    "fixture/ListSubscriptionDefinitionVersions.yaml"

requestListSubscriptionDefinitions :: ListSubscriptionDefinitions -> TestTree
requestListSubscriptionDefinitions =
  req
    "ListSubscriptionDefinitions"
    "fixture/ListSubscriptionDefinitions.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestResetDeployments :: ResetDeployments -> TestTree
requestResetDeployments =
  req
    "ResetDeployments"
    "fixture/ResetDeployments.yaml"

requestStartBulkDeployment :: StartBulkDeployment -> TestTree
requestStartBulkDeployment =
  req
    "StartBulkDeployment"
    "fixture/StartBulkDeployment.yaml"

requestStopBulkDeployment :: StopBulkDeployment -> TestTree
requestStopBulkDeployment =
  req
    "StopBulkDeployment"
    "fixture/StopBulkDeployment.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateConnectivityInfo :: UpdateConnectivityInfo -> TestTree
requestUpdateConnectivityInfo =
  req
    "UpdateConnectivityInfo"
    "fixture/UpdateConnectivityInfo.yaml"

requestUpdateConnectorDefinition :: UpdateConnectorDefinition -> TestTree
requestUpdateConnectorDefinition =
  req
    "UpdateConnectorDefinition"
    "fixture/UpdateConnectorDefinition.yaml"

requestUpdateCoreDefinition :: UpdateCoreDefinition -> TestTree
requestUpdateCoreDefinition =
  req
    "UpdateCoreDefinition"
    "fixture/UpdateCoreDefinition.yaml"

requestUpdateDeviceDefinition :: UpdateDeviceDefinition -> TestTree
requestUpdateDeviceDefinition =
  req
    "UpdateDeviceDefinition"
    "fixture/UpdateDeviceDefinition.yaml"

requestUpdateFunctionDefinition :: UpdateFunctionDefinition -> TestTree
requestUpdateFunctionDefinition =
  req
    "UpdateFunctionDefinition"
    "fixture/UpdateFunctionDefinition.yaml"

requestUpdateGroup :: UpdateGroup -> TestTree
requestUpdateGroup =
  req
    "UpdateGroup"
    "fixture/UpdateGroup.yaml"

requestUpdateGroupCertificateConfiguration :: UpdateGroupCertificateConfiguration -> TestTree
requestUpdateGroupCertificateConfiguration =
  req
    "UpdateGroupCertificateConfiguration"
    "fixture/UpdateGroupCertificateConfiguration.yaml"

requestUpdateLoggerDefinition :: UpdateLoggerDefinition -> TestTree
requestUpdateLoggerDefinition =
  req
    "UpdateLoggerDefinition"
    "fixture/UpdateLoggerDefinition.yaml"

requestUpdateResourceDefinition :: UpdateResourceDefinition -> TestTree
requestUpdateResourceDefinition =
  req
    "UpdateResourceDefinition"
    "fixture/UpdateResourceDefinition.yaml"

requestUpdateSubscriptionDefinition :: UpdateSubscriptionDefinition -> TestTree
requestUpdateSubscriptionDefinition =
  req
    "UpdateSubscriptionDefinition"
    "fixture/UpdateSubscriptionDefinition.yaml"

requestUpdateThingRuntimeConfiguration :: UpdateThingRuntimeConfiguration -> TestTree
requestUpdateThingRuntimeConfiguration =
  req
    "UpdateThingRuntimeConfiguration"
    "fixture/UpdateThingRuntimeConfiguration.yaml"

-- Responses

responseAssociateRoleToGroup :: AssociateRoleToGroupResponse -> TestTree
responseAssociateRoleToGroup =
  res
    "AssociateRoleToGroupResponse"
    "fixture/AssociateRoleToGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateRoleToGroup)

responseAssociateServiceRoleToAccount :: AssociateServiceRoleToAccountResponse -> TestTree
responseAssociateServiceRoleToAccount =
  res
    "AssociateServiceRoleToAccountResponse"
    "fixture/AssociateServiceRoleToAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateServiceRoleToAccount)

responseCreateConnectorDefinition :: CreateConnectorDefinitionResponse -> TestTree
responseCreateConnectorDefinition =
  res
    "CreateConnectorDefinitionResponse"
    "fixture/CreateConnectorDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConnectorDefinition)

responseCreateConnectorDefinitionVersion :: CreateConnectorDefinitionVersionResponse -> TestTree
responseCreateConnectorDefinitionVersion =
  res
    "CreateConnectorDefinitionVersionResponse"
    "fixture/CreateConnectorDefinitionVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateConnectorDefinitionVersion)

responseCreateCoreDefinition :: CreateCoreDefinitionResponse -> TestTree
responseCreateCoreDefinition =
  res
    "CreateCoreDefinitionResponse"
    "fixture/CreateCoreDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCoreDefinition)

responseCreateCoreDefinitionVersion :: CreateCoreDefinitionVersionResponse -> TestTree
responseCreateCoreDefinitionVersion =
  res
    "CreateCoreDefinitionVersionResponse"
    "fixture/CreateCoreDefinitionVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCoreDefinitionVersion)

responseCreateDeployment :: CreateDeploymentResponse -> TestTree
responseCreateDeployment =
  res
    "CreateDeploymentResponse"
    "fixture/CreateDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDeployment)

responseCreateDeviceDefinition :: CreateDeviceDefinitionResponse -> TestTree
responseCreateDeviceDefinition =
  res
    "CreateDeviceDefinitionResponse"
    "fixture/CreateDeviceDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDeviceDefinition)

responseCreateDeviceDefinitionVersion :: CreateDeviceDefinitionVersionResponse -> TestTree
responseCreateDeviceDefinitionVersion =
  res
    "CreateDeviceDefinitionVersionResponse"
    "fixture/CreateDeviceDefinitionVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDeviceDefinitionVersion)

responseCreateFunctionDefinition :: CreateFunctionDefinitionResponse -> TestTree
responseCreateFunctionDefinition =
  res
    "CreateFunctionDefinitionResponse"
    "fixture/CreateFunctionDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFunctionDefinition)

responseCreateFunctionDefinitionVersion :: CreateFunctionDefinitionVersionResponse -> TestTree
responseCreateFunctionDefinitionVersion =
  res
    "CreateFunctionDefinitionVersionResponse"
    "fixture/CreateFunctionDefinitionVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFunctionDefinitionVersion)

responseCreateGroup :: CreateGroupResponse -> TestTree
responseCreateGroup =
  res
    "CreateGroupResponse"
    "fixture/CreateGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGroup)

responseCreateGroupCertificateAuthority :: CreateGroupCertificateAuthorityResponse -> TestTree
responseCreateGroupCertificateAuthority =
  res
    "CreateGroupCertificateAuthorityResponse"
    "fixture/CreateGroupCertificateAuthorityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGroupCertificateAuthority)

responseCreateGroupVersion :: CreateGroupVersionResponse -> TestTree
responseCreateGroupVersion =
  res
    "CreateGroupVersionResponse"
    "fixture/CreateGroupVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGroupVersion)

responseCreateLoggerDefinition :: CreateLoggerDefinitionResponse -> TestTree
responseCreateLoggerDefinition =
  res
    "CreateLoggerDefinitionResponse"
    "fixture/CreateLoggerDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLoggerDefinition)

responseCreateLoggerDefinitionVersion :: CreateLoggerDefinitionVersionResponse -> TestTree
responseCreateLoggerDefinitionVersion =
  res
    "CreateLoggerDefinitionVersionResponse"
    "fixture/CreateLoggerDefinitionVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateLoggerDefinitionVersion)

responseCreateResourceDefinition :: CreateResourceDefinitionResponse -> TestTree
responseCreateResourceDefinition =
  res
    "CreateResourceDefinitionResponse"
    "fixture/CreateResourceDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateResourceDefinition)

responseCreateResourceDefinitionVersion :: CreateResourceDefinitionVersionResponse -> TestTree
responseCreateResourceDefinitionVersion =
  res
    "CreateResourceDefinitionVersionResponse"
    "fixture/CreateResourceDefinitionVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateResourceDefinitionVersion)

responseCreateSoftwareUpdateJob :: CreateSoftwareUpdateJobResponse -> TestTree
responseCreateSoftwareUpdateJob =
  res
    "CreateSoftwareUpdateJobResponse"
    "fixture/CreateSoftwareUpdateJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSoftwareUpdateJob)

responseCreateSubscriptionDefinition :: CreateSubscriptionDefinitionResponse -> TestTree
responseCreateSubscriptionDefinition =
  res
    "CreateSubscriptionDefinitionResponse"
    "fixture/CreateSubscriptionDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSubscriptionDefinition)

responseCreateSubscriptionDefinitionVersion :: CreateSubscriptionDefinitionVersionResponse -> TestTree
responseCreateSubscriptionDefinitionVersion =
  res
    "CreateSubscriptionDefinitionVersionResponse"
    "fixture/CreateSubscriptionDefinitionVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSubscriptionDefinitionVersion)

responseDeleteConnectorDefinition :: DeleteConnectorDefinitionResponse -> TestTree
responseDeleteConnectorDefinition =
  res
    "DeleteConnectorDefinitionResponse"
    "fixture/DeleteConnectorDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteConnectorDefinition)

responseDeleteCoreDefinition :: DeleteCoreDefinitionResponse -> TestTree
responseDeleteCoreDefinition =
  res
    "DeleteCoreDefinitionResponse"
    "fixture/DeleteCoreDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCoreDefinition)

responseDeleteDeviceDefinition :: DeleteDeviceDefinitionResponse -> TestTree
responseDeleteDeviceDefinition =
  res
    "DeleteDeviceDefinitionResponse"
    "fixture/DeleteDeviceDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDeviceDefinition)

responseDeleteFunctionDefinition :: DeleteFunctionDefinitionResponse -> TestTree
responseDeleteFunctionDefinition =
  res
    "DeleteFunctionDefinitionResponse"
    "fixture/DeleteFunctionDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFunctionDefinition)

responseDeleteGroup :: DeleteGroupResponse -> TestTree
responseDeleteGroup =
  res
    "DeleteGroupResponse"
    "fixture/DeleteGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGroup)

responseDeleteLoggerDefinition :: DeleteLoggerDefinitionResponse -> TestTree
responseDeleteLoggerDefinition =
  res
    "DeleteLoggerDefinitionResponse"
    "fixture/DeleteLoggerDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteLoggerDefinition)

responseDeleteResourceDefinition :: DeleteResourceDefinitionResponse -> TestTree
responseDeleteResourceDefinition =
  res
    "DeleteResourceDefinitionResponse"
    "fixture/DeleteResourceDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourceDefinition)

responseDeleteSubscriptionDefinition :: DeleteSubscriptionDefinitionResponse -> TestTree
responseDeleteSubscriptionDefinition =
  res
    "DeleteSubscriptionDefinitionResponse"
    "fixture/DeleteSubscriptionDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSubscriptionDefinition)

responseDisassociateRoleFromGroup :: DisassociateRoleFromGroupResponse -> TestTree
responseDisassociateRoleFromGroup =
  res
    "DisassociateRoleFromGroupResponse"
    "fixture/DisassociateRoleFromGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateRoleFromGroup)

responseDisassociateServiceRoleFromAccount :: DisassociateServiceRoleFromAccountResponse -> TestTree
responseDisassociateServiceRoleFromAccount =
  res
    "DisassociateServiceRoleFromAccountResponse"
    "fixture/DisassociateServiceRoleFromAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateServiceRoleFromAccount)

responseGetAssociatedRole :: GetAssociatedRoleResponse -> TestTree
responseGetAssociatedRole =
  res
    "GetAssociatedRoleResponse"
    "fixture/GetAssociatedRoleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAssociatedRole)

responseGetBulkDeploymentStatus :: GetBulkDeploymentStatusResponse -> TestTree
responseGetBulkDeploymentStatus =
  res
    "GetBulkDeploymentStatusResponse"
    "fixture/GetBulkDeploymentStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetBulkDeploymentStatus)

responseGetConnectivityInfo :: GetConnectivityInfoResponse -> TestTree
responseGetConnectivityInfo =
  res
    "GetConnectivityInfoResponse"
    "fixture/GetConnectivityInfoResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConnectivityInfo)

responseGetConnectorDefinition :: GetConnectorDefinitionResponse -> TestTree
responseGetConnectorDefinition =
  res
    "GetConnectorDefinitionResponse"
    "fixture/GetConnectorDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConnectorDefinition)

responseGetConnectorDefinitionVersion :: GetConnectorDefinitionVersionResponse -> TestTree
responseGetConnectorDefinitionVersion =
  res
    "GetConnectorDefinitionVersionResponse"
    "fixture/GetConnectorDefinitionVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConnectorDefinitionVersion)

responseGetCoreDefinition :: GetCoreDefinitionResponse -> TestTree
responseGetCoreDefinition =
  res
    "GetCoreDefinitionResponse"
    "fixture/GetCoreDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCoreDefinition)

responseGetCoreDefinitionVersion :: GetCoreDefinitionVersionResponse -> TestTree
responseGetCoreDefinitionVersion =
  res
    "GetCoreDefinitionVersionResponse"
    "fixture/GetCoreDefinitionVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCoreDefinitionVersion)

responseGetDeploymentStatus :: GetDeploymentStatusResponse -> TestTree
responseGetDeploymentStatus =
  res
    "GetDeploymentStatusResponse"
    "fixture/GetDeploymentStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeploymentStatus)

responseGetDeviceDefinition :: GetDeviceDefinitionResponse -> TestTree
responseGetDeviceDefinition =
  res
    "GetDeviceDefinitionResponse"
    "fixture/GetDeviceDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeviceDefinition)

responseGetDeviceDefinitionVersion :: GetDeviceDefinitionVersionResponse -> TestTree
responseGetDeviceDefinitionVersion =
  res
    "GetDeviceDefinitionVersionResponse"
    "fixture/GetDeviceDefinitionVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDeviceDefinitionVersion)

responseGetFunctionDefinition :: GetFunctionDefinitionResponse -> TestTree
responseGetFunctionDefinition =
  res
    "GetFunctionDefinitionResponse"
    "fixture/GetFunctionDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFunctionDefinition)

responseGetFunctionDefinitionVersion :: GetFunctionDefinitionVersionResponse -> TestTree
responseGetFunctionDefinitionVersion =
  res
    "GetFunctionDefinitionVersionResponse"
    "fixture/GetFunctionDefinitionVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFunctionDefinitionVersion)

responseGetGroup :: GetGroupResponse -> TestTree
responseGetGroup =
  res
    "GetGroupResponse"
    "fixture/GetGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGroup)

responseGetGroupCertificateAuthority :: GetGroupCertificateAuthorityResponse -> TestTree
responseGetGroupCertificateAuthority =
  res
    "GetGroupCertificateAuthorityResponse"
    "fixture/GetGroupCertificateAuthorityResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGroupCertificateAuthority)

responseGetGroupCertificateConfiguration :: GetGroupCertificateConfigurationResponse -> TestTree
responseGetGroupCertificateConfiguration =
  res
    "GetGroupCertificateConfigurationResponse"
    "fixture/GetGroupCertificateConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGroupCertificateConfiguration)

responseGetGroupVersion :: GetGroupVersionResponse -> TestTree
responseGetGroupVersion =
  res
    "GetGroupVersionResponse"
    "fixture/GetGroupVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetGroupVersion)

responseGetLoggerDefinition :: GetLoggerDefinitionResponse -> TestTree
responseGetLoggerDefinition =
  res
    "GetLoggerDefinitionResponse"
    "fixture/GetLoggerDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLoggerDefinition)

responseGetLoggerDefinitionVersion :: GetLoggerDefinitionVersionResponse -> TestTree
responseGetLoggerDefinitionVersion =
  res
    "GetLoggerDefinitionVersionResponse"
    "fixture/GetLoggerDefinitionVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLoggerDefinitionVersion)

responseGetResourceDefinition :: GetResourceDefinitionResponse -> TestTree
responseGetResourceDefinition =
  res
    "GetResourceDefinitionResponse"
    "fixture/GetResourceDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourceDefinition)

responseGetResourceDefinitionVersion :: GetResourceDefinitionVersionResponse -> TestTree
responseGetResourceDefinitionVersion =
  res
    "GetResourceDefinitionVersionResponse"
    "fixture/GetResourceDefinitionVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourceDefinitionVersion)

responseGetServiceRoleForAccount :: GetServiceRoleForAccountResponse -> TestTree
responseGetServiceRoleForAccount =
  res
    "GetServiceRoleForAccountResponse"
    "fixture/GetServiceRoleForAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetServiceRoleForAccount)

responseGetSubscriptionDefinition :: GetSubscriptionDefinitionResponse -> TestTree
responseGetSubscriptionDefinition =
  res
    "GetSubscriptionDefinitionResponse"
    "fixture/GetSubscriptionDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSubscriptionDefinition)

responseGetSubscriptionDefinitionVersion :: GetSubscriptionDefinitionVersionResponse -> TestTree
responseGetSubscriptionDefinitionVersion =
  res
    "GetSubscriptionDefinitionVersionResponse"
    "fixture/GetSubscriptionDefinitionVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSubscriptionDefinitionVersion)

responseGetThingRuntimeConfiguration :: GetThingRuntimeConfigurationResponse -> TestTree
responseGetThingRuntimeConfiguration =
  res
    "GetThingRuntimeConfigurationResponse"
    "fixture/GetThingRuntimeConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetThingRuntimeConfiguration)

responseListBulkDeploymentDetailedReports :: ListBulkDeploymentDetailedReportsResponse -> TestTree
responseListBulkDeploymentDetailedReports =
  res
    "ListBulkDeploymentDetailedReportsResponse"
    "fixture/ListBulkDeploymentDetailedReportsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBulkDeploymentDetailedReports)

responseListBulkDeployments :: ListBulkDeploymentsResponse -> TestTree
responseListBulkDeployments =
  res
    "ListBulkDeploymentsResponse"
    "fixture/ListBulkDeploymentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListBulkDeployments)

responseListConnectorDefinitionVersions :: ListConnectorDefinitionVersionsResponse -> TestTree
responseListConnectorDefinitionVersions =
  res
    "ListConnectorDefinitionVersionsResponse"
    "fixture/ListConnectorDefinitionVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConnectorDefinitionVersions)

responseListConnectorDefinitions :: ListConnectorDefinitionsResponse -> TestTree
responseListConnectorDefinitions =
  res
    "ListConnectorDefinitionsResponse"
    "fixture/ListConnectorDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListConnectorDefinitions)

responseListCoreDefinitionVersions :: ListCoreDefinitionVersionsResponse -> TestTree
responseListCoreDefinitionVersions =
  res
    "ListCoreDefinitionVersionsResponse"
    "fixture/ListCoreDefinitionVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCoreDefinitionVersions)

responseListCoreDefinitions :: ListCoreDefinitionsResponse -> TestTree
responseListCoreDefinitions =
  res
    "ListCoreDefinitionsResponse"
    "fixture/ListCoreDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCoreDefinitions)

responseListDeployments :: ListDeploymentsResponse -> TestTree
responseListDeployments =
  res
    "ListDeploymentsResponse"
    "fixture/ListDeploymentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDeployments)

responseListDeviceDefinitionVersions :: ListDeviceDefinitionVersionsResponse -> TestTree
responseListDeviceDefinitionVersions =
  res
    "ListDeviceDefinitionVersionsResponse"
    "fixture/ListDeviceDefinitionVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDeviceDefinitionVersions)

responseListDeviceDefinitions :: ListDeviceDefinitionsResponse -> TestTree
responseListDeviceDefinitions =
  res
    "ListDeviceDefinitionsResponse"
    "fixture/ListDeviceDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDeviceDefinitions)

responseListFunctionDefinitionVersions :: ListFunctionDefinitionVersionsResponse -> TestTree
responseListFunctionDefinitionVersions =
  res
    "ListFunctionDefinitionVersionsResponse"
    "fixture/ListFunctionDefinitionVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFunctionDefinitionVersions)

responseListFunctionDefinitions :: ListFunctionDefinitionsResponse -> TestTree
responseListFunctionDefinitions =
  res
    "ListFunctionDefinitionsResponse"
    "fixture/ListFunctionDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFunctionDefinitions)

responseListGroupCertificateAuthorities :: ListGroupCertificateAuthoritiesResponse -> TestTree
responseListGroupCertificateAuthorities =
  res
    "ListGroupCertificateAuthoritiesResponse"
    "fixture/ListGroupCertificateAuthoritiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGroupCertificateAuthorities)

responseListGroupVersions :: ListGroupVersionsResponse -> TestTree
responseListGroupVersions =
  res
    "ListGroupVersionsResponse"
    "fixture/ListGroupVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGroupVersions)

responseListGroups :: ListGroupsResponse -> TestTree
responseListGroups =
  res
    "ListGroupsResponse"
    "fixture/ListGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGroups)

responseListLoggerDefinitionVersions :: ListLoggerDefinitionVersionsResponse -> TestTree
responseListLoggerDefinitionVersions =
  res
    "ListLoggerDefinitionVersionsResponse"
    "fixture/ListLoggerDefinitionVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLoggerDefinitionVersions)

responseListLoggerDefinitions :: ListLoggerDefinitionsResponse -> TestTree
responseListLoggerDefinitions =
  res
    "ListLoggerDefinitionsResponse"
    "fixture/ListLoggerDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListLoggerDefinitions)

responseListResourceDefinitionVersions :: ListResourceDefinitionVersionsResponse -> TestTree
responseListResourceDefinitionVersions =
  res
    "ListResourceDefinitionVersionsResponse"
    "fixture/ListResourceDefinitionVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourceDefinitionVersions)

responseListResourceDefinitions :: ListResourceDefinitionsResponse -> TestTree
responseListResourceDefinitions =
  res
    "ListResourceDefinitionsResponse"
    "fixture/ListResourceDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourceDefinitions)

responseListSubscriptionDefinitionVersions :: ListSubscriptionDefinitionVersionsResponse -> TestTree
responseListSubscriptionDefinitionVersions =
  res
    "ListSubscriptionDefinitionVersionsResponse"
    "fixture/ListSubscriptionDefinitionVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSubscriptionDefinitionVersions)

responseListSubscriptionDefinitions :: ListSubscriptionDefinitionsResponse -> TestTree
responseListSubscriptionDefinitions =
  res
    "ListSubscriptionDefinitionsResponse"
    "fixture/ListSubscriptionDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListSubscriptionDefinitions)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseResetDeployments :: ResetDeploymentsResponse -> TestTree
responseResetDeployments =
  res
    "ResetDeploymentsResponse"
    "fixture/ResetDeploymentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetDeployments)

responseStartBulkDeployment :: StartBulkDeploymentResponse -> TestTree
responseStartBulkDeployment =
  res
    "StartBulkDeploymentResponse"
    "fixture/StartBulkDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartBulkDeployment)

responseStopBulkDeployment :: StopBulkDeploymentResponse -> TestTree
responseStopBulkDeployment =
  res
    "StopBulkDeploymentResponse"
    "fixture/StopBulkDeploymentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopBulkDeployment)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateConnectivityInfo :: UpdateConnectivityInfoResponse -> TestTree
responseUpdateConnectivityInfo =
  res
    "UpdateConnectivityInfoResponse"
    "fixture/UpdateConnectivityInfoResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConnectivityInfo)

responseUpdateConnectorDefinition :: UpdateConnectorDefinitionResponse -> TestTree
responseUpdateConnectorDefinition =
  res
    "UpdateConnectorDefinitionResponse"
    "fixture/UpdateConnectorDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateConnectorDefinition)

responseUpdateCoreDefinition :: UpdateCoreDefinitionResponse -> TestTree
responseUpdateCoreDefinition =
  res
    "UpdateCoreDefinitionResponse"
    "fixture/UpdateCoreDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCoreDefinition)

responseUpdateDeviceDefinition :: UpdateDeviceDefinitionResponse -> TestTree
responseUpdateDeviceDefinition =
  res
    "UpdateDeviceDefinitionResponse"
    "fixture/UpdateDeviceDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDeviceDefinition)

responseUpdateFunctionDefinition :: UpdateFunctionDefinitionResponse -> TestTree
responseUpdateFunctionDefinition =
  res
    "UpdateFunctionDefinitionResponse"
    "fixture/UpdateFunctionDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFunctionDefinition)

responseUpdateGroup :: UpdateGroupResponse -> TestTree
responseUpdateGroup =
  res
    "UpdateGroupResponse"
    "fixture/UpdateGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGroup)

responseUpdateGroupCertificateConfiguration :: UpdateGroupCertificateConfigurationResponse -> TestTree
responseUpdateGroupCertificateConfiguration =
  res
    "UpdateGroupCertificateConfigurationResponse"
    "fixture/UpdateGroupCertificateConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateGroupCertificateConfiguration)

responseUpdateLoggerDefinition :: UpdateLoggerDefinitionResponse -> TestTree
responseUpdateLoggerDefinition =
  res
    "UpdateLoggerDefinitionResponse"
    "fixture/UpdateLoggerDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateLoggerDefinition)

responseUpdateResourceDefinition :: UpdateResourceDefinitionResponse -> TestTree
responseUpdateResourceDefinition =
  res
    "UpdateResourceDefinitionResponse"
    "fixture/UpdateResourceDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResourceDefinition)

responseUpdateSubscriptionDefinition :: UpdateSubscriptionDefinitionResponse -> TestTree
responseUpdateSubscriptionDefinition =
  res
    "UpdateSubscriptionDefinitionResponse"
    "fixture/UpdateSubscriptionDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSubscriptionDefinition)

responseUpdateThingRuntimeConfiguration :: UpdateThingRuntimeConfigurationResponse -> TestTree
responseUpdateThingRuntimeConfiguration =
  res
    "UpdateThingRuntimeConfigurationResponse"
    "fixture/UpdateThingRuntimeConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateThingRuntimeConfiguration)
