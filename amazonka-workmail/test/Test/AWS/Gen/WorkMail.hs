{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.WorkMail
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.WorkMail where

import Data.Proxy
import Network.AWS.WorkMail
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.WorkMail.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestUpdatePrimaryEmailAddress $
--             updatePrimaryEmailAddress
--
--         , requestDescribeResource $
--             describeResource
--
--         , requestCreateAlias $
--             createAlias
--
--         , requestResetPassword $
--             resetPassword
--
--         , requestDescribeGroup $
--             describeGroup
--
--         , requestRegisterToWorkMail $
--             registerToWorkMail
--
--         , requestListAliases $
--             listAliases
--
--         , requestPutMailboxPermissions $
--             putMailboxPermissions
--
--         , requestDeleteMailboxPermissions $
--             deleteMailboxPermissions
--
--         , requestListUsers $
--             listUsers
--
--         , requestAssociateMemberToGroup $
--             associateMemberToGroup
--
--         , requestDeleteResource $
--             deleteResource
--
--         , requestUpdateResource $
--             updateResource
--
--         , requestDisassociateMemberFromGroup $
--             disassociateMemberFromGroup
--
--         , requestListResources $
--             listResources
--
--         , requestDeregisterFromWorkMail $
--             deregisterFromWorkMail
--
--         , requestListMailboxPermissions $
--             listMailboxPermissions
--
--         , requestListGroupMembers $
--             listGroupMembers
--
--         , requestDisassociateDelegateFromResource $
--             disassociateDelegateFromResource
--
--         , requestListResourceDelegates $
--             listResourceDelegates
--
--         , requestDescribeUser $
--             describeUser
--
--         , requestDeleteAlias $
--             deleteAlias
--
--         , requestListOrganizations $
--             listOrganizations
--
--         , requestAssociateDelegateToResource $
--             associateDelegateToResource
--
--         , requestCreateUser $
--             createUser
--
--         , requestDeleteUser $
--             deleteUser
--
--         , requestCreateGroup $
--             createGroup
--
--         , requestDeleteGroup $
--             deleteGroup
--
--         , requestListGroups $
--             listGroups
--
--         , requestDescribeOrganization $
--             describeOrganization
--
--         , requestCreateResource $
--             createResource
--
--           ]

--     , testGroup "response"
--         [ responseUpdatePrimaryEmailAddress $
--             updatePrimaryEmailAddressResponse
--
--         , responseDescribeResource $
--             describeResourceResponse
--
--         , responseCreateAlias $
--             createAliasResponse
--
--         , responseResetPassword $
--             resetPasswordResponse
--
--         , responseDescribeGroup $
--             describeGroupResponse
--
--         , responseRegisterToWorkMail $
--             registerToWorkMailResponse
--
--         , responseListAliases $
--             listAliasesResponse
--
--         , responsePutMailboxPermissions $
--             putMailboxPermissionsResponse
--
--         , responseDeleteMailboxPermissions $
--             deleteMailboxPermissionsResponse
--
--         , responseListUsers $
--             listUsersResponse
--
--         , responseAssociateMemberToGroup $
--             associateMemberToGroupResponse
--
--         , responseDeleteResource $
--             deleteResourceResponse
--
--         , responseUpdateResource $
--             updateResourceResponse
--
--         , responseDisassociateMemberFromGroup $
--             disassociateMemberFromGroupResponse
--
--         , responseListResources $
--             listResourcesResponse
--
--         , responseDeregisterFromWorkMail $
--             deregisterFromWorkMailResponse
--
--         , responseListMailboxPermissions $
--             listMailboxPermissionsResponse
--
--         , responseListGroupMembers $
--             listGroupMembersResponse
--
--         , responseDisassociateDelegateFromResource $
--             disassociateDelegateFromResourceResponse
--
--         , responseListResourceDelegates $
--             listResourceDelegatesResponse
--
--         , responseDescribeUser $
--             describeUserResponse
--
--         , responseDeleteAlias $
--             deleteAliasResponse
--
--         , responseListOrganizations $
--             listOrganizationsResponse
--
--         , responseAssociateDelegateToResource $
--             associateDelegateToResourceResponse
--
--         , responseCreateUser $
--             createUserResponse
--
--         , responseDeleteUser $
--             deleteUserResponse
--
--         , responseCreateGroup $
--             createGroupResponse
--
--         , responseDeleteGroup $
--             deleteGroupResponse
--
--         , responseListGroups $
--             listGroupsResponse
--
--         , responseDescribeOrganization $
--             describeOrganizationResponse
--
--         , responseCreateResource $
--             createResourceResponse
--
--           ]
--     ]

-- Requests

requestUpdatePrimaryEmailAddress :: UpdatePrimaryEmailAddress -> TestTree
requestUpdatePrimaryEmailAddress = req
    "UpdatePrimaryEmailAddress"
    "fixture/UpdatePrimaryEmailAddress.yaml"

requestDescribeResource :: DescribeResource -> TestTree
requestDescribeResource = req
    "DescribeResource"
    "fixture/DescribeResource.yaml"

requestCreateAlias :: CreateAlias -> TestTree
requestCreateAlias = req
    "CreateAlias"
    "fixture/CreateAlias.yaml"

requestResetPassword :: ResetPassword -> TestTree
requestResetPassword = req
    "ResetPassword"
    "fixture/ResetPassword.yaml"

requestDescribeGroup :: DescribeGroup -> TestTree
requestDescribeGroup = req
    "DescribeGroup"
    "fixture/DescribeGroup.yaml"

requestRegisterToWorkMail :: RegisterToWorkMail -> TestTree
requestRegisterToWorkMail = req
    "RegisterToWorkMail"
    "fixture/RegisterToWorkMail.yaml"

requestListAliases :: ListAliases -> TestTree
requestListAliases = req
    "ListAliases"
    "fixture/ListAliases.yaml"

requestPutMailboxPermissions :: PutMailboxPermissions -> TestTree
requestPutMailboxPermissions = req
    "PutMailboxPermissions"
    "fixture/PutMailboxPermissions.yaml"

requestDeleteMailboxPermissions :: DeleteMailboxPermissions -> TestTree
requestDeleteMailboxPermissions = req
    "DeleteMailboxPermissions"
    "fixture/DeleteMailboxPermissions.yaml"

requestListUsers :: ListUsers -> TestTree
requestListUsers = req
    "ListUsers"
    "fixture/ListUsers.yaml"

requestAssociateMemberToGroup :: AssociateMemberToGroup -> TestTree
requestAssociateMemberToGroup = req
    "AssociateMemberToGroup"
    "fixture/AssociateMemberToGroup.yaml"

requestDeleteResource :: DeleteResource -> TestTree
requestDeleteResource = req
    "DeleteResource"
    "fixture/DeleteResource.yaml"

requestUpdateResource :: UpdateResource -> TestTree
requestUpdateResource = req
    "UpdateResource"
    "fixture/UpdateResource.yaml"

requestDisassociateMemberFromGroup :: DisassociateMemberFromGroup -> TestTree
requestDisassociateMemberFromGroup = req
    "DisassociateMemberFromGroup"
    "fixture/DisassociateMemberFromGroup.yaml"

requestListResources :: ListResources -> TestTree
requestListResources = req
    "ListResources"
    "fixture/ListResources.yaml"

requestDeregisterFromWorkMail :: DeregisterFromWorkMail -> TestTree
requestDeregisterFromWorkMail = req
    "DeregisterFromWorkMail"
    "fixture/DeregisterFromWorkMail.yaml"

requestListMailboxPermissions :: ListMailboxPermissions -> TestTree
requestListMailboxPermissions = req
    "ListMailboxPermissions"
    "fixture/ListMailboxPermissions.yaml"

requestListGroupMembers :: ListGroupMembers -> TestTree
requestListGroupMembers = req
    "ListGroupMembers"
    "fixture/ListGroupMembers.yaml"

requestDisassociateDelegateFromResource :: DisassociateDelegateFromResource -> TestTree
requestDisassociateDelegateFromResource = req
    "DisassociateDelegateFromResource"
    "fixture/DisassociateDelegateFromResource.yaml"

requestListResourceDelegates :: ListResourceDelegates -> TestTree
requestListResourceDelegates = req
    "ListResourceDelegates"
    "fixture/ListResourceDelegates.yaml"

requestDescribeUser :: DescribeUser -> TestTree
requestDescribeUser = req
    "DescribeUser"
    "fixture/DescribeUser.yaml"

requestDeleteAlias :: DeleteAlias -> TestTree
requestDeleteAlias = req
    "DeleteAlias"
    "fixture/DeleteAlias.yaml"

requestListOrganizations :: ListOrganizations -> TestTree
requestListOrganizations = req
    "ListOrganizations"
    "fixture/ListOrganizations.yaml"

requestAssociateDelegateToResource :: AssociateDelegateToResource -> TestTree
requestAssociateDelegateToResource = req
    "AssociateDelegateToResource"
    "fixture/AssociateDelegateToResource.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser = req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser = req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestCreateGroup :: CreateGroup -> TestTree
requestCreateGroup = req
    "CreateGroup"
    "fixture/CreateGroup.yaml"

requestDeleteGroup :: DeleteGroup -> TestTree
requestDeleteGroup = req
    "DeleteGroup"
    "fixture/DeleteGroup.yaml"

requestListGroups :: ListGroups -> TestTree
requestListGroups = req
    "ListGroups"
    "fixture/ListGroups.yaml"

requestDescribeOrganization :: DescribeOrganization -> TestTree
requestDescribeOrganization = req
    "DescribeOrganization"
    "fixture/DescribeOrganization.yaml"

requestCreateResource :: CreateResource -> TestTree
requestCreateResource = req
    "CreateResource"
    "fixture/CreateResource.yaml"

-- Responses

responseUpdatePrimaryEmailAddress :: UpdatePrimaryEmailAddressResponse -> TestTree
responseUpdatePrimaryEmailAddress = res
    "UpdatePrimaryEmailAddressResponse"
    "fixture/UpdatePrimaryEmailAddressResponse.proto"
    workMail
    (Proxy :: Proxy UpdatePrimaryEmailAddress)

responseDescribeResource :: DescribeResourceResponse -> TestTree
responseDescribeResource = res
    "DescribeResourceResponse"
    "fixture/DescribeResourceResponse.proto"
    workMail
    (Proxy :: Proxy DescribeResource)

responseCreateAlias :: CreateAliasResponse -> TestTree
responseCreateAlias = res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    workMail
    (Proxy :: Proxy CreateAlias)

responseResetPassword :: ResetPasswordResponse -> TestTree
responseResetPassword = res
    "ResetPasswordResponse"
    "fixture/ResetPasswordResponse.proto"
    workMail
    (Proxy :: Proxy ResetPassword)

responseDescribeGroup :: DescribeGroupResponse -> TestTree
responseDescribeGroup = res
    "DescribeGroupResponse"
    "fixture/DescribeGroupResponse.proto"
    workMail
    (Proxy :: Proxy DescribeGroup)

responseRegisterToWorkMail :: RegisterToWorkMailResponse -> TestTree
responseRegisterToWorkMail = res
    "RegisterToWorkMailResponse"
    "fixture/RegisterToWorkMailResponse.proto"
    workMail
    (Proxy :: Proxy RegisterToWorkMail)

responseListAliases :: ListAliasesResponse -> TestTree
responseListAliases = res
    "ListAliasesResponse"
    "fixture/ListAliasesResponse.proto"
    workMail
    (Proxy :: Proxy ListAliases)

responsePutMailboxPermissions :: PutMailboxPermissionsResponse -> TestTree
responsePutMailboxPermissions = res
    "PutMailboxPermissionsResponse"
    "fixture/PutMailboxPermissionsResponse.proto"
    workMail
    (Proxy :: Proxy PutMailboxPermissions)

responseDeleteMailboxPermissions :: DeleteMailboxPermissionsResponse -> TestTree
responseDeleteMailboxPermissions = res
    "DeleteMailboxPermissionsResponse"
    "fixture/DeleteMailboxPermissionsResponse.proto"
    workMail
    (Proxy :: Proxy DeleteMailboxPermissions)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers = res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    workMail
    (Proxy :: Proxy ListUsers)

responseAssociateMemberToGroup :: AssociateMemberToGroupResponse -> TestTree
responseAssociateMemberToGroup = res
    "AssociateMemberToGroupResponse"
    "fixture/AssociateMemberToGroupResponse.proto"
    workMail
    (Proxy :: Proxy AssociateMemberToGroup)

responseDeleteResource :: DeleteResourceResponse -> TestTree
responseDeleteResource = res
    "DeleteResourceResponse"
    "fixture/DeleteResourceResponse.proto"
    workMail
    (Proxy :: Proxy DeleteResource)

responseUpdateResource :: UpdateResourceResponse -> TestTree
responseUpdateResource = res
    "UpdateResourceResponse"
    "fixture/UpdateResourceResponse.proto"
    workMail
    (Proxy :: Proxy UpdateResource)

responseDisassociateMemberFromGroup :: DisassociateMemberFromGroupResponse -> TestTree
responseDisassociateMemberFromGroup = res
    "DisassociateMemberFromGroupResponse"
    "fixture/DisassociateMemberFromGroupResponse.proto"
    workMail
    (Proxy :: Proxy DisassociateMemberFromGroup)

responseListResources :: ListResourcesResponse -> TestTree
responseListResources = res
    "ListResourcesResponse"
    "fixture/ListResourcesResponse.proto"
    workMail
    (Proxy :: Proxy ListResources)

responseDeregisterFromWorkMail :: DeregisterFromWorkMailResponse -> TestTree
responseDeregisterFromWorkMail = res
    "DeregisterFromWorkMailResponse"
    "fixture/DeregisterFromWorkMailResponse.proto"
    workMail
    (Proxy :: Proxy DeregisterFromWorkMail)

responseListMailboxPermissions :: ListMailboxPermissionsResponse -> TestTree
responseListMailboxPermissions = res
    "ListMailboxPermissionsResponse"
    "fixture/ListMailboxPermissionsResponse.proto"
    workMail
    (Proxy :: Proxy ListMailboxPermissions)

responseListGroupMembers :: ListGroupMembersResponse -> TestTree
responseListGroupMembers = res
    "ListGroupMembersResponse"
    "fixture/ListGroupMembersResponse.proto"
    workMail
    (Proxy :: Proxy ListGroupMembers)

responseDisassociateDelegateFromResource :: DisassociateDelegateFromResourceResponse -> TestTree
responseDisassociateDelegateFromResource = res
    "DisassociateDelegateFromResourceResponse"
    "fixture/DisassociateDelegateFromResourceResponse.proto"
    workMail
    (Proxy :: Proxy DisassociateDelegateFromResource)

responseListResourceDelegates :: ListResourceDelegatesResponse -> TestTree
responseListResourceDelegates = res
    "ListResourceDelegatesResponse"
    "fixture/ListResourceDelegatesResponse.proto"
    workMail
    (Proxy :: Proxy ListResourceDelegates)

responseDescribeUser :: DescribeUserResponse -> TestTree
responseDescribeUser = res
    "DescribeUserResponse"
    "fixture/DescribeUserResponse.proto"
    workMail
    (Proxy :: Proxy DescribeUser)

responseDeleteAlias :: DeleteAliasResponse -> TestTree
responseDeleteAlias = res
    "DeleteAliasResponse"
    "fixture/DeleteAliasResponse.proto"
    workMail
    (Proxy :: Proxy DeleteAlias)

responseListOrganizations :: ListOrganizationsResponse -> TestTree
responseListOrganizations = res
    "ListOrganizationsResponse"
    "fixture/ListOrganizationsResponse.proto"
    workMail
    (Proxy :: Proxy ListOrganizations)

responseAssociateDelegateToResource :: AssociateDelegateToResourceResponse -> TestTree
responseAssociateDelegateToResource = res
    "AssociateDelegateToResourceResponse"
    "fixture/AssociateDelegateToResourceResponse.proto"
    workMail
    (Proxy :: Proxy AssociateDelegateToResource)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser = res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    workMail
    (Proxy :: Proxy CreateUser)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser = res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    workMail
    (Proxy :: Proxy DeleteUser)

responseCreateGroup :: CreateGroupResponse -> TestTree
responseCreateGroup = res
    "CreateGroupResponse"
    "fixture/CreateGroupResponse.proto"
    workMail
    (Proxy :: Proxy CreateGroup)

responseDeleteGroup :: DeleteGroupResponse -> TestTree
responseDeleteGroup = res
    "DeleteGroupResponse"
    "fixture/DeleteGroupResponse.proto"
    workMail
    (Proxy :: Proxy DeleteGroup)

responseListGroups :: ListGroupsResponse -> TestTree
responseListGroups = res
    "ListGroupsResponse"
    "fixture/ListGroupsResponse.proto"
    workMail
    (Proxy :: Proxy ListGroups)

responseDescribeOrganization :: DescribeOrganizationResponse -> TestTree
responseDescribeOrganization = res
    "DescribeOrganizationResponse"
    "fixture/DescribeOrganizationResponse.proto"
    workMail
    (Proxy :: Proxy DescribeOrganization)

responseCreateResource :: CreateResourceResponse -> TestTree
responseCreateResource = res
    "CreateResourceResponse"
    "fixture/CreateResourceResponse.proto"
    workMail
    (Proxy :: Proxy CreateResource)
