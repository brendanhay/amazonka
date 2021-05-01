{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.WorkMail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--         [ requestCreateOrganization $
--             newCreateOrganization
--
--         , requestDeleteAlias $
--             newDeleteAlias
--
--         , requestStartMailboxExportJob $
--             newStartMailboxExportJob
--
--         , requestDescribeResource $
--             newDescribeResource
--
--         , requestListResourceDelegates $
--             newListResourceDelegates
--
--         , requestDeleteAccessControlRule $
--             newDeleteAccessControlRule
--
--         , requestDisassociateDelegateFromResource $
--             newDisassociateDelegateFromResource
--
--         , requestGetDefaultRetentionPolicy $
--             newGetDefaultRetentionPolicy
--
--         , requestListGroups $
--             newListGroups
--
--         , requestListMailboxExportJobs $
--             newListMailboxExportJobs
--
--         , requestDescribeOrganization $
--             newDescribeOrganization
--
--         , requestCreateResource $
--             newCreateResource
--
--         , requestUpdateResource $
--             newUpdateResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDeleteResource $
--             newDeleteResource
--
--         , requestCreateGroup $
--             newCreateGroup
--
--         , requestTagResource $
--             newTagResource
--
--         , requestAssociateMemberToGroup $
--             newAssociateMemberToGroup
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestPutRetentionPolicy $
--             newPutRetentionPolicy
--
--         , requestPutMailboxPermissions $
--             newPutMailboxPermissions
--
--         , requestAssociateDelegateToResource $
--             newAssociateDelegateToResource
--
--         , requestRegisterToWorkMail $
--             newRegisterToWorkMail
--
--         , requestDeleteOrganization $
--             newDeleteOrganization
--
--         , requestDescribeMailboxExportJob $
--             newDescribeMailboxExportJob
--
--         , requestListOrganizations $
--             newListOrganizations
--
--         , requestUpdatePrimaryEmailAddress $
--             newUpdatePrimaryEmailAddress
--
--         , requestListAccessControlRules $
--             newListAccessControlRules
--
--         , requestPutAccessControlRule $
--             newPutAccessControlRule
--
--         , requestDescribeUser $
--             newDescribeUser
--
--         , requestCancelMailboxExportJob $
--             newCancelMailboxExportJob
--
--         , requestDeregisterFromWorkMail $
--             newDeregisterFromWorkMail
--
--         , requestListGroupMembers $
--             newListGroupMembers
--
--         , requestDeleteGroup $
--             newDeleteGroup
--
--         , requestListMailboxPermissions $
--             newListMailboxPermissions
--
--         , requestDisassociateMemberFromGroup $
--             newDisassociateMemberFromGroup
--
--         , requestUpdateMailboxQuota $
--             newUpdateMailboxQuota
--
--         , requestListResources $
--             newListResources
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestListUsers $
--             newListUsers
--
--         , requestGetMailboxDetails $
--             newGetMailboxDetails
--
--         , requestDeleteMailboxPermissions $
--             newDeleteMailboxPermissions
--
--         , requestDeleteRetentionPolicy $
--             newDeleteRetentionPolicy
--
--         , requestDescribeGroup $
--             newDescribeGroup
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestCreateAlias $
--             newCreateAlias
--
--         , requestGetAccessControlEffect $
--             newGetAccessControlEffect
--
--         , requestListAliases $
--             newListAliases
--
--         , requestResetPassword $
--             newResetPassword
--
--           ]

--     , testGroup "response"
--         [ responseCreateOrganization $
--             newCreateOrganizationResponse
--
--         , responseDeleteAlias $
--             newDeleteAliasResponse
--
--         , responseStartMailboxExportJob $
--             newStartMailboxExportJobResponse
--
--         , responseDescribeResource $
--             newDescribeResourceResponse
--
--         , responseListResourceDelegates $
--             newListResourceDelegatesResponse
--
--         , responseDeleteAccessControlRule $
--             newDeleteAccessControlRuleResponse
--
--         , responseDisassociateDelegateFromResource $
--             newDisassociateDelegateFromResourceResponse
--
--         , responseGetDefaultRetentionPolicy $
--             newGetDefaultRetentionPolicyResponse
--
--         , responseListGroups $
--             newListGroupsResponse
--
--         , responseListMailboxExportJobs $
--             newListMailboxExportJobsResponse
--
--         , responseDescribeOrganization $
--             newDescribeOrganizationResponse
--
--         , responseCreateResource $
--             newCreateResourceResponse
--
--         , responseUpdateResource $
--             newUpdateResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDeleteResource $
--             newDeleteResourceResponse
--
--         , responseCreateGroup $
--             newCreateGroupResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseAssociateMemberToGroup $
--             newAssociateMemberToGroupResponse
--
--         , responseCreateUser $
--             newCreateUserResponse
--
--         , responsePutRetentionPolicy $
--             newPutRetentionPolicyResponse
--
--         , responsePutMailboxPermissions $
--             newPutMailboxPermissionsResponse
--
--         , responseAssociateDelegateToResource $
--             newAssociateDelegateToResourceResponse
--
--         , responseRegisterToWorkMail $
--             newRegisterToWorkMailResponse
--
--         , responseDeleteOrganization $
--             newDeleteOrganizationResponse
--
--         , responseDescribeMailboxExportJob $
--             newDescribeMailboxExportJobResponse
--
--         , responseListOrganizations $
--             newListOrganizationsResponse
--
--         , responseUpdatePrimaryEmailAddress $
--             newUpdatePrimaryEmailAddressResponse
--
--         , responseListAccessControlRules $
--             newListAccessControlRulesResponse
--
--         , responsePutAccessControlRule $
--             newPutAccessControlRuleResponse
--
--         , responseDescribeUser $
--             newDescribeUserResponse
--
--         , responseCancelMailboxExportJob $
--             newCancelMailboxExportJobResponse
--
--         , responseDeregisterFromWorkMail $
--             newDeregisterFromWorkMailResponse
--
--         , responseListGroupMembers $
--             newListGroupMembersResponse
--
--         , responseDeleteGroup $
--             newDeleteGroupResponse
--
--         , responseListMailboxPermissions $
--             newListMailboxPermissionsResponse
--
--         , responseDisassociateMemberFromGroup $
--             newDisassociateMemberFromGroupResponse
--
--         , responseUpdateMailboxQuota $
--             newUpdateMailboxQuotaResponse
--
--         , responseListResources $
--             newListResourcesResponse
--
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseListUsers $
--             newListUsersResponse
--
--         , responseGetMailboxDetails $
--             newGetMailboxDetailsResponse
--
--         , responseDeleteMailboxPermissions $
--             newDeleteMailboxPermissionsResponse
--
--         , responseDeleteRetentionPolicy $
--             newDeleteRetentionPolicyResponse
--
--         , responseDescribeGroup $
--             newDescribeGroupResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseCreateAlias $
--             newCreateAliasResponse
--
--         , responseGetAccessControlEffect $
--             newGetAccessControlEffectResponse
--
--         , responseListAliases $
--             newListAliasesResponse
--
--         , responseResetPassword $
--             newResetPasswordResponse
--
--           ]
--     ]

-- Requests

requestCreateOrganization :: CreateOrganization -> TestTree
requestCreateOrganization =
  req
    "CreateOrganization"
    "fixture/CreateOrganization.yaml"

requestDeleteAlias :: DeleteAlias -> TestTree
requestDeleteAlias =
  req
    "DeleteAlias"
    "fixture/DeleteAlias.yaml"

requestStartMailboxExportJob :: StartMailboxExportJob -> TestTree
requestStartMailboxExportJob =
  req
    "StartMailboxExportJob"
    "fixture/StartMailboxExportJob.yaml"

requestDescribeResource :: DescribeResource -> TestTree
requestDescribeResource =
  req
    "DescribeResource"
    "fixture/DescribeResource.yaml"

requestListResourceDelegates :: ListResourceDelegates -> TestTree
requestListResourceDelegates =
  req
    "ListResourceDelegates"
    "fixture/ListResourceDelegates.yaml"

requestDeleteAccessControlRule :: DeleteAccessControlRule -> TestTree
requestDeleteAccessControlRule =
  req
    "DeleteAccessControlRule"
    "fixture/DeleteAccessControlRule.yaml"

requestDisassociateDelegateFromResource :: DisassociateDelegateFromResource -> TestTree
requestDisassociateDelegateFromResource =
  req
    "DisassociateDelegateFromResource"
    "fixture/DisassociateDelegateFromResource.yaml"

requestGetDefaultRetentionPolicy :: GetDefaultRetentionPolicy -> TestTree
requestGetDefaultRetentionPolicy =
  req
    "GetDefaultRetentionPolicy"
    "fixture/GetDefaultRetentionPolicy.yaml"

requestListGroups :: ListGroups -> TestTree
requestListGroups =
  req
    "ListGroups"
    "fixture/ListGroups.yaml"

requestListMailboxExportJobs :: ListMailboxExportJobs -> TestTree
requestListMailboxExportJobs =
  req
    "ListMailboxExportJobs"
    "fixture/ListMailboxExportJobs.yaml"

requestDescribeOrganization :: DescribeOrganization -> TestTree
requestDescribeOrganization =
  req
    "DescribeOrganization"
    "fixture/DescribeOrganization.yaml"

requestCreateResource :: CreateResource -> TestTree
requestCreateResource =
  req
    "CreateResource"
    "fixture/CreateResource.yaml"

requestUpdateResource :: UpdateResource -> TestTree
requestUpdateResource =
  req
    "UpdateResource"
    "fixture/UpdateResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDeleteResource :: DeleteResource -> TestTree
requestDeleteResource =
  req
    "DeleteResource"
    "fixture/DeleteResource.yaml"

requestCreateGroup :: CreateGroup -> TestTree
requestCreateGroup =
  req
    "CreateGroup"
    "fixture/CreateGroup.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestAssociateMemberToGroup :: AssociateMemberToGroup -> TestTree
requestAssociateMemberToGroup =
  req
    "AssociateMemberToGroup"
    "fixture/AssociateMemberToGroup.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser =
  req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestPutRetentionPolicy :: PutRetentionPolicy -> TestTree
requestPutRetentionPolicy =
  req
    "PutRetentionPolicy"
    "fixture/PutRetentionPolicy.yaml"

requestPutMailboxPermissions :: PutMailboxPermissions -> TestTree
requestPutMailboxPermissions =
  req
    "PutMailboxPermissions"
    "fixture/PutMailboxPermissions.yaml"

requestAssociateDelegateToResource :: AssociateDelegateToResource -> TestTree
requestAssociateDelegateToResource =
  req
    "AssociateDelegateToResource"
    "fixture/AssociateDelegateToResource.yaml"

requestRegisterToWorkMail :: RegisterToWorkMail -> TestTree
requestRegisterToWorkMail =
  req
    "RegisterToWorkMail"
    "fixture/RegisterToWorkMail.yaml"

requestDeleteOrganization :: DeleteOrganization -> TestTree
requestDeleteOrganization =
  req
    "DeleteOrganization"
    "fixture/DeleteOrganization.yaml"

requestDescribeMailboxExportJob :: DescribeMailboxExportJob -> TestTree
requestDescribeMailboxExportJob =
  req
    "DescribeMailboxExportJob"
    "fixture/DescribeMailboxExportJob.yaml"

requestListOrganizations :: ListOrganizations -> TestTree
requestListOrganizations =
  req
    "ListOrganizations"
    "fixture/ListOrganizations.yaml"

requestUpdatePrimaryEmailAddress :: UpdatePrimaryEmailAddress -> TestTree
requestUpdatePrimaryEmailAddress =
  req
    "UpdatePrimaryEmailAddress"
    "fixture/UpdatePrimaryEmailAddress.yaml"

requestListAccessControlRules :: ListAccessControlRules -> TestTree
requestListAccessControlRules =
  req
    "ListAccessControlRules"
    "fixture/ListAccessControlRules.yaml"

requestPutAccessControlRule :: PutAccessControlRule -> TestTree
requestPutAccessControlRule =
  req
    "PutAccessControlRule"
    "fixture/PutAccessControlRule.yaml"

requestDescribeUser :: DescribeUser -> TestTree
requestDescribeUser =
  req
    "DescribeUser"
    "fixture/DescribeUser.yaml"

requestCancelMailboxExportJob :: CancelMailboxExportJob -> TestTree
requestCancelMailboxExportJob =
  req
    "CancelMailboxExportJob"
    "fixture/CancelMailboxExportJob.yaml"

requestDeregisterFromWorkMail :: DeregisterFromWorkMail -> TestTree
requestDeregisterFromWorkMail =
  req
    "DeregisterFromWorkMail"
    "fixture/DeregisterFromWorkMail.yaml"

requestListGroupMembers :: ListGroupMembers -> TestTree
requestListGroupMembers =
  req
    "ListGroupMembers"
    "fixture/ListGroupMembers.yaml"

requestDeleteGroup :: DeleteGroup -> TestTree
requestDeleteGroup =
  req
    "DeleteGroup"
    "fixture/DeleteGroup.yaml"

requestListMailboxPermissions :: ListMailboxPermissions -> TestTree
requestListMailboxPermissions =
  req
    "ListMailboxPermissions"
    "fixture/ListMailboxPermissions.yaml"

requestDisassociateMemberFromGroup :: DisassociateMemberFromGroup -> TestTree
requestDisassociateMemberFromGroup =
  req
    "DisassociateMemberFromGroup"
    "fixture/DisassociateMemberFromGroup.yaml"

requestUpdateMailboxQuota :: UpdateMailboxQuota -> TestTree
requestUpdateMailboxQuota =
  req
    "UpdateMailboxQuota"
    "fixture/UpdateMailboxQuota.yaml"

requestListResources :: ListResources -> TestTree
requestListResources =
  req
    "ListResources"
    "fixture/ListResources.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestListUsers :: ListUsers -> TestTree
requestListUsers =
  req
    "ListUsers"
    "fixture/ListUsers.yaml"

requestGetMailboxDetails :: GetMailboxDetails -> TestTree
requestGetMailboxDetails =
  req
    "GetMailboxDetails"
    "fixture/GetMailboxDetails.yaml"

requestDeleteMailboxPermissions :: DeleteMailboxPermissions -> TestTree
requestDeleteMailboxPermissions =
  req
    "DeleteMailboxPermissions"
    "fixture/DeleteMailboxPermissions.yaml"

requestDeleteRetentionPolicy :: DeleteRetentionPolicy -> TestTree
requestDeleteRetentionPolicy =
  req
    "DeleteRetentionPolicy"
    "fixture/DeleteRetentionPolicy.yaml"

requestDescribeGroup :: DescribeGroup -> TestTree
requestDescribeGroup =
  req
    "DescribeGroup"
    "fixture/DescribeGroup.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestCreateAlias :: CreateAlias -> TestTree
requestCreateAlias =
  req
    "CreateAlias"
    "fixture/CreateAlias.yaml"

requestGetAccessControlEffect :: GetAccessControlEffect -> TestTree
requestGetAccessControlEffect =
  req
    "GetAccessControlEffect"
    "fixture/GetAccessControlEffect.yaml"

requestListAliases :: ListAliases -> TestTree
requestListAliases =
  req
    "ListAliases"
    "fixture/ListAliases.yaml"

requestResetPassword :: ResetPassword -> TestTree
requestResetPassword =
  req
    "ResetPassword"
    "fixture/ResetPassword.yaml"

-- Responses

responseCreateOrganization :: CreateOrganizationResponse -> TestTree
responseCreateOrganization =
  res
    "CreateOrganizationResponse"
    "fixture/CreateOrganizationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateOrganization)

responseDeleteAlias :: DeleteAliasResponse -> TestTree
responseDeleteAlias =
  res
    "DeleteAliasResponse"
    "fixture/DeleteAliasResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAlias)

responseStartMailboxExportJob :: StartMailboxExportJobResponse -> TestTree
responseStartMailboxExportJob =
  res
    "StartMailboxExportJobResponse"
    "fixture/StartMailboxExportJobResponse.proto"
    defaultService
    (Proxy :: Proxy StartMailboxExportJob)

responseDescribeResource :: DescribeResourceResponse -> TestTree
responseDescribeResource =
  res
    "DescribeResourceResponse"
    "fixture/DescribeResourceResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeResource)

responseListResourceDelegates :: ListResourceDelegatesResponse -> TestTree
responseListResourceDelegates =
  res
    "ListResourceDelegatesResponse"
    "fixture/ListResourceDelegatesResponse.proto"
    defaultService
    (Proxy :: Proxy ListResourceDelegates)

responseDeleteAccessControlRule :: DeleteAccessControlRuleResponse -> TestTree
responseDeleteAccessControlRule =
  res
    "DeleteAccessControlRuleResponse"
    "fixture/DeleteAccessControlRuleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAccessControlRule)

responseDisassociateDelegateFromResource :: DisassociateDelegateFromResourceResponse -> TestTree
responseDisassociateDelegateFromResource =
  res
    "DisassociateDelegateFromResourceResponse"
    "fixture/DisassociateDelegateFromResourceResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateDelegateFromResource)

responseGetDefaultRetentionPolicy :: GetDefaultRetentionPolicyResponse -> TestTree
responseGetDefaultRetentionPolicy =
  res
    "GetDefaultRetentionPolicyResponse"
    "fixture/GetDefaultRetentionPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy GetDefaultRetentionPolicy)

responseListGroups :: ListGroupsResponse -> TestTree
responseListGroups =
  res
    "ListGroupsResponse"
    "fixture/ListGroupsResponse.proto"
    defaultService
    (Proxy :: Proxy ListGroups)

responseListMailboxExportJobs :: ListMailboxExportJobsResponse -> TestTree
responseListMailboxExportJobs =
  res
    "ListMailboxExportJobsResponse"
    "fixture/ListMailboxExportJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListMailboxExportJobs)

responseDescribeOrganization :: DescribeOrganizationResponse -> TestTree
responseDescribeOrganization =
  res
    "DescribeOrganizationResponse"
    "fixture/DescribeOrganizationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeOrganization)

responseCreateResource :: CreateResourceResponse -> TestTree
responseCreateResource =
  res
    "CreateResourceResponse"
    "fixture/CreateResourceResponse.proto"
    defaultService
    (Proxy :: Proxy CreateResource)

responseUpdateResource :: UpdateResourceResponse -> TestTree
responseUpdateResource =
  res
    "UpdateResourceResponse"
    "fixture/UpdateResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseDeleteResource :: DeleteResourceResponse -> TestTree
responseDeleteResource =
  res
    "DeleteResourceResponse"
    "fixture/DeleteResourceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteResource)

responseCreateGroup :: CreateGroupResponse -> TestTree
responseCreateGroup =
  res
    "CreateGroupResponse"
    "fixture/CreateGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateGroup)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseAssociateMemberToGroup :: AssociateMemberToGroupResponse -> TestTree
responseAssociateMemberToGroup =
  res
    "AssociateMemberToGroupResponse"
    "fixture/AssociateMemberToGroupResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateMemberToGroup)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUser)

responsePutRetentionPolicy :: PutRetentionPolicyResponse -> TestTree
responsePutRetentionPolicy =
  res
    "PutRetentionPolicyResponse"
    "fixture/PutRetentionPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutRetentionPolicy)

responsePutMailboxPermissions :: PutMailboxPermissionsResponse -> TestTree
responsePutMailboxPermissions =
  res
    "PutMailboxPermissionsResponse"
    "fixture/PutMailboxPermissionsResponse.proto"
    defaultService
    (Proxy :: Proxy PutMailboxPermissions)

responseAssociateDelegateToResource :: AssociateDelegateToResourceResponse -> TestTree
responseAssociateDelegateToResource =
  res
    "AssociateDelegateToResourceResponse"
    "fixture/AssociateDelegateToResourceResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateDelegateToResource)

responseRegisterToWorkMail :: RegisterToWorkMailResponse -> TestTree
responseRegisterToWorkMail =
  res
    "RegisterToWorkMailResponse"
    "fixture/RegisterToWorkMailResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterToWorkMail)

responseDeleteOrganization :: DeleteOrganizationResponse -> TestTree
responseDeleteOrganization =
  res
    "DeleteOrganizationResponse"
    "fixture/DeleteOrganizationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteOrganization)

responseDescribeMailboxExportJob :: DescribeMailboxExportJobResponse -> TestTree
responseDescribeMailboxExportJob =
  res
    "DescribeMailboxExportJobResponse"
    "fixture/DescribeMailboxExportJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeMailboxExportJob)

responseListOrganizations :: ListOrganizationsResponse -> TestTree
responseListOrganizations =
  res
    "ListOrganizationsResponse"
    "fixture/ListOrganizationsResponse.proto"
    defaultService
    (Proxy :: Proxy ListOrganizations)

responseUpdatePrimaryEmailAddress :: UpdatePrimaryEmailAddressResponse -> TestTree
responseUpdatePrimaryEmailAddress =
  res
    "UpdatePrimaryEmailAddressResponse"
    "fixture/UpdatePrimaryEmailAddressResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePrimaryEmailAddress)

responseListAccessControlRules :: ListAccessControlRulesResponse -> TestTree
responseListAccessControlRules =
  res
    "ListAccessControlRulesResponse"
    "fixture/ListAccessControlRulesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAccessControlRules)

responsePutAccessControlRule :: PutAccessControlRuleResponse -> TestTree
responsePutAccessControlRule =
  res
    "PutAccessControlRuleResponse"
    "fixture/PutAccessControlRuleResponse.proto"
    defaultService
    (Proxy :: Proxy PutAccessControlRule)

responseDescribeUser :: DescribeUserResponse -> TestTree
responseDescribeUser =
  res
    "DescribeUserResponse"
    "fixture/DescribeUserResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUser)

responseCancelMailboxExportJob :: CancelMailboxExportJobResponse -> TestTree
responseCancelMailboxExportJob =
  res
    "CancelMailboxExportJobResponse"
    "fixture/CancelMailboxExportJobResponse.proto"
    defaultService
    (Proxy :: Proxy CancelMailboxExportJob)

responseDeregisterFromWorkMail :: DeregisterFromWorkMailResponse -> TestTree
responseDeregisterFromWorkMail =
  res
    "DeregisterFromWorkMailResponse"
    "fixture/DeregisterFromWorkMailResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterFromWorkMail)

responseListGroupMembers :: ListGroupMembersResponse -> TestTree
responseListGroupMembers =
  res
    "ListGroupMembersResponse"
    "fixture/ListGroupMembersResponse.proto"
    defaultService
    (Proxy :: Proxy ListGroupMembers)

responseDeleteGroup :: DeleteGroupResponse -> TestTree
responseDeleteGroup =
  res
    "DeleteGroupResponse"
    "fixture/DeleteGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteGroup)

responseListMailboxPermissions :: ListMailboxPermissionsResponse -> TestTree
responseListMailboxPermissions =
  res
    "ListMailboxPermissionsResponse"
    "fixture/ListMailboxPermissionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListMailboxPermissions)

responseDisassociateMemberFromGroup :: DisassociateMemberFromGroupResponse -> TestTree
responseDisassociateMemberFromGroup =
  res
    "DisassociateMemberFromGroupResponse"
    "fixture/DisassociateMemberFromGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateMemberFromGroup)

responseUpdateMailboxQuota :: UpdateMailboxQuotaResponse -> TestTree
responseUpdateMailboxQuota =
  res
    "UpdateMailboxQuotaResponse"
    "fixture/UpdateMailboxQuotaResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateMailboxQuota)

responseListResources :: ListResourcesResponse -> TestTree
responseListResources =
  res
    "ListResourcesResponse"
    "fixture/ListResourcesResponse.proto"
    defaultService
    (Proxy :: Proxy ListResources)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUser)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers =
  res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    defaultService
    (Proxy :: Proxy ListUsers)

responseGetMailboxDetails :: GetMailboxDetailsResponse -> TestTree
responseGetMailboxDetails =
  res
    "GetMailboxDetailsResponse"
    "fixture/GetMailboxDetailsResponse.proto"
    defaultService
    (Proxy :: Proxy GetMailboxDetails)

responseDeleteMailboxPermissions :: DeleteMailboxPermissionsResponse -> TestTree
responseDeleteMailboxPermissions =
  res
    "DeleteMailboxPermissionsResponse"
    "fixture/DeleteMailboxPermissionsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMailboxPermissions)

responseDeleteRetentionPolicy :: DeleteRetentionPolicyResponse -> TestTree
responseDeleteRetentionPolicy =
  res
    "DeleteRetentionPolicyResponse"
    "fixture/DeleteRetentionPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRetentionPolicy)

responseDescribeGroup :: DescribeGroupResponse -> TestTree
responseDescribeGroup =
  res
    "DescribeGroupResponse"
    "fixture/DescribeGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeGroup)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseCreateAlias :: CreateAliasResponse -> TestTree
responseCreateAlias =
  res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAlias)

responseGetAccessControlEffect :: GetAccessControlEffectResponse -> TestTree
responseGetAccessControlEffect =
  res
    "GetAccessControlEffectResponse"
    "fixture/GetAccessControlEffectResponse.proto"
    defaultService
    (Proxy :: Proxy GetAccessControlEffect)

responseListAliases :: ListAliasesResponse -> TestTree
responseListAliases =
  res
    "ListAliasesResponse"
    "fixture/ListAliasesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAliases)

responseResetPassword :: ResetPasswordResponse -> TestTree
responseResetPassword =
  res
    "ResetPasswordResponse"
    "fixture/ResetPasswordResponse.proto"
    defaultService
    (Proxy :: Proxy ResetPassword)
