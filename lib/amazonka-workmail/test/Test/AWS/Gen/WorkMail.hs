{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.WorkMail
-- Copyright   : (c) 2013-2020 Brendan Hay
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
--         [ requestUpdatePrimaryEmailAddress $
--             mkUpdatePrimaryEmailAddress
--
--         , requestDescribeResource $
--             mkDescribeResource
--
--         , requestCreateOrganization $
--             mkCreateOrganization
--
--         , requestCreateAlias $
--             mkCreateAlias
--
--         , requestDeleteOrganization $
--             mkDeleteOrganization
--
--         , requestResetPassword $
--             mkResetPassword
--
--         , requestDescribeGroup $
--             mkDescribeGroup
--
--         , requestDescribeMailboxExportJob $
--             mkDescribeMailboxExportJob
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestRegisterToWorkMail $
--             mkRegisterToWorkMail
--
--         , requestListAliases $
--             mkListAliases
--
--         , requestPutMailboxPermissions $
--             mkPutMailboxPermissions
--
--         , requestDeleteMailboxPermissions $
--             mkDeleteMailboxPermissions
--
--         , requestListUsers $
--             mkListUsers
--
--         , requestGetMailboxDetails $
--             mkGetMailboxDetails
--
--         , requestAssociateMemberToGroup $
--             mkAssociateMemberToGroup
--
--         , requestDeleteResource $
--             mkDeleteResource
--
--         , requestUpdateResource $
--             mkUpdateResource
--
--         , requestDisassociateMemberFromGroup $
--             mkDisassociateMemberFromGroup
--
--         , requestListResources $
--             mkListResources
--
--         , requestDeregisterFromWorkMail $
--             mkDeregisterFromWorkMail
--
--         , requestListMailboxExportJobs $
--             mkListMailboxExportJobs
--
--         , requestListMailboxPermissions $
--             mkListMailboxPermissions
--
--         , requestListGroupMembers $
--             mkListGroupMembers
--
--         , requestDisassociateDelegateFromResource $
--             mkDisassociateDelegateFromResource
--
--         , requestDeleteAccessControlRule $
--             mkDeleteAccessControlRule
--
--         , requestListResourceDelegates $
--             mkListResourceDelegates
--
--         , requestListAccessControlRules $
--             mkListAccessControlRules
--
--         , requestDescribeUser $
--             mkDescribeUser
--
--         , requestPutAccessControlRule $
--             mkPutAccessControlRule
--
--         , requestStartMailboxExportJob $
--             mkStartMailboxExportJob
--
--         , requestDeleteAlias $
--             mkDeleteAlias
--
--         , requestListOrganizations $
--             mkListOrganizations
--
--         , requestAssociateDelegateToResource $
--             mkAssociateDelegateToResource
--
--         , requestGetAccessControlEffect $
--             mkGetAccessControlEffect
--
--         , requestDeleteRetentionPolicy $
--             mkDeleteRetentionPolicy
--
--         , requestCreateUser $
--             mkCreateUser
--
--         , requestPutRetentionPolicy $
--             mkPutRetentionPolicy
--
--         , requestDeleteUser $
--             mkDeleteUser
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestCreateGroup $
--             mkCreateGroup
--
--         , requestUpdateMailboxQuota $
--             mkUpdateMailboxQuota
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestDeleteGroup $
--             mkDeleteGroup
--
--         , requestListGroups $
--             mkListGroups
--
--         , requestDescribeOrganization $
--             mkDescribeOrganization
--
--         , requestCreateResource $
--             mkCreateResource
--
--         , requestGetDefaultRetentionPolicy $
--             mkGetDefaultRetentionPolicy
--
--         , requestCancelMailboxExportJob $
--             mkCancelMailboxExportJob
--
--           ]

--     , testGroup "response"
--         [ responseUpdatePrimaryEmailAddress $
--             mkUpdatePrimaryEmailAddressResponse
--
--         , responseDescribeResource $
--             mkDescribeResourceResponse
--
--         , responseCreateOrganization $
--             mkCreateOrganizationResponse
--
--         , responseCreateAlias $
--             mkCreateAliasResponse
--
--         , responseDeleteOrganization $
--             mkDeleteOrganizationResponse
--
--         , responseResetPassword $
--             mkResetPasswordResponse
--
--         , responseDescribeGroup $
--             mkDescribeGroupResponse
--
--         , responseDescribeMailboxExportJob $
--             mkDescribeMailboxExportJobResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseRegisterToWorkMail $
--             mkRegisterToWorkMailResponse
--
--         , responseListAliases $
--             mkListAliasesResponse
--
--         , responsePutMailboxPermissions $
--             mkPutMailboxPermissionsResponse
--
--         , responseDeleteMailboxPermissions $
--             mkDeleteMailboxPermissionsResponse
--
--         , responseListUsers $
--             mkListUsersResponse
--
--         , responseGetMailboxDetails $
--             mkGetMailboxDetailsResponse
--
--         , responseAssociateMemberToGroup $
--             mkAssociateMemberToGroupResponse
--
--         , responseDeleteResource $
--             mkDeleteResourceResponse
--
--         , responseUpdateResource $
--             mkUpdateResourceResponse
--
--         , responseDisassociateMemberFromGroup $
--             mkDisassociateMemberFromGroupResponse
--
--         , responseListResources $
--             mkListResourcesResponse
--
--         , responseDeregisterFromWorkMail $
--             mkDeregisterFromWorkMailResponse
--
--         , responseListMailboxExportJobs $
--             mkListMailboxExportJobsResponse
--
--         , responseListMailboxPermissions $
--             mkListMailboxPermissionsResponse
--
--         , responseListGroupMembers $
--             mkListGroupMembersResponse
--
--         , responseDisassociateDelegateFromResource $
--             mkDisassociateDelegateFromResourceResponse
--
--         , responseDeleteAccessControlRule $
--             mkDeleteAccessControlRuleResponse
--
--         , responseListResourceDelegates $
--             mkListResourceDelegatesResponse
--
--         , responseListAccessControlRules $
--             mkListAccessControlRulesResponse
--
--         , responseDescribeUser $
--             mkDescribeUserResponse
--
--         , responsePutAccessControlRule $
--             mkPutAccessControlRuleResponse
--
--         , responseStartMailboxExportJob $
--             mkStartMailboxExportJobResponse
--
--         , responseDeleteAlias $
--             mkDeleteAliasResponse
--
--         , responseListOrganizations $
--             mkListOrganizationsResponse
--
--         , responseAssociateDelegateToResource $
--             mkAssociateDelegateToResourceResponse
--
--         , responseGetAccessControlEffect $
--             mkGetAccessControlEffectResponse
--
--         , responseDeleteRetentionPolicy $
--             mkDeleteRetentionPolicyResponse
--
--         , responseCreateUser $
--             mkCreateUserResponse
--
--         , responsePutRetentionPolicy $
--             mkPutRetentionPolicyResponse
--
--         , responseDeleteUser $
--             mkDeleteUserResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseCreateGroup $
--             mkCreateGroupResponse
--
--         , responseUpdateMailboxQuota $
--             mkUpdateMailboxQuotaResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseDeleteGroup $
--             mkDeleteGroupResponse
--
--         , responseListGroups $
--             mkListGroupsResponse
--
--         , responseDescribeOrganization $
--             mkDescribeOrganizationResponse
--
--         , responseCreateResource $
--             mkCreateResourceResponse
--
--         , responseGetDefaultRetentionPolicy $
--             mkGetDefaultRetentionPolicyResponse
--
--         , responseCancelMailboxExportJob $
--             mkCancelMailboxExportJobResponse
--
--           ]
--     ]

-- Requests

requestUpdatePrimaryEmailAddress :: UpdatePrimaryEmailAddress -> TestTree
requestUpdatePrimaryEmailAddress =
  req
    "UpdatePrimaryEmailAddress"
    "fixture/UpdatePrimaryEmailAddress.yaml"

requestDescribeResource :: DescribeResource -> TestTree
requestDescribeResource =
  req
    "DescribeResource"
    "fixture/DescribeResource.yaml"

requestCreateOrganization :: CreateOrganization -> TestTree
requestCreateOrganization =
  req
    "CreateOrganization"
    "fixture/CreateOrganization.yaml"

requestCreateAlias :: CreateAlias -> TestTree
requestCreateAlias =
  req
    "CreateAlias"
    "fixture/CreateAlias.yaml"

requestDeleteOrganization :: DeleteOrganization -> TestTree
requestDeleteOrganization =
  req
    "DeleteOrganization"
    "fixture/DeleteOrganization.yaml"

requestResetPassword :: ResetPassword -> TestTree
requestResetPassword =
  req
    "ResetPassword"
    "fixture/ResetPassword.yaml"

requestDescribeGroup :: DescribeGroup -> TestTree
requestDescribeGroup =
  req
    "DescribeGroup"
    "fixture/DescribeGroup.yaml"

requestDescribeMailboxExportJob :: DescribeMailboxExportJob -> TestTree
requestDescribeMailboxExportJob =
  req
    "DescribeMailboxExportJob"
    "fixture/DescribeMailboxExportJob.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestRegisterToWorkMail :: RegisterToWorkMail -> TestTree
requestRegisterToWorkMail =
  req
    "RegisterToWorkMail"
    "fixture/RegisterToWorkMail.yaml"

requestListAliases :: ListAliases -> TestTree
requestListAliases =
  req
    "ListAliases"
    "fixture/ListAliases.yaml"

requestPutMailboxPermissions :: PutMailboxPermissions -> TestTree
requestPutMailboxPermissions =
  req
    "PutMailboxPermissions"
    "fixture/PutMailboxPermissions.yaml"

requestDeleteMailboxPermissions :: DeleteMailboxPermissions -> TestTree
requestDeleteMailboxPermissions =
  req
    "DeleteMailboxPermissions"
    "fixture/DeleteMailboxPermissions.yaml"

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

requestAssociateMemberToGroup :: AssociateMemberToGroup -> TestTree
requestAssociateMemberToGroup =
  req
    "AssociateMemberToGroup"
    "fixture/AssociateMemberToGroup.yaml"

requestDeleteResource :: DeleteResource -> TestTree
requestDeleteResource =
  req
    "DeleteResource"
    "fixture/DeleteResource.yaml"

requestUpdateResource :: UpdateResource -> TestTree
requestUpdateResource =
  req
    "UpdateResource"
    "fixture/UpdateResource.yaml"

requestDisassociateMemberFromGroup :: DisassociateMemberFromGroup -> TestTree
requestDisassociateMemberFromGroup =
  req
    "DisassociateMemberFromGroup"
    "fixture/DisassociateMemberFromGroup.yaml"

requestListResources :: ListResources -> TestTree
requestListResources =
  req
    "ListResources"
    "fixture/ListResources.yaml"

requestDeregisterFromWorkMail :: DeregisterFromWorkMail -> TestTree
requestDeregisterFromWorkMail =
  req
    "DeregisterFromWorkMail"
    "fixture/DeregisterFromWorkMail.yaml"

requestListMailboxExportJobs :: ListMailboxExportJobs -> TestTree
requestListMailboxExportJobs =
  req
    "ListMailboxExportJobs"
    "fixture/ListMailboxExportJobs.yaml"

requestListMailboxPermissions :: ListMailboxPermissions -> TestTree
requestListMailboxPermissions =
  req
    "ListMailboxPermissions"
    "fixture/ListMailboxPermissions.yaml"

requestListGroupMembers :: ListGroupMembers -> TestTree
requestListGroupMembers =
  req
    "ListGroupMembers"
    "fixture/ListGroupMembers.yaml"

requestDisassociateDelegateFromResource :: DisassociateDelegateFromResource -> TestTree
requestDisassociateDelegateFromResource =
  req
    "DisassociateDelegateFromResource"
    "fixture/DisassociateDelegateFromResource.yaml"

requestDeleteAccessControlRule :: DeleteAccessControlRule -> TestTree
requestDeleteAccessControlRule =
  req
    "DeleteAccessControlRule"
    "fixture/DeleteAccessControlRule.yaml"

requestListResourceDelegates :: ListResourceDelegates -> TestTree
requestListResourceDelegates =
  req
    "ListResourceDelegates"
    "fixture/ListResourceDelegates.yaml"

requestListAccessControlRules :: ListAccessControlRules -> TestTree
requestListAccessControlRules =
  req
    "ListAccessControlRules"
    "fixture/ListAccessControlRules.yaml"

requestDescribeUser :: DescribeUser -> TestTree
requestDescribeUser =
  req
    "DescribeUser"
    "fixture/DescribeUser.yaml"

requestPutAccessControlRule :: PutAccessControlRule -> TestTree
requestPutAccessControlRule =
  req
    "PutAccessControlRule"
    "fixture/PutAccessControlRule.yaml"

requestStartMailboxExportJob :: StartMailboxExportJob -> TestTree
requestStartMailboxExportJob =
  req
    "StartMailboxExportJob"
    "fixture/StartMailboxExportJob.yaml"

requestDeleteAlias :: DeleteAlias -> TestTree
requestDeleteAlias =
  req
    "DeleteAlias"
    "fixture/DeleteAlias.yaml"

requestListOrganizations :: ListOrganizations -> TestTree
requestListOrganizations =
  req
    "ListOrganizations"
    "fixture/ListOrganizations.yaml"

requestAssociateDelegateToResource :: AssociateDelegateToResource -> TestTree
requestAssociateDelegateToResource =
  req
    "AssociateDelegateToResource"
    "fixture/AssociateDelegateToResource.yaml"

requestGetAccessControlEffect :: GetAccessControlEffect -> TestTree
requestGetAccessControlEffect =
  req
    "GetAccessControlEffect"
    "fixture/GetAccessControlEffect.yaml"

requestDeleteRetentionPolicy :: DeleteRetentionPolicy -> TestTree
requestDeleteRetentionPolicy =
  req
    "DeleteRetentionPolicy"
    "fixture/DeleteRetentionPolicy.yaml"

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

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestCreateGroup :: CreateGroup -> TestTree
requestCreateGroup =
  req
    "CreateGroup"
    "fixture/CreateGroup.yaml"

requestUpdateMailboxQuota :: UpdateMailboxQuota -> TestTree
requestUpdateMailboxQuota =
  req
    "UpdateMailboxQuota"
    "fixture/UpdateMailboxQuota.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDeleteGroup :: DeleteGroup -> TestTree
requestDeleteGroup =
  req
    "DeleteGroup"
    "fixture/DeleteGroup.yaml"

requestListGroups :: ListGroups -> TestTree
requestListGroups =
  req
    "ListGroups"
    "fixture/ListGroups.yaml"

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

requestGetDefaultRetentionPolicy :: GetDefaultRetentionPolicy -> TestTree
requestGetDefaultRetentionPolicy =
  req
    "GetDefaultRetentionPolicy"
    "fixture/GetDefaultRetentionPolicy.yaml"

requestCancelMailboxExportJob :: CancelMailboxExportJob -> TestTree
requestCancelMailboxExportJob =
  req
    "CancelMailboxExportJob"
    "fixture/CancelMailboxExportJob.yaml"

-- Responses

responseUpdatePrimaryEmailAddress :: UpdatePrimaryEmailAddressResponse -> TestTree
responseUpdatePrimaryEmailAddress =
  res
    "UpdatePrimaryEmailAddressResponse"
    "fixture/UpdatePrimaryEmailAddressResponse.proto"
    workMailService
    (Proxy :: Proxy UpdatePrimaryEmailAddress)

responseDescribeResource :: DescribeResourceResponse -> TestTree
responseDescribeResource =
  res
    "DescribeResourceResponse"
    "fixture/DescribeResourceResponse.proto"
    workMailService
    (Proxy :: Proxy DescribeResource)

responseCreateOrganization :: CreateOrganizationResponse -> TestTree
responseCreateOrganization =
  res
    "CreateOrganizationResponse"
    "fixture/CreateOrganizationResponse.proto"
    workMailService
    (Proxy :: Proxy CreateOrganization)

responseCreateAlias :: CreateAliasResponse -> TestTree
responseCreateAlias =
  res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    workMailService
    (Proxy :: Proxy CreateAlias)

responseDeleteOrganization :: DeleteOrganizationResponse -> TestTree
responseDeleteOrganization =
  res
    "DeleteOrganizationResponse"
    "fixture/DeleteOrganizationResponse.proto"
    workMailService
    (Proxy :: Proxy DeleteOrganization)

responseResetPassword :: ResetPasswordResponse -> TestTree
responseResetPassword =
  res
    "ResetPasswordResponse"
    "fixture/ResetPasswordResponse.proto"
    workMailService
    (Proxy :: Proxy ResetPassword)

responseDescribeGroup :: DescribeGroupResponse -> TestTree
responseDescribeGroup =
  res
    "DescribeGroupResponse"
    "fixture/DescribeGroupResponse.proto"
    workMailService
    (Proxy :: Proxy DescribeGroup)

responseDescribeMailboxExportJob :: DescribeMailboxExportJobResponse -> TestTree
responseDescribeMailboxExportJob =
  res
    "DescribeMailboxExportJobResponse"
    "fixture/DescribeMailboxExportJobResponse.proto"
    workMailService
    (Proxy :: Proxy DescribeMailboxExportJob)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    workMailService
    (Proxy :: Proxy ListTagsForResource)

responseRegisterToWorkMail :: RegisterToWorkMailResponse -> TestTree
responseRegisterToWorkMail =
  res
    "RegisterToWorkMailResponse"
    "fixture/RegisterToWorkMailResponse.proto"
    workMailService
    (Proxy :: Proxy RegisterToWorkMail)

responseListAliases :: ListAliasesResponse -> TestTree
responseListAliases =
  res
    "ListAliasesResponse"
    "fixture/ListAliasesResponse.proto"
    workMailService
    (Proxy :: Proxy ListAliases)

responsePutMailboxPermissions :: PutMailboxPermissionsResponse -> TestTree
responsePutMailboxPermissions =
  res
    "PutMailboxPermissionsResponse"
    "fixture/PutMailboxPermissionsResponse.proto"
    workMailService
    (Proxy :: Proxy PutMailboxPermissions)

responseDeleteMailboxPermissions :: DeleteMailboxPermissionsResponse -> TestTree
responseDeleteMailboxPermissions =
  res
    "DeleteMailboxPermissionsResponse"
    "fixture/DeleteMailboxPermissionsResponse.proto"
    workMailService
    (Proxy :: Proxy DeleteMailboxPermissions)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers =
  res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    workMailService
    (Proxy :: Proxy ListUsers)

responseGetMailboxDetails :: GetMailboxDetailsResponse -> TestTree
responseGetMailboxDetails =
  res
    "GetMailboxDetailsResponse"
    "fixture/GetMailboxDetailsResponse.proto"
    workMailService
    (Proxy :: Proxy GetMailboxDetails)

responseAssociateMemberToGroup :: AssociateMemberToGroupResponse -> TestTree
responseAssociateMemberToGroup =
  res
    "AssociateMemberToGroupResponse"
    "fixture/AssociateMemberToGroupResponse.proto"
    workMailService
    (Proxy :: Proxy AssociateMemberToGroup)

responseDeleteResource :: DeleteResourceResponse -> TestTree
responseDeleteResource =
  res
    "DeleteResourceResponse"
    "fixture/DeleteResourceResponse.proto"
    workMailService
    (Proxy :: Proxy DeleteResource)

responseUpdateResource :: UpdateResourceResponse -> TestTree
responseUpdateResource =
  res
    "UpdateResourceResponse"
    "fixture/UpdateResourceResponse.proto"
    workMailService
    (Proxy :: Proxy UpdateResource)

responseDisassociateMemberFromGroup :: DisassociateMemberFromGroupResponse -> TestTree
responseDisassociateMemberFromGroup =
  res
    "DisassociateMemberFromGroupResponse"
    "fixture/DisassociateMemberFromGroupResponse.proto"
    workMailService
    (Proxy :: Proxy DisassociateMemberFromGroup)

responseListResources :: ListResourcesResponse -> TestTree
responseListResources =
  res
    "ListResourcesResponse"
    "fixture/ListResourcesResponse.proto"
    workMailService
    (Proxy :: Proxy ListResources)

responseDeregisterFromWorkMail :: DeregisterFromWorkMailResponse -> TestTree
responseDeregisterFromWorkMail =
  res
    "DeregisterFromWorkMailResponse"
    "fixture/DeregisterFromWorkMailResponse.proto"
    workMailService
    (Proxy :: Proxy DeregisterFromWorkMail)

responseListMailboxExportJobs :: ListMailboxExportJobsResponse -> TestTree
responseListMailboxExportJobs =
  res
    "ListMailboxExportJobsResponse"
    "fixture/ListMailboxExportJobsResponse.proto"
    workMailService
    (Proxy :: Proxy ListMailboxExportJobs)

responseListMailboxPermissions :: ListMailboxPermissionsResponse -> TestTree
responseListMailboxPermissions =
  res
    "ListMailboxPermissionsResponse"
    "fixture/ListMailboxPermissionsResponse.proto"
    workMailService
    (Proxy :: Proxy ListMailboxPermissions)

responseListGroupMembers :: ListGroupMembersResponse -> TestTree
responseListGroupMembers =
  res
    "ListGroupMembersResponse"
    "fixture/ListGroupMembersResponse.proto"
    workMailService
    (Proxy :: Proxy ListGroupMembers)

responseDisassociateDelegateFromResource :: DisassociateDelegateFromResourceResponse -> TestTree
responseDisassociateDelegateFromResource =
  res
    "DisassociateDelegateFromResourceResponse"
    "fixture/DisassociateDelegateFromResourceResponse.proto"
    workMailService
    (Proxy :: Proxy DisassociateDelegateFromResource)

responseDeleteAccessControlRule :: DeleteAccessControlRuleResponse -> TestTree
responseDeleteAccessControlRule =
  res
    "DeleteAccessControlRuleResponse"
    "fixture/DeleteAccessControlRuleResponse.proto"
    workMailService
    (Proxy :: Proxy DeleteAccessControlRule)

responseListResourceDelegates :: ListResourceDelegatesResponse -> TestTree
responseListResourceDelegates =
  res
    "ListResourceDelegatesResponse"
    "fixture/ListResourceDelegatesResponse.proto"
    workMailService
    (Proxy :: Proxy ListResourceDelegates)

responseListAccessControlRules :: ListAccessControlRulesResponse -> TestTree
responseListAccessControlRules =
  res
    "ListAccessControlRulesResponse"
    "fixture/ListAccessControlRulesResponse.proto"
    workMailService
    (Proxy :: Proxy ListAccessControlRules)

responseDescribeUser :: DescribeUserResponse -> TestTree
responseDescribeUser =
  res
    "DescribeUserResponse"
    "fixture/DescribeUserResponse.proto"
    workMailService
    (Proxy :: Proxy DescribeUser)

responsePutAccessControlRule :: PutAccessControlRuleResponse -> TestTree
responsePutAccessControlRule =
  res
    "PutAccessControlRuleResponse"
    "fixture/PutAccessControlRuleResponse.proto"
    workMailService
    (Proxy :: Proxy PutAccessControlRule)

responseStartMailboxExportJob :: StartMailboxExportJobResponse -> TestTree
responseStartMailboxExportJob =
  res
    "StartMailboxExportJobResponse"
    "fixture/StartMailboxExportJobResponse.proto"
    workMailService
    (Proxy :: Proxy StartMailboxExportJob)

responseDeleteAlias :: DeleteAliasResponse -> TestTree
responseDeleteAlias =
  res
    "DeleteAliasResponse"
    "fixture/DeleteAliasResponse.proto"
    workMailService
    (Proxy :: Proxy DeleteAlias)

responseListOrganizations :: ListOrganizationsResponse -> TestTree
responseListOrganizations =
  res
    "ListOrganizationsResponse"
    "fixture/ListOrganizationsResponse.proto"
    workMailService
    (Proxy :: Proxy ListOrganizations)

responseAssociateDelegateToResource :: AssociateDelegateToResourceResponse -> TestTree
responseAssociateDelegateToResource =
  res
    "AssociateDelegateToResourceResponse"
    "fixture/AssociateDelegateToResourceResponse.proto"
    workMailService
    (Proxy :: Proxy AssociateDelegateToResource)

responseGetAccessControlEffect :: GetAccessControlEffectResponse -> TestTree
responseGetAccessControlEffect =
  res
    "GetAccessControlEffectResponse"
    "fixture/GetAccessControlEffectResponse.proto"
    workMailService
    (Proxy :: Proxy GetAccessControlEffect)

responseDeleteRetentionPolicy :: DeleteRetentionPolicyResponse -> TestTree
responseDeleteRetentionPolicy =
  res
    "DeleteRetentionPolicyResponse"
    "fixture/DeleteRetentionPolicyResponse.proto"
    workMailService
    (Proxy :: Proxy DeleteRetentionPolicy)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    workMailService
    (Proxy :: Proxy CreateUser)

responsePutRetentionPolicy :: PutRetentionPolicyResponse -> TestTree
responsePutRetentionPolicy =
  res
    "PutRetentionPolicyResponse"
    "fixture/PutRetentionPolicyResponse.proto"
    workMailService
    (Proxy :: Proxy PutRetentionPolicy)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    workMailService
    (Proxy :: Proxy DeleteUser)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    workMailService
    (Proxy :: Proxy TagResource)

responseCreateGroup :: CreateGroupResponse -> TestTree
responseCreateGroup =
  res
    "CreateGroupResponse"
    "fixture/CreateGroupResponse.proto"
    workMailService
    (Proxy :: Proxy CreateGroup)

responseUpdateMailboxQuota :: UpdateMailboxQuotaResponse -> TestTree
responseUpdateMailboxQuota =
  res
    "UpdateMailboxQuotaResponse"
    "fixture/UpdateMailboxQuotaResponse.proto"
    workMailService
    (Proxy :: Proxy UpdateMailboxQuota)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    workMailService
    (Proxy :: Proxy UntagResource)

responseDeleteGroup :: DeleteGroupResponse -> TestTree
responseDeleteGroup =
  res
    "DeleteGroupResponse"
    "fixture/DeleteGroupResponse.proto"
    workMailService
    (Proxy :: Proxy DeleteGroup)

responseListGroups :: ListGroupsResponse -> TestTree
responseListGroups =
  res
    "ListGroupsResponse"
    "fixture/ListGroupsResponse.proto"
    workMailService
    (Proxy :: Proxy ListGroups)

responseDescribeOrganization :: DescribeOrganizationResponse -> TestTree
responseDescribeOrganization =
  res
    "DescribeOrganizationResponse"
    "fixture/DescribeOrganizationResponse.proto"
    workMailService
    (Proxy :: Proxy DescribeOrganization)

responseCreateResource :: CreateResourceResponse -> TestTree
responseCreateResource =
  res
    "CreateResourceResponse"
    "fixture/CreateResourceResponse.proto"
    workMailService
    (Proxy :: Proxy CreateResource)

responseGetDefaultRetentionPolicy :: GetDefaultRetentionPolicyResponse -> TestTree
responseGetDefaultRetentionPolicy =
  res
    "GetDefaultRetentionPolicyResponse"
    "fixture/GetDefaultRetentionPolicyResponse.proto"
    workMailService
    (Proxy :: Proxy GetDefaultRetentionPolicy)

responseCancelMailboxExportJob :: CancelMailboxExportJobResponse -> TestTree
responseCancelMailboxExportJob =
  res
    "CancelMailboxExportJobResponse"
    "fixture/CancelMailboxExportJobResponse.proto"
    workMailService
    (Proxy :: Proxy CancelMailboxExportJob)
