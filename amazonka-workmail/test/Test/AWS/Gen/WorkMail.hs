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
--         [ requestDescribeResource $
--             newDescribeResource
--
--         , requestDeleteAlias $
--             newDeleteAlias
--
--         , requestCreateOrganization $
--             newCreateOrganization
--
--         , requestStartMailboxExportJob $
--             newStartMailboxExportJob
--
--         , requestDeleteAccessControlRule $
--             newDeleteAccessControlRule
--
--         , requestListResourceDelegates $
--             newListResourceDelegates
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
--         , requestCreateResource $
--             newCreateResource
--
--         , requestListMailboxExportJobs $
--             newListMailboxExportJobs
--
--         , requestDescribeOrganization $
--             newDescribeOrganization
--
--         , requestUpdateResource $
--             newUpdateResource
--
--         , requestDeleteResource $
--             newDeleteResource
--
--         , requestListMobileDeviceAccessRules $
--             newListMobileDeviceAccessRules
--
--         , requestCreateGroup $
--             newCreateGroup
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestAssociateMemberToGroup $
--             newAssociateMemberToGroup
--
--         , requestPutRetentionPolicy $
--             newPutRetentionPolicy
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestPutMailboxPermissions $
--             newPutMailboxPermissions
--
--         , requestGetMobileDeviceAccessEffect $
--             newGetMobileDeviceAccessEffect
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
--         , requestAssociateDelegateToResource $
--             newAssociateDelegateToResource
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
--         , requestCreateMobileDeviceAccessRule $
--             newCreateMobileDeviceAccessRule
--
--         , requestDeleteGroup $
--             newDeleteGroup
--
--         , requestListGroupMembers $
--             newListGroupMembers
--
--         , requestListMailboxPermissions $
--             newListMailboxPermissions
--
--         , requestDeregisterFromWorkMail $
--             newDeregisterFromWorkMail
--
--         , requestUpdateMailboxQuota $
--             newUpdateMailboxQuota
--
--         , requestUpdateMobileDeviceAccessRule $
--             newUpdateMobileDeviceAccessRule
--
--         , requestDeleteMobileDeviceAccessRule $
--             newDeleteMobileDeviceAccessRule
--
--         , requestDisassociateMemberFromGroup $
--             newDisassociateMemberFromGroup
--
--         , requestListResources $
--             newListResources
--
--         , requestGetMailboxDetails $
--             newGetMailboxDetails
--
--         , requestListUsers $
--             newListUsers
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestDeleteRetentionPolicy $
--             newDeleteRetentionPolicy
--
--         , requestDeleteMailboxPermissions $
--             newDeleteMailboxPermissions
--
--         , requestResetPassword $
--             newResetPassword
--
--         , requestListAliases $
--             newListAliases
--
--         , requestDescribeGroup $
--             newDescribeGroup
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestGetAccessControlEffect $
--             newGetAccessControlEffect
--
--         , requestCreateAlias $
--             newCreateAlias
--
--           ]

--     , testGroup "response"
--         [ responseDescribeResource $
--             newDescribeResourceResponse
--
--         , responseDeleteAlias $
--             newDeleteAliasResponse
--
--         , responseCreateOrganization $
--             newCreateOrganizationResponse
--
--         , responseStartMailboxExportJob $
--             newStartMailboxExportJobResponse
--
--         , responseDeleteAccessControlRule $
--             newDeleteAccessControlRuleResponse
--
--         , responseListResourceDelegates $
--             newListResourceDelegatesResponse
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
--         , responseCreateResource $
--             newCreateResourceResponse
--
--         , responseListMailboxExportJobs $
--             newListMailboxExportJobsResponse
--
--         , responseDescribeOrganization $
--             newDescribeOrganizationResponse
--
--         , responseUpdateResource $
--             newUpdateResourceResponse
--
--         , responseDeleteResource $
--             newDeleteResourceResponse
--
--         , responseListMobileDeviceAccessRules $
--             newListMobileDeviceAccessRulesResponse
--
--         , responseCreateGroup $
--             newCreateGroupResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseAssociateMemberToGroup $
--             newAssociateMemberToGroupResponse
--
--         , responsePutRetentionPolicy $
--             newPutRetentionPolicyResponse
--
--         , responseCreateUser $
--             newCreateUserResponse
--
--         , responsePutMailboxPermissions $
--             newPutMailboxPermissionsResponse
--
--         , responseGetMobileDeviceAccessEffect $
--             newGetMobileDeviceAccessEffectResponse
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
--         , responseAssociateDelegateToResource $
--             newAssociateDelegateToResourceResponse
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
--         , responseCreateMobileDeviceAccessRule $
--             newCreateMobileDeviceAccessRuleResponse
--
--         , responseDeleteGroup $
--             newDeleteGroupResponse
--
--         , responseListGroupMembers $
--             newListGroupMembersResponse
--
--         , responseListMailboxPermissions $
--             newListMailboxPermissionsResponse
--
--         , responseDeregisterFromWorkMail $
--             newDeregisterFromWorkMailResponse
--
--         , responseUpdateMailboxQuota $
--             newUpdateMailboxQuotaResponse
--
--         , responseUpdateMobileDeviceAccessRule $
--             newUpdateMobileDeviceAccessRuleResponse
--
--         , responseDeleteMobileDeviceAccessRule $
--             newDeleteMobileDeviceAccessRuleResponse
--
--         , responseDisassociateMemberFromGroup $
--             newDisassociateMemberFromGroupResponse
--
--         , responseListResources $
--             newListResourcesResponse
--
--         , responseGetMailboxDetails $
--             newGetMailboxDetailsResponse
--
--         , responseListUsers $
--             newListUsersResponse
--
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseDeleteRetentionPolicy $
--             newDeleteRetentionPolicyResponse
--
--         , responseDeleteMailboxPermissions $
--             newDeleteMailboxPermissionsResponse
--
--         , responseResetPassword $
--             newResetPasswordResponse
--
--         , responseListAliases $
--             newListAliasesResponse
--
--         , responseDescribeGroup $
--             newDescribeGroupResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseGetAccessControlEffect $
--             newGetAccessControlEffectResponse
--
--         , responseCreateAlias $
--             newCreateAliasResponse
--
--           ]
--     ]

-- Requests

requestDescribeResource :: DescribeResource -> TestTree
requestDescribeResource =
  req
    "DescribeResource"
    "fixture/DescribeResource.yaml"

requestDeleteAlias :: DeleteAlias -> TestTree
requestDeleteAlias =
  req
    "DeleteAlias"
    "fixture/DeleteAlias.yaml"

requestCreateOrganization :: CreateOrganization -> TestTree
requestCreateOrganization =
  req
    "CreateOrganization"
    "fixture/CreateOrganization.yaml"

requestStartMailboxExportJob :: StartMailboxExportJob -> TestTree
requestStartMailboxExportJob =
  req
    "StartMailboxExportJob"
    "fixture/StartMailboxExportJob.yaml"

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

requestCreateResource :: CreateResource -> TestTree
requestCreateResource =
  req
    "CreateResource"
    "fixture/CreateResource.yaml"

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

requestUpdateResource :: UpdateResource -> TestTree
requestUpdateResource =
  req
    "UpdateResource"
    "fixture/UpdateResource.yaml"

requestDeleteResource :: DeleteResource -> TestTree
requestDeleteResource =
  req
    "DeleteResource"
    "fixture/DeleteResource.yaml"

requestListMobileDeviceAccessRules :: ListMobileDeviceAccessRules -> TestTree
requestListMobileDeviceAccessRules =
  req
    "ListMobileDeviceAccessRules"
    "fixture/ListMobileDeviceAccessRules.yaml"

requestCreateGroup :: CreateGroup -> TestTree
requestCreateGroup =
  req
    "CreateGroup"
    "fixture/CreateGroup.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

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

requestPutRetentionPolicy :: PutRetentionPolicy -> TestTree
requestPutRetentionPolicy =
  req
    "PutRetentionPolicy"
    "fixture/PutRetentionPolicy.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser =
  req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestPutMailboxPermissions :: PutMailboxPermissions -> TestTree
requestPutMailboxPermissions =
  req
    "PutMailboxPermissions"
    "fixture/PutMailboxPermissions.yaml"

requestGetMobileDeviceAccessEffect :: GetMobileDeviceAccessEffect -> TestTree
requestGetMobileDeviceAccessEffect =
  req
    "GetMobileDeviceAccessEffect"
    "fixture/GetMobileDeviceAccessEffect.yaml"

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

requestAssociateDelegateToResource :: AssociateDelegateToResource -> TestTree
requestAssociateDelegateToResource =
  req
    "AssociateDelegateToResource"
    "fixture/AssociateDelegateToResource.yaml"

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

requestCreateMobileDeviceAccessRule :: CreateMobileDeviceAccessRule -> TestTree
requestCreateMobileDeviceAccessRule =
  req
    "CreateMobileDeviceAccessRule"
    "fixture/CreateMobileDeviceAccessRule.yaml"

requestDeleteGroup :: DeleteGroup -> TestTree
requestDeleteGroup =
  req
    "DeleteGroup"
    "fixture/DeleteGroup.yaml"

requestListGroupMembers :: ListGroupMembers -> TestTree
requestListGroupMembers =
  req
    "ListGroupMembers"
    "fixture/ListGroupMembers.yaml"

requestListMailboxPermissions :: ListMailboxPermissions -> TestTree
requestListMailboxPermissions =
  req
    "ListMailboxPermissions"
    "fixture/ListMailboxPermissions.yaml"

requestDeregisterFromWorkMail :: DeregisterFromWorkMail -> TestTree
requestDeregisterFromWorkMail =
  req
    "DeregisterFromWorkMail"
    "fixture/DeregisterFromWorkMail.yaml"

requestUpdateMailboxQuota :: UpdateMailboxQuota -> TestTree
requestUpdateMailboxQuota =
  req
    "UpdateMailboxQuota"
    "fixture/UpdateMailboxQuota.yaml"

requestUpdateMobileDeviceAccessRule :: UpdateMobileDeviceAccessRule -> TestTree
requestUpdateMobileDeviceAccessRule =
  req
    "UpdateMobileDeviceAccessRule"
    "fixture/UpdateMobileDeviceAccessRule.yaml"

requestDeleteMobileDeviceAccessRule :: DeleteMobileDeviceAccessRule -> TestTree
requestDeleteMobileDeviceAccessRule =
  req
    "DeleteMobileDeviceAccessRule"
    "fixture/DeleteMobileDeviceAccessRule.yaml"

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

requestGetMailboxDetails :: GetMailboxDetails -> TestTree
requestGetMailboxDetails =
  req
    "GetMailboxDetails"
    "fixture/GetMailboxDetails.yaml"

requestListUsers :: ListUsers -> TestTree
requestListUsers =
  req
    "ListUsers"
    "fixture/ListUsers.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestDeleteRetentionPolicy :: DeleteRetentionPolicy -> TestTree
requestDeleteRetentionPolicy =
  req
    "DeleteRetentionPolicy"
    "fixture/DeleteRetentionPolicy.yaml"

requestDeleteMailboxPermissions :: DeleteMailboxPermissions -> TestTree
requestDeleteMailboxPermissions =
  req
    "DeleteMailboxPermissions"
    "fixture/DeleteMailboxPermissions.yaml"

requestResetPassword :: ResetPassword -> TestTree
requestResetPassword =
  req
    "ResetPassword"
    "fixture/ResetPassword.yaml"

requestListAliases :: ListAliases -> TestTree
requestListAliases =
  req
    "ListAliases"
    "fixture/ListAliases.yaml"

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

requestGetAccessControlEffect :: GetAccessControlEffect -> TestTree
requestGetAccessControlEffect =
  req
    "GetAccessControlEffect"
    "fixture/GetAccessControlEffect.yaml"

requestCreateAlias :: CreateAlias -> TestTree
requestCreateAlias =
  req
    "CreateAlias"
    "fixture/CreateAlias.yaml"

-- Responses

responseDescribeResource :: DescribeResourceResponse -> TestTree
responseDescribeResource =
  res
    "DescribeResourceResponse"
    "fixture/DescribeResourceResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeResource)

responseDeleteAlias :: DeleteAliasResponse -> TestTree
responseDeleteAlias =
  res
    "DeleteAliasResponse"
    "fixture/DeleteAliasResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAlias)

responseCreateOrganization :: CreateOrganizationResponse -> TestTree
responseCreateOrganization =
  res
    "CreateOrganizationResponse"
    "fixture/CreateOrganizationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateOrganization)

responseStartMailboxExportJob :: StartMailboxExportJobResponse -> TestTree
responseStartMailboxExportJob =
  res
    "StartMailboxExportJobResponse"
    "fixture/StartMailboxExportJobResponse.proto"
    defaultService
    (Proxy :: Proxy StartMailboxExportJob)

responseDeleteAccessControlRule :: DeleteAccessControlRuleResponse -> TestTree
responseDeleteAccessControlRule =
  res
    "DeleteAccessControlRuleResponse"
    "fixture/DeleteAccessControlRuleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteAccessControlRule)

responseListResourceDelegates :: ListResourceDelegatesResponse -> TestTree
responseListResourceDelegates =
  res
    "ListResourceDelegatesResponse"
    "fixture/ListResourceDelegatesResponse.proto"
    defaultService
    (Proxy :: Proxy ListResourceDelegates)

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

responseCreateResource :: CreateResourceResponse -> TestTree
responseCreateResource =
  res
    "CreateResourceResponse"
    "fixture/CreateResourceResponse.proto"
    defaultService
    (Proxy :: Proxy CreateResource)

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

responseUpdateResource :: UpdateResourceResponse -> TestTree
responseUpdateResource =
  res
    "UpdateResourceResponse"
    "fixture/UpdateResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateResource)

responseDeleteResource :: DeleteResourceResponse -> TestTree
responseDeleteResource =
  res
    "DeleteResourceResponse"
    "fixture/DeleteResourceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteResource)

responseListMobileDeviceAccessRules :: ListMobileDeviceAccessRulesResponse -> TestTree
responseListMobileDeviceAccessRules =
  res
    "ListMobileDeviceAccessRulesResponse"
    "fixture/ListMobileDeviceAccessRulesResponse.proto"
    defaultService
    (Proxy :: Proxy ListMobileDeviceAccessRules)

responseCreateGroup :: CreateGroupResponse -> TestTree
responseCreateGroup =
  res
    "CreateGroupResponse"
    "fixture/CreateGroupResponse.proto"
    defaultService
    (Proxy :: Proxy CreateGroup)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

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

responsePutRetentionPolicy :: PutRetentionPolicyResponse -> TestTree
responsePutRetentionPolicy =
  res
    "PutRetentionPolicyResponse"
    "fixture/PutRetentionPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy PutRetentionPolicy)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUser)

responsePutMailboxPermissions :: PutMailboxPermissionsResponse -> TestTree
responsePutMailboxPermissions =
  res
    "PutMailboxPermissionsResponse"
    "fixture/PutMailboxPermissionsResponse.proto"
    defaultService
    (Proxy :: Proxy PutMailboxPermissions)

responseGetMobileDeviceAccessEffect :: GetMobileDeviceAccessEffectResponse -> TestTree
responseGetMobileDeviceAccessEffect =
  res
    "GetMobileDeviceAccessEffectResponse"
    "fixture/GetMobileDeviceAccessEffectResponse.proto"
    defaultService
    (Proxy :: Proxy GetMobileDeviceAccessEffect)

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

responseAssociateDelegateToResource :: AssociateDelegateToResourceResponse -> TestTree
responseAssociateDelegateToResource =
  res
    "AssociateDelegateToResourceResponse"
    "fixture/AssociateDelegateToResourceResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateDelegateToResource)

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

responseCreateMobileDeviceAccessRule :: CreateMobileDeviceAccessRuleResponse -> TestTree
responseCreateMobileDeviceAccessRule =
  res
    "CreateMobileDeviceAccessRuleResponse"
    "fixture/CreateMobileDeviceAccessRuleResponse.proto"
    defaultService
    (Proxy :: Proxy CreateMobileDeviceAccessRule)

responseDeleteGroup :: DeleteGroupResponse -> TestTree
responseDeleteGroup =
  res
    "DeleteGroupResponse"
    "fixture/DeleteGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteGroup)

responseListGroupMembers :: ListGroupMembersResponse -> TestTree
responseListGroupMembers =
  res
    "ListGroupMembersResponse"
    "fixture/ListGroupMembersResponse.proto"
    defaultService
    (Proxy :: Proxy ListGroupMembers)

responseListMailboxPermissions :: ListMailboxPermissionsResponse -> TestTree
responseListMailboxPermissions =
  res
    "ListMailboxPermissionsResponse"
    "fixture/ListMailboxPermissionsResponse.proto"
    defaultService
    (Proxy :: Proxy ListMailboxPermissions)

responseDeregisterFromWorkMail :: DeregisterFromWorkMailResponse -> TestTree
responseDeregisterFromWorkMail =
  res
    "DeregisterFromWorkMailResponse"
    "fixture/DeregisterFromWorkMailResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterFromWorkMail)

responseUpdateMailboxQuota :: UpdateMailboxQuotaResponse -> TestTree
responseUpdateMailboxQuota =
  res
    "UpdateMailboxQuotaResponse"
    "fixture/UpdateMailboxQuotaResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateMailboxQuota)

responseUpdateMobileDeviceAccessRule :: UpdateMobileDeviceAccessRuleResponse -> TestTree
responseUpdateMobileDeviceAccessRule =
  res
    "UpdateMobileDeviceAccessRuleResponse"
    "fixture/UpdateMobileDeviceAccessRuleResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateMobileDeviceAccessRule)

responseDeleteMobileDeviceAccessRule :: DeleteMobileDeviceAccessRuleResponse -> TestTree
responseDeleteMobileDeviceAccessRule =
  res
    "DeleteMobileDeviceAccessRuleResponse"
    "fixture/DeleteMobileDeviceAccessRuleResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMobileDeviceAccessRule)

responseDisassociateMemberFromGroup :: DisassociateMemberFromGroupResponse -> TestTree
responseDisassociateMemberFromGroup =
  res
    "DisassociateMemberFromGroupResponse"
    "fixture/DisassociateMemberFromGroupResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateMemberFromGroup)

responseListResources :: ListResourcesResponse -> TestTree
responseListResources =
  res
    "ListResourcesResponse"
    "fixture/ListResourcesResponse.proto"
    defaultService
    (Proxy :: Proxy ListResources)

responseGetMailboxDetails :: GetMailboxDetailsResponse -> TestTree
responseGetMailboxDetails =
  res
    "GetMailboxDetailsResponse"
    "fixture/GetMailboxDetailsResponse.proto"
    defaultService
    (Proxy :: Proxy GetMailboxDetails)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers =
  res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    defaultService
    (Proxy :: Proxy ListUsers)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUser)

responseDeleteRetentionPolicy :: DeleteRetentionPolicyResponse -> TestTree
responseDeleteRetentionPolicy =
  res
    "DeleteRetentionPolicyResponse"
    "fixture/DeleteRetentionPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteRetentionPolicy)

responseDeleteMailboxPermissions :: DeleteMailboxPermissionsResponse -> TestTree
responseDeleteMailboxPermissions =
  res
    "DeleteMailboxPermissionsResponse"
    "fixture/DeleteMailboxPermissionsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteMailboxPermissions)

responseResetPassword :: ResetPasswordResponse -> TestTree
responseResetPassword =
  res
    "ResetPasswordResponse"
    "fixture/ResetPasswordResponse.proto"
    defaultService
    (Proxy :: Proxy ResetPassword)

responseListAliases :: ListAliasesResponse -> TestTree
responseListAliases =
  res
    "ListAliasesResponse"
    "fixture/ListAliasesResponse.proto"
    defaultService
    (Proxy :: Proxy ListAliases)

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

responseGetAccessControlEffect :: GetAccessControlEffectResponse -> TestTree
responseGetAccessControlEffect =
  res
    "GetAccessControlEffectResponse"
    "fixture/GetAccessControlEffectResponse.proto"
    defaultService
    (Proxy :: Proxy GetAccessControlEffect)

responseCreateAlias :: CreateAliasResponse -> TestTree
responseCreateAlias =
  res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAlias)
