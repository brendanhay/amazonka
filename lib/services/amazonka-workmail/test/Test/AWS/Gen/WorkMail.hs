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

import qualified Data.Proxy as Proxy
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
--         [ requestDescribeInboundDmarcSettings $
--             newDescribeInboundDmarcSettings
--
--         , requestGetMailDomain $
--             newGetMailDomain
--
--         , requestUpdatePrimaryEmailAddress $
--             newUpdatePrimaryEmailAddress
--
--         , requestDescribeResource $
--             newDescribeResource
--
--         , requestCreateOrganization $
--             newCreateOrganization
--
--         , requestCreateAlias $
--             newCreateAlias
--
--         , requestDeleteOrganization $
--             newDeleteOrganization
--
--         , requestResetPassword $
--             newResetPassword
--
--         , requestDescribeGroup $
--             newDescribeGroup
--
--         , requestDescribeMailboxExportJob $
--             newDescribeMailboxExportJob
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestRegisterToWorkMail $
--             newRegisterToWorkMail
--
--         , requestListAliases $
--             newListAliases
--
--         , requestPutMailboxPermissions $
--             newPutMailboxPermissions
--
--         , requestGetMobileDeviceAccessEffect $
--             newGetMobileDeviceAccessEffect
--
--         , requestDeleteMailboxPermissions $
--             newDeleteMailboxPermissions
--
--         , requestListUsers $
--             newListUsers
--
--         , requestPutInboundDmarcSettings $
--             newPutInboundDmarcSettings
--
--         , requestGetMailboxDetails $
--             newGetMailboxDetails
--
--         , requestAssociateMemberToGroup $
--             newAssociateMemberToGroup
--
--         , requestDeleteResource $
--             newDeleteResource
--
--         , requestUpdateResource $
--             newUpdateResource
--
--         , requestDisassociateMemberFromGroup $
--             newDisassociateMemberFromGroup
--
--         , requestListResources $
--             newListResources
--
--         , requestDeregisterFromWorkMail $
--             newDeregisterFromWorkMail
--
--         , requestListMailboxExportJobs $
--             newListMailboxExportJobs
--
--         , requestCreateMobileDeviceAccessRule $
--             newCreateMobileDeviceAccessRule
--
--         , requestListMailboxPermissions $
--             newListMailboxPermissions
--
--         , requestGetMobileDeviceAccessOverride $
--             newGetMobileDeviceAccessOverride
--
--         , requestListGroupMembers $
--             newListGroupMembers
--
--         , requestDisassociateDelegateFromResource $
--             newDisassociateDelegateFromResource
--
--         , requestDeleteAccessControlRule $
--             newDeleteAccessControlRule
--
--         , requestListResourceDelegates $
--             newListResourceDelegates
--
--         , requestListAccessControlRules $
--             newListAccessControlRules
--
--         , requestDescribeUser $
--             newDescribeUser
--
--         , requestPutAccessControlRule $
--             newPutAccessControlRule
--
--         , requestStartMailboxExportJob $
--             newStartMailboxExportJob
--
--         , requestDeleteAlias $
--             newDeleteAlias
--
--         , requestListOrganizations $
--             newListOrganizations
--
--         , requestAssociateDelegateToResource $
--             newAssociateDelegateToResource
--
--         , requestGetAccessControlEffect $
--             newGetAccessControlEffect
--
--         , requestDeleteRetentionPolicy $
--             newDeleteRetentionPolicy
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestPutRetentionPolicy $
--             newPutRetentionPolicy
--
--         , requestListMailDomains $
--             newListMailDomains
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestTagResource $
--             newTagResource
--
--         , requestRegisterMailDomain $
--             newRegisterMailDomain
--
--         , requestUpdateDefaultMailDomain $
--             newUpdateDefaultMailDomain
--
--         , requestUpdateMobileDeviceAccessRule $
--             newUpdateMobileDeviceAccessRule
--
--         , requestDeleteMobileDeviceAccessRule $
--             newDeleteMobileDeviceAccessRule
--
--         , requestCreateGroup $
--             newCreateGroup
--
--         , requestUpdateMailboxQuota $
--             newUpdateMailboxQuota
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestListMobileDeviceAccessRules $
--             newListMobileDeviceAccessRules
--
--         , requestDeleteGroup $
--             newDeleteGroup
--
--         , requestListGroups $
--             newListGroups
--
--         , requestDescribeOrganization $
--             newDescribeOrganization
--
--         , requestCreateResource $
--             newCreateResource
--
--         , requestGetDefaultRetentionPolicy $
--             newGetDefaultRetentionPolicy
--
--         , requestDeregisterMailDomain $
--             newDeregisterMailDomain
--
--         , requestCancelMailboxExportJob $
--             newCancelMailboxExportJob
--
--         , requestListMobileDeviceAccessOverrides $
--             newListMobileDeviceAccessOverrides
--
--         , requestDeleteMobileDeviceAccessOverride $
--             newDeleteMobileDeviceAccessOverride
--
--         , requestPutMobileDeviceAccessOverride $
--             newPutMobileDeviceAccessOverride
--
--           ]

--     , testGroup "response"
--         [ responseDescribeInboundDmarcSettings $
--             newDescribeInboundDmarcSettingsResponse
--
--         , responseGetMailDomain $
--             newGetMailDomainResponse
--
--         , responseUpdatePrimaryEmailAddress $
--             newUpdatePrimaryEmailAddressResponse
--
--         , responseDescribeResource $
--             newDescribeResourceResponse
--
--         , responseCreateOrganization $
--             newCreateOrganizationResponse
--
--         , responseCreateAlias $
--             newCreateAliasResponse
--
--         , responseDeleteOrganization $
--             newDeleteOrganizationResponse
--
--         , responseResetPassword $
--             newResetPasswordResponse
--
--         , responseDescribeGroup $
--             newDescribeGroupResponse
--
--         , responseDescribeMailboxExportJob $
--             newDescribeMailboxExportJobResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseRegisterToWorkMail $
--             newRegisterToWorkMailResponse
--
--         , responseListAliases $
--             newListAliasesResponse
--
--         , responsePutMailboxPermissions $
--             newPutMailboxPermissionsResponse
--
--         , responseGetMobileDeviceAccessEffect $
--             newGetMobileDeviceAccessEffectResponse
--
--         , responseDeleteMailboxPermissions $
--             newDeleteMailboxPermissionsResponse
--
--         , responseListUsers $
--             newListUsersResponse
--
--         , responsePutInboundDmarcSettings $
--             newPutInboundDmarcSettingsResponse
--
--         , responseGetMailboxDetails $
--             newGetMailboxDetailsResponse
--
--         , responseAssociateMemberToGroup $
--             newAssociateMemberToGroupResponse
--
--         , responseDeleteResource $
--             newDeleteResourceResponse
--
--         , responseUpdateResource $
--             newUpdateResourceResponse
--
--         , responseDisassociateMemberFromGroup $
--             newDisassociateMemberFromGroupResponse
--
--         , responseListResources $
--             newListResourcesResponse
--
--         , responseDeregisterFromWorkMail $
--             newDeregisterFromWorkMailResponse
--
--         , responseListMailboxExportJobs $
--             newListMailboxExportJobsResponse
--
--         , responseCreateMobileDeviceAccessRule $
--             newCreateMobileDeviceAccessRuleResponse
--
--         , responseListMailboxPermissions $
--             newListMailboxPermissionsResponse
--
--         , responseGetMobileDeviceAccessOverride $
--             newGetMobileDeviceAccessOverrideResponse
--
--         , responseListGroupMembers $
--             newListGroupMembersResponse
--
--         , responseDisassociateDelegateFromResource $
--             newDisassociateDelegateFromResourceResponse
--
--         , responseDeleteAccessControlRule $
--             newDeleteAccessControlRuleResponse
--
--         , responseListResourceDelegates $
--             newListResourceDelegatesResponse
--
--         , responseListAccessControlRules $
--             newListAccessControlRulesResponse
--
--         , responseDescribeUser $
--             newDescribeUserResponse
--
--         , responsePutAccessControlRule $
--             newPutAccessControlRuleResponse
--
--         , responseStartMailboxExportJob $
--             newStartMailboxExportJobResponse
--
--         , responseDeleteAlias $
--             newDeleteAliasResponse
--
--         , responseListOrganizations $
--             newListOrganizationsResponse
--
--         , responseAssociateDelegateToResource $
--             newAssociateDelegateToResourceResponse
--
--         , responseGetAccessControlEffect $
--             newGetAccessControlEffectResponse
--
--         , responseDeleteRetentionPolicy $
--             newDeleteRetentionPolicyResponse
--
--         , responseCreateUser $
--             newCreateUserResponse
--
--         , responsePutRetentionPolicy $
--             newPutRetentionPolicyResponse
--
--         , responseListMailDomains $
--             newListMailDomainsResponse
--
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseRegisterMailDomain $
--             newRegisterMailDomainResponse
--
--         , responseUpdateDefaultMailDomain $
--             newUpdateDefaultMailDomainResponse
--
--         , responseUpdateMobileDeviceAccessRule $
--             newUpdateMobileDeviceAccessRuleResponse
--
--         , responseDeleteMobileDeviceAccessRule $
--             newDeleteMobileDeviceAccessRuleResponse
--
--         , responseCreateGroup $
--             newCreateGroupResponse
--
--         , responseUpdateMailboxQuota $
--             newUpdateMailboxQuotaResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseListMobileDeviceAccessRules $
--             newListMobileDeviceAccessRulesResponse
--
--         , responseDeleteGroup $
--             newDeleteGroupResponse
--
--         , responseListGroups $
--             newListGroupsResponse
--
--         , responseDescribeOrganization $
--             newDescribeOrganizationResponse
--
--         , responseCreateResource $
--             newCreateResourceResponse
--
--         , responseGetDefaultRetentionPolicy $
--             newGetDefaultRetentionPolicyResponse
--
--         , responseDeregisterMailDomain $
--             newDeregisterMailDomainResponse
--
--         , responseCancelMailboxExportJob $
--             newCancelMailboxExportJobResponse
--
--         , responseListMobileDeviceAccessOverrides $
--             newListMobileDeviceAccessOverridesResponse
--
--         , responseDeleteMobileDeviceAccessOverride $
--             newDeleteMobileDeviceAccessOverrideResponse
--
--         , responsePutMobileDeviceAccessOverride $
--             newPutMobileDeviceAccessOverrideResponse
--
--           ]
--     ]

-- Requests

requestDescribeInboundDmarcSettings :: DescribeInboundDmarcSettings -> TestTree
requestDescribeInboundDmarcSettings =
  req
    "DescribeInboundDmarcSettings"
    "fixture/DescribeInboundDmarcSettings.yaml"

requestGetMailDomain :: GetMailDomain -> TestTree
requestGetMailDomain =
  req
    "GetMailDomain"
    "fixture/GetMailDomain.yaml"

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

requestGetMobileDeviceAccessEffect :: GetMobileDeviceAccessEffect -> TestTree
requestGetMobileDeviceAccessEffect =
  req
    "GetMobileDeviceAccessEffect"
    "fixture/GetMobileDeviceAccessEffect.yaml"

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

requestPutInboundDmarcSettings :: PutInboundDmarcSettings -> TestTree
requestPutInboundDmarcSettings =
  req
    "PutInboundDmarcSettings"
    "fixture/PutInboundDmarcSettings.yaml"

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

requestCreateMobileDeviceAccessRule :: CreateMobileDeviceAccessRule -> TestTree
requestCreateMobileDeviceAccessRule =
  req
    "CreateMobileDeviceAccessRule"
    "fixture/CreateMobileDeviceAccessRule.yaml"

requestListMailboxPermissions :: ListMailboxPermissions -> TestTree
requestListMailboxPermissions =
  req
    "ListMailboxPermissions"
    "fixture/ListMailboxPermissions.yaml"

requestGetMobileDeviceAccessOverride :: GetMobileDeviceAccessOverride -> TestTree
requestGetMobileDeviceAccessOverride =
  req
    "GetMobileDeviceAccessOverride"
    "fixture/GetMobileDeviceAccessOverride.yaml"

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

requestListMailDomains :: ListMailDomains -> TestTree
requestListMailDomains =
  req
    "ListMailDomains"
    "fixture/ListMailDomains.yaml"

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

requestRegisterMailDomain :: RegisterMailDomain -> TestTree
requestRegisterMailDomain =
  req
    "RegisterMailDomain"
    "fixture/RegisterMailDomain.yaml"

requestUpdateDefaultMailDomain :: UpdateDefaultMailDomain -> TestTree
requestUpdateDefaultMailDomain =
  req
    "UpdateDefaultMailDomain"
    "fixture/UpdateDefaultMailDomain.yaml"

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

requestListMobileDeviceAccessRules :: ListMobileDeviceAccessRules -> TestTree
requestListMobileDeviceAccessRules =
  req
    "ListMobileDeviceAccessRules"
    "fixture/ListMobileDeviceAccessRules.yaml"

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

requestDeregisterMailDomain :: DeregisterMailDomain -> TestTree
requestDeregisterMailDomain =
  req
    "DeregisterMailDomain"
    "fixture/DeregisterMailDomain.yaml"

requestCancelMailboxExportJob :: CancelMailboxExportJob -> TestTree
requestCancelMailboxExportJob =
  req
    "CancelMailboxExportJob"
    "fixture/CancelMailboxExportJob.yaml"

requestListMobileDeviceAccessOverrides :: ListMobileDeviceAccessOverrides -> TestTree
requestListMobileDeviceAccessOverrides =
  req
    "ListMobileDeviceAccessOverrides"
    "fixture/ListMobileDeviceAccessOverrides.yaml"

requestDeleteMobileDeviceAccessOverride :: DeleteMobileDeviceAccessOverride -> TestTree
requestDeleteMobileDeviceAccessOverride =
  req
    "DeleteMobileDeviceAccessOverride"
    "fixture/DeleteMobileDeviceAccessOverride.yaml"

requestPutMobileDeviceAccessOverride :: PutMobileDeviceAccessOverride -> TestTree
requestPutMobileDeviceAccessOverride =
  req
    "PutMobileDeviceAccessOverride"
    "fixture/PutMobileDeviceAccessOverride.yaml"

-- Responses

responseDescribeInboundDmarcSettings :: DescribeInboundDmarcSettingsResponse -> TestTree
responseDescribeInboundDmarcSettings =
  res
    "DescribeInboundDmarcSettingsResponse"
    "fixture/DescribeInboundDmarcSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInboundDmarcSettings)

responseGetMailDomain :: GetMailDomainResponse -> TestTree
responseGetMailDomain =
  res
    "GetMailDomainResponse"
    "fixture/GetMailDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMailDomain)

responseUpdatePrimaryEmailAddress :: UpdatePrimaryEmailAddressResponse -> TestTree
responseUpdatePrimaryEmailAddress =
  res
    "UpdatePrimaryEmailAddressResponse"
    "fixture/UpdatePrimaryEmailAddressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePrimaryEmailAddress)

responseDescribeResource :: DescribeResourceResponse -> TestTree
responseDescribeResource =
  res
    "DescribeResourceResponse"
    "fixture/DescribeResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeResource)

responseCreateOrganization :: CreateOrganizationResponse -> TestTree
responseCreateOrganization =
  res
    "CreateOrganizationResponse"
    "fixture/CreateOrganizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateOrganization)

responseCreateAlias :: CreateAliasResponse -> TestTree
responseCreateAlias =
  res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAlias)

responseDeleteOrganization :: DeleteOrganizationResponse -> TestTree
responseDeleteOrganization =
  res
    "DeleteOrganizationResponse"
    "fixture/DeleteOrganizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteOrganization)

responseResetPassword :: ResetPasswordResponse -> TestTree
responseResetPassword =
  res
    "ResetPasswordResponse"
    "fixture/ResetPasswordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetPassword)

responseDescribeGroup :: DescribeGroupResponse -> TestTree
responseDescribeGroup =
  res
    "DescribeGroupResponse"
    "fixture/DescribeGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGroup)

responseDescribeMailboxExportJob :: DescribeMailboxExportJobResponse -> TestTree
responseDescribeMailboxExportJob =
  res
    "DescribeMailboxExportJobResponse"
    "fixture/DescribeMailboxExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMailboxExportJob)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseRegisterToWorkMail :: RegisterToWorkMailResponse -> TestTree
responseRegisterToWorkMail =
  res
    "RegisterToWorkMailResponse"
    "fixture/RegisterToWorkMailResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterToWorkMail)

responseListAliases :: ListAliasesResponse -> TestTree
responseListAliases =
  res
    "ListAliasesResponse"
    "fixture/ListAliasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAliases)

responsePutMailboxPermissions :: PutMailboxPermissionsResponse -> TestTree
responsePutMailboxPermissions =
  res
    "PutMailboxPermissionsResponse"
    "fixture/PutMailboxPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutMailboxPermissions)

responseGetMobileDeviceAccessEffect :: GetMobileDeviceAccessEffectResponse -> TestTree
responseGetMobileDeviceAccessEffect =
  res
    "GetMobileDeviceAccessEffectResponse"
    "fixture/GetMobileDeviceAccessEffectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMobileDeviceAccessEffect)

responseDeleteMailboxPermissions :: DeleteMailboxPermissionsResponse -> TestTree
responseDeleteMailboxPermissions =
  res
    "DeleteMailboxPermissionsResponse"
    "fixture/DeleteMailboxPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMailboxPermissions)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers =
  res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUsers)

responsePutInboundDmarcSettings :: PutInboundDmarcSettingsResponse -> TestTree
responsePutInboundDmarcSettings =
  res
    "PutInboundDmarcSettingsResponse"
    "fixture/PutInboundDmarcSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutInboundDmarcSettings)

responseGetMailboxDetails :: GetMailboxDetailsResponse -> TestTree
responseGetMailboxDetails =
  res
    "GetMailboxDetailsResponse"
    "fixture/GetMailboxDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMailboxDetails)

responseAssociateMemberToGroup :: AssociateMemberToGroupResponse -> TestTree
responseAssociateMemberToGroup =
  res
    "AssociateMemberToGroupResponse"
    "fixture/AssociateMemberToGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateMemberToGroup)

responseDeleteResource :: DeleteResourceResponse -> TestTree
responseDeleteResource =
  res
    "DeleteResourceResponse"
    "fixture/DeleteResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResource)

responseUpdateResource :: UpdateResourceResponse -> TestTree
responseUpdateResource =
  res
    "UpdateResourceResponse"
    "fixture/UpdateResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResource)

responseDisassociateMemberFromGroup :: DisassociateMemberFromGroupResponse -> TestTree
responseDisassociateMemberFromGroup =
  res
    "DisassociateMemberFromGroupResponse"
    "fixture/DisassociateMemberFromGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateMemberFromGroup)

responseListResources :: ListResourcesResponse -> TestTree
responseListResources =
  res
    "ListResourcesResponse"
    "fixture/ListResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResources)

responseDeregisterFromWorkMail :: DeregisterFromWorkMailResponse -> TestTree
responseDeregisterFromWorkMail =
  res
    "DeregisterFromWorkMailResponse"
    "fixture/DeregisterFromWorkMailResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterFromWorkMail)

responseListMailboxExportJobs :: ListMailboxExportJobsResponse -> TestTree
responseListMailboxExportJobs =
  res
    "ListMailboxExportJobsResponse"
    "fixture/ListMailboxExportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMailboxExportJobs)

responseCreateMobileDeviceAccessRule :: CreateMobileDeviceAccessRuleResponse -> TestTree
responseCreateMobileDeviceAccessRule =
  res
    "CreateMobileDeviceAccessRuleResponse"
    "fixture/CreateMobileDeviceAccessRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMobileDeviceAccessRule)

responseListMailboxPermissions :: ListMailboxPermissionsResponse -> TestTree
responseListMailboxPermissions =
  res
    "ListMailboxPermissionsResponse"
    "fixture/ListMailboxPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMailboxPermissions)

responseGetMobileDeviceAccessOverride :: GetMobileDeviceAccessOverrideResponse -> TestTree
responseGetMobileDeviceAccessOverride =
  res
    "GetMobileDeviceAccessOverrideResponse"
    "fixture/GetMobileDeviceAccessOverrideResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMobileDeviceAccessOverride)

responseListGroupMembers :: ListGroupMembersResponse -> TestTree
responseListGroupMembers =
  res
    "ListGroupMembersResponse"
    "fixture/ListGroupMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGroupMembers)

responseDisassociateDelegateFromResource :: DisassociateDelegateFromResourceResponse -> TestTree
responseDisassociateDelegateFromResource =
  res
    "DisassociateDelegateFromResourceResponse"
    "fixture/DisassociateDelegateFromResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateDelegateFromResource)

responseDeleteAccessControlRule :: DeleteAccessControlRuleResponse -> TestTree
responseDeleteAccessControlRule =
  res
    "DeleteAccessControlRuleResponse"
    "fixture/DeleteAccessControlRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAccessControlRule)

responseListResourceDelegates :: ListResourceDelegatesResponse -> TestTree
responseListResourceDelegates =
  res
    "ListResourceDelegatesResponse"
    "fixture/ListResourceDelegatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourceDelegates)

responseListAccessControlRules :: ListAccessControlRulesResponse -> TestTree
responseListAccessControlRules =
  res
    "ListAccessControlRulesResponse"
    "fixture/ListAccessControlRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccessControlRules)

responseDescribeUser :: DescribeUserResponse -> TestTree
responseDescribeUser =
  res
    "DescribeUserResponse"
    "fixture/DescribeUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUser)

responsePutAccessControlRule :: PutAccessControlRuleResponse -> TestTree
responsePutAccessControlRule =
  res
    "PutAccessControlRuleResponse"
    "fixture/PutAccessControlRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAccessControlRule)

responseStartMailboxExportJob :: StartMailboxExportJobResponse -> TestTree
responseStartMailboxExportJob =
  res
    "StartMailboxExportJobResponse"
    "fixture/StartMailboxExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartMailboxExportJob)

responseDeleteAlias :: DeleteAliasResponse -> TestTree
responseDeleteAlias =
  res
    "DeleteAliasResponse"
    "fixture/DeleteAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAlias)

responseListOrganizations :: ListOrganizationsResponse -> TestTree
responseListOrganizations =
  res
    "ListOrganizationsResponse"
    "fixture/ListOrganizationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOrganizations)

responseAssociateDelegateToResource :: AssociateDelegateToResourceResponse -> TestTree
responseAssociateDelegateToResource =
  res
    "AssociateDelegateToResourceResponse"
    "fixture/AssociateDelegateToResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateDelegateToResource)

responseGetAccessControlEffect :: GetAccessControlEffectResponse -> TestTree
responseGetAccessControlEffect =
  res
    "GetAccessControlEffectResponse"
    "fixture/GetAccessControlEffectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccessControlEffect)

responseDeleteRetentionPolicy :: DeleteRetentionPolicyResponse -> TestTree
responseDeleteRetentionPolicy =
  res
    "DeleteRetentionPolicyResponse"
    "fixture/DeleteRetentionPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRetentionPolicy)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUser)

responsePutRetentionPolicy :: PutRetentionPolicyResponse -> TestTree
responsePutRetentionPolicy =
  res
    "PutRetentionPolicyResponse"
    "fixture/PutRetentionPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRetentionPolicy)

responseListMailDomains :: ListMailDomainsResponse -> TestTree
responseListMailDomains =
  res
    "ListMailDomainsResponse"
    "fixture/ListMailDomainsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMailDomains)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUser)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseRegisterMailDomain :: RegisterMailDomainResponse -> TestTree
responseRegisterMailDomain =
  res
    "RegisterMailDomainResponse"
    "fixture/RegisterMailDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterMailDomain)

responseUpdateDefaultMailDomain :: UpdateDefaultMailDomainResponse -> TestTree
responseUpdateDefaultMailDomain =
  res
    "UpdateDefaultMailDomainResponse"
    "fixture/UpdateDefaultMailDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDefaultMailDomain)

responseUpdateMobileDeviceAccessRule :: UpdateMobileDeviceAccessRuleResponse -> TestTree
responseUpdateMobileDeviceAccessRule =
  res
    "UpdateMobileDeviceAccessRuleResponse"
    "fixture/UpdateMobileDeviceAccessRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMobileDeviceAccessRule)

responseDeleteMobileDeviceAccessRule :: DeleteMobileDeviceAccessRuleResponse -> TestTree
responseDeleteMobileDeviceAccessRule =
  res
    "DeleteMobileDeviceAccessRuleResponse"
    "fixture/DeleteMobileDeviceAccessRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMobileDeviceAccessRule)

responseCreateGroup :: CreateGroupResponse -> TestTree
responseCreateGroup =
  res
    "CreateGroupResponse"
    "fixture/CreateGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGroup)

responseUpdateMailboxQuota :: UpdateMailboxQuotaResponse -> TestTree
responseUpdateMailboxQuota =
  res
    "UpdateMailboxQuotaResponse"
    "fixture/UpdateMailboxQuotaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMailboxQuota)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseListMobileDeviceAccessRules :: ListMobileDeviceAccessRulesResponse -> TestTree
responseListMobileDeviceAccessRules =
  res
    "ListMobileDeviceAccessRulesResponse"
    "fixture/ListMobileDeviceAccessRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMobileDeviceAccessRules)

responseDeleteGroup :: DeleteGroupResponse -> TestTree
responseDeleteGroup =
  res
    "DeleteGroupResponse"
    "fixture/DeleteGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGroup)

responseListGroups :: ListGroupsResponse -> TestTree
responseListGroups =
  res
    "ListGroupsResponse"
    "fixture/ListGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGroups)

responseDescribeOrganization :: DescribeOrganizationResponse -> TestTree
responseDescribeOrganization =
  res
    "DescribeOrganizationResponse"
    "fixture/DescribeOrganizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOrganization)

responseCreateResource :: CreateResourceResponse -> TestTree
responseCreateResource =
  res
    "CreateResourceResponse"
    "fixture/CreateResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateResource)

responseGetDefaultRetentionPolicy :: GetDefaultRetentionPolicyResponse -> TestTree
responseGetDefaultRetentionPolicy =
  res
    "GetDefaultRetentionPolicyResponse"
    "fixture/GetDefaultRetentionPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDefaultRetentionPolicy)

responseDeregisterMailDomain :: DeregisterMailDomainResponse -> TestTree
responseDeregisterMailDomain =
  res
    "DeregisterMailDomainResponse"
    "fixture/DeregisterMailDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterMailDomain)

responseCancelMailboxExportJob :: CancelMailboxExportJobResponse -> TestTree
responseCancelMailboxExportJob =
  res
    "CancelMailboxExportJobResponse"
    "fixture/CancelMailboxExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelMailboxExportJob)

responseListMobileDeviceAccessOverrides :: ListMobileDeviceAccessOverridesResponse -> TestTree
responseListMobileDeviceAccessOverrides =
  res
    "ListMobileDeviceAccessOverridesResponse"
    "fixture/ListMobileDeviceAccessOverridesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMobileDeviceAccessOverrides)

responseDeleteMobileDeviceAccessOverride :: DeleteMobileDeviceAccessOverrideResponse -> TestTree
responseDeleteMobileDeviceAccessOverride =
  res
    "DeleteMobileDeviceAccessOverrideResponse"
    "fixture/DeleteMobileDeviceAccessOverrideResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMobileDeviceAccessOverride)

responsePutMobileDeviceAccessOverride :: PutMobileDeviceAccessOverrideResponse -> TestTree
responsePutMobileDeviceAccessOverride =
  res
    "PutMobileDeviceAccessOverrideResponse"
    "fixture/PutMobileDeviceAccessOverrideResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutMobileDeviceAccessOverride)
