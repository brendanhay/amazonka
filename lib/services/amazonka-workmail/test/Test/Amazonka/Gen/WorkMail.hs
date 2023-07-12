{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.WorkMail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.WorkMail where

import Amazonka.WorkMail
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.WorkMail.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateDelegateToResource $
--             newAssociateDelegateToResource
--
--         , requestAssociateMemberToGroup $
--             newAssociateMemberToGroup
--
--         , requestAssumeImpersonationRole $
--             newAssumeImpersonationRole
--
--         , requestCancelMailboxExportJob $
--             newCancelMailboxExportJob
--
--         , requestCreateAlias $
--             newCreateAlias
--
--         , requestCreateAvailabilityConfiguration $
--             newCreateAvailabilityConfiguration
--
--         , requestCreateGroup $
--             newCreateGroup
--
--         , requestCreateImpersonationRole $
--             newCreateImpersonationRole
--
--         , requestCreateMobileDeviceAccessRule $
--             newCreateMobileDeviceAccessRule
--
--         , requestCreateOrganization $
--             newCreateOrganization
--
--         , requestCreateResource $
--             newCreateResource
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestDeleteAccessControlRule $
--             newDeleteAccessControlRule
--
--         , requestDeleteAlias $
--             newDeleteAlias
--
--         , requestDeleteAvailabilityConfiguration $
--             newDeleteAvailabilityConfiguration
--
--         , requestDeleteEmailMonitoringConfiguration $
--             newDeleteEmailMonitoringConfiguration
--
--         , requestDeleteGroup $
--             newDeleteGroup
--
--         , requestDeleteImpersonationRole $
--             newDeleteImpersonationRole
--
--         , requestDeleteMailboxPermissions $
--             newDeleteMailboxPermissions
--
--         , requestDeleteMobileDeviceAccessOverride $
--             newDeleteMobileDeviceAccessOverride
--
--         , requestDeleteMobileDeviceAccessRule $
--             newDeleteMobileDeviceAccessRule
--
--         , requestDeleteOrganization $
--             newDeleteOrganization
--
--         , requestDeleteResource $
--             newDeleteResource
--
--         , requestDeleteRetentionPolicy $
--             newDeleteRetentionPolicy
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestDeregisterFromWorkMail $
--             newDeregisterFromWorkMail
--
--         , requestDeregisterMailDomain $
--             newDeregisterMailDomain
--
--         , requestDescribeEmailMonitoringConfiguration $
--             newDescribeEmailMonitoringConfiguration
--
--         , requestDescribeGroup $
--             newDescribeGroup
--
--         , requestDescribeInboundDmarcSettings $
--             newDescribeInboundDmarcSettings
--
--         , requestDescribeMailboxExportJob $
--             newDescribeMailboxExportJob
--
--         , requestDescribeOrganization $
--             newDescribeOrganization
--
--         , requestDescribeResource $
--             newDescribeResource
--
--         , requestDescribeUser $
--             newDescribeUser
--
--         , requestDisassociateDelegateFromResource $
--             newDisassociateDelegateFromResource
--
--         , requestDisassociateMemberFromGroup $
--             newDisassociateMemberFromGroup
--
--         , requestGetAccessControlEffect $
--             newGetAccessControlEffect
--
--         , requestGetDefaultRetentionPolicy $
--             newGetDefaultRetentionPolicy
--
--         , requestGetImpersonationRole $
--             newGetImpersonationRole
--
--         , requestGetImpersonationRoleEffect $
--             newGetImpersonationRoleEffect
--
--         , requestGetMailDomain $
--             newGetMailDomain
--
--         , requestGetMailboxDetails $
--             newGetMailboxDetails
--
--         , requestGetMobileDeviceAccessEffect $
--             newGetMobileDeviceAccessEffect
--
--         , requestGetMobileDeviceAccessOverride $
--             newGetMobileDeviceAccessOverride
--
--         , requestListAccessControlRules $
--             newListAccessControlRules
--
--         , requestListAliases $
--             newListAliases
--
--         , requestListAvailabilityConfigurations $
--             newListAvailabilityConfigurations
--
--         , requestListGroupMembers $
--             newListGroupMembers
--
--         , requestListGroups $
--             newListGroups
--
--         , requestListImpersonationRoles $
--             newListImpersonationRoles
--
--         , requestListMailDomains $
--             newListMailDomains
--
--         , requestListMailboxExportJobs $
--             newListMailboxExportJobs
--
--         , requestListMailboxPermissions $
--             newListMailboxPermissions
--
--         , requestListMobileDeviceAccessOverrides $
--             newListMobileDeviceAccessOverrides
--
--         , requestListMobileDeviceAccessRules $
--             newListMobileDeviceAccessRules
--
--         , requestListOrganizations $
--             newListOrganizations
--
--         , requestListResourceDelegates $
--             newListResourceDelegates
--
--         , requestListResources $
--             newListResources
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListUsers $
--             newListUsers
--
--         , requestPutAccessControlRule $
--             newPutAccessControlRule
--
--         , requestPutEmailMonitoringConfiguration $
--             newPutEmailMonitoringConfiguration
--
--         , requestPutInboundDmarcSettings $
--             newPutInboundDmarcSettings
--
--         , requestPutMailboxPermissions $
--             newPutMailboxPermissions
--
--         , requestPutMobileDeviceAccessOverride $
--             newPutMobileDeviceAccessOverride
--
--         , requestPutRetentionPolicy $
--             newPutRetentionPolicy
--
--         , requestRegisterMailDomain $
--             newRegisterMailDomain
--
--         , requestRegisterToWorkMail $
--             newRegisterToWorkMail
--
--         , requestResetPassword $
--             newResetPassword
--
--         , requestStartMailboxExportJob $
--             newStartMailboxExportJob
--
--         , requestTagResource $
--             newTagResource
--
--         , requestTestAvailabilityConfiguration $
--             newTestAvailabilityConfiguration
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateAvailabilityConfiguration $
--             newUpdateAvailabilityConfiguration
--
--         , requestUpdateDefaultMailDomain $
--             newUpdateDefaultMailDomain
--
--         , requestUpdateImpersonationRole $
--             newUpdateImpersonationRole
--
--         , requestUpdateMailboxQuota $
--             newUpdateMailboxQuota
--
--         , requestUpdateMobileDeviceAccessRule $
--             newUpdateMobileDeviceAccessRule
--
--         , requestUpdatePrimaryEmailAddress $
--             newUpdatePrimaryEmailAddress
--
--         , requestUpdateResource $
--             newUpdateResource
--
--           ]

--     , testGroup "response"
--         [ responseAssociateDelegateToResource $
--             newAssociateDelegateToResourceResponse
--
--         , responseAssociateMemberToGroup $
--             newAssociateMemberToGroupResponse
--
--         , responseAssumeImpersonationRole $
--             newAssumeImpersonationRoleResponse
--
--         , responseCancelMailboxExportJob $
--             newCancelMailboxExportJobResponse
--
--         , responseCreateAlias $
--             newCreateAliasResponse
--
--         , responseCreateAvailabilityConfiguration $
--             newCreateAvailabilityConfigurationResponse
--
--         , responseCreateGroup $
--             newCreateGroupResponse
--
--         , responseCreateImpersonationRole $
--             newCreateImpersonationRoleResponse
--
--         , responseCreateMobileDeviceAccessRule $
--             newCreateMobileDeviceAccessRuleResponse
--
--         , responseCreateOrganization $
--             newCreateOrganizationResponse
--
--         , responseCreateResource $
--             newCreateResourceResponse
--
--         , responseCreateUser $
--             newCreateUserResponse
--
--         , responseDeleteAccessControlRule $
--             newDeleteAccessControlRuleResponse
--
--         , responseDeleteAlias $
--             newDeleteAliasResponse
--
--         , responseDeleteAvailabilityConfiguration $
--             newDeleteAvailabilityConfigurationResponse
--
--         , responseDeleteEmailMonitoringConfiguration $
--             newDeleteEmailMonitoringConfigurationResponse
--
--         , responseDeleteGroup $
--             newDeleteGroupResponse
--
--         , responseDeleteImpersonationRole $
--             newDeleteImpersonationRoleResponse
--
--         , responseDeleteMailboxPermissions $
--             newDeleteMailboxPermissionsResponse
--
--         , responseDeleteMobileDeviceAccessOverride $
--             newDeleteMobileDeviceAccessOverrideResponse
--
--         , responseDeleteMobileDeviceAccessRule $
--             newDeleteMobileDeviceAccessRuleResponse
--
--         , responseDeleteOrganization $
--             newDeleteOrganizationResponse
--
--         , responseDeleteResource $
--             newDeleteResourceResponse
--
--         , responseDeleteRetentionPolicy $
--             newDeleteRetentionPolicyResponse
--
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseDeregisterFromWorkMail $
--             newDeregisterFromWorkMailResponse
--
--         , responseDeregisterMailDomain $
--             newDeregisterMailDomainResponse
--
--         , responseDescribeEmailMonitoringConfiguration $
--             newDescribeEmailMonitoringConfigurationResponse
--
--         , responseDescribeGroup $
--             newDescribeGroupResponse
--
--         , responseDescribeInboundDmarcSettings $
--             newDescribeInboundDmarcSettingsResponse
--
--         , responseDescribeMailboxExportJob $
--             newDescribeMailboxExportJobResponse
--
--         , responseDescribeOrganization $
--             newDescribeOrganizationResponse
--
--         , responseDescribeResource $
--             newDescribeResourceResponse
--
--         , responseDescribeUser $
--             newDescribeUserResponse
--
--         , responseDisassociateDelegateFromResource $
--             newDisassociateDelegateFromResourceResponse
--
--         , responseDisassociateMemberFromGroup $
--             newDisassociateMemberFromGroupResponse
--
--         , responseGetAccessControlEffect $
--             newGetAccessControlEffectResponse
--
--         , responseGetDefaultRetentionPolicy $
--             newGetDefaultRetentionPolicyResponse
--
--         , responseGetImpersonationRole $
--             newGetImpersonationRoleResponse
--
--         , responseGetImpersonationRoleEffect $
--             newGetImpersonationRoleEffectResponse
--
--         , responseGetMailDomain $
--             newGetMailDomainResponse
--
--         , responseGetMailboxDetails $
--             newGetMailboxDetailsResponse
--
--         , responseGetMobileDeviceAccessEffect $
--             newGetMobileDeviceAccessEffectResponse
--
--         , responseGetMobileDeviceAccessOverride $
--             newGetMobileDeviceAccessOverrideResponse
--
--         , responseListAccessControlRules $
--             newListAccessControlRulesResponse
--
--         , responseListAliases $
--             newListAliasesResponse
--
--         , responseListAvailabilityConfigurations $
--             newListAvailabilityConfigurationsResponse
--
--         , responseListGroupMembers $
--             newListGroupMembersResponse
--
--         , responseListGroups $
--             newListGroupsResponse
--
--         , responseListImpersonationRoles $
--             newListImpersonationRolesResponse
--
--         , responseListMailDomains $
--             newListMailDomainsResponse
--
--         , responseListMailboxExportJobs $
--             newListMailboxExportJobsResponse
--
--         , responseListMailboxPermissions $
--             newListMailboxPermissionsResponse
--
--         , responseListMobileDeviceAccessOverrides $
--             newListMobileDeviceAccessOverridesResponse
--
--         , responseListMobileDeviceAccessRules $
--             newListMobileDeviceAccessRulesResponse
--
--         , responseListOrganizations $
--             newListOrganizationsResponse
--
--         , responseListResourceDelegates $
--             newListResourceDelegatesResponse
--
--         , responseListResources $
--             newListResourcesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListUsers $
--             newListUsersResponse
--
--         , responsePutAccessControlRule $
--             newPutAccessControlRuleResponse
--
--         , responsePutEmailMonitoringConfiguration $
--             newPutEmailMonitoringConfigurationResponse
--
--         , responsePutInboundDmarcSettings $
--             newPutInboundDmarcSettingsResponse
--
--         , responsePutMailboxPermissions $
--             newPutMailboxPermissionsResponse
--
--         , responsePutMobileDeviceAccessOverride $
--             newPutMobileDeviceAccessOverrideResponse
--
--         , responsePutRetentionPolicy $
--             newPutRetentionPolicyResponse
--
--         , responseRegisterMailDomain $
--             newRegisterMailDomainResponse
--
--         , responseRegisterToWorkMail $
--             newRegisterToWorkMailResponse
--
--         , responseResetPassword $
--             newResetPasswordResponse
--
--         , responseStartMailboxExportJob $
--             newStartMailboxExportJobResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseTestAvailabilityConfiguration $
--             newTestAvailabilityConfigurationResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateAvailabilityConfiguration $
--             newUpdateAvailabilityConfigurationResponse
--
--         , responseUpdateDefaultMailDomain $
--             newUpdateDefaultMailDomainResponse
--
--         , responseUpdateImpersonationRole $
--             newUpdateImpersonationRoleResponse
--
--         , responseUpdateMailboxQuota $
--             newUpdateMailboxQuotaResponse
--
--         , responseUpdateMobileDeviceAccessRule $
--             newUpdateMobileDeviceAccessRuleResponse
--
--         , responseUpdatePrimaryEmailAddress $
--             newUpdatePrimaryEmailAddressResponse
--
--         , responseUpdateResource $
--             newUpdateResourceResponse
--
--           ]
--     ]

-- Requests

requestAssociateDelegateToResource :: AssociateDelegateToResource -> TestTree
requestAssociateDelegateToResource =
  req
    "AssociateDelegateToResource"
    "fixture/AssociateDelegateToResource.yaml"

requestAssociateMemberToGroup :: AssociateMemberToGroup -> TestTree
requestAssociateMemberToGroup =
  req
    "AssociateMemberToGroup"
    "fixture/AssociateMemberToGroup.yaml"

requestAssumeImpersonationRole :: AssumeImpersonationRole -> TestTree
requestAssumeImpersonationRole =
  req
    "AssumeImpersonationRole"
    "fixture/AssumeImpersonationRole.yaml"

requestCancelMailboxExportJob :: CancelMailboxExportJob -> TestTree
requestCancelMailboxExportJob =
  req
    "CancelMailboxExportJob"
    "fixture/CancelMailboxExportJob.yaml"

requestCreateAlias :: CreateAlias -> TestTree
requestCreateAlias =
  req
    "CreateAlias"
    "fixture/CreateAlias.yaml"

requestCreateAvailabilityConfiguration :: CreateAvailabilityConfiguration -> TestTree
requestCreateAvailabilityConfiguration =
  req
    "CreateAvailabilityConfiguration"
    "fixture/CreateAvailabilityConfiguration.yaml"

requestCreateGroup :: CreateGroup -> TestTree
requestCreateGroup =
  req
    "CreateGroup"
    "fixture/CreateGroup.yaml"

requestCreateImpersonationRole :: CreateImpersonationRole -> TestTree
requestCreateImpersonationRole =
  req
    "CreateImpersonationRole"
    "fixture/CreateImpersonationRole.yaml"

requestCreateMobileDeviceAccessRule :: CreateMobileDeviceAccessRule -> TestTree
requestCreateMobileDeviceAccessRule =
  req
    "CreateMobileDeviceAccessRule"
    "fixture/CreateMobileDeviceAccessRule.yaml"

requestCreateOrganization :: CreateOrganization -> TestTree
requestCreateOrganization =
  req
    "CreateOrganization"
    "fixture/CreateOrganization.yaml"

requestCreateResource :: CreateResource -> TestTree
requestCreateResource =
  req
    "CreateResource"
    "fixture/CreateResource.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser =
  req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestDeleteAccessControlRule :: DeleteAccessControlRule -> TestTree
requestDeleteAccessControlRule =
  req
    "DeleteAccessControlRule"
    "fixture/DeleteAccessControlRule.yaml"

requestDeleteAlias :: DeleteAlias -> TestTree
requestDeleteAlias =
  req
    "DeleteAlias"
    "fixture/DeleteAlias.yaml"

requestDeleteAvailabilityConfiguration :: DeleteAvailabilityConfiguration -> TestTree
requestDeleteAvailabilityConfiguration =
  req
    "DeleteAvailabilityConfiguration"
    "fixture/DeleteAvailabilityConfiguration.yaml"

requestDeleteEmailMonitoringConfiguration :: DeleteEmailMonitoringConfiguration -> TestTree
requestDeleteEmailMonitoringConfiguration =
  req
    "DeleteEmailMonitoringConfiguration"
    "fixture/DeleteEmailMonitoringConfiguration.yaml"

requestDeleteGroup :: DeleteGroup -> TestTree
requestDeleteGroup =
  req
    "DeleteGroup"
    "fixture/DeleteGroup.yaml"

requestDeleteImpersonationRole :: DeleteImpersonationRole -> TestTree
requestDeleteImpersonationRole =
  req
    "DeleteImpersonationRole"
    "fixture/DeleteImpersonationRole.yaml"

requestDeleteMailboxPermissions :: DeleteMailboxPermissions -> TestTree
requestDeleteMailboxPermissions =
  req
    "DeleteMailboxPermissions"
    "fixture/DeleteMailboxPermissions.yaml"

requestDeleteMobileDeviceAccessOverride :: DeleteMobileDeviceAccessOverride -> TestTree
requestDeleteMobileDeviceAccessOverride =
  req
    "DeleteMobileDeviceAccessOverride"
    "fixture/DeleteMobileDeviceAccessOverride.yaml"

requestDeleteMobileDeviceAccessRule :: DeleteMobileDeviceAccessRule -> TestTree
requestDeleteMobileDeviceAccessRule =
  req
    "DeleteMobileDeviceAccessRule"
    "fixture/DeleteMobileDeviceAccessRule.yaml"

requestDeleteOrganization :: DeleteOrganization -> TestTree
requestDeleteOrganization =
  req
    "DeleteOrganization"
    "fixture/DeleteOrganization.yaml"

requestDeleteResource :: DeleteResource -> TestTree
requestDeleteResource =
  req
    "DeleteResource"
    "fixture/DeleteResource.yaml"

requestDeleteRetentionPolicy :: DeleteRetentionPolicy -> TestTree
requestDeleteRetentionPolicy =
  req
    "DeleteRetentionPolicy"
    "fixture/DeleteRetentionPolicy.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestDeregisterFromWorkMail :: DeregisterFromWorkMail -> TestTree
requestDeregisterFromWorkMail =
  req
    "DeregisterFromWorkMail"
    "fixture/DeregisterFromWorkMail.yaml"

requestDeregisterMailDomain :: DeregisterMailDomain -> TestTree
requestDeregisterMailDomain =
  req
    "DeregisterMailDomain"
    "fixture/DeregisterMailDomain.yaml"

requestDescribeEmailMonitoringConfiguration :: DescribeEmailMonitoringConfiguration -> TestTree
requestDescribeEmailMonitoringConfiguration =
  req
    "DescribeEmailMonitoringConfiguration"
    "fixture/DescribeEmailMonitoringConfiguration.yaml"

requestDescribeGroup :: DescribeGroup -> TestTree
requestDescribeGroup =
  req
    "DescribeGroup"
    "fixture/DescribeGroup.yaml"

requestDescribeInboundDmarcSettings :: DescribeInboundDmarcSettings -> TestTree
requestDescribeInboundDmarcSettings =
  req
    "DescribeInboundDmarcSettings"
    "fixture/DescribeInboundDmarcSettings.yaml"

requestDescribeMailboxExportJob :: DescribeMailboxExportJob -> TestTree
requestDescribeMailboxExportJob =
  req
    "DescribeMailboxExportJob"
    "fixture/DescribeMailboxExportJob.yaml"

requestDescribeOrganization :: DescribeOrganization -> TestTree
requestDescribeOrganization =
  req
    "DescribeOrganization"
    "fixture/DescribeOrganization.yaml"

requestDescribeResource :: DescribeResource -> TestTree
requestDescribeResource =
  req
    "DescribeResource"
    "fixture/DescribeResource.yaml"

requestDescribeUser :: DescribeUser -> TestTree
requestDescribeUser =
  req
    "DescribeUser"
    "fixture/DescribeUser.yaml"

requestDisassociateDelegateFromResource :: DisassociateDelegateFromResource -> TestTree
requestDisassociateDelegateFromResource =
  req
    "DisassociateDelegateFromResource"
    "fixture/DisassociateDelegateFromResource.yaml"

requestDisassociateMemberFromGroup :: DisassociateMemberFromGroup -> TestTree
requestDisassociateMemberFromGroup =
  req
    "DisassociateMemberFromGroup"
    "fixture/DisassociateMemberFromGroup.yaml"

requestGetAccessControlEffect :: GetAccessControlEffect -> TestTree
requestGetAccessControlEffect =
  req
    "GetAccessControlEffect"
    "fixture/GetAccessControlEffect.yaml"

requestGetDefaultRetentionPolicy :: GetDefaultRetentionPolicy -> TestTree
requestGetDefaultRetentionPolicy =
  req
    "GetDefaultRetentionPolicy"
    "fixture/GetDefaultRetentionPolicy.yaml"

requestGetImpersonationRole :: GetImpersonationRole -> TestTree
requestGetImpersonationRole =
  req
    "GetImpersonationRole"
    "fixture/GetImpersonationRole.yaml"

requestGetImpersonationRoleEffect :: GetImpersonationRoleEffect -> TestTree
requestGetImpersonationRoleEffect =
  req
    "GetImpersonationRoleEffect"
    "fixture/GetImpersonationRoleEffect.yaml"

requestGetMailDomain :: GetMailDomain -> TestTree
requestGetMailDomain =
  req
    "GetMailDomain"
    "fixture/GetMailDomain.yaml"

requestGetMailboxDetails :: GetMailboxDetails -> TestTree
requestGetMailboxDetails =
  req
    "GetMailboxDetails"
    "fixture/GetMailboxDetails.yaml"

requestGetMobileDeviceAccessEffect :: GetMobileDeviceAccessEffect -> TestTree
requestGetMobileDeviceAccessEffect =
  req
    "GetMobileDeviceAccessEffect"
    "fixture/GetMobileDeviceAccessEffect.yaml"

requestGetMobileDeviceAccessOverride :: GetMobileDeviceAccessOverride -> TestTree
requestGetMobileDeviceAccessOverride =
  req
    "GetMobileDeviceAccessOverride"
    "fixture/GetMobileDeviceAccessOverride.yaml"

requestListAccessControlRules :: ListAccessControlRules -> TestTree
requestListAccessControlRules =
  req
    "ListAccessControlRules"
    "fixture/ListAccessControlRules.yaml"

requestListAliases :: ListAliases -> TestTree
requestListAliases =
  req
    "ListAliases"
    "fixture/ListAliases.yaml"

requestListAvailabilityConfigurations :: ListAvailabilityConfigurations -> TestTree
requestListAvailabilityConfigurations =
  req
    "ListAvailabilityConfigurations"
    "fixture/ListAvailabilityConfigurations.yaml"

requestListGroupMembers :: ListGroupMembers -> TestTree
requestListGroupMembers =
  req
    "ListGroupMembers"
    "fixture/ListGroupMembers.yaml"

requestListGroups :: ListGroups -> TestTree
requestListGroups =
  req
    "ListGroups"
    "fixture/ListGroups.yaml"

requestListImpersonationRoles :: ListImpersonationRoles -> TestTree
requestListImpersonationRoles =
  req
    "ListImpersonationRoles"
    "fixture/ListImpersonationRoles.yaml"

requestListMailDomains :: ListMailDomains -> TestTree
requestListMailDomains =
  req
    "ListMailDomains"
    "fixture/ListMailDomains.yaml"

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

requestListMobileDeviceAccessOverrides :: ListMobileDeviceAccessOverrides -> TestTree
requestListMobileDeviceAccessOverrides =
  req
    "ListMobileDeviceAccessOverrides"
    "fixture/ListMobileDeviceAccessOverrides.yaml"

requestListMobileDeviceAccessRules :: ListMobileDeviceAccessRules -> TestTree
requestListMobileDeviceAccessRules =
  req
    "ListMobileDeviceAccessRules"
    "fixture/ListMobileDeviceAccessRules.yaml"

requestListOrganizations :: ListOrganizations -> TestTree
requestListOrganizations =
  req
    "ListOrganizations"
    "fixture/ListOrganizations.yaml"

requestListResourceDelegates :: ListResourceDelegates -> TestTree
requestListResourceDelegates =
  req
    "ListResourceDelegates"
    "fixture/ListResourceDelegates.yaml"

requestListResources :: ListResources -> TestTree
requestListResources =
  req
    "ListResources"
    "fixture/ListResources.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListUsers :: ListUsers -> TestTree
requestListUsers =
  req
    "ListUsers"
    "fixture/ListUsers.yaml"

requestPutAccessControlRule :: PutAccessControlRule -> TestTree
requestPutAccessControlRule =
  req
    "PutAccessControlRule"
    "fixture/PutAccessControlRule.yaml"

requestPutEmailMonitoringConfiguration :: PutEmailMonitoringConfiguration -> TestTree
requestPutEmailMonitoringConfiguration =
  req
    "PutEmailMonitoringConfiguration"
    "fixture/PutEmailMonitoringConfiguration.yaml"

requestPutInboundDmarcSettings :: PutInboundDmarcSettings -> TestTree
requestPutInboundDmarcSettings =
  req
    "PutInboundDmarcSettings"
    "fixture/PutInboundDmarcSettings.yaml"

requestPutMailboxPermissions :: PutMailboxPermissions -> TestTree
requestPutMailboxPermissions =
  req
    "PutMailboxPermissions"
    "fixture/PutMailboxPermissions.yaml"

requestPutMobileDeviceAccessOverride :: PutMobileDeviceAccessOverride -> TestTree
requestPutMobileDeviceAccessOverride =
  req
    "PutMobileDeviceAccessOverride"
    "fixture/PutMobileDeviceAccessOverride.yaml"

requestPutRetentionPolicy :: PutRetentionPolicy -> TestTree
requestPutRetentionPolicy =
  req
    "PutRetentionPolicy"
    "fixture/PutRetentionPolicy.yaml"

requestRegisterMailDomain :: RegisterMailDomain -> TestTree
requestRegisterMailDomain =
  req
    "RegisterMailDomain"
    "fixture/RegisterMailDomain.yaml"

requestRegisterToWorkMail :: RegisterToWorkMail -> TestTree
requestRegisterToWorkMail =
  req
    "RegisterToWorkMail"
    "fixture/RegisterToWorkMail.yaml"

requestResetPassword :: ResetPassword -> TestTree
requestResetPassword =
  req
    "ResetPassword"
    "fixture/ResetPassword.yaml"

requestStartMailboxExportJob :: StartMailboxExportJob -> TestTree
requestStartMailboxExportJob =
  req
    "StartMailboxExportJob"
    "fixture/StartMailboxExportJob.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestTestAvailabilityConfiguration :: TestAvailabilityConfiguration -> TestTree
requestTestAvailabilityConfiguration =
  req
    "TestAvailabilityConfiguration"
    "fixture/TestAvailabilityConfiguration.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateAvailabilityConfiguration :: UpdateAvailabilityConfiguration -> TestTree
requestUpdateAvailabilityConfiguration =
  req
    "UpdateAvailabilityConfiguration"
    "fixture/UpdateAvailabilityConfiguration.yaml"

requestUpdateDefaultMailDomain :: UpdateDefaultMailDomain -> TestTree
requestUpdateDefaultMailDomain =
  req
    "UpdateDefaultMailDomain"
    "fixture/UpdateDefaultMailDomain.yaml"

requestUpdateImpersonationRole :: UpdateImpersonationRole -> TestTree
requestUpdateImpersonationRole =
  req
    "UpdateImpersonationRole"
    "fixture/UpdateImpersonationRole.yaml"

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

requestUpdatePrimaryEmailAddress :: UpdatePrimaryEmailAddress -> TestTree
requestUpdatePrimaryEmailAddress =
  req
    "UpdatePrimaryEmailAddress"
    "fixture/UpdatePrimaryEmailAddress.yaml"

requestUpdateResource :: UpdateResource -> TestTree
requestUpdateResource =
  req
    "UpdateResource"
    "fixture/UpdateResource.yaml"

-- Responses

responseAssociateDelegateToResource :: AssociateDelegateToResourceResponse -> TestTree
responseAssociateDelegateToResource =
  res
    "AssociateDelegateToResourceResponse"
    "fixture/AssociateDelegateToResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateDelegateToResource)

responseAssociateMemberToGroup :: AssociateMemberToGroupResponse -> TestTree
responseAssociateMemberToGroup =
  res
    "AssociateMemberToGroupResponse"
    "fixture/AssociateMemberToGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateMemberToGroup)

responseAssumeImpersonationRole :: AssumeImpersonationRoleResponse -> TestTree
responseAssumeImpersonationRole =
  res
    "AssumeImpersonationRoleResponse"
    "fixture/AssumeImpersonationRoleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssumeImpersonationRole)

responseCancelMailboxExportJob :: CancelMailboxExportJobResponse -> TestTree
responseCancelMailboxExportJob =
  res
    "CancelMailboxExportJobResponse"
    "fixture/CancelMailboxExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelMailboxExportJob)

responseCreateAlias :: CreateAliasResponse -> TestTree
responseCreateAlias =
  res
    "CreateAliasResponse"
    "fixture/CreateAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAlias)

responseCreateAvailabilityConfiguration :: CreateAvailabilityConfigurationResponse -> TestTree
responseCreateAvailabilityConfiguration =
  res
    "CreateAvailabilityConfigurationResponse"
    "fixture/CreateAvailabilityConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAvailabilityConfiguration)

responseCreateGroup :: CreateGroupResponse -> TestTree
responseCreateGroup =
  res
    "CreateGroupResponse"
    "fixture/CreateGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGroup)

responseCreateImpersonationRole :: CreateImpersonationRoleResponse -> TestTree
responseCreateImpersonationRole =
  res
    "CreateImpersonationRoleResponse"
    "fixture/CreateImpersonationRoleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateImpersonationRole)

responseCreateMobileDeviceAccessRule :: CreateMobileDeviceAccessRuleResponse -> TestTree
responseCreateMobileDeviceAccessRule =
  res
    "CreateMobileDeviceAccessRuleResponse"
    "fixture/CreateMobileDeviceAccessRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateMobileDeviceAccessRule)

responseCreateOrganization :: CreateOrganizationResponse -> TestTree
responseCreateOrganization =
  res
    "CreateOrganizationResponse"
    "fixture/CreateOrganizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateOrganization)

responseCreateResource :: CreateResourceResponse -> TestTree
responseCreateResource =
  res
    "CreateResourceResponse"
    "fixture/CreateResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateResource)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUser)

responseDeleteAccessControlRule :: DeleteAccessControlRuleResponse -> TestTree
responseDeleteAccessControlRule =
  res
    "DeleteAccessControlRuleResponse"
    "fixture/DeleteAccessControlRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAccessControlRule)

responseDeleteAlias :: DeleteAliasResponse -> TestTree
responseDeleteAlias =
  res
    "DeleteAliasResponse"
    "fixture/DeleteAliasResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAlias)

responseDeleteAvailabilityConfiguration :: DeleteAvailabilityConfigurationResponse -> TestTree
responseDeleteAvailabilityConfiguration =
  res
    "DeleteAvailabilityConfigurationResponse"
    "fixture/DeleteAvailabilityConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAvailabilityConfiguration)

responseDeleteEmailMonitoringConfiguration :: DeleteEmailMonitoringConfigurationResponse -> TestTree
responseDeleteEmailMonitoringConfiguration =
  res
    "DeleteEmailMonitoringConfigurationResponse"
    "fixture/DeleteEmailMonitoringConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEmailMonitoringConfiguration)

responseDeleteGroup :: DeleteGroupResponse -> TestTree
responseDeleteGroup =
  res
    "DeleteGroupResponse"
    "fixture/DeleteGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteGroup)

responseDeleteImpersonationRole :: DeleteImpersonationRoleResponse -> TestTree
responseDeleteImpersonationRole =
  res
    "DeleteImpersonationRoleResponse"
    "fixture/DeleteImpersonationRoleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteImpersonationRole)

responseDeleteMailboxPermissions :: DeleteMailboxPermissionsResponse -> TestTree
responseDeleteMailboxPermissions =
  res
    "DeleteMailboxPermissionsResponse"
    "fixture/DeleteMailboxPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMailboxPermissions)

responseDeleteMobileDeviceAccessOverride :: DeleteMobileDeviceAccessOverrideResponse -> TestTree
responseDeleteMobileDeviceAccessOverride =
  res
    "DeleteMobileDeviceAccessOverrideResponse"
    "fixture/DeleteMobileDeviceAccessOverrideResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMobileDeviceAccessOverride)

responseDeleteMobileDeviceAccessRule :: DeleteMobileDeviceAccessRuleResponse -> TestTree
responseDeleteMobileDeviceAccessRule =
  res
    "DeleteMobileDeviceAccessRuleResponse"
    "fixture/DeleteMobileDeviceAccessRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteMobileDeviceAccessRule)

responseDeleteOrganization :: DeleteOrganizationResponse -> TestTree
responseDeleteOrganization =
  res
    "DeleteOrganizationResponse"
    "fixture/DeleteOrganizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteOrganization)

responseDeleteResource :: DeleteResourceResponse -> TestTree
responseDeleteResource =
  res
    "DeleteResourceResponse"
    "fixture/DeleteResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResource)

responseDeleteRetentionPolicy :: DeleteRetentionPolicyResponse -> TestTree
responseDeleteRetentionPolicy =
  res
    "DeleteRetentionPolicyResponse"
    "fixture/DeleteRetentionPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteRetentionPolicy)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUser)

responseDeregisterFromWorkMail :: DeregisterFromWorkMailResponse -> TestTree
responseDeregisterFromWorkMail =
  res
    "DeregisterFromWorkMailResponse"
    "fixture/DeregisterFromWorkMailResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterFromWorkMail)

responseDeregisterMailDomain :: DeregisterMailDomainResponse -> TestTree
responseDeregisterMailDomain =
  res
    "DeregisterMailDomainResponse"
    "fixture/DeregisterMailDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterMailDomain)

responseDescribeEmailMonitoringConfiguration :: DescribeEmailMonitoringConfigurationResponse -> TestTree
responseDescribeEmailMonitoringConfiguration =
  res
    "DescribeEmailMonitoringConfigurationResponse"
    "fixture/DescribeEmailMonitoringConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEmailMonitoringConfiguration)

responseDescribeGroup :: DescribeGroupResponse -> TestTree
responseDescribeGroup =
  res
    "DescribeGroupResponse"
    "fixture/DescribeGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeGroup)

responseDescribeInboundDmarcSettings :: DescribeInboundDmarcSettingsResponse -> TestTree
responseDescribeInboundDmarcSettings =
  res
    "DescribeInboundDmarcSettingsResponse"
    "fixture/DescribeInboundDmarcSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeInboundDmarcSettings)

responseDescribeMailboxExportJob :: DescribeMailboxExportJobResponse -> TestTree
responseDescribeMailboxExportJob =
  res
    "DescribeMailboxExportJobResponse"
    "fixture/DescribeMailboxExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeMailboxExportJob)

responseDescribeOrganization :: DescribeOrganizationResponse -> TestTree
responseDescribeOrganization =
  res
    "DescribeOrganizationResponse"
    "fixture/DescribeOrganizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOrganization)

responseDescribeResource :: DescribeResourceResponse -> TestTree
responseDescribeResource =
  res
    "DescribeResourceResponse"
    "fixture/DescribeResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeResource)

responseDescribeUser :: DescribeUserResponse -> TestTree
responseDescribeUser =
  res
    "DescribeUserResponse"
    "fixture/DescribeUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUser)

responseDisassociateDelegateFromResource :: DisassociateDelegateFromResourceResponse -> TestTree
responseDisassociateDelegateFromResource =
  res
    "DisassociateDelegateFromResourceResponse"
    "fixture/DisassociateDelegateFromResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateDelegateFromResource)

responseDisassociateMemberFromGroup :: DisassociateMemberFromGroupResponse -> TestTree
responseDisassociateMemberFromGroup =
  res
    "DisassociateMemberFromGroupResponse"
    "fixture/DisassociateMemberFromGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateMemberFromGroup)

responseGetAccessControlEffect :: GetAccessControlEffectResponse -> TestTree
responseGetAccessControlEffect =
  res
    "GetAccessControlEffectResponse"
    "fixture/GetAccessControlEffectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAccessControlEffect)

responseGetDefaultRetentionPolicy :: GetDefaultRetentionPolicyResponse -> TestTree
responseGetDefaultRetentionPolicy =
  res
    "GetDefaultRetentionPolicyResponse"
    "fixture/GetDefaultRetentionPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDefaultRetentionPolicy)

responseGetImpersonationRole :: GetImpersonationRoleResponse -> TestTree
responseGetImpersonationRole =
  res
    "GetImpersonationRoleResponse"
    "fixture/GetImpersonationRoleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetImpersonationRole)

responseGetImpersonationRoleEffect :: GetImpersonationRoleEffectResponse -> TestTree
responseGetImpersonationRoleEffect =
  res
    "GetImpersonationRoleEffectResponse"
    "fixture/GetImpersonationRoleEffectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetImpersonationRoleEffect)

responseGetMailDomain :: GetMailDomainResponse -> TestTree
responseGetMailDomain =
  res
    "GetMailDomainResponse"
    "fixture/GetMailDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMailDomain)

responseGetMailboxDetails :: GetMailboxDetailsResponse -> TestTree
responseGetMailboxDetails =
  res
    "GetMailboxDetailsResponse"
    "fixture/GetMailboxDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMailboxDetails)

responseGetMobileDeviceAccessEffect :: GetMobileDeviceAccessEffectResponse -> TestTree
responseGetMobileDeviceAccessEffect =
  res
    "GetMobileDeviceAccessEffectResponse"
    "fixture/GetMobileDeviceAccessEffectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMobileDeviceAccessEffect)

responseGetMobileDeviceAccessOverride :: GetMobileDeviceAccessOverrideResponse -> TestTree
responseGetMobileDeviceAccessOverride =
  res
    "GetMobileDeviceAccessOverrideResponse"
    "fixture/GetMobileDeviceAccessOverrideResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMobileDeviceAccessOverride)

responseListAccessControlRules :: ListAccessControlRulesResponse -> TestTree
responseListAccessControlRules =
  res
    "ListAccessControlRulesResponse"
    "fixture/ListAccessControlRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccessControlRules)

responseListAliases :: ListAliasesResponse -> TestTree
responseListAliases =
  res
    "ListAliasesResponse"
    "fixture/ListAliasesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAliases)

responseListAvailabilityConfigurations :: ListAvailabilityConfigurationsResponse -> TestTree
responseListAvailabilityConfigurations =
  res
    "ListAvailabilityConfigurationsResponse"
    "fixture/ListAvailabilityConfigurationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAvailabilityConfigurations)

responseListGroupMembers :: ListGroupMembersResponse -> TestTree
responseListGroupMembers =
  res
    "ListGroupMembersResponse"
    "fixture/ListGroupMembersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGroupMembers)

responseListGroups :: ListGroupsResponse -> TestTree
responseListGroups =
  res
    "ListGroupsResponse"
    "fixture/ListGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListGroups)

responseListImpersonationRoles :: ListImpersonationRolesResponse -> TestTree
responseListImpersonationRoles =
  res
    "ListImpersonationRolesResponse"
    "fixture/ListImpersonationRolesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListImpersonationRoles)

responseListMailDomains :: ListMailDomainsResponse -> TestTree
responseListMailDomains =
  res
    "ListMailDomainsResponse"
    "fixture/ListMailDomainsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMailDomains)

responseListMailboxExportJobs :: ListMailboxExportJobsResponse -> TestTree
responseListMailboxExportJobs =
  res
    "ListMailboxExportJobsResponse"
    "fixture/ListMailboxExportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMailboxExportJobs)

responseListMailboxPermissions :: ListMailboxPermissionsResponse -> TestTree
responseListMailboxPermissions =
  res
    "ListMailboxPermissionsResponse"
    "fixture/ListMailboxPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMailboxPermissions)

responseListMobileDeviceAccessOverrides :: ListMobileDeviceAccessOverridesResponse -> TestTree
responseListMobileDeviceAccessOverrides =
  res
    "ListMobileDeviceAccessOverridesResponse"
    "fixture/ListMobileDeviceAccessOverridesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMobileDeviceAccessOverrides)

responseListMobileDeviceAccessRules :: ListMobileDeviceAccessRulesResponse -> TestTree
responseListMobileDeviceAccessRules =
  res
    "ListMobileDeviceAccessRulesResponse"
    "fixture/ListMobileDeviceAccessRulesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListMobileDeviceAccessRules)

responseListOrganizations :: ListOrganizationsResponse -> TestTree
responseListOrganizations =
  res
    "ListOrganizationsResponse"
    "fixture/ListOrganizationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOrganizations)

responseListResourceDelegates :: ListResourceDelegatesResponse -> TestTree
responseListResourceDelegates =
  res
    "ListResourceDelegatesResponse"
    "fixture/ListResourceDelegatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourceDelegates)

responseListResources :: ListResourcesResponse -> TestTree
responseListResources =
  res
    "ListResourcesResponse"
    "fixture/ListResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResources)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListUsers :: ListUsersResponse -> TestTree
responseListUsers =
  res
    "ListUsersResponse"
    "fixture/ListUsersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListUsers)

responsePutAccessControlRule :: PutAccessControlRuleResponse -> TestTree
responsePutAccessControlRule =
  res
    "PutAccessControlRuleResponse"
    "fixture/PutAccessControlRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutAccessControlRule)

responsePutEmailMonitoringConfiguration :: PutEmailMonitoringConfigurationResponse -> TestTree
responsePutEmailMonitoringConfiguration =
  res
    "PutEmailMonitoringConfigurationResponse"
    "fixture/PutEmailMonitoringConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutEmailMonitoringConfiguration)

responsePutInboundDmarcSettings :: PutInboundDmarcSettingsResponse -> TestTree
responsePutInboundDmarcSettings =
  res
    "PutInboundDmarcSettingsResponse"
    "fixture/PutInboundDmarcSettingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutInboundDmarcSettings)

responsePutMailboxPermissions :: PutMailboxPermissionsResponse -> TestTree
responsePutMailboxPermissions =
  res
    "PutMailboxPermissionsResponse"
    "fixture/PutMailboxPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutMailboxPermissions)

responsePutMobileDeviceAccessOverride :: PutMobileDeviceAccessOverrideResponse -> TestTree
responsePutMobileDeviceAccessOverride =
  res
    "PutMobileDeviceAccessOverrideResponse"
    "fixture/PutMobileDeviceAccessOverrideResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutMobileDeviceAccessOverride)

responsePutRetentionPolicy :: PutRetentionPolicyResponse -> TestTree
responsePutRetentionPolicy =
  res
    "PutRetentionPolicyResponse"
    "fixture/PutRetentionPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutRetentionPolicy)

responseRegisterMailDomain :: RegisterMailDomainResponse -> TestTree
responseRegisterMailDomain =
  res
    "RegisterMailDomainResponse"
    "fixture/RegisterMailDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterMailDomain)

responseRegisterToWorkMail :: RegisterToWorkMailResponse -> TestTree
responseRegisterToWorkMail =
  res
    "RegisterToWorkMailResponse"
    "fixture/RegisterToWorkMailResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterToWorkMail)

responseResetPassword :: ResetPasswordResponse -> TestTree
responseResetPassword =
  res
    "ResetPasswordResponse"
    "fixture/ResetPasswordResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ResetPassword)

responseStartMailboxExportJob :: StartMailboxExportJobResponse -> TestTree
responseStartMailboxExportJob =
  res
    "StartMailboxExportJobResponse"
    "fixture/StartMailboxExportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartMailboxExportJob)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseTestAvailabilityConfiguration :: TestAvailabilityConfigurationResponse -> TestTree
responseTestAvailabilityConfiguration =
  res
    "TestAvailabilityConfigurationResponse"
    "fixture/TestAvailabilityConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TestAvailabilityConfiguration)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateAvailabilityConfiguration :: UpdateAvailabilityConfigurationResponse -> TestTree
responseUpdateAvailabilityConfiguration =
  res
    "UpdateAvailabilityConfigurationResponse"
    "fixture/UpdateAvailabilityConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAvailabilityConfiguration)

responseUpdateDefaultMailDomain :: UpdateDefaultMailDomainResponse -> TestTree
responseUpdateDefaultMailDomain =
  res
    "UpdateDefaultMailDomainResponse"
    "fixture/UpdateDefaultMailDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDefaultMailDomain)

responseUpdateImpersonationRole :: UpdateImpersonationRoleResponse -> TestTree
responseUpdateImpersonationRole =
  res
    "UpdateImpersonationRoleResponse"
    "fixture/UpdateImpersonationRoleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateImpersonationRole)

responseUpdateMailboxQuota :: UpdateMailboxQuotaResponse -> TestTree
responseUpdateMailboxQuota =
  res
    "UpdateMailboxQuotaResponse"
    "fixture/UpdateMailboxQuotaResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMailboxQuota)

responseUpdateMobileDeviceAccessRule :: UpdateMobileDeviceAccessRuleResponse -> TestTree
responseUpdateMobileDeviceAccessRule =
  res
    "UpdateMobileDeviceAccessRuleResponse"
    "fixture/UpdateMobileDeviceAccessRuleResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateMobileDeviceAccessRule)

responseUpdatePrimaryEmailAddress :: UpdatePrimaryEmailAddressResponse -> TestTree
responseUpdatePrimaryEmailAddress =
  res
    "UpdatePrimaryEmailAddressResponse"
    "fixture/UpdatePrimaryEmailAddressResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePrimaryEmailAddress)

responseUpdateResource :: UpdateResourceResponse -> TestTree
responseUpdateResource =
  res
    "UpdateResourceResponse"
    "fixture/UpdateResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResource)
