{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Organizations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Organizations where

import Data.Proxy
import Network.AWS.Organizations
import Test.AWS.Fixture
import Test.AWS.Organizations.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListAccountsForParent $
--             newListAccountsForParent
--
--         , requestCreateOrganization $
--             newCreateOrganization
--
--         , requestDescribeCreateAccountStatus $
--             newDescribeCreateAccountStatus
--
--         , requestDisablePolicyType $
--             newDisablePolicyType
--
--         , requestListPolicies $
--             newListPolicies
--
--         , requestCreatePolicy $
--             newCreatePolicy
--
--         , requestListHandshakesForAccount $
--             newListHandshakesForAccount
--
--         , requestUpdatePolicy $
--             newUpdatePolicy
--
--         , requestDeletePolicy $
--             newDeletePolicy
--
--         , requestEnablePolicyType $
--             newEnablePolicyType
--
--         , requestListChildren $
--             newListChildren
--
--         , requestListAWSServiceAccessForOrganization $
--             newListAWSServiceAccessForOrganization
--
--         , requestDescribeOrganization $
--             newDescribeOrganization
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestListCreateAccountStatus $
--             newListCreateAccountStatus
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListAccounts $
--             newListAccounts
--
--         , requestDescribeOrganizationalUnit $
--             newDescribeOrganizationalUnit
--
--         , requestListDelegatedServicesForAccount $
--             newListDelegatedServicesForAccount
--
--         , requestEnableAWSServiceAccess $
--             newEnableAWSServiceAccess
--
--         , requestCreateGovCloudAccount $
--             newCreateGovCloudAccount
--
--         , requestDeleteOrganization $
--             newDeleteOrganization
--
--         , requestRemoveAccountFromOrganization $
--             newRemoveAccountFromOrganization
--
--         , requestAcceptHandshake $
--             newAcceptHandshake
--
--         , requestListRoots $
--             newListRoots
--
--         , requestEnableAllFeatures $
--             newEnableAllFeatures
--
--         , requestDetachPolicy $
--             newDetachPolicy
--
--         , requestDescribeAccount $
--             newDescribeAccount
--
--         , requestCreateOrganizationalUnit $
--             newCreateOrganizationalUnit
--
--         , requestRegisterDelegatedAdministrator $
--             newRegisterDelegatedAdministrator
--
--         , requestCancelHandshake $
--             newCancelHandshake
--
--         , requestDeleteOrganizationalUnit $
--             newDeleteOrganizationalUnit
--
--         , requestListDelegatedAdministrators $
--             newListDelegatedAdministrators
--
--         , requestUpdateOrganizationalUnit $
--             newUpdateOrganizationalUnit
--
--         , requestListOrganizationalUnitsForParent $
--             newListOrganizationalUnitsForParent
--
--         , requestListPoliciesForTarget $
--             newListPoliciesForTarget
--
--         , requestListHandshakesForOrganization $
--             newListHandshakesForOrganization
--
--         , requestAttachPolicy $
--             newAttachPolicy
--
--         , requestDeclineHandshake $
--             newDeclineHandshake
--
--         , requestListTargetsForPolicy $
--             newListTargetsForPolicy
--
--         , requestDescribeEffectivePolicy $
--             newDescribeEffectivePolicy
--
--         , requestInviteAccountToOrganization $
--             newInviteAccountToOrganization
--
--         , requestDisableAWSServiceAccess $
--             newDisableAWSServiceAccess
--
--         , requestMoveAccount $
--             newMoveAccount
--
--         , requestLeaveOrganization $
--             newLeaveOrganization
--
--         , requestDeregisterDelegatedAdministrator $
--             newDeregisterDelegatedAdministrator
--
--         , requestCreateAccount $
--             newCreateAccount
--
--         , requestListParents $
--             newListParents
--
--         , requestDescribePolicy $
--             newDescribePolicy
--
--         , requestDescribeHandshake $
--             newDescribeHandshake
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--           ]

--     , testGroup "response"
--         [ responseListAccountsForParent $
--             newListAccountsForParentResponse
--
--         , responseCreateOrganization $
--             newCreateOrganizationResponse
--
--         , responseDescribeCreateAccountStatus $
--             newDescribeCreateAccountStatusResponse
--
--         , responseDisablePolicyType $
--             newDisablePolicyTypeResponse
--
--         , responseListPolicies $
--             newListPoliciesResponse
--
--         , responseCreatePolicy $
--             newCreatePolicyResponse
--
--         , responseListHandshakesForAccount $
--             newListHandshakesForAccountResponse
--
--         , responseUpdatePolicy $
--             newUpdatePolicyResponse
--
--         , responseDeletePolicy $
--             newDeletePolicyResponse
--
--         , responseEnablePolicyType $
--             newEnablePolicyTypeResponse
--
--         , responseListChildren $
--             newListChildrenResponse
--
--         , responseListAWSServiceAccessForOrganization $
--             newListAWSServiceAccessForOrganizationResponse
--
--         , responseDescribeOrganization $
--             newDescribeOrganizationResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseListCreateAccountStatus $
--             newListCreateAccountStatusResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListAccounts $
--             newListAccountsResponse
--
--         , responseDescribeOrganizationalUnit $
--             newDescribeOrganizationalUnitResponse
--
--         , responseListDelegatedServicesForAccount $
--             newListDelegatedServicesForAccountResponse
--
--         , responseEnableAWSServiceAccess $
--             newEnableAWSServiceAccessResponse
--
--         , responseCreateGovCloudAccount $
--             newCreateGovCloudAccountResponse
--
--         , responseDeleteOrganization $
--             newDeleteOrganizationResponse
--
--         , responseRemoveAccountFromOrganization $
--             newRemoveAccountFromOrganizationResponse
--
--         , responseAcceptHandshake $
--             newAcceptHandshakeResponse
--
--         , responseListRoots $
--             newListRootsResponse
--
--         , responseEnableAllFeatures $
--             newEnableAllFeaturesResponse
--
--         , responseDetachPolicy $
--             newDetachPolicyResponse
--
--         , responseDescribeAccount $
--             newDescribeAccountResponse
--
--         , responseCreateOrganizationalUnit $
--             newCreateOrganizationalUnitResponse
--
--         , responseRegisterDelegatedAdministrator $
--             newRegisterDelegatedAdministratorResponse
--
--         , responseCancelHandshake $
--             newCancelHandshakeResponse
--
--         , responseDeleteOrganizationalUnit $
--             newDeleteOrganizationalUnitResponse
--
--         , responseListDelegatedAdministrators $
--             newListDelegatedAdministratorsResponse
--
--         , responseUpdateOrganizationalUnit $
--             newUpdateOrganizationalUnitResponse
--
--         , responseListOrganizationalUnitsForParent $
--             newListOrganizationalUnitsForParentResponse
--
--         , responseListPoliciesForTarget $
--             newListPoliciesForTargetResponse
--
--         , responseListHandshakesForOrganization $
--             newListHandshakesForOrganizationResponse
--
--         , responseAttachPolicy $
--             newAttachPolicyResponse
--
--         , responseDeclineHandshake $
--             newDeclineHandshakeResponse
--
--         , responseListTargetsForPolicy $
--             newListTargetsForPolicyResponse
--
--         , responseDescribeEffectivePolicy $
--             newDescribeEffectivePolicyResponse
--
--         , responseInviteAccountToOrganization $
--             newInviteAccountToOrganizationResponse
--
--         , responseDisableAWSServiceAccess $
--             newDisableAWSServiceAccessResponse
--
--         , responseMoveAccount $
--             newMoveAccountResponse
--
--         , responseLeaveOrganization $
--             newLeaveOrganizationResponse
--
--         , responseDeregisterDelegatedAdministrator $
--             newDeregisterDelegatedAdministratorResponse
--
--         , responseCreateAccount $
--             newCreateAccountResponse
--
--         , responseListParents $
--             newListParentsResponse
--
--         , responseDescribePolicy $
--             newDescribePolicyResponse
--
--         , responseDescribeHandshake $
--             newDescribeHandshakeResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--           ]
--     ]

-- Requests

requestListAccountsForParent :: ListAccountsForParent -> TestTree
requestListAccountsForParent =
  req
    "ListAccountsForParent"
    "fixture/ListAccountsForParent.yaml"

requestCreateOrganization :: CreateOrganization -> TestTree
requestCreateOrganization =
  req
    "CreateOrganization"
    "fixture/CreateOrganization.yaml"

requestDescribeCreateAccountStatus :: DescribeCreateAccountStatus -> TestTree
requestDescribeCreateAccountStatus =
  req
    "DescribeCreateAccountStatus"
    "fixture/DescribeCreateAccountStatus.yaml"

requestDisablePolicyType :: DisablePolicyType -> TestTree
requestDisablePolicyType =
  req
    "DisablePolicyType"
    "fixture/DisablePolicyType.yaml"

requestListPolicies :: ListPolicies -> TestTree
requestListPolicies =
  req
    "ListPolicies"
    "fixture/ListPolicies.yaml"

requestCreatePolicy :: CreatePolicy -> TestTree
requestCreatePolicy =
  req
    "CreatePolicy"
    "fixture/CreatePolicy.yaml"

requestListHandshakesForAccount :: ListHandshakesForAccount -> TestTree
requestListHandshakesForAccount =
  req
    "ListHandshakesForAccount"
    "fixture/ListHandshakesForAccount.yaml"

requestUpdatePolicy :: UpdatePolicy -> TestTree
requestUpdatePolicy =
  req
    "UpdatePolicy"
    "fixture/UpdatePolicy.yaml"

requestDeletePolicy :: DeletePolicy -> TestTree
requestDeletePolicy =
  req
    "DeletePolicy"
    "fixture/DeletePolicy.yaml"

requestEnablePolicyType :: EnablePolicyType -> TestTree
requestEnablePolicyType =
  req
    "EnablePolicyType"
    "fixture/EnablePolicyType.yaml"

requestListChildren :: ListChildren -> TestTree
requestListChildren =
  req
    "ListChildren"
    "fixture/ListChildren.yaml"

requestListAWSServiceAccessForOrganization :: ListAWSServiceAccessForOrganization -> TestTree
requestListAWSServiceAccessForOrganization =
  req
    "ListAWSServiceAccessForOrganization"
    "fixture/ListAWSServiceAccessForOrganization.yaml"

requestDescribeOrganization :: DescribeOrganization -> TestTree
requestDescribeOrganization =
  req
    "DescribeOrganization"
    "fixture/DescribeOrganization.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestListCreateAccountStatus :: ListCreateAccountStatus -> TestTree
requestListCreateAccountStatus =
  req
    "ListCreateAccountStatus"
    "fixture/ListCreateAccountStatus.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListAccounts :: ListAccounts -> TestTree
requestListAccounts =
  req
    "ListAccounts"
    "fixture/ListAccounts.yaml"

requestDescribeOrganizationalUnit :: DescribeOrganizationalUnit -> TestTree
requestDescribeOrganizationalUnit =
  req
    "DescribeOrganizationalUnit"
    "fixture/DescribeOrganizationalUnit.yaml"

requestListDelegatedServicesForAccount :: ListDelegatedServicesForAccount -> TestTree
requestListDelegatedServicesForAccount =
  req
    "ListDelegatedServicesForAccount"
    "fixture/ListDelegatedServicesForAccount.yaml"

requestEnableAWSServiceAccess :: EnableAWSServiceAccess -> TestTree
requestEnableAWSServiceAccess =
  req
    "EnableAWSServiceAccess"
    "fixture/EnableAWSServiceAccess.yaml"

requestCreateGovCloudAccount :: CreateGovCloudAccount -> TestTree
requestCreateGovCloudAccount =
  req
    "CreateGovCloudAccount"
    "fixture/CreateGovCloudAccount.yaml"

requestDeleteOrganization :: DeleteOrganization -> TestTree
requestDeleteOrganization =
  req
    "DeleteOrganization"
    "fixture/DeleteOrganization.yaml"

requestRemoveAccountFromOrganization :: RemoveAccountFromOrganization -> TestTree
requestRemoveAccountFromOrganization =
  req
    "RemoveAccountFromOrganization"
    "fixture/RemoveAccountFromOrganization.yaml"

requestAcceptHandshake :: AcceptHandshake -> TestTree
requestAcceptHandshake =
  req
    "AcceptHandshake"
    "fixture/AcceptHandshake.yaml"

requestListRoots :: ListRoots -> TestTree
requestListRoots =
  req
    "ListRoots"
    "fixture/ListRoots.yaml"

requestEnableAllFeatures :: EnableAllFeatures -> TestTree
requestEnableAllFeatures =
  req
    "EnableAllFeatures"
    "fixture/EnableAllFeatures.yaml"

requestDetachPolicy :: DetachPolicy -> TestTree
requestDetachPolicy =
  req
    "DetachPolicy"
    "fixture/DetachPolicy.yaml"

requestDescribeAccount :: DescribeAccount -> TestTree
requestDescribeAccount =
  req
    "DescribeAccount"
    "fixture/DescribeAccount.yaml"

requestCreateOrganizationalUnit :: CreateOrganizationalUnit -> TestTree
requestCreateOrganizationalUnit =
  req
    "CreateOrganizationalUnit"
    "fixture/CreateOrganizationalUnit.yaml"

requestRegisterDelegatedAdministrator :: RegisterDelegatedAdministrator -> TestTree
requestRegisterDelegatedAdministrator =
  req
    "RegisterDelegatedAdministrator"
    "fixture/RegisterDelegatedAdministrator.yaml"

requestCancelHandshake :: CancelHandshake -> TestTree
requestCancelHandshake =
  req
    "CancelHandshake"
    "fixture/CancelHandshake.yaml"

requestDeleteOrganizationalUnit :: DeleteOrganizationalUnit -> TestTree
requestDeleteOrganizationalUnit =
  req
    "DeleteOrganizationalUnit"
    "fixture/DeleteOrganizationalUnit.yaml"

requestListDelegatedAdministrators :: ListDelegatedAdministrators -> TestTree
requestListDelegatedAdministrators =
  req
    "ListDelegatedAdministrators"
    "fixture/ListDelegatedAdministrators.yaml"

requestUpdateOrganizationalUnit :: UpdateOrganizationalUnit -> TestTree
requestUpdateOrganizationalUnit =
  req
    "UpdateOrganizationalUnit"
    "fixture/UpdateOrganizationalUnit.yaml"

requestListOrganizationalUnitsForParent :: ListOrganizationalUnitsForParent -> TestTree
requestListOrganizationalUnitsForParent =
  req
    "ListOrganizationalUnitsForParent"
    "fixture/ListOrganizationalUnitsForParent.yaml"

requestListPoliciesForTarget :: ListPoliciesForTarget -> TestTree
requestListPoliciesForTarget =
  req
    "ListPoliciesForTarget"
    "fixture/ListPoliciesForTarget.yaml"

requestListHandshakesForOrganization :: ListHandshakesForOrganization -> TestTree
requestListHandshakesForOrganization =
  req
    "ListHandshakesForOrganization"
    "fixture/ListHandshakesForOrganization.yaml"

requestAttachPolicy :: AttachPolicy -> TestTree
requestAttachPolicy =
  req
    "AttachPolicy"
    "fixture/AttachPolicy.yaml"

requestDeclineHandshake :: DeclineHandshake -> TestTree
requestDeclineHandshake =
  req
    "DeclineHandshake"
    "fixture/DeclineHandshake.yaml"

requestListTargetsForPolicy :: ListTargetsForPolicy -> TestTree
requestListTargetsForPolicy =
  req
    "ListTargetsForPolicy"
    "fixture/ListTargetsForPolicy.yaml"

requestDescribeEffectivePolicy :: DescribeEffectivePolicy -> TestTree
requestDescribeEffectivePolicy =
  req
    "DescribeEffectivePolicy"
    "fixture/DescribeEffectivePolicy.yaml"

requestInviteAccountToOrganization :: InviteAccountToOrganization -> TestTree
requestInviteAccountToOrganization =
  req
    "InviteAccountToOrganization"
    "fixture/InviteAccountToOrganization.yaml"

requestDisableAWSServiceAccess :: DisableAWSServiceAccess -> TestTree
requestDisableAWSServiceAccess =
  req
    "DisableAWSServiceAccess"
    "fixture/DisableAWSServiceAccess.yaml"

requestMoveAccount :: MoveAccount -> TestTree
requestMoveAccount =
  req
    "MoveAccount"
    "fixture/MoveAccount.yaml"

requestLeaveOrganization :: LeaveOrganization -> TestTree
requestLeaveOrganization =
  req
    "LeaveOrganization"
    "fixture/LeaveOrganization.yaml"

requestDeregisterDelegatedAdministrator :: DeregisterDelegatedAdministrator -> TestTree
requestDeregisterDelegatedAdministrator =
  req
    "DeregisterDelegatedAdministrator"
    "fixture/DeregisterDelegatedAdministrator.yaml"

requestCreateAccount :: CreateAccount -> TestTree
requestCreateAccount =
  req
    "CreateAccount"
    "fixture/CreateAccount.yaml"

requestListParents :: ListParents -> TestTree
requestListParents =
  req
    "ListParents"
    "fixture/ListParents.yaml"

requestDescribePolicy :: DescribePolicy -> TestTree
requestDescribePolicy =
  req
    "DescribePolicy"
    "fixture/DescribePolicy.yaml"

requestDescribeHandshake :: DescribeHandshake -> TestTree
requestDescribeHandshake =
  req
    "DescribeHandshake"
    "fixture/DescribeHandshake.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

-- Responses

responseListAccountsForParent :: ListAccountsForParentResponse -> TestTree
responseListAccountsForParent =
  res
    "ListAccountsForParentResponse"
    "fixture/ListAccountsForParentResponse.proto"
    defaultService
    (Proxy :: Proxy ListAccountsForParent)

responseCreateOrganization :: CreateOrganizationResponse -> TestTree
responseCreateOrganization =
  res
    "CreateOrganizationResponse"
    "fixture/CreateOrganizationResponse.proto"
    defaultService
    (Proxy :: Proxy CreateOrganization)

responseDescribeCreateAccountStatus :: DescribeCreateAccountStatusResponse -> TestTree
responseDescribeCreateAccountStatus =
  res
    "DescribeCreateAccountStatusResponse"
    "fixture/DescribeCreateAccountStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCreateAccountStatus)

responseDisablePolicyType :: DisablePolicyTypeResponse -> TestTree
responseDisablePolicyType =
  res
    "DisablePolicyTypeResponse"
    "fixture/DisablePolicyTypeResponse.proto"
    defaultService
    (Proxy :: Proxy DisablePolicyType)

responseListPolicies :: ListPoliciesResponse -> TestTree
responseListPolicies =
  res
    "ListPoliciesResponse"
    "fixture/ListPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy ListPolicies)

responseCreatePolicy :: CreatePolicyResponse -> TestTree
responseCreatePolicy =
  res
    "CreatePolicyResponse"
    "fixture/CreatePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePolicy)

responseListHandshakesForAccount :: ListHandshakesForAccountResponse -> TestTree
responseListHandshakesForAccount =
  res
    "ListHandshakesForAccountResponse"
    "fixture/ListHandshakesForAccountResponse.proto"
    defaultService
    (Proxy :: Proxy ListHandshakesForAccount)

responseUpdatePolicy :: UpdatePolicyResponse -> TestTree
responseUpdatePolicy =
  res
    "UpdatePolicyResponse"
    "fixture/UpdatePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePolicy)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy =
  res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePolicy)

responseEnablePolicyType :: EnablePolicyTypeResponse -> TestTree
responseEnablePolicyType =
  res
    "EnablePolicyTypeResponse"
    "fixture/EnablePolicyTypeResponse.proto"
    defaultService
    (Proxy :: Proxy EnablePolicyType)

responseListChildren :: ListChildrenResponse -> TestTree
responseListChildren =
  res
    "ListChildrenResponse"
    "fixture/ListChildrenResponse.proto"
    defaultService
    (Proxy :: Proxy ListChildren)

responseListAWSServiceAccessForOrganization :: ListAWSServiceAccessForOrganizationResponse -> TestTree
responseListAWSServiceAccessForOrganization =
  res
    "ListAWSServiceAccessForOrganizationResponse"
    "fixture/ListAWSServiceAccessForOrganizationResponse.proto"
    defaultService
    (Proxy :: Proxy ListAWSServiceAccessForOrganization)

responseDescribeOrganization :: DescribeOrganizationResponse -> TestTree
responseDescribeOrganization =
  res
    "DescribeOrganizationResponse"
    "fixture/DescribeOrganizationResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeOrganization)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseListCreateAccountStatus :: ListCreateAccountStatusResponse -> TestTree
responseListCreateAccountStatus =
  res
    "ListCreateAccountStatusResponse"
    "fixture/ListCreateAccountStatusResponse.proto"
    defaultService
    (Proxy :: Proxy ListCreateAccountStatus)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseListAccounts :: ListAccountsResponse -> TestTree
responseListAccounts =
  res
    "ListAccountsResponse"
    "fixture/ListAccountsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAccounts)

responseDescribeOrganizationalUnit :: DescribeOrganizationalUnitResponse -> TestTree
responseDescribeOrganizationalUnit =
  res
    "DescribeOrganizationalUnitResponse"
    "fixture/DescribeOrganizationalUnitResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeOrganizationalUnit)

responseListDelegatedServicesForAccount :: ListDelegatedServicesForAccountResponse -> TestTree
responseListDelegatedServicesForAccount =
  res
    "ListDelegatedServicesForAccountResponse"
    "fixture/ListDelegatedServicesForAccountResponse.proto"
    defaultService
    (Proxy :: Proxy ListDelegatedServicesForAccount)

responseEnableAWSServiceAccess :: EnableAWSServiceAccessResponse -> TestTree
responseEnableAWSServiceAccess =
  res
    "EnableAWSServiceAccessResponse"
    "fixture/EnableAWSServiceAccessResponse.proto"
    defaultService
    (Proxy :: Proxy EnableAWSServiceAccess)

responseCreateGovCloudAccount :: CreateGovCloudAccountResponse -> TestTree
responseCreateGovCloudAccount =
  res
    "CreateGovCloudAccountResponse"
    "fixture/CreateGovCloudAccountResponse.proto"
    defaultService
    (Proxy :: Proxy CreateGovCloudAccount)

responseDeleteOrganization :: DeleteOrganizationResponse -> TestTree
responseDeleteOrganization =
  res
    "DeleteOrganizationResponse"
    "fixture/DeleteOrganizationResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteOrganization)

responseRemoveAccountFromOrganization :: RemoveAccountFromOrganizationResponse -> TestTree
responseRemoveAccountFromOrganization =
  res
    "RemoveAccountFromOrganizationResponse"
    "fixture/RemoveAccountFromOrganizationResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveAccountFromOrganization)

responseAcceptHandshake :: AcceptHandshakeResponse -> TestTree
responseAcceptHandshake =
  res
    "AcceptHandshakeResponse"
    "fixture/AcceptHandshakeResponse.proto"
    defaultService
    (Proxy :: Proxy AcceptHandshake)

responseListRoots :: ListRootsResponse -> TestTree
responseListRoots =
  res
    "ListRootsResponse"
    "fixture/ListRootsResponse.proto"
    defaultService
    (Proxy :: Proxy ListRoots)

responseEnableAllFeatures :: EnableAllFeaturesResponse -> TestTree
responseEnableAllFeatures =
  res
    "EnableAllFeaturesResponse"
    "fixture/EnableAllFeaturesResponse.proto"
    defaultService
    (Proxy :: Proxy EnableAllFeatures)

responseDetachPolicy :: DetachPolicyResponse -> TestTree
responseDetachPolicy =
  res
    "DetachPolicyResponse"
    "fixture/DetachPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DetachPolicy)

responseDescribeAccount :: DescribeAccountResponse -> TestTree
responseDescribeAccount =
  res
    "DescribeAccountResponse"
    "fixture/DescribeAccountResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAccount)

responseCreateOrganizationalUnit :: CreateOrganizationalUnitResponse -> TestTree
responseCreateOrganizationalUnit =
  res
    "CreateOrganizationalUnitResponse"
    "fixture/CreateOrganizationalUnitResponse.proto"
    defaultService
    (Proxy :: Proxy CreateOrganizationalUnit)

responseRegisterDelegatedAdministrator :: RegisterDelegatedAdministratorResponse -> TestTree
responseRegisterDelegatedAdministrator =
  res
    "RegisterDelegatedAdministratorResponse"
    "fixture/RegisterDelegatedAdministratorResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterDelegatedAdministrator)

responseCancelHandshake :: CancelHandshakeResponse -> TestTree
responseCancelHandshake =
  res
    "CancelHandshakeResponse"
    "fixture/CancelHandshakeResponse.proto"
    defaultService
    (Proxy :: Proxy CancelHandshake)

responseDeleteOrganizationalUnit :: DeleteOrganizationalUnitResponse -> TestTree
responseDeleteOrganizationalUnit =
  res
    "DeleteOrganizationalUnitResponse"
    "fixture/DeleteOrganizationalUnitResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteOrganizationalUnit)

responseListDelegatedAdministrators :: ListDelegatedAdministratorsResponse -> TestTree
responseListDelegatedAdministrators =
  res
    "ListDelegatedAdministratorsResponse"
    "fixture/ListDelegatedAdministratorsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDelegatedAdministrators)

responseUpdateOrganizationalUnit :: UpdateOrganizationalUnitResponse -> TestTree
responseUpdateOrganizationalUnit =
  res
    "UpdateOrganizationalUnitResponse"
    "fixture/UpdateOrganizationalUnitResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateOrganizationalUnit)

responseListOrganizationalUnitsForParent :: ListOrganizationalUnitsForParentResponse -> TestTree
responseListOrganizationalUnitsForParent =
  res
    "ListOrganizationalUnitsForParentResponse"
    "fixture/ListOrganizationalUnitsForParentResponse.proto"
    defaultService
    (Proxy :: Proxy ListOrganizationalUnitsForParent)

responseListPoliciesForTarget :: ListPoliciesForTargetResponse -> TestTree
responseListPoliciesForTarget =
  res
    "ListPoliciesForTargetResponse"
    "fixture/ListPoliciesForTargetResponse.proto"
    defaultService
    (Proxy :: Proxy ListPoliciesForTarget)

responseListHandshakesForOrganization :: ListHandshakesForOrganizationResponse -> TestTree
responseListHandshakesForOrganization =
  res
    "ListHandshakesForOrganizationResponse"
    "fixture/ListHandshakesForOrganizationResponse.proto"
    defaultService
    (Proxy :: Proxy ListHandshakesForOrganization)

responseAttachPolicy :: AttachPolicyResponse -> TestTree
responseAttachPolicy =
  res
    "AttachPolicyResponse"
    "fixture/AttachPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy AttachPolicy)

responseDeclineHandshake :: DeclineHandshakeResponse -> TestTree
responseDeclineHandshake =
  res
    "DeclineHandshakeResponse"
    "fixture/DeclineHandshakeResponse.proto"
    defaultService
    (Proxy :: Proxy DeclineHandshake)

responseListTargetsForPolicy :: ListTargetsForPolicyResponse -> TestTree
responseListTargetsForPolicy =
  res
    "ListTargetsForPolicyResponse"
    "fixture/ListTargetsForPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy ListTargetsForPolicy)

responseDescribeEffectivePolicy :: DescribeEffectivePolicyResponse -> TestTree
responseDescribeEffectivePolicy =
  res
    "DescribeEffectivePolicyResponse"
    "fixture/DescribeEffectivePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEffectivePolicy)

responseInviteAccountToOrganization :: InviteAccountToOrganizationResponse -> TestTree
responseInviteAccountToOrganization =
  res
    "InviteAccountToOrganizationResponse"
    "fixture/InviteAccountToOrganizationResponse.proto"
    defaultService
    (Proxy :: Proxy InviteAccountToOrganization)

responseDisableAWSServiceAccess :: DisableAWSServiceAccessResponse -> TestTree
responseDisableAWSServiceAccess =
  res
    "DisableAWSServiceAccessResponse"
    "fixture/DisableAWSServiceAccessResponse.proto"
    defaultService
    (Proxy :: Proxy DisableAWSServiceAccess)

responseMoveAccount :: MoveAccountResponse -> TestTree
responseMoveAccount =
  res
    "MoveAccountResponse"
    "fixture/MoveAccountResponse.proto"
    defaultService
    (Proxy :: Proxy MoveAccount)

responseLeaveOrganization :: LeaveOrganizationResponse -> TestTree
responseLeaveOrganization =
  res
    "LeaveOrganizationResponse"
    "fixture/LeaveOrganizationResponse.proto"
    defaultService
    (Proxy :: Proxy LeaveOrganization)

responseDeregisterDelegatedAdministrator :: DeregisterDelegatedAdministratorResponse -> TestTree
responseDeregisterDelegatedAdministrator =
  res
    "DeregisterDelegatedAdministratorResponse"
    "fixture/DeregisterDelegatedAdministratorResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterDelegatedAdministrator)

responseCreateAccount :: CreateAccountResponse -> TestTree
responseCreateAccount =
  res
    "CreateAccountResponse"
    "fixture/CreateAccountResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAccount)

responseListParents :: ListParentsResponse -> TestTree
responseListParents =
  res
    "ListParentsResponse"
    "fixture/ListParentsResponse.proto"
    defaultService
    (Proxy :: Proxy ListParents)

responseDescribePolicy :: DescribePolicyResponse -> TestTree
responseDescribePolicy =
  res
    "DescribePolicyResponse"
    "fixture/DescribePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePolicy)

responseDescribeHandshake :: DescribeHandshakeResponse -> TestTree
responseDescribeHandshake =
  res
    "DescribeHandshakeResponse"
    "fixture/DescribeHandshakeResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeHandshake)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)
