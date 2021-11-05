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

import qualified Data.Proxy as Proxy
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
--         [ requestListHandshakesForAccount $
--             newListHandshakesForAccount
--
--         , requestDescribeAccount $
--             newDescribeAccount
--
--         , requestListPolicies $
--             newListPolicies
--
--         , requestCreatePolicy $
--             newCreatePolicy
--
--         , requestListRoots $
--             newListRoots
--
--         , requestAcceptHandshake $
--             newAcceptHandshake
--
--         , requestCreateOrganization $
--             newCreateOrganization
--
--         , requestEnableAllFeatures $
--             newEnableAllFeatures
--
--         , requestDeleteOrganization $
--             newDeleteOrganization
--
--         , requestDescribeHandshake $
--             newDescribeHandshake
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDescribePolicy $
--             newDescribePolicy
--
--         , requestListDelegatedServicesForAccount $
--             newListDelegatedServicesForAccount
--
--         , requestDisableAWSServiceAccess $
--             newDisableAWSServiceAccess
--
--         , requestDescribeEffectivePolicy $
--             newDescribeEffectivePolicy
--
--         , requestLeaveOrganization $
--             newLeaveOrganization
--
--         , requestMoveAccount $
--             newMoveAccount
--
--         , requestListAccounts $
--             newListAccounts
--
--         , requestInviteAccountToOrganization $
--             newInviteAccountToOrganization
--
--         , requestListAWSServiceAccessForOrganization $
--             newListAWSServiceAccessForOrganization
--
--         , requestListOrganizationalUnitsForParent $
--             newListOrganizationalUnitsForParent
--
--         , requestCancelHandshake $
--             newCancelHandshake
--
--         , requestListChildren $
--             newListChildren
--
--         , requestListDelegatedAdministrators $
--             newListDelegatedAdministrators
--
--         , requestDeletePolicy $
--             newDeletePolicy
--
--         , requestUpdatePolicy $
--             newUpdatePolicy
--
--         , requestEnablePolicyType $
--             newEnablePolicyType
--
--         , requestDisablePolicyType $
--             newDisablePolicyType
--
--         , requestDescribeCreateAccountStatus $
--             newDescribeCreateAccountStatus
--
--         , requestCreateOrganizationalUnit $
--             newCreateOrganizationalUnit
--
--         , requestListAccountsForParent $
--             newListAccountsForParent
--
--         , requestDetachPolicy $
--             newDetachPolicy
--
--         , requestRemoveAccountFromOrganization $
--             newRemoveAccountFromOrganization
--
--         , requestCreateGovCloudAccount $
--             newCreateGovCloudAccount
--
--         , requestEnableAWSServiceAccess $
--             newEnableAWSServiceAccess
--
--         , requestDescribeOrganizationalUnit $
--             newDescribeOrganizationalUnit
--
--         , requestListParents $
--             newListParents
--
--         , requestCreateAccount $
--             newCreateAccount
--
--         , requestDeregisterDelegatedAdministrator $
--             newDeregisterDelegatedAdministrator
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListCreateAccountStatus $
--             newListCreateAccountStatus
--
--         , requestListTargetsForPolicy $
--             newListTargetsForPolicy
--
--         , requestDeclineHandshake $
--             newDeclineHandshake
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestAttachPolicy $
--             newAttachPolicy
--
--         , requestListPoliciesForTarget $
--             newListPoliciesForTarget
--
--         , requestDescribeOrganization $
--             newDescribeOrganization
--
--         , requestListHandshakesForOrganization $
--             newListHandshakesForOrganization
--
--         , requestRegisterDelegatedAdministrator $
--             newRegisterDelegatedAdministrator
--
--         , requestDeleteOrganizationalUnit $
--             newDeleteOrganizationalUnit
--
--         , requestUpdateOrganizationalUnit $
--             newUpdateOrganizationalUnit
--
--           ]

--     , testGroup "response"
--         [ responseListHandshakesForAccount $
--             newListHandshakesForAccountResponse
--
--         , responseDescribeAccount $
--             newDescribeAccountResponse
--
--         , responseListPolicies $
--             newListPoliciesResponse
--
--         , responseCreatePolicy $
--             newCreatePolicyResponse
--
--         , responseListRoots $
--             newListRootsResponse
--
--         , responseAcceptHandshake $
--             newAcceptHandshakeResponse
--
--         , responseCreateOrganization $
--             newCreateOrganizationResponse
--
--         , responseEnableAllFeatures $
--             newEnableAllFeaturesResponse
--
--         , responseDeleteOrganization $
--             newDeleteOrganizationResponse
--
--         , responseDescribeHandshake $
--             newDescribeHandshakeResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDescribePolicy $
--             newDescribePolicyResponse
--
--         , responseListDelegatedServicesForAccount $
--             newListDelegatedServicesForAccountResponse
--
--         , responseDisableAWSServiceAccess $
--             newDisableAWSServiceAccessResponse
--
--         , responseDescribeEffectivePolicy $
--             newDescribeEffectivePolicyResponse
--
--         , responseLeaveOrganization $
--             newLeaveOrganizationResponse
--
--         , responseMoveAccount $
--             newMoveAccountResponse
--
--         , responseListAccounts $
--             newListAccountsResponse
--
--         , responseInviteAccountToOrganization $
--             newInviteAccountToOrganizationResponse
--
--         , responseListAWSServiceAccessForOrganization $
--             newListAWSServiceAccessForOrganizationResponse
--
--         , responseListOrganizationalUnitsForParent $
--             newListOrganizationalUnitsForParentResponse
--
--         , responseCancelHandshake $
--             newCancelHandshakeResponse
--
--         , responseListChildren $
--             newListChildrenResponse
--
--         , responseListDelegatedAdministrators $
--             newListDelegatedAdministratorsResponse
--
--         , responseDeletePolicy $
--             newDeletePolicyResponse
--
--         , responseUpdatePolicy $
--             newUpdatePolicyResponse
--
--         , responseEnablePolicyType $
--             newEnablePolicyTypeResponse
--
--         , responseDisablePolicyType $
--             newDisablePolicyTypeResponse
--
--         , responseDescribeCreateAccountStatus $
--             newDescribeCreateAccountStatusResponse
--
--         , responseCreateOrganizationalUnit $
--             newCreateOrganizationalUnitResponse
--
--         , responseListAccountsForParent $
--             newListAccountsForParentResponse
--
--         , responseDetachPolicy $
--             newDetachPolicyResponse
--
--         , responseRemoveAccountFromOrganization $
--             newRemoveAccountFromOrganizationResponse
--
--         , responseCreateGovCloudAccount $
--             newCreateGovCloudAccountResponse
--
--         , responseEnableAWSServiceAccess $
--             newEnableAWSServiceAccessResponse
--
--         , responseDescribeOrganizationalUnit $
--             newDescribeOrganizationalUnitResponse
--
--         , responseListParents $
--             newListParentsResponse
--
--         , responseCreateAccount $
--             newCreateAccountResponse
--
--         , responseDeregisterDelegatedAdministrator $
--             newDeregisterDelegatedAdministratorResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListCreateAccountStatus $
--             newListCreateAccountStatusResponse
--
--         , responseListTargetsForPolicy $
--             newListTargetsForPolicyResponse
--
--         , responseDeclineHandshake $
--             newDeclineHandshakeResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseAttachPolicy $
--             newAttachPolicyResponse
--
--         , responseListPoliciesForTarget $
--             newListPoliciesForTargetResponse
--
--         , responseDescribeOrganization $
--             newDescribeOrganizationResponse
--
--         , responseListHandshakesForOrganization $
--             newListHandshakesForOrganizationResponse
--
--         , responseRegisterDelegatedAdministrator $
--             newRegisterDelegatedAdministratorResponse
--
--         , responseDeleteOrganizationalUnit $
--             newDeleteOrganizationalUnitResponse
--
--         , responseUpdateOrganizationalUnit $
--             newUpdateOrganizationalUnitResponse
--
--           ]
--     ]

-- Requests

requestListHandshakesForAccount :: ListHandshakesForAccount -> TestTree
requestListHandshakesForAccount =
  req
    "ListHandshakesForAccount"
    "fixture/ListHandshakesForAccount.yaml"

requestDescribeAccount :: DescribeAccount -> TestTree
requestDescribeAccount =
  req
    "DescribeAccount"
    "fixture/DescribeAccount.yaml"

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

requestListRoots :: ListRoots -> TestTree
requestListRoots =
  req
    "ListRoots"
    "fixture/ListRoots.yaml"

requestAcceptHandshake :: AcceptHandshake -> TestTree
requestAcceptHandshake =
  req
    "AcceptHandshake"
    "fixture/AcceptHandshake.yaml"

requestCreateOrganization :: CreateOrganization -> TestTree
requestCreateOrganization =
  req
    "CreateOrganization"
    "fixture/CreateOrganization.yaml"

requestEnableAllFeatures :: EnableAllFeatures -> TestTree
requestEnableAllFeatures =
  req
    "EnableAllFeatures"
    "fixture/EnableAllFeatures.yaml"

requestDeleteOrganization :: DeleteOrganization -> TestTree
requestDeleteOrganization =
  req
    "DeleteOrganization"
    "fixture/DeleteOrganization.yaml"

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

requestDescribePolicy :: DescribePolicy -> TestTree
requestDescribePolicy =
  req
    "DescribePolicy"
    "fixture/DescribePolicy.yaml"

requestListDelegatedServicesForAccount :: ListDelegatedServicesForAccount -> TestTree
requestListDelegatedServicesForAccount =
  req
    "ListDelegatedServicesForAccount"
    "fixture/ListDelegatedServicesForAccount.yaml"

requestDisableAWSServiceAccess :: DisableAWSServiceAccess -> TestTree
requestDisableAWSServiceAccess =
  req
    "DisableAWSServiceAccess"
    "fixture/DisableAWSServiceAccess.yaml"

requestDescribeEffectivePolicy :: DescribeEffectivePolicy -> TestTree
requestDescribeEffectivePolicy =
  req
    "DescribeEffectivePolicy"
    "fixture/DescribeEffectivePolicy.yaml"

requestLeaveOrganization :: LeaveOrganization -> TestTree
requestLeaveOrganization =
  req
    "LeaveOrganization"
    "fixture/LeaveOrganization.yaml"

requestMoveAccount :: MoveAccount -> TestTree
requestMoveAccount =
  req
    "MoveAccount"
    "fixture/MoveAccount.yaml"

requestListAccounts :: ListAccounts -> TestTree
requestListAccounts =
  req
    "ListAccounts"
    "fixture/ListAccounts.yaml"

requestInviteAccountToOrganization :: InviteAccountToOrganization -> TestTree
requestInviteAccountToOrganization =
  req
    "InviteAccountToOrganization"
    "fixture/InviteAccountToOrganization.yaml"

requestListAWSServiceAccessForOrganization :: ListAWSServiceAccessForOrganization -> TestTree
requestListAWSServiceAccessForOrganization =
  req
    "ListAWSServiceAccessForOrganization"
    "fixture/ListAWSServiceAccessForOrganization.yaml"

requestListOrganizationalUnitsForParent :: ListOrganizationalUnitsForParent -> TestTree
requestListOrganizationalUnitsForParent =
  req
    "ListOrganizationalUnitsForParent"
    "fixture/ListOrganizationalUnitsForParent.yaml"

requestCancelHandshake :: CancelHandshake -> TestTree
requestCancelHandshake =
  req
    "CancelHandshake"
    "fixture/CancelHandshake.yaml"

requestListChildren :: ListChildren -> TestTree
requestListChildren =
  req
    "ListChildren"
    "fixture/ListChildren.yaml"

requestListDelegatedAdministrators :: ListDelegatedAdministrators -> TestTree
requestListDelegatedAdministrators =
  req
    "ListDelegatedAdministrators"
    "fixture/ListDelegatedAdministrators.yaml"

requestDeletePolicy :: DeletePolicy -> TestTree
requestDeletePolicy =
  req
    "DeletePolicy"
    "fixture/DeletePolicy.yaml"

requestUpdatePolicy :: UpdatePolicy -> TestTree
requestUpdatePolicy =
  req
    "UpdatePolicy"
    "fixture/UpdatePolicy.yaml"

requestEnablePolicyType :: EnablePolicyType -> TestTree
requestEnablePolicyType =
  req
    "EnablePolicyType"
    "fixture/EnablePolicyType.yaml"

requestDisablePolicyType :: DisablePolicyType -> TestTree
requestDisablePolicyType =
  req
    "DisablePolicyType"
    "fixture/DisablePolicyType.yaml"

requestDescribeCreateAccountStatus :: DescribeCreateAccountStatus -> TestTree
requestDescribeCreateAccountStatus =
  req
    "DescribeCreateAccountStatus"
    "fixture/DescribeCreateAccountStatus.yaml"

requestCreateOrganizationalUnit :: CreateOrganizationalUnit -> TestTree
requestCreateOrganizationalUnit =
  req
    "CreateOrganizationalUnit"
    "fixture/CreateOrganizationalUnit.yaml"

requestListAccountsForParent :: ListAccountsForParent -> TestTree
requestListAccountsForParent =
  req
    "ListAccountsForParent"
    "fixture/ListAccountsForParent.yaml"

requestDetachPolicy :: DetachPolicy -> TestTree
requestDetachPolicy =
  req
    "DetachPolicy"
    "fixture/DetachPolicy.yaml"

requestRemoveAccountFromOrganization :: RemoveAccountFromOrganization -> TestTree
requestRemoveAccountFromOrganization =
  req
    "RemoveAccountFromOrganization"
    "fixture/RemoveAccountFromOrganization.yaml"

requestCreateGovCloudAccount :: CreateGovCloudAccount -> TestTree
requestCreateGovCloudAccount =
  req
    "CreateGovCloudAccount"
    "fixture/CreateGovCloudAccount.yaml"

requestEnableAWSServiceAccess :: EnableAWSServiceAccess -> TestTree
requestEnableAWSServiceAccess =
  req
    "EnableAWSServiceAccess"
    "fixture/EnableAWSServiceAccess.yaml"

requestDescribeOrganizationalUnit :: DescribeOrganizationalUnit -> TestTree
requestDescribeOrganizationalUnit =
  req
    "DescribeOrganizationalUnit"
    "fixture/DescribeOrganizationalUnit.yaml"

requestListParents :: ListParents -> TestTree
requestListParents =
  req
    "ListParents"
    "fixture/ListParents.yaml"

requestCreateAccount :: CreateAccount -> TestTree
requestCreateAccount =
  req
    "CreateAccount"
    "fixture/CreateAccount.yaml"

requestDeregisterDelegatedAdministrator :: DeregisterDelegatedAdministrator -> TestTree
requestDeregisterDelegatedAdministrator =
  req
    "DeregisterDelegatedAdministrator"
    "fixture/DeregisterDelegatedAdministrator.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListCreateAccountStatus :: ListCreateAccountStatus -> TestTree
requestListCreateAccountStatus =
  req
    "ListCreateAccountStatus"
    "fixture/ListCreateAccountStatus.yaml"

requestListTargetsForPolicy :: ListTargetsForPolicy -> TestTree
requestListTargetsForPolicy =
  req
    "ListTargetsForPolicy"
    "fixture/ListTargetsForPolicy.yaml"

requestDeclineHandshake :: DeclineHandshake -> TestTree
requestDeclineHandshake =
  req
    "DeclineHandshake"
    "fixture/DeclineHandshake.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestAttachPolicy :: AttachPolicy -> TestTree
requestAttachPolicy =
  req
    "AttachPolicy"
    "fixture/AttachPolicy.yaml"

requestListPoliciesForTarget :: ListPoliciesForTarget -> TestTree
requestListPoliciesForTarget =
  req
    "ListPoliciesForTarget"
    "fixture/ListPoliciesForTarget.yaml"

requestDescribeOrganization :: DescribeOrganization -> TestTree
requestDescribeOrganization =
  req
    "DescribeOrganization"
    "fixture/DescribeOrganization.yaml"

requestListHandshakesForOrganization :: ListHandshakesForOrganization -> TestTree
requestListHandshakesForOrganization =
  req
    "ListHandshakesForOrganization"
    "fixture/ListHandshakesForOrganization.yaml"

requestRegisterDelegatedAdministrator :: RegisterDelegatedAdministrator -> TestTree
requestRegisterDelegatedAdministrator =
  req
    "RegisterDelegatedAdministrator"
    "fixture/RegisterDelegatedAdministrator.yaml"

requestDeleteOrganizationalUnit :: DeleteOrganizationalUnit -> TestTree
requestDeleteOrganizationalUnit =
  req
    "DeleteOrganizationalUnit"
    "fixture/DeleteOrganizationalUnit.yaml"

requestUpdateOrganizationalUnit :: UpdateOrganizationalUnit -> TestTree
requestUpdateOrganizationalUnit =
  req
    "UpdateOrganizationalUnit"
    "fixture/UpdateOrganizationalUnit.yaml"

-- Responses

responseListHandshakesForAccount :: ListHandshakesForAccountResponse -> TestTree
responseListHandshakesForAccount =
  res
    "ListHandshakesForAccountResponse"
    "fixture/ListHandshakesForAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHandshakesForAccount)

responseDescribeAccount :: DescribeAccountResponse -> TestTree
responseDescribeAccount =
  res
    "DescribeAccountResponse"
    "fixture/DescribeAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccount)

responseListPolicies :: ListPoliciesResponse -> TestTree
responseListPolicies =
  res
    "ListPoliciesResponse"
    "fixture/ListPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPolicies)

responseCreatePolicy :: CreatePolicyResponse -> TestTree
responseCreatePolicy =
  res
    "CreatePolicyResponse"
    "fixture/CreatePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePolicy)

responseListRoots :: ListRootsResponse -> TestTree
responseListRoots =
  res
    "ListRootsResponse"
    "fixture/ListRootsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRoots)

responseAcceptHandshake :: AcceptHandshakeResponse -> TestTree
responseAcceptHandshake =
  res
    "AcceptHandshakeResponse"
    "fixture/AcceptHandshakeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptHandshake)

responseCreateOrganization :: CreateOrganizationResponse -> TestTree
responseCreateOrganization =
  res
    "CreateOrganizationResponse"
    "fixture/CreateOrganizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateOrganization)

responseEnableAllFeatures :: EnableAllFeaturesResponse -> TestTree
responseEnableAllFeatures =
  res
    "EnableAllFeaturesResponse"
    "fixture/EnableAllFeaturesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableAllFeatures)

responseDeleteOrganization :: DeleteOrganizationResponse -> TestTree
responseDeleteOrganization =
  res
    "DeleteOrganizationResponse"
    "fixture/DeleteOrganizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteOrganization)

responseDescribeHandshake :: DescribeHandshakeResponse -> TestTree
responseDescribeHandshake =
  res
    "DescribeHandshakeResponse"
    "fixture/DescribeHandshakeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeHandshake)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseDescribePolicy :: DescribePolicyResponse -> TestTree
responseDescribePolicy =
  res
    "DescribePolicyResponse"
    "fixture/DescribePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePolicy)

responseListDelegatedServicesForAccount :: ListDelegatedServicesForAccountResponse -> TestTree
responseListDelegatedServicesForAccount =
  res
    "ListDelegatedServicesForAccountResponse"
    "fixture/ListDelegatedServicesForAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDelegatedServicesForAccount)

responseDisableAWSServiceAccess :: DisableAWSServiceAccessResponse -> TestTree
responseDisableAWSServiceAccess =
  res
    "DisableAWSServiceAccessResponse"
    "fixture/DisableAWSServiceAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableAWSServiceAccess)

responseDescribeEffectivePolicy :: DescribeEffectivePolicyResponse -> TestTree
responseDescribeEffectivePolicy =
  res
    "DescribeEffectivePolicyResponse"
    "fixture/DescribeEffectivePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEffectivePolicy)

responseLeaveOrganization :: LeaveOrganizationResponse -> TestTree
responseLeaveOrganization =
  res
    "LeaveOrganizationResponse"
    "fixture/LeaveOrganizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy LeaveOrganization)

responseMoveAccount :: MoveAccountResponse -> TestTree
responseMoveAccount =
  res
    "MoveAccountResponse"
    "fixture/MoveAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy MoveAccount)

responseListAccounts :: ListAccountsResponse -> TestTree
responseListAccounts =
  res
    "ListAccountsResponse"
    "fixture/ListAccountsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccounts)

responseInviteAccountToOrganization :: InviteAccountToOrganizationResponse -> TestTree
responseInviteAccountToOrganization =
  res
    "InviteAccountToOrganizationResponse"
    "fixture/InviteAccountToOrganizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy InviteAccountToOrganization)

responseListAWSServiceAccessForOrganization :: ListAWSServiceAccessForOrganizationResponse -> TestTree
responseListAWSServiceAccessForOrganization =
  res
    "ListAWSServiceAccessForOrganizationResponse"
    "fixture/ListAWSServiceAccessForOrganizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAWSServiceAccessForOrganization)

responseListOrganizationalUnitsForParent :: ListOrganizationalUnitsForParentResponse -> TestTree
responseListOrganizationalUnitsForParent =
  res
    "ListOrganizationalUnitsForParentResponse"
    "fixture/ListOrganizationalUnitsForParentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOrganizationalUnitsForParent)

responseCancelHandshake :: CancelHandshakeResponse -> TestTree
responseCancelHandshake =
  res
    "CancelHandshakeResponse"
    "fixture/CancelHandshakeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelHandshake)

responseListChildren :: ListChildrenResponse -> TestTree
responseListChildren =
  res
    "ListChildrenResponse"
    "fixture/ListChildrenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChildren)

responseListDelegatedAdministrators :: ListDelegatedAdministratorsResponse -> TestTree
responseListDelegatedAdministrators =
  res
    "ListDelegatedAdministratorsResponse"
    "fixture/ListDelegatedAdministratorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDelegatedAdministrators)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy =
  res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePolicy)

responseUpdatePolicy :: UpdatePolicyResponse -> TestTree
responseUpdatePolicy =
  res
    "UpdatePolicyResponse"
    "fixture/UpdatePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePolicy)

responseEnablePolicyType :: EnablePolicyTypeResponse -> TestTree
responseEnablePolicyType =
  res
    "EnablePolicyTypeResponse"
    "fixture/EnablePolicyTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnablePolicyType)

responseDisablePolicyType :: DisablePolicyTypeResponse -> TestTree
responseDisablePolicyType =
  res
    "DisablePolicyTypeResponse"
    "fixture/DisablePolicyTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisablePolicyType)

responseDescribeCreateAccountStatus :: DescribeCreateAccountStatusResponse -> TestTree
responseDescribeCreateAccountStatus =
  res
    "DescribeCreateAccountStatusResponse"
    "fixture/DescribeCreateAccountStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCreateAccountStatus)

responseCreateOrganizationalUnit :: CreateOrganizationalUnitResponse -> TestTree
responseCreateOrganizationalUnit =
  res
    "CreateOrganizationalUnitResponse"
    "fixture/CreateOrganizationalUnitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateOrganizationalUnit)

responseListAccountsForParent :: ListAccountsForParentResponse -> TestTree
responseListAccountsForParent =
  res
    "ListAccountsForParentResponse"
    "fixture/ListAccountsForParentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccountsForParent)

responseDetachPolicy :: DetachPolicyResponse -> TestTree
responseDetachPolicy =
  res
    "DetachPolicyResponse"
    "fixture/DetachPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachPolicy)

responseRemoveAccountFromOrganization :: RemoveAccountFromOrganizationResponse -> TestTree
responseRemoveAccountFromOrganization =
  res
    "RemoveAccountFromOrganizationResponse"
    "fixture/RemoveAccountFromOrganizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveAccountFromOrganization)

responseCreateGovCloudAccount :: CreateGovCloudAccountResponse -> TestTree
responseCreateGovCloudAccount =
  res
    "CreateGovCloudAccountResponse"
    "fixture/CreateGovCloudAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGovCloudAccount)

responseEnableAWSServiceAccess :: EnableAWSServiceAccessResponse -> TestTree
responseEnableAWSServiceAccess =
  res
    "EnableAWSServiceAccessResponse"
    "fixture/EnableAWSServiceAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableAWSServiceAccess)

responseDescribeOrganizationalUnit :: DescribeOrganizationalUnitResponse -> TestTree
responseDescribeOrganizationalUnit =
  res
    "DescribeOrganizationalUnitResponse"
    "fixture/DescribeOrganizationalUnitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOrganizationalUnit)

responseListParents :: ListParentsResponse -> TestTree
responseListParents =
  res
    "ListParentsResponse"
    "fixture/ListParentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListParents)

responseCreateAccount :: CreateAccountResponse -> TestTree
responseCreateAccount =
  res
    "CreateAccountResponse"
    "fixture/CreateAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAccount)

responseDeregisterDelegatedAdministrator :: DeregisterDelegatedAdministratorResponse -> TestTree
responseDeregisterDelegatedAdministrator =
  res
    "DeregisterDelegatedAdministratorResponse"
    "fixture/DeregisterDelegatedAdministratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterDelegatedAdministrator)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseListCreateAccountStatus :: ListCreateAccountStatusResponse -> TestTree
responseListCreateAccountStatus =
  res
    "ListCreateAccountStatusResponse"
    "fixture/ListCreateAccountStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCreateAccountStatus)

responseListTargetsForPolicy :: ListTargetsForPolicyResponse -> TestTree
responseListTargetsForPolicy =
  res
    "ListTargetsForPolicyResponse"
    "fixture/ListTargetsForPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTargetsForPolicy)

responseDeclineHandshake :: DeclineHandshakeResponse -> TestTree
responseDeclineHandshake =
  res
    "DeclineHandshakeResponse"
    "fixture/DeclineHandshakeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeclineHandshake)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseAttachPolicy :: AttachPolicyResponse -> TestTree
responseAttachPolicy =
  res
    "AttachPolicyResponse"
    "fixture/AttachPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachPolicy)

responseListPoliciesForTarget :: ListPoliciesForTargetResponse -> TestTree
responseListPoliciesForTarget =
  res
    "ListPoliciesForTargetResponse"
    "fixture/ListPoliciesForTargetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPoliciesForTarget)

responseDescribeOrganization :: DescribeOrganizationResponse -> TestTree
responseDescribeOrganization =
  res
    "DescribeOrganizationResponse"
    "fixture/DescribeOrganizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOrganization)

responseListHandshakesForOrganization :: ListHandshakesForOrganizationResponse -> TestTree
responseListHandshakesForOrganization =
  res
    "ListHandshakesForOrganizationResponse"
    "fixture/ListHandshakesForOrganizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHandshakesForOrganization)

responseRegisterDelegatedAdministrator :: RegisterDelegatedAdministratorResponse -> TestTree
responseRegisterDelegatedAdministrator =
  res
    "RegisterDelegatedAdministratorResponse"
    "fixture/RegisterDelegatedAdministratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterDelegatedAdministrator)

responseDeleteOrganizationalUnit :: DeleteOrganizationalUnitResponse -> TestTree
responseDeleteOrganizationalUnit =
  res
    "DeleteOrganizationalUnitResponse"
    "fixture/DeleteOrganizationalUnitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteOrganizationalUnit)

responseUpdateOrganizationalUnit :: UpdateOrganizationalUnitResponse -> TestTree
responseUpdateOrganizationalUnit =
  res
    "UpdateOrganizationalUnitResponse"
    "fixture/UpdateOrganizationalUnitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateOrganizationalUnit)
