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
--         , requestCreatePolicy $
--             newCreatePolicy
--
--         , requestDisablePolicyType $
--             newDisablePolicyType
--
--         , requestDescribeCreateAccountStatus $
--             newDescribeCreateAccountStatus
--
--         , requestListPolicies $
--             newListPolicies
--
--         , requestListHandshakesForAccount $
--             newListHandshakesForAccount
--
--         , requestListChildren $
--             newListChildren
--
--         , requestDeletePolicy $
--             newDeletePolicy
--
--         , requestEnablePolicyType $
--             newEnablePolicyType
--
--         , requestUpdatePolicy $
--             newUpdatePolicy
--
--         , requestListAWSServiceAccessForOrganization $
--             newListAWSServiceAccessForOrganization
--
--         , requestDescribeOrganization $
--             newDescribeOrganization
--
--         , requestListCreateAccountStatus $
--             newListCreateAccountStatus
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestListAccounts $
--             newListAccounts
--
--         , requestTagResource $
--             newTagResource
--
--         , requestEnableAWSServiceAccess $
--             newEnableAWSServiceAccess
--
--         , requestDescribeOrganizationalUnit $
--             newDescribeOrganizationalUnit
--
--         , requestListDelegatedServicesForAccount $
--             newListDelegatedServicesForAccount
--
--         , requestRemoveAccountFromOrganization $
--             newRemoveAccountFromOrganization
--
--         , requestCreateGovCloudAccount $
--             newCreateGovCloudAccount
--
--         , requestDeleteOrganization $
--             newDeleteOrganization
--
--         , requestListRoots $
--             newListRoots
--
--         , requestEnableAllFeatures $
--             newEnableAllFeatures
--
--         , requestAcceptHandshake $
--             newAcceptHandshake
--
--         , requestDetachPolicy $
--             newDetachPolicy
--
--         , requestCreateOrganizationalUnit $
--             newCreateOrganizationalUnit
--
--         , requestDescribeAccount $
--             newDescribeAccount
--
--         , requestListDelegatedAdministrators $
--             newListDelegatedAdministrators
--
--         , requestUpdateOrganizationalUnit $
--             newUpdateOrganizationalUnit
--
--         , requestDeleteOrganizationalUnit $
--             newDeleteOrganizationalUnit
--
--         , requestCancelHandshake $
--             newCancelHandshake
--
--         , requestRegisterDelegatedAdministrator $
--             newRegisterDelegatedAdministrator
--
--         , requestListHandshakesForOrganization $
--             newListHandshakesForOrganization
--
--         , requestListPoliciesForTarget $
--             newListPoliciesForTarget
--
--         , requestListOrganizationalUnitsForParent $
--             newListOrganizationalUnitsForParent
--
--         , requestListTargetsForPolicy $
--             newListTargetsForPolicy
--
--         , requestAttachPolicy $
--             newAttachPolicy
--
--         , requestDeclineHandshake $
--             newDeclineHandshake
--
--         , requestDescribeEffectivePolicy $
--             newDescribeEffectivePolicy
--
--         , requestDeregisterDelegatedAdministrator $
--             newDeregisterDelegatedAdministrator
--
--         , requestMoveAccount $
--             newMoveAccount
--
--         , requestInviteAccountToOrganization $
--             newInviteAccountToOrganization
--
--         , requestLeaveOrganization $
--             newLeaveOrganization
--
--         , requestDisableAWSServiceAccess $
--             newDisableAWSServiceAccess
--
--         , requestListParents $
--             newListParents
--
--         , requestDescribePolicy $
--             newDescribePolicy
--
--         , requestCreateAccount $
--             newCreateAccount
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDescribeHandshake $
--             newDescribeHandshake
--
--           ]

--     , testGroup "response"
--         [ responseListAccountsForParent $
--             newListAccountsForParentResponse
--
--         , responseCreateOrganization $
--             newCreateOrganizationResponse
--
--         , responseCreatePolicy $
--             newCreatePolicyResponse
--
--         , responseDisablePolicyType $
--             newDisablePolicyTypeResponse
--
--         , responseDescribeCreateAccountStatus $
--             newDescribeCreateAccountStatusResponse
--
--         , responseListPolicies $
--             newListPoliciesResponse
--
--         , responseListHandshakesForAccount $
--             newListHandshakesForAccountResponse
--
--         , responseListChildren $
--             newListChildrenResponse
--
--         , responseDeletePolicy $
--             newDeletePolicyResponse
--
--         , responseEnablePolicyType $
--             newEnablePolicyTypeResponse
--
--         , responseUpdatePolicy $
--             newUpdatePolicyResponse
--
--         , responseListAWSServiceAccessForOrganization $
--             newListAWSServiceAccessForOrganizationResponse
--
--         , responseDescribeOrganization $
--             newDescribeOrganizationResponse
--
--         , responseListCreateAccountStatus $
--             newListCreateAccountStatusResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseListAccounts $
--             newListAccountsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseEnableAWSServiceAccess $
--             newEnableAWSServiceAccessResponse
--
--         , responseDescribeOrganizationalUnit $
--             newDescribeOrganizationalUnitResponse
--
--         , responseListDelegatedServicesForAccount $
--             newListDelegatedServicesForAccountResponse
--
--         , responseRemoveAccountFromOrganization $
--             newRemoveAccountFromOrganizationResponse
--
--         , responseCreateGovCloudAccount $
--             newCreateGovCloudAccountResponse
--
--         , responseDeleteOrganization $
--             newDeleteOrganizationResponse
--
--         , responseListRoots $
--             newListRootsResponse
--
--         , responseEnableAllFeatures $
--             newEnableAllFeaturesResponse
--
--         , responseAcceptHandshake $
--             newAcceptHandshakeResponse
--
--         , responseDetachPolicy $
--             newDetachPolicyResponse
--
--         , responseCreateOrganizationalUnit $
--             newCreateOrganizationalUnitResponse
--
--         , responseDescribeAccount $
--             newDescribeAccountResponse
--
--         , responseListDelegatedAdministrators $
--             newListDelegatedAdministratorsResponse
--
--         , responseUpdateOrganizationalUnit $
--             newUpdateOrganizationalUnitResponse
--
--         , responseDeleteOrganizationalUnit $
--             newDeleteOrganizationalUnitResponse
--
--         , responseCancelHandshake $
--             newCancelHandshakeResponse
--
--         , responseRegisterDelegatedAdministrator $
--             newRegisterDelegatedAdministratorResponse
--
--         , responseListHandshakesForOrganization $
--             newListHandshakesForOrganizationResponse
--
--         , responseListPoliciesForTarget $
--             newListPoliciesForTargetResponse
--
--         , responseListOrganizationalUnitsForParent $
--             newListOrganizationalUnitsForParentResponse
--
--         , responseListTargetsForPolicy $
--             newListTargetsForPolicyResponse
--
--         , responseAttachPolicy $
--             newAttachPolicyResponse
--
--         , responseDeclineHandshake $
--             newDeclineHandshakeResponse
--
--         , responseDescribeEffectivePolicy $
--             newDescribeEffectivePolicyResponse
--
--         , responseDeregisterDelegatedAdministrator $
--             newDeregisterDelegatedAdministratorResponse
--
--         , responseMoveAccount $
--             newMoveAccountResponse
--
--         , responseInviteAccountToOrganization $
--             newInviteAccountToOrganizationResponse
--
--         , responseLeaveOrganization $
--             newLeaveOrganizationResponse
--
--         , responseDisableAWSServiceAccess $
--             newDisableAWSServiceAccessResponse
--
--         , responseListParents $
--             newListParentsResponse
--
--         , responseDescribePolicy $
--             newDescribePolicyResponse
--
--         , responseCreateAccount $
--             newCreateAccountResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDescribeHandshake $
--             newDescribeHandshakeResponse
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

requestCreatePolicy :: CreatePolicy -> TestTree
requestCreatePolicy =
  req
    "CreatePolicy"
    "fixture/CreatePolicy.yaml"

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

requestListPolicies :: ListPolicies -> TestTree
requestListPolicies =
  req
    "ListPolicies"
    "fixture/ListPolicies.yaml"

requestListHandshakesForAccount :: ListHandshakesForAccount -> TestTree
requestListHandshakesForAccount =
  req
    "ListHandshakesForAccount"
    "fixture/ListHandshakesForAccount.yaml"

requestListChildren :: ListChildren -> TestTree
requestListChildren =
  req
    "ListChildren"
    "fixture/ListChildren.yaml"

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

requestUpdatePolicy :: UpdatePolicy -> TestTree
requestUpdatePolicy =
  req
    "UpdatePolicy"
    "fixture/UpdatePolicy.yaml"

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

requestListCreateAccountStatus :: ListCreateAccountStatus -> TestTree
requestListCreateAccountStatus =
  req
    "ListCreateAccountStatus"
    "fixture/ListCreateAccountStatus.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestListAccounts :: ListAccounts -> TestTree
requestListAccounts =
  req
    "ListAccounts"
    "fixture/ListAccounts.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

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

requestListDelegatedServicesForAccount :: ListDelegatedServicesForAccount -> TestTree
requestListDelegatedServicesForAccount =
  req
    "ListDelegatedServicesForAccount"
    "fixture/ListDelegatedServicesForAccount.yaml"

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

requestDeleteOrganization :: DeleteOrganization -> TestTree
requestDeleteOrganization =
  req
    "DeleteOrganization"
    "fixture/DeleteOrganization.yaml"

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

requestAcceptHandshake :: AcceptHandshake -> TestTree
requestAcceptHandshake =
  req
    "AcceptHandshake"
    "fixture/AcceptHandshake.yaml"

requestDetachPolicy :: DetachPolicy -> TestTree
requestDetachPolicy =
  req
    "DetachPolicy"
    "fixture/DetachPolicy.yaml"

requestCreateOrganizationalUnit :: CreateOrganizationalUnit -> TestTree
requestCreateOrganizationalUnit =
  req
    "CreateOrganizationalUnit"
    "fixture/CreateOrganizationalUnit.yaml"

requestDescribeAccount :: DescribeAccount -> TestTree
requestDescribeAccount =
  req
    "DescribeAccount"
    "fixture/DescribeAccount.yaml"

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

requestDeleteOrganizationalUnit :: DeleteOrganizationalUnit -> TestTree
requestDeleteOrganizationalUnit =
  req
    "DeleteOrganizationalUnit"
    "fixture/DeleteOrganizationalUnit.yaml"

requestCancelHandshake :: CancelHandshake -> TestTree
requestCancelHandshake =
  req
    "CancelHandshake"
    "fixture/CancelHandshake.yaml"

requestRegisterDelegatedAdministrator :: RegisterDelegatedAdministrator -> TestTree
requestRegisterDelegatedAdministrator =
  req
    "RegisterDelegatedAdministrator"
    "fixture/RegisterDelegatedAdministrator.yaml"

requestListHandshakesForOrganization :: ListHandshakesForOrganization -> TestTree
requestListHandshakesForOrganization =
  req
    "ListHandshakesForOrganization"
    "fixture/ListHandshakesForOrganization.yaml"

requestListPoliciesForTarget :: ListPoliciesForTarget -> TestTree
requestListPoliciesForTarget =
  req
    "ListPoliciesForTarget"
    "fixture/ListPoliciesForTarget.yaml"

requestListOrganizationalUnitsForParent :: ListOrganizationalUnitsForParent -> TestTree
requestListOrganizationalUnitsForParent =
  req
    "ListOrganizationalUnitsForParent"
    "fixture/ListOrganizationalUnitsForParent.yaml"

requestListTargetsForPolicy :: ListTargetsForPolicy -> TestTree
requestListTargetsForPolicy =
  req
    "ListTargetsForPolicy"
    "fixture/ListTargetsForPolicy.yaml"

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

requestDescribeEffectivePolicy :: DescribeEffectivePolicy -> TestTree
requestDescribeEffectivePolicy =
  req
    "DescribeEffectivePolicy"
    "fixture/DescribeEffectivePolicy.yaml"

requestDeregisterDelegatedAdministrator :: DeregisterDelegatedAdministrator -> TestTree
requestDeregisterDelegatedAdministrator =
  req
    "DeregisterDelegatedAdministrator"
    "fixture/DeregisterDelegatedAdministrator.yaml"

requestMoveAccount :: MoveAccount -> TestTree
requestMoveAccount =
  req
    "MoveAccount"
    "fixture/MoveAccount.yaml"

requestInviteAccountToOrganization :: InviteAccountToOrganization -> TestTree
requestInviteAccountToOrganization =
  req
    "InviteAccountToOrganization"
    "fixture/InviteAccountToOrganization.yaml"

requestLeaveOrganization :: LeaveOrganization -> TestTree
requestLeaveOrganization =
  req
    "LeaveOrganization"
    "fixture/LeaveOrganization.yaml"

requestDisableAWSServiceAccess :: DisableAWSServiceAccess -> TestTree
requestDisableAWSServiceAccess =
  req
    "DisableAWSServiceAccess"
    "fixture/DisableAWSServiceAccess.yaml"

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

requestCreateAccount :: CreateAccount -> TestTree
requestCreateAccount =
  req
    "CreateAccount"
    "fixture/CreateAccount.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDescribeHandshake :: DescribeHandshake -> TestTree
requestDescribeHandshake =
  req
    "DescribeHandshake"
    "fixture/DescribeHandshake.yaml"

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

responseCreatePolicy :: CreatePolicyResponse -> TestTree
responseCreatePolicy =
  res
    "CreatePolicyResponse"
    "fixture/CreatePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePolicy)

responseDisablePolicyType :: DisablePolicyTypeResponse -> TestTree
responseDisablePolicyType =
  res
    "DisablePolicyTypeResponse"
    "fixture/DisablePolicyTypeResponse.proto"
    defaultService
    (Proxy :: Proxy DisablePolicyType)

responseDescribeCreateAccountStatus :: DescribeCreateAccountStatusResponse -> TestTree
responseDescribeCreateAccountStatus =
  res
    "DescribeCreateAccountStatusResponse"
    "fixture/DescribeCreateAccountStatusResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeCreateAccountStatus)

responseListPolicies :: ListPoliciesResponse -> TestTree
responseListPolicies =
  res
    "ListPoliciesResponse"
    "fixture/ListPoliciesResponse.proto"
    defaultService
    (Proxy :: Proxy ListPolicies)

responseListHandshakesForAccount :: ListHandshakesForAccountResponse -> TestTree
responseListHandshakesForAccount =
  res
    "ListHandshakesForAccountResponse"
    "fixture/ListHandshakesForAccountResponse.proto"
    defaultService
    (Proxy :: Proxy ListHandshakesForAccount)

responseListChildren :: ListChildrenResponse -> TestTree
responseListChildren =
  res
    "ListChildrenResponse"
    "fixture/ListChildrenResponse.proto"
    defaultService
    (Proxy :: Proxy ListChildren)

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

responseUpdatePolicy :: UpdatePolicyResponse -> TestTree
responseUpdatePolicy =
  res
    "UpdatePolicyResponse"
    "fixture/UpdatePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy UpdatePolicy)

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

responseListCreateAccountStatus :: ListCreateAccountStatusResponse -> TestTree
responseListCreateAccountStatus =
  res
    "ListCreateAccountStatusResponse"
    "fixture/ListCreateAccountStatusResponse.proto"
    defaultService
    (Proxy :: Proxy ListCreateAccountStatus)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseListAccounts :: ListAccountsResponse -> TestTree
responseListAccounts =
  res
    "ListAccountsResponse"
    "fixture/ListAccountsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAccounts)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseEnableAWSServiceAccess :: EnableAWSServiceAccessResponse -> TestTree
responseEnableAWSServiceAccess =
  res
    "EnableAWSServiceAccessResponse"
    "fixture/EnableAWSServiceAccessResponse.proto"
    defaultService
    (Proxy :: Proxy EnableAWSServiceAccess)

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

responseRemoveAccountFromOrganization :: RemoveAccountFromOrganizationResponse -> TestTree
responseRemoveAccountFromOrganization =
  res
    "RemoveAccountFromOrganizationResponse"
    "fixture/RemoveAccountFromOrganizationResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveAccountFromOrganization)

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

responseAcceptHandshake :: AcceptHandshakeResponse -> TestTree
responseAcceptHandshake =
  res
    "AcceptHandshakeResponse"
    "fixture/AcceptHandshakeResponse.proto"
    defaultService
    (Proxy :: Proxy AcceptHandshake)

responseDetachPolicy :: DetachPolicyResponse -> TestTree
responseDetachPolicy =
  res
    "DetachPolicyResponse"
    "fixture/DetachPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DetachPolicy)

responseCreateOrganizationalUnit :: CreateOrganizationalUnitResponse -> TestTree
responseCreateOrganizationalUnit =
  res
    "CreateOrganizationalUnitResponse"
    "fixture/CreateOrganizationalUnitResponse.proto"
    defaultService
    (Proxy :: Proxy CreateOrganizationalUnit)

responseDescribeAccount :: DescribeAccountResponse -> TestTree
responseDescribeAccount =
  res
    "DescribeAccountResponse"
    "fixture/DescribeAccountResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeAccount)

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

responseDeleteOrganizationalUnit :: DeleteOrganizationalUnitResponse -> TestTree
responseDeleteOrganizationalUnit =
  res
    "DeleteOrganizationalUnitResponse"
    "fixture/DeleteOrganizationalUnitResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteOrganizationalUnit)

responseCancelHandshake :: CancelHandshakeResponse -> TestTree
responseCancelHandshake =
  res
    "CancelHandshakeResponse"
    "fixture/CancelHandshakeResponse.proto"
    defaultService
    (Proxy :: Proxy CancelHandshake)

responseRegisterDelegatedAdministrator :: RegisterDelegatedAdministratorResponse -> TestTree
responseRegisterDelegatedAdministrator =
  res
    "RegisterDelegatedAdministratorResponse"
    "fixture/RegisterDelegatedAdministratorResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterDelegatedAdministrator)

responseListHandshakesForOrganization :: ListHandshakesForOrganizationResponse -> TestTree
responseListHandshakesForOrganization =
  res
    "ListHandshakesForOrganizationResponse"
    "fixture/ListHandshakesForOrganizationResponse.proto"
    defaultService
    (Proxy :: Proxy ListHandshakesForOrganization)

responseListPoliciesForTarget :: ListPoliciesForTargetResponse -> TestTree
responseListPoliciesForTarget =
  res
    "ListPoliciesForTargetResponse"
    "fixture/ListPoliciesForTargetResponse.proto"
    defaultService
    (Proxy :: Proxy ListPoliciesForTarget)

responseListOrganizationalUnitsForParent :: ListOrganizationalUnitsForParentResponse -> TestTree
responseListOrganizationalUnitsForParent =
  res
    "ListOrganizationalUnitsForParentResponse"
    "fixture/ListOrganizationalUnitsForParentResponse.proto"
    defaultService
    (Proxy :: Proxy ListOrganizationalUnitsForParent)

responseListTargetsForPolicy :: ListTargetsForPolicyResponse -> TestTree
responseListTargetsForPolicy =
  res
    "ListTargetsForPolicyResponse"
    "fixture/ListTargetsForPolicyResponse.proto"
    defaultService
    (Proxy :: Proxy ListTargetsForPolicy)

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

responseDescribeEffectivePolicy :: DescribeEffectivePolicyResponse -> TestTree
responseDescribeEffectivePolicy =
  res
    "DescribeEffectivePolicyResponse"
    "fixture/DescribeEffectivePolicyResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeEffectivePolicy)

responseDeregisterDelegatedAdministrator :: DeregisterDelegatedAdministratorResponse -> TestTree
responseDeregisterDelegatedAdministrator =
  res
    "DeregisterDelegatedAdministratorResponse"
    "fixture/DeregisterDelegatedAdministratorResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterDelegatedAdministrator)

responseMoveAccount :: MoveAccountResponse -> TestTree
responseMoveAccount =
  res
    "MoveAccountResponse"
    "fixture/MoveAccountResponse.proto"
    defaultService
    (Proxy :: Proxy MoveAccount)

responseInviteAccountToOrganization :: InviteAccountToOrganizationResponse -> TestTree
responseInviteAccountToOrganization =
  res
    "InviteAccountToOrganizationResponse"
    "fixture/InviteAccountToOrganizationResponse.proto"
    defaultService
    (Proxy :: Proxy InviteAccountToOrganization)

responseLeaveOrganization :: LeaveOrganizationResponse -> TestTree
responseLeaveOrganization =
  res
    "LeaveOrganizationResponse"
    "fixture/LeaveOrganizationResponse.proto"
    defaultService
    (Proxy :: Proxy LeaveOrganization)

responseDisableAWSServiceAccess :: DisableAWSServiceAccessResponse -> TestTree
responseDisableAWSServiceAccess =
  res
    "DisableAWSServiceAccessResponse"
    "fixture/DisableAWSServiceAccessResponse.proto"
    defaultService
    (Proxy :: Proxy DisableAWSServiceAccess)

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

responseCreateAccount :: CreateAccountResponse -> TestTree
responseCreateAccount =
  res
    "CreateAccountResponse"
    "fixture/CreateAccountResponse.proto"
    defaultService
    (Proxy :: Proxy CreateAccount)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseDescribeHandshake :: DescribeHandshakeResponse -> TestTree
responseDescribeHandshake =
  res
    "DescribeHandshakeResponse"
    "fixture/DescribeHandshakeResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeHandshake)
