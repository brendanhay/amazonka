{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Organizations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Organizations where

import Amazonka.Organizations
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Organizations.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAcceptHandshake $
--             newAcceptHandshake
--
--         , requestAttachPolicy $
--             newAttachPolicy
--
--         , requestCancelHandshake $
--             newCancelHandshake
--
--         , requestCloseAccount $
--             newCloseAccount
--
--         , requestCreateAccount $
--             newCreateAccount
--
--         , requestCreateGovCloudAccount $
--             newCreateGovCloudAccount
--
--         , requestCreateOrganization $
--             newCreateOrganization
--
--         , requestCreateOrganizationalUnit $
--             newCreateOrganizationalUnit
--
--         , requestCreatePolicy $
--             newCreatePolicy
--
--         , requestDeclineHandshake $
--             newDeclineHandshake
--
--         , requestDeleteOrganization $
--             newDeleteOrganization
--
--         , requestDeleteOrganizationalUnit $
--             newDeleteOrganizationalUnit
--
--         , requestDeletePolicy $
--             newDeletePolicy
--
--         , requestDeleteResourcePolicy $
--             newDeleteResourcePolicy
--
--         , requestDeregisterDelegatedAdministrator $
--             newDeregisterDelegatedAdministrator
--
--         , requestDescribeAccount $
--             newDescribeAccount
--
--         , requestDescribeCreateAccountStatus $
--             newDescribeCreateAccountStatus
--
--         , requestDescribeEffectivePolicy $
--             newDescribeEffectivePolicy
--
--         , requestDescribeHandshake $
--             newDescribeHandshake
--
--         , requestDescribeOrganization $
--             newDescribeOrganization
--
--         , requestDescribeOrganizationalUnit $
--             newDescribeOrganizationalUnit
--
--         , requestDescribePolicy $
--             newDescribePolicy
--
--         , requestDescribeResourcePolicy $
--             newDescribeResourcePolicy
--
--         , requestDetachPolicy $
--             newDetachPolicy
--
--         , requestDisableAWSServiceAccess $
--             newDisableAWSServiceAccess
--
--         , requestDisablePolicyType $
--             newDisablePolicyType
--
--         , requestEnableAWSServiceAccess $
--             newEnableAWSServiceAccess
--
--         , requestEnableAllFeatures $
--             newEnableAllFeatures
--
--         , requestEnablePolicyType $
--             newEnablePolicyType
--
--         , requestInviteAccountToOrganization $
--             newInviteAccountToOrganization
--
--         , requestLeaveOrganization $
--             newLeaveOrganization
--
--         , requestListAWSServiceAccessForOrganization $
--             newListAWSServiceAccessForOrganization
--
--         , requestListAccounts $
--             newListAccounts
--
--         , requestListAccountsForParent $
--             newListAccountsForParent
--
--         , requestListChildren $
--             newListChildren
--
--         , requestListCreateAccountStatus $
--             newListCreateAccountStatus
--
--         , requestListDelegatedAdministrators $
--             newListDelegatedAdministrators
--
--         , requestListDelegatedServicesForAccount $
--             newListDelegatedServicesForAccount
--
--         , requestListHandshakesForAccount $
--             newListHandshakesForAccount
--
--         , requestListHandshakesForOrganization $
--             newListHandshakesForOrganization
--
--         , requestListOrganizationalUnitsForParent $
--             newListOrganizationalUnitsForParent
--
--         , requestListParents $
--             newListParents
--
--         , requestListPolicies $
--             newListPolicies
--
--         , requestListPoliciesForTarget $
--             newListPoliciesForTarget
--
--         , requestListRoots $
--             newListRoots
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestListTargetsForPolicy $
--             newListTargetsForPolicy
--
--         , requestMoveAccount $
--             newMoveAccount
--
--         , requestPutResourcePolicy $
--             newPutResourcePolicy
--
--         , requestRegisterDelegatedAdministrator $
--             newRegisterDelegatedAdministrator
--
--         , requestRemoveAccountFromOrganization $
--             newRemoveAccountFromOrganization
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateOrganizationalUnit $
--             newUpdateOrganizationalUnit
--
--         , requestUpdatePolicy $
--             newUpdatePolicy
--
--           ]

--     , testGroup "response"
--         [ responseAcceptHandshake $
--             newAcceptHandshakeResponse
--
--         , responseAttachPolicy $
--             newAttachPolicyResponse
--
--         , responseCancelHandshake $
--             newCancelHandshakeResponse
--
--         , responseCloseAccount $
--             newCloseAccountResponse
--
--         , responseCreateAccount $
--             newCreateAccountResponse
--
--         , responseCreateGovCloudAccount $
--             newCreateGovCloudAccountResponse
--
--         , responseCreateOrganization $
--             newCreateOrganizationResponse
--
--         , responseCreateOrganizationalUnit $
--             newCreateOrganizationalUnitResponse
--
--         , responseCreatePolicy $
--             newCreatePolicyResponse
--
--         , responseDeclineHandshake $
--             newDeclineHandshakeResponse
--
--         , responseDeleteOrganization $
--             newDeleteOrganizationResponse
--
--         , responseDeleteOrganizationalUnit $
--             newDeleteOrganizationalUnitResponse
--
--         , responseDeletePolicy $
--             newDeletePolicyResponse
--
--         , responseDeleteResourcePolicy $
--             newDeleteResourcePolicyResponse
--
--         , responseDeregisterDelegatedAdministrator $
--             newDeregisterDelegatedAdministratorResponse
--
--         , responseDescribeAccount $
--             newDescribeAccountResponse
--
--         , responseDescribeCreateAccountStatus $
--             newDescribeCreateAccountStatusResponse
--
--         , responseDescribeEffectivePolicy $
--             newDescribeEffectivePolicyResponse
--
--         , responseDescribeHandshake $
--             newDescribeHandshakeResponse
--
--         , responseDescribeOrganization $
--             newDescribeOrganizationResponse
--
--         , responseDescribeOrganizationalUnit $
--             newDescribeOrganizationalUnitResponse
--
--         , responseDescribePolicy $
--             newDescribePolicyResponse
--
--         , responseDescribeResourcePolicy $
--             newDescribeResourcePolicyResponse
--
--         , responseDetachPolicy $
--             newDetachPolicyResponse
--
--         , responseDisableAWSServiceAccess $
--             newDisableAWSServiceAccessResponse
--
--         , responseDisablePolicyType $
--             newDisablePolicyTypeResponse
--
--         , responseEnableAWSServiceAccess $
--             newEnableAWSServiceAccessResponse
--
--         , responseEnableAllFeatures $
--             newEnableAllFeaturesResponse
--
--         , responseEnablePolicyType $
--             newEnablePolicyTypeResponse
--
--         , responseInviteAccountToOrganization $
--             newInviteAccountToOrganizationResponse
--
--         , responseLeaveOrganization $
--             newLeaveOrganizationResponse
--
--         , responseListAWSServiceAccessForOrganization $
--             newListAWSServiceAccessForOrganizationResponse
--
--         , responseListAccounts $
--             newListAccountsResponse
--
--         , responseListAccountsForParent $
--             newListAccountsForParentResponse
--
--         , responseListChildren $
--             newListChildrenResponse
--
--         , responseListCreateAccountStatus $
--             newListCreateAccountStatusResponse
--
--         , responseListDelegatedAdministrators $
--             newListDelegatedAdministratorsResponse
--
--         , responseListDelegatedServicesForAccount $
--             newListDelegatedServicesForAccountResponse
--
--         , responseListHandshakesForAccount $
--             newListHandshakesForAccountResponse
--
--         , responseListHandshakesForOrganization $
--             newListHandshakesForOrganizationResponse
--
--         , responseListOrganizationalUnitsForParent $
--             newListOrganizationalUnitsForParentResponse
--
--         , responseListParents $
--             newListParentsResponse
--
--         , responseListPolicies $
--             newListPoliciesResponse
--
--         , responseListPoliciesForTarget $
--             newListPoliciesForTargetResponse
--
--         , responseListRoots $
--             newListRootsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseListTargetsForPolicy $
--             newListTargetsForPolicyResponse
--
--         , responseMoveAccount $
--             newMoveAccountResponse
--
--         , responsePutResourcePolicy $
--             newPutResourcePolicyResponse
--
--         , responseRegisterDelegatedAdministrator $
--             newRegisterDelegatedAdministratorResponse
--
--         , responseRemoveAccountFromOrganization $
--             newRemoveAccountFromOrganizationResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateOrganizationalUnit $
--             newUpdateOrganizationalUnitResponse
--
--         , responseUpdatePolicy $
--             newUpdatePolicyResponse
--
--           ]
--     ]

-- Requests

requestAcceptHandshake :: AcceptHandshake -> TestTree
requestAcceptHandshake =
  req
    "AcceptHandshake"
    "fixture/AcceptHandshake.yaml"

requestAttachPolicy :: AttachPolicy -> TestTree
requestAttachPolicy =
  req
    "AttachPolicy"
    "fixture/AttachPolicy.yaml"

requestCancelHandshake :: CancelHandshake -> TestTree
requestCancelHandshake =
  req
    "CancelHandshake"
    "fixture/CancelHandshake.yaml"

requestCloseAccount :: CloseAccount -> TestTree
requestCloseAccount =
  req
    "CloseAccount"
    "fixture/CloseAccount.yaml"

requestCreateAccount :: CreateAccount -> TestTree
requestCreateAccount =
  req
    "CreateAccount"
    "fixture/CreateAccount.yaml"

requestCreateGovCloudAccount :: CreateGovCloudAccount -> TestTree
requestCreateGovCloudAccount =
  req
    "CreateGovCloudAccount"
    "fixture/CreateGovCloudAccount.yaml"

requestCreateOrganization :: CreateOrganization -> TestTree
requestCreateOrganization =
  req
    "CreateOrganization"
    "fixture/CreateOrganization.yaml"

requestCreateOrganizationalUnit :: CreateOrganizationalUnit -> TestTree
requestCreateOrganizationalUnit =
  req
    "CreateOrganizationalUnit"
    "fixture/CreateOrganizationalUnit.yaml"

requestCreatePolicy :: CreatePolicy -> TestTree
requestCreatePolicy =
  req
    "CreatePolicy"
    "fixture/CreatePolicy.yaml"

requestDeclineHandshake :: DeclineHandshake -> TestTree
requestDeclineHandshake =
  req
    "DeclineHandshake"
    "fixture/DeclineHandshake.yaml"

requestDeleteOrganization :: DeleteOrganization -> TestTree
requestDeleteOrganization =
  req
    "DeleteOrganization"
    "fixture/DeleteOrganization.yaml"

requestDeleteOrganizationalUnit :: DeleteOrganizationalUnit -> TestTree
requestDeleteOrganizationalUnit =
  req
    "DeleteOrganizationalUnit"
    "fixture/DeleteOrganizationalUnit.yaml"

requestDeletePolicy :: DeletePolicy -> TestTree
requestDeletePolicy =
  req
    "DeletePolicy"
    "fixture/DeletePolicy.yaml"

requestDeleteResourcePolicy :: DeleteResourcePolicy -> TestTree
requestDeleteResourcePolicy =
  req
    "DeleteResourcePolicy"
    "fixture/DeleteResourcePolicy.yaml"

requestDeregisterDelegatedAdministrator :: DeregisterDelegatedAdministrator -> TestTree
requestDeregisterDelegatedAdministrator =
  req
    "DeregisterDelegatedAdministrator"
    "fixture/DeregisterDelegatedAdministrator.yaml"

requestDescribeAccount :: DescribeAccount -> TestTree
requestDescribeAccount =
  req
    "DescribeAccount"
    "fixture/DescribeAccount.yaml"

requestDescribeCreateAccountStatus :: DescribeCreateAccountStatus -> TestTree
requestDescribeCreateAccountStatus =
  req
    "DescribeCreateAccountStatus"
    "fixture/DescribeCreateAccountStatus.yaml"

requestDescribeEffectivePolicy :: DescribeEffectivePolicy -> TestTree
requestDescribeEffectivePolicy =
  req
    "DescribeEffectivePolicy"
    "fixture/DescribeEffectivePolicy.yaml"

requestDescribeHandshake :: DescribeHandshake -> TestTree
requestDescribeHandshake =
  req
    "DescribeHandshake"
    "fixture/DescribeHandshake.yaml"

requestDescribeOrganization :: DescribeOrganization -> TestTree
requestDescribeOrganization =
  req
    "DescribeOrganization"
    "fixture/DescribeOrganization.yaml"

requestDescribeOrganizationalUnit :: DescribeOrganizationalUnit -> TestTree
requestDescribeOrganizationalUnit =
  req
    "DescribeOrganizationalUnit"
    "fixture/DescribeOrganizationalUnit.yaml"

requestDescribePolicy :: DescribePolicy -> TestTree
requestDescribePolicy =
  req
    "DescribePolicy"
    "fixture/DescribePolicy.yaml"

requestDescribeResourcePolicy :: DescribeResourcePolicy -> TestTree
requestDescribeResourcePolicy =
  req
    "DescribeResourcePolicy"
    "fixture/DescribeResourcePolicy.yaml"

requestDetachPolicy :: DetachPolicy -> TestTree
requestDetachPolicy =
  req
    "DetachPolicy"
    "fixture/DetachPolicy.yaml"

requestDisableAWSServiceAccess :: DisableAWSServiceAccess -> TestTree
requestDisableAWSServiceAccess =
  req
    "DisableAWSServiceAccess"
    "fixture/DisableAWSServiceAccess.yaml"

requestDisablePolicyType :: DisablePolicyType -> TestTree
requestDisablePolicyType =
  req
    "DisablePolicyType"
    "fixture/DisablePolicyType.yaml"

requestEnableAWSServiceAccess :: EnableAWSServiceAccess -> TestTree
requestEnableAWSServiceAccess =
  req
    "EnableAWSServiceAccess"
    "fixture/EnableAWSServiceAccess.yaml"

requestEnableAllFeatures :: EnableAllFeatures -> TestTree
requestEnableAllFeatures =
  req
    "EnableAllFeatures"
    "fixture/EnableAllFeatures.yaml"

requestEnablePolicyType :: EnablePolicyType -> TestTree
requestEnablePolicyType =
  req
    "EnablePolicyType"
    "fixture/EnablePolicyType.yaml"

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

requestListAWSServiceAccessForOrganization :: ListAWSServiceAccessForOrganization -> TestTree
requestListAWSServiceAccessForOrganization =
  req
    "ListAWSServiceAccessForOrganization"
    "fixture/ListAWSServiceAccessForOrganization.yaml"

requestListAccounts :: ListAccounts -> TestTree
requestListAccounts =
  req
    "ListAccounts"
    "fixture/ListAccounts.yaml"

requestListAccountsForParent :: ListAccountsForParent -> TestTree
requestListAccountsForParent =
  req
    "ListAccountsForParent"
    "fixture/ListAccountsForParent.yaml"

requestListChildren :: ListChildren -> TestTree
requestListChildren =
  req
    "ListChildren"
    "fixture/ListChildren.yaml"

requestListCreateAccountStatus :: ListCreateAccountStatus -> TestTree
requestListCreateAccountStatus =
  req
    "ListCreateAccountStatus"
    "fixture/ListCreateAccountStatus.yaml"

requestListDelegatedAdministrators :: ListDelegatedAdministrators -> TestTree
requestListDelegatedAdministrators =
  req
    "ListDelegatedAdministrators"
    "fixture/ListDelegatedAdministrators.yaml"

requestListDelegatedServicesForAccount :: ListDelegatedServicesForAccount -> TestTree
requestListDelegatedServicesForAccount =
  req
    "ListDelegatedServicesForAccount"
    "fixture/ListDelegatedServicesForAccount.yaml"

requestListHandshakesForAccount :: ListHandshakesForAccount -> TestTree
requestListHandshakesForAccount =
  req
    "ListHandshakesForAccount"
    "fixture/ListHandshakesForAccount.yaml"

requestListHandshakesForOrganization :: ListHandshakesForOrganization -> TestTree
requestListHandshakesForOrganization =
  req
    "ListHandshakesForOrganization"
    "fixture/ListHandshakesForOrganization.yaml"

requestListOrganizationalUnitsForParent :: ListOrganizationalUnitsForParent -> TestTree
requestListOrganizationalUnitsForParent =
  req
    "ListOrganizationalUnitsForParent"
    "fixture/ListOrganizationalUnitsForParent.yaml"

requestListParents :: ListParents -> TestTree
requestListParents =
  req
    "ListParents"
    "fixture/ListParents.yaml"

requestListPolicies :: ListPolicies -> TestTree
requestListPolicies =
  req
    "ListPolicies"
    "fixture/ListPolicies.yaml"

requestListPoliciesForTarget :: ListPoliciesForTarget -> TestTree
requestListPoliciesForTarget =
  req
    "ListPoliciesForTarget"
    "fixture/ListPoliciesForTarget.yaml"

requestListRoots :: ListRoots -> TestTree
requestListRoots =
  req
    "ListRoots"
    "fixture/ListRoots.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestListTargetsForPolicy :: ListTargetsForPolicy -> TestTree
requestListTargetsForPolicy =
  req
    "ListTargetsForPolicy"
    "fixture/ListTargetsForPolicy.yaml"

requestMoveAccount :: MoveAccount -> TestTree
requestMoveAccount =
  req
    "MoveAccount"
    "fixture/MoveAccount.yaml"

requestPutResourcePolicy :: PutResourcePolicy -> TestTree
requestPutResourcePolicy =
  req
    "PutResourcePolicy"
    "fixture/PutResourcePolicy.yaml"

requestRegisterDelegatedAdministrator :: RegisterDelegatedAdministrator -> TestTree
requestRegisterDelegatedAdministrator =
  req
    "RegisterDelegatedAdministrator"
    "fixture/RegisterDelegatedAdministrator.yaml"

requestRemoveAccountFromOrganization :: RemoveAccountFromOrganization -> TestTree
requestRemoveAccountFromOrganization =
  req
    "RemoveAccountFromOrganization"
    "fixture/RemoveAccountFromOrganization.yaml"

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

requestUpdateOrganizationalUnit :: UpdateOrganizationalUnit -> TestTree
requestUpdateOrganizationalUnit =
  req
    "UpdateOrganizationalUnit"
    "fixture/UpdateOrganizationalUnit.yaml"

requestUpdatePolicy :: UpdatePolicy -> TestTree
requestUpdatePolicy =
  req
    "UpdatePolicy"
    "fixture/UpdatePolicy.yaml"

-- Responses

responseAcceptHandshake :: AcceptHandshakeResponse -> TestTree
responseAcceptHandshake =
  res
    "AcceptHandshakeResponse"
    "fixture/AcceptHandshakeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptHandshake)

responseAttachPolicy :: AttachPolicyResponse -> TestTree
responseAttachPolicy =
  res
    "AttachPolicyResponse"
    "fixture/AttachPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AttachPolicy)

responseCancelHandshake :: CancelHandshakeResponse -> TestTree
responseCancelHandshake =
  res
    "CancelHandshakeResponse"
    "fixture/CancelHandshakeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CancelHandshake)

responseCloseAccount :: CloseAccountResponse -> TestTree
responseCloseAccount =
  res
    "CloseAccountResponse"
    "fixture/CloseAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CloseAccount)

responseCreateAccount :: CreateAccountResponse -> TestTree
responseCreateAccount =
  res
    "CreateAccountResponse"
    "fixture/CreateAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAccount)

responseCreateGovCloudAccount :: CreateGovCloudAccountResponse -> TestTree
responseCreateGovCloudAccount =
  res
    "CreateGovCloudAccountResponse"
    "fixture/CreateGovCloudAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateGovCloudAccount)

responseCreateOrganization :: CreateOrganizationResponse -> TestTree
responseCreateOrganization =
  res
    "CreateOrganizationResponse"
    "fixture/CreateOrganizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateOrganization)

responseCreateOrganizationalUnit :: CreateOrganizationalUnitResponse -> TestTree
responseCreateOrganizationalUnit =
  res
    "CreateOrganizationalUnitResponse"
    "fixture/CreateOrganizationalUnitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateOrganizationalUnit)

responseCreatePolicy :: CreatePolicyResponse -> TestTree
responseCreatePolicy =
  res
    "CreatePolicyResponse"
    "fixture/CreatePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePolicy)

responseDeclineHandshake :: DeclineHandshakeResponse -> TestTree
responseDeclineHandshake =
  res
    "DeclineHandshakeResponse"
    "fixture/DeclineHandshakeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeclineHandshake)

responseDeleteOrganization :: DeleteOrganizationResponse -> TestTree
responseDeleteOrganization =
  res
    "DeleteOrganizationResponse"
    "fixture/DeleteOrganizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteOrganization)

responseDeleteOrganizationalUnit :: DeleteOrganizationalUnitResponse -> TestTree
responseDeleteOrganizationalUnit =
  res
    "DeleteOrganizationalUnitResponse"
    "fixture/DeleteOrganizationalUnitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteOrganizationalUnit)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy =
  res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePolicy)

responseDeleteResourcePolicy :: DeleteResourcePolicyResponse -> TestTree
responseDeleteResourcePolicy =
  res
    "DeleteResourcePolicyResponse"
    "fixture/DeleteResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourcePolicy)

responseDeregisterDelegatedAdministrator :: DeregisterDelegatedAdministratorResponse -> TestTree
responseDeregisterDelegatedAdministrator =
  res
    "DeregisterDelegatedAdministratorResponse"
    "fixture/DeregisterDelegatedAdministratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterDelegatedAdministrator)

responseDescribeAccount :: DescribeAccountResponse -> TestTree
responseDescribeAccount =
  res
    "DescribeAccountResponse"
    "fixture/DescribeAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeAccount)

responseDescribeCreateAccountStatus :: DescribeCreateAccountStatusResponse -> TestTree
responseDescribeCreateAccountStatus =
  res
    "DescribeCreateAccountStatusResponse"
    "fixture/DescribeCreateAccountStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeCreateAccountStatus)

responseDescribeEffectivePolicy :: DescribeEffectivePolicyResponse -> TestTree
responseDescribeEffectivePolicy =
  res
    "DescribeEffectivePolicyResponse"
    "fixture/DescribeEffectivePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeEffectivePolicy)

responseDescribeHandshake :: DescribeHandshakeResponse -> TestTree
responseDescribeHandshake =
  res
    "DescribeHandshakeResponse"
    "fixture/DescribeHandshakeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeHandshake)

responseDescribeOrganization :: DescribeOrganizationResponse -> TestTree
responseDescribeOrganization =
  res
    "DescribeOrganizationResponse"
    "fixture/DescribeOrganizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOrganization)

responseDescribeOrganizationalUnit :: DescribeOrganizationalUnitResponse -> TestTree
responseDescribeOrganizationalUnit =
  res
    "DescribeOrganizationalUnitResponse"
    "fixture/DescribeOrganizationalUnitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeOrganizationalUnit)

responseDescribePolicy :: DescribePolicyResponse -> TestTree
responseDescribePolicy =
  res
    "DescribePolicyResponse"
    "fixture/DescribePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePolicy)

responseDescribeResourcePolicy :: DescribeResourcePolicyResponse -> TestTree
responseDescribeResourcePolicy =
  res
    "DescribeResourcePolicyResponse"
    "fixture/DescribeResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeResourcePolicy)

responseDetachPolicy :: DetachPolicyResponse -> TestTree
responseDetachPolicy =
  res
    "DetachPolicyResponse"
    "fixture/DetachPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DetachPolicy)

responseDisableAWSServiceAccess :: DisableAWSServiceAccessResponse -> TestTree
responseDisableAWSServiceAccess =
  res
    "DisableAWSServiceAccessResponse"
    "fixture/DisableAWSServiceAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableAWSServiceAccess)

responseDisablePolicyType :: DisablePolicyTypeResponse -> TestTree
responseDisablePolicyType =
  res
    "DisablePolicyTypeResponse"
    "fixture/DisablePolicyTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisablePolicyType)

responseEnableAWSServiceAccess :: EnableAWSServiceAccessResponse -> TestTree
responseEnableAWSServiceAccess =
  res
    "EnableAWSServiceAccessResponse"
    "fixture/EnableAWSServiceAccessResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableAWSServiceAccess)

responseEnableAllFeatures :: EnableAllFeaturesResponse -> TestTree
responseEnableAllFeatures =
  res
    "EnableAllFeaturesResponse"
    "fixture/EnableAllFeaturesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableAllFeatures)

responseEnablePolicyType :: EnablePolicyTypeResponse -> TestTree
responseEnablePolicyType =
  res
    "EnablePolicyTypeResponse"
    "fixture/EnablePolicyTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnablePolicyType)

responseInviteAccountToOrganization :: InviteAccountToOrganizationResponse -> TestTree
responseInviteAccountToOrganization =
  res
    "InviteAccountToOrganizationResponse"
    "fixture/InviteAccountToOrganizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy InviteAccountToOrganization)

responseLeaveOrganization :: LeaveOrganizationResponse -> TestTree
responseLeaveOrganization =
  res
    "LeaveOrganizationResponse"
    "fixture/LeaveOrganizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy LeaveOrganization)

responseListAWSServiceAccessForOrganization :: ListAWSServiceAccessForOrganizationResponse -> TestTree
responseListAWSServiceAccessForOrganization =
  res
    "ListAWSServiceAccessForOrganizationResponse"
    "fixture/ListAWSServiceAccessForOrganizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAWSServiceAccessForOrganization)

responseListAccounts :: ListAccountsResponse -> TestTree
responseListAccounts =
  res
    "ListAccountsResponse"
    "fixture/ListAccountsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccounts)

responseListAccountsForParent :: ListAccountsForParentResponse -> TestTree
responseListAccountsForParent =
  res
    "ListAccountsForParentResponse"
    "fixture/ListAccountsForParentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccountsForParent)

responseListChildren :: ListChildrenResponse -> TestTree
responseListChildren =
  res
    "ListChildrenResponse"
    "fixture/ListChildrenResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListChildren)

responseListCreateAccountStatus :: ListCreateAccountStatusResponse -> TestTree
responseListCreateAccountStatus =
  res
    "ListCreateAccountStatusResponse"
    "fixture/ListCreateAccountStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCreateAccountStatus)

responseListDelegatedAdministrators :: ListDelegatedAdministratorsResponse -> TestTree
responseListDelegatedAdministrators =
  res
    "ListDelegatedAdministratorsResponse"
    "fixture/ListDelegatedAdministratorsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDelegatedAdministrators)

responseListDelegatedServicesForAccount :: ListDelegatedServicesForAccountResponse -> TestTree
responseListDelegatedServicesForAccount =
  res
    "ListDelegatedServicesForAccountResponse"
    "fixture/ListDelegatedServicesForAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDelegatedServicesForAccount)

responseListHandshakesForAccount :: ListHandshakesForAccountResponse -> TestTree
responseListHandshakesForAccount =
  res
    "ListHandshakesForAccountResponse"
    "fixture/ListHandshakesForAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHandshakesForAccount)

responseListHandshakesForOrganization :: ListHandshakesForOrganizationResponse -> TestTree
responseListHandshakesForOrganization =
  res
    "ListHandshakesForOrganizationResponse"
    "fixture/ListHandshakesForOrganizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListHandshakesForOrganization)

responseListOrganizationalUnitsForParent :: ListOrganizationalUnitsForParentResponse -> TestTree
responseListOrganizationalUnitsForParent =
  res
    "ListOrganizationalUnitsForParentResponse"
    "fixture/ListOrganizationalUnitsForParentResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListOrganizationalUnitsForParent)

responseListParents :: ListParentsResponse -> TestTree
responseListParents =
  res
    "ListParentsResponse"
    "fixture/ListParentsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListParents)

responseListPolicies :: ListPoliciesResponse -> TestTree
responseListPolicies =
  res
    "ListPoliciesResponse"
    "fixture/ListPoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPolicies)

responseListPoliciesForTarget :: ListPoliciesForTargetResponse -> TestTree
responseListPoliciesForTarget =
  res
    "ListPoliciesForTargetResponse"
    "fixture/ListPoliciesForTargetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPoliciesForTarget)

responseListRoots :: ListRootsResponse -> TestTree
responseListRoots =
  res
    "ListRootsResponse"
    "fixture/ListRootsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListRoots)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseListTargetsForPolicy :: ListTargetsForPolicyResponse -> TestTree
responseListTargetsForPolicy =
  res
    "ListTargetsForPolicyResponse"
    "fixture/ListTargetsForPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTargetsForPolicy)

responseMoveAccount :: MoveAccountResponse -> TestTree
responseMoveAccount =
  res
    "MoveAccountResponse"
    "fixture/MoveAccountResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy MoveAccount)

responsePutResourcePolicy :: PutResourcePolicyResponse -> TestTree
responsePutResourcePolicy =
  res
    "PutResourcePolicyResponse"
    "fixture/PutResourcePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutResourcePolicy)

responseRegisterDelegatedAdministrator :: RegisterDelegatedAdministratorResponse -> TestTree
responseRegisterDelegatedAdministrator =
  res
    "RegisterDelegatedAdministratorResponse"
    "fixture/RegisterDelegatedAdministratorResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterDelegatedAdministrator)

responseRemoveAccountFromOrganization :: RemoveAccountFromOrganizationResponse -> TestTree
responseRemoveAccountFromOrganization =
  res
    "RemoveAccountFromOrganizationResponse"
    "fixture/RemoveAccountFromOrganizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveAccountFromOrganization)

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

responseUpdateOrganizationalUnit :: UpdateOrganizationalUnitResponse -> TestTree
responseUpdateOrganizationalUnit =
  res
    "UpdateOrganizationalUnitResponse"
    "fixture/UpdateOrganizationalUnitResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateOrganizationalUnit)

responseUpdatePolicy :: UpdatePolicyResponse -> TestTree
responseUpdatePolicy =
  res
    "UpdatePolicyResponse"
    "fixture/UpdatePolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdatePolicy)
