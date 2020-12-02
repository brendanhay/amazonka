{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Organizations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
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
--         [ requestListHandshakesForAccount $
--             listHandshakesForAccount
--
--         , requestDescribeAccount $
--             describeAccount
--
--         , requestListPolicies $
--             listPolicies
--
--         , requestCreatePolicy $
--             createPolicy
--
--         , requestListRoots $
--             listRoots
--
--         , requestAcceptHandshake $
--             acceptHandshake
--
--         , requestCreateOrganization $
--             createOrganization
--
--         , requestEnableAllFeatures $
--             enableAllFeatures
--
--         , requestDeleteOrganization $
--             deleteOrganization
--
--         , requestDescribeHandshake $
--             describeHandshake
--
--         , requestDescribePolicy $
--             describePolicy
--
--         , requestDisableAWSServiceAccess $
--             disableAWSServiceAccess
--
--         , requestLeaveOrganization $
--             leaveOrganization
--
--         , requestMoveAccount $
--             moveAccount
--
--         , requestListAccounts $
--             listAccounts
--
--         , requestInviteAccountToOrganization $
--             inviteAccountToOrganization
--
--         , requestListAWSServiceAccessForOrganization $
--             listAWSServiceAccessForOrganization
--
--         , requestListOrganizationalUnitsForParent $
--             listOrganizationalUnitsForParent
--
--         , requestCancelHandshake $
--             cancelHandshake
--
--         , requestListChildren $
--             listChildren
--
--         , requestDeletePolicy $
--             deletePolicy
--
--         , requestUpdatePolicy $
--             updatePolicy
--
--         , requestEnablePolicyType $
--             enablePolicyType
--
--         , requestDisablePolicyType $
--             disablePolicyType
--
--         , requestDescribeCreateAccountStatus $
--             describeCreateAccountStatus
--
--         , requestCreateOrganizationalUnit $
--             createOrganizationalUnit
--
--         , requestListAccountsForParent $
--             listAccountsForParent
--
--         , requestDetachPolicy $
--             detachPolicy
--
--         , requestRemoveAccountFromOrganization $
--             removeAccountFromOrganization
--
--         , requestEnableAWSServiceAccess $
--             enableAWSServiceAccess
--
--         , requestDescribeOrganizationalUnit $
--             describeOrganizationalUnit
--
--         , requestListParents $
--             listParents
--
--         , requestCreateAccount $
--             createAccount
--
--         , requestListCreateAccountStatus $
--             listCreateAccountStatus
--
--         , requestListTargetsForPolicy $
--             listTargetsForPolicy
--
--         , requestDeclineHandshake $
--             declineHandshake
--
--         , requestAttachPolicy $
--             attachPolicy
--
--         , requestListPoliciesForTarget $
--             listPoliciesForTarget
--
--         , requestDescribeOrganization $
--             describeOrganization
--
--         , requestListHandshakesForOrganization $
--             listHandshakesForOrganization
--
--         , requestDeleteOrganizationalUnit $
--             deleteOrganizationalUnit
--
--         , requestUpdateOrganizationalUnit $
--             updateOrganizationalUnit
--
--           ]

--     , testGroup "response"
--         [ responseListHandshakesForAccount $
--             listHandshakesForAccountResponse
--
--         , responseDescribeAccount $
--             describeAccountResponse
--
--         , responseListPolicies $
--             listPoliciesResponse
--
--         , responseCreatePolicy $
--             createPolicyResponse
--
--         , responseListRoots $
--             listRootsResponse
--
--         , responseAcceptHandshake $
--             acceptHandshakeResponse
--
--         , responseCreateOrganization $
--             createOrganizationResponse
--
--         , responseEnableAllFeatures $
--             enableAllFeaturesResponse
--
--         , responseDeleteOrganization $
--             deleteOrganizationResponse
--
--         , responseDescribeHandshake $
--             describeHandshakeResponse
--
--         , responseDescribePolicy $
--             describePolicyResponse
--
--         , responseDisableAWSServiceAccess $
--             disableAWSServiceAccessResponse
--
--         , responseLeaveOrganization $
--             leaveOrganizationResponse
--
--         , responseMoveAccount $
--             moveAccountResponse
--
--         , responseListAccounts $
--             listAccountsResponse
--
--         , responseInviteAccountToOrganization $
--             inviteAccountToOrganizationResponse
--
--         , responseListAWSServiceAccessForOrganization $
--             listAWSServiceAccessForOrganizationResponse
--
--         , responseListOrganizationalUnitsForParent $
--             listOrganizationalUnitsForParentResponse
--
--         , responseCancelHandshake $
--             cancelHandshakeResponse
--
--         , responseListChildren $
--             listChildrenResponse
--
--         , responseDeletePolicy $
--             deletePolicyResponse
--
--         , responseUpdatePolicy $
--             updatePolicyResponse
--
--         , responseEnablePolicyType $
--             enablePolicyTypeResponse
--
--         , responseDisablePolicyType $
--             disablePolicyTypeResponse
--
--         , responseDescribeCreateAccountStatus $
--             describeCreateAccountStatusResponse
--
--         , responseCreateOrganizationalUnit $
--             createOrganizationalUnitResponse
--
--         , responseListAccountsForParent $
--             listAccountsForParentResponse
--
--         , responseDetachPolicy $
--             detachPolicyResponse
--
--         , responseRemoveAccountFromOrganization $
--             removeAccountFromOrganizationResponse
--
--         , responseEnableAWSServiceAccess $
--             enableAWSServiceAccessResponse
--
--         , responseDescribeOrganizationalUnit $
--             describeOrganizationalUnitResponse
--
--         , responseListParents $
--             listParentsResponse
--
--         , responseCreateAccount $
--             createAccountResponse
--
--         , responseListCreateAccountStatus $
--             listCreateAccountStatusResponse
--
--         , responseListTargetsForPolicy $
--             listTargetsForPolicyResponse
--
--         , responseDeclineHandshake $
--             declineHandshakeResponse
--
--         , responseAttachPolicy $
--             attachPolicyResponse
--
--         , responseListPoliciesForTarget $
--             listPoliciesForTargetResponse
--
--         , responseDescribeOrganization $
--             describeOrganizationResponse
--
--         , responseListHandshakesForOrganization $
--             listHandshakesForOrganizationResponse
--
--         , responseDeleteOrganizationalUnit $
--             deleteOrganizationalUnitResponse
--
--         , responseUpdateOrganizationalUnit $
--             updateOrganizationalUnitResponse
--
--           ]
--     ]

-- Requests

requestListHandshakesForAccount :: ListHandshakesForAccount -> TestTree
requestListHandshakesForAccount = req
    "ListHandshakesForAccount"
    "fixture/ListHandshakesForAccount.yaml"

requestDescribeAccount :: DescribeAccount -> TestTree
requestDescribeAccount = req
    "DescribeAccount"
    "fixture/DescribeAccount.yaml"

requestListPolicies :: ListPolicies -> TestTree
requestListPolicies = req
    "ListPolicies"
    "fixture/ListPolicies.yaml"

requestCreatePolicy :: CreatePolicy -> TestTree
requestCreatePolicy = req
    "CreatePolicy"
    "fixture/CreatePolicy.yaml"

requestListRoots :: ListRoots -> TestTree
requestListRoots = req
    "ListRoots"
    "fixture/ListRoots.yaml"

requestAcceptHandshake :: AcceptHandshake -> TestTree
requestAcceptHandshake = req
    "AcceptHandshake"
    "fixture/AcceptHandshake.yaml"

requestCreateOrganization :: CreateOrganization -> TestTree
requestCreateOrganization = req
    "CreateOrganization"
    "fixture/CreateOrganization.yaml"

requestEnableAllFeatures :: EnableAllFeatures -> TestTree
requestEnableAllFeatures = req
    "EnableAllFeatures"
    "fixture/EnableAllFeatures.yaml"

requestDeleteOrganization :: DeleteOrganization -> TestTree
requestDeleteOrganization = req
    "DeleteOrganization"
    "fixture/DeleteOrganization.yaml"

requestDescribeHandshake :: DescribeHandshake -> TestTree
requestDescribeHandshake = req
    "DescribeHandshake"
    "fixture/DescribeHandshake.yaml"

requestDescribePolicy :: DescribePolicy -> TestTree
requestDescribePolicy = req
    "DescribePolicy"
    "fixture/DescribePolicy.yaml"

requestDisableAWSServiceAccess :: DisableAWSServiceAccess -> TestTree
requestDisableAWSServiceAccess = req
    "DisableAWSServiceAccess"
    "fixture/DisableAWSServiceAccess.yaml"

requestLeaveOrganization :: LeaveOrganization -> TestTree
requestLeaveOrganization = req
    "LeaveOrganization"
    "fixture/LeaveOrganization.yaml"

requestMoveAccount :: MoveAccount -> TestTree
requestMoveAccount = req
    "MoveAccount"
    "fixture/MoveAccount.yaml"

requestListAccounts :: ListAccounts -> TestTree
requestListAccounts = req
    "ListAccounts"
    "fixture/ListAccounts.yaml"

requestInviteAccountToOrganization :: InviteAccountToOrganization -> TestTree
requestInviteAccountToOrganization = req
    "InviteAccountToOrganization"
    "fixture/InviteAccountToOrganization.yaml"

requestListAWSServiceAccessForOrganization :: ListAWSServiceAccessForOrganization -> TestTree
requestListAWSServiceAccessForOrganization = req
    "ListAWSServiceAccessForOrganization"
    "fixture/ListAWSServiceAccessForOrganization.yaml"

requestListOrganizationalUnitsForParent :: ListOrganizationalUnitsForParent -> TestTree
requestListOrganizationalUnitsForParent = req
    "ListOrganizationalUnitsForParent"
    "fixture/ListOrganizationalUnitsForParent.yaml"

requestCancelHandshake :: CancelHandshake -> TestTree
requestCancelHandshake = req
    "CancelHandshake"
    "fixture/CancelHandshake.yaml"

requestListChildren :: ListChildren -> TestTree
requestListChildren = req
    "ListChildren"
    "fixture/ListChildren.yaml"

requestDeletePolicy :: DeletePolicy -> TestTree
requestDeletePolicy = req
    "DeletePolicy"
    "fixture/DeletePolicy.yaml"

requestUpdatePolicy :: UpdatePolicy -> TestTree
requestUpdatePolicy = req
    "UpdatePolicy"
    "fixture/UpdatePolicy.yaml"

requestEnablePolicyType :: EnablePolicyType -> TestTree
requestEnablePolicyType = req
    "EnablePolicyType"
    "fixture/EnablePolicyType.yaml"

requestDisablePolicyType :: DisablePolicyType -> TestTree
requestDisablePolicyType = req
    "DisablePolicyType"
    "fixture/DisablePolicyType.yaml"

requestDescribeCreateAccountStatus :: DescribeCreateAccountStatus -> TestTree
requestDescribeCreateAccountStatus = req
    "DescribeCreateAccountStatus"
    "fixture/DescribeCreateAccountStatus.yaml"

requestCreateOrganizationalUnit :: CreateOrganizationalUnit -> TestTree
requestCreateOrganizationalUnit = req
    "CreateOrganizationalUnit"
    "fixture/CreateOrganizationalUnit.yaml"

requestListAccountsForParent :: ListAccountsForParent -> TestTree
requestListAccountsForParent = req
    "ListAccountsForParent"
    "fixture/ListAccountsForParent.yaml"

requestDetachPolicy :: DetachPolicy -> TestTree
requestDetachPolicy = req
    "DetachPolicy"
    "fixture/DetachPolicy.yaml"

requestRemoveAccountFromOrganization :: RemoveAccountFromOrganization -> TestTree
requestRemoveAccountFromOrganization = req
    "RemoveAccountFromOrganization"
    "fixture/RemoveAccountFromOrganization.yaml"

requestEnableAWSServiceAccess :: EnableAWSServiceAccess -> TestTree
requestEnableAWSServiceAccess = req
    "EnableAWSServiceAccess"
    "fixture/EnableAWSServiceAccess.yaml"

requestDescribeOrganizationalUnit :: DescribeOrganizationalUnit -> TestTree
requestDescribeOrganizationalUnit = req
    "DescribeOrganizationalUnit"
    "fixture/DescribeOrganizationalUnit.yaml"

requestListParents :: ListParents -> TestTree
requestListParents = req
    "ListParents"
    "fixture/ListParents.yaml"

requestCreateAccount :: CreateAccount -> TestTree
requestCreateAccount = req
    "CreateAccount"
    "fixture/CreateAccount.yaml"

requestListCreateAccountStatus :: ListCreateAccountStatus -> TestTree
requestListCreateAccountStatus = req
    "ListCreateAccountStatus"
    "fixture/ListCreateAccountStatus.yaml"

requestListTargetsForPolicy :: ListTargetsForPolicy -> TestTree
requestListTargetsForPolicy = req
    "ListTargetsForPolicy"
    "fixture/ListTargetsForPolicy.yaml"

requestDeclineHandshake :: DeclineHandshake -> TestTree
requestDeclineHandshake = req
    "DeclineHandshake"
    "fixture/DeclineHandshake.yaml"

requestAttachPolicy :: AttachPolicy -> TestTree
requestAttachPolicy = req
    "AttachPolicy"
    "fixture/AttachPolicy.yaml"

requestListPoliciesForTarget :: ListPoliciesForTarget -> TestTree
requestListPoliciesForTarget = req
    "ListPoliciesForTarget"
    "fixture/ListPoliciesForTarget.yaml"

requestDescribeOrganization :: DescribeOrganization -> TestTree
requestDescribeOrganization = req
    "DescribeOrganization"
    "fixture/DescribeOrganization.yaml"

requestListHandshakesForOrganization :: ListHandshakesForOrganization -> TestTree
requestListHandshakesForOrganization = req
    "ListHandshakesForOrganization"
    "fixture/ListHandshakesForOrganization.yaml"

requestDeleteOrganizationalUnit :: DeleteOrganizationalUnit -> TestTree
requestDeleteOrganizationalUnit = req
    "DeleteOrganizationalUnit"
    "fixture/DeleteOrganizationalUnit.yaml"

requestUpdateOrganizationalUnit :: UpdateOrganizationalUnit -> TestTree
requestUpdateOrganizationalUnit = req
    "UpdateOrganizationalUnit"
    "fixture/UpdateOrganizationalUnit.yaml"

-- Responses

responseListHandshakesForAccount :: ListHandshakesForAccountResponse -> TestTree
responseListHandshakesForAccount = res
    "ListHandshakesForAccountResponse"
    "fixture/ListHandshakesForAccountResponse.proto"
    organizations
    (Proxy :: Proxy ListHandshakesForAccount)

responseDescribeAccount :: DescribeAccountResponse -> TestTree
responseDescribeAccount = res
    "DescribeAccountResponse"
    "fixture/DescribeAccountResponse.proto"
    organizations
    (Proxy :: Proxy DescribeAccount)

responseListPolicies :: ListPoliciesResponse -> TestTree
responseListPolicies = res
    "ListPoliciesResponse"
    "fixture/ListPoliciesResponse.proto"
    organizations
    (Proxy :: Proxy ListPolicies)

responseCreatePolicy :: CreatePolicyResponse -> TestTree
responseCreatePolicy = res
    "CreatePolicyResponse"
    "fixture/CreatePolicyResponse.proto"
    organizations
    (Proxy :: Proxy CreatePolicy)

responseListRoots :: ListRootsResponse -> TestTree
responseListRoots = res
    "ListRootsResponse"
    "fixture/ListRootsResponse.proto"
    organizations
    (Proxy :: Proxy ListRoots)

responseAcceptHandshake :: AcceptHandshakeResponse -> TestTree
responseAcceptHandshake = res
    "AcceptHandshakeResponse"
    "fixture/AcceptHandshakeResponse.proto"
    organizations
    (Proxy :: Proxy AcceptHandshake)

responseCreateOrganization :: CreateOrganizationResponse -> TestTree
responseCreateOrganization = res
    "CreateOrganizationResponse"
    "fixture/CreateOrganizationResponse.proto"
    organizations
    (Proxy :: Proxy CreateOrganization)

responseEnableAllFeatures :: EnableAllFeaturesResponse -> TestTree
responseEnableAllFeatures = res
    "EnableAllFeaturesResponse"
    "fixture/EnableAllFeaturesResponse.proto"
    organizations
    (Proxy :: Proxy EnableAllFeatures)

responseDeleteOrganization :: DeleteOrganizationResponse -> TestTree
responseDeleteOrganization = res
    "DeleteOrganizationResponse"
    "fixture/DeleteOrganizationResponse.proto"
    organizations
    (Proxy :: Proxy DeleteOrganization)

responseDescribeHandshake :: DescribeHandshakeResponse -> TestTree
responseDescribeHandshake = res
    "DescribeHandshakeResponse"
    "fixture/DescribeHandshakeResponse.proto"
    organizations
    (Proxy :: Proxy DescribeHandshake)

responseDescribePolicy :: DescribePolicyResponse -> TestTree
responseDescribePolicy = res
    "DescribePolicyResponse"
    "fixture/DescribePolicyResponse.proto"
    organizations
    (Proxy :: Proxy DescribePolicy)

responseDisableAWSServiceAccess :: DisableAWSServiceAccessResponse -> TestTree
responseDisableAWSServiceAccess = res
    "DisableAWSServiceAccessResponse"
    "fixture/DisableAWSServiceAccessResponse.proto"
    organizations
    (Proxy :: Proxy DisableAWSServiceAccess)

responseLeaveOrganization :: LeaveOrganizationResponse -> TestTree
responseLeaveOrganization = res
    "LeaveOrganizationResponse"
    "fixture/LeaveOrganizationResponse.proto"
    organizations
    (Proxy :: Proxy LeaveOrganization)

responseMoveAccount :: MoveAccountResponse -> TestTree
responseMoveAccount = res
    "MoveAccountResponse"
    "fixture/MoveAccountResponse.proto"
    organizations
    (Proxy :: Proxy MoveAccount)

responseListAccounts :: ListAccountsResponse -> TestTree
responseListAccounts = res
    "ListAccountsResponse"
    "fixture/ListAccountsResponse.proto"
    organizations
    (Proxy :: Proxy ListAccounts)

responseInviteAccountToOrganization :: InviteAccountToOrganizationResponse -> TestTree
responseInviteAccountToOrganization = res
    "InviteAccountToOrganizationResponse"
    "fixture/InviteAccountToOrganizationResponse.proto"
    organizations
    (Proxy :: Proxy InviteAccountToOrganization)

responseListAWSServiceAccessForOrganization :: ListAWSServiceAccessForOrganizationResponse -> TestTree
responseListAWSServiceAccessForOrganization = res
    "ListAWSServiceAccessForOrganizationResponse"
    "fixture/ListAWSServiceAccessForOrganizationResponse.proto"
    organizations
    (Proxy :: Proxy ListAWSServiceAccessForOrganization)

responseListOrganizationalUnitsForParent :: ListOrganizationalUnitsForParentResponse -> TestTree
responseListOrganizationalUnitsForParent = res
    "ListOrganizationalUnitsForParentResponse"
    "fixture/ListOrganizationalUnitsForParentResponse.proto"
    organizations
    (Proxy :: Proxy ListOrganizationalUnitsForParent)

responseCancelHandshake :: CancelHandshakeResponse -> TestTree
responseCancelHandshake = res
    "CancelHandshakeResponse"
    "fixture/CancelHandshakeResponse.proto"
    organizations
    (Proxy :: Proxy CancelHandshake)

responseListChildren :: ListChildrenResponse -> TestTree
responseListChildren = res
    "ListChildrenResponse"
    "fixture/ListChildrenResponse.proto"
    organizations
    (Proxy :: Proxy ListChildren)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy = res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    organizations
    (Proxy :: Proxy DeletePolicy)

responseUpdatePolicy :: UpdatePolicyResponse -> TestTree
responseUpdatePolicy = res
    "UpdatePolicyResponse"
    "fixture/UpdatePolicyResponse.proto"
    organizations
    (Proxy :: Proxy UpdatePolicy)

responseEnablePolicyType :: EnablePolicyTypeResponse -> TestTree
responseEnablePolicyType = res
    "EnablePolicyTypeResponse"
    "fixture/EnablePolicyTypeResponse.proto"
    organizations
    (Proxy :: Proxy EnablePolicyType)

responseDisablePolicyType :: DisablePolicyTypeResponse -> TestTree
responseDisablePolicyType = res
    "DisablePolicyTypeResponse"
    "fixture/DisablePolicyTypeResponse.proto"
    organizations
    (Proxy :: Proxy DisablePolicyType)

responseDescribeCreateAccountStatus :: DescribeCreateAccountStatusResponse -> TestTree
responseDescribeCreateAccountStatus = res
    "DescribeCreateAccountStatusResponse"
    "fixture/DescribeCreateAccountStatusResponse.proto"
    organizations
    (Proxy :: Proxy DescribeCreateAccountStatus)

responseCreateOrganizationalUnit :: CreateOrganizationalUnitResponse -> TestTree
responseCreateOrganizationalUnit = res
    "CreateOrganizationalUnitResponse"
    "fixture/CreateOrganizationalUnitResponse.proto"
    organizations
    (Proxy :: Proxy CreateOrganizationalUnit)

responseListAccountsForParent :: ListAccountsForParentResponse -> TestTree
responseListAccountsForParent = res
    "ListAccountsForParentResponse"
    "fixture/ListAccountsForParentResponse.proto"
    organizations
    (Proxy :: Proxy ListAccountsForParent)

responseDetachPolicy :: DetachPolicyResponse -> TestTree
responseDetachPolicy = res
    "DetachPolicyResponse"
    "fixture/DetachPolicyResponse.proto"
    organizations
    (Proxy :: Proxy DetachPolicy)

responseRemoveAccountFromOrganization :: RemoveAccountFromOrganizationResponse -> TestTree
responseRemoveAccountFromOrganization = res
    "RemoveAccountFromOrganizationResponse"
    "fixture/RemoveAccountFromOrganizationResponse.proto"
    organizations
    (Proxy :: Proxy RemoveAccountFromOrganization)

responseEnableAWSServiceAccess :: EnableAWSServiceAccessResponse -> TestTree
responseEnableAWSServiceAccess = res
    "EnableAWSServiceAccessResponse"
    "fixture/EnableAWSServiceAccessResponse.proto"
    organizations
    (Proxy :: Proxy EnableAWSServiceAccess)

responseDescribeOrganizationalUnit :: DescribeOrganizationalUnitResponse -> TestTree
responseDescribeOrganizationalUnit = res
    "DescribeOrganizationalUnitResponse"
    "fixture/DescribeOrganizationalUnitResponse.proto"
    organizations
    (Proxy :: Proxy DescribeOrganizationalUnit)

responseListParents :: ListParentsResponse -> TestTree
responseListParents = res
    "ListParentsResponse"
    "fixture/ListParentsResponse.proto"
    organizations
    (Proxy :: Proxy ListParents)

responseCreateAccount :: CreateAccountResponse -> TestTree
responseCreateAccount = res
    "CreateAccountResponse"
    "fixture/CreateAccountResponse.proto"
    organizations
    (Proxy :: Proxy CreateAccount)

responseListCreateAccountStatus :: ListCreateAccountStatusResponse -> TestTree
responseListCreateAccountStatus = res
    "ListCreateAccountStatusResponse"
    "fixture/ListCreateAccountStatusResponse.proto"
    organizations
    (Proxy :: Proxy ListCreateAccountStatus)

responseListTargetsForPolicy :: ListTargetsForPolicyResponse -> TestTree
responseListTargetsForPolicy = res
    "ListTargetsForPolicyResponse"
    "fixture/ListTargetsForPolicyResponse.proto"
    organizations
    (Proxy :: Proxy ListTargetsForPolicy)

responseDeclineHandshake :: DeclineHandshakeResponse -> TestTree
responseDeclineHandshake = res
    "DeclineHandshakeResponse"
    "fixture/DeclineHandshakeResponse.proto"
    organizations
    (Proxy :: Proxy DeclineHandshake)

responseAttachPolicy :: AttachPolicyResponse -> TestTree
responseAttachPolicy = res
    "AttachPolicyResponse"
    "fixture/AttachPolicyResponse.proto"
    organizations
    (Proxy :: Proxy AttachPolicy)

responseListPoliciesForTarget :: ListPoliciesForTargetResponse -> TestTree
responseListPoliciesForTarget = res
    "ListPoliciesForTargetResponse"
    "fixture/ListPoliciesForTargetResponse.proto"
    organizations
    (Proxy :: Proxy ListPoliciesForTarget)

responseDescribeOrganization :: DescribeOrganizationResponse -> TestTree
responseDescribeOrganization = res
    "DescribeOrganizationResponse"
    "fixture/DescribeOrganizationResponse.proto"
    organizations
    (Proxy :: Proxy DescribeOrganization)

responseListHandshakesForOrganization :: ListHandshakesForOrganizationResponse -> TestTree
responseListHandshakesForOrganization = res
    "ListHandshakesForOrganizationResponse"
    "fixture/ListHandshakesForOrganizationResponse.proto"
    organizations
    (Proxy :: Proxy ListHandshakesForOrganization)

responseDeleteOrganizationalUnit :: DeleteOrganizationalUnitResponse -> TestTree
responseDeleteOrganizationalUnit = res
    "DeleteOrganizationalUnitResponse"
    "fixture/DeleteOrganizationalUnitResponse.proto"
    organizations
    (Proxy :: Proxy DeleteOrganizationalUnit)

responseUpdateOrganizationalUnit :: UpdateOrganizationalUnitResponse -> TestTree
responseUpdateOrganizationalUnit = res
    "UpdateOrganizationalUnitResponse"
    "fixture/UpdateOrganizationalUnitResponse.proto"
    organizations
    (Proxy :: Proxy UpdateOrganizationalUnit)
