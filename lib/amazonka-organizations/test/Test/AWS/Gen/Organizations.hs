{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Organizations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.Organizations where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.Organizations
import Test.AWS.Organizations.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestListHandshakesForAccount $
--             mkListHandshakesForAccount
--
--         , requestDescribeAccount $
--             mkDescribeAccount
--
--         , requestListPolicies $
--             mkListPolicies
--
--         , requestCreatePolicy $
--             mkCreatePolicy
--
--         , requestListRoots $
--             mkListRoots
--
--         , requestAcceptHandshake $
--             mkAcceptHandshake
--
--         , requestCreateOrganization $
--             mkCreateOrganization
--
--         , requestEnableAllFeatures $
--             mkEnableAllFeatures
--
--         , requestDeleteOrganization $
--             mkDeleteOrganization
--
--         , requestDescribeHandshake $
--             mkDescribeHandshake
--
--         , requestListTagsForResource $
--             mkListTagsForResource
--
--         , requestDescribePolicy $
--             mkDescribePolicy
--
--         , requestListDelegatedServicesForAccount $
--             mkListDelegatedServicesForAccount
--
--         , requestDisableAWSServiceAccess $
--             mkDisableAWSServiceAccess
--
--         , requestDescribeEffectivePolicy $
--             mkDescribeEffectivePolicy
--
--         , requestLeaveOrganization $
--             mkLeaveOrganization
--
--         , requestMoveAccount $
--             mkMoveAccount
--
--         , requestListAccounts $
--             mkListAccounts
--
--         , requestInviteAccountToOrganization $
--             mkInviteAccountToOrganization
--
--         , requestListAWSServiceAccessForOrganization $
--             mkListAWSServiceAccessForOrganization
--
--         , requestListOrganizationalUnitsForParent $
--             mkListOrganizationalUnitsForParent
--
--         , requestCancelHandshake $
--             mkCancelHandshake
--
--         , requestListChildren $
--             mkListChildren
--
--         , requestListDelegatedAdministrators $
--             mkListDelegatedAdministrators
--
--         , requestDeletePolicy $
--             mkDeletePolicy
--
--         , requestUpdatePolicy $
--             mkUpdatePolicy
--
--         , requestEnablePolicyType $
--             mkEnablePolicyType
--
--         , requestDisablePolicyType $
--             mkDisablePolicyType
--
--         , requestDescribeCreateAccountStatus $
--             mkDescribeCreateAccountStatus
--
--         , requestCreateOrganizationalUnit $
--             mkCreateOrganizationalUnit
--
--         , requestListAccountsForParent $
--             mkListAccountsForParent
--
--         , requestDetachPolicy $
--             mkDetachPolicy
--
--         , requestRemoveAccountFromOrganization $
--             mkRemoveAccountFromOrganization
--
--         , requestCreateGovCloudAccount $
--             mkCreateGovCloudAccount
--
--         , requestEnableAWSServiceAccess $
--             mkEnableAWSServiceAccess
--
--         , requestDescribeOrganizationalUnit $
--             mkDescribeOrganizationalUnit
--
--         , requestListParents $
--             mkListParents
--
--         , requestCreateAccount $
--             mkCreateAccount
--
--         , requestDeregisterDelegatedAdministrator $
--             mkDeregisterDelegatedAdministrator
--
--         , requestTagResource $
--             mkTagResource
--
--         , requestListCreateAccountStatus $
--             mkListCreateAccountStatus
--
--         , requestListTargetsForPolicy $
--             mkListTargetsForPolicy
--
--         , requestDeclineHandshake $
--             mkDeclineHandshake
--
--         , requestUntagResource $
--             mkUntagResource
--
--         , requestAttachPolicy $
--             mkAttachPolicy
--
--         , requestListPoliciesForTarget $
--             mkListPoliciesForTarget
--
--         , requestDescribeOrganization $
--             mkDescribeOrganization
--
--         , requestListHandshakesForOrganization $
--             mkListHandshakesForOrganization
--
--         , requestRegisterDelegatedAdministrator $
--             mkRegisterDelegatedAdministrator
--
--         , requestDeleteOrganizationalUnit $
--             mkDeleteOrganizationalUnit
--
--         , requestUpdateOrganizationalUnit $
--             mkUpdateOrganizationalUnit
--
--           ]

--     , testGroup "response"
--         [ responseListHandshakesForAccount $
--             mkListHandshakesForAccountResponse
--
--         , responseDescribeAccount $
--             mkDescribeAccountResponse
--
--         , responseListPolicies $
--             mkListPoliciesResponse
--
--         , responseCreatePolicy $
--             mkCreatePolicyResponse
--
--         , responseListRoots $
--             mkListRootsResponse
--
--         , responseAcceptHandshake $
--             mkAcceptHandshakeResponse
--
--         , responseCreateOrganization $
--             mkCreateOrganizationResponse
--
--         , responseEnableAllFeatures $
--             mkEnableAllFeaturesResponse
--
--         , responseDeleteOrganization $
--             mkDeleteOrganizationResponse
--
--         , responseDescribeHandshake $
--             mkDescribeHandshakeResponse
--
--         , responseListTagsForResource $
--             mkListTagsForResourceResponse
--
--         , responseDescribePolicy $
--             mkDescribePolicyResponse
--
--         , responseListDelegatedServicesForAccount $
--             mkListDelegatedServicesForAccountResponse
--
--         , responseDisableAWSServiceAccess $
--             mkDisableAWSServiceAccessResponse
--
--         , responseDescribeEffectivePolicy $
--             mkDescribeEffectivePolicyResponse
--
--         , responseLeaveOrganization $
--             mkLeaveOrganizationResponse
--
--         , responseMoveAccount $
--             mkMoveAccountResponse
--
--         , responseListAccounts $
--             mkListAccountsResponse
--
--         , responseInviteAccountToOrganization $
--             mkInviteAccountToOrganizationResponse
--
--         , responseListAWSServiceAccessForOrganization $
--             mkListAWSServiceAccessForOrganizationResponse
--
--         , responseListOrganizationalUnitsForParent $
--             mkListOrganizationalUnitsForParentResponse
--
--         , responseCancelHandshake $
--             mkCancelHandshakeResponse
--
--         , responseListChildren $
--             mkListChildrenResponse
--
--         , responseListDelegatedAdministrators $
--             mkListDelegatedAdministratorsResponse
--
--         , responseDeletePolicy $
--             mkDeletePolicyResponse
--
--         , responseUpdatePolicy $
--             mkUpdatePolicyResponse
--
--         , responseEnablePolicyType $
--             mkEnablePolicyTypeResponse
--
--         , responseDisablePolicyType $
--             mkDisablePolicyTypeResponse
--
--         , responseDescribeCreateAccountStatus $
--             mkDescribeCreateAccountStatusResponse
--
--         , responseCreateOrganizationalUnit $
--             mkCreateOrganizationalUnitResponse
--
--         , responseListAccountsForParent $
--             mkListAccountsForParentResponse
--
--         , responseDetachPolicy $
--             mkDetachPolicyResponse
--
--         , responseRemoveAccountFromOrganization $
--             mkRemoveAccountFromOrganizationResponse
--
--         , responseCreateGovCloudAccount $
--             mkCreateGovCloudAccountResponse
--
--         , responseEnableAWSServiceAccess $
--             mkEnableAWSServiceAccessResponse
--
--         , responseDescribeOrganizationalUnit $
--             mkDescribeOrganizationalUnitResponse
--
--         , responseListParents $
--             mkListParentsResponse
--
--         , responseCreateAccount $
--             mkCreateAccountResponse
--
--         , responseDeregisterDelegatedAdministrator $
--             mkDeregisterDelegatedAdministratorResponse
--
--         , responseTagResource $
--             mkTagResourceResponse
--
--         , responseListCreateAccountStatus $
--             mkListCreateAccountStatusResponse
--
--         , responseListTargetsForPolicy $
--             mkListTargetsForPolicyResponse
--
--         , responseDeclineHandshake $
--             mkDeclineHandshakeResponse
--
--         , responseUntagResource $
--             mkUntagResourceResponse
--
--         , responseAttachPolicy $
--             mkAttachPolicyResponse
--
--         , responseListPoliciesForTarget $
--             mkListPoliciesForTargetResponse
--
--         , responseDescribeOrganization $
--             mkDescribeOrganizationResponse
--
--         , responseListHandshakesForOrganization $
--             mkListHandshakesForOrganizationResponse
--
--         , responseRegisterDelegatedAdministrator $
--             mkRegisterDelegatedAdministratorResponse
--
--         , responseDeleteOrganizationalUnit $
--             mkDeleteOrganizationalUnitResponse
--
--         , responseUpdateOrganizationalUnit $
--             mkUpdateOrganizationalUnitResponse
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

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDescribePolicy :: DescribePolicy -> TestTree
requestDescribePolicy = req
    "DescribePolicy"
    "fixture/DescribePolicy.yaml"

requestListDelegatedServicesForAccount :: ListDelegatedServicesForAccount -> TestTree
requestListDelegatedServicesForAccount = req
    "ListDelegatedServicesForAccount"
    "fixture/ListDelegatedServicesForAccount.yaml"

requestDisableAWSServiceAccess :: DisableAWSServiceAccess -> TestTree
requestDisableAWSServiceAccess = req
    "DisableAWSServiceAccess"
    "fixture/DisableAWSServiceAccess.yaml"

requestDescribeEffectivePolicy :: DescribeEffectivePolicy -> TestTree
requestDescribeEffectivePolicy = req
    "DescribeEffectivePolicy"
    "fixture/DescribeEffectivePolicy.yaml"

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

requestListDelegatedAdministrators :: ListDelegatedAdministrators -> TestTree
requestListDelegatedAdministrators = req
    "ListDelegatedAdministrators"
    "fixture/ListDelegatedAdministrators.yaml"

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

requestCreateGovCloudAccount :: CreateGovCloudAccount -> TestTree
requestCreateGovCloudAccount = req
    "CreateGovCloudAccount"
    "fixture/CreateGovCloudAccount.yaml"

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

requestDeregisterDelegatedAdministrator :: DeregisterDelegatedAdministrator -> TestTree
requestDeregisterDelegatedAdministrator = req
    "DeregisterDelegatedAdministrator"
    "fixture/DeregisterDelegatedAdministrator.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource = req
    "TagResource"
    "fixture/TagResource.yaml"

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

requestUntagResource :: UntagResource -> TestTree
requestUntagResource = req
    "UntagResource"
    "fixture/UntagResource.yaml"

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

requestRegisterDelegatedAdministrator :: RegisterDelegatedAdministrator -> TestTree
requestRegisterDelegatedAdministrator = req
    "RegisterDelegatedAdministrator"
    "fixture/RegisterDelegatedAdministrator.yaml"

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
    mkServiceConfig
    (Proxy :: Proxy ListHandshakesForAccount)

responseDescribeAccount :: DescribeAccountResponse -> TestTree
responseDescribeAccount = res
    "DescribeAccountResponse"
    "fixture/DescribeAccountResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeAccount)

responseListPolicies :: ListPoliciesResponse -> TestTree
responseListPolicies = res
    "ListPoliciesResponse"
    "fixture/ListPoliciesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListPolicies)

responseCreatePolicy :: CreatePolicyResponse -> TestTree
responseCreatePolicy = res
    "CreatePolicyResponse"
    "fixture/CreatePolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreatePolicy)

responseListRoots :: ListRootsResponse -> TestTree
responseListRoots = res
    "ListRootsResponse"
    "fixture/ListRootsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListRoots)

responseAcceptHandshake :: AcceptHandshakeResponse -> TestTree
responseAcceptHandshake = res
    "AcceptHandshakeResponse"
    "fixture/AcceptHandshakeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AcceptHandshake)

responseCreateOrganization :: CreateOrganizationResponse -> TestTree
responseCreateOrganization = res
    "CreateOrganizationResponse"
    "fixture/CreateOrganizationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateOrganization)

responseEnableAllFeatures :: EnableAllFeaturesResponse -> TestTree
responseEnableAllFeatures = res
    "EnableAllFeaturesResponse"
    "fixture/EnableAllFeaturesResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy EnableAllFeatures)

responseDeleteOrganization :: DeleteOrganizationResponse -> TestTree
responseDeleteOrganization = res
    "DeleteOrganizationResponse"
    "fixture/DeleteOrganizationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteOrganization)

responseDescribeHandshake :: DescribeHandshakeResponse -> TestTree
responseDescribeHandshake = res
    "DescribeHandshakeResponse"
    "fixture/DescribeHandshakeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeHandshake)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTagsForResource)

responseDescribePolicy :: DescribePolicyResponse -> TestTree
responseDescribePolicy = res
    "DescribePolicyResponse"
    "fixture/DescribePolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribePolicy)

responseListDelegatedServicesForAccount :: ListDelegatedServicesForAccountResponse -> TestTree
responseListDelegatedServicesForAccount = res
    "ListDelegatedServicesForAccountResponse"
    "fixture/ListDelegatedServicesForAccountResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListDelegatedServicesForAccount)

responseDisableAWSServiceAccess :: DisableAWSServiceAccessResponse -> TestTree
responseDisableAWSServiceAccess = res
    "DisableAWSServiceAccessResponse"
    "fixture/DisableAWSServiceAccessResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisableAWSServiceAccess)

responseDescribeEffectivePolicy :: DescribeEffectivePolicyResponse -> TestTree
responseDescribeEffectivePolicy = res
    "DescribeEffectivePolicyResponse"
    "fixture/DescribeEffectivePolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeEffectivePolicy)

responseLeaveOrganization :: LeaveOrganizationResponse -> TestTree
responseLeaveOrganization = res
    "LeaveOrganizationResponse"
    "fixture/LeaveOrganizationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy LeaveOrganization)

responseMoveAccount :: MoveAccountResponse -> TestTree
responseMoveAccount = res
    "MoveAccountResponse"
    "fixture/MoveAccountResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy MoveAccount)

responseListAccounts :: ListAccountsResponse -> TestTree
responseListAccounts = res
    "ListAccountsResponse"
    "fixture/ListAccountsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListAccounts)

responseInviteAccountToOrganization :: InviteAccountToOrganizationResponse -> TestTree
responseInviteAccountToOrganization = res
    "InviteAccountToOrganizationResponse"
    "fixture/InviteAccountToOrganizationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy InviteAccountToOrganization)

responseListAWSServiceAccessForOrganization :: ListAWSServiceAccessForOrganizationResponse -> TestTree
responseListAWSServiceAccessForOrganization = res
    "ListAWSServiceAccessForOrganizationResponse"
    "fixture/ListAWSServiceAccessForOrganizationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListAWSServiceAccessForOrganization)

responseListOrganizationalUnitsForParent :: ListOrganizationalUnitsForParentResponse -> TestTree
responseListOrganizationalUnitsForParent = res
    "ListOrganizationalUnitsForParentResponse"
    "fixture/ListOrganizationalUnitsForParentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListOrganizationalUnitsForParent)

responseCancelHandshake :: CancelHandshakeResponse -> TestTree
responseCancelHandshake = res
    "CancelHandshakeResponse"
    "fixture/CancelHandshakeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CancelHandshake)

responseListChildren :: ListChildrenResponse -> TestTree
responseListChildren = res
    "ListChildrenResponse"
    "fixture/ListChildrenResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListChildren)

responseListDelegatedAdministrators :: ListDelegatedAdministratorsResponse -> TestTree
responseListDelegatedAdministrators = res
    "ListDelegatedAdministratorsResponse"
    "fixture/ListDelegatedAdministratorsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListDelegatedAdministrators)

responseDeletePolicy :: DeletePolicyResponse -> TestTree
responseDeletePolicy = res
    "DeletePolicyResponse"
    "fixture/DeletePolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeletePolicy)

responseUpdatePolicy :: UpdatePolicyResponse -> TestTree
responseUpdatePolicy = res
    "UpdatePolicyResponse"
    "fixture/UpdatePolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdatePolicy)

responseEnablePolicyType :: EnablePolicyTypeResponse -> TestTree
responseEnablePolicyType = res
    "EnablePolicyTypeResponse"
    "fixture/EnablePolicyTypeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy EnablePolicyType)

responseDisablePolicyType :: DisablePolicyTypeResponse -> TestTree
responseDisablePolicyType = res
    "DisablePolicyTypeResponse"
    "fixture/DisablePolicyTypeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DisablePolicyType)

responseDescribeCreateAccountStatus :: DescribeCreateAccountStatusResponse -> TestTree
responseDescribeCreateAccountStatus = res
    "DescribeCreateAccountStatusResponse"
    "fixture/DescribeCreateAccountStatusResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeCreateAccountStatus)

responseCreateOrganizationalUnit :: CreateOrganizationalUnitResponse -> TestTree
responseCreateOrganizationalUnit = res
    "CreateOrganizationalUnitResponse"
    "fixture/CreateOrganizationalUnitResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateOrganizationalUnit)

responseListAccountsForParent :: ListAccountsForParentResponse -> TestTree
responseListAccountsForParent = res
    "ListAccountsForParentResponse"
    "fixture/ListAccountsForParentResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListAccountsForParent)

responseDetachPolicy :: DetachPolicyResponse -> TestTree
responseDetachPolicy = res
    "DetachPolicyResponse"
    "fixture/DetachPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DetachPolicy)

responseRemoveAccountFromOrganization :: RemoveAccountFromOrganizationResponse -> TestTree
responseRemoveAccountFromOrganization = res
    "RemoveAccountFromOrganizationResponse"
    "fixture/RemoveAccountFromOrganizationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RemoveAccountFromOrganization)

responseCreateGovCloudAccount :: CreateGovCloudAccountResponse -> TestTree
responseCreateGovCloudAccount = res
    "CreateGovCloudAccountResponse"
    "fixture/CreateGovCloudAccountResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateGovCloudAccount)

responseEnableAWSServiceAccess :: EnableAWSServiceAccessResponse -> TestTree
responseEnableAWSServiceAccess = res
    "EnableAWSServiceAccessResponse"
    "fixture/EnableAWSServiceAccessResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy EnableAWSServiceAccess)

responseDescribeOrganizationalUnit :: DescribeOrganizationalUnitResponse -> TestTree
responseDescribeOrganizationalUnit = res
    "DescribeOrganizationalUnitResponse"
    "fixture/DescribeOrganizationalUnitResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeOrganizationalUnit)

responseListParents :: ListParentsResponse -> TestTree
responseListParents = res
    "ListParentsResponse"
    "fixture/ListParentsResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListParents)

responseCreateAccount :: CreateAccountResponse -> TestTree
responseCreateAccount = res
    "CreateAccountResponse"
    "fixture/CreateAccountResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy CreateAccount)

responseDeregisterDelegatedAdministrator :: DeregisterDelegatedAdministratorResponse -> TestTree
responseDeregisterDelegatedAdministrator = res
    "DeregisterDelegatedAdministratorResponse"
    "fixture/DeregisterDelegatedAdministratorResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeregisterDelegatedAdministrator)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource = res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy TagResource)

responseListCreateAccountStatus :: ListCreateAccountStatusResponse -> TestTree
responseListCreateAccountStatus = res
    "ListCreateAccountStatusResponse"
    "fixture/ListCreateAccountStatusResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListCreateAccountStatus)

responseListTargetsForPolicy :: ListTargetsForPolicyResponse -> TestTree
responseListTargetsForPolicy = res
    "ListTargetsForPolicyResponse"
    "fixture/ListTargetsForPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListTargetsForPolicy)

responseDeclineHandshake :: DeclineHandshakeResponse -> TestTree
responseDeclineHandshake = res
    "DeclineHandshakeResponse"
    "fixture/DeclineHandshakeResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeclineHandshake)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UntagResource)

responseAttachPolicy :: AttachPolicyResponse -> TestTree
responseAttachPolicy = res
    "AttachPolicyResponse"
    "fixture/AttachPolicyResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy AttachPolicy)

responseListPoliciesForTarget :: ListPoliciesForTargetResponse -> TestTree
responseListPoliciesForTarget = res
    "ListPoliciesForTargetResponse"
    "fixture/ListPoliciesForTargetResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListPoliciesForTarget)

responseDescribeOrganization :: DescribeOrganizationResponse -> TestTree
responseDescribeOrganization = res
    "DescribeOrganizationResponse"
    "fixture/DescribeOrganizationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DescribeOrganization)

responseListHandshakesForOrganization :: ListHandshakesForOrganizationResponse -> TestTree
responseListHandshakesForOrganization = res
    "ListHandshakesForOrganizationResponse"
    "fixture/ListHandshakesForOrganizationResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy ListHandshakesForOrganization)

responseRegisterDelegatedAdministrator :: RegisterDelegatedAdministratorResponse -> TestTree
responseRegisterDelegatedAdministrator = res
    "RegisterDelegatedAdministratorResponse"
    "fixture/RegisterDelegatedAdministratorResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy RegisterDelegatedAdministrator)

responseDeleteOrganizationalUnit :: DeleteOrganizationalUnitResponse -> TestTree
responseDeleteOrganizationalUnit = res
    "DeleteOrganizationalUnitResponse"
    "fixture/DeleteOrganizationalUnitResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy DeleteOrganizationalUnit)

responseUpdateOrganizationalUnit :: UpdateOrganizationalUnitResponse -> TestTree
responseUpdateOrganizationalUnit = res
    "UpdateOrganizationalUnitResponse"
    "fixture/UpdateOrganizationalUnitResponse.proto"
    mkServiceConfig
    (Proxy :: Proxy UpdateOrganizationalUnit)
