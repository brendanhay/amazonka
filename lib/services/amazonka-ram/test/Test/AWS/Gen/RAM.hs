{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.RAM
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.RAM where

import Amazonka.RAM
import qualified Data.Proxy as Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.AWS.RAM.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestPromoteResourceShareCreatedFromPolicy $
--             newPromoteResourceShareCreatedFromPolicy
--
--         , requestGetResourceShares $
--             newGetResourceShares
--
--         , requestGetResourcePolicies $
--             newGetResourcePolicies
--
--         , requestListPendingInvitationResources $
--             newListPendingInvitationResources
--
--         , requestGetPermission $
--             newGetPermission
--
--         , requestEnableSharingWithAwsOrganization $
--             newEnableSharingWithAwsOrganization
--
--         , requestAssociateResourceSharePermission $
--             newAssociateResourceSharePermission
--
--         , requestCreateResourceShare $
--             newCreateResourceShare
--
--         , requestListPrincipals $
--             newListPrincipals
--
--         , requestListResources $
--             newListResources
--
--         , requestAcceptResourceShareInvitation $
--             newAcceptResourceShareInvitation
--
--         , requestDeleteResourceShare $
--             newDeleteResourceShare
--
--         , requestUpdateResourceShare $
--             newUpdateResourceShare
--
--         , requestRejectResourceShareInvitation $
--             newRejectResourceShareInvitation
--
--         , requestListPermissions $
--             newListPermissions
--
--         , requestDisassociateResourceShare $
--             newDisassociateResourceShare
--
--         , requestListResourceSharePermissions $
--             newListResourceSharePermissions
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListResourceTypes $
--             newListResourceTypes
--
--         , requestGetResourceShareAssociations $
--             newGetResourceShareAssociations
--
--         , requestGetResourceShareInvitations $
--             newGetResourceShareInvitations
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestAssociateResourceShare $
--             newAssociateResourceShare
--
--         , requestDisassociateResourceSharePermission $
--             newDisassociateResourceSharePermission
--
--           ]

--     , testGroup "response"
--         [ responsePromoteResourceShareCreatedFromPolicy $
--             newPromoteResourceShareCreatedFromPolicyResponse
--
--         , responseGetResourceShares $
--             newGetResourceSharesResponse
--
--         , responseGetResourcePolicies $
--             newGetResourcePoliciesResponse
--
--         , responseListPendingInvitationResources $
--             newListPendingInvitationResourcesResponse
--
--         , responseGetPermission $
--             newGetPermissionResponse
--
--         , responseEnableSharingWithAwsOrganization $
--             newEnableSharingWithAwsOrganizationResponse
--
--         , responseAssociateResourceSharePermission $
--             newAssociateResourceSharePermissionResponse
--
--         , responseCreateResourceShare $
--             newCreateResourceShareResponse
--
--         , responseListPrincipals $
--             newListPrincipalsResponse
--
--         , responseListResources $
--             newListResourcesResponse
--
--         , responseAcceptResourceShareInvitation $
--             newAcceptResourceShareInvitationResponse
--
--         , responseDeleteResourceShare $
--             newDeleteResourceShareResponse
--
--         , responseUpdateResourceShare $
--             newUpdateResourceShareResponse
--
--         , responseRejectResourceShareInvitation $
--             newRejectResourceShareInvitationResponse
--
--         , responseListPermissions $
--             newListPermissionsResponse
--
--         , responseDisassociateResourceShare $
--             newDisassociateResourceShareResponse
--
--         , responseListResourceSharePermissions $
--             newListResourceSharePermissionsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListResourceTypes $
--             newListResourceTypesResponse
--
--         , responseGetResourceShareAssociations $
--             newGetResourceShareAssociationsResponse
--
--         , responseGetResourceShareInvitations $
--             newGetResourceShareInvitationsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseAssociateResourceShare $
--             newAssociateResourceShareResponse
--
--         , responseDisassociateResourceSharePermission $
--             newDisassociateResourceSharePermissionResponse
--
--           ]
--     ]

-- Requests

requestPromoteResourceShareCreatedFromPolicy :: PromoteResourceShareCreatedFromPolicy -> TestTree
requestPromoteResourceShareCreatedFromPolicy =
  req
    "PromoteResourceShareCreatedFromPolicy"
    "fixture/PromoteResourceShareCreatedFromPolicy.yaml"

requestGetResourceShares :: GetResourceShares -> TestTree
requestGetResourceShares =
  req
    "GetResourceShares"
    "fixture/GetResourceShares.yaml"

requestGetResourcePolicies :: GetResourcePolicies -> TestTree
requestGetResourcePolicies =
  req
    "GetResourcePolicies"
    "fixture/GetResourcePolicies.yaml"

requestListPendingInvitationResources :: ListPendingInvitationResources -> TestTree
requestListPendingInvitationResources =
  req
    "ListPendingInvitationResources"
    "fixture/ListPendingInvitationResources.yaml"

requestGetPermission :: GetPermission -> TestTree
requestGetPermission =
  req
    "GetPermission"
    "fixture/GetPermission.yaml"

requestEnableSharingWithAwsOrganization :: EnableSharingWithAwsOrganization -> TestTree
requestEnableSharingWithAwsOrganization =
  req
    "EnableSharingWithAwsOrganization"
    "fixture/EnableSharingWithAwsOrganization.yaml"

requestAssociateResourceSharePermission :: AssociateResourceSharePermission -> TestTree
requestAssociateResourceSharePermission =
  req
    "AssociateResourceSharePermission"
    "fixture/AssociateResourceSharePermission.yaml"

requestCreateResourceShare :: CreateResourceShare -> TestTree
requestCreateResourceShare =
  req
    "CreateResourceShare"
    "fixture/CreateResourceShare.yaml"

requestListPrincipals :: ListPrincipals -> TestTree
requestListPrincipals =
  req
    "ListPrincipals"
    "fixture/ListPrincipals.yaml"

requestListResources :: ListResources -> TestTree
requestListResources =
  req
    "ListResources"
    "fixture/ListResources.yaml"

requestAcceptResourceShareInvitation :: AcceptResourceShareInvitation -> TestTree
requestAcceptResourceShareInvitation =
  req
    "AcceptResourceShareInvitation"
    "fixture/AcceptResourceShareInvitation.yaml"

requestDeleteResourceShare :: DeleteResourceShare -> TestTree
requestDeleteResourceShare =
  req
    "DeleteResourceShare"
    "fixture/DeleteResourceShare.yaml"

requestUpdateResourceShare :: UpdateResourceShare -> TestTree
requestUpdateResourceShare =
  req
    "UpdateResourceShare"
    "fixture/UpdateResourceShare.yaml"

requestRejectResourceShareInvitation :: RejectResourceShareInvitation -> TestTree
requestRejectResourceShareInvitation =
  req
    "RejectResourceShareInvitation"
    "fixture/RejectResourceShareInvitation.yaml"

requestListPermissions :: ListPermissions -> TestTree
requestListPermissions =
  req
    "ListPermissions"
    "fixture/ListPermissions.yaml"

requestDisassociateResourceShare :: DisassociateResourceShare -> TestTree
requestDisassociateResourceShare =
  req
    "DisassociateResourceShare"
    "fixture/DisassociateResourceShare.yaml"

requestListResourceSharePermissions :: ListResourceSharePermissions -> TestTree
requestListResourceSharePermissions =
  req
    "ListResourceSharePermissions"
    "fixture/ListResourceSharePermissions.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListResourceTypes :: ListResourceTypes -> TestTree
requestListResourceTypes =
  req
    "ListResourceTypes"
    "fixture/ListResourceTypes.yaml"

requestGetResourceShareAssociations :: GetResourceShareAssociations -> TestTree
requestGetResourceShareAssociations =
  req
    "GetResourceShareAssociations"
    "fixture/GetResourceShareAssociations.yaml"

requestGetResourceShareInvitations :: GetResourceShareInvitations -> TestTree
requestGetResourceShareInvitations =
  req
    "GetResourceShareInvitations"
    "fixture/GetResourceShareInvitations.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestAssociateResourceShare :: AssociateResourceShare -> TestTree
requestAssociateResourceShare =
  req
    "AssociateResourceShare"
    "fixture/AssociateResourceShare.yaml"

requestDisassociateResourceSharePermission :: DisassociateResourceSharePermission -> TestTree
requestDisassociateResourceSharePermission =
  req
    "DisassociateResourceSharePermission"
    "fixture/DisassociateResourceSharePermission.yaml"

-- Responses

responsePromoteResourceShareCreatedFromPolicy :: PromoteResourceShareCreatedFromPolicyResponse -> TestTree
responsePromoteResourceShareCreatedFromPolicy =
  res
    "PromoteResourceShareCreatedFromPolicyResponse"
    "fixture/PromoteResourceShareCreatedFromPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PromoteResourceShareCreatedFromPolicy)

responseGetResourceShares :: GetResourceSharesResponse -> TestTree
responseGetResourceShares =
  res
    "GetResourceSharesResponse"
    "fixture/GetResourceSharesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourceShares)

responseGetResourcePolicies :: GetResourcePoliciesResponse -> TestTree
responseGetResourcePolicies =
  res
    "GetResourcePoliciesResponse"
    "fixture/GetResourcePoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourcePolicies)

responseListPendingInvitationResources :: ListPendingInvitationResourcesResponse -> TestTree
responseListPendingInvitationResources =
  res
    "ListPendingInvitationResourcesResponse"
    "fixture/ListPendingInvitationResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPendingInvitationResources)

responseGetPermission :: GetPermissionResponse -> TestTree
responseGetPermission =
  res
    "GetPermissionResponse"
    "fixture/GetPermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPermission)

responseEnableSharingWithAwsOrganization :: EnableSharingWithAwsOrganizationResponse -> TestTree
responseEnableSharingWithAwsOrganization =
  res
    "EnableSharingWithAwsOrganizationResponse"
    "fixture/EnableSharingWithAwsOrganizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableSharingWithAwsOrganization)

responseAssociateResourceSharePermission :: AssociateResourceSharePermissionResponse -> TestTree
responseAssociateResourceSharePermission =
  res
    "AssociateResourceSharePermissionResponse"
    "fixture/AssociateResourceSharePermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateResourceSharePermission)

responseCreateResourceShare :: CreateResourceShareResponse -> TestTree
responseCreateResourceShare =
  res
    "CreateResourceShareResponse"
    "fixture/CreateResourceShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateResourceShare)

responseListPrincipals :: ListPrincipalsResponse -> TestTree
responseListPrincipals =
  res
    "ListPrincipalsResponse"
    "fixture/ListPrincipalsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPrincipals)

responseListResources :: ListResourcesResponse -> TestTree
responseListResources =
  res
    "ListResourcesResponse"
    "fixture/ListResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResources)

responseAcceptResourceShareInvitation :: AcceptResourceShareInvitationResponse -> TestTree
responseAcceptResourceShareInvitation =
  res
    "AcceptResourceShareInvitationResponse"
    "fixture/AcceptResourceShareInvitationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptResourceShareInvitation)

responseDeleteResourceShare :: DeleteResourceShareResponse -> TestTree
responseDeleteResourceShare =
  res
    "DeleteResourceShareResponse"
    "fixture/DeleteResourceShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourceShare)

responseUpdateResourceShare :: UpdateResourceShareResponse -> TestTree
responseUpdateResourceShare =
  res
    "UpdateResourceShareResponse"
    "fixture/UpdateResourceShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResourceShare)

responseRejectResourceShareInvitation :: RejectResourceShareInvitationResponse -> TestTree
responseRejectResourceShareInvitation =
  res
    "RejectResourceShareInvitationResponse"
    "fixture/RejectResourceShareInvitationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectResourceShareInvitation)

responseListPermissions :: ListPermissionsResponse -> TestTree
responseListPermissions =
  res
    "ListPermissionsResponse"
    "fixture/ListPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPermissions)

responseDisassociateResourceShare :: DisassociateResourceShareResponse -> TestTree
responseDisassociateResourceShare =
  res
    "DisassociateResourceShareResponse"
    "fixture/DisassociateResourceShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateResourceShare)

responseListResourceSharePermissions :: ListResourceSharePermissionsResponse -> TestTree
responseListResourceSharePermissions =
  res
    "ListResourceSharePermissionsResponse"
    "fixture/ListResourceSharePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourceSharePermissions)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseListResourceTypes :: ListResourceTypesResponse -> TestTree
responseListResourceTypes =
  res
    "ListResourceTypesResponse"
    "fixture/ListResourceTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourceTypes)

responseGetResourceShareAssociations :: GetResourceShareAssociationsResponse -> TestTree
responseGetResourceShareAssociations =
  res
    "GetResourceShareAssociationsResponse"
    "fixture/GetResourceShareAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourceShareAssociations)

responseGetResourceShareInvitations :: GetResourceShareInvitationsResponse -> TestTree
responseGetResourceShareInvitations =
  res
    "GetResourceShareInvitationsResponse"
    "fixture/GetResourceShareInvitationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourceShareInvitations)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseAssociateResourceShare :: AssociateResourceShareResponse -> TestTree
responseAssociateResourceShare =
  res
    "AssociateResourceShareResponse"
    "fixture/AssociateResourceShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateResourceShare)

responseDisassociateResourceSharePermission :: DisassociateResourceSharePermissionResponse -> TestTree
responseDisassociateResourceSharePermission =
  res
    "DisassociateResourceSharePermissionResponse"
    "fixture/DisassociateResourceSharePermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateResourceSharePermission)
