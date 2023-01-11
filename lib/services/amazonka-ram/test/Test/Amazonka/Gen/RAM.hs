{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.RAM
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.RAM where

import Amazonka.RAM
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.RAM.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAcceptResourceShareInvitation $
--             newAcceptResourceShareInvitation
--
--         , requestAssociateResourceShare $
--             newAssociateResourceShare
--
--         , requestAssociateResourceSharePermission $
--             newAssociateResourceSharePermission
--
--         , requestCreateResourceShare $
--             newCreateResourceShare
--
--         , requestDeleteResourceShare $
--             newDeleteResourceShare
--
--         , requestDisassociateResourceShare $
--             newDisassociateResourceShare
--
--         , requestDisassociateResourceSharePermission $
--             newDisassociateResourceSharePermission
--
--         , requestEnableSharingWithAwsOrganization $
--             newEnableSharingWithAwsOrganization
--
--         , requestGetPermission $
--             newGetPermission
--
--         , requestGetResourcePolicies $
--             newGetResourcePolicies
--
--         , requestGetResourceShareAssociations $
--             newGetResourceShareAssociations
--
--         , requestGetResourceShareInvitations $
--             newGetResourceShareInvitations
--
--         , requestGetResourceShares $
--             newGetResourceShares
--
--         , requestListPendingInvitationResources $
--             newListPendingInvitationResources
--
--         , requestListPermissionVersions $
--             newListPermissionVersions
--
--         , requestListPermissions $
--             newListPermissions
--
--         , requestListPrincipals $
--             newListPrincipals
--
--         , requestListResourceSharePermissions $
--             newListResourceSharePermissions
--
--         , requestListResourceTypes $
--             newListResourceTypes
--
--         , requestListResources $
--             newListResources
--
--         , requestPromoteResourceShareCreatedFromPolicy $
--             newPromoteResourceShareCreatedFromPolicy
--
--         , requestRejectResourceShareInvitation $
--             newRejectResourceShareInvitation
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateResourceShare $
--             newUpdateResourceShare
--
--           ]

--     , testGroup "response"
--         [ responseAcceptResourceShareInvitation $
--             newAcceptResourceShareInvitationResponse
--
--         , responseAssociateResourceShare $
--             newAssociateResourceShareResponse
--
--         , responseAssociateResourceSharePermission $
--             newAssociateResourceSharePermissionResponse
--
--         , responseCreateResourceShare $
--             newCreateResourceShareResponse
--
--         , responseDeleteResourceShare $
--             newDeleteResourceShareResponse
--
--         , responseDisassociateResourceShare $
--             newDisassociateResourceShareResponse
--
--         , responseDisassociateResourceSharePermission $
--             newDisassociateResourceSharePermissionResponse
--
--         , responseEnableSharingWithAwsOrganization $
--             newEnableSharingWithAwsOrganizationResponse
--
--         , responseGetPermission $
--             newGetPermissionResponse
--
--         , responseGetResourcePolicies $
--             newGetResourcePoliciesResponse
--
--         , responseGetResourceShareAssociations $
--             newGetResourceShareAssociationsResponse
--
--         , responseGetResourceShareInvitations $
--             newGetResourceShareInvitationsResponse
--
--         , responseGetResourceShares $
--             newGetResourceSharesResponse
--
--         , responseListPendingInvitationResources $
--             newListPendingInvitationResourcesResponse
--
--         , responseListPermissionVersions $
--             newListPermissionVersionsResponse
--
--         , responseListPermissions $
--             newListPermissionsResponse
--
--         , responseListPrincipals $
--             newListPrincipalsResponse
--
--         , responseListResourceSharePermissions $
--             newListResourceSharePermissionsResponse
--
--         , responseListResourceTypes $
--             newListResourceTypesResponse
--
--         , responseListResources $
--             newListResourcesResponse
--
--         , responsePromoteResourceShareCreatedFromPolicy $
--             newPromoteResourceShareCreatedFromPolicyResponse
--
--         , responseRejectResourceShareInvitation $
--             newRejectResourceShareInvitationResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateResourceShare $
--             newUpdateResourceShareResponse
--
--           ]
--     ]

-- Requests

requestAcceptResourceShareInvitation :: AcceptResourceShareInvitation -> TestTree
requestAcceptResourceShareInvitation =
  req
    "AcceptResourceShareInvitation"
    "fixture/AcceptResourceShareInvitation.yaml"

requestAssociateResourceShare :: AssociateResourceShare -> TestTree
requestAssociateResourceShare =
  req
    "AssociateResourceShare"
    "fixture/AssociateResourceShare.yaml"

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

requestDeleteResourceShare :: DeleteResourceShare -> TestTree
requestDeleteResourceShare =
  req
    "DeleteResourceShare"
    "fixture/DeleteResourceShare.yaml"

requestDisassociateResourceShare :: DisassociateResourceShare -> TestTree
requestDisassociateResourceShare =
  req
    "DisassociateResourceShare"
    "fixture/DisassociateResourceShare.yaml"

requestDisassociateResourceSharePermission :: DisassociateResourceSharePermission -> TestTree
requestDisassociateResourceSharePermission =
  req
    "DisassociateResourceSharePermission"
    "fixture/DisassociateResourceSharePermission.yaml"

requestEnableSharingWithAwsOrganization :: EnableSharingWithAwsOrganization -> TestTree
requestEnableSharingWithAwsOrganization =
  req
    "EnableSharingWithAwsOrganization"
    "fixture/EnableSharingWithAwsOrganization.yaml"

requestGetPermission :: GetPermission -> TestTree
requestGetPermission =
  req
    "GetPermission"
    "fixture/GetPermission.yaml"

requestGetResourcePolicies :: GetResourcePolicies -> TestTree
requestGetResourcePolicies =
  req
    "GetResourcePolicies"
    "fixture/GetResourcePolicies.yaml"

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

requestGetResourceShares :: GetResourceShares -> TestTree
requestGetResourceShares =
  req
    "GetResourceShares"
    "fixture/GetResourceShares.yaml"

requestListPendingInvitationResources :: ListPendingInvitationResources -> TestTree
requestListPendingInvitationResources =
  req
    "ListPendingInvitationResources"
    "fixture/ListPendingInvitationResources.yaml"

requestListPermissionVersions :: ListPermissionVersions -> TestTree
requestListPermissionVersions =
  req
    "ListPermissionVersions"
    "fixture/ListPermissionVersions.yaml"

requestListPermissions :: ListPermissions -> TestTree
requestListPermissions =
  req
    "ListPermissions"
    "fixture/ListPermissions.yaml"

requestListPrincipals :: ListPrincipals -> TestTree
requestListPrincipals =
  req
    "ListPrincipals"
    "fixture/ListPrincipals.yaml"

requestListResourceSharePermissions :: ListResourceSharePermissions -> TestTree
requestListResourceSharePermissions =
  req
    "ListResourceSharePermissions"
    "fixture/ListResourceSharePermissions.yaml"

requestListResourceTypes :: ListResourceTypes -> TestTree
requestListResourceTypes =
  req
    "ListResourceTypes"
    "fixture/ListResourceTypes.yaml"

requestListResources :: ListResources -> TestTree
requestListResources =
  req
    "ListResources"
    "fixture/ListResources.yaml"

requestPromoteResourceShareCreatedFromPolicy :: PromoteResourceShareCreatedFromPolicy -> TestTree
requestPromoteResourceShareCreatedFromPolicy =
  req
    "PromoteResourceShareCreatedFromPolicy"
    "fixture/PromoteResourceShareCreatedFromPolicy.yaml"

requestRejectResourceShareInvitation :: RejectResourceShareInvitation -> TestTree
requestRejectResourceShareInvitation =
  req
    "RejectResourceShareInvitation"
    "fixture/RejectResourceShareInvitation.yaml"

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

requestUpdateResourceShare :: UpdateResourceShare -> TestTree
requestUpdateResourceShare =
  req
    "UpdateResourceShare"
    "fixture/UpdateResourceShare.yaml"

-- Responses

responseAcceptResourceShareInvitation :: AcceptResourceShareInvitationResponse -> TestTree
responseAcceptResourceShareInvitation =
  res
    "AcceptResourceShareInvitationResponse"
    "fixture/AcceptResourceShareInvitationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AcceptResourceShareInvitation)

responseAssociateResourceShare :: AssociateResourceShareResponse -> TestTree
responseAssociateResourceShare =
  res
    "AssociateResourceShareResponse"
    "fixture/AssociateResourceShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateResourceShare)

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

responseDeleteResourceShare :: DeleteResourceShareResponse -> TestTree
responseDeleteResourceShare =
  res
    "DeleteResourceShareResponse"
    "fixture/DeleteResourceShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteResourceShare)

responseDisassociateResourceShare :: DisassociateResourceShareResponse -> TestTree
responseDisassociateResourceShare =
  res
    "DisassociateResourceShareResponse"
    "fixture/DisassociateResourceShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateResourceShare)

responseDisassociateResourceSharePermission :: DisassociateResourceSharePermissionResponse -> TestTree
responseDisassociateResourceSharePermission =
  res
    "DisassociateResourceSharePermissionResponse"
    "fixture/DisassociateResourceSharePermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateResourceSharePermission)

responseEnableSharingWithAwsOrganization :: EnableSharingWithAwsOrganizationResponse -> TestTree
responseEnableSharingWithAwsOrganization =
  res
    "EnableSharingWithAwsOrganizationResponse"
    "fixture/EnableSharingWithAwsOrganizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableSharingWithAwsOrganization)

responseGetPermission :: GetPermissionResponse -> TestTree
responseGetPermission =
  res
    "GetPermissionResponse"
    "fixture/GetPermissionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetPermission)

responseGetResourcePolicies :: GetResourcePoliciesResponse -> TestTree
responseGetResourcePolicies =
  res
    "GetResourcePoliciesResponse"
    "fixture/GetResourcePoliciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourcePolicies)

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

responseGetResourceShares :: GetResourceSharesResponse -> TestTree
responseGetResourceShares =
  res
    "GetResourceSharesResponse"
    "fixture/GetResourceSharesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetResourceShares)

responseListPendingInvitationResources :: ListPendingInvitationResourcesResponse -> TestTree
responseListPendingInvitationResources =
  res
    "ListPendingInvitationResourcesResponse"
    "fixture/ListPendingInvitationResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPendingInvitationResources)

responseListPermissionVersions :: ListPermissionVersionsResponse -> TestTree
responseListPermissionVersions =
  res
    "ListPermissionVersionsResponse"
    "fixture/ListPermissionVersionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPermissionVersions)

responseListPermissions :: ListPermissionsResponse -> TestTree
responseListPermissions =
  res
    "ListPermissionsResponse"
    "fixture/ListPermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPermissions)

responseListPrincipals :: ListPrincipalsResponse -> TestTree
responseListPrincipals =
  res
    "ListPrincipalsResponse"
    "fixture/ListPrincipalsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPrincipals)

responseListResourceSharePermissions :: ListResourceSharePermissionsResponse -> TestTree
responseListResourceSharePermissions =
  res
    "ListResourceSharePermissionsResponse"
    "fixture/ListResourceSharePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourceSharePermissions)

responseListResourceTypes :: ListResourceTypesResponse -> TestTree
responseListResourceTypes =
  res
    "ListResourceTypesResponse"
    "fixture/ListResourceTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResourceTypes)

responseListResources :: ListResourcesResponse -> TestTree
responseListResources =
  res
    "ListResourcesResponse"
    "fixture/ListResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListResources)

responsePromoteResourceShareCreatedFromPolicy :: PromoteResourceShareCreatedFromPolicyResponse -> TestTree
responsePromoteResourceShareCreatedFromPolicy =
  res
    "PromoteResourceShareCreatedFromPolicyResponse"
    "fixture/PromoteResourceShareCreatedFromPolicyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PromoteResourceShareCreatedFromPolicy)

responseRejectResourceShareInvitation :: RejectResourceShareInvitationResponse -> TestTree
responseRejectResourceShareInvitation =
  res
    "RejectResourceShareInvitationResponse"
    "fixture/RejectResourceShareInvitationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RejectResourceShareInvitation)

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

responseUpdateResourceShare :: UpdateResourceShareResponse -> TestTree
responseUpdateResourceShare =
  res
    "UpdateResourceShareResponse"
    "fixture/UpdateResourceShareResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateResourceShare)
