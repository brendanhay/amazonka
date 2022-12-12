{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.RAM
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2018-01-04@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- This is the /Resource Access Manager API Reference/. This documentation
-- provides descriptions and syntax for each of the actions and data types
-- in RAM. RAM is a service that helps you securely share your Amazon Web
-- Services resources across Amazon Web Services accounts. If you have
-- multiple Amazon Web Services accounts, you can use RAM to share those
-- resources with other accounts. If you use Organizations to manage your
-- accounts, then you share your resources with your organization or
-- organizational units (OUs). For supported resource types, you can also
-- share resources with individual Identity and Access Management (IAM)
-- roles an users.
--
-- To learn more about RAM, see the following resources:
--
-- -   <http://aws.amazon.com/ram Resource Access Manager product page>
--
-- -   <https://docs.aws.amazon.com/ram/latest/userguide/ Resource Access Manager User Guide>
module Amazonka.RAM
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** IdempotentParameterMismatchException
    _IdempotentParameterMismatchException,

    -- ** InvalidClientTokenException
    _InvalidClientTokenException,

    -- ** InvalidMaxResultsException
    _InvalidMaxResultsException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** InvalidResourceTypeException
    _InvalidResourceTypeException,

    -- ** InvalidStateTransitionException
    _InvalidStateTransitionException,

    -- ** MalformedArnException
    _MalformedArnException,

    -- ** MissingRequiredParameterException
    _MissingRequiredParameterException,

    -- ** OperationNotPermittedException
    _OperationNotPermittedException,

    -- ** ResourceArnNotFoundException
    _ResourceArnNotFoundException,

    -- ** ResourceShareInvitationAlreadyAcceptedException
    _ResourceShareInvitationAlreadyAcceptedException,

    -- ** ResourceShareInvitationAlreadyRejectedException
    _ResourceShareInvitationAlreadyRejectedException,

    -- ** ResourceShareInvitationArnNotFoundException
    _ResourceShareInvitationArnNotFoundException,

    -- ** ResourceShareInvitationExpiredException
    _ResourceShareInvitationExpiredException,

    -- ** ResourceShareLimitExceededException
    _ResourceShareLimitExceededException,

    -- ** ServerInternalException
    _ServerInternalException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** TagLimitExceededException
    _TagLimitExceededException,

    -- ** TagPolicyViolationException
    _TagPolicyViolationException,

    -- ** ThrottlingException
    _ThrottlingException,

    -- ** UnknownResourceException
    _UnknownResourceException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AcceptResourceShareInvitation
    AcceptResourceShareInvitation (AcceptResourceShareInvitation'),
    newAcceptResourceShareInvitation,
    AcceptResourceShareInvitationResponse (AcceptResourceShareInvitationResponse'),
    newAcceptResourceShareInvitationResponse,

    -- ** AssociateResourceShare
    AssociateResourceShare (AssociateResourceShare'),
    newAssociateResourceShare,
    AssociateResourceShareResponse (AssociateResourceShareResponse'),
    newAssociateResourceShareResponse,

    -- ** AssociateResourceSharePermission
    AssociateResourceSharePermission (AssociateResourceSharePermission'),
    newAssociateResourceSharePermission,
    AssociateResourceSharePermissionResponse (AssociateResourceSharePermissionResponse'),
    newAssociateResourceSharePermissionResponse,

    -- ** CreateResourceShare
    CreateResourceShare (CreateResourceShare'),
    newCreateResourceShare,
    CreateResourceShareResponse (CreateResourceShareResponse'),
    newCreateResourceShareResponse,

    -- ** DeleteResourceShare
    DeleteResourceShare (DeleteResourceShare'),
    newDeleteResourceShare,
    DeleteResourceShareResponse (DeleteResourceShareResponse'),
    newDeleteResourceShareResponse,

    -- ** DisassociateResourceShare
    DisassociateResourceShare (DisassociateResourceShare'),
    newDisassociateResourceShare,
    DisassociateResourceShareResponse (DisassociateResourceShareResponse'),
    newDisassociateResourceShareResponse,

    -- ** DisassociateResourceSharePermission
    DisassociateResourceSharePermission (DisassociateResourceSharePermission'),
    newDisassociateResourceSharePermission,
    DisassociateResourceSharePermissionResponse (DisassociateResourceSharePermissionResponse'),
    newDisassociateResourceSharePermissionResponse,

    -- ** EnableSharingWithAwsOrganization
    EnableSharingWithAwsOrganization (EnableSharingWithAwsOrganization'),
    newEnableSharingWithAwsOrganization,
    EnableSharingWithAwsOrganizationResponse (EnableSharingWithAwsOrganizationResponse'),
    newEnableSharingWithAwsOrganizationResponse,

    -- ** GetPermission
    GetPermission (GetPermission'),
    newGetPermission,
    GetPermissionResponse (GetPermissionResponse'),
    newGetPermissionResponse,

    -- ** GetResourcePolicies (Paginated)
    GetResourcePolicies (GetResourcePolicies'),
    newGetResourcePolicies,
    GetResourcePoliciesResponse (GetResourcePoliciesResponse'),
    newGetResourcePoliciesResponse,

    -- ** GetResourceShareAssociations (Paginated)
    GetResourceShareAssociations (GetResourceShareAssociations'),
    newGetResourceShareAssociations,
    GetResourceShareAssociationsResponse (GetResourceShareAssociationsResponse'),
    newGetResourceShareAssociationsResponse,

    -- ** GetResourceShareInvitations (Paginated)
    GetResourceShareInvitations (GetResourceShareInvitations'),
    newGetResourceShareInvitations,
    GetResourceShareInvitationsResponse (GetResourceShareInvitationsResponse'),
    newGetResourceShareInvitationsResponse,

    -- ** GetResourceShares (Paginated)
    GetResourceShares (GetResourceShares'),
    newGetResourceShares,
    GetResourceSharesResponse (GetResourceSharesResponse'),
    newGetResourceSharesResponse,

    -- ** ListPendingInvitationResources
    ListPendingInvitationResources (ListPendingInvitationResources'),
    newListPendingInvitationResources,
    ListPendingInvitationResourcesResponse (ListPendingInvitationResourcesResponse'),
    newListPendingInvitationResourcesResponse,

    -- ** ListPermissionVersions
    ListPermissionVersions (ListPermissionVersions'),
    newListPermissionVersions,
    ListPermissionVersionsResponse (ListPermissionVersionsResponse'),
    newListPermissionVersionsResponse,

    -- ** ListPermissions
    ListPermissions (ListPermissions'),
    newListPermissions,
    ListPermissionsResponse (ListPermissionsResponse'),
    newListPermissionsResponse,

    -- ** ListPrincipals (Paginated)
    ListPrincipals (ListPrincipals'),
    newListPrincipals,
    ListPrincipalsResponse (ListPrincipalsResponse'),
    newListPrincipalsResponse,

    -- ** ListResourceSharePermissions
    ListResourceSharePermissions (ListResourceSharePermissions'),
    newListResourceSharePermissions,
    ListResourceSharePermissionsResponse (ListResourceSharePermissionsResponse'),
    newListResourceSharePermissionsResponse,

    -- ** ListResourceTypes
    ListResourceTypes (ListResourceTypes'),
    newListResourceTypes,
    ListResourceTypesResponse (ListResourceTypesResponse'),
    newListResourceTypesResponse,

    -- ** ListResources (Paginated)
    ListResources (ListResources'),
    newListResources,
    ListResourcesResponse (ListResourcesResponse'),
    newListResourcesResponse,

    -- ** PromoteResourceShareCreatedFromPolicy
    PromoteResourceShareCreatedFromPolicy (PromoteResourceShareCreatedFromPolicy'),
    newPromoteResourceShareCreatedFromPolicy,
    PromoteResourceShareCreatedFromPolicyResponse (PromoteResourceShareCreatedFromPolicyResponse'),
    newPromoteResourceShareCreatedFromPolicyResponse,

    -- ** RejectResourceShareInvitation
    RejectResourceShareInvitation (RejectResourceShareInvitation'),
    newRejectResourceShareInvitation,
    RejectResourceShareInvitationResponse (RejectResourceShareInvitationResponse'),
    newRejectResourceShareInvitationResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** UpdateResourceShare
    UpdateResourceShare (UpdateResourceShare'),
    newUpdateResourceShare,
    UpdateResourceShareResponse (UpdateResourceShareResponse'),
    newUpdateResourceShareResponse,

    -- * Types

    -- ** ResourceOwner
    ResourceOwner (..),

    -- ** ResourceRegionScope
    ResourceRegionScope (..),

    -- ** ResourceRegionScopeFilter
    ResourceRegionScopeFilter (..),

    -- ** ResourceShareAssociationStatus
    ResourceShareAssociationStatus (..),

    -- ** ResourceShareAssociationType
    ResourceShareAssociationType (..),

    -- ** ResourceShareFeatureSet
    ResourceShareFeatureSet (..),

    -- ** ResourceShareInvitationStatus
    ResourceShareInvitationStatus (..),

    -- ** ResourceShareStatus
    ResourceShareStatus (..),

    -- ** ResourceStatus
    ResourceStatus (..),

    -- ** Principal
    Principal (Principal'),
    newPrincipal,

    -- ** Resource
    Resource (Resource'),
    newResource,

    -- ** ResourceShare
    ResourceShare (ResourceShare'),
    newResourceShare,

    -- ** ResourceShareAssociation
    ResourceShareAssociation (ResourceShareAssociation'),
    newResourceShareAssociation,

    -- ** ResourceShareInvitation
    ResourceShareInvitation (ResourceShareInvitation'),
    newResourceShareInvitation,

    -- ** ResourceSharePermissionDetail
    ResourceSharePermissionDetail (ResourceSharePermissionDetail'),
    newResourceSharePermissionDetail,

    -- ** ResourceSharePermissionSummary
    ResourceSharePermissionSummary (ResourceSharePermissionSummary'),
    newResourceSharePermissionSummary,

    -- ** ServiceNameAndResourceType
    ServiceNameAndResourceType (ServiceNameAndResourceType'),
    newServiceNameAndResourceType,

    -- ** Tag
    Tag (Tag'),
    newTag,

    -- ** TagFilter
    TagFilter (TagFilter'),
    newTagFilter,
  )
where

import Amazonka.RAM.AcceptResourceShareInvitation
import Amazonka.RAM.AssociateResourceShare
import Amazonka.RAM.AssociateResourceSharePermission
import Amazonka.RAM.CreateResourceShare
import Amazonka.RAM.DeleteResourceShare
import Amazonka.RAM.DisassociateResourceShare
import Amazonka.RAM.DisassociateResourceSharePermission
import Amazonka.RAM.EnableSharingWithAwsOrganization
import Amazonka.RAM.GetPermission
import Amazonka.RAM.GetResourcePolicies
import Amazonka.RAM.GetResourceShareAssociations
import Amazonka.RAM.GetResourceShareInvitations
import Amazonka.RAM.GetResourceShares
import Amazonka.RAM.Lens
import Amazonka.RAM.ListPendingInvitationResources
import Amazonka.RAM.ListPermissionVersions
import Amazonka.RAM.ListPermissions
import Amazonka.RAM.ListPrincipals
import Amazonka.RAM.ListResourceSharePermissions
import Amazonka.RAM.ListResourceTypes
import Amazonka.RAM.ListResources
import Amazonka.RAM.PromoteResourceShareCreatedFromPolicy
import Amazonka.RAM.RejectResourceShareInvitation
import Amazonka.RAM.TagResource
import Amazonka.RAM.Types
import Amazonka.RAM.UntagResource
import Amazonka.RAM.UpdateResourceShare
import Amazonka.RAM.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'RAM'.

-- $operations
-- Some AWS operations return results that are incomplete and require subsequent
-- requests in order to obtain the entire result set. The process of sending
-- subsequent requests to continue where a previous request left off is called
-- pagination. For example, the 'ListObjects' operation of Amazon S3 returns up to
-- 1000 objects at a time, and you must send subsequent requests with the
-- appropriate Marker in order to retrieve the next page of results.
--
-- Operations that have an 'AWSPager' instance can transparently perform subsequent
-- requests, correctly setting Markers and other request facets to iterate through
-- the entire result set of a truncated API operation. Operations which support
-- this have an additional note in the documentation.
--
-- Many operations have the ability to filter results on the server side. See the
-- individual operation parameters for details.

-- $waiters
-- Waiters poll by repeatedly sending a request until some remote success condition
-- configured by the 'Wait' specification is fulfilled. The 'Wait' specification
-- determines how many attempts should be made, in addition to delay and retry strategies.
