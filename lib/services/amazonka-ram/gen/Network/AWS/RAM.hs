{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Network.AWS.RAM
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- Services resources across Amazon Web Services accounts and within your
-- organization or organizational units (OUs) in Organizations. For
-- supported resource types, you can also share resources with IAM roles
-- and IAM users. If you have multiple Amazon Web Services accounts, you
-- can use RAM to share those resources with other accounts.
--
-- To learn more about RAM, see the following resources:
--
-- -   <http://aws.amazon.com/ram Resource Access Manager product page>
--
-- -   <https://docs.aws.amazon.com/ram/latest/userguide/ Resource Access Manager User Guide>
module Network.AWS.RAM
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** InvalidClientTokenException
    _InvalidClientTokenException,

    -- ** TagPolicyViolationException
    _TagPolicyViolationException,

    -- ** InvalidParameterException
    _InvalidParameterException,

    -- ** InvalidResourceTypeException
    _InvalidResourceTypeException,

    -- ** ResourceShareInvitationArnNotFoundException
    _ResourceShareInvitationArnNotFoundException,

    -- ** InvalidNextTokenException
    _InvalidNextTokenException,

    -- ** MalformedArnException
    _MalformedArnException,

    -- ** ServerInternalException
    _ServerInternalException,

    -- ** UnknownResourceException
    _UnknownResourceException,

    -- ** OperationNotPermittedException
    _OperationNotPermittedException,

    -- ** InvalidMaxResultsException
    _InvalidMaxResultsException,

    -- ** ServiceUnavailableException
    _ServiceUnavailableException,

    -- ** ResourceArnNotFoundException
    _ResourceArnNotFoundException,

    -- ** IdempotentParameterMismatchException
    _IdempotentParameterMismatchException,

    -- ** MissingRequiredParameterException
    _MissingRequiredParameterException,

    -- ** TagLimitExceededException
    _TagLimitExceededException,

    -- ** InvalidStateTransitionException
    _InvalidStateTransitionException,

    -- ** ResourceShareInvitationAlreadyAcceptedException
    _ResourceShareInvitationAlreadyAcceptedException,

    -- ** ResourceShareLimitExceededException
    _ResourceShareLimitExceededException,

    -- ** ResourceShareInvitationExpiredException
    _ResourceShareInvitationExpiredException,

    -- ** ResourceShareInvitationAlreadyRejectedException
    _ResourceShareInvitationAlreadyRejectedException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** PromoteResourceShareCreatedFromPolicy
    PromoteResourceShareCreatedFromPolicy (PromoteResourceShareCreatedFromPolicy'),
    newPromoteResourceShareCreatedFromPolicy,
    PromoteResourceShareCreatedFromPolicyResponse (PromoteResourceShareCreatedFromPolicyResponse'),
    newPromoteResourceShareCreatedFromPolicyResponse,

    -- ** GetResourceShares (Paginated)
    GetResourceShares (GetResourceShares'),
    newGetResourceShares,
    GetResourceSharesResponse (GetResourceSharesResponse'),
    newGetResourceSharesResponse,

    -- ** GetResourcePolicies (Paginated)
    GetResourcePolicies (GetResourcePolicies'),
    newGetResourcePolicies,
    GetResourcePoliciesResponse (GetResourcePoliciesResponse'),
    newGetResourcePoliciesResponse,

    -- ** ListPendingInvitationResources
    ListPendingInvitationResources (ListPendingInvitationResources'),
    newListPendingInvitationResources,
    ListPendingInvitationResourcesResponse (ListPendingInvitationResourcesResponse'),
    newListPendingInvitationResourcesResponse,

    -- ** GetPermission
    GetPermission (GetPermission'),
    newGetPermission,
    GetPermissionResponse (GetPermissionResponse'),
    newGetPermissionResponse,

    -- ** EnableSharingWithAwsOrganization
    EnableSharingWithAwsOrganization (EnableSharingWithAwsOrganization'),
    newEnableSharingWithAwsOrganization,
    EnableSharingWithAwsOrganizationResponse (EnableSharingWithAwsOrganizationResponse'),
    newEnableSharingWithAwsOrganizationResponse,

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

    -- ** ListPrincipals (Paginated)
    ListPrincipals (ListPrincipals'),
    newListPrincipals,
    ListPrincipalsResponse (ListPrincipalsResponse'),
    newListPrincipalsResponse,

    -- ** ListResources (Paginated)
    ListResources (ListResources'),
    newListResources,
    ListResourcesResponse (ListResourcesResponse'),
    newListResourcesResponse,

    -- ** AcceptResourceShareInvitation
    AcceptResourceShareInvitation (AcceptResourceShareInvitation'),
    newAcceptResourceShareInvitation,
    AcceptResourceShareInvitationResponse (AcceptResourceShareInvitationResponse'),
    newAcceptResourceShareInvitationResponse,

    -- ** DeleteResourceShare
    DeleteResourceShare (DeleteResourceShare'),
    newDeleteResourceShare,
    DeleteResourceShareResponse (DeleteResourceShareResponse'),
    newDeleteResourceShareResponse,

    -- ** UpdateResourceShare
    UpdateResourceShare (UpdateResourceShare'),
    newUpdateResourceShare,
    UpdateResourceShareResponse (UpdateResourceShareResponse'),
    newUpdateResourceShareResponse,

    -- ** RejectResourceShareInvitation
    RejectResourceShareInvitation (RejectResourceShareInvitation'),
    newRejectResourceShareInvitation,
    RejectResourceShareInvitationResponse (RejectResourceShareInvitationResponse'),
    newRejectResourceShareInvitationResponse,

    -- ** ListPermissions
    ListPermissions (ListPermissions'),
    newListPermissions,
    ListPermissionsResponse (ListPermissionsResponse'),
    newListPermissionsResponse,

    -- ** DisassociateResourceShare
    DisassociateResourceShare (DisassociateResourceShare'),
    newDisassociateResourceShare,
    DisassociateResourceShareResponse (DisassociateResourceShareResponse'),
    newDisassociateResourceShareResponse,

    -- ** ListResourceSharePermissions
    ListResourceSharePermissions (ListResourceSharePermissions'),
    newListResourceSharePermissions,
    ListResourceSharePermissionsResponse (ListResourceSharePermissionsResponse'),
    newListResourceSharePermissionsResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** ListResourceTypes
    ListResourceTypes (ListResourceTypes'),
    newListResourceTypes,
    ListResourceTypesResponse (ListResourceTypesResponse'),
    newListResourceTypesResponse,

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

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- ** AssociateResourceShare
    AssociateResourceShare (AssociateResourceShare'),
    newAssociateResourceShare,
    AssociateResourceShareResponse (AssociateResourceShareResponse'),
    newAssociateResourceShareResponse,

    -- ** DisassociateResourceSharePermission
    DisassociateResourceSharePermission (DisassociateResourceSharePermission'),
    newDisassociateResourceSharePermission,
    DisassociateResourceSharePermissionResponse (DisassociateResourceSharePermissionResponse'),
    newDisassociateResourceSharePermissionResponse,

    -- * Types

    -- ** ResourceOwner
    ResourceOwner (..),

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

import Network.AWS.RAM.AcceptResourceShareInvitation
import Network.AWS.RAM.AssociateResourceShare
import Network.AWS.RAM.AssociateResourceSharePermission
import Network.AWS.RAM.CreateResourceShare
import Network.AWS.RAM.DeleteResourceShare
import Network.AWS.RAM.DisassociateResourceShare
import Network.AWS.RAM.DisassociateResourceSharePermission
import Network.AWS.RAM.EnableSharingWithAwsOrganization
import Network.AWS.RAM.GetPermission
import Network.AWS.RAM.GetResourcePolicies
import Network.AWS.RAM.GetResourceShareAssociations
import Network.AWS.RAM.GetResourceShareInvitations
import Network.AWS.RAM.GetResourceShares
import Network.AWS.RAM.Lens
import Network.AWS.RAM.ListPendingInvitationResources
import Network.AWS.RAM.ListPermissions
import Network.AWS.RAM.ListPrincipals
import Network.AWS.RAM.ListResourceSharePermissions
import Network.AWS.RAM.ListResourceTypes
import Network.AWS.RAM.ListResources
import Network.AWS.RAM.PromoteResourceShareCreatedFromPolicy
import Network.AWS.RAM.RejectResourceShareInvitation
import Network.AWS.RAM.TagResource
import Network.AWS.RAM.Types
import Network.AWS.RAM.UntagResource
import Network.AWS.RAM.UpdateResourceShare
import Network.AWS.RAM.Waiters

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
