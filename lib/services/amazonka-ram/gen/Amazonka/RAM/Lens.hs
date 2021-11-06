{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RAM.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RAM.Lens
  ( -- * Operations

    -- ** PromoteResourceShareCreatedFromPolicy
    promoteResourceShareCreatedFromPolicy_resourceShareArn,
    promoteResourceShareCreatedFromPolicyResponse_returnValue,
    promoteResourceShareCreatedFromPolicyResponse_httpStatus,

    -- ** GetResourceShares
    getResourceShares_tagFilters,
    getResourceShares_nextToken,
    getResourceShares_name,
    getResourceShares_resourceShareStatus,
    getResourceShares_permissionArn,
    getResourceShares_maxResults,
    getResourceShares_resourceShareArns,
    getResourceShares_resourceOwner,
    getResourceSharesResponse_resourceShares,
    getResourceSharesResponse_nextToken,
    getResourceSharesResponse_httpStatus,

    -- ** GetResourcePolicies
    getResourcePolicies_nextToken,
    getResourcePolicies_principal,
    getResourcePolicies_maxResults,
    getResourcePolicies_resourceArns,
    getResourcePoliciesResponse_nextToken,
    getResourcePoliciesResponse_policies,
    getResourcePoliciesResponse_httpStatus,

    -- ** ListPendingInvitationResources
    listPendingInvitationResources_nextToken,
    listPendingInvitationResources_maxResults,
    listPendingInvitationResources_resourceShareInvitationArn,
    listPendingInvitationResourcesResponse_resources,
    listPendingInvitationResourcesResponse_nextToken,
    listPendingInvitationResourcesResponse_httpStatus,

    -- ** GetPermission
    getPermission_permissionVersion,
    getPermission_permissionArn,
    getPermissionResponse_permission,
    getPermissionResponse_httpStatus,

    -- ** EnableSharingWithAwsOrganization
    enableSharingWithAwsOrganizationResponse_returnValue,
    enableSharingWithAwsOrganizationResponse_httpStatus,

    -- ** AssociateResourceSharePermission
    associateResourceSharePermission_replace,
    associateResourceSharePermission_clientToken,
    associateResourceSharePermission_permissionVersion,
    associateResourceSharePermission_resourceShareArn,
    associateResourceSharePermission_permissionArn,
    associateResourceSharePermissionResponse_clientToken,
    associateResourceSharePermissionResponse_returnValue,
    associateResourceSharePermissionResponse_httpStatus,

    -- ** CreateResourceShare
    createResourceShare_clientToken,
    createResourceShare_allowExternalPrincipals,
    createResourceShare_principals,
    createResourceShare_resourceArns,
    createResourceShare_permissionArns,
    createResourceShare_tags,
    createResourceShare_name,
    createResourceShareResponse_clientToken,
    createResourceShareResponse_resourceShare,
    createResourceShareResponse_httpStatus,

    -- ** ListPrincipals
    listPrincipals_resourceType,
    listPrincipals_principals,
    listPrincipals_nextToken,
    listPrincipals_resourceArn,
    listPrincipals_maxResults,
    listPrincipals_resourceShareArns,
    listPrincipals_resourceOwner,
    listPrincipalsResponse_principals,
    listPrincipalsResponse_nextToken,
    listPrincipalsResponse_httpStatus,

    -- ** ListResources
    listResources_resourceType,
    listResources_nextToken,
    listResources_resourceArns,
    listResources_principal,
    listResources_maxResults,
    listResources_resourceShareArns,
    listResources_resourceOwner,
    listResourcesResponse_resources,
    listResourcesResponse_nextToken,
    listResourcesResponse_httpStatus,

    -- ** AcceptResourceShareInvitation
    acceptResourceShareInvitation_clientToken,
    acceptResourceShareInvitation_resourceShareInvitationArn,
    acceptResourceShareInvitationResponse_clientToken,
    acceptResourceShareInvitationResponse_resourceShareInvitation,
    acceptResourceShareInvitationResponse_httpStatus,

    -- ** DeleteResourceShare
    deleteResourceShare_clientToken,
    deleteResourceShare_resourceShareArn,
    deleteResourceShareResponse_clientToken,
    deleteResourceShareResponse_returnValue,
    deleteResourceShareResponse_httpStatus,

    -- ** UpdateResourceShare
    updateResourceShare_clientToken,
    updateResourceShare_allowExternalPrincipals,
    updateResourceShare_name,
    updateResourceShare_resourceShareArn,
    updateResourceShareResponse_clientToken,
    updateResourceShareResponse_resourceShare,
    updateResourceShareResponse_httpStatus,

    -- ** RejectResourceShareInvitation
    rejectResourceShareInvitation_clientToken,
    rejectResourceShareInvitation_resourceShareInvitationArn,
    rejectResourceShareInvitationResponse_clientToken,
    rejectResourceShareInvitationResponse_resourceShareInvitation,
    rejectResourceShareInvitationResponse_httpStatus,

    -- ** ListPermissions
    listPermissions_resourceType,
    listPermissions_nextToken,
    listPermissions_maxResults,
    listPermissionsResponse_nextToken,
    listPermissionsResponse_permissions,
    listPermissionsResponse_httpStatus,

    -- ** DisassociateResourceShare
    disassociateResourceShare_clientToken,
    disassociateResourceShare_principals,
    disassociateResourceShare_resourceArns,
    disassociateResourceShare_resourceShareArn,
    disassociateResourceShareResponse_clientToken,
    disassociateResourceShareResponse_resourceShareAssociations,
    disassociateResourceShareResponse_httpStatus,

    -- ** ListResourceSharePermissions
    listResourceSharePermissions_nextToken,
    listResourceSharePermissions_maxResults,
    listResourceSharePermissions_resourceShareArn,
    listResourceSharePermissionsResponse_nextToken,
    listResourceSharePermissionsResponse_permissions,
    listResourceSharePermissionsResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceShareArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** ListResourceTypes
    listResourceTypes_nextToken,
    listResourceTypes_maxResults,
    listResourceTypesResponse_nextToken,
    listResourceTypesResponse_resourceTypes,
    listResourceTypesResponse_httpStatus,

    -- ** GetResourceShareAssociations
    getResourceShareAssociations_nextToken,
    getResourceShareAssociations_resourceArn,
    getResourceShareAssociations_principal,
    getResourceShareAssociations_associationStatus,
    getResourceShareAssociations_maxResults,
    getResourceShareAssociations_resourceShareArns,
    getResourceShareAssociations_associationType,
    getResourceShareAssociationsResponse_resourceShareAssociations,
    getResourceShareAssociationsResponse_nextToken,
    getResourceShareAssociationsResponse_httpStatus,

    -- ** GetResourceShareInvitations
    getResourceShareInvitations_nextToken,
    getResourceShareInvitations_resourceShareInvitationArns,
    getResourceShareInvitations_maxResults,
    getResourceShareInvitations_resourceShareArns,
    getResourceShareInvitationsResponse_resourceShareInvitations,
    getResourceShareInvitationsResponse_nextToken,
    getResourceShareInvitationsResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceShareArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** AssociateResourceShare
    associateResourceShare_clientToken,
    associateResourceShare_principals,
    associateResourceShare_resourceArns,
    associateResourceShare_resourceShareArn,
    associateResourceShareResponse_clientToken,
    associateResourceShareResponse_resourceShareAssociations,
    associateResourceShareResponse_httpStatus,

    -- ** DisassociateResourceSharePermission
    disassociateResourceSharePermission_clientToken,
    disassociateResourceSharePermission_resourceShareArn,
    disassociateResourceSharePermission_permissionArn,
    disassociateResourceSharePermissionResponse_clientToken,
    disassociateResourceSharePermissionResponse_returnValue,
    disassociateResourceSharePermissionResponse_httpStatus,

    -- * Types

    -- ** Principal
    principal_creationTime,
    principal_resourceShareArn,
    principal_lastUpdatedTime,
    principal_external,
    principal_id,

    -- ** Resource
    resource_creationTime,
    resource_status,
    resource_resourceShareArn,
    resource_lastUpdatedTime,
    resource_arn,
    resource_resourceGroupArn,
    resource_statusMessage,
    resource_type,

    -- ** ResourceShare
    resourceShare_creationTime,
    resourceShare_status,
    resourceShare_resourceShareArn,
    resourceShare_owningAccountId,
    resourceShare_lastUpdatedTime,
    resourceShare_allowExternalPrincipals,
    resourceShare_name,
    resourceShare_statusMessage,
    resourceShare_featureSet,
    resourceShare_tags,

    -- ** ResourceShareAssociation
    resourceShareAssociation_creationTime,
    resourceShareAssociation_status,
    resourceShareAssociation_resourceShareArn,
    resourceShareAssociation_lastUpdatedTime,
    resourceShareAssociation_external,
    resourceShareAssociation_resourceShareName,
    resourceShareAssociation_associatedEntity,
    resourceShareAssociation_associationType,
    resourceShareAssociation_statusMessage,

    -- ** ResourceShareInvitation
    resourceShareInvitation_status,
    resourceShareInvitation_senderAccountId,
    resourceShareInvitation_resourceShareArn,
    resourceShareInvitation_receiverAccountId,
    resourceShareInvitation_resourceShareAssociations,
    resourceShareInvitation_resourceShareName,
    resourceShareInvitation_receiverArn,
    resourceShareInvitation_invitationTimestamp,
    resourceShareInvitation_resourceShareInvitationArn,

    -- ** ResourceSharePermissionDetail
    resourceSharePermissionDetail_creationTime,
    resourceSharePermissionDetail_resourceType,
    resourceSharePermissionDetail_lastUpdatedTime,
    resourceSharePermissionDetail_arn,
    resourceSharePermissionDetail_defaultVersion,
    resourceSharePermissionDetail_name,
    resourceSharePermissionDetail_version,
    resourceSharePermissionDetail_isResourceTypeDefault,
    resourceSharePermissionDetail_permission,

    -- ** ResourceSharePermissionSummary
    resourceSharePermissionSummary_creationTime,
    resourceSharePermissionSummary_status,
    resourceSharePermissionSummary_resourceType,
    resourceSharePermissionSummary_lastUpdatedTime,
    resourceSharePermissionSummary_arn,
    resourceSharePermissionSummary_defaultVersion,
    resourceSharePermissionSummary_name,
    resourceSharePermissionSummary_version,
    resourceSharePermissionSummary_isResourceTypeDefault,

    -- ** ServiceNameAndResourceType
    serviceNameAndResourceType_resourceType,
    serviceNameAndResourceType_serviceName,

    -- ** Tag
    tag_value,
    tag_key,

    -- ** TagFilter
    tagFilter_tagValues,
    tagFilter_tagKey,
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
import Amazonka.RAM.ListPendingInvitationResources
import Amazonka.RAM.ListPermissions
import Amazonka.RAM.ListPrincipals
import Amazonka.RAM.ListResourceSharePermissions
import Amazonka.RAM.ListResourceTypes
import Amazonka.RAM.ListResources
import Amazonka.RAM.PromoteResourceShareCreatedFromPolicy
import Amazonka.RAM.RejectResourceShareInvitation
import Amazonka.RAM.TagResource
import Amazonka.RAM.Types.Principal
import Amazonka.RAM.Types.Resource
import Amazonka.RAM.Types.ResourceShare
import Amazonka.RAM.Types.ResourceShareAssociation
import Amazonka.RAM.Types.ResourceShareInvitation
import Amazonka.RAM.Types.ResourceSharePermissionDetail
import Amazonka.RAM.Types.ResourceSharePermissionSummary
import Amazonka.RAM.Types.ServiceNameAndResourceType
import Amazonka.RAM.Types.Tag
import Amazonka.RAM.Types.TagFilter
import Amazonka.RAM.UntagResource
import Amazonka.RAM.UpdateResourceShare
