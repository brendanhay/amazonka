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

    -- ** AcceptResourceShareInvitation
    acceptResourceShareInvitation_clientToken,
    acceptResourceShareInvitation_resourceShareInvitationArn,
    acceptResourceShareInvitationResponse_clientToken,
    acceptResourceShareInvitationResponse_resourceShareInvitation,
    acceptResourceShareInvitationResponse_httpStatus,

    -- ** AssociateResourceShare
    associateResourceShare_clientToken,
    associateResourceShare_principals,
    associateResourceShare_resourceArns,
    associateResourceShare_resourceShareArn,
    associateResourceShareResponse_clientToken,
    associateResourceShareResponse_resourceShareAssociations,
    associateResourceShareResponse_httpStatus,

    -- ** AssociateResourceSharePermission
    associateResourceSharePermission_clientToken,
    associateResourceSharePermission_permissionVersion,
    associateResourceSharePermission_replace,
    associateResourceSharePermission_resourceShareArn,
    associateResourceSharePermission_permissionArn,
    associateResourceSharePermissionResponse_clientToken,
    associateResourceSharePermissionResponse_returnValue,
    associateResourceSharePermissionResponse_httpStatus,

    -- ** CreateResourceShare
    createResourceShare_tags,
    createResourceShare_clientToken,
    createResourceShare_permissionArns,
    createResourceShare_principals,
    createResourceShare_resourceArns,
    createResourceShare_allowExternalPrincipals,
    createResourceShare_name,
    createResourceShareResponse_clientToken,
    createResourceShareResponse_resourceShare,
    createResourceShareResponse_httpStatus,

    -- ** DeleteResourceShare
    deleteResourceShare_clientToken,
    deleteResourceShare_resourceShareArn,
    deleteResourceShareResponse_clientToken,
    deleteResourceShareResponse_returnValue,
    deleteResourceShareResponse_httpStatus,

    -- ** DisassociateResourceShare
    disassociateResourceShare_clientToken,
    disassociateResourceShare_principals,
    disassociateResourceShare_resourceArns,
    disassociateResourceShare_resourceShareArn,
    disassociateResourceShareResponse_clientToken,
    disassociateResourceShareResponse_resourceShareAssociations,
    disassociateResourceShareResponse_httpStatus,

    -- ** DisassociateResourceSharePermission
    disassociateResourceSharePermission_clientToken,
    disassociateResourceSharePermission_resourceShareArn,
    disassociateResourceSharePermission_permissionArn,
    disassociateResourceSharePermissionResponse_clientToken,
    disassociateResourceSharePermissionResponse_returnValue,
    disassociateResourceSharePermissionResponse_httpStatus,

    -- ** EnableSharingWithAwsOrganization
    enableSharingWithAwsOrganizationResponse_returnValue,
    enableSharingWithAwsOrganizationResponse_httpStatus,

    -- ** GetPermission
    getPermission_permissionVersion,
    getPermission_permissionArn,
    getPermissionResponse_permission,
    getPermissionResponse_httpStatus,

    -- ** GetResourcePolicies
    getResourcePolicies_principal,
    getResourcePolicies_nextToken,
    getResourcePolicies_maxResults,
    getResourcePolicies_resourceArns,
    getResourcePoliciesResponse_nextToken,
    getResourcePoliciesResponse_policies,
    getResourcePoliciesResponse_httpStatus,

    -- ** GetResourceShareAssociations
    getResourceShareAssociations_principal,
    getResourceShareAssociations_nextToken,
    getResourceShareAssociations_associationStatus,
    getResourceShareAssociations_maxResults,
    getResourceShareAssociations_resourceShareArns,
    getResourceShareAssociations_resourceArn,
    getResourceShareAssociations_associationType,
    getResourceShareAssociationsResponse_nextToken,
    getResourceShareAssociationsResponse_resourceShareAssociations,
    getResourceShareAssociationsResponse_httpStatus,

    -- ** GetResourceShareInvitations
    getResourceShareInvitations_nextToken,
    getResourceShareInvitations_maxResults,
    getResourceShareInvitations_resourceShareArns,
    getResourceShareInvitations_resourceShareInvitationArns,
    getResourceShareInvitationsResponse_resourceShareInvitations,
    getResourceShareInvitationsResponse_nextToken,
    getResourceShareInvitationsResponse_httpStatus,

    -- ** GetResourceShares
    getResourceShares_name,
    getResourceShares_nextToken,
    getResourceShares_resourceShareStatus,
    getResourceShares_permissionArn,
    getResourceShares_tagFilters,
    getResourceShares_maxResults,
    getResourceShares_resourceShareArns,
    getResourceShares_resourceOwner,
    getResourceSharesResponse_nextToken,
    getResourceSharesResponse_resourceShares,
    getResourceSharesResponse_httpStatus,

    -- ** ListPendingInvitationResources
    listPendingInvitationResources_nextToken,
    listPendingInvitationResources_resourceRegionScope,
    listPendingInvitationResources_maxResults,
    listPendingInvitationResources_resourceShareInvitationArn,
    listPendingInvitationResourcesResponse_nextToken,
    listPendingInvitationResourcesResponse_resources,
    listPendingInvitationResourcesResponse_httpStatus,

    -- ** ListPermissionVersions
    listPermissionVersions_nextToken,
    listPermissionVersions_maxResults,
    listPermissionVersions_permissionArn,
    listPermissionVersionsResponse_nextToken,
    listPermissionVersionsResponse_permissions,
    listPermissionVersionsResponse_httpStatus,

    -- ** ListPermissions
    listPermissions_resourceType,
    listPermissions_nextToken,
    listPermissions_maxResults,
    listPermissionsResponse_nextToken,
    listPermissionsResponse_permissions,
    listPermissionsResponse_httpStatus,

    -- ** ListPrincipals
    listPrincipals_resourceType,
    listPrincipals_nextToken,
    listPrincipals_principals,
    listPrincipals_maxResults,
    listPrincipals_resourceShareArns,
    listPrincipals_resourceArn,
    listPrincipals_resourceOwner,
    listPrincipalsResponse_nextToken,
    listPrincipalsResponse_principals,
    listPrincipalsResponse_httpStatus,

    -- ** ListResourceSharePermissions
    listResourceSharePermissions_nextToken,
    listResourceSharePermissions_maxResults,
    listResourceSharePermissions_resourceShareArn,
    listResourceSharePermissionsResponse_nextToken,
    listResourceSharePermissionsResponse_permissions,
    listResourceSharePermissionsResponse_httpStatus,

    -- ** ListResourceTypes
    listResourceTypes_nextToken,
    listResourceTypes_resourceRegionScope,
    listResourceTypes_maxResults,
    listResourceTypesResponse_nextToken,
    listResourceTypesResponse_resourceTypes,
    listResourceTypesResponse_httpStatus,

    -- ** ListResources
    listResources_principal,
    listResources_resourceType,
    listResources_nextToken,
    listResources_resourceRegionScope,
    listResources_maxResults,
    listResources_resourceShareArns,
    listResources_resourceArns,
    listResources_resourceOwner,
    listResourcesResponse_nextToken,
    listResourcesResponse_resources,
    listResourcesResponse_httpStatus,

    -- ** PromoteResourceShareCreatedFromPolicy
    promoteResourceShareCreatedFromPolicy_resourceShareArn,
    promoteResourceShareCreatedFromPolicyResponse_returnValue,
    promoteResourceShareCreatedFromPolicyResponse_httpStatus,

    -- ** RejectResourceShareInvitation
    rejectResourceShareInvitation_clientToken,
    rejectResourceShareInvitation_resourceShareInvitationArn,
    rejectResourceShareInvitationResponse_clientToken,
    rejectResourceShareInvitationResponse_resourceShareInvitation,
    rejectResourceShareInvitationResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceShareArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceShareArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateResourceShare
    updateResourceShare_name,
    updateResourceShare_clientToken,
    updateResourceShare_allowExternalPrincipals,
    updateResourceShare_resourceShareArn,
    updateResourceShareResponse_clientToken,
    updateResourceShareResponse_resourceShare,
    updateResourceShareResponse_httpStatus,

    -- * Types

    -- ** Principal
    principal_external,
    principal_resourceShareArn,
    principal_lastUpdatedTime,
    principal_id,
    principal_creationTime,

    -- ** Resource
    resource_type,
    resource_resourceRegionScope,
    resource_arn,
    resource_resourceShareArn,
    resource_status,
    resource_lastUpdatedTime,
    resource_creationTime,
    resource_statusMessage,
    resource_resourceGroupArn,

    -- ** ResourceShare
    resourceShare_tags,
    resourceShare_name,
    resourceShare_resourceShareArn,
    resourceShare_status,
    resourceShare_lastUpdatedTime,
    resourceShare_featureSet,
    resourceShare_creationTime,
    resourceShare_statusMessage,
    resourceShare_owningAccountId,
    resourceShare_allowExternalPrincipals,

    -- ** ResourceShareAssociation
    resourceShareAssociation_external,
    resourceShareAssociation_associationType,
    resourceShareAssociation_associatedEntity,
    resourceShareAssociation_resourceShareArn,
    resourceShareAssociation_status,
    resourceShareAssociation_lastUpdatedTime,
    resourceShareAssociation_creationTime,
    resourceShareAssociation_statusMessage,
    resourceShareAssociation_resourceShareName,

    -- ** ResourceShareInvitation
    resourceShareInvitation_resourceShareArn,
    resourceShareInvitation_status,
    resourceShareInvitation_resourceShareAssociations,
    resourceShareInvitation_invitationTimestamp,
    resourceShareInvitation_senderAccountId,
    resourceShareInvitation_receiverAccountId,
    resourceShareInvitation_receiverArn,
    resourceShareInvitation_resourceShareInvitationArn,
    resourceShareInvitation_resourceShareName,

    -- ** ResourceSharePermissionDetail
    resourceSharePermissionDetail_resourceType,
    resourceSharePermissionDetail_name,
    resourceSharePermissionDetail_arn,
    resourceSharePermissionDetail_permission,
    resourceSharePermissionDetail_lastUpdatedTime,
    resourceSharePermissionDetail_defaultVersion,
    resourceSharePermissionDetail_creationTime,
    resourceSharePermissionDetail_isResourceTypeDefault,
    resourceSharePermissionDetail_version,

    -- ** ResourceSharePermissionSummary
    resourceSharePermissionSummary_resourceType,
    resourceSharePermissionSummary_name,
    resourceSharePermissionSummary_arn,
    resourceSharePermissionSummary_status,
    resourceSharePermissionSummary_lastUpdatedTime,
    resourceSharePermissionSummary_defaultVersion,
    resourceSharePermissionSummary_creationTime,
    resourceSharePermissionSummary_isResourceTypeDefault,
    resourceSharePermissionSummary_version,

    -- ** ServiceNameAndResourceType
    serviceNameAndResourceType_resourceType,
    serviceNameAndResourceType_resourceRegionScope,
    serviceNameAndResourceType_serviceName,

    -- ** Tag
    tag_key,
    tag_value,

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
import Amazonka.RAM.ListPermissionVersions
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
