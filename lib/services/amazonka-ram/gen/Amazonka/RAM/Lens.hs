{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RAM.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    createResourceShare_allowExternalPrincipals,
    createResourceShare_clientToken,
    createResourceShare_permissionArns,
    createResourceShare_principals,
    createResourceShare_resourceArns,
    createResourceShare_tags,
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
    getResourcePolicies_maxResults,
    getResourcePolicies_nextToken,
    getResourcePolicies_principal,
    getResourcePolicies_resourceArns,
    getResourcePoliciesResponse_nextToken,
    getResourcePoliciesResponse_policies,
    getResourcePoliciesResponse_httpStatus,

    -- ** GetResourceShareAssociations
    getResourceShareAssociations_associationStatus,
    getResourceShareAssociations_maxResults,
    getResourceShareAssociations_nextToken,
    getResourceShareAssociations_principal,
    getResourceShareAssociations_resourceArn,
    getResourceShareAssociations_resourceShareArns,
    getResourceShareAssociations_associationType,
    getResourceShareAssociationsResponse_nextToken,
    getResourceShareAssociationsResponse_resourceShareAssociations,
    getResourceShareAssociationsResponse_httpStatus,

    -- ** GetResourceShareInvitations
    getResourceShareInvitations_maxResults,
    getResourceShareInvitations_nextToken,
    getResourceShareInvitations_resourceShareArns,
    getResourceShareInvitations_resourceShareInvitationArns,
    getResourceShareInvitationsResponse_nextToken,
    getResourceShareInvitationsResponse_resourceShareInvitations,
    getResourceShareInvitationsResponse_httpStatus,

    -- ** GetResourceShares
    getResourceShares_maxResults,
    getResourceShares_name,
    getResourceShares_nextToken,
    getResourceShares_permissionArn,
    getResourceShares_resourceShareArns,
    getResourceShares_resourceShareStatus,
    getResourceShares_tagFilters,
    getResourceShares_resourceOwner,
    getResourceSharesResponse_nextToken,
    getResourceSharesResponse_resourceShares,
    getResourceSharesResponse_httpStatus,

    -- ** ListPendingInvitationResources
    listPendingInvitationResources_maxResults,
    listPendingInvitationResources_nextToken,
    listPendingInvitationResources_resourceRegionScope,
    listPendingInvitationResources_resourceShareInvitationArn,
    listPendingInvitationResourcesResponse_nextToken,
    listPendingInvitationResourcesResponse_resources,
    listPendingInvitationResourcesResponse_httpStatus,

    -- ** ListPermissionVersions
    listPermissionVersions_maxResults,
    listPermissionVersions_nextToken,
    listPermissionVersions_permissionArn,
    listPermissionVersionsResponse_nextToken,
    listPermissionVersionsResponse_permissions,
    listPermissionVersionsResponse_httpStatus,

    -- ** ListPermissions
    listPermissions_maxResults,
    listPermissions_nextToken,
    listPermissions_resourceType,
    listPermissionsResponse_nextToken,
    listPermissionsResponse_permissions,
    listPermissionsResponse_httpStatus,

    -- ** ListPrincipals
    listPrincipals_maxResults,
    listPrincipals_nextToken,
    listPrincipals_principals,
    listPrincipals_resourceArn,
    listPrincipals_resourceShareArns,
    listPrincipals_resourceType,
    listPrincipals_resourceOwner,
    listPrincipalsResponse_nextToken,
    listPrincipalsResponse_principals,
    listPrincipalsResponse_httpStatus,

    -- ** ListResourceSharePermissions
    listResourceSharePermissions_maxResults,
    listResourceSharePermissions_nextToken,
    listResourceSharePermissions_resourceShareArn,
    listResourceSharePermissionsResponse_nextToken,
    listResourceSharePermissionsResponse_permissions,
    listResourceSharePermissionsResponse_httpStatus,

    -- ** ListResourceTypes
    listResourceTypes_maxResults,
    listResourceTypes_nextToken,
    listResourceTypes_resourceRegionScope,
    listResourceTypesResponse_nextToken,
    listResourceTypesResponse_resourceTypes,
    listResourceTypesResponse_httpStatus,

    -- ** ListResources
    listResources_maxResults,
    listResources_nextToken,
    listResources_principal,
    listResources_resourceArns,
    listResources_resourceRegionScope,
    listResources_resourceShareArns,
    listResources_resourceType,
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
    updateResourceShare_allowExternalPrincipals,
    updateResourceShare_clientToken,
    updateResourceShare_name,
    updateResourceShare_resourceShareArn,
    updateResourceShareResponse_clientToken,
    updateResourceShareResponse_resourceShare,
    updateResourceShareResponse_httpStatus,

    -- * Types

    -- ** Principal
    principal_creationTime,
    principal_external,
    principal_id,
    principal_lastUpdatedTime,
    principal_resourceShareArn,

    -- ** Resource
    resource_arn,
    resource_creationTime,
    resource_lastUpdatedTime,
    resource_resourceGroupArn,
    resource_resourceRegionScope,
    resource_resourceShareArn,
    resource_status,
    resource_statusMessage,
    resource_type,

    -- ** ResourceShare
    resourceShare_allowExternalPrincipals,
    resourceShare_creationTime,
    resourceShare_featureSet,
    resourceShare_lastUpdatedTime,
    resourceShare_name,
    resourceShare_owningAccountId,
    resourceShare_resourceShareArn,
    resourceShare_status,
    resourceShare_statusMessage,
    resourceShare_tags,

    -- ** ResourceShareAssociation
    resourceShareAssociation_associatedEntity,
    resourceShareAssociation_associationType,
    resourceShareAssociation_creationTime,
    resourceShareAssociation_external,
    resourceShareAssociation_lastUpdatedTime,
    resourceShareAssociation_resourceShareArn,
    resourceShareAssociation_resourceShareName,
    resourceShareAssociation_status,
    resourceShareAssociation_statusMessage,

    -- ** ResourceShareInvitation
    resourceShareInvitation_invitationTimestamp,
    resourceShareInvitation_receiverAccountId,
    resourceShareInvitation_receiverArn,
    resourceShareInvitation_resourceShareArn,
    resourceShareInvitation_resourceShareAssociations,
    resourceShareInvitation_resourceShareInvitationArn,
    resourceShareInvitation_resourceShareName,
    resourceShareInvitation_senderAccountId,
    resourceShareInvitation_status,

    -- ** ResourceSharePermissionDetail
    resourceSharePermissionDetail_arn,
    resourceSharePermissionDetail_creationTime,
    resourceSharePermissionDetail_defaultVersion,
    resourceSharePermissionDetail_isResourceTypeDefault,
    resourceSharePermissionDetail_lastUpdatedTime,
    resourceSharePermissionDetail_name,
    resourceSharePermissionDetail_permission,
    resourceSharePermissionDetail_resourceType,
    resourceSharePermissionDetail_version,

    -- ** ResourceSharePermissionSummary
    resourceSharePermissionSummary_arn,
    resourceSharePermissionSummary_creationTime,
    resourceSharePermissionSummary_defaultVersion,
    resourceSharePermissionSummary_isResourceTypeDefault,
    resourceSharePermissionSummary_lastUpdatedTime,
    resourceSharePermissionSummary_name,
    resourceSharePermissionSummary_resourceType,
    resourceSharePermissionSummary_status,
    resourceSharePermissionSummary_version,

    -- ** ServiceNameAndResourceType
    serviceNameAndResourceType_resourceRegionScope,
    serviceNameAndResourceType_resourceType,
    serviceNameAndResourceType_serviceName,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TagFilter
    tagFilter_tagKey,
    tagFilter_tagValues,
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
