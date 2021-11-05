{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalogAppRegistry.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalogAppRegistry.Lens
  ( -- * Operations

    -- ** AssociateAttributeGroup
    associateAttributeGroup_application,
    associateAttributeGroup_attributeGroup,
    associateAttributeGroupResponse_applicationArn,
    associateAttributeGroupResponse_attributeGroupArn,
    associateAttributeGroupResponse_httpStatus,

    -- ** ListAttributeGroups
    listAttributeGroups_nextToken,
    listAttributeGroups_maxResults,
    listAttributeGroupsResponse_attributeGroups,
    listAttributeGroupsResponse_nextToken,
    listAttributeGroupsResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** SyncResource
    syncResource_resourceType,
    syncResource_resource,
    syncResourceResponse_applicationArn,
    syncResourceResponse_actionTaken,
    syncResourceResponse_resourceArn,
    syncResourceResponse_httpStatus,

    -- ** DeleteApplication
    deleteApplication_application,
    deleteApplicationResponse_application,
    deleteApplicationResponse_httpStatus,

    -- ** UpdateApplication
    updateApplication_name,
    updateApplication_description,
    updateApplication_application,
    updateApplicationResponse_application,
    updateApplicationResponse_httpStatus,

    -- ** AssociateResource
    associateResource_application,
    associateResource_resourceType,
    associateResource_resource,
    associateResourceResponse_applicationArn,
    associateResourceResponse_resourceArn,
    associateResourceResponse_httpStatus,

    -- ** CreateApplication
    createApplication_description,
    createApplication_tags,
    createApplication_name,
    createApplication_clientToken,
    createApplicationResponse_application,
    createApplicationResponse_httpStatus,

    -- ** DisassociateAttributeGroup
    disassociateAttributeGroup_application,
    disassociateAttributeGroup_attributeGroup,
    disassociateAttributeGroupResponse_applicationArn,
    disassociateAttributeGroupResponse_attributeGroupArn,
    disassociateAttributeGroupResponse_httpStatus,

    -- ** GetApplication
    getApplication_application,
    getApplicationResponse_creationTime,
    getApplicationResponse_arn,
    getApplicationResponse_integrations,
    getApplicationResponse_associatedResourceCount,
    getApplicationResponse_name,
    getApplicationResponse_id,
    getApplicationResponse_lastUpdateTime,
    getApplicationResponse_description,
    getApplicationResponse_tags,
    getApplicationResponse_httpStatus,

    -- ** GetAssociatedResource
    getAssociatedResource_application,
    getAssociatedResource_resourceType,
    getAssociatedResource_resource,
    getAssociatedResourceResponse_resource,
    getAssociatedResourceResponse_httpStatus,

    -- ** CreateAttributeGroup
    createAttributeGroup_description,
    createAttributeGroup_tags,
    createAttributeGroup_name,
    createAttributeGroup_attributes,
    createAttributeGroup_clientToken,
    createAttributeGroupResponse_attributeGroup,
    createAttributeGroupResponse_httpStatus,

    -- ** DeleteAttributeGroup
    deleteAttributeGroup_attributeGroup,
    deleteAttributeGroupResponse_attributeGroup,
    deleteAttributeGroupResponse_httpStatus,

    -- ** UpdateAttributeGroup
    updateAttributeGroup_name,
    updateAttributeGroup_attributes,
    updateAttributeGroup_description,
    updateAttributeGroup_attributeGroup,
    updateAttributeGroupResponse_attributeGroup,
    updateAttributeGroupResponse_httpStatus,

    -- ** ListAssociatedAttributeGroups
    listAssociatedAttributeGroups_nextToken,
    listAssociatedAttributeGroups_maxResults,
    listAssociatedAttributeGroups_application,
    listAssociatedAttributeGroupsResponse_attributeGroups,
    listAssociatedAttributeGroupsResponse_nextToken,
    listAssociatedAttributeGroupsResponse_httpStatus,

    -- ** GetAttributeGroup
    getAttributeGroup_attributeGroup,
    getAttributeGroupResponse_creationTime,
    getAttributeGroupResponse_arn,
    getAttributeGroupResponse_name,
    getAttributeGroupResponse_attributes,
    getAttributeGroupResponse_id,
    getAttributeGroupResponse_lastUpdateTime,
    getAttributeGroupResponse_description,
    getAttributeGroupResponse_tags,
    getAttributeGroupResponse_httpStatus,

    -- ** DisassociateResource
    disassociateResource_application,
    disassociateResource_resourceType,
    disassociateResource_resource,
    disassociateResourceResponse_applicationArn,
    disassociateResourceResponse_resourceArn,
    disassociateResourceResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** ListAssociatedResources
    listAssociatedResources_nextToken,
    listAssociatedResources_maxResults,
    listAssociatedResources_application,
    listAssociatedResourcesResponse_resources,
    listAssociatedResourcesResponse_nextToken,
    listAssociatedResourcesResponse_httpStatus,

    -- ** ListApplications
    listApplications_nextToken,
    listApplications_maxResults,
    listApplicationsResponse_nextToken,
    listApplicationsResponse_applications,
    listApplicationsResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- * Types

    -- ** Application
    application_creationTime,
    application_arn,
    application_name,
    application_id,
    application_lastUpdateTime,
    application_description,
    application_tags,

    -- ** ApplicationSummary
    applicationSummary_creationTime,
    applicationSummary_arn,
    applicationSummary_name,
    applicationSummary_id,
    applicationSummary_lastUpdateTime,
    applicationSummary_description,

    -- ** AttributeGroup
    attributeGroup_creationTime,
    attributeGroup_arn,
    attributeGroup_name,
    attributeGroup_id,
    attributeGroup_lastUpdateTime,
    attributeGroup_description,
    attributeGroup_tags,

    -- ** AttributeGroupSummary
    attributeGroupSummary_creationTime,
    attributeGroupSummary_arn,
    attributeGroupSummary_name,
    attributeGroupSummary_id,
    attributeGroupSummary_lastUpdateTime,
    attributeGroupSummary_description,

    -- ** Integrations
    integrations_resourceGroup,

    -- ** Resource
    resource_arn,
    resource_integrations,
    resource_associationTime,
    resource_name,

    -- ** ResourceGroup
    resourceGroup_state,
    resourceGroup_arn,
    resourceGroup_errorMessage,

    -- ** ResourceInfo
    resourceInfo_arn,
    resourceInfo_name,

    -- ** ResourceIntegrations
    resourceIntegrations_resourceGroup,
  )
where

import Network.AWS.ServiceCatalogAppRegistry.AssociateAttributeGroup
import Network.AWS.ServiceCatalogAppRegistry.AssociateResource
import Network.AWS.ServiceCatalogAppRegistry.CreateApplication
import Network.AWS.ServiceCatalogAppRegistry.CreateAttributeGroup
import Network.AWS.ServiceCatalogAppRegistry.DeleteApplication
import Network.AWS.ServiceCatalogAppRegistry.DeleteAttributeGroup
import Network.AWS.ServiceCatalogAppRegistry.DisassociateAttributeGroup
import Network.AWS.ServiceCatalogAppRegistry.DisassociateResource
import Network.AWS.ServiceCatalogAppRegistry.GetApplication
import Network.AWS.ServiceCatalogAppRegistry.GetAssociatedResource
import Network.AWS.ServiceCatalogAppRegistry.GetAttributeGroup
import Network.AWS.ServiceCatalogAppRegistry.ListApplications
import Network.AWS.ServiceCatalogAppRegistry.ListAssociatedAttributeGroups
import Network.AWS.ServiceCatalogAppRegistry.ListAssociatedResources
import Network.AWS.ServiceCatalogAppRegistry.ListAttributeGroups
import Network.AWS.ServiceCatalogAppRegistry.ListTagsForResource
import Network.AWS.ServiceCatalogAppRegistry.SyncResource
import Network.AWS.ServiceCatalogAppRegistry.TagResource
import Network.AWS.ServiceCatalogAppRegistry.Types.Application
import Network.AWS.ServiceCatalogAppRegistry.Types.ApplicationSummary
import Network.AWS.ServiceCatalogAppRegistry.Types.AttributeGroup
import Network.AWS.ServiceCatalogAppRegistry.Types.AttributeGroupSummary
import Network.AWS.ServiceCatalogAppRegistry.Types.Integrations
import Network.AWS.ServiceCatalogAppRegistry.Types.Resource
import Network.AWS.ServiceCatalogAppRegistry.Types.ResourceGroup
import Network.AWS.ServiceCatalogAppRegistry.Types.ResourceInfo
import Network.AWS.ServiceCatalogAppRegistry.Types.ResourceIntegrations
import Network.AWS.ServiceCatalogAppRegistry.UntagResource
import Network.AWS.ServiceCatalogAppRegistry.UpdateApplication
import Network.AWS.ServiceCatalogAppRegistry.UpdateAttributeGroup
