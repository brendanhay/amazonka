{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ServiceCatalogAppRegistry.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalogAppRegistry.Lens
  ( -- * Operations

    -- ** AssociateAttributeGroup
    associateAttributeGroup_application,
    associateAttributeGroup_attributeGroup,
    associateAttributeGroupResponse_applicationArn,
    associateAttributeGroupResponse_attributeGroupArn,
    associateAttributeGroupResponse_httpStatus,

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

    -- ** CreateAttributeGroup
    createAttributeGroup_description,
    createAttributeGroup_tags,
    createAttributeGroup_name,
    createAttributeGroup_attributes,
    createAttributeGroup_clientToken,
    createAttributeGroupResponse_attributeGroup,
    createAttributeGroupResponse_httpStatus,

    -- ** DeleteApplication
    deleteApplication_application,
    deleteApplicationResponse_application,
    deleteApplicationResponse_httpStatus,

    -- ** DeleteAttributeGroup
    deleteAttributeGroup_attributeGroup,
    deleteAttributeGroupResponse_attributeGroup,
    deleteAttributeGroupResponse_httpStatus,

    -- ** DisassociateAttributeGroup
    disassociateAttributeGroup_application,
    disassociateAttributeGroup_attributeGroup,
    disassociateAttributeGroupResponse_applicationArn,
    disassociateAttributeGroupResponse_attributeGroupArn,
    disassociateAttributeGroupResponse_httpStatus,

    -- ** DisassociateResource
    disassociateResource_application,
    disassociateResource_resourceType,
    disassociateResource_resource,
    disassociateResourceResponse_applicationArn,
    disassociateResourceResponse_resourceArn,
    disassociateResourceResponse_httpStatus,

    -- ** GetApplication
    getApplication_application,
    getApplicationResponse_arn,
    getApplicationResponse_associatedResourceCount,
    getApplicationResponse_creationTime,
    getApplicationResponse_description,
    getApplicationResponse_id,
    getApplicationResponse_integrations,
    getApplicationResponse_lastUpdateTime,
    getApplicationResponse_name,
    getApplicationResponse_tags,
    getApplicationResponse_httpStatus,

    -- ** GetAssociatedResource
    getAssociatedResource_application,
    getAssociatedResource_resourceType,
    getAssociatedResource_resource,
    getAssociatedResourceResponse_resource,
    getAssociatedResourceResponse_httpStatus,

    -- ** GetAttributeGroup
    getAttributeGroup_attributeGroup,
    getAttributeGroupResponse_arn,
    getAttributeGroupResponse_attributes,
    getAttributeGroupResponse_createdBy,
    getAttributeGroupResponse_creationTime,
    getAttributeGroupResponse_description,
    getAttributeGroupResponse_id,
    getAttributeGroupResponse_lastUpdateTime,
    getAttributeGroupResponse_name,
    getAttributeGroupResponse_tags,
    getAttributeGroupResponse_httpStatus,

    -- ** GetConfiguration
    getConfigurationResponse_configuration,
    getConfigurationResponse_httpStatus,

    -- ** ListApplications
    listApplications_maxResults,
    listApplications_nextToken,
    listApplicationsResponse_applications,
    listApplicationsResponse_nextToken,
    listApplicationsResponse_httpStatus,

    -- ** ListAssociatedAttributeGroups
    listAssociatedAttributeGroups_maxResults,
    listAssociatedAttributeGroups_nextToken,
    listAssociatedAttributeGroups_application,
    listAssociatedAttributeGroupsResponse_attributeGroups,
    listAssociatedAttributeGroupsResponse_nextToken,
    listAssociatedAttributeGroupsResponse_httpStatus,

    -- ** ListAssociatedResources
    listAssociatedResources_maxResults,
    listAssociatedResources_nextToken,
    listAssociatedResources_application,
    listAssociatedResourcesResponse_nextToken,
    listAssociatedResourcesResponse_resources,
    listAssociatedResourcesResponse_httpStatus,

    -- ** ListAttributeGroups
    listAttributeGroups_maxResults,
    listAttributeGroups_nextToken,
    listAttributeGroupsResponse_attributeGroups,
    listAttributeGroupsResponse_nextToken,
    listAttributeGroupsResponse_httpStatus,

    -- ** ListAttributeGroupsForApplication
    listAttributeGroupsForApplication_maxResults,
    listAttributeGroupsForApplication_nextToken,
    listAttributeGroupsForApplication_application,
    listAttributeGroupsForApplicationResponse_attributeGroupsDetails,
    listAttributeGroupsForApplicationResponse_nextToken,
    listAttributeGroupsForApplicationResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** PutConfiguration
    putConfiguration_configuration,

    -- ** SyncResource
    syncResource_resourceType,
    syncResource_resource,
    syncResourceResponse_actionTaken,
    syncResourceResponse_applicationArn,
    syncResourceResponse_resourceArn,
    syncResourceResponse_httpStatus,

    -- ** TagResource
    tagResource_resourceArn,
    tagResource_tags,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateApplication
    updateApplication_description,
    updateApplication_name,
    updateApplication_application,
    updateApplicationResponse_application,
    updateApplicationResponse_httpStatus,

    -- ** UpdateAttributeGroup
    updateAttributeGroup_attributes,
    updateAttributeGroup_description,
    updateAttributeGroup_name,
    updateAttributeGroup_attributeGroup,
    updateAttributeGroupResponse_attributeGroup,
    updateAttributeGroupResponse_httpStatus,

    -- * Types

    -- ** AppRegistryConfiguration
    appRegistryConfiguration_tagQueryConfiguration,

    -- ** Application
    application_arn,
    application_creationTime,
    application_description,
    application_id,
    application_lastUpdateTime,
    application_name,
    application_tags,

    -- ** ApplicationSummary
    applicationSummary_arn,
    applicationSummary_creationTime,
    applicationSummary_description,
    applicationSummary_id,
    applicationSummary_lastUpdateTime,
    applicationSummary_name,

    -- ** AttributeGroup
    attributeGroup_arn,
    attributeGroup_creationTime,
    attributeGroup_description,
    attributeGroup_id,
    attributeGroup_lastUpdateTime,
    attributeGroup_name,
    attributeGroup_tags,

    -- ** AttributeGroupDetails
    attributeGroupDetails_arn,
    attributeGroupDetails_createdBy,
    attributeGroupDetails_id,
    attributeGroupDetails_name,

    -- ** AttributeGroupSummary
    attributeGroupSummary_arn,
    attributeGroupSummary_createdBy,
    attributeGroupSummary_creationTime,
    attributeGroupSummary_description,
    attributeGroupSummary_id,
    attributeGroupSummary_lastUpdateTime,
    attributeGroupSummary_name,

    -- ** Integrations
    integrations_resourceGroup,

    -- ** Resource
    resource_arn,
    resource_associationTime,
    resource_integrations,
    resource_name,

    -- ** ResourceDetails
    resourceDetails_tagValue,

    -- ** ResourceGroup
    resourceGroup_arn,
    resourceGroup_errorMessage,
    resourceGroup_state,

    -- ** ResourceInfo
    resourceInfo_arn,
    resourceInfo_name,
    resourceInfo_resourceDetails,
    resourceInfo_resourceType,

    -- ** ResourceIntegrations
    resourceIntegrations_resourceGroup,

    -- ** TagQueryConfiguration
    tagQueryConfiguration_tagKey,
  )
where

import Amazonka.ServiceCatalogAppRegistry.AssociateAttributeGroup
import Amazonka.ServiceCatalogAppRegistry.AssociateResource
import Amazonka.ServiceCatalogAppRegistry.CreateApplication
import Amazonka.ServiceCatalogAppRegistry.CreateAttributeGroup
import Amazonka.ServiceCatalogAppRegistry.DeleteApplication
import Amazonka.ServiceCatalogAppRegistry.DeleteAttributeGroup
import Amazonka.ServiceCatalogAppRegistry.DisassociateAttributeGroup
import Amazonka.ServiceCatalogAppRegistry.DisassociateResource
import Amazonka.ServiceCatalogAppRegistry.GetApplication
import Amazonka.ServiceCatalogAppRegistry.GetAssociatedResource
import Amazonka.ServiceCatalogAppRegistry.GetAttributeGroup
import Amazonka.ServiceCatalogAppRegistry.GetConfiguration
import Amazonka.ServiceCatalogAppRegistry.ListApplications
import Amazonka.ServiceCatalogAppRegistry.ListAssociatedAttributeGroups
import Amazonka.ServiceCatalogAppRegistry.ListAssociatedResources
import Amazonka.ServiceCatalogAppRegistry.ListAttributeGroups
import Amazonka.ServiceCatalogAppRegistry.ListAttributeGroupsForApplication
import Amazonka.ServiceCatalogAppRegistry.ListTagsForResource
import Amazonka.ServiceCatalogAppRegistry.PutConfiguration
import Amazonka.ServiceCatalogAppRegistry.SyncResource
import Amazonka.ServiceCatalogAppRegistry.TagResource
import Amazonka.ServiceCatalogAppRegistry.Types.AppRegistryConfiguration
import Amazonka.ServiceCatalogAppRegistry.Types.Application
import Amazonka.ServiceCatalogAppRegistry.Types.ApplicationSummary
import Amazonka.ServiceCatalogAppRegistry.Types.AttributeGroup
import Amazonka.ServiceCatalogAppRegistry.Types.AttributeGroupDetails
import Amazonka.ServiceCatalogAppRegistry.Types.AttributeGroupSummary
import Amazonka.ServiceCatalogAppRegistry.Types.Integrations
import Amazonka.ServiceCatalogAppRegistry.Types.Resource
import Amazonka.ServiceCatalogAppRegistry.Types.ResourceDetails
import Amazonka.ServiceCatalogAppRegistry.Types.ResourceGroup
import Amazonka.ServiceCatalogAppRegistry.Types.ResourceInfo
import Amazonka.ServiceCatalogAppRegistry.Types.ResourceIntegrations
import Amazonka.ServiceCatalogAppRegistry.Types.TagQueryConfiguration
import Amazonka.ServiceCatalogAppRegistry.UntagResource
import Amazonka.ServiceCatalogAppRegistry.UpdateApplication
import Amazonka.ServiceCatalogAppRegistry.UpdateAttributeGroup
