{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ServiceCatalogAppRegistry.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    createApplication_tags,
    createApplication_description,
    createApplication_name,
    createApplication_clientToken,
    createApplicationResponse_application,
    createApplicationResponse_httpStatus,

    -- ** CreateAttributeGroup
    createAttributeGroup_tags,
    createAttributeGroup_description,
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
    getApplicationResponse_tags,
    getApplicationResponse_name,
    getApplicationResponse_integrations,
    getApplicationResponse_arn,
    getApplicationResponse_description,
    getApplicationResponse_id,
    getApplicationResponse_creationTime,
    getApplicationResponse_lastUpdateTime,
    getApplicationResponse_associatedResourceCount,
    getApplicationResponse_httpStatus,

    -- ** GetAssociatedResource
    getAssociatedResource_application,
    getAssociatedResource_resourceType,
    getAssociatedResource_resource,
    getAssociatedResourceResponse_resource,
    getAssociatedResourceResponse_httpStatus,

    -- ** GetAttributeGroup
    getAttributeGroup_attributeGroup,
    getAttributeGroupResponse_tags,
    getAttributeGroupResponse_name,
    getAttributeGroupResponse_arn,
    getAttributeGroupResponse_description,
    getAttributeGroupResponse_id,
    getAttributeGroupResponse_creationTime,
    getAttributeGroupResponse_lastUpdateTime,
    getAttributeGroupResponse_attributes,
    getAttributeGroupResponse_httpStatus,

    -- ** GetConfiguration
    getConfigurationResponse_configuration,
    getConfigurationResponse_httpStatus,

    -- ** ListApplications
    listApplications_nextToken,
    listApplications_maxResults,
    listApplicationsResponse_nextToken,
    listApplicationsResponse_applications,
    listApplicationsResponse_httpStatus,

    -- ** ListAssociatedAttributeGroups
    listAssociatedAttributeGroups_nextToken,
    listAssociatedAttributeGroups_maxResults,
    listAssociatedAttributeGroups_application,
    listAssociatedAttributeGroupsResponse_nextToken,
    listAssociatedAttributeGroupsResponse_attributeGroups,
    listAssociatedAttributeGroupsResponse_httpStatus,

    -- ** ListAssociatedResources
    listAssociatedResources_nextToken,
    listAssociatedResources_maxResults,
    listAssociatedResources_application,
    listAssociatedResourcesResponse_nextToken,
    listAssociatedResourcesResponse_resources,
    listAssociatedResourcesResponse_httpStatus,

    -- ** ListAttributeGroups
    listAttributeGroups_nextToken,
    listAttributeGroups_maxResults,
    listAttributeGroupsResponse_nextToken,
    listAttributeGroupsResponse_attributeGroups,
    listAttributeGroupsResponse_httpStatus,

    -- ** ListAttributeGroupsForApplication
    listAttributeGroupsForApplication_nextToken,
    listAttributeGroupsForApplication_maxResults,
    listAttributeGroupsForApplication_application,
    listAttributeGroupsForApplicationResponse_nextToken,
    listAttributeGroupsForApplicationResponse_attributeGroupsDetails,
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
    syncResourceResponse_applicationArn,
    syncResourceResponse_actionTaken,
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
    updateApplication_name,
    updateApplication_description,
    updateApplication_application,
    updateApplicationResponse_application,
    updateApplicationResponse_httpStatus,

    -- ** UpdateAttributeGroup
    updateAttributeGroup_name,
    updateAttributeGroup_description,
    updateAttributeGroup_attributes,
    updateAttributeGroup_attributeGroup,
    updateAttributeGroupResponse_attributeGroup,
    updateAttributeGroupResponse_httpStatus,

    -- * Types

    -- ** AppRegistryConfiguration
    appRegistryConfiguration_tagQueryConfiguration,

    -- ** Application
    application_tags,
    application_name,
    application_arn,
    application_description,
    application_id,
    application_creationTime,
    application_lastUpdateTime,

    -- ** ApplicationSummary
    applicationSummary_name,
    applicationSummary_arn,
    applicationSummary_description,
    applicationSummary_id,
    applicationSummary_creationTime,
    applicationSummary_lastUpdateTime,

    -- ** AttributeGroup
    attributeGroup_tags,
    attributeGroup_name,
    attributeGroup_arn,
    attributeGroup_description,
    attributeGroup_id,
    attributeGroup_creationTime,
    attributeGroup_lastUpdateTime,

    -- ** AttributeGroupDetails
    attributeGroupDetails_name,
    attributeGroupDetails_arn,
    attributeGroupDetails_id,

    -- ** AttributeGroupSummary
    attributeGroupSummary_name,
    attributeGroupSummary_arn,
    attributeGroupSummary_description,
    attributeGroupSummary_id,
    attributeGroupSummary_creationTime,
    attributeGroupSummary_lastUpdateTime,

    -- ** Integrations
    integrations_resourceGroup,

    -- ** Resource
    resource_name,
    resource_integrations,
    resource_arn,
    resource_associationTime,

    -- ** ResourceDetails
    resourceDetails_tagValue,

    -- ** ResourceGroup
    resourceGroup_errorMessage,
    resourceGroup_arn,
    resourceGroup_state,

    -- ** ResourceInfo
    resourceInfo_resourceType,
    resourceInfo_name,
    resourceInfo_arn,
    resourceInfo_resourceDetails,

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
