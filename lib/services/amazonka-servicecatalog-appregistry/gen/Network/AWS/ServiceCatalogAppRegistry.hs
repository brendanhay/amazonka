{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Amazonka.ServiceCatalogAppRegistry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Derived from API version @2020-06-24@ of the AWS service descriptions, licensed under Apache 2.0.
--
-- Amazon Web Services Service Catalog AppRegistry enables organizations to
-- understand the application context of their Amazon Web Services
-- resources. AppRegistry provides a repository of your applications, their
-- resources, and the application metadata that you use within your
-- enterprise.
module Amazonka.ServiceCatalogAppRegistry
  ( -- * Service Configuration
    defaultService,

    -- * Errors
    -- $errors

    -- ** ValidationException
    _ValidationException,

    -- ** ConflictException
    _ConflictException,

    -- ** ServiceQuotaExceededException
    _ServiceQuotaExceededException,

    -- ** InternalServerException
    _InternalServerException,

    -- ** ResourceNotFoundException
    _ResourceNotFoundException,

    -- * Waiters
    -- $waiters

    -- * Operations
    -- $operations

    -- ** AssociateAttributeGroup
    AssociateAttributeGroup (AssociateAttributeGroup'),
    newAssociateAttributeGroup,
    AssociateAttributeGroupResponse (AssociateAttributeGroupResponse'),
    newAssociateAttributeGroupResponse,

    -- ** ListAttributeGroups (Paginated)
    ListAttributeGroups (ListAttributeGroups'),
    newListAttributeGroups,
    ListAttributeGroupsResponse (ListAttributeGroupsResponse'),
    newListAttributeGroupsResponse,

    -- ** ListTagsForResource
    ListTagsForResource (ListTagsForResource'),
    newListTagsForResource,
    ListTagsForResourceResponse (ListTagsForResourceResponse'),
    newListTagsForResourceResponse,

    -- ** SyncResource
    SyncResource (SyncResource'),
    newSyncResource,
    SyncResourceResponse (SyncResourceResponse'),
    newSyncResourceResponse,

    -- ** DeleteApplication
    DeleteApplication (DeleteApplication'),
    newDeleteApplication,
    DeleteApplicationResponse (DeleteApplicationResponse'),
    newDeleteApplicationResponse,

    -- ** UpdateApplication
    UpdateApplication (UpdateApplication'),
    newUpdateApplication,
    UpdateApplicationResponse (UpdateApplicationResponse'),
    newUpdateApplicationResponse,

    -- ** AssociateResource
    AssociateResource (AssociateResource'),
    newAssociateResource,
    AssociateResourceResponse (AssociateResourceResponse'),
    newAssociateResourceResponse,

    -- ** CreateApplication
    CreateApplication (CreateApplication'),
    newCreateApplication,
    CreateApplicationResponse (CreateApplicationResponse'),
    newCreateApplicationResponse,

    -- ** DisassociateAttributeGroup
    DisassociateAttributeGroup (DisassociateAttributeGroup'),
    newDisassociateAttributeGroup,
    DisassociateAttributeGroupResponse (DisassociateAttributeGroupResponse'),
    newDisassociateAttributeGroupResponse,

    -- ** GetApplication
    GetApplication (GetApplication'),
    newGetApplication,
    GetApplicationResponse (GetApplicationResponse'),
    newGetApplicationResponse,

    -- ** GetAssociatedResource
    GetAssociatedResource (GetAssociatedResource'),
    newGetAssociatedResource,
    GetAssociatedResourceResponse (GetAssociatedResourceResponse'),
    newGetAssociatedResourceResponse,

    -- ** CreateAttributeGroup
    CreateAttributeGroup (CreateAttributeGroup'),
    newCreateAttributeGroup,
    CreateAttributeGroupResponse (CreateAttributeGroupResponse'),
    newCreateAttributeGroupResponse,

    -- ** DeleteAttributeGroup
    DeleteAttributeGroup (DeleteAttributeGroup'),
    newDeleteAttributeGroup,
    DeleteAttributeGroupResponse (DeleteAttributeGroupResponse'),
    newDeleteAttributeGroupResponse,

    -- ** UpdateAttributeGroup
    UpdateAttributeGroup (UpdateAttributeGroup'),
    newUpdateAttributeGroup,
    UpdateAttributeGroupResponse (UpdateAttributeGroupResponse'),
    newUpdateAttributeGroupResponse,

    -- ** ListAssociatedAttributeGroups (Paginated)
    ListAssociatedAttributeGroups (ListAssociatedAttributeGroups'),
    newListAssociatedAttributeGroups,
    ListAssociatedAttributeGroupsResponse (ListAssociatedAttributeGroupsResponse'),
    newListAssociatedAttributeGroupsResponse,

    -- ** GetAttributeGroup
    GetAttributeGroup (GetAttributeGroup'),
    newGetAttributeGroup,
    GetAttributeGroupResponse (GetAttributeGroupResponse'),
    newGetAttributeGroupResponse,

    -- ** DisassociateResource
    DisassociateResource (DisassociateResource'),
    newDisassociateResource,
    DisassociateResourceResponse (DisassociateResourceResponse'),
    newDisassociateResourceResponse,

    -- ** TagResource
    TagResource (TagResource'),
    newTagResource,
    TagResourceResponse (TagResourceResponse'),
    newTagResourceResponse,

    -- ** ListAssociatedResources (Paginated)
    ListAssociatedResources (ListAssociatedResources'),
    newListAssociatedResources,
    ListAssociatedResourcesResponse (ListAssociatedResourcesResponse'),
    newListAssociatedResourcesResponse,

    -- ** ListApplications (Paginated)
    ListApplications (ListApplications'),
    newListApplications,
    ListApplicationsResponse (ListApplicationsResponse'),
    newListApplicationsResponse,

    -- ** UntagResource
    UntagResource (UntagResource'),
    newUntagResource,
    UntagResourceResponse (UntagResourceResponse'),
    newUntagResourceResponse,

    -- * Types

    -- ** ResourceGroupState
    ResourceGroupState (..),

    -- ** ResourceType
    ResourceType (..),

    -- ** SyncAction
    SyncAction (..),

    -- ** Application
    Application (Application'),
    newApplication,

    -- ** ApplicationSummary
    ApplicationSummary (ApplicationSummary'),
    newApplicationSummary,

    -- ** AttributeGroup
    AttributeGroup (AttributeGroup'),
    newAttributeGroup,

    -- ** AttributeGroupSummary
    AttributeGroupSummary (AttributeGroupSummary'),
    newAttributeGroupSummary,

    -- ** Integrations
    Integrations (Integrations'),
    newIntegrations,

    -- ** Resource
    Resource (Resource'),
    newResource,

    -- ** ResourceGroup
    ResourceGroup (ResourceGroup'),
    newResourceGroup,

    -- ** ResourceInfo
    ResourceInfo (ResourceInfo'),
    newResourceInfo,

    -- ** ResourceIntegrations
    ResourceIntegrations (ResourceIntegrations'),
    newResourceIntegrations,
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
import Amazonka.ServiceCatalogAppRegistry.Lens
import Amazonka.ServiceCatalogAppRegistry.ListApplications
import Amazonka.ServiceCatalogAppRegistry.ListAssociatedAttributeGroups
import Amazonka.ServiceCatalogAppRegistry.ListAssociatedResources
import Amazonka.ServiceCatalogAppRegistry.ListAttributeGroups
import Amazonka.ServiceCatalogAppRegistry.ListTagsForResource
import Amazonka.ServiceCatalogAppRegistry.SyncResource
import Amazonka.ServiceCatalogAppRegistry.TagResource
import Amazonka.ServiceCatalogAppRegistry.Types
import Amazonka.ServiceCatalogAppRegistry.UntagResource
import Amazonka.ServiceCatalogAppRegistry.UpdateApplication
import Amazonka.ServiceCatalogAppRegistry.UpdateAttributeGroup
import Amazonka.ServiceCatalogAppRegistry.Waiters

-- $errors
-- Error matchers are designed for use with the functions provided by
-- <http://hackage.haskell.org/package/lens/docs/Control-Exception-Lens.html Control.Exception.Lens>.
-- This allows catching (and rethrowing) service specific errors returned
-- by 'ServiceCatalogAppRegistry'.

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
