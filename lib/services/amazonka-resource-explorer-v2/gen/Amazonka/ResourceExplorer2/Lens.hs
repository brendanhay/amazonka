{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ResourceExplorer2.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResourceExplorer2.Lens
  ( -- * Operations

    -- ** AssociateDefaultView
    associateDefaultView_viewArn,
    associateDefaultViewResponse_viewArn,
    associateDefaultViewResponse_httpStatus,

    -- ** BatchGetView
    batchGetView_viewArns,
    batchGetViewResponse_errors,
    batchGetViewResponse_views,
    batchGetViewResponse_httpStatus,

    -- ** CreateIndex
    createIndex_clientToken,
    createIndex_tags,
    createIndexResponse_arn,
    createIndexResponse_createdAt,
    createIndexResponse_state,
    createIndexResponse_httpStatus,

    -- ** CreateView
    createView_clientToken,
    createView_filters,
    createView_includedProperties,
    createView_tags,
    createView_viewName,
    createViewResponse_view,
    createViewResponse_httpStatus,

    -- ** DeleteIndex
    deleteIndex_arn,
    deleteIndexResponse_arn,
    deleteIndexResponse_lastUpdatedAt,
    deleteIndexResponse_state,
    deleteIndexResponse_httpStatus,

    -- ** DeleteView
    deleteView_viewArn,
    deleteViewResponse_viewArn,
    deleteViewResponse_httpStatus,

    -- ** DisassociateDefaultView

    -- ** GetDefaultView
    getDefaultViewResponse_viewArn,
    getDefaultViewResponse_httpStatus,

    -- ** GetIndex
    getIndexResponse_arn,
    getIndexResponse_createdAt,
    getIndexResponse_lastUpdatedAt,
    getIndexResponse_replicatingFrom,
    getIndexResponse_replicatingTo,
    getIndexResponse_state,
    getIndexResponse_tags,
    getIndexResponse_type,
    getIndexResponse_httpStatus,

    -- ** GetView
    getView_viewArn,
    getViewResponse_tags,
    getViewResponse_view,
    getViewResponse_httpStatus,

    -- ** ListIndexes
    listIndexes_maxResults,
    listIndexes_nextToken,
    listIndexes_regions,
    listIndexes_type,
    listIndexesResponse_indexes,
    listIndexesResponse_nextToken,
    listIndexesResponse_httpStatus,

    -- ** ListSupportedResourceTypes
    listSupportedResourceTypes_maxResults,
    listSupportedResourceTypes_nextToken,
    listSupportedResourceTypesResponse_nextToken,
    listSupportedResourceTypesResponse_resourceTypes,
    listSupportedResourceTypesResponse_httpStatus,

    -- ** ListTagsForResource
    listTagsForResource_resourceArn,
    listTagsForResourceResponse_tags,
    listTagsForResourceResponse_httpStatus,

    -- ** ListViews
    listViews_maxResults,
    listViews_nextToken,
    listViewsResponse_nextToken,
    listViewsResponse_views,
    listViewsResponse_httpStatus,

    -- ** Search
    search_maxResults,
    search_nextToken,
    search_viewArn,
    search_queryString,
    searchResponse_count,
    searchResponse_nextToken,
    searchResponse_resources,
    searchResponse_viewArn,
    searchResponse_httpStatus,

    -- ** TagResource
    tagResource_tags,
    tagResource_resourceArn,
    tagResourceResponse_httpStatus,

    -- ** UntagResource
    untagResource_resourceArn,
    untagResource_tagKeys,
    untagResourceResponse_httpStatus,

    -- ** UpdateIndexType
    updateIndexType_arn,
    updateIndexType_type,
    updateIndexTypeResponse_arn,
    updateIndexTypeResponse_lastUpdatedAt,
    updateIndexTypeResponse_state,
    updateIndexTypeResponse_type,
    updateIndexTypeResponse_httpStatus,

    -- ** UpdateView
    updateView_filters,
    updateView_includedProperties,
    updateView_viewArn,
    updateViewResponse_view,
    updateViewResponse_httpStatus,

    -- * Types

    -- ** BatchGetViewError
    batchGetViewError_errorMessage,
    batchGetViewError_viewArn,

    -- ** Document

    -- ** IncludedProperty
    includedProperty_name,

    -- ** Index
    index_arn,
    index_region,
    index_type,

    -- ** Resource
    resource_arn,
    resource_lastReportedAt,
    resource_owningAccountId,
    resource_properties,
    resource_region,
    resource_resourceType,
    resource_service,

    -- ** ResourceCount
    resourceCount_complete,
    resourceCount_totalResources,

    -- ** ResourceProperty
    resourceProperty_data,
    resourceProperty_lastReportedAt,
    resourceProperty_name,

    -- ** SearchFilter
    searchFilter_filterString,

    -- ** SupportedResourceType
    supportedResourceType_resourceType,
    supportedResourceType_service,

    -- ** View
    view_filters,
    view_includedProperties,
    view_lastUpdatedAt,
    view_owner,
    view_scope,
    view_viewArn,
  )
where

import Amazonka.ResourceExplorer2.AssociateDefaultView
import Amazonka.ResourceExplorer2.BatchGetView
import Amazonka.ResourceExplorer2.CreateIndex
import Amazonka.ResourceExplorer2.CreateView
import Amazonka.ResourceExplorer2.DeleteIndex
import Amazonka.ResourceExplorer2.DeleteView
import Amazonka.ResourceExplorer2.DisassociateDefaultView
import Amazonka.ResourceExplorer2.GetDefaultView
import Amazonka.ResourceExplorer2.GetIndex
import Amazonka.ResourceExplorer2.GetView
import Amazonka.ResourceExplorer2.ListIndexes
import Amazonka.ResourceExplorer2.ListSupportedResourceTypes
import Amazonka.ResourceExplorer2.ListTagsForResource
import Amazonka.ResourceExplorer2.ListViews
import Amazonka.ResourceExplorer2.Search
import Amazonka.ResourceExplorer2.TagResource
import Amazonka.ResourceExplorer2.Types.BatchGetViewError
import Amazonka.ResourceExplorer2.Types.Document
import Amazonka.ResourceExplorer2.Types.IncludedProperty
import Amazonka.ResourceExplorer2.Types.Index
import Amazonka.ResourceExplorer2.Types.Resource
import Amazonka.ResourceExplorer2.Types.ResourceCount
import Amazonka.ResourceExplorer2.Types.ResourceProperty
import Amazonka.ResourceExplorer2.Types.SearchFilter
import Amazonka.ResourceExplorer2.Types.SupportedResourceType
import Amazonka.ResourceExplorer2.Types.View
import Amazonka.ResourceExplorer2.UntagResource
import Amazonka.ResourceExplorer2.UpdateIndexType
import Amazonka.ResourceExplorer2.UpdateView
