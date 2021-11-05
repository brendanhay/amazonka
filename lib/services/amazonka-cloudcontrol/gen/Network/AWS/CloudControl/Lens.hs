{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudControl.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudControl.Lens
  ( -- * Operations

    -- ** GetResource
    getResource_typeVersionId,
    getResource_roleArn,
    getResource_typeName,
    getResource_identifier,
    getResourceResponse_typeName,
    getResourceResponse_resourceDescription,
    getResourceResponse_httpStatus,

    -- ** ListResourceRequests
    listResourceRequests_nextToken,
    listResourceRequests_resourceRequestStatusFilter,
    listResourceRequests_maxResults,
    listResourceRequestsResponse_resourceRequestStatusSummaries,
    listResourceRequestsResponse_nextToken,
    listResourceRequestsResponse_httpStatus,

    -- ** CancelResourceRequest
    cancelResourceRequest_requestToken,
    cancelResourceRequestResponse_progressEvent,
    cancelResourceRequestResponse_httpStatus,

    -- ** DeleteResource
    deleteResource_clientToken,
    deleteResource_typeVersionId,
    deleteResource_roleArn,
    deleteResource_typeName,
    deleteResource_identifier,
    deleteResourceResponse_progressEvent,
    deleteResourceResponse_httpStatus,

    -- ** UpdateResource
    updateResource_clientToken,
    updateResource_typeVersionId,
    updateResource_roleArn,
    updateResource_typeName,
    updateResource_identifier,
    updateResource_patchDocument,
    updateResourceResponse_progressEvent,
    updateResourceResponse_httpStatus,

    -- ** ListResources
    listResources_resourceModel,
    listResources_nextToken,
    listResources_typeVersionId,
    listResources_maxResults,
    listResources_roleArn,
    listResources_typeName,
    listResourcesResponse_resourceDescriptions,
    listResourcesResponse_typeName,
    listResourcesResponse_nextToken,
    listResourcesResponse_httpStatus,

    -- ** CreateResource
    createResource_clientToken,
    createResource_typeVersionId,
    createResource_roleArn,
    createResource_typeName,
    createResource_desiredState,
    createResourceResponse_progressEvent,
    createResourceResponse_httpStatus,

    -- ** GetResourceRequestStatus
    getResourceRequestStatus_requestToken,
    getResourceRequestStatusResponse_progressEvent,
    getResourceRequestStatusResponse_httpStatus,

    -- * Types

    -- ** ProgressEvent
    progressEvent_retryAfter,
    progressEvent_typeName,
    progressEvent_requestToken,
    progressEvent_resourceModel,
    progressEvent_operation,
    progressEvent_identifier,
    progressEvent_operationStatus,
    progressEvent_eventTime,
    progressEvent_statusMessage,
    progressEvent_errorCode,

    -- ** ResourceDescription
    resourceDescription_identifier,
    resourceDescription_properties,

    -- ** ResourceRequestStatusFilter
    resourceRequestStatusFilter_operationStatuses,
    resourceRequestStatusFilter_operations,
  )
where

import Network.AWS.CloudControl.CancelResourceRequest
import Network.AWS.CloudControl.CreateResource
import Network.AWS.CloudControl.DeleteResource
import Network.AWS.CloudControl.GetResource
import Network.AWS.CloudControl.GetResourceRequestStatus
import Network.AWS.CloudControl.ListResourceRequests
import Network.AWS.CloudControl.ListResources
import Network.AWS.CloudControl.Types.ProgressEvent
import Network.AWS.CloudControl.Types.ResourceDescription
import Network.AWS.CloudControl.Types.ResourceRequestStatusFilter
import Network.AWS.CloudControl.UpdateResource
