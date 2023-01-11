{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudControl.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudControl.Lens
  ( -- * Operations

    -- ** CancelResourceRequest
    cancelResourceRequest_requestToken,
    cancelResourceRequestResponse_progressEvent,
    cancelResourceRequestResponse_httpStatus,

    -- ** CreateResource
    createResource_clientToken,
    createResource_roleArn,
    createResource_typeVersionId,
    createResource_typeName,
    createResource_desiredState,
    createResourceResponse_progressEvent,
    createResourceResponse_httpStatus,

    -- ** DeleteResource
    deleteResource_clientToken,
    deleteResource_roleArn,
    deleteResource_typeVersionId,
    deleteResource_typeName,
    deleteResource_identifier,
    deleteResourceResponse_progressEvent,
    deleteResourceResponse_httpStatus,

    -- ** GetResource
    getResource_roleArn,
    getResource_typeVersionId,
    getResource_typeName,
    getResource_identifier,
    getResourceResponse_resourceDescription,
    getResourceResponse_typeName,
    getResourceResponse_httpStatus,

    -- ** GetResourceRequestStatus
    getResourceRequestStatus_requestToken,
    getResourceRequestStatusResponse_progressEvent,
    getResourceRequestStatusResponse_httpStatus,

    -- ** ListResourceRequests
    listResourceRequests_maxResults,
    listResourceRequests_nextToken,
    listResourceRequests_resourceRequestStatusFilter,
    listResourceRequestsResponse_nextToken,
    listResourceRequestsResponse_resourceRequestStatusSummaries,
    listResourceRequestsResponse_httpStatus,

    -- ** ListResources
    listResources_maxResults,
    listResources_nextToken,
    listResources_resourceModel,
    listResources_roleArn,
    listResources_typeVersionId,
    listResources_typeName,
    listResourcesResponse_nextToken,
    listResourcesResponse_resourceDescriptions,
    listResourcesResponse_typeName,
    listResourcesResponse_httpStatus,

    -- ** UpdateResource
    updateResource_clientToken,
    updateResource_roleArn,
    updateResource_typeVersionId,
    updateResource_typeName,
    updateResource_identifier,
    updateResource_patchDocument,
    updateResourceResponse_progressEvent,
    updateResourceResponse_httpStatus,

    -- * Types

    -- ** ProgressEvent
    progressEvent_errorCode,
    progressEvent_eventTime,
    progressEvent_identifier,
    progressEvent_operation,
    progressEvent_operationStatus,
    progressEvent_requestToken,
    progressEvent_resourceModel,
    progressEvent_retryAfter,
    progressEvent_statusMessage,
    progressEvent_typeName,

    -- ** ResourceDescription
    resourceDescription_identifier,
    resourceDescription_properties,

    -- ** ResourceRequestStatusFilter
    resourceRequestStatusFilter_operationStatuses,
    resourceRequestStatusFilter_operations,
  )
where

import Amazonka.CloudControl.CancelResourceRequest
import Amazonka.CloudControl.CreateResource
import Amazonka.CloudControl.DeleteResource
import Amazonka.CloudControl.GetResource
import Amazonka.CloudControl.GetResourceRequestStatus
import Amazonka.CloudControl.ListResourceRequests
import Amazonka.CloudControl.ListResources
import Amazonka.CloudControl.Types.ProgressEvent
import Amazonka.CloudControl.Types.ResourceDescription
import Amazonka.CloudControl.Types.ResourceRequestStatusFilter
import Amazonka.CloudControl.UpdateResource
