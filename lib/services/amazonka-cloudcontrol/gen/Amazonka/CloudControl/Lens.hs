{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudControl.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudControl.Lens
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
