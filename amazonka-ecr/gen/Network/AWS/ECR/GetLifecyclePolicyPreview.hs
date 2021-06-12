{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.GetLifecyclePolicyPreview
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the results of the lifecycle policy preview request for the
-- specified repository.
--
-- This operation returns paginated results.
module Network.AWS.ECR.GetLifecyclePolicyPreview
  ( -- * Creating a Request
    GetLifecyclePolicyPreview (..),
    newGetLifecyclePolicyPreview,

    -- * Request Lenses
    getLifecyclePolicyPreview_nextToken,
    getLifecyclePolicyPreview_imageIds,
    getLifecyclePolicyPreview_maxResults,
    getLifecyclePolicyPreview_registryId,
    getLifecyclePolicyPreview_filter,
    getLifecyclePolicyPreview_repositoryName,

    -- * Destructuring the Response
    GetLifecyclePolicyPreviewResponse (..),
    newGetLifecyclePolicyPreviewResponse,

    -- * Response Lenses
    getLifecyclePolicyPreviewResponse_nextToken,
    getLifecyclePolicyPreviewResponse_status,
    getLifecyclePolicyPreviewResponse_registryId,
    getLifecyclePolicyPreviewResponse_repositoryName,
    getLifecyclePolicyPreviewResponse_summary,
    getLifecyclePolicyPreviewResponse_lifecyclePolicyText,
    getLifecyclePolicyPreviewResponse_previewResults,
    getLifecyclePolicyPreviewResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetLifecyclePolicyPreview' smart constructor.
data GetLifecyclePolicyPreview = GetLifecyclePolicyPreview'
  { -- | The @nextToken@ value returned from a previous paginated 
    -- @GetLifecyclePolicyPreviewRequest@ request where @maxResults@ was used
    -- and the  results exceeded the value of that parameter. Pagination
    -- continues from the end of the  previous results that returned the
    -- @nextToken@ value. This value is  @null@ when there are no more results
    -- to return. This option cannot be used when you specify images with
    -- @imageIds@.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of imageIDs to be included.
    imageIds :: Core.Maybe [ImageIdentifier],
    -- | The maximum number of repository results returned by
    -- @GetLifecyclePolicyPreviewRequest@ in  paginated output. When this
    -- parameter is used, @GetLifecyclePolicyPreviewRequest@ only returns 
    -- @maxResults@ results in a single page along with a @nextToken@  response
    -- element. The remaining results of the initial request can be seen by
    -- sending  another @GetLifecyclePolicyPreviewRequest@ request with the
    -- returned @nextToken@  value. This value can be between 1 and 1000. If
    -- this  parameter is not used, then @GetLifecyclePolicyPreviewRequest@
    -- returns up to  100 results and a @nextToken@ value, if  applicable. This
    -- option cannot be used when you specify images with @imageIds@.
    maxResults :: Core.Maybe Core.Natural,
    -- | The AWS account ID associated with the registry that contains the
    -- repository. If you do not specify a registry, the default registry is
    -- assumed.
    registryId :: Core.Maybe Core.Text,
    -- | An optional parameter that filters results based on image tag status and
    -- all tags, if tagged.
    filter' :: Core.Maybe LifecyclePolicyPreviewFilter,
    -- | The name of the repository.
    repositoryName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetLifecyclePolicyPreview' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getLifecyclePolicyPreview_nextToken' - The @nextToken@ value returned from a previous paginated 
-- @GetLifecyclePolicyPreviewRequest@ request where @maxResults@ was used
-- and the  results exceeded the value of that parameter. Pagination
-- continues from the end of the  previous results that returned the
-- @nextToken@ value. This value is  @null@ when there are no more results
-- to return. This option cannot be used when you specify images with
-- @imageIds@.
--
-- 'imageIds', 'getLifecyclePolicyPreview_imageIds' - The list of imageIDs to be included.
--
-- 'maxResults', 'getLifecyclePolicyPreview_maxResults' - The maximum number of repository results returned by
-- @GetLifecyclePolicyPreviewRequest@ in  paginated output. When this
-- parameter is used, @GetLifecyclePolicyPreviewRequest@ only returns 
-- @maxResults@ results in a single page along with a @nextToken@  response
-- element. The remaining results of the initial request can be seen by
-- sending  another @GetLifecyclePolicyPreviewRequest@ request with the
-- returned @nextToken@  value. This value can be between 1 and 1000. If
-- this  parameter is not used, then @GetLifecyclePolicyPreviewRequest@
-- returns up to  100 results and a @nextToken@ value, if  applicable. This
-- option cannot be used when you specify images with @imageIds@.
--
-- 'registryId', 'getLifecyclePolicyPreview_registryId' - The AWS account ID associated with the registry that contains the
-- repository. If you do not specify a registry, the default registry is
-- assumed.
--
-- 'filter'', 'getLifecyclePolicyPreview_filter' - An optional parameter that filters results based on image tag status and
-- all tags, if tagged.
--
-- 'repositoryName', 'getLifecyclePolicyPreview_repositoryName' - The name of the repository.
newGetLifecyclePolicyPreview ::
  -- | 'repositoryName'
  Core.Text ->
  GetLifecyclePolicyPreview
newGetLifecyclePolicyPreview pRepositoryName_ =
  GetLifecyclePolicyPreview'
    { nextToken =
        Core.Nothing,
      imageIds = Core.Nothing,
      maxResults = Core.Nothing,
      registryId = Core.Nothing,
      filter' = Core.Nothing,
      repositoryName = pRepositoryName_
    }

-- | The @nextToken@ value returned from a previous paginated 
-- @GetLifecyclePolicyPreviewRequest@ request where @maxResults@ was used
-- and the  results exceeded the value of that parameter. Pagination
-- continues from the end of the  previous results that returned the
-- @nextToken@ value. This value is  @null@ when there are no more results
-- to return. This option cannot be used when you specify images with
-- @imageIds@.
getLifecyclePolicyPreview_nextToken :: Lens.Lens' GetLifecyclePolicyPreview (Core.Maybe Core.Text)
getLifecyclePolicyPreview_nextToken = Lens.lens (\GetLifecyclePolicyPreview' {nextToken} -> nextToken) (\s@GetLifecyclePolicyPreview' {} a -> s {nextToken = a} :: GetLifecyclePolicyPreview)

-- | The list of imageIDs to be included.
getLifecyclePolicyPreview_imageIds :: Lens.Lens' GetLifecyclePolicyPreview (Core.Maybe [ImageIdentifier])
getLifecyclePolicyPreview_imageIds = Lens.lens (\GetLifecyclePolicyPreview' {imageIds} -> imageIds) (\s@GetLifecyclePolicyPreview' {} a -> s {imageIds = a} :: GetLifecyclePolicyPreview) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of repository results returned by
-- @GetLifecyclePolicyPreviewRequest@ in  paginated output. When this
-- parameter is used, @GetLifecyclePolicyPreviewRequest@ only returns 
-- @maxResults@ results in a single page along with a @nextToken@  response
-- element. The remaining results of the initial request can be seen by
-- sending  another @GetLifecyclePolicyPreviewRequest@ request with the
-- returned @nextToken@  value. This value can be between 1 and 1000. If
-- this  parameter is not used, then @GetLifecyclePolicyPreviewRequest@
-- returns up to  100 results and a @nextToken@ value, if  applicable. This
-- option cannot be used when you specify images with @imageIds@.
getLifecyclePolicyPreview_maxResults :: Lens.Lens' GetLifecyclePolicyPreview (Core.Maybe Core.Natural)
getLifecyclePolicyPreview_maxResults = Lens.lens (\GetLifecyclePolicyPreview' {maxResults} -> maxResults) (\s@GetLifecyclePolicyPreview' {} a -> s {maxResults = a} :: GetLifecyclePolicyPreview)

-- | The AWS account ID associated with the registry that contains the
-- repository. If you do not specify a registry, the default registry is
-- assumed.
getLifecyclePolicyPreview_registryId :: Lens.Lens' GetLifecyclePolicyPreview (Core.Maybe Core.Text)
getLifecyclePolicyPreview_registryId = Lens.lens (\GetLifecyclePolicyPreview' {registryId} -> registryId) (\s@GetLifecyclePolicyPreview' {} a -> s {registryId = a} :: GetLifecyclePolicyPreview)

-- | An optional parameter that filters results based on image tag status and
-- all tags, if tagged.
getLifecyclePolicyPreview_filter :: Lens.Lens' GetLifecyclePolicyPreview (Core.Maybe LifecyclePolicyPreviewFilter)
getLifecyclePolicyPreview_filter = Lens.lens (\GetLifecyclePolicyPreview' {filter'} -> filter') (\s@GetLifecyclePolicyPreview' {} a -> s {filter' = a} :: GetLifecyclePolicyPreview)

-- | The name of the repository.
getLifecyclePolicyPreview_repositoryName :: Lens.Lens' GetLifecyclePolicyPreview Core.Text
getLifecyclePolicyPreview_repositoryName = Lens.lens (\GetLifecyclePolicyPreview' {repositoryName} -> repositoryName) (\s@GetLifecyclePolicyPreview' {} a -> s {repositoryName = a} :: GetLifecyclePolicyPreview)

instance Core.AWSPager GetLifecyclePolicyPreview where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getLifecyclePolicyPreviewResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getLifecyclePolicyPreviewResponse_previewResults
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getLifecyclePolicyPreview_nextToken
          Lens..~ rs
          Lens.^? getLifecyclePolicyPreviewResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest GetLifecyclePolicyPreview where
  type
    AWSResponse GetLifecyclePolicyPreview =
      GetLifecyclePolicyPreviewResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLifecyclePolicyPreviewResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "status")
            Core.<*> (x Core..?> "registryId")
            Core.<*> (x Core..?> "repositoryName")
            Core.<*> (x Core..?> "summary")
            Core.<*> (x Core..?> "lifecyclePolicyText")
            Core.<*> (x Core..?> "previewResults" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetLifecyclePolicyPreview

instance Core.NFData GetLifecyclePolicyPreview

instance Core.ToHeaders GetLifecyclePolicyPreview where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.GetLifecyclePolicyPreview" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetLifecyclePolicyPreview where
  toJSON GetLifecyclePolicyPreview' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("imageIds" Core..=) Core.<$> imageIds,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("registryId" Core..=) Core.<$> registryId,
            ("filter" Core..=) Core.<$> filter',
            Core.Just ("repositoryName" Core..= repositoryName)
          ]
      )

instance Core.ToPath GetLifecyclePolicyPreview where
  toPath = Core.const "/"

instance Core.ToQuery GetLifecyclePolicyPreview where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetLifecyclePolicyPreviewResponse' smart constructor.
data GetLifecyclePolicyPreviewResponse = GetLifecyclePolicyPreviewResponse'
  { -- | The @nextToken@ value to include in a future @GetLifecyclePolicyPreview@
    -- request. When the results of a @GetLifecyclePolicyPreview@ request
    -- exceed @maxResults@, this value can be used to retrieve the next page of
    -- results. This value is @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The status of the lifecycle policy preview request.
    status :: Core.Maybe LifecyclePolicyPreviewStatus,
    -- | The registry ID associated with the request.
    registryId :: Core.Maybe Core.Text,
    -- | The repository name associated with the request.
    repositoryName :: Core.Maybe Core.Text,
    -- | The list of images that is returned as a result of the action.
    summary :: Core.Maybe LifecyclePolicyPreviewSummary,
    -- | The JSON lifecycle policy text.
    lifecyclePolicyText :: Core.Maybe Core.Text,
    -- | The results of the lifecycle policy preview request.
    previewResults :: Core.Maybe [LifecyclePolicyPreviewResult],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetLifecyclePolicyPreviewResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getLifecyclePolicyPreviewResponse_nextToken' - The @nextToken@ value to include in a future @GetLifecyclePolicyPreview@
-- request. When the results of a @GetLifecyclePolicyPreview@ request
-- exceed @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
--
-- 'status', 'getLifecyclePolicyPreviewResponse_status' - The status of the lifecycle policy preview request.
--
-- 'registryId', 'getLifecyclePolicyPreviewResponse_registryId' - The registry ID associated with the request.
--
-- 'repositoryName', 'getLifecyclePolicyPreviewResponse_repositoryName' - The repository name associated with the request.
--
-- 'summary', 'getLifecyclePolicyPreviewResponse_summary' - The list of images that is returned as a result of the action.
--
-- 'lifecyclePolicyText', 'getLifecyclePolicyPreviewResponse_lifecyclePolicyText' - The JSON lifecycle policy text.
--
-- 'previewResults', 'getLifecyclePolicyPreviewResponse_previewResults' - The results of the lifecycle policy preview request.
--
-- 'httpStatus', 'getLifecyclePolicyPreviewResponse_httpStatus' - The response's http status code.
newGetLifecyclePolicyPreviewResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetLifecyclePolicyPreviewResponse
newGetLifecyclePolicyPreviewResponse pHttpStatus_ =
  GetLifecyclePolicyPreviewResponse'
    { nextToken =
        Core.Nothing,
      status = Core.Nothing,
      registryId = Core.Nothing,
      repositoryName = Core.Nothing,
      summary = Core.Nothing,
      lifecyclePolicyText = Core.Nothing,
      previewResults = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ value to include in a future @GetLifecyclePolicyPreview@
-- request. When the results of a @GetLifecyclePolicyPreview@ request
-- exceed @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
getLifecyclePolicyPreviewResponse_nextToken :: Lens.Lens' GetLifecyclePolicyPreviewResponse (Core.Maybe Core.Text)
getLifecyclePolicyPreviewResponse_nextToken = Lens.lens (\GetLifecyclePolicyPreviewResponse' {nextToken} -> nextToken) (\s@GetLifecyclePolicyPreviewResponse' {} a -> s {nextToken = a} :: GetLifecyclePolicyPreviewResponse)

-- | The status of the lifecycle policy preview request.
getLifecyclePolicyPreviewResponse_status :: Lens.Lens' GetLifecyclePolicyPreviewResponse (Core.Maybe LifecyclePolicyPreviewStatus)
getLifecyclePolicyPreviewResponse_status = Lens.lens (\GetLifecyclePolicyPreviewResponse' {status} -> status) (\s@GetLifecyclePolicyPreviewResponse' {} a -> s {status = a} :: GetLifecyclePolicyPreviewResponse)

-- | The registry ID associated with the request.
getLifecyclePolicyPreviewResponse_registryId :: Lens.Lens' GetLifecyclePolicyPreviewResponse (Core.Maybe Core.Text)
getLifecyclePolicyPreviewResponse_registryId = Lens.lens (\GetLifecyclePolicyPreviewResponse' {registryId} -> registryId) (\s@GetLifecyclePolicyPreviewResponse' {} a -> s {registryId = a} :: GetLifecyclePolicyPreviewResponse)

-- | The repository name associated with the request.
getLifecyclePolicyPreviewResponse_repositoryName :: Lens.Lens' GetLifecyclePolicyPreviewResponse (Core.Maybe Core.Text)
getLifecyclePolicyPreviewResponse_repositoryName = Lens.lens (\GetLifecyclePolicyPreviewResponse' {repositoryName} -> repositoryName) (\s@GetLifecyclePolicyPreviewResponse' {} a -> s {repositoryName = a} :: GetLifecyclePolicyPreviewResponse)

-- | The list of images that is returned as a result of the action.
getLifecyclePolicyPreviewResponse_summary :: Lens.Lens' GetLifecyclePolicyPreviewResponse (Core.Maybe LifecyclePolicyPreviewSummary)
getLifecyclePolicyPreviewResponse_summary = Lens.lens (\GetLifecyclePolicyPreviewResponse' {summary} -> summary) (\s@GetLifecyclePolicyPreviewResponse' {} a -> s {summary = a} :: GetLifecyclePolicyPreviewResponse)

-- | The JSON lifecycle policy text.
getLifecyclePolicyPreviewResponse_lifecyclePolicyText :: Lens.Lens' GetLifecyclePolicyPreviewResponse (Core.Maybe Core.Text)
getLifecyclePolicyPreviewResponse_lifecyclePolicyText = Lens.lens (\GetLifecyclePolicyPreviewResponse' {lifecyclePolicyText} -> lifecyclePolicyText) (\s@GetLifecyclePolicyPreviewResponse' {} a -> s {lifecyclePolicyText = a} :: GetLifecyclePolicyPreviewResponse)

-- | The results of the lifecycle policy preview request.
getLifecyclePolicyPreviewResponse_previewResults :: Lens.Lens' GetLifecyclePolicyPreviewResponse (Core.Maybe [LifecyclePolicyPreviewResult])
getLifecyclePolicyPreviewResponse_previewResults = Lens.lens (\GetLifecyclePolicyPreviewResponse' {previewResults} -> previewResults) (\s@GetLifecyclePolicyPreviewResponse' {} a -> s {previewResults = a} :: GetLifecyclePolicyPreviewResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getLifecyclePolicyPreviewResponse_httpStatus :: Lens.Lens' GetLifecyclePolicyPreviewResponse Core.Int
getLifecyclePolicyPreviewResponse_httpStatus = Lens.lens (\GetLifecyclePolicyPreviewResponse' {httpStatus} -> httpStatus) (\s@GetLifecyclePolicyPreviewResponse' {} a -> s {httpStatus = a} :: GetLifecyclePolicyPreviewResponse)

instance
  Core.NFData
    GetLifecyclePolicyPreviewResponse
