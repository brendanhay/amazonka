{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
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
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of imageIDs to be included.
    imageIds :: Prelude.Maybe [ImageIdentifier],
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
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The AWS account ID associated with the registry that contains the
    -- repository. If you do not specify a registry, the default registry is
    -- assumed.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | An optional parameter that filters results based on image tag status and
    -- all tags, if tagged.
    filter' :: Prelude.Maybe LifecyclePolicyPreviewFilter,
    -- | The name of the repository.
    repositoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  GetLifecyclePolicyPreview
newGetLifecyclePolicyPreview pRepositoryName_ =
  GetLifecyclePolicyPreview'
    { nextToken =
        Prelude.Nothing,
      imageIds = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      registryId = Prelude.Nothing,
      filter' = Prelude.Nothing,
      repositoryName = pRepositoryName_
    }

-- | The @nextToken@ value returned from a previous paginated 
-- @GetLifecyclePolicyPreviewRequest@ request where @maxResults@ was used
-- and the  results exceeded the value of that parameter. Pagination
-- continues from the end of the  previous results that returned the
-- @nextToken@ value. This value is  @null@ when there are no more results
-- to return. This option cannot be used when you specify images with
-- @imageIds@.
getLifecyclePolicyPreview_nextToken :: Lens.Lens' GetLifecyclePolicyPreview (Prelude.Maybe Prelude.Text)
getLifecyclePolicyPreview_nextToken = Lens.lens (\GetLifecyclePolicyPreview' {nextToken} -> nextToken) (\s@GetLifecyclePolicyPreview' {} a -> s {nextToken = a} :: GetLifecyclePolicyPreview)

-- | The list of imageIDs to be included.
getLifecyclePolicyPreview_imageIds :: Lens.Lens' GetLifecyclePolicyPreview (Prelude.Maybe [ImageIdentifier])
getLifecyclePolicyPreview_imageIds = Lens.lens (\GetLifecyclePolicyPreview' {imageIds} -> imageIds) (\s@GetLifecyclePolicyPreview' {} a -> s {imageIds = a} :: GetLifecyclePolicyPreview) Prelude.. Lens.mapping Prelude._Coerce

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
getLifecyclePolicyPreview_maxResults :: Lens.Lens' GetLifecyclePolicyPreview (Prelude.Maybe Prelude.Natural)
getLifecyclePolicyPreview_maxResults = Lens.lens (\GetLifecyclePolicyPreview' {maxResults} -> maxResults) (\s@GetLifecyclePolicyPreview' {} a -> s {maxResults = a} :: GetLifecyclePolicyPreview)

-- | The AWS account ID associated with the registry that contains the
-- repository. If you do not specify a registry, the default registry is
-- assumed.
getLifecyclePolicyPreview_registryId :: Lens.Lens' GetLifecyclePolicyPreview (Prelude.Maybe Prelude.Text)
getLifecyclePolicyPreview_registryId = Lens.lens (\GetLifecyclePolicyPreview' {registryId} -> registryId) (\s@GetLifecyclePolicyPreview' {} a -> s {registryId = a} :: GetLifecyclePolicyPreview)

-- | An optional parameter that filters results based on image tag status and
-- all tags, if tagged.
getLifecyclePolicyPreview_filter :: Lens.Lens' GetLifecyclePolicyPreview (Prelude.Maybe LifecyclePolicyPreviewFilter)
getLifecyclePolicyPreview_filter = Lens.lens (\GetLifecyclePolicyPreview' {filter'} -> filter') (\s@GetLifecyclePolicyPreview' {} a -> s {filter' = a} :: GetLifecyclePolicyPreview)

-- | The name of the repository.
getLifecyclePolicyPreview_repositoryName :: Lens.Lens' GetLifecyclePolicyPreview Prelude.Text
getLifecyclePolicyPreview_repositoryName = Lens.lens (\GetLifecyclePolicyPreview' {repositoryName} -> repositoryName) (\s@GetLifecyclePolicyPreview' {} a -> s {repositoryName = a} :: GetLifecyclePolicyPreview)

instance Pager.AWSPager GetLifecyclePolicyPreview where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? getLifecyclePolicyPreviewResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? getLifecyclePolicyPreviewResponse_previewResults
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& getLifecyclePolicyPreview_nextToken
          Lens..~ rs
          Lens.^? getLifecyclePolicyPreviewResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest GetLifecyclePolicyPreview where
  type
    Rs GetLifecyclePolicyPreview =
      GetLifecyclePolicyPreviewResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLifecyclePolicyPreviewResponse'
            Prelude.<$> (x Prelude..?> "nextToken")
            Prelude.<*> (x Prelude..?> "status")
            Prelude.<*> (x Prelude..?> "registryId")
            Prelude.<*> (x Prelude..?> "repositoryName")
            Prelude.<*> (x Prelude..?> "summary")
            Prelude.<*> (x Prelude..?> "lifecyclePolicyText")
            Prelude.<*> ( x Prelude..?> "previewResults"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLifecyclePolicyPreview

instance Prelude.NFData GetLifecyclePolicyPreview

instance Prelude.ToHeaders GetLifecyclePolicyPreview where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonEC2ContainerRegistry_V20150921.GetLifecyclePolicyPreview" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetLifecyclePolicyPreview where
  toJSON GetLifecyclePolicyPreview' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("nextToken" Prelude..=) Prelude.<$> nextToken,
            ("imageIds" Prelude..=) Prelude.<$> imageIds,
            ("maxResults" Prelude..=) Prelude.<$> maxResults,
            ("registryId" Prelude..=) Prelude.<$> registryId,
            ("filter" Prelude..=) Prelude.<$> filter',
            Prelude.Just
              ("repositoryName" Prelude..= repositoryName)
          ]
      )

instance Prelude.ToPath GetLifecyclePolicyPreview where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetLifecyclePolicyPreview where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLifecyclePolicyPreviewResponse' smart constructor.
data GetLifecyclePolicyPreviewResponse = GetLifecyclePolicyPreviewResponse'
  { -- | The @nextToken@ value to include in a future @GetLifecyclePolicyPreview@
    -- request. When the results of a @GetLifecyclePolicyPreview@ request
    -- exceed @maxResults@, this value can be used to retrieve the next page of
    -- results. This value is @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The status of the lifecycle policy preview request.
    status :: Prelude.Maybe LifecyclePolicyPreviewStatus,
    -- | The registry ID associated with the request.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The repository name associated with the request.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The list of images that is returned as a result of the action.
    summary :: Prelude.Maybe LifecyclePolicyPreviewSummary,
    -- | The JSON lifecycle policy text.
    lifecyclePolicyText :: Prelude.Maybe Prelude.Text,
    -- | The results of the lifecycle policy preview request.
    previewResults :: Prelude.Maybe [LifecyclePolicyPreviewResult],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetLifecyclePolicyPreviewResponse
newGetLifecyclePolicyPreviewResponse pHttpStatus_ =
  GetLifecyclePolicyPreviewResponse'
    { nextToken =
        Prelude.Nothing,
      status = Prelude.Nothing,
      registryId = Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      summary = Prelude.Nothing,
      lifecyclePolicyText = Prelude.Nothing,
      previewResults = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ value to include in a future @GetLifecyclePolicyPreview@
-- request. When the results of a @GetLifecyclePolicyPreview@ request
-- exceed @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
getLifecyclePolicyPreviewResponse_nextToken :: Lens.Lens' GetLifecyclePolicyPreviewResponse (Prelude.Maybe Prelude.Text)
getLifecyclePolicyPreviewResponse_nextToken = Lens.lens (\GetLifecyclePolicyPreviewResponse' {nextToken} -> nextToken) (\s@GetLifecyclePolicyPreviewResponse' {} a -> s {nextToken = a} :: GetLifecyclePolicyPreviewResponse)

-- | The status of the lifecycle policy preview request.
getLifecyclePolicyPreviewResponse_status :: Lens.Lens' GetLifecyclePolicyPreviewResponse (Prelude.Maybe LifecyclePolicyPreviewStatus)
getLifecyclePolicyPreviewResponse_status = Lens.lens (\GetLifecyclePolicyPreviewResponse' {status} -> status) (\s@GetLifecyclePolicyPreviewResponse' {} a -> s {status = a} :: GetLifecyclePolicyPreviewResponse)

-- | The registry ID associated with the request.
getLifecyclePolicyPreviewResponse_registryId :: Lens.Lens' GetLifecyclePolicyPreviewResponse (Prelude.Maybe Prelude.Text)
getLifecyclePolicyPreviewResponse_registryId = Lens.lens (\GetLifecyclePolicyPreviewResponse' {registryId} -> registryId) (\s@GetLifecyclePolicyPreviewResponse' {} a -> s {registryId = a} :: GetLifecyclePolicyPreviewResponse)

-- | The repository name associated with the request.
getLifecyclePolicyPreviewResponse_repositoryName :: Lens.Lens' GetLifecyclePolicyPreviewResponse (Prelude.Maybe Prelude.Text)
getLifecyclePolicyPreviewResponse_repositoryName = Lens.lens (\GetLifecyclePolicyPreviewResponse' {repositoryName} -> repositoryName) (\s@GetLifecyclePolicyPreviewResponse' {} a -> s {repositoryName = a} :: GetLifecyclePolicyPreviewResponse)

-- | The list of images that is returned as a result of the action.
getLifecyclePolicyPreviewResponse_summary :: Lens.Lens' GetLifecyclePolicyPreviewResponse (Prelude.Maybe LifecyclePolicyPreviewSummary)
getLifecyclePolicyPreviewResponse_summary = Lens.lens (\GetLifecyclePolicyPreviewResponse' {summary} -> summary) (\s@GetLifecyclePolicyPreviewResponse' {} a -> s {summary = a} :: GetLifecyclePolicyPreviewResponse)

-- | The JSON lifecycle policy text.
getLifecyclePolicyPreviewResponse_lifecyclePolicyText :: Lens.Lens' GetLifecyclePolicyPreviewResponse (Prelude.Maybe Prelude.Text)
getLifecyclePolicyPreviewResponse_lifecyclePolicyText = Lens.lens (\GetLifecyclePolicyPreviewResponse' {lifecyclePolicyText} -> lifecyclePolicyText) (\s@GetLifecyclePolicyPreviewResponse' {} a -> s {lifecyclePolicyText = a} :: GetLifecyclePolicyPreviewResponse)

-- | The results of the lifecycle policy preview request.
getLifecyclePolicyPreviewResponse_previewResults :: Lens.Lens' GetLifecyclePolicyPreviewResponse (Prelude.Maybe [LifecyclePolicyPreviewResult])
getLifecyclePolicyPreviewResponse_previewResults = Lens.lens (\GetLifecyclePolicyPreviewResponse' {previewResults} -> previewResults) (\s@GetLifecyclePolicyPreviewResponse' {} a -> s {previewResults = a} :: GetLifecyclePolicyPreviewResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
getLifecyclePolicyPreviewResponse_httpStatus :: Lens.Lens' GetLifecyclePolicyPreviewResponse Prelude.Int
getLifecyclePolicyPreviewResponse_httpStatus = Lens.lens (\GetLifecyclePolicyPreviewResponse' {httpStatus} -> httpStatus) (\s@GetLifecyclePolicyPreviewResponse' {} a -> s {httpStatus = a} :: GetLifecyclePolicyPreviewResponse)

instance
  Prelude.NFData
    GetLifecyclePolicyPreviewResponse
