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
-- Module      : Network.AWS.ECR.ListImages
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the image IDs for the specified repository.
--
-- You can filter images based on whether or not they are tagged by using
-- the @tagStatus@ filter and specifying either @TAGGED@, @UNTAGGED@ or
-- @ANY@. For example, you can filter your results to return only
-- @UNTAGGED@ images and then pipe that result to a BatchDeleteImage
-- operation to delete them. Or, you can filter your results to return only
-- @TAGGED@ images to list all of the tags in your repository.
--
-- This operation returns paginated results.
module Network.AWS.ECR.ListImages
  ( -- * Creating a Request
    ListImages (..),
    newListImages,

    -- * Request Lenses
    listImages_nextToken,
    listImages_maxResults,
    listImages_registryId,
    listImages_filter,
    listImages_repositoryName,

    -- * Destructuring the Response
    ListImagesResponse (..),
    newListImagesResponse,

    -- * Response Lenses
    listImagesResponse_nextToken,
    listImagesResponse_imageIds,
    listImagesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListImages' smart constructor.
data ListImages = ListImages'
  { -- | The @nextToken@ value returned from a previous paginated @ListImages@
    -- request where @maxResults@ was used and the results exceeded the value
    -- of that parameter. Pagination continues from the end of the previous
    -- results that returned the @nextToken@ value. This value is @null@ when
    -- there are no more results to return.
    --
    -- This token should be treated as an opaque identifier that is only used
    -- to retrieve the next items in a list and not for other programmatic
    -- purposes.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of image results returned by @ListImages@ in
    -- paginated output. When this parameter is used, @ListImages@ only returns
    -- @maxResults@ results in a single page along with a @nextToken@ response
    -- element. The remaining results of the initial request can be seen by
    -- sending another @ListImages@ request with the returned @nextToken@
    -- value. This value can be between 1 and 1000. If this parameter is not
    -- used, then @ListImages@ returns up to 100 results and a @nextToken@
    -- value, if applicable.
    maxResults :: Core.Maybe Core.Natural,
    -- | The AWS account ID associated with the registry that contains the
    -- repository in which to list images. If you do not specify a registry,
    -- the default registry is assumed.
    registryId :: Core.Maybe Core.Text,
    -- | The filter key and value with which to filter your @ListImages@ results.
    filter' :: Core.Maybe ListImagesFilter,
    -- | The repository with image IDs to be listed.
    repositoryName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListImages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listImages_nextToken' - The @nextToken@ value returned from a previous paginated @ListImages@
-- request where @maxResults@ was used and the results exceeded the value
-- of that parameter. Pagination continues from the end of the previous
-- results that returned the @nextToken@ value. This value is @null@ when
-- there are no more results to return.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
--
-- 'maxResults', 'listImages_maxResults' - The maximum number of image results returned by @ListImages@ in
-- paginated output. When this parameter is used, @ListImages@ only returns
-- @maxResults@ results in a single page along with a @nextToken@ response
-- element. The remaining results of the initial request can be seen by
-- sending another @ListImages@ request with the returned @nextToken@
-- value. This value can be between 1 and 1000. If this parameter is not
-- used, then @ListImages@ returns up to 100 results and a @nextToken@
-- value, if applicable.
--
-- 'registryId', 'listImages_registryId' - The AWS account ID associated with the registry that contains the
-- repository in which to list images. If you do not specify a registry,
-- the default registry is assumed.
--
-- 'filter'', 'listImages_filter' - The filter key and value with which to filter your @ListImages@ results.
--
-- 'repositoryName', 'listImages_repositoryName' - The repository with image IDs to be listed.
newListImages ::
  -- | 'repositoryName'
  Core.Text ->
  ListImages
newListImages pRepositoryName_ =
  ListImages'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      registryId = Core.Nothing,
      filter' = Core.Nothing,
      repositoryName = pRepositoryName_
    }

-- | The @nextToken@ value returned from a previous paginated @ListImages@
-- request where @maxResults@ was used and the results exceeded the value
-- of that parameter. Pagination continues from the end of the previous
-- results that returned the @nextToken@ value. This value is @null@ when
-- there are no more results to return.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
listImages_nextToken :: Lens.Lens' ListImages (Core.Maybe Core.Text)
listImages_nextToken = Lens.lens (\ListImages' {nextToken} -> nextToken) (\s@ListImages' {} a -> s {nextToken = a} :: ListImages)

-- | The maximum number of image results returned by @ListImages@ in
-- paginated output. When this parameter is used, @ListImages@ only returns
-- @maxResults@ results in a single page along with a @nextToken@ response
-- element. The remaining results of the initial request can be seen by
-- sending another @ListImages@ request with the returned @nextToken@
-- value. This value can be between 1 and 1000. If this parameter is not
-- used, then @ListImages@ returns up to 100 results and a @nextToken@
-- value, if applicable.
listImages_maxResults :: Lens.Lens' ListImages (Core.Maybe Core.Natural)
listImages_maxResults = Lens.lens (\ListImages' {maxResults} -> maxResults) (\s@ListImages' {} a -> s {maxResults = a} :: ListImages)

-- | The AWS account ID associated with the registry that contains the
-- repository in which to list images. If you do not specify a registry,
-- the default registry is assumed.
listImages_registryId :: Lens.Lens' ListImages (Core.Maybe Core.Text)
listImages_registryId = Lens.lens (\ListImages' {registryId} -> registryId) (\s@ListImages' {} a -> s {registryId = a} :: ListImages)

-- | The filter key and value with which to filter your @ListImages@ results.
listImages_filter :: Lens.Lens' ListImages (Core.Maybe ListImagesFilter)
listImages_filter = Lens.lens (\ListImages' {filter'} -> filter') (\s@ListImages' {} a -> s {filter' = a} :: ListImages)

-- | The repository with image IDs to be listed.
listImages_repositoryName :: Lens.Lens' ListImages Core.Text
listImages_repositoryName = Lens.lens (\ListImages' {repositoryName} -> repositoryName) (\s@ListImages' {} a -> s {repositoryName = a} :: ListImages)

instance Core.AWSPager ListImages where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listImagesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listImagesResponse_imageIds Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listImages_nextToken
          Lens..~ rs
          Lens.^? listImagesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListImages where
  type AWSResponse ListImages = ListImagesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListImagesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "imageIds" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListImages

instance Core.NFData ListImages

instance Core.ToHeaders ListImages where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.ListImages" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListImages where
  toJSON ListImages' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("registryId" Core..=) Core.<$> registryId,
            ("filter" Core..=) Core.<$> filter',
            Core.Just ("repositoryName" Core..= repositoryName)
          ]
      )

instance Core.ToPath ListImages where
  toPath = Core.const "/"

instance Core.ToQuery ListImages where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListImagesResponse' smart constructor.
data ListImagesResponse = ListImagesResponse'
  { -- | The @nextToken@ value to include in a future @ListImages@ request. When
    -- the results of a @ListImages@ request exceed @maxResults@, this value
    -- can be used to retrieve the next page of results. This value is @null@
    -- when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of image IDs for the requested repository.
    imageIds :: Core.Maybe [ImageIdentifier],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListImagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listImagesResponse_nextToken' - The @nextToken@ value to include in a future @ListImages@ request. When
-- the results of a @ListImages@ request exceed @maxResults@, this value
-- can be used to retrieve the next page of results. This value is @null@
-- when there are no more results to return.
--
-- 'imageIds', 'listImagesResponse_imageIds' - The list of image IDs for the requested repository.
--
-- 'httpStatus', 'listImagesResponse_httpStatus' - The response's http status code.
newListImagesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListImagesResponse
newListImagesResponse pHttpStatus_ =
  ListImagesResponse'
    { nextToken = Core.Nothing,
      imageIds = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ value to include in a future @ListImages@ request. When
-- the results of a @ListImages@ request exceed @maxResults@, this value
-- can be used to retrieve the next page of results. This value is @null@
-- when there are no more results to return.
listImagesResponse_nextToken :: Lens.Lens' ListImagesResponse (Core.Maybe Core.Text)
listImagesResponse_nextToken = Lens.lens (\ListImagesResponse' {nextToken} -> nextToken) (\s@ListImagesResponse' {} a -> s {nextToken = a} :: ListImagesResponse)

-- | The list of image IDs for the requested repository.
listImagesResponse_imageIds :: Lens.Lens' ListImagesResponse (Core.Maybe [ImageIdentifier])
listImagesResponse_imageIds = Lens.lens (\ListImagesResponse' {imageIds} -> imageIds) (\s@ListImagesResponse' {} a -> s {imageIds = a} :: ListImagesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listImagesResponse_httpStatus :: Lens.Lens' ListImagesResponse Core.Int
listImagesResponse_httpStatus = Lens.lens (\ListImagesResponse' {httpStatus} -> httpStatus) (\s@ListImagesResponse' {} a -> s {httpStatus = a} :: ListImagesResponse)

instance Core.NFData ListImagesResponse
