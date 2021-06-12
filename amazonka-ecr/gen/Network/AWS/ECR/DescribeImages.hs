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
-- Module      : Network.AWS.ECR.DescribeImages
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata about the images in a repository.
--
-- Beginning with Docker version 1.9, the Docker client compresses image
-- layers before pushing them to a V2 Docker registry. The output of the
-- @docker images@ command shows the uncompressed image size, so it may
-- return a larger image size than the image sizes returned by
-- DescribeImages.
--
-- This operation returns paginated results.
module Network.AWS.ECR.DescribeImages
  ( -- * Creating a Request
    DescribeImages (..),
    newDescribeImages,

    -- * Request Lenses
    describeImages_nextToken,
    describeImages_imageIds,
    describeImages_maxResults,
    describeImages_registryId,
    describeImages_filter,
    describeImages_repositoryName,

    -- * Destructuring the Response
    DescribeImagesResponse (..),
    newDescribeImagesResponse,

    -- * Response Lenses
    describeImagesResponse_nextToken,
    describeImagesResponse_imageDetails,
    describeImagesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeImages' smart constructor.
data DescribeImages = DescribeImages'
  { -- | The @nextToken@ value returned from a previous paginated
    -- @DescribeImages@ request where @maxResults@ was used and the results
    -- exceeded the value of that parameter. Pagination continues from the end
    -- of the previous results that returned the @nextToken@ value. This value
    -- is @null@ when there are no more results to return. This option cannot
    -- be used when you specify images with @imageIds@.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of image IDs for the requested repository.
    imageIds :: Core.Maybe [ImageIdentifier],
    -- | The maximum number of repository results returned by @DescribeImages@ in
    -- paginated output. When this parameter is used, @DescribeImages@ only
    -- returns @maxResults@ results in a single page along with a @nextToken@
    -- response element. The remaining results of the initial request can be
    -- seen by sending another @DescribeImages@ request with the returned
    -- @nextToken@ value. This value can be between 1 and 1000. If this
    -- parameter is not used, then @DescribeImages@ returns up to 100 results
    -- and a @nextToken@ value, if applicable. This option cannot be used when
    -- you specify images with @imageIds@.
    maxResults :: Core.Maybe Core.Natural,
    -- | The AWS account ID associated with the registry that contains the
    -- repository in which to describe images. If you do not specify a
    -- registry, the default registry is assumed.
    registryId :: Core.Maybe Core.Text,
    -- | The filter key and value with which to filter your @DescribeImages@
    -- results.
    filter' :: Core.Maybe DescribeImagesFilter,
    -- | The repository that contains the images to describe.
    repositoryName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeImages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeImages_nextToken' - The @nextToken@ value returned from a previous paginated
-- @DescribeImages@ request where @maxResults@ was used and the results
-- exceeded the value of that parameter. Pagination continues from the end
-- of the previous results that returned the @nextToken@ value. This value
-- is @null@ when there are no more results to return. This option cannot
-- be used when you specify images with @imageIds@.
--
-- 'imageIds', 'describeImages_imageIds' - The list of image IDs for the requested repository.
--
-- 'maxResults', 'describeImages_maxResults' - The maximum number of repository results returned by @DescribeImages@ in
-- paginated output. When this parameter is used, @DescribeImages@ only
-- returns @maxResults@ results in a single page along with a @nextToken@
-- response element. The remaining results of the initial request can be
-- seen by sending another @DescribeImages@ request with the returned
-- @nextToken@ value. This value can be between 1 and 1000. If this
-- parameter is not used, then @DescribeImages@ returns up to 100 results
-- and a @nextToken@ value, if applicable. This option cannot be used when
-- you specify images with @imageIds@.
--
-- 'registryId', 'describeImages_registryId' - The AWS account ID associated with the registry that contains the
-- repository in which to describe images. If you do not specify a
-- registry, the default registry is assumed.
--
-- 'filter'', 'describeImages_filter' - The filter key and value with which to filter your @DescribeImages@
-- results.
--
-- 'repositoryName', 'describeImages_repositoryName' - The repository that contains the images to describe.
newDescribeImages ::
  -- | 'repositoryName'
  Core.Text ->
  DescribeImages
newDescribeImages pRepositoryName_ =
  DescribeImages'
    { nextToken = Core.Nothing,
      imageIds = Core.Nothing,
      maxResults = Core.Nothing,
      registryId = Core.Nothing,
      filter' = Core.Nothing,
      repositoryName = pRepositoryName_
    }

-- | The @nextToken@ value returned from a previous paginated
-- @DescribeImages@ request where @maxResults@ was used and the results
-- exceeded the value of that parameter. Pagination continues from the end
-- of the previous results that returned the @nextToken@ value. This value
-- is @null@ when there are no more results to return. This option cannot
-- be used when you specify images with @imageIds@.
describeImages_nextToken :: Lens.Lens' DescribeImages (Core.Maybe Core.Text)
describeImages_nextToken = Lens.lens (\DescribeImages' {nextToken} -> nextToken) (\s@DescribeImages' {} a -> s {nextToken = a} :: DescribeImages)

-- | The list of image IDs for the requested repository.
describeImages_imageIds :: Lens.Lens' DescribeImages (Core.Maybe [ImageIdentifier])
describeImages_imageIds = Lens.lens (\DescribeImages' {imageIds} -> imageIds) (\s@DescribeImages' {} a -> s {imageIds = a} :: DescribeImages) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of repository results returned by @DescribeImages@ in
-- paginated output. When this parameter is used, @DescribeImages@ only
-- returns @maxResults@ results in a single page along with a @nextToken@
-- response element. The remaining results of the initial request can be
-- seen by sending another @DescribeImages@ request with the returned
-- @nextToken@ value. This value can be between 1 and 1000. If this
-- parameter is not used, then @DescribeImages@ returns up to 100 results
-- and a @nextToken@ value, if applicable. This option cannot be used when
-- you specify images with @imageIds@.
describeImages_maxResults :: Lens.Lens' DescribeImages (Core.Maybe Core.Natural)
describeImages_maxResults = Lens.lens (\DescribeImages' {maxResults} -> maxResults) (\s@DescribeImages' {} a -> s {maxResults = a} :: DescribeImages)

-- | The AWS account ID associated with the registry that contains the
-- repository in which to describe images. If you do not specify a
-- registry, the default registry is assumed.
describeImages_registryId :: Lens.Lens' DescribeImages (Core.Maybe Core.Text)
describeImages_registryId = Lens.lens (\DescribeImages' {registryId} -> registryId) (\s@DescribeImages' {} a -> s {registryId = a} :: DescribeImages)

-- | The filter key and value with which to filter your @DescribeImages@
-- results.
describeImages_filter :: Lens.Lens' DescribeImages (Core.Maybe DescribeImagesFilter)
describeImages_filter = Lens.lens (\DescribeImages' {filter'} -> filter') (\s@DescribeImages' {} a -> s {filter' = a} :: DescribeImages)

-- | The repository that contains the images to describe.
describeImages_repositoryName :: Lens.Lens' DescribeImages Core.Text
describeImages_repositoryName = Lens.lens (\DescribeImages' {repositoryName} -> repositoryName) (\s@DescribeImages' {} a -> s {repositoryName = a} :: DescribeImages)

instance Core.AWSPager DescribeImages where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeImagesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeImagesResponse_imageDetails
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeImages_nextToken
          Lens..~ rs
          Lens.^? describeImagesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest DescribeImages where
  type
    AWSResponse DescribeImages =
      DescribeImagesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeImagesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "imageDetails" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeImages

instance Core.NFData DescribeImages

instance Core.ToHeaders DescribeImages where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.DescribeImages" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeImages where
  toJSON DescribeImages' {..} =
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

instance Core.ToPath DescribeImages where
  toPath = Core.const "/"

instance Core.ToQuery DescribeImages where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeImagesResponse' smart constructor.
data DescribeImagesResponse = DescribeImagesResponse'
  { -- | The @nextToken@ value to include in a future @DescribeImages@ request.
    -- When the results of a @DescribeImages@ request exceed @maxResults@, this
    -- value can be used to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of ImageDetail objects that contain data about the image.
    imageDetails :: Core.Maybe [ImageDetail],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeImagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeImagesResponse_nextToken' - The @nextToken@ value to include in a future @DescribeImages@ request.
-- When the results of a @DescribeImages@ request exceed @maxResults@, this
-- value can be used to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'imageDetails', 'describeImagesResponse_imageDetails' - A list of ImageDetail objects that contain data about the image.
--
-- 'httpStatus', 'describeImagesResponse_httpStatus' - The response's http status code.
newDescribeImagesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeImagesResponse
newDescribeImagesResponse pHttpStatus_ =
  DescribeImagesResponse'
    { nextToken = Core.Nothing,
      imageDetails = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ value to include in a future @DescribeImages@ request.
-- When the results of a @DescribeImages@ request exceed @maxResults@, this
-- value can be used to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeImagesResponse_nextToken :: Lens.Lens' DescribeImagesResponse (Core.Maybe Core.Text)
describeImagesResponse_nextToken = Lens.lens (\DescribeImagesResponse' {nextToken} -> nextToken) (\s@DescribeImagesResponse' {} a -> s {nextToken = a} :: DescribeImagesResponse)

-- | A list of ImageDetail objects that contain data about the image.
describeImagesResponse_imageDetails :: Lens.Lens' DescribeImagesResponse (Core.Maybe [ImageDetail])
describeImagesResponse_imageDetails = Lens.lens (\DescribeImagesResponse' {imageDetails} -> imageDetails) (\s@DescribeImagesResponse' {} a -> s {imageDetails = a} :: DescribeImagesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeImagesResponse_httpStatus :: Lens.Lens' DescribeImagesResponse Core.Int
describeImagesResponse_httpStatus = Lens.lens (\DescribeImagesResponse' {httpStatus} -> httpStatus) (\s@DescribeImagesResponse' {} a -> s {httpStatus = a} :: DescribeImagesResponse)

instance Core.NFData DescribeImagesResponse
