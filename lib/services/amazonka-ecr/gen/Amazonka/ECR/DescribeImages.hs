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
-- Module      : Amazonka.ECR.DescribeImages
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
module Amazonka.ECR.DescribeImages
  ( -- * Creating a Request
    DescribeImages (..),
    newDescribeImages,

    -- * Request Lenses
    describeImages_registryId,
    describeImages_imageIds,
    describeImages_nextToken,
    describeImages_filter,
    describeImages_maxResults,
    describeImages_repositoryName,

    -- * Destructuring the Response
    DescribeImagesResponse (..),
    newDescribeImagesResponse,

    -- * Response Lenses
    describeImagesResponse_imageDetails,
    describeImagesResponse_nextToken,
    describeImagesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.ECR.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeImages' smart constructor.
data DescribeImages = DescribeImages'
  { -- | The Amazon Web Services account ID associated with the registry that
    -- contains the repository in which to describe images. If you do not
    -- specify a registry, the default registry is assumed.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The list of image IDs for the requested repository.
    imageIds :: Prelude.Maybe [ImageIdentifier],
    -- | The @nextToken@ value returned from a previous paginated
    -- @DescribeImages@ request where @maxResults@ was used and the results
    -- exceeded the value of that parameter. Pagination continues from the end
    -- of the previous results that returned the @nextToken@ value. This value
    -- is @null@ when there are no more results to return. This option cannot
    -- be used when you specify images with @imageIds@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The filter key and value with which to filter your @DescribeImages@
    -- results.
    filter' :: Prelude.Maybe DescribeImagesFilter,
    -- | The maximum number of repository results returned by @DescribeImages@ in
    -- paginated output. When this parameter is used, @DescribeImages@ only
    -- returns @maxResults@ results in a single page along with a @nextToken@
    -- response element. The remaining results of the initial request can be
    -- seen by sending another @DescribeImages@ request with the returned
    -- @nextToken@ value. This value can be between 1 and 1000. If this
    -- parameter is not used, then @DescribeImages@ returns up to 100 results
    -- and a @nextToken@ value, if applicable. This option cannot be used when
    -- you specify images with @imageIds@.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The repository that contains the images to describe.
    repositoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeImages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'describeImages_registryId' - The Amazon Web Services account ID associated with the registry that
-- contains the repository in which to describe images. If you do not
-- specify a registry, the default registry is assumed.
--
-- 'imageIds', 'describeImages_imageIds' - The list of image IDs for the requested repository.
--
-- 'nextToken', 'describeImages_nextToken' - The @nextToken@ value returned from a previous paginated
-- @DescribeImages@ request where @maxResults@ was used and the results
-- exceeded the value of that parameter. Pagination continues from the end
-- of the previous results that returned the @nextToken@ value. This value
-- is @null@ when there are no more results to return. This option cannot
-- be used when you specify images with @imageIds@.
--
-- 'filter'', 'describeImages_filter' - The filter key and value with which to filter your @DescribeImages@
-- results.
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
-- 'repositoryName', 'describeImages_repositoryName' - The repository that contains the images to describe.
newDescribeImages ::
  -- | 'repositoryName'
  Prelude.Text ->
  DescribeImages
newDescribeImages pRepositoryName_ =
  DescribeImages'
    { registryId = Prelude.Nothing,
      imageIds = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      repositoryName = pRepositoryName_
    }

-- | The Amazon Web Services account ID associated with the registry that
-- contains the repository in which to describe images. If you do not
-- specify a registry, the default registry is assumed.
describeImages_registryId :: Lens.Lens' DescribeImages (Prelude.Maybe Prelude.Text)
describeImages_registryId = Lens.lens (\DescribeImages' {registryId} -> registryId) (\s@DescribeImages' {} a -> s {registryId = a} :: DescribeImages)

-- | The list of image IDs for the requested repository.
describeImages_imageIds :: Lens.Lens' DescribeImages (Prelude.Maybe [ImageIdentifier])
describeImages_imageIds = Lens.lens (\DescribeImages' {imageIds} -> imageIds) (\s@DescribeImages' {} a -> s {imageIds = a} :: DescribeImages) Prelude.. Lens.mapping Lens.coerced

-- | The @nextToken@ value returned from a previous paginated
-- @DescribeImages@ request where @maxResults@ was used and the results
-- exceeded the value of that parameter. Pagination continues from the end
-- of the previous results that returned the @nextToken@ value. This value
-- is @null@ when there are no more results to return. This option cannot
-- be used when you specify images with @imageIds@.
describeImages_nextToken :: Lens.Lens' DescribeImages (Prelude.Maybe Prelude.Text)
describeImages_nextToken = Lens.lens (\DescribeImages' {nextToken} -> nextToken) (\s@DescribeImages' {} a -> s {nextToken = a} :: DescribeImages)

-- | The filter key and value with which to filter your @DescribeImages@
-- results.
describeImages_filter :: Lens.Lens' DescribeImages (Prelude.Maybe DescribeImagesFilter)
describeImages_filter = Lens.lens (\DescribeImages' {filter'} -> filter') (\s@DescribeImages' {} a -> s {filter' = a} :: DescribeImages)

-- | The maximum number of repository results returned by @DescribeImages@ in
-- paginated output. When this parameter is used, @DescribeImages@ only
-- returns @maxResults@ results in a single page along with a @nextToken@
-- response element. The remaining results of the initial request can be
-- seen by sending another @DescribeImages@ request with the returned
-- @nextToken@ value. This value can be between 1 and 1000. If this
-- parameter is not used, then @DescribeImages@ returns up to 100 results
-- and a @nextToken@ value, if applicable. This option cannot be used when
-- you specify images with @imageIds@.
describeImages_maxResults :: Lens.Lens' DescribeImages (Prelude.Maybe Prelude.Natural)
describeImages_maxResults = Lens.lens (\DescribeImages' {maxResults} -> maxResults) (\s@DescribeImages' {} a -> s {maxResults = a} :: DescribeImages)

-- | The repository that contains the images to describe.
describeImages_repositoryName :: Lens.Lens' DescribeImages Prelude.Text
describeImages_repositoryName = Lens.lens (\DescribeImages' {repositoryName} -> repositoryName) (\s@DescribeImages' {} a -> s {repositoryName = a} :: DescribeImages)

instance Core.AWSPager DescribeImages where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeImagesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeImagesResponse_imageDetails
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeImages_nextToken
          Lens..~ rs
          Lens.^? describeImagesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest DescribeImages where
  type
    AWSResponse DescribeImages =
      DescribeImagesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeImagesResponse'
            Prelude.<$> (x Core..?> "imageDetails" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeImages

instance Prelude.NFData DescribeImages

instance Core.ToHeaders DescribeImages where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.DescribeImages" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeImages where
  toJSON DescribeImages' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("registryId" Core..=) Prelude.<$> registryId,
            ("imageIds" Core..=) Prelude.<$> imageIds,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("filter" Core..=) Prelude.<$> filter',
            ("maxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just
              ("repositoryName" Core..= repositoryName)
          ]
      )

instance Core.ToPath DescribeImages where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeImages where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeImagesResponse' smart constructor.
data DescribeImagesResponse = DescribeImagesResponse'
  { -- | A list of ImageDetail objects that contain data about the image.
    imageDetails :: Prelude.Maybe [ImageDetail],
    -- | The @nextToken@ value to include in a future @DescribeImages@ request.
    -- When the results of a @DescribeImages@ request exceed @maxResults@, this
    -- value can be used to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeImagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageDetails', 'describeImagesResponse_imageDetails' - A list of ImageDetail objects that contain data about the image.
--
-- 'nextToken', 'describeImagesResponse_nextToken' - The @nextToken@ value to include in a future @DescribeImages@ request.
-- When the results of a @DescribeImages@ request exceed @maxResults@, this
-- value can be used to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeImagesResponse_httpStatus' - The response's http status code.
newDescribeImagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeImagesResponse
newDescribeImagesResponse pHttpStatus_ =
  DescribeImagesResponse'
    { imageDetails =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of ImageDetail objects that contain data about the image.
describeImagesResponse_imageDetails :: Lens.Lens' DescribeImagesResponse (Prelude.Maybe [ImageDetail])
describeImagesResponse_imageDetails = Lens.lens (\DescribeImagesResponse' {imageDetails} -> imageDetails) (\s@DescribeImagesResponse' {} a -> s {imageDetails = a} :: DescribeImagesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The @nextToken@ value to include in a future @DescribeImages@ request.
-- When the results of a @DescribeImages@ request exceed @maxResults@, this
-- value can be used to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeImagesResponse_nextToken :: Lens.Lens' DescribeImagesResponse (Prelude.Maybe Prelude.Text)
describeImagesResponse_nextToken = Lens.lens (\DescribeImagesResponse' {nextToken} -> nextToken) (\s@DescribeImagesResponse' {} a -> s {nextToken = a} :: DescribeImagesResponse)

-- | The response's http status code.
describeImagesResponse_httpStatus :: Lens.Lens' DescribeImagesResponse Prelude.Int
describeImagesResponse_httpStatus = Lens.lens (\DescribeImagesResponse' {httpStatus} -> httpStatus) (\s@DescribeImagesResponse' {} a -> s {httpStatus = a} :: DescribeImagesResponse)

instance Prelude.NFData DescribeImagesResponse
