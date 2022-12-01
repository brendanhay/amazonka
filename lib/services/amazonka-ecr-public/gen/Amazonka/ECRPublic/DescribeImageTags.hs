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
-- Module      : Amazonka.ECRPublic.DescribeImageTags
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the image tag details for a repository in a public registry.
--
-- This operation returns paginated results.
module Amazonka.ECRPublic.DescribeImageTags
  ( -- * Creating a Request
    DescribeImageTags (..),
    newDescribeImageTags,

    -- * Request Lenses
    describeImageTags_nextToken,
    describeImageTags_maxResults,
    describeImageTags_registryId,
    describeImageTags_repositoryName,

    -- * Destructuring the Response
    DescribeImageTagsResponse (..),
    newDescribeImageTagsResponse,

    -- * Response Lenses
    describeImageTagsResponse_nextToken,
    describeImageTagsResponse_imageTagDetails,
    describeImageTagsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ECRPublic.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeImageTags' smart constructor.
data DescribeImageTags = DescribeImageTags'
  { -- | The @nextToken@ value returned from a previous paginated
    -- @DescribeImageTags@ request where @maxResults@ was used and the results
    -- exceeded the value of that parameter. Pagination continues from the end
    -- of the previous results that returned the @nextToken@ value. This value
    -- is @null@ when there are no more results to return. This option cannot
    -- be used when you specify images with @imageIds@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of repository results returned by @DescribeImageTags@
    -- in paginated output. When this parameter is used, @DescribeImageTags@
    -- only returns @maxResults@ results in a single page along with a
    -- @nextToken@ response element. The remaining results of the initial
    -- request can be seen by sending another @DescribeImageTags@ request with
    -- the returned @nextToken@ value. This value can be between 1 and 1000. If
    -- this parameter is not used, then @DescribeImageTags@ returns up to 100
    -- results and a @nextToken@ value, if applicable. This option cannot be
    -- used when you specify images with @imageIds@.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The AWS account ID associated with the public registry that contains the
    -- repository in which to describe images. If you do not specify a
    -- registry, the default public registry is assumed.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository that contains the image tag details to
    -- describe.
    repositoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeImageTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeImageTags_nextToken' - The @nextToken@ value returned from a previous paginated
-- @DescribeImageTags@ request where @maxResults@ was used and the results
-- exceeded the value of that parameter. Pagination continues from the end
-- of the previous results that returned the @nextToken@ value. This value
-- is @null@ when there are no more results to return. This option cannot
-- be used when you specify images with @imageIds@.
--
-- 'maxResults', 'describeImageTags_maxResults' - The maximum number of repository results returned by @DescribeImageTags@
-- in paginated output. When this parameter is used, @DescribeImageTags@
-- only returns @maxResults@ results in a single page along with a
-- @nextToken@ response element. The remaining results of the initial
-- request can be seen by sending another @DescribeImageTags@ request with
-- the returned @nextToken@ value. This value can be between 1 and 1000. If
-- this parameter is not used, then @DescribeImageTags@ returns up to 100
-- results and a @nextToken@ value, if applicable. This option cannot be
-- used when you specify images with @imageIds@.
--
-- 'registryId', 'describeImageTags_registryId' - The AWS account ID associated with the public registry that contains the
-- repository in which to describe images. If you do not specify a
-- registry, the default public registry is assumed.
--
-- 'repositoryName', 'describeImageTags_repositoryName' - The name of the repository that contains the image tag details to
-- describe.
newDescribeImageTags ::
  -- | 'repositoryName'
  Prelude.Text ->
  DescribeImageTags
newDescribeImageTags pRepositoryName_ =
  DescribeImageTags'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      registryId = Prelude.Nothing,
      repositoryName = pRepositoryName_
    }

-- | The @nextToken@ value returned from a previous paginated
-- @DescribeImageTags@ request where @maxResults@ was used and the results
-- exceeded the value of that parameter. Pagination continues from the end
-- of the previous results that returned the @nextToken@ value. This value
-- is @null@ when there are no more results to return. This option cannot
-- be used when you specify images with @imageIds@.
describeImageTags_nextToken :: Lens.Lens' DescribeImageTags (Prelude.Maybe Prelude.Text)
describeImageTags_nextToken = Lens.lens (\DescribeImageTags' {nextToken} -> nextToken) (\s@DescribeImageTags' {} a -> s {nextToken = a} :: DescribeImageTags)

-- | The maximum number of repository results returned by @DescribeImageTags@
-- in paginated output. When this parameter is used, @DescribeImageTags@
-- only returns @maxResults@ results in a single page along with a
-- @nextToken@ response element. The remaining results of the initial
-- request can be seen by sending another @DescribeImageTags@ request with
-- the returned @nextToken@ value. This value can be between 1 and 1000. If
-- this parameter is not used, then @DescribeImageTags@ returns up to 100
-- results and a @nextToken@ value, if applicable. This option cannot be
-- used when you specify images with @imageIds@.
describeImageTags_maxResults :: Lens.Lens' DescribeImageTags (Prelude.Maybe Prelude.Natural)
describeImageTags_maxResults = Lens.lens (\DescribeImageTags' {maxResults} -> maxResults) (\s@DescribeImageTags' {} a -> s {maxResults = a} :: DescribeImageTags)

-- | The AWS account ID associated with the public registry that contains the
-- repository in which to describe images. If you do not specify a
-- registry, the default public registry is assumed.
describeImageTags_registryId :: Lens.Lens' DescribeImageTags (Prelude.Maybe Prelude.Text)
describeImageTags_registryId = Lens.lens (\DescribeImageTags' {registryId} -> registryId) (\s@DescribeImageTags' {} a -> s {registryId = a} :: DescribeImageTags)

-- | The name of the repository that contains the image tag details to
-- describe.
describeImageTags_repositoryName :: Lens.Lens' DescribeImageTags Prelude.Text
describeImageTags_repositoryName = Lens.lens (\DescribeImageTags' {repositoryName} -> repositoryName) (\s@DescribeImageTags' {} a -> s {repositoryName = a} :: DescribeImageTags)

instance Core.AWSPager DescribeImageTags where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeImageTagsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeImageTagsResponse_imageTagDetails
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeImageTags_nextToken
          Lens..~ rs
          Lens.^? describeImageTagsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeImageTags where
  type
    AWSResponse DescribeImageTags =
      DescribeImageTagsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeImageTagsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "imageTagDetails"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeImageTags where
  hashWithSalt _salt DescribeImageTags' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` registryId
      `Prelude.hashWithSalt` repositoryName

instance Prelude.NFData DescribeImageTags where
  rnf DescribeImageTags' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf repositoryName

instance Core.ToHeaders DescribeImageTags where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SpencerFrontendService.DescribeImageTags" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeImageTags where
  toJSON DescribeImageTags' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("registryId" Core..=) Prelude.<$> registryId,
            Prelude.Just
              ("repositoryName" Core..= repositoryName)
          ]
      )

instance Core.ToPath DescribeImageTags where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeImageTags where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeImageTagsResponse' smart constructor.
data DescribeImageTagsResponse = DescribeImageTagsResponse'
  { -- | The @nextToken@ value to include in a future @DescribeImageTags@
    -- request. When the results of a @DescribeImageTags@ request exceed
    -- @maxResults@, this value can be used to retrieve the next page of
    -- results. This value is @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The image tag details for the images in the requested repository.
    imageTagDetails :: Prelude.Maybe [ImageTagDetail],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeImageTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeImageTagsResponse_nextToken' - The @nextToken@ value to include in a future @DescribeImageTags@
-- request. When the results of a @DescribeImageTags@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
--
-- 'imageTagDetails', 'describeImageTagsResponse_imageTagDetails' - The image tag details for the images in the requested repository.
--
-- 'httpStatus', 'describeImageTagsResponse_httpStatus' - The response's http status code.
newDescribeImageTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeImageTagsResponse
newDescribeImageTagsResponse pHttpStatus_ =
  DescribeImageTagsResponse'
    { nextToken =
        Prelude.Nothing,
      imageTagDetails = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @nextToken@ value to include in a future @DescribeImageTags@
-- request. When the results of a @DescribeImageTags@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
describeImageTagsResponse_nextToken :: Lens.Lens' DescribeImageTagsResponse (Prelude.Maybe Prelude.Text)
describeImageTagsResponse_nextToken = Lens.lens (\DescribeImageTagsResponse' {nextToken} -> nextToken) (\s@DescribeImageTagsResponse' {} a -> s {nextToken = a} :: DescribeImageTagsResponse)

-- | The image tag details for the images in the requested repository.
describeImageTagsResponse_imageTagDetails :: Lens.Lens' DescribeImageTagsResponse (Prelude.Maybe [ImageTagDetail])
describeImageTagsResponse_imageTagDetails = Lens.lens (\DescribeImageTagsResponse' {imageTagDetails} -> imageTagDetails) (\s@DescribeImageTagsResponse' {} a -> s {imageTagDetails = a} :: DescribeImageTagsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeImageTagsResponse_httpStatus :: Lens.Lens' DescribeImageTagsResponse Prelude.Int
describeImageTagsResponse_httpStatus = Lens.lens (\DescribeImageTagsResponse' {httpStatus} -> httpStatus) (\s@DescribeImageTagsResponse' {} a -> s {httpStatus = a} :: DescribeImageTagsResponse)

instance Prelude.NFData DescribeImageTagsResponse where
  rnf DescribeImageTagsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf imageTagDetails
      `Prelude.seq` Prelude.rnf httpStatus
