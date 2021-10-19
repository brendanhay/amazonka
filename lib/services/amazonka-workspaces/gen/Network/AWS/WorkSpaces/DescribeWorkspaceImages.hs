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
-- Module      : Network.AWS.WorkSpaces.DescribeWorkspaceImages
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes one or more specified images, if the
-- image identifiers are provided. Otherwise, all images in the account are
-- described.
--
-- This operation returns paginated results.
module Network.AWS.WorkSpaces.DescribeWorkspaceImages
  ( -- * Creating a Request
    DescribeWorkspaceImages (..),
    newDescribeWorkspaceImages,

    -- * Request Lenses
    describeWorkspaceImages_imageIds,
    describeWorkspaceImages_nextToken,
    describeWorkspaceImages_imageType,
    describeWorkspaceImages_maxResults,

    -- * Destructuring the Response
    DescribeWorkspaceImagesResponse (..),
    newDescribeWorkspaceImagesResponse,

    -- * Response Lenses
    describeWorkspaceImagesResponse_images,
    describeWorkspaceImagesResponse_nextToken,
    describeWorkspaceImagesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newDescribeWorkspaceImages' smart constructor.
data DescribeWorkspaceImages = DescribeWorkspaceImages'
  { -- | The identifier of the image.
    imageIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | If you received a @NextToken@ from a previous call that was paginated,
    -- provide this token to receive the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The type (owned or shared) of the image.
    imageType :: Prelude.Maybe ImageType,
    -- | The maximum number of items to return.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWorkspaceImages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageIds', 'describeWorkspaceImages_imageIds' - The identifier of the image.
--
-- 'nextToken', 'describeWorkspaceImages_nextToken' - If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
--
-- 'imageType', 'describeWorkspaceImages_imageType' - The type (owned or shared) of the image.
--
-- 'maxResults', 'describeWorkspaceImages_maxResults' - The maximum number of items to return.
newDescribeWorkspaceImages ::
  DescribeWorkspaceImages
newDescribeWorkspaceImages =
  DescribeWorkspaceImages'
    { imageIds =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      imageType = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The identifier of the image.
describeWorkspaceImages_imageIds :: Lens.Lens' DescribeWorkspaceImages (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeWorkspaceImages_imageIds = Lens.lens (\DescribeWorkspaceImages' {imageIds} -> imageIds) (\s@DescribeWorkspaceImages' {} a -> s {imageIds = a} :: DescribeWorkspaceImages) Prelude.. Lens.mapping Lens.coerced

-- | If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
describeWorkspaceImages_nextToken :: Lens.Lens' DescribeWorkspaceImages (Prelude.Maybe Prelude.Text)
describeWorkspaceImages_nextToken = Lens.lens (\DescribeWorkspaceImages' {nextToken} -> nextToken) (\s@DescribeWorkspaceImages' {} a -> s {nextToken = a} :: DescribeWorkspaceImages)

-- | The type (owned or shared) of the image.
describeWorkspaceImages_imageType :: Lens.Lens' DescribeWorkspaceImages (Prelude.Maybe ImageType)
describeWorkspaceImages_imageType = Lens.lens (\DescribeWorkspaceImages' {imageType} -> imageType) (\s@DescribeWorkspaceImages' {} a -> s {imageType = a} :: DescribeWorkspaceImages)

-- | The maximum number of items to return.
describeWorkspaceImages_maxResults :: Lens.Lens' DescribeWorkspaceImages (Prelude.Maybe Prelude.Natural)
describeWorkspaceImages_maxResults = Lens.lens (\DescribeWorkspaceImages' {maxResults} -> maxResults) (\s@DescribeWorkspaceImages' {} a -> s {maxResults = a} :: DescribeWorkspaceImages)

instance Core.AWSPager DescribeWorkspaceImages where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeWorkspaceImagesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeWorkspaceImagesResponse_images
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeWorkspaceImages_nextToken
          Lens..~ rs
          Lens.^? describeWorkspaceImagesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeWorkspaceImages where
  type
    AWSResponse DescribeWorkspaceImages =
      DescribeWorkspaceImagesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWorkspaceImagesResponse'
            Prelude.<$> (x Core..?> "Images" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeWorkspaceImages

instance Prelude.NFData DescribeWorkspaceImages

instance Core.ToHeaders DescribeWorkspaceImages where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.DescribeWorkspaceImages" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeWorkspaceImages where
  toJSON DescribeWorkspaceImages' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ImageIds" Core..=) Prelude.<$> imageIds,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("ImageType" Core..=) Prelude.<$> imageType,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath DescribeWorkspaceImages where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeWorkspaceImages where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeWorkspaceImagesResponse' smart constructor.
data DescribeWorkspaceImagesResponse = DescribeWorkspaceImagesResponse'
  { -- | Information about the images.
    images :: Prelude.Maybe [WorkspaceImage],
    -- | The token to use to retrieve the next page of results. This value is
    -- null when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeWorkspaceImagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'images', 'describeWorkspaceImagesResponse_images' - Information about the images.
--
-- 'nextToken', 'describeWorkspaceImagesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
--
-- 'httpStatus', 'describeWorkspaceImagesResponse_httpStatus' - The response's http status code.
newDescribeWorkspaceImagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeWorkspaceImagesResponse
newDescribeWorkspaceImagesResponse pHttpStatus_ =
  DescribeWorkspaceImagesResponse'
    { images =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the images.
describeWorkspaceImagesResponse_images :: Lens.Lens' DescribeWorkspaceImagesResponse (Prelude.Maybe [WorkspaceImage])
describeWorkspaceImagesResponse_images = Lens.lens (\DescribeWorkspaceImagesResponse' {images} -> images) (\s@DescribeWorkspaceImagesResponse' {} a -> s {images = a} :: DescribeWorkspaceImagesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
describeWorkspaceImagesResponse_nextToken :: Lens.Lens' DescribeWorkspaceImagesResponse (Prelude.Maybe Prelude.Text)
describeWorkspaceImagesResponse_nextToken = Lens.lens (\DescribeWorkspaceImagesResponse' {nextToken} -> nextToken) (\s@DescribeWorkspaceImagesResponse' {} a -> s {nextToken = a} :: DescribeWorkspaceImagesResponse)

-- | The response's http status code.
describeWorkspaceImagesResponse_httpStatus :: Lens.Lens' DescribeWorkspaceImagesResponse Prelude.Int
describeWorkspaceImagesResponse_httpStatus = Lens.lens (\DescribeWorkspaceImagesResponse' {httpStatus} -> httpStatus) (\s@DescribeWorkspaceImagesResponse' {} a -> s {httpStatus = a} :: DescribeWorkspaceImagesResponse)

instance
  Prelude.NFData
    DescribeWorkspaceImagesResponse
