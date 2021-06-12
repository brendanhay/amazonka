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
    describeWorkspaceImages_imageType,
    describeWorkspaceImages_nextToken,
    describeWorkspaceImages_imageIds,
    describeWorkspaceImages_maxResults,

    -- * Destructuring the Response
    DescribeWorkspaceImagesResponse (..),
    newDescribeWorkspaceImagesResponse,

    -- * Response Lenses
    describeWorkspaceImagesResponse_nextToken,
    describeWorkspaceImagesResponse_images,
    describeWorkspaceImagesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newDescribeWorkspaceImages' smart constructor.
data DescribeWorkspaceImages = DescribeWorkspaceImages'
  { -- | The type (owned or shared) of the image.
    imageType :: Core.Maybe ImageType,
    -- | If you received a @NextToken@ from a previous call that was paginated,
    -- provide this token to receive the next set of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The identifier of the image.
    imageIds :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The maximum number of items to return.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeWorkspaceImages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageType', 'describeWorkspaceImages_imageType' - The type (owned or shared) of the image.
--
-- 'nextToken', 'describeWorkspaceImages_nextToken' - If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
--
-- 'imageIds', 'describeWorkspaceImages_imageIds' - The identifier of the image.
--
-- 'maxResults', 'describeWorkspaceImages_maxResults' - The maximum number of items to return.
newDescribeWorkspaceImages ::
  DescribeWorkspaceImages
newDescribeWorkspaceImages =
  DescribeWorkspaceImages'
    { imageType = Core.Nothing,
      nextToken = Core.Nothing,
      imageIds = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The type (owned or shared) of the image.
describeWorkspaceImages_imageType :: Lens.Lens' DescribeWorkspaceImages (Core.Maybe ImageType)
describeWorkspaceImages_imageType = Lens.lens (\DescribeWorkspaceImages' {imageType} -> imageType) (\s@DescribeWorkspaceImages' {} a -> s {imageType = a} :: DescribeWorkspaceImages)

-- | If you received a @NextToken@ from a previous call that was paginated,
-- provide this token to receive the next set of results.
describeWorkspaceImages_nextToken :: Lens.Lens' DescribeWorkspaceImages (Core.Maybe Core.Text)
describeWorkspaceImages_nextToken = Lens.lens (\DescribeWorkspaceImages' {nextToken} -> nextToken) (\s@DescribeWorkspaceImages' {} a -> s {nextToken = a} :: DescribeWorkspaceImages)

-- | The identifier of the image.
describeWorkspaceImages_imageIds :: Lens.Lens' DescribeWorkspaceImages (Core.Maybe (Core.NonEmpty Core.Text))
describeWorkspaceImages_imageIds = Lens.lens (\DescribeWorkspaceImages' {imageIds} -> imageIds) (\s@DescribeWorkspaceImages' {} a -> s {imageIds = a} :: DescribeWorkspaceImages) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of items to return.
describeWorkspaceImages_maxResults :: Lens.Lens' DescribeWorkspaceImages (Core.Maybe Core.Natural)
describeWorkspaceImages_maxResults = Lens.lens (\DescribeWorkspaceImages' {maxResults} -> maxResults) (\s@DescribeWorkspaceImages' {} a -> s {maxResults = a} :: DescribeWorkspaceImages)

instance Core.AWSPager DescribeWorkspaceImages where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeWorkspaceImagesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeWorkspaceImagesResponse_images
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeWorkspaceImages_nextToken
          Lens..~ rs
          Lens.^? describeWorkspaceImagesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeWorkspaceImages where
  type
    AWSResponse DescribeWorkspaceImages =
      DescribeWorkspaceImagesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeWorkspaceImagesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Images" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeWorkspaceImages

instance Core.NFData DescribeWorkspaceImages

instance Core.ToHeaders DescribeWorkspaceImages where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.DescribeWorkspaceImages" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeWorkspaceImages where
  toJSON DescribeWorkspaceImages' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ImageType" Core..=) Core.<$> imageType,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("ImageIds" Core..=) Core.<$> imageIds,
            ("MaxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath DescribeWorkspaceImages where
  toPath = Core.const "/"

instance Core.ToQuery DescribeWorkspaceImages where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeWorkspaceImagesResponse' smart constructor.
data DescribeWorkspaceImagesResponse = DescribeWorkspaceImagesResponse'
  { -- | The token to use to retrieve the next set of results, or null if no more
    -- results are available.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the images.
    images :: Core.Maybe [WorkspaceImage],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeWorkspaceImagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeWorkspaceImagesResponse_nextToken' - The token to use to retrieve the next set of results, or null if no more
-- results are available.
--
-- 'images', 'describeWorkspaceImagesResponse_images' - Information about the images.
--
-- 'httpStatus', 'describeWorkspaceImagesResponse_httpStatus' - The response's http status code.
newDescribeWorkspaceImagesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeWorkspaceImagesResponse
newDescribeWorkspaceImagesResponse pHttpStatus_ =
  DescribeWorkspaceImagesResponse'
    { nextToken =
        Core.Nothing,
      images = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next set of results, or null if no more
-- results are available.
describeWorkspaceImagesResponse_nextToken :: Lens.Lens' DescribeWorkspaceImagesResponse (Core.Maybe Core.Text)
describeWorkspaceImagesResponse_nextToken = Lens.lens (\DescribeWorkspaceImagesResponse' {nextToken} -> nextToken) (\s@DescribeWorkspaceImagesResponse' {} a -> s {nextToken = a} :: DescribeWorkspaceImagesResponse)

-- | Information about the images.
describeWorkspaceImagesResponse_images :: Lens.Lens' DescribeWorkspaceImagesResponse (Core.Maybe [WorkspaceImage])
describeWorkspaceImagesResponse_images = Lens.lens (\DescribeWorkspaceImagesResponse' {images} -> images) (\s@DescribeWorkspaceImagesResponse' {} a -> s {images = a} :: DescribeWorkspaceImagesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeWorkspaceImagesResponse_httpStatus :: Lens.Lens' DescribeWorkspaceImagesResponse Core.Int
describeWorkspaceImagesResponse_httpStatus = Lens.lens (\DescribeWorkspaceImagesResponse' {httpStatus} -> httpStatus) (\s@DescribeWorkspaceImagesResponse' {} a -> s {httpStatus = a} :: DescribeWorkspaceImagesResponse)

instance Core.NFData DescribeWorkspaceImagesResponse
