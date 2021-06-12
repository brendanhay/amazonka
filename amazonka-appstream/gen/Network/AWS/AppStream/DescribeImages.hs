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
-- Module      : Network.AWS.AppStream.DescribeImages
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes one or more specified images, if the
-- image names or image ARNs are provided. Otherwise, all images in the
-- account are described.
--
-- This operation returns paginated results.
module Network.AWS.AppStream.DescribeImages
  ( -- * Creating a Request
    DescribeImages (..),
    newDescribeImages,

    -- * Request Lenses
    describeImages_names,
    describeImages_nextToken,
    describeImages_arns,
    describeImages_maxResults,
    describeImages_type,

    -- * Destructuring the Response
    DescribeImagesResponse (..),
    newDescribeImagesResponse,

    -- * Response Lenses
    describeImagesResponse_nextToken,
    describeImagesResponse_images,
    describeImagesResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeImages' smart constructor.
data DescribeImages = DescribeImages'
  { -- | The names of the public or private images to describe.
    names :: Core.Maybe [Core.Text],
    -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If this value is null, it retrieves the first page.
    nextToken :: Core.Maybe Core.Text,
    -- | The ARNs of the public, private, and shared images to describe.
    arns :: Core.Maybe [Core.Text],
    -- | The maximum size of each page of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The type of image (public, private, or shared) to describe.
    type' :: Core.Maybe VisibilityType
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
-- 'names', 'describeImages_names' - The names of the public or private images to describe.
--
-- 'nextToken', 'describeImages_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
--
-- 'arns', 'describeImages_arns' - The ARNs of the public, private, and shared images to describe.
--
-- 'maxResults', 'describeImages_maxResults' - The maximum size of each page of results.
--
-- 'type'', 'describeImages_type' - The type of image (public, private, or shared) to describe.
newDescribeImages ::
  DescribeImages
newDescribeImages =
  DescribeImages'
    { names = Core.Nothing,
      nextToken = Core.Nothing,
      arns = Core.Nothing,
      maxResults = Core.Nothing,
      type' = Core.Nothing
    }

-- | The names of the public or private images to describe.
describeImages_names :: Lens.Lens' DescribeImages (Core.Maybe [Core.Text])
describeImages_names = Lens.lens (\DescribeImages' {names} -> names) (\s@DescribeImages' {} a -> s {names = a} :: DescribeImages) Core.. Lens.mapping Lens._Coerce

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If this value is null, it retrieves the first page.
describeImages_nextToken :: Lens.Lens' DescribeImages (Core.Maybe Core.Text)
describeImages_nextToken = Lens.lens (\DescribeImages' {nextToken} -> nextToken) (\s@DescribeImages' {} a -> s {nextToken = a} :: DescribeImages)

-- | The ARNs of the public, private, and shared images to describe.
describeImages_arns :: Lens.Lens' DescribeImages (Core.Maybe [Core.Text])
describeImages_arns = Lens.lens (\DescribeImages' {arns} -> arns) (\s@DescribeImages' {} a -> s {arns = a} :: DescribeImages) Core.. Lens.mapping Lens._Coerce

-- | The maximum size of each page of results.
describeImages_maxResults :: Lens.Lens' DescribeImages (Core.Maybe Core.Natural)
describeImages_maxResults = Lens.lens (\DescribeImages' {maxResults} -> maxResults) (\s@DescribeImages' {} a -> s {maxResults = a} :: DescribeImages)

-- | The type of image (public, private, or shared) to describe.
describeImages_type :: Lens.Lens' DescribeImages (Core.Maybe VisibilityType)
describeImages_type = Lens.lens (\DescribeImages' {type'} -> type') (\s@DescribeImages' {} a -> s {type' = a} :: DescribeImages)

instance Core.AWSPager DescribeImages where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeImagesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeImagesResponse_images Core.. Lens._Just
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
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Images" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeImages

instance Core.NFData DescribeImages

instance Core.ToHeaders DescribeImages where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.DescribeImages" ::
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
          [ ("Names" Core..=) Core.<$> names,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("Arns" Core..=) Core.<$> arns,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Type" Core..=) Core.<$> type'
          ]
      )

instance Core.ToPath DescribeImages where
  toPath = Core.const "/"

instance Core.ToQuery DescribeImages where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeImagesResponse' smart constructor.
data DescribeImagesResponse = DescribeImagesResponse'
  { -- | The pagination token to use to retrieve the next page of results for
    -- this operation. If there are no more pages, this value is null.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the images.
    images :: Core.Maybe [Image],
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
-- 'nextToken', 'describeImagesResponse_nextToken' - The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
--
-- 'images', 'describeImagesResponse_images' - Information about the images.
--
-- 'httpStatus', 'describeImagesResponse_httpStatus' - The response's http status code.
newDescribeImagesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeImagesResponse
newDescribeImagesResponse pHttpStatus_ =
  DescribeImagesResponse'
    { nextToken = Core.Nothing,
      images = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The pagination token to use to retrieve the next page of results for
-- this operation. If there are no more pages, this value is null.
describeImagesResponse_nextToken :: Lens.Lens' DescribeImagesResponse (Core.Maybe Core.Text)
describeImagesResponse_nextToken = Lens.lens (\DescribeImagesResponse' {nextToken} -> nextToken) (\s@DescribeImagesResponse' {} a -> s {nextToken = a} :: DescribeImagesResponse)

-- | Information about the images.
describeImagesResponse_images :: Lens.Lens' DescribeImagesResponse (Core.Maybe [Image])
describeImagesResponse_images = Lens.lens (\DescribeImagesResponse' {images} -> images) (\s@DescribeImagesResponse' {} a -> s {images = a} :: DescribeImagesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeImagesResponse_httpStatus :: Lens.Lens' DescribeImagesResponse Core.Int
describeImagesResponse_httpStatus = Lens.lens (\DescribeImagesResponse' {httpStatus} -> httpStatus) (\s@DescribeImagesResponse' {} a -> s {httpStatus = a} :: DescribeImagesResponse)

instance Core.NFData DescribeImagesResponse
