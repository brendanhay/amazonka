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
-- Module      : Network.AWS.SageMaker.ListImages
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the images in your account and their properties. The list can be
-- filtered by creation time or modified time, and whether the image name
-- contains a specified string.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListImages
  ( -- * Creating a Request
    ListImages (..),
    newListImages,

    -- * Request Lenses
    listImages_lastModifiedTimeBefore,
    listImages_sortOrder,
    listImages_nextToken,
    listImages_nameContains,
    listImages_maxResults,
    listImages_creationTimeBefore,
    listImages_lastModifiedTimeAfter,
    listImages_sortBy,
    listImages_creationTimeAfter,

    -- * Destructuring the Response
    ListImagesResponse (..),
    newListImagesResponse,

    -- * Response Lenses
    listImagesResponse_nextToken,
    listImagesResponse_images,
    listImagesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListImages' smart constructor.
data ListImages = ListImages'
  { -- | A filter that returns only images modified on or before the specified
    -- time.
    lastModifiedTimeBefore :: Core.Maybe Core.POSIX,
    -- | The sort order. The default value is @DESCENDING@.
    sortOrder :: Core.Maybe ImageSortOrder,
    -- | If the previous call to @ListImages@ didn\'t return the full set of
    -- images, the call returns a token for getting the next set of images.
    nextToken :: Core.Maybe Core.Text,
    -- | A filter that returns only images whose name contains the specified
    -- string.
    nameContains :: Core.Maybe Core.Text,
    -- | The maximum number of images to return in the response. The default
    -- value is 10.
    maxResults :: Core.Maybe Core.Natural,
    -- | A filter that returns only images created on or before the specified
    -- time.
    creationTimeBefore :: Core.Maybe Core.POSIX,
    -- | A filter that returns only images modified on or after the specified
    -- time.
    lastModifiedTimeAfter :: Core.Maybe Core.POSIX,
    -- | The property used to sort results. The default value is @CREATION_TIME@.
    sortBy :: Core.Maybe ImageSortBy,
    -- | A filter that returns only images created on or after the specified
    -- time.
    creationTimeAfter :: Core.Maybe Core.POSIX
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
-- 'lastModifiedTimeBefore', 'listImages_lastModifiedTimeBefore' - A filter that returns only images modified on or before the specified
-- time.
--
-- 'sortOrder', 'listImages_sortOrder' - The sort order. The default value is @DESCENDING@.
--
-- 'nextToken', 'listImages_nextToken' - If the previous call to @ListImages@ didn\'t return the full set of
-- images, the call returns a token for getting the next set of images.
--
-- 'nameContains', 'listImages_nameContains' - A filter that returns only images whose name contains the specified
-- string.
--
-- 'maxResults', 'listImages_maxResults' - The maximum number of images to return in the response. The default
-- value is 10.
--
-- 'creationTimeBefore', 'listImages_creationTimeBefore' - A filter that returns only images created on or before the specified
-- time.
--
-- 'lastModifiedTimeAfter', 'listImages_lastModifiedTimeAfter' - A filter that returns only images modified on or after the specified
-- time.
--
-- 'sortBy', 'listImages_sortBy' - The property used to sort results. The default value is @CREATION_TIME@.
--
-- 'creationTimeAfter', 'listImages_creationTimeAfter' - A filter that returns only images created on or after the specified
-- time.
newListImages ::
  ListImages
newListImages =
  ListImages'
    { lastModifiedTimeBefore = Core.Nothing,
      sortOrder = Core.Nothing,
      nextToken = Core.Nothing,
      nameContains = Core.Nothing,
      maxResults = Core.Nothing,
      creationTimeBefore = Core.Nothing,
      lastModifiedTimeAfter = Core.Nothing,
      sortBy = Core.Nothing,
      creationTimeAfter = Core.Nothing
    }

-- | A filter that returns only images modified on or before the specified
-- time.
listImages_lastModifiedTimeBefore :: Lens.Lens' ListImages (Core.Maybe Core.UTCTime)
listImages_lastModifiedTimeBefore = Lens.lens (\ListImages' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListImages' {} a -> s {lastModifiedTimeBefore = a} :: ListImages) Core.. Lens.mapping Core._Time

-- | The sort order. The default value is @DESCENDING@.
listImages_sortOrder :: Lens.Lens' ListImages (Core.Maybe ImageSortOrder)
listImages_sortOrder = Lens.lens (\ListImages' {sortOrder} -> sortOrder) (\s@ListImages' {} a -> s {sortOrder = a} :: ListImages)

-- | If the previous call to @ListImages@ didn\'t return the full set of
-- images, the call returns a token for getting the next set of images.
listImages_nextToken :: Lens.Lens' ListImages (Core.Maybe Core.Text)
listImages_nextToken = Lens.lens (\ListImages' {nextToken} -> nextToken) (\s@ListImages' {} a -> s {nextToken = a} :: ListImages)

-- | A filter that returns only images whose name contains the specified
-- string.
listImages_nameContains :: Lens.Lens' ListImages (Core.Maybe Core.Text)
listImages_nameContains = Lens.lens (\ListImages' {nameContains} -> nameContains) (\s@ListImages' {} a -> s {nameContains = a} :: ListImages)

-- | The maximum number of images to return in the response. The default
-- value is 10.
listImages_maxResults :: Lens.Lens' ListImages (Core.Maybe Core.Natural)
listImages_maxResults = Lens.lens (\ListImages' {maxResults} -> maxResults) (\s@ListImages' {} a -> s {maxResults = a} :: ListImages)

-- | A filter that returns only images created on or before the specified
-- time.
listImages_creationTimeBefore :: Lens.Lens' ListImages (Core.Maybe Core.UTCTime)
listImages_creationTimeBefore = Lens.lens (\ListImages' {creationTimeBefore} -> creationTimeBefore) (\s@ListImages' {} a -> s {creationTimeBefore = a} :: ListImages) Core.. Lens.mapping Core._Time

-- | A filter that returns only images modified on or after the specified
-- time.
listImages_lastModifiedTimeAfter :: Lens.Lens' ListImages (Core.Maybe Core.UTCTime)
listImages_lastModifiedTimeAfter = Lens.lens (\ListImages' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListImages' {} a -> s {lastModifiedTimeAfter = a} :: ListImages) Core.. Lens.mapping Core._Time

-- | The property used to sort results. The default value is @CREATION_TIME@.
listImages_sortBy :: Lens.Lens' ListImages (Core.Maybe ImageSortBy)
listImages_sortBy = Lens.lens (\ListImages' {sortBy} -> sortBy) (\s@ListImages' {} a -> s {sortBy = a} :: ListImages)

-- | A filter that returns only images created on or after the specified
-- time.
listImages_creationTimeAfter :: Lens.Lens' ListImages (Core.Maybe Core.UTCTime)
listImages_creationTimeAfter = Lens.lens (\ListImages' {creationTimeAfter} -> creationTimeAfter) (\s@ListImages' {} a -> s {creationTimeAfter = a} :: ListImages) Core.. Lens.mapping Core._Time

instance Core.AWSPager ListImages where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listImagesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listImagesResponse_images Core.. Lens._Just
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
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Images" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListImages

instance Core.NFData ListImages

instance Core.ToHeaders ListImages where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListImages" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListImages where
  toJSON ListImages' {..} =
    Core.object
      ( Core.catMaybes
          [ ("LastModifiedTimeBefore" Core..=)
              Core.<$> lastModifiedTimeBefore,
            ("SortOrder" Core..=) Core.<$> sortOrder,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("NameContains" Core..=) Core.<$> nameContains,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("CreationTimeBefore" Core..=)
              Core.<$> creationTimeBefore,
            ("LastModifiedTimeAfter" Core..=)
              Core.<$> lastModifiedTimeAfter,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("CreationTimeAfter" Core..=)
              Core.<$> creationTimeAfter
          ]
      )

instance Core.ToPath ListImages where
  toPath = Core.const "/"

instance Core.ToQuery ListImages where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListImagesResponse' smart constructor.
data ListImagesResponse = ListImagesResponse'
  { -- | A token for getting the next set of images, if there are any.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of images and their properties.
    images :: Core.Maybe [Image],
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
-- 'nextToken', 'listImagesResponse_nextToken' - A token for getting the next set of images, if there are any.
--
-- 'images', 'listImagesResponse_images' - A list of images and their properties.
--
-- 'httpStatus', 'listImagesResponse_httpStatus' - The response's http status code.
newListImagesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListImagesResponse
newListImagesResponse pHttpStatus_ =
  ListImagesResponse'
    { nextToken = Core.Nothing,
      images = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token for getting the next set of images, if there are any.
listImagesResponse_nextToken :: Lens.Lens' ListImagesResponse (Core.Maybe Core.Text)
listImagesResponse_nextToken = Lens.lens (\ListImagesResponse' {nextToken} -> nextToken) (\s@ListImagesResponse' {} a -> s {nextToken = a} :: ListImagesResponse)

-- | A list of images and their properties.
listImagesResponse_images :: Lens.Lens' ListImagesResponse (Core.Maybe [Image])
listImagesResponse_images = Lens.lens (\ListImagesResponse' {images} -> images) (\s@ListImagesResponse' {} a -> s {images = a} :: ListImagesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listImagesResponse_httpStatus :: Lens.Lens' ListImagesResponse Core.Int
listImagesResponse_httpStatus = Lens.lens (\ListImagesResponse' {httpStatus} -> httpStatus) (\s@ListImagesResponse' {} a -> s {httpStatus = a} :: ListImagesResponse)

instance Core.NFData ListImagesResponse
