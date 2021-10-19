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
    listImages_nameContains,
    listImages_lastModifiedTimeBefore,
    listImages_creationTimeAfter,
    listImages_nextToken,
    listImages_sortOrder,
    listImages_lastModifiedTimeAfter,
    listImages_creationTimeBefore,
    listImages_maxResults,
    listImages_sortBy,

    -- * Destructuring the Response
    ListImagesResponse (..),
    newListImagesResponse,

    -- * Response Lenses
    listImagesResponse_images,
    listImagesResponse_nextToken,
    listImagesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListImages' smart constructor.
data ListImages = ListImages'
  { -- | A filter that returns only images whose name contains the specified
    -- string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only images modified on or before the specified
    -- time.
    lastModifiedTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | A filter that returns only images created on or after the specified
    -- time.
    creationTimeAfter :: Prelude.Maybe Core.POSIX,
    -- | If the previous call to @ListImages@ didn\'t return the full set of
    -- images, the call returns a token for getting the next set of images.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The sort order. The default value is @DESCENDING@.
    sortOrder :: Prelude.Maybe ImageSortOrder,
    -- | A filter that returns only images modified on or after the specified
    -- time.
    lastModifiedTimeAfter :: Prelude.Maybe Core.POSIX,
    -- | A filter that returns only images created on or before the specified
    -- time.
    creationTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | The maximum number of images to return in the response. The default
    -- value is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The property used to sort results. The default value is @CREATION_TIME@.
    sortBy :: Prelude.Maybe ImageSortBy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListImages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nameContains', 'listImages_nameContains' - A filter that returns only images whose name contains the specified
-- string.
--
-- 'lastModifiedTimeBefore', 'listImages_lastModifiedTimeBefore' - A filter that returns only images modified on or before the specified
-- time.
--
-- 'creationTimeAfter', 'listImages_creationTimeAfter' - A filter that returns only images created on or after the specified
-- time.
--
-- 'nextToken', 'listImages_nextToken' - If the previous call to @ListImages@ didn\'t return the full set of
-- images, the call returns a token for getting the next set of images.
--
-- 'sortOrder', 'listImages_sortOrder' - The sort order. The default value is @DESCENDING@.
--
-- 'lastModifiedTimeAfter', 'listImages_lastModifiedTimeAfter' - A filter that returns only images modified on or after the specified
-- time.
--
-- 'creationTimeBefore', 'listImages_creationTimeBefore' - A filter that returns only images created on or before the specified
-- time.
--
-- 'maxResults', 'listImages_maxResults' - The maximum number of images to return in the response. The default
-- value is 10.
--
-- 'sortBy', 'listImages_sortBy' - The property used to sort results. The default value is @CREATION_TIME@.
newListImages ::
  ListImages
newListImages =
  ListImages'
    { nameContains = Prelude.Nothing,
      lastModifiedTimeBefore = Prelude.Nothing,
      creationTimeAfter = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      lastModifiedTimeAfter = Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      sortBy = Prelude.Nothing
    }

-- | A filter that returns only images whose name contains the specified
-- string.
listImages_nameContains :: Lens.Lens' ListImages (Prelude.Maybe Prelude.Text)
listImages_nameContains = Lens.lens (\ListImages' {nameContains} -> nameContains) (\s@ListImages' {} a -> s {nameContains = a} :: ListImages)

-- | A filter that returns only images modified on or before the specified
-- time.
listImages_lastModifiedTimeBefore :: Lens.Lens' ListImages (Prelude.Maybe Prelude.UTCTime)
listImages_lastModifiedTimeBefore = Lens.lens (\ListImages' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListImages' {} a -> s {lastModifiedTimeBefore = a} :: ListImages) Prelude.. Lens.mapping Core._Time

-- | A filter that returns only images created on or after the specified
-- time.
listImages_creationTimeAfter :: Lens.Lens' ListImages (Prelude.Maybe Prelude.UTCTime)
listImages_creationTimeAfter = Lens.lens (\ListImages' {creationTimeAfter} -> creationTimeAfter) (\s@ListImages' {} a -> s {creationTimeAfter = a} :: ListImages) Prelude.. Lens.mapping Core._Time

-- | If the previous call to @ListImages@ didn\'t return the full set of
-- images, the call returns a token for getting the next set of images.
listImages_nextToken :: Lens.Lens' ListImages (Prelude.Maybe Prelude.Text)
listImages_nextToken = Lens.lens (\ListImages' {nextToken} -> nextToken) (\s@ListImages' {} a -> s {nextToken = a} :: ListImages)

-- | The sort order. The default value is @DESCENDING@.
listImages_sortOrder :: Lens.Lens' ListImages (Prelude.Maybe ImageSortOrder)
listImages_sortOrder = Lens.lens (\ListImages' {sortOrder} -> sortOrder) (\s@ListImages' {} a -> s {sortOrder = a} :: ListImages)

-- | A filter that returns only images modified on or after the specified
-- time.
listImages_lastModifiedTimeAfter :: Lens.Lens' ListImages (Prelude.Maybe Prelude.UTCTime)
listImages_lastModifiedTimeAfter = Lens.lens (\ListImages' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListImages' {} a -> s {lastModifiedTimeAfter = a} :: ListImages) Prelude.. Lens.mapping Core._Time

-- | A filter that returns only images created on or before the specified
-- time.
listImages_creationTimeBefore :: Lens.Lens' ListImages (Prelude.Maybe Prelude.UTCTime)
listImages_creationTimeBefore = Lens.lens (\ListImages' {creationTimeBefore} -> creationTimeBefore) (\s@ListImages' {} a -> s {creationTimeBefore = a} :: ListImages) Prelude.. Lens.mapping Core._Time

-- | The maximum number of images to return in the response. The default
-- value is 10.
listImages_maxResults :: Lens.Lens' ListImages (Prelude.Maybe Prelude.Natural)
listImages_maxResults = Lens.lens (\ListImages' {maxResults} -> maxResults) (\s@ListImages' {} a -> s {maxResults = a} :: ListImages)

-- | The property used to sort results. The default value is @CREATION_TIME@.
listImages_sortBy :: Lens.Lens' ListImages (Prelude.Maybe ImageSortBy)
listImages_sortBy = Lens.lens (\ListImages' {sortBy} -> sortBy) (\s@ListImages' {} a -> s {sortBy = a} :: ListImages)

instance Core.AWSPager ListImages where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listImagesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listImagesResponse_images Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listImages_nextToken
          Lens..~ rs
          Lens.^? listImagesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListImages where
  type AWSResponse ListImages = ListImagesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListImagesResponse'
            Prelude.<$> (x Core..?> "Images" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListImages

instance Prelude.NFData ListImages

instance Core.ToHeaders ListImages where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListImages" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListImages where
  toJSON ListImages' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NameContains" Core..=) Prelude.<$> nameContains,
            ("LastModifiedTimeBefore" Core..=)
              Prelude.<$> lastModifiedTimeBefore,
            ("CreationTimeAfter" Core..=)
              Prelude.<$> creationTimeAfter,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("LastModifiedTimeAfter" Core..=)
              Prelude.<$> lastModifiedTimeAfter,
            ("CreationTimeBefore" Core..=)
              Prelude.<$> creationTimeBefore,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("SortBy" Core..=) Prelude.<$> sortBy
          ]
      )

instance Core.ToPath ListImages where
  toPath = Prelude.const "/"

instance Core.ToQuery ListImages where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListImagesResponse' smart constructor.
data ListImagesResponse = ListImagesResponse'
  { -- | A list of images and their properties.
    images :: Prelude.Maybe [Image],
    -- | A token for getting the next set of images, if there are any.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListImagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'images', 'listImagesResponse_images' - A list of images and their properties.
--
-- 'nextToken', 'listImagesResponse_nextToken' - A token for getting the next set of images, if there are any.
--
-- 'httpStatus', 'listImagesResponse_httpStatus' - The response's http status code.
newListImagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListImagesResponse
newListImagesResponse pHttpStatus_ =
  ListImagesResponse'
    { images = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of images and their properties.
listImagesResponse_images :: Lens.Lens' ListImagesResponse (Prelude.Maybe [Image])
listImagesResponse_images = Lens.lens (\ListImagesResponse' {images} -> images) (\s@ListImagesResponse' {} a -> s {images = a} :: ListImagesResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token for getting the next set of images, if there are any.
listImagesResponse_nextToken :: Lens.Lens' ListImagesResponse (Prelude.Maybe Prelude.Text)
listImagesResponse_nextToken = Lens.lens (\ListImagesResponse' {nextToken} -> nextToken) (\s@ListImagesResponse' {} a -> s {nextToken = a} :: ListImagesResponse)

-- | The response's http status code.
listImagesResponse_httpStatus :: Lens.Lens' ListImagesResponse Prelude.Int
listImagesResponse_httpStatus = Lens.lens (\ListImagesResponse' {httpStatus} -> httpStatus) (\s@ListImagesResponse' {} a -> s {httpStatus = a} :: ListImagesResponse)

instance Prelude.NFData ListImagesResponse
