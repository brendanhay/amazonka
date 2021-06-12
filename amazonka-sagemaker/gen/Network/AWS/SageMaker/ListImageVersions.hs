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
-- Module      : Network.AWS.SageMaker.ListImageVersions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a specified image and their properties. The list
-- can be filtered by creation time or modified time.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListImageVersions
  ( -- * Creating a Request
    ListImageVersions (..),
    newListImageVersions,

    -- * Request Lenses
    listImageVersions_lastModifiedTimeBefore,
    listImageVersions_sortOrder,
    listImageVersions_nextToken,
    listImageVersions_maxResults,
    listImageVersions_creationTimeBefore,
    listImageVersions_lastModifiedTimeAfter,
    listImageVersions_sortBy,
    listImageVersions_creationTimeAfter,
    listImageVersions_imageName,

    -- * Destructuring the Response
    ListImageVersionsResponse (..),
    newListImageVersionsResponse,

    -- * Response Lenses
    listImageVersionsResponse_nextToken,
    listImageVersionsResponse_imageVersions,
    listImageVersionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListImageVersions' smart constructor.
data ListImageVersions = ListImageVersions'
  { -- | A filter that returns only versions modified on or before the specified
    -- time.
    lastModifiedTimeBefore :: Core.Maybe Core.POSIX,
    -- | The sort order. The default value is @DESCENDING@.
    sortOrder :: Core.Maybe ImageVersionSortOrder,
    -- | If the previous call to @ListImageVersions@ didn\'t return the full set
    -- of versions, the call returns a token for getting the next set of
    -- versions.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of versions to return in the response. The default
    -- value is 10.
    maxResults :: Core.Maybe Core.Natural,
    -- | A filter that returns only versions created on or before the specified
    -- time.
    creationTimeBefore :: Core.Maybe Core.POSIX,
    -- | A filter that returns only versions modified on or after the specified
    -- time.
    lastModifiedTimeAfter :: Core.Maybe Core.POSIX,
    -- | The property used to sort results. The default value is @CREATION_TIME@.
    sortBy :: Core.Maybe ImageVersionSortBy,
    -- | A filter that returns only versions created on or after the specified
    -- time.
    creationTimeAfter :: Core.Maybe Core.POSIX,
    -- | The name of the image to list the versions of.
    imageName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListImageVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedTimeBefore', 'listImageVersions_lastModifiedTimeBefore' - A filter that returns only versions modified on or before the specified
-- time.
--
-- 'sortOrder', 'listImageVersions_sortOrder' - The sort order. The default value is @DESCENDING@.
--
-- 'nextToken', 'listImageVersions_nextToken' - If the previous call to @ListImageVersions@ didn\'t return the full set
-- of versions, the call returns a token for getting the next set of
-- versions.
--
-- 'maxResults', 'listImageVersions_maxResults' - The maximum number of versions to return in the response. The default
-- value is 10.
--
-- 'creationTimeBefore', 'listImageVersions_creationTimeBefore' - A filter that returns only versions created on or before the specified
-- time.
--
-- 'lastModifiedTimeAfter', 'listImageVersions_lastModifiedTimeAfter' - A filter that returns only versions modified on or after the specified
-- time.
--
-- 'sortBy', 'listImageVersions_sortBy' - The property used to sort results. The default value is @CREATION_TIME@.
--
-- 'creationTimeAfter', 'listImageVersions_creationTimeAfter' - A filter that returns only versions created on or after the specified
-- time.
--
-- 'imageName', 'listImageVersions_imageName' - The name of the image to list the versions of.
newListImageVersions ::
  -- | 'imageName'
  Core.Text ->
  ListImageVersions
newListImageVersions pImageName_ =
  ListImageVersions'
    { lastModifiedTimeBefore =
        Core.Nothing,
      sortOrder = Core.Nothing,
      nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      creationTimeBefore = Core.Nothing,
      lastModifiedTimeAfter = Core.Nothing,
      sortBy = Core.Nothing,
      creationTimeAfter = Core.Nothing,
      imageName = pImageName_
    }

-- | A filter that returns only versions modified on or before the specified
-- time.
listImageVersions_lastModifiedTimeBefore :: Lens.Lens' ListImageVersions (Core.Maybe Core.UTCTime)
listImageVersions_lastModifiedTimeBefore = Lens.lens (\ListImageVersions' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListImageVersions' {} a -> s {lastModifiedTimeBefore = a} :: ListImageVersions) Core.. Lens.mapping Core._Time

-- | The sort order. The default value is @DESCENDING@.
listImageVersions_sortOrder :: Lens.Lens' ListImageVersions (Core.Maybe ImageVersionSortOrder)
listImageVersions_sortOrder = Lens.lens (\ListImageVersions' {sortOrder} -> sortOrder) (\s@ListImageVersions' {} a -> s {sortOrder = a} :: ListImageVersions)

-- | If the previous call to @ListImageVersions@ didn\'t return the full set
-- of versions, the call returns a token for getting the next set of
-- versions.
listImageVersions_nextToken :: Lens.Lens' ListImageVersions (Core.Maybe Core.Text)
listImageVersions_nextToken = Lens.lens (\ListImageVersions' {nextToken} -> nextToken) (\s@ListImageVersions' {} a -> s {nextToken = a} :: ListImageVersions)

-- | The maximum number of versions to return in the response. The default
-- value is 10.
listImageVersions_maxResults :: Lens.Lens' ListImageVersions (Core.Maybe Core.Natural)
listImageVersions_maxResults = Lens.lens (\ListImageVersions' {maxResults} -> maxResults) (\s@ListImageVersions' {} a -> s {maxResults = a} :: ListImageVersions)

-- | A filter that returns only versions created on or before the specified
-- time.
listImageVersions_creationTimeBefore :: Lens.Lens' ListImageVersions (Core.Maybe Core.UTCTime)
listImageVersions_creationTimeBefore = Lens.lens (\ListImageVersions' {creationTimeBefore} -> creationTimeBefore) (\s@ListImageVersions' {} a -> s {creationTimeBefore = a} :: ListImageVersions) Core.. Lens.mapping Core._Time

-- | A filter that returns only versions modified on or after the specified
-- time.
listImageVersions_lastModifiedTimeAfter :: Lens.Lens' ListImageVersions (Core.Maybe Core.UTCTime)
listImageVersions_lastModifiedTimeAfter = Lens.lens (\ListImageVersions' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListImageVersions' {} a -> s {lastModifiedTimeAfter = a} :: ListImageVersions) Core.. Lens.mapping Core._Time

-- | The property used to sort results. The default value is @CREATION_TIME@.
listImageVersions_sortBy :: Lens.Lens' ListImageVersions (Core.Maybe ImageVersionSortBy)
listImageVersions_sortBy = Lens.lens (\ListImageVersions' {sortBy} -> sortBy) (\s@ListImageVersions' {} a -> s {sortBy = a} :: ListImageVersions)

-- | A filter that returns only versions created on or after the specified
-- time.
listImageVersions_creationTimeAfter :: Lens.Lens' ListImageVersions (Core.Maybe Core.UTCTime)
listImageVersions_creationTimeAfter = Lens.lens (\ListImageVersions' {creationTimeAfter} -> creationTimeAfter) (\s@ListImageVersions' {} a -> s {creationTimeAfter = a} :: ListImageVersions) Core.. Lens.mapping Core._Time

-- | The name of the image to list the versions of.
listImageVersions_imageName :: Lens.Lens' ListImageVersions Core.Text
listImageVersions_imageName = Lens.lens (\ListImageVersions' {imageName} -> imageName) (\s@ListImageVersions' {} a -> s {imageName = a} :: ListImageVersions)

instance Core.AWSPager ListImageVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listImageVersionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listImageVersionsResponse_imageVersions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listImageVersions_nextToken
          Lens..~ rs
          Lens.^? listImageVersionsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListImageVersions where
  type
    AWSResponse ListImageVersions =
      ListImageVersionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListImageVersionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "ImageVersions" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListImageVersions

instance Core.NFData ListImageVersions

instance Core.ToHeaders ListImageVersions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListImageVersions" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListImageVersions where
  toJSON ListImageVersions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("LastModifiedTimeBefore" Core..=)
              Core.<$> lastModifiedTimeBefore,
            ("SortOrder" Core..=) Core.<$> sortOrder,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("CreationTimeBefore" Core..=)
              Core.<$> creationTimeBefore,
            ("LastModifiedTimeAfter" Core..=)
              Core.<$> lastModifiedTimeAfter,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("CreationTimeAfter" Core..=)
              Core.<$> creationTimeAfter,
            Core.Just ("ImageName" Core..= imageName)
          ]
      )

instance Core.ToPath ListImageVersions where
  toPath = Core.const "/"

instance Core.ToQuery ListImageVersions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListImageVersionsResponse' smart constructor.
data ListImageVersionsResponse = ListImageVersionsResponse'
  { -- | A token for getting the next set of versions, if there are any.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of versions and their properties.
    imageVersions :: Core.Maybe [ImageVersion],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListImageVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listImageVersionsResponse_nextToken' - A token for getting the next set of versions, if there are any.
--
-- 'imageVersions', 'listImageVersionsResponse_imageVersions' - A list of versions and their properties.
--
-- 'httpStatus', 'listImageVersionsResponse_httpStatus' - The response's http status code.
newListImageVersionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListImageVersionsResponse
newListImageVersionsResponse pHttpStatus_ =
  ListImageVersionsResponse'
    { nextToken =
        Core.Nothing,
      imageVersions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token for getting the next set of versions, if there are any.
listImageVersionsResponse_nextToken :: Lens.Lens' ListImageVersionsResponse (Core.Maybe Core.Text)
listImageVersionsResponse_nextToken = Lens.lens (\ListImageVersionsResponse' {nextToken} -> nextToken) (\s@ListImageVersionsResponse' {} a -> s {nextToken = a} :: ListImageVersionsResponse)

-- | A list of versions and their properties.
listImageVersionsResponse_imageVersions :: Lens.Lens' ListImageVersionsResponse (Core.Maybe [ImageVersion])
listImageVersionsResponse_imageVersions = Lens.lens (\ListImageVersionsResponse' {imageVersions} -> imageVersions) (\s@ListImageVersionsResponse' {} a -> s {imageVersions = a} :: ListImageVersionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listImageVersionsResponse_httpStatus :: Lens.Lens' ListImageVersionsResponse Core.Int
listImageVersionsResponse_httpStatus = Lens.lens (\ListImageVersionsResponse' {httpStatus} -> httpStatus) (\s@ListImageVersionsResponse' {} a -> s {httpStatus = a} :: ListImageVersionsResponse)

instance Core.NFData ListImageVersionsResponse
