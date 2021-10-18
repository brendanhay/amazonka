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
    listImageVersions_nextToken,
    listImageVersions_sortOrder,
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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListImageVersions' smart constructor.
data ListImageVersions = ListImageVersions'
  { -- | A filter that returns only versions modified on or before the specified
    -- time.
    lastModifiedTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | If the previous call to @ListImageVersions@ didn\'t return the full set
    -- of versions, the call returns a token for getting the next set of
    -- versions.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The sort order. The default value is @DESCENDING@.
    sortOrder :: Prelude.Maybe ImageVersionSortOrder,
    -- | The maximum number of versions to return in the response. The default
    -- value is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A filter that returns only versions created on or before the specified
    -- time.
    creationTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | A filter that returns only versions modified on or after the specified
    -- time.
    lastModifiedTimeAfter :: Prelude.Maybe Core.POSIX,
    -- | The property used to sort results. The default value is @CREATION_TIME@.
    sortBy :: Prelude.Maybe ImageVersionSortBy,
    -- | A filter that returns only versions created on or after the specified
    -- time.
    creationTimeAfter :: Prelude.Maybe Core.POSIX,
    -- | The name of the image to list the versions of.
    imageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'nextToken', 'listImageVersions_nextToken' - If the previous call to @ListImageVersions@ didn\'t return the full set
-- of versions, the call returns a token for getting the next set of
-- versions.
--
-- 'sortOrder', 'listImageVersions_sortOrder' - The sort order. The default value is @DESCENDING@.
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
  Prelude.Text ->
  ListImageVersions
newListImageVersions pImageName_ =
  ListImageVersions'
    { lastModifiedTimeBefore =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      lastModifiedTimeAfter = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      creationTimeAfter = Prelude.Nothing,
      imageName = pImageName_
    }

-- | A filter that returns only versions modified on or before the specified
-- time.
listImageVersions_lastModifiedTimeBefore :: Lens.Lens' ListImageVersions (Prelude.Maybe Prelude.UTCTime)
listImageVersions_lastModifiedTimeBefore = Lens.lens (\ListImageVersions' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListImageVersions' {} a -> s {lastModifiedTimeBefore = a} :: ListImageVersions) Prelude.. Lens.mapping Core._Time

-- | If the previous call to @ListImageVersions@ didn\'t return the full set
-- of versions, the call returns a token for getting the next set of
-- versions.
listImageVersions_nextToken :: Lens.Lens' ListImageVersions (Prelude.Maybe Prelude.Text)
listImageVersions_nextToken = Lens.lens (\ListImageVersions' {nextToken} -> nextToken) (\s@ListImageVersions' {} a -> s {nextToken = a} :: ListImageVersions)

-- | The sort order. The default value is @DESCENDING@.
listImageVersions_sortOrder :: Lens.Lens' ListImageVersions (Prelude.Maybe ImageVersionSortOrder)
listImageVersions_sortOrder = Lens.lens (\ListImageVersions' {sortOrder} -> sortOrder) (\s@ListImageVersions' {} a -> s {sortOrder = a} :: ListImageVersions)

-- | The maximum number of versions to return in the response. The default
-- value is 10.
listImageVersions_maxResults :: Lens.Lens' ListImageVersions (Prelude.Maybe Prelude.Natural)
listImageVersions_maxResults = Lens.lens (\ListImageVersions' {maxResults} -> maxResults) (\s@ListImageVersions' {} a -> s {maxResults = a} :: ListImageVersions)

-- | A filter that returns only versions created on or before the specified
-- time.
listImageVersions_creationTimeBefore :: Lens.Lens' ListImageVersions (Prelude.Maybe Prelude.UTCTime)
listImageVersions_creationTimeBefore = Lens.lens (\ListImageVersions' {creationTimeBefore} -> creationTimeBefore) (\s@ListImageVersions' {} a -> s {creationTimeBefore = a} :: ListImageVersions) Prelude.. Lens.mapping Core._Time

-- | A filter that returns only versions modified on or after the specified
-- time.
listImageVersions_lastModifiedTimeAfter :: Lens.Lens' ListImageVersions (Prelude.Maybe Prelude.UTCTime)
listImageVersions_lastModifiedTimeAfter = Lens.lens (\ListImageVersions' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListImageVersions' {} a -> s {lastModifiedTimeAfter = a} :: ListImageVersions) Prelude.. Lens.mapping Core._Time

-- | The property used to sort results. The default value is @CREATION_TIME@.
listImageVersions_sortBy :: Lens.Lens' ListImageVersions (Prelude.Maybe ImageVersionSortBy)
listImageVersions_sortBy = Lens.lens (\ListImageVersions' {sortBy} -> sortBy) (\s@ListImageVersions' {} a -> s {sortBy = a} :: ListImageVersions)

-- | A filter that returns only versions created on or after the specified
-- time.
listImageVersions_creationTimeAfter :: Lens.Lens' ListImageVersions (Prelude.Maybe Prelude.UTCTime)
listImageVersions_creationTimeAfter = Lens.lens (\ListImageVersions' {creationTimeAfter} -> creationTimeAfter) (\s@ListImageVersions' {} a -> s {creationTimeAfter = a} :: ListImageVersions) Prelude.. Lens.mapping Core._Time

-- | The name of the image to list the versions of.
listImageVersions_imageName :: Lens.Lens' ListImageVersions Prelude.Text
listImageVersions_imageName = Lens.lens (\ListImageVersions' {imageName} -> imageName) (\s@ListImageVersions' {} a -> s {imageName = a} :: ListImageVersions)

instance Core.AWSPager ListImageVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listImageVersionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listImageVersionsResponse_imageVersions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listImageVersions_nextToken
          Lens..~ rs
          Lens.^? listImageVersionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListImageVersions where
  type
    AWSResponse ListImageVersions =
      ListImageVersionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListImageVersionsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "ImageVersions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListImageVersions

instance Prelude.NFData ListImageVersions

instance Core.ToHeaders ListImageVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.ListImageVersions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListImageVersions where
  toJSON ListImageVersions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("LastModifiedTimeBefore" Core..=)
              Prelude.<$> lastModifiedTimeBefore,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("CreationTimeBefore" Core..=)
              Prelude.<$> creationTimeBefore,
            ("LastModifiedTimeAfter" Core..=)
              Prelude.<$> lastModifiedTimeAfter,
            ("SortBy" Core..=) Prelude.<$> sortBy,
            ("CreationTimeAfter" Core..=)
              Prelude.<$> creationTimeAfter,
            Prelude.Just ("ImageName" Core..= imageName)
          ]
      )

instance Core.ToPath ListImageVersions where
  toPath = Prelude.const "/"

instance Core.ToQuery ListImageVersions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListImageVersionsResponse' smart constructor.
data ListImageVersionsResponse = ListImageVersionsResponse'
  { -- | A token for getting the next set of versions, if there are any.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of versions and their properties.
    imageVersions :: Prelude.Maybe [ImageVersion],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListImageVersionsResponse
newListImageVersionsResponse pHttpStatus_ =
  ListImageVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      imageVersions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token for getting the next set of versions, if there are any.
listImageVersionsResponse_nextToken :: Lens.Lens' ListImageVersionsResponse (Prelude.Maybe Prelude.Text)
listImageVersionsResponse_nextToken = Lens.lens (\ListImageVersionsResponse' {nextToken} -> nextToken) (\s@ListImageVersionsResponse' {} a -> s {nextToken = a} :: ListImageVersionsResponse)

-- | A list of versions and their properties.
listImageVersionsResponse_imageVersions :: Lens.Lens' ListImageVersionsResponse (Prelude.Maybe [ImageVersion])
listImageVersionsResponse_imageVersions = Lens.lens (\ListImageVersionsResponse' {imageVersions} -> imageVersions) (\s@ListImageVersionsResponse' {} a -> s {imageVersions = a} :: ListImageVersionsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listImageVersionsResponse_httpStatus :: Lens.Lens' ListImageVersionsResponse Prelude.Int
listImageVersionsResponse_httpStatus = Lens.lens (\ListImageVersionsResponse' {httpStatus} -> httpStatus) (\s@ListImageVersionsResponse' {} a -> s {httpStatus = a} :: ListImageVersionsResponse)

instance Prelude.NFData ListImageVersionsResponse
