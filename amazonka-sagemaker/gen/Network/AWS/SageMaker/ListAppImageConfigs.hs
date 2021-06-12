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
-- Module      : Network.AWS.SageMaker.ListAppImageConfigs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the AppImageConfigs in your account and their properties. The list
-- can be filtered by creation time or modified time, and whether the
-- AppImageConfig name contains a specified string.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListAppImageConfigs
  ( -- * Creating a Request
    ListAppImageConfigs (..),
    newListAppImageConfigs,

    -- * Request Lenses
    listAppImageConfigs_sortOrder,
    listAppImageConfigs_nextToken,
    listAppImageConfigs_nameContains,
    listAppImageConfigs_maxResults,
    listAppImageConfigs_modifiedTimeBefore,
    listAppImageConfigs_creationTimeBefore,
    listAppImageConfigs_sortBy,
    listAppImageConfigs_creationTimeAfter,
    listAppImageConfigs_modifiedTimeAfter,

    -- * Destructuring the Response
    ListAppImageConfigsResponse (..),
    newListAppImageConfigsResponse,

    -- * Response Lenses
    listAppImageConfigsResponse_nextToken,
    listAppImageConfigsResponse_appImageConfigs,
    listAppImageConfigsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListAppImageConfigs' smart constructor.
data ListAppImageConfigs = ListAppImageConfigs'
  { -- | The sort order. The default value is @Descending@.
    sortOrder :: Core.Maybe SortOrder,
    -- | If the previous call to @ListImages@ didn\'t return the full set of
    -- AppImageConfigs, the call returns a token for getting the next set of
    -- AppImageConfigs.
    nextToken :: Core.Maybe Core.Text,
    -- | A filter that returns only AppImageConfigs whose name contains the
    -- specified string.
    nameContains :: Core.Maybe Core.Text,
    -- | The maximum number of AppImageConfigs to return in the response. The
    -- default value is 10.
    maxResults :: Core.Maybe Core.Natural,
    -- | A filter that returns only AppImageConfigs modified on or before the
    -- specified time.
    modifiedTimeBefore :: Core.Maybe Core.POSIX,
    -- | A filter that returns only AppImageConfigs created on or before the
    -- specified time.
    creationTimeBefore :: Core.Maybe Core.POSIX,
    -- | The property used to sort results. The default value is @CreationTime@.
    sortBy :: Core.Maybe AppImageConfigSortKey,
    -- | A filter that returns only AppImageConfigs created on or after the
    -- specified time.
    creationTimeAfter :: Core.Maybe Core.POSIX,
    -- | A filter that returns only AppImageConfigs modified on or after the
    -- specified time.
    modifiedTimeAfter :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAppImageConfigs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listAppImageConfigs_sortOrder' - The sort order. The default value is @Descending@.
--
-- 'nextToken', 'listAppImageConfigs_nextToken' - If the previous call to @ListImages@ didn\'t return the full set of
-- AppImageConfigs, the call returns a token for getting the next set of
-- AppImageConfigs.
--
-- 'nameContains', 'listAppImageConfigs_nameContains' - A filter that returns only AppImageConfigs whose name contains the
-- specified string.
--
-- 'maxResults', 'listAppImageConfigs_maxResults' - The maximum number of AppImageConfigs to return in the response. The
-- default value is 10.
--
-- 'modifiedTimeBefore', 'listAppImageConfigs_modifiedTimeBefore' - A filter that returns only AppImageConfigs modified on or before the
-- specified time.
--
-- 'creationTimeBefore', 'listAppImageConfigs_creationTimeBefore' - A filter that returns only AppImageConfigs created on or before the
-- specified time.
--
-- 'sortBy', 'listAppImageConfigs_sortBy' - The property used to sort results. The default value is @CreationTime@.
--
-- 'creationTimeAfter', 'listAppImageConfigs_creationTimeAfter' - A filter that returns only AppImageConfigs created on or after the
-- specified time.
--
-- 'modifiedTimeAfter', 'listAppImageConfigs_modifiedTimeAfter' - A filter that returns only AppImageConfigs modified on or after the
-- specified time.
newListAppImageConfigs ::
  ListAppImageConfigs
newListAppImageConfigs =
  ListAppImageConfigs'
    { sortOrder = Core.Nothing,
      nextToken = Core.Nothing,
      nameContains = Core.Nothing,
      maxResults = Core.Nothing,
      modifiedTimeBefore = Core.Nothing,
      creationTimeBefore = Core.Nothing,
      sortBy = Core.Nothing,
      creationTimeAfter = Core.Nothing,
      modifiedTimeAfter = Core.Nothing
    }

-- | The sort order. The default value is @Descending@.
listAppImageConfigs_sortOrder :: Lens.Lens' ListAppImageConfigs (Core.Maybe SortOrder)
listAppImageConfigs_sortOrder = Lens.lens (\ListAppImageConfigs' {sortOrder} -> sortOrder) (\s@ListAppImageConfigs' {} a -> s {sortOrder = a} :: ListAppImageConfigs)

-- | If the previous call to @ListImages@ didn\'t return the full set of
-- AppImageConfigs, the call returns a token for getting the next set of
-- AppImageConfigs.
listAppImageConfigs_nextToken :: Lens.Lens' ListAppImageConfigs (Core.Maybe Core.Text)
listAppImageConfigs_nextToken = Lens.lens (\ListAppImageConfigs' {nextToken} -> nextToken) (\s@ListAppImageConfigs' {} a -> s {nextToken = a} :: ListAppImageConfigs)

-- | A filter that returns only AppImageConfigs whose name contains the
-- specified string.
listAppImageConfigs_nameContains :: Lens.Lens' ListAppImageConfigs (Core.Maybe Core.Text)
listAppImageConfigs_nameContains = Lens.lens (\ListAppImageConfigs' {nameContains} -> nameContains) (\s@ListAppImageConfigs' {} a -> s {nameContains = a} :: ListAppImageConfigs)

-- | The maximum number of AppImageConfigs to return in the response. The
-- default value is 10.
listAppImageConfigs_maxResults :: Lens.Lens' ListAppImageConfigs (Core.Maybe Core.Natural)
listAppImageConfigs_maxResults = Lens.lens (\ListAppImageConfigs' {maxResults} -> maxResults) (\s@ListAppImageConfigs' {} a -> s {maxResults = a} :: ListAppImageConfigs)

-- | A filter that returns only AppImageConfigs modified on or before the
-- specified time.
listAppImageConfigs_modifiedTimeBefore :: Lens.Lens' ListAppImageConfigs (Core.Maybe Core.UTCTime)
listAppImageConfigs_modifiedTimeBefore = Lens.lens (\ListAppImageConfigs' {modifiedTimeBefore} -> modifiedTimeBefore) (\s@ListAppImageConfigs' {} a -> s {modifiedTimeBefore = a} :: ListAppImageConfigs) Core.. Lens.mapping Core._Time

-- | A filter that returns only AppImageConfigs created on or before the
-- specified time.
listAppImageConfigs_creationTimeBefore :: Lens.Lens' ListAppImageConfigs (Core.Maybe Core.UTCTime)
listAppImageConfigs_creationTimeBefore = Lens.lens (\ListAppImageConfigs' {creationTimeBefore} -> creationTimeBefore) (\s@ListAppImageConfigs' {} a -> s {creationTimeBefore = a} :: ListAppImageConfigs) Core.. Lens.mapping Core._Time

-- | The property used to sort results. The default value is @CreationTime@.
listAppImageConfigs_sortBy :: Lens.Lens' ListAppImageConfigs (Core.Maybe AppImageConfigSortKey)
listAppImageConfigs_sortBy = Lens.lens (\ListAppImageConfigs' {sortBy} -> sortBy) (\s@ListAppImageConfigs' {} a -> s {sortBy = a} :: ListAppImageConfigs)

-- | A filter that returns only AppImageConfigs created on or after the
-- specified time.
listAppImageConfigs_creationTimeAfter :: Lens.Lens' ListAppImageConfigs (Core.Maybe Core.UTCTime)
listAppImageConfigs_creationTimeAfter = Lens.lens (\ListAppImageConfigs' {creationTimeAfter} -> creationTimeAfter) (\s@ListAppImageConfigs' {} a -> s {creationTimeAfter = a} :: ListAppImageConfigs) Core.. Lens.mapping Core._Time

-- | A filter that returns only AppImageConfigs modified on or after the
-- specified time.
listAppImageConfigs_modifiedTimeAfter :: Lens.Lens' ListAppImageConfigs (Core.Maybe Core.UTCTime)
listAppImageConfigs_modifiedTimeAfter = Lens.lens (\ListAppImageConfigs' {modifiedTimeAfter} -> modifiedTimeAfter) (\s@ListAppImageConfigs' {} a -> s {modifiedTimeAfter = a} :: ListAppImageConfigs) Core.. Lens.mapping Core._Time

instance Core.AWSPager ListAppImageConfigs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAppImageConfigsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listAppImageConfigsResponse_appImageConfigs
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listAppImageConfigs_nextToken
          Lens..~ rs
          Lens.^? listAppImageConfigsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListAppImageConfigs where
  type
    AWSResponse ListAppImageConfigs =
      ListAppImageConfigsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAppImageConfigsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "AppImageConfigs" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListAppImageConfigs

instance Core.NFData ListAppImageConfigs

instance Core.ToHeaders ListAppImageConfigs where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListAppImageConfigs" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListAppImageConfigs where
  toJSON ListAppImageConfigs' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SortOrder" Core..=) Core.<$> sortOrder,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("NameContains" Core..=) Core.<$> nameContains,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("ModifiedTimeBefore" Core..=)
              Core.<$> modifiedTimeBefore,
            ("CreationTimeBefore" Core..=)
              Core.<$> creationTimeBefore,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("CreationTimeAfter" Core..=)
              Core.<$> creationTimeAfter,
            ("ModifiedTimeAfter" Core..=)
              Core.<$> modifiedTimeAfter
          ]
      )

instance Core.ToPath ListAppImageConfigs where
  toPath = Core.const "/"

instance Core.ToQuery ListAppImageConfigs where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListAppImageConfigsResponse' smart constructor.
data ListAppImageConfigsResponse = ListAppImageConfigsResponse'
  { -- | A token for getting the next set of AppImageConfigs, if there are any.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of AppImageConfigs and their properties.
    appImageConfigs :: Core.Maybe [AppImageConfigDetails],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAppImageConfigsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAppImageConfigsResponse_nextToken' - A token for getting the next set of AppImageConfigs, if there are any.
--
-- 'appImageConfigs', 'listAppImageConfigsResponse_appImageConfigs' - A list of AppImageConfigs and their properties.
--
-- 'httpStatus', 'listAppImageConfigsResponse_httpStatus' - The response's http status code.
newListAppImageConfigsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListAppImageConfigsResponse
newListAppImageConfigsResponse pHttpStatus_ =
  ListAppImageConfigsResponse'
    { nextToken =
        Core.Nothing,
      appImageConfigs = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token for getting the next set of AppImageConfigs, if there are any.
listAppImageConfigsResponse_nextToken :: Lens.Lens' ListAppImageConfigsResponse (Core.Maybe Core.Text)
listAppImageConfigsResponse_nextToken = Lens.lens (\ListAppImageConfigsResponse' {nextToken} -> nextToken) (\s@ListAppImageConfigsResponse' {} a -> s {nextToken = a} :: ListAppImageConfigsResponse)

-- | A list of AppImageConfigs and their properties.
listAppImageConfigsResponse_appImageConfigs :: Lens.Lens' ListAppImageConfigsResponse (Core.Maybe [AppImageConfigDetails])
listAppImageConfigsResponse_appImageConfigs = Lens.lens (\ListAppImageConfigsResponse' {appImageConfigs} -> appImageConfigs) (\s@ListAppImageConfigsResponse' {} a -> s {appImageConfigs = a} :: ListAppImageConfigsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listAppImageConfigsResponse_httpStatus :: Lens.Lens' ListAppImageConfigsResponse Core.Int
listAppImageConfigsResponse_httpStatus = Lens.lens (\ListAppImageConfigsResponse' {httpStatus} -> httpStatus) (\s@ListAppImageConfigsResponse' {} a -> s {httpStatus = a} :: ListAppImageConfigsResponse)

instance Core.NFData ListAppImageConfigsResponse
