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
-- Module      : Network.AWS.IoT.SearchIndex
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The query search index.
module Network.AWS.IoT.SearchIndex
  ( -- * Creating a Request
    SearchIndex (..),
    newSearchIndex,

    -- * Request Lenses
    searchIndex_nextToken,
    searchIndex_indexName,
    searchIndex_maxResults,
    searchIndex_queryVersion,
    searchIndex_queryString,

    -- * Destructuring the Response
    SearchIndexResponse (..),
    newSearchIndexResponse,

    -- * Response Lenses
    searchIndexResponse_nextToken,
    searchIndexResponse_things,
    searchIndexResponse_thingGroups,
    searchIndexResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSearchIndex' smart constructor.
data SearchIndex = SearchIndex'
  { -- | The token used to get the next set of results, or @null@ if there are no
    -- additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The search index name.
    indexName :: Core.Maybe Core.Text,
    -- | The maximum number of results to return at one time.
    maxResults :: Core.Maybe Core.Natural,
    -- | The query version.
    queryVersion :: Core.Maybe Core.Text,
    -- | The search query string.
    queryString :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SearchIndex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchIndex_nextToken' - The token used to get the next set of results, or @null@ if there are no
-- additional results.
--
-- 'indexName', 'searchIndex_indexName' - The search index name.
--
-- 'maxResults', 'searchIndex_maxResults' - The maximum number of results to return at one time.
--
-- 'queryVersion', 'searchIndex_queryVersion' - The query version.
--
-- 'queryString', 'searchIndex_queryString' - The search query string.
newSearchIndex ::
  -- | 'queryString'
  Core.Text ->
  SearchIndex
newSearchIndex pQueryString_ =
  SearchIndex'
    { nextToken = Core.Nothing,
      indexName = Core.Nothing,
      maxResults = Core.Nothing,
      queryVersion = Core.Nothing,
      queryString = pQueryString_
    }

-- | The token used to get the next set of results, or @null@ if there are no
-- additional results.
searchIndex_nextToken :: Lens.Lens' SearchIndex (Core.Maybe Core.Text)
searchIndex_nextToken = Lens.lens (\SearchIndex' {nextToken} -> nextToken) (\s@SearchIndex' {} a -> s {nextToken = a} :: SearchIndex)

-- | The search index name.
searchIndex_indexName :: Lens.Lens' SearchIndex (Core.Maybe Core.Text)
searchIndex_indexName = Lens.lens (\SearchIndex' {indexName} -> indexName) (\s@SearchIndex' {} a -> s {indexName = a} :: SearchIndex)

-- | The maximum number of results to return at one time.
searchIndex_maxResults :: Lens.Lens' SearchIndex (Core.Maybe Core.Natural)
searchIndex_maxResults = Lens.lens (\SearchIndex' {maxResults} -> maxResults) (\s@SearchIndex' {} a -> s {maxResults = a} :: SearchIndex)

-- | The query version.
searchIndex_queryVersion :: Lens.Lens' SearchIndex (Core.Maybe Core.Text)
searchIndex_queryVersion = Lens.lens (\SearchIndex' {queryVersion} -> queryVersion) (\s@SearchIndex' {} a -> s {queryVersion = a} :: SearchIndex)

-- | The search query string.
searchIndex_queryString :: Lens.Lens' SearchIndex Core.Text
searchIndex_queryString = Lens.lens (\SearchIndex' {queryString} -> queryString) (\s@SearchIndex' {} a -> s {queryString = a} :: SearchIndex)

instance Core.AWSRequest SearchIndex where
  type AWSResponse SearchIndex = SearchIndexResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchIndexResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "things" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "thingGroups" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SearchIndex

instance Core.NFData SearchIndex

instance Core.ToHeaders SearchIndex where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON SearchIndex where
  toJSON SearchIndex' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("indexName" Core..=) Core.<$> indexName,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("queryVersion" Core..=) Core.<$> queryVersion,
            Core.Just ("queryString" Core..= queryString)
          ]
      )

instance Core.ToPath SearchIndex where
  toPath = Core.const "/indices/search"

instance Core.ToQuery SearchIndex where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newSearchIndexResponse' smart constructor.
data SearchIndexResponse = SearchIndexResponse'
  { -- | The token used to get the next set of results, or @null@ if there are no
    -- additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The things that match the search query.
    things :: Core.Maybe [ThingDocument],
    -- | The thing groups that match the search query.
    thingGroups :: Core.Maybe [ThingGroupDocument],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SearchIndexResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchIndexResponse_nextToken' - The token used to get the next set of results, or @null@ if there are no
-- additional results.
--
-- 'things', 'searchIndexResponse_things' - The things that match the search query.
--
-- 'thingGroups', 'searchIndexResponse_thingGroups' - The thing groups that match the search query.
--
-- 'httpStatus', 'searchIndexResponse_httpStatus' - The response's http status code.
newSearchIndexResponse ::
  -- | 'httpStatus'
  Core.Int ->
  SearchIndexResponse
newSearchIndexResponse pHttpStatus_ =
  SearchIndexResponse'
    { nextToken = Core.Nothing,
      things = Core.Nothing,
      thingGroups = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token used to get the next set of results, or @null@ if there are no
-- additional results.
searchIndexResponse_nextToken :: Lens.Lens' SearchIndexResponse (Core.Maybe Core.Text)
searchIndexResponse_nextToken = Lens.lens (\SearchIndexResponse' {nextToken} -> nextToken) (\s@SearchIndexResponse' {} a -> s {nextToken = a} :: SearchIndexResponse)

-- | The things that match the search query.
searchIndexResponse_things :: Lens.Lens' SearchIndexResponse (Core.Maybe [ThingDocument])
searchIndexResponse_things = Lens.lens (\SearchIndexResponse' {things} -> things) (\s@SearchIndexResponse' {} a -> s {things = a} :: SearchIndexResponse) Core.. Lens.mapping Lens._Coerce

-- | The thing groups that match the search query.
searchIndexResponse_thingGroups :: Lens.Lens' SearchIndexResponse (Core.Maybe [ThingGroupDocument])
searchIndexResponse_thingGroups = Lens.lens (\SearchIndexResponse' {thingGroups} -> thingGroups) (\s@SearchIndexResponse' {} a -> s {thingGroups = a} :: SearchIndexResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
searchIndexResponse_httpStatus :: Lens.Lens' SearchIndexResponse Core.Int
searchIndexResponse_httpStatus = Lens.lens (\SearchIndexResponse' {httpStatus} -> httpStatus) (\s@SearchIndexResponse' {} a -> s {httpStatus = a} :: SearchIndexResponse)

instance Core.NFData SearchIndexResponse
