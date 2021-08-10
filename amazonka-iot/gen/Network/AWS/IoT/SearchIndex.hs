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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSearchIndex' smart constructor.
data SearchIndex = SearchIndex'
  { -- | The token used to get the next set of results, or @null@ if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The search index name.
    indexName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return at one time.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The query version.
    queryVersion :: Prelude.Maybe Prelude.Text,
    -- | The search query string.
    queryString :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  SearchIndex
newSearchIndex pQueryString_ =
  SearchIndex'
    { nextToken = Prelude.Nothing,
      indexName = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      queryVersion = Prelude.Nothing,
      queryString = pQueryString_
    }

-- | The token used to get the next set of results, or @null@ if there are no
-- additional results.
searchIndex_nextToken :: Lens.Lens' SearchIndex (Prelude.Maybe Prelude.Text)
searchIndex_nextToken = Lens.lens (\SearchIndex' {nextToken} -> nextToken) (\s@SearchIndex' {} a -> s {nextToken = a} :: SearchIndex)

-- | The search index name.
searchIndex_indexName :: Lens.Lens' SearchIndex (Prelude.Maybe Prelude.Text)
searchIndex_indexName = Lens.lens (\SearchIndex' {indexName} -> indexName) (\s@SearchIndex' {} a -> s {indexName = a} :: SearchIndex)

-- | The maximum number of results to return at one time.
searchIndex_maxResults :: Lens.Lens' SearchIndex (Prelude.Maybe Prelude.Natural)
searchIndex_maxResults = Lens.lens (\SearchIndex' {maxResults} -> maxResults) (\s@SearchIndex' {} a -> s {maxResults = a} :: SearchIndex)

-- | The query version.
searchIndex_queryVersion :: Lens.Lens' SearchIndex (Prelude.Maybe Prelude.Text)
searchIndex_queryVersion = Lens.lens (\SearchIndex' {queryVersion} -> queryVersion) (\s@SearchIndex' {} a -> s {queryVersion = a} :: SearchIndex)

-- | The search query string.
searchIndex_queryString :: Lens.Lens' SearchIndex Prelude.Text
searchIndex_queryString = Lens.lens (\SearchIndex' {queryString} -> queryString) (\s@SearchIndex' {} a -> s {queryString = a} :: SearchIndex)

instance Core.AWSRequest SearchIndex where
  type AWSResponse SearchIndex = SearchIndexResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchIndexResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "things" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "thingGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchIndex

instance Prelude.NFData SearchIndex

instance Core.ToHeaders SearchIndex where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON SearchIndex where
  toJSON SearchIndex' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("indexName" Core..=) Prelude.<$> indexName,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            ("queryVersion" Core..=) Prelude.<$> queryVersion,
            Prelude.Just ("queryString" Core..= queryString)
          ]
      )

instance Core.ToPath SearchIndex where
  toPath = Prelude.const "/indices/search"

instance Core.ToQuery SearchIndex where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchIndexResponse' smart constructor.
data SearchIndexResponse = SearchIndexResponse'
  { -- | The token used to get the next set of results, or @null@ if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The things that match the search query.
    things :: Prelude.Maybe [ThingDocument],
    -- | The thing groups that match the search query.
    thingGroups :: Prelude.Maybe [ThingGroupDocument],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  SearchIndexResponse
newSearchIndexResponse pHttpStatus_ =
  SearchIndexResponse'
    { nextToken = Prelude.Nothing,
      things = Prelude.Nothing,
      thingGroups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token used to get the next set of results, or @null@ if there are no
-- additional results.
searchIndexResponse_nextToken :: Lens.Lens' SearchIndexResponse (Prelude.Maybe Prelude.Text)
searchIndexResponse_nextToken = Lens.lens (\SearchIndexResponse' {nextToken} -> nextToken) (\s@SearchIndexResponse' {} a -> s {nextToken = a} :: SearchIndexResponse)

-- | The things that match the search query.
searchIndexResponse_things :: Lens.Lens' SearchIndexResponse (Prelude.Maybe [ThingDocument])
searchIndexResponse_things = Lens.lens (\SearchIndexResponse' {things} -> things) (\s@SearchIndexResponse' {} a -> s {things = a} :: SearchIndexResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The thing groups that match the search query.
searchIndexResponse_thingGroups :: Lens.Lens' SearchIndexResponse (Prelude.Maybe [ThingGroupDocument])
searchIndexResponse_thingGroups = Lens.lens (\SearchIndexResponse' {thingGroups} -> thingGroups) (\s@SearchIndexResponse' {} a -> s {thingGroups = a} :: SearchIndexResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
searchIndexResponse_httpStatus :: Lens.Lens' SearchIndexResponse Prelude.Int
searchIndexResponse_httpStatus = Lens.lens (\SearchIndexResponse' {httpStatus} -> httpStatus) (\s@SearchIndexResponse' {} a -> s {httpStatus = a} :: SearchIndexResponse)

instance Prelude.NFData SearchIndexResponse
