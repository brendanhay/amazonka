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
-- Module      : Amazonka.IoT.SearchIndex
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The query search index.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions SearchIndex>
-- action.
module Amazonka.IoT.SearchIndex
  ( -- * Creating a Request
    SearchIndex (..),
    newSearchIndex,

    -- * Request Lenses
    searchIndex_indexName,
    searchIndex_maxResults,
    searchIndex_nextToken,
    searchIndex_queryVersion,
    searchIndex_queryString,

    -- * Destructuring the Response
    SearchIndexResponse (..),
    newSearchIndexResponse,

    -- * Response Lenses
    searchIndexResponse_nextToken,
    searchIndexResponse_thingGroups,
    searchIndexResponse_things,
    searchIndexResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchIndex' smart constructor.
data SearchIndex = SearchIndex'
  { -- | The search index name.
    indexName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return at one time.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token used to get the next set of results, or @null@ if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The query version.
    queryVersion :: Prelude.Maybe Prelude.Text,
    -- | The search query string. For more information about the search query
    -- syntax, see
    -- <https://docs.aws.amazon.com/iot/latest/developerguide/query-syntax.html Query syntax>.
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
-- 'indexName', 'searchIndex_indexName' - The search index name.
--
-- 'maxResults', 'searchIndex_maxResults' - The maximum number of results to return at one time.
--
-- 'nextToken', 'searchIndex_nextToken' - The token used to get the next set of results, or @null@ if there are no
-- additional results.
--
-- 'queryVersion', 'searchIndex_queryVersion' - The query version.
--
-- 'queryString', 'searchIndex_queryString' - The search query string. For more information about the search query
-- syntax, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/query-syntax.html Query syntax>.
newSearchIndex ::
  -- | 'queryString'
  Prelude.Text ->
  SearchIndex
newSearchIndex pQueryString_ =
  SearchIndex'
    { indexName = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      queryVersion = Prelude.Nothing,
      queryString = pQueryString_
    }

-- | The search index name.
searchIndex_indexName :: Lens.Lens' SearchIndex (Prelude.Maybe Prelude.Text)
searchIndex_indexName = Lens.lens (\SearchIndex' {indexName} -> indexName) (\s@SearchIndex' {} a -> s {indexName = a} :: SearchIndex)

-- | The maximum number of results to return at one time.
searchIndex_maxResults :: Lens.Lens' SearchIndex (Prelude.Maybe Prelude.Natural)
searchIndex_maxResults = Lens.lens (\SearchIndex' {maxResults} -> maxResults) (\s@SearchIndex' {} a -> s {maxResults = a} :: SearchIndex)

-- | The token used to get the next set of results, or @null@ if there are no
-- additional results.
searchIndex_nextToken :: Lens.Lens' SearchIndex (Prelude.Maybe Prelude.Text)
searchIndex_nextToken = Lens.lens (\SearchIndex' {nextToken} -> nextToken) (\s@SearchIndex' {} a -> s {nextToken = a} :: SearchIndex)

-- | The query version.
searchIndex_queryVersion :: Lens.Lens' SearchIndex (Prelude.Maybe Prelude.Text)
searchIndex_queryVersion = Lens.lens (\SearchIndex' {queryVersion} -> queryVersion) (\s@SearchIndex' {} a -> s {queryVersion = a} :: SearchIndex)

-- | The search query string. For more information about the search query
-- syntax, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/query-syntax.html Query syntax>.
searchIndex_queryString :: Lens.Lens' SearchIndex Prelude.Text
searchIndex_queryString = Lens.lens (\SearchIndex' {queryString} -> queryString) (\s@SearchIndex' {} a -> s {queryString = a} :: SearchIndex)

instance Core.AWSRequest SearchIndex where
  type AWSResponse SearchIndex = SearchIndexResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchIndexResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "thingGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "things" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchIndex where
  hashWithSalt _salt SearchIndex' {..} =
    _salt `Prelude.hashWithSalt` indexName
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` queryVersion
      `Prelude.hashWithSalt` queryString

instance Prelude.NFData SearchIndex where
  rnf SearchIndex' {..} =
    Prelude.rnf indexName
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf queryVersion
      `Prelude.seq` Prelude.rnf queryString

instance Data.ToHeaders SearchIndex where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON SearchIndex where
  toJSON SearchIndex' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("indexName" Data..=) Prelude.<$> indexName,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("queryVersion" Data..=) Prelude.<$> queryVersion,
            Prelude.Just ("queryString" Data..= queryString)
          ]
      )

instance Data.ToPath SearchIndex where
  toPath = Prelude.const "/indices/search"

instance Data.ToQuery SearchIndex where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchIndexResponse' smart constructor.
data SearchIndexResponse = SearchIndexResponse'
  { -- | The token used to get the next set of results, or @null@ if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The thing groups that match the search query.
    thingGroups :: Prelude.Maybe [ThingGroupDocument],
    -- | The things that match the search query.
    things :: Prelude.Maybe [ThingDocument],
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
-- 'thingGroups', 'searchIndexResponse_thingGroups' - The thing groups that match the search query.
--
-- 'things', 'searchIndexResponse_things' - The things that match the search query.
--
-- 'httpStatus', 'searchIndexResponse_httpStatus' - The response's http status code.
newSearchIndexResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchIndexResponse
newSearchIndexResponse pHttpStatus_ =
  SearchIndexResponse'
    { nextToken = Prelude.Nothing,
      thingGroups = Prelude.Nothing,
      things = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token used to get the next set of results, or @null@ if there are no
-- additional results.
searchIndexResponse_nextToken :: Lens.Lens' SearchIndexResponse (Prelude.Maybe Prelude.Text)
searchIndexResponse_nextToken = Lens.lens (\SearchIndexResponse' {nextToken} -> nextToken) (\s@SearchIndexResponse' {} a -> s {nextToken = a} :: SearchIndexResponse)

-- | The thing groups that match the search query.
searchIndexResponse_thingGroups :: Lens.Lens' SearchIndexResponse (Prelude.Maybe [ThingGroupDocument])
searchIndexResponse_thingGroups = Lens.lens (\SearchIndexResponse' {thingGroups} -> thingGroups) (\s@SearchIndexResponse' {} a -> s {thingGroups = a} :: SearchIndexResponse) Prelude.. Lens.mapping Lens.coerced

-- | The things that match the search query.
searchIndexResponse_things :: Lens.Lens' SearchIndexResponse (Prelude.Maybe [ThingDocument])
searchIndexResponse_things = Lens.lens (\SearchIndexResponse' {things} -> things) (\s@SearchIndexResponse' {} a -> s {things = a} :: SearchIndexResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
searchIndexResponse_httpStatus :: Lens.Lens' SearchIndexResponse Prelude.Int
searchIndexResponse_httpStatus = Lens.lens (\SearchIndexResponse' {httpStatus} -> httpStatus) (\s@SearchIndexResponse' {} a -> s {httpStatus = a} :: SearchIndexResponse)

instance Prelude.NFData SearchIndexResponse where
  rnf SearchIndexResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf thingGroups
      `Prelude.seq` Prelude.rnf things
      `Prelude.seq` Prelude.rnf httpStatus
