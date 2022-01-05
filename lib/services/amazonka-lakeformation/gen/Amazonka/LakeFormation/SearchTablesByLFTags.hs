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
-- Module      : Amazonka.LakeFormation.SearchTablesByLFTags
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation allows a search on @TABLE@ resources by @LFTag@s. This
-- will be used by admins who want to grant user permissions on certain
-- LFTags. Before making a grant, the admin can use @SearchTablesByLFTags@
-- to find all resources where the given @LFTag@s are valid to verify
-- whether the returned resources can be shared.
module Amazonka.LakeFormation.SearchTablesByLFTags
  ( -- * Creating a Request
    SearchTablesByLFTags (..),
    newSearchTablesByLFTags,

    -- * Request Lenses
    searchTablesByLFTags_catalogId,
    searchTablesByLFTags_nextToken,
    searchTablesByLFTags_maxResults,
    searchTablesByLFTags_expression,

    -- * Destructuring the Response
    SearchTablesByLFTagsResponse (..),
    newSearchTablesByLFTagsResponse,

    -- * Response Lenses
    searchTablesByLFTagsResponse_tableList,
    searchTablesByLFTagsResponse_nextToken,
    searchTablesByLFTagsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.LakeFormation.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchTablesByLFTags' smart constructor.
data SearchTablesByLFTags = SearchTablesByLFTags'
  { -- | The identifier for the Data Catalog. By default, the account ID. The
    -- Data Catalog is the persistent metadata store. It contains database
    -- definitions, table definitions, and other control information to manage
    -- your AWS Lake Formation environment.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | A continuation token, if this is not the first call to retrieve this
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A list of conditions (@LFTag@ structures) to search for in table
    -- resources.
    expression :: Prelude.NonEmpty LFTag
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchTablesByLFTags' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'searchTablesByLFTags_catalogId' - The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your AWS Lake Formation environment.
--
-- 'nextToken', 'searchTablesByLFTags_nextToken' - A continuation token, if this is not the first call to retrieve this
-- list.
--
-- 'maxResults', 'searchTablesByLFTags_maxResults' - The maximum number of results to return.
--
-- 'expression', 'searchTablesByLFTags_expression' - A list of conditions (@LFTag@ structures) to search for in table
-- resources.
newSearchTablesByLFTags ::
  -- | 'expression'
  Prelude.NonEmpty LFTag ->
  SearchTablesByLFTags
newSearchTablesByLFTags pExpression_ =
  SearchTablesByLFTags'
    { catalogId = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      expression = Lens.coerced Lens.# pExpression_
    }

-- | The identifier for the Data Catalog. By default, the account ID. The
-- Data Catalog is the persistent metadata store. It contains database
-- definitions, table definitions, and other control information to manage
-- your AWS Lake Formation environment.
searchTablesByLFTags_catalogId :: Lens.Lens' SearchTablesByLFTags (Prelude.Maybe Prelude.Text)
searchTablesByLFTags_catalogId = Lens.lens (\SearchTablesByLFTags' {catalogId} -> catalogId) (\s@SearchTablesByLFTags' {} a -> s {catalogId = a} :: SearchTablesByLFTags)

-- | A continuation token, if this is not the first call to retrieve this
-- list.
searchTablesByLFTags_nextToken :: Lens.Lens' SearchTablesByLFTags (Prelude.Maybe Prelude.Text)
searchTablesByLFTags_nextToken = Lens.lens (\SearchTablesByLFTags' {nextToken} -> nextToken) (\s@SearchTablesByLFTags' {} a -> s {nextToken = a} :: SearchTablesByLFTags)

-- | The maximum number of results to return.
searchTablesByLFTags_maxResults :: Lens.Lens' SearchTablesByLFTags (Prelude.Maybe Prelude.Natural)
searchTablesByLFTags_maxResults = Lens.lens (\SearchTablesByLFTags' {maxResults} -> maxResults) (\s@SearchTablesByLFTags' {} a -> s {maxResults = a} :: SearchTablesByLFTags)

-- | A list of conditions (@LFTag@ structures) to search for in table
-- resources.
searchTablesByLFTags_expression :: Lens.Lens' SearchTablesByLFTags (Prelude.NonEmpty LFTag)
searchTablesByLFTags_expression = Lens.lens (\SearchTablesByLFTags' {expression} -> expression) (\s@SearchTablesByLFTags' {} a -> s {expression = a} :: SearchTablesByLFTags) Prelude.. Lens.coerced

instance Core.AWSRequest SearchTablesByLFTags where
  type
    AWSResponse SearchTablesByLFTags =
      SearchTablesByLFTagsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchTablesByLFTagsResponse'
            Prelude.<$> (x Core..?> "TableList" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchTablesByLFTags where
  hashWithSalt _salt SearchTablesByLFTags' {..} =
    _salt `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` expression

instance Prelude.NFData SearchTablesByLFTags where
  rnf SearchTablesByLFTags' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf expression

instance Core.ToHeaders SearchTablesByLFTags where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSLakeFormation.SearchTablesByLFTags" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SearchTablesByLFTags where
  toJSON SearchTablesByLFTags' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CatalogId" Core..=) Prelude.<$> catalogId,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("Expression" Core..= expression)
          ]
      )

instance Core.ToPath SearchTablesByLFTags where
  toPath = Prelude.const "/"

instance Core.ToQuery SearchTablesByLFTags where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchTablesByLFTagsResponse' smart constructor.
data SearchTablesByLFTagsResponse = SearchTablesByLFTagsResponse'
  { -- | A list of tables that meet the tag conditions.
    tableList :: Prelude.Maybe [TaggedTable],
    -- | A continuation token, present if the current list segment is not the
    -- last.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchTablesByLFTagsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableList', 'searchTablesByLFTagsResponse_tableList' - A list of tables that meet the tag conditions.
--
-- 'nextToken', 'searchTablesByLFTagsResponse_nextToken' - A continuation token, present if the current list segment is not the
-- last.
--
-- 'httpStatus', 'searchTablesByLFTagsResponse_httpStatus' - The response's http status code.
newSearchTablesByLFTagsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchTablesByLFTagsResponse
newSearchTablesByLFTagsResponse pHttpStatus_ =
  SearchTablesByLFTagsResponse'
    { tableList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of tables that meet the tag conditions.
searchTablesByLFTagsResponse_tableList :: Lens.Lens' SearchTablesByLFTagsResponse (Prelude.Maybe [TaggedTable])
searchTablesByLFTagsResponse_tableList = Lens.lens (\SearchTablesByLFTagsResponse' {tableList} -> tableList) (\s@SearchTablesByLFTagsResponse' {} a -> s {tableList = a} :: SearchTablesByLFTagsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A continuation token, present if the current list segment is not the
-- last.
searchTablesByLFTagsResponse_nextToken :: Lens.Lens' SearchTablesByLFTagsResponse (Prelude.Maybe Prelude.Text)
searchTablesByLFTagsResponse_nextToken = Lens.lens (\SearchTablesByLFTagsResponse' {nextToken} -> nextToken) (\s@SearchTablesByLFTagsResponse' {} a -> s {nextToken = a} :: SearchTablesByLFTagsResponse)

-- | The response's http status code.
searchTablesByLFTagsResponse_httpStatus :: Lens.Lens' SearchTablesByLFTagsResponse Prelude.Int
searchTablesByLFTagsResponse_httpStatus = Lens.lens (\SearchTablesByLFTagsResponse' {httpStatus} -> httpStatus) (\s@SearchTablesByLFTagsResponse' {} a -> s {httpStatus = a} :: SearchTablesByLFTagsResponse)

instance Prelude.NFData SearchTablesByLFTagsResponse where
  rnf SearchTablesByLFTagsResponse' {..} =
    Prelude.rnf tableList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
