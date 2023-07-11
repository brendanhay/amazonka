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
-- Module      : Amazonka.Glue.ListDataQualityRulesets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of rulesets for the specified list of Glue
-- tables.
module Amazonka.Glue.ListDataQualityRulesets
  ( -- * Creating a Request
    ListDataQualityRulesets (..),
    newListDataQualityRulesets,

    -- * Request Lenses
    listDataQualityRulesets_filter,
    listDataQualityRulesets_maxResults,
    listDataQualityRulesets_nextToken,
    listDataQualityRulesets_tags,

    -- * Destructuring the Response
    ListDataQualityRulesetsResponse (..),
    newListDataQualityRulesetsResponse,

    -- * Response Lenses
    listDataQualityRulesetsResponse_nextToken,
    listDataQualityRulesetsResponse_rulesets,
    listDataQualityRulesetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDataQualityRulesets' smart constructor.
data ListDataQualityRulesets = ListDataQualityRulesets'
  { -- | The filter criteria.
    filter' :: Prelude.Maybe DataQualityRulesetFilterCriteria,
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A paginated token to offset the results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of key-value pair tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDataQualityRulesets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'listDataQualityRulesets_filter' - The filter criteria.
--
-- 'maxResults', 'listDataQualityRulesets_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'listDataQualityRulesets_nextToken' - A paginated token to offset the results.
--
-- 'tags', 'listDataQualityRulesets_tags' - A list of key-value pair tags.
newListDataQualityRulesets ::
  ListDataQualityRulesets
newListDataQualityRulesets =
  ListDataQualityRulesets'
    { filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The filter criteria.
listDataQualityRulesets_filter :: Lens.Lens' ListDataQualityRulesets (Prelude.Maybe DataQualityRulesetFilterCriteria)
listDataQualityRulesets_filter = Lens.lens (\ListDataQualityRulesets' {filter'} -> filter') (\s@ListDataQualityRulesets' {} a -> s {filter' = a} :: ListDataQualityRulesets)

-- | The maximum number of results to return.
listDataQualityRulesets_maxResults :: Lens.Lens' ListDataQualityRulesets (Prelude.Maybe Prelude.Natural)
listDataQualityRulesets_maxResults = Lens.lens (\ListDataQualityRulesets' {maxResults} -> maxResults) (\s@ListDataQualityRulesets' {} a -> s {maxResults = a} :: ListDataQualityRulesets)

-- | A paginated token to offset the results.
listDataQualityRulesets_nextToken :: Lens.Lens' ListDataQualityRulesets (Prelude.Maybe Prelude.Text)
listDataQualityRulesets_nextToken = Lens.lens (\ListDataQualityRulesets' {nextToken} -> nextToken) (\s@ListDataQualityRulesets' {} a -> s {nextToken = a} :: ListDataQualityRulesets)

-- | A list of key-value pair tags.
listDataQualityRulesets_tags :: Lens.Lens' ListDataQualityRulesets (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
listDataQualityRulesets_tags = Lens.lens (\ListDataQualityRulesets' {tags} -> tags) (\s@ListDataQualityRulesets' {} a -> s {tags = a} :: ListDataQualityRulesets) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest ListDataQualityRulesets where
  type
    AWSResponse ListDataQualityRulesets =
      ListDataQualityRulesetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDataQualityRulesetsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Rulesets" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDataQualityRulesets where
  hashWithSalt _salt ListDataQualityRulesets' {..} =
    _salt
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` tags

instance Prelude.NFData ListDataQualityRulesets where
  rnf ListDataQualityRulesets' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf tags

instance Data.ToHeaders ListDataQualityRulesets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.ListDataQualityRulesets" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDataQualityRulesets where
  toJSON ListDataQualityRulesets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filter" Data..=) Prelude.<$> filter',
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Tags" Data..=) Prelude.<$> tags
          ]
      )

instance Data.ToPath ListDataQualityRulesets where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDataQualityRulesets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDataQualityRulesetsResponse' smart constructor.
data ListDataQualityRulesetsResponse = ListDataQualityRulesetsResponse'
  { -- | A pagination token, if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A paginated list of rulesets for the specified list of Glue tables.
    rulesets :: Prelude.Maybe [DataQualityRulesetListDetails],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDataQualityRulesetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDataQualityRulesetsResponse_nextToken' - A pagination token, if more results are available.
--
-- 'rulesets', 'listDataQualityRulesetsResponse_rulesets' - A paginated list of rulesets for the specified list of Glue tables.
--
-- 'httpStatus', 'listDataQualityRulesetsResponse_httpStatus' - The response's http status code.
newListDataQualityRulesetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDataQualityRulesetsResponse
newListDataQualityRulesetsResponse pHttpStatus_ =
  ListDataQualityRulesetsResponse'
    { nextToken =
        Prelude.Nothing,
      rulesets = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A pagination token, if more results are available.
listDataQualityRulesetsResponse_nextToken :: Lens.Lens' ListDataQualityRulesetsResponse (Prelude.Maybe Prelude.Text)
listDataQualityRulesetsResponse_nextToken = Lens.lens (\ListDataQualityRulesetsResponse' {nextToken} -> nextToken) (\s@ListDataQualityRulesetsResponse' {} a -> s {nextToken = a} :: ListDataQualityRulesetsResponse)

-- | A paginated list of rulesets for the specified list of Glue tables.
listDataQualityRulesetsResponse_rulesets :: Lens.Lens' ListDataQualityRulesetsResponse (Prelude.Maybe [DataQualityRulesetListDetails])
listDataQualityRulesetsResponse_rulesets = Lens.lens (\ListDataQualityRulesetsResponse' {rulesets} -> rulesets) (\s@ListDataQualityRulesetsResponse' {} a -> s {rulesets = a} :: ListDataQualityRulesetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDataQualityRulesetsResponse_httpStatus :: Lens.Lens' ListDataQualityRulesetsResponse Prelude.Int
listDataQualityRulesetsResponse_httpStatus = Lens.lens (\ListDataQualityRulesetsResponse' {httpStatus} -> httpStatus) (\s@ListDataQualityRulesetsResponse' {} a -> s {httpStatus = a} :: ListDataQualityRulesetsResponse)

instance
  Prelude.NFData
    ListDataQualityRulesetsResponse
  where
  rnf ListDataQualityRulesetsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf rulesets
      `Prelude.seq` Prelude.rnf httpStatus
