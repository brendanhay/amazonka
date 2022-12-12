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
-- Module      : Amazonka.ResourceExplorer2.ListIndexes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of all of the indexes in Amazon Web Services Regions
-- that are currently collecting resource information for Amazon Web
-- Services Resource Explorer.
--
-- This operation returns paginated results.
module Amazonka.ResourceExplorer2.ListIndexes
  ( -- * Creating a Request
    ListIndexes (..),
    newListIndexes,

    -- * Request Lenses
    listIndexes_maxResults,
    listIndexes_nextToken,
    listIndexes_regions,
    listIndexes_type,

    -- * Destructuring the Response
    ListIndexesResponse (..),
    newListIndexesResponse,

    -- * Response Lenses
    listIndexesResponse_indexes,
    listIndexesResponse_nextToken,
    listIndexesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResourceExplorer2.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newListIndexes' smart constructor.
data ListIndexes = ListIndexes'
  { -- | The maximum number of results that you want included on each page of the
    -- response. If you do not include this parameter, it defaults to a value
    -- appropriate to the operation. If additional items exist beyond those
    -- included in the current response, the @NextToken@ response element is
    -- present and has a value (is not null). Include that value as the
    -- @NextToken@ request parameter in the next call to the operation to get
    -- the next part of the results.
    --
    -- An API operation can return fewer results than the maximum even when
    -- there are more results available. You should check @NextToken@ after
    -- every operation to ensure that you receive all of the results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The parameter for receiving additional results if you receive a
    -- @NextToken@ response in a previous request. A @NextToken@ response
    -- indicates that more output is available. Set this parameter to the value
    -- of the previous call\'s @NextToken@ response to indicate where the
    -- output should continue from.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | If specified, limits the response to only information about the index in
    -- the specified list of Amazon Web Services Regions.
    regions :: Prelude.Maybe [Prelude.Text],
    -- | If specified, limits the output to only indexes of the specified Type,
    -- either @LOCAL@ or @AGGREGATOR@.
    --
    -- Use this option to discover the aggregator index for your account.
    type' :: Prelude.Maybe IndexType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIndexes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listIndexes_maxResults' - The maximum number of results that you want included on each page of the
-- response. If you do not include this parameter, it defaults to a value
-- appropriate to the operation. If additional items exist beyond those
-- included in the current response, the @NextToken@ response element is
-- present and has a value (is not null). Include that value as the
-- @NextToken@ request parameter in the next call to the operation to get
-- the next part of the results.
--
-- An API operation can return fewer results than the maximum even when
-- there are more results available. You should check @NextToken@ after
-- every operation to ensure that you receive all of the results.
--
-- 'nextToken', 'listIndexes_nextToken' - The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
--
-- 'regions', 'listIndexes_regions' - If specified, limits the response to only information about the index in
-- the specified list of Amazon Web Services Regions.
--
-- 'type'', 'listIndexes_type' - If specified, limits the output to only indexes of the specified Type,
-- either @LOCAL@ or @AGGREGATOR@.
--
-- Use this option to discover the aggregator index for your account.
newListIndexes ::
  ListIndexes
newListIndexes =
  ListIndexes'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      regions = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The maximum number of results that you want included on each page of the
-- response. If you do not include this parameter, it defaults to a value
-- appropriate to the operation. If additional items exist beyond those
-- included in the current response, the @NextToken@ response element is
-- present and has a value (is not null). Include that value as the
-- @NextToken@ request parameter in the next call to the operation to get
-- the next part of the results.
--
-- An API operation can return fewer results than the maximum even when
-- there are more results available. You should check @NextToken@ after
-- every operation to ensure that you receive all of the results.
listIndexes_maxResults :: Lens.Lens' ListIndexes (Prelude.Maybe Prelude.Natural)
listIndexes_maxResults = Lens.lens (\ListIndexes' {maxResults} -> maxResults) (\s@ListIndexes' {} a -> s {maxResults = a} :: ListIndexes)

-- | The parameter for receiving additional results if you receive a
-- @NextToken@ response in a previous request. A @NextToken@ response
-- indicates that more output is available. Set this parameter to the value
-- of the previous call\'s @NextToken@ response to indicate where the
-- output should continue from.
listIndexes_nextToken :: Lens.Lens' ListIndexes (Prelude.Maybe Prelude.Text)
listIndexes_nextToken = Lens.lens (\ListIndexes' {nextToken} -> nextToken) (\s@ListIndexes' {} a -> s {nextToken = a} :: ListIndexes)

-- | If specified, limits the response to only information about the index in
-- the specified list of Amazon Web Services Regions.
listIndexes_regions :: Lens.Lens' ListIndexes (Prelude.Maybe [Prelude.Text])
listIndexes_regions = Lens.lens (\ListIndexes' {regions} -> regions) (\s@ListIndexes' {} a -> s {regions = a} :: ListIndexes) Prelude.. Lens.mapping Lens.coerced

-- | If specified, limits the output to only indexes of the specified Type,
-- either @LOCAL@ or @AGGREGATOR@.
--
-- Use this option to discover the aggregator index for your account.
listIndexes_type :: Lens.Lens' ListIndexes (Prelude.Maybe IndexType)
listIndexes_type = Lens.lens (\ListIndexes' {type'} -> type') (\s@ListIndexes' {} a -> s {type' = a} :: ListIndexes)

instance Core.AWSPager ListIndexes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listIndexesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listIndexesResponse_indexes Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listIndexes_nextToken
          Lens..~ rs
          Lens.^? listIndexesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListIndexes where
  type AWSResponse ListIndexes = ListIndexesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListIndexesResponse'
            Prelude.<$> (x Data..?> "Indexes" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListIndexes where
  hashWithSalt _salt ListIndexes' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` regions
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ListIndexes where
  rnf ListIndexes' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf regions
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders ListIndexes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListIndexes where
  toJSON ListIndexes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Regions" Data..=) Prelude.<$> regions,
            ("Type" Data..=) Prelude.<$> type'
          ]
      )

instance Data.ToPath ListIndexes where
  toPath = Prelude.const "/ListIndexes"

instance Data.ToQuery ListIndexes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListIndexesResponse' smart constructor.
data ListIndexesResponse = ListIndexesResponse'
  { -- | A structure that contains the details and status of each index.
    indexes :: Prelude.Maybe [Index],
    -- | If present, indicates that more output is available than is included in
    -- the current response. Use this value in the @NextToken@ request
    -- parameter in a subsequent call to the operation to get the next part of
    -- the output. You should repeat this until the @NextToken@ response
    -- element comes back as @null@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListIndexesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexes', 'listIndexesResponse_indexes' - A structure that contains the details and status of each index.
--
-- 'nextToken', 'listIndexesResponse_nextToken' - If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
--
-- 'httpStatus', 'listIndexesResponse_httpStatus' - The response's http status code.
newListIndexesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListIndexesResponse
newListIndexesResponse pHttpStatus_ =
  ListIndexesResponse'
    { indexes = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A structure that contains the details and status of each index.
listIndexesResponse_indexes :: Lens.Lens' ListIndexesResponse (Prelude.Maybe [Index])
listIndexesResponse_indexes = Lens.lens (\ListIndexesResponse' {indexes} -> indexes) (\s@ListIndexesResponse' {} a -> s {indexes = a} :: ListIndexesResponse) Prelude.. Lens.mapping Lens.coerced

-- | If present, indicates that more output is available than is included in
-- the current response. Use this value in the @NextToken@ request
-- parameter in a subsequent call to the operation to get the next part of
-- the output. You should repeat this until the @NextToken@ response
-- element comes back as @null@.
listIndexesResponse_nextToken :: Lens.Lens' ListIndexesResponse (Prelude.Maybe Prelude.Text)
listIndexesResponse_nextToken = Lens.lens (\ListIndexesResponse' {nextToken} -> nextToken) (\s@ListIndexesResponse' {} a -> s {nextToken = a} :: ListIndexesResponse)

-- | The response's http status code.
listIndexesResponse_httpStatus :: Lens.Lens' ListIndexesResponse Prelude.Int
listIndexesResponse_httpStatus = Lens.lens (\ListIndexesResponse' {httpStatus} -> httpStatus) (\s@ListIndexesResponse' {} a -> s {httpStatus = a} :: ListIndexesResponse)

instance Prelude.NFData ListIndexesResponse where
  rnf ListIndexesResponse' {..} =
    Prelude.rnf indexes
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
