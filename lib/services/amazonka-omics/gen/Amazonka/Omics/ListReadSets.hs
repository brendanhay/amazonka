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
-- Module      : Amazonka.Omics.ListReadSets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of read sets.
--
-- This operation returns paginated results.
module Amazonka.Omics.ListReadSets
  ( -- * Creating a Request
    ListReadSets (..),
    newListReadSets,

    -- * Request Lenses
    listReadSets_filter,
    listReadSets_maxResults,
    listReadSets_nextToken,
    listReadSets_sequenceStoreId,

    -- * Destructuring the Response
    ListReadSetsResponse (..),
    newListReadSetsResponse,

    -- * Response Lenses
    listReadSetsResponse_nextToken,
    listReadSetsResponse_httpStatus,
    listReadSetsResponse_readSets,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListReadSets' smart constructor.
data ListReadSets = ListReadSets'
  { -- | A filter to apply to the list.
    filter' :: Prelude.Maybe ReadSetFilter,
    -- | The maximum number of read sets to return in one page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The jobs\' sequence store ID.
    sequenceStoreId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReadSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'listReadSets_filter' - A filter to apply to the list.
--
-- 'maxResults', 'listReadSets_maxResults' - The maximum number of read sets to return in one page of results.
--
-- 'nextToken', 'listReadSets_nextToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
--
-- 'sequenceStoreId', 'listReadSets_sequenceStoreId' - The jobs\' sequence store ID.
newListReadSets ::
  -- | 'sequenceStoreId'
  Prelude.Text ->
  ListReadSets
newListReadSets pSequenceStoreId_ =
  ListReadSets'
    { filter' = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sequenceStoreId = pSequenceStoreId_
    }

-- | A filter to apply to the list.
listReadSets_filter :: Lens.Lens' ListReadSets (Prelude.Maybe ReadSetFilter)
listReadSets_filter = Lens.lens (\ListReadSets' {filter'} -> filter') (\s@ListReadSets' {} a -> s {filter' = a} :: ListReadSets)

-- | The maximum number of read sets to return in one page of results.
listReadSets_maxResults :: Lens.Lens' ListReadSets (Prelude.Maybe Prelude.Natural)
listReadSets_maxResults = Lens.lens (\ListReadSets' {maxResults} -> maxResults) (\s@ListReadSets' {} a -> s {maxResults = a} :: ListReadSets)

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
listReadSets_nextToken :: Lens.Lens' ListReadSets (Prelude.Maybe Prelude.Text)
listReadSets_nextToken = Lens.lens (\ListReadSets' {nextToken} -> nextToken) (\s@ListReadSets' {} a -> s {nextToken = a} :: ListReadSets)

-- | The jobs\' sequence store ID.
listReadSets_sequenceStoreId :: Lens.Lens' ListReadSets Prelude.Text
listReadSets_sequenceStoreId = Lens.lens (\ListReadSets' {sequenceStoreId} -> sequenceStoreId) (\s@ListReadSets' {} a -> s {sequenceStoreId = a} :: ListReadSets)

instance Core.AWSPager ListReadSets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listReadSetsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. listReadSetsResponse_readSets) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listReadSets_nextToken
          Lens..~ rs
          Lens.^? listReadSetsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListReadSets where
  type AWSResponse ListReadSets = ListReadSetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReadSetsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "readSets" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListReadSets where
  hashWithSalt _salt ListReadSets' {..} =
    _salt
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sequenceStoreId

instance Prelude.NFData ListReadSets where
  rnf ListReadSets' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sequenceStoreId

instance Data.ToHeaders ListReadSets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListReadSets where
  toJSON ListReadSets' {..} =
    Data.object
      ( Prelude.catMaybes
          [("filter" Data..=) Prelude.<$> filter']
      )

instance Data.ToPath ListReadSets where
  toPath ListReadSets' {..} =
    Prelude.mconcat
      [ "/sequencestore/",
        Data.toBS sequenceStoreId,
        "/readsets"
      ]

instance Data.ToQuery ListReadSets where
  toQuery ListReadSets' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListReadSetsResponse' smart constructor.
data ListReadSetsResponse = ListReadSetsResponse'
  { -- | A pagination token that\'s included if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of read sets.
    readSets :: [ReadSetListItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReadSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listReadSetsResponse_nextToken' - A pagination token that\'s included if more results are available.
--
-- 'httpStatus', 'listReadSetsResponse_httpStatus' - The response's http status code.
--
-- 'readSets', 'listReadSetsResponse_readSets' - A list of read sets.
newListReadSetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListReadSetsResponse
newListReadSetsResponse pHttpStatus_ =
  ListReadSetsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      readSets = Prelude.mempty
    }

-- | A pagination token that\'s included if more results are available.
listReadSetsResponse_nextToken :: Lens.Lens' ListReadSetsResponse (Prelude.Maybe Prelude.Text)
listReadSetsResponse_nextToken = Lens.lens (\ListReadSetsResponse' {nextToken} -> nextToken) (\s@ListReadSetsResponse' {} a -> s {nextToken = a} :: ListReadSetsResponse)

-- | The response's http status code.
listReadSetsResponse_httpStatus :: Lens.Lens' ListReadSetsResponse Prelude.Int
listReadSetsResponse_httpStatus = Lens.lens (\ListReadSetsResponse' {httpStatus} -> httpStatus) (\s@ListReadSetsResponse' {} a -> s {httpStatus = a} :: ListReadSetsResponse)

-- | A list of read sets.
listReadSetsResponse_readSets :: Lens.Lens' ListReadSetsResponse [ReadSetListItem]
listReadSetsResponse_readSets = Lens.lens (\ListReadSetsResponse' {readSets} -> readSets) (\s@ListReadSetsResponse' {} a -> s {readSets = a} :: ListReadSetsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListReadSetsResponse where
  rnf ListReadSetsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf readSets
