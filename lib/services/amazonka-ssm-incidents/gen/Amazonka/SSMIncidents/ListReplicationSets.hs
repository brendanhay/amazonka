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
-- Module      : Amazonka.SSMIncidents.ListReplicationSets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists details about the replication set configured in your account.
--
-- This operation returns paginated results.
module Amazonka.SSMIncidents.ListReplicationSets
  ( -- * Creating a Request
    ListReplicationSets (..),
    newListReplicationSets,

    -- * Request Lenses
    listReplicationSets_maxResults,
    listReplicationSets_nextToken,

    -- * Destructuring the Response
    ListReplicationSetsResponse (..),
    newListReplicationSetsResponse,

    -- * Response Lenses
    listReplicationSetsResponse_nextToken,
    listReplicationSetsResponse_httpStatus,
    listReplicationSetsResponse_replicationSetArns,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMIncidents.Types

-- | /See:/ 'newListReplicationSets' smart constructor.
data ListReplicationSets = ListReplicationSets'
  { -- | The maximum number of results per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The pagination token to continue to the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReplicationSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listReplicationSets_maxResults' - The maximum number of results per page.
--
-- 'nextToken', 'listReplicationSets_nextToken' - The pagination token to continue to the next page of results.
newListReplicationSets ::
  ListReplicationSets
newListReplicationSets =
  ListReplicationSets'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results per page.
listReplicationSets_maxResults :: Lens.Lens' ListReplicationSets (Prelude.Maybe Prelude.Natural)
listReplicationSets_maxResults = Lens.lens (\ListReplicationSets' {maxResults} -> maxResults) (\s@ListReplicationSets' {} a -> s {maxResults = a} :: ListReplicationSets)

-- | The pagination token to continue to the next page of results.
listReplicationSets_nextToken :: Lens.Lens' ListReplicationSets (Prelude.Maybe Prelude.Text)
listReplicationSets_nextToken = Lens.lens (\ListReplicationSets' {nextToken} -> nextToken) (\s@ListReplicationSets' {} a -> s {nextToken = a} :: ListReplicationSets)

instance Core.AWSPager ListReplicationSets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listReplicationSetsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listReplicationSetsResponse_replicationSetArns
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listReplicationSets_nextToken
          Lens..~ rs
          Lens.^? listReplicationSetsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListReplicationSets where
  type
    AWSResponse ListReplicationSets =
      ListReplicationSetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReplicationSetsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "replicationSetArns"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListReplicationSets where
  hashWithSalt _salt ListReplicationSets' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListReplicationSets where
  rnf ListReplicationSets' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListReplicationSets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListReplicationSets where
  toJSON ListReplicationSets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListReplicationSets where
  toPath = Prelude.const "/listReplicationSets"

instance Data.ToQuery ListReplicationSets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListReplicationSetsResponse' smart constructor.
data ListReplicationSetsResponse = ListReplicationSetsResponse'
  { -- | The pagination token to continue to the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the list replication set.
    replicationSetArns :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReplicationSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listReplicationSetsResponse_nextToken' - The pagination token to continue to the next page of results.
--
-- 'httpStatus', 'listReplicationSetsResponse_httpStatus' - The response's http status code.
--
-- 'replicationSetArns', 'listReplicationSetsResponse_replicationSetArns' - The Amazon Resource Name (ARN) of the list replication set.
newListReplicationSetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListReplicationSetsResponse
newListReplicationSetsResponse pHttpStatus_ =
  ListReplicationSetsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      replicationSetArns = Prelude.mempty
    }

-- | The pagination token to continue to the next page of results.
listReplicationSetsResponse_nextToken :: Lens.Lens' ListReplicationSetsResponse (Prelude.Maybe Prelude.Text)
listReplicationSetsResponse_nextToken = Lens.lens (\ListReplicationSetsResponse' {nextToken} -> nextToken) (\s@ListReplicationSetsResponse' {} a -> s {nextToken = a} :: ListReplicationSetsResponse)

-- | The response's http status code.
listReplicationSetsResponse_httpStatus :: Lens.Lens' ListReplicationSetsResponse Prelude.Int
listReplicationSetsResponse_httpStatus = Lens.lens (\ListReplicationSetsResponse' {httpStatus} -> httpStatus) (\s@ListReplicationSetsResponse' {} a -> s {httpStatus = a} :: ListReplicationSetsResponse)

-- | The Amazon Resource Name (ARN) of the list replication set.
listReplicationSetsResponse_replicationSetArns :: Lens.Lens' ListReplicationSetsResponse [Prelude.Text]
listReplicationSetsResponse_replicationSetArns = Lens.lens (\ListReplicationSetsResponse' {replicationSetArns} -> replicationSetArns) (\s@ListReplicationSetsResponse' {} a -> s {replicationSetArns = a} :: ListReplicationSetsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListReplicationSetsResponse where
  rnf ListReplicationSetsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf replicationSetArns
