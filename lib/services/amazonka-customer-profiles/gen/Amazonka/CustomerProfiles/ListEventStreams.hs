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
-- Module      : Amazonka.CustomerProfiles.ListEventStreams
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all the event streams in a specific domain.
--
-- This operation returns paginated results.
module Amazonka.CustomerProfiles.ListEventStreams
  ( -- * Creating a Request
    ListEventStreams (..),
    newListEventStreams,

    -- * Request Lenses
    listEventStreams_maxResults,
    listEventStreams_nextToken,
    listEventStreams_domainName,

    -- * Destructuring the Response
    ListEventStreamsResponse (..),
    newListEventStreamsResponse,

    -- * Response Lenses
    listEventStreamsResponse_items,
    listEventStreamsResponse_nextToken,
    listEventStreamsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEventStreams' smart constructor.
data ListEventStreams = ListEventStreams'
  { -- | The maximum number of objects returned per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique name of the domain.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEventStreams' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listEventStreams_maxResults' - The maximum number of objects returned per page.
--
-- 'nextToken', 'listEventStreams_nextToken' - Identifies the next page of results to return.
--
-- 'domainName', 'listEventStreams_domainName' - The unique name of the domain.
newListEventStreams ::
  -- | 'domainName'
  Prelude.Text ->
  ListEventStreams
newListEventStreams pDomainName_ =
  ListEventStreams'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | The maximum number of objects returned per page.
listEventStreams_maxResults :: Lens.Lens' ListEventStreams (Prelude.Maybe Prelude.Natural)
listEventStreams_maxResults = Lens.lens (\ListEventStreams' {maxResults} -> maxResults) (\s@ListEventStreams' {} a -> s {maxResults = a} :: ListEventStreams)

-- | Identifies the next page of results to return.
listEventStreams_nextToken :: Lens.Lens' ListEventStreams (Prelude.Maybe Prelude.Text)
listEventStreams_nextToken = Lens.lens (\ListEventStreams' {nextToken} -> nextToken) (\s@ListEventStreams' {} a -> s {nextToken = a} :: ListEventStreams)

-- | The unique name of the domain.
listEventStreams_domainName :: Lens.Lens' ListEventStreams Prelude.Text
listEventStreams_domainName = Lens.lens (\ListEventStreams' {domainName} -> domainName) (\s@ListEventStreams' {} a -> s {domainName = a} :: ListEventStreams)

instance Core.AWSPager ListEventStreams where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEventStreamsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listEventStreamsResponse_items
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listEventStreams_nextToken
          Lens..~ rs
          Lens.^? listEventStreamsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListEventStreams where
  type
    AWSResponse ListEventStreams =
      ListEventStreamsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEventStreamsResponse'
            Prelude.<$> (x Data..?> "Items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEventStreams where
  hashWithSalt _salt ListEventStreams' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData ListEventStreams where
  rnf ListEventStreams' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders ListEventStreams where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListEventStreams where
  toPath ListEventStreams' {..} =
    Prelude.mconcat
      ["/domains/", Data.toBS domainName, "/event-streams"]

instance Data.ToQuery ListEventStreams where
  toQuery ListEventStreams' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newListEventStreamsResponse' smart constructor.
data ListEventStreamsResponse = ListEventStreamsResponse'
  { -- | Contains summary information about an EventStream.
    items :: Prelude.Maybe [EventStreamSummary],
    -- | Identifies the next page of results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEventStreamsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'listEventStreamsResponse_items' - Contains summary information about an EventStream.
--
-- 'nextToken', 'listEventStreamsResponse_nextToken' - Identifies the next page of results to return.
--
-- 'httpStatus', 'listEventStreamsResponse_httpStatus' - The response's http status code.
newListEventStreamsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEventStreamsResponse
newListEventStreamsResponse pHttpStatus_ =
  ListEventStreamsResponse'
    { items = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains summary information about an EventStream.
listEventStreamsResponse_items :: Lens.Lens' ListEventStreamsResponse (Prelude.Maybe [EventStreamSummary])
listEventStreamsResponse_items = Lens.lens (\ListEventStreamsResponse' {items} -> items) (\s@ListEventStreamsResponse' {} a -> s {items = a} :: ListEventStreamsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Identifies the next page of results to return.
listEventStreamsResponse_nextToken :: Lens.Lens' ListEventStreamsResponse (Prelude.Maybe Prelude.Text)
listEventStreamsResponse_nextToken = Lens.lens (\ListEventStreamsResponse' {nextToken} -> nextToken) (\s@ListEventStreamsResponse' {} a -> s {nextToken = a} :: ListEventStreamsResponse)

-- | The response's http status code.
listEventStreamsResponse_httpStatus :: Lens.Lens' ListEventStreamsResponse Prelude.Int
listEventStreamsResponse_httpStatus = Lens.lens (\ListEventStreamsResponse' {httpStatus} -> httpStatus) (\s@ListEventStreamsResponse' {} a -> s {httpStatus = a} :: ListEventStreamsResponse)

instance Prelude.NFData ListEventStreamsResponse where
  rnf ListEventStreamsResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
