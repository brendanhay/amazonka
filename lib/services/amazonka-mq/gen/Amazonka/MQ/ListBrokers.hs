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
-- Module      : Amazonka.MQ.ListBrokers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all brokers.
--
-- This operation returns paginated results.
module Amazonka.MQ.ListBrokers
  ( -- * Creating a Request
    ListBrokers (..),
    newListBrokers,

    -- * Request Lenses
    listBrokers_maxResults,
    listBrokers_nextToken,

    -- * Destructuring the Response
    ListBrokersResponse (..),
    newListBrokersResponse,

    -- * Response Lenses
    listBrokersResponse_brokerSummaries,
    listBrokersResponse_nextToken,
    listBrokersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MQ.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListBrokers' smart constructor.
data ListBrokers = ListBrokers'
  { -- | The maximum number of brokers that Amazon MQ can return per page (20 by
    -- default). This value must be an integer from 5 to 100.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token that specifies the next page of results Amazon MQ should
    -- return. To request the first page, leave nextToken empty.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBrokers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listBrokers_maxResults' - The maximum number of brokers that Amazon MQ can return per page (20 by
-- default). This value must be an integer from 5 to 100.
--
-- 'nextToken', 'listBrokers_nextToken' - The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
newListBrokers ::
  ListBrokers
newListBrokers =
  ListBrokers'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of brokers that Amazon MQ can return per page (20 by
-- default). This value must be an integer from 5 to 100.
listBrokers_maxResults :: Lens.Lens' ListBrokers (Prelude.Maybe Prelude.Natural)
listBrokers_maxResults = Lens.lens (\ListBrokers' {maxResults} -> maxResults) (\s@ListBrokers' {} a -> s {maxResults = a} :: ListBrokers)

-- | The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
listBrokers_nextToken :: Lens.Lens' ListBrokers (Prelude.Maybe Prelude.Text)
listBrokers_nextToken = Lens.lens (\ListBrokers' {nextToken} -> nextToken) (\s@ListBrokers' {} a -> s {nextToken = a} :: ListBrokers)

instance Core.AWSPager ListBrokers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBrokersResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listBrokersResponse_brokerSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listBrokers_nextToken
          Lens..~ rs
          Lens.^? listBrokersResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListBrokers where
  type AWSResponse ListBrokers = ListBrokersResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBrokersResponse'
            Prelude.<$> ( x Data..?> "brokerSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBrokers where
  hashWithSalt _salt ListBrokers' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListBrokers where
  rnf ListBrokers' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListBrokers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListBrokers where
  toPath = Prelude.const "/v1/brokers"

instance Data.ToQuery ListBrokers where
  toQuery ListBrokers' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListBrokersResponse' smart constructor.
data ListBrokersResponse = ListBrokersResponse'
  { -- | A list of information about all brokers.
    brokerSummaries :: Prelude.Maybe [BrokerSummary],
    -- | The token that specifies the next page of results Amazon MQ should
    -- return. To request the first page, leave nextToken empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListBrokersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'brokerSummaries', 'listBrokersResponse_brokerSummaries' - A list of information about all brokers.
--
-- 'nextToken', 'listBrokersResponse_nextToken' - The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
--
-- 'httpStatus', 'listBrokersResponse_httpStatus' - The response's http status code.
newListBrokersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBrokersResponse
newListBrokersResponse pHttpStatus_ =
  ListBrokersResponse'
    { brokerSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of information about all brokers.
listBrokersResponse_brokerSummaries :: Lens.Lens' ListBrokersResponse (Prelude.Maybe [BrokerSummary])
listBrokersResponse_brokerSummaries = Lens.lens (\ListBrokersResponse' {brokerSummaries} -> brokerSummaries) (\s@ListBrokersResponse' {} a -> s {brokerSummaries = a} :: ListBrokersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
listBrokersResponse_nextToken :: Lens.Lens' ListBrokersResponse (Prelude.Maybe Prelude.Text)
listBrokersResponse_nextToken = Lens.lens (\ListBrokersResponse' {nextToken} -> nextToken) (\s@ListBrokersResponse' {} a -> s {nextToken = a} :: ListBrokersResponse)

-- | The response's http status code.
listBrokersResponse_httpStatus :: Lens.Lens' ListBrokersResponse Prelude.Int
listBrokersResponse_httpStatus = Lens.lens (\ListBrokersResponse' {httpStatus} -> httpStatus) (\s@ListBrokersResponse' {} a -> s {httpStatus = a} :: ListBrokersResponse)

instance Prelude.NFData ListBrokersResponse where
  rnf ListBrokersResponse' {..} =
    Prelude.rnf brokerSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
