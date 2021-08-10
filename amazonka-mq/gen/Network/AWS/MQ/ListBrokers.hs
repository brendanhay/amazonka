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
-- Module      : Network.AWS.MQ.ListBrokers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all brokers.
--
-- This operation returns paginated results.
module Network.AWS.MQ.ListBrokers
  ( -- * Creating a Request
    ListBrokers (..),
    newListBrokers,

    -- * Request Lenses
    listBrokers_nextToken,
    listBrokers_maxResults,

    -- * Destructuring the Response
    ListBrokersResponse (..),
    newListBrokersResponse,

    -- * Response Lenses
    listBrokersResponse_nextToken,
    listBrokersResponse_brokerSummaries,
    listBrokersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListBrokers' smart constructor.
data ListBrokers = ListBrokers'
  { -- | The token that specifies the next page of results Amazon MQ should
    -- return. To request the first page, leave nextToken empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of brokers that Amazon MQ can return per page (20 by
    -- default). This value must be an integer from 5 to 100.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'nextToken', 'listBrokers_nextToken' - The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
--
-- 'maxResults', 'listBrokers_maxResults' - The maximum number of brokers that Amazon MQ can return per page (20 by
-- default). This value must be an integer from 5 to 100.
newListBrokers ::
  ListBrokers
newListBrokers =
  ListBrokers'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
listBrokers_nextToken :: Lens.Lens' ListBrokers (Prelude.Maybe Prelude.Text)
listBrokers_nextToken = Lens.lens (\ListBrokers' {nextToken} -> nextToken) (\s@ListBrokers' {} a -> s {nextToken = a} :: ListBrokers)

-- | The maximum number of brokers that Amazon MQ can return per page (20 by
-- default). This value must be an integer from 5 to 100.
listBrokers_maxResults :: Lens.Lens' ListBrokers (Prelude.Maybe Prelude.Natural)
listBrokers_maxResults = Lens.lens (\ListBrokers' {maxResults} -> maxResults) (\s@ListBrokers' {} a -> s {maxResults = a} :: ListBrokers)

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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBrokersResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "brokerSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListBrokers

instance Prelude.NFData ListBrokers

instance Core.ToHeaders ListBrokers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListBrokers where
  toPath = Prelude.const "/v1/brokers"

instance Core.ToQuery ListBrokers where
  toQuery ListBrokers' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListBrokersResponse' smart constructor.
data ListBrokersResponse = ListBrokersResponse'
  { -- | The token that specifies the next page of results Amazon MQ should
    -- return. To request the first page, leave nextToken empty.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of information about all brokers.
    brokerSummaries :: Prelude.Maybe [BrokerSummary],
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
-- 'nextToken', 'listBrokersResponse_nextToken' - The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
--
-- 'brokerSummaries', 'listBrokersResponse_brokerSummaries' - A list of information about all brokers.
--
-- 'httpStatus', 'listBrokersResponse_httpStatus' - The response's http status code.
newListBrokersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListBrokersResponse
newListBrokersResponse pHttpStatus_ =
  ListBrokersResponse'
    { nextToken = Prelude.Nothing,
      brokerSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
listBrokersResponse_nextToken :: Lens.Lens' ListBrokersResponse (Prelude.Maybe Prelude.Text)
listBrokersResponse_nextToken = Lens.lens (\ListBrokersResponse' {nextToken} -> nextToken) (\s@ListBrokersResponse' {} a -> s {nextToken = a} :: ListBrokersResponse)

-- | A list of information about all brokers.
listBrokersResponse_brokerSummaries :: Lens.Lens' ListBrokersResponse (Prelude.Maybe [BrokerSummary])
listBrokersResponse_brokerSummaries = Lens.lens (\ListBrokersResponse' {brokerSummaries} -> brokerSummaries) (\s@ListBrokersResponse' {} a -> s {brokerSummaries = a} :: ListBrokersResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listBrokersResponse_httpStatus :: Lens.Lens' ListBrokersResponse Prelude.Int
listBrokersResponse_httpStatus = Lens.lens (\ListBrokersResponse' {httpStatus} -> httpStatus) (\s@ListBrokersResponse' {} a -> s {httpStatus = a} :: ListBrokersResponse)

instance Prelude.NFData ListBrokersResponse
