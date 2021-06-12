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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListBrokers' smart constructor.
data ListBrokers = ListBrokers'
  { -- | The token that specifies the next page of results Amazon MQ should
    -- return. To request the first page, leave nextToken empty.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of brokers that Amazon MQ can return per page (20 by
    -- default). This value must be an integer from 5 to 100.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
listBrokers_nextToken :: Lens.Lens' ListBrokers (Core.Maybe Core.Text)
listBrokers_nextToken = Lens.lens (\ListBrokers' {nextToken} -> nextToken) (\s@ListBrokers' {} a -> s {nextToken = a} :: ListBrokers)

-- | The maximum number of brokers that Amazon MQ can return per page (20 by
-- default). This value must be an integer from 5 to 100.
listBrokers_maxResults :: Lens.Lens' ListBrokers (Core.Maybe Core.Natural)
listBrokers_maxResults = Lens.lens (\ListBrokers' {maxResults} -> maxResults) (\s@ListBrokers' {} a -> s {maxResults = a} :: ListBrokers)

instance Core.AWSPager ListBrokers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listBrokersResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listBrokersResponse_brokerSummaries
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listBrokers_nextToken
          Lens..~ rs
          Lens.^? listBrokersResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListBrokers where
  type AWSResponse ListBrokers = ListBrokersResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBrokersResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "brokerSummaries" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListBrokers

instance Core.NFData ListBrokers

instance Core.ToHeaders ListBrokers where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListBrokers where
  toPath = Core.const "/v1/brokers"

instance Core.ToQuery ListBrokers where
  toQuery ListBrokers' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListBrokersResponse' smart constructor.
data ListBrokersResponse = ListBrokersResponse'
  { -- | The token that specifies the next page of results Amazon MQ should
    -- return. To request the first page, leave nextToken empty.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of information about all brokers.
    brokerSummaries :: Core.Maybe [BrokerSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListBrokersResponse
newListBrokersResponse pHttpStatus_ =
  ListBrokersResponse'
    { nextToken = Core.Nothing,
      brokerSummaries = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token that specifies the next page of results Amazon MQ should
-- return. To request the first page, leave nextToken empty.
listBrokersResponse_nextToken :: Lens.Lens' ListBrokersResponse (Core.Maybe Core.Text)
listBrokersResponse_nextToken = Lens.lens (\ListBrokersResponse' {nextToken} -> nextToken) (\s@ListBrokersResponse' {} a -> s {nextToken = a} :: ListBrokersResponse)

-- | A list of information about all brokers.
listBrokersResponse_brokerSummaries :: Lens.Lens' ListBrokersResponse (Core.Maybe [BrokerSummary])
listBrokersResponse_brokerSummaries = Lens.lens (\ListBrokersResponse' {brokerSummaries} -> brokerSummaries) (\s@ListBrokersResponse' {} a -> s {brokerSummaries = a} :: ListBrokersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listBrokersResponse_httpStatus :: Lens.Lens' ListBrokersResponse Core.Int
listBrokersResponse_httpStatus = Lens.lens (\ListBrokersResponse' {httpStatus} -> httpStatus) (\s@ListBrokersResponse' {} a -> s {httpStatus = a} :: ListBrokersResponse)

instance Core.NFData ListBrokersResponse
