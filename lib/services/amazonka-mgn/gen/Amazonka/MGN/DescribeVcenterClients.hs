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
-- Module      : Amazonka.MGN.DescribeVcenterClients
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the installed vCenter clients.
--
-- This operation returns paginated results.
module Amazonka.MGN.DescribeVcenterClients
  ( -- * Creating a Request
    DescribeVcenterClients (..),
    newDescribeVcenterClients,

    -- * Request Lenses
    describeVcenterClients_maxResults,
    describeVcenterClients_nextToken,

    -- * Destructuring the Response
    DescribeVcenterClientsResponse (..),
    newDescribeVcenterClientsResponse,

    -- * Response Lenses
    describeVcenterClientsResponse_items,
    describeVcenterClientsResponse_nextToken,
    describeVcenterClientsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeVcenterClients' smart constructor.
data DescribeVcenterClients = DescribeVcenterClients'
  { -- | Maximum results to be returned in DescribeVcenterClients.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Next pagination token to be provided for DescribeVcenterClients.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVcenterClients' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'describeVcenterClients_maxResults' - Maximum results to be returned in DescribeVcenterClients.
--
-- 'nextToken', 'describeVcenterClients_nextToken' - Next pagination token to be provided for DescribeVcenterClients.
newDescribeVcenterClients ::
  DescribeVcenterClients
newDescribeVcenterClients =
  DescribeVcenterClients'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Maximum results to be returned in DescribeVcenterClients.
describeVcenterClients_maxResults :: Lens.Lens' DescribeVcenterClients (Prelude.Maybe Prelude.Natural)
describeVcenterClients_maxResults = Lens.lens (\DescribeVcenterClients' {maxResults} -> maxResults) (\s@DescribeVcenterClients' {} a -> s {maxResults = a} :: DescribeVcenterClients)

-- | Next pagination token to be provided for DescribeVcenterClients.
describeVcenterClients_nextToken :: Lens.Lens' DescribeVcenterClients (Prelude.Maybe Prelude.Text)
describeVcenterClients_nextToken = Lens.lens (\DescribeVcenterClients' {nextToken} -> nextToken) (\s@DescribeVcenterClients' {} a -> s {nextToken = a} :: DescribeVcenterClients)

instance Core.AWSPager DescribeVcenterClients where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeVcenterClientsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeVcenterClientsResponse_items
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeVcenterClients_nextToken
          Lens..~ rs
          Lens.^? describeVcenterClientsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeVcenterClients where
  type
    AWSResponse DescribeVcenterClients =
      DescribeVcenterClientsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeVcenterClientsResponse'
            Prelude.<$> (x Data..?> "items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeVcenterClients where
  hashWithSalt _salt DescribeVcenterClients' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeVcenterClients where
  rnf DescribeVcenterClients' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeVcenterClients where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeVcenterClients where
  toPath = Prelude.const "/DescribeVcenterClients"

instance Data.ToQuery DescribeVcenterClients where
  toQuery DescribeVcenterClients' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeVcenterClientsResponse' smart constructor.
data DescribeVcenterClientsResponse = DescribeVcenterClientsResponse'
  { -- | List of items returned by DescribeVcenterClients.
    items :: Prelude.Maybe [VcenterClient],
    -- | Next pagination token returned from DescribeVcenterClients.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVcenterClientsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'describeVcenterClientsResponse_items' - List of items returned by DescribeVcenterClients.
--
-- 'nextToken', 'describeVcenterClientsResponse_nextToken' - Next pagination token returned from DescribeVcenterClients.
--
-- 'httpStatus', 'describeVcenterClientsResponse_httpStatus' - The response's http status code.
newDescribeVcenterClientsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeVcenterClientsResponse
newDescribeVcenterClientsResponse pHttpStatus_ =
  DescribeVcenterClientsResponse'
    { items =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of items returned by DescribeVcenterClients.
describeVcenterClientsResponse_items :: Lens.Lens' DescribeVcenterClientsResponse (Prelude.Maybe [VcenterClient])
describeVcenterClientsResponse_items = Lens.lens (\DescribeVcenterClientsResponse' {items} -> items) (\s@DescribeVcenterClientsResponse' {} a -> s {items = a} :: DescribeVcenterClientsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Next pagination token returned from DescribeVcenterClients.
describeVcenterClientsResponse_nextToken :: Lens.Lens' DescribeVcenterClientsResponse (Prelude.Maybe Prelude.Text)
describeVcenterClientsResponse_nextToken = Lens.lens (\DescribeVcenterClientsResponse' {nextToken} -> nextToken) (\s@DescribeVcenterClientsResponse' {} a -> s {nextToken = a} :: DescribeVcenterClientsResponse)

-- | The response's http status code.
describeVcenterClientsResponse_httpStatus :: Lens.Lens' DescribeVcenterClientsResponse Prelude.Int
describeVcenterClientsResponse_httpStatus = Lens.lens (\DescribeVcenterClientsResponse' {httpStatus} -> httpStatus) (\s@DescribeVcenterClientsResponse' {} a -> s {httpStatus = a} :: DescribeVcenterClientsResponse)

instance
  Prelude.NFData
    DescribeVcenterClientsResponse
  where
  rnf DescribeVcenterClientsResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
