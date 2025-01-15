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
-- Module      : Amazonka.MGN.DescribeSourceServers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all SourceServers or multiple SourceServers by ID.
--
-- This operation returns paginated results.
module Amazonka.MGN.DescribeSourceServers
  ( -- * Creating a Request
    DescribeSourceServers (..),
    newDescribeSourceServers,

    -- * Request Lenses
    describeSourceServers_filters,
    describeSourceServers_maxResults,
    describeSourceServers_nextToken,

    -- * Destructuring the Response
    DescribeSourceServersResponse (..),
    newDescribeSourceServersResponse,

    -- * Response Lenses
    describeSourceServersResponse_items,
    describeSourceServersResponse_nextToken,
    describeSourceServersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeSourceServers' smart constructor.
data DescribeSourceServers = DescribeSourceServers'
  { -- | Request to filter Source Servers list.
    filters :: Prelude.Maybe DescribeSourceServersRequestFilters,
    -- | Request to filter Source Servers list by maximum results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Request to filter Source Servers list by next token.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSourceServers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeSourceServers_filters' - Request to filter Source Servers list.
--
-- 'maxResults', 'describeSourceServers_maxResults' - Request to filter Source Servers list by maximum results.
--
-- 'nextToken', 'describeSourceServers_nextToken' - Request to filter Source Servers list by next token.
newDescribeSourceServers ::
  DescribeSourceServers
newDescribeSourceServers =
  DescribeSourceServers'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Request to filter Source Servers list.
describeSourceServers_filters :: Lens.Lens' DescribeSourceServers (Prelude.Maybe DescribeSourceServersRequestFilters)
describeSourceServers_filters = Lens.lens (\DescribeSourceServers' {filters} -> filters) (\s@DescribeSourceServers' {} a -> s {filters = a} :: DescribeSourceServers)

-- | Request to filter Source Servers list by maximum results.
describeSourceServers_maxResults :: Lens.Lens' DescribeSourceServers (Prelude.Maybe Prelude.Natural)
describeSourceServers_maxResults = Lens.lens (\DescribeSourceServers' {maxResults} -> maxResults) (\s@DescribeSourceServers' {} a -> s {maxResults = a} :: DescribeSourceServers)

-- | Request to filter Source Servers list by next token.
describeSourceServers_nextToken :: Lens.Lens' DescribeSourceServers (Prelude.Maybe Prelude.Text)
describeSourceServers_nextToken = Lens.lens (\DescribeSourceServers' {nextToken} -> nextToken) (\s@DescribeSourceServers' {} a -> s {nextToken = a} :: DescribeSourceServers)

instance Core.AWSPager DescribeSourceServers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeSourceServersResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeSourceServersResponse_items
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeSourceServers_nextToken
              Lens..~ rs
              Lens.^? describeSourceServersResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest DescribeSourceServers where
  type
    AWSResponse DescribeSourceServers =
      DescribeSourceServersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSourceServersResponse'
            Prelude.<$> (x Data..?> "items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSourceServers where
  hashWithSalt _salt DescribeSourceServers' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeSourceServers where
  rnf DescribeSourceServers' {..} =
    Prelude.rnf filters `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken

instance Data.ToHeaders DescribeSourceServers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeSourceServers where
  toJSON DescribeSourceServers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filters" Data..=) Prelude.<$> filters,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribeSourceServers where
  toPath = Prelude.const "/DescribeSourceServers"

instance Data.ToQuery DescribeSourceServers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSourceServersResponse' smart constructor.
data DescribeSourceServersResponse = DescribeSourceServersResponse'
  { -- | Request to filter Source Servers list by item.
    items :: Prelude.Maybe [SourceServer],
    -- | Request to filter Source Servers next token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSourceServersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'describeSourceServersResponse_items' - Request to filter Source Servers list by item.
--
-- 'nextToken', 'describeSourceServersResponse_nextToken' - Request to filter Source Servers next token.
--
-- 'httpStatus', 'describeSourceServersResponse_httpStatus' - The response's http status code.
newDescribeSourceServersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSourceServersResponse
newDescribeSourceServersResponse pHttpStatus_ =
  DescribeSourceServersResponse'
    { items =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Request to filter Source Servers list by item.
describeSourceServersResponse_items :: Lens.Lens' DescribeSourceServersResponse (Prelude.Maybe [SourceServer])
describeSourceServersResponse_items = Lens.lens (\DescribeSourceServersResponse' {items} -> items) (\s@DescribeSourceServersResponse' {} a -> s {items = a} :: DescribeSourceServersResponse) Prelude.. Lens.mapping Lens.coerced

-- | Request to filter Source Servers next token.
describeSourceServersResponse_nextToken :: Lens.Lens' DescribeSourceServersResponse (Prelude.Maybe Prelude.Text)
describeSourceServersResponse_nextToken = Lens.lens (\DescribeSourceServersResponse' {nextToken} -> nextToken) (\s@DescribeSourceServersResponse' {} a -> s {nextToken = a} :: DescribeSourceServersResponse)

-- | The response's http status code.
describeSourceServersResponse_httpStatus :: Lens.Lens' DescribeSourceServersResponse Prelude.Int
describeSourceServersResponse_httpStatus = Lens.lens (\DescribeSourceServersResponse' {httpStatus} -> httpStatus) (\s@DescribeSourceServersResponse' {} a -> s {httpStatus = a} :: DescribeSourceServersResponse)

instance Prelude.NFData DescribeSourceServersResponse where
  rnf DescribeSourceServersResponse' {..} =
    Prelude.rnf items `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
