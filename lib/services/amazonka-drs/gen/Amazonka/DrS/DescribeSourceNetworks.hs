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
-- Module      : Amazonka.DrS.DescribeSourceNetworks
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all Source Networks or multiple Source Networks filtered by ID.
--
-- This operation returns paginated results.
module Amazonka.DrS.DescribeSourceNetworks
  ( -- * Creating a Request
    DescribeSourceNetworks (..),
    newDescribeSourceNetworks,

    -- * Request Lenses
    describeSourceNetworks_filters,
    describeSourceNetworks_maxResults,
    describeSourceNetworks_nextToken,

    -- * Destructuring the Response
    DescribeSourceNetworksResponse (..),
    newDescribeSourceNetworksResponse,

    -- * Response Lenses
    describeSourceNetworksResponse_items,
    describeSourceNetworksResponse_nextToken,
    describeSourceNetworksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeSourceNetworks' smart constructor.
data DescribeSourceNetworks = DescribeSourceNetworks'
  { -- | A set of filters by which to return Source Networks.
    filters :: Prelude.Maybe DescribeSourceNetworksRequestFilters,
    -- | Maximum number of Source Networks to retrieve.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token of the next Source Networks to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSourceNetworks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeSourceNetworks_filters' - A set of filters by which to return Source Networks.
--
-- 'maxResults', 'describeSourceNetworks_maxResults' - Maximum number of Source Networks to retrieve.
--
-- 'nextToken', 'describeSourceNetworks_nextToken' - The token of the next Source Networks to retrieve.
newDescribeSourceNetworks ::
  DescribeSourceNetworks
newDescribeSourceNetworks =
  DescribeSourceNetworks'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | A set of filters by which to return Source Networks.
describeSourceNetworks_filters :: Lens.Lens' DescribeSourceNetworks (Prelude.Maybe DescribeSourceNetworksRequestFilters)
describeSourceNetworks_filters = Lens.lens (\DescribeSourceNetworks' {filters} -> filters) (\s@DescribeSourceNetworks' {} a -> s {filters = a} :: DescribeSourceNetworks)

-- | Maximum number of Source Networks to retrieve.
describeSourceNetworks_maxResults :: Lens.Lens' DescribeSourceNetworks (Prelude.Maybe Prelude.Natural)
describeSourceNetworks_maxResults = Lens.lens (\DescribeSourceNetworks' {maxResults} -> maxResults) (\s@DescribeSourceNetworks' {} a -> s {maxResults = a} :: DescribeSourceNetworks)

-- | The token of the next Source Networks to retrieve.
describeSourceNetworks_nextToken :: Lens.Lens' DescribeSourceNetworks (Prelude.Maybe Prelude.Text)
describeSourceNetworks_nextToken = Lens.lens (\DescribeSourceNetworks' {nextToken} -> nextToken) (\s@DescribeSourceNetworks' {} a -> s {nextToken = a} :: DescribeSourceNetworks)

instance Core.AWSPager DescribeSourceNetworks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeSourceNetworksResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeSourceNetworksResponse_items
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeSourceNetworks_nextToken
          Lens..~ rs
          Lens.^? describeSourceNetworksResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeSourceNetworks where
  type
    AWSResponse DescribeSourceNetworks =
      DescribeSourceNetworksResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSourceNetworksResponse'
            Prelude.<$> (x Data..?> "items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSourceNetworks where
  hashWithSalt _salt DescribeSourceNetworks' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeSourceNetworks where
  rnf DescribeSourceNetworks' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeSourceNetworks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeSourceNetworks where
  toJSON DescribeSourceNetworks' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filters" Data..=) Prelude.<$> filters,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribeSourceNetworks where
  toPath = Prelude.const "/DescribeSourceNetworks"

instance Data.ToQuery DescribeSourceNetworks where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSourceNetworksResponse' smart constructor.
data DescribeSourceNetworksResponse = DescribeSourceNetworksResponse'
  { -- | An array of Source Networks.
    items :: Prelude.Maybe [SourceNetwork],
    -- | The token of the next Source Networks to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSourceNetworksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'describeSourceNetworksResponse_items' - An array of Source Networks.
--
-- 'nextToken', 'describeSourceNetworksResponse_nextToken' - The token of the next Source Networks to retrieve.
--
-- 'httpStatus', 'describeSourceNetworksResponse_httpStatus' - The response's http status code.
newDescribeSourceNetworksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSourceNetworksResponse
newDescribeSourceNetworksResponse pHttpStatus_ =
  DescribeSourceNetworksResponse'
    { items =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of Source Networks.
describeSourceNetworksResponse_items :: Lens.Lens' DescribeSourceNetworksResponse (Prelude.Maybe [SourceNetwork])
describeSourceNetworksResponse_items = Lens.lens (\DescribeSourceNetworksResponse' {items} -> items) (\s@DescribeSourceNetworksResponse' {} a -> s {items = a} :: DescribeSourceNetworksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token of the next Source Networks to retrieve.
describeSourceNetworksResponse_nextToken :: Lens.Lens' DescribeSourceNetworksResponse (Prelude.Maybe Prelude.Text)
describeSourceNetworksResponse_nextToken = Lens.lens (\DescribeSourceNetworksResponse' {nextToken} -> nextToken) (\s@DescribeSourceNetworksResponse' {} a -> s {nextToken = a} :: DescribeSourceNetworksResponse)

-- | The response's http status code.
describeSourceNetworksResponse_httpStatus :: Lens.Lens' DescribeSourceNetworksResponse Prelude.Int
describeSourceNetworksResponse_httpStatus = Lens.lens (\DescribeSourceNetworksResponse' {httpStatus} -> httpStatus) (\s@DescribeSourceNetworksResponse' {} a -> s {httpStatus = a} :: DescribeSourceNetworksResponse)

instance
  Prelude.NFData
    DescribeSourceNetworksResponse
  where
  rnf DescribeSourceNetworksResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
