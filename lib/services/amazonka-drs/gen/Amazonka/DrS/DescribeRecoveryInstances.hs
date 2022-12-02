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
-- Module      : Amazonka.DrS.DescribeRecoveryInstances
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all Recovery Instances or multiple Recovery Instances by ID.
--
-- This operation returns paginated results.
module Amazonka.DrS.DescribeRecoveryInstances
  ( -- * Creating a Request
    DescribeRecoveryInstances (..),
    newDescribeRecoveryInstances,

    -- * Request Lenses
    describeRecoveryInstances_nextToken,
    describeRecoveryInstances_filters,
    describeRecoveryInstances_maxResults,

    -- * Destructuring the Response
    DescribeRecoveryInstancesResponse (..),
    newDescribeRecoveryInstancesResponse,

    -- * Response Lenses
    describeRecoveryInstancesResponse_items,
    describeRecoveryInstancesResponse_nextToken,
    describeRecoveryInstancesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeRecoveryInstances' smart constructor.
data DescribeRecoveryInstances = DescribeRecoveryInstances'
  { -- | The token of the next Recovery Instance to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A set of filters by which to return Recovery Instances.
    filters :: Prelude.Maybe DescribeRecoveryInstancesRequestFilters,
    -- | Maximum number of Recovery Instances to retrieve.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRecoveryInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeRecoveryInstances_nextToken' - The token of the next Recovery Instance to retrieve.
--
-- 'filters', 'describeRecoveryInstances_filters' - A set of filters by which to return Recovery Instances.
--
-- 'maxResults', 'describeRecoveryInstances_maxResults' - Maximum number of Recovery Instances to retrieve.
newDescribeRecoveryInstances ::
  DescribeRecoveryInstances
newDescribeRecoveryInstances =
  DescribeRecoveryInstances'
    { nextToken =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token of the next Recovery Instance to retrieve.
describeRecoveryInstances_nextToken :: Lens.Lens' DescribeRecoveryInstances (Prelude.Maybe Prelude.Text)
describeRecoveryInstances_nextToken = Lens.lens (\DescribeRecoveryInstances' {nextToken} -> nextToken) (\s@DescribeRecoveryInstances' {} a -> s {nextToken = a} :: DescribeRecoveryInstances)

-- | A set of filters by which to return Recovery Instances.
describeRecoveryInstances_filters :: Lens.Lens' DescribeRecoveryInstances (Prelude.Maybe DescribeRecoveryInstancesRequestFilters)
describeRecoveryInstances_filters = Lens.lens (\DescribeRecoveryInstances' {filters} -> filters) (\s@DescribeRecoveryInstances' {} a -> s {filters = a} :: DescribeRecoveryInstances)

-- | Maximum number of Recovery Instances to retrieve.
describeRecoveryInstances_maxResults :: Lens.Lens' DescribeRecoveryInstances (Prelude.Maybe Prelude.Natural)
describeRecoveryInstances_maxResults = Lens.lens (\DescribeRecoveryInstances' {maxResults} -> maxResults) (\s@DescribeRecoveryInstances' {} a -> s {maxResults = a} :: DescribeRecoveryInstances)

instance Core.AWSPager DescribeRecoveryInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeRecoveryInstancesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeRecoveryInstancesResponse_items
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeRecoveryInstances_nextToken
          Lens..~ rs
          Lens.^? describeRecoveryInstancesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeRecoveryInstances where
  type
    AWSResponse DescribeRecoveryInstances =
      DescribeRecoveryInstancesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRecoveryInstancesResponse'
            Prelude.<$> (x Data..?> "items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeRecoveryInstances where
  hashWithSalt _salt DescribeRecoveryInstances' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeRecoveryInstances where
  rnf DescribeRecoveryInstances' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders DescribeRecoveryInstances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeRecoveryInstances where
  toJSON DescribeRecoveryInstances' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("filters" Data..=) Prelude.<$> filters,
            ("maxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath DescribeRecoveryInstances where
  toPath = Prelude.const "/DescribeRecoveryInstances"

instance Data.ToQuery DescribeRecoveryInstances where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRecoveryInstancesResponse' smart constructor.
data DescribeRecoveryInstancesResponse = DescribeRecoveryInstancesResponse'
  { -- | An array of Recovery Instances.
    items :: Prelude.Maybe [RecoveryInstance],
    -- | The token of the next Recovery Instance to retrieve.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRecoveryInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'describeRecoveryInstancesResponse_items' - An array of Recovery Instances.
--
-- 'nextToken', 'describeRecoveryInstancesResponse_nextToken' - The token of the next Recovery Instance to retrieve.
--
-- 'httpStatus', 'describeRecoveryInstancesResponse_httpStatus' - The response's http status code.
newDescribeRecoveryInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRecoveryInstancesResponse
newDescribeRecoveryInstancesResponse pHttpStatus_ =
  DescribeRecoveryInstancesResponse'
    { items =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of Recovery Instances.
describeRecoveryInstancesResponse_items :: Lens.Lens' DescribeRecoveryInstancesResponse (Prelude.Maybe [RecoveryInstance])
describeRecoveryInstancesResponse_items = Lens.lens (\DescribeRecoveryInstancesResponse' {items} -> items) (\s@DescribeRecoveryInstancesResponse' {} a -> s {items = a} :: DescribeRecoveryInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token of the next Recovery Instance to retrieve.
describeRecoveryInstancesResponse_nextToken :: Lens.Lens' DescribeRecoveryInstancesResponse (Prelude.Maybe Prelude.Text)
describeRecoveryInstancesResponse_nextToken = Lens.lens (\DescribeRecoveryInstancesResponse' {nextToken} -> nextToken) (\s@DescribeRecoveryInstancesResponse' {} a -> s {nextToken = a} :: DescribeRecoveryInstancesResponse)

-- | The response's http status code.
describeRecoveryInstancesResponse_httpStatus :: Lens.Lens' DescribeRecoveryInstancesResponse Prelude.Int
describeRecoveryInstancesResponse_httpStatus = Lens.lens (\DescribeRecoveryInstancesResponse' {httpStatus} -> httpStatus) (\s@DescribeRecoveryInstancesResponse' {} a -> s {httpStatus = a} :: DescribeRecoveryInstancesResponse)

instance
  Prelude.NFData
    DescribeRecoveryInstancesResponse
  where
  rnf DescribeRecoveryInstancesResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
