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
-- Module      : Amazonka.LicenseManagerUserSubscriptions.ListInstances
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the EC2 instances providing user-based subscriptions.
--
-- This operation returns paginated results.
module Amazonka.LicenseManagerUserSubscriptions.ListInstances
  ( -- * Creating a Request
    ListInstances (..),
    newListInstances,

    -- * Request Lenses
    listInstances_filters,
    listInstances_maxResults,
    listInstances_nextToken,

    -- * Destructuring the Response
    ListInstancesResponse (..),
    newListInstancesResponse,

    -- * Response Lenses
    listInstancesResponse_instanceSummaries,
    listInstancesResponse_nextToken,
    listInstancesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManagerUserSubscriptions.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListInstances' smart constructor.
data ListInstances = ListInstances'
  { -- | An array of structures that you can use to filter the results to those
    -- that match one or more sets of key-value pairs that you specify.
    filters :: Prelude.Maybe [Filter],
    -- | Maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listInstances_filters' - An array of structures that you can use to filter the results to those
-- that match one or more sets of key-value pairs that you specify.
--
-- 'maxResults', 'listInstances_maxResults' - Maximum number of results to return in a single call.
--
-- 'nextToken', 'listInstances_nextToken' - Token for the next set of results.
newListInstances ::
  ListInstances
newListInstances =
  ListInstances'
    { filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | An array of structures that you can use to filter the results to those
-- that match one or more sets of key-value pairs that you specify.
listInstances_filters :: Lens.Lens' ListInstances (Prelude.Maybe [Filter])
listInstances_filters = Lens.lens (\ListInstances' {filters} -> filters) (\s@ListInstances' {} a -> s {filters = a} :: ListInstances) Prelude.. Lens.mapping Lens.coerced

-- | Maximum number of results to return in a single call.
listInstances_maxResults :: Lens.Lens' ListInstances (Prelude.Maybe Prelude.Int)
listInstances_maxResults = Lens.lens (\ListInstances' {maxResults} -> maxResults) (\s@ListInstances' {} a -> s {maxResults = a} :: ListInstances)

-- | Token for the next set of results.
listInstances_nextToken :: Lens.Lens' ListInstances (Prelude.Maybe Prelude.Text)
listInstances_nextToken = Lens.lens (\ListInstances' {nextToken} -> nextToken) (\s@ListInstances' {} a -> s {nextToken = a} :: ListInstances)

instance Core.AWSPager ListInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listInstancesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listInstancesResponse_instanceSummaries
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listInstances_nextToken
              Lens..~ rs
              Lens.^? listInstancesResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListInstances where
  type
    AWSResponse ListInstances =
      ListInstancesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInstancesResponse'
            Prelude.<$> ( x
                            Data..?> "InstanceSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListInstances where
  hashWithSalt _salt ListInstances' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListInstances where
  rnf ListInstances' {..} =
    Prelude.rnf filters `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken

instance Data.ToHeaders ListInstances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListInstances where
  toJSON ListInstances' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListInstances where
  toPath = Prelude.const "/instance/ListInstances"

instance Data.ToQuery ListInstances where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListInstancesResponse' smart constructor.
data ListInstancesResponse = ListInstancesResponse'
  { -- | Metadata that describes the list instances operation.
    instanceSummaries :: Prelude.Maybe [InstanceSummary],
    -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceSummaries', 'listInstancesResponse_instanceSummaries' - Metadata that describes the list instances operation.
--
-- 'nextToken', 'listInstancesResponse_nextToken' - Token for the next set of results.
--
-- 'httpStatus', 'listInstancesResponse_httpStatus' - The response's http status code.
newListInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListInstancesResponse
newListInstancesResponse pHttpStatus_ =
  ListInstancesResponse'
    { instanceSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Metadata that describes the list instances operation.
listInstancesResponse_instanceSummaries :: Lens.Lens' ListInstancesResponse (Prelude.Maybe [InstanceSummary])
listInstancesResponse_instanceSummaries = Lens.lens (\ListInstancesResponse' {instanceSummaries} -> instanceSummaries) (\s@ListInstancesResponse' {} a -> s {instanceSummaries = a} :: ListInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Token for the next set of results.
listInstancesResponse_nextToken :: Lens.Lens' ListInstancesResponse (Prelude.Maybe Prelude.Text)
listInstancesResponse_nextToken = Lens.lens (\ListInstancesResponse' {nextToken} -> nextToken) (\s@ListInstancesResponse' {} a -> s {nextToken = a} :: ListInstancesResponse)

-- | The response's http status code.
listInstancesResponse_httpStatus :: Lens.Lens' ListInstancesResponse Prelude.Int
listInstancesResponse_httpStatus = Lens.lens (\ListInstancesResponse' {httpStatus} -> httpStatus) (\s@ListInstancesResponse' {} a -> s {httpStatus = a} :: ListInstancesResponse)

instance Prelude.NFData ListInstancesResponse where
  rnf ListInstancesResponse' {..} =
    Prelude.rnf instanceSummaries `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
