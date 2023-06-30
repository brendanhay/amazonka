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
-- Module      : Amazonka.LicenseManagerLinuxSubscriptions.ListLinuxSubscriptionInstances
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the running Amazon EC2 instances that were discovered with
-- commercial Linux subscriptions.
--
-- This operation returns paginated results.
module Amazonka.LicenseManagerLinuxSubscriptions.ListLinuxSubscriptionInstances
  ( -- * Creating a Request
    ListLinuxSubscriptionInstances (..),
    newListLinuxSubscriptionInstances,

    -- * Request Lenses
    listLinuxSubscriptionInstances_filters,
    listLinuxSubscriptionInstances_maxResults,
    listLinuxSubscriptionInstances_nextToken,

    -- * Destructuring the Response
    ListLinuxSubscriptionInstancesResponse (..),
    newListLinuxSubscriptionInstancesResponse,

    -- * Response Lenses
    listLinuxSubscriptionInstancesResponse_instances,
    listLinuxSubscriptionInstancesResponse_nextToken,
    listLinuxSubscriptionInstancesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManagerLinuxSubscriptions.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | NextToken length limit is half of ddb accepted limit. Increase this
-- limit if parameters in request increases.
--
-- /See:/ 'newListLinuxSubscriptionInstances' smart constructor.
data ListLinuxSubscriptionInstances = ListLinuxSubscriptionInstances'
  { -- | An array of structures that you can use to filter the results to those
    -- that match one or more sets of key-value pairs that you specify. For
    -- example, you can filter by the name of @AmiID@ with an optional operator
    -- to see subscriptions that match, partially match, or don\'t match a
    -- certain Amazon Machine Image (AMI) ID.
    --
    -- The valid names for this filter are:
    --
    -- -   @AmiID@
    --
    -- -   @InstanceID@
    --
    -- -   @AccountID@
    --
    -- -   @Status@
    --
    -- -   @Region@
    --
    -- -   @UsageOperation@
    --
    -- -   @ProductCode@
    --
    -- -   @InstanceType@
    --
    -- The valid Operators for this filter are:
    --
    -- -   @contains@
    --
    -- -   @equals@
    --
    -- -   @Notequal@
    filters :: Prelude.Maybe [Filter],
    -- | Maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLinuxSubscriptionInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listLinuxSubscriptionInstances_filters' - An array of structures that you can use to filter the results to those
-- that match one or more sets of key-value pairs that you specify. For
-- example, you can filter by the name of @AmiID@ with an optional operator
-- to see subscriptions that match, partially match, or don\'t match a
-- certain Amazon Machine Image (AMI) ID.
--
-- The valid names for this filter are:
--
-- -   @AmiID@
--
-- -   @InstanceID@
--
-- -   @AccountID@
--
-- -   @Status@
--
-- -   @Region@
--
-- -   @UsageOperation@
--
-- -   @ProductCode@
--
-- -   @InstanceType@
--
-- The valid Operators for this filter are:
--
-- -   @contains@
--
-- -   @equals@
--
-- -   @Notequal@
--
-- 'maxResults', 'listLinuxSubscriptionInstances_maxResults' - Maximum number of results to return in a single call.
--
-- 'nextToken', 'listLinuxSubscriptionInstances_nextToken' - Token for the next set of results.
newListLinuxSubscriptionInstances ::
  ListLinuxSubscriptionInstances
newListLinuxSubscriptionInstances =
  ListLinuxSubscriptionInstances'
    { filters =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | An array of structures that you can use to filter the results to those
-- that match one or more sets of key-value pairs that you specify. For
-- example, you can filter by the name of @AmiID@ with an optional operator
-- to see subscriptions that match, partially match, or don\'t match a
-- certain Amazon Machine Image (AMI) ID.
--
-- The valid names for this filter are:
--
-- -   @AmiID@
--
-- -   @InstanceID@
--
-- -   @AccountID@
--
-- -   @Status@
--
-- -   @Region@
--
-- -   @UsageOperation@
--
-- -   @ProductCode@
--
-- -   @InstanceType@
--
-- The valid Operators for this filter are:
--
-- -   @contains@
--
-- -   @equals@
--
-- -   @Notequal@
listLinuxSubscriptionInstances_filters :: Lens.Lens' ListLinuxSubscriptionInstances (Prelude.Maybe [Filter])
listLinuxSubscriptionInstances_filters = Lens.lens (\ListLinuxSubscriptionInstances' {filters} -> filters) (\s@ListLinuxSubscriptionInstances' {} a -> s {filters = a} :: ListLinuxSubscriptionInstances) Prelude.. Lens.mapping Lens.coerced

-- | Maximum number of results to return in a single call.
listLinuxSubscriptionInstances_maxResults :: Lens.Lens' ListLinuxSubscriptionInstances (Prelude.Maybe Prelude.Int)
listLinuxSubscriptionInstances_maxResults = Lens.lens (\ListLinuxSubscriptionInstances' {maxResults} -> maxResults) (\s@ListLinuxSubscriptionInstances' {} a -> s {maxResults = a} :: ListLinuxSubscriptionInstances)

-- | Token for the next set of results.
listLinuxSubscriptionInstances_nextToken :: Lens.Lens' ListLinuxSubscriptionInstances (Prelude.Maybe Prelude.Text)
listLinuxSubscriptionInstances_nextToken = Lens.lens (\ListLinuxSubscriptionInstances' {nextToken} -> nextToken) (\s@ListLinuxSubscriptionInstances' {} a -> s {nextToken = a} :: ListLinuxSubscriptionInstances)

instance Core.AWSPager ListLinuxSubscriptionInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listLinuxSubscriptionInstancesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listLinuxSubscriptionInstancesResponse_instances
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listLinuxSubscriptionInstances_nextToken
          Lens..~ rs
          Lens.^? listLinuxSubscriptionInstancesResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListLinuxSubscriptionInstances
  where
  type
    AWSResponse ListLinuxSubscriptionInstances =
      ListLinuxSubscriptionInstancesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLinuxSubscriptionInstancesResponse'
            Prelude.<$> (x Data..?> "Instances" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListLinuxSubscriptionInstances
  where
  hashWithSalt
    _salt
    ListLinuxSubscriptionInstances' {..} =
      _salt
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    ListLinuxSubscriptionInstances
  where
  rnf ListLinuxSubscriptionInstances' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance
  Data.ToHeaders
    ListLinuxSubscriptionInstances
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListLinuxSubscriptionInstances where
  toJSON ListLinuxSubscriptionInstances' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListLinuxSubscriptionInstances where
  toPath =
    Prelude.const
      "/subscription/ListLinuxSubscriptionInstances"

instance Data.ToQuery ListLinuxSubscriptionInstances where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListLinuxSubscriptionInstancesResponse' smart constructor.
data ListLinuxSubscriptionInstancesResponse = ListLinuxSubscriptionInstancesResponse'
  { -- | An array that contains instance objects.
    instances :: Prelude.Maybe [Instance],
    -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLinuxSubscriptionInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instances', 'listLinuxSubscriptionInstancesResponse_instances' - An array that contains instance objects.
--
-- 'nextToken', 'listLinuxSubscriptionInstancesResponse_nextToken' - Token for the next set of results.
--
-- 'httpStatus', 'listLinuxSubscriptionInstancesResponse_httpStatus' - The response's http status code.
newListLinuxSubscriptionInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLinuxSubscriptionInstancesResponse
newListLinuxSubscriptionInstancesResponse
  pHttpStatus_ =
    ListLinuxSubscriptionInstancesResponse'
      { instances =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An array that contains instance objects.
listLinuxSubscriptionInstancesResponse_instances :: Lens.Lens' ListLinuxSubscriptionInstancesResponse (Prelude.Maybe [Instance])
listLinuxSubscriptionInstancesResponse_instances = Lens.lens (\ListLinuxSubscriptionInstancesResponse' {instances} -> instances) (\s@ListLinuxSubscriptionInstancesResponse' {} a -> s {instances = a} :: ListLinuxSubscriptionInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Token for the next set of results.
listLinuxSubscriptionInstancesResponse_nextToken :: Lens.Lens' ListLinuxSubscriptionInstancesResponse (Prelude.Maybe Prelude.Text)
listLinuxSubscriptionInstancesResponse_nextToken = Lens.lens (\ListLinuxSubscriptionInstancesResponse' {nextToken} -> nextToken) (\s@ListLinuxSubscriptionInstancesResponse' {} a -> s {nextToken = a} :: ListLinuxSubscriptionInstancesResponse)

-- | The response's http status code.
listLinuxSubscriptionInstancesResponse_httpStatus :: Lens.Lens' ListLinuxSubscriptionInstancesResponse Prelude.Int
listLinuxSubscriptionInstancesResponse_httpStatus = Lens.lens (\ListLinuxSubscriptionInstancesResponse' {httpStatus} -> httpStatus) (\s@ListLinuxSubscriptionInstancesResponse' {} a -> s {httpStatus = a} :: ListLinuxSubscriptionInstancesResponse)

instance
  Prelude.NFData
    ListLinuxSubscriptionInstancesResponse
  where
  rnf ListLinuxSubscriptionInstancesResponse' {..} =
    Prelude.rnf instances
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
