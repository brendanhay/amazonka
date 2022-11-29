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
-- Module      : Amazonka.MemoryDb.DescribeServiceUpdates
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details of the service updates
module Amazonka.MemoryDb.DescribeServiceUpdates
  ( -- * Creating a Request
    DescribeServiceUpdates (..),
    newDescribeServiceUpdates,

    -- * Request Lenses
    describeServiceUpdates_nextToken,
    describeServiceUpdates_status,
    describeServiceUpdates_maxResults,
    describeServiceUpdates_serviceUpdateName,
    describeServiceUpdates_clusterNames,

    -- * Destructuring the Response
    DescribeServiceUpdatesResponse (..),
    newDescribeServiceUpdatesResponse,

    -- * Response Lenses
    describeServiceUpdatesResponse_nextToken,
    describeServiceUpdatesResponse_serviceUpdates,
    describeServiceUpdatesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MemoryDb.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeServiceUpdates' smart constructor.
data DescribeServiceUpdates = DescribeServiceUpdates'
  { -- | An optional argument to pass in case the total number of records exceeds
    -- the value of MaxResults. If nextToken is returned, there are more
    -- results available. The value of nextToken is a unique pagination token
    -- for each page. Make the call again using the returned token to retrieve
    -- the next page. Keep all other arguments unchanged.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The status(es) of the service updates to filter on
    status :: Prelude.Maybe [ServiceUpdateStatus],
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified MaxResults value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The unique ID of the service update to describe.
    serviceUpdateName :: Prelude.Maybe Prelude.Text,
    -- | The list of cluster names to identify service updates to apply
    clusterNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeServiceUpdates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeServiceUpdates_nextToken' - An optional argument to pass in case the total number of records exceeds
-- the value of MaxResults. If nextToken is returned, there are more
-- results available. The value of nextToken is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
--
-- 'status', 'describeServiceUpdates_status' - The status(es) of the service updates to filter on
--
-- 'maxResults', 'describeServiceUpdates_maxResults' - The maximum number of records to include in the response. If more
-- records exist than the specified MaxResults value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- 'serviceUpdateName', 'describeServiceUpdates_serviceUpdateName' - The unique ID of the service update to describe.
--
-- 'clusterNames', 'describeServiceUpdates_clusterNames' - The list of cluster names to identify service updates to apply
newDescribeServiceUpdates ::
  DescribeServiceUpdates
newDescribeServiceUpdates =
  DescribeServiceUpdates'
    { nextToken =
        Prelude.Nothing,
      status = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      serviceUpdateName = Prelude.Nothing,
      clusterNames = Prelude.Nothing
    }

-- | An optional argument to pass in case the total number of records exceeds
-- the value of MaxResults. If nextToken is returned, there are more
-- results available. The value of nextToken is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
describeServiceUpdates_nextToken :: Lens.Lens' DescribeServiceUpdates (Prelude.Maybe Prelude.Text)
describeServiceUpdates_nextToken = Lens.lens (\DescribeServiceUpdates' {nextToken} -> nextToken) (\s@DescribeServiceUpdates' {} a -> s {nextToken = a} :: DescribeServiceUpdates)

-- | The status(es) of the service updates to filter on
describeServiceUpdates_status :: Lens.Lens' DescribeServiceUpdates (Prelude.Maybe [ServiceUpdateStatus])
describeServiceUpdates_status = Lens.lens (\DescribeServiceUpdates' {status} -> status) (\s@DescribeServiceUpdates' {} a -> s {status = a} :: DescribeServiceUpdates) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of records to include in the response. If more
-- records exist than the specified MaxResults value, a token is included
-- in the response so that the remaining results can be retrieved.
describeServiceUpdates_maxResults :: Lens.Lens' DescribeServiceUpdates (Prelude.Maybe Prelude.Int)
describeServiceUpdates_maxResults = Lens.lens (\DescribeServiceUpdates' {maxResults} -> maxResults) (\s@DescribeServiceUpdates' {} a -> s {maxResults = a} :: DescribeServiceUpdates)

-- | The unique ID of the service update to describe.
describeServiceUpdates_serviceUpdateName :: Lens.Lens' DescribeServiceUpdates (Prelude.Maybe Prelude.Text)
describeServiceUpdates_serviceUpdateName = Lens.lens (\DescribeServiceUpdates' {serviceUpdateName} -> serviceUpdateName) (\s@DescribeServiceUpdates' {} a -> s {serviceUpdateName = a} :: DescribeServiceUpdates)

-- | The list of cluster names to identify service updates to apply
describeServiceUpdates_clusterNames :: Lens.Lens' DescribeServiceUpdates (Prelude.Maybe [Prelude.Text])
describeServiceUpdates_clusterNames = Lens.lens (\DescribeServiceUpdates' {clusterNames} -> clusterNames) (\s@DescribeServiceUpdates' {} a -> s {clusterNames = a} :: DescribeServiceUpdates) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest DescribeServiceUpdates where
  type
    AWSResponse DescribeServiceUpdates =
      DescribeServiceUpdatesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeServiceUpdatesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "ServiceUpdates" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeServiceUpdates where
  hashWithSalt _salt DescribeServiceUpdates' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` serviceUpdateName
      `Prelude.hashWithSalt` clusterNames

instance Prelude.NFData DescribeServiceUpdates where
  rnf DescribeServiceUpdates' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf serviceUpdateName
      `Prelude.seq` Prelude.rnf clusterNames

instance Core.ToHeaders DescribeServiceUpdates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonMemoryDB.DescribeServiceUpdates" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeServiceUpdates where
  toJSON DescribeServiceUpdates' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Status" Core..=) Prelude.<$> status,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("ServiceUpdateName" Core..=)
              Prelude.<$> serviceUpdateName,
            ("ClusterNames" Core..=) Prelude.<$> clusterNames
          ]
      )

instance Core.ToPath DescribeServiceUpdates where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeServiceUpdates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeServiceUpdatesResponse' smart constructor.
data DescribeServiceUpdatesResponse = DescribeServiceUpdatesResponse'
  { -- | An optional argument to pass in case the total number of records exceeds
    -- the value of MaxResults. If nextToken is returned, there are more
    -- results available. The value of nextToken is a unique pagination token
    -- for each page. Make the call again using the returned token to retrieve
    -- the next page. Keep all other arguments unchanged.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of service updates
    serviceUpdates :: Prelude.Maybe [ServiceUpdate],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeServiceUpdatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeServiceUpdatesResponse_nextToken' - An optional argument to pass in case the total number of records exceeds
-- the value of MaxResults. If nextToken is returned, there are more
-- results available. The value of nextToken is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
--
-- 'serviceUpdates', 'describeServiceUpdatesResponse_serviceUpdates' - A list of service updates
--
-- 'httpStatus', 'describeServiceUpdatesResponse_httpStatus' - The response's http status code.
newDescribeServiceUpdatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeServiceUpdatesResponse
newDescribeServiceUpdatesResponse pHttpStatus_ =
  DescribeServiceUpdatesResponse'
    { nextToken =
        Prelude.Nothing,
      serviceUpdates = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An optional argument to pass in case the total number of records exceeds
-- the value of MaxResults. If nextToken is returned, there are more
-- results available. The value of nextToken is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
describeServiceUpdatesResponse_nextToken :: Lens.Lens' DescribeServiceUpdatesResponse (Prelude.Maybe Prelude.Text)
describeServiceUpdatesResponse_nextToken = Lens.lens (\DescribeServiceUpdatesResponse' {nextToken} -> nextToken) (\s@DescribeServiceUpdatesResponse' {} a -> s {nextToken = a} :: DescribeServiceUpdatesResponse)

-- | A list of service updates
describeServiceUpdatesResponse_serviceUpdates :: Lens.Lens' DescribeServiceUpdatesResponse (Prelude.Maybe [ServiceUpdate])
describeServiceUpdatesResponse_serviceUpdates = Lens.lens (\DescribeServiceUpdatesResponse' {serviceUpdates} -> serviceUpdates) (\s@DescribeServiceUpdatesResponse' {} a -> s {serviceUpdates = a} :: DescribeServiceUpdatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeServiceUpdatesResponse_httpStatus :: Lens.Lens' DescribeServiceUpdatesResponse Prelude.Int
describeServiceUpdatesResponse_httpStatus = Lens.lens (\DescribeServiceUpdatesResponse' {httpStatus} -> httpStatus) (\s@DescribeServiceUpdatesResponse' {} a -> s {httpStatus = a} :: DescribeServiceUpdatesResponse)

instance
  Prelude.NFData
    DescribeServiceUpdatesResponse
  where
  rnf DescribeServiceUpdatesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf serviceUpdates
      `Prelude.seq` Prelude.rnf httpStatus
