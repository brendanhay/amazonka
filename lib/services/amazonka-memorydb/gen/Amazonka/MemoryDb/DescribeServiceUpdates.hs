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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    describeServiceUpdates_status,
    describeServiceUpdates_serviceUpdateName,
    describeServiceUpdates_clusterNames,
    describeServiceUpdates_nextToken,
    describeServiceUpdates_maxResults,

    -- * Destructuring the Response
    DescribeServiceUpdatesResponse (..),
    newDescribeServiceUpdatesResponse,

    -- * Response Lenses
    describeServiceUpdatesResponse_serviceUpdates,
    describeServiceUpdatesResponse_nextToken,
    describeServiceUpdatesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MemoryDb.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeServiceUpdates' smart constructor.
data DescribeServiceUpdates = DescribeServiceUpdates'
  { -- | The status(es) of the service updates to filter on
    status :: Prelude.Maybe [ServiceUpdateStatus],
    -- | The unique ID of the service update to describe.
    serviceUpdateName :: Prelude.Maybe Prelude.Text,
    -- | The list of cluster names to identify service updates to apply
    clusterNames :: Prelude.Maybe [Prelude.Text],
    -- | An optional argument to pass in case the total number of records exceeds
    -- the value of MaxResults. If nextToken is returned, there are more
    -- results available. The value of nextToken is a unique pagination token
    -- for each page. Make the call again using the returned token to retrieve
    -- the next page. Keep all other arguments unchanged.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of records to include in the response. If more
    -- records exist than the specified MaxResults value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Prelude.Maybe Prelude.Int
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
-- 'status', 'describeServiceUpdates_status' - The status(es) of the service updates to filter on
--
-- 'serviceUpdateName', 'describeServiceUpdates_serviceUpdateName' - The unique ID of the service update to describe.
--
-- 'clusterNames', 'describeServiceUpdates_clusterNames' - The list of cluster names to identify service updates to apply
--
-- 'nextToken', 'describeServiceUpdates_nextToken' - An optional argument to pass in case the total number of records exceeds
-- the value of MaxResults. If nextToken is returned, there are more
-- results available. The value of nextToken is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
--
-- 'maxResults', 'describeServiceUpdates_maxResults' - The maximum number of records to include in the response. If more
-- records exist than the specified MaxResults value, a token is included
-- in the response so that the remaining results can be retrieved.
newDescribeServiceUpdates ::
  DescribeServiceUpdates
newDescribeServiceUpdates =
  DescribeServiceUpdates'
    { status = Prelude.Nothing,
      serviceUpdateName = Prelude.Nothing,
      clusterNames = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The status(es) of the service updates to filter on
describeServiceUpdates_status :: Lens.Lens' DescribeServiceUpdates (Prelude.Maybe [ServiceUpdateStatus])
describeServiceUpdates_status = Lens.lens (\DescribeServiceUpdates' {status} -> status) (\s@DescribeServiceUpdates' {} a -> s {status = a} :: DescribeServiceUpdates) Prelude.. Lens.mapping Lens.coerced

-- | The unique ID of the service update to describe.
describeServiceUpdates_serviceUpdateName :: Lens.Lens' DescribeServiceUpdates (Prelude.Maybe Prelude.Text)
describeServiceUpdates_serviceUpdateName = Lens.lens (\DescribeServiceUpdates' {serviceUpdateName} -> serviceUpdateName) (\s@DescribeServiceUpdates' {} a -> s {serviceUpdateName = a} :: DescribeServiceUpdates)

-- | The list of cluster names to identify service updates to apply
describeServiceUpdates_clusterNames :: Lens.Lens' DescribeServiceUpdates (Prelude.Maybe [Prelude.Text])
describeServiceUpdates_clusterNames = Lens.lens (\DescribeServiceUpdates' {clusterNames} -> clusterNames) (\s@DescribeServiceUpdates' {} a -> s {clusterNames = a} :: DescribeServiceUpdates) Prelude.. Lens.mapping Lens.coerced

-- | An optional argument to pass in case the total number of records exceeds
-- the value of MaxResults. If nextToken is returned, there are more
-- results available. The value of nextToken is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
describeServiceUpdates_nextToken :: Lens.Lens' DescribeServiceUpdates (Prelude.Maybe Prelude.Text)
describeServiceUpdates_nextToken = Lens.lens (\DescribeServiceUpdates' {nextToken} -> nextToken) (\s@DescribeServiceUpdates' {} a -> s {nextToken = a} :: DescribeServiceUpdates)

-- | The maximum number of records to include in the response. If more
-- records exist than the specified MaxResults value, a token is included
-- in the response so that the remaining results can be retrieved.
describeServiceUpdates_maxResults :: Lens.Lens' DescribeServiceUpdates (Prelude.Maybe Prelude.Int)
describeServiceUpdates_maxResults = Lens.lens (\DescribeServiceUpdates' {maxResults} -> maxResults) (\s@DescribeServiceUpdates' {} a -> s {maxResults = a} :: DescribeServiceUpdates)

instance Core.AWSRequest DescribeServiceUpdates where
  type
    AWSResponse DescribeServiceUpdates =
      DescribeServiceUpdatesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeServiceUpdatesResponse'
            Prelude.<$> (x Core..?> "ServiceUpdates" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeServiceUpdates where
  hashWithSalt _salt DescribeServiceUpdates' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` serviceUpdateName
      `Prelude.hashWithSalt` clusterNames
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeServiceUpdates where
  rnf DescribeServiceUpdates' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf serviceUpdateName
      `Prelude.seq` Prelude.rnf clusterNames
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

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
          [ ("Status" Core..=) Prelude.<$> status,
            ("ServiceUpdateName" Core..=)
              Prelude.<$> serviceUpdateName,
            ("ClusterNames" Core..=) Prelude.<$> clusterNames,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath DescribeServiceUpdates where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeServiceUpdates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeServiceUpdatesResponse' smart constructor.
data DescribeServiceUpdatesResponse = DescribeServiceUpdatesResponse'
  { -- | A list of service updates
    serviceUpdates :: Prelude.Maybe [ServiceUpdate],
    -- | An optional argument to pass in case the total number of records exceeds
    -- the value of MaxResults. If nextToken is returned, there are more
    -- results available. The value of nextToken is a unique pagination token
    -- for each page. Make the call again using the returned token to retrieve
    -- the next page. Keep all other arguments unchanged.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'serviceUpdates', 'describeServiceUpdatesResponse_serviceUpdates' - A list of service updates
--
-- 'nextToken', 'describeServiceUpdatesResponse_nextToken' - An optional argument to pass in case the total number of records exceeds
-- the value of MaxResults. If nextToken is returned, there are more
-- results available. The value of nextToken is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
--
-- 'httpStatus', 'describeServiceUpdatesResponse_httpStatus' - The response's http status code.
newDescribeServiceUpdatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeServiceUpdatesResponse
newDescribeServiceUpdatesResponse pHttpStatus_ =
  DescribeServiceUpdatesResponse'
    { serviceUpdates =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of service updates
describeServiceUpdatesResponse_serviceUpdates :: Lens.Lens' DescribeServiceUpdatesResponse (Prelude.Maybe [ServiceUpdate])
describeServiceUpdatesResponse_serviceUpdates = Lens.lens (\DescribeServiceUpdatesResponse' {serviceUpdates} -> serviceUpdates) (\s@DescribeServiceUpdatesResponse' {} a -> s {serviceUpdates = a} :: DescribeServiceUpdatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | An optional argument to pass in case the total number of records exceeds
-- the value of MaxResults. If nextToken is returned, there are more
-- results available. The value of nextToken is a unique pagination token
-- for each page. Make the call again using the returned token to retrieve
-- the next page. Keep all other arguments unchanged.
describeServiceUpdatesResponse_nextToken :: Lens.Lens' DescribeServiceUpdatesResponse (Prelude.Maybe Prelude.Text)
describeServiceUpdatesResponse_nextToken = Lens.lens (\DescribeServiceUpdatesResponse' {nextToken} -> nextToken) (\s@DescribeServiceUpdatesResponse' {} a -> s {nextToken = a} :: DescribeServiceUpdatesResponse)

-- | The response's http status code.
describeServiceUpdatesResponse_httpStatus :: Lens.Lens' DescribeServiceUpdatesResponse Prelude.Int
describeServiceUpdatesResponse_httpStatus = Lens.lens (\DescribeServiceUpdatesResponse' {httpStatus} -> httpStatus) (\s@DescribeServiceUpdatesResponse' {} a -> s {httpStatus = a} :: DescribeServiceUpdatesResponse)

instance
  Prelude.NFData
    DescribeServiceUpdatesResponse
  where
  rnf DescribeServiceUpdatesResponse' {..} =
    Prelude.rnf serviceUpdates
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
