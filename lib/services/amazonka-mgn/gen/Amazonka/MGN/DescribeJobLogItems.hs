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
-- Module      : Amazonka.MGN.DescribeJobLogItems
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves detailed job log items with paging.
--
-- This operation returns paginated results.
module Amazonka.MGN.DescribeJobLogItems
  ( -- * Creating a Request
    DescribeJobLogItems (..),
    newDescribeJobLogItems,

    -- * Request Lenses
    describeJobLogItems_maxResults,
    describeJobLogItems_nextToken,
    describeJobLogItems_jobID,

    -- * Destructuring the Response
    DescribeJobLogItemsResponse (..),
    newDescribeJobLogItemsResponse,

    -- * Response Lenses
    describeJobLogItemsResponse_items,
    describeJobLogItemsResponse_nextToken,
    describeJobLogItemsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeJobLogItems' smart constructor.
data DescribeJobLogItems = DescribeJobLogItems'
  { -- | Request to describe Job log item maximum results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Request to describe Job log next token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Request to describe Job log job ID.
    jobID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeJobLogItems' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'describeJobLogItems_maxResults' - Request to describe Job log item maximum results.
--
-- 'nextToken', 'describeJobLogItems_nextToken' - Request to describe Job log next token.
--
-- 'jobID', 'describeJobLogItems_jobID' - Request to describe Job log job ID.
newDescribeJobLogItems ::
  -- | 'jobID'
  Prelude.Text ->
  DescribeJobLogItems
newDescribeJobLogItems pJobID_ =
  DescribeJobLogItems'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      jobID = pJobID_
    }

-- | Request to describe Job log item maximum results.
describeJobLogItems_maxResults :: Lens.Lens' DescribeJobLogItems (Prelude.Maybe Prelude.Natural)
describeJobLogItems_maxResults = Lens.lens (\DescribeJobLogItems' {maxResults} -> maxResults) (\s@DescribeJobLogItems' {} a -> s {maxResults = a} :: DescribeJobLogItems)

-- | Request to describe Job log next token.
describeJobLogItems_nextToken :: Lens.Lens' DescribeJobLogItems (Prelude.Maybe Prelude.Text)
describeJobLogItems_nextToken = Lens.lens (\DescribeJobLogItems' {nextToken} -> nextToken) (\s@DescribeJobLogItems' {} a -> s {nextToken = a} :: DescribeJobLogItems)

-- | Request to describe Job log job ID.
describeJobLogItems_jobID :: Lens.Lens' DescribeJobLogItems Prelude.Text
describeJobLogItems_jobID = Lens.lens (\DescribeJobLogItems' {jobID} -> jobID) (\s@DescribeJobLogItems' {} a -> s {jobID = a} :: DescribeJobLogItems)

instance Core.AWSPager DescribeJobLogItems where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeJobLogItemsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeJobLogItemsResponse_items
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeJobLogItems_nextToken
          Lens..~ rs
          Lens.^? describeJobLogItemsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeJobLogItems where
  type
    AWSResponse DescribeJobLogItems =
      DescribeJobLogItemsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeJobLogItemsResponse'
            Prelude.<$> (x Data..?> "items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeJobLogItems where
  hashWithSalt _salt DescribeJobLogItems' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` jobID

instance Prelude.NFData DescribeJobLogItems where
  rnf DescribeJobLogItems' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf jobID

instance Data.ToHeaders DescribeJobLogItems where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeJobLogItems where
  toJSON DescribeJobLogItems' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("jobID" Data..= jobID)
          ]
      )

instance Data.ToPath DescribeJobLogItems where
  toPath = Prelude.const "/DescribeJobLogItems"

instance Data.ToQuery DescribeJobLogItems where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeJobLogItemsResponse' smart constructor.
data DescribeJobLogItemsResponse = DescribeJobLogItemsResponse'
  { -- | Request to describe Job log response items.
    items :: Prelude.Maybe [JobLog],
    -- | Request to describe Job log response next token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeJobLogItemsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'describeJobLogItemsResponse_items' - Request to describe Job log response items.
--
-- 'nextToken', 'describeJobLogItemsResponse_nextToken' - Request to describe Job log response next token.
--
-- 'httpStatus', 'describeJobLogItemsResponse_httpStatus' - The response's http status code.
newDescribeJobLogItemsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeJobLogItemsResponse
newDescribeJobLogItemsResponse pHttpStatus_ =
  DescribeJobLogItemsResponse'
    { items =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Request to describe Job log response items.
describeJobLogItemsResponse_items :: Lens.Lens' DescribeJobLogItemsResponse (Prelude.Maybe [JobLog])
describeJobLogItemsResponse_items = Lens.lens (\DescribeJobLogItemsResponse' {items} -> items) (\s@DescribeJobLogItemsResponse' {} a -> s {items = a} :: DescribeJobLogItemsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Request to describe Job log response next token.
describeJobLogItemsResponse_nextToken :: Lens.Lens' DescribeJobLogItemsResponse (Prelude.Maybe Prelude.Text)
describeJobLogItemsResponse_nextToken = Lens.lens (\DescribeJobLogItemsResponse' {nextToken} -> nextToken) (\s@DescribeJobLogItemsResponse' {} a -> s {nextToken = a} :: DescribeJobLogItemsResponse)

-- | The response's http status code.
describeJobLogItemsResponse_httpStatus :: Lens.Lens' DescribeJobLogItemsResponse Prelude.Int
describeJobLogItemsResponse_httpStatus = Lens.lens (\DescribeJobLogItemsResponse' {httpStatus} -> httpStatus) (\s@DescribeJobLogItemsResponse' {} a -> s {httpStatus = a} :: DescribeJobLogItemsResponse)

instance Prelude.NFData DescribeJobLogItemsResponse where
  rnf DescribeJobLogItemsResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
