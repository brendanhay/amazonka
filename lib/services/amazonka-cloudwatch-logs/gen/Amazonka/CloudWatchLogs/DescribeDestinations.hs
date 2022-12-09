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
-- Module      : Amazonka.CloudWatchLogs.DescribeDestinations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all your destinations. The results are ASCII-sorted by destination
-- name.
--
-- This operation returns paginated results.
module Amazonka.CloudWatchLogs.DescribeDestinations
  ( -- * Creating a Request
    DescribeDestinations (..),
    newDescribeDestinations,

    -- * Request Lenses
    describeDestinations_destinationNamePrefix,
    describeDestinations_limit,
    describeDestinations_nextToken,

    -- * Destructuring the Response
    DescribeDestinationsResponse (..),
    newDescribeDestinationsResponse,

    -- * Response Lenses
    describeDestinationsResponse_destinations,
    describeDestinationsResponse_nextToken,
    describeDestinationsResponse_httpStatus,
  )
where

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDestinations' smart constructor.
data DescribeDestinations = DescribeDestinations'
  { -- | The prefix to match. If you don\'t specify a value, no prefix filter is
    -- applied.
    destinationNamePrefix :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items returned. If you don\'t specify a value, the
    -- default maximum value of 50 items is used.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDestinations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationNamePrefix', 'describeDestinations_destinationNamePrefix' - The prefix to match. If you don\'t specify a value, no prefix filter is
-- applied.
--
-- 'limit', 'describeDestinations_limit' - The maximum number of items returned. If you don\'t specify a value, the
-- default maximum value of 50 items is used.
--
-- 'nextToken', 'describeDestinations_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
newDescribeDestinations ::
  DescribeDestinations
newDescribeDestinations =
  DescribeDestinations'
    { destinationNamePrefix =
        Prelude.Nothing,
      limit = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The prefix to match. If you don\'t specify a value, no prefix filter is
-- applied.
describeDestinations_destinationNamePrefix :: Lens.Lens' DescribeDestinations (Prelude.Maybe Prelude.Text)
describeDestinations_destinationNamePrefix = Lens.lens (\DescribeDestinations' {destinationNamePrefix} -> destinationNamePrefix) (\s@DescribeDestinations' {} a -> s {destinationNamePrefix = a} :: DescribeDestinations)

-- | The maximum number of items returned. If you don\'t specify a value, the
-- default maximum value of 50 items is used.
describeDestinations_limit :: Lens.Lens' DescribeDestinations (Prelude.Maybe Prelude.Natural)
describeDestinations_limit = Lens.lens (\DescribeDestinations' {limit} -> limit) (\s@DescribeDestinations' {} a -> s {limit = a} :: DescribeDestinations)

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeDestinations_nextToken :: Lens.Lens' DescribeDestinations (Prelude.Maybe Prelude.Text)
describeDestinations_nextToken = Lens.lens (\DescribeDestinations' {nextToken} -> nextToken) (\s@DescribeDestinations' {} a -> s {nextToken = a} :: DescribeDestinations)

instance Core.AWSPager DescribeDestinations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDestinationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDestinationsResponse_destinations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeDestinations_nextToken
          Lens..~ rs
          Lens.^? describeDestinationsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeDestinations where
  type
    AWSResponse DescribeDestinations =
      DescribeDestinationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDestinationsResponse'
            Prelude.<$> (x Data..?> "destinations" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDestinations where
  hashWithSalt _salt DescribeDestinations' {..} =
    _salt `Prelude.hashWithSalt` destinationNamePrefix
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeDestinations where
  rnf DescribeDestinations' {..} =
    Prelude.rnf destinationNamePrefix
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeDestinations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Logs_20140328.DescribeDestinations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeDestinations where
  toJSON DescribeDestinations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DestinationNamePrefix" Data..=)
              Prelude.<$> destinationNamePrefix,
            ("limit" Data..=) Prelude.<$> limit,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribeDestinations where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDestinations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDestinationsResponse' smart constructor.
data DescribeDestinationsResponse = DescribeDestinationsResponse'
  { -- | The destinations.
    destinations :: Prelude.Maybe [Destination],
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDestinationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinations', 'describeDestinationsResponse_destinations' - The destinations.
--
-- 'nextToken', 'describeDestinationsResponse_nextToken' - Undocumented member.
--
-- 'httpStatus', 'describeDestinationsResponse_httpStatus' - The response's http status code.
newDescribeDestinationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDestinationsResponse
newDescribeDestinationsResponse pHttpStatus_ =
  DescribeDestinationsResponse'
    { destinations =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The destinations.
describeDestinationsResponse_destinations :: Lens.Lens' DescribeDestinationsResponse (Prelude.Maybe [Destination])
describeDestinationsResponse_destinations = Lens.lens (\DescribeDestinationsResponse' {destinations} -> destinations) (\s@DescribeDestinationsResponse' {} a -> s {destinations = a} :: DescribeDestinationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
describeDestinationsResponse_nextToken :: Lens.Lens' DescribeDestinationsResponse (Prelude.Maybe Prelude.Text)
describeDestinationsResponse_nextToken = Lens.lens (\DescribeDestinationsResponse' {nextToken} -> nextToken) (\s@DescribeDestinationsResponse' {} a -> s {nextToken = a} :: DescribeDestinationsResponse)

-- | The response's http status code.
describeDestinationsResponse_httpStatus :: Lens.Lens' DescribeDestinationsResponse Prelude.Int
describeDestinationsResponse_httpStatus = Lens.lens (\DescribeDestinationsResponse' {httpStatus} -> httpStatus) (\s@DescribeDestinationsResponse' {} a -> s {httpStatus = a} :: DescribeDestinationsResponse)

instance Prelude.NFData DescribeDestinationsResponse where
  rnf DescribeDestinationsResponse' {..} =
    Prelude.rnf destinations
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
