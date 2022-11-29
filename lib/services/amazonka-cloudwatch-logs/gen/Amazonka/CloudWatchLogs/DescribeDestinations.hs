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
    describeDestinations_nextToken,
    describeDestinations_limit,
    describeDestinations_destinationNamePrefix,

    -- * Destructuring the Response
    DescribeDestinationsResponse (..),
    newDescribeDestinationsResponse,

    -- * Response Lenses
    describeDestinationsResponse_nextToken,
    describeDestinationsResponse_destinations,
    describeDestinationsResponse_httpStatus,
  )
where

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDestinations' smart constructor.
data DescribeDestinations = DescribeDestinations'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items returned. If you don\'t specify a value, the
    -- default maximum value of 50 items is used.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The prefix to match. If you don\'t specify a value, no prefix filter is
    -- applied.
    destinationNamePrefix :: Prelude.Maybe Prelude.Text
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
-- 'nextToken', 'describeDestinations_nextToken' - The token for the next set of items to return. (You received this token
-- from a previous call.)
--
-- 'limit', 'describeDestinations_limit' - The maximum number of items returned. If you don\'t specify a value, the
-- default maximum value of 50 items is used.
--
-- 'destinationNamePrefix', 'describeDestinations_destinationNamePrefix' - The prefix to match. If you don\'t specify a value, no prefix filter is
-- applied.
newDescribeDestinations ::
  DescribeDestinations
newDescribeDestinations =
  DescribeDestinations'
    { nextToken = Prelude.Nothing,
      limit = Prelude.Nothing,
      destinationNamePrefix = Prelude.Nothing
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeDestinations_nextToken :: Lens.Lens' DescribeDestinations (Prelude.Maybe Prelude.Text)
describeDestinations_nextToken = Lens.lens (\DescribeDestinations' {nextToken} -> nextToken) (\s@DescribeDestinations' {} a -> s {nextToken = a} :: DescribeDestinations)

-- | The maximum number of items returned. If you don\'t specify a value, the
-- default maximum value of 50 items is used.
describeDestinations_limit :: Lens.Lens' DescribeDestinations (Prelude.Maybe Prelude.Natural)
describeDestinations_limit = Lens.lens (\DescribeDestinations' {limit} -> limit) (\s@DescribeDestinations' {} a -> s {limit = a} :: DescribeDestinations)

-- | The prefix to match. If you don\'t specify a value, no prefix filter is
-- applied.
describeDestinations_destinationNamePrefix :: Lens.Lens' DescribeDestinations (Prelude.Maybe Prelude.Text)
describeDestinations_destinationNamePrefix = Lens.lens (\DescribeDestinations' {destinationNamePrefix} -> destinationNamePrefix) (\s@DescribeDestinations' {} a -> s {destinationNamePrefix = a} :: DescribeDestinations)

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
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "destinations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDestinations where
  hashWithSalt _salt DescribeDestinations' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` destinationNamePrefix

instance Prelude.NFData DescribeDestinations where
  rnf DescribeDestinations' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf destinationNamePrefix

instance Core.ToHeaders DescribeDestinations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Logs_20140328.DescribeDestinations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeDestinations where
  toJSON DescribeDestinations' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("limit" Core..=) Prelude.<$> limit,
            ("DestinationNamePrefix" Core..=)
              Prelude.<$> destinationNamePrefix
          ]
      )

instance Core.ToPath DescribeDestinations where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeDestinations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDestinationsResponse' smart constructor.
data DescribeDestinationsResponse = DescribeDestinationsResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | The destinations.
    destinations :: Prelude.Maybe [Destination],
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
-- 'nextToken', 'describeDestinationsResponse_nextToken' - Undocumented member.
--
-- 'destinations', 'describeDestinationsResponse_destinations' - The destinations.
--
-- 'httpStatus', 'describeDestinationsResponse_httpStatus' - The response's http status code.
newDescribeDestinationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDestinationsResponse
newDescribeDestinationsResponse pHttpStatus_ =
  DescribeDestinationsResponse'
    { nextToken =
        Prelude.Nothing,
      destinations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeDestinationsResponse_nextToken :: Lens.Lens' DescribeDestinationsResponse (Prelude.Maybe Prelude.Text)
describeDestinationsResponse_nextToken = Lens.lens (\DescribeDestinationsResponse' {nextToken} -> nextToken) (\s@DescribeDestinationsResponse' {} a -> s {nextToken = a} :: DescribeDestinationsResponse)

-- | The destinations.
describeDestinationsResponse_destinations :: Lens.Lens' DescribeDestinationsResponse (Prelude.Maybe [Destination])
describeDestinationsResponse_destinations = Lens.lens (\DescribeDestinationsResponse' {destinations} -> destinations) (\s@DescribeDestinationsResponse' {} a -> s {destinations = a} :: DescribeDestinationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeDestinationsResponse_httpStatus :: Lens.Lens' DescribeDestinationsResponse Prelude.Int
describeDestinationsResponse_httpStatus = Lens.lens (\DescribeDestinationsResponse' {httpStatus} -> httpStatus) (\s@DescribeDestinationsResponse' {} a -> s {httpStatus = a} :: DescribeDestinationsResponse)

instance Prelude.NFData DescribeDestinationsResponse where
  rnf DescribeDestinationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf destinations
      `Prelude.seq` Prelude.rnf httpStatus
