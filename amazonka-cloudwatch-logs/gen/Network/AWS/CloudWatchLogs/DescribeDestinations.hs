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
-- Module      : Network.AWS.CloudWatchLogs.DescribeDestinations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all your destinations. The results are ASCII-sorted by destination
-- name.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchLogs.DescribeDestinations
  ( -- * Creating a Request
    DescribeDestinations (..),
    newDescribeDestinations,

    -- * Request Lenses
    describeDestinations_nextToken,
    describeDestinations_destinationNamePrefix,
    describeDestinations_limit,

    -- * Destructuring the Response
    DescribeDestinationsResponse (..),
    newDescribeDestinationsResponse,

    -- * Response Lenses
    describeDestinationsResponse_nextToken,
    describeDestinationsResponse_destinations,
    describeDestinationsResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeDestinations' smart constructor.
data DescribeDestinations = DescribeDestinations'
  { -- | The token for the next set of items to return. (You received this token
    -- from a previous call.)
    nextToken :: Core.Maybe Core.Text,
    -- | The prefix to match. If you don\'t specify a value, no prefix filter is
    -- applied.
    destinationNamePrefix :: Core.Maybe Core.Text,
    -- | The maximum number of items returned. If you don\'t specify a value, the
    -- default is up to 50 items.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'destinationNamePrefix', 'describeDestinations_destinationNamePrefix' - The prefix to match. If you don\'t specify a value, no prefix filter is
-- applied.
--
-- 'limit', 'describeDestinations_limit' - The maximum number of items returned. If you don\'t specify a value, the
-- default is up to 50 items.
newDescribeDestinations ::
  DescribeDestinations
newDescribeDestinations =
  DescribeDestinations'
    { nextToken = Core.Nothing,
      destinationNamePrefix = Core.Nothing,
      limit = Core.Nothing
    }

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
describeDestinations_nextToken :: Lens.Lens' DescribeDestinations (Core.Maybe Core.Text)
describeDestinations_nextToken = Lens.lens (\DescribeDestinations' {nextToken} -> nextToken) (\s@DescribeDestinations' {} a -> s {nextToken = a} :: DescribeDestinations)

-- | The prefix to match. If you don\'t specify a value, no prefix filter is
-- applied.
describeDestinations_destinationNamePrefix :: Lens.Lens' DescribeDestinations (Core.Maybe Core.Text)
describeDestinations_destinationNamePrefix = Lens.lens (\DescribeDestinations' {destinationNamePrefix} -> destinationNamePrefix) (\s@DescribeDestinations' {} a -> s {destinationNamePrefix = a} :: DescribeDestinations)

-- | The maximum number of items returned. If you don\'t specify a value, the
-- default is up to 50 items.
describeDestinations_limit :: Lens.Lens' DescribeDestinations (Core.Maybe Core.Natural)
describeDestinations_limit = Lens.lens (\DescribeDestinations' {limit} -> limit) (\s@DescribeDestinations' {} a -> s {limit = a} :: DescribeDestinations)

instance Core.AWSPager DescribeDestinations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDestinationsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDestinationsResponse_destinations
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeDestinations_nextToken
          Lens..~ rs
          Lens.^? describeDestinationsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeDestinations where
  type
    AWSResponse DescribeDestinations =
      DescribeDestinationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDestinationsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "destinations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeDestinations

instance Core.NFData DescribeDestinations

instance Core.ToHeaders DescribeDestinations where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Logs_20140328.DescribeDestinations" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeDestinations where
  toJSON DescribeDestinations' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("DestinationNamePrefix" Core..=)
              Core.<$> destinationNamePrefix,
            ("limit" Core..=) Core.<$> limit
          ]
      )

instance Core.ToPath DescribeDestinations where
  toPath = Core.const "/"

instance Core.ToQuery DescribeDestinations where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeDestinationsResponse' smart constructor.
data DescribeDestinationsResponse = DescribeDestinationsResponse'
  { nextToken :: Core.Maybe Core.Text,
    -- | The destinations.
    destinations :: Core.Maybe [Destination],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeDestinationsResponse
newDescribeDestinationsResponse pHttpStatus_ =
  DescribeDestinationsResponse'
    { nextToken =
        Core.Nothing,
      destinations = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeDestinationsResponse_nextToken :: Lens.Lens' DescribeDestinationsResponse (Core.Maybe Core.Text)
describeDestinationsResponse_nextToken = Lens.lens (\DescribeDestinationsResponse' {nextToken} -> nextToken) (\s@DescribeDestinationsResponse' {} a -> s {nextToken = a} :: DescribeDestinationsResponse)

-- | The destinations.
describeDestinationsResponse_destinations :: Lens.Lens' DescribeDestinationsResponse (Core.Maybe [Destination])
describeDestinationsResponse_destinations = Lens.lens (\DescribeDestinationsResponse' {destinations} -> destinations) (\s@DescribeDestinationsResponse' {} a -> s {destinations = a} :: DescribeDestinationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeDestinationsResponse_httpStatus :: Lens.Lens' DescribeDestinationsResponse Core.Int
describeDestinationsResponse_httpStatus = Lens.lens (\DescribeDestinationsResponse' {httpStatus} -> httpStatus) (\s@DescribeDestinationsResponse' {} a -> s {httpStatus = a} :: DescribeDestinationsResponse)

instance Core.NFData DescribeDestinationsResponse
