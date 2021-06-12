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
-- Module      : Network.AWS.SageMaker.ListDeviceFleets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of devices in the fleet.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListDeviceFleets
  ( -- * Creating a Request
    ListDeviceFleets (..),
    newListDeviceFleets,

    -- * Request Lenses
    listDeviceFleets_lastModifiedTimeBefore,
    listDeviceFleets_sortOrder,
    listDeviceFleets_nextToken,
    listDeviceFleets_nameContains,
    listDeviceFleets_maxResults,
    listDeviceFleets_creationTimeBefore,
    listDeviceFleets_lastModifiedTimeAfter,
    listDeviceFleets_sortBy,
    listDeviceFleets_creationTimeAfter,

    -- * Destructuring the Response
    ListDeviceFleetsResponse (..),
    newListDeviceFleetsResponse,

    -- * Response Lenses
    listDeviceFleetsResponse_nextToken,
    listDeviceFleetsResponse_httpStatus,
    listDeviceFleetsResponse_deviceFleetSummaries,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListDeviceFleets' smart constructor.
data ListDeviceFleets = ListDeviceFleets'
  { -- | Select fleets where the job was updated before X
    lastModifiedTimeBefore :: Core.Maybe Core.POSIX,
    -- | What direction to sort in.
    sortOrder :: Core.Maybe SortOrder,
    -- | The response from the last list when returning a list large enough to
    -- need tokening.
    nextToken :: Core.Maybe Core.Text,
    -- | Filter for fleets containing this name in their fleet device name.
    nameContains :: Core.Maybe Core.Text,
    -- | The maximum number of results to select.
    maxResults :: Core.Maybe Core.Int,
    -- | Filter fleets where the edge packaging job was created before specified
    -- time.
    creationTimeBefore :: Core.Maybe Core.POSIX,
    -- | Select fleets where the job was updated after X
    lastModifiedTimeAfter :: Core.Maybe Core.POSIX,
    -- | The column to sort by.
    sortBy :: Core.Maybe ListDeviceFleetsSortBy,
    -- | Filter fleets where packaging job was created after specified time.
    creationTimeAfter :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDeviceFleets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedTimeBefore', 'listDeviceFleets_lastModifiedTimeBefore' - Select fleets where the job was updated before X
--
-- 'sortOrder', 'listDeviceFleets_sortOrder' - What direction to sort in.
--
-- 'nextToken', 'listDeviceFleets_nextToken' - The response from the last list when returning a list large enough to
-- need tokening.
--
-- 'nameContains', 'listDeviceFleets_nameContains' - Filter for fleets containing this name in their fleet device name.
--
-- 'maxResults', 'listDeviceFleets_maxResults' - The maximum number of results to select.
--
-- 'creationTimeBefore', 'listDeviceFleets_creationTimeBefore' - Filter fleets where the edge packaging job was created before specified
-- time.
--
-- 'lastModifiedTimeAfter', 'listDeviceFleets_lastModifiedTimeAfter' - Select fleets where the job was updated after X
--
-- 'sortBy', 'listDeviceFleets_sortBy' - The column to sort by.
--
-- 'creationTimeAfter', 'listDeviceFleets_creationTimeAfter' - Filter fleets where packaging job was created after specified time.
newListDeviceFleets ::
  ListDeviceFleets
newListDeviceFleets =
  ListDeviceFleets'
    { lastModifiedTimeBefore =
        Core.Nothing,
      sortOrder = Core.Nothing,
      nextToken = Core.Nothing,
      nameContains = Core.Nothing,
      maxResults = Core.Nothing,
      creationTimeBefore = Core.Nothing,
      lastModifiedTimeAfter = Core.Nothing,
      sortBy = Core.Nothing,
      creationTimeAfter = Core.Nothing
    }

-- | Select fleets where the job was updated before X
listDeviceFleets_lastModifiedTimeBefore :: Lens.Lens' ListDeviceFleets (Core.Maybe Core.UTCTime)
listDeviceFleets_lastModifiedTimeBefore = Lens.lens (\ListDeviceFleets' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListDeviceFleets' {} a -> s {lastModifiedTimeBefore = a} :: ListDeviceFleets) Core.. Lens.mapping Core._Time

-- | What direction to sort in.
listDeviceFleets_sortOrder :: Lens.Lens' ListDeviceFleets (Core.Maybe SortOrder)
listDeviceFleets_sortOrder = Lens.lens (\ListDeviceFleets' {sortOrder} -> sortOrder) (\s@ListDeviceFleets' {} a -> s {sortOrder = a} :: ListDeviceFleets)

-- | The response from the last list when returning a list large enough to
-- need tokening.
listDeviceFleets_nextToken :: Lens.Lens' ListDeviceFleets (Core.Maybe Core.Text)
listDeviceFleets_nextToken = Lens.lens (\ListDeviceFleets' {nextToken} -> nextToken) (\s@ListDeviceFleets' {} a -> s {nextToken = a} :: ListDeviceFleets)

-- | Filter for fleets containing this name in their fleet device name.
listDeviceFleets_nameContains :: Lens.Lens' ListDeviceFleets (Core.Maybe Core.Text)
listDeviceFleets_nameContains = Lens.lens (\ListDeviceFleets' {nameContains} -> nameContains) (\s@ListDeviceFleets' {} a -> s {nameContains = a} :: ListDeviceFleets)

-- | The maximum number of results to select.
listDeviceFleets_maxResults :: Lens.Lens' ListDeviceFleets (Core.Maybe Core.Int)
listDeviceFleets_maxResults = Lens.lens (\ListDeviceFleets' {maxResults} -> maxResults) (\s@ListDeviceFleets' {} a -> s {maxResults = a} :: ListDeviceFleets)

-- | Filter fleets where the edge packaging job was created before specified
-- time.
listDeviceFleets_creationTimeBefore :: Lens.Lens' ListDeviceFleets (Core.Maybe Core.UTCTime)
listDeviceFleets_creationTimeBefore = Lens.lens (\ListDeviceFleets' {creationTimeBefore} -> creationTimeBefore) (\s@ListDeviceFleets' {} a -> s {creationTimeBefore = a} :: ListDeviceFleets) Core.. Lens.mapping Core._Time

-- | Select fleets where the job was updated after X
listDeviceFleets_lastModifiedTimeAfter :: Lens.Lens' ListDeviceFleets (Core.Maybe Core.UTCTime)
listDeviceFleets_lastModifiedTimeAfter = Lens.lens (\ListDeviceFleets' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListDeviceFleets' {} a -> s {lastModifiedTimeAfter = a} :: ListDeviceFleets) Core.. Lens.mapping Core._Time

-- | The column to sort by.
listDeviceFleets_sortBy :: Lens.Lens' ListDeviceFleets (Core.Maybe ListDeviceFleetsSortBy)
listDeviceFleets_sortBy = Lens.lens (\ListDeviceFleets' {sortBy} -> sortBy) (\s@ListDeviceFleets' {} a -> s {sortBy = a} :: ListDeviceFleets)

-- | Filter fleets where packaging job was created after specified time.
listDeviceFleets_creationTimeAfter :: Lens.Lens' ListDeviceFleets (Core.Maybe Core.UTCTime)
listDeviceFleets_creationTimeAfter = Lens.lens (\ListDeviceFleets' {creationTimeAfter} -> creationTimeAfter) (\s@ListDeviceFleets' {} a -> s {creationTimeAfter = a} :: ListDeviceFleets) Core.. Lens.mapping Core._Time

instance Core.AWSPager ListDeviceFleets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDeviceFleetsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^. listDeviceFleetsResponse_deviceFleetSummaries
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listDeviceFleets_nextToken
          Lens..~ rs
          Lens.^? listDeviceFleetsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListDeviceFleets where
  type
    AWSResponse ListDeviceFleets =
      ListDeviceFleetsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeviceFleetsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..?> "DeviceFleetSummaries"
                         Core..!@ Core.mempty
                     )
      )

instance Core.Hashable ListDeviceFleets

instance Core.NFData ListDeviceFleets

instance Core.ToHeaders ListDeviceFleets where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListDeviceFleets" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListDeviceFleets where
  toJSON ListDeviceFleets' {..} =
    Core.object
      ( Core.catMaybes
          [ ("LastModifiedTimeBefore" Core..=)
              Core.<$> lastModifiedTimeBefore,
            ("SortOrder" Core..=) Core.<$> sortOrder,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("NameContains" Core..=) Core.<$> nameContains,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("CreationTimeBefore" Core..=)
              Core.<$> creationTimeBefore,
            ("LastModifiedTimeAfter" Core..=)
              Core.<$> lastModifiedTimeAfter,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("CreationTimeAfter" Core..=)
              Core.<$> creationTimeAfter
          ]
      )

instance Core.ToPath ListDeviceFleets where
  toPath = Core.const "/"

instance Core.ToQuery ListDeviceFleets where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListDeviceFleetsResponse' smart constructor.
data ListDeviceFleetsResponse = ListDeviceFleetsResponse'
  { -- | The response from the last list when returning a list large enough to
    -- need tokening.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | Summary of the device fleet.
    deviceFleetSummaries :: [DeviceFleetSummary]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDeviceFleetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDeviceFleetsResponse_nextToken' - The response from the last list when returning a list large enough to
-- need tokening.
--
-- 'httpStatus', 'listDeviceFleetsResponse_httpStatus' - The response's http status code.
--
-- 'deviceFleetSummaries', 'listDeviceFleetsResponse_deviceFleetSummaries' - Summary of the device fleet.
newListDeviceFleetsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListDeviceFleetsResponse
newListDeviceFleetsResponse pHttpStatus_ =
  ListDeviceFleetsResponse'
    { nextToken = Core.Nothing,
      httpStatus = pHttpStatus_,
      deviceFleetSummaries = Core.mempty
    }

-- | The response from the last list when returning a list large enough to
-- need tokening.
listDeviceFleetsResponse_nextToken :: Lens.Lens' ListDeviceFleetsResponse (Core.Maybe Core.Text)
listDeviceFleetsResponse_nextToken = Lens.lens (\ListDeviceFleetsResponse' {nextToken} -> nextToken) (\s@ListDeviceFleetsResponse' {} a -> s {nextToken = a} :: ListDeviceFleetsResponse)

-- | The response's http status code.
listDeviceFleetsResponse_httpStatus :: Lens.Lens' ListDeviceFleetsResponse Core.Int
listDeviceFleetsResponse_httpStatus = Lens.lens (\ListDeviceFleetsResponse' {httpStatus} -> httpStatus) (\s@ListDeviceFleetsResponse' {} a -> s {httpStatus = a} :: ListDeviceFleetsResponse)

-- | Summary of the device fleet.
listDeviceFleetsResponse_deviceFleetSummaries :: Lens.Lens' ListDeviceFleetsResponse [DeviceFleetSummary]
listDeviceFleetsResponse_deviceFleetSummaries = Lens.lens (\ListDeviceFleetsResponse' {deviceFleetSummaries} -> deviceFleetSummaries) (\s@ListDeviceFleetsResponse' {} a -> s {deviceFleetSummaries = a} :: ListDeviceFleetsResponse) Core.. Lens._Coerce

instance Core.NFData ListDeviceFleetsResponse
