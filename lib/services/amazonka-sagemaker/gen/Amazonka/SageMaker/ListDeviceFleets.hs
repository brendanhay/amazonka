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
-- Module      : Amazonka.SageMaker.ListDeviceFleets
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of devices in the fleet.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListDeviceFleets
  ( -- * Creating a Request
    ListDeviceFleets (..),
    newListDeviceFleets,

    -- * Request Lenses
    listDeviceFleets_sortOrder,
    listDeviceFleets_nextToken,
    listDeviceFleets_lastModifiedTimeAfter,
    listDeviceFleets_nameContains,
    listDeviceFleets_lastModifiedTimeBefore,
    listDeviceFleets_creationTimeBefore,
    listDeviceFleets_sortBy,
    listDeviceFleets_maxResults,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListDeviceFleets' smart constructor.
data ListDeviceFleets = ListDeviceFleets'
  { -- | What direction to sort in.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | The response from the last list when returning a list large enough to
    -- need tokening.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Select fleets where the job was updated after X
    lastModifiedTimeAfter :: Prelude.Maybe Core.POSIX,
    -- | Filter for fleets containing this name in their fleet device name.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | Select fleets where the job was updated before X
    lastModifiedTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | Filter fleets where the edge packaging job was created before specified
    -- time.
    creationTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | The column to sort by.
    sortBy :: Prelude.Maybe ListDeviceFleetsSortBy,
    -- | The maximum number of results to select.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | Filter fleets where packaging job was created after specified time.
    creationTimeAfter :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDeviceFleets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listDeviceFleets_sortOrder' - What direction to sort in.
--
-- 'nextToken', 'listDeviceFleets_nextToken' - The response from the last list when returning a list large enough to
-- need tokening.
--
-- 'lastModifiedTimeAfter', 'listDeviceFleets_lastModifiedTimeAfter' - Select fleets where the job was updated after X
--
-- 'nameContains', 'listDeviceFleets_nameContains' - Filter for fleets containing this name in their fleet device name.
--
-- 'lastModifiedTimeBefore', 'listDeviceFleets_lastModifiedTimeBefore' - Select fleets where the job was updated before X
--
-- 'creationTimeBefore', 'listDeviceFleets_creationTimeBefore' - Filter fleets where the edge packaging job was created before specified
-- time.
--
-- 'sortBy', 'listDeviceFleets_sortBy' - The column to sort by.
--
-- 'maxResults', 'listDeviceFleets_maxResults' - The maximum number of results to select.
--
-- 'creationTimeAfter', 'listDeviceFleets_creationTimeAfter' - Filter fleets where packaging job was created after specified time.
newListDeviceFleets ::
  ListDeviceFleets
newListDeviceFleets =
  ListDeviceFleets'
    { sortOrder = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      lastModifiedTimeAfter = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      lastModifiedTimeBefore = Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      creationTimeAfter = Prelude.Nothing
    }

-- | What direction to sort in.
listDeviceFleets_sortOrder :: Lens.Lens' ListDeviceFleets (Prelude.Maybe SortOrder)
listDeviceFleets_sortOrder = Lens.lens (\ListDeviceFleets' {sortOrder} -> sortOrder) (\s@ListDeviceFleets' {} a -> s {sortOrder = a} :: ListDeviceFleets)

-- | The response from the last list when returning a list large enough to
-- need tokening.
listDeviceFleets_nextToken :: Lens.Lens' ListDeviceFleets (Prelude.Maybe Prelude.Text)
listDeviceFleets_nextToken = Lens.lens (\ListDeviceFleets' {nextToken} -> nextToken) (\s@ListDeviceFleets' {} a -> s {nextToken = a} :: ListDeviceFleets)

-- | Select fleets where the job was updated after X
listDeviceFleets_lastModifiedTimeAfter :: Lens.Lens' ListDeviceFleets (Prelude.Maybe Prelude.UTCTime)
listDeviceFleets_lastModifiedTimeAfter = Lens.lens (\ListDeviceFleets' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListDeviceFleets' {} a -> s {lastModifiedTimeAfter = a} :: ListDeviceFleets) Prelude.. Lens.mapping Core._Time

-- | Filter for fleets containing this name in their fleet device name.
listDeviceFleets_nameContains :: Lens.Lens' ListDeviceFleets (Prelude.Maybe Prelude.Text)
listDeviceFleets_nameContains = Lens.lens (\ListDeviceFleets' {nameContains} -> nameContains) (\s@ListDeviceFleets' {} a -> s {nameContains = a} :: ListDeviceFleets)

-- | Select fleets where the job was updated before X
listDeviceFleets_lastModifiedTimeBefore :: Lens.Lens' ListDeviceFleets (Prelude.Maybe Prelude.UTCTime)
listDeviceFleets_lastModifiedTimeBefore = Lens.lens (\ListDeviceFleets' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListDeviceFleets' {} a -> s {lastModifiedTimeBefore = a} :: ListDeviceFleets) Prelude.. Lens.mapping Core._Time

-- | Filter fleets where the edge packaging job was created before specified
-- time.
listDeviceFleets_creationTimeBefore :: Lens.Lens' ListDeviceFleets (Prelude.Maybe Prelude.UTCTime)
listDeviceFleets_creationTimeBefore = Lens.lens (\ListDeviceFleets' {creationTimeBefore} -> creationTimeBefore) (\s@ListDeviceFleets' {} a -> s {creationTimeBefore = a} :: ListDeviceFleets) Prelude.. Lens.mapping Core._Time

-- | The column to sort by.
listDeviceFleets_sortBy :: Lens.Lens' ListDeviceFleets (Prelude.Maybe ListDeviceFleetsSortBy)
listDeviceFleets_sortBy = Lens.lens (\ListDeviceFleets' {sortBy} -> sortBy) (\s@ListDeviceFleets' {} a -> s {sortBy = a} :: ListDeviceFleets)

-- | The maximum number of results to select.
listDeviceFleets_maxResults :: Lens.Lens' ListDeviceFleets (Prelude.Maybe Prelude.Int)
listDeviceFleets_maxResults = Lens.lens (\ListDeviceFleets' {maxResults} -> maxResults) (\s@ListDeviceFleets' {} a -> s {maxResults = a} :: ListDeviceFleets)

-- | Filter fleets where packaging job was created after specified time.
listDeviceFleets_creationTimeAfter :: Lens.Lens' ListDeviceFleets (Prelude.Maybe Prelude.UTCTime)
listDeviceFleets_creationTimeAfter = Lens.lens (\ListDeviceFleets' {creationTimeAfter} -> creationTimeAfter) (\s@ListDeviceFleets' {} a -> s {creationTimeAfter = a} :: ListDeviceFleets) Prelude.. Lens.mapping Core._Time

instance Core.AWSPager ListDeviceFleets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDeviceFleetsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listDeviceFleetsResponse_deviceFleetSummaries
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDeviceFleets_nextToken
          Lens..~ rs
          Lens.^? listDeviceFleetsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListDeviceFleets where
  type
    AWSResponse ListDeviceFleets =
      ListDeviceFleetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeviceFleetsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "DeviceFleetSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListDeviceFleets where
  hashWithSalt _salt ListDeviceFleets' {..} =
    _salt `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` lastModifiedTimeAfter
      `Prelude.hashWithSalt` nameContains
      `Prelude.hashWithSalt` lastModifiedTimeBefore
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` creationTimeAfter

instance Prelude.NFData ListDeviceFleets where
  rnf ListDeviceFleets' {..} =
    Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf lastModifiedTimeAfter
      `Prelude.seq` Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf lastModifiedTimeBefore
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf creationTimeAfter

instance Core.ToHeaders ListDeviceFleets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListDeviceFleets" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListDeviceFleets where
  toJSON ListDeviceFleets' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("LastModifiedTimeAfter" Core..=)
              Prelude.<$> lastModifiedTimeAfter,
            ("NameContains" Core..=) Prelude.<$> nameContains,
            ("LastModifiedTimeBefore" Core..=)
              Prelude.<$> lastModifiedTimeBefore,
            ("CreationTimeBefore" Core..=)
              Prelude.<$> creationTimeBefore,
            ("SortBy" Core..=) Prelude.<$> sortBy,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("CreationTimeAfter" Core..=)
              Prelude.<$> creationTimeAfter
          ]
      )

instance Core.ToPath ListDeviceFleets where
  toPath = Prelude.const "/"

instance Core.ToQuery ListDeviceFleets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDeviceFleetsResponse' smart constructor.
data ListDeviceFleetsResponse = ListDeviceFleetsResponse'
  { -- | The response from the last list when returning a list large enough to
    -- need tokening.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Summary of the device fleet.
    deviceFleetSummaries :: [DeviceFleetSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListDeviceFleetsResponse
newListDeviceFleetsResponse pHttpStatus_ =
  ListDeviceFleetsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      deviceFleetSummaries = Prelude.mempty
    }

-- | The response from the last list when returning a list large enough to
-- need tokening.
listDeviceFleetsResponse_nextToken :: Lens.Lens' ListDeviceFleetsResponse (Prelude.Maybe Prelude.Text)
listDeviceFleetsResponse_nextToken = Lens.lens (\ListDeviceFleetsResponse' {nextToken} -> nextToken) (\s@ListDeviceFleetsResponse' {} a -> s {nextToken = a} :: ListDeviceFleetsResponse)

-- | The response's http status code.
listDeviceFleetsResponse_httpStatus :: Lens.Lens' ListDeviceFleetsResponse Prelude.Int
listDeviceFleetsResponse_httpStatus = Lens.lens (\ListDeviceFleetsResponse' {httpStatus} -> httpStatus) (\s@ListDeviceFleetsResponse' {} a -> s {httpStatus = a} :: ListDeviceFleetsResponse)

-- | Summary of the device fleet.
listDeviceFleetsResponse_deviceFleetSummaries :: Lens.Lens' ListDeviceFleetsResponse [DeviceFleetSummary]
listDeviceFleetsResponse_deviceFleetSummaries = Lens.lens (\ListDeviceFleetsResponse' {deviceFleetSummaries} -> deviceFleetSummaries) (\s@ListDeviceFleetsResponse' {} a -> s {deviceFleetSummaries = a} :: ListDeviceFleetsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListDeviceFleetsResponse where
  rnf ListDeviceFleetsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf deviceFleetSummaries
