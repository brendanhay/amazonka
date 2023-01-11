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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    listDeviceFleets_creationTimeAfter,
    listDeviceFleets_creationTimeBefore,
    listDeviceFleets_lastModifiedTimeAfter,
    listDeviceFleets_lastModifiedTimeBefore,
    listDeviceFleets_maxResults,
    listDeviceFleets_nameContains,
    listDeviceFleets_nextToken,
    listDeviceFleets_sortBy,
    listDeviceFleets_sortOrder,

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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListDeviceFleets' smart constructor.
data ListDeviceFleets = ListDeviceFleets'
  { -- | Filter fleets where packaging job was created after specified time.
    creationTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | Filter fleets where the edge packaging job was created before specified
    -- time.
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | Select fleets where the job was updated after X
    lastModifiedTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | Select fleets where the job was updated before X
    lastModifiedTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | The maximum number of results to select.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | Filter for fleets containing this name in their fleet device name.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | The response from the last list when returning a list large enough to
    -- need tokening.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The column to sort by.
    sortBy :: Prelude.Maybe ListDeviceFleetsSortBy,
    -- | What direction to sort in.
    sortOrder :: Prelude.Maybe SortOrder
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
-- 'creationTimeAfter', 'listDeviceFleets_creationTimeAfter' - Filter fleets where packaging job was created after specified time.
--
-- 'creationTimeBefore', 'listDeviceFleets_creationTimeBefore' - Filter fleets where the edge packaging job was created before specified
-- time.
--
-- 'lastModifiedTimeAfter', 'listDeviceFleets_lastModifiedTimeAfter' - Select fleets where the job was updated after X
--
-- 'lastModifiedTimeBefore', 'listDeviceFleets_lastModifiedTimeBefore' - Select fleets where the job was updated before X
--
-- 'maxResults', 'listDeviceFleets_maxResults' - The maximum number of results to select.
--
-- 'nameContains', 'listDeviceFleets_nameContains' - Filter for fleets containing this name in their fleet device name.
--
-- 'nextToken', 'listDeviceFleets_nextToken' - The response from the last list when returning a list large enough to
-- need tokening.
--
-- 'sortBy', 'listDeviceFleets_sortBy' - The column to sort by.
--
-- 'sortOrder', 'listDeviceFleets_sortOrder' - What direction to sort in.
newListDeviceFleets ::
  ListDeviceFleets
newListDeviceFleets =
  ListDeviceFleets'
    { creationTimeAfter =
        Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      lastModifiedTimeAfter = Prelude.Nothing,
      lastModifiedTimeBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing
    }

-- | Filter fleets where packaging job was created after specified time.
listDeviceFleets_creationTimeAfter :: Lens.Lens' ListDeviceFleets (Prelude.Maybe Prelude.UTCTime)
listDeviceFleets_creationTimeAfter = Lens.lens (\ListDeviceFleets' {creationTimeAfter} -> creationTimeAfter) (\s@ListDeviceFleets' {} a -> s {creationTimeAfter = a} :: ListDeviceFleets) Prelude.. Lens.mapping Data._Time

-- | Filter fleets where the edge packaging job was created before specified
-- time.
listDeviceFleets_creationTimeBefore :: Lens.Lens' ListDeviceFleets (Prelude.Maybe Prelude.UTCTime)
listDeviceFleets_creationTimeBefore = Lens.lens (\ListDeviceFleets' {creationTimeBefore} -> creationTimeBefore) (\s@ListDeviceFleets' {} a -> s {creationTimeBefore = a} :: ListDeviceFleets) Prelude.. Lens.mapping Data._Time

-- | Select fleets where the job was updated after X
listDeviceFleets_lastModifiedTimeAfter :: Lens.Lens' ListDeviceFleets (Prelude.Maybe Prelude.UTCTime)
listDeviceFleets_lastModifiedTimeAfter = Lens.lens (\ListDeviceFleets' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListDeviceFleets' {} a -> s {lastModifiedTimeAfter = a} :: ListDeviceFleets) Prelude.. Lens.mapping Data._Time

-- | Select fleets where the job was updated before X
listDeviceFleets_lastModifiedTimeBefore :: Lens.Lens' ListDeviceFleets (Prelude.Maybe Prelude.UTCTime)
listDeviceFleets_lastModifiedTimeBefore = Lens.lens (\ListDeviceFleets' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListDeviceFleets' {} a -> s {lastModifiedTimeBefore = a} :: ListDeviceFleets) Prelude.. Lens.mapping Data._Time

-- | The maximum number of results to select.
listDeviceFleets_maxResults :: Lens.Lens' ListDeviceFleets (Prelude.Maybe Prelude.Int)
listDeviceFleets_maxResults = Lens.lens (\ListDeviceFleets' {maxResults} -> maxResults) (\s@ListDeviceFleets' {} a -> s {maxResults = a} :: ListDeviceFleets)

-- | Filter for fleets containing this name in their fleet device name.
listDeviceFleets_nameContains :: Lens.Lens' ListDeviceFleets (Prelude.Maybe Prelude.Text)
listDeviceFleets_nameContains = Lens.lens (\ListDeviceFleets' {nameContains} -> nameContains) (\s@ListDeviceFleets' {} a -> s {nameContains = a} :: ListDeviceFleets)

-- | The response from the last list when returning a list large enough to
-- need tokening.
listDeviceFleets_nextToken :: Lens.Lens' ListDeviceFleets (Prelude.Maybe Prelude.Text)
listDeviceFleets_nextToken = Lens.lens (\ListDeviceFleets' {nextToken} -> nextToken) (\s@ListDeviceFleets' {} a -> s {nextToken = a} :: ListDeviceFleets)

-- | The column to sort by.
listDeviceFleets_sortBy :: Lens.Lens' ListDeviceFleets (Prelude.Maybe ListDeviceFleetsSortBy)
listDeviceFleets_sortBy = Lens.lens (\ListDeviceFleets' {sortBy} -> sortBy) (\s@ListDeviceFleets' {} a -> s {sortBy = a} :: ListDeviceFleets)

-- | What direction to sort in.
listDeviceFleets_sortOrder :: Lens.Lens' ListDeviceFleets (Prelude.Maybe SortOrder)
listDeviceFleets_sortOrder = Lens.lens (\ListDeviceFleets' {sortOrder} -> sortOrder) (\s@ListDeviceFleets' {} a -> s {sortOrder = a} :: ListDeviceFleets)

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
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "DeviceFleetSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListDeviceFleets where
  hashWithSalt _salt ListDeviceFleets' {..} =
    _salt `Prelude.hashWithSalt` creationTimeAfter
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` lastModifiedTimeAfter
      `Prelude.hashWithSalt` lastModifiedTimeBefore
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nameContains
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData ListDeviceFleets where
  rnf ListDeviceFleets' {..} =
    Prelude.rnf creationTimeAfter
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf lastModifiedTimeAfter
      `Prelude.seq` Prelude.rnf lastModifiedTimeBefore
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder

instance Data.ToHeaders ListDeviceFleets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.ListDeviceFleets" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDeviceFleets where
  toJSON ListDeviceFleets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreationTimeAfter" Data..=)
              Prelude.<$> creationTimeAfter,
            ("CreationTimeBefore" Data..=)
              Prelude.<$> creationTimeBefore,
            ("LastModifiedTimeAfter" Data..=)
              Prelude.<$> lastModifiedTimeAfter,
            ("LastModifiedTimeBefore" Data..=)
              Prelude.<$> lastModifiedTimeBefore,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NameContains" Data..=) Prelude.<$> nameContains,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder
          ]
      )

instance Data.ToPath ListDeviceFleets where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDeviceFleets where
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
