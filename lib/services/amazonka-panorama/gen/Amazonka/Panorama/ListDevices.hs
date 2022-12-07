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
-- Module      : Amazonka.Panorama.ListDevices
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of devices.
module Amazonka.Panorama.ListDevices
  ( -- * Creating a Request
    ListDevices (..),
    newListDevices,

    -- * Request Lenses
    listDevices_nameFilter,
    listDevices_sortOrder,
    listDevices_nextToken,
    listDevices_sortBy,
    listDevices_maxResults,
    listDevices_deviceAggregatedStatusFilter,

    -- * Destructuring the Response
    ListDevicesResponse (..),
    newListDevicesResponse,

    -- * Response Lenses
    listDevicesResponse_nextToken,
    listDevicesResponse_httpStatus,
    listDevicesResponse_devices,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Panorama.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDevices' smart constructor.
data ListDevices = ListDevices'
  { -- | Filter based on device\'s name. Prefixes supported.
    nameFilter :: Prelude.Maybe Prelude.Text,
    -- | The sorting order for the returned list. SortOrder is DESCENDING by
    -- default based on CREATED_TIME. Otherwise, SortOrder is ASCENDING.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | Specify the pagination token from a previous request to retrieve the
    -- next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The target column to be sorted on. Default column sort is CREATED_TIME.
    sortBy :: Prelude.Maybe ListDevicesSortBy,
    -- | The maximum number of devices to return in one page of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Filter based on a device\'s status.
    deviceAggregatedStatusFilter :: Prelude.Maybe DeviceAggregatedStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDevices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nameFilter', 'listDevices_nameFilter' - Filter based on device\'s name. Prefixes supported.
--
-- 'sortOrder', 'listDevices_sortOrder' - The sorting order for the returned list. SortOrder is DESCENDING by
-- default based on CREATED_TIME. Otherwise, SortOrder is ASCENDING.
--
-- 'nextToken', 'listDevices_nextToken' - Specify the pagination token from a previous request to retrieve the
-- next page of results.
--
-- 'sortBy', 'listDevices_sortBy' - The target column to be sorted on. Default column sort is CREATED_TIME.
--
-- 'maxResults', 'listDevices_maxResults' - The maximum number of devices to return in one page of results.
--
-- 'deviceAggregatedStatusFilter', 'listDevices_deviceAggregatedStatusFilter' - Filter based on a device\'s status.
newListDevices ::
  ListDevices
newListDevices =
  ListDevices'
    { nameFilter = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      deviceAggregatedStatusFilter = Prelude.Nothing
    }

-- | Filter based on device\'s name. Prefixes supported.
listDevices_nameFilter :: Lens.Lens' ListDevices (Prelude.Maybe Prelude.Text)
listDevices_nameFilter = Lens.lens (\ListDevices' {nameFilter} -> nameFilter) (\s@ListDevices' {} a -> s {nameFilter = a} :: ListDevices)

-- | The sorting order for the returned list. SortOrder is DESCENDING by
-- default based on CREATED_TIME. Otherwise, SortOrder is ASCENDING.
listDevices_sortOrder :: Lens.Lens' ListDevices (Prelude.Maybe SortOrder)
listDevices_sortOrder = Lens.lens (\ListDevices' {sortOrder} -> sortOrder) (\s@ListDevices' {} a -> s {sortOrder = a} :: ListDevices)

-- | Specify the pagination token from a previous request to retrieve the
-- next page of results.
listDevices_nextToken :: Lens.Lens' ListDevices (Prelude.Maybe Prelude.Text)
listDevices_nextToken = Lens.lens (\ListDevices' {nextToken} -> nextToken) (\s@ListDevices' {} a -> s {nextToken = a} :: ListDevices)

-- | The target column to be sorted on. Default column sort is CREATED_TIME.
listDevices_sortBy :: Lens.Lens' ListDevices (Prelude.Maybe ListDevicesSortBy)
listDevices_sortBy = Lens.lens (\ListDevices' {sortBy} -> sortBy) (\s@ListDevices' {} a -> s {sortBy = a} :: ListDevices)

-- | The maximum number of devices to return in one page of results.
listDevices_maxResults :: Lens.Lens' ListDevices (Prelude.Maybe Prelude.Natural)
listDevices_maxResults = Lens.lens (\ListDevices' {maxResults} -> maxResults) (\s@ListDevices' {} a -> s {maxResults = a} :: ListDevices)

-- | Filter based on a device\'s status.
listDevices_deviceAggregatedStatusFilter :: Lens.Lens' ListDevices (Prelude.Maybe DeviceAggregatedStatus)
listDevices_deviceAggregatedStatusFilter = Lens.lens (\ListDevices' {deviceAggregatedStatusFilter} -> deviceAggregatedStatusFilter) (\s@ListDevices' {} a -> s {deviceAggregatedStatusFilter = a} :: ListDevices)

instance Core.AWSRequest ListDevices where
  type AWSResponse ListDevices = ListDevicesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDevicesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Devices" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListDevices where
  hashWithSalt _salt ListDevices' {..} =
    _salt `Prelude.hashWithSalt` nameFilter
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` deviceAggregatedStatusFilter

instance Prelude.NFData ListDevices where
  rnf ListDevices' {..} =
    Prelude.rnf nameFilter
      `Prelude.seq` Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf deviceAggregatedStatusFilter

instance Data.ToHeaders ListDevices where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListDevices where
  toPath = Prelude.const "/devices"

instance Data.ToQuery ListDevices where
  toQuery ListDevices' {..} =
    Prelude.mconcat
      [ "NameFilter" Data.=: nameFilter,
        "SortOrder" Data.=: sortOrder,
        "NextToken" Data.=: nextToken,
        "SortBy" Data.=: sortBy,
        "MaxResults" Data.=: maxResults,
        "DeviceAggregatedStatusFilter"
          Data.=: deviceAggregatedStatusFilter
      ]

-- | /See:/ 'newListDevicesResponse' smart constructor.
data ListDevicesResponse = ListDevicesResponse'
  { -- | A pagination token that\'s included if more results are available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of devices.
    devices :: [Device]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDevicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDevicesResponse_nextToken' - A pagination token that\'s included if more results are available.
--
-- 'httpStatus', 'listDevicesResponse_httpStatus' - The response's http status code.
--
-- 'devices', 'listDevicesResponse_devices' - A list of devices.
newListDevicesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDevicesResponse
newListDevicesResponse pHttpStatus_ =
  ListDevicesResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      devices = Prelude.mempty
    }

-- | A pagination token that\'s included if more results are available.
listDevicesResponse_nextToken :: Lens.Lens' ListDevicesResponse (Prelude.Maybe Prelude.Text)
listDevicesResponse_nextToken = Lens.lens (\ListDevicesResponse' {nextToken} -> nextToken) (\s@ListDevicesResponse' {} a -> s {nextToken = a} :: ListDevicesResponse)

-- | The response's http status code.
listDevicesResponse_httpStatus :: Lens.Lens' ListDevicesResponse Prelude.Int
listDevicesResponse_httpStatus = Lens.lens (\ListDevicesResponse' {httpStatus} -> httpStatus) (\s@ListDevicesResponse' {} a -> s {httpStatus = a} :: ListDevicesResponse)

-- | A list of devices.
listDevicesResponse_devices :: Lens.Lens' ListDevicesResponse [Device]
listDevicesResponse_devices = Lens.lens (\ListDevicesResponse' {devices} -> devices) (\s@ListDevicesResponse' {} a -> s {devices = a} :: ListDevicesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListDevicesResponse where
  rnf ListDevicesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf devices
