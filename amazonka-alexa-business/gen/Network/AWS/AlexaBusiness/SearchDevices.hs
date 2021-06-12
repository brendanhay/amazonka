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
-- Module      : Network.AWS.AlexaBusiness.SearchDevices
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches devices and lists the ones that meet a set of filter criteria.
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.SearchDevices
  ( -- * Creating a Request
    SearchDevices (..),
    newSearchDevices,

    -- * Request Lenses
    searchDevices_nextToken,
    searchDevices_sortCriteria,
    searchDevices_maxResults,
    searchDevices_filters,

    -- * Destructuring the Response
    SearchDevicesResponse (..),
    newSearchDevicesResponse,

    -- * Response Lenses
    searchDevicesResponse_nextToken,
    searchDevicesResponse_devices,
    searchDevicesResponse_totalCount,
    searchDevicesResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSearchDevices' smart constructor.
data SearchDevices = SearchDevices'
  { -- | An optional token returned from a prior request. Use this token for
    -- pagination of results from this action. If this parameter is specified,
    -- the response includes only results beyond the token, up to the value
    -- specified by @MaxResults@.
    nextToken :: Core.Maybe Core.Text,
    -- | The sort order to use in listing the specified set of devices. Supported
    -- sort keys are DeviceName, DeviceStatus, RoomName, DeviceType,
    -- DeviceSerialNumber, ConnectionStatus, NetworkProfileName,
    -- NetworkProfileArn, Feature, and FailureCode.
    sortCriteria :: Core.Maybe [Sort],
    -- | The maximum number of results to include in the response. If more
    -- results exist than the specified @MaxResults@ value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Core.Maybe Core.Natural,
    -- | The filters to use to list a specified set of devices. Supported filter
    -- keys are DeviceName, DeviceStatus, DeviceStatusDetailCode, RoomName,
    -- DeviceType, DeviceSerialNumber, UnassociatedOnly, ConnectionStatus
    -- (ONLINE and OFFLINE), NetworkProfileName, NetworkProfileArn, Feature,
    -- and FailureCode.
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SearchDevices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchDevices_nextToken' - An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
--
-- 'sortCriteria', 'searchDevices_sortCriteria' - The sort order to use in listing the specified set of devices. Supported
-- sort keys are DeviceName, DeviceStatus, RoomName, DeviceType,
-- DeviceSerialNumber, ConnectionStatus, NetworkProfileName,
-- NetworkProfileArn, Feature, and FailureCode.
--
-- 'maxResults', 'searchDevices_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
--
-- 'filters', 'searchDevices_filters' - The filters to use to list a specified set of devices. Supported filter
-- keys are DeviceName, DeviceStatus, DeviceStatusDetailCode, RoomName,
-- DeviceType, DeviceSerialNumber, UnassociatedOnly, ConnectionStatus
-- (ONLINE and OFFLINE), NetworkProfileName, NetworkProfileArn, Feature,
-- and FailureCode.
newSearchDevices ::
  SearchDevices
newSearchDevices =
  SearchDevices'
    { nextToken = Core.Nothing,
      sortCriteria = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing
    }

-- | An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
searchDevices_nextToken :: Lens.Lens' SearchDevices (Core.Maybe Core.Text)
searchDevices_nextToken = Lens.lens (\SearchDevices' {nextToken} -> nextToken) (\s@SearchDevices' {} a -> s {nextToken = a} :: SearchDevices)

-- | The sort order to use in listing the specified set of devices. Supported
-- sort keys are DeviceName, DeviceStatus, RoomName, DeviceType,
-- DeviceSerialNumber, ConnectionStatus, NetworkProfileName,
-- NetworkProfileArn, Feature, and FailureCode.
searchDevices_sortCriteria :: Lens.Lens' SearchDevices (Core.Maybe [Sort])
searchDevices_sortCriteria = Lens.lens (\SearchDevices' {sortCriteria} -> sortCriteria) (\s@SearchDevices' {} a -> s {sortCriteria = a} :: SearchDevices) Core.. Lens.mapping Lens._Coerce

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
searchDevices_maxResults :: Lens.Lens' SearchDevices (Core.Maybe Core.Natural)
searchDevices_maxResults = Lens.lens (\SearchDevices' {maxResults} -> maxResults) (\s@SearchDevices' {} a -> s {maxResults = a} :: SearchDevices)

-- | The filters to use to list a specified set of devices. Supported filter
-- keys are DeviceName, DeviceStatus, DeviceStatusDetailCode, RoomName,
-- DeviceType, DeviceSerialNumber, UnassociatedOnly, ConnectionStatus
-- (ONLINE and OFFLINE), NetworkProfileName, NetworkProfileArn, Feature,
-- and FailureCode.
searchDevices_filters :: Lens.Lens' SearchDevices (Core.Maybe [Filter])
searchDevices_filters = Lens.lens (\SearchDevices' {filters} -> filters) (\s@SearchDevices' {} a -> s {filters = a} :: SearchDevices) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager SearchDevices where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchDevicesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? searchDevicesResponse_devices Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& searchDevices_nextToken
          Lens..~ rs
          Lens.^? searchDevicesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest SearchDevices where
  type
    AWSResponse SearchDevices =
      SearchDevicesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchDevicesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "Devices" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "TotalCount")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SearchDevices

instance Core.NFData SearchDevices

instance Core.ToHeaders SearchDevices where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.SearchDevices" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON SearchDevices where
  toJSON SearchDevices' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("SortCriteria" Core..=) Core.<$> sortCriteria,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("Filters" Core..=) Core.<$> filters
          ]
      )

instance Core.ToPath SearchDevices where
  toPath = Core.const "/"

instance Core.ToQuery SearchDevices where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newSearchDevicesResponse' smart constructor.
data SearchDevicesResponse = SearchDevicesResponse'
  { -- | The token returned to indicate that there is more data available.
    nextToken :: Core.Maybe Core.Text,
    -- | The devices that meet the specified set of filter criteria, in sort
    -- order.
    devices :: Core.Maybe [DeviceData],
    -- | The total number of devices returned.
    totalCount :: Core.Maybe Core.Int,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SearchDevicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'searchDevicesResponse_nextToken' - The token returned to indicate that there is more data available.
--
-- 'devices', 'searchDevicesResponse_devices' - The devices that meet the specified set of filter criteria, in sort
-- order.
--
-- 'totalCount', 'searchDevicesResponse_totalCount' - The total number of devices returned.
--
-- 'httpStatus', 'searchDevicesResponse_httpStatus' - The response's http status code.
newSearchDevicesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  SearchDevicesResponse
newSearchDevicesResponse pHttpStatus_ =
  SearchDevicesResponse'
    { nextToken = Core.Nothing,
      devices = Core.Nothing,
      totalCount = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token returned to indicate that there is more data available.
searchDevicesResponse_nextToken :: Lens.Lens' SearchDevicesResponse (Core.Maybe Core.Text)
searchDevicesResponse_nextToken = Lens.lens (\SearchDevicesResponse' {nextToken} -> nextToken) (\s@SearchDevicesResponse' {} a -> s {nextToken = a} :: SearchDevicesResponse)

-- | The devices that meet the specified set of filter criteria, in sort
-- order.
searchDevicesResponse_devices :: Lens.Lens' SearchDevicesResponse (Core.Maybe [DeviceData])
searchDevicesResponse_devices = Lens.lens (\SearchDevicesResponse' {devices} -> devices) (\s@SearchDevicesResponse' {} a -> s {devices = a} :: SearchDevicesResponse) Core.. Lens.mapping Lens._Coerce

-- | The total number of devices returned.
searchDevicesResponse_totalCount :: Lens.Lens' SearchDevicesResponse (Core.Maybe Core.Int)
searchDevicesResponse_totalCount = Lens.lens (\SearchDevicesResponse' {totalCount} -> totalCount) (\s@SearchDevicesResponse' {} a -> s {totalCount = a} :: SearchDevicesResponse)

-- | The response's http status code.
searchDevicesResponse_httpStatus :: Lens.Lens' SearchDevicesResponse Core.Int
searchDevicesResponse_httpStatus = Lens.lens (\SearchDevicesResponse' {httpStatus} -> httpStatus) (\s@SearchDevicesResponse' {} a -> s {httpStatus = a} :: SearchDevicesResponse)

instance Core.NFData SearchDevicesResponse
