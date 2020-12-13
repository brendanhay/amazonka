{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.SearchDevices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches devices and lists the ones that meet a set of filter criteria.
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.SearchDevices
  ( -- * Creating a request
    SearchDevices (..),
    mkSearchDevices,

    -- ** Request lenses
    sdFilters,
    sdSortCriteria,
    sdNextToken,
    sdMaxResults,

    -- * Destructuring the response
    SearchDevicesResponse (..),
    mkSearchDevicesResponse,

    -- ** Response lenses
    sdrsNextToken,
    sdrsDevices,
    sdrsTotalCount,
    sdrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSearchDevices' smart constructor.
data SearchDevices = SearchDevices'
  { -- | The filters to use to list a specified set of devices. Supported filter keys are DeviceName, DeviceStatus, DeviceStatusDetailCode, RoomName, DeviceType, DeviceSerialNumber, UnassociatedOnly, ConnectionStatus (ONLINE and OFFLINE), NetworkProfileName, NetworkProfileArn, Feature, and FailureCode.
    filters :: Lude.Maybe [Filter],
    -- | The sort order to use in listing the specified set of devices. Supported sort keys are DeviceName, DeviceStatus, RoomName, DeviceType, DeviceSerialNumber, ConnectionStatus, NetworkProfileName, NetworkProfileArn, Feature, and FailureCode.
    sortCriteria :: Lude.Maybe [Sort],
    -- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
    nextToken :: Lude.Maybe Lude.Text,
    -- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchDevices' with the minimum fields required to make a request.
--
-- * 'filters' - The filters to use to list a specified set of devices. Supported filter keys are DeviceName, DeviceStatus, DeviceStatusDetailCode, RoomName, DeviceType, DeviceSerialNumber, UnassociatedOnly, ConnectionStatus (ONLINE and OFFLINE), NetworkProfileName, NetworkProfileArn, Feature, and FailureCode.
-- * 'sortCriteria' - The sort order to use in listing the specified set of devices. Supported sort keys are DeviceName, DeviceStatus, RoomName, DeviceType, DeviceSerialNumber, ConnectionStatus, NetworkProfileName, NetworkProfileArn, Feature, and FailureCode.
-- * 'nextToken' - An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
-- * 'maxResults' - The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
mkSearchDevices ::
  SearchDevices
mkSearchDevices =
  SearchDevices'
    { filters = Lude.Nothing,
      sortCriteria = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The filters to use to list a specified set of devices. Supported filter keys are DeviceName, DeviceStatus, DeviceStatusDetailCode, RoomName, DeviceType, DeviceSerialNumber, UnassociatedOnly, ConnectionStatus (ONLINE and OFFLINE), NetworkProfileName, NetworkProfileArn, Feature, and FailureCode.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdFilters :: Lens.Lens' SearchDevices (Lude.Maybe [Filter])
sdFilters = Lens.lens (filters :: SearchDevices -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: SearchDevices)
{-# DEPRECATED sdFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The sort order to use in listing the specified set of devices. Supported sort keys are DeviceName, DeviceStatus, RoomName, DeviceType, DeviceSerialNumber, ConnectionStatus, NetworkProfileName, NetworkProfileArn, Feature, and FailureCode.
--
-- /Note:/ Consider using 'sortCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdSortCriteria :: Lens.Lens' SearchDevices (Lude.Maybe [Sort])
sdSortCriteria = Lens.lens (sortCriteria :: SearchDevices -> Lude.Maybe [Sort]) (\s a -> s {sortCriteria = a} :: SearchDevices)
{-# DEPRECATED sdSortCriteria "Use generic-lens or generic-optics with 'sortCriteria' instead." #-}

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdNextToken :: Lens.Lens' SearchDevices (Lude.Maybe Lude.Text)
sdNextToken = Lens.lens (nextToken :: SearchDevices -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: SearchDevices)
{-# DEPRECATED sdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdMaxResults :: Lens.Lens' SearchDevices (Lude.Maybe Lude.Natural)
sdMaxResults = Lens.lens (maxResults :: SearchDevices -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: SearchDevices)
{-# DEPRECATED sdMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager SearchDevices where
  page rq rs
    | Page.stop (rs Lens.^. sdrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. sdrsDevices) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& sdNextToken Lens..~ rs Lens.^. sdrsNextToken

instance Lude.AWSRequest SearchDevices where
  type Rs SearchDevices = SearchDevicesResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          SearchDevicesResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Devices" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "TotalCount")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SearchDevices where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.SearchDevices" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SearchDevices where
  toJSON SearchDevices' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("SortCriteria" Lude..=) Lude.<$> sortCriteria,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath SearchDevices where
  toPath = Lude.const "/"

instance Lude.ToQuery SearchDevices where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSearchDevicesResponse' smart constructor.
data SearchDevicesResponse = SearchDevicesResponse'
  { -- | The token returned to indicate that there is more data available.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The devices that meet the specified set of filter criteria, in sort order.
    devices :: Lude.Maybe [DeviceData],
    -- | The total number of devices returned.
    totalCount :: Lude.Maybe Lude.Int,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchDevicesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token returned to indicate that there is more data available.
-- * 'devices' - The devices that meet the specified set of filter criteria, in sort order.
-- * 'totalCount' - The total number of devices returned.
-- * 'responseStatus' - The response status code.
mkSearchDevicesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SearchDevicesResponse
mkSearchDevicesResponse pResponseStatus_ =
  SearchDevicesResponse'
    { nextToken = Lude.Nothing,
      devices = Lude.Nothing,
      totalCount = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token returned to indicate that there is more data available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdrsNextToken :: Lens.Lens' SearchDevicesResponse (Lude.Maybe Lude.Text)
sdrsNextToken = Lens.lens (nextToken :: SearchDevicesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: SearchDevicesResponse)
{-# DEPRECATED sdrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The devices that meet the specified set of filter criteria, in sort order.
--
-- /Note:/ Consider using 'devices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdrsDevices :: Lens.Lens' SearchDevicesResponse (Lude.Maybe [DeviceData])
sdrsDevices = Lens.lens (devices :: SearchDevicesResponse -> Lude.Maybe [DeviceData]) (\s a -> s {devices = a} :: SearchDevicesResponse)
{-# DEPRECATED sdrsDevices "Use generic-lens or generic-optics with 'devices' instead." #-}

-- | The total number of devices returned.
--
-- /Note:/ Consider using 'totalCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdrsTotalCount :: Lens.Lens' SearchDevicesResponse (Lude.Maybe Lude.Int)
sdrsTotalCount = Lens.lens (totalCount :: SearchDevicesResponse -> Lude.Maybe Lude.Int) (\s a -> s {totalCount = a} :: SearchDevicesResponse)
{-# DEPRECATED sdrsTotalCount "Use generic-lens or generic-optics with 'totalCount' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdrsResponseStatus :: Lens.Lens' SearchDevicesResponse Lude.Int
sdrsResponseStatus = Lens.lens (responseStatus :: SearchDevicesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SearchDevicesResponse)
{-# DEPRECATED sdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
