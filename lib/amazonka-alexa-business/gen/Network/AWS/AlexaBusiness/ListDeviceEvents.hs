{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.ListDeviceEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the device event history, including device connection status, for up to 30 days.
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.ListDeviceEvents
  ( -- * Creating a request
    ListDeviceEvents (..),
    mkListDeviceEvents,

    -- ** Request lenses
    ldeDeviceARN,
    ldeNextToken,
    ldeEventType,
    ldeMaxResults,

    -- * Destructuring the response
    ListDeviceEventsResponse (..),
    mkListDeviceEventsResponse,

    -- ** Response lenses
    ldersNextToken,
    ldersDeviceEvents,
    ldersResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListDeviceEvents' smart constructor.
data ListDeviceEvents = ListDeviceEvents'
  { -- | The ARN of a device.
    deviceARN :: Lude.Text,
    -- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response only includes results beyond the token, up to the value specified by MaxResults. When the end of results is reached, the response has a value of null.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The event type to filter device events. If EventType isn't specified, this returns a list of all device events in reverse chronological order. If EventType is specified, this returns a list of device events for that EventType in reverse chronological order.
    eventType :: Lude.Maybe DeviceEventType,
    -- | The maximum number of results to include in the response. The default value is 50. If more results exist than the specified MaxResults value, a token is included in the response so that the remaining results can be retrieved.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDeviceEvents' with the minimum fields required to make a request.
--
-- * 'deviceARN' - The ARN of a device.
-- * 'nextToken' - An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response only includes results beyond the token, up to the value specified by MaxResults. When the end of results is reached, the response has a value of null.
-- * 'eventType' - The event type to filter device events. If EventType isn't specified, this returns a list of all device events in reverse chronological order. If EventType is specified, this returns a list of device events for that EventType in reverse chronological order.
-- * 'maxResults' - The maximum number of results to include in the response. The default value is 50. If more results exist than the specified MaxResults value, a token is included in the response so that the remaining results can be retrieved.
mkListDeviceEvents ::
  -- | 'deviceARN'
  Lude.Text ->
  ListDeviceEvents
mkListDeviceEvents pDeviceARN_ =
  ListDeviceEvents'
    { deviceARN = pDeviceARN_,
      nextToken = Lude.Nothing,
      eventType = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The ARN of a device.
--
-- /Note:/ Consider using 'deviceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldeDeviceARN :: Lens.Lens' ListDeviceEvents Lude.Text
ldeDeviceARN = Lens.lens (deviceARN :: ListDeviceEvents -> Lude.Text) (\s a -> s {deviceARN = a} :: ListDeviceEvents)
{-# DEPRECATED ldeDeviceARN "Use generic-lens or generic-optics with 'deviceARN' instead." #-}

-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response only includes results beyond the token, up to the value specified by MaxResults. When the end of results is reached, the response has a value of null.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldeNextToken :: Lens.Lens' ListDeviceEvents (Lude.Maybe Lude.Text)
ldeNextToken = Lens.lens (nextToken :: ListDeviceEvents -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDeviceEvents)
{-# DEPRECATED ldeNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The event type to filter device events. If EventType isn't specified, this returns a list of all device events in reverse chronological order. If EventType is specified, this returns a list of device events for that EventType in reverse chronological order.
--
-- /Note:/ Consider using 'eventType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldeEventType :: Lens.Lens' ListDeviceEvents (Lude.Maybe DeviceEventType)
ldeEventType = Lens.lens (eventType :: ListDeviceEvents -> Lude.Maybe DeviceEventType) (\s a -> s {eventType = a} :: ListDeviceEvents)
{-# DEPRECATED ldeEventType "Use generic-lens or generic-optics with 'eventType' instead." #-}

-- | The maximum number of results to include in the response. The default value is 50. If more results exist than the specified MaxResults value, a token is included in the response so that the remaining results can be retrieved.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldeMaxResults :: Lens.Lens' ListDeviceEvents (Lude.Maybe Lude.Natural)
ldeMaxResults = Lens.lens (maxResults :: ListDeviceEvents -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListDeviceEvents)
{-# DEPRECATED ldeMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListDeviceEvents where
  page rq rs
    | Page.stop (rs Lens.^. ldersNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ldersDeviceEvents) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ldeNextToken Lens..~ rs Lens.^. ldersNextToken

instance Lude.AWSRequest ListDeviceEvents where
  type Rs ListDeviceEvents = ListDeviceEventsResponse
  request = Req.postJSON alexaBusinessService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDeviceEventsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "DeviceEvents" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDeviceEvents where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AlexaForBusiness.ListDeviceEvents" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListDeviceEvents where
  toJSON ListDeviceEvents' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("DeviceArn" Lude..= deviceARN),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("EventType" Lude..=) Lude.<$> eventType,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListDeviceEvents where
  toPath = Lude.const "/"

instance Lude.ToQuery ListDeviceEvents where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListDeviceEventsResponse' smart constructor.
data ListDeviceEventsResponse = ListDeviceEventsResponse'
  { -- | The token returned to indicate that there is more data available.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The device events requested for the device ARN.
    deviceEvents :: Lude.Maybe [DeviceEvent],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDeviceEventsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token returned to indicate that there is more data available.
-- * 'deviceEvents' - The device events requested for the device ARN.
-- * 'responseStatus' - The response status code.
mkListDeviceEventsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDeviceEventsResponse
mkListDeviceEventsResponse pResponseStatus_ =
  ListDeviceEventsResponse'
    { nextToken = Lude.Nothing,
      deviceEvents = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The token returned to indicate that there is more data available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldersNextToken :: Lens.Lens' ListDeviceEventsResponse (Lude.Maybe Lude.Text)
ldersNextToken = Lens.lens (nextToken :: ListDeviceEventsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDeviceEventsResponse)
{-# DEPRECATED ldersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The device events requested for the device ARN.
--
-- /Note:/ Consider using 'deviceEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldersDeviceEvents :: Lens.Lens' ListDeviceEventsResponse (Lude.Maybe [DeviceEvent])
ldersDeviceEvents = Lens.lens (deviceEvents :: ListDeviceEventsResponse -> Lude.Maybe [DeviceEvent]) (\s a -> s {deviceEvents = a} :: ListDeviceEventsResponse)
{-# DEPRECATED ldersDeviceEvents "Use generic-lens or generic-optics with 'deviceEvents' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldersResponseStatus :: Lens.Lens' ListDeviceEventsResponse Lude.Int
ldersResponseStatus = Lens.lens (responseStatus :: ListDeviceEventsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDeviceEventsResponse)
{-# DEPRECATED ldersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
