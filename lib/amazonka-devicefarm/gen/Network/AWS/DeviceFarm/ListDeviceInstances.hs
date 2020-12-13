{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListDeviceInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the private device instances associated with one or more AWS accounts.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListDeviceInstances
  ( -- * Creating a request
    ListDeviceInstances (..),
    mkListDeviceInstances,

    -- ** Request lenses
    ldiNextToken,
    ldiMaxResults,

    -- * Destructuring the response
    ListDeviceInstancesResponse (..),
    mkListDeviceInstancesResponse,

    -- ** Response lenses
    ldirsNextToken,
    ldirsDeviceInstances,
    ldirsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListDeviceInstances' smart constructor.
data ListDeviceInstances = ListDeviceInstances'
  { -- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | An integer that specifies the maximum number of items you want to return in the API response.
    maxResults :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDeviceInstances' with the minimum fields required to make a request.
--
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
-- * 'maxResults' - An integer that specifies the maximum number of items you want to return in the API response.
mkListDeviceInstances ::
  ListDeviceInstances
mkListDeviceInstances =
  ListDeviceInstances'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldiNextToken :: Lens.Lens' ListDeviceInstances (Lude.Maybe Lude.Text)
ldiNextToken = Lens.lens (nextToken :: ListDeviceInstances -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDeviceInstances)
{-# DEPRECATED ldiNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An integer that specifies the maximum number of items you want to return in the API response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldiMaxResults :: Lens.Lens' ListDeviceInstances (Lude.Maybe Lude.Int)
ldiMaxResults = Lens.lens (maxResults :: ListDeviceInstances -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: ListDeviceInstances)
{-# DEPRECATED ldiMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListDeviceInstances where
  page rq rs
    | Page.stop (rs Lens.^. ldirsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ldirsDeviceInstances) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ldiNextToken Lens..~ rs Lens.^. ldirsNextToken

instance Lude.AWSRequest ListDeviceInstances where
  type Rs ListDeviceInstances = ListDeviceInstancesResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDeviceInstancesResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "deviceInstances" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDeviceInstances where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.ListDeviceInstances" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListDeviceInstances where
  toJSON ListDeviceInstances' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListDeviceInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery ListDeviceInstances where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListDeviceInstancesResponse' smart constructor.
data ListDeviceInstancesResponse = ListDeviceInstancesResponse'
  { -- | An identifier that can be used in the next call to this operation to return the next set of items in the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | An object that contains information about your device instances.
    deviceInstances :: Lude.Maybe [DeviceInstance],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDeviceInstancesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - An identifier that can be used in the next call to this operation to return the next set of items in the list.
-- * 'deviceInstances' - An object that contains information about your device instances.
-- * 'responseStatus' - The response status code.
mkListDeviceInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDeviceInstancesResponse
mkListDeviceInstancesResponse pResponseStatus_ =
  ListDeviceInstancesResponse'
    { nextToken = Lude.Nothing,
      deviceInstances = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An identifier that can be used in the next call to this operation to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldirsNextToken :: Lens.Lens' ListDeviceInstancesResponse (Lude.Maybe Lude.Text)
ldirsNextToken = Lens.lens (nextToken :: ListDeviceInstancesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDeviceInstancesResponse)
{-# DEPRECATED ldirsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | An object that contains information about your device instances.
--
-- /Note:/ Consider using 'deviceInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldirsDeviceInstances :: Lens.Lens' ListDeviceInstancesResponse (Lude.Maybe [DeviceInstance])
ldirsDeviceInstances = Lens.lens (deviceInstances :: ListDeviceInstancesResponse -> Lude.Maybe [DeviceInstance]) (\s a -> s {deviceInstances = a} :: ListDeviceInstancesResponse)
{-# DEPRECATED ldirsDeviceInstances "Use generic-lens or generic-optics with 'deviceInstances' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldirsResponseStatus :: Lens.Lens' ListDeviceInstancesResponse Lude.Int
ldirsResponseStatus = Lens.lens (responseStatus :: ListDeviceInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDeviceInstancesResponse)
{-# DEPRECATED ldirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
