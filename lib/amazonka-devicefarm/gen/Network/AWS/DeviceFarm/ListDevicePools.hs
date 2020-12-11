{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListDevicePools
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about device pools.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListDevicePools
  ( -- * Creating a request
    ListDevicePools (..),
    mkListDevicePools,

    -- ** Request lenses
    ldpNextToken,
    ldpType,
    ldpArn,

    -- * Destructuring the response
    ListDevicePoolsResponse (..),
    mkListDevicePoolsResponse,

    -- ** Response lenses
    ldprsDevicePools,
    ldprsNextToken,
    ldprsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the result of a list device pools request.
--
-- /See:/ 'mkListDevicePools' smart constructor.
data ListDevicePools = ListDevicePools'
  { nextToken ::
      Lude.Maybe Lude.Text,
    type' :: Lude.Maybe DevicePoolType,
    arn :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDevicePools' with the minimum fields required to make a request.
--
-- * 'arn' - The project ARN.
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
-- * 'type'' - The device pools' type.
--
-- Allowed values include:
--
--     * CURATED: A device pool that is created and managed by AWS Device Farm.
--
--
--     * PRIVATE: A device pool that is created and managed by the device pool developer.
mkListDevicePools ::
  -- | 'arn'
  Lude.Text ->
  ListDevicePools
mkListDevicePools pArn_ =
  ListDevicePools'
    { nextToken = Lude.Nothing,
      type' = Lude.Nothing,
      arn = pArn_
    }

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldpNextToken :: Lens.Lens' ListDevicePools (Lude.Maybe Lude.Text)
ldpNextToken = Lens.lens (nextToken :: ListDevicePools -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDevicePools)
{-# DEPRECATED ldpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The device pools' type.
--
-- Allowed values include:
--
--     * CURATED: A device pool that is created and managed by AWS Device Farm.
--
--
--     * PRIVATE: A device pool that is created and managed by the device pool developer.
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldpType :: Lens.Lens' ListDevicePools (Lude.Maybe DevicePoolType)
ldpType = Lens.lens (type' :: ListDevicePools -> Lude.Maybe DevicePoolType) (\s a -> s {type' = a} :: ListDevicePools)
{-# DEPRECATED ldpType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The project ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldpArn :: Lens.Lens' ListDevicePools Lude.Text
ldpArn = Lens.lens (arn :: ListDevicePools -> Lude.Text) (\s a -> s {arn = a} :: ListDevicePools)
{-# DEPRECATED ldpArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Page.AWSPager ListDevicePools where
  page rq rs
    | Page.stop (rs Lens.^. ldprsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ldprsDevicePools) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ldpNextToken Lens..~ rs Lens.^. ldprsNextToken

instance Lude.AWSRequest ListDevicePools where
  type Rs ListDevicePools = ListDevicePoolsResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDevicePoolsResponse'
            Lude.<$> (x Lude..?> "devicePools" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDevicePools where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.ListDevicePools" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListDevicePools where
  toJSON ListDevicePools' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("type" Lude..=) Lude.<$> type',
            Lude.Just ("arn" Lude..= arn)
          ]
      )

instance Lude.ToPath ListDevicePools where
  toPath = Lude.const "/"

instance Lude.ToQuery ListDevicePools where
  toQuery = Lude.const Lude.mempty

-- | Represents the result of a list device pools request.
--
-- /See:/ 'mkListDevicePoolsResponse' smart constructor.
data ListDevicePoolsResponse = ListDevicePoolsResponse'
  { devicePools ::
      Lude.Maybe [DevicePool],
    nextToken :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDevicePoolsResponse' with the minimum fields required to make a request.
--
-- * 'devicePools' - Information about the device pools.
-- * 'nextToken' - If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
-- * 'responseStatus' - The response status code.
mkListDevicePoolsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDevicePoolsResponse
mkListDevicePoolsResponse pResponseStatus_ =
  ListDevicePoolsResponse'
    { devicePools = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the device pools.
--
-- /Note:/ Consider using 'devicePools' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldprsDevicePools :: Lens.Lens' ListDevicePoolsResponse (Lude.Maybe [DevicePool])
ldprsDevicePools = Lens.lens (devicePools :: ListDevicePoolsResponse -> Lude.Maybe [DevicePool]) (\s a -> s {devicePools = a} :: ListDevicePoolsResponse)
{-# DEPRECATED ldprsDevicePools "Use generic-lens or generic-optics with 'devicePools' instead." #-}

-- | If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldprsNextToken :: Lens.Lens' ListDevicePoolsResponse (Lude.Maybe Lude.Text)
ldprsNextToken = Lens.lens (nextToken :: ListDevicePoolsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDevicePoolsResponse)
{-# DEPRECATED ldprsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldprsResponseStatus :: Lens.Lens' ListDevicePoolsResponse Lude.Int
ldprsResponseStatus = Lens.lens (responseStatus :: ListDevicePoolsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDevicePoolsResponse)
{-# DEPRECATED ldprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
