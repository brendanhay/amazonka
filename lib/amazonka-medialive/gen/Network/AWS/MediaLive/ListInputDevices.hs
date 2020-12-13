{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.ListInputDevices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List input devices
--
-- This operation returns paginated results.
module Network.AWS.MediaLive.ListInputDevices
  ( -- * Creating a request
    ListInputDevices (..),
    mkListInputDevices,

    -- ** Request lenses
    lidNextToken,
    lidMaxResults,

    -- * Destructuring the response
    ListInputDevicesResponse (..),
    mkListInputDevicesResponse,

    -- ** Response lenses
    lidrsInputDevices,
    lidrsNextToken,
    lidrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Placeholder documentation for ListInputDevicesRequest
--
-- /See:/ 'mkListInputDevices' smart constructor.
data ListInputDevices = ListInputDevices'
  { nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListInputDevices' with the minimum fields required to make a request.
--
-- * 'nextToken' -
-- * 'maxResults' -
mkListInputDevices ::
  ListInputDevices
mkListInputDevices =
  ListInputDevices'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lidNextToken :: Lens.Lens' ListInputDevices (Lude.Maybe Lude.Text)
lidNextToken = Lens.lens (nextToken :: ListInputDevices -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListInputDevices)
{-# DEPRECATED lidNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lidMaxResults :: Lens.Lens' ListInputDevices (Lude.Maybe Lude.Natural)
lidMaxResults = Lens.lens (maxResults :: ListInputDevices -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListInputDevices)
{-# DEPRECATED lidMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListInputDevices where
  page rq rs
    | Page.stop (rs Lens.^. lidrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lidrsInputDevices) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lidNextToken Lens..~ rs Lens.^. lidrsNextToken

instance Lude.AWSRequest ListInputDevices where
  type Rs ListInputDevices = ListInputDevicesResponse
  request = Req.get mediaLiveService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListInputDevicesResponse'
            Lude.<$> (x Lude..?> "inputDevices" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListInputDevices where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath ListInputDevices where
  toPath = Lude.const "/prod/inputDevices"

instance Lude.ToQuery ListInputDevices where
  toQuery ListInputDevices' {..} =
    Lude.mconcat
      ["nextToken" Lude.=: nextToken, "maxResults" Lude.=: maxResults]

-- | Placeholder documentation for ListInputDevicesResponse
--
-- /See:/ 'mkListInputDevicesResponse' smart constructor.
data ListInputDevicesResponse = ListInputDevicesResponse'
  { -- | The list of input devices.
    inputDevices :: Lude.Maybe [InputDeviceSummary],
    -- | A token to get additional list results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListInputDevicesResponse' with the minimum fields required to make a request.
--
-- * 'inputDevices' - The list of input devices.
-- * 'nextToken' - A token to get additional list results.
-- * 'responseStatus' - The response status code.
mkListInputDevicesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListInputDevicesResponse
mkListInputDevicesResponse pResponseStatus_ =
  ListInputDevicesResponse'
    { inputDevices = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of input devices.
--
-- /Note:/ Consider using 'inputDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lidrsInputDevices :: Lens.Lens' ListInputDevicesResponse (Lude.Maybe [InputDeviceSummary])
lidrsInputDevices = Lens.lens (inputDevices :: ListInputDevicesResponse -> Lude.Maybe [InputDeviceSummary]) (\s a -> s {inputDevices = a} :: ListInputDevicesResponse)
{-# DEPRECATED lidrsInputDevices "Use generic-lens or generic-optics with 'inputDevices' instead." #-}

-- | A token to get additional list results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lidrsNextToken :: Lens.Lens' ListInputDevicesResponse (Lude.Maybe Lude.Text)
lidrsNextToken = Lens.lens (nextToken :: ListInputDevicesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListInputDevicesResponse)
{-# DEPRECATED lidrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lidrsResponseStatus :: Lens.Lens' ListInputDevicesResponse Lude.Int
lidrsResponseStatus = Lens.lens (responseStatus :: ListInputDevicesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListInputDevicesResponse)
{-# DEPRECATED lidrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
