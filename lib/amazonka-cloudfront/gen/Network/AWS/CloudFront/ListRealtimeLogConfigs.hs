{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListRealtimeLogConfigs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of real-time log configurations.
--
-- You can optionally specify the maximum number of items to receive in the response. If the total number of items in the list exceeds the maximum that you specify, or the default maximum, the response is paginated. To get the next page of items, send a subsequent request that specifies the @NextMarker@ value from the current response as the @Marker@ value in the subsequent request.
module Network.AWS.CloudFront.ListRealtimeLogConfigs
  ( -- * Creating a request
    ListRealtimeLogConfigs (..),
    mkListRealtimeLogConfigs,

    -- ** Request lenses
    lrlcMarker,
    lrlcMaxItems,

    -- * Destructuring the response
    ListRealtimeLogConfigsResponse (..),
    mkListRealtimeLogConfigsResponse,

    -- ** Response lenses
    lrlcrsRealtimeLogConfigs,
    lrlcrsResponseStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListRealtimeLogConfigs' smart constructor.
data ListRealtimeLogConfigs = ListRealtimeLogConfigs'
  { marker ::
      Lude.Maybe Lude.Text,
    maxItems :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListRealtimeLogConfigs' with the minimum fields required to make a request.
--
-- * 'marker' - Use this field when paginating results to indicate where to begin in your list of real-time log configurations. The response includes real-time log configurations in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
-- * 'maxItems' - The maximum number of real-time log configurations that you want in the response.
mkListRealtimeLogConfigs ::
  ListRealtimeLogConfigs
mkListRealtimeLogConfigs =
  ListRealtimeLogConfigs'
    { marker = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | Use this field when paginating results to indicate where to begin in your list of real-time log configurations. The response includes real-time log configurations in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrlcMarker :: Lens.Lens' ListRealtimeLogConfigs (Lude.Maybe Lude.Text)
lrlcMarker = Lens.lens (marker :: ListRealtimeLogConfigs -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListRealtimeLogConfigs)
{-# DEPRECATED lrlcMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of real-time log configurations that you want in the response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrlcMaxItems :: Lens.Lens' ListRealtimeLogConfigs (Lude.Maybe Lude.Text)
lrlcMaxItems = Lens.lens (maxItems :: ListRealtimeLogConfigs -> Lude.Maybe Lude.Text) (\s a -> s {maxItems = a} :: ListRealtimeLogConfigs)
{-# DEPRECATED lrlcMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Lude.AWSRequest ListRealtimeLogConfigs where
  type Rs ListRealtimeLogConfigs = ListRealtimeLogConfigsResponse
  request = Req.get cloudFrontService
  response =
    Res.receiveXML
      ( \s h x ->
          ListRealtimeLogConfigsResponse'
            Lude.<$> (Lude.parseXML x) Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListRealtimeLogConfigs where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListRealtimeLogConfigs where
  toPath = Lude.const "/2020-05-31/realtime-log-config"

instance Lude.ToQuery ListRealtimeLogConfigs where
  toQuery ListRealtimeLogConfigs' {..} =
    Lude.mconcat
      ["Marker" Lude.=: marker, "MaxItems" Lude.=: maxItems]

-- | /See:/ 'mkListRealtimeLogConfigsResponse' smart constructor.
data ListRealtimeLogConfigsResponse = ListRealtimeLogConfigsResponse'
  { realtimeLogConfigs ::
      Lude.Maybe RealtimeLogConfigs,
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

-- | Creates a value of 'ListRealtimeLogConfigsResponse' with the minimum fields required to make a request.
--
-- * 'realtimeLogConfigs' - A list of real-time log configurations.
-- * 'responseStatus' - The response status code.
mkListRealtimeLogConfigsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListRealtimeLogConfigsResponse
mkListRealtimeLogConfigsResponse pResponseStatus_ =
  ListRealtimeLogConfigsResponse'
    { realtimeLogConfigs =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of real-time log configurations.
--
-- /Note:/ Consider using 'realtimeLogConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrlcrsRealtimeLogConfigs :: Lens.Lens' ListRealtimeLogConfigsResponse (Lude.Maybe RealtimeLogConfigs)
lrlcrsRealtimeLogConfigs = Lens.lens (realtimeLogConfigs :: ListRealtimeLogConfigsResponse -> Lude.Maybe RealtimeLogConfigs) (\s a -> s {realtimeLogConfigs = a} :: ListRealtimeLogConfigsResponse)
{-# DEPRECATED lrlcrsRealtimeLogConfigs "Use generic-lens or generic-optics with 'realtimeLogConfigs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrlcrsResponseStatus :: Lens.Lens' ListRealtimeLogConfigsResponse Lude.Int
lrlcrsResponseStatus = Lens.lens (responseStatus :: ListRealtimeLogConfigsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListRealtimeLogConfigsResponse)
{-# DEPRECATED lrlcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
