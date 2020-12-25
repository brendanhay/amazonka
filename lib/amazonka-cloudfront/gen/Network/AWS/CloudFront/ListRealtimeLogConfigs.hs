{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    lrlcrrsRealtimeLogConfigs,
    lrlcrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListRealtimeLogConfigs' smart constructor.
data ListRealtimeLogConfigs = ListRealtimeLogConfigs'
  { -- | Use this field when paginating results to indicate where to begin in your list of real-time log configurations. The response includes real-time log configurations in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
    marker :: Core.Maybe Types.String,
    -- | The maximum number of real-time log configurations that you want in the response.
    maxItems :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRealtimeLogConfigs' value with any optional fields omitted.
mkListRealtimeLogConfigs ::
  ListRealtimeLogConfigs
mkListRealtimeLogConfigs =
  ListRealtimeLogConfigs'
    { marker = Core.Nothing,
      maxItems = Core.Nothing
    }

-- | Use this field when paginating results to indicate where to begin in your list of real-time log configurations. The response includes real-time log configurations in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrlcMarker :: Lens.Lens' ListRealtimeLogConfigs (Core.Maybe Types.String)
lrlcMarker = Lens.field @"marker"
{-# DEPRECATED lrlcMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of real-time log configurations that you want in the response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrlcMaxItems :: Lens.Lens' ListRealtimeLogConfigs (Core.Maybe Types.String)
lrlcMaxItems = Lens.field @"maxItems"
{-# DEPRECATED lrlcMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Core.AWSRequest ListRealtimeLogConfigs where
  type Rs ListRealtimeLogConfigs = ListRealtimeLogConfigsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/2020-05-31/realtime-log-config",
        Core._rqQuery =
          Core.toQueryValue "Marker" Core.<$> marker
            Core.<> (Core.toQueryValue "MaxItems" Core.<$> maxItems),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ListRealtimeLogConfigsResponse'
            Core.<$> (Core.parseXML x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListRealtimeLogConfigsResponse' smart constructor.
data ListRealtimeLogConfigsResponse = ListRealtimeLogConfigsResponse'
  { -- | A list of real-time log configurations.
    realtimeLogConfigs :: Core.Maybe Types.RealtimeLogConfigs,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRealtimeLogConfigsResponse' value with any optional fields omitted.
mkListRealtimeLogConfigsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListRealtimeLogConfigsResponse
mkListRealtimeLogConfigsResponse responseStatus =
  ListRealtimeLogConfigsResponse'
    { realtimeLogConfigs =
        Core.Nothing,
      responseStatus
    }

-- | A list of real-time log configurations.
--
-- /Note:/ Consider using 'realtimeLogConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrlcrrsRealtimeLogConfigs :: Lens.Lens' ListRealtimeLogConfigsResponse (Core.Maybe Types.RealtimeLogConfigs)
lrlcrrsRealtimeLogConfigs = Lens.field @"realtimeLogConfigs"
{-# DEPRECATED lrlcrrsRealtimeLogConfigs "Use generic-lens or generic-optics with 'realtimeLogConfigs' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrlcrrsResponseStatus :: Lens.Lens' ListRealtimeLogConfigsResponse Core.Int
lrlcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lrlcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
