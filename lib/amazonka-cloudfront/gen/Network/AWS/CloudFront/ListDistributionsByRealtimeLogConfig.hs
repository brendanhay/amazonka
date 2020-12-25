{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListDistributionsByRealtimeLogConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of distributions that have a cache behavior that’s associated with the specified real-time log configuration.
--
-- You can specify the real-time log configuration by its name or its Amazon Resource Name (ARN). You must provide at least one. If you provide both, CloudFront uses the name to identify the real-time log configuration to list distributions for.
-- You can optionally specify the maximum number of items to receive in the response. If the total number of items in the list exceeds the maximum that you specify, or the default maximum, the response is paginated. To get the next page of items, send a subsequent request that specifies the @NextMarker@ value from the current response as the @Marker@ value in the subsequent request.
module Network.AWS.CloudFront.ListDistributionsByRealtimeLogConfig
  ( -- * Creating a request
    ListDistributionsByRealtimeLogConfig (..),
    mkListDistributionsByRealtimeLogConfig,

    -- ** Request lenses
    ldbrlcMarker,
    ldbrlcMaxItems,
    ldbrlcRealtimeLogConfigArn,
    ldbrlcRealtimeLogConfigName,

    -- * Destructuring the response
    ListDistributionsByRealtimeLogConfigResponse (..),
    mkListDistributionsByRealtimeLogConfigResponse,

    -- ** Response lenses
    ldbrlcrrsDistributionList,
    ldbrlcrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListDistributionsByRealtimeLogConfig' smart constructor.
data ListDistributionsByRealtimeLogConfig = ListDistributionsByRealtimeLogConfig'
  { -- | Use this field when paginating results to indicate where to begin in your list of distributions. The response includes distributions in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
    marker :: Core.Maybe Types.String,
    -- | The maximum number of distributions that you want in the response.
    maxItems :: Core.Maybe Types.String,
    -- | The Amazon Resource Name (ARN) of the real-time log configuration whose associated distributions you want to list.
    realtimeLogConfigArn :: Core.Maybe Types.String,
    -- | The name of the real-time log configuration whose associated distributions you want to list.
    realtimeLogConfigName :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDistributionsByRealtimeLogConfig' value with any optional fields omitted.
mkListDistributionsByRealtimeLogConfig ::
  ListDistributionsByRealtimeLogConfig
mkListDistributionsByRealtimeLogConfig =
  ListDistributionsByRealtimeLogConfig'
    { marker = Core.Nothing,
      maxItems = Core.Nothing,
      realtimeLogConfigArn = Core.Nothing,
      realtimeLogConfigName = Core.Nothing
    }

-- | Use this field when paginating results to indicate where to begin in your list of distributions. The response includes distributions in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbrlcMarker :: Lens.Lens' ListDistributionsByRealtimeLogConfig (Core.Maybe Types.String)
ldbrlcMarker = Lens.field @"marker"
{-# DEPRECATED ldbrlcMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of distributions that you want in the response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbrlcMaxItems :: Lens.Lens' ListDistributionsByRealtimeLogConfig (Core.Maybe Types.String)
ldbrlcMaxItems = Lens.field @"maxItems"
{-# DEPRECATED ldbrlcMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The Amazon Resource Name (ARN) of the real-time log configuration whose associated distributions you want to list.
--
-- /Note:/ Consider using 'realtimeLogConfigArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbrlcRealtimeLogConfigArn :: Lens.Lens' ListDistributionsByRealtimeLogConfig (Core.Maybe Types.String)
ldbrlcRealtimeLogConfigArn = Lens.field @"realtimeLogConfigArn"
{-# DEPRECATED ldbrlcRealtimeLogConfigArn "Use generic-lens or generic-optics with 'realtimeLogConfigArn' instead." #-}

-- | The name of the real-time log configuration whose associated distributions you want to list.
--
-- /Note:/ Consider using 'realtimeLogConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbrlcRealtimeLogConfigName :: Lens.Lens' ListDistributionsByRealtimeLogConfig (Core.Maybe Types.String)
ldbrlcRealtimeLogConfigName = Lens.field @"realtimeLogConfigName"
{-# DEPRECATED ldbrlcRealtimeLogConfigName "Use generic-lens or generic-optics with 'realtimeLogConfigName' instead." #-}

instance Core.ToXML ListDistributionsByRealtimeLogConfig where
  toXML ListDistributionsByRealtimeLogConfig {..} =
    Core.toXMLNode "Marker" Core.<$> marker
      Core.<> Core.toXMLNode "MaxItems"
      Core.<$> maxItems
      Core.<> Core.toXMLNode "RealtimeLogConfigArn"
      Core.<$> realtimeLogConfigArn
      Core.<> Core.toXMLNode "RealtimeLogConfigName"
      Core.<$> realtimeLogConfigName
  toXMLDocument =
    Core.mkXMLElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}ListDistributionsByRealtimeLogConfigRequest"

instance Core.AWSRequest ListDistributionsByRealtimeLogConfig where
  type
    Rs ListDistributionsByRealtimeLogConfig =
      ListDistributionsByRealtimeLogConfigResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath "/2020-05-31/distributionsByRealtimeLogConfig/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toXMLBody x
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ListDistributionsByRealtimeLogConfigResponse'
            Core.<$> (Core.parseXML x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListDistributionsByRealtimeLogConfigResponse' smart constructor.
data ListDistributionsByRealtimeLogConfigResponse = ListDistributionsByRealtimeLogConfigResponse'
  { distributionList :: Core.Maybe Types.DistributionList,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListDistributionsByRealtimeLogConfigResponse' value with any optional fields omitted.
mkListDistributionsByRealtimeLogConfigResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListDistributionsByRealtimeLogConfigResponse
mkListDistributionsByRealtimeLogConfigResponse responseStatus =
  ListDistributionsByRealtimeLogConfigResponse'
    { distributionList =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'distributionList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbrlcrrsDistributionList :: Lens.Lens' ListDistributionsByRealtimeLogConfigResponse (Core.Maybe Types.DistributionList)
ldbrlcrrsDistributionList = Lens.field @"distributionList"
{-# DEPRECATED ldbrlcrrsDistributionList "Use generic-lens or generic-optics with 'distributionList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbrlcrrsResponseStatus :: Lens.Lens' ListDistributionsByRealtimeLogConfigResponse Core.Int
ldbrlcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ldbrlcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
