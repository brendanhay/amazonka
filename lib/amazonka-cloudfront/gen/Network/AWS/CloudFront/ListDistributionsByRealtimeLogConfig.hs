{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListDistributionsByRealtimeLogConfig (..)
    , mkListDistributionsByRealtimeLogConfig
    -- ** Request lenses
    , ldbrlcMarker
    , ldbrlcMaxItems
    , ldbrlcRealtimeLogConfigArn
    , ldbrlcRealtimeLogConfigName

    -- * Destructuring the response
    , ListDistributionsByRealtimeLogConfigResponse (..)
    , mkListDistributionsByRealtimeLogConfigResponse
    -- ** Response lenses
    , ldbrlcrrsDistributionList
    , ldbrlcrrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListDistributionsByRealtimeLogConfig' smart constructor.
data ListDistributionsByRealtimeLogConfig = ListDistributionsByRealtimeLogConfig'
  { marker :: Core.Maybe Core.Text
    -- ^ Use this field when paginating results to indicate where to begin in your list of distributions. The response includes distributions in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
  , maxItems :: Core.Maybe Core.Text
    -- ^ The maximum number of distributions that you want in the response.
  , realtimeLogConfigArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the real-time log configuration whose associated distributions you want to list.
  , realtimeLogConfigName :: Core.Maybe Core.Text
    -- ^ The name of the real-time log configuration whose associated distributions you want to list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDistributionsByRealtimeLogConfig' value with any optional fields omitted.
mkListDistributionsByRealtimeLogConfig
    :: ListDistributionsByRealtimeLogConfig
mkListDistributionsByRealtimeLogConfig
  = ListDistributionsByRealtimeLogConfig'{marker = Core.Nothing,
                                          maxItems = Core.Nothing,
                                          realtimeLogConfigArn = Core.Nothing,
                                          realtimeLogConfigName = Core.Nothing}

-- | Use this field when paginating results to indicate where to begin in your list of distributions. The response includes distributions in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbrlcMarker :: Lens.Lens' ListDistributionsByRealtimeLogConfig (Core.Maybe Core.Text)
ldbrlcMarker = Lens.field @"marker"
{-# INLINEABLE ldbrlcMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of distributions that you want in the response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbrlcMaxItems :: Lens.Lens' ListDistributionsByRealtimeLogConfig (Core.Maybe Core.Text)
ldbrlcMaxItems = Lens.field @"maxItems"
{-# INLINEABLE ldbrlcMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

-- | The Amazon Resource Name (ARN) of the real-time log configuration whose associated distributions you want to list.
--
-- /Note:/ Consider using 'realtimeLogConfigArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbrlcRealtimeLogConfigArn :: Lens.Lens' ListDistributionsByRealtimeLogConfig (Core.Maybe Core.Text)
ldbrlcRealtimeLogConfigArn = Lens.field @"realtimeLogConfigArn"
{-# INLINEABLE ldbrlcRealtimeLogConfigArn #-}
{-# DEPRECATED realtimeLogConfigArn "Use generic-lens or generic-optics with 'realtimeLogConfigArn' instead"  #-}

-- | The name of the real-time log configuration whose associated distributions you want to list.
--
-- /Note:/ Consider using 'realtimeLogConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbrlcRealtimeLogConfigName :: Lens.Lens' ListDistributionsByRealtimeLogConfig (Core.Maybe Core.Text)
ldbrlcRealtimeLogConfigName = Lens.field @"realtimeLogConfigName"
{-# INLINEABLE ldbrlcRealtimeLogConfigName #-}
{-# DEPRECATED realtimeLogConfigName "Use generic-lens or generic-optics with 'realtimeLogConfigName' instead"  #-}

instance Core.ToQuery ListDistributionsByRealtimeLogConfig where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListDistributionsByRealtimeLogConfig where
        toHeaders _ = Core.pure Core.mempty

instance Core.ToXML ListDistributionsByRealtimeLogConfig where
        toXML ListDistributionsByRealtimeLogConfig{..}
          = Core.maybe Core.mempty (Core.toXMLElement "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "MaxItems") maxItems
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "RealtimeLogConfigArn")
                realtimeLogConfigArn
              Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "RealtimeLogConfigName")
                realtimeLogConfigName
        toXMLDocument
          = Core.newXMLDocument
              "{http://cloudfront.amazonaws.com/doc/2020-05-31/}ListDistributionsByRealtimeLogConfigRequest"

instance Core.AWSRequest ListDistributionsByRealtimeLogConfig where
        type Rs ListDistributionsByRealtimeLogConfig =
             ListDistributionsByRealtimeLogConfigResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/2020-05-31/distributionsByRealtimeLogConfig/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toXMLBody (Core.toXMLDocument x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ListDistributionsByRealtimeLogConfigResponse' Core.<$>
                   (Core.parseXML x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListDistributionsByRealtimeLogConfigResponse' smart constructor.
data ListDistributionsByRealtimeLogConfigResponse = ListDistributionsByRealtimeLogConfigResponse'
  { distributionList :: Core.Maybe Types.DistributionList
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListDistributionsByRealtimeLogConfigResponse' value with any optional fields omitted.
mkListDistributionsByRealtimeLogConfigResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListDistributionsByRealtimeLogConfigResponse
mkListDistributionsByRealtimeLogConfigResponse responseStatus
  = ListDistributionsByRealtimeLogConfigResponse'{distributionList =
                                                    Core.Nothing,
                                                  responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'distributionList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbrlcrrsDistributionList :: Lens.Lens' ListDistributionsByRealtimeLogConfigResponse (Core.Maybe Types.DistributionList)
ldbrlcrrsDistributionList = Lens.field @"distributionList"
{-# INLINEABLE ldbrlcrrsDistributionList #-}
{-# DEPRECATED distributionList "Use generic-lens or generic-optics with 'distributionList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldbrlcrrsResponseStatus :: Lens.Lens' ListDistributionsByRealtimeLogConfigResponse Core.Int
ldbrlcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ldbrlcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
