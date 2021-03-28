{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetSegments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the configuration, dimension, and other settings for all the segments that are associated with an application.
module Network.AWS.Pinpoint.GetSegments
    (
    -- * Creating a request
      GetSegments (..)
    , mkGetSegments
    -- ** Request lenses
    , gssApplicationId
    , gssPageSize
    , gssToken

    -- * Destructuring the response
    , GetSegmentsResponse (..)
    , mkGetSegmentsResponse
    -- ** Response lenses
    , gsrrsSegmentsResponse
    , gsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSegments' smart constructor.
data GetSegments = GetSegments'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  , pageSize :: Core.Maybe Core.Text
    -- ^ The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
  , token :: Core.Maybe Core.Text
    -- ^ The NextToken string that specifies which page of results to return in a paginated response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSegments' value with any optional fields omitted.
mkGetSegments
    :: Core.Text -- ^ 'applicationId'
    -> GetSegments
mkGetSegments applicationId
  = GetSegments'{applicationId, pageSize = Core.Nothing,
                 token = Core.Nothing}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssApplicationId :: Lens.Lens' GetSegments Core.Text
gssApplicationId = Lens.field @"applicationId"
{-# INLINEABLE gssApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssPageSize :: Lens.Lens' GetSegments (Core.Maybe Core.Text)
gssPageSize = Lens.field @"pageSize"
{-# INLINEABLE gssPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

-- | The NextToken string that specifies which page of results to return in a paginated response.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gssToken :: Lens.Lens' GetSegments (Core.Maybe Core.Text)
gssToken = Lens.field @"token"
{-# INLINEABLE gssToken #-}
{-# DEPRECATED token "Use generic-lens or generic-optics with 'token' instead"  #-}

instance Core.ToQuery GetSegments where
        toQuery GetSegments{..}
          = Core.maybe Core.mempty (Core.toQueryPair "page-size") pageSize
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "token") token

instance Core.ToHeaders GetSegments where
        toHeaders GetSegments{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetSegments where
        type Rs GetSegments = GetSegmentsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<> "/segments",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetSegmentsResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetSegmentsResponse' smart constructor.
data GetSegmentsResponse = GetSegmentsResponse'
  { segmentsResponse :: Types.SegmentsResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSegmentsResponse' value with any optional fields omitted.
mkGetSegmentsResponse
    :: Types.SegmentsResponse -- ^ 'segmentsResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> GetSegmentsResponse
mkGetSegmentsResponse segmentsResponse responseStatus
  = GetSegmentsResponse'{segmentsResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'segmentsResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsSegmentsResponse :: Lens.Lens' GetSegmentsResponse Types.SegmentsResponse
gsrrsSegmentsResponse = Lens.field @"segmentsResponse"
{-# INLINEABLE gsrrsSegmentsResponse #-}
{-# DEPRECATED segmentsResponse "Use generic-lens or generic-optics with 'segmentsResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsResponseStatus :: Lens.Lens' GetSegmentsResponse Core.Int
gsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
