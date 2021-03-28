{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.ListJourneys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the status, configuration, and other settings for all the journeys that are associated with an application.
module Network.AWS.Pinpoint.ListJourneys
    (
    -- * Creating a request
      ListJourneys (..)
    , mkListJourneys
    -- ** Request lenses
    , ljApplicationId
    , ljPageSize
    , ljToken

    -- * Destructuring the response
    , ListJourneysResponse (..)
    , mkListJourneysResponse
    -- ** Response lenses
    , ljrrsJourneysResponse
    , ljrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListJourneys' smart constructor.
data ListJourneys = ListJourneys'
  { applicationId :: Core.Text
    -- ^ The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
  , pageSize :: Core.Maybe Core.Text
    -- ^ The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
  , token :: Core.Maybe Core.Text
    -- ^ The NextToken string that specifies which page of results to return in a paginated response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListJourneys' value with any optional fields omitted.
mkListJourneys
    :: Core.Text -- ^ 'applicationId'
    -> ListJourneys
mkListJourneys applicationId
  = ListJourneys'{applicationId, pageSize = Core.Nothing,
                  token = Core.Nothing}

-- | The unique identifier for the application. This identifier is displayed as the __Project ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljApplicationId :: Lens.Lens' ListJourneys Core.Text
ljApplicationId = Lens.field @"applicationId"
{-# INLINEABLE ljApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | The maximum number of items to include in each page of a paginated response. This parameter is not supported for application, campaign, and journey metrics.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljPageSize :: Lens.Lens' ListJourneys (Core.Maybe Core.Text)
ljPageSize = Lens.field @"pageSize"
{-# INLINEABLE ljPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

-- | The NextToken string that specifies which page of results to return in a paginated response.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljToken :: Lens.Lens' ListJourneys (Core.Maybe Core.Text)
ljToken = Lens.field @"token"
{-# INLINEABLE ljToken #-}
{-# DEPRECATED token "Use generic-lens or generic-optics with 'token' instead"  #-}

instance Core.ToQuery ListJourneys where
        toQuery ListJourneys{..}
          = Core.maybe Core.mempty (Core.toQueryPair "page-size") pageSize
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "token") token

instance Core.ToHeaders ListJourneys where
        toHeaders ListJourneys{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListJourneys where
        type Rs ListJourneys = ListJourneysResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/v1/apps/" Core.<> Core.toText applicationId Core.<> "/journeys",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListJourneysResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListJourneysResponse' smart constructor.
data ListJourneysResponse = ListJourneysResponse'
  { journeysResponse :: Types.JourneysResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListJourneysResponse' value with any optional fields omitted.
mkListJourneysResponse
    :: Types.JourneysResponse -- ^ 'journeysResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> ListJourneysResponse
mkListJourneysResponse journeysResponse responseStatus
  = ListJourneysResponse'{journeysResponse, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'journeysResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrrsJourneysResponse :: Lens.Lens' ListJourneysResponse Types.JourneysResponse
ljrrsJourneysResponse = Lens.field @"journeysResponse"
{-# INLINEABLE ljrrsJourneysResponse #-}
{-# DEPRECATED journeysResponse "Use generic-lens or generic-optics with 'journeysResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljrrsResponseStatus :: Lens.Lens' ListJourneysResponse Core.Int
ljrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ljrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
