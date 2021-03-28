{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.GetRecommenderConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about an Amazon Pinpoint configuration for a recommender model.
module Network.AWS.Pinpoint.GetRecommenderConfiguration
    (
    -- * Creating a request
      GetRecommenderConfiguration (..)
    , mkGetRecommenderConfiguration
    -- ** Request lenses
    , grcRecommenderId

    -- * Destructuring the response
    , GetRecommenderConfigurationResponse (..)
    , mkGetRecommenderConfigurationResponse
    -- ** Response lenses
    , grcrfrsRecommenderConfigurationResponse
    , grcrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetRecommenderConfiguration' smart constructor.
newtype GetRecommenderConfiguration = GetRecommenderConfiguration'
  { recommenderId :: Core.Text
    -- ^ The unique identifier for the recommender model configuration. This identifier is displayed as the __Recommender ID__ on the Amazon Pinpoint console.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetRecommenderConfiguration' value with any optional fields omitted.
mkGetRecommenderConfiguration
    :: Core.Text -- ^ 'recommenderId'
    -> GetRecommenderConfiguration
mkGetRecommenderConfiguration recommenderId
  = GetRecommenderConfiguration'{recommenderId}

-- | The unique identifier for the recommender model configuration. This identifier is displayed as the __Recommender ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'recommenderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcRecommenderId :: Lens.Lens' GetRecommenderConfiguration Core.Text
grcRecommenderId = Lens.field @"recommenderId"
{-# INLINEABLE grcRecommenderId #-}
{-# DEPRECATED recommenderId "Use generic-lens or generic-optics with 'recommenderId' instead"  #-}

instance Core.ToQuery GetRecommenderConfiguration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetRecommenderConfiguration where
        toHeaders GetRecommenderConfiguration{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetRecommenderConfiguration where
        type Rs GetRecommenderConfiguration =
             GetRecommenderConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/v1/recommenders/" Core.<> Core.toText recommenderId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetRecommenderConfigurationResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetRecommenderConfigurationResponse' smart constructor.
data GetRecommenderConfigurationResponse = GetRecommenderConfigurationResponse'
  { recommenderConfigurationResponse :: Types.RecommenderConfigurationResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRecommenderConfigurationResponse' value with any optional fields omitted.
mkGetRecommenderConfigurationResponse
    :: Types.RecommenderConfigurationResponse -- ^ 'recommenderConfigurationResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> GetRecommenderConfigurationResponse
mkGetRecommenderConfigurationResponse
  recommenderConfigurationResponse responseStatus
  = GetRecommenderConfigurationResponse'{recommenderConfigurationResponse,
                                         responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'recommenderConfigurationResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcrfrsRecommenderConfigurationResponse :: Lens.Lens' GetRecommenderConfigurationResponse Types.RecommenderConfigurationResponse
grcrfrsRecommenderConfigurationResponse = Lens.field @"recommenderConfigurationResponse"
{-# INLINEABLE grcrfrsRecommenderConfigurationResponse #-}
{-# DEPRECATED recommenderConfigurationResponse "Use generic-lens or generic-optics with 'recommenderConfigurationResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcrfrsResponseStatus :: Lens.Lens' GetRecommenderConfigurationResponse Core.Int
grcrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grcrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
