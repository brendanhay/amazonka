{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.UpdateRecommenderConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an Amazon Pinpoint configuration for a recommender model.
module Network.AWS.Pinpoint.UpdateRecommenderConfiguration
    (
    -- * Creating a request
      UpdateRecommenderConfiguration (..)
    , mkUpdateRecommenderConfiguration
    -- ** Request lenses
    , urcRecommenderId
    , urcUpdateRecommenderConfiguration

    -- * Destructuring the response
    , UpdateRecommenderConfigurationResponse (..)
    , mkUpdateRecommenderConfigurationResponse
    -- ** Response lenses
    , urcrrsRecommenderConfigurationResponse
    , urcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateRecommenderConfiguration' smart constructor.
data UpdateRecommenderConfiguration = UpdateRecommenderConfiguration'
  { recommenderId :: Core.Text
    -- ^ The unique identifier for the recommender model configuration. This identifier is displayed as the __Recommender ID__ on the Amazon Pinpoint console.
  , updateRecommenderConfiguration :: UpdateRecommenderConfiguration
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRecommenderConfiguration' value with any optional fields omitted.
mkUpdateRecommenderConfiguration
    :: Core.Text -- ^ 'recommenderId'
    -> Types.UpdateRecommenderConfiguration -- ^ 'updateRecommenderConfiguration'
    -> UpdateRecommenderConfiguration
mkUpdateRecommenderConfiguration recommenderId
  updateRecommenderConfiguration
  = UpdateRecommenderConfiguration'{recommenderId,
                                    updateRecommenderConfiguration}

-- | The unique identifier for the recommender model configuration. This identifier is displayed as the __Recommender ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'recommenderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urcRecommenderId :: Lens.Lens' UpdateRecommenderConfiguration Core.Text
urcRecommenderId = Lens.field @"recommenderId"
{-# INLINEABLE urcRecommenderId #-}
{-# DEPRECATED recommenderId "Use generic-lens or generic-optics with 'recommenderId' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'updateRecommenderConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urcUpdateRecommenderConfiguration :: Lens.Lens' UpdateRecommenderConfiguration UpdateRecommenderConfiguration
urcUpdateRecommenderConfiguration = Lens.field @"updateRecommenderConfiguration"
{-# INLINEABLE urcUpdateRecommenderConfiguration #-}
{-# DEPRECATED updateRecommenderConfiguration "Use generic-lens or generic-optics with 'updateRecommenderConfiguration' instead"  #-}

instance Core.ToQuery UpdateRecommenderConfiguration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateRecommenderConfiguration where
        toHeaders UpdateRecommenderConfiguration{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateRecommenderConfiguration where
        toJSON UpdateRecommenderConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("UpdateRecommenderConfiguration" Core..=
                       updateRecommenderConfiguration)])

instance Core.AWSRequest UpdateRecommenderConfiguration where
        type Rs UpdateRecommenderConfiguration =
             UpdateRecommenderConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/v1/recommenders/" Core.<> Core.toText recommenderId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateRecommenderConfigurationResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateRecommenderConfigurationResponse' smart constructor.
data UpdateRecommenderConfigurationResponse = UpdateRecommenderConfigurationResponse'
  { recommenderConfigurationResponse :: Types.RecommenderConfigurationResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateRecommenderConfigurationResponse' value with any optional fields omitted.
mkUpdateRecommenderConfigurationResponse
    :: Types.RecommenderConfigurationResponse -- ^ 'recommenderConfigurationResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdateRecommenderConfigurationResponse
mkUpdateRecommenderConfigurationResponse
  recommenderConfigurationResponse responseStatus
  = UpdateRecommenderConfigurationResponse'{recommenderConfigurationResponse,
                                            responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'recommenderConfigurationResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urcrrsRecommenderConfigurationResponse :: Lens.Lens' UpdateRecommenderConfigurationResponse Types.RecommenderConfigurationResponse
urcrrsRecommenderConfigurationResponse = Lens.field @"recommenderConfigurationResponse"
{-# INLINEABLE urcrrsRecommenderConfigurationResponse #-}
{-# DEPRECATED recommenderConfigurationResponse "Use generic-lens or generic-optics with 'recommenderConfigurationResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urcrrsResponseStatus :: Lens.Lens' UpdateRecommenderConfigurationResponse Core.Int
urcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE urcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
