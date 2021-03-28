{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.CreateRecommenderConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Pinpoint configuration for a recommender model.
module Network.AWS.Pinpoint.CreateRecommenderConfiguration
    (
    -- * Creating a request
      CreateRecommenderConfiguration (..)
    , mkCreateRecommenderConfiguration
    -- ** Request lenses
    , crcCreateRecommenderConfiguration

    -- * Destructuring the response
    , CreateRecommenderConfigurationResponse (..)
    , mkCreateRecommenderConfigurationResponse
    -- ** Response lenses
    , crcrrsRecommenderConfigurationResponse
    , crcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateRecommenderConfiguration' smart constructor.
newtype CreateRecommenderConfiguration = CreateRecommenderConfiguration'
  { createRecommenderConfiguration :: CreateRecommenderConfiguration
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRecommenderConfiguration' value with any optional fields omitted.
mkCreateRecommenderConfiguration
    :: Types.CreateRecommenderConfiguration -- ^ 'createRecommenderConfiguration'
    -> CreateRecommenderConfiguration
mkCreateRecommenderConfiguration createRecommenderConfiguration
  = CreateRecommenderConfiguration'{createRecommenderConfiguration}

-- | Undocumented field.
--
-- /Note:/ Consider using 'createRecommenderConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crcCreateRecommenderConfiguration :: Lens.Lens' CreateRecommenderConfiguration CreateRecommenderConfiguration
crcCreateRecommenderConfiguration = Lens.field @"createRecommenderConfiguration"
{-# INLINEABLE crcCreateRecommenderConfiguration #-}
{-# DEPRECATED createRecommenderConfiguration "Use generic-lens or generic-optics with 'createRecommenderConfiguration' instead"  #-}

instance Core.ToQuery CreateRecommenderConfiguration where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateRecommenderConfiguration where
        toHeaders CreateRecommenderConfiguration{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateRecommenderConfiguration where
        toJSON CreateRecommenderConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("CreateRecommenderConfiguration" Core..=
                       createRecommenderConfiguration)])

instance Core.AWSRequest CreateRecommenderConfiguration where
        type Rs CreateRecommenderConfiguration =
             CreateRecommenderConfigurationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/v1/recommenders",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateRecommenderConfigurationResponse' Core.<$>
                   (Core.eitherParseJSON x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateRecommenderConfigurationResponse' smart constructor.
data CreateRecommenderConfigurationResponse = CreateRecommenderConfigurationResponse'
  { recommenderConfigurationResponse :: Types.RecommenderConfigurationResponse
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRecommenderConfigurationResponse' value with any optional fields omitted.
mkCreateRecommenderConfigurationResponse
    :: Types.RecommenderConfigurationResponse -- ^ 'recommenderConfigurationResponse'
    -> Core.Int -- ^ 'responseStatus'
    -> CreateRecommenderConfigurationResponse
mkCreateRecommenderConfigurationResponse
  recommenderConfigurationResponse responseStatus
  = CreateRecommenderConfigurationResponse'{recommenderConfigurationResponse,
                                            responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'recommenderConfigurationResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crcrrsRecommenderConfigurationResponse :: Lens.Lens' CreateRecommenderConfigurationResponse Types.RecommenderConfigurationResponse
crcrrsRecommenderConfigurationResponse = Lens.field @"recommenderConfigurationResponse"
{-# INLINEABLE crcrrsRecommenderConfigurationResponse #-}
{-# DEPRECATED recommenderConfigurationResponse "Use generic-lens or generic-optics with 'recommenderConfigurationResponse' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crcrrsResponseStatus :: Lens.Lens' CreateRecommenderConfigurationResponse Core.Int
crcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE crcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
