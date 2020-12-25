{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetRecommenderConfiguration (..),
    mkGetRecommenderConfiguration,

    -- ** Request lenses
    grcRecommenderId,

    -- * Destructuring the response
    GetRecommenderConfigurationResponse (..),
    mkGetRecommenderConfigurationResponse,

    -- ** Response lenses
    grcrfrsRecommenderConfigurationResponse,
    grcrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetRecommenderConfiguration' smart constructor.
newtype GetRecommenderConfiguration = GetRecommenderConfiguration'
  { -- | The unique identifier for the recommender model configuration. This identifier is displayed as the __Recommender ID__ on the Amazon Pinpoint console.
    recommenderId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetRecommenderConfiguration' value with any optional fields omitted.
mkGetRecommenderConfiguration ::
  -- | 'recommenderId'
  Core.Text ->
  GetRecommenderConfiguration
mkGetRecommenderConfiguration recommenderId =
  GetRecommenderConfiguration' {recommenderId}

-- | The unique identifier for the recommender model configuration. This identifier is displayed as the __Recommender ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'recommenderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcRecommenderId :: Lens.Lens' GetRecommenderConfiguration Core.Text
grcRecommenderId = Lens.field @"recommenderId"
{-# DEPRECATED grcRecommenderId "Use generic-lens or generic-optics with 'recommenderId' instead." #-}

instance Core.AWSRequest GetRecommenderConfiguration where
  type
    Rs GetRecommenderConfiguration =
      GetRecommenderConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/v1/recommenders/" Core.<> (Core.toText recommenderId)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRecommenderConfigurationResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetRecommenderConfigurationResponse' smart constructor.
data GetRecommenderConfigurationResponse = GetRecommenderConfigurationResponse'
  { recommenderConfigurationResponse :: Types.RecommenderConfigurationResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRecommenderConfigurationResponse' value with any optional fields omitted.
mkGetRecommenderConfigurationResponse ::
  -- | 'recommenderConfigurationResponse'
  Types.RecommenderConfigurationResponse ->
  -- | 'responseStatus'
  Core.Int ->
  GetRecommenderConfigurationResponse
mkGetRecommenderConfigurationResponse
  recommenderConfigurationResponse
  responseStatus =
    GetRecommenderConfigurationResponse'
      { recommenderConfigurationResponse,
        responseStatus
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'recommenderConfigurationResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcrfrsRecommenderConfigurationResponse :: Lens.Lens' GetRecommenderConfigurationResponse Types.RecommenderConfigurationResponse
grcrfrsRecommenderConfigurationResponse = Lens.field @"recommenderConfigurationResponse"
{-# DEPRECATED grcrfrsRecommenderConfigurationResponse "Use generic-lens or generic-optics with 'recommenderConfigurationResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grcrfrsResponseStatus :: Lens.Lens' GetRecommenderConfigurationResponse Core.Int
grcrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grcrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
