{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.DeleteRecommenderConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Pinpoint configuration for a recommender model.
module Network.AWS.Pinpoint.DeleteRecommenderConfiguration
  ( -- * Creating a request
    DeleteRecommenderConfiguration (..),
    mkDeleteRecommenderConfiguration,

    -- ** Request lenses
    drcRecommenderId,

    -- * Destructuring the response
    DeleteRecommenderConfigurationResponse (..),
    mkDeleteRecommenderConfigurationResponse,

    -- ** Response lenses
    drcrrsRecommenderConfigurationResponse,
    drcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteRecommenderConfiguration' smart constructor.
newtype DeleteRecommenderConfiguration = DeleteRecommenderConfiguration'
  { -- | The unique identifier for the recommender model configuration. This identifier is displayed as the __Recommender ID__ on the Amazon Pinpoint console.
    recommenderId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRecommenderConfiguration' value with any optional fields omitted.
mkDeleteRecommenderConfiguration ::
  -- | 'recommenderId'
  Core.Text ->
  DeleteRecommenderConfiguration
mkDeleteRecommenderConfiguration recommenderId =
  DeleteRecommenderConfiguration' {recommenderId}

-- | The unique identifier for the recommender model configuration. This identifier is displayed as the __Recommender ID__ on the Amazon Pinpoint console.
--
-- /Note:/ Consider using 'recommenderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcRecommenderId :: Lens.Lens' DeleteRecommenderConfiguration Core.Text
drcRecommenderId = Lens.field @"recommenderId"
{-# DEPRECATED drcRecommenderId "Use generic-lens or generic-optics with 'recommenderId' instead." #-}

instance Core.AWSRequest DeleteRecommenderConfiguration where
  type
    Rs DeleteRecommenderConfiguration =
      DeleteRecommenderConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
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
          DeleteRecommenderConfigurationResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteRecommenderConfigurationResponse' smart constructor.
data DeleteRecommenderConfigurationResponse = DeleteRecommenderConfigurationResponse'
  { recommenderConfigurationResponse :: Types.RecommenderConfigurationResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteRecommenderConfigurationResponse' value with any optional fields omitted.
mkDeleteRecommenderConfigurationResponse ::
  -- | 'recommenderConfigurationResponse'
  Types.RecommenderConfigurationResponse ->
  -- | 'responseStatus'
  Core.Int ->
  DeleteRecommenderConfigurationResponse
mkDeleteRecommenderConfigurationResponse
  recommenderConfigurationResponse
  responseStatus =
    DeleteRecommenderConfigurationResponse'
      { recommenderConfigurationResponse,
        responseStatus
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'recommenderConfigurationResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcrrsRecommenderConfigurationResponse :: Lens.Lens' DeleteRecommenderConfigurationResponse Types.RecommenderConfigurationResponse
drcrrsRecommenderConfigurationResponse = Lens.field @"recommenderConfigurationResponse"
{-# DEPRECATED drcrrsRecommenderConfigurationResponse "Use generic-lens or generic-optics with 'recommenderConfigurationResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcrrsResponseStatus :: Lens.Lens' DeleteRecommenderConfigurationResponse Core.Int
drcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
