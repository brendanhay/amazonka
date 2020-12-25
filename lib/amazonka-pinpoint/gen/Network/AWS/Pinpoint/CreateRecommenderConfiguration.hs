{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateRecommenderConfiguration (..),
    mkCreateRecommenderConfiguration,

    -- ** Request lenses
    crcCreateRecommenderConfiguration,

    -- * Destructuring the response
    CreateRecommenderConfigurationResponse (..),
    mkCreateRecommenderConfigurationResponse,

    -- ** Response lenses
    crcrrsRecommenderConfigurationResponse,
    crcrrsResponseStatus,
  )
where

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
mkCreateRecommenderConfiguration ::
  -- | 'createRecommenderConfiguration'
  Types.CreateRecommenderConfiguration ->
  CreateRecommenderConfiguration
mkCreateRecommenderConfiguration createRecommenderConfiguration =
  CreateRecommenderConfiguration' {createRecommenderConfiguration}

-- | Undocumented field.
--
-- /Note:/ Consider using 'createRecommenderConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crcCreateRecommenderConfiguration :: Lens.Lens' CreateRecommenderConfiguration CreateRecommenderConfiguration
crcCreateRecommenderConfiguration = Lens.field @"createRecommenderConfiguration"
{-# DEPRECATED crcCreateRecommenderConfiguration "Use generic-lens or generic-optics with 'createRecommenderConfiguration' instead." #-}

instance Core.FromJSON CreateRecommenderConfiguration where
  toJSON CreateRecommenderConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "CreateRecommenderConfiguration"
                  Core..= createRecommenderConfiguration
              )
          ]
      )

instance Core.AWSRequest CreateRecommenderConfiguration where
  type
    Rs CreateRecommenderConfiguration =
      CreateRecommenderConfigurationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/v1/recommenders",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRecommenderConfigurationResponse'
            Core.<$> (Core.eitherParseJSON x) Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateRecommenderConfigurationResponse' smart constructor.
data CreateRecommenderConfigurationResponse = CreateRecommenderConfigurationResponse'
  { recommenderConfigurationResponse :: Types.RecommenderConfigurationResponse,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateRecommenderConfigurationResponse' value with any optional fields omitted.
mkCreateRecommenderConfigurationResponse ::
  -- | 'recommenderConfigurationResponse'
  Types.RecommenderConfigurationResponse ->
  -- | 'responseStatus'
  Core.Int ->
  CreateRecommenderConfigurationResponse
mkCreateRecommenderConfigurationResponse
  recommenderConfigurationResponse
  responseStatus =
    CreateRecommenderConfigurationResponse'
      { recommenderConfigurationResponse,
        responseStatus
      }

-- | Undocumented field.
--
-- /Note:/ Consider using 'recommenderConfigurationResponse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crcrrsRecommenderConfigurationResponse :: Lens.Lens' CreateRecommenderConfigurationResponse Types.RecommenderConfigurationResponse
crcrrsRecommenderConfigurationResponse = Lens.field @"recommenderConfigurationResponse"
{-# DEPRECATED crcrrsRecommenderConfigurationResponse "Use generic-lens or generic-optics with 'recommenderConfigurationResponse' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crcrrsResponseStatus :: Lens.Lens' CreateRecommenderConfigurationResponse Core.Int
crcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
