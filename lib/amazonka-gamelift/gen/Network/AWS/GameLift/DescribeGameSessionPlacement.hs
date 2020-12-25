{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeGameSessionPlacement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves properties and current status of a game session placement request. To get game session placement details, specify the placement ID. If successful, a 'GameSessionPlacement' object is returned.
--
--
--     * 'CreateGameSession'
--
--
--     * 'DescribeGameSessions'
--
--
--     * 'DescribeGameSessionDetails'
--
--
--     * 'SearchGameSessions'
--
--
--     * 'UpdateGameSession'
--
--
--     * 'GetGameSessionLogUrl'
--
--
--     * Game session placements
--
--     * 'StartGameSessionPlacement'
--
--
--     * 'DescribeGameSessionPlacement'
--
--
--     * 'StopGameSessionPlacement'
module Network.AWS.GameLift.DescribeGameSessionPlacement
  ( -- * Creating a request
    DescribeGameSessionPlacement (..),
    mkDescribeGameSessionPlacement,

    -- ** Request lenses
    dgspPlacementId,

    -- * Destructuring the response
    DescribeGameSessionPlacementResponse (..),
    mkDescribeGameSessionPlacementResponse,

    -- ** Response lenses
    dgsprrsGameSessionPlacement,
    dgsprrsResponseStatus,
  )
where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDescribeGameSessionPlacement' smart constructor.
newtype DescribeGameSessionPlacement = DescribeGameSessionPlacement'
  { -- | A unique identifier for a game session placement to retrieve.
    placementId :: Types.IdStringModel
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeGameSessionPlacement' value with any optional fields omitted.
mkDescribeGameSessionPlacement ::
  -- | 'placementId'
  Types.IdStringModel ->
  DescribeGameSessionPlacement
mkDescribeGameSessionPlacement placementId =
  DescribeGameSessionPlacement' {placementId}

-- | A unique identifier for a game session placement to retrieve.
--
-- /Note:/ Consider using 'placementId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgspPlacementId :: Lens.Lens' DescribeGameSessionPlacement Types.IdStringModel
dgspPlacementId = Lens.field @"placementId"
{-# DEPRECATED dgspPlacementId "Use generic-lens or generic-optics with 'placementId' instead." #-}

instance Core.FromJSON DescribeGameSessionPlacement where
  toJSON DescribeGameSessionPlacement {..} =
    Core.object
      (Core.catMaybes [Core.Just ("PlacementId" Core..= placementId)])

instance Core.AWSRequest DescribeGameSessionPlacement where
  type
    Rs DescribeGameSessionPlacement =
      DescribeGameSessionPlacementResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "GameLift.DescribeGameSessionPlacement")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGameSessionPlacementResponse'
            Core.<$> (x Core..:? "GameSessionPlacement")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribeGameSessionPlacementResponse' smart constructor.
data DescribeGameSessionPlacementResponse = DescribeGameSessionPlacementResponse'
  { -- | Object that describes the requested game session placement.
    gameSessionPlacement :: Core.Maybe Types.GameSessionPlacement,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeGameSessionPlacementResponse' value with any optional fields omitted.
mkDescribeGameSessionPlacementResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeGameSessionPlacementResponse
mkDescribeGameSessionPlacementResponse responseStatus =
  DescribeGameSessionPlacementResponse'
    { gameSessionPlacement =
        Core.Nothing,
      responseStatus
    }

-- | Object that describes the requested game session placement.
--
-- /Note:/ Consider using 'gameSessionPlacement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsprrsGameSessionPlacement :: Lens.Lens' DescribeGameSessionPlacementResponse (Core.Maybe Types.GameSessionPlacement)
dgsprrsGameSessionPlacement = Lens.field @"gameSessionPlacement"
{-# DEPRECATED dgsprrsGameSessionPlacement "Use generic-lens or generic-optics with 'gameSessionPlacement' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsprrsResponseStatus :: Lens.Lens' DescribeGameSessionPlacementResponse Core.Int
dgsprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dgsprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
