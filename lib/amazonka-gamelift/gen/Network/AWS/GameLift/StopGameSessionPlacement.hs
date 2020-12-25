{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.StopGameSessionPlacement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a game session placement that is in @PENDING@ status. To stop a placement, provide the placement ID values. If successful, the placement is moved to @CANCELLED@ status.
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
module Network.AWS.GameLift.StopGameSessionPlacement
  ( -- * Creating a request
    StopGameSessionPlacement (..),
    mkStopGameSessionPlacement,

    -- ** Request lenses
    sPlacementId,

    -- * Destructuring the response
    StopGameSessionPlacementResponse (..),
    mkStopGameSessionPlacementResponse,

    -- ** Response lenses
    sgsprfrsGameSessionPlacement,
    sgsprfrsResponseStatus,
  )
where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkStopGameSessionPlacement' smart constructor.
newtype StopGameSessionPlacement = StopGameSessionPlacement'
  { -- | A unique identifier for a game session placement to cancel.
    placementId :: Types.IdStringModel
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopGameSessionPlacement' value with any optional fields omitted.
mkStopGameSessionPlacement ::
  -- | 'placementId'
  Types.IdStringModel ->
  StopGameSessionPlacement
mkStopGameSessionPlacement placementId =
  StopGameSessionPlacement' {placementId}

-- | A unique identifier for a game session placement to cancel.
--
-- /Note:/ Consider using 'placementId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sPlacementId :: Lens.Lens' StopGameSessionPlacement Types.IdStringModel
sPlacementId = Lens.field @"placementId"
{-# DEPRECATED sPlacementId "Use generic-lens or generic-optics with 'placementId' instead." #-}

instance Core.FromJSON StopGameSessionPlacement where
  toJSON StopGameSessionPlacement {..} =
    Core.object
      (Core.catMaybes [Core.Just ("PlacementId" Core..= placementId)])

instance Core.AWSRequest StopGameSessionPlacement where
  type Rs StopGameSessionPlacement = StopGameSessionPlacementResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "GameLift.StopGameSessionPlacement")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StopGameSessionPlacementResponse'
            Core.<$> (x Core..:? "GameSessionPlacement")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkStopGameSessionPlacementResponse' smart constructor.
data StopGameSessionPlacementResponse = StopGameSessionPlacementResponse'
  { -- | Object that describes the canceled game session placement, with @CANCELLED@ status and an end time stamp.
    gameSessionPlacement :: Core.Maybe Types.GameSessionPlacement,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StopGameSessionPlacementResponse' value with any optional fields omitted.
mkStopGameSessionPlacementResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StopGameSessionPlacementResponse
mkStopGameSessionPlacementResponse responseStatus =
  StopGameSessionPlacementResponse'
    { gameSessionPlacement =
        Core.Nothing,
      responseStatus
    }

-- | Object that describes the canceled game session placement, with @CANCELLED@ status and an end time stamp.
--
-- /Note:/ Consider using 'gameSessionPlacement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgsprfrsGameSessionPlacement :: Lens.Lens' StopGameSessionPlacementResponse (Core.Maybe Types.GameSessionPlacement)
sgsprfrsGameSessionPlacement = Lens.field @"gameSessionPlacement"
{-# DEPRECATED sgsprfrsGameSessionPlacement "Use generic-lens or generic-optics with 'gameSessionPlacement' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgsprfrsResponseStatus :: Lens.Lens' StopGameSessionPlacementResponse Core.Int
sgsprfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sgsprfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
