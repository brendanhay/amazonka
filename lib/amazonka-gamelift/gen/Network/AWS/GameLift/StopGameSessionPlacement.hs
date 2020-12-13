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
    sgspfrsGameSessionPlacement,
    sgspfrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkStopGameSessionPlacement' smart constructor.
newtype StopGameSessionPlacement = StopGameSessionPlacement'
  { -- | A unique identifier for a game session placement to cancel.
    placementId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopGameSessionPlacement' with the minimum fields required to make a request.
--
-- * 'placementId' - A unique identifier for a game session placement to cancel.
mkStopGameSessionPlacement ::
  -- | 'placementId'
  Lude.Text ->
  StopGameSessionPlacement
mkStopGameSessionPlacement pPlacementId_ =
  StopGameSessionPlacement' {placementId = pPlacementId_}

-- | A unique identifier for a game session placement to cancel.
--
-- /Note:/ Consider using 'placementId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sPlacementId :: Lens.Lens' StopGameSessionPlacement Lude.Text
sPlacementId = Lens.lens (placementId :: StopGameSessionPlacement -> Lude.Text) (\s a -> s {placementId = a} :: StopGameSessionPlacement)
{-# DEPRECATED sPlacementId "Use generic-lens or generic-optics with 'placementId' instead." #-}

instance Lude.AWSRequest StopGameSessionPlacement where
  type Rs StopGameSessionPlacement = StopGameSessionPlacementResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          StopGameSessionPlacementResponse'
            Lude.<$> (x Lude..?> "GameSessionPlacement")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopGameSessionPlacement where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.StopGameSessionPlacement" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopGameSessionPlacement where
  toJSON StopGameSessionPlacement' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("PlacementId" Lude..= placementId)])

instance Lude.ToPath StopGameSessionPlacement where
  toPath = Lude.const "/"

instance Lude.ToQuery StopGameSessionPlacement where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkStopGameSessionPlacementResponse' smart constructor.
data StopGameSessionPlacementResponse = StopGameSessionPlacementResponse'
  { -- | Object that describes the canceled game session placement, with @CANCELLED@ status and an end time stamp.
    gameSessionPlacement :: Lude.Maybe GameSessionPlacement,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopGameSessionPlacementResponse' with the minimum fields required to make a request.
--
-- * 'gameSessionPlacement' - Object that describes the canceled game session placement, with @CANCELLED@ status and an end time stamp.
-- * 'responseStatus' - The response status code.
mkStopGameSessionPlacementResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopGameSessionPlacementResponse
mkStopGameSessionPlacementResponse pResponseStatus_ =
  StopGameSessionPlacementResponse'
    { gameSessionPlacement =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Object that describes the canceled game session placement, with @CANCELLED@ status and an end time stamp.
--
-- /Note:/ Consider using 'gameSessionPlacement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgspfrsGameSessionPlacement :: Lens.Lens' StopGameSessionPlacementResponse (Lude.Maybe GameSessionPlacement)
sgspfrsGameSessionPlacement = Lens.lens (gameSessionPlacement :: StopGameSessionPlacementResponse -> Lude.Maybe GameSessionPlacement) (\s a -> s {gameSessionPlacement = a} :: StopGameSessionPlacementResponse)
{-# DEPRECATED sgspfrsGameSessionPlacement "Use generic-lens or generic-optics with 'gameSessionPlacement' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sgspfrsResponseStatus :: Lens.Lens' StopGameSessionPlacementResponse Lude.Int
sgspfrsResponseStatus = Lens.lens (responseStatus :: StopGameSessionPlacementResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopGameSessionPlacementResponse)
{-# DEPRECATED sgspfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
