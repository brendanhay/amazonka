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
    dgsprsGameSessionPlacement,
    dgsprsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDescribeGameSessionPlacement' smart constructor.
newtype DescribeGameSessionPlacement = DescribeGameSessionPlacement'
  { -- | A unique identifier for a game session placement to retrieve.
    placementId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeGameSessionPlacement' with the minimum fields required to make a request.
--
-- * 'placementId' - A unique identifier for a game session placement to retrieve.
mkDescribeGameSessionPlacement ::
  -- | 'placementId'
  Lude.Text ->
  DescribeGameSessionPlacement
mkDescribeGameSessionPlacement pPlacementId_ =
  DescribeGameSessionPlacement' {placementId = pPlacementId_}

-- | A unique identifier for a game session placement to retrieve.
--
-- /Note:/ Consider using 'placementId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgspPlacementId :: Lens.Lens' DescribeGameSessionPlacement Lude.Text
dgspPlacementId = Lens.lens (placementId :: DescribeGameSessionPlacement -> Lude.Text) (\s a -> s {placementId = a} :: DescribeGameSessionPlacement)
{-# DEPRECATED dgspPlacementId "Use generic-lens or generic-optics with 'placementId' instead." #-}

instance Lude.AWSRequest DescribeGameSessionPlacement where
  type
    Rs DescribeGameSessionPlacement =
      DescribeGameSessionPlacementResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeGameSessionPlacementResponse'
            Lude.<$> (x Lude..?> "GameSessionPlacement")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeGameSessionPlacement where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DescribeGameSessionPlacement" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeGameSessionPlacement where
  toJSON DescribeGameSessionPlacement' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("PlacementId" Lude..= placementId)])

instance Lude.ToPath DescribeGameSessionPlacement where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeGameSessionPlacement where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribeGameSessionPlacementResponse' smart constructor.
data DescribeGameSessionPlacementResponse = DescribeGameSessionPlacementResponse'
  { -- | Object that describes the requested game session placement.
    gameSessionPlacement :: Lude.Maybe GameSessionPlacement,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeGameSessionPlacementResponse' with the minimum fields required to make a request.
--
-- * 'gameSessionPlacement' - Object that describes the requested game session placement.
-- * 'responseStatus' - The response status code.
mkDescribeGameSessionPlacementResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeGameSessionPlacementResponse
mkDescribeGameSessionPlacementResponse pResponseStatus_ =
  DescribeGameSessionPlacementResponse'
    { gameSessionPlacement =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Object that describes the requested game session placement.
--
-- /Note:/ Consider using 'gameSessionPlacement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsprsGameSessionPlacement :: Lens.Lens' DescribeGameSessionPlacementResponse (Lude.Maybe GameSessionPlacement)
dgsprsGameSessionPlacement = Lens.lens (gameSessionPlacement :: DescribeGameSessionPlacementResponse -> Lude.Maybe GameSessionPlacement) (\s a -> s {gameSessionPlacement = a} :: DescribeGameSessionPlacementResponse)
{-# DEPRECATED dgsprsGameSessionPlacement "Use generic-lens or generic-optics with 'gameSessionPlacement' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsprsResponseStatus :: Lens.Lens' DescribeGameSessionPlacementResponse Lude.Int
dgsprsResponseStatus = Lens.lens (responseStatus :: DescribeGameSessionPlacementResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeGameSessionPlacementResponse)
{-# DEPRECATED dgsprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
