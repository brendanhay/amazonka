{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.StopGameSessionPlacement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a game session placement that is in @PENDING@ status. To stop a
-- placement, provide the placement ID values. If successful, the placement
-- is moved to @CANCELLED@ status.
--
-- -   CreateGameSession
--
-- -   DescribeGameSessions
--
-- -   DescribeGameSessionDetails
--
-- -   SearchGameSessions
--
-- -   UpdateGameSession
--
-- -   GetGameSessionLogUrl
--
-- -   Game session placements
--
--     -   StartGameSessionPlacement
--
--     -   DescribeGameSessionPlacement
--
--     -   StopGameSessionPlacement
module Network.AWS.GameLift.StopGameSessionPlacement
  ( -- * Creating a Request
    StopGameSessionPlacement (..),
    newStopGameSessionPlacement,

    -- * Request Lenses
    stopGameSessionPlacement_placementId,

    -- * Destructuring the Response
    StopGameSessionPlacementResponse (..),
    newStopGameSessionPlacementResponse,

    -- * Response Lenses
    stopGameSessionPlacementResponse_gameSessionPlacement,
    stopGameSessionPlacementResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newStopGameSessionPlacement' smart constructor.
data StopGameSessionPlacement = StopGameSessionPlacement'
  { -- | A unique identifier for a game session placement to cancel.
    placementId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopGameSessionPlacement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'placementId', 'stopGameSessionPlacement_placementId' - A unique identifier for a game session placement to cancel.
newStopGameSessionPlacement ::
  -- | 'placementId'
  Prelude.Text ->
  StopGameSessionPlacement
newStopGameSessionPlacement pPlacementId_ =
  StopGameSessionPlacement'
    { placementId =
        pPlacementId_
    }

-- | A unique identifier for a game session placement to cancel.
stopGameSessionPlacement_placementId :: Lens.Lens' StopGameSessionPlacement Prelude.Text
stopGameSessionPlacement_placementId = Lens.lens (\StopGameSessionPlacement' {placementId} -> placementId) (\s@StopGameSessionPlacement' {} a -> s {placementId = a} :: StopGameSessionPlacement)

instance Core.AWSRequest StopGameSessionPlacement where
  type
    AWSResponse StopGameSessionPlacement =
      StopGameSessionPlacementResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StopGameSessionPlacementResponse'
            Prelude.<$> (x Core..?> "GameSessionPlacement")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopGameSessionPlacement

instance Prelude.NFData StopGameSessionPlacement

instance Core.ToHeaders StopGameSessionPlacement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.StopGameSessionPlacement" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StopGameSessionPlacement where
  toJSON StopGameSessionPlacement' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("PlacementId" Core..= placementId)]
      )

instance Core.ToPath StopGameSessionPlacement where
  toPath = Prelude.const "/"

instance Core.ToQuery StopGameSessionPlacement where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newStopGameSessionPlacementResponse' smart constructor.
data StopGameSessionPlacementResponse = StopGameSessionPlacementResponse'
  { -- | Object that describes the canceled game session placement, with
    -- @CANCELLED@ status and an end time stamp.
    gameSessionPlacement :: Prelude.Maybe GameSessionPlacement,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopGameSessionPlacementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameSessionPlacement', 'stopGameSessionPlacementResponse_gameSessionPlacement' - Object that describes the canceled game session placement, with
-- @CANCELLED@ status and an end time stamp.
--
-- 'httpStatus', 'stopGameSessionPlacementResponse_httpStatus' - The response's http status code.
newStopGameSessionPlacementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopGameSessionPlacementResponse
newStopGameSessionPlacementResponse pHttpStatus_ =
  StopGameSessionPlacementResponse'
    { gameSessionPlacement =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Object that describes the canceled game session placement, with
-- @CANCELLED@ status and an end time stamp.
stopGameSessionPlacementResponse_gameSessionPlacement :: Lens.Lens' StopGameSessionPlacementResponse (Prelude.Maybe GameSessionPlacement)
stopGameSessionPlacementResponse_gameSessionPlacement = Lens.lens (\StopGameSessionPlacementResponse' {gameSessionPlacement} -> gameSessionPlacement) (\s@StopGameSessionPlacementResponse' {} a -> s {gameSessionPlacement = a} :: StopGameSessionPlacementResponse)

-- | The response's http status code.
stopGameSessionPlacementResponse_httpStatus :: Lens.Lens' StopGameSessionPlacementResponse Prelude.Int
stopGameSessionPlacementResponse_httpStatus = Lens.lens (\StopGameSessionPlacementResponse' {httpStatus} -> httpStatus) (\s@StopGameSessionPlacementResponse' {} a -> s {httpStatus = a} :: StopGameSessionPlacementResponse)

instance
  Prelude.NFData
    StopGameSessionPlacementResponse
