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
-- Module      : Network.AWS.GameLift.DescribeGameSessionPlacement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves properties and current status of a game session placement
-- request. To get game session placement details, specify the placement
-- ID. If successful, a GameSessionPlacement object is returned.
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
module Network.AWS.GameLift.DescribeGameSessionPlacement
  ( -- * Creating a Request
    DescribeGameSessionPlacement (..),
    newDescribeGameSessionPlacement,

    -- * Request Lenses
    describeGameSessionPlacement_placementId,

    -- * Destructuring the Response
    DescribeGameSessionPlacementResponse (..),
    newDescribeGameSessionPlacementResponse,

    -- * Response Lenses
    describeGameSessionPlacementResponse_gameSessionPlacement,
    describeGameSessionPlacementResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDescribeGameSessionPlacement' smart constructor.
data DescribeGameSessionPlacement = DescribeGameSessionPlacement'
  { -- | A unique identifier for a game session placement to retrieve.
    placementId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeGameSessionPlacement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'placementId', 'describeGameSessionPlacement_placementId' - A unique identifier for a game session placement to retrieve.
newDescribeGameSessionPlacement ::
  -- | 'placementId'
  Core.Text ->
  DescribeGameSessionPlacement
newDescribeGameSessionPlacement pPlacementId_ =
  DescribeGameSessionPlacement'
    { placementId =
        pPlacementId_
    }

-- | A unique identifier for a game session placement to retrieve.
describeGameSessionPlacement_placementId :: Lens.Lens' DescribeGameSessionPlacement Core.Text
describeGameSessionPlacement_placementId = Lens.lens (\DescribeGameSessionPlacement' {placementId} -> placementId) (\s@DescribeGameSessionPlacement' {} a -> s {placementId = a} :: DescribeGameSessionPlacement)

instance Core.AWSRequest DescribeGameSessionPlacement where
  type
    AWSResponse DescribeGameSessionPlacement =
      DescribeGameSessionPlacementResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGameSessionPlacementResponse'
            Core.<$> (x Core..?> "GameSessionPlacement")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeGameSessionPlacement

instance Core.NFData DescribeGameSessionPlacement

instance Core.ToHeaders DescribeGameSessionPlacement where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.DescribeGameSessionPlacement" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeGameSessionPlacement where
  toJSON DescribeGameSessionPlacement' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("PlacementId" Core..= placementId)]
      )

instance Core.ToPath DescribeGameSessionPlacement where
  toPath = Core.const "/"

instance Core.ToQuery DescribeGameSessionPlacement where
  toQuery = Core.const Core.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newDescribeGameSessionPlacementResponse' smart constructor.
data DescribeGameSessionPlacementResponse = DescribeGameSessionPlacementResponse'
  { -- | Object that describes the requested game session placement.
    gameSessionPlacement :: Core.Maybe GameSessionPlacement,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeGameSessionPlacementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameSessionPlacement', 'describeGameSessionPlacementResponse_gameSessionPlacement' - Object that describes the requested game session placement.
--
-- 'httpStatus', 'describeGameSessionPlacementResponse_httpStatus' - The response's http status code.
newDescribeGameSessionPlacementResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeGameSessionPlacementResponse
newDescribeGameSessionPlacementResponse pHttpStatus_ =
  DescribeGameSessionPlacementResponse'
    { gameSessionPlacement =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Object that describes the requested game session placement.
describeGameSessionPlacementResponse_gameSessionPlacement :: Lens.Lens' DescribeGameSessionPlacementResponse (Core.Maybe GameSessionPlacement)
describeGameSessionPlacementResponse_gameSessionPlacement = Lens.lens (\DescribeGameSessionPlacementResponse' {gameSessionPlacement} -> gameSessionPlacement) (\s@DescribeGameSessionPlacementResponse' {} a -> s {gameSessionPlacement = a} :: DescribeGameSessionPlacementResponse)

-- | The response's http status code.
describeGameSessionPlacementResponse_httpStatus :: Lens.Lens' DescribeGameSessionPlacementResponse Core.Int
describeGameSessionPlacementResponse_httpStatus = Lens.lens (\DescribeGameSessionPlacementResponse' {httpStatus} -> httpStatus) (\s@DescribeGameSessionPlacementResponse' {} a -> s {httpStatus = a} :: DescribeGameSessionPlacementResponse)

instance
  Core.NFData
    DescribeGameSessionPlacementResponse
