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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDescribeGameSessionPlacement' smart constructor.
data DescribeGameSessionPlacement = DescribeGameSessionPlacement'
  { -- | A unique identifier for a game session placement to retrieve.
    placementId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeGameSessionPlacement
newDescribeGameSessionPlacement pPlacementId_ =
  DescribeGameSessionPlacement'
    { placementId =
        pPlacementId_
    }

-- | A unique identifier for a game session placement to retrieve.
describeGameSessionPlacement_placementId :: Lens.Lens' DescribeGameSessionPlacement Prelude.Text
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
            Prelude.<$> (x Core..?> "GameSessionPlacement")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeGameSessionPlacement

instance Prelude.NFData DescribeGameSessionPlacement

instance Core.ToHeaders DescribeGameSessionPlacement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.DescribeGameSessionPlacement" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeGameSessionPlacement where
  toJSON DescribeGameSessionPlacement' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("PlacementId" Core..= placementId)]
      )

instance Core.ToPath DescribeGameSessionPlacement where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeGameSessionPlacement where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newDescribeGameSessionPlacementResponse' smart constructor.
data DescribeGameSessionPlacementResponse = DescribeGameSessionPlacementResponse'
  { -- | Object that describes the requested game session placement.
    gameSessionPlacement :: Prelude.Maybe GameSessionPlacement,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeGameSessionPlacementResponse
newDescribeGameSessionPlacementResponse pHttpStatus_ =
  DescribeGameSessionPlacementResponse'
    { gameSessionPlacement =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Object that describes the requested game session placement.
describeGameSessionPlacementResponse_gameSessionPlacement :: Lens.Lens' DescribeGameSessionPlacementResponse (Prelude.Maybe GameSessionPlacement)
describeGameSessionPlacementResponse_gameSessionPlacement = Lens.lens (\DescribeGameSessionPlacementResponse' {gameSessionPlacement} -> gameSessionPlacement) (\s@DescribeGameSessionPlacementResponse' {} a -> s {gameSessionPlacement = a} :: DescribeGameSessionPlacementResponse)

-- | The response's http status code.
describeGameSessionPlacementResponse_httpStatus :: Lens.Lens' DescribeGameSessionPlacementResponse Prelude.Int
describeGameSessionPlacementResponse_httpStatus = Lens.lens (\DescribeGameSessionPlacementResponse' {httpStatus} -> httpStatus) (\s@DescribeGameSessionPlacementResponse' {} a -> s {httpStatus = a} :: DescribeGameSessionPlacementResponse)

instance
  Prelude.NFData
    DescribeGameSessionPlacementResponse
