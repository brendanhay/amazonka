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
-- Module      : Amazonka.GameLift.DescribeGameSessionPlacement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information, including current status, about a game session
-- placement request.
--
-- To get game session placement details, specify the placement ID.
--
-- This operation is not designed to be continually called to track game
-- session status. This practice can cause you to exceed your API limit,
-- which results in errors. Instead, you must configure configure an Amazon
-- Simple Notification Service (SNS) topic to receive notifications from
-- FlexMatch or queues. Continuously polling with
-- @DescribeGameSessionPlacement@ should only be used for games in
-- development with low game session usage.
module Amazonka.GameLift.DescribeGameSessionPlacement
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeGameSessionPlacement' smart constructor.
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGameSessionPlacementResponse'
            Prelude.<$> (x Data..?> "GameSessionPlacement")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeGameSessionPlacement
  where
  hashWithSalt _salt DescribeGameSessionPlacement' {..} =
    _salt `Prelude.hashWithSalt` placementId

instance Prelude.NFData DescribeGameSessionPlacement where
  rnf DescribeGameSessionPlacement' {..} =
    Prelude.rnf placementId

instance Data.ToHeaders DescribeGameSessionPlacement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "GameLift.DescribeGameSessionPlacement" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeGameSessionPlacement where
  toJSON DescribeGameSessionPlacement' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("PlacementId" Data..= placementId)]
      )

instance Data.ToPath DescribeGameSessionPlacement where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeGameSessionPlacement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeGameSessionPlacementResponse' smart constructor.
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
  where
  rnf DescribeGameSessionPlacementResponse' {..} =
    Prelude.rnf gameSessionPlacement
      `Prelude.seq` Prelude.rnf httpStatus
