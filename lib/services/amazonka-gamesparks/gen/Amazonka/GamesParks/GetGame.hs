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
-- Module      : Amazonka.GamesParks.GetGame
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about a game.
module Amazonka.GamesParks.GetGame
  ( -- * Creating a Request
    GetGame (..),
    newGetGame,

    -- * Request Lenses
    getGame_gameName,

    -- * Destructuring the Response
    GetGameResponse (..),
    newGetGameResponse,

    -- * Response Lenses
    getGameResponse_game,
    getGameResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GamesParks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetGame' smart constructor.
data GetGame = GetGame'
  { -- | The name of the game.
    gameName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGame' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameName', 'getGame_gameName' - The name of the game.
newGetGame ::
  -- | 'gameName'
  Prelude.Text ->
  GetGame
newGetGame pGameName_ =
  GetGame' {gameName = pGameName_}

-- | The name of the game.
getGame_gameName :: Lens.Lens' GetGame Prelude.Text
getGame_gameName = Lens.lens (\GetGame' {gameName} -> gameName) (\s@GetGame' {} a -> s {gameName = a} :: GetGame)

instance Core.AWSRequest GetGame where
  type AWSResponse GetGame = GetGameResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGameResponse'
            Prelude.<$> (x Core..?> "Game")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetGame where
  hashWithSalt _salt GetGame' {..} =
    _salt `Prelude.hashWithSalt` gameName

instance Prelude.NFData GetGame where
  rnf GetGame' {..} = Prelude.rnf gameName

instance Core.ToHeaders GetGame where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetGame where
  toPath GetGame' {..} =
    Prelude.mconcat ["/game/", Core.toBS gameName]

instance Core.ToQuery GetGame where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetGameResponse' smart constructor.
data GetGameResponse = GetGameResponse'
  { -- | The details of the game.
    game :: Prelude.Maybe GameDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGameResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'game', 'getGameResponse_game' - The details of the game.
--
-- 'httpStatus', 'getGameResponse_httpStatus' - The response's http status code.
newGetGameResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetGameResponse
newGetGameResponse pHttpStatus_ =
  GetGameResponse'
    { game = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the game.
getGameResponse_game :: Lens.Lens' GetGameResponse (Prelude.Maybe GameDetails)
getGameResponse_game = Lens.lens (\GetGameResponse' {game} -> game) (\s@GetGameResponse' {} a -> s {game = a} :: GetGameResponse)

-- | The response's http status code.
getGameResponse_httpStatus :: Lens.Lens' GetGameResponse Prelude.Int
getGameResponse_httpStatus = Lens.lens (\GetGameResponse' {httpStatus} -> httpStatus) (\s@GetGameResponse' {} a -> s {httpStatus = a} :: GetGameResponse)

instance Prelude.NFData GetGameResponse where
  rnf GetGameResponse' {..} =
    Prelude.rnf game
      `Prelude.seq` Prelude.rnf httpStatus
