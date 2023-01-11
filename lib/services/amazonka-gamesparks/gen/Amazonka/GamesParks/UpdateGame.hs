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
-- Module      : Amazonka.GamesParks.UpdateGame
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates details of the game.
module Amazonka.GamesParks.UpdateGame
  ( -- * Creating a Request
    UpdateGame (..),
    newUpdateGame,

    -- * Request Lenses
    updateGame_description,
    updateGame_gameName,

    -- * Destructuring the Response
    UpdateGameResponse (..),
    newUpdateGameResponse,

    -- * Response Lenses
    updateGameResponse_game,
    updateGameResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GamesParks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateGame' smart constructor.
data UpdateGame = UpdateGame'
  { -- | The description of the game.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the game.
    gameName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGame' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateGame_description' - The description of the game.
--
-- 'gameName', 'updateGame_gameName' - The name of the game.
newUpdateGame ::
  -- | 'gameName'
  Prelude.Text ->
  UpdateGame
newUpdateGame pGameName_ =
  UpdateGame'
    { description = Prelude.Nothing,
      gameName = pGameName_
    }

-- | The description of the game.
updateGame_description :: Lens.Lens' UpdateGame (Prelude.Maybe Prelude.Text)
updateGame_description = Lens.lens (\UpdateGame' {description} -> description) (\s@UpdateGame' {} a -> s {description = a} :: UpdateGame)

-- | The name of the game.
updateGame_gameName :: Lens.Lens' UpdateGame Prelude.Text
updateGame_gameName = Lens.lens (\UpdateGame' {gameName} -> gameName) (\s@UpdateGame' {} a -> s {gameName = a} :: UpdateGame)

instance Core.AWSRequest UpdateGame where
  type AWSResponse UpdateGame = UpdateGameResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGameResponse'
            Prelude.<$> (x Data..?> "Game")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateGame where
  hashWithSalt _salt UpdateGame' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` gameName

instance Prelude.NFData UpdateGame where
  rnf UpdateGame' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf gameName

instance Data.ToHeaders UpdateGame where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateGame where
  toJSON UpdateGame' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Description" Data..=) Prelude.<$> description]
      )

instance Data.ToPath UpdateGame where
  toPath UpdateGame' {..} =
    Prelude.mconcat ["/game/", Data.toBS gameName]

instance Data.ToQuery UpdateGame where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateGameResponse' smart constructor.
data UpdateGameResponse = UpdateGameResponse'
  { -- | The details of the game.
    game :: Prelude.Maybe GameDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGameResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'game', 'updateGameResponse_game' - The details of the game.
--
-- 'httpStatus', 'updateGameResponse_httpStatus' - The response's http status code.
newUpdateGameResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateGameResponse
newUpdateGameResponse pHttpStatus_ =
  UpdateGameResponse'
    { game = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the game.
updateGameResponse_game :: Lens.Lens' UpdateGameResponse (Prelude.Maybe GameDetails)
updateGameResponse_game = Lens.lens (\UpdateGameResponse' {game} -> game) (\s@UpdateGameResponse' {} a -> s {game = a} :: UpdateGameResponse)

-- | The response's http status code.
updateGameResponse_httpStatus :: Lens.Lens' UpdateGameResponse Prelude.Int
updateGameResponse_httpStatus = Lens.lens (\UpdateGameResponse' {httpStatus} -> httpStatus) (\s@UpdateGameResponse' {} a -> s {httpStatus = a} :: UpdateGameResponse)

instance Prelude.NFData UpdateGameResponse where
  rnf UpdateGameResponse' {..} =
    Prelude.rnf game
      `Prelude.seq` Prelude.rnf httpStatus
