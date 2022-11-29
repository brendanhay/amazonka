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
-- Module      : Amazonka.GamesParks.DeleteGame
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a game.
module Amazonka.GamesParks.DeleteGame
  ( -- * Creating a Request
    DeleteGame (..),
    newDeleteGame,

    -- * Request Lenses
    deleteGame_gameName,

    -- * Destructuring the Response
    DeleteGameResponse (..),
    newDeleteGameResponse,

    -- * Response Lenses
    deleteGameResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GamesParks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteGame' smart constructor.
data DeleteGame = DeleteGame'
  { -- | The name of the game to delete.
    gameName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteGame' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameName', 'deleteGame_gameName' - The name of the game to delete.
newDeleteGame ::
  -- | 'gameName'
  Prelude.Text ->
  DeleteGame
newDeleteGame pGameName_ =
  DeleteGame' {gameName = pGameName_}

-- | The name of the game to delete.
deleteGame_gameName :: Lens.Lens' DeleteGame Prelude.Text
deleteGame_gameName = Lens.lens (\DeleteGame' {gameName} -> gameName) (\s@DeleteGame' {} a -> s {gameName = a} :: DeleteGame)

instance Core.AWSRequest DeleteGame where
  type AWSResponse DeleteGame = DeleteGameResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteGameResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteGame where
  hashWithSalt _salt DeleteGame' {..} =
    _salt `Prelude.hashWithSalt` gameName

instance Prelude.NFData DeleteGame where
  rnf DeleteGame' {..} = Prelude.rnf gameName

instance Core.ToHeaders DeleteGame where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteGame where
  toPath DeleteGame' {..} =
    Prelude.mconcat ["/game/", Core.toBS gameName]

instance Core.ToQuery DeleteGame where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteGameResponse' smart constructor.
data DeleteGameResponse = DeleteGameResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteGameResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteGameResponse_httpStatus' - The response's http status code.
newDeleteGameResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteGameResponse
newDeleteGameResponse pHttpStatus_ =
  DeleteGameResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteGameResponse_httpStatus :: Lens.Lens' DeleteGameResponse Prelude.Int
deleteGameResponse_httpStatus = Lens.lens (\DeleteGameResponse' {httpStatus} -> httpStatus) (\s@DeleteGameResponse' {} a -> s {httpStatus = a} :: DeleteGameResponse)

instance Prelude.NFData DeleteGameResponse where
  rnf DeleteGameResponse' {..} = Prelude.rnf httpStatus
