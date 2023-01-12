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
-- Module      : Amazonka.GamesParks.DeleteStage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a stage from a game, along with the associated game runtime.
module Amazonka.GamesParks.DeleteStage
  ( -- * Creating a Request
    DeleteStage (..),
    newDeleteStage,

    -- * Request Lenses
    deleteStage_gameName,
    deleteStage_stageName,

    -- * Destructuring the Response
    DeleteStageResponse (..),
    newDeleteStageResponse,

    -- * Response Lenses
    deleteStageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GamesParks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteStage' smart constructor.
data DeleteStage = DeleteStage'
  { -- | The name of the game.
    gameName :: Prelude.Text,
    -- | The name of the stage to delete.
    stageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameName', 'deleteStage_gameName' - The name of the game.
--
-- 'stageName', 'deleteStage_stageName' - The name of the stage to delete.
newDeleteStage ::
  -- | 'gameName'
  Prelude.Text ->
  -- | 'stageName'
  Prelude.Text ->
  DeleteStage
newDeleteStage pGameName_ pStageName_ =
  DeleteStage'
    { gameName = pGameName_,
      stageName = pStageName_
    }

-- | The name of the game.
deleteStage_gameName :: Lens.Lens' DeleteStage Prelude.Text
deleteStage_gameName = Lens.lens (\DeleteStage' {gameName} -> gameName) (\s@DeleteStage' {} a -> s {gameName = a} :: DeleteStage)

-- | The name of the stage to delete.
deleteStage_stageName :: Lens.Lens' DeleteStage Prelude.Text
deleteStage_stageName = Lens.lens (\DeleteStage' {stageName} -> stageName) (\s@DeleteStage' {} a -> s {stageName = a} :: DeleteStage)

instance Core.AWSRequest DeleteStage where
  type AWSResponse DeleteStage = DeleteStageResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteStageResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteStage where
  hashWithSalt _salt DeleteStage' {..} =
    _salt `Prelude.hashWithSalt` gameName
      `Prelude.hashWithSalt` stageName

instance Prelude.NFData DeleteStage where
  rnf DeleteStage' {..} =
    Prelude.rnf gameName
      `Prelude.seq` Prelude.rnf stageName

instance Data.ToHeaders DeleteStage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteStage where
  toPath DeleteStage' {..} =
    Prelude.mconcat
      [ "/game/",
        Data.toBS gameName,
        "/stage/",
        Data.toBS stageName
      ]

instance Data.ToQuery DeleteStage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteStageResponse' smart constructor.
data DeleteStageResponse = DeleteStageResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteStageResponse_httpStatus' - The response's http status code.
newDeleteStageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteStageResponse
newDeleteStageResponse pHttpStatus_ =
  DeleteStageResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteStageResponse_httpStatus :: Lens.Lens' DeleteStageResponse Prelude.Int
deleteStageResponse_httpStatus = Lens.lens (\DeleteStageResponse' {httpStatus} -> httpStatus) (\s@DeleteStageResponse' {} a -> s {httpStatus = a} :: DeleteStageResponse)

instance Prelude.NFData DeleteStageResponse where
  rnf DeleteStageResponse' {..} = Prelude.rnf httpStatus
