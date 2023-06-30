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
-- Module      : Amazonka.GamesParks.UpdateStage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the metadata of a stage.
module Amazonka.GamesParks.UpdateStage
  ( -- * Creating a Request
    UpdateStage (..),
    newUpdateStage,

    -- * Request Lenses
    updateStage_description,
    updateStage_role,
    updateStage_gameName,
    updateStage_stageName,

    -- * Destructuring the Response
    UpdateStageResponse (..),
    newUpdateStageResponse,

    -- * Response Lenses
    updateStageResponse_stage,
    updateStageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GamesParks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateStage' smart constructor.
data UpdateStage = UpdateStage'
  { -- | The description of the stage.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the role to use for the game snapshots
    -- deployed to this stage.
    role' :: Prelude.Maybe Prelude.Text,
    -- | The name of the game.
    gameName :: Prelude.Text,
    -- | The name of the stage.
    stageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateStage_description' - The description of the stage.
--
-- 'role'', 'updateStage_role' - The Amazon Resource Name (ARN) of the role to use for the game snapshots
-- deployed to this stage.
--
-- 'gameName', 'updateStage_gameName' - The name of the game.
--
-- 'stageName', 'updateStage_stageName' - The name of the stage.
newUpdateStage ::
  -- | 'gameName'
  Prelude.Text ->
  -- | 'stageName'
  Prelude.Text ->
  UpdateStage
newUpdateStage pGameName_ pStageName_ =
  UpdateStage'
    { description = Prelude.Nothing,
      role' = Prelude.Nothing,
      gameName = pGameName_,
      stageName = pStageName_
    }

-- | The description of the stage.
updateStage_description :: Lens.Lens' UpdateStage (Prelude.Maybe Prelude.Text)
updateStage_description = Lens.lens (\UpdateStage' {description} -> description) (\s@UpdateStage' {} a -> s {description = a} :: UpdateStage)

-- | The Amazon Resource Name (ARN) of the role to use for the game snapshots
-- deployed to this stage.
updateStage_role :: Lens.Lens' UpdateStage (Prelude.Maybe Prelude.Text)
updateStage_role = Lens.lens (\UpdateStage' {role'} -> role') (\s@UpdateStage' {} a -> s {role' = a} :: UpdateStage)

-- | The name of the game.
updateStage_gameName :: Lens.Lens' UpdateStage Prelude.Text
updateStage_gameName = Lens.lens (\UpdateStage' {gameName} -> gameName) (\s@UpdateStage' {} a -> s {gameName = a} :: UpdateStage)

-- | The name of the stage.
updateStage_stageName :: Lens.Lens' UpdateStage Prelude.Text
updateStage_stageName = Lens.lens (\UpdateStage' {stageName} -> stageName) (\s@UpdateStage' {} a -> s {stageName = a} :: UpdateStage)

instance Core.AWSRequest UpdateStage where
  type AWSResponse UpdateStage = UpdateStageResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateStageResponse'
            Prelude.<$> (x Data..?> "Stage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateStage where
  hashWithSalt _salt UpdateStage' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` gameName
      `Prelude.hashWithSalt` stageName

instance Prelude.NFData UpdateStage where
  rnf UpdateStage' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf gameName
      `Prelude.seq` Prelude.rnf stageName

instance Data.ToHeaders UpdateStage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateStage where
  toJSON UpdateStage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Role" Data..=) Prelude.<$> role'
          ]
      )

instance Data.ToPath UpdateStage where
  toPath UpdateStage' {..} =
    Prelude.mconcat
      [ "/game/",
        Data.toBS gameName,
        "/stage/",
        Data.toBS stageName
      ]

instance Data.ToQuery UpdateStage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateStageResponse' smart constructor.
data UpdateStageResponse = UpdateStageResponse'
  { -- | Properties that provide details of the updated stage.
    stage :: Prelude.Maybe StageDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stage', 'updateStageResponse_stage' - Properties that provide details of the updated stage.
--
-- 'httpStatus', 'updateStageResponse_httpStatus' - The response's http status code.
newUpdateStageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateStageResponse
newUpdateStageResponse pHttpStatus_ =
  UpdateStageResponse'
    { stage = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Properties that provide details of the updated stage.
updateStageResponse_stage :: Lens.Lens' UpdateStageResponse (Prelude.Maybe StageDetails)
updateStageResponse_stage = Lens.lens (\UpdateStageResponse' {stage} -> stage) (\s@UpdateStageResponse' {} a -> s {stage = a} :: UpdateStageResponse)

-- | The response's http status code.
updateStageResponse_httpStatus :: Lens.Lens' UpdateStageResponse Prelude.Int
updateStageResponse_httpStatus = Lens.lens (\UpdateStageResponse' {httpStatus} -> httpStatus) (\s@UpdateStageResponse' {} a -> s {httpStatus = a} :: UpdateStageResponse)

instance Prelude.NFData UpdateStageResponse where
  rnf UpdateStageResponse' {..} =
    Prelude.rnf stage
      `Prelude.seq` Prelude.rnf httpStatus
