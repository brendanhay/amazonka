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
-- Module      : Amazonka.GamesParks.GetStage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a stage.
module Amazonka.GamesParks.GetStage
  ( -- * Creating a Request
    GetStage (..),
    newGetStage,

    -- * Request Lenses
    getStage_gameName,
    getStage_stageName,

    -- * Destructuring the Response
    GetStageResponse (..),
    newGetStageResponse,

    -- * Response Lenses
    getStageResponse_stage,
    getStageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GamesParks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetStage' smart constructor.
data GetStage = GetStage'
  { -- | The name of the game.
    gameName :: Prelude.Text,
    -- | The name of the stage.
    stageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gameName', 'getStage_gameName' - The name of the game.
--
-- 'stageName', 'getStage_stageName' - The name of the stage.
newGetStage ::
  -- | 'gameName'
  Prelude.Text ->
  -- | 'stageName'
  Prelude.Text ->
  GetStage
newGetStage pGameName_ pStageName_ =
  GetStage'
    { gameName = pGameName_,
      stageName = pStageName_
    }

-- | The name of the game.
getStage_gameName :: Lens.Lens' GetStage Prelude.Text
getStage_gameName = Lens.lens (\GetStage' {gameName} -> gameName) (\s@GetStage' {} a -> s {gameName = a} :: GetStage)

-- | The name of the stage.
getStage_stageName :: Lens.Lens' GetStage Prelude.Text
getStage_stageName = Lens.lens (\GetStage' {stageName} -> stageName) (\s@GetStage' {} a -> s {stageName = a} :: GetStage)

instance Core.AWSRequest GetStage where
  type AWSResponse GetStage = GetStageResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStageResponse'
            Prelude.<$> (x Data..?> "Stage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetStage where
  hashWithSalt _salt GetStage' {..} =
    _salt
      `Prelude.hashWithSalt` gameName
      `Prelude.hashWithSalt` stageName

instance Prelude.NFData GetStage where
  rnf GetStage' {..} =
    Prelude.rnf gameName
      `Prelude.seq` Prelude.rnf stageName

instance Data.ToHeaders GetStage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetStage where
  toPath GetStage' {..} =
    Prelude.mconcat
      [ "/game/",
        Data.toBS gameName,
        "/stage/",
        Data.toBS stageName
      ]

instance Data.ToQuery GetStage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetStageResponse' smart constructor.
data GetStageResponse = GetStageResponse'
  { -- | Properties that provide details of the stage.
    stage :: Prelude.Maybe StageDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stage', 'getStageResponse_stage' - Properties that provide details of the stage.
--
-- 'httpStatus', 'getStageResponse_httpStatus' - The response's http status code.
newGetStageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetStageResponse
newGetStageResponse pHttpStatus_ =
  GetStageResponse'
    { stage = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Properties that provide details of the stage.
getStageResponse_stage :: Lens.Lens' GetStageResponse (Prelude.Maybe StageDetails)
getStageResponse_stage = Lens.lens (\GetStageResponse' {stage} -> stage) (\s@GetStageResponse' {} a -> s {stage = a} :: GetStageResponse)

-- | The response's http status code.
getStageResponse_httpStatus :: Lens.Lens' GetStageResponse Prelude.Int
getStageResponse_httpStatus = Lens.lens (\GetStageResponse' {httpStatus} -> httpStatus) (\s@GetStageResponse' {} a -> s {httpStatus = a} :: GetStageResponse)

instance Prelude.NFData GetStageResponse where
  rnf GetStageResponse' {..} =
    Prelude.rnf stage
      `Prelude.seq` Prelude.rnf httpStatus
