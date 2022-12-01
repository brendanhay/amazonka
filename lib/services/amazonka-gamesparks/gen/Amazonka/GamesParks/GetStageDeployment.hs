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
-- Module      : Amazonka.GamesParks.GetStageDeployment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a stage deployment.
module Amazonka.GamesParks.GetStageDeployment
  ( -- * Creating a Request
    GetStageDeployment (..),
    newGetStageDeployment,

    -- * Request Lenses
    getStageDeployment_deploymentId,
    getStageDeployment_gameName,
    getStageDeployment_stageName,

    -- * Destructuring the Response
    GetStageDeploymentResponse (..),
    newGetStageDeploymentResponse,

    -- * Response Lenses
    getStageDeploymentResponse_stageDeployment,
    getStageDeploymentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GamesParks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetStageDeployment' smart constructor.
data GetStageDeployment = GetStageDeployment'
  { -- | The identifier of the stage deployment. @StartStageDeployment@ returns
    -- the identifier that you use here.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | The name of the game.
    gameName :: Prelude.Text,
    -- | The name of the stage.
    stageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStageDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentId', 'getStageDeployment_deploymentId' - The identifier of the stage deployment. @StartStageDeployment@ returns
-- the identifier that you use here.
--
-- 'gameName', 'getStageDeployment_gameName' - The name of the game.
--
-- 'stageName', 'getStageDeployment_stageName' - The name of the stage.
newGetStageDeployment ::
  -- | 'gameName'
  Prelude.Text ->
  -- | 'stageName'
  Prelude.Text ->
  GetStageDeployment
newGetStageDeployment pGameName_ pStageName_ =
  GetStageDeployment'
    { deploymentId = Prelude.Nothing,
      gameName = pGameName_,
      stageName = pStageName_
    }

-- | The identifier of the stage deployment. @StartStageDeployment@ returns
-- the identifier that you use here.
getStageDeployment_deploymentId :: Lens.Lens' GetStageDeployment (Prelude.Maybe Prelude.Text)
getStageDeployment_deploymentId = Lens.lens (\GetStageDeployment' {deploymentId} -> deploymentId) (\s@GetStageDeployment' {} a -> s {deploymentId = a} :: GetStageDeployment)

-- | The name of the game.
getStageDeployment_gameName :: Lens.Lens' GetStageDeployment Prelude.Text
getStageDeployment_gameName = Lens.lens (\GetStageDeployment' {gameName} -> gameName) (\s@GetStageDeployment' {} a -> s {gameName = a} :: GetStageDeployment)

-- | The name of the stage.
getStageDeployment_stageName :: Lens.Lens' GetStageDeployment Prelude.Text
getStageDeployment_stageName = Lens.lens (\GetStageDeployment' {stageName} -> stageName) (\s@GetStageDeployment' {} a -> s {stageName = a} :: GetStageDeployment)

instance Core.AWSRequest GetStageDeployment where
  type
    AWSResponse GetStageDeployment =
      GetStageDeploymentResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStageDeploymentResponse'
            Prelude.<$> (x Core..?> "StageDeployment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetStageDeployment where
  hashWithSalt _salt GetStageDeployment' {..} =
    _salt `Prelude.hashWithSalt` deploymentId
      `Prelude.hashWithSalt` gameName
      `Prelude.hashWithSalt` stageName

instance Prelude.NFData GetStageDeployment where
  rnf GetStageDeployment' {..} =
    Prelude.rnf deploymentId
      `Prelude.seq` Prelude.rnf gameName
      `Prelude.seq` Prelude.rnf stageName

instance Core.ToHeaders GetStageDeployment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetStageDeployment where
  toPath GetStageDeployment' {..} =
    Prelude.mconcat
      [ "/game/",
        Core.toBS gameName,
        "/stage/",
        Core.toBS stageName,
        "/deployment"
      ]

instance Core.ToQuery GetStageDeployment where
  toQuery GetStageDeployment' {..} =
    Prelude.mconcat
      ["DeploymentId" Core.=: deploymentId]

-- | /See:/ 'newGetStageDeploymentResponse' smart constructor.
data GetStageDeploymentResponse = GetStageDeploymentResponse'
  { -- | Properties that provide details of the stage deployment.
    stageDeployment :: Prelude.Maybe StageDeploymentDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStageDeploymentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stageDeployment', 'getStageDeploymentResponse_stageDeployment' - Properties that provide details of the stage deployment.
--
-- 'httpStatus', 'getStageDeploymentResponse_httpStatus' - The response's http status code.
newGetStageDeploymentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetStageDeploymentResponse
newGetStageDeploymentResponse pHttpStatus_ =
  GetStageDeploymentResponse'
    { stageDeployment =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Properties that provide details of the stage deployment.
getStageDeploymentResponse_stageDeployment :: Lens.Lens' GetStageDeploymentResponse (Prelude.Maybe StageDeploymentDetails)
getStageDeploymentResponse_stageDeployment = Lens.lens (\GetStageDeploymentResponse' {stageDeployment} -> stageDeployment) (\s@GetStageDeploymentResponse' {} a -> s {stageDeployment = a} :: GetStageDeploymentResponse)

-- | The response's http status code.
getStageDeploymentResponse_httpStatus :: Lens.Lens' GetStageDeploymentResponse Prelude.Int
getStageDeploymentResponse_httpStatus = Lens.lens (\GetStageDeploymentResponse' {httpStatus} -> httpStatus) (\s@GetStageDeploymentResponse' {} a -> s {httpStatus = a} :: GetStageDeploymentResponse)

instance Prelude.NFData GetStageDeploymentResponse where
  rnf GetStageDeploymentResponse' {..} =
    Prelude.rnf stageDeployment
      `Prelude.seq` Prelude.rnf httpStatus
