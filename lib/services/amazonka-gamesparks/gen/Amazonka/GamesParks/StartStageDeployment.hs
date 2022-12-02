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
-- Module      : Amazonka.GamesParks.StartStageDeployment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deploys a snapshot to the stage and creates a new game runtime.
--
-- After you call this operation, you can check the deployment status by
-- using @GetStageDeployment@.
--
-- If there are any players connected to the previous game runtime, then
-- both runtimes persist. Existing connections to the previous runtime are
-- maintained. When players disconnect and reconnect, they connect to the
-- new runtime. After there are no connections to the previous game
-- runtime, it is deleted.
module Amazonka.GamesParks.StartStageDeployment
  ( -- * Creating a Request
    StartStageDeployment (..),
    newStartStageDeployment,

    -- * Request Lenses
    startStageDeployment_clientToken,
    startStageDeployment_gameName,
    startStageDeployment_snapshotId,
    startStageDeployment_stageName,

    -- * Destructuring the Response
    StartStageDeploymentResponse (..),
    newStartStageDeploymentResponse,

    -- * Response Lenses
    startStageDeploymentResponse_stageDeployment,
    startStageDeploymentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GamesParks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartStageDeployment' smart constructor.
data StartStageDeployment = StartStageDeployment'
  { -- | A client-defined token. With an active client token in the request, this
    -- action is idempotent.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the game.
    gameName :: Prelude.Text,
    -- | The identifier of the snapshot to deploy.
    snapshotId :: Prelude.Text,
    -- | The name of the stage to deploy the snapshot onto.
    stageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartStageDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'startStageDeployment_clientToken' - A client-defined token. With an active client token in the request, this
-- action is idempotent.
--
-- 'gameName', 'startStageDeployment_gameName' - The name of the game.
--
-- 'snapshotId', 'startStageDeployment_snapshotId' - The identifier of the snapshot to deploy.
--
-- 'stageName', 'startStageDeployment_stageName' - The name of the stage to deploy the snapshot onto.
newStartStageDeployment ::
  -- | 'gameName'
  Prelude.Text ->
  -- | 'snapshotId'
  Prelude.Text ->
  -- | 'stageName'
  Prelude.Text ->
  StartStageDeployment
newStartStageDeployment
  pGameName_
  pSnapshotId_
  pStageName_ =
    StartStageDeployment'
      { clientToken =
          Prelude.Nothing,
        gameName = pGameName_,
        snapshotId = pSnapshotId_,
        stageName = pStageName_
      }

-- | A client-defined token. With an active client token in the request, this
-- action is idempotent.
startStageDeployment_clientToken :: Lens.Lens' StartStageDeployment (Prelude.Maybe Prelude.Text)
startStageDeployment_clientToken = Lens.lens (\StartStageDeployment' {clientToken} -> clientToken) (\s@StartStageDeployment' {} a -> s {clientToken = a} :: StartStageDeployment)

-- | The name of the game.
startStageDeployment_gameName :: Lens.Lens' StartStageDeployment Prelude.Text
startStageDeployment_gameName = Lens.lens (\StartStageDeployment' {gameName} -> gameName) (\s@StartStageDeployment' {} a -> s {gameName = a} :: StartStageDeployment)

-- | The identifier of the snapshot to deploy.
startStageDeployment_snapshotId :: Lens.Lens' StartStageDeployment Prelude.Text
startStageDeployment_snapshotId = Lens.lens (\StartStageDeployment' {snapshotId} -> snapshotId) (\s@StartStageDeployment' {} a -> s {snapshotId = a} :: StartStageDeployment)

-- | The name of the stage to deploy the snapshot onto.
startStageDeployment_stageName :: Lens.Lens' StartStageDeployment Prelude.Text
startStageDeployment_stageName = Lens.lens (\StartStageDeployment' {stageName} -> stageName) (\s@StartStageDeployment' {} a -> s {stageName = a} :: StartStageDeployment)

instance Core.AWSRequest StartStageDeployment where
  type
    AWSResponse StartStageDeployment =
      StartStageDeploymentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartStageDeploymentResponse'
            Prelude.<$> (x Data..?> "StageDeployment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartStageDeployment where
  hashWithSalt _salt StartStageDeployment' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` gameName
      `Prelude.hashWithSalt` snapshotId
      `Prelude.hashWithSalt` stageName

instance Prelude.NFData StartStageDeployment where
  rnf StartStageDeployment' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf gameName
      `Prelude.seq` Prelude.rnf snapshotId
      `Prelude.seq` Prelude.rnf stageName

instance Data.ToHeaders StartStageDeployment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartStageDeployment where
  toJSON StartStageDeployment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("SnapshotId" Data..= snapshotId)
          ]
      )

instance Data.ToPath StartStageDeployment where
  toPath StartStageDeployment' {..} =
    Prelude.mconcat
      [ "/game/",
        Data.toBS gameName,
        "/stage/",
        Data.toBS stageName,
        "/deployment"
      ]

instance Data.ToQuery StartStageDeployment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartStageDeploymentResponse' smart constructor.
data StartStageDeploymentResponse = StartStageDeploymentResponse'
  { -- | Properties that describe the stage deployment.
    stageDeployment :: Prelude.Maybe StageDeploymentDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartStageDeploymentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stageDeployment', 'startStageDeploymentResponse_stageDeployment' - Properties that describe the stage deployment.
--
-- 'httpStatus', 'startStageDeploymentResponse_httpStatus' - The response's http status code.
newStartStageDeploymentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartStageDeploymentResponse
newStartStageDeploymentResponse pHttpStatus_ =
  StartStageDeploymentResponse'
    { stageDeployment =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Properties that describe the stage deployment.
startStageDeploymentResponse_stageDeployment :: Lens.Lens' StartStageDeploymentResponse (Prelude.Maybe StageDeploymentDetails)
startStageDeploymentResponse_stageDeployment = Lens.lens (\StartStageDeploymentResponse' {stageDeployment} -> stageDeployment) (\s@StartStageDeploymentResponse' {} a -> s {stageDeployment = a} :: StartStageDeploymentResponse)

-- | The response's http status code.
startStageDeploymentResponse_httpStatus :: Lens.Lens' StartStageDeploymentResponse Prelude.Int
startStageDeploymentResponse_httpStatus = Lens.lens (\StartStageDeploymentResponse' {httpStatus} -> httpStatus) (\s@StartStageDeploymentResponse' {} a -> s {httpStatus = a} :: StartStageDeploymentResponse)

instance Prelude.NFData StartStageDeploymentResponse where
  rnf StartStageDeploymentResponse' {..} =
    Prelude.rnf stageDeployment
      `Prelude.seq` Prelude.rnf httpStatus
