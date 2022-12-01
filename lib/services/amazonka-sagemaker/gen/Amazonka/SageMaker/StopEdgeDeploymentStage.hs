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
-- Module      : Amazonka.SageMaker.StopEdgeDeploymentStage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a stage in an edge deployment plan.
module Amazonka.SageMaker.StopEdgeDeploymentStage
  ( -- * Creating a Request
    StopEdgeDeploymentStage (..),
    newStopEdgeDeploymentStage,

    -- * Request Lenses
    stopEdgeDeploymentStage_edgeDeploymentPlanName,
    stopEdgeDeploymentStage_stageName,

    -- * Destructuring the Response
    StopEdgeDeploymentStageResponse (..),
    newStopEdgeDeploymentStageResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newStopEdgeDeploymentStage' smart constructor.
data StopEdgeDeploymentStage = StopEdgeDeploymentStage'
  { -- | The name of the edge deployment plan to stop.
    edgeDeploymentPlanName :: Prelude.Text,
    -- | The name of the stage to stop.
    stageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopEdgeDeploymentStage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'edgeDeploymentPlanName', 'stopEdgeDeploymentStage_edgeDeploymentPlanName' - The name of the edge deployment plan to stop.
--
-- 'stageName', 'stopEdgeDeploymentStage_stageName' - The name of the stage to stop.
newStopEdgeDeploymentStage ::
  -- | 'edgeDeploymentPlanName'
  Prelude.Text ->
  -- | 'stageName'
  Prelude.Text ->
  StopEdgeDeploymentStage
newStopEdgeDeploymentStage
  pEdgeDeploymentPlanName_
  pStageName_ =
    StopEdgeDeploymentStage'
      { edgeDeploymentPlanName =
          pEdgeDeploymentPlanName_,
        stageName = pStageName_
      }

-- | The name of the edge deployment plan to stop.
stopEdgeDeploymentStage_edgeDeploymentPlanName :: Lens.Lens' StopEdgeDeploymentStage Prelude.Text
stopEdgeDeploymentStage_edgeDeploymentPlanName = Lens.lens (\StopEdgeDeploymentStage' {edgeDeploymentPlanName} -> edgeDeploymentPlanName) (\s@StopEdgeDeploymentStage' {} a -> s {edgeDeploymentPlanName = a} :: StopEdgeDeploymentStage)

-- | The name of the stage to stop.
stopEdgeDeploymentStage_stageName :: Lens.Lens' StopEdgeDeploymentStage Prelude.Text
stopEdgeDeploymentStage_stageName = Lens.lens (\StopEdgeDeploymentStage' {stageName} -> stageName) (\s@StopEdgeDeploymentStage' {} a -> s {stageName = a} :: StopEdgeDeploymentStage)

instance Core.AWSRequest StopEdgeDeploymentStage where
  type
    AWSResponse StopEdgeDeploymentStage =
      StopEdgeDeploymentStageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      StopEdgeDeploymentStageResponse'

instance Prelude.Hashable StopEdgeDeploymentStage where
  hashWithSalt _salt StopEdgeDeploymentStage' {..} =
    _salt `Prelude.hashWithSalt` edgeDeploymentPlanName
      `Prelude.hashWithSalt` stageName

instance Prelude.NFData StopEdgeDeploymentStage where
  rnf StopEdgeDeploymentStage' {..} =
    Prelude.rnf edgeDeploymentPlanName
      `Prelude.seq` Prelude.rnf stageName

instance Core.ToHeaders StopEdgeDeploymentStage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.StopEdgeDeploymentStage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StopEdgeDeploymentStage where
  toJSON StopEdgeDeploymentStage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "EdgeDeploymentPlanName"
                  Core..= edgeDeploymentPlanName
              ),
            Prelude.Just ("StageName" Core..= stageName)
          ]
      )

instance Core.ToPath StopEdgeDeploymentStage where
  toPath = Prelude.const "/"

instance Core.ToQuery StopEdgeDeploymentStage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopEdgeDeploymentStageResponse' smart constructor.
data StopEdgeDeploymentStageResponse = StopEdgeDeploymentStageResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopEdgeDeploymentStageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStopEdgeDeploymentStageResponse ::
  StopEdgeDeploymentStageResponse
newStopEdgeDeploymentStageResponse =
  StopEdgeDeploymentStageResponse'

instance
  Prelude.NFData
    StopEdgeDeploymentStageResponse
  where
  rnf _ = ()
