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
-- Module      : Amazonka.SageMaker.StartEdgeDeploymentStage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a stage in an edge deployment plan.
module Amazonka.SageMaker.StartEdgeDeploymentStage
  ( -- * Creating a Request
    StartEdgeDeploymentStage (..),
    newStartEdgeDeploymentStage,

    -- * Request Lenses
    startEdgeDeploymentStage_edgeDeploymentPlanName,
    startEdgeDeploymentStage_stageName,

    -- * Destructuring the Response
    StartEdgeDeploymentStageResponse (..),
    newStartEdgeDeploymentStageResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newStartEdgeDeploymentStage' smart constructor.
data StartEdgeDeploymentStage = StartEdgeDeploymentStage'
  { -- | The name of the edge deployment plan to start.
    edgeDeploymentPlanName :: Prelude.Text,
    -- | The name of the stage to start.
    stageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartEdgeDeploymentStage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'edgeDeploymentPlanName', 'startEdgeDeploymentStage_edgeDeploymentPlanName' - The name of the edge deployment plan to start.
--
-- 'stageName', 'startEdgeDeploymentStage_stageName' - The name of the stage to start.
newStartEdgeDeploymentStage ::
  -- | 'edgeDeploymentPlanName'
  Prelude.Text ->
  -- | 'stageName'
  Prelude.Text ->
  StartEdgeDeploymentStage
newStartEdgeDeploymentStage
  pEdgeDeploymentPlanName_
  pStageName_ =
    StartEdgeDeploymentStage'
      { edgeDeploymentPlanName =
          pEdgeDeploymentPlanName_,
        stageName = pStageName_
      }

-- | The name of the edge deployment plan to start.
startEdgeDeploymentStage_edgeDeploymentPlanName :: Lens.Lens' StartEdgeDeploymentStage Prelude.Text
startEdgeDeploymentStage_edgeDeploymentPlanName = Lens.lens (\StartEdgeDeploymentStage' {edgeDeploymentPlanName} -> edgeDeploymentPlanName) (\s@StartEdgeDeploymentStage' {} a -> s {edgeDeploymentPlanName = a} :: StartEdgeDeploymentStage)

-- | The name of the stage to start.
startEdgeDeploymentStage_stageName :: Lens.Lens' StartEdgeDeploymentStage Prelude.Text
startEdgeDeploymentStage_stageName = Lens.lens (\StartEdgeDeploymentStage' {stageName} -> stageName) (\s@StartEdgeDeploymentStage' {} a -> s {stageName = a} :: StartEdgeDeploymentStage)

instance Core.AWSRequest StartEdgeDeploymentStage where
  type
    AWSResponse StartEdgeDeploymentStage =
      StartEdgeDeploymentStageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      StartEdgeDeploymentStageResponse'

instance Prelude.Hashable StartEdgeDeploymentStage where
  hashWithSalt _salt StartEdgeDeploymentStage' {..} =
    _salt
      `Prelude.hashWithSalt` edgeDeploymentPlanName
      `Prelude.hashWithSalt` stageName

instance Prelude.NFData StartEdgeDeploymentStage where
  rnf StartEdgeDeploymentStage' {..} =
    Prelude.rnf edgeDeploymentPlanName
      `Prelude.seq` Prelude.rnf stageName

instance Data.ToHeaders StartEdgeDeploymentStage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.StartEdgeDeploymentStage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartEdgeDeploymentStage where
  toJSON StartEdgeDeploymentStage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "EdgeDeploymentPlanName"
                  Data..= edgeDeploymentPlanName
              ),
            Prelude.Just ("StageName" Data..= stageName)
          ]
      )

instance Data.ToPath StartEdgeDeploymentStage where
  toPath = Prelude.const "/"

instance Data.ToQuery StartEdgeDeploymentStage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartEdgeDeploymentStageResponse' smart constructor.
data StartEdgeDeploymentStageResponse = StartEdgeDeploymentStageResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartEdgeDeploymentStageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStartEdgeDeploymentStageResponse ::
  StartEdgeDeploymentStageResponse
newStartEdgeDeploymentStageResponse =
  StartEdgeDeploymentStageResponse'

instance
  Prelude.NFData
    StartEdgeDeploymentStageResponse
  where
  rnf _ = ()
