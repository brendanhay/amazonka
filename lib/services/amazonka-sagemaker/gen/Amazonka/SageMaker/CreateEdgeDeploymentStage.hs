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
-- Module      : Amazonka.SageMaker.CreateEdgeDeploymentStage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new stage in an existing edge deployment plan.
module Amazonka.SageMaker.CreateEdgeDeploymentStage
  ( -- * Creating a Request
    CreateEdgeDeploymentStage (..),
    newCreateEdgeDeploymentStage,

    -- * Request Lenses
    createEdgeDeploymentStage_edgeDeploymentPlanName,
    createEdgeDeploymentStage_stages,

    -- * Destructuring the Response
    CreateEdgeDeploymentStageResponse (..),
    newCreateEdgeDeploymentStageResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateEdgeDeploymentStage' smart constructor.
data CreateEdgeDeploymentStage = CreateEdgeDeploymentStage'
  { -- | The name of the edge deployment plan.
    edgeDeploymentPlanName :: Prelude.Text,
    -- | List of stages to be added to the edge deployment plan.
    stages :: [DeploymentStage]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEdgeDeploymentStage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'edgeDeploymentPlanName', 'createEdgeDeploymentStage_edgeDeploymentPlanName' - The name of the edge deployment plan.
--
-- 'stages', 'createEdgeDeploymentStage_stages' - List of stages to be added to the edge deployment plan.
newCreateEdgeDeploymentStage ::
  -- | 'edgeDeploymentPlanName'
  Prelude.Text ->
  CreateEdgeDeploymentStage
newCreateEdgeDeploymentStage pEdgeDeploymentPlanName_ =
  CreateEdgeDeploymentStage'
    { edgeDeploymentPlanName =
        pEdgeDeploymentPlanName_,
      stages = Prelude.mempty
    }

-- | The name of the edge deployment plan.
createEdgeDeploymentStage_edgeDeploymentPlanName :: Lens.Lens' CreateEdgeDeploymentStage Prelude.Text
createEdgeDeploymentStage_edgeDeploymentPlanName = Lens.lens (\CreateEdgeDeploymentStage' {edgeDeploymentPlanName} -> edgeDeploymentPlanName) (\s@CreateEdgeDeploymentStage' {} a -> s {edgeDeploymentPlanName = a} :: CreateEdgeDeploymentStage)

-- | List of stages to be added to the edge deployment plan.
createEdgeDeploymentStage_stages :: Lens.Lens' CreateEdgeDeploymentStage [DeploymentStage]
createEdgeDeploymentStage_stages = Lens.lens (\CreateEdgeDeploymentStage' {stages} -> stages) (\s@CreateEdgeDeploymentStage' {} a -> s {stages = a} :: CreateEdgeDeploymentStage) Prelude.. Lens.coerced

instance Core.AWSRequest CreateEdgeDeploymentStage where
  type
    AWSResponse CreateEdgeDeploymentStage =
      CreateEdgeDeploymentStageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      CreateEdgeDeploymentStageResponse'

instance Prelude.Hashable CreateEdgeDeploymentStage where
  hashWithSalt _salt CreateEdgeDeploymentStage' {..} =
    _salt
      `Prelude.hashWithSalt` edgeDeploymentPlanName
      `Prelude.hashWithSalt` stages

instance Prelude.NFData CreateEdgeDeploymentStage where
  rnf CreateEdgeDeploymentStage' {..} =
    Prelude.rnf edgeDeploymentPlanName
      `Prelude.seq` Prelude.rnf stages

instance Data.ToHeaders CreateEdgeDeploymentStage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.CreateEdgeDeploymentStage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateEdgeDeploymentStage where
  toJSON CreateEdgeDeploymentStage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "EdgeDeploymentPlanName"
                  Data..= edgeDeploymentPlanName
              ),
            Prelude.Just ("Stages" Data..= stages)
          ]
      )

instance Data.ToPath CreateEdgeDeploymentStage where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateEdgeDeploymentStage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEdgeDeploymentStageResponse' smart constructor.
data CreateEdgeDeploymentStageResponse = CreateEdgeDeploymentStageResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEdgeDeploymentStageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreateEdgeDeploymentStageResponse ::
  CreateEdgeDeploymentStageResponse
newCreateEdgeDeploymentStageResponse =
  CreateEdgeDeploymentStageResponse'

instance
  Prelude.NFData
    CreateEdgeDeploymentStageResponse
  where
  rnf _ = ()
