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
-- Module      : Amazonka.SageMaker.CreateEdgeDeploymentPlan
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an edge deployment plan, consisting of multiple stages. Each
-- stage may have a different deployment configuration and devices.
module Amazonka.SageMaker.CreateEdgeDeploymentPlan
  ( -- * Creating a Request
    CreateEdgeDeploymentPlan (..),
    newCreateEdgeDeploymentPlan,

    -- * Request Lenses
    createEdgeDeploymentPlan_tags,
    createEdgeDeploymentPlan_stages,
    createEdgeDeploymentPlan_edgeDeploymentPlanName,
    createEdgeDeploymentPlan_modelConfigs,
    createEdgeDeploymentPlan_deviceFleetName,

    -- * Destructuring the Response
    CreateEdgeDeploymentPlanResponse (..),
    newCreateEdgeDeploymentPlanResponse,

    -- * Response Lenses
    createEdgeDeploymentPlanResponse_httpStatus,
    createEdgeDeploymentPlanResponse_edgeDeploymentPlanArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateEdgeDeploymentPlan' smart constructor.
data CreateEdgeDeploymentPlan = CreateEdgeDeploymentPlan'
  { -- | List of tags with which to tag the edge deployment plan.
    tags :: Prelude.Maybe [Tag],
    -- | List of stages of the edge deployment plan. The number of stages is
    -- limited to 10 per deployment.
    stages :: Prelude.Maybe [DeploymentStage],
    -- | The name of the edge deployment plan.
    edgeDeploymentPlanName :: Prelude.Text,
    -- | List of models associated with the edge deployment plan.
    modelConfigs :: [EdgeDeploymentModelConfig],
    -- | The device fleet used for this edge deployment plan.
    deviceFleetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEdgeDeploymentPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createEdgeDeploymentPlan_tags' - List of tags with which to tag the edge deployment plan.
--
-- 'stages', 'createEdgeDeploymentPlan_stages' - List of stages of the edge deployment plan. The number of stages is
-- limited to 10 per deployment.
--
-- 'edgeDeploymentPlanName', 'createEdgeDeploymentPlan_edgeDeploymentPlanName' - The name of the edge deployment plan.
--
-- 'modelConfigs', 'createEdgeDeploymentPlan_modelConfigs' - List of models associated with the edge deployment plan.
--
-- 'deviceFleetName', 'createEdgeDeploymentPlan_deviceFleetName' - The device fleet used for this edge deployment plan.
newCreateEdgeDeploymentPlan ::
  -- | 'edgeDeploymentPlanName'
  Prelude.Text ->
  -- | 'deviceFleetName'
  Prelude.Text ->
  CreateEdgeDeploymentPlan
newCreateEdgeDeploymentPlan
  pEdgeDeploymentPlanName_
  pDeviceFleetName_ =
    CreateEdgeDeploymentPlan'
      { tags = Prelude.Nothing,
        stages = Prelude.Nothing,
        edgeDeploymentPlanName = pEdgeDeploymentPlanName_,
        modelConfigs = Prelude.mempty,
        deviceFleetName = pDeviceFleetName_
      }

-- | List of tags with which to tag the edge deployment plan.
createEdgeDeploymentPlan_tags :: Lens.Lens' CreateEdgeDeploymentPlan (Prelude.Maybe [Tag])
createEdgeDeploymentPlan_tags = Lens.lens (\CreateEdgeDeploymentPlan' {tags} -> tags) (\s@CreateEdgeDeploymentPlan' {} a -> s {tags = a} :: CreateEdgeDeploymentPlan) Prelude.. Lens.mapping Lens.coerced

-- | List of stages of the edge deployment plan. The number of stages is
-- limited to 10 per deployment.
createEdgeDeploymentPlan_stages :: Lens.Lens' CreateEdgeDeploymentPlan (Prelude.Maybe [DeploymentStage])
createEdgeDeploymentPlan_stages = Lens.lens (\CreateEdgeDeploymentPlan' {stages} -> stages) (\s@CreateEdgeDeploymentPlan' {} a -> s {stages = a} :: CreateEdgeDeploymentPlan) Prelude.. Lens.mapping Lens.coerced

-- | The name of the edge deployment plan.
createEdgeDeploymentPlan_edgeDeploymentPlanName :: Lens.Lens' CreateEdgeDeploymentPlan Prelude.Text
createEdgeDeploymentPlan_edgeDeploymentPlanName = Lens.lens (\CreateEdgeDeploymentPlan' {edgeDeploymentPlanName} -> edgeDeploymentPlanName) (\s@CreateEdgeDeploymentPlan' {} a -> s {edgeDeploymentPlanName = a} :: CreateEdgeDeploymentPlan)

-- | List of models associated with the edge deployment plan.
createEdgeDeploymentPlan_modelConfigs :: Lens.Lens' CreateEdgeDeploymentPlan [EdgeDeploymentModelConfig]
createEdgeDeploymentPlan_modelConfigs = Lens.lens (\CreateEdgeDeploymentPlan' {modelConfigs} -> modelConfigs) (\s@CreateEdgeDeploymentPlan' {} a -> s {modelConfigs = a} :: CreateEdgeDeploymentPlan) Prelude.. Lens.coerced

-- | The device fleet used for this edge deployment plan.
createEdgeDeploymentPlan_deviceFleetName :: Lens.Lens' CreateEdgeDeploymentPlan Prelude.Text
createEdgeDeploymentPlan_deviceFleetName = Lens.lens (\CreateEdgeDeploymentPlan' {deviceFleetName} -> deviceFleetName) (\s@CreateEdgeDeploymentPlan' {} a -> s {deviceFleetName = a} :: CreateEdgeDeploymentPlan)

instance Core.AWSRequest CreateEdgeDeploymentPlan where
  type
    AWSResponse CreateEdgeDeploymentPlan =
      CreateEdgeDeploymentPlanResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEdgeDeploymentPlanResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "EdgeDeploymentPlanArn")
      )

instance Prelude.Hashable CreateEdgeDeploymentPlan where
  hashWithSalt _salt CreateEdgeDeploymentPlan' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` stages
      `Prelude.hashWithSalt` edgeDeploymentPlanName
      `Prelude.hashWithSalt` modelConfigs
      `Prelude.hashWithSalt` deviceFleetName

instance Prelude.NFData CreateEdgeDeploymentPlan where
  rnf CreateEdgeDeploymentPlan' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf stages
      `Prelude.seq` Prelude.rnf edgeDeploymentPlanName
      `Prelude.seq` Prelude.rnf modelConfigs
      `Prelude.seq` Prelude.rnf deviceFleetName

instance Core.ToHeaders CreateEdgeDeploymentPlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.CreateEdgeDeploymentPlan" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateEdgeDeploymentPlan where
  toJSON CreateEdgeDeploymentPlan' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("Stages" Core..=) Prelude.<$> stages,
            Prelude.Just
              ( "EdgeDeploymentPlanName"
                  Core..= edgeDeploymentPlanName
              ),
            Prelude.Just ("ModelConfigs" Core..= modelConfigs),
            Prelude.Just
              ("DeviceFleetName" Core..= deviceFleetName)
          ]
      )

instance Core.ToPath CreateEdgeDeploymentPlan where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateEdgeDeploymentPlan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEdgeDeploymentPlanResponse' smart constructor.
data CreateEdgeDeploymentPlanResponse = CreateEdgeDeploymentPlanResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the edge deployment plan.
    edgeDeploymentPlanArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEdgeDeploymentPlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createEdgeDeploymentPlanResponse_httpStatus' - The response's http status code.
--
-- 'edgeDeploymentPlanArn', 'createEdgeDeploymentPlanResponse_edgeDeploymentPlanArn' - The ARN of the edge deployment plan.
newCreateEdgeDeploymentPlanResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'edgeDeploymentPlanArn'
  Prelude.Text ->
  CreateEdgeDeploymentPlanResponse
newCreateEdgeDeploymentPlanResponse
  pHttpStatus_
  pEdgeDeploymentPlanArn_ =
    CreateEdgeDeploymentPlanResponse'
      { httpStatus =
          pHttpStatus_,
        edgeDeploymentPlanArn =
          pEdgeDeploymentPlanArn_
      }

-- | The response's http status code.
createEdgeDeploymentPlanResponse_httpStatus :: Lens.Lens' CreateEdgeDeploymentPlanResponse Prelude.Int
createEdgeDeploymentPlanResponse_httpStatus = Lens.lens (\CreateEdgeDeploymentPlanResponse' {httpStatus} -> httpStatus) (\s@CreateEdgeDeploymentPlanResponse' {} a -> s {httpStatus = a} :: CreateEdgeDeploymentPlanResponse)

-- | The ARN of the edge deployment plan.
createEdgeDeploymentPlanResponse_edgeDeploymentPlanArn :: Lens.Lens' CreateEdgeDeploymentPlanResponse Prelude.Text
createEdgeDeploymentPlanResponse_edgeDeploymentPlanArn = Lens.lens (\CreateEdgeDeploymentPlanResponse' {edgeDeploymentPlanArn} -> edgeDeploymentPlanArn) (\s@CreateEdgeDeploymentPlanResponse' {} a -> s {edgeDeploymentPlanArn = a} :: CreateEdgeDeploymentPlanResponse)

instance
  Prelude.NFData
    CreateEdgeDeploymentPlanResponse
  where
  rnf CreateEdgeDeploymentPlanResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf edgeDeploymentPlanArn
