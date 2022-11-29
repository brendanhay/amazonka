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
-- Module      : Amazonka.SageMaker.DescribeEdgeDeploymentPlan
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an edge deployment plan with deployment status per stage.
module Amazonka.SageMaker.DescribeEdgeDeploymentPlan
  ( -- * Creating a Request
    DescribeEdgeDeploymentPlan (..),
    newDescribeEdgeDeploymentPlan,

    -- * Request Lenses
    describeEdgeDeploymentPlan_nextToken,
    describeEdgeDeploymentPlan_maxResults,
    describeEdgeDeploymentPlan_edgeDeploymentPlanName,

    -- * Destructuring the Response
    DescribeEdgeDeploymentPlanResponse (..),
    newDescribeEdgeDeploymentPlanResponse,

    -- * Response Lenses
    describeEdgeDeploymentPlanResponse_nextToken,
    describeEdgeDeploymentPlanResponse_edgeDeploymentFailed,
    describeEdgeDeploymentPlanResponse_lastModifiedTime,
    describeEdgeDeploymentPlanResponse_creationTime,
    describeEdgeDeploymentPlanResponse_edgeDeploymentPending,
    describeEdgeDeploymentPlanResponse_edgeDeploymentSuccess,
    describeEdgeDeploymentPlanResponse_httpStatus,
    describeEdgeDeploymentPlanResponse_edgeDeploymentPlanArn,
    describeEdgeDeploymentPlanResponse_edgeDeploymentPlanName,
    describeEdgeDeploymentPlanResponse_modelConfigs,
    describeEdgeDeploymentPlanResponse_deviceFleetName,
    describeEdgeDeploymentPlanResponse_stages,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeEdgeDeploymentPlan' smart constructor.
data DescribeEdgeDeploymentPlan = DescribeEdgeDeploymentPlan'
  { -- | If the edge deployment plan has enough stages to require tokening, then
    -- this is the response from the last list of stages returned.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to select (50 by default).
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The name of the deployment plan to describe.
    edgeDeploymentPlanName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEdgeDeploymentPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeEdgeDeploymentPlan_nextToken' - If the edge deployment plan has enough stages to require tokening, then
-- this is the response from the last list of stages returned.
--
-- 'maxResults', 'describeEdgeDeploymentPlan_maxResults' - The maximum number of results to select (50 by default).
--
-- 'edgeDeploymentPlanName', 'describeEdgeDeploymentPlan_edgeDeploymentPlanName' - The name of the deployment plan to describe.
newDescribeEdgeDeploymentPlan ::
  -- | 'edgeDeploymentPlanName'
  Prelude.Text ->
  DescribeEdgeDeploymentPlan
newDescribeEdgeDeploymentPlan
  pEdgeDeploymentPlanName_ =
    DescribeEdgeDeploymentPlan'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        edgeDeploymentPlanName =
          pEdgeDeploymentPlanName_
      }

-- | If the edge deployment plan has enough stages to require tokening, then
-- this is the response from the last list of stages returned.
describeEdgeDeploymentPlan_nextToken :: Lens.Lens' DescribeEdgeDeploymentPlan (Prelude.Maybe Prelude.Text)
describeEdgeDeploymentPlan_nextToken = Lens.lens (\DescribeEdgeDeploymentPlan' {nextToken} -> nextToken) (\s@DescribeEdgeDeploymentPlan' {} a -> s {nextToken = a} :: DescribeEdgeDeploymentPlan)

-- | The maximum number of results to select (50 by default).
describeEdgeDeploymentPlan_maxResults :: Lens.Lens' DescribeEdgeDeploymentPlan (Prelude.Maybe Prelude.Int)
describeEdgeDeploymentPlan_maxResults = Lens.lens (\DescribeEdgeDeploymentPlan' {maxResults} -> maxResults) (\s@DescribeEdgeDeploymentPlan' {} a -> s {maxResults = a} :: DescribeEdgeDeploymentPlan)

-- | The name of the deployment plan to describe.
describeEdgeDeploymentPlan_edgeDeploymentPlanName :: Lens.Lens' DescribeEdgeDeploymentPlan Prelude.Text
describeEdgeDeploymentPlan_edgeDeploymentPlanName = Lens.lens (\DescribeEdgeDeploymentPlan' {edgeDeploymentPlanName} -> edgeDeploymentPlanName) (\s@DescribeEdgeDeploymentPlan' {} a -> s {edgeDeploymentPlanName = a} :: DescribeEdgeDeploymentPlan)

instance Core.AWSRequest DescribeEdgeDeploymentPlan where
  type
    AWSResponse DescribeEdgeDeploymentPlan =
      DescribeEdgeDeploymentPlanResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEdgeDeploymentPlanResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "EdgeDeploymentFailed")
            Prelude.<*> (x Core..?> "LastModifiedTime")
            Prelude.<*> (x Core..?> "CreationTime")
            Prelude.<*> (x Core..?> "EdgeDeploymentPending")
            Prelude.<*> (x Core..?> "EdgeDeploymentSuccess")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "EdgeDeploymentPlanArn")
            Prelude.<*> (x Core..:> "EdgeDeploymentPlanName")
            Prelude.<*> (x Core..?> "ModelConfigs" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..:> "DeviceFleetName")
            Prelude.<*> (x Core..?> "Stages" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable DescribeEdgeDeploymentPlan where
  hashWithSalt _salt DescribeEdgeDeploymentPlan' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` edgeDeploymentPlanName

instance Prelude.NFData DescribeEdgeDeploymentPlan where
  rnf DescribeEdgeDeploymentPlan' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf edgeDeploymentPlanName

instance Core.ToHeaders DescribeEdgeDeploymentPlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DescribeEdgeDeploymentPlan" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeEdgeDeploymentPlan where
  toJSON DescribeEdgeDeploymentPlan' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just
              ( "EdgeDeploymentPlanName"
                  Core..= edgeDeploymentPlanName
              )
          ]
      )

instance Core.ToPath DescribeEdgeDeploymentPlan where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeEdgeDeploymentPlan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEdgeDeploymentPlanResponse' smart constructor.
data DescribeEdgeDeploymentPlanResponse = DescribeEdgeDeploymentPlanResponse'
  { -- | Token to use when calling the next set of stages in the edge deployment
    -- plan.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of edge devices that failed the deployment.
    edgeDeploymentFailed :: Prelude.Maybe Prelude.Int,
    -- | The time when the edge deployment plan was last updated.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The time when the edge deployment plan was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The number of edge devices yet to pick up deployment, or in progress.
    edgeDeploymentPending :: Prelude.Maybe Prelude.Int,
    -- | The number of edge devices with the successful deployment.
    edgeDeploymentSuccess :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of edge deployment plan.
    edgeDeploymentPlanArn :: Prelude.Text,
    -- | The name of the edge deployment plan.
    edgeDeploymentPlanName :: Prelude.Text,
    -- | List of models associated with the edge deployment plan.
    modelConfigs :: [EdgeDeploymentModelConfig],
    -- | The device fleet used for this edge deployment plan.
    deviceFleetName :: Prelude.Text,
    -- | List of stages in the edge deployment plan.
    stages :: [DeploymentStageStatusSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEdgeDeploymentPlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeEdgeDeploymentPlanResponse_nextToken' - Token to use when calling the next set of stages in the edge deployment
-- plan.
--
-- 'edgeDeploymentFailed', 'describeEdgeDeploymentPlanResponse_edgeDeploymentFailed' - The number of edge devices that failed the deployment.
--
-- 'lastModifiedTime', 'describeEdgeDeploymentPlanResponse_lastModifiedTime' - The time when the edge deployment plan was last updated.
--
-- 'creationTime', 'describeEdgeDeploymentPlanResponse_creationTime' - The time when the edge deployment plan was created.
--
-- 'edgeDeploymentPending', 'describeEdgeDeploymentPlanResponse_edgeDeploymentPending' - The number of edge devices yet to pick up deployment, or in progress.
--
-- 'edgeDeploymentSuccess', 'describeEdgeDeploymentPlanResponse_edgeDeploymentSuccess' - The number of edge devices with the successful deployment.
--
-- 'httpStatus', 'describeEdgeDeploymentPlanResponse_httpStatus' - The response's http status code.
--
-- 'edgeDeploymentPlanArn', 'describeEdgeDeploymentPlanResponse_edgeDeploymentPlanArn' - The ARN of edge deployment plan.
--
-- 'edgeDeploymentPlanName', 'describeEdgeDeploymentPlanResponse_edgeDeploymentPlanName' - The name of the edge deployment plan.
--
-- 'modelConfigs', 'describeEdgeDeploymentPlanResponse_modelConfigs' - List of models associated with the edge deployment plan.
--
-- 'deviceFleetName', 'describeEdgeDeploymentPlanResponse_deviceFleetName' - The device fleet used for this edge deployment plan.
--
-- 'stages', 'describeEdgeDeploymentPlanResponse_stages' - List of stages in the edge deployment plan.
newDescribeEdgeDeploymentPlanResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'edgeDeploymentPlanArn'
  Prelude.Text ->
  -- | 'edgeDeploymentPlanName'
  Prelude.Text ->
  -- | 'deviceFleetName'
  Prelude.Text ->
  DescribeEdgeDeploymentPlanResponse
newDescribeEdgeDeploymentPlanResponse
  pHttpStatus_
  pEdgeDeploymentPlanArn_
  pEdgeDeploymentPlanName_
  pDeviceFleetName_ =
    DescribeEdgeDeploymentPlanResponse'
      { nextToken =
          Prelude.Nothing,
        edgeDeploymentFailed = Prelude.Nothing,
        lastModifiedTime = Prelude.Nothing,
        creationTime = Prelude.Nothing,
        edgeDeploymentPending = Prelude.Nothing,
        edgeDeploymentSuccess = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        edgeDeploymentPlanArn =
          pEdgeDeploymentPlanArn_,
        edgeDeploymentPlanName =
          pEdgeDeploymentPlanName_,
        modelConfigs = Prelude.mempty,
        deviceFleetName = pDeviceFleetName_,
        stages = Prelude.mempty
      }

-- | Token to use when calling the next set of stages in the edge deployment
-- plan.
describeEdgeDeploymentPlanResponse_nextToken :: Lens.Lens' DescribeEdgeDeploymentPlanResponse (Prelude.Maybe Prelude.Text)
describeEdgeDeploymentPlanResponse_nextToken = Lens.lens (\DescribeEdgeDeploymentPlanResponse' {nextToken} -> nextToken) (\s@DescribeEdgeDeploymentPlanResponse' {} a -> s {nextToken = a} :: DescribeEdgeDeploymentPlanResponse)

-- | The number of edge devices that failed the deployment.
describeEdgeDeploymentPlanResponse_edgeDeploymentFailed :: Lens.Lens' DescribeEdgeDeploymentPlanResponse (Prelude.Maybe Prelude.Int)
describeEdgeDeploymentPlanResponse_edgeDeploymentFailed = Lens.lens (\DescribeEdgeDeploymentPlanResponse' {edgeDeploymentFailed} -> edgeDeploymentFailed) (\s@DescribeEdgeDeploymentPlanResponse' {} a -> s {edgeDeploymentFailed = a} :: DescribeEdgeDeploymentPlanResponse)

-- | The time when the edge deployment plan was last updated.
describeEdgeDeploymentPlanResponse_lastModifiedTime :: Lens.Lens' DescribeEdgeDeploymentPlanResponse (Prelude.Maybe Prelude.UTCTime)
describeEdgeDeploymentPlanResponse_lastModifiedTime = Lens.lens (\DescribeEdgeDeploymentPlanResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeEdgeDeploymentPlanResponse' {} a -> s {lastModifiedTime = a} :: DescribeEdgeDeploymentPlanResponse) Prelude.. Lens.mapping Core._Time

-- | The time when the edge deployment plan was created.
describeEdgeDeploymentPlanResponse_creationTime :: Lens.Lens' DescribeEdgeDeploymentPlanResponse (Prelude.Maybe Prelude.UTCTime)
describeEdgeDeploymentPlanResponse_creationTime = Lens.lens (\DescribeEdgeDeploymentPlanResponse' {creationTime} -> creationTime) (\s@DescribeEdgeDeploymentPlanResponse' {} a -> s {creationTime = a} :: DescribeEdgeDeploymentPlanResponse) Prelude.. Lens.mapping Core._Time

-- | The number of edge devices yet to pick up deployment, or in progress.
describeEdgeDeploymentPlanResponse_edgeDeploymentPending :: Lens.Lens' DescribeEdgeDeploymentPlanResponse (Prelude.Maybe Prelude.Int)
describeEdgeDeploymentPlanResponse_edgeDeploymentPending = Lens.lens (\DescribeEdgeDeploymentPlanResponse' {edgeDeploymentPending} -> edgeDeploymentPending) (\s@DescribeEdgeDeploymentPlanResponse' {} a -> s {edgeDeploymentPending = a} :: DescribeEdgeDeploymentPlanResponse)

-- | The number of edge devices with the successful deployment.
describeEdgeDeploymentPlanResponse_edgeDeploymentSuccess :: Lens.Lens' DescribeEdgeDeploymentPlanResponse (Prelude.Maybe Prelude.Int)
describeEdgeDeploymentPlanResponse_edgeDeploymentSuccess = Lens.lens (\DescribeEdgeDeploymentPlanResponse' {edgeDeploymentSuccess} -> edgeDeploymentSuccess) (\s@DescribeEdgeDeploymentPlanResponse' {} a -> s {edgeDeploymentSuccess = a} :: DescribeEdgeDeploymentPlanResponse)

-- | The response's http status code.
describeEdgeDeploymentPlanResponse_httpStatus :: Lens.Lens' DescribeEdgeDeploymentPlanResponse Prelude.Int
describeEdgeDeploymentPlanResponse_httpStatus = Lens.lens (\DescribeEdgeDeploymentPlanResponse' {httpStatus} -> httpStatus) (\s@DescribeEdgeDeploymentPlanResponse' {} a -> s {httpStatus = a} :: DescribeEdgeDeploymentPlanResponse)

-- | The ARN of edge deployment plan.
describeEdgeDeploymentPlanResponse_edgeDeploymentPlanArn :: Lens.Lens' DescribeEdgeDeploymentPlanResponse Prelude.Text
describeEdgeDeploymentPlanResponse_edgeDeploymentPlanArn = Lens.lens (\DescribeEdgeDeploymentPlanResponse' {edgeDeploymentPlanArn} -> edgeDeploymentPlanArn) (\s@DescribeEdgeDeploymentPlanResponse' {} a -> s {edgeDeploymentPlanArn = a} :: DescribeEdgeDeploymentPlanResponse)

-- | The name of the edge deployment plan.
describeEdgeDeploymentPlanResponse_edgeDeploymentPlanName :: Lens.Lens' DescribeEdgeDeploymentPlanResponse Prelude.Text
describeEdgeDeploymentPlanResponse_edgeDeploymentPlanName = Lens.lens (\DescribeEdgeDeploymentPlanResponse' {edgeDeploymentPlanName} -> edgeDeploymentPlanName) (\s@DescribeEdgeDeploymentPlanResponse' {} a -> s {edgeDeploymentPlanName = a} :: DescribeEdgeDeploymentPlanResponse)

-- | List of models associated with the edge deployment plan.
describeEdgeDeploymentPlanResponse_modelConfigs :: Lens.Lens' DescribeEdgeDeploymentPlanResponse [EdgeDeploymentModelConfig]
describeEdgeDeploymentPlanResponse_modelConfigs = Lens.lens (\DescribeEdgeDeploymentPlanResponse' {modelConfigs} -> modelConfigs) (\s@DescribeEdgeDeploymentPlanResponse' {} a -> s {modelConfigs = a} :: DescribeEdgeDeploymentPlanResponse) Prelude.. Lens.coerced

-- | The device fleet used for this edge deployment plan.
describeEdgeDeploymentPlanResponse_deviceFleetName :: Lens.Lens' DescribeEdgeDeploymentPlanResponse Prelude.Text
describeEdgeDeploymentPlanResponse_deviceFleetName = Lens.lens (\DescribeEdgeDeploymentPlanResponse' {deviceFleetName} -> deviceFleetName) (\s@DescribeEdgeDeploymentPlanResponse' {} a -> s {deviceFleetName = a} :: DescribeEdgeDeploymentPlanResponse)

-- | List of stages in the edge deployment plan.
describeEdgeDeploymentPlanResponse_stages :: Lens.Lens' DescribeEdgeDeploymentPlanResponse [DeploymentStageStatusSummary]
describeEdgeDeploymentPlanResponse_stages = Lens.lens (\DescribeEdgeDeploymentPlanResponse' {stages} -> stages) (\s@DescribeEdgeDeploymentPlanResponse' {} a -> s {stages = a} :: DescribeEdgeDeploymentPlanResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    DescribeEdgeDeploymentPlanResponse
  where
  rnf DescribeEdgeDeploymentPlanResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf edgeDeploymentFailed
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf edgeDeploymentPending
      `Prelude.seq` Prelude.rnf edgeDeploymentSuccess
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf edgeDeploymentPlanArn
      `Prelude.seq` Prelude.rnf edgeDeploymentPlanName
      `Prelude.seq` Prelude.rnf modelConfigs
      `Prelude.seq` Prelude.rnf deviceFleetName
      `Prelude.seq` Prelude.rnf stages
