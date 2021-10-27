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
-- Module      : Network.AWS.AppConfig.GetDeploymentStrategy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve information about a deployment strategy. A deployment strategy
-- defines important criteria for rolling out your configuration to the
-- designated targets. A deployment strategy includes: the overall duration
-- required, a percentage of targets to receive the deployment during each
-- interval, an algorithm that defines how percentage grows, and bake time.
module Network.AWS.AppConfig.GetDeploymentStrategy
  ( -- * Creating a Request
    GetDeploymentStrategy (..),
    newGetDeploymentStrategy,

    -- * Request Lenses
    getDeploymentStrategy_deploymentStrategyId,

    -- * Destructuring the Response
    DeploymentStrategy (..),
    newDeploymentStrategy,

    -- * Response Lenses
    deploymentStrategy_growthFactor,
    deploymentStrategy_replicateTo,
    deploymentStrategy_name,
    deploymentStrategy_id,
    deploymentStrategy_deploymentDurationInMinutes,
    deploymentStrategy_finalBakeTimeInMinutes,
    deploymentStrategy_description,
    deploymentStrategy_growthType,
  )
where

import Network.AWS.AppConfig.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDeploymentStrategy' smart constructor.
data GetDeploymentStrategy = GetDeploymentStrategy'
  { -- | The ID of the deployment strategy to get.
    deploymentStrategyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDeploymentStrategy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentStrategyId', 'getDeploymentStrategy_deploymentStrategyId' - The ID of the deployment strategy to get.
newGetDeploymentStrategy ::
  -- | 'deploymentStrategyId'
  Prelude.Text ->
  GetDeploymentStrategy
newGetDeploymentStrategy pDeploymentStrategyId_ =
  GetDeploymentStrategy'
    { deploymentStrategyId =
        pDeploymentStrategyId_
    }

-- | The ID of the deployment strategy to get.
getDeploymentStrategy_deploymentStrategyId :: Lens.Lens' GetDeploymentStrategy Prelude.Text
getDeploymentStrategy_deploymentStrategyId = Lens.lens (\GetDeploymentStrategy' {deploymentStrategyId} -> deploymentStrategyId) (\s@GetDeploymentStrategy' {} a -> s {deploymentStrategyId = a} :: GetDeploymentStrategy)

instance Core.AWSRequest GetDeploymentStrategy where
  type
    AWSResponse GetDeploymentStrategy =
      DeploymentStrategy
  request = Request.get defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable GetDeploymentStrategy

instance Prelude.NFData GetDeploymentStrategy

instance Core.ToHeaders GetDeploymentStrategy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetDeploymentStrategy where
  toPath GetDeploymentStrategy' {..} =
    Prelude.mconcat
      [ "/deploymentstrategies/",
        Core.toBS deploymentStrategyId
      ]

instance Core.ToQuery GetDeploymentStrategy where
  toQuery = Prelude.const Prelude.mempty
