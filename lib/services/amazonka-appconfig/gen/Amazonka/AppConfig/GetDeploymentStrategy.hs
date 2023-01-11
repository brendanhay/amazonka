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
-- Module      : Amazonka.AppConfig.GetDeploymentStrategy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a deployment strategy. A deployment strategy
-- defines important criteria for rolling out your configuration to the
-- designated targets. A deployment strategy includes the overall duration
-- required, a percentage of targets to receive the deployment during each
-- interval, an algorithm that defines how percentage grows, and bake time.
module Amazonka.AppConfig.GetDeploymentStrategy
  ( -- * Creating a Request
    GetDeploymentStrategy (..),
    newGetDeploymentStrategy,

    -- * Request Lenses
    getDeploymentStrategy_deploymentStrategyId,

    -- * Destructuring the Response
    DeploymentStrategy (..),
    newDeploymentStrategy,

    -- * Response Lenses
    deploymentStrategy_deploymentDurationInMinutes,
    deploymentStrategy_description,
    deploymentStrategy_finalBakeTimeInMinutes,
    deploymentStrategy_growthFactor,
    deploymentStrategy_growthType,
    deploymentStrategy_id,
    deploymentStrategy_name,
    deploymentStrategy_replicateTo,
  )
where

import Amazonka.AppConfig.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable GetDeploymentStrategy where
  hashWithSalt _salt GetDeploymentStrategy' {..} =
    _salt `Prelude.hashWithSalt` deploymentStrategyId

instance Prelude.NFData GetDeploymentStrategy where
  rnf GetDeploymentStrategy' {..} =
    Prelude.rnf deploymentStrategyId

instance Data.ToHeaders GetDeploymentStrategy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetDeploymentStrategy where
  toPath GetDeploymentStrategy' {..} =
    Prelude.mconcat
      [ "/deploymentstrategies/",
        Data.toBS deploymentStrategyId
      ]

instance Data.ToQuery GetDeploymentStrategy where
  toQuery = Prelude.const Prelude.mempty
