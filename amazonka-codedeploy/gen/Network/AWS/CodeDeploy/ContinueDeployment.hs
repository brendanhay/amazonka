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
-- Module      : Network.AWS.CodeDeploy.ContinueDeployment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For a blue\/green deployment, starts the process of rerouting traffic
-- from instances in the original environment to instances in the
-- replacement environment without waiting for a specified wait time to
-- elapse. (Traffic rerouting, which is achieved by registering instances
-- in the replacement environment with the load balancer, can start as soon
-- as all instances have a status of Ready.)
module Network.AWS.CodeDeploy.ContinueDeployment
  ( -- * Creating a Request
    ContinueDeployment (..),
    newContinueDeployment,

    -- * Request Lenses
    continueDeployment_deploymentId,
    continueDeployment_deploymentWaitType,

    -- * Destructuring the Response
    ContinueDeploymentResponse (..),
    newContinueDeploymentResponse,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newContinueDeployment' smart constructor.
data ContinueDeployment = ContinueDeployment'
  { -- | The unique ID of a blue\/green deployment for which you want to start
    -- rerouting traffic to the replacement environment.
    deploymentId :: Core.Maybe Core.Text,
    -- | The status of the deployment\'s waiting period. @READY_WAIT@ indicates
    -- that the deployment is ready to start shifting traffic.
    -- @TERMINATION_WAIT@ indicates that the traffic is shifted, but the
    -- original target is not terminated.
    deploymentWaitType :: Core.Maybe DeploymentWaitType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ContinueDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentId', 'continueDeployment_deploymentId' - The unique ID of a blue\/green deployment for which you want to start
-- rerouting traffic to the replacement environment.
--
-- 'deploymentWaitType', 'continueDeployment_deploymentWaitType' - The status of the deployment\'s waiting period. @READY_WAIT@ indicates
-- that the deployment is ready to start shifting traffic.
-- @TERMINATION_WAIT@ indicates that the traffic is shifted, but the
-- original target is not terminated.
newContinueDeployment ::
  ContinueDeployment
newContinueDeployment =
  ContinueDeployment'
    { deploymentId = Core.Nothing,
      deploymentWaitType = Core.Nothing
    }

-- | The unique ID of a blue\/green deployment for which you want to start
-- rerouting traffic to the replacement environment.
continueDeployment_deploymentId :: Lens.Lens' ContinueDeployment (Core.Maybe Core.Text)
continueDeployment_deploymentId = Lens.lens (\ContinueDeployment' {deploymentId} -> deploymentId) (\s@ContinueDeployment' {} a -> s {deploymentId = a} :: ContinueDeployment)

-- | The status of the deployment\'s waiting period. @READY_WAIT@ indicates
-- that the deployment is ready to start shifting traffic.
-- @TERMINATION_WAIT@ indicates that the traffic is shifted, but the
-- original target is not terminated.
continueDeployment_deploymentWaitType :: Lens.Lens' ContinueDeployment (Core.Maybe DeploymentWaitType)
continueDeployment_deploymentWaitType = Lens.lens (\ContinueDeployment' {deploymentWaitType} -> deploymentWaitType) (\s@ContinueDeployment' {} a -> s {deploymentWaitType = a} :: ContinueDeployment)

instance Core.AWSRequest ContinueDeployment where
  type
    AWSResponse ContinueDeployment =
      ContinueDeploymentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull ContinueDeploymentResponse'

instance Core.Hashable ContinueDeployment

instance Core.NFData ContinueDeployment

instance Core.ToHeaders ContinueDeployment where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.ContinueDeployment" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ContinueDeployment where
  toJSON ContinueDeployment' {..} =
    Core.object
      ( Core.catMaybes
          [ ("deploymentId" Core..=) Core.<$> deploymentId,
            ("deploymentWaitType" Core..=)
              Core.<$> deploymentWaitType
          ]
      )

instance Core.ToPath ContinueDeployment where
  toPath = Core.const "/"

instance Core.ToQuery ContinueDeployment where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newContinueDeploymentResponse' smart constructor.
data ContinueDeploymentResponse = ContinueDeploymentResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ContinueDeploymentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newContinueDeploymentResponse ::
  ContinueDeploymentResponse
newContinueDeploymentResponse =
  ContinueDeploymentResponse'

instance Core.NFData ContinueDeploymentResponse
