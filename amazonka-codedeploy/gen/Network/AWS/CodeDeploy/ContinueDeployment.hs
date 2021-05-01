{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newContinueDeployment' smart constructor.
data ContinueDeployment = ContinueDeployment'
  { -- | The unique ID of a blue\/green deployment for which you want to start
    -- rerouting traffic to the replacement environment.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | The status of the deployment\'s waiting period. @READY_WAIT@ indicates
    -- that the deployment is ready to start shifting traffic.
    -- @TERMINATION_WAIT@ indicates that the traffic is shifted, but the
    -- original target is not terminated.
    deploymentWaitType :: Prelude.Maybe DeploymentWaitType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { deploymentId = Prelude.Nothing,
      deploymentWaitType = Prelude.Nothing
    }

-- | The unique ID of a blue\/green deployment for which you want to start
-- rerouting traffic to the replacement environment.
continueDeployment_deploymentId :: Lens.Lens' ContinueDeployment (Prelude.Maybe Prelude.Text)
continueDeployment_deploymentId = Lens.lens (\ContinueDeployment' {deploymentId} -> deploymentId) (\s@ContinueDeployment' {} a -> s {deploymentId = a} :: ContinueDeployment)

-- | The status of the deployment\'s waiting period. @READY_WAIT@ indicates
-- that the deployment is ready to start shifting traffic.
-- @TERMINATION_WAIT@ indicates that the traffic is shifted, but the
-- original target is not terminated.
continueDeployment_deploymentWaitType :: Lens.Lens' ContinueDeployment (Prelude.Maybe DeploymentWaitType)
continueDeployment_deploymentWaitType = Lens.lens (\ContinueDeployment' {deploymentWaitType} -> deploymentWaitType) (\s@ContinueDeployment' {} a -> s {deploymentWaitType = a} :: ContinueDeployment)

instance Prelude.AWSRequest ContinueDeployment where
  type
    Rs ContinueDeployment =
      ContinueDeploymentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull ContinueDeploymentResponse'

instance Prelude.Hashable ContinueDeployment

instance Prelude.NFData ContinueDeployment

instance Prelude.ToHeaders ContinueDeployment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeDeploy_20141006.ContinueDeployment" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ContinueDeployment where
  toJSON ContinueDeployment' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("deploymentId" Prelude..=)
              Prelude.<$> deploymentId,
            ("deploymentWaitType" Prelude..=)
              Prelude.<$> deploymentWaitType
          ]
      )

instance Prelude.ToPath ContinueDeployment where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ContinueDeployment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newContinueDeploymentResponse' smart constructor.
data ContinueDeploymentResponse = ContinueDeploymentResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ContinueDeploymentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newContinueDeploymentResponse ::
  ContinueDeploymentResponse
newContinueDeploymentResponse =
  ContinueDeploymentResponse'

instance Prelude.NFData ContinueDeploymentResponse
