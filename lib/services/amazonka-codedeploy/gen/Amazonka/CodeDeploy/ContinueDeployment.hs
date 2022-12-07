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
-- Module      : Amazonka.CodeDeploy.ContinueDeployment
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.CodeDeploy.ContinueDeployment
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

import Amazonka.CodeDeploy.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest ContinueDeployment where
  type
    AWSResponse ContinueDeployment =
      ContinueDeploymentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull ContinueDeploymentResponse'

instance Prelude.Hashable ContinueDeployment where
  hashWithSalt _salt ContinueDeployment' {..} =
    _salt `Prelude.hashWithSalt` deploymentId
      `Prelude.hashWithSalt` deploymentWaitType

instance Prelude.NFData ContinueDeployment where
  rnf ContinueDeployment' {..} =
    Prelude.rnf deploymentId
      `Prelude.seq` Prelude.rnf deploymentWaitType

instance Data.ToHeaders ContinueDeployment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeDeploy_20141006.ContinueDeployment" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ContinueDeployment where
  toJSON ContinueDeployment' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("deploymentId" Data..=) Prelude.<$> deploymentId,
            ("deploymentWaitType" Data..=)
              Prelude.<$> deploymentWaitType
          ]
      )

instance Data.ToPath ContinueDeployment where
  toPath = Prelude.const "/"

instance Data.ToQuery ContinueDeployment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newContinueDeploymentResponse' smart constructor.
data ContinueDeploymentResponse = ContinueDeploymentResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContinueDeploymentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newContinueDeploymentResponse ::
  ContinueDeploymentResponse
newContinueDeploymentResponse =
  ContinueDeploymentResponse'

instance Prelude.NFData ContinueDeploymentResponse where
  rnf _ = ()
