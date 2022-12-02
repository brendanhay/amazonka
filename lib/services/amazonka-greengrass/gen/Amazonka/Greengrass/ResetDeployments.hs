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
-- Module      : Amazonka.Greengrass.ResetDeployments
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets a group\'s deployments.
module Amazonka.Greengrass.ResetDeployments
  ( -- * Creating a Request
    ResetDeployments (..),
    newResetDeployments,

    -- * Request Lenses
    resetDeployments_force,
    resetDeployments_amznClientToken,
    resetDeployments_groupId,

    -- * Destructuring the Response
    ResetDeploymentsResponse (..),
    newResetDeploymentsResponse,

    -- * Response Lenses
    resetDeploymentsResponse_deploymentId,
    resetDeploymentsResponse_deploymentArn,
    resetDeploymentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Information needed to reset deployments.
--
-- /See:/ 'newResetDeployments' smart constructor.
data ResetDeployments = ResetDeployments'
  { -- | If true, performs a best-effort only core reset.
    force :: Prelude.Maybe Prelude.Bool,
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Greengrass group.
    groupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetDeployments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'force', 'resetDeployments_force' - If true, performs a best-effort only core reset.
--
-- 'amznClientToken', 'resetDeployments_amznClientToken' - A client token used to correlate requests and responses.
--
-- 'groupId', 'resetDeployments_groupId' - The ID of the Greengrass group.
newResetDeployments ::
  -- | 'groupId'
  Prelude.Text ->
  ResetDeployments
newResetDeployments pGroupId_ =
  ResetDeployments'
    { force = Prelude.Nothing,
      amznClientToken = Prelude.Nothing,
      groupId = pGroupId_
    }

-- | If true, performs a best-effort only core reset.
resetDeployments_force :: Lens.Lens' ResetDeployments (Prelude.Maybe Prelude.Bool)
resetDeployments_force = Lens.lens (\ResetDeployments' {force} -> force) (\s@ResetDeployments' {} a -> s {force = a} :: ResetDeployments)

-- | A client token used to correlate requests and responses.
resetDeployments_amznClientToken :: Lens.Lens' ResetDeployments (Prelude.Maybe Prelude.Text)
resetDeployments_amznClientToken = Lens.lens (\ResetDeployments' {amznClientToken} -> amznClientToken) (\s@ResetDeployments' {} a -> s {amznClientToken = a} :: ResetDeployments)

-- | The ID of the Greengrass group.
resetDeployments_groupId :: Lens.Lens' ResetDeployments Prelude.Text
resetDeployments_groupId = Lens.lens (\ResetDeployments' {groupId} -> groupId) (\s@ResetDeployments' {} a -> s {groupId = a} :: ResetDeployments)

instance Core.AWSRequest ResetDeployments where
  type
    AWSResponse ResetDeployments =
      ResetDeploymentsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ResetDeploymentsResponse'
            Prelude.<$> (x Data..?> "DeploymentId")
            Prelude.<*> (x Data..?> "DeploymentArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ResetDeployments where
  hashWithSalt _salt ResetDeployments' {..} =
    _salt `Prelude.hashWithSalt` force
      `Prelude.hashWithSalt` amznClientToken
      `Prelude.hashWithSalt` groupId

instance Prelude.NFData ResetDeployments where
  rnf ResetDeployments' {..} =
    Prelude.rnf force
      `Prelude.seq` Prelude.rnf amznClientToken
      `Prelude.seq` Prelude.rnf groupId

instance Data.ToHeaders ResetDeployments where
  toHeaders ResetDeployments' {..} =
    Prelude.mconcat
      [ "X-Amzn-Client-Token" Data.=# amznClientToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON ResetDeployments where
  toJSON ResetDeployments' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Force" Data..=) Prelude.<$> force]
      )

instance Data.ToPath ResetDeployments where
  toPath ResetDeployments' {..} =
    Prelude.mconcat
      [ "/greengrass/groups/",
        Data.toBS groupId,
        "/deployments/$reset"
      ]

instance Data.ToQuery ResetDeployments where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newResetDeploymentsResponse' smart constructor.
data ResetDeploymentsResponse = ResetDeploymentsResponse'
  { -- | The ID of the deployment.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the deployment.
    deploymentArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetDeploymentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentId', 'resetDeploymentsResponse_deploymentId' - The ID of the deployment.
--
-- 'deploymentArn', 'resetDeploymentsResponse_deploymentArn' - The ARN of the deployment.
--
-- 'httpStatus', 'resetDeploymentsResponse_httpStatus' - The response's http status code.
newResetDeploymentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ResetDeploymentsResponse
newResetDeploymentsResponse pHttpStatus_ =
  ResetDeploymentsResponse'
    { deploymentId =
        Prelude.Nothing,
      deploymentArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the deployment.
resetDeploymentsResponse_deploymentId :: Lens.Lens' ResetDeploymentsResponse (Prelude.Maybe Prelude.Text)
resetDeploymentsResponse_deploymentId = Lens.lens (\ResetDeploymentsResponse' {deploymentId} -> deploymentId) (\s@ResetDeploymentsResponse' {} a -> s {deploymentId = a} :: ResetDeploymentsResponse)

-- | The ARN of the deployment.
resetDeploymentsResponse_deploymentArn :: Lens.Lens' ResetDeploymentsResponse (Prelude.Maybe Prelude.Text)
resetDeploymentsResponse_deploymentArn = Lens.lens (\ResetDeploymentsResponse' {deploymentArn} -> deploymentArn) (\s@ResetDeploymentsResponse' {} a -> s {deploymentArn = a} :: ResetDeploymentsResponse)

-- | The response's http status code.
resetDeploymentsResponse_httpStatus :: Lens.Lens' ResetDeploymentsResponse Prelude.Int
resetDeploymentsResponse_httpStatus = Lens.lens (\ResetDeploymentsResponse' {httpStatus} -> httpStatus) (\s@ResetDeploymentsResponse' {} a -> s {httpStatus = a} :: ResetDeploymentsResponse)

instance Prelude.NFData ResetDeploymentsResponse where
  rnf ResetDeploymentsResponse' {..} =
    Prelude.rnf deploymentId
      `Prelude.seq` Prelude.rnf deploymentArn
      `Prelude.seq` Prelude.rnf httpStatus
