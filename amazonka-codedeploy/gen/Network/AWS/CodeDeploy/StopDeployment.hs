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
-- Module      : Network.AWS.CodeDeploy.StopDeployment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to stop an ongoing deployment.
module Network.AWS.CodeDeploy.StopDeployment
  ( -- * Creating a Request
    StopDeployment (..),
    newStopDeployment,

    -- * Request Lenses
    stopDeployment_autoRollbackEnabled,
    stopDeployment_deploymentId,

    -- * Destructuring the Response
    StopDeploymentResponse (..),
    newStopDeploymentResponse,

    -- * Response Lenses
    stopDeploymentResponse_statusMessage,
    stopDeploymentResponse_status,
    stopDeploymentResponse_httpStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @StopDeployment@ operation.
--
-- /See:/ 'newStopDeployment' smart constructor.
data StopDeployment = StopDeployment'
  { -- | Indicates, when a deployment is stopped, whether instances that have
    -- been updated should be rolled back to the previous version of the
    -- application revision.
    autoRollbackEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The unique ID of a deployment.
    deploymentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoRollbackEnabled', 'stopDeployment_autoRollbackEnabled' - Indicates, when a deployment is stopped, whether instances that have
-- been updated should be rolled back to the previous version of the
-- application revision.
--
-- 'deploymentId', 'stopDeployment_deploymentId' - The unique ID of a deployment.
newStopDeployment ::
  -- | 'deploymentId'
  Prelude.Text ->
  StopDeployment
newStopDeployment pDeploymentId_ =
  StopDeployment'
    { autoRollbackEnabled =
        Prelude.Nothing,
      deploymentId = pDeploymentId_
    }

-- | Indicates, when a deployment is stopped, whether instances that have
-- been updated should be rolled back to the previous version of the
-- application revision.
stopDeployment_autoRollbackEnabled :: Lens.Lens' StopDeployment (Prelude.Maybe Prelude.Bool)
stopDeployment_autoRollbackEnabled = Lens.lens (\StopDeployment' {autoRollbackEnabled} -> autoRollbackEnabled) (\s@StopDeployment' {} a -> s {autoRollbackEnabled = a} :: StopDeployment)

-- | The unique ID of a deployment.
stopDeployment_deploymentId :: Lens.Lens' StopDeployment Prelude.Text
stopDeployment_deploymentId = Lens.lens (\StopDeployment' {deploymentId} -> deploymentId) (\s@StopDeployment' {} a -> s {deploymentId = a} :: StopDeployment)

instance Core.AWSRequest StopDeployment where
  type
    AWSResponse StopDeployment =
      StopDeploymentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StopDeploymentResponse'
            Prelude.<$> (x Core..?> "statusMessage")
            Prelude.<*> (x Core..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StopDeployment

instance Prelude.NFData StopDeployment

instance Core.ToHeaders StopDeployment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.StopDeployment" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StopDeployment where
  toJSON StopDeployment' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("autoRollbackEnabled" Core..=)
              Prelude.<$> autoRollbackEnabled,
            Prelude.Just ("deploymentId" Core..= deploymentId)
          ]
      )

instance Core.ToPath StopDeployment where
  toPath = Prelude.const "/"

instance Core.ToQuery StopDeployment where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @StopDeployment@ operation.
--
-- /See:/ 'newStopDeploymentResponse' smart constructor.
data StopDeploymentResponse = StopDeploymentResponse'
  { -- | An accompanying status message.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The status of the stop deployment operation:
    --
    -- -   Pending: The stop operation is pending.
    --
    -- -   Succeeded: The stop operation was successful.
    status :: Prelude.Maybe StopStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopDeploymentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessage', 'stopDeploymentResponse_statusMessage' - An accompanying status message.
--
-- 'status', 'stopDeploymentResponse_status' - The status of the stop deployment operation:
--
-- -   Pending: The stop operation is pending.
--
-- -   Succeeded: The stop operation was successful.
--
-- 'httpStatus', 'stopDeploymentResponse_httpStatus' - The response's http status code.
newStopDeploymentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StopDeploymentResponse
newStopDeploymentResponse pHttpStatus_ =
  StopDeploymentResponse'
    { statusMessage =
        Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An accompanying status message.
stopDeploymentResponse_statusMessage :: Lens.Lens' StopDeploymentResponse (Prelude.Maybe Prelude.Text)
stopDeploymentResponse_statusMessage = Lens.lens (\StopDeploymentResponse' {statusMessage} -> statusMessage) (\s@StopDeploymentResponse' {} a -> s {statusMessage = a} :: StopDeploymentResponse)

-- | The status of the stop deployment operation:
--
-- -   Pending: The stop operation is pending.
--
-- -   Succeeded: The stop operation was successful.
stopDeploymentResponse_status :: Lens.Lens' StopDeploymentResponse (Prelude.Maybe StopStatus)
stopDeploymentResponse_status = Lens.lens (\StopDeploymentResponse' {status} -> status) (\s@StopDeploymentResponse' {} a -> s {status = a} :: StopDeploymentResponse)

-- | The response's http status code.
stopDeploymentResponse_httpStatus :: Lens.Lens' StopDeploymentResponse Prelude.Int
stopDeploymentResponse_httpStatus = Lens.lens (\StopDeploymentResponse' {httpStatus} -> httpStatus) (\s@StopDeploymentResponse' {} a -> s {httpStatus = a} :: StopDeploymentResponse)

instance Prelude.NFData StopDeploymentResponse
