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
-- Module      : Amazonka.GreengrassV2.CancelDeployment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a deployment. This operation cancels the deployment for devices
-- that haven\'t yet received it. If a device already received the
-- deployment, this operation doesn\'t change anything for that device.
module Amazonka.GreengrassV2.CancelDeployment
  ( -- * Creating a Request
    CancelDeployment (..),
    newCancelDeployment,

    -- * Request Lenses
    cancelDeployment_deploymentId,

    -- * Destructuring the Response
    CancelDeploymentResponse (..),
    newCancelDeploymentResponse,

    -- * Response Lenses
    cancelDeploymentResponse_message,
    cancelDeploymentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GreengrassV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelDeployment' smart constructor.
data CancelDeployment = CancelDeployment'
  { -- | The ID of the deployment.
    deploymentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentId', 'cancelDeployment_deploymentId' - The ID of the deployment.
newCancelDeployment ::
  -- | 'deploymentId'
  Prelude.Text ->
  CancelDeployment
newCancelDeployment pDeploymentId_ =
  CancelDeployment' {deploymentId = pDeploymentId_}

-- | The ID of the deployment.
cancelDeployment_deploymentId :: Lens.Lens' CancelDeployment Prelude.Text
cancelDeployment_deploymentId = Lens.lens (\CancelDeployment' {deploymentId} -> deploymentId) (\s@CancelDeployment' {} a -> s {deploymentId = a} :: CancelDeployment)

instance Core.AWSRequest CancelDeployment where
  type
    AWSResponse CancelDeployment =
      CancelDeploymentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelDeploymentResponse'
            Prelude.<$> (x Core..?> "message")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelDeployment where
  hashWithSalt _salt CancelDeployment' {..} =
    _salt `Prelude.hashWithSalt` deploymentId

instance Prelude.NFData CancelDeployment where
  rnf CancelDeployment' {..} = Prelude.rnf deploymentId

instance Core.ToHeaders CancelDeployment where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CancelDeployment where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath CancelDeployment where
  toPath CancelDeployment' {..} =
    Prelude.mconcat
      [ "/greengrass/v2/deployments/",
        Core.toBS deploymentId,
        "/cancel"
      ]

instance Core.ToQuery CancelDeployment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelDeploymentResponse' smart constructor.
data CancelDeploymentResponse = CancelDeploymentResponse'
  { -- | A message that communicates if the cancel was successful.
    message :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelDeploymentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'cancelDeploymentResponse_message' - A message that communicates if the cancel was successful.
--
-- 'httpStatus', 'cancelDeploymentResponse_httpStatus' - The response's http status code.
newCancelDeploymentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelDeploymentResponse
newCancelDeploymentResponse pHttpStatus_ =
  CancelDeploymentResponse'
    { message =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A message that communicates if the cancel was successful.
cancelDeploymentResponse_message :: Lens.Lens' CancelDeploymentResponse (Prelude.Maybe Prelude.Text)
cancelDeploymentResponse_message = Lens.lens (\CancelDeploymentResponse' {message} -> message) (\s@CancelDeploymentResponse' {} a -> s {message = a} :: CancelDeploymentResponse)

-- | The response's http status code.
cancelDeploymentResponse_httpStatus :: Lens.Lens' CancelDeploymentResponse Prelude.Int
cancelDeploymentResponse_httpStatus = Lens.lens (\CancelDeploymentResponse' {httpStatus} -> httpStatus) (\s@CancelDeploymentResponse' {} a -> s {httpStatus = a} :: CancelDeploymentResponse)

instance Prelude.NFData CancelDeploymentResponse where
  rnf CancelDeploymentResponse' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf httpStatus
