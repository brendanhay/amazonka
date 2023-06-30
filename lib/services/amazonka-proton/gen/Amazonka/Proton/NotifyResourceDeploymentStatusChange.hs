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
-- Module      : Amazonka.Proton.NotifyResourceDeploymentStatusChange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Notify Proton of status changes to a provisioned resource when you use
-- self-managed provisioning.
--
-- For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-works-prov-methods.html#ag-works-prov-methods-self Self-managed provisioning>
-- in the /Proton User Guide/.
module Amazonka.Proton.NotifyResourceDeploymentStatusChange
  ( -- * Creating a Request
    NotifyResourceDeploymentStatusChange (..),
    newNotifyResourceDeploymentStatusChange,

    -- * Request Lenses
    notifyResourceDeploymentStatusChange_deploymentId,
    notifyResourceDeploymentStatusChange_outputs,
    notifyResourceDeploymentStatusChange_status,
    notifyResourceDeploymentStatusChange_statusMessage,
    notifyResourceDeploymentStatusChange_resourceArn,

    -- * Destructuring the Response
    NotifyResourceDeploymentStatusChangeResponse (..),
    newNotifyResourceDeploymentStatusChangeResponse,

    -- * Response Lenses
    notifyResourceDeploymentStatusChangeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newNotifyResourceDeploymentStatusChange' smart constructor.
data NotifyResourceDeploymentStatusChange = NotifyResourceDeploymentStatusChange'
  { -- | The deployment ID for your provisioned resource.
    deploymentId :: Prelude.Maybe Prelude.Text,
    -- | The provisioned resource state change detail data that\'s returned by
    -- Proton.
    outputs :: Prelude.Maybe [Data.Sensitive Output],
    -- | The status of your provisioned resource.
    status :: Prelude.Maybe ResourceDeploymentStatus,
    -- | The deployment status message for your provisioned resource.
    statusMessage :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The provisioned resource Amazon Resource Name (ARN).
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotifyResourceDeploymentStatusChange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentId', 'notifyResourceDeploymentStatusChange_deploymentId' - The deployment ID for your provisioned resource.
--
-- 'outputs', 'notifyResourceDeploymentStatusChange_outputs' - The provisioned resource state change detail data that\'s returned by
-- Proton.
--
-- 'status', 'notifyResourceDeploymentStatusChange_status' - The status of your provisioned resource.
--
-- 'statusMessage', 'notifyResourceDeploymentStatusChange_statusMessage' - The deployment status message for your provisioned resource.
--
-- 'resourceArn', 'notifyResourceDeploymentStatusChange_resourceArn' - The provisioned resource Amazon Resource Name (ARN).
newNotifyResourceDeploymentStatusChange ::
  -- | 'resourceArn'
  Prelude.Text ->
  NotifyResourceDeploymentStatusChange
newNotifyResourceDeploymentStatusChange pResourceArn_ =
  NotifyResourceDeploymentStatusChange'
    { deploymentId =
        Prelude.Nothing,
      outputs = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      resourceArn = pResourceArn_
    }

-- | The deployment ID for your provisioned resource.
notifyResourceDeploymentStatusChange_deploymentId :: Lens.Lens' NotifyResourceDeploymentStatusChange (Prelude.Maybe Prelude.Text)
notifyResourceDeploymentStatusChange_deploymentId = Lens.lens (\NotifyResourceDeploymentStatusChange' {deploymentId} -> deploymentId) (\s@NotifyResourceDeploymentStatusChange' {} a -> s {deploymentId = a} :: NotifyResourceDeploymentStatusChange)

-- | The provisioned resource state change detail data that\'s returned by
-- Proton.
notifyResourceDeploymentStatusChange_outputs :: Lens.Lens' NotifyResourceDeploymentStatusChange (Prelude.Maybe [Output])
notifyResourceDeploymentStatusChange_outputs = Lens.lens (\NotifyResourceDeploymentStatusChange' {outputs} -> outputs) (\s@NotifyResourceDeploymentStatusChange' {} a -> s {outputs = a} :: NotifyResourceDeploymentStatusChange) Prelude.. Lens.mapping Lens.coerced

-- | The status of your provisioned resource.
notifyResourceDeploymentStatusChange_status :: Lens.Lens' NotifyResourceDeploymentStatusChange (Prelude.Maybe ResourceDeploymentStatus)
notifyResourceDeploymentStatusChange_status = Lens.lens (\NotifyResourceDeploymentStatusChange' {status} -> status) (\s@NotifyResourceDeploymentStatusChange' {} a -> s {status = a} :: NotifyResourceDeploymentStatusChange)

-- | The deployment status message for your provisioned resource.
notifyResourceDeploymentStatusChange_statusMessage :: Lens.Lens' NotifyResourceDeploymentStatusChange (Prelude.Maybe Prelude.Text)
notifyResourceDeploymentStatusChange_statusMessage = Lens.lens (\NotifyResourceDeploymentStatusChange' {statusMessage} -> statusMessage) (\s@NotifyResourceDeploymentStatusChange' {} a -> s {statusMessage = a} :: NotifyResourceDeploymentStatusChange) Prelude.. Lens.mapping Data._Sensitive

-- | The provisioned resource Amazon Resource Name (ARN).
notifyResourceDeploymentStatusChange_resourceArn :: Lens.Lens' NotifyResourceDeploymentStatusChange Prelude.Text
notifyResourceDeploymentStatusChange_resourceArn = Lens.lens (\NotifyResourceDeploymentStatusChange' {resourceArn} -> resourceArn) (\s@NotifyResourceDeploymentStatusChange' {} a -> s {resourceArn = a} :: NotifyResourceDeploymentStatusChange)

instance
  Core.AWSRequest
    NotifyResourceDeploymentStatusChange
  where
  type
    AWSResponse NotifyResourceDeploymentStatusChange =
      NotifyResourceDeploymentStatusChangeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          NotifyResourceDeploymentStatusChangeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    NotifyResourceDeploymentStatusChange
  where
  hashWithSalt
    _salt
    NotifyResourceDeploymentStatusChange' {..} =
      _salt
        `Prelude.hashWithSalt` deploymentId
        `Prelude.hashWithSalt` outputs
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` statusMessage
        `Prelude.hashWithSalt` resourceArn

instance
  Prelude.NFData
    NotifyResourceDeploymentStatusChange
  where
  rnf NotifyResourceDeploymentStatusChange' {..} =
    Prelude.rnf deploymentId
      `Prelude.seq` Prelude.rnf outputs
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf resourceArn

instance
  Data.ToHeaders
    NotifyResourceDeploymentStatusChange
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.NotifyResourceDeploymentStatusChange" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    NotifyResourceDeploymentStatusChange
  where
  toJSON NotifyResourceDeploymentStatusChange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("deploymentId" Data..=) Prelude.<$> deploymentId,
            ("outputs" Data..=) Prelude.<$> outputs,
            ("status" Data..=) Prelude.<$> status,
            ("statusMessage" Data..=) Prelude.<$> statusMessage,
            Prelude.Just ("resourceArn" Data..= resourceArn)
          ]
      )

instance
  Data.ToPath
    NotifyResourceDeploymentStatusChange
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    NotifyResourceDeploymentStatusChange
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newNotifyResourceDeploymentStatusChangeResponse' smart constructor.
data NotifyResourceDeploymentStatusChangeResponse = NotifyResourceDeploymentStatusChangeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotifyResourceDeploymentStatusChangeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'notifyResourceDeploymentStatusChangeResponse_httpStatus' - The response's http status code.
newNotifyResourceDeploymentStatusChangeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  NotifyResourceDeploymentStatusChangeResponse
newNotifyResourceDeploymentStatusChangeResponse
  pHttpStatus_ =
    NotifyResourceDeploymentStatusChangeResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
notifyResourceDeploymentStatusChangeResponse_httpStatus :: Lens.Lens' NotifyResourceDeploymentStatusChangeResponse Prelude.Int
notifyResourceDeploymentStatusChangeResponse_httpStatus = Lens.lens (\NotifyResourceDeploymentStatusChangeResponse' {httpStatus} -> httpStatus) (\s@NotifyResourceDeploymentStatusChangeResponse' {} a -> s {httpStatus = a} :: NotifyResourceDeploymentStatusChangeResponse)

instance
  Prelude.NFData
    NotifyResourceDeploymentStatusChangeResponse
  where
  rnf NotifyResourceDeploymentStatusChangeResponse' {..} =
    Prelude.rnf httpStatus
