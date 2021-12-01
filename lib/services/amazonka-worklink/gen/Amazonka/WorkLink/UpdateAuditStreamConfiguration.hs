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
-- Module      : Amazonka.WorkLink.UpdateAuditStreamConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the audit stream configuration for the fleet.
module Amazonka.WorkLink.UpdateAuditStreamConfiguration
  ( -- * Creating a Request
    UpdateAuditStreamConfiguration (..),
    newUpdateAuditStreamConfiguration,

    -- * Request Lenses
    updateAuditStreamConfiguration_auditStreamArn,
    updateAuditStreamConfiguration_fleetArn,

    -- * Destructuring the Response
    UpdateAuditStreamConfigurationResponse (..),
    newUpdateAuditStreamConfigurationResponse,

    -- * Response Lenses
    updateAuditStreamConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkLink.Types

-- | /See:/ 'newUpdateAuditStreamConfiguration' smart constructor.
data UpdateAuditStreamConfiguration = UpdateAuditStreamConfiguration'
  { -- | The ARN of the Amazon Kinesis data stream that receives the audit
    -- events.
    auditStreamArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the fleet.
    fleetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAuditStreamConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'auditStreamArn', 'updateAuditStreamConfiguration_auditStreamArn' - The ARN of the Amazon Kinesis data stream that receives the audit
-- events.
--
-- 'fleetArn', 'updateAuditStreamConfiguration_fleetArn' - The ARN of the fleet.
newUpdateAuditStreamConfiguration ::
  -- | 'fleetArn'
  Prelude.Text ->
  UpdateAuditStreamConfiguration
newUpdateAuditStreamConfiguration pFleetArn_ =
  UpdateAuditStreamConfiguration'
    { auditStreamArn =
        Prelude.Nothing,
      fleetArn = pFleetArn_
    }

-- | The ARN of the Amazon Kinesis data stream that receives the audit
-- events.
updateAuditStreamConfiguration_auditStreamArn :: Lens.Lens' UpdateAuditStreamConfiguration (Prelude.Maybe Prelude.Text)
updateAuditStreamConfiguration_auditStreamArn = Lens.lens (\UpdateAuditStreamConfiguration' {auditStreamArn} -> auditStreamArn) (\s@UpdateAuditStreamConfiguration' {} a -> s {auditStreamArn = a} :: UpdateAuditStreamConfiguration)

-- | The ARN of the fleet.
updateAuditStreamConfiguration_fleetArn :: Lens.Lens' UpdateAuditStreamConfiguration Prelude.Text
updateAuditStreamConfiguration_fleetArn = Lens.lens (\UpdateAuditStreamConfiguration' {fleetArn} -> fleetArn) (\s@UpdateAuditStreamConfiguration' {} a -> s {fleetArn = a} :: UpdateAuditStreamConfiguration)

instance
  Core.AWSRequest
    UpdateAuditStreamConfiguration
  where
  type
    AWSResponse UpdateAuditStreamConfiguration =
      UpdateAuditStreamConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateAuditStreamConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateAuditStreamConfiguration
  where
  hashWithSalt
    salt'
    UpdateAuditStreamConfiguration' {..} =
      salt' `Prelude.hashWithSalt` fleetArn
        `Prelude.hashWithSalt` auditStreamArn

instance
  Prelude.NFData
    UpdateAuditStreamConfiguration
  where
  rnf UpdateAuditStreamConfiguration' {..} =
    Prelude.rnf auditStreamArn
      `Prelude.seq` Prelude.rnf fleetArn

instance
  Core.ToHeaders
    UpdateAuditStreamConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateAuditStreamConfiguration where
  toJSON UpdateAuditStreamConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AuditStreamArn" Core..=)
              Prelude.<$> auditStreamArn,
            Prelude.Just ("FleetArn" Core..= fleetArn)
          ]
      )

instance Core.ToPath UpdateAuditStreamConfiguration where
  toPath =
    Prelude.const "/updateAuditStreamConfiguration"

instance Core.ToQuery UpdateAuditStreamConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAuditStreamConfigurationResponse' smart constructor.
data UpdateAuditStreamConfigurationResponse = UpdateAuditStreamConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAuditStreamConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateAuditStreamConfigurationResponse_httpStatus' - The response's http status code.
newUpdateAuditStreamConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAuditStreamConfigurationResponse
newUpdateAuditStreamConfigurationResponse
  pHttpStatus_ =
    UpdateAuditStreamConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateAuditStreamConfigurationResponse_httpStatus :: Lens.Lens' UpdateAuditStreamConfigurationResponse Prelude.Int
updateAuditStreamConfigurationResponse_httpStatus = Lens.lens (\UpdateAuditStreamConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateAuditStreamConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateAuditStreamConfigurationResponse)

instance
  Prelude.NFData
    UpdateAuditStreamConfigurationResponse
  where
  rnf UpdateAuditStreamConfigurationResponse' {..} =
    Prelude.rnf httpStatus
