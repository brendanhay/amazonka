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
-- Module      : Amazonka.Shield.DisassociateHealthCheck
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes health-based detection from the Shield Advanced protection for a
-- resource. Shield Advanced health-based detection uses the health of your
-- Amazon Web Services resource to improve responsiveness and accuracy in
-- attack detection and response.
--
-- You define the health check in Route 53 and then associate or
-- disassociate it with your Shield Advanced protection. For more
-- information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/ddos-overview.html#ddos-advanced-health-check-option Shield Advanced Health-Based Detection>
-- in the /WAF Developer Guide/.
module Amazonka.Shield.DisassociateHealthCheck
  ( -- * Creating a Request
    DisassociateHealthCheck (..),
    newDisassociateHealthCheck,

    -- * Request Lenses
    disassociateHealthCheck_protectionId,
    disassociateHealthCheck_healthCheckArn,

    -- * Destructuring the Response
    DisassociateHealthCheckResponse (..),
    newDisassociateHealthCheckResponse,

    -- * Response Lenses
    disassociateHealthCheckResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Shield.Types

-- | /See:/ 'newDisassociateHealthCheck' smart constructor.
data DisassociateHealthCheck = DisassociateHealthCheck'
  { -- | The unique identifier (ID) for the Protection object to remove the
    -- health check association from.
    protectionId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the health check that is associated
    -- with the protection.
    healthCheckArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateHealthCheck' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protectionId', 'disassociateHealthCheck_protectionId' - The unique identifier (ID) for the Protection object to remove the
-- health check association from.
--
-- 'healthCheckArn', 'disassociateHealthCheck_healthCheckArn' - The Amazon Resource Name (ARN) of the health check that is associated
-- with the protection.
newDisassociateHealthCheck ::
  -- | 'protectionId'
  Prelude.Text ->
  -- | 'healthCheckArn'
  Prelude.Text ->
  DisassociateHealthCheck
newDisassociateHealthCheck
  pProtectionId_
  pHealthCheckArn_ =
    DisassociateHealthCheck'
      { protectionId =
          pProtectionId_,
        healthCheckArn = pHealthCheckArn_
      }

-- | The unique identifier (ID) for the Protection object to remove the
-- health check association from.
disassociateHealthCheck_protectionId :: Lens.Lens' DisassociateHealthCheck Prelude.Text
disassociateHealthCheck_protectionId = Lens.lens (\DisassociateHealthCheck' {protectionId} -> protectionId) (\s@DisassociateHealthCheck' {} a -> s {protectionId = a} :: DisassociateHealthCheck)

-- | The Amazon Resource Name (ARN) of the health check that is associated
-- with the protection.
disassociateHealthCheck_healthCheckArn :: Lens.Lens' DisassociateHealthCheck Prelude.Text
disassociateHealthCheck_healthCheckArn = Lens.lens (\DisassociateHealthCheck' {healthCheckArn} -> healthCheckArn) (\s@DisassociateHealthCheck' {} a -> s {healthCheckArn = a} :: DisassociateHealthCheck)

instance Core.AWSRequest DisassociateHealthCheck where
  type
    AWSResponse DisassociateHealthCheck =
      DisassociateHealthCheckResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateHealthCheckResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateHealthCheck where
  hashWithSalt _salt DisassociateHealthCheck' {..} =
    _salt
      `Prelude.hashWithSalt` protectionId
      `Prelude.hashWithSalt` healthCheckArn

instance Prelude.NFData DisassociateHealthCheck where
  rnf DisassociateHealthCheck' {..} =
    Prelude.rnf protectionId
      `Prelude.seq` Prelude.rnf healthCheckArn

instance Data.ToHeaders DisassociateHealthCheck where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSShield_20160616.DisassociateHealthCheck" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateHealthCheck where
  toJSON DisassociateHealthCheck' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ProtectionId" Data..= protectionId),
            Prelude.Just
              ("HealthCheckArn" Data..= healthCheckArn)
          ]
      )

instance Data.ToPath DisassociateHealthCheck where
  toPath = Prelude.const "/"

instance Data.ToQuery DisassociateHealthCheck where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateHealthCheckResponse' smart constructor.
data DisassociateHealthCheckResponse = DisassociateHealthCheckResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateHealthCheckResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateHealthCheckResponse_httpStatus' - The response's http status code.
newDisassociateHealthCheckResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateHealthCheckResponse
newDisassociateHealthCheckResponse pHttpStatus_ =
  DisassociateHealthCheckResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateHealthCheckResponse_httpStatus :: Lens.Lens' DisassociateHealthCheckResponse Prelude.Int
disassociateHealthCheckResponse_httpStatus = Lens.lens (\DisassociateHealthCheckResponse' {httpStatus} -> httpStatus) (\s@DisassociateHealthCheckResponse' {} a -> s {httpStatus = a} :: DisassociateHealthCheckResponse)

instance
  Prelude.NFData
    DisassociateHealthCheckResponse
  where
  rnf DisassociateHealthCheckResponse' {..} =
    Prelude.rnf httpStatus
