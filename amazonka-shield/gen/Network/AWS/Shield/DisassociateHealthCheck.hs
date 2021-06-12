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
-- Module      : Network.AWS.Shield.DisassociateHealthCheck
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes health-based detection from the Shield Advanced protection for a
-- resource. Shield Advanced health-based detection uses the health of your
-- AWS resource to improve responsiveness and accuracy in attack detection
-- and mitigation.
--
-- You define the health check in Route 53 and then associate or
-- disassociate it with your Shield Advanced protection. For more
-- information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/ddos-overview.html#ddos-advanced-health-check-option Shield Advanced Health-Based Detection>
-- in the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF and AWS Shield Developer Guide>.
module Network.AWS.Shield.DisassociateHealthCheck
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Shield.Types

-- | /See:/ 'newDisassociateHealthCheck' smart constructor.
data DisassociateHealthCheck = DisassociateHealthCheck'
  { -- | The unique identifier (ID) for the Protection object to remove the
    -- health check association from.
    protectionId :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the health check that is associated
    -- with the protection.
    healthCheckArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'healthCheckArn'
  Core.Text ->
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
disassociateHealthCheck_protectionId :: Lens.Lens' DisassociateHealthCheck Core.Text
disassociateHealthCheck_protectionId = Lens.lens (\DisassociateHealthCheck' {protectionId} -> protectionId) (\s@DisassociateHealthCheck' {} a -> s {protectionId = a} :: DisassociateHealthCheck)

-- | The Amazon Resource Name (ARN) of the health check that is associated
-- with the protection.
disassociateHealthCheck_healthCheckArn :: Lens.Lens' DisassociateHealthCheck Core.Text
disassociateHealthCheck_healthCheckArn = Lens.lens (\DisassociateHealthCheck' {healthCheckArn} -> healthCheckArn) (\s@DisassociateHealthCheck' {} a -> s {healthCheckArn = a} :: DisassociateHealthCheck)

instance Core.AWSRequest DisassociateHealthCheck where
  type
    AWSResponse DisassociateHealthCheck =
      DisassociateHealthCheckResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateHealthCheckResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DisassociateHealthCheck

instance Core.NFData DisassociateHealthCheck

instance Core.ToHeaders DisassociateHealthCheck where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShield_20160616.DisassociateHealthCheck" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DisassociateHealthCheck where
  toJSON DisassociateHealthCheck' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ProtectionId" Core..= protectionId),
            Core.Just ("HealthCheckArn" Core..= healthCheckArn)
          ]
      )

instance Core.ToPath DisassociateHealthCheck where
  toPath = Core.const "/"

instance Core.ToQuery DisassociateHealthCheck where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDisassociateHealthCheckResponse' smart constructor.
data DisassociateHealthCheckResponse = DisassociateHealthCheckResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DisassociateHealthCheckResponse
newDisassociateHealthCheckResponse pHttpStatus_ =
  DisassociateHealthCheckResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateHealthCheckResponse_httpStatus :: Lens.Lens' DisassociateHealthCheckResponse Core.Int
disassociateHealthCheckResponse_httpStatus = Lens.lens (\DisassociateHealthCheckResponse' {httpStatus} -> httpStatus) (\s@DisassociateHealthCheckResponse' {} a -> s {httpStatus = a} :: DisassociateHealthCheckResponse)

instance Core.NFData DisassociateHealthCheckResponse
