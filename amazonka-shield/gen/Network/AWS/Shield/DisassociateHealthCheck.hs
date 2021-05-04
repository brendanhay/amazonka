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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Shield.Types

-- | /See:/ 'newDisassociateHealthCheck' smart constructor.
data DisassociateHealthCheck = DisassociateHealthCheck'
  { -- | The unique identifier (ID) for the Protection object to remove the
    -- health check association from.
    protectionId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the health check that is associated
    -- with the protection.
    healthCheckArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DisassociateHealthCheck where
  type
    Rs DisassociateHealthCheck =
      DisassociateHealthCheckResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateHealthCheckResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateHealthCheck

instance Prelude.NFData DisassociateHealthCheck

instance Prelude.ToHeaders DisassociateHealthCheck where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSShield_20160616.DisassociateHealthCheck" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DisassociateHealthCheck where
  toJSON DisassociateHealthCheck' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ProtectionId" Prelude..= protectionId),
            Prelude.Just
              ("HealthCheckArn" Prelude..= healthCheckArn)
          ]
      )

instance Prelude.ToPath DisassociateHealthCheck where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DisassociateHealthCheck where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateHealthCheckResponse' smart constructor.
data DisassociateHealthCheckResponse = DisassociateHealthCheckResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
