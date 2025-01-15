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
-- Module      : Amazonka.Shield.AssociateHealthCheck
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds health-based detection to the Shield Advanced protection for a
-- resource. Shield Advanced health-based detection uses the health of your
-- Amazon Web Services resource to improve responsiveness and accuracy in
-- attack detection and response.
--
-- You define the health check in Route 53 and then associate it with your
-- Shield Advanced protection. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/ddos-overview.html#ddos-advanced-health-check-option Shield Advanced Health-Based Detection>
-- in the /WAF Developer Guide/.
module Amazonka.Shield.AssociateHealthCheck
  ( -- * Creating a Request
    AssociateHealthCheck (..),
    newAssociateHealthCheck,

    -- * Request Lenses
    associateHealthCheck_protectionId,
    associateHealthCheck_healthCheckArn,

    -- * Destructuring the Response
    AssociateHealthCheckResponse (..),
    newAssociateHealthCheckResponse,

    -- * Response Lenses
    associateHealthCheckResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Shield.Types

-- | /See:/ 'newAssociateHealthCheck' smart constructor.
data AssociateHealthCheck = AssociateHealthCheck'
  { -- | The unique identifier (ID) for the Protection object to add the health
    -- check association to.
    protectionId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the health check to associate with the
    -- protection.
    healthCheckArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateHealthCheck' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protectionId', 'associateHealthCheck_protectionId' - The unique identifier (ID) for the Protection object to add the health
-- check association to.
--
-- 'healthCheckArn', 'associateHealthCheck_healthCheckArn' - The Amazon Resource Name (ARN) of the health check to associate with the
-- protection.
newAssociateHealthCheck ::
  -- | 'protectionId'
  Prelude.Text ->
  -- | 'healthCheckArn'
  Prelude.Text ->
  AssociateHealthCheck
newAssociateHealthCheck
  pProtectionId_
  pHealthCheckArn_ =
    AssociateHealthCheck'
      { protectionId =
          pProtectionId_,
        healthCheckArn = pHealthCheckArn_
      }

-- | The unique identifier (ID) for the Protection object to add the health
-- check association to.
associateHealthCheck_protectionId :: Lens.Lens' AssociateHealthCheck Prelude.Text
associateHealthCheck_protectionId = Lens.lens (\AssociateHealthCheck' {protectionId} -> protectionId) (\s@AssociateHealthCheck' {} a -> s {protectionId = a} :: AssociateHealthCheck)

-- | The Amazon Resource Name (ARN) of the health check to associate with the
-- protection.
associateHealthCheck_healthCheckArn :: Lens.Lens' AssociateHealthCheck Prelude.Text
associateHealthCheck_healthCheckArn = Lens.lens (\AssociateHealthCheck' {healthCheckArn} -> healthCheckArn) (\s@AssociateHealthCheck' {} a -> s {healthCheckArn = a} :: AssociateHealthCheck)

instance Core.AWSRequest AssociateHealthCheck where
  type
    AWSResponse AssociateHealthCheck =
      AssociateHealthCheckResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateHealthCheckResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateHealthCheck where
  hashWithSalt _salt AssociateHealthCheck' {..} =
    _salt
      `Prelude.hashWithSalt` protectionId
      `Prelude.hashWithSalt` healthCheckArn

instance Prelude.NFData AssociateHealthCheck where
  rnf AssociateHealthCheck' {..} =
    Prelude.rnf protectionId `Prelude.seq`
      Prelude.rnf healthCheckArn

instance Data.ToHeaders AssociateHealthCheck where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSShield_20160616.AssociateHealthCheck" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateHealthCheck where
  toJSON AssociateHealthCheck' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ProtectionId" Data..= protectionId),
            Prelude.Just
              ("HealthCheckArn" Data..= healthCheckArn)
          ]
      )

instance Data.ToPath AssociateHealthCheck where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateHealthCheck where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateHealthCheckResponse' smart constructor.
data AssociateHealthCheckResponse = AssociateHealthCheckResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateHealthCheckResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateHealthCheckResponse_httpStatus' - The response's http status code.
newAssociateHealthCheckResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateHealthCheckResponse
newAssociateHealthCheckResponse pHttpStatus_ =
  AssociateHealthCheckResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
associateHealthCheckResponse_httpStatus :: Lens.Lens' AssociateHealthCheckResponse Prelude.Int
associateHealthCheckResponse_httpStatus = Lens.lens (\AssociateHealthCheckResponse' {httpStatus} -> httpStatus) (\s@AssociateHealthCheckResponse' {} a -> s {httpStatus = a} :: AssociateHealthCheckResponse)

instance Prelude.NFData AssociateHealthCheckResponse where
  rnf AssociateHealthCheckResponse' {..} =
    Prelude.rnf httpStatus
