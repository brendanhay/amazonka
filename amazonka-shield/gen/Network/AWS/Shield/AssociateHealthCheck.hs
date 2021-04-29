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
-- Module      : Network.AWS.Shield.AssociateHealthCheck
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds health-based detection to the Shield Advanced protection for a
-- resource. Shield Advanced health-based detection uses the health of your
-- AWS resource to improve responsiveness and accuracy in attack detection
-- and mitigation.
--
-- You define the health check in Route 53 and then associate it with your
-- Shield Advanced protection. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/ddos-overview.html#ddos-advanced-health-check-option Shield Advanced Health-Based Detection>
-- in the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/ AWS WAF and AWS Shield Developer Guide>.
module Network.AWS.Shield.AssociateHealthCheck
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Shield.Types

-- | /See:/ 'newAssociateHealthCheck' smart constructor.
data AssociateHealthCheck = AssociateHealthCheck'
  { -- | The unique identifier (ID) for the Protection object to add the health
    -- check association to.
    protectionId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the health check to associate with the
    -- protection.
    healthCheckArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest AssociateHealthCheck where
  type
    Rs AssociateHealthCheck =
      AssociateHealthCheckResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateHealthCheckResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateHealthCheck

instance Prelude.NFData AssociateHealthCheck

instance Prelude.ToHeaders AssociateHealthCheck where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSShield_20160616.AssociateHealthCheck" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AssociateHealthCheck where
  toJSON AssociateHealthCheck' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ProtectionId" Prelude..= protectionId),
            Prelude.Just
              ("HealthCheckArn" Prelude..= healthCheckArn)
          ]
      )

instance Prelude.ToPath AssociateHealthCheck where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AssociateHealthCheck where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateHealthCheckResponse' smart constructor.
data AssociateHealthCheckResponse = AssociateHealthCheckResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData AssociateHealthCheckResponse
