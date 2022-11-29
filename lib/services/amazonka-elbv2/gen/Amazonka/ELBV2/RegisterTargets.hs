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
-- Module      : Amazonka.ELBV2.RegisterTargets
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers the specified targets with the specified target group.
--
-- If the target is an EC2 instance, it must be in the @running@ state when
-- you register it.
--
-- By default, the load balancer routes requests to registered targets
-- using the protocol and port for the target group. Alternatively, you can
-- override the port for a target when you register it. You can register
-- each EC2 instance or IP address with the same target group multiple
-- times using different ports.
--
-- With a Network Load Balancer, you cannot register instances by instance
-- ID if they have the following instance types: C1, CC1, CC2, CG1, CG2,
-- CR1, CS1, G1, G2, HI1, HS1, M1, M2, M3, and T1. You can register
-- instances of these types by IP address.
module Amazonka.ELBV2.RegisterTargets
  ( -- * Creating a Request
    RegisterTargets (..),
    newRegisterTargets,

    -- * Request Lenses
    registerTargets_targetGroupArn,
    registerTargets_targets,

    -- * Destructuring the Response
    RegisterTargetsResponse (..),
    newRegisterTargetsResponse,

    -- * Response Lenses
    registerTargetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ELBV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRegisterTargets' smart constructor.
data RegisterTargets = RegisterTargets'
  { -- | The Amazon Resource Name (ARN) of the target group.
    targetGroupArn :: Prelude.Text,
    -- | The targets.
    targets :: [TargetDescription]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterTargets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetGroupArn', 'registerTargets_targetGroupArn' - The Amazon Resource Name (ARN) of the target group.
--
-- 'targets', 'registerTargets_targets' - The targets.
newRegisterTargets ::
  -- | 'targetGroupArn'
  Prelude.Text ->
  RegisterTargets
newRegisterTargets pTargetGroupArn_ =
  RegisterTargets'
    { targetGroupArn = pTargetGroupArn_,
      targets = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) of the target group.
registerTargets_targetGroupArn :: Lens.Lens' RegisterTargets Prelude.Text
registerTargets_targetGroupArn = Lens.lens (\RegisterTargets' {targetGroupArn} -> targetGroupArn) (\s@RegisterTargets' {} a -> s {targetGroupArn = a} :: RegisterTargets)

-- | The targets.
registerTargets_targets :: Lens.Lens' RegisterTargets [TargetDescription]
registerTargets_targets = Lens.lens (\RegisterTargets' {targets} -> targets) (\s@RegisterTargets' {} a -> s {targets = a} :: RegisterTargets) Prelude.. Lens.coerced

instance Core.AWSRequest RegisterTargets where
  type
    AWSResponse RegisterTargets =
      RegisterTargetsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "RegisterTargetsResult"
      ( \s h x ->
          RegisterTargetsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterTargets where
  hashWithSalt _salt RegisterTargets' {..} =
    _salt `Prelude.hashWithSalt` targetGroupArn
      `Prelude.hashWithSalt` targets

instance Prelude.NFData RegisterTargets where
  rnf RegisterTargets' {..} =
    Prelude.rnf targetGroupArn
      `Prelude.seq` Prelude.rnf targets

instance Core.ToHeaders RegisterTargets where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath RegisterTargets where
  toPath = Prelude.const "/"

instance Core.ToQuery RegisterTargets where
  toQuery RegisterTargets' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("RegisterTargets" :: Prelude.ByteString),
        "Version"
          Core.=: ("2015-12-01" :: Prelude.ByteString),
        "TargetGroupArn" Core.=: targetGroupArn,
        "Targets" Core.=: Core.toQueryList "member" targets
      ]

-- | /See:/ 'newRegisterTargetsResponse' smart constructor.
data RegisterTargetsResponse = RegisterTargetsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterTargetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'registerTargetsResponse_httpStatus' - The response's http status code.
newRegisterTargetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterTargetsResponse
newRegisterTargetsResponse pHttpStatus_ =
  RegisterTargetsResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
registerTargetsResponse_httpStatus :: Lens.Lens' RegisterTargetsResponse Prelude.Int
registerTargetsResponse_httpStatus = Lens.lens (\RegisterTargetsResponse' {httpStatus} -> httpStatus) (\s@RegisterTargetsResponse' {} a -> s {httpStatus = a} :: RegisterTargetsResponse)

instance Prelude.NFData RegisterTargetsResponse where
  rnf RegisterTargetsResponse' {..} =
    Prelude.rnf httpStatus
