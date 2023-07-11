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
-- Module      : Amazonka.ELBV2.DeregisterTargets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters the specified targets from the specified target group. After
-- the targets are deregistered, they no longer receive traffic from the
-- load balancer.
module Amazonka.ELBV2.DeregisterTargets
  ( -- * Creating a Request
    DeregisterTargets (..),
    newDeregisterTargets,

    -- * Request Lenses
    deregisterTargets_targetGroupArn,
    deregisterTargets_targets,

    -- * Destructuring the Response
    DeregisterTargetsResponse (..),
    newDeregisterTargetsResponse,

    -- * Response Lenses
    deregisterTargetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELBV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeregisterTargets' smart constructor.
data DeregisterTargets = DeregisterTargets'
  { -- | The Amazon Resource Name (ARN) of the target group.
    targetGroupArn :: Prelude.Text,
    -- | The targets. If you specified a port override when you registered a
    -- target, you must specify both the target ID and the port when you
    -- deregister it.
    targets :: [TargetDescription]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterTargets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetGroupArn', 'deregisterTargets_targetGroupArn' - The Amazon Resource Name (ARN) of the target group.
--
-- 'targets', 'deregisterTargets_targets' - The targets. If you specified a port override when you registered a
-- target, you must specify both the target ID and the port when you
-- deregister it.
newDeregisterTargets ::
  -- | 'targetGroupArn'
  Prelude.Text ->
  DeregisterTargets
newDeregisterTargets pTargetGroupArn_ =
  DeregisterTargets'
    { targetGroupArn =
        pTargetGroupArn_,
      targets = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) of the target group.
deregisterTargets_targetGroupArn :: Lens.Lens' DeregisterTargets Prelude.Text
deregisterTargets_targetGroupArn = Lens.lens (\DeregisterTargets' {targetGroupArn} -> targetGroupArn) (\s@DeregisterTargets' {} a -> s {targetGroupArn = a} :: DeregisterTargets)

-- | The targets. If you specified a port override when you registered a
-- target, you must specify both the target ID and the port when you
-- deregister it.
deregisterTargets_targets :: Lens.Lens' DeregisterTargets [TargetDescription]
deregisterTargets_targets = Lens.lens (\DeregisterTargets' {targets} -> targets) (\s@DeregisterTargets' {} a -> s {targets = a} :: DeregisterTargets) Prelude.. Lens.coerced

instance Core.AWSRequest DeregisterTargets where
  type
    AWSResponse DeregisterTargets =
      DeregisterTargetsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeregisterTargetsResult"
      ( \s h x ->
          DeregisterTargetsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeregisterTargets where
  hashWithSalt _salt DeregisterTargets' {..} =
    _salt
      `Prelude.hashWithSalt` targetGroupArn
      `Prelude.hashWithSalt` targets

instance Prelude.NFData DeregisterTargets where
  rnf DeregisterTargets' {..} =
    Prelude.rnf targetGroupArn
      `Prelude.seq` Prelude.rnf targets

instance Data.ToHeaders DeregisterTargets where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeregisterTargets where
  toPath = Prelude.const "/"

instance Data.ToQuery DeregisterTargets where
  toQuery DeregisterTargets' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeregisterTargets" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-12-01" :: Prelude.ByteString),
        "TargetGroupArn" Data.=: targetGroupArn,
        "Targets" Data.=: Data.toQueryList "member" targets
      ]

-- | /See:/ 'newDeregisterTargetsResponse' smart constructor.
data DeregisterTargetsResponse = DeregisterTargetsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterTargetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deregisterTargetsResponse_httpStatus' - The response's http status code.
newDeregisterTargetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeregisterTargetsResponse
newDeregisterTargetsResponse pHttpStatus_ =
  DeregisterTargetsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deregisterTargetsResponse_httpStatus :: Lens.Lens' DeregisterTargetsResponse Prelude.Int
deregisterTargetsResponse_httpStatus = Lens.lens (\DeregisterTargetsResponse' {httpStatus} -> httpStatus) (\s@DeregisterTargetsResponse' {} a -> s {httpStatus = a} :: DeregisterTargetsResponse)

instance Prelude.NFData DeregisterTargetsResponse where
  rnf DeregisterTargetsResponse' {..} =
    Prelude.rnf httpStatus
