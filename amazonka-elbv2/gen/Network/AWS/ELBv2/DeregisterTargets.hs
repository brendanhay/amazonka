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
-- Module      : Network.AWS.ELBv2.DeregisterTargets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters the specified targets from the specified target group. After
-- the targets are deregistered, they no longer receive traffic from the
-- load balancer.
module Network.AWS.ELBv2.DeregisterTargets
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

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeregisterTargets' smart constructor.
data DeregisterTargets = DeregisterTargets'
  { -- | The Amazon Resource Name (ARN) of the target group.
    targetGroupArn :: Prelude.Text,
    -- | The targets. If you specified a port override when you registered a
    -- target, you must specify both the target ID and the port when you
    -- deregister it.
    targets :: [TargetDescription]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
deregisterTargets_targets = Lens.lens (\DeregisterTargets' {targets} -> targets) (\s@DeregisterTargets' {} a -> s {targets = a} :: DeregisterTargets) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest DeregisterTargets where
  type Rs DeregisterTargets = DeregisterTargetsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeregisterTargetsResult"
      ( \s h x ->
          DeregisterTargetsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeregisterTargets

instance Prelude.NFData DeregisterTargets

instance Prelude.ToHeaders DeregisterTargets where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeregisterTargets where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeregisterTargets where
  toQuery DeregisterTargets' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeregisterTargets" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2015-12-01" :: Prelude.ByteString),
        "TargetGroupArn" Prelude.=: targetGroupArn,
        "Targets"
          Prelude.=: Prelude.toQueryList "member" targets
      ]

-- | /See:/ 'newDeregisterTargetsResponse' smart constructor.
data DeregisterTargetsResponse = DeregisterTargetsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeregisterTargetsResponse
