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
-- Module      : Amazonka.VPCLattice.RegisterTargets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers the targets with the target group. If it\'s a Lambda target,
-- you can only have one target in a target group.
module Amazonka.VPCLattice.RegisterTargets
  ( -- * Creating a Request
    RegisterTargets (..),
    newRegisterTargets,

    -- * Request Lenses
    registerTargets_targetGroupIdentifier,
    registerTargets_targets,

    -- * Destructuring the Response
    RegisterTargetsResponse (..),
    newRegisterTargetsResponse,

    -- * Response Lenses
    registerTargetsResponse_successful,
    registerTargetsResponse_unsuccessful,
    registerTargetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newRegisterTargets' smart constructor.
data RegisterTargets = RegisterTargets'
  { -- | The ID or Amazon Resource Name (ARN) of the target group.
    targetGroupIdentifier :: Prelude.Text,
    -- | The targets.
    targets :: Prelude.NonEmpty Target
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
-- 'targetGroupIdentifier', 'registerTargets_targetGroupIdentifier' - The ID or Amazon Resource Name (ARN) of the target group.
--
-- 'targets', 'registerTargets_targets' - The targets.
newRegisterTargets ::
  -- | 'targetGroupIdentifier'
  Prelude.Text ->
  -- | 'targets'
  Prelude.NonEmpty Target ->
  RegisterTargets
newRegisterTargets pTargetGroupIdentifier_ pTargets_ =
  RegisterTargets'
    { targetGroupIdentifier =
        pTargetGroupIdentifier_,
      targets = Lens.coerced Lens.# pTargets_
    }

-- | The ID or Amazon Resource Name (ARN) of the target group.
registerTargets_targetGroupIdentifier :: Lens.Lens' RegisterTargets Prelude.Text
registerTargets_targetGroupIdentifier = Lens.lens (\RegisterTargets' {targetGroupIdentifier} -> targetGroupIdentifier) (\s@RegisterTargets' {} a -> s {targetGroupIdentifier = a} :: RegisterTargets)

-- | The targets.
registerTargets_targets :: Lens.Lens' RegisterTargets (Prelude.NonEmpty Target)
registerTargets_targets = Lens.lens (\RegisterTargets' {targets} -> targets) (\s@RegisterTargets' {} a -> s {targets = a} :: RegisterTargets) Prelude.. Lens.coerced

instance Core.AWSRequest RegisterTargets where
  type
    AWSResponse RegisterTargets =
      RegisterTargetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterTargetsResponse'
            Prelude.<$> (x Data..?> "successful" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "unsuccessful" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterTargets where
  hashWithSalt _salt RegisterTargets' {..} =
    _salt
      `Prelude.hashWithSalt` targetGroupIdentifier
      `Prelude.hashWithSalt` targets

instance Prelude.NFData RegisterTargets where
  rnf RegisterTargets' {..} =
    Prelude.rnf targetGroupIdentifier
      `Prelude.seq` Prelude.rnf targets

instance Data.ToHeaders RegisterTargets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RegisterTargets where
  toJSON RegisterTargets' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("targets" Data..= targets)]
      )

instance Data.ToPath RegisterTargets where
  toPath RegisterTargets' {..} =
    Prelude.mconcat
      [ "/targetgroups/",
        Data.toBS targetGroupIdentifier,
        "/registertargets"
      ]

instance Data.ToQuery RegisterTargets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterTargetsResponse' smart constructor.
data RegisterTargetsResponse = RegisterTargetsResponse'
  { -- | The targets that were successfully registered.
    successful :: Prelude.Maybe [Target],
    -- | The targets that were not registered.
    unsuccessful :: Prelude.Maybe [TargetFailure],
    -- | The response's http status code.
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
-- 'successful', 'registerTargetsResponse_successful' - The targets that were successfully registered.
--
-- 'unsuccessful', 'registerTargetsResponse_unsuccessful' - The targets that were not registered.
--
-- 'httpStatus', 'registerTargetsResponse_httpStatus' - The response's http status code.
newRegisterTargetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterTargetsResponse
newRegisterTargetsResponse pHttpStatus_ =
  RegisterTargetsResponse'
    { successful =
        Prelude.Nothing,
      unsuccessful = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The targets that were successfully registered.
registerTargetsResponse_successful :: Lens.Lens' RegisterTargetsResponse (Prelude.Maybe [Target])
registerTargetsResponse_successful = Lens.lens (\RegisterTargetsResponse' {successful} -> successful) (\s@RegisterTargetsResponse' {} a -> s {successful = a} :: RegisterTargetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The targets that were not registered.
registerTargetsResponse_unsuccessful :: Lens.Lens' RegisterTargetsResponse (Prelude.Maybe [TargetFailure])
registerTargetsResponse_unsuccessful = Lens.lens (\RegisterTargetsResponse' {unsuccessful} -> unsuccessful) (\s@RegisterTargetsResponse' {} a -> s {unsuccessful = a} :: RegisterTargetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
registerTargetsResponse_httpStatus :: Lens.Lens' RegisterTargetsResponse Prelude.Int
registerTargetsResponse_httpStatus = Lens.lens (\RegisterTargetsResponse' {httpStatus} -> httpStatus) (\s@RegisterTargetsResponse' {} a -> s {httpStatus = a} :: RegisterTargetsResponse)

instance Prelude.NFData RegisterTargetsResponse where
  rnf RegisterTargetsResponse' {..} =
    Prelude.rnf successful
      `Prelude.seq` Prelude.rnf unsuccessful
      `Prelude.seq` Prelude.rnf httpStatus
