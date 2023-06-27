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
-- Module      : Amazonka.VPCLattice.DeregisterTargets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters the specified targets from the specified target group.
module Amazonka.VPCLattice.DeregisterTargets
  ( -- * Creating a Request
    DeregisterTargets (..),
    newDeregisterTargets,

    -- * Request Lenses
    deregisterTargets_targetGroupIdentifier,
    deregisterTargets_targets,

    -- * Destructuring the Response
    DeregisterTargetsResponse (..),
    newDeregisterTargetsResponse,

    -- * Response Lenses
    deregisterTargetsResponse_successful,
    deregisterTargetsResponse_unsuccessful,
    deregisterTargetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newDeregisterTargets' smart constructor.
data DeregisterTargets = DeregisterTargets'
  { -- | The ID or Amazon Resource Name (ARN) of the target group.
    targetGroupIdentifier :: Prelude.Text,
    -- | The targets to deregister.
    targets :: Prelude.NonEmpty Target
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
-- 'targetGroupIdentifier', 'deregisterTargets_targetGroupIdentifier' - The ID or Amazon Resource Name (ARN) of the target group.
--
-- 'targets', 'deregisterTargets_targets' - The targets to deregister.
newDeregisterTargets ::
  -- | 'targetGroupIdentifier'
  Prelude.Text ->
  -- | 'targets'
  Prelude.NonEmpty Target ->
  DeregisterTargets
newDeregisterTargets
  pTargetGroupIdentifier_
  pTargets_ =
    DeregisterTargets'
      { targetGroupIdentifier =
          pTargetGroupIdentifier_,
        targets = Lens.coerced Lens.# pTargets_
      }

-- | The ID or Amazon Resource Name (ARN) of the target group.
deregisterTargets_targetGroupIdentifier :: Lens.Lens' DeregisterTargets Prelude.Text
deregisterTargets_targetGroupIdentifier = Lens.lens (\DeregisterTargets' {targetGroupIdentifier} -> targetGroupIdentifier) (\s@DeregisterTargets' {} a -> s {targetGroupIdentifier = a} :: DeregisterTargets)

-- | The targets to deregister.
deregisterTargets_targets :: Lens.Lens' DeregisterTargets (Prelude.NonEmpty Target)
deregisterTargets_targets = Lens.lens (\DeregisterTargets' {targets} -> targets) (\s@DeregisterTargets' {} a -> s {targets = a} :: DeregisterTargets) Prelude.. Lens.coerced

instance Core.AWSRequest DeregisterTargets where
  type
    AWSResponse DeregisterTargets =
      DeregisterTargetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeregisterTargetsResponse'
            Prelude.<$> (x Data..?> "successful" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "unsuccessful" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeregisterTargets where
  hashWithSalt _salt DeregisterTargets' {..} =
    _salt
      `Prelude.hashWithSalt` targetGroupIdentifier
      `Prelude.hashWithSalt` targets

instance Prelude.NFData DeregisterTargets where
  rnf DeregisterTargets' {..} =
    Prelude.rnf targetGroupIdentifier
      `Prelude.seq` Prelude.rnf targets

instance Data.ToHeaders DeregisterTargets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeregisterTargets where
  toJSON DeregisterTargets' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("targets" Data..= targets)]
      )

instance Data.ToPath DeregisterTargets where
  toPath DeregisterTargets' {..} =
    Prelude.mconcat
      [ "/targetgroups/",
        Data.toBS targetGroupIdentifier,
        "/deregistertargets"
      ]

instance Data.ToQuery DeregisterTargets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterTargetsResponse' smart constructor.
data DeregisterTargetsResponse = DeregisterTargetsResponse'
  { -- | The targets that were successfully deregistered.
    successful :: Prelude.Maybe [Target],
    -- | The targets that the operation couldn\'t deregister.
    unsuccessful :: Prelude.Maybe [TargetFailure],
    -- | The response's http status code.
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
-- 'successful', 'deregisterTargetsResponse_successful' - The targets that were successfully deregistered.
--
-- 'unsuccessful', 'deregisterTargetsResponse_unsuccessful' - The targets that the operation couldn\'t deregister.
--
-- 'httpStatus', 'deregisterTargetsResponse_httpStatus' - The response's http status code.
newDeregisterTargetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeregisterTargetsResponse
newDeregisterTargetsResponse pHttpStatus_ =
  DeregisterTargetsResponse'
    { successful =
        Prelude.Nothing,
      unsuccessful = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The targets that were successfully deregistered.
deregisterTargetsResponse_successful :: Lens.Lens' DeregisterTargetsResponse (Prelude.Maybe [Target])
deregisterTargetsResponse_successful = Lens.lens (\DeregisterTargetsResponse' {successful} -> successful) (\s@DeregisterTargetsResponse' {} a -> s {successful = a} :: DeregisterTargetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The targets that the operation couldn\'t deregister.
deregisterTargetsResponse_unsuccessful :: Lens.Lens' DeregisterTargetsResponse (Prelude.Maybe [TargetFailure])
deregisterTargetsResponse_unsuccessful = Lens.lens (\DeregisterTargetsResponse' {unsuccessful} -> unsuccessful) (\s@DeregisterTargetsResponse' {} a -> s {unsuccessful = a} :: DeregisterTargetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
deregisterTargetsResponse_httpStatus :: Lens.Lens' DeregisterTargetsResponse Prelude.Int
deregisterTargetsResponse_httpStatus = Lens.lens (\DeregisterTargetsResponse' {httpStatus} -> httpStatus) (\s@DeregisterTargetsResponse' {} a -> s {httpStatus = a} :: DeregisterTargetsResponse)

instance Prelude.NFData DeregisterTargetsResponse where
  rnf DeregisterTargetsResponse' {..} =
    Prelude.rnf successful
      `Prelude.seq` Prelude.rnf unsuccessful
      `Prelude.seq` Prelude.rnf httpStatus
