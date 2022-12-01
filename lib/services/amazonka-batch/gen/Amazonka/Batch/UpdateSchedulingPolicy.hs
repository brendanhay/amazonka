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
-- Module      : Amazonka.Batch.UpdateSchedulingPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a scheduling policy.
module Amazonka.Batch.UpdateSchedulingPolicy
  ( -- * Creating a Request
    UpdateSchedulingPolicy (..),
    newUpdateSchedulingPolicy,

    -- * Request Lenses
    updateSchedulingPolicy_fairsharePolicy,
    updateSchedulingPolicy_arn,

    -- * Destructuring the Response
    UpdateSchedulingPolicyResponse (..),
    newUpdateSchedulingPolicyResponse,

    -- * Response Lenses
    updateSchedulingPolicyResponse_httpStatus,
  )
where

import Amazonka.Batch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for @UpdateSchedulingPolicy@.
--
-- /See:/ 'newUpdateSchedulingPolicy' smart constructor.
data UpdateSchedulingPolicy = UpdateSchedulingPolicy'
  { -- | The fair share policy.
    fairsharePolicy :: Prelude.Maybe FairsharePolicy,
    -- | The Amazon Resource Name (ARN) of the scheduling policy to update.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSchedulingPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fairsharePolicy', 'updateSchedulingPolicy_fairsharePolicy' - The fair share policy.
--
-- 'arn', 'updateSchedulingPolicy_arn' - The Amazon Resource Name (ARN) of the scheduling policy to update.
newUpdateSchedulingPolicy ::
  -- | 'arn'
  Prelude.Text ->
  UpdateSchedulingPolicy
newUpdateSchedulingPolicy pArn_ =
  UpdateSchedulingPolicy'
    { fairsharePolicy =
        Prelude.Nothing,
      arn = pArn_
    }

-- | The fair share policy.
updateSchedulingPolicy_fairsharePolicy :: Lens.Lens' UpdateSchedulingPolicy (Prelude.Maybe FairsharePolicy)
updateSchedulingPolicy_fairsharePolicy = Lens.lens (\UpdateSchedulingPolicy' {fairsharePolicy} -> fairsharePolicy) (\s@UpdateSchedulingPolicy' {} a -> s {fairsharePolicy = a} :: UpdateSchedulingPolicy)

-- | The Amazon Resource Name (ARN) of the scheduling policy to update.
updateSchedulingPolicy_arn :: Lens.Lens' UpdateSchedulingPolicy Prelude.Text
updateSchedulingPolicy_arn = Lens.lens (\UpdateSchedulingPolicy' {arn} -> arn) (\s@UpdateSchedulingPolicy' {} a -> s {arn = a} :: UpdateSchedulingPolicy)

instance Core.AWSRequest UpdateSchedulingPolicy where
  type
    AWSResponse UpdateSchedulingPolicy =
      UpdateSchedulingPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateSchedulingPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSchedulingPolicy where
  hashWithSalt _salt UpdateSchedulingPolicy' {..} =
    _salt `Prelude.hashWithSalt` fairsharePolicy
      `Prelude.hashWithSalt` arn

instance Prelude.NFData UpdateSchedulingPolicy where
  rnf UpdateSchedulingPolicy' {..} =
    Prelude.rnf fairsharePolicy
      `Prelude.seq` Prelude.rnf arn

instance Core.ToHeaders UpdateSchedulingPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateSchedulingPolicy where
  toJSON UpdateSchedulingPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("fairsharePolicy" Core..=)
              Prelude.<$> fairsharePolicy,
            Prelude.Just ("arn" Core..= arn)
          ]
      )

instance Core.ToPath UpdateSchedulingPolicy where
  toPath = Prelude.const "/v1/updateschedulingpolicy"

instance Core.ToQuery UpdateSchedulingPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSchedulingPolicyResponse' smart constructor.
data UpdateSchedulingPolicyResponse = UpdateSchedulingPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSchedulingPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateSchedulingPolicyResponse_httpStatus' - The response's http status code.
newUpdateSchedulingPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSchedulingPolicyResponse
newUpdateSchedulingPolicyResponse pHttpStatus_ =
  UpdateSchedulingPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateSchedulingPolicyResponse_httpStatus :: Lens.Lens' UpdateSchedulingPolicyResponse Prelude.Int
updateSchedulingPolicyResponse_httpStatus = Lens.lens (\UpdateSchedulingPolicyResponse' {httpStatus} -> httpStatus) (\s@UpdateSchedulingPolicyResponse' {} a -> s {httpStatus = a} :: UpdateSchedulingPolicyResponse)

instance
  Prelude.NFData
    UpdateSchedulingPolicyResponse
  where
  rnf UpdateSchedulingPolicyResponse' {..} =
    Prelude.rnf httpStatus
