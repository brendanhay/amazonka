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
-- Module      : Amazonka.RedshiftServerLess.UpdateUsageLimit
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a usage limit in Amazon Redshift Serverless. You can\'t update
-- the usage type or period of a usage limit.
module Amazonka.RedshiftServerLess.UpdateUsageLimit
  ( -- * Creating a Request
    UpdateUsageLimit (..),
    newUpdateUsageLimit,

    -- * Request Lenses
    updateUsageLimit_breachAction,
    updateUsageLimit_amount,
    updateUsageLimit_usageLimitId,

    -- * Destructuring the Response
    UpdateUsageLimitResponse (..),
    newUpdateUsageLimitResponse,

    -- * Response Lenses
    updateUsageLimitResponse_usageLimit,
    updateUsageLimitResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateUsageLimit' smart constructor.
data UpdateUsageLimit = UpdateUsageLimit'
  { -- | The new action that Amazon Redshift Serverless takes when the limit is
    -- reached.
    breachAction :: Prelude.Maybe UsageLimitBreachAction,
    -- | The new limit amount. For more information about this parameter.
    amount :: Prelude.Maybe Prelude.Integer,
    -- | The identifier of the usage limit to update.
    usageLimitId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUsageLimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'breachAction', 'updateUsageLimit_breachAction' - The new action that Amazon Redshift Serverless takes when the limit is
-- reached.
--
-- 'amount', 'updateUsageLimit_amount' - The new limit amount. For more information about this parameter.
--
-- 'usageLimitId', 'updateUsageLimit_usageLimitId' - The identifier of the usage limit to update.
newUpdateUsageLimit ::
  -- | 'usageLimitId'
  Prelude.Text ->
  UpdateUsageLimit
newUpdateUsageLimit pUsageLimitId_ =
  UpdateUsageLimit'
    { breachAction = Prelude.Nothing,
      amount = Prelude.Nothing,
      usageLimitId = pUsageLimitId_
    }

-- | The new action that Amazon Redshift Serverless takes when the limit is
-- reached.
updateUsageLimit_breachAction :: Lens.Lens' UpdateUsageLimit (Prelude.Maybe UsageLimitBreachAction)
updateUsageLimit_breachAction = Lens.lens (\UpdateUsageLimit' {breachAction} -> breachAction) (\s@UpdateUsageLimit' {} a -> s {breachAction = a} :: UpdateUsageLimit)

-- | The new limit amount. For more information about this parameter.
updateUsageLimit_amount :: Lens.Lens' UpdateUsageLimit (Prelude.Maybe Prelude.Integer)
updateUsageLimit_amount = Lens.lens (\UpdateUsageLimit' {amount} -> amount) (\s@UpdateUsageLimit' {} a -> s {amount = a} :: UpdateUsageLimit)

-- | The identifier of the usage limit to update.
updateUsageLimit_usageLimitId :: Lens.Lens' UpdateUsageLimit Prelude.Text
updateUsageLimit_usageLimitId = Lens.lens (\UpdateUsageLimit' {usageLimitId} -> usageLimitId) (\s@UpdateUsageLimit' {} a -> s {usageLimitId = a} :: UpdateUsageLimit)

instance Core.AWSRequest UpdateUsageLimit where
  type
    AWSResponse UpdateUsageLimit =
      UpdateUsageLimitResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateUsageLimitResponse'
            Prelude.<$> (x Core..?> "usageLimit")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateUsageLimit where
  hashWithSalt _salt UpdateUsageLimit' {..} =
    _salt `Prelude.hashWithSalt` breachAction
      `Prelude.hashWithSalt` amount
      `Prelude.hashWithSalt` usageLimitId

instance Prelude.NFData UpdateUsageLimit where
  rnf UpdateUsageLimit' {..} =
    Prelude.rnf breachAction
      `Prelude.seq` Prelude.rnf amount
      `Prelude.seq` Prelude.rnf usageLimitId

instance Core.ToHeaders UpdateUsageLimit where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RedshiftServerless.UpdateUsageLimit" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateUsageLimit where
  toJSON UpdateUsageLimit' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("breachAction" Core..=) Prelude.<$> breachAction,
            ("amount" Core..=) Prelude.<$> amount,
            Prelude.Just ("usageLimitId" Core..= usageLimitId)
          ]
      )

instance Core.ToPath UpdateUsageLimit where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateUsageLimit where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateUsageLimitResponse' smart constructor.
data UpdateUsageLimitResponse = UpdateUsageLimitResponse'
  { -- | The updated usage limit object.
    usageLimit :: Prelude.Maybe UsageLimit,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateUsageLimitResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'usageLimit', 'updateUsageLimitResponse_usageLimit' - The updated usage limit object.
--
-- 'httpStatus', 'updateUsageLimitResponse_httpStatus' - The response's http status code.
newUpdateUsageLimitResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateUsageLimitResponse
newUpdateUsageLimitResponse pHttpStatus_ =
  UpdateUsageLimitResponse'
    { usageLimit =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated usage limit object.
updateUsageLimitResponse_usageLimit :: Lens.Lens' UpdateUsageLimitResponse (Prelude.Maybe UsageLimit)
updateUsageLimitResponse_usageLimit = Lens.lens (\UpdateUsageLimitResponse' {usageLimit} -> usageLimit) (\s@UpdateUsageLimitResponse' {} a -> s {usageLimit = a} :: UpdateUsageLimitResponse)

-- | The response's http status code.
updateUsageLimitResponse_httpStatus :: Lens.Lens' UpdateUsageLimitResponse Prelude.Int
updateUsageLimitResponse_httpStatus = Lens.lens (\UpdateUsageLimitResponse' {httpStatus} -> httpStatus) (\s@UpdateUsageLimitResponse' {} a -> s {httpStatus = a} :: UpdateUsageLimitResponse)

instance Prelude.NFData UpdateUsageLimitResponse where
  rnf UpdateUsageLimitResponse' {..} =
    Prelude.rnf usageLimit
      `Prelude.seq` Prelude.rnf httpStatus
