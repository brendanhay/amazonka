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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    updateUsageLimit_amount,
    updateUsageLimit_breachAction,
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateUsageLimit' smart constructor.
data UpdateUsageLimit = UpdateUsageLimit'
  { -- | The new limit amount. If time-based, this amount is in Redshift
    -- Processing Units (RPU) consumed per hour. If data-based, this amount is
    -- in terabytes (TB) of data transferred between Regions in cross-account
    -- sharing. The value must be a positive number.
    amount :: Prelude.Maybe Prelude.Integer,
    -- | The new action that Amazon Redshift Serverless takes when the limit is
    -- reached.
    breachAction :: Prelude.Maybe UsageLimitBreachAction,
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
-- 'amount', 'updateUsageLimit_amount' - The new limit amount. If time-based, this amount is in Redshift
-- Processing Units (RPU) consumed per hour. If data-based, this amount is
-- in terabytes (TB) of data transferred between Regions in cross-account
-- sharing. The value must be a positive number.
--
-- 'breachAction', 'updateUsageLimit_breachAction' - The new action that Amazon Redshift Serverless takes when the limit is
-- reached.
--
-- 'usageLimitId', 'updateUsageLimit_usageLimitId' - The identifier of the usage limit to update.
newUpdateUsageLimit ::
  -- | 'usageLimitId'
  Prelude.Text ->
  UpdateUsageLimit
newUpdateUsageLimit pUsageLimitId_ =
  UpdateUsageLimit'
    { amount = Prelude.Nothing,
      breachAction = Prelude.Nothing,
      usageLimitId = pUsageLimitId_
    }

-- | The new limit amount. If time-based, this amount is in Redshift
-- Processing Units (RPU) consumed per hour. If data-based, this amount is
-- in terabytes (TB) of data transferred between Regions in cross-account
-- sharing. The value must be a positive number.
updateUsageLimit_amount :: Lens.Lens' UpdateUsageLimit (Prelude.Maybe Prelude.Integer)
updateUsageLimit_amount = Lens.lens (\UpdateUsageLimit' {amount} -> amount) (\s@UpdateUsageLimit' {} a -> s {amount = a} :: UpdateUsageLimit)

-- | The new action that Amazon Redshift Serverless takes when the limit is
-- reached.
updateUsageLimit_breachAction :: Lens.Lens' UpdateUsageLimit (Prelude.Maybe UsageLimitBreachAction)
updateUsageLimit_breachAction = Lens.lens (\UpdateUsageLimit' {breachAction} -> breachAction) (\s@UpdateUsageLimit' {} a -> s {breachAction = a} :: UpdateUsageLimit)

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
            Prelude.<$> (x Data..?> "usageLimit")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateUsageLimit where
  hashWithSalt _salt UpdateUsageLimit' {..} =
    _salt
      `Prelude.hashWithSalt` amount
      `Prelude.hashWithSalt` breachAction
      `Prelude.hashWithSalt` usageLimitId

instance Prelude.NFData UpdateUsageLimit where
  rnf UpdateUsageLimit' {..} =
    Prelude.rnf amount `Prelude.seq`
      Prelude.rnf breachAction `Prelude.seq`
        Prelude.rnf usageLimitId

instance Data.ToHeaders UpdateUsageLimit where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RedshiftServerless.UpdateUsageLimit" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateUsageLimit where
  toJSON UpdateUsageLimit' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("amount" Data..=) Prelude.<$> amount,
            ("breachAction" Data..=) Prelude.<$> breachAction,
            Prelude.Just ("usageLimitId" Data..= usageLimitId)
          ]
      )

instance Data.ToPath UpdateUsageLimit where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateUsageLimit where
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
    Prelude.rnf usageLimit `Prelude.seq`
      Prelude.rnf httpStatus
