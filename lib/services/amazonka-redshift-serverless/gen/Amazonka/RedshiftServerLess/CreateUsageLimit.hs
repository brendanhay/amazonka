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
-- Module      : Amazonka.RedshiftServerLess.CreateUsageLimit
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a usage limit for a specified Amazon Redshift Serverless usage
-- type. The usage limit is identified by the returned usage limit
-- identifier.
module Amazonka.RedshiftServerLess.CreateUsageLimit
  ( -- * Creating a Request
    CreateUsageLimit (..),
    newCreateUsageLimit,

    -- * Request Lenses
    createUsageLimit_period,
    createUsageLimit_breachAction,
    createUsageLimit_amount,
    createUsageLimit_resourceArn,
    createUsageLimit_usageType,

    -- * Destructuring the Response
    CreateUsageLimitResponse (..),
    newCreateUsageLimitResponse,

    -- * Response Lenses
    createUsageLimitResponse_usageLimit,
    createUsageLimitResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateUsageLimit' smart constructor.
data CreateUsageLimit = CreateUsageLimit'
  { -- | The time period that the amount applies to. A weekly period begins on
    -- Sunday. The default is monthly.
    period :: Prelude.Maybe UsageLimitPeriod,
    -- | The action that Amazon Redshift Serverless takes when the limit is
    -- reached. The default is log.
    breachAction :: Prelude.Maybe UsageLimitBreachAction,
    -- | The limit amount. If time-based, this amount is in Redshift Processing
    -- Units (RPU) consumed per hour. If data-based, this amount is in
    -- terabytes (TB) of data transferred between Regions in cross-account
    -- sharing. The value must be a positive number.
    amount :: Prelude.Integer,
    -- | The Amazon Resource Name (ARN) of the Amazon Redshift Serverless
    -- resource to create the usage limit for.
    resourceArn :: Prelude.Text,
    -- | The type of Amazon Redshift Serverless usage to create a usage limit
    -- for.
    usageType :: UsageLimitUsageType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateUsageLimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'period', 'createUsageLimit_period' - The time period that the amount applies to. A weekly period begins on
-- Sunday. The default is monthly.
--
-- 'breachAction', 'createUsageLimit_breachAction' - The action that Amazon Redshift Serverless takes when the limit is
-- reached. The default is log.
--
-- 'amount', 'createUsageLimit_amount' - The limit amount. If time-based, this amount is in Redshift Processing
-- Units (RPU) consumed per hour. If data-based, this amount is in
-- terabytes (TB) of data transferred between Regions in cross-account
-- sharing. The value must be a positive number.
--
-- 'resourceArn', 'createUsageLimit_resourceArn' - The Amazon Resource Name (ARN) of the Amazon Redshift Serverless
-- resource to create the usage limit for.
--
-- 'usageType', 'createUsageLimit_usageType' - The type of Amazon Redshift Serverless usage to create a usage limit
-- for.
newCreateUsageLimit ::
  -- | 'amount'
  Prelude.Integer ->
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'usageType'
  UsageLimitUsageType ->
  CreateUsageLimit
newCreateUsageLimit
  pAmount_
  pResourceArn_
  pUsageType_ =
    CreateUsageLimit'
      { period = Prelude.Nothing,
        breachAction = Prelude.Nothing,
        amount = pAmount_,
        resourceArn = pResourceArn_,
        usageType = pUsageType_
      }

-- | The time period that the amount applies to. A weekly period begins on
-- Sunday. The default is monthly.
createUsageLimit_period :: Lens.Lens' CreateUsageLimit (Prelude.Maybe UsageLimitPeriod)
createUsageLimit_period = Lens.lens (\CreateUsageLimit' {period} -> period) (\s@CreateUsageLimit' {} a -> s {period = a} :: CreateUsageLimit)

-- | The action that Amazon Redshift Serverless takes when the limit is
-- reached. The default is log.
createUsageLimit_breachAction :: Lens.Lens' CreateUsageLimit (Prelude.Maybe UsageLimitBreachAction)
createUsageLimit_breachAction = Lens.lens (\CreateUsageLimit' {breachAction} -> breachAction) (\s@CreateUsageLimit' {} a -> s {breachAction = a} :: CreateUsageLimit)

-- | The limit amount. If time-based, this amount is in Redshift Processing
-- Units (RPU) consumed per hour. If data-based, this amount is in
-- terabytes (TB) of data transferred between Regions in cross-account
-- sharing. The value must be a positive number.
createUsageLimit_amount :: Lens.Lens' CreateUsageLimit Prelude.Integer
createUsageLimit_amount = Lens.lens (\CreateUsageLimit' {amount} -> amount) (\s@CreateUsageLimit' {} a -> s {amount = a} :: CreateUsageLimit)

-- | The Amazon Resource Name (ARN) of the Amazon Redshift Serverless
-- resource to create the usage limit for.
createUsageLimit_resourceArn :: Lens.Lens' CreateUsageLimit Prelude.Text
createUsageLimit_resourceArn = Lens.lens (\CreateUsageLimit' {resourceArn} -> resourceArn) (\s@CreateUsageLimit' {} a -> s {resourceArn = a} :: CreateUsageLimit)

-- | The type of Amazon Redshift Serverless usage to create a usage limit
-- for.
createUsageLimit_usageType :: Lens.Lens' CreateUsageLimit UsageLimitUsageType
createUsageLimit_usageType = Lens.lens (\CreateUsageLimit' {usageType} -> usageType) (\s@CreateUsageLimit' {} a -> s {usageType = a} :: CreateUsageLimit)

instance Core.AWSRequest CreateUsageLimit where
  type
    AWSResponse CreateUsageLimit =
      CreateUsageLimitResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUsageLimitResponse'
            Prelude.<$> (x Data..?> "usageLimit")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateUsageLimit where
  hashWithSalt _salt CreateUsageLimit' {..} =
    _salt `Prelude.hashWithSalt` period
      `Prelude.hashWithSalt` breachAction
      `Prelude.hashWithSalt` amount
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` usageType

instance Prelude.NFData CreateUsageLimit where
  rnf CreateUsageLimit' {..} =
    Prelude.rnf period
      `Prelude.seq` Prelude.rnf breachAction
      `Prelude.seq` Prelude.rnf amount
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf usageType

instance Data.ToHeaders CreateUsageLimit where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RedshiftServerless.CreateUsageLimit" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateUsageLimit where
  toJSON CreateUsageLimit' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("period" Data..=) Prelude.<$> period,
            ("breachAction" Data..=) Prelude.<$> breachAction,
            Prelude.Just ("amount" Data..= amount),
            Prelude.Just ("resourceArn" Data..= resourceArn),
            Prelude.Just ("usageType" Data..= usageType)
          ]
      )

instance Data.ToPath CreateUsageLimit where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateUsageLimit where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateUsageLimitResponse' smart constructor.
data CreateUsageLimitResponse = CreateUsageLimitResponse'
  { -- | The returned usage limit object.
    usageLimit :: Prelude.Maybe UsageLimit,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateUsageLimitResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'usageLimit', 'createUsageLimitResponse_usageLimit' - The returned usage limit object.
--
-- 'httpStatus', 'createUsageLimitResponse_httpStatus' - The response's http status code.
newCreateUsageLimitResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateUsageLimitResponse
newCreateUsageLimitResponse pHttpStatus_ =
  CreateUsageLimitResponse'
    { usageLimit =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The returned usage limit object.
createUsageLimitResponse_usageLimit :: Lens.Lens' CreateUsageLimitResponse (Prelude.Maybe UsageLimit)
createUsageLimitResponse_usageLimit = Lens.lens (\CreateUsageLimitResponse' {usageLimit} -> usageLimit) (\s@CreateUsageLimitResponse' {} a -> s {usageLimit = a} :: CreateUsageLimitResponse)

-- | The response's http status code.
createUsageLimitResponse_httpStatus :: Lens.Lens' CreateUsageLimitResponse Prelude.Int
createUsageLimitResponse_httpStatus = Lens.lens (\CreateUsageLimitResponse' {httpStatus} -> httpStatus) (\s@CreateUsageLimitResponse' {} a -> s {httpStatus = a} :: CreateUsageLimitResponse)

instance Prelude.NFData CreateUsageLimitResponse where
  rnf CreateUsageLimitResponse' {..} =
    Prelude.rnf usageLimit
      `Prelude.seq` Prelude.rnf httpStatus
