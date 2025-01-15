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
-- Module      : Amazonka.Route53RecoveryControlConfig.DescribeSafetyRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a safety rule.
module Amazonka.Route53RecoveryControlConfig.DescribeSafetyRule
  ( -- * Creating a Request
    DescribeSafetyRule (..),
    newDescribeSafetyRule,

    -- * Request Lenses
    describeSafetyRule_safetyRuleArn,

    -- * Destructuring the Response
    DescribeSafetyRuleResponse (..),
    newDescribeSafetyRuleResponse,

    -- * Response Lenses
    describeSafetyRuleResponse_assertionRule,
    describeSafetyRuleResponse_gatingRule,
    describeSafetyRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryControlConfig.Types

-- | /See:/ 'newDescribeSafetyRule' smart constructor.
data DescribeSafetyRule = DescribeSafetyRule'
  { -- | The ARN of the safety rule.
    safetyRuleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSafetyRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'safetyRuleArn', 'describeSafetyRule_safetyRuleArn' - The ARN of the safety rule.
newDescribeSafetyRule ::
  -- | 'safetyRuleArn'
  Prelude.Text ->
  DescribeSafetyRule
newDescribeSafetyRule pSafetyRuleArn_ =
  DescribeSafetyRule'
    { safetyRuleArn =
        pSafetyRuleArn_
    }

-- | The ARN of the safety rule.
describeSafetyRule_safetyRuleArn :: Lens.Lens' DescribeSafetyRule Prelude.Text
describeSafetyRule_safetyRuleArn = Lens.lens (\DescribeSafetyRule' {safetyRuleArn} -> safetyRuleArn) (\s@DescribeSafetyRule' {} a -> s {safetyRuleArn = a} :: DescribeSafetyRule)

instance Core.AWSRequest DescribeSafetyRule where
  type
    AWSResponse DescribeSafetyRule =
      DescribeSafetyRuleResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSafetyRuleResponse'
            Prelude.<$> (x Data..?> "AssertionRule")
            Prelude.<*> (x Data..?> "GatingRule")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSafetyRule where
  hashWithSalt _salt DescribeSafetyRule' {..} =
    _salt `Prelude.hashWithSalt` safetyRuleArn

instance Prelude.NFData DescribeSafetyRule where
  rnf DescribeSafetyRule' {..} =
    Prelude.rnf safetyRuleArn

instance Data.ToHeaders DescribeSafetyRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeSafetyRule where
  toPath DescribeSafetyRule' {..} =
    Prelude.mconcat
      ["/safetyrule/", Data.toBS safetyRuleArn]

instance Data.ToQuery DescribeSafetyRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSafetyRuleResponse' smart constructor.
data DescribeSafetyRuleResponse = DescribeSafetyRuleResponse'
  { -- | The assertion rule in the response.
    assertionRule :: Prelude.Maybe AssertionRule,
    -- | The gating rule in the response.
    gatingRule :: Prelude.Maybe GatingRule,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSafetyRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assertionRule', 'describeSafetyRuleResponse_assertionRule' - The assertion rule in the response.
--
-- 'gatingRule', 'describeSafetyRuleResponse_gatingRule' - The gating rule in the response.
--
-- 'httpStatus', 'describeSafetyRuleResponse_httpStatus' - The response's http status code.
newDescribeSafetyRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSafetyRuleResponse
newDescribeSafetyRuleResponse pHttpStatus_ =
  DescribeSafetyRuleResponse'
    { assertionRule =
        Prelude.Nothing,
      gatingRule = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The assertion rule in the response.
describeSafetyRuleResponse_assertionRule :: Lens.Lens' DescribeSafetyRuleResponse (Prelude.Maybe AssertionRule)
describeSafetyRuleResponse_assertionRule = Lens.lens (\DescribeSafetyRuleResponse' {assertionRule} -> assertionRule) (\s@DescribeSafetyRuleResponse' {} a -> s {assertionRule = a} :: DescribeSafetyRuleResponse)

-- | The gating rule in the response.
describeSafetyRuleResponse_gatingRule :: Lens.Lens' DescribeSafetyRuleResponse (Prelude.Maybe GatingRule)
describeSafetyRuleResponse_gatingRule = Lens.lens (\DescribeSafetyRuleResponse' {gatingRule} -> gatingRule) (\s@DescribeSafetyRuleResponse' {} a -> s {gatingRule = a} :: DescribeSafetyRuleResponse)

-- | The response's http status code.
describeSafetyRuleResponse_httpStatus :: Lens.Lens' DescribeSafetyRuleResponse Prelude.Int
describeSafetyRuleResponse_httpStatus = Lens.lens (\DescribeSafetyRuleResponse' {httpStatus} -> httpStatus) (\s@DescribeSafetyRuleResponse' {} a -> s {httpStatus = a} :: DescribeSafetyRuleResponse)

instance Prelude.NFData DescribeSafetyRuleResponse where
  rnf DescribeSafetyRuleResponse' {..} =
    Prelude.rnf assertionRule `Prelude.seq`
      Prelude.rnf gatingRule `Prelude.seq`
        Prelude.rnf httpStatus
