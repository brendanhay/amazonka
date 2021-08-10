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
-- Module      : Network.AWS.WAF.GetRateBasedRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- Returns the RateBasedRule that is specified by the @RuleId@ that you
-- included in the @GetRateBasedRule@ request.
module Network.AWS.WAF.GetRateBasedRule
  ( -- * Creating a Request
    GetRateBasedRule (..),
    newGetRateBasedRule,

    -- * Request Lenses
    getRateBasedRule_ruleId,

    -- * Destructuring the Response
    GetRateBasedRuleResponse (..),
    newGetRateBasedRuleResponse,

    -- * Response Lenses
    getRateBasedRuleResponse_rule,
    getRateBasedRuleResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | /See:/ 'newGetRateBasedRule' smart constructor.
data GetRateBasedRule = GetRateBasedRule'
  { -- | The @RuleId@ of the RateBasedRule that you want to get. @RuleId@ is
    -- returned by CreateRateBasedRule and by ListRateBasedRules.
    ruleId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRateBasedRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleId', 'getRateBasedRule_ruleId' - The @RuleId@ of the RateBasedRule that you want to get. @RuleId@ is
-- returned by CreateRateBasedRule and by ListRateBasedRules.
newGetRateBasedRule ::
  -- | 'ruleId'
  Prelude.Text ->
  GetRateBasedRule
newGetRateBasedRule pRuleId_ =
  GetRateBasedRule' {ruleId = pRuleId_}

-- | The @RuleId@ of the RateBasedRule that you want to get. @RuleId@ is
-- returned by CreateRateBasedRule and by ListRateBasedRules.
getRateBasedRule_ruleId :: Lens.Lens' GetRateBasedRule Prelude.Text
getRateBasedRule_ruleId = Lens.lens (\GetRateBasedRule' {ruleId} -> ruleId) (\s@GetRateBasedRule' {} a -> s {ruleId = a} :: GetRateBasedRule)

instance Core.AWSRequest GetRateBasedRule where
  type
    AWSResponse GetRateBasedRule =
      GetRateBasedRuleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRateBasedRuleResponse'
            Prelude.<$> (x Core..?> "Rule")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRateBasedRule

instance Prelude.NFData GetRateBasedRule

instance Core.ToHeaders GetRateBasedRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_20150824.GetRateBasedRule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetRateBasedRule where
  toJSON GetRateBasedRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("RuleId" Core..= ruleId)]
      )

instance Core.ToPath GetRateBasedRule where
  toPath = Prelude.const "/"

instance Core.ToQuery GetRateBasedRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRateBasedRuleResponse' smart constructor.
data GetRateBasedRuleResponse = GetRateBasedRuleResponse'
  { -- | Information about the RateBasedRule that you specified in the
    -- @GetRateBasedRule@ request.
    rule :: Prelude.Maybe RateBasedRule,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRateBasedRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rule', 'getRateBasedRuleResponse_rule' - Information about the RateBasedRule that you specified in the
-- @GetRateBasedRule@ request.
--
-- 'httpStatus', 'getRateBasedRuleResponse_httpStatus' - The response's http status code.
newGetRateBasedRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRateBasedRuleResponse
newGetRateBasedRuleResponse pHttpStatus_ =
  GetRateBasedRuleResponse'
    { rule = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the RateBasedRule that you specified in the
-- @GetRateBasedRule@ request.
getRateBasedRuleResponse_rule :: Lens.Lens' GetRateBasedRuleResponse (Prelude.Maybe RateBasedRule)
getRateBasedRuleResponse_rule = Lens.lens (\GetRateBasedRuleResponse' {rule} -> rule) (\s@GetRateBasedRuleResponse' {} a -> s {rule = a} :: GetRateBasedRuleResponse)

-- | The response's http status code.
getRateBasedRuleResponse_httpStatus :: Lens.Lens' GetRateBasedRuleResponse Prelude.Int
getRateBasedRuleResponse_httpStatus = Lens.lens (\GetRateBasedRuleResponse' {httpStatus} -> httpStatus) (\s@GetRateBasedRuleResponse' {} a -> s {httpStatus = a} :: GetRateBasedRuleResponse)

instance Prelude.NFData GetRateBasedRuleResponse
