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
-- Module      : Amazonka.WAF.GetRule
-- Copyright   : (c) 2013-2022 Brendan Hay
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
-- Returns the Rule that is specified by the @RuleId@ that you included in
-- the @GetRule@ request.
module Amazonka.WAF.GetRule
  ( -- * Creating a Request
    GetRule (..),
    newGetRule,

    -- * Request Lenses
    getRule_ruleId,

    -- * Destructuring the Response
    GetRuleResponse (..),
    newGetRuleResponse,

    -- * Response Lenses
    getRuleResponse_rule,
    getRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAF.Types

-- | /See:/ 'newGetRule' smart constructor.
data GetRule = GetRule'
  { -- | The @RuleId@ of the Rule that you want to get. @RuleId@ is returned by
    -- CreateRule and by ListRules.
    ruleId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleId', 'getRule_ruleId' - The @RuleId@ of the Rule that you want to get. @RuleId@ is returned by
-- CreateRule and by ListRules.
newGetRule ::
  -- | 'ruleId'
  Prelude.Text ->
  GetRule
newGetRule pRuleId_ = GetRule' {ruleId = pRuleId_}

-- | The @RuleId@ of the Rule that you want to get. @RuleId@ is returned by
-- CreateRule and by ListRules.
getRule_ruleId :: Lens.Lens' GetRule Prelude.Text
getRule_ruleId = Lens.lens (\GetRule' {ruleId} -> ruleId) (\s@GetRule' {} a -> s {ruleId = a} :: GetRule)

instance Core.AWSRequest GetRule where
  type AWSResponse GetRule = GetRuleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRuleResponse'
            Prelude.<$> (x Data..?> "Rule")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRule where
  hashWithSalt _salt GetRule' {..} =
    _salt `Prelude.hashWithSalt` ruleId

instance Prelude.NFData GetRule where
  rnf GetRule' {..} = Prelude.rnf ruleId

instance Data.ToHeaders GetRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSWAF_20150824.GetRule" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetRule where
  toJSON GetRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("RuleId" Data..= ruleId)]
      )

instance Data.ToPath GetRule where
  toPath = Prelude.const "/"

instance Data.ToQuery GetRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRuleResponse' smart constructor.
data GetRuleResponse = GetRuleResponse'
  { -- | Information about the Rule that you specified in the @GetRule@ request.
    -- For more information, see the following topics:
    --
    -- -   Rule: Contains @MetricName@, @Name@, an array of @Predicate@
    --     objects, and @RuleId@
    --
    -- -   Predicate: Each @Predicate@ object contains @DataId@, @Negated@, and
    --     @Type@
    rule :: Prelude.Maybe Rule,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rule', 'getRuleResponse_rule' - Information about the Rule that you specified in the @GetRule@ request.
-- For more information, see the following topics:
--
-- -   Rule: Contains @MetricName@, @Name@, an array of @Predicate@
--     objects, and @RuleId@
--
-- -   Predicate: Each @Predicate@ object contains @DataId@, @Negated@, and
--     @Type@
--
-- 'httpStatus', 'getRuleResponse_httpStatus' - The response's http status code.
newGetRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRuleResponse
newGetRuleResponse pHttpStatus_ =
  GetRuleResponse'
    { rule = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the Rule that you specified in the @GetRule@ request.
-- For more information, see the following topics:
--
-- -   Rule: Contains @MetricName@, @Name@, an array of @Predicate@
--     objects, and @RuleId@
--
-- -   Predicate: Each @Predicate@ object contains @DataId@, @Negated@, and
--     @Type@
getRuleResponse_rule :: Lens.Lens' GetRuleResponse (Prelude.Maybe Rule)
getRuleResponse_rule = Lens.lens (\GetRuleResponse' {rule} -> rule) (\s@GetRuleResponse' {} a -> s {rule = a} :: GetRuleResponse)

-- | The response's http status code.
getRuleResponse_httpStatus :: Lens.Lens' GetRuleResponse Prelude.Int
getRuleResponse_httpStatus = Lens.lens (\GetRuleResponse' {httpStatus} -> httpStatus) (\s@GetRuleResponse' {} a -> s {httpStatus = a} :: GetRuleResponse)

instance Prelude.NFData GetRuleResponse where
  rnf GetRuleResponse' {..} =
    Prelude.rnf rule
      `Prelude.seq` Prelude.rnf httpStatus
