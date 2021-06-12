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
-- Module      : Network.AWS.WAFRegional.GetRule
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
-- Returns the Rule that is specified by the @RuleId@ that you included in
-- the @GetRule@ request.
module Network.AWS.WAFRegional.GetRule
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAFRegional.Types

-- | /See:/ 'newGetRule' smart constructor.
data GetRule = GetRule'
  { -- | The @RuleId@ of the Rule that you want to get. @RuleId@ is returned by
    -- CreateRule and by ListRules.
    ruleId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetRule
newGetRule pRuleId_ = GetRule' {ruleId = pRuleId_}

-- | The @RuleId@ of the Rule that you want to get. @RuleId@ is returned by
-- CreateRule and by ListRules.
getRule_ruleId :: Lens.Lens' GetRule Core.Text
getRule_ruleId = Lens.lens (\GetRule' {ruleId} -> ruleId) (\s@GetRule' {} a -> s {ruleId = a} :: GetRule)

instance Core.AWSRequest GetRule where
  type AWSResponse GetRule = GetRuleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRuleResponse'
            Core.<$> (x Core..?> "Rule")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetRule

instance Core.NFData GetRule

instance Core.ToHeaders GetRule where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_Regional_20161128.GetRule" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetRule where
  toJSON GetRule' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("RuleId" Core..= ruleId)]
      )

instance Core.ToPath GetRule where
  toPath = Core.const "/"

instance Core.ToQuery GetRule where
  toQuery = Core.const Core.mempty

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
    rule :: Core.Maybe Rule,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetRuleResponse
newGetRuleResponse pHttpStatus_ =
  GetRuleResponse'
    { rule = Core.Nothing,
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
getRuleResponse_rule :: Lens.Lens' GetRuleResponse (Core.Maybe Rule)
getRuleResponse_rule = Lens.lens (\GetRuleResponse' {rule} -> rule) (\s@GetRuleResponse' {} a -> s {rule = a} :: GetRuleResponse)

-- | The response's http status code.
getRuleResponse_httpStatus :: Lens.Lens' GetRuleResponse Core.Int
getRuleResponse_httpStatus = Lens.lens (\GetRuleResponse' {httpStatus} -> httpStatus) (\s@GetRuleResponse' {} a -> s {httpStatus = a} :: GetRuleResponse)

instance Core.NFData GetRuleResponse
