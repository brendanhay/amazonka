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
-- Module      : Amazonka.IoT.GetTopicRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the rule.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions GetTopicRule>
-- action.
module Amazonka.IoT.GetTopicRule
  ( -- * Creating a Request
    GetTopicRule (..),
    newGetTopicRule,

    -- * Request Lenses
    getTopicRule_ruleName,

    -- * Destructuring the Response
    GetTopicRuleResponse (..),
    newGetTopicRuleResponse,

    -- * Response Lenses
    getTopicRuleResponse_rule,
    getTopicRuleResponse_ruleArn,
    getTopicRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the GetTopicRule operation.
--
-- /See:/ 'newGetTopicRule' smart constructor.
data GetTopicRule = GetTopicRule'
  { -- | The name of the rule.
    ruleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTopicRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleName', 'getTopicRule_ruleName' - The name of the rule.
newGetTopicRule ::
  -- | 'ruleName'
  Prelude.Text ->
  GetTopicRule
newGetTopicRule pRuleName_ =
  GetTopicRule' {ruleName = pRuleName_}

-- | The name of the rule.
getTopicRule_ruleName :: Lens.Lens' GetTopicRule Prelude.Text
getTopicRule_ruleName = Lens.lens (\GetTopicRule' {ruleName} -> ruleName) (\s@GetTopicRule' {} a -> s {ruleName = a} :: GetTopicRule)

instance Core.AWSRequest GetTopicRule where
  type AWSResponse GetTopicRule = GetTopicRuleResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTopicRuleResponse'
            Prelude.<$> (x Data..?> "rule")
            Prelude.<*> (x Data..?> "ruleArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTopicRule where
  hashWithSalt _salt GetTopicRule' {..} =
    _salt `Prelude.hashWithSalt` ruleName

instance Prelude.NFData GetTopicRule where
  rnf GetTopicRule' {..} = Prelude.rnf ruleName

instance Data.ToHeaders GetTopicRule where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetTopicRule where
  toPath GetTopicRule' {..} =
    Prelude.mconcat ["/rules/", Data.toBS ruleName]

instance Data.ToQuery GetTopicRule where
  toQuery = Prelude.const Prelude.mempty

-- | The output from the GetTopicRule operation.
--
-- /See:/ 'newGetTopicRuleResponse' smart constructor.
data GetTopicRuleResponse = GetTopicRuleResponse'
  { -- | The rule.
    rule :: Prelude.Maybe TopicRule,
    -- | The rule ARN.
    ruleArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTopicRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rule', 'getTopicRuleResponse_rule' - The rule.
--
-- 'ruleArn', 'getTopicRuleResponse_ruleArn' - The rule ARN.
--
-- 'httpStatus', 'getTopicRuleResponse_httpStatus' - The response's http status code.
newGetTopicRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTopicRuleResponse
newGetTopicRuleResponse pHttpStatus_ =
  GetTopicRuleResponse'
    { rule = Prelude.Nothing,
      ruleArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The rule.
getTopicRuleResponse_rule :: Lens.Lens' GetTopicRuleResponse (Prelude.Maybe TopicRule)
getTopicRuleResponse_rule = Lens.lens (\GetTopicRuleResponse' {rule} -> rule) (\s@GetTopicRuleResponse' {} a -> s {rule = a} :: GetTopicRuleResponse)

-- | The rule ARN.
getTopicRuleResponse_ruleArn :: Lens.Lens' GetTopicRuleResponse (Prelude.Maybe Prelude.Text)
getTopicRuleResponse_ruleArn = Lens.lens (\GetTopicRuleResponse' {ruleArn} -> ruleArn) (\s@GetTopicRuleResponse' {} a -> s {ruleArn = a} :: GetTopicRuleResponse)

-- | The response's http status code.
getTopicRuleResponse_httpStatus :: Lens.Lens' GetTopicRuleResponse Prelude.Int
getTopicRuleResponse_httpStatus = Lens.lens (\GetTopicRuleResponse' {httpStatus} -> httpStatus) (\s@GetTopicRuleResponse' {} a -> s {httpStatus = a} :: GetTopicRuleResponse)

instance Prelude.NFData GetTopicRuleResponse where
  rnf GetTopicRuleResponse' {..} =
    Prelude.rnf rule
      `Prelude.seq` Prelude.rnf ruleArn
      `Prelude.seq` Prelude.rnf httpStatus
