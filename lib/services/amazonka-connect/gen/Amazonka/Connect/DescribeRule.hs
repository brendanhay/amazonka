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
-- Module      : Amazonka.Connect.DescribeRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a rule for the specified Amazon Connect instance.
module Amazonka.Connect.DescribeRule
  ( -- * Creating a Request
    DescribeRule (..),
    newDescribeRule,

    -- * Request Lenses
    describeRule_instanceId,
    describeRule_ruleId,

    -- * Destructuring the Response
    DescribeRuleResponse (..),
    newDescribeRuleResponse,

    -- * Response Lenses
    describeRuleResponse_httpStatus,
    describeRuleResponse_rule,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeRule' smart constructor.
data DescribeRule = DescribeRule'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | A unique identifier for the rule.
    ruleId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'describeRule_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'ruleId', 'describeRule_ruleId' - A unique identifier for the rule.
newDescribeRule ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'ruleId'
  Prelude.Text ->
  DescribeRule
newDescribeRule pInstanceId_ pRuleId_ =
  DescribeRule'
    { instanceId = pInstanceId_,
      ruleId = pRuleId_
    }

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
describeRule_instanceId :: Lens.Lens' DescribeRule Prelude.Text
describeRule_instanceId = Lens.lens (\DescribeRule' {instanceId} -> instanceId) (\s@DescribeRule' {} a -> s {instanceId = a} :: DescribeRule)

-- | A unique identifier for the rule.
describeRule_ruleId :: Lens.Lens' DescribeRule Prelude.Text
describeRule_ruleId = Lens.lens (\DescribeRule' {ruleId} -> ruleId) (\s@DescribeRule' {} a -> s {ruleId = a} :: DescribeRule)

instance Core.AWSRequest DescribeRule where
  type AWSResponse DescribeRule = DescribeRuleResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRuleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Rule")
      )

instance Prelude.Hashable DescribeRule where
  hashWithSalt _salt DescribeRule' {..} =
    _salt `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` ruleId

instance Prelude.NFData DescribeRule where
  rnf DescribeRule' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf ruleId

instance Data.ToHeaders DescribeRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeRule where
  toPath DescribeRule' {..} =
    Prelude.mconcat
      [ "/rules/",
        Data.toBS instanceId,
        "/",
        Data.toBS ruleId
      ]

instance Data.ToQuery DescribeRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRuleResponse' smart constructor.
data DescribeRuleResponse = DescribeRuleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the rule.
    rule :: Rule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeRuleResponse_httpStatus' - The response's http status code.
--
-- 'rule', 'describeRuleResponse_rule' - Information about the rule.
newDescribeRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'rule'
  Rule ->
  DescribeRuleResponse
newDescribeRuleResponse pHttpStatus_ pRule_ =
  DescribeRuleResponse'
    { httpStatus = pHttpStatus_,
      rule = pRule_
    }

-- | The response's http status code.
describeRuleResponse_httpStatus :: Lens.Lens' DescribeRuleResponse Prelude.Int
describeRuleResponse_httpStatus = Lens.lens (\DescribeRuleResponse' {httpStatus} -> httpStatus) (\s@DescribeRuleResponse' {} a -> s {httpStatus = a} :: DescribeRuleResponse)

-- | Information about the rule.
describeRuleResponse_rule :: Lens.Lens' DescribeRuleResponse Rule
describeRuleResponse_rule = Lens.lens (\DescribeRuleResponse' {rule} -> rule) (\s@DescribeRuleResponse' {} a -> s {rule = a} :: DescribeRuleResponse)

instance Prelude.NFData DescribeRuleResponse where
  rnf DescribeRuleResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf rule
