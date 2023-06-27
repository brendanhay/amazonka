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
-- Module      : Amazonka.SecurityHub.BatchGetAutomationRules
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of details for automation rules based on rule Amazon
-- Resource Names (ARNs).
module Amazonka.SecurityHub.BatchGetAutomationRules
  ( -- * Creating a Request
    BatchGetAutomationRules (..),
    newBatchGetAutomationRules,

    -- * Request Lenses
    batchGetAutomationRules_automationRulesArns,

    -- * Destructuring the Response
    BatchGetAutomationRulesResponse (..),
    newBatchGetAutomationRulesResponse,

    -- * Response Lenses
    batchGetAutomationRulesResponse_rules,
    batchGetAutomationRulesResponse_unprocessedAutomationRules,
    batchGetAutomationRulesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newBatchGetAutomationRules' smart constructor.
data BatchGetAutomationRules = BatchGetAutomationRules'
  { -- | A list of rule ARNs to get details for.
    automationRulesArns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetAutomationRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'automationRulesArns', 'batchGetAutomationRules_automationRulesArns' - A list of rule ARNs to get details for.
newBatchGetAutomationRules ::
  -- | 'automationRulesArns'
  Prelude.NonEmpty Prelude.Text ->
  BatchGetAutomationRules
newBatchGetAutomationRules pAutomationRulesArns_ =
  BatchGetAutomationRules'
    { automationRulesArns =
        Lens.coerced Lens.# pAutomationRulesArns_
    }

-- | A list of rule ARNs to get details for.
batchGetAutomationRules_automationRulesArns :: Lens.Lens' BatchGetAutomationRules (Prelude.NonEmpty Prelude.Text)
batchGetAutomationRules_automationRulesArns = Lens.lens (\BatchGetAutomationRules' {automationRulesArns} -> automationRulesArns) (\s@BatchGetAutomationRules' {} a -> s {automationRulesArns = a} :: BatchGetAutomationRules) Prelude.. Lens.coerced

instance Core.AWSRequest BatchGetAutomationRules where
  type
    AWSResponse BatchGetAutomationRules =
      BatchGetAutomationRulesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetAutomationRulesResponse'
            Prelude.<$> (x Data..?> "Rules" Core..!@ Prelude.mempty)
            Prelude.<*> ( x
                            Data..?> "UnprocessedAutomationRules"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetAutomationRules where
  hashWithSalt _salt BatchGetAutomationRules' {..} =
    _salt `Prelude.hashWithSalt` automationRulesArns

instance Prelude.NFData BatchGetAutomationRules where
  rnf BatchGetAutomationRules' {..} =
    Prelude.rnf automationRulesArns

instance Data.ToHeaders BatchGetAutomationRules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchGetAutomationRules where
  toJSON BatchGetAutomationRules' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AutomationRulesArns" Data..= automationRulesArns)
          ]
      )

instance Data.ToPath BatchGetAutomationRules where
  toPath = Prelude.const "/automationrules/get"

instance Data.ToQuery BatchGetAutomationRules where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetAutomationRulesResponse' smart constructor.
data BatchGetAutomationRulesResponse = BatchGetAutomationRulesResponse'
  { -- | A list of rule details for the provided rule ARNs.
    rules :: Prelude.Maybe [AutomationRulesConfig],
    -- | A list of objects containing @RuleArn@, @ErrorCode@, and @ErrorMessage@.
    -- This parameter tells you which automation rules the request didn\'t
    -- retrieve and why.
    unprocessedAutomationRules :: Prelude.Maybe [UnprocessedAutomationRule],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetAutomationRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rules', 'batchGetAutomationRulesResponse_rules' - A list of rule details for the provided rule ARNs.
--
-- 'unprocessedAutomationRules', 'batchGetAutomationRulesResponse_unprocessedAutomationRules' - A list of objects containing @RuleArn@, @ErrorCode@, and @ErrorMessage@.
-- This parameter tells you which automation rules the request didn\'t
-- retrieve and why.
--
-- 'httpStatus', 'batchGetAutomationRulesResponse_httpStatus' - The response's http status code.
newBatchGetAutomationRulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetAutomationRulesResponse
newBatchGetAutomationRulesResponse pHttpStatus_ =
  BatchGetAutomationRulesResponse'
    { rules =
        Prelude.Nothing,
      unprocessedAutomationRules =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of rule details for the provided rule ARNs.
batchGetAutomationRulesResponse_rules :: Lens.Lens' BatchGetAutomationRulesResponse (Prelude.Maybe [AutomationRulesConfig])
batchGetAutomationRulesResponse_rules = Lens.lens (\BatchGetAutomationRulesResponse' {rules} -> rules) (\s@BatchGetAutomationRulesResponse' {} a -> s {rules = a} :: BatchGetAutomationRulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of objects containing @RuleArn@, @ErrorCode@, and @ErrorMessage@.
-- This parameter tells you which automation rules the request didn\'t
-- retrieve and why.
batchGetAutomationRulesResponse_unprocessedAutomationRules :: Lens.Lens' BatchGetAutomationRulesResponse (Prelude.Maybe [UnprocessedAutomationRule])
batchGetAutomationRulesResponse_unprocessedAutomationRules = Lens.lens (\BatchGetAutomationRulesResponse' {unprocessedAutomationRules} -> unprocessedAutomationRules) (\s@BatchGetAutomationRulesResponse' {} a -> s {unprocessedAutomationRules = a} :: BatchGetAutomationRulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetAutomationRulesResponse_httpStatus :: Lens.Lens' BatchGetAutomationRulesResponse Prelude.Int
batchGetAutomationRulesResponse_httpStatus = Lens.lens (\BatchGetAutomationRulesResponse' {httpStatus} -> httpStatus) (\s@BatchGetAutomationRulesResponse' {} a -> s {httpStatus = a} :: BatchGetAutomationRulesResponse)

instance
  Prelude.NFData
    BatchGetAutomationRulesResponse
  where
  rnf BatchGetAutomationRulesResponse' {..} =
    Prelude.rnf rules
      `Prelude.seq` Prelude.rnf unprocessedAutomationRules
      `Prelude.seq` Prelude.rnf httpStatus
