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
-- Module      : Amazonka.SecurityHub.BatchDeleteAutomationRules
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more automation rules.
module Amazonka.SecurityHub.BatchDeleteAutomationRules
  ( -- * Creating a Request
    BatchDeleteAutomationRules (..),
    newBatchDeleteAutomationRules,

    -- * Request Lenses
    batchDeleteAutomationRules_automationRulesArns,

    -- * Destructuring the Response
    BatchDeleteAutomationRulesResponse (..),
    newBatchDeleteAutomationRulesResponse,

    -- * Response Lenses
    batchDeleteAutomationRulesResponse_processedAutomationRules,
    batchDeleteAutomationRulesResponse_unprocessedAutomationRules,
    batchDeleteAutomationRulesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newBatchDeleteAutomationRules' smart constructor.
data BatchDeleteAutomationRules = BatchDeleteAutomationRules'
  { -- | A list of Amazon Resource Names (ARNs) for the rules that are to be
    -- deleted.
    automationRulesArns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteAutomationRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'automationRulesArns', 'batchDeleteAutomationRules_automationRulesArns' - A list of Amazon Resource Names (ARNs) for the rules that are to be
-- deleted.
newBatchDeleteAutomationRules ::
  -- | 'automationRulesArns'
  Prelude.NonEmpty Prelude.Text ->
  BatchDeleteAutomationRules
newBatchDeleteAutomationRules pAutomationRulesArns_ =
  BatchDeleteAutomationRules'
    { automationRulesArns =
        Lens.coerced Lens.# pAutomationRulesArns_
    }

-- | A list of Amazon Resource Names (ARNs) for the rules that are to be
-- deleted.
batchDeleteAutomationRules_automationRulesArns :: Lens.Lens' BatchDeleteAutomationRules (Prelude.NonEmpty Prelude.Text)
batchDeleteAutomationRules_automationRulesArns = Lens.lens (\BatchDeleteAutomationRules' {automationRulesArns} -> automationRulesArns) (\s@BatchDeleteAutomationRules' {} a -> s {automationRulesArns = a} :: BatchDeleteAutomationRules) Prelude.. Lens.coerced

instance Core.AWSRequest BatchDeleteAutomationRules where
  type
    AWSResponse BatchDeleteAutomationRules =
      BatchDeleteAutomationRulesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDeleteAutomationRulesResponse'
            Prelude.<$> (x Data..?> "ProcessedAutomationRules")
            Prelude.<*> ( x
                            Data..?> "UnprocessedAutomationRules"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchDeleteAutomationRules where
  hashWithSalt _salt BatchDeleteAutomationRules' {..} =
    _salt `Prelude.hashWithSalt` automationRulesArns

instance Prelude.NFData BatchDeleteAutomationRules where
  rnf BatchDeleteAutomationRules' {..} =
    Prelude.rnf automationRulesArns

instance Data.ToHeaders BatchDeleteAutomationRules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchDeleteAutomationRules where
  toJSON BatchDeleteAutomationRules' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AutomationRulesArns" Data..= automationRulesArns)
          ]
      )

instance Data.ToPath BatchDeleteAutomationRules where
  toPath = Prelude.const "/automationrules/delete"

instance Data.ToQuery BatchDeleteAutomationRules where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchDeleteAutomationRulesResponse' smart constructor.
data BatchDeleteAutomationRulesResponse = BatchDeleteAutomationRulesResponse'
  { -- | A list of properly processed rule ARNs.
    processedAutomationRules :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of objects containing @RuleArn@, @ErrorCode@, and @ErrorMessage@.
    -- This parameter tells you which automation rules the request didn\'t
    -- delete and why.
    unprocessedAutomationRules :: Prelude.Maybe [UnprocessedAutomationRule],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteAutomationRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'processedAutomationRules', 'batchDeleteAutomationRulesResponse_processedAutomationRules' - A list of properly processed rule ARNs.
--
-- 'unprocessedAutomationRules', 'batchDeleteAutomationRulesResponse_unprocessedAutomationRules' - A list of objects containing @RuleArn@, @ErrorCode@, and @ErrorMessage@.
-- This parameter tells you which automation rules the request didn\'t
-- delete and why.
--
-- 'httpStatus', 'batchDeleteAutomationRulesResponse_httpStatus' - The response's http status code.
newBatchDeleteAutomationRulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchDeleteAutomationRulesResponse
newBatchDeleteAutomationRulesResponse pHttpStatus_ =
  BatchDeleteAutomationRulesResponse'
    { processedAutomationRules =
        Prelude.Nothing,
      unprocessedAutomationRules =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of properly processed rule ARNs.
batchDeleteAutomationRulesResponse_processedAutomationRules :: Lens.Lens' BatchDeleteAutomationRulesResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
batchDeleteAutomationRulesResponse_processedAutomationRules = Lens.lens (\BatchDeleteAutomationRulesResponse' {processedAutomationRules} -> processedAutomationRules) (\s@BatchDeleteAutomationRulesResponse' {} a -> s {processedAutomationRules = a} :: BatchDeleteAutomationRulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of objects containing @RuleArn@, @ErrorCode@, and @ErrorMessage@.
-- This parameter tells you which automation rules the request didn\'t
-- delete and why.
batchDeleteAutomationRulesResponse_unprocessedAutomationRules :: Lens.Lens' BatchDeleteAutomationRulesResponse (Prelude.Maybe [UnprocessedAutomationRule])
batchDeleteAutomationRulesResponse_unprocessedAutomationRules = Lens.lens (\BatchDeleteAutomationRulesResponse' {unprocessedAutomationRules} -> unprocessedAutomationRules) (\s@BatchDeleteAutomationRulesResponse' {} a -> s {unprocessedAutomationRules = a} :: BatchDeleteAutomationRulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchDeleteAutomationRulesResponse_httpStatus :: Lens.Lens' BatchDeleteAutomationRulesResponse Prelude.Int
batchDeleteAutomationRulesResponse_httpStatus = Lens.lens (\BatchDeleteAutomationRulesResponse' {httpStatus} -> httpStatus) (\s@BatchDeleteAutomationRulesResponse' {} a -> s {httpStatus = a} :: BatchDeleteAutomationRulesResponse)

instance
  Prelude.NFData
    BatchDeleteAutomationRulesResponse
  where
  rnf BatchDeleteAutomationRulesResponse' {..} =
    Prelude.rnf processedAutomationRules
      `Prelude.seq` Prelude.rnf unprocessedAutomationRules
      `Prelude.seq` Prelude.rnf httpStatus
