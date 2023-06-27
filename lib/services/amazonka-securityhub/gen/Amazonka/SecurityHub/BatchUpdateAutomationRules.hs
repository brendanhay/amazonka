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
-- Module      : Amazonka.SecurityHub.BatchUpdateAutomationRules
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates one or more automation rules based on rule Amazon Resource Names
-- (ARNs) and input parameters.
module Amazonka.SecurityHub.BatchUpdateAutomationRules
  ( -- * Creating a Request
    BatchUpdateAutomationRules (..),
    newBatchUpdateAutomationRules,

    -- * Request Lenses
    batchUpdateAutomationRules_updateAutomationRulesRequestItems,

    -- * Destructuring the Response
    BatchUpdateAutomationRulesResponse (..),
    newBatchUpdateAutomationRulesResponse,

    -- * Response Lenses
    batchUpdateAutomationRulesResponse_processedAutomationRules,
    batchUpdateAutomationRulesResponse_unprocessedAutomationRules,
    batchUpdateAutomationRulesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newBatchUpdateAutomationRules' smart constructor.
data BatchUpdateAutomationRules = BatchUpdateAutomationRules'
  { -- | An array of ARNs for the rules that are to be updated. Optionally, you
    -- can also include @RuleStatus@ and @RuleOrder@.
    updateAutomationRulesRequestItems :: Prelude.NonEmpty UpdateAutomationRulesRequestItem
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchUpdateAutomationRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'updateAutomationRulesRequestItems', 'batchUpdateAutomationRules_updateAutomationRulesRequestItems' - An array of ARNs for the rules that are to be updated. Optionally, you
-- can also include @RuleStatus@ and @RuleOrder@.
newBatchUpdateAutomationRules ::
  -- | 'updateAutomationRulesRequestItems'
  Prelude.NonEmpty UpdateAutomationRulesRequestItem ->
  BatchUpdateAutomationRules
newBatchUpdateAutomationRules
  pUpdateAutomationRulesRequestItems_ =
    BatchUpdateAutomationRules'
      { updateAutomationRulesRequestItems =
          Lens.coerced
            Lens.# pUpdateAutomationRulesRequestItems_
      }

-- | An array of ARNs for the rules that are to be updated. Optionally, you
-- can also include @RuleStatus@ and @RuleOrder@.
batchUpdateAutomationRules_updateAutomationRulesRequestItems :: Lens.Lens' BatchUpdateAutomationRules (Prelude.NonEmpty UpdateAutomationRulesRequestItem)
batchUpdateAutomationRules_updateAutomationRulesRequestItems = Lens.lens (\BatchUpdateAutomationRules' {updateAutomationRulesRequestItems} -> updateAutomationRulesRequestItems) (\s@BatchUpdateAutomationRules' {} a -> s {updateAutomationRulesRequestItems = a} :: BatchUpdateAutomationRules) Prelude.. Lens.coerced

instance Core.AWSRequest BatchUpdateAutomationRules where
  type
    AWSResponse BatchUpdateAutomationRules =
      BatchUpdateAutomationRulesResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchUpdateAutomationRulesResponse'
            Prelude.<$> (x Data..?> "ProcessedAutomationRules")
            Prelude.<*> ( x
                            Data..?> "UnprocessedAutomationRules"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchUpdateAutomationRules where
  hashWithSalt _salt BatchUpdateAutomationRules' {..} =
    _salt
      `Prelude.hashWithSalt` updateAutomationRulesRequestItems

instance Prelude.NFData BatchUpdateAutomationRules where
  rnf BatchUpdateAutomationRules' {..} =
    Prelude.rnf updateAutomationRulesRequestItems

instance Data.ToHeaders BatchUpdateAutomationRules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchUpdateAutomationRules where
  toJSON BatchUpdateAutomationRules' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "UpdateAutomationRulesRequestItems"
                  Data..= updateAutomationRulesRequestItems
              )
          ]
      )

instance Data.ToPath BatchUpdateAutomationRules where
  toPath = Prelude.const "/automationrules/update"

instance Data.ToQuery BatchUpdateAutomationRules where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchUpdateAutomationRulesResponse' smart constructor.
data BatchUpdateAutomationRulesResponse = BatchUpdateAutomationRulesResponse'
  { -- | A list of properly processed rule ARNs.
    processedAutomationRules :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of objects containing @RuleArn@, @ErrorCode@, and @ErrorMessage@.
    -- This parameter tells you which automation rules the request didn\'t
    -- update and why.
    unprocessedAutomationRules :: Prelude.Maybe [UnprocessedAutomationRule],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchUpdateAutomationRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'processedAutomationRules', 'batchUpdateAutomationRulesResponse_processedAutomationRules' - A list of properly processed rule ARNs.
--
-- 'unprocessedAutomationRules', 'batchUpdateAutomationRulesResponse_unprocessedAutomationRules' - A list of objects containing @RuleArn@, @ErrorCode@, and @ErrorMessage@.
-- This parameter tells you which automation rules the request didn\'t
-- update and why.
--
-- 'httpStatus', 'batchUpdateAutomationRulesResponse_httpStatus' - The response's http status code.
newBatchUpdateAutomationRulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchUpdateAutomationRulesResponse
newBatchUpdateAutomationRulesResponse pHttpStatus_ =
  BatchUpdateAutomationRulesResponse'
    { processedAutomationRules =
        Prelude.Nothing,
      unprocessedAutomationRules =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of properly processed rule ARNs.
batchUpdateAutomationRulesResponse_processedAutomationRules :: Lens.Lens' BatchUpdateAutomationRulesResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
batchUpdateAutomationRulesResponse_processedAutomationRules = Lens.lens (\BatchUpdateAutomationRulesResponse' {processedAutomationRules} -> processedAutomationRules) (\s@BatchUpdateAutomationRulesResponse' {} a -> s {processedAutomationRules = a} :: BatchUpdateAutomationRulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of objects containing @RuleArn@, @ErrorCode@, and @ErrorMessage@.
-- This parameter tells you which automation rules the request didn\'t
-- update and why.
batchUpdateAutomationRulesResponse_unprocessedAutomationRules :: Lens.Lens' BatchUpdateAutomationRulesResponse (Prelude.Maybe [UnprocessedAutomationRule])
batchUpdateAutomationRulesResponse_unprocessedAutomationRules = Lens.lens (\BatchUpdateAutomationRulesResponse' {unprocessedAutomationRules} -> unprocessedAutomationRules) (\s@BatchUpdateAutomationRulesResponse' {} a -> s {unprocessedAutomationRules = a} :: BatchUpdateAutomationRulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchUpdateAutomationRulesResponse_httpStatus :: Lens.Lens' BatchUpdateAutomationRulesResponse Prelude.Int
batchUpdateAutomationRulesResponse_httpStatus = Lens.lens (\BatchUpdateAutomationRulesResponse' {httpStatus} -> httpStatus) (\s@BatchUpdateAutomationRulesResponse' {} a -> s {httpStatus = a} :: BatchUpdateAutomationRulesResponse)

instance
  Prelude.NFData
    BatchUpdateAutomationRulesResponse
  where
  rnf BatchUpdateAutomationRulesResponse' {..} =
    Prelude.rnf processedAutomationRules
      `Prelude.seq` Prelude.rnf unprocessedAutomationRules
      `Prelude.seq` Prelude.rnf httpStatus
