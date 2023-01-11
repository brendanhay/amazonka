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
-- Module      : Amazonka.Glue.BatchGetTriggers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of resource metadata for a given list of trigger names.
-- After calling the @ListTriggers@ operation, you can call this operation
-- to access the data to which you have been granted permissions. This
-- operation supports all IAM permissions, including permission conditions
-- that uses tags.
module Amazonka.Glue.BatchGetTriggers
  ( -- * Creating a Request
    BatchGetTriggers (..),
    newBatchGetTriggers,

    -- * Request Lenses
    batchGetTriggers_triggerNames,

    -- * Destructuring the Response
    BatchGetTriggersResponse (..),
    newBatchGetTriggersResponse,

    -- * Response Lenses
    batchGetTriggersResponse_triggers,
    batchGetTriggersResponse_triggersNotFound,
    batchGetTriggersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchGetTriggers' smart constructor.
data BatchGetTriggers = BatchGetTriggers'
  { -- | A list of trigger names, which may be the names returned from the
    -- @ListTriggers@ operation.
    triggerNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetTriggers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'triggerNames', 'batchGetTriggers_triggerNames' - A list of trigger names, which may be the names returned from the
-- @ListTriggers@ operation.
newBatchGetTriggers ::
  BatchGetTriggers
newBatchGetTriggers =
  BatchGetTriggers' {triggerNames = Prelude.mempty}

-- | A list of trigger names, which may be the names returned from the
-- @ListTriggers@ operation.
batchGetTriggers_triggerNames :: Lens.Lens' BatchGetTriggers [Prelude.Text]
batchGetTriggers_triggerNames = Lens.lens (\BatchGetTriggers' {triggerNames} -> triggerNames) (\s@BatchGetTriggers' {} a -> s {triggerNames = a} :: BatchGetTriggers) Prelude.. Lens.coerced

instance Core.AWSRequest BatchGetTriggers where
  type
    AWSResponse BatchGetTriggers =
      BatchGetTriggersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetTriggersResponse'
            Prelude.<$> (x Data..?> "Triggers" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Data..?> "TriggersNotFound"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetTriggers where
  hashWithSalt _salt BatchGetTriggers' {..} =
    _salt `Prelude.hashWithSalt` triggerNames

instance Prelude.NFData BatchGetTriggers where
  rnf BatchGetTriggers' {..} = Prelude.rnf triggerNames

instance Data.ToHeaders BatchGetTriggers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.BatchGetTriggers" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchGetTriggers where
  toJSON BatchGetTriggers' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("TriggerNames" Data..= triggerNames)]
      )

instance Data.ToPath BatchGetTriggers where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchGetTriggers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetTriggersResponse' smart constructor.
data BatchGetTriggersResponse = BatchGetTriggersResponse'
  { -- | A list of trigger definitions.
    triggers :: Prelude.Maybe [Trigger],
    -- | A list of names of triggers not found.
    triggersNotFound :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetTriggersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'triggers', 'batchGetTriggersResponse_triggers' - A list of trigger definitions.
--
-- 'triggersNotFound', 'batchGetTriggersResponse_triggersNotFound' - A list of names of triggers not found.
--
-- 'httpStatus', 'batchGetTriggersResponse_httpStatus' - The response's http status code.
newBatchGetTriggersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetTriggersResponse
newBatchGetTriggersResponse pHttpStatus_ =
  BatchGetTriggersResponse'
    { triggers =
        Prelude.Nothing,
      triggersNotFound = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of trigger definitions.
batchGetTriggersResponse_triggers :: Lens.Lens' BatchGetTriggersResponse (Prelude.Maybe [Trigger])
batchGetTriggersResponse_triggers = Lens.lens (\BatchGetTriggersResponse' {triggers} -> triggers) (\s@BatchGetTriggersResponse' {} a -> s {triggers = a} :: BatchGetTriggersResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of names of triggers not found.
batchGetTriggersResponse_triggersNotFound :: Lens.Lens' BatchGetTriggersResponse (Prelude.Maybe [Prelude.Text])
batchGetTriggersResponse_triggersNotFound = Lens.lens (\BatchGetTriggersResponse' {triggersNotFound} -> triggersNotFound) (\s@BatchGetTriggersResponse' {} a -> s {triggersNotFound = a} :: BatchGetTriggersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetTriggersResponse_httpStatus :: Lens.Lens' BatchGetTriggersResponse Prelude.Int
batchGetTriggersResponse_httpStatus = Lens.lens (\BatchGetTriggersResponse' {httpStatus} -> httpStatus) (\s@BatchGetTriggersResponse' {} a -> s {httpStatus = a} :: BatchGetTriggersResponse)

instance Prelude.NFData BatchGetTriggersResponse where
  rnf BatchGetTriggersResponse' {..} =
    Prelude.rnf triggers
      `Prelude.seq` Prelude.rnf triggersNotFound
      `Prelude.seq` Prelude.rnf httpStatus
