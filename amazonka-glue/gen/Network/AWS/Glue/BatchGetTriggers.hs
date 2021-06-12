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
-- Module      : Network.AWS.Glue.BatchGetTriggers
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.Glue.BatchGetTriggers
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

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchGetTriggers' smart constructor.
data BatchGetTriggers = BatchGetTriggers'
  { -- | A list of trigger names, which may be the names returned from the
    -- @ListTriggers@ operation.
    triggerNames :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  BatchGetTriggers' {triggerNames = Core.mempty}

-- | A list of trigger names, which may be the names returned from the
-- @ListTriggers@ operation.
batchGetTriggers_triggerNames :: Lens.Lens' BatchGetTriggers [Core.Text]
batchGetTriggers_triggerNames = Lens.lens (\BatchGetTriggers' {triggerNames} -> triggerNames) (\s@BatchGetTriggers' {} a -> s {triggerNames = a} :: BatchGetTriggers) Core.. Lens._Coerce

instance Core.AWSRequest BatchGetTriggers where
  type
    AWSResponse BatchGetTriggers =
      BatchGetTriggersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetTriggersResponse'
            Core.<$> (x Core..?> "Triggers" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "TriggersNotFound" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable BatchGetTriggers

instance Core.NFData BatchGetTriggers

instance Core.ToHeaders BatchGetTriggers where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.BatchGetTriggers" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON BatchGetTriggers where
  toJSON BatchGetTriggers' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("TriggerNames" Core..= triggerNames)]
      )

instance Core.ToPath BatchGetTriggers where
  toPath = Core.const "/"

instance Core.ToQuery BatchGetTriggers where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newBatchGetTriggersResponse' smart constructor.
data BatchGetTriggersResponse = BatchGetTriggersResponse'
  { -- | A list of trigger definitions.
    triggers :: Core.Maybe [Trigger],
    -- | A list of names of triggers not found.
    triggersNotFound :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  BatchGetTriggersResponse
newBatchGetTriggersResponse pHttpStatus_ =
  BatchGetTriggersResponse'
    { triggers = Core.Nothing,
      triggersNotFound = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of trigger definitions.
batchGetTriggersResponse_triggers :: Lens.Lens' BatchGetTriggersResponse (Core.Maybe [Trigger])
batchGetTriggersResponse_triggers = Lens.lens (\BatchGetTriggersResponse' {triggers} -> triggers) (\s@BatchGetTriggersResponse' {} a -> s {triggers = a} :: BatchGetTriggersResponse) Core.. Lens.mapping Lens._Coerce

-- | A list of names of triggers not found.
batchGetTriggersResponse_triggersNotFound :: Lens.Lens' BatchGetTriggersResponse (Core.Maybe [Core.Text])
batchGetTriggersResponse_triggersNotFound = Lens.lens (\BatchGetTriggersResponse' {triggersNotFound} -> triggersNotFound) (\s@BatchGetTriggersResponse' {} a -> s {triggersNotFound = a} :: BatchGetTriggersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchGetTriggersResponse_httpStatus :: Lens.Lens' BatchGetTriggersResponse Core.Int
batchGetTriggersResponse_httpStatus = Lens.lens (\BatchGetTriggersResponse' {httpStatus} -> httpStatus) (\s@BatchGetTriggersResponse' {} a -> s {httpStatus = a} :: BatchGetTriggersResponse)

instance Core.NFData BatchGetTriggersResponse
