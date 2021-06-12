{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.Types.ChangeMessageVisibilityBatchRequestEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SQS.Types.ChangeMessageVisibilityBatchRequestEntry where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Encloses a receipt handle and an entry id for each message in
-- @ ChangeMessageVisibilityBatch.@
--
-- All of the following list parameters must be prefixed with
-- @ChangeMessageVisibilityBatchRequestEntry.n@, where @n@ is an integer
-- value starting with @1@. For example, a parameter list for this action
-- might look like this:
--
-- @&ChangeMessageVisibilityBatchRequestEntry.1.Id=change_visibility_msg_2@
--
-- @&ChangeMessageVisibilityBatchRequestEntry.1.ReceiptHandle=your_receipt_handle@
--
-- @&ChangeMessageVisibilityBatchRequestEntry.1.VisibilityTimeout=45@
--
-- /See:/ 'newChangeMessageVisibilityBatchRequestEntry' smart constructor.
data ChangeMessageVisibilityBatchRequestEntry = ChangeMessageVisibilityBatchRequestEntry'
  { -- | The new value (in seconds) for the message\'s visibility timeout.
    visibilityTimeout :: Core.Maybe Core.Int,
    -- | An identifier for this particular receipt handle used to communicate the
    -- result.
    --
    -- The @Id@s of a batch request need to be unique within a request.
    --
    -- This identifier can have up to 80 characters. The following characters
    -- are accepted: alphanumeric characters, hyphens(-), and underscores (_).
    id :: Core.Text,
    -- | A receipt handle.
    receiptHandle :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ChangeMessageVisibilityBatchRequestEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'visibilityTimeout', 'changeMessageVisibilityBatchRequestEntry_visibilityTimeout' - The new value (in seconds) for the message\'s visibility timeout.
--
-- 'id', 'changeMessageVisibilityBatchRequestEntry_id' - An identifier for this particular receipt handle used to communicate the
-- result.
--
-- The @Id@s of a batch request need to be unique within a request.
--
-- This identifier can have up to 80 characters. The following characters
-- are accepted: alphanumeric characters, hyphens(-), and underscores (_).
--
-- 'receiptHandle', 'changeMessageVisibilityBatchRequestEntry_receiptHandle' - A receipt handle.
newChangeMessageVisibilityBatchRequestEntry ::
  -- | 'id'
  Core.Text ->
  -- | 'receiptHandle'
  Core.Text ->
  ChangeMessageVisibilityBatchRequestEntry
newChangeMessageVisibilityBatchRequestEntry
  pId_
  pReceiptHandle_ =
    ChangeMessageVisibilityBatchRequestEntry'
      { visibilityTimeout =
          Core.Nothing,
        id = pId_,
        receiptHandle = pReceiptHandle_
      }

-- | The new value (in seconds) for the message\'s visibility timeout.
changeMessageVisibilityBatchRequestEntry_visibilityTimeout :: Lens.Lens' ChangeMessageVisibilityBatchRequestEntry (Core.Maybe Core.Int)
changeMessageVisibilityBatchRequestEntry_visibilityTimeout = Lens.lens (\ChangeMessageVisibilityBatchRequestEntry' {visibilityTimeout} -> visibilityTimeout) (\s@ChangeMessageVisibilityBatchRequestEntry' {} a -> s {visibilityTimeout = a} :: ChangeMessageVisibilityBatchRequestEntry)

-- | An identifier for this particular receipt handle used to communicate the
-- result.
--
-- The @Id@s of a batch request need to be unique within a request.
--
-- This identifier can have up to 80 characters. The following characters
-- are accepted: alphanumeric characters, hyphens(-), and underscores (_).
changeMessageVisibilityBatchRequestEntry_id :: Lens.Lens' ChangeMessageVisibilityBatchRequestEntry Core.Text
changeMessageVisibilityBatchRequestEntry_id = Lens.lens (\ChangeMessageVisibilityBatchRequestEntry' {id} -> id) (\s@ChangeMessageVisibilityBatchRequestEntry' {} a -> s {id = a} :: ChangeMessageVisibilityBatchRequestEntry)

-- | A receipt handle.
changeMessageVisibilityBatchRequestEntry_receiptHandle :: Lens.Lens' ChangeMessageVisibilityBatchRequestEntry Core.Text
changeMessageVisibilityBatchRequestEntry_receiptHandle = Lens.lens (\ChangeMessageVisibilityBatchRequestEntry' {receiptHandle} -> receiptHandle) (\s@ChangeMessageVisibilityBatchRequestEntry' {} a -> s {receiptHandle = a} :: ChangeMessageVisibilityBatchRequestEntry)

instance
  Core.Hashable
    ChangeMessageVisibilityBatchRequestEntry

instance
  Core.NFData
    ChangeMessageVisibilityBatchRequestEntry

instance
  Core.ToQuery
    ChangeMessageVisibilityBatchRequestEntry
  where
  toQuery ChangeMessageVisibilityBatchRequestEntry' {..} =
    Core.mconcat
      [ "VisibilityTimeout" Core.=: visibilityTimeout,
        "Id" Core.=: id,
        "ReceiptHandle" Core.=: receiptHandle
      ]
