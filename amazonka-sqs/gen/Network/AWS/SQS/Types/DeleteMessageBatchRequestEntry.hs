{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SQS.Types.DeleteMessageBatchRequestEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SQS.Types.DeleteMessageBatchRequestEntry where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Encloses a receipt handle and an identifier for it.
--
-- /See:/ 'newDeleteMessageBatchRequestEntry' smart constructor.
data DeleteMessageBatchRequestEntry = DeleteMessageBatchRequestEntry'
  { -- | An identifier for this particular receipt handle. This is used to
    -- communicate the result.
    --
    -- The @Id@s of a batch request need to be unique within a request.
    --
    -- This identifier can have up to 80 characters. The following characters
    -- are accepted: alphanumeric characters, hyphens(-), and underscores (_).
    id :: Prelude.Text,
    -- | A receipt handle.
    receiptHandle :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteMessageBatchRequestEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteMessageBatchRequestEntry_id' - An identifier for this particular receipt handle. This is used to
-- communicate the result.
--
-- The @Id@s of a batch request need to be unique within a request.
--
-- This identifier can have up to 80 characters. The following characters
-- are accepted: alphanumeric characters, hyphens(-), and underscores (_).
--
-- 'receiptHandle', 'deleteMessageBatchRequestEntry_receiptHandle' - A receipt handle.
newDeleteMessageBatchRequestEntry ::
  -- | 'id'
  Prelude.Text ->
  -- | 'receiptHandle'
  Prelude.Text ->
  DeleteMessageBatchRequestEntry
newDeleteMessageBatchRequestEntry
  pId_
  pReceiptHandle_ =
    DeleteMessageBatchRequestEntry'
      { id = pId_,
        receiptHandle = pReceiptHandle_
      }

-- | An identifier for this particular receipt handle. This is used to
-- communicate the result.
--
-- The @Id@s of a batch request need to be unique within a request.
--
-- This identifier can have up to 80 characters. The following characters
-- are accepted: alphanumeric characters, hyphens(-), and underscores (_).
deleteMessageBatchRequestEntry_id :: Lens.Lens' DeleteMessageBatchRequestEntry Prelude.Text
deleteMessageBatchRequestEntry_id = Lens.lens (\DeleteMessageBatchRequestEntry' {id} -> id) (\s@DeleteMessageBatchRequestEntry' {} a -> s {id = a} :: DeleteMessageBatchRequestEntry)

-- | A receipt handle.
deleteMessageBatchRequestEntry_receiptHandle :: Lens.Lens' DeleteMessageBatchRequestEntry Prelude.Text
deleteMessageBatchRequestEntry_receiptHandle = Lens.lens (\DeleteMessageBatchRequestEntry' {receiptHandle} -> receiptHandle) (\s@DeleteMessageBatchRequestEntry' {} a -> s {receiptHandle = a} :: DeleteMessageBatchRequestEntry)

instance
  Prelude.Hashable
    DeleteMessageBatchRequestEntry

instance
  Prelude.NFData
    DeleteMessageBatchRequestEntry

instance
  Prelude.ToQuery
    DeleteMessageBatchRequestEntry
  where
  toQuery DeleteMessageBatchRequestEntry' {..} =
    Prelude.mconcat
      [ "Id" Prelude.=: id,
        "ReceiptHandle" Prelude.=: receiptHandle
      ]
