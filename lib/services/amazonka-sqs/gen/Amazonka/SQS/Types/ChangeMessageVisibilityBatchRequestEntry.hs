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
-- Module      : Amazonka.SQS.Types.ChangeMessageVisibilityBatchRequestEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SQS.Types.ChangeMessageVisibilityBatchRequestEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Encloses a receipt handle and an entry ID for each message in
-- @ @@ChangeMessageVisibilityBatch@@.@
--
-- /See:/ 'newChangeMessageVisibilityBatchRequestEntry' smart constructor.
data ChangeMessageVisibilityBatchRequestEntry = ChangeMessageVisibilityBatchRequestEntry'
  { -- | The new value (in seconds) for the message\'s visibility timeout.
    visibilityTimeout :: Prelude.Maybe Prelude.Int,
    -- | An identifier for this particular receipt handle used to communicate the
    -- result.
    --
    -- The @Id@s of a batch request need to be unique within a request.
    --
    -- This identifier can have up to 80 characters. The following characters
    -- are accepted: alphanumeric characters, hyphens(-), and underscores (_).
    id :: Prelude.Text,
    -- | A receipt handle.
    receiptHandle :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'receiptHandle'
  Prelude.Text ->
  ChangeMessageVisibilityBatchRequestEntry
newChangeMessageVisibilityBatchRequestEntry
  pId_
  pReceiptHandle_ =
    ChangeMessageVisibilityBatchRequestEntry'
      { visibilityTimeout =
          Prelude.Nothing,
        id = pId_,
        receiptHandle = pReceiptHandle_
      }

-- | The new value (in seconds) for the message\'s visibility timeout.
changeMessageVisibilityBatchRequestEntry_visibilityTimeout :: Lens.Lens' ChangeMessageVisibilityBatchRequestEntry (Prelude.Maybe Prelude.Int)
changeMessageVisibilityBatchRequestEntry_visibilityTimeout = Lens.lens (\ChangeMessageVisibilityBatchRequestEntry' {visibilityTimeout} -> visibilityTimeout) (\s@ChangeMessageVisibilityBatchRequestEntry' {} a -> s {visibilityTimeout = a} :: ChangeMessageVisibilityBatchRequestEntry)

-- | An identifier for this particular receipt handle used to communicate the
-- result.
--
-- The @Id@s of a batch request need to be unique within a request.
--
-- This identifier can have up to 80 characters. The following characters
-- are accepted: alphanumeric characters, hyphens(-), and underscores (_).
changeMessageVisibilityBatchRequestEntry_id :: Lens.Lens' ChangeMessageVisibilityBatchRequestEntry Prelude.Text
changeMessageVisibilityBatchRequestEntry_id = Lens.lens (\ChangeMessageVisibilityBatchRequestEntry' {id} -> id) (\s@ChangeMessageVisibilityBatchRequestEntry' {} a -> s {id = a} :: ChangeMessageVisibilityBatchRequestEntry)

-- | A receipt handle.
changeMessageVisibilityBatchRequestEntry_receiptHandle :: Lens.Lens' ChangeMessageVisibilityBatchRequestEntry Prelude.Text
changeMessageVisibilityBatchRequestEntry_receiptHandle = Lens.lens (\ChangeMessageVisibilityBatchRequestEntry' {receiptHandle} -> receiptHandle) (\s@ChangeMessageVisibilityBatchRequestEntry' {} a -> s {receiptHandle = a} :: ChangeMessageVisibilityBatchRequestEntry)

instance
  Prelude.Hashable
    ChangeMessageVisibilityBatchRequestEntry
  where
  hashWithSalt
    _salt
    ChangeMessageVisibilityBatchRequestEntry' {..} =
      _salt
        `Prelude.hashWithSalt` visibilityTimeout
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` receiptHandle

instance
  Prelude.NFData
    ChangeMessageVisibilityBatchRequestEntry
  where
  rnf ChangeMessageVisibilityBatchRequestEntry' {..} =
    Prelude.rnf visibilityTimeout
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf receiptHandle

instance
  Data.ToQuery
    ChangeMessageVisibilityBatchRequestEntry
  where
  toQuery ChangeMessageVisibilityBatchRequestEntry' {..} =
    Prelude.mconcat
      [ "VisibilityTimeout" Data.=: visibilityTimeout,
        "Id" Data.=: id,
        "ReceiptHandle" Data.=: receiptHandle
      ]
