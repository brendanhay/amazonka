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
-- Module      : Amazonka.HoneyCode.Types.FailedBatchItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HoneyCode.Types.FailedBatchItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A single item in a batch that failed to perform the intended action
-- because of an error preventing it from succeeding.
--
-- /See:/ 'newFailedBatchItem' smart constructor.
data FailedBatchItem = FailedBatchItem'
  { -- | The id of the batch item that failed. This is the batch item id for the
    -- BatchCreateTableRows and BatchUpsertTableRows operations and the row id
    -- for the BatchUpdateTableRows and BatchDeleteTableRows operations.
    id :: Prelude.Text,
    -- | The error message that indicates why the batch item failed.
    errorMessage :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailedBatchItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'failedBatchItem_id' - The id of the batch item that failed. This is the batch item id for the
-- BatchCreateTableRows and BatchUpsertTableRows operations and the row id
-- for the BatchUpdateTableRows and BatchDeleteTableRows operations.
--
-- 'errorMessage', 'failedBatchItem_errorMessage' - The error message that indicates why the batch item failed.
newFailedBatchItem ::
  -- | 'id'
  Prelude.Text ->
  -- | 'errorMessage'
  Prelude.Text ->
  FailedBatchItem
newFailedBatchItem pId_ pErrorMessage_ =
  FailedBatchItem'
    { id = pId_,
      errorMessage = pErrorMessage_
    }

-- | The id of the batch item that failed. This is the batch item id for the
-- BatchCreateTableRows and BatchUpsertTableRows operations and the row id
-- for the BatchUpdateTableRows and BatchDeleteTableRows operations.
failedBatchItem_id :: Lens.Lens' FailedBatchItem Prelude.Text
failedBatchItem_id = Lens.lens (\FailedBatchItem' {id} -> id) (\s@FailedBatchItem' {} a -> s {id = a} :: FailedBatchItem)

-- | The error message that indicates why the batch item failed.
failedBatchItem_errorMessage :: Lens.Lens' FailedBatchItem Prelude.Text
failedBatchItem_errorMessage = Lens.lens (\FailedBatchItem' {errorMessage} -> errorMessage) (\s@FailedBatchItem' {} a -> s {errorMessage = a} :: FailedBatchItem)

instance Core.FromJSON FailedBatchItem where
  parseJSON =
    Core.withObject
      "FailedBatchItem"
      ( \x ->
          FailedBatchItem'
            Prelude.<$> (x Core..: "id")
            Prelude.<*> (x Core..: "errorMessage")
      )

instance Prelude.Hashable FailedBatchItem where
  hashWithSalt _salt FailedBatchItem' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` errorMessage

instance Prelude.NFData FailedBatchItem where
  rnf FailedBatchItem' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf errorMessage
