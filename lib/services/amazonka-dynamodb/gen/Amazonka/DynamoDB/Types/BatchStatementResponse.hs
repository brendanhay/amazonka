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
-- Module      : Amazonka.DynamoDB.Types.BatchStatementResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.BatchStatementResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.BatchStatementError
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | A PartiQL batch statement response..
--
-- /See:/ 'newBatchStatementResponse' smart constructor.
data BatchStatementResponse = BatchStatementResponse'
  { -- | The error associated with a failed PartiQL batch statement.
    error :: Prelude.Maybe BatchStatementError,
    -- | A DynamoDB item associated with a BatchStatementResponse
    item :: Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue),
    -- | The table name associated with a failed PartiQL batch statement.
    tableName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchStatementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'batchStatementResponse_error' - The error associated with a failed PartiQL batch statement.
--
-- 'item', 'batchStatementResponse_item' - A DynamoDB item associated with a BatchStatementResponse
--
-- 'tableName', 'batchStatementResponse_tableName' - The table name associated with a failed PartiQL batch statement.
newBatchStatementResponse ::
  BatchStatementResponse
newBatchStatementResponse =
  BatchStatementResponse'
    { error = Prelude.Nothing,
      item = Prelude.Nothing,
      tableName = Prelude.Nothing
    }

-- | The error associated with a failed PartiQL batch statement.
batchStatementResponse_error :: Lens.Lens' BatchStatementResponse (Prelude.Maybe BatchStatementError)
batchStatementResponse_error = Lens.lens (\BatchStatementResponse' {error} -> error) (\s@BatchStatementResponse' {} a -> s {error = a} :: BatchStatementResponse)

-- | A DynamoDB item associated with a BatchStatementResponse
batchStatementResponse_item :: Lens.Lens' BatchStatementResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue))
batchStatementResponse_item = Lens.lens (\BatchStatementResponse' {item} -> item) (\s@BatchStatementResponse' {} a -> s {item = a} :: BatchStatementResponse) Prelude.. Lens.mapping Lens.coerced

-- | The table name associated with a failed PartiQL batch statement.
batchStatementResponse_tableName :: Lens.Lens' BatchStatementResponse (Prelude.Maybe Prelude.Text)
batchStatementResponse_tableName = Lens.lens (\BatchStatementResponse' {tableName} -> tableName) (\s@BatchStatementResponse' {} a -> s {tableName = a} :: BatchStatementResponse)

instance Data.FromJSON BatchStatementResponse where
  parseJSON =
    Data.withObject
      "BatchStatementResponse"
      ( \x ->
          BatchStatementResponse'
            Prelude.<$> (x Data..:? "Error")
            Prelude.<*> (x Data..:? "Item" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "TableName")
      )

instance Prelude.Hashable BatchStatementResponse where
  hashWithSalt _salt BatchStatementResponse' {..} =
    _salt `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` item
      `Prelude.hashWithSalt` tableName

instance Prelude.NFData BatchStatementResponse where
  rnf BatchStatementResponse' {..} =
    Prelude.rnf error
      `Prelude.seq` Prelude.rnf item
      `Prelude.seq` Prelude.rnf tableName
