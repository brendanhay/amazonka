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
-- Module      : Network.AWS.DynamoDB.Types.BatchStatementResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.BatchStatementResponse where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.AttributeValue
import Network.AWS.DynamoDB.Types.BatchStatementError
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A PartiQL batch statement response..
--
-- /See:/ 'newBatchStatementResponse' smart constructor.
data BatchStatementResponse = BatchStatementResponse'
  { -- | The table name associated with a failed PartiQL batch statement.
    tableName :: Prelude.Maybe Prelude.Text,
    -- | A DynamoDB item associated with a BatchStatementResponse
    item :: Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue),
    -- | The error associated with a failed PartiQL batch statement.
    error :: Prelude.Maybe BatchStatementError
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
-- 'tableName', 'batchStatementResponse_tableName' - The table name associated with a failed PartiQL batch statement.
--
-- 'item', 'batchStatementResponse_item' - A DynamoDB item associated with a BatchStatementResponse
--
-- 'error', 'batchStatementResponse_error' - The error associated with a failed PartiQL batch statement.
newBatchStatementResponse ::
  BatchStatementResponse
newBatchStatementResponse =
  BatchStatementResponse'
    { tableName =
        Prelude.Nothing,
      item = Prelude.Nothing,
      error = Prelude.Nothing
    }

-- | The table name associated with a failed PartiQL batch statement.
batchStatementResponse_tableName :: Lens.Lens' BatchStatementResponse (Prelude.Maybe Prelude.Text)
batchStatementResponse_tableName = Lens.lens (\BatchStatementResponse' {tableName} -> tableName) (\s@BatchStatementResponse' {} a -> s {tableName = a} :: BatchStatementResponse)

-- | A DynamoDB item associated with a BatchStatementResponse
batchStatementResponse_item :: Lens.Lens' BatchStatementResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text AttributeValue))
batchStatementResponse_item = Lens.lens (\BatchStatementResponse' {item} -> item) (\s@BatchStatementResponse' {} a -> s {item = a} :: BatchStatementResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The error associated with a failed PartiQL batch statement.
batchStatementResponse_error :: Lens.Lens' BatchStatementResponse (Prelude.Maybe BatchStatementError)
batchStatementResponse_error = Lens.lens (\BatchStatementResponse' {error} -> error) (\s@BatchStatementResponse' {} a -> s {error = a} :: BatchStatementResponse)

instance Core.FromJSON BatchStatementResponse where
  parseJSON =
    Core.withObject
      "BatchStatementResponse"
      ( \x ->
          BatchStatementResponse'
            Prelude.<$> (x Core..:? "TableName")
            Prelude.<*> (x Core..:? "Item" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Error")
      )

instance Prelude.Hashable BatchStatementResponse

instance Prelude.NFData BatchStatementResponse
