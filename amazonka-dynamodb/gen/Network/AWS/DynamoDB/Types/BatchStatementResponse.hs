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

-- | A PartiQL batch statement response..
--
-- /See:/ 'newBatchStatementResponse' smart constructor.
data BatchStatementResponse = BatchStatementResponse'
  { -- | The table name associated with a failed PartiQL batch statement.
    tableName :: Core.Maybe Core.Text,
    -- | A DynamoDB item associated with a BatchStatementResponse
    item :: Core.Maybe (Core.HashMap Core.Text AttributeValue),
    -- | The error associated with a failed PartiQL batch statement.
    error :: Core.Maybe BatchStatementError
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { tableName = Core.Nothing,
      item = Core.Nothing,
      error = Core.Nothing
    }

-- | The table name associated with a failed PartiQL batch statement.
batchStatementResponse_tableName :: Lens.Lens' BatchStatementResponse (Core.Maybe Core.Text)
batchStatementResponse_tableName = Lens.lens (\BatchStatementResponse' {tableName} -> tableName) (\s@BatchStatementResponse' {} a -> s {tableName = a} :: BatchStatementResponse)

-- | A DynamoDB item associated with a BatchStatementResponse
batchStatementResponse_item :: Lens.Lens' BatchStatementResponse (Core.Maybe (Core.HashMap Core.Text AttributeValue))
batchStatementResponse_item = Lens.lens (\BatchStatementResponse' {item} -> item) (\s@BatchStatementResponse' {} a -> s {item = a} :: BatchStatementResponse) Core.. Lens.mapping Lens._Coerce

-- | The error associated with a failed PartiQL batch statement.
batchStatementResponse_error :: Lens.Lens' BatchStatementResponse (Core.Maybe BatchStatementError)
batchStatementResponse_error = Lens.lens (\BatchStatementResponse' {error} -> error) (\s@BatchStatementResponse' {} a -> s {error = a} :: BatchStatementResponse)

instance Core.FromJSON BatchStatementResponse where
  parseJSON =
    Core.withObject
      "BatchStatementResponse"
      ( \x ->
          BatchStatementResponse'
            Core.<$> (x Core..:? "TableName")
            Core.<*> (x Core..:? "Item" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Error")
      )

instance Core.Hashable BatchStatementResponse

instance Core.NFData BatchStatementResponse
