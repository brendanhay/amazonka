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
-- Module      : Network.AWS.DynamoDB.Types.BatchStatementRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.BatchStatementRequest where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types.AttributeValue
import qualified Network.AWS.Lens as Lens

-- | A PartiQL batch statement request.
--
-- /See:/ 'newBatchStatementRequest' smart constructor.
data BatchStatementRequest = BatchStatementRequest'
  { -- | The read consistency of the PartiQL batch request.
    consistentRead :: Core.Maybe Core.Bool,
    -- | The parameters associated with a PartiQL statement in the batch request.
    parameters :: Core.Maybe (Core.NonEmpty AttributeValue),
    -- | A valid PartiQL statement.
    statement :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchStatementRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'consistentRead', 'batchStatementRequest_consistentRead' - The read consistency of the PartiQL batch request.
--
-- 'parameters', 'batchStatementRequest_parameters' - The parameters associated with a PartiQL statement in the batch request.
--
-- 'statement', 'batchStatementRequest_statement' - A valid PartiQL statement.
newBatchStatementRequest ::
  -- | 'statement'
  Core.Text ->
  BatchStatementRequest
newBatchStatementRequest pStatement_ =
  BatchStatementRequest'
    { consistentRead =
        Core.Nothing,
      parameters = Core.Nothing,
      statement = pStatement_
    }

-- | The read consistency of the PartiQL batch request.
batchStatementRequest_consistentRead :: Lens.Lens' BatchStatementRequest (Core.Maybe Core.Bool)
batchStatementRequest_consistentRead = Lens.lens (\BatchStatementRequest' {consistentRead} -> consistentRead) (\s@BatchStatementRequest' {} a -> s {consistentRead = a} :: BatchStatementRequest)

-- | The parameters associated with a PartiQL statement in the batch request.
batchStatementRequest_parameters :: Lens.Lens' BatchStatementRequest (Core.Maybe (Core.NonEmpty AttributeValue))
batchStatementRequest_parameters = Lens.lens (\BatchStatementRequest' {parameters} -> parameters) (\s@BatchStatementRequest' {} a -> s {parameters = a} :: BatchStatementRequest) Core.. Lens.mapping Lens._Coerce

-- | A valid PartiQL statement.
batchStatementRequest_statement :: Lens.Lens' BatchStatementRequest Core.Text
batchStatementRequest_statement = Lens.lens (\BatchStatementRequest' {statement} -> statement) (\s@BatchStatementRequest' {} a -> s {statement = a} :: BatchStatementRequest)

instance Core.Hashable BatchStatementRequest

instance Core.NFData BatchStatementRequest

instance Core.ToJSON BatchStatementRequest where
  toJSON BatchStatementRequest' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ConsistentRead" Core..=) Core.<$> consistentRead,
            ("Parameters" Core..=) Core.<$> parameters,
            Core.Just ("Statement" Core..= statement)
          ]
      )
