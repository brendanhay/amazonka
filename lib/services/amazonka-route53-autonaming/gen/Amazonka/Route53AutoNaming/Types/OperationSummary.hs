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
-- Module      : Amazonka.Route53AutoNaming.Types.OperationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53AutoNaming.Types.OperationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53AutoNaming.Types.OperationStatus

-- | A complex type that contains information about an operation that matches
-- the criteria that you specified in a
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_ListOperations.html ListOperations>
-- request.
--
-- /See:/ 'newOperationSummary' smart constructor.
data OperationSummary = OperationSummary'
  { -- | The ID for an operation.
    id :: Prelude.Maybe Prelude.Text,
    -- | The status of the operation. Values include the following:
    --
    -- -   __SUBMITTED__: This is the initial state immediately after you
    --     submit a request.
    --
    -- -   __PENDING__: Cloud Map is performing the operation.
    --
    -- -   __SUCCESS__: The operation succeeded.
    --
    -- -   __FAIL__: The operation failed. For the failure reason, see
    --     @ErrorMessage@.
    status :: Prelude.Maybe OperationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OperationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'operationSummary_id' - The ID for an operation.
--
-- 'status', 'operationSummary_status' - The status of the operation. Values include the following:
--
-- -   __SUBMITTED__: This is the initial state immediately after you
--     submit a request.
--
-- -   __PENDING__: Cloud Map is performing the operation.
--
-- -   __SUCCESS__: The operation succeeded.
--
-- -   __FAIL__: The operation failed. For the failure reason, see
--     @ErrorMessage@.
newOperationSummary ::
  OperationSummary
newOperationSummary =
  OperationSummary'
    { id = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The ID for an operation.
operationSummary_id :: Lens.Lens' OperationSummary (Prelude.Maybe Prelude.Text)
operationSummary_id = Lens.lens (\OperationSummary' {id} -> id) (\s@OperationSummary' {} a -> s {id = a} :: OperationSummary)

-- | The status of the operation. Values include the following:
--
-- -   __SUBMITTED__: This is the initial state immediately after you
--     submit a request.
--
-- -   __PENDING__: Cloud Map is performing the operation.
--
-- -   __SUCCESS__: The operation succeeded.
--
-- -   __FAIL__: The operation failed. For the failure reason, see
--     @ErrorMessage@.
operationSummary_status :: Lens.Lens' OperationSummary (Prelude.Maybe OperationStatus)
operationSummary_status = Lens.lens (\OperationSummary' {status} -> status) (\s@OperationSummary' {} a -> s {status = a} :: OperationSummary)

instance Data.FromJSON OperationSummary where
  parseJSON =
    Data.withObject
      "OperationSummary"
      ( \x ->
          OperationSummary'
            Prelude.<$> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable OperationSummary where
  hashWithSalt _salt OperationSummary' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` status

instance Prelude.NFData OperationSummary where
  rnf OperationSummary' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf status
