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
-- Module      : Network.AWS.CloudControl.Types.ResourceRequestStatusFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudControl.Types.ResourceRequestStatusFilter where

import Network.AWS.CloudControl.Types.Operation
import Network.AWS.CloudControl.Types.OperationStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The filter criteria to use in determining the requests returned.
--
-- /See:/ 'newResourceRequestStatusFilter' smart constructor.
data ResourceRequestStatusFilter = ResourceRequestStatusFilter'
  { -- | The operation statuses to include in the filter.
    --
    -- -   @PENDING@: The operation has been requested, but not yet initiated.
    --
    -- -   @IN_PROGRESS@: The operation is currently in progress.
    --
    -- -   @SUCCESS@: The operation has successfully completed.
    --
    -- -   @FAILED@: The operation has failed.
    --
    -- -   @CANCEL_IN_PROGRESS@: The operation is currently in the process of
    --     being canceled.
    --
    -- -   @CANCEL_COMPLETE@: The operation has been canceled.
    operationStatuses :: Prelude.Maybe [OperationStatus],
    -- | The operation types to include in the filter.
    operations :: Prelude.Maybe [Operation]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceRequestStatusFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationStatuses', 'resourceRequestStatusFilter_operationStatuses' - The operation statuses to include in the filter.
--
-- -   @PENDING@: The operation has been requested, but not yet initiated.
--
-- -   @IN_PROGRESS@: The operation is currently in progress.
--
-- -   @SUCCESS@: The operation has successfully completed.
--
-- -   @FAILED@: The operation has failed.
--
-- -   @CANCEL_IN_PROGRESS@: The operation is currently in the process of
--     being canceled.
--
-- -   @CANCEL_COMPLETE@: The operation has been canceled.
--
-- 'operations', 'resourceRequestStatusFilter_operations' - The operation types to include in the filter.
newResourceRequestStatusFilter ::
  ResourceRequestStatusFilter
newResourceRequestStatusFilter =
  ResourceRequestStatusFilter'
    { operationStatuses =
        Prelude.Nothing,
      operations = Prelude.Nothing
    }

-- | The operation statuses to include in the filter.
--
-- -   @PENDING@: The operation has been requested, but not yet initiated.
--
-- -   @IN_PROGRESS@: The operation is currently in progress.
--
-- -   @SUCCESS@: The operation has successfully completed.
--
-- -   @FAILED@: The operation has failed.
--
-- -   @CANCEL_IN_PROGRESS@: The operation is currently in the process of
--     being canceled.
--
-- -   @CANCEL_COMPLETE@: The operation has been canceled.
resourceRequestStatusFilter_operationStatuses :: Lens.Lens' ResourceRequestStatusFilter (Prelude.Maybe [OperationStatus])
resourceRequestStatusFilter_operationStatuses = Lens.lens (\ResourceRequestStatusFilter' {operationStatuses} -> operationStatuses) (\s@ResourceRequestStatusFilter' {} a -> s {operationStatuses = a} :: ResourceRequestStatusFilter) Prelude.. Lens.mapping Lens.coerced

-- | The operation types to include in the filter.
resourceRequestStatusFilter_operations :: Lens.Lens' ResourceRequestStatusFilter (Prelude.Maybe [Operation])
resourceRequestStatusFilter_operations = Lens.lens (\ResourceRequestStatusFilter' {operations} -> operations) (\s@ResourceRequestStatusFilter' {} a -> s {operations = a} :: ResourceRequestStatusFilter) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable ResourceRequestStatusFilter

instance Prelude.NFData ResourceRequestStatusFilter

instance Core.ToJSON ResourceRequestStatusFilter where
  toJSON ResourceRequestStatusFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("OperationStatuses" Core..=)
              Prelude.<$> operationStatuses,
            ("Operations" Core..=) Prelude.<$> operations
          ]
      )
