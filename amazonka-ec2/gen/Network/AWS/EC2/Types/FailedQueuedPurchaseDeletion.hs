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
-- Module      : Network.AWS.EC2.Types.FailedQueuedPurchaseDeletion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FailedQueuedPurchaseDeletion where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.DeleteQueuedReservedInstancesError
import qualified Network.AWS.Lens as Lens

-- | Describes a Reserved Instance whose queued purchase was not deleted.
--
-- /See:/ 'newFailedQueuedPurchaseDeletion' smart constructor.
data FailedQueuedPurchaseDeletion = FailedQueuedPurchaseDeletion'
  { -- | The ID of the Reserved Instance.
    reservedInstancesId :: Core.Maybe Core.Text,
    -- | The error.
    error :: Core.Maybe DeleteQueuedReservedInstancesError
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FailedQueuedPurchaseDeletion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedInstancesId', 'failedQueuedPurchaseDeletion_reservedInstancesId' - The ID of the Reserved Instance.
--
-- 'error', 'failedQueuedPurchaseDeletion_error' - The error.
newFailedQueuedPurchaseDeletion ::
  FailedQueuedPurchaseDeletion
newFailedQueuedPurchaseDeletion =
  FailedQueuedPurchaseDeletion'
    { reservedInstancesId =
        Core.Nothing,
      error = Core.Nothing
    }

-- | The ID of the Reserved Instance.
failedQueuedPurchaseDeletion_reservedInstancesId :: Lens.Lens' FailedQueuedPurchaseDeletion (Core.Maybe Core.Text)
failedQueuedPurchaseDeletion_reservedInstancesId = Lens.lens (\FailedQueuedPurchaseDeletion' {reservedInstancesId} -> reservedInstancesId) (\s@FailedQueuedPurchaseDeletion' {} a -> s {reservedInstancesId = a} :: FailedQueuedPurchaseDeletion)

-- | The error.
failedQueuedPurchaseDeletion_error :: Lens.Lens' FailedQueuedPurchaseDeletion (Core.Maybe DeleteQueuedReservedInstancesError)
failedQueuedPurchaseDeletion_error = Lens.lens (\FailedQueuedPurchaseDeletion' {error} -> error) (\s@FailedQueuedPurchaseDeletion' {} a -> s {error = a} :: FailedQueuedPurchaseDeletion)

instance Core.FromXML FailedQueuedPurchaseDeletion where
  parseXML x =
    FailedQueuedPurchaseDeletion'
      Core.<$> (x Core..@? "reservedInstancesId")
      Core.<*> (x Core..@? "error")

instance Core.Hashable FailedQueuedPurchaseDeletion

instance Core.NFData FailedQueuedPurchaseDeletion
