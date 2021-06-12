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
-- Module      : Network.AWS.SageMaker.Types.OfflineStoreStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.OfflineStoreStatus where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.OfflineStoreStatusValue

-- | The status of @OfflineStore@.
--
-- /See:/ 'newOfflineStoreStatus' smart constructor.
data OfflineStoreStatus = OfflineStoreStatus'
  { -- | The justification for why the OfflineStoreStatus is Blocked (if
    -- applicable).
    blockedReason :: Core.Maybe Core.Text,
    -- | An @OfflineStore@ status.
    status :: OfflineStoreStatusValue
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OfflineStoreStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blockedReason', 'offlineStoreStatus_blockedReason' - The justification for why the OfflineStoreStatus is Blocked (if
-- applicable).
--
-- 'status', 'offlineStoreStatus_status' - An @OfflineStore@ status.
newOfflineStoreStatus ::
  -- | 'status'
  OfflineStoreStatusValue ->
  OfflineStoreStatus
newOfflineStoreStatus pStatus_ =
  OfflineStoreStatus'
    { blockedReason = Core.Nothing,
      status = pStatus_
    }

-- | The justification for why the OfflineStoreStatus is Blocked (if
-- applicable).
offlineStoreStatus_blockedReason :: Lens.Lens' OfflineStoreStatus (Core.Maybe Core.Text)
offlineStoreStatus_blockedReason = Lens.lens (\OfflineStoreStatus' {blockedReason} -> blockedReason) (\s@OfflineStoreStatus' {} a -> s {blockedReason = a} :: OfflineStoreStatus)

-- | An @OfflineStore@ status.
offlineStoreStatus_status :: Lens.Lens' OfflineStoreStatus OfflineStoreStatusValue
offlineStoreStatus_status = Lens.lens (\OfflineStoreStatus' {status} -> status) (\s@OfflineStoreStatus' {} a -> s {status = a} :: OfflineStoreStatus)

instance Core.FromJSON OfflineStoreStatus where
  parseJSON =
    Core.withObject
      "OfflineStoreStatus"
      ( \x ->
          OfflineStoreStatus'
            Core.<$> (x Core..:? "BlockedReason")
            Core.<*> (x Core..: "Status")
      )

instance Core.Hashable OfflineStoreStatus

instance Core.NFData OfflineStoreStatus
