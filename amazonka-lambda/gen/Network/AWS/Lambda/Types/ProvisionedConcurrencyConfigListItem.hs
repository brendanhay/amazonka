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
-- Module      : Network.AWS.Lambda.Types.ProvisionedConcurrencyConfigListItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.ProvisionedConcurrencyConfigListItem where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types.ProvisionedConcurrencyStatusEnum
import qualified Network.AWS.Lens as Lens

-- | Details about the provisioned concurrency configuration for a function
-- alias or version.
--
-- /See:/ 'newProvisionedConcurrencyConfigListItem' smart constructor.
data ProvisionedConcurrencyConfigListItem = ProvisionedConcurrencyConfigListItem'
  { -- | The status of the allocation process.
    status :: Core.Maybe ProvisionedConcurrencyStatusEnum,
    -- | The amount of provisioned concurrency available.
    availableProvisionedConcurrentExecutions :: Core.Maybe Core.Natural,
    -- | The amount of provisioned concurrency requested.
    requestedProvisionedConcurrentExecutions :: Core.Maybe Core.Natural,
    -- | The Amazon Resource Name (ARN) of the alias or version.
    functionArn :: Core.Maybe Core.Text,
    -- | The amount of provisioned concurrency allocated.
    allocatedProvisionedConcurrentExecutions :: Core.Maybe Core.Natural,
    -- | The date and time that a user last updated the configuration, in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format>.
    lastModified :: Core.Maybe Core.Text,
    -- | For failed allocations, the reason that provisioned concurrency could
    -- not be allocated.
    statusReason :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ProvisionedConcurrencyConfigListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'provisionedConcurrencyConfigListItem_status' - The status of the allocation process.
--
-- 'availableProvisionedConcurrentExecutions', 'provisionedConcurrencyConfigListItem_availableProvisionedConcurrentExecutions' - The amount of provisioned concurrency available.
--
-- 'requestedProvisionedConcurrentExecutions', 'provisionedConcurrencyConfigListItem_requestedProvisionedConcurrentExecutions' - The amount of provisioned concurrency requested.
--
-- 'functionArn', 'provisionedConcurrencyConfigListItem_functionArn' - The Amazon Resource Name (ARN) of the alias or version.
--
-- 'allocatedProvisionedConcurrentExecutions', 'provisionedConcurrencyConfigListItem_allocatedProvisionedConcurrentExecutions' - The amount of provisioned concurrency allocated.
--
-- 'lastModified', 'provisionedConcurrencyConfigListItem_lastModified' - The date and time that a user last updated the configuration, in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format>.
--
-- 'statusReason', 'provisionedConcurrencyConfigListItem_statusReason' - For failed allocations, the reason that provisioned concurrency could
-- not be allocated.
newProvisionedConcurrencyConfigListItem ::
  ProvisionedConcurrencyConfigListItem
newProvisionedConcurrencyConfigListItem =
  ProvisionedConcurrencyConfigListItem'
    { status =
        Core.Nothing,
      availableProvisionedConcurrentExecutions =
        Core.Nothing,
      requestedProvisionedConcurrentExecutions =
        Core.Nothing,
      functionArn = Core.Nothing,
      allocatedProvisionedConcurrentExecutions =
        Core.Nothing,
      lastModified = Core.Nothing,
      statusReason = Core.Nothing
    }

-- | The status of the allocation process.
provisionedConcurrencyConfigListItem_status :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Core.Maybe ProvisionedConcurrencyStatusEnum)
provisionedConcurrencyConfigListItem_status = Lens.lens (\ProvisionedConcurrencyConfigListItem' {status} -> status) (\s@ProvisionedConcurrencyConfigListItem' {} a -> s {status = a} :: ProvisionedConcurrencyConfigListItem)

-- | The amount of provisioned concurrency available.
provisionedConcurrencyConfigListItem_availableProvisionedConcurrentExecutions :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Core.Maybe Core.Natural)
provisionedConcurrencyConfigListItem_availableProvisionedConcurrentExecutions = Lens.lens (\ProvisionedConcurrencyConfigListItem' {availableProvisionedConcurrentExecutions} -> availableProvisionedConcurrentExecutions) (\s@ProvisionedConcurrencyConfigListItem' {} a -> s {availableProvisionedConcurrentExecutions = a} :: ProvisionedConcurrencyConfigListItem)

-- | The amount of provisioned concurrency requested.
provisionedConcurrencyConfigListItem_requestedProvisionedConcurrentExecutions :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Core.Maybe Core.Natural)
provisionedConcurrencyConfigListItem_requestedProvisionedConcurrentExecutions = Lens.lens (\ProvisionedConcurrencyConfigListItem' {requestedProvisionedConcurrentExecutions} -> requestedProvisionedConcurrentExecutions) (\s@ProvisionedConcurrencyConfigListItem' {} a -> s {requestedProvisionedConcurrentExecutions = a} :: ProvisionedConcurrencyConfigListItem)

-- | The Amazon Resource Name (ARN) of the alias or version.
provisionedConcurrencyConfigListItem_functionArn :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Core.Maybe Core.Text)
provisionedConcurrencyConfigListItem_functionArn = Lens.lens (\ProvisionedConcurrencyConfigListItem' {functionArn} -> functionArn) (\s@ProvisionedConcurrencyConfigListItem' {} a -> s {functionArn = a} :: ProvisionedConcurrencyConfigListItem)

-- | The amount of provisioned concurrency allocated.
provisionedConcurrencyConfigListItem_allocatedProvisionedConcurrentExecutions :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Core.Maybe Core.Natural)
provisionedConcurrencyConfigListItem_allocatedProvisionedConcurrentExecutions = Lens.lens (\ProvisionedConcurrencyConfigListItem' {allocatedProvisionedConcurrentExecutions} -> allocatedProvisionedConcurrentExecutions) (\s@ProvisionedConcurrencyConfigListItem' {} a -> s {allocatedProvisionedConcurrentExecutions = a} :: ProvisionedConcurrencyConfigListItem)

-- | The date and time that a user last updated the configuration, in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format>.
provisionedConcurrencyConfigListItem_lastModified :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Core.Maybe Core.Text)
provisionedConcurrencyConfigListItem_lastModified = Lens.lens (\ProvisionedConcurrencyConfigListItem' {lastModified} -> lastModified) (\s@ProvisionedConcurrencyConfigListItem' {} a -> s {lastModified = a} :: ProvisionedConcurrencyConfigListItem)

-- | For failed allocations, the reason that provisioned concurrency could
-- not be allocated.
provisionedConcurrencyConfigListItem_statusReason :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Core.Maybe Core.Text)
provisionedConcurrencyConfigListItem_statusReason = Lens.lens (\ProvisionedConcurrencyConfigListItem' {statusReason} -> statusReason) (\s@ProvisionedConcurrencyConfigListItem' {} a -> s {statusReason = a} :: ProvisionedConcurrencyConfigListItem)

instance
  Core.FromJSON
    ProvisionedConcurrencyConfigListItem
  where
  parseJSON =
    Core.withObject
      "ProvisionedConcurrencyConfigListItem"
      ( \x ->
          ProvisionedConcurrencyConfigListItem'
            Core.<$> (x Core..:? "Status")
            Core.<*> ( x
                         Core..:? "AvailableProvisionedConcurrentExecutions"
                     )
            Core.<*> ( x
                         Core..:? "RequestedProvisionedConcurrentExecutions"
                     )
            Core.<*> (x Core..:? "FunctionArn")
            Core.<*> ( x
                         Core..:? "AllocatedProvisionedConcurrentExecutions"
                     )
            Core.<*> (x Core..:? "LastModified")
            Core.<*> (x Core..:? "StatusReason")
      )

instance
  Core.Hashable
    ProvisionedConcurrencyConfigListItem

instance
  Core.NFData
    ProvisionedConcurrencyConfigListItem
