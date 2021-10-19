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
import qualified Network.AWS.Prelude as Prelude

-- | Details about the provisioned concurrency configuration for a function
-- alias or version.
--
-- /See:/ 'newProvisionedConcurrencyConfigListItem' smart constructor.
data ProvisionedConcurrencyConfigListItem = ProvisionedConcurrencyConfigListItem'
  { -- | The status of the allocation process.
    status :: Prelude.Maybe ProvisionedConcurrencyStatusEnum,
    -- | The Amazon Resource Name (ARN) of the alias or version.
    functionArn :: Prelude.Maybe Prelude.Text,
    -- | The amount of provisioned concurrency requested.
    requestedProvisionedConcurrentExecutions :: Prelude.Maybe Prelude.Natural,
    -- | The amount of provisioned concurrency available.
    availableProvisionedConcurrentExecutions :: Prelude.Maybe Prelude.Natural,
    -- | For failed allocations, the reason that provisioned concurrency could
    -- not be allocated.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | The amount of provisioned concurrency allocated.
    allocatedProvisionedConcurrentExecutions :: Prelude.Maybe Prelude.Natural,
    -- | The date and time that a user last updated the configuration, in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format>.
    lastModified :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'functionArn', 'provisionedConcurrencyConfigListItem_functionArn' - The Amazon Resource Name (ARN) of the alias or version.
--
-- 'requestedProvisionedConcurrentExecutions', 'provisionedConcurrencyConfigListItem_requestedProvisionedConcurrentExecutions' - The amount of provisioned concurrency requested.
--
-- 'availableProvisionedConcurrentExecutions', 'provisionedConcurrencyConfigListItem_availableProvisionedConcurrentExecutions' - The amount of provisioned concurrency available.
--
-- 'statusReason', 'provisionedConcurrencyConfigListItem_statusReason' - For failed allocations, the reason that provisioned concurrency could
-- not be allocated.
--
-- 'allocatedProvisionedConcurrentExecutions', 'provisionedConcurrencyConfigListItem_allocatedProvisionedConcurrentExecutions' - The amount of provisioned concurrency allocated.
--
-- 'lastModified', 'provisionedConcurrencyConfigListItem_lastModified' - The date and time that a user last updated the configuration, in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format>.
newProvisionedConcurrencyConfigListItem ::
  ProvisionedConcurrencyConfigListItem
newProvisionedConcurrencyConfigListItem =
  ProvisionedConcurrencyConfigListItem'
    { status =
        Prelude.Nothing,
      functionArn = Prelude.Nothing,
      requestedProvisionedConcurrentExecutions =
        Prelude.Nothing,
      availableProvisionedConcurrentExecutions =
        Prelude.Nothing,
      statusReason = Prelude.Nothing,
      allocatedProvisionedConcurrentExecutions =
        Prelude.Nothing,
      lastModified = Prelude.Nothing
    }

-- | The status of the allocation process.
provisionedConcurrencyConfigListItem_status :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Prelude.Maybe ProvisionedConcurrencyStatusEnum)
provisionedConcurrencyConfigListItem_status = Lens.lens (\ProvisionedConcurrencyConfigListItem' {status} -> status) (\s@ProvisionedConcurrencyConfigListItem' {} a -> s {status = a} :: ProvisionedConcurrencyConfigListItem)

-- | The Amazon Resource Name (ARN) of the alias or version.
provisionedConcurrencyConfigListItem_functionArn :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Prelude.Maybe Prelude.Text)
provisionedConcurrencyConfigListItem_functionArn = Lens.lens (\ProvisionedConcurrencyConfigListItem' {functionArn} -> functionArn) (\s@ProvisionedConcurrencyConfigListItem' {} a -> s {functionArn = a} :: ProvisionedConcurrencyConfigListItem)

-- | The amount of provisioned concurrency requested.
provisionedConcurrencyConfigListItem_requestedProvisionedConcurrentExecutions :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Prelude.Maybe Prelude.Natural)
provisionedConcurrencyConfigListItem_requestedProvisionedConcurrentExecutions = Lens.lens (\ProvisionedConcurrencyConfigListItem' {requestedProvisionedConcurrentExecutions} -> requestedProvisionedConcurrentExecutions) (\s@ProvisionedConcurrencyConfigListItem' {} a -> s {requestedProvisionedConcurrentExecutions = a} :: ProvisionedConcurrencyConfigListItem)

-- | The amount of provisioned concurrency available.
provisionedConcurrencyConfigListItem_availableProvisionedConcurrentExecutions :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Prelude.Maybe Prelude.Natural)
provisionedConcurrencyConfigListItem_availableProvisionedConcurrentExecutions = Lens.lens (\ProvisionedConcurrencyConfigListItem' {availableProvisionedConcurrentExecutions} -> availableProvisionedConcurrentExecutions) (\s@ProvisionedConcurrencyConfigListItem' {} a -> s {availableProvisionedConcurrentExecutions = a} :: ProvisionedConcurrencyConfigListItem)

-- | For failed allocations, the reason that provisioned concurrency could
-- not be allocated.
provisionedConcurrencyConfigListItem_statusReason :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Prelude.Maybe Prelude.Text)
provisionedConcurrencyConfigListItem_statusReason = Lens.lens (\ProvisionedConcurrencyConfigListItem' {statusReason} -> statusReason) (\s@ProvisionedConcurrencyConfigListItem' {} a -> s {statusReason = a} :: ProvisionedConcurrencyConfigListItem)

-- | The amount of provisioned concurrency allocated.
provisionedConcurrencyConfigListItem_allocatedProvisionedConcurrentExecutions :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Prelude.Maybe Prelude.Natural)
provisionedConcurrencyConfigListItem_allocatedProvisionedConcurrentExecutions = Lens.lens (\ProvisionedConcurrencyConfigListItem' {allocatedProvisionedConcurrentExecutions} -> allocatedProvisionedConcurrentExecutions) (\s@ProvisionedConcurrencyConfigListItem' {} a -> s {allocatedProvisionedConcurrentExecutions = a} :: ProvisionedConcurrencyConfigListItem)

-- | The date and time that a user last updated the configuration, in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format>.
provisionedConcurrencyConfigListItem_lastModified :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Prelude.Maybe Prelude.Text)
provisionedConcurrencyConfigListItem_lastModified = Lens.lens (\ProvisionedConcurrencyConfigListItem' {lastModified} -> lastModified) (\s@ProvisionedConcurrencyConfigListItem' {} a -> s {lastModified = a} :: ProvisionedConcurrencyConfigListItem)

instance
  Core.FromJSON
    ProvisionedConcurrencyConfigListItem
  where
  parseJSON =
    Core.withObject
      "ProvisionedConcurrencyConfigListItem"
      ( \x ->
          ProvisionedConcurrencyConfigListItem'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "FunctionArn")
            Prelude.<*> ( x
                            Core..:? "RequestedProvisionedConcurrentExecutions"
                        )
            Prelude.<*> ( x
                            Core..:? "AvailableProvisionedConcurrentExecutions"
                        )
            Prelude.<*> (x Core..:? "StatusReason")
            Prelude.<*> ( x
                            Core..:? "AllocatedProvisionedConcurrentExecutions"
                        )
            Prelude.<*> (x Core..:? "LastModified")
      )

instance
  Prelude.Hashable
    ProvisionedConcurrencyConfigListItem

instance
  Prelude.NFData
    ProvisionedConcurrencyConfigListItem
