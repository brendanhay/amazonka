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
-- Module      : Amazonka.Lambda.Types.ProvisionedConcurrencyConfigListItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.ProvisionedConcurrencyConfigListItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types.ProvisionedConcurrencyStatusEnum
import qualified Amazonka.Prelude as Prelude

-- | Details about the provisioned concurrency configuration for a function
-- alias or version.
--
-- /See:/ 'newProvisionedConcurrencyConfigListItem' smart constructor.
data ProvisionedConcurrencyConfigListItem = ProvisionedConcurrencyConfigListItem'
  { -- | The amount of provisioned concurrency allocated. When a weighted alias
    -- is used during linear and canary deployments, this value fluctuates
    -- depending on the amount of concurrency that is provisioned for the
    -- function versions.
    allocatedProvisionedConcurrentExecutions :: Prelude.Maybe Prelude.Natural,
    -- | The amount of provisioned concurrency available.
    availableProvisionedConcurrentExecutions :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the alias or version.
    functionArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that a user last updated the configuration, in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format>.
    lastModified :: Prelude.Maybe Prelude.Text,
    -- | The amount of provisioned concurrency requested.
    requestedProvisionedConcurrentExecutions :: Prelude.Maybe Prelude.Natural,
    -- | The status of the allocation process.
    status :: Prelude.Maybe ProvisionedConcurrencyStatusEnum,
    -- | For failed allocations, the reason that provisioned concurrency could
    -- not be allocated.
    statusReason :: Prelude.Maybe Prelude.Text
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
-- 'allocatedProvisionedConcurrentExecutions', 'provisionedConcurrencyConfigListItem_allocatedProvisionedConcurrentExecutions' - The amount of provisioned concurrency allocated. When a weighted alias
-- is used during linear and canary deployments, this value fluctuates
-- depending on the amount of concurrency that is provisioned for the
-- function versions.
--
-- 'availableProvisionedConcurrentExecutions', 'provisionedConcurrencyConfigListItem_availableProvisionedConcurrentExecutions' - The amount of provisioned concurrency available.
--
-- 'functionArn', 'provisionedConcurrencyConfigListItem_functionArn' - The Amazon Resource Name (ARN) of the alias or version.
--
-- 'lastModified', 'provisionedConcurrencyConfigListItem_lastModified' - The date and time that a user last updated the configuration, in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format>.
--
-- 'requestedProvisionedConcurrentExecutions', 'provisionedConcurrencyConfigListItem_requestedProvisionedConcurrentExecutions' - The amount of provisioned concurrency requested.
--
-- 'status', 'provisionedConcurrencyConfigListItem_status' - The status of the allocation process.
--
-- 'statusReason', 'provisionedConcurrencyConfigListItem_statusReason' - For failed allocations, the reason that provisioned concurrency could
-- not be allocated.
newProvisionedConcurrencyConfigListItem ::
  ProvisionedConcurrencyConfigListItem
newProvisionedConcurrencyConfigListItem =
  ProvisionedConcurrencyConfigListItem'
    { allocatedProvisionedConcurrentExecutions =
        Prelude.Nothing,
      availableProvisionedConcurrentExecutions =
        Prelude.Nothing,
      functionArn = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      requestedProvisionedConcurrentExecutions =
        Prelude.Nothing,
      status = Prelude.Nothing,
      statusReason = Prelude.Nothing
    }

-- | The amount of provisioned concurrency allocated. When a weighted alias
-- is used during linear and canary deployments, this value fluctuates
-- depending on the amount of concurrency that is provisioned for the
-- function versions.
provisionedConcurrencyConfigListItem_allocatedProvisionedConcurrentExecutions :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Prelude.Maybe Prelude.Natural)
provisionedConcurrencyConfigListItem_allocatedProvisionedConcurrentExecutions = Lens.lens (\ProvisionedConcurrencyConfigListItem' {allocatedProvisionedConcurrentExecutions} -> allocatedProvisionedConcurrentExecutions) (\s@ProvisionedConcurrencyConfigListItem' {} a -> s {allocatedProvisionedConcurrentExecutions = a} :: ProvisionedConcurrencyConfigListItem)

-- | The amount of provisioned concurrency available.
provisionedConcurrencyConfigListItem_availableProvisionedConcurrentExecutions :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Prelude.Maybe Prelude.Natural)
provisionedConcurrencyConfigListItem_availableProvisionedConcurrentExecutions = Lens.lens (\ProvisionedConcurrencyConfigListItem' {availableProvisionedConcurrentExecutions} -> availableProvisionedConcurrentExecutions) (\s@ProvisionedConcurrencyConfigListItem' {} a -> s {availableProvisionedConcurrentExecutions = a} :: ProvisionedConcurrencyConfigListItem)

-- | The Amazon Resource Name (ARN) of the alias or version.
provisionedConcurrencyConfigListItem_functionArn :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Prelude.Maybe Prelude.Text)
provisionedConcurrencyConfigListItem_functionArn = Lens.lens (\ProvisionedConcurrencyConfigListItem' {functionArn} -> functionArn) (\s@ProvisionedConcurrencyConfigListItem' {} a -> s {functionArn = a} :: ProvisionedConcurrencyConfigListItem)

-- | The date and time that a user last updated the configuration, in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601 format>.
provisionedConcurrencyConfigListItem_lastModified :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Prelude.Maybe Prelude.Text)
provisionedConcurrencyConfigListItem_lastModified = Lens.lens (\ProvisionedConcurrencyConfigListItem' {lastModified} -> lastModified) (\s@ProvisionedConcurrencyConfigListItem' {} a -> s {lastModified = a} :: ProvisionedConcurrencyConfigListItem)

-- | The amount of provisioned concurrency requested.
provisionedConcurrencyConfigListItem_requestedProvisionedConcurrentExecutions :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Prelude.Maybe Prelude.Natural)
provisionedConcurrencyConfigListItem_requestedProvisionedConcurrentExecutions = Lens.lens (\ProvisionedConcurrencyConfigListItem' {requestedProvisionedConcurrentExecutions} -> requestedProvisionedConcurrentExecutions) (\s@ProvisionedConcurrencyConfigListItem' {} a -> s {requestedProvisionedConcurrentExecutions = a} :: ProvisionedConcurrencyConfigListItem)

-- | The status of the allocation process.
provisionedConcurrencyConfigListItem_status :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Prelude.Maybe ProvisionedConcurrencyStatusEnum)
provisionedConcurrencyConfigListItem_status = Lens.lens (\ProvisionedConcurrencyConfigListItem' {status} -> status) (\s@ProvisionedConcurrencyConfigListItem' {} a -> s {status = a} :: ProvisionedConcurrencyConfigListItem)

-- | For failed allocations, the reason that provisioned concurrency could
-- not be allocated.
provisionedConcurrencyConfigListItem_statusReason :: Lens.Lens' ProvisionedConcurrencyConfigListItem (Prelude.Maybe Prelude.Text)
provisionedConcurrencyConfigListItem_statusReason = Lens.lens (\ProvisionedConcurrencyConfigListItem' {statusReason} -> statusReason) (\s@ProvisionedConcurrencyConfigListItem' {} a -> s {statusReason = a} :: ProvisionedConcurrencyConfigListItem)

instance
  Data.FromJSON
    ProvisionedConcurrencyConfigListItem
  where
  parseJSON =
    Data.withObject
      "ProvisionedConcurrencyConfigListItem"
      ( \x ->
          ProvisionedConcurrencyConfigListItem'
            Prelude.<$> ( x
                            Data..:? "AllocatedProvisionedConcurrentExecutions"
                        )
            Prelude.<*> ( x
                            Data..:? "AvailableProvisionedConcurrentExecutions"
                        )
            Prelude.<*> (x Data..:? "FunctionArn")
            Prelude.<*> (x Data..:? "LastModified")
            Prelude.<*> ( x
                            Data..:? "RequestedProvisionedConcurrentExecutions"
                        )
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusReason")
      )

instance
  Prelude.Hashable
    ProvisionedConcurrencyConfigListItem
  where
  hashWithSalt
    _salt
    ProvisionedConcurrencyConfigListItem' {..} =
      _salt
        `Prelude.hashWithSalt` allocatedProvisionedConcurrentExecutions
        `Prelude.hashWithSalt` availableProvisionedConcurrentExecutions
        `Prelude.hashWithSalt` functionArn
        `Prelude.hashWithSalt` lastModified
        `Prelude.hashWithSalt` requestedProvisionedConcurrentExecutions
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` statusReason

instance
  Prelude.NFData
    ProvisionedConcurrencyConfigListItem
  where
  rnf ProvisionedConcurrencyConfigListItem' {..} =
    Prelude.rnf
      allocatedProvisionedConcurrentExecutions
      `Prelude.seq` Prelude.rnf availableProvisionedConcurrentExecutions
      `Prelude.seq` Prelude.rnf functionArn
      `Prelude.seq` Prelude.rnf lastModified
      `Prelude.seq` Prelude.rnf requestedProvisionedConcurrentExecutions
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusReason
