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
-- Module      : Network.AWS.CostExplorer.Types.EBSResourceUtilization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.EBSResourceUtilization where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The EBS field that contains a list of EBS metrics associated with the
-- current instance.
--
-- /See:/ 'newEBSResourceUtilization' smart constructor.
data EBSResourceUtilization = EBSResourceUtilization'
  { -- | The maximum size of write operations per second.
    ebsWriteBytesPerSecond :: Core.Maybe Core.Text,
    -- | The maximum number of read operations per second.
    ebsReadOpsPerSecond :: Core.Maybe Core.Text,
    -- | The maximum number of write operations per second.
    ebsWriteOpsPerSecond :: Core.Maybe Core.Text,
    -- | The maximum size of read operations per second
    ebsReadBytesPerSecond :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EBSResourceUtilization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ebsWriteBytesPerSecond', 'eBSResourceUtilization_ebsWriteBytesPerSecond' - The maximum size of write operations per second.
--
-- 'ebsReadOpsPerSecond', 'eBSResourceUtilization_ebsReadOpsPerSecond' - The maximum number of read operations per second.
--
-- 'ebsWriteOpsPerSecond', 'eBSResourceUtilization_ebsWriteOpsPerSecond' - The maximum number of write operations per second.
--
-- 'ebsReadBytesPerSecond', 'eBSResourceUtilization_ebsReadBytesPerSecond' - The maximum size of read operations per second
newEBSResourceUtilization ::
  EBSResourceUtilization
newEBSResourceUtilization =
  EBSResourceUtilization'
    { ebsWriteBytesPerSecond =
        Core.Nothing,
      ebsReadOpsPerSecond = Core.Nothing,
      ebsWriteOpsPerSecond = Core.Nothing,
      ebsReadBytesPerSecond = Core.Nothing
    }

-- | The maximum size of write operations per second.
eBSResourceUtilization_ebsWriteBytesPerSecond :: Lens.Lens' EBSResourceUtilization (Core.Maybe Core.Text)
eBSResourceUtilization_ebsWriteBytesPerSecond = Lens.lens (\EBSResourceUtilization' {ebsWriteBytesPerSecond} -> ebsWriteBytesPerSecond) (\s@EBSResourceUtilization' {} a -> s {ebsWriteBytesPerSecond = a} :: EBSResourceUtilization)

-- | The maximum number of read operations per second.
eBSResourceUtilization_ebsReadOpsPerSecond :: Lens.Lens' EBSResourceUtilization (Core.Maybe Core.Text)
eBSResourceUtilization_ebsReadOpsPerSecond = Lens.lens (\EBSResourceUtilization' {ebsReadOpsPerSecond} -> ebsReadOpsPerSecond) (\s@EBSResourceUtilization' {} a -> s {ebsReadOpsPerSecond = a} :: EBSResourceUtilization)

-- | The maximum number of write operations per second.
eBSResourceUtilization_ebsWriteOpsPerSecond :: Lens.Lens' EBSResourceUtilization (Core.Maybe Core.Text)
eBSResourceUtilization_ebsWriteOpsPerSecond = Lens.lens (\EBSResourceUtilization' {ebsWriteOpsPerSecond} -> ebsWriteOpsPerSecond) (\s@EBSResourceUtilization' {} a -> s {ebsWriteOpsPerSecond = a} :: EBSResourceUtilization)

-- | The maximum size of read operations per second
eBSResourceUtilization_ebsReadBytesPerSecond :: Lens.Lens' EBSResourceUtilization (Core.Maybe Core.Text)
eBSResourceUtilization_ebsReadBytesPerSecond = Lens.lens (\EBSResourceUtilization' {ebsReadBytesPerSecond} -> ebsReadBytesPerSecond) (\s@EBSResourceUtilization' {} a -> s {ebsReadBytesPerSecond = a} :: EBSResourceUtilization)

instance Core.FromJSON EBSResourceUtilization where
  parseJSON =
    Core.withObject
      "EBSResourceUtilization"
      ( \x ->
          EBSResourceUtilization'
            Core.<$> (x Core..:? "EbsWriteBytesPerSecond")
            Core.<*> (x Core..:? "EbsReadOpsPerSecond")
            Core.<*> (x Core..:? "EbsWriteOpsPerSecond")
            Core.<*> (x Core..:? "EbsReadBytesPerSecond")
      )

instance Core.Hashable EBSResourceUtilization

instance Core.NFData EBSResourceUtilization
