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
-- Module      : Amazonka.CostExplorer.Types.EBSResourceUtilization
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.EBSResourceUtilization where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The EBS field that contains a list of EBS metrics that are associated
-- with the current instance.
--
-- /See:/ 'newEBSResourceUtilization' smart constructor.
data EBSResourceUtilization = EBSResourceUtilization'
  { -- | The maximum number of write operations per second.
    ebsWriteOpsPerSecond :: Prelude.Maybe Prelude.Text,
    -- | The maximum size of read operations per second
    ebsReadBytesPerSecond :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of read operations per second.
    ebsReadOpsPerSecond :: Prelude.Maybe Prelude.Text,
    -- | The maximum size of write operations per second.
    ebsWriteBytesPerSecond :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EBSResourceUtilization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ebsWriteOpsPerSecond', 'eBSResourceUtilization_ebsWriteOpsPerSecond' - The maximum number of write operations per second.
--
-- 'ebsReadBytesPerSecond', 'eBSResourceUtilization_ebsReadBytesPerSecond' - The maximum size of read operations per second
--
-- 'ebsReadOpsPerSecond', 'eBSResourceUtilization_ebsReadOpsPerSecond' - The maximum number of read operations per second.
--
-- 'ebsWriteBytesPerSecond', 'eBSResourceUtilization_ebsWriteBytesPerSecond' - The maximum size of write operations per second.
newEBSResourceUtilization ::
  EBSResourceUtilization
newEBSResourceUtilization =
  EBSResourceUtilization'
    { ebsWriteOpsPerSecond =
        Prelude.Nothing,
      ebsReadBytesPerSecond = Prelude.Nothing,
      ebsReadOpsPerSecond = Prelude.Nothing,
      ebsWriteBytesPerSecond = Prelude.Nothing
    }

-- | The maximum number of write operations per second.
eBSResourceUtilization_ebsWriteOpsPerSecond :: Lens.Lens' EBSResourceUtilization (Prelude.Maybe Prelude.Text)
eBSResourceUtilization_ebsWriteOpsPerSecond = Lens.lens (\EBSResourceUtilization' {ebsWriteOpsPerSecond} -> ebsWriteOpsPerSecond) (\s@EBSResourceUtilization' {} a -> s {ebsWriteOpsPerSecond = a} :: EBSResourceUtilization)

-- | The maximum size of read operations per second
eBSResourceUtilization_ebsReadBytesPerSecond :: Lens.Lens' EBSResourceUtilization (Prelude.Maybe Prelude.Text)
eBSResourceUtilization_ebsReadBytesPerSecond = Lens.lens (\EBSResourceUtilization' {ebsReadBytesPerSecond} -> ebsReadBytesPerSecond) (\s@EBSResourceUtilization' {} a -> s {ebsReadBytesPerSecond = a} :: EBSResourceUtilization)

-- | The maximum number of read operations per second.
eBSResourceUtilization_ebsReadOpsPerSecond :: Lens.Lens' EBSResourceUtilization (Prelude.Maybe Prelude.Text)
eBSResourceUtilization_ebsReadOpsPerSecond = Lens.lens (\EBSResourceUtilization' {ebsReadOpsPerSecond} -> ebsReadOpsPerSecond) (\s@EBSResourceUtilization' {} a -> s {ebsReadOpsPerSecond = a} :: EBSResourceUtilization)

-- | The maximum size of write operations per second.
eBSResourceUtilization_ebsWriteBytesPerSecond :: Lens.Lens' EBSResourceUtilization (Prelude.Maybe Prelude.Text)
eBSResourceUtilization_ebsWriteBytesPerSecond = Lens.lens (\EBSResourceUtilization' {ebsWriteBytesPerSecond} -> ebsWriteBytesPerSecond) (\s@EBSResourceUtilization' {} a -> s {ebsWriteBytesPerSecond = a} :: EBSResourceUtilization)

instance Data.FromJSON EBSResourceUtilization where
  parseJSON =
    Data.withObject
      "EBSResourceUtilization"
      ( \x ->
          EBSResourceUtilization'
            Prelude.<$> (x Data..:? "EbsWriteOpsPerSecond")
            Prelude.<*> (x Data..:? "EbsReadBytesPerSecond")
            Prelude.<*> (x Data..:? "EbsReadOpsPerSecond")
            Prelude.<*> (x Data..:? "EbsWriteBytesPerSecond")
      )

instance Prelude.Hashable EBSResourceUtilization where
  hashWithSalt _salt EBSResourceUtilization' {..} =
    _salt `Prelude.hashWithSalt` ebsWriteOpsPerSecond
      `Prelude.hashWithSalt` ebsReadBytesPerSecond
      `Prelude.hashWithSalt` ebsReadOpsPerSecond
      `Prelude.hashWithSalt` ebsWriteBytesPerSecond

instance Prelude.NFData EBSResourceUtilization where
  rnf EBSResourceUtilization' {..} =
    Prelude.rnf ebsWriteOpsPerSecond
      `Prelude.seq` Prelude.rnf ebsReadBytesPerSecond
      `Prelude.seq` Prelude.rnf ebsReadOpsPerSecond
      `Prelude.seq` Prelude.rnf ebsWriteBytesPerSecond
