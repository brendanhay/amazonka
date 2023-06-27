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
-- Module      : Amazonka.DataSync.Types.Capacity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.Capacity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The storage capacity of an on-premises storage system resource (for
-- example, a volume).
--
-- /See:/ 'newCapacity' smart constructor.
data Capacity = Capacity'
  { -- | The amount of space that\'s being used in a storage system resource
    -- without accounting for compression or deduplication.
    logicalUsed :: Prelude.Maybe Prelude.Natural,
    -- | The total amount of space available in a storage system resource.
    provisioned :: Prelude.Maybe Prelude.Natural,
    -- | The amount of space that\'s being used in a storage system resource.
    used :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Capacity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logicalUsed', 'capacity_logicalUsed' - The amount of space that\'s being used in a storage system resource
-- without accounting for compression or deduplication.
--
-- 'provisioned', 'capacity_provisioned' - The total amount of space available in a storage system resource.
--
-- 'used', 'capacity_used' - The amount of space that\'s being used in a storage system resource.
newCapacity ::
  Capacity
newCapacity =
  Capacity'
    { logicalUsed = Prelude.Nothing,
      provisioned = Prelude.Nothing,
      used = Prelude.Nothing
    }

-- | The amount of space that\'s being used in a storage system resource
-- without accounting for compression or deduplication.
capacity_logicalUsed :: Lens.Lens' Capacity (Prelude.Maybe Prelude.Natural)
capacity_logicalUsed = Lens.lens (\Capacity' {logicalUsed} -> logicalUsed) (\s@Capacity' {} a -> s {logicalUsed = a} :: Capacity)

-- | The total amount of space available in a storage system resource.
capacity_provisioned :: Lens.Lens' Capacity (Prelude.Maybe Prelude.Natural)
capacity_provisioned = Lens.lens (\Capacity' {provisioned} -> provisioned) (\s@Capacity' {} a -> s {provisioned = a} :: Capacity)

-- | The amount of space that\'s being used in a storage system resource.
capacity_used :: Lens.Lens' Capacity (Prelude.Maybe Prelude.Natural)
capacity_used = Lens.lens (\Capacity' {used} -> used) (\s@Capacity' {} a -> s {used = a} :: Capacity)

instance Data.FromJSON Capacity where
  parseJSON =
    Data.withObject
      "Capacity"
      ( \x ->
          Capacity'
            Prelude.<$> (x Data..:? "LogicalUsed")
            Prelude.<*> (x Data..:? "Provisioned")
            Prelude.<*> (x Data..:? "Used")
      )

instance Prelude.Hashable Capacity where
  hashWithSalt _salt Capacity' {..} =
    _salt
      `Prelude.hashWithSalt` logicalUsed
      `Prelude.hashWithSalt` provisioned
      `Prelude.hashWithSalt` used

instance Prelude.NFData Capacity where
  rnf Capacity' {..} =
    Prelude.rnf logicalUsed
      `Prelude.seq` Prelude.rnf provisioned
      `Prelude.seq` Prelude.rnf used
