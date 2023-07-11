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
-- Module      : Amazonka.AutoScaling.Types.MemoryMiBRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.MemoryMiBRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the minimum and maximum for the @MemoryMiB@ object when you
-- specify InstanceRequirements for an Auto Scaling group.
--
-- /See:/ 'newMemoryMiBRequest' smart constructor.
data MemoryMiBRequest = MemoryMiBRequest'
  { -- | The memory maximum in MiB.
    max :: Prelude.Maybe Prelude.Natural,
    -- | The memory minimum in MiB.
    min :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MemoryMiBRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'memoryMiBRequest_max' - The memory maximum in MiB.
--
-- 'min', 'memoryMiBRequest_min' - The memory minimum in MiB.
newMemoryMiBRequest ::
  -- | 'min'
  Prelude.Natural ->
  MemoryMiBRequest
newMemoryMiBRequest pMin_ =
  MemoryMiBRequest'
    { max = Prelude.Nothing,
      min = pMin_
    }

-- | The memory maximum in MiB.
memoryMiBRequest_max :: Lens.Lens' MemoryMiBRequest (Prelude.Maybe Prelude.Natural)
memoryMiBRequest_max = Lens.lens (\MemoryMiBRequest' {max} -> max) (\s@MemoryMiBRequest' {} a -> s {max = a} :: MemoryMiBRequest)

-- | The memory minimum in MiB.
memoryMiBRequest_min :: Lens.Lens' MemoryMiBRequest Prelude.Natural
memoryMiBRequest_min = Lens.lens (\MemoryMiBRequest' {min} -> min) (\s@MemoryMiBRequest' {} a -> s {min = a} :: MemoryMiBRequest)

instance Data.FromXML MemoryMiBRequest where
  parseXML x =
    MemoryMiBRequest'
      Prelude.<$> (x Data..@? "Max")
      Prelude.<*> (x Data..@ "Min")

instance Prelude.Hashable MemoryMiBRequest where
  hashWithSalt _salt MemoryMiBRequest' {..} =
    _salt
      `Prelude.hashWithSalt` max
      `Prelude.hashWithSalt` min

instance Prelude.NFData MemoryMiBRequest where
  rnf MemoryMiBRequest' {..} =
    Prelude.rnf max `Prelude.seq` Prelude.rnf min

instance Data.ToQuery MemoryMiBRequest where
  toQuery MemoryMiBRequest' {..} =
    Prelude.mconcat
      ["Max" Data.=: max, "Min" Data.=: min]
