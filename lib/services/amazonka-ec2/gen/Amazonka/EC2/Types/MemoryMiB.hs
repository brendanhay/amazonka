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
-- Module      : Amazonka.EC2.Types.MemoryMiB
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.MemoryMiB where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The minimum and maximum amount of memory, in MiB.
--
-- /See:/ 'newMemoryMiB' smart constructor.
data MemoryMiB = MemoryMiB'
  { -- | The maximum amount of memory, in MiB. If this parameter is not
    -- specified, there is no maximum limit.
    max :: Prelude.Maybe Prelude.Int,
    -- | The minimum amount of memory, in MiB. If this parameter is not
    -- specified, there is no minimum limit.
    min :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MemoryMiB' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'memoryMiB_max' - The maximum amount of memory, in MiB. If this parameter is not
-- specified, there is no maximum limit.
--
-- 'min', 'memoryMiB_min' - The minimum amount of memory, in MiB. If this parameter is not
-- specified, there is no minimum limit.
newMemoryMiB ::
  MemoryMiB
newMemoryMiB =
  MemoryMiB'
    { max = Prelude.Nothing,
      min = Prelude.Nothing
    }

-- | The maximum amount of memory, in MiB. If this parameter is not
-- specified, there is no maximum limit.
memoryMiB_max :: Lens.Lens' MemoryMiB (Prelude.Maybe Prelude.Int)
memoryMiB_max = Lens.lens (\MemoryMiB' {max} -> max) (\s@MemoryMiB' {} a -> s {max = a} :: MemoryMiB)

-- | The minimum amount of memory, in MiB. If this parameter is not
-- specified, there is no minimum limit.
memoryMiB_min :: Lens.Lens' MemoryMiB (Prelude.Maybe Prelude.Int)
memoryMiB_min = Lens.lens (\MemoryMiB' {min} -> min) (\s@MemoryMiB' {} a -> s {min = a} :: MemoryMiB)

instance Data.FromXML MemoryMiB where
  parseXML x =
    MemoryMiB'
      Prelude.<$> (x Data..@? "max")
      Prelude.<*> (x Data..@? "min")

instance Prelude.Hashable MemoryMiB where
  hashWithSalt _salt MemoryMiB' {..} =
    _salt
      `Prelude.hashWithSalt` max
      `Prelude.hashWithSalt` min

instance Prelude.NFData MemoryMiB where
  rnf MemoryMiB' {..} =
    Prelude.rnf max `Prelude.seq` Prelude.rnf min

instance Data.ToQuery MemoryMiB where
  toQuery MemoryMiB' {..} =
    Prelude.mconcat
      ["Max" Data.=: max, "Min" Data.=: min]
