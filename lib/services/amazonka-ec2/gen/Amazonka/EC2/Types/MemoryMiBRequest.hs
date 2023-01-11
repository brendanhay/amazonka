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
-- Module      : Amazonka.EC2.Types.MemoryMiBRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.MemoryMiBRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The minimum and maximum amount of memory, in MiB.
--
-- /See:/ 'newMemoryMiBRequest' smart constructor.
data MemoryMiBRequest = MemoryMiBRequest'
  { -- | The maximum amount of memory, in MiB. To specify no maximum limit, omit
    -- this parameter.
    max :: Prelude.Maybe Prelude.Int,
    -- | The minimum amount of memory, in MiB. To specify no minimum limit,
    -- specify @0@.
    min :: Prelude.Int
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
-- 'max', 'memoryMiBRequest_max' - The maximum amount of memory, in MiB. To specify no maximum limit, omit
-- this parameter.
--
-- 'min', 'memoryMiBRequest_min' - The minimum amount of memory, in MiB. To specify no minimum limit,
-- specify @0@.
newMemoryMiBRequest ::
  -- | 'min'
  Prelude.Int ->
  MemoryMiBRequest
newMemoryMiBRequest pMin_ =
  MemoryMiBRequest'
    { max = Prelude.Nothing,
      min = pMin_
    }

-- | The maximum amount of memory, in MiB. To specify no maximum limit, omit
-- this parameter.
memoryMiBRequest_max :: Lens.Lens' MemoryMiBRequest (Prelude.Maybe Prelude.Int)
memoryMiBRequest_max = Lens.lens (\MemoryMiBRequest' {max} -> max) (\s@MemoryMiBRequest' {} a -> s {max = a} :: MemoryMiBRequest)

-- | The minimum amount of memory, in MiB. To specify no minimum limit,
-- specify @0@.
memoryMiBRequest_min :: Lens.Lens' MemoryMiBRequest Prelude.Int
memoryMiBRequest_min = Lens.lens (\MemoryMiBRequest' {min} -> min) (\s@MemoryMiBRequest' {} a -> s {min = a} :: MemoryMiBRequest)

instance Prelude.Hashable MemoryMiBRequest where
  hashWithSalt _salt MemoryMiBRequest' {..} =
    _salt `Prelude.hashWithSalt` max
      `Prelude.hashWithSalt` min

instance Prelude.NFData MemoryMiBRequest where
  rnf MemoryMiBRequest' {..} =
    Prelude.rnf max `Prelude.seq` Prelude.rnf min

instance Data.ToQuery MemoryMiBRequest where
  toQuery MemoryMiBRequest' {..} =
    Prelude.mconcat
      ["Max" Data.=: max, "Min" Data.=: min]
