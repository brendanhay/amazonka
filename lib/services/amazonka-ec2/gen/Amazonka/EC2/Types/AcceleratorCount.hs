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
-- Module      : Amazonka.EC2.Types.AcceleratorCount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.AcceleratorCount where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The minimum and maximum number of accelerators (GPUs, FPGAs, or Amazon
-- Web Services Inferentia chips) on an instance.
--
-- /See:/ 'newAcceleratorCount' smart constructor.
data AcceleratorCount = AcceleratorCount'
  { -- | The maximum number of accelerators. If this parameter is not specified,
    -- there is no maximum limit.
    max :: Prelude.Maybe Prelude.Int,
    -- | The minimum number of accelerators. If this parameter is not specified,
    -- there is no minimum limit.
    min :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceleratorCount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'acceleratorCount_max' - The maximum number of accelerators. If this parameter is not specified,
-- there is no maximum limit.
--
-- 'min', 'acceleratorCount_min' - The minimum number of accelerators. If this parameter is not specified,
-- there is no minimum limit.
newAcceleratorCount ::
  AcceleratorCount
newAcceleratorCount =
  AcceleratorCount'
    { max = Prelude.Nothing,
      min = Prelude.Nothing
    }

-- | The maximum number of accelerators. If this parameter is not specified,
-- there is no maximum limit.
acceleratorCount_max :: Lens.Lens' AcceleratorCount (Prelude.Maybe Prelude.Int)
acceleratorCount_max = Lens.lens (\AcceleratorCount' {max} -> max) (\s@AcceleratorCount' {} a -> s {max = a} :: AcceleratorCount)

-- | The minimum number of accelerators. If this parameter is not specified,
-- there is no minimum limit.
acceleratorCount_min :: Lens.Lens' AcceleratorCount (Prelude.Maybe Prelude.Int)
acceleratorCount_min = Lens.lens (\AcceleratorCount' {min} -> min) (\s@AcceleratorCount' {} a -> s {min = a} :: AcceleratorCount)

instance Data.FromXML AcceleratorCount where
  parseXML x =
    AcceleratorCount'
      Prelude.<$> (x Data..@? "max") Prelude.<*> (x Data..@? "min")

instance Prelude.Hashable AcceleratorCount where
  hashWithSalt _salt AcceleratorCount' {..} =
    _salt `Prelude.hashWithSalt` max
      `Prelude.hashWithSalt` min

instance Prelude.NFData AcceleratorCount where
  rnf AcceleratorCount' {..} =
    Prelude.rnf max `Prelude.seq` Prelude.rnf min

instance Data.ToQuery AcceleratorCount where
  toQuery AcceleratorCount' {..} =
    Prelude.mconcat
      ["Max" Data.=: max, "Min" Data.=: min]
