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
-- Module      : Amazonka.EC2.Types.VCpuCountRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VCpuCountRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The minimum and maximum number of vCPUs.
--
-- /See:/ 'newVCpuCountRange' smart constructor.
data VCpuCountRange = VCpuCountRange'
  { -- | The maximum number of vCPUs. If this parameter is not specified, there
    -- is no maximum limit.
    max :: Prelude.Maybe Prelude.Int,
    -- | The minimum number of vCPUs. If the value is @0@, there is no minimum
    -- limit.
    min :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VCpuCountRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'vCpuCountRange_max' - The maximum number of vCPUs. If this parameter is not specified, there
-- is no maximum limit.
--
-- 'min', 'vCpuCountRange_min' - The minimum number of vCPUs. If the value is @0@, there is no minimum
-- limit.
newVCpuCountRange ::
  VCpuCountRange
newVCpuCountRange =
  VCpuCountRange'
    { max = Prelude.Nothing,
      min = Prelude.Nothing
    }

-- | The maximum number of vCPUs. If this parameter is not specified, there
-- is no maximum limit.
vCpuCountRange_max :: Lens.Lens' VCpuCountRange (Prelude.Maybe Prelude.Int)
vCpuCountRange_max = Lens.lens (\VCpuCountRange' {max} -> max) (\s@VCpuCountRange' {} a -> s {max = a} :: VCpuCountRange)

-- | The minimum number of vCPUs. If the value is @0@, there is no minimum
-- limit.
vCpuCountRange_min :: Lens.Lens' VCpuCountRange (Prelude.Maybe Prelude.Int)
vCpuCountRange_min = Lens.lens (\VCpuCountRange' {min} -> min) (\s@VCpuCountRange' {} a -> s {min = a} :: VCpuCountRange)

instance Data.FromXML VCpuCountRange where
  parseXML x =
    VCpuCountRange'
      Prelude.<$> (x Data..@? "max")
      Prelude.<*> (x Data..@? "min")

instance Prelude.Hashable VCpuCountRange where
  hashWithSalt _salt VCpuCountRange' {..} =
    _salt
      `Prelude.hashWithSalt` max
      `Prelude.hashWithSalt` min

instance Prelude.NFData VCpuCountRange where
  rnf VCpuCountRange' {..} =
    Prelude.rnf max `Prelude.seq` Prelude.rnf min

instance Data.ToQuery VCpuCountRange where
  toQuery VCpuCountRange' {..} =
    Prelude.mconcat
      ["Max" Data.=: max, "Min" Data.=: min]
