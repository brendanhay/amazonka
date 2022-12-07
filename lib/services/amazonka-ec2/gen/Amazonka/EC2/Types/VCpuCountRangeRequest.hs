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
-- Module      : Amazonka.EC2.Types.VCpuCountRangeRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VCpuCountRangeRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The minimum and maximum number of vCPUs.
--
-- /See:/ 'newVCpuCountRangeRequest' smart constructor.
data VCpuCountRangeRequest = VCpuCountRangeRequest'
  { -- | The maximum number of vCPUs. To specify no maximum limit, omit this
    -- parameter.
    max :: Prelude.Maybe Prelude.Int,
    -- | The minimum number of vCPUs. To specify no minimum limit, specify @0@.
    min :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VCpuCountRangeRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'vCpuCountRangeRequest_max' - The maximum number of vCPUs. To specify no maximum limit, omit this
-- parameter.
--
-- 'min', 'vCpuCountRangeRequest_min' - The minimum number of vCPUs. To specify no minimum limit, specify @0@.
newVCpuCountRangeRequest ::
  -- | 'min'
  Prelude.Int ->
  VCpuCountRangeRequest
newVCpuCountRangeRequest pMin_ =
  VCpuCountRangeRequest'
    { max = Prelude.Nothing,
      min = pMin_
    }

-- | The maximum number of vCPUs. To specify no maximum limit, omit this
-- parameter.
vCpuCountRangeRequest_max :: Lens.Lens' VCpuCountRangeRequest (Prelude.Maybe Prelude.Int)
vCpuCountRangeRequest_max = Lens.lens (\VCpuCountRangeRequest' {max} -> max) (\s@VCpuCountRangeRequest' {} a -> s {max = a} :: VCpuCountRangeRequest)

-- | The minimum number of vCPUs. To specify no minimum limit, specify @0@.
vCpuCountRangeRequest_min :: Lens.Lens' VCpuCountRangeRequest Prelude.Int
vCpuCountRangeRequest_min = Lens.lens (\VCpuCountRangeRequest' {min} -> min) (\s@VCpuCountRangeRequest' {} a -> s {min = a} :: VCpuCountRangeRequest)

instance Prelude.Hashable VCpuCountRangeRequest where
  hashWithSalt _salt VCpuCountRangeRequest' {..} =
    _salt `Prelude.hashWithSalt` max
      `Prelude.hashWithSalt` min

instance Prelude.NFData VCpuCountRangeRequest where
  rnf VCpuCountRangeRequest' {..} =
    Prelude.rnf max `Prelude.seq` Prelude.rnf min

instance Data.ToQuery VCpuCountRangeRequest where
  toQuery VCpuCountRangeRequest' {..} =
    Prelude.mconcat
      ["Max" Data.=: max, "Min" Data.=: min]
