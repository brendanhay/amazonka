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
-- Module      : Amazonka.AutoScaling.Types.VCpuCountRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.VCpuCountRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the minimum and maximum for the @VCpuCount@ object when you
-- specify InstanceRequirements for an Auto Scaling group.
--
-- /See:/ 'newVCpuCountRequest' smart constructor.
data VCpuCountRequest = VCpuCountRequest'
  { -- | The maximum number of vCPUs.
    max :: Prelude.Maybe Prelude.Natural,
    -- | The minimum number of vCPUs.
    min :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VCpuCountRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'vCpuCountRequest_max' - The maximum number of vCPUs.
--
-- 'min', 'vCpuCountRequest_min' - The minimum number of vCPUs.
newVCpuCountRequest ::
  -- | 'min'
  Prelude.Natural ->
  VCpuCountRequest
newVCpuCountRequest pMin_ =
  VCpuCountRequest'
    { max = Prelude.Nothing,
      min = pMin_
    }

-- | The maximum number of vCPUs.
vCpuCountRequest_max :: Lens.Lens' VCpuCountRequest (Prelude.Maybe Prelude.Natural)
vCpuCountRequest_max = Lens.lens (\VCpuCountRequest' {max} -> max) (\s@VCpuCountRequest' {} a -> s {max = a} :: VCpuCountRequest)

-- | The minimum number of vCPUs.
vCpuCountRequest_min :: Lens.Lens' VCpuCountRequest Prelude.Natural
vCpuCountRequest_min = Lens.lens (\VCpuCountRequest' {min} -> min) (\s@VCpuCountRequest' {} a -> s {min = a} :: VCpuCountRequest)

instance Data.FromXML VCpuCountRequest where
  parseXML x =
    VCpuCountRequest'
      Prelude.<$> (x Data..@? "Max")
      Prelude.<*> (x Data..@ "Min")

instance Prelude.Hashable VCpuCountRequest where
  hashWithSalt _salt VCpuCountRequest' {..} =
    _salt
      `Prelude.hashWithSalt` max
      `Prelude.hashWithSalt` min

instance Prelude.NFData VCpuCountRequest where
  rnf VCpuCountRequest' {..} =
    Prelude.rnf max `Prelude.seq` Prelude.rnf min

instance Data.ToQuery VCpuCountRequest where
  toQuery VCpuCountRequest' {..} =
    Prelude.mconcat
      ["Max" Data.=: max, "Min" Data.=: min]
