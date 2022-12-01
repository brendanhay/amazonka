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
-- Module      : Amazonka.AutoScaling.Types.BaselineEbsBandwidthMbpsRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.BaselineEbsBandwidthMbpsRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies the minimum and maximum for the @BaselineEbsBandwidthMbps@
-- object when you specify InstanceRequirements for an Auto Scaling group.
--
-- /See:/ 'newBaselineEbsBandwidthMbpsRequest' smart constructor.
data BaselineEbsBandwidthMbpsRequest = BaselineEbsBandwidthMbpsRequest'
  { -- | The maximum value in Mbps.
    max :: Prelude.Maybe Prelude.Natural,
    -- | The minimum value in Mbps.
    min :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BaselineEbsBandwidthMbpsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'baselineEbsBandwidthMbpsRequest_max' - The maximum value in Mbps.
--
-- 'min', 'baselineEbsBandwidthMbpsRequest_min' - The minimum value in Mbps.
newBaselineEbsBandwidthMbpsRequest ::
  BaselineEbsBandwidthMbpsRequest
newBaselineEbsBandwidthMbpsRequest =
  BaselineEbsBandwidthMbpsRequest'
    { max =
        Prelude.Nothing,
      min = Prelude.Nothing
    }

-- | The maximum value in Mbps.
baselineEbsBandwidthMbpsRequest_max :: Lens.Lens' BaselineEbsBandwidthMbpsRequest (Prelude.Maybe Prelude.Natural)
baselineEbsBandwidthMbpsRequest_max = Lens.lens (\BaselineEbsBandwidthMbpsRequest' {max} -> max) (\s@BaselineEbsBandwidthMbpsRequest' {} a -> s {max = a} :: BaselineEbsBandwidthMbpsRequest)

-- | The minimum value in Mbps.
baselineEbsBandwidthMbpsRequest_min :: Lens.Lens' BaselineEbsBandwidthMbpsRequest (Prelude.Maybe Prelude.Natural)
baselineEbsBandwidthMbpsRequest_min = Lens.lens (\BaselineEbsBandwidthMbpsRequest' {min} -> min) (\s@BaselineEbsBandwidthMbpsRequest' {} a -> s {min = a} :: BaselineEbsBandwidthMbpsRequest)

instance Core.FromXML BaselineEbsBandwidthMbpsRequest where
  parseXML x =
    BaselineEbsBandwidthMbpsRequest'
      Prelude.<$> (x Core..@? "Max") Prelude.<*> (x Core..@? "Min")

instance
  Prelude.Hashable
    BaselineEbsBandwidthMbpsRequest
  where
  hashWithSalt
    _salt
    BaselineEbsBandwidthMbpsRequest' {..} =
      _salt `Prelude.hashWithSalt` max
        `Prelude.hashWithSalt` min

instance
  Prelude.NFData
    BaselineEbsBandwidthMbpsRequest
  where
  rnf BaselineEbsBandwidthMbpsRequest' {..} =
    Prelude.rnf max `Prelude.seq` Prelude.rnf min

instance Core.ToQuery BaselineEbsBandwidthMbpsRequest where
  toQuery BaselineEbsBandwidthMbpsRequest' {..} =
    Prelude.mconcat
      ["Max" Core.=: max, "Min" Core.=: min]
