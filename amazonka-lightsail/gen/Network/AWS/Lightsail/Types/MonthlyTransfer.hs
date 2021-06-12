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
-- Module      : Network.AWS.Lightsail.Types.MonthlyTransfer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.MonthlyTransfer where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the monthly data transfer in and out of your virtual private
-- server (or /instance/).
--
-- /See:/ 'newMonthlyTransfer' smart constructor.
data MonthlyTransfer = MonthlyTransfer'
  { -- | The amount allocated per month (in GB).
    gbPerMonthAllocated :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MonthlyTransfer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gbPerMonthAllocated', 'monthlyTransfer_gbPerMonthAllocated' - The amount allocated per month (in GB).
newMonthlyTransfer ::
  MonthlyTransfer
newMonthlyTransfer =
  MonthlyTransfer'
    { gbPerMonthAllocated =
        Core.Nothing
    }

-- | The amount allocated per month (in GB).
monthlyTransfer_gbPerMonthAllocated :: Lens.Lens' MonthlyTransfer (Core.Maybe Core.Int)
monthlyTransfer_gbPerMonthAllocated = Lens.lens (\MonthlyTransfer' {gbPerMonthAllocated} -> gbPerMonthAllocated) (\s@MonthlyTransfer' {} a -> s {gbPerMonthAllocated = a} :: MonthlyTransfer)

instance Core.FromJSON MonthlyTransfer where
  parseJSON =
    Core.withObject
      "MonthlyTransfer"
      ( \x ->
          MonthlyTransfer'
            Core.<$> (x Core..:? "gbPerMonthAllocated")
      )

instance Core.Hashable MonthlyTransfer

instance Core.NFData MonthlyTransfer
