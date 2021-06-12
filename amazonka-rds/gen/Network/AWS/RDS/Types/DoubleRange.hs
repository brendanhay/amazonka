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
-- Module      : Network.AWS.RDS.Types.DoubleRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DoubleRange where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A range of double values.
--
-- /See:/ 'newDoubleRange' smart constructor.
data DoubleRange = DoubleRange'
  { -- | The maximum value in the range.
    to :: Core.Maybe Core.Double,
    -- | The minimum value in the range.
    from :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DoubleRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'to', 'doubleRange_to' - The maximum value in the range.
--
-- 'from', 'doubleRange_from' - The minimum value in the range.
newDoubleRange ::
  DoubleRange
newDoubleRange =
  DoubleRange'
    { to = Core.Nothing,
      from = Core.Nothing
    }

-- | The maximum value in the range.
doubleRange_to :: Lens.Lens' DoubleRange (Core.Maybe Core.Double)
doubleRange_to = Lens.lens (\DoubleRange' {to} -> to) (\s@DoubleRange' {} a -> s {to = a} :: DoubleRange)

-- | The minimum value in the range.
doubleRange_from :: Lens.Lens' DoubleRange (Core.Maybe Core.Double)
doubleRange_from = Lens.lens (\DoubleRange' {from} -> from) (\s@DoubleRange' {} a -> s {from = a} :: DoubleRange)

instance Core.FromXML DoubleRange where
  parseXML x =
    DoubleRange'
      Core.<$> (x Core..@? "To") Core.<*> (x Core..@? "From")

instance Core.Hashable DoubleRange

instance Core.NFData DoubleRange
