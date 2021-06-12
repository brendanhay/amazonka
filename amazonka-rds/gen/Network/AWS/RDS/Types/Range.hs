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
-- Module      : Network.AWS.RDS.Types.Range
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.Range where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A range of integer values.
--
-- /See:/ 'newRange' smart constructor.
data Range = Range'
  { -- | The maximum value in the range.
    to :: Core.Maybe Core.Int,
    -- | The minimum value in the range.
    from :: Core.Maybe Core.Int,
    -- | The step value for the range. For example, if you have a range of 5,000
    -- to 10,000, with a step value of 1,000, the valid values start at 5,000
    -- and step up by 1,000. Even though 7,500 is within the range, it isn\'t a
    -- valid value for the range. The valid values are 5,000, 6,000, 7,000,
    -- 8,000...
    step :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Range' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'to', 'range_to' - The maximum value in the range.
--
-- 'from', 'range_from' - The minimum value in the range.
--
-- 'step', 'range_step' - The step value for the range. For example, if you have a range of 5,000
-- to 10,000, with a step value of 1,000, the valid values start at 5,000
-- and step up by 1,000. Even though 7,500 is within the range, it isn\'t a
-- valid value for the range. The valid values are 5,000, 6,000, 7,000,
-- 8,000...
newRange ::
  Range
newRange =
  Range'
    { to = Core.Nothing,
      from = Core.Nothing,
      step = Core.Nothing
    }

-- | The maximum value in the range.
range_to :: Lens.Lens' Range (Core.Maybe Core.Int)
range_to = Lens.lens (\Range' {to} -> to) (\s@Range' {} a -> s {to = a} :: Range)

-- | The minimum value in the range.
range_from :: Lens.Lens' Range (Core.Maybe Core.Int)
range_from = Lens.lens (\Range' {from} -> from) (\s@Range' {} a -> s {from = a} :: Range)

-- | The step value for the range. For example, if you have a range of 5,000
-- to 10,000, with a step value of 1,000, the valid values start at 5,000
-- and step up by 1,000. Even though 7,500 is within the range, it isn\'t a
-- valid value for the range. The valid values are 5,000, 6,000, 7,000,
-- 8,000...
range_step :: Lens.Lens' Range (Core.Maybe Core.Int)
range_step = Lens.lens (\Range' {step} -> step) (\s@Range' {} a -> s {step = a} :: Range)

instance Core.FromXML Range where
  parseXML x =
    Range'
      Core.<$> (x Core..@? "To")
      Core.<*> (x Core..@? "From")
      Core.<*> (x Core..@? "Step")

instance Core.Hashable Range

instance Core.NFData Range
