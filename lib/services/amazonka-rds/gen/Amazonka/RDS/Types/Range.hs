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
-- Module      : Amazonka.RDS.Types.Range
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.Range where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A range of integer values.
--
-- /See:/ 'newRange' smart constructor.
data Range = Range'
  { -- | The minimum value in the range.
    from :: Prelude.Maybe Prelude.Int,
    -- | The step value for the range. For example, if you have a range of 5,000
    -- to 10,000, with a step value of 1,000, the valid values start at 5,000
    -- and step up by 1,000. Even though 7,500 is within the range, it isn\'t a
    -- valid value for the range. The valid values are 5,000, 6,000, 7,000,
    -- 8,000...
    step :: Prelude.Maybe Prelude.Int,
    -- | The maximum value in the range.
    to :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Range' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'from', 'range_from' - The minimum value in the range.
--
-- 'step', 'range_step' - The step value for the range. For example, if you have a range of 5,000
-- to 10,000, with a step value of 1,000, the valid values start at 5,000
-- and step up by 1,000. Even though 7,500 is within the range, it isn\'t a
-- valid value for the range. The valid values are 5,000, 6,000, 7,000,
-- 8,000...
--
-- 'to', 'range_to' - The maximum value in the range.
newRange ::
  Range
newRange =
  Range'
    { from = Prelude.Nothing,
      step = Prelude.Nothing,
      to = Prelude.Nothing
    }

-- | The minimum value in the range.
range_from :: Lens.Lens' Range (Prelude.Maybe Prelude.Int)
range_from = Lens.lens (\Range' {from} -> from) (\s@Range' {} a -> s {from = a} :: Range)

-- | The step value for the range. For example, if you have a range of 5,000
-- to 10,000, with a step value of 1,000, the valid values start at 5,000
-- and step up by 1,000. Even though 7,500 is within the range, it isn\'t a
-- valid value for the range. The valid values are 5,000, 6,000, 7,000,
-- 8,000...
range_step :: Lens.Lens' Range (Prelude.Maybe Prelude.Int)
range_step = Lens.lens (\Range' {step} -> step) (\s@Range' {} a -> s {step = a} :: Range)

-- | The maximum value in the range.
range_to :: Lens.Lens' Range (Prelude.Maybe Prelude.Int)
range_to = Lens.lens (\Range' {to} -> to) (\s@Range' {} a -> s {to = a} :: Range)

instance Data.FromXML Range where
  parseXML x =
    Range'
      Prelude.<$> (x Data..@? "From")
      Prelude.<*> (x Data..@? "Step")
      Prelude.<*> (x Data..@? "To")

instance Prelude.Hashable Range where
  hashWithSalt _salt Range' {..} =
    _salt
      `Prelude.hashWithSalt` from
      `Prelude.hashWithSalt` step
      `Prelude.hashWithSalt` to

instance Prelude.NFData Range where
  rnf Range' {..} =
    Prelude.rnf from `Prelude.seq`
      Prelude.rnf step `Prelude.seq`
        Prelude.rnf to
