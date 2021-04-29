{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A range of double values.
--
-- /See:/ 'newDoubleRange' smart constructor.
data DoubleRange = DoubleRange'
  { -- | The maximum value in the range.
    to :: Prelude.Maybe Prelude.Double,
    -- | The minimum value in the range.
    from :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { to = Prelude.Nothing,
      from = Prelude.Nothing
    }

-- | The maximum value in the range.
doubleRange_to :: Lens.Lens' DoubleRange (Prelude.Maybe Prelude.Double)
doubleRange_to = Lens.lens (\DoubleRange' {to} -> to) (\s@DoubleRange' {} a -> s {to = a} :: DoubleRange)

-- | The minimum value in the range.
doubleRange_from :: Lens.Lens' DoubleRange (Prelude.Maybe Prelude.Double)
doubleRange_from = Lens.lens (\DoubleRange' {from} -> from) (\s@DoubleRange' {} a -> s {from = a} :: DoubleRange)

instance Prelude.FromXML DoubleRange where
  parseXML x =
    DoubleRange'
      Prelude.<$> (x Prelude..@? "To")
      Prelude.<*> (x Prelude..@? "From")

instance Prelude.Hashable DoubleRange

instance Prelude.NFData DoubleRange
