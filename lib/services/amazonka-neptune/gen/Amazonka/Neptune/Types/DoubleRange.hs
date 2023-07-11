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
-- Module      : Amazonka.Neptune.Types.DoubleRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Neptune.Types.DoubleRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A range of double values.
--
-- /See:/ 'newDoubleRange' smart constructor.
data DoubleRange = DoubleRange'
  { -- | The minimum value in the range.
    from :: Prelude.Maybe Prelude.Double,
    -- | The maximum value in the range.
    to :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DoubleRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'from', 'doubleRange_from' - The minimum value in the range.
--
-- 'to', 'doubleRange_to' - The maximum value in the range.
newDoubleRange ::
  DoubleRange
newDoubleRange =
  DoubleRange'
    { from = Prelude.Nothing,
      to = Prelude.Nothing
    }

-- | The minimum value in the range.
doubleRange_from :: Lens.Lens' DoubleRange (Prelude.Maybe Prelude.Double)
doubleRange_from = Lens.lens (\DoubleRange' {from} -> from) (\s@DoubleRange' {} a -> s {from = a} :: DoubleRange)

-- | The maximum value in the range.
doubleRange_to :: Lens.Lens' DoubleRange (Prelude.Maybe Prelude.Double)
doubleRange_to = Lens.lens (\DoubleRange' {to} -> to) (\s@DoubleRange' {} a -> s {to = a} :: DoubleRange)

instance Data.FromXML DoubleRange where
  parseXML x =
    DoubleRange'
      Prelude.<$> (x Data..@? "From")
      Prelude.<*> (x Data..@? "To")

instance Prelude.Hashable DoubleRange where
  hashWithSalt _salt DoubleRange' {..} =
    _salt
      `Prelude.hashWithSalt` from
      `Prelude.hashWithSalt` to

instance Prelude.NFData DoubleRange where
  rnf DoubleRange' {..} =
    Prelude.rnf from `Prelude.seq` Prelude.rnf to
