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
-- Module      : Amazonka.QuickSight.Types.RangeConstant
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RangeConstant where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that represents a range constant.
--
-- /See:/ 'newRangeConstant' smart constructor.
data RangeConstant = RangeConstant'
  { -- | The maximum value for a range constant.
    maximum :: Prelude.Maybe Prelude.Text,
    -- | The minimum value for a range constant.
    minimum :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RangeConstant' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximum', 'rangeConstant_maximum' - The maximum value for a range constant.
--
-- 'minimum', 'rangeConstant_minimum' - The minimum value for a range constant.
newRangeConstant ::
  RangeConstant
newRangeConstant =
  RangeConstant'
    { maximum = Prelude.Nothing,
      minimum = Prelude.Nothing
    }

-- | The maximum value for a range constant.
rangeConstant_maximum :: Lens.Lens' RangeConstant (Prelude.Maybe Prelude.Text)
rangeConstant_maximum = Lens.lens (\RangeConstant' {maximum} -> maximum) (\s@RangeConstant' {} a -> s {maximum = a} :: RangeConstant)

-- | The minimum value for a range constant.
rangeConstant_minimum :: Lens.Lens' RangeConstant (Prelude.Maybe Prelude.Text)
rangeConstant_minimum = Lens.lens (\RangeConstant' {minimum} -> minimum) (\s@RangeConstant' {} a -> s {minimum = a} :: RangeConstant)

instance Data.FromJSON RangeConstant where
  parseJSON =
    Data.withObject
      "RangeConstant"
      ( \x ->
          RangeConstant'
            Prelude.<$> (x Data..:? "Maximum")
            Prelude.<*> (x Data..:? "Minimum")
      )

instance Prelude.Hashable RangeConstant where
  hashWithSalt _salt RangeConstant' {..} =
    _salt
      `Prelude.hashWithSalt` maximum
      `Prelude.hashWithSalt` minimum

instance Prelude.NFData RangeConstant where
  rnf RangeConstant' {..} =
    Prelude.rnf maximum
      `Prelude.seq` Prelude.rnf minimum

instance Data.ToJSON RangeConstant where
  toJSON RangeConstant' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Maximum" Data..=) Prelude.<$> maximum,
            ("Minimum" Data..=) Prelude.<$> minimum
          ]
      )
