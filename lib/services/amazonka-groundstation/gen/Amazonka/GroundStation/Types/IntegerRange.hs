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
-- Module      : Amazonka.GroundStation.Types.IntegerRange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.IntegerRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An integer range that has a minimum and maximum value.
--
-- /See:/ 'newIntegerRange' smart constructor.
data IntegerRange = IntegerRange'
  { -- | A maximum value.
    maximum :: Prelude.Int,
    -- | A minimum value.
    minimum :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IntegerRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximum', 'integerRange_maximum' - A maximum value.
--
-- 'minimum', 'integerRange_minimum' - A minimum value.
newIntegerRange ::
  -- | 'maximum'
  Prelude.Int ->
  -- | 'minimum'
  Prelude.Int ->
  IntegerRange
newIntegerRange pMaximum_ pMinimum_ =
  IntegerRange'
    { maximum = pMaximum_,
      minimum = pMinimum_
    }

-- | A maximum value.
integerRange_maximum :: Lens.Lens' IntegerRange Prelude.Int
integerRange_maximum = Lens.lens (\IntegerRange' {maximum} -> maximum) (\s@IntegerRange' {} a -> s {maximum = a} :: IntegerRange)

-- | A minimum value.
integerRange_minimum :: Lens.Lens' IntegerRange Prelude.Int
integerRange_minimum = Lens.lens (\IntegerRange' {minimum} -> minimum) (\s@IntegerRange' {} a -> s {minimum = a} :: IntegerRange)

instance Data.FromJSON IntegerRange where
  parseJSON =
    Data.withObject
      "IntegerRange"
      ( \x ->
          IntegerRange'
            Prelude.<$> (x Data..: "maximum")
            Prelude.<*> (x Data..: "minimum")
      )

instance Prelude.Hashable IntegerRange where
  hashWithSalt _salt IntegerRange' {..} =
    _salt
      `Prelude.hashWithSalt` maximum
      `Prelude.hashWithSalt` minimum

instance Prelude.NFData IntegerRange where
  rnf IntegerRange' {..} =
    Prelude.rnf maximum
      `Prelude.seq` Prelude.rnf minimum

instance Data.ToJSON IntegerRange where
  toJSON IntegerRange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("maximum" Data..= maximum),
            Prelude.Just ("minimum" Data..= minimum)
          ]
      )
