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
-- Module      : Amazonka.SecurityHub.Types.BooleanFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.BooleanFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Boolean filter for querying findings.
--
-- /See:/ 'newBooleanFilter' smart constructor.
data BooleanFilter = BooleanFilter'
  { -- | The value of the boolean.
    value :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BooleanFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'booleanFilter_value' - The value of the boolean.
newBooleanFilter ::
  BooleanFilter
newBooleanFilter =
  BooleanFilter' {value = Prelude.Nothing}

-- | The value of the boolean.
booleanFilter_value :: Lens.Lens' BooleanFilter (Prelude.Maybe Prelude.Bool)
booleanFilter_value = Lens.lens (\BooleanFilter' {value} -> value) (\s@BooleanFilter' {} a -> s {value = a} :: BooleanFilter)

instance Data.FromJSON BooleanFilter where
  parseJSON =
    Data.withObject
      "BooleanFilter"
      ( \x ->
          BooleanFilter' Prelude.<$> (x Data..:? "Value")
      )

instance Prelude.Hashable BooleanFilter where
  hashWithSalt _salt BooleanFilter' {..} =
    _salt `Prelude.hashWithSalt` value

instance Prelude.NFData BooleanFilter where
  rnf BooleanFilter' {..} = Prelude.rnf value

instance Data.ToJSON BooleanFilter where
  toJSON BooleanFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Value" Data..=) Prelude.<$> value]
      )
