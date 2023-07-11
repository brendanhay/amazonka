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
-- Module      : Amazonka.Glue.Types.FilterValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.FilterValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.FilterValueType
import qualified Amazonka.Prelude as Prelude

-- | Represents a single entry in the list of values for a
-- @FilterExpression@.
--
-- /See:/ 'newFilterValue' smart constructor.
data FilterValue = FilterValue'
  { -- | The type of filter value.
    type' :: FilterValueType,
    -- | The value to be associated.
    value :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'filterValue_type' - The type of filter value.
--
-- 'value', 'filterValue_value' - The value to be associated.
newFilterValue ::
  -- | 'type''
  FilterValueType ->
  FilterValue
newFilterValue pType_ =
  FilterValue'
    { type' = pType_,
      value = Prelude.mempty
    }

-- | The type of filter value.
filterValue_type :: Lens.Lens' FilterValue FilterValueType
filterValue_type = Lens.lens (\FilterValue' {type'} -> type') (\s@FilterValue' {} a -> s {type' = a} :: FilterValue)

-- | The value to be associated.
filterValue_value :: Lens.Lens' FilterValue [Prelude.Text]
filterValue_value = Lens.lens (\FilterValue' {value} -> value) (\s@FilterValue' {} a -> s {value = a} :: FilterValue) Prelude.. Lens.coerced

instance Data.FromJSON FilterValue where
  parseJSON =
    Data.withObject
      "FilterValue"
      ( \x ->
          FilterValue'
            Prelude.<$> (x Data..: "Type")
            Prelude.<*> (x Data..:? "Value" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable FilterValue where
  hashWithSalt _salt FilterValue' {..} =
    _salt
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` value

instance Prelude.NFData FilterValue where
  rnf FilterValue' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf value

instance Data.ToJSON FilterValue where
  toJSON FilterValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Type" Data..= type'),
            Prelude.Just ("Value" Data..= value)
          ]
      )
