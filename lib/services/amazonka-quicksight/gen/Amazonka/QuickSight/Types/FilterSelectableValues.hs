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
-- Module      : Amazonka.QuickSight.Types.FilterSelectableValues
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FilterSelectableValues where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A list of selectable values that are used in a control.
--
-- /See:/ 'newFilterSelectableValues' smart constructor.
data FilterSelectableValues = FilterSelectableValues'
  { -- | The values that are used in the @FilterSelectableValues@.
    values :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterSelectableValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'filterSelectableValues_values' - The values that are used in the @FilterSelectableValues@.
newFilterSelectableValues ::
  FilterSelectableValues
newFilterSelectableValues =
  FilterSelectableValues' {values = Prelude.Nothing}

-- | The values that are used in the @FilterSelectableValues@.
filterSelectableValues_values :: Lens.Lens' FilterSelectableValues (Prelude.Maybe [Prelude.Text])
filterSelectableValues_values = Lens.lens (\FilterSelectableValues' {values} -> values) (\s@FilterSelectableValues' {} a -> s {values = a} :: FilterSelectableValues) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON FilterSelectableValues where
  parseJSON =
    Data.withObject
      "FilterSelectableValues"
      ( \x ->
          FilterSelectableValues'
            Prelude.<$> (x Data..:? "Values" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable FilterSelectableValues where
  hashWithSalt _salt FilterSelectableValues' {..} =
    _salt `Prelude.hashWithSalt` values

instance Prelude.NFData FilterSelectableValues where
  rnf FilterSelectableValues' {..} = Prelude.rnf values

instance Data.ToJSON FilterSelectableValues where
  toJSON FilterSelectableValues' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Values" Data..=) Prelude.<$> values]
      )
