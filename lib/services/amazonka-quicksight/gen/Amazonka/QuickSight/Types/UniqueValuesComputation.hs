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
-- Module      : Amazonka.QuickSight.Types.UniqueValuesComputation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.UniqueValuesComputation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DimensionField

-- | The unique values computation configuration.
--
-- /See:/ 'newUniqueValuesComputation' smart constructor.
data UniqueValuesComputation = UniqueValuesComputation'
  { -- | The name of a computation.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID for a computation.
    computationId :: Prelude.Text,
    -- | The category field that is used in a computation.
    category :: DimensionField
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UniqueValuesComputation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'uniqueValuesComputation_name' - The name of a computation.
--
-- 'computationId', 'uniqueValuesComputation_computationId' - The ID for a computation.
--
-- 'category', 'uniqueValuesComputation_category' - The category field that is used in a computation.
newUniqueValuesComputation ::
  -- | 'computationId'
  Prelude.Text ->
  -- | 'category'
  DimensionField ->
  UniqueValuesComputation
newUniqueValuesComputation pComputationId_ pCategory_ =
  UniqueValuesComputation'
    { name = Prelude.Nothing,
      computationId = pComputationId_,
      category = pCategory_
    }

-- | The name of a computation.
uniqueValuesComputation_name :: Lens.Lens' UniqueValuesComputation (Prelude.Maybe Prelude.Text)
uniqueValuesComputation_name = Lens.lens (\UniqueValuesComputation' {name} -> name) (\s@UniqueValuesComputation' {} a -> s {name = a} :: UniqueValuesComputation)

-- | The ID for a computation.
uniqueValuesComputation_computationId :: Lens.Lens' UniqueValuesComputation Prelude.Text
uniqueValuesComputation_computationId = Lens.lens (\UniqueValuesComputation' {computationId} -> computationId) (\s@UniqueValuesComputation' {} a -> s {computationId = a} :: UniqueValuesComputation)

-- | The category field that is used in a computation.
uniqueValuesComputation_category :: Lens.Lens' UniqueValuesComputation DimensionField
uniqueValuesComputation_category = Lens.lens (\UniqueValuesComputation' {category} -> category) (\s@UniqueValuesComputation' {} a -> s {category = a} :: UniqueValuesComputation)

instance Data.FromJSON UniqueValuesComputation where
  parseJSON =
    Data.withObject
      "UniqueValuesComputation"
      ( \x ->
          UniqueValuesComputation'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..: "ComputationId")
            Prelude.<*> (x Data..: "Category")
      )

instance Prelude.Hashable UniqueValuesComputation where
  hashWithSalt _salt UniqueValuesComputation' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` computationId
      `Prelude.hashWithSalt` category

instance Prelude.NFData UniqueValuesComputation where
  rnf UniqueValuesComputation' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf computationId
      `Prelude.seq` Prelude.rnf category

instance Data.ToJSON UniqueValuesComputation where
  toJSON UniqueValuesComputation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            Prelude.Just ("ComputationId" Data..= computationId),
            Prelude.Just ("Category" Data..= category)
          ]
      )
