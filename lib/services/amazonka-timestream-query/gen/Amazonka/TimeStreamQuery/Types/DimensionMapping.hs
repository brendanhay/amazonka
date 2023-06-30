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
-- Module      : Amazonka.TimeStreamQuery.Types.DimensionMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamQuery.Types.DimensionMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TimeStreamQuery.Types.DimensionValueType

-- | This type is used to map column(s) from the query result to a dimension
-- in the destination table.
--
-- /See:/ 'newDimensionMapping' smart constructor.
data DimensionMapping = DimensionMapping'
  { -- | Column name from query result.
    name :: Prelude.Text,
    -- | Type for the dimension.
    dimensionValueType :: DimensionValueType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DimensionMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'dimensionMapping_name' - Column name from query result.
--
-- 'dimensionValueType', 'dimensionMapping_dimensionValueType' - Type for the dimension.
newDimensionMapping ::
  -- | 'name'
  Prelude.Text ->
  -- | 'dimensionValueType'
  DimensionValueType ->
  DimensionMapping
newDimensionMapping pName_ pDimensionValueType_ =
  DimensionMapping'
    { name = pName_,
      dimensionValueType = pDimensionValueType_
    }

-- | Column name from query result.
dimensionMapping_name :: Lens.Lens' DimensionMapping Prelude.Text
dimensionMapping_name = Lens.lens (\DimensionMapping' {name} -> name) (\s@DimensionMapping' {} a -> s {name = a} :: DimensionMapping)

-- | Type for the dimension.
dimensionMapping_dimensionValueType :: Lens.Lens' DimensionMapping DimensionValueType
dimensionMapping_dimensionValueType = Lens.lens (\DimensionMapping' {dimensionValueType} -> dimensionValueType) (\s@DimensionMapping' {} a -> s {dimensionValueType = a} :: DimensionMapping)

instance Data.FromJSON DimensionMapping where
  parseJSON =
    Data.withObject
      "DimensionMapping"
      ( \x ->
          DimensionMapping'
            Prelude.<$> (x Data..: "Name")
            Prelude.<*> (x Data..: "DimensionValueType")
      )

instance Prelude.Hashable DimensionMapping where
  hashWithSalt _salt DimensionMapping' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` dimensionValueType

instance Prelude.NFData DimensionMapping where
  rnf DimensionMapping' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf dimensionValueType

instance Data.ToJSON DimensionMapping where
  toJSON DimensionMapping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("DimensionValueType" Data..= dimensionValueType)
          ]
      )
