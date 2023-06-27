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
-- Module      : Amazonka.TimeStreamWrite.Types.Dimension
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.TimeStreamWrite.Types.Dimension where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.TimeStreamWrite.Types.DimensionValueType

-- | Represents the metadata attributes of the time series. For example, the
-- name and Availability Zone of an EC2 instance or the name of the
-- manufacturer of a wind turbine are dimensions.
--
-- /See:/ 'newDimension' smart constructor.
data Dimension = Dimension'
  { -- | The data type of the dimension for the time-series data point.
    dimensionValueType :: Prelude.Maybe DimensionValueType,
    -- | Dimension represents the metadata attributes of the time series. For
    -- example, the name and Availability Zone of an EC2 instance or the name
    -- of the manufacturer of a wind turbine are dimensions.
    --
    -- For constraints on dimension names, see
    -- <https://docs.aws.amazon.com/timestream/latest/developerguide/ts-limits.html#limits.naming Naming Constraints>.
    name :: Prelude.Text,
    -- | The value of the dimension.
    value :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Dimension' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensionValueType', 'dimension_dimensionValueType' - The data type of the dimension for the time-series data point.
--
-- 'name', 'dimension_name' - Dimension represents the metadata attributes of the time series. For
-- example, the name and Availability Zone of an EC2 instance or the name
-- of the manufacturer of a wind turbine are dimensions.
--
-- For constraints on dimension names, see
-- <https://docs.aws.amazon.com/timestream/latest/developerguide/ts-limits.html#limits.naming Naming Constraints>.
--
-- 'value', 'dimension_value' - The value of the dimension.
newDimension ::
  -- | 'name'
  Prelude.Text ->
  -- | 'value'
  Prelude.Text ->
  Dimension
newDimension pName_ pValue_ =
  Dimension'
    { dimensionValueType = Prelude.Nothing,
      name = pName_,
      value = pValue_
    }

-- | The data type of the dimension for the time-series data point.
dimension_dimensionValueType :: Lens.Lens' Dimension (Prelude.Maybe DimensionValueType)
dimension_dimensionValueType = Lens.lens (\Dimension' {dimensionValueType} -> dimensionValueType) (\s@Dimension' {} a -> s {dimensionValueType = a} :: Dimension)

-- | Dimension represents the metadata attributes of the time series. For
-- example, the name and Availability Zone of an EC2 instance or the name
-- of the manufacturer of a wind turbine are dimensions.
--
-- For constraints on dimension names, see
-- <https://docs.aws.amazon.com/timestream/latest/developerguide/ts-limits.html#limits.naming Naming Constraints>.
dimension_name :: Lens.Lens' Dimension Prelude.Text
dimension_name = Lens.lens (\Dimension' {name} -> name) (\s@Dimension' {} a -> s {name = a} :: Dimension)

-- | The value of the dimension.
dimension_value :: Lens.Lens' Dimension Prelude.Text
dimension_value = Lens.lens (\Dimension' {value} -> value) (\s@Dimension' {} a -> s {value = a} :: Dimension)

instance Prelude.Hashable Dimension where
  hashWithSalt _salt Dimension' {..} =
    _salt
      `Prelude.hashWithSalt` dimensionValueType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData Dimension where
  rnf Dimension' {..} =
    Prelude.rnf dimensionValueType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf value

instance Data.ToJSON Dimension where
  toJSON Dimension' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DimensionValueType" Data..=)
              Prelude.<$> dimensionValueType,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Value" Data..= value)
          ]
      )
