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
-- Module      : Amazonka.IotTwinMaker.Types.DataType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.DataType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types.DataValue
import Amazonka.IotTwinMaker.Types.Relationship
import Amazonka.IotTwinMaker.Types.Type
import qualified Amazonka.Prelude as Prelude

-- | An object that specifies the data type of a property.
--
-- /See:/ 'newDataType' smart constructor.
data DataType = DataType'
  { -- | The allowed values for this data type.
    allowedValues :: Prelude.Maybe [DataValue],
    -- | The nested type in the data type.
    nestedType :: Prelude.Maybe DataType,
    -- | A relationship that associates a component with another component.
    relationship :: Prelude.Maybe Relationship,
    -- | The unit of measure used in this data type.
    unitOfMeasure :: Prelude.Maybe Prelude.Text,
    -- | The underlying type of the data type.
    type' :: Type
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowedValues', 'dataType_allowedValues' - The allowed values for this data type.
--
-- 'nestedType', 'dataType_nestedType' - The nested type in the data type.
--
-- 'relationship', 'dataType_relationship' - A relationship that associates a component with another component.
--
-- 'unitOfMeasure', 'dataType_unitOfMeasure' - The unit of measure used in this data type.
--
-- 'type'', 'dataType_type' - The underlying type of the data type.
newDataType ::
  -- | 'type''
  Type ->
  DataType
newDataType pType_ =
  DataType'
    { allowedValues = Prelude.Nothing,
      nestedType = Prelude.Nothing,
      relationship = Prelude.Nothing,
      unitOfMeasure = Prelude.Nothing,
      type' = pType_
    }

-- | The allowed values for this data type.
dataType_allowedValues :: Lens.Lens' DataType (Prelude.Maybe [DataValue])
dataType_allowedValues = Lens.lens (\DataType' {allowedValues} -> allowedValues) (\s@DataType' {} a -> s {allowedValues = a} :: DataType) Prelude.. Lens.mapping Lens.coerced

-- | The nested type in the data type.
dataType_nestedType :: Lens.Lens' DataType (Prelude.Maybe DataType)
dataType_nestedType = Lens.lens (\DataType' {nestedType} -> nestedType) (\s@DataType' {} a -> s {nestedType = a} :: DataType)

-- | A relationship that associates a component with another component.
dataType_relationship :: Lens.Lens' DataType (Prelude.Maybe Relationship)
dataType_relationship = Lens.lens (\DataType' {relationship} -> relationship) (\s@DataType' {} a -> s {relationship = a} :: DataType)

-- | The unit of measure used in this data type.
dataType_unitOfMeasure :: Lens.Lens' DataType (Prelude.Maybe Prelude.Text)
dataType_unitOfMeasure = Lens.lens (\DataType' {unitOfMeasure} -> unitOfMeasure) (\s@DataType' {} a -> s {unitOfMeasure = a} :: DataType)

-- | The underlying type of the data type.
dataType_type :: Lens.Lens' DataType Type
dataType_type = Lens.lens (\DataType' {type'} -> type') (\s@DataType' {} a -> s {type' = a} :: DataType)

instance Data.FromJSON DataType where
  parseJSON =
    Data.withObject
      "DataType"
      ( \x ->
          DataType'
            Prelude.<$> (x Data..:? "allowedValues" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "nestedType")
            Prelude.<*> (x Data..:? "relationship")
            Prelude.<*> (x Data..:? "unitOfMeasure")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable DataType where
  hashWithSalt _salt DataType' {..} =
    _salt `Prelude.hashWithSalt` allowedValues
      `Prelude.hashWithSalt` nestedType
      `Prelude.hashWithSalt` relationship
      `Prelude.hashWithSalt` unitOfMeasure
      `Prelude.hashWithSalt` type'

instance Prelude.NFData DataType where
  rnf DataType' {..} =
    Prelude.rnf allowedValues
      `Prelude.seq` Prelude.rnf nestedType
      `Prelude.seq` Prelude.rnf relationship
      `Prelude.seq` Prelude.rnf unitOfMeasure
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON DataType where
  toJSON DataType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("allowedValues" Data..=) Prelude.<$> allowedValues,
            ("nestedType" Data..=) Prelude.<$> nestedType,
            ("relationship" Data..=) Prelude.<$> relationship,
            ("unitOfMeasure" Data..=) Prelude.<$> unitOfMeasure,
            Prelude.Just ("type" Data..= type')
          ]
      )
