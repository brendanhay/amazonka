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
-- Module      : Amazonka.IoTFleetWise.Types.Attribute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.Attribute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTFleetWise.Types.NodeDataType
import qualified Amazonka.Prelude as Prelude

-- | A signal that represents static information about the vehicle, such as
-- engine type or manufacturing date.
--
-- /See:/ 'newAttribute' smart constructor.
data Attribute = Attribute'
  { -- | The specified possible maximum value of the attribute.
    max :: Prelude.Maybe Prelude.Double,
    -- | The default value of the attribute.
    defaultValue :: Prelude.Maybe Prelude.Text,
    -- | A brief description of the attribute.
    description :: Prelude.Maybe Prelude.Text,
    -- | The specified possible minimum value of the attribute.
    min :: Prelude.Maybe Prelude.Double,
    -- | A specified value for the attribute.
    assignedValue :: Prelude.Maybe Prelude.Text,
    -- | A list of possible values an attribute can be assigned.
    allowedValues :: Prelude.Maybe [Prelude.Text],
    -- | The scientific unit for the attribute.
    unit :: Prelude.Maybe Prelude.Text,
    -- | The fully qualified name of the attribute. For example, the fully
    -- qualified name of an attribute might be @Vehicle.Body.Engine.Type@.
    fullyQualifiedName :: Prelude.Text,
    -- | The specified data type of the attribute.
    dataType :: NodeDataType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Attribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'attribute_max' - The specified possible maximum value of the attribute.
--
-- 'defaultValue', 'attribute_defaultValue' - The default value of the attribute.
--
-- 'description', 'attribute_description' - A brief description of the attribute.
--
-- 'min', 'attribute_min' - The specified possible minimum value of the attribute.
--
-- 'assignedValue', 'attribute_assignedValue' - A specified value for the attribute.
--
-- 'allowedValues', 'attribute_allowedValues' - A list of possible values an attribute can be assigned.
--
-- 'unit', 'attribute_unit' - The scientific unit for the attribute.
--
-- 'fullyQualifiedName', 'attribute_fullyQualifiedName' - The fully qualified name of the attribute. For example, the fully
-- qualified name of an attribute might be @Vehicle.Body.Engine.Type@.
--
-- 'dataType', 'attribute_dataType' - The specified data type of the attribute.
newAttribute ::
  -- | 'fullyQualifiedName'
  Prelude.Text ->
  -- | 'dataType'
  NodeDataType ->
  Attribute
newAttribute pFullyQualifiedName_ pDataType_ =
  Attribute'
    { max = Prelude.Nothing,
      defaultValue = Prelude.Nothing,
      description = Prelude.Nothing,
      min = Prelude.Nothing,
      assignedValue = Prelude.Nothing,
      allowedValues = Prelude.Nothing,
      unit = Prelude.Nothing,
      fullyQualifiedName = pFullyQualifiedName_,
      dataType = pDataType_
    }

-- | The specified possible maximum value of the attribute.
attribute_max :: Lens.Lens' Attribute (Prelude.Maybe Prelude.Double)
attribute_max = Lens.lens (\Attribute' {max} -> max) (\s@Attribute' {} a -> s {max = a} :: Attribute)

-- | The default value of the attribute.
attribute_defaultValue :: Lens.Lens' Attribute (Prelude.Maybe Prelude.Text)
attribute_defaultValue = Lens.lens (\Attribute' {defaultValue} -> defaultValue) (\s@Attribute' {} a -> s {defaultValue = a} :: Attribute)

-- | A brief description of the attribute.
attribute_description :: Lens.Lens' Attribute (Prelude.Maybe Prelude.Text)
attribute_description = Lens.lens (\Attribute' {description} -> description) (\s@Attribute' {} a -> s {description = a} :: Attribute)

-- | The specified possible minimum value of the attribute.
attribute_min :: Lens.Lens' Attribute (Prelude.Maybe Prelude.Double)
attribute_min = Lens.lens (\Attribute' {min} -> min) (\s@Attribute' {} a -> s {min = a} :: Attribute)

-- | A specified value for the attribute.
attribute_assignedValue :: Lens.Lens' Attribute (Prelude.Maybe Prelude.Text)
attribute_assignedValue = Lens.lens (\Attribute' {assignedValue} -> assignedValue) (\s@Attribute' {} a -> s {assignedValue = a} :: Attribute)

-- | A list of possible values an attribute can be assigned.
attribute_allowedValues :: Lens.Lens' Attribute (Prelude.Maybe [Prelude.Text])
attribute_allowedValues = Lens.lens (\Attribute' {allowedValues} -> allowedValues) (\s@Attribute' {} a -> s {allowedValues = a} :: Attribute) Prelude.. Lens.mapping Lens.coerced

-- | The scientific unit for the attribute.
attribute_unit :: Lens.Lens' Attribute (Prelude.Maybe Prelude.Text)
attribute_unit = Lens.lens (\Attribute' {unit} -> unit) (\s@Attribute' {} a -> s {unit = a} :: Attribute)

-- | The fully qualified name of the attribute. For example, the fully
-- qualified name of an attribute might be @Vehicle.Body.Engine.Type@.
attribute_fullyQualifiedName :: Lens.Lens' Attribute Prelude.Text
attribute_fullyQualifiedName = Lens.lens (\Attribute' {fullyQualifiedName} -> fullyQualifiedName) (\s@Attribute' {} a -> s {fullyQualifiedName = a} :: Attribute)

-- | The specified data type of the attribute.
attribute_dataType :: Lens.Lens' Attribute NodeDataType
attribute_dataType = Lens.lens (\Attribute' {dataType} -> dataType) (\s@Attribute' {} a -> s {dataType = a} :: Attribute)

instance Core.FromJSON Attribute where
  parseJSON =
    Core.withObject
      "Attribute"
      ( \x ->
          Attribute'
            Prelude.<$> (x Core..:? "max")
            Prelude.<*> (x Core..:? "defaultValue")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "min")
            Prelude.<*> (x Core..:? "assignedValue")
            Prelude.<*> (x Core..:? "allowedValues" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "unit")
            Prelude.<*> (x Core..: "fullyQualifiedName")
            Prelude.<*> (x Core..: "dataType")
      )

instance Prelude.Hashable Attribute where
  hashWithSalt _salt Attribute' {..} =
    _salt `Prelude.hashWithSalt` max
      `Prelude.hashWithSalt` defaultValue
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` min
      `Prelude.hashWithSalt` assignedValue
      `Prelude.hashWithSalt` allowedValues
      `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` fullyQualifiedName
      `Prelude.hashWithSalt` dataType

instance Prelude.NFData Attribute where
  rnf Attribute' {..} =
    Prelude.rnf max
      `Prelude.seq` Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf min
      `Prelude.seq` Prelude.rnf assignedValue
      `Prelude.seq` Prelude.rnf allowedValues
      `Prelude.seq` Prelude.rnf unit
      `Prelude.seq` Prelude.rnf fullyQualifiedName
      `Prelude.seq` Prelude.rnf dataType

instance Core.ToJSON Attribute where
  toJSON Attribute' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("max" Core..=) Prelude.<$> max,
            ("defaultValue" Core..=) Prelude.<$> defaultValue,
            ("description" Core..=) Prelude.<$> description,
            ("min" Core..=) Prelude.<$> min,
            ("assignedValue" Core..=) Prelude.<$> assignedValue,
            ("allowedValues" Core..=) Prelude.<$> allowedValues,
            ("unit" Core..=) Prelude.<$> unit,
            Prelude.Just
              ("fullyQualifiedName" Core..= fullyQualifiedName),
            Prelude.Just ("dataType" Core..= dataType)
          ]
      )
