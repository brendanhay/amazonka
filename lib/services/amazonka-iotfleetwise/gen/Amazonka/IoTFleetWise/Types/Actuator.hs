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
-- Module      : Amazonka.IoTFleetWise.Types.Actuator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.Actuator where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types.NodeDataType
import qualified Amazonka.Prelude as Prelude

-- | A signal that represents a vehicle device such as the engine, heater,
-- and door locks. Data from an actuator reports the state of a certain
-- vehicle device.
--
-- Updating actuator data can change the state of a device. For example,
-- you can turn on or off the heater by updating its actuator data.
--
-- /See:/ 'newActuator' smart constructor.
data Actuator = Actuator'
  { -- | A list of possible values an actuator can take.
    allowedValues :: Prelude.Maybe [Prelude.Text],
    -- | A specified value for the actuator.
    assignedValue :: Prelude.Maybe Prelude.Text,
    -- | A comment in addition to the description.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The deprecation message for the node or the branch that was moved or
    -- deleted.
    deprecationMessage :: Prelude.Maybe Prelude.Text,
    -- | A brief description of the actuator.
    description :: Prelude.Maybe Prelude.Text,
    -- | The specified possible maximum value of an actuator.
    max :: Prelude.Maybe Prelude.Double,
    -- | The specified possible minimum value of an actuator.
    min :: Prelude.Maybe Prelude.Double,
    -- | The scientific unit for the actuator.
    unit :: Prelude.Maybe Prelude.Text,
    -- | The fully qualified name of the actuator. For example, the fully
    -- qualified name of an actuator might be @Vehicle.Front.Left.Door.Lock@.
    fullyQualifiedName :: Prelude.Text,
    -- | The specified data type of the actuator.
    dataType :: NodeDataType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Actuator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowedValues', 'actuator_allowedValues' - A list of possible values an actuator can take.
--
-- 'assignedValue', 'actuator_assignedValue' - A specified value for the actuator.
--
-- 'comment', 'actuator_comment' - A comment in addition to the description.
--
-- 'deprecationMessage', 'actuator_deprecationMessage' - The deprecation message for the node or the branch that was moved or
-- deleted.
--
-- 'description', 'actuator_description' - A brief description of the actuator.
--
-- 'max', 'actuator_max' - The specified possible maximum value of an actuator.
--
-- 'min', 'actuator_min' - The specified possible minimum value of an actuator.
--
-- 'unit', 'actuator_unit' - The scientific unit for the actuator.
--
-- 'fullyQualifiedName', 'actuator_fullyQualifiedName' - The fully qualified name of the actuator. For example, the fully
-- qualified name of an actuator might be @Vehicle.Front.Left.Door.Lock@.
--
-- 'dataType', 'actuator_dataType' - The specified data type of the actuator.
newActuator ::
  -- | 'fullyQualifiedName'
  Prelude.Text ->
  -- | 'dataType'
  NodeDataType ->
  Actuator
newActuator pFullyQualifiedName_ pDataType_ =
  Actuator'
    { allowedValues = Prelude.Nothing,
      assignedValue = Prelude.Nothing,
      comment = Prelude.Nothing,
      deprecationMessage = Prelude.Nothing,
      description = Prelude.Nothing,
      max = Prelude.Nothing,
      min = Prelude.Nothing,
      unit = Prelude.Nothing,
      fullyQualifiedName = pFullyQualifiedName_,
      dataType = pDataType_
    }

-- | A list of possible values an actuator can take.
actuator_allowedValues :: Lens.Lens' Actuator (Prelude.Maybe [Prelude.Text])
actuator_allowedValues = Lens.lens (\Actuator' {allowedValues} -> allowedValues) (\s@Actuator' {} a -> s {allowedValues = a} :: Actuator) Prelude.. Lens.mapping Lens.coerced

-- | A specified value for the actuator.
actuator_assignedValue :: Lens.Lens' Actuator (Prelude.Maybe Prelude.Text)
actuator_assignedValue = Lens.lens (\Actuator' {assignedValue} -> assignedValue) (\s@Actuator' {} a -> s {assignedValue = a} :: Actuator)

-- | A comment in addition to the description.
actuator_comment :: Lens.Lens' Actuator (Prelude.Maybe Prelude.Text)
actuator_comment = Lens.lens (\Actuator' {comment} -> comment) (\s@Actuator' {} a -> s {comment = a} :: Actuator)

-- | The deprecation message for the node or the branch that was moved or
-- deleted.
actuator_deprecationMessage :: Lens.Lens' Actuator (Prelude.Maybe Prelude.Text)
actuator_deprecationMessage = Lens.lens (\Actuator' {deprecationMessage} -> deprecationMessage) (\s@Actuator' {} a -> s {deprecationMessage = a} :: Actuator)

-- | A brief description of the actuator.
actuator_description :: Lens.Lens' Actuator (Prelude.Maybe Prelude.Text)
actuator_description = Lens.lens (\Actuator' {description} -> description) (\s@Actuator' {} a -> s {description = a} :: Actuator)

-- | The specified possible maximum value of an actuator.
actuator_max :: Lens.Lens' Actuator (Prelude.Maybe Prelude.Double)
actuator_max = Lens.lens (\Actuator' {max} -> max) (\s@Actuator' {} a -> s {max = a} :: Actuator)

-- | The specified possible minimum value of an actuator.
actuator_min :: Lens.Lens' Actuator (Prelude.Maybe Prelude.Double)
actuator_min = Lens.lens (\Actuator' {min} -> min) (\s@Actuator' {} a -> s {min = a} :: Actuator)

-- | The scientific unit for the actuator.
actuator_unit :: Lens.Lens' Actuator (Prelude.Maybe Prelude.Text)
actuator_unit = Lens.lens (\Actuator' {unit} -> unit) (\s@Actuator' {} a -> s {unit = a} :: Actuator)

-- | The fully qualified name of the actuator. For example, the fully
-- qualified name of an actuator might be @Vehicle.Front.Left.Door.Lock@.
actuator_fullyQualifiedName :: Lens.Lens' Actuator Prelude.Text
actuator_fullyQualifiedName = Lens.lens (\Actuator' {fullyQualifiedName} -> fullyQualifiedName) (\s@Actuator' {} a -> s {fullyQualifiedName = a} :: Actuator)

-- | The specified data type of the actuator.
actuator_dataType :: Lens.Lens' Actuator NodeDataType
actuator_dataType = Lens.lens (\Actuator' {dataType} -> dataType) (\s@Actuator' {} a -> s {dataType = a} :: Actuator)

instance Data.FromJSON Actuator where
  parseJSON =
    Data.withObject
      "Actuator"
      ( \x ->
          Actuator'
            Prelude.<$> (x Data..:? "allowedValues" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "assignedValue")
            Prelude.<*> (x Data..:? "comment")
            Prelude.<*> (x Data..:? "deprecationMessage")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "max")
            Prelude.<*> (x Data..:? "min")
            Prelude.<*> (x Data..:? "unit")
            Prelude.<*> (x Data..: "fullyQualifiedName")
            Prelude.<*> (x Data..: "dataType")
      )

instance Prelude.Hashable Actuator where
  hashWithSalt _salt Actuator' {..} =
    _salt
      `Prelude.hashWithSalt` allowedValues
      `Prelude.hashWithSalt` assignedValue
      `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` deprecationMessage
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` max
      `Prelude.hashWithSalt` min
      `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` fullyQualifiedName
      `Prelude.hashWithSalt` dataType

instance Prelude.NFData Actuator where
  rnf Actuator' {..} =
    Prelude.rnf allowedValues
      `Prelude.seq` Prelude.rnf assignedValue
      `Prelude.seq` Prelude.rnf comment
      `Prelude.seq` Prelude.rnf deprecationMessage
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf max
      `Prelude.seq` Prelude.rnf min
      `Prelude.seq` Prelude.rnf unit
      `Prelude.seq` Prelude.rnf fullyQualifiedName
      `Prelude.seq` Prelude.rnf dataType

instance Data.ToJSON Actuator where
  toJSON Actuator' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("allowedValues" Data..=) Prelude.<$> allowedValues,
            ("assignedValue" Data..=) Prelude.<$> assignedValue,
            ("comment" Data..=) Prelude.<$> comment,
            ("deprecationMessage" Data..=)
              Prelude.<$> deprecationMessage,
            ("description" Data..=) Prelude.<$> description,
            ("max" Data..=) Prelude.<$> max,
            ("min" Data..=) Prelude.<$> min,
            ("unit" Data..=) Prelude.<$> unit,
            Prelude.Just
              ("fullyQualifiedName" Data..= fullyQualifiedName),
            Prelude.Just ("dataType" Data..= dataType)
          ]
      )
