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
-- Module      : Amazonka.IoTFleetWise.Types.Sensor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.Sensor where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types.NodeDataType
import qualified Amazonka.Prelude as Prelude

-- | An input component that reports the environmental condition of a
-- vehicle.
--
-- You can collect data about fluid levels, temperatures, vibrations, or
-- battery voltage from sensors.
--
-- /See:/ 'newSensor' smart constructor.
data Sensor = Sensor'
  { -- | A list of possible values a sensor can take.
    allowedValues :: Prelude.Maybe [Prelude.Text],
    -- | A brief description of a sensor.
    description :: Prelude.Maybe Prelude.Text,
    -- | The specified possible maximum value of the sensor.
    max :: Prelude.Maybe Prelude.Double,
    -- | The specified possible minimum value of the sensor.
    min :: Prelude.Maybe Prelude.Double,
    -- | The scientific unit of measurement for data collected by the sensor.
    unit :: Prelude.Maybe Prelude.Text,
    -- | The fully qualified name of the sensor. For example, the fully qualified
    -- name of a sensor might be @Vehicle.Body.Engine.Battery@.
    fullyQualifiedName :: Prelude.Text,
    -- | The specified data type of the sensor.
    dataType :: NodeDataType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Sensor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowedValues', 'sensor_allowedValues' - A list of possible values a sensor can take.
--
-- 'description', 'sensor_description' - A brief description of a sensor.
--
-- 'max', 'sensor_max' - The specified possible maximum value of the sensor.
--
-- 'min', 'sensor_min' - The specified possible minimum value of the sensor.
--
-- 'unit', 'sensor_unit' - The scientific unit of measurement for data collected by the sensor.
--
-- 'fullyQualifiedName', 'sensor_fullyQualifiedName' - The fully qualified name of the sensor. For example, the fully qualified
-- name of a sensor might be @Vehicle.Body.Engine.Battery@.
--
-- 'dataType', 'sensor_dataType' - The specified data type of the sensor.
newSensor ::
  -- | 'fullyQualifiedName'
  Prelude.Text ->
  -- | 'dataType'
  NodeDataType ->
  Sensor
newSensor pFullyQualifiedName_ pDataType_ =
  Sensor'
    { allowedValues = Prelude.Nothing,
      description = Prelude.Nothing,
      max = Prelude.Nothing,
      min = Prelude.Nothing,
      unit = Prelude.Nothing,
      fullyQualifiedName = pFullyQualifiedName_,
      dataType = pDataType_
    }

-- | A list of possible values a sensor can take.
sensor_allowedValues :: Lens.Lens' Sensor (Prelude.Maybe [Prelude.Text])
sensor_allowedValues = Lens.lens (\Sensor' {allowedValues} -> allowedValues) (\s@Sensor' {} a -> s {allowedValues = a} :: Sensor) Prelude.. Lens.mapping Lens.coerced

-- | A brief description of a sensor.
sensor_description :: Lens.Lens' Sensor (Prelude.Maybe Prelude.Text)
sensor_description = Lens.lens (\Sensor' {description} -> description) (\s@Sensor' {} a -> s {description = a} :: Sensor)

-- | The specified possible maximum value of the sensor.
sensor_max :: Lens.Lens' Sensor (Prelude.Maybe Prelude.Double)
sensor_max = Lens.lens (\Sensor' {max} -> max) (\s@Sensor' {} a -> s {max = a} :: Sensor)

-- | The specified possible minimum value of the sensor.
sensor_min :: Lens.Lens' Sensor (Prelude.Maybe Prelude.Double)
sensor_min = Lens.lens (\Sensor' {min} -> min) (\s@Sensor' {} a -> s {min = a} :: Sensor)

-- | The scientific unit of measurement for data collected by the sensor.
sensor_unit :: Lens.Lens' Sensor (Prelude.Maybe Prelude.Text)
sensor_unit = Lens.lens (\Sensor' {unit} -> unit) (\s@Sensor' {} a -> s {unit = a} :: Sensor)

-- | The fully qualified name of the sensor. For example, the fully qualified
-- name of a sensor might be @Vehicle.Body.Engine.Battery@.
sensor_fullyQualifiedName :: Lens.Lens' Sensor Prelude.Text
sensor_fullyQualifiedName = Lens.lens (\Sensor' {fullyQualifiedName} -> fullyQualifiedName) (\s@Sensor' {} a -> s {fullyQualifiedName = a} :: Sensor)

-- | The specified data type of the sensor.
sensor_dataType :: Lens.Lens' Sensor NodeDataType
sensor_dataType = Lens.lens (\Sensor' {dataType} -> dataType) (\s@Sensor' {} a -> s {dataType = a} :: Sensor)

instance Data.FromJSON Sensor where
  parseJSON =
    Data.withObject
      "Sensor"
      ( \x ->
          Sensor'
            Prelude.<$> (x Data..:? "allowedValues" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "max")
            Prelude.<*> (x Data..:? "min")
            Prelude.<*> (x Data..:? "unit")
            Prelude.<*> (x Data..: "fullyQualifiedName")
            Prelude.<*> (x Data..: "dataType")
      )

instance Prelude.Hashable Sensor where
  hashWithSalt _salt Sensor' {..} =
    _salt
      `Prelude.hashWithSalt` allowedValues
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` max
      `Prelude.hashWithSalt` min
      `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` fullyQualifiedName
      `Prelude.hashWithSalt` dataType

instance Prelude.NFData Sensor where
  rnf Sensor' {..} =
    Prelude.rnf allowedValues
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf max
      `Prelude.seq` Prelude.rnf min
      `Prelude.seq` Prelude.rnf unit
      `Prelude.seq` Prelude.rnf fullyQualifiedName
      `Prelude.seq` Prelude.rnf dataType

instance Data.ToJSON Sensor where
  toJSON Sensor' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("allowedValues" Data..=) Prelude.<$> allowedValues,
            ("description" Data..=) Prelude.<$> description,
            ("max" Data..=) Prelude.<$> max,
            ("min" Data..=) Prelude.<$> min,
            ("unit" Data..=) Prelude.<$> unit,
            Prelude.Just
              ("fullyQualifiedName" Data..= fullyQualifiedName),
            Prelude.Just ("dataType" Data..= dataType)
          ]
      )
