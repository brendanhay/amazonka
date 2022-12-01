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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.Sensor where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
  { -- | The specified possible maximum value of the sensor.
    max :: Prelude.Maybe Prelude.Double,
    -- | A brief description of a sensor.
    description :: Prelude.Maybe Prelude.Text,
    -- | The specified possible minimum value of the sensor.
    min :: Prelude.Maybe Prelude.Double,
    -- | A list of possible values a sensor can take.
    allowedValues :: Prelude.Maybe [Prelude.Text],
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
-- 'max', 'sensor_max' - The specified possible maximum value of the sensor.
--
-- 'description', 'sensor_description' - A brief description of a sensor.
--
-- 'min', 'sensor_min' - The specified possible minimum value of the sensor.
--
-- 'allowedValues', 'sensor_allowedValues' - A list of possible values a sensor can take.
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
    { max = Prelude.Nothing,
      description = Prelude.Nothing,
      min = Prelude.Nothing,
      allowedValues = Prelude.Nothing,
      unit = Prelude.Nothing,
      fullyQualifiedName = pFullyQualifiedName_,
      dataType = pDataType_
    }

-- | The specified possible maximum value of the sensor.
sensor_max :: Lens.Lens' Sensor (Prelude.Maybe Prelude.Double)
sensor_max = Lens.lens (\Sensor' {max} -> max) (\s@Sensor' {} a -> s {max = a} :: Sensor)

-- | A brief description of a sensor.
sensor_description :: Lens.Lens' Sensor (Prelude.Maybe Prelude.Text)
sensor_description = Lens.lens (\Sensor' {description} -> description) (\s@Sensor' {} a -> s {description = a} :: Sensor)

-- | The specified possible minimum value of the sensor.
sensor_min :: Lens.Lens' Sensor (Prelude.Maybe Prelude.Double)
sensor_min = Lens.lens (\Sensor' {min} -> min) (\s@Sensor' {} a -> s {min = a} :: Sensor)

-- | A list of possible values a sensor can take.
sensor_allowedValues :: Lens.Lens' Sensor (Prelude.Maybe [Prelude.Text])
sensor_allowedValues = Lens.lens (\Sensor' {allowedValues} -> allowedValues) (\s@Sensor' {} a -> s {allowedValues = a} :: Sensor) Prelude.. Lens.mapping Lens.coerced

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

instance Core.FromJSON Sensor where
  parseJSON =
    Core.withObject
      "Sensor"
      ( \x ->
          Sensor'
            Prelude.<$> (x Core..:? "max")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "min")
            Prelude.<*> (x Core..:? "allowedValues" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "unit")
            Prelude.<*> (x Core..: "fullyQualifiedName")
            Prelude.<*> (x Core..: "dataType")
      )

instance Prelude.Hashable Sensor where
  hashWithSalt _salt Sensor' {..} =
    _salt `Prelude.hashWithSalt` max
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` min
      `Prelude.hashWithSalt` allowedValues
      `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` fullyQualifiedName
      `Prelude.hashWithSalt` dataType

instance Prelude.NFData Sensor where
  rnf Sensor' {..} =
    Prelude.rnf max
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf min
      `Prelude.seq` Prelude.rnf allowedValues
      `Prelude.seq` Prelude.rnf unit
      `Prelude.seq` Prelude.rnf fullyQualifiedName
      `Prelude.seq` Prelude.rnf dataType

instance Core.ToJSON Sensor where
  toJSON Sensor' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("max" Core..=) Prelude.<$> max,
            ("description" Core..=) Prelude.<$> description,
            ("min" Core..=) Prelude.<$> min,
            ("allowedValues" Core..=) Prelude.<$> allowedValues,
            ("unit" Core..=) Prelude.<$> unit,
            Prelude.Just
              ("fullyQualifiedName" Core..= fullyQualifiedName),
            Prelude.Just ("dataType" Core..= dataType)
          ]
      )
