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
-- Module      : Amazonka.IoTSiteWise.Types.PropertyType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.PropertyType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.Attribute
import Amazonka.IoTSiteWise.Types.Measurement
import Amazonka.IoTSiteWise.Types.Metric
import Amazonka.IoTSiteWise.Types.Transform
import qualified Amazonka.Prelude as Prelude

-- | Contains a property type, which can be one of @attribute@,
-- @measurement@, @metric@, or @transform@.
--
-- /See:/ 'newPropertyType' smart constructor.
data PropertyType = PropertyType'
  { -- | Specifies an asset attribute property. An attribute generally contains
    -- static information, such as the serial number of an
    -- <https://en.wikipedia.org/wiki/Internet_of_things#Industrial_applications IIoT>
    -- wind turbine.
    attribute :: Prelude.Maybe Attribute,
    -- | Specifies an asset measurement property. A measurement represents a
    -- device\'s raw sensor data stream, such as timestamped temperature values
    -- or timestamped power values.
    measurement :: Prelude.Maybe Measurement,
    -- | Specifies an asset metric property. A metric contains a mathematical
    -- expression that uses aggregate functions to process all input data
    -- points over a time interval and output a single data point, such as to
    -- calculate the average hourly temperature.
    metric :: Prelude.Maybe Metric,
    -- | Specifies an asset transform property. A transform contains a
    -- mathematical expression that maps a property\'s data points from one
    -- form to another, such as a unit conversion from Celsius to Fahrenheit.
    transform :: Prelude.Maybe Transform
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PropertyType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attribute', 'propertyType_attribute' - Specifies an asset attribute property. An attribute generally contains
-- static information, such as the serial number of an
-- <https://en.wikipedia.org/wiki/Internet_of_things#Industrial_applications IIoT>
-- wind turbine.
--
-- 'measurement', 'propertyType_measurement' - Specifies an asset measurement property. A measurement represents a
-- device\'s raw sensor data stream, such as timestamped temperature values
-- or timestamped power values.
--
-- 'metric', 'propertyType_metric' - Specifies an asset metric property. A metric contains a mathematical
-- expression that uses aggregate functions to process all input data
-- points over a time interval and output a single data point, such as to
-- calculate the average hourly temperature.
--
-- 'transform', 'propertyType_transform' - Specifies an asset transform property. A transform contains a
-- mathematical expression that maps a property\'s data points from one
-- form to another, such as a unit conversion from Celsius to Fahrenheit.
newPropertyType ::
  PropertyType
newPropertyType =
  PropertyType'
    { attribute = Prelude.Nothing,
      measurement = Prelude.Nothing,
      metric = Prelude.Nothing,
      transform = Prelude.Nothing
    }

-- | Specifies an asset attribute property. An attribute generally contains
-- static information, such as the serial number of an
-- <https://en.wikipedia.org/wiki/Internet_of_things#Industrial_applications IIoT>
-- wind turbine.
propertyType_attribute :: Lens.Lens' PropertyType (Prelude.Maybe Attribute)
propertyType_attribute = Lens.lens (\PropertyType' {attribute} -> attribute) (\s@PropertyType' {} a -> s {attribute = a} :: PropertyType)

-- | Specifies an asset measurement property. A measurement represents a
-- device\'s raw sensor data stream, such as timestamped temperature values
-- or timestamped power values.
propertyType_measurement :: Lens.Lens' PropertyType (Prelude.Maybe Measurement)
propertyType_measurement = Lens.lens (\PropertyType' {measurement} -> measurement) (\s@PropertyType' {} a -> s {measurement = a} :: PropertyType)

-- | Specifies an asset metric property. A metric contains a mathematical
-- expression that uses aggregate functions to process all input data
-- points over a time interval and output a single data point, such as to
-- calculate the average hourly temperature.
propertyType_metric :: Lens.Lens' PropertyType (Prelude.Maybe Metric)
propertyType_metric = Lens.lens (\PropertyType' {metric} -> metric) (\s@PropertyType' {} a -> s {metric = a} :: PropertyType)

-- | Specifies an asset transform property. A transform contains a
-- mathematical expression that maps a property\'s data points from one
-- form to another, such as a unit conversion from Celsius to Fahrenheit.
propertyType_transform :: Lens.Lens' PropertyType (Prelude.Maybe Transform)
propertyType_transform = Lens.lens (\PropertyType' {transform} -> transform) (\s@PropertyType' {} a -> s {transform = a} :: PropertyType)

instance Data.FromJSON PropertyType where
  parseJSON =
    Data.withObject
      "PropertyType"
      ( \x ->
          PropertyType'
            Prelude.<$> (x Data..:? "attribute")
            Prelude.<*> (x Data..:? "measurement")
            Prelude.<*> (x Data..:? "metric")
            Prelude.<*> (x Data..:? "transform")
      )

instance Prelude.Hashable PropertyType where
  hashWithSalt _salt PropertyType' {..} =
    _salt `Prelude.hashWithSalt` attribute
      `Prelude.hashWithSalt` measurement
      `Prelude.hashWithSalt` metric
      `Prelude.hashWithSalt` transform

instance Prelude.NFData PropertyType where
  rnf PropertyType' {..} =
    Prelude.rnf attribute
      `Prelude.seq` Prelude.rnf measurement
      `Prelude.seq` Prelude.rnf metric
      `Prelude.seq` Prelude.rnf transform

instance Data.ToJSON PropertyType where
  toJSON PropertyType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("attribute" Data..=) Prelude.<$> attribute,
            ("measurement" Data..=) Prelude.<$> measurement,
            ("metric" Data..=) Prelude.<$> metric,
            ("transform" Data..=) Prelude.<$> transform
          ]
      )
