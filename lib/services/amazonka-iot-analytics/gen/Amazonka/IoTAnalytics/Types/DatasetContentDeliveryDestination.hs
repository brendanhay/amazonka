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
-- Module      : Amazonka.IoTAnalytics.Types.DatasetContentDeliveryDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.DatasetContentDeliveryDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types.IotEventsDestinationConfiguration
import Amazonka.IoTAnalytics.Types.S3DestinationConfiguration
import qualified Amazonka.Prelude as Prelude

-- | The destination to which dataset contents are delivered.
--
-- /See:/ 'newDatasetContentDeliveryDestination' smart constructor.
data DatasetContentDeliveryDestination = DatasetContentDeliveryDestination'
  { -- | Configuration information for delivery of dataset contents to IoT
    -- Events.
    iotEventsDestinationConfiguration :: Prelude.Maybe IotEventsDestinationConfiguration,
    -- | Configuration information for delivery of dataset contents to Amazon S3.
    s3DestinationConfiguration :: Prelude.Maybe S3DestinationConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetContentDeliveryDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iotEventsDestinationConfiguration', 'datasetContentDeliveryDestination_iotEventsDestinationConfiguration' - Configuration information for delivery of dataset contents to IoT
-- Events.
--
-- 's3DestinationConfiguration', 'datasetContentDeliveryDestination_s3DestinationConfiguration' - Configuration information for delivery of dataset contents to Amazon S3.
newDatasetContentDeliveryDestination ::
  DatasetContentDeliveryDestination
newDatasetContentDeliveryDestination =
  DatasetContentDeliveryDestination'
    { iotEventsDestinationConfiguration =
        Prelude.Nothing,
      s3DestinationConfiguration =
        Prelude.Nothing
    }

-- | Configuration information for delivery of dataset contents to IoT
-- Events.
datasetContentDeliveryDestination_iotEventsDestinationConfiguration :: Lens.Lens' DatasetContentDeliveryDestination (Prelude.Maybe IotEventsDestinationConfiguration)
datasetContentDeliveryDestination_iotEventsDestinationConfiguration = Lens.lens (\DatasetContentDeliveryDestination' {iotEventsDestinationConfiguration} -> iotEventsDestinationConfiguration) (\s@DatasetContentDeliveryDestination' {} a -> s {iotEventsDestinationConfiguration = a} :: DatasetContentDeliveryDestination)

-- | Configuration information for delivery of dataset contents to Amazon S3.
datasetContentDeliveryDestination_s3DestinationConfiguration :: Lens.Lens' DatasetContentDeliveryDestination (Prelude.Maybe S3DestinationConfiguration)
datasetContentDeliveryDestination_s3DestinationConfiguration = Lens.lens (\DatasetContentDeliveryDestination' {s3DestinationConfiguration} -> s3DestinationConfiguration) (\s@DatasetContentDeliveryDestination' {} a -> s {s3DestinationConfiguration = a} :: DatasetContentDeliveryDestination)

instance
  Data.FromJSON
    DatasetContentDeliveryDestination
  where
  parseJSON =
    Data.withObject
      "DatasetContentDeliveryDestination"
      ( \x ->
          DatasetContentDeliveryDestination'
            Prelude.<$> (x Data..:? "iotEventsDestinationConfiguration")
            Prelude.<*> (x Data..:? "s3DestinationConfiguration")
      )

instance
  Prelude.Hashable
    DatasetContentDeliveryDestination
  where
  hashWithSalt
    _salt
    DatasetContentDeliveryDestination' {..} =
      _salt
        `Prelude.hashWithSalt` iotEventsDestinationConfiguration
        `Prelude.hashWithSalt` s3DestinationConfiguration

instance
  Prelude.NFData
    DatasetContentDeliveryDestination
  where
  rnf DatasetContentDeliveryDestination' {..} =
    Prelude.rnf iotEventsDestinationConfiguration `Prelude.seq`
      Prelude.rnf s3DestinationConfiguration

instance
  Data.ToJSON
    DatasetContentDeliveryDestination
  where
  toJSON DatasetContentDeliveryDestination' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("iotEventsDestinationConfiguration" Data..=)
              Prelude.<$> iotEventsDestinationConfiguration,
            ("s3DestinationConfiguration" Data..=)
              Prelude.<$> s3DestinationConfiguration
          ]
      )
