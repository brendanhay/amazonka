{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetContentDeliveryDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetContentDeliveryDestination where

import Network.AWS.IoTAnalytics.Types.IotEventsDestinationConfiguration
import Network.AWS.IoTAnalytics.Types.S3DestinationConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The destination to which dataset contents are delivered.
--
-- /See:/ 'newDatasetContentDeliveryDestination' smart constructor.
data DatasetContentDeliveryDestination = DatasetContentDeliveryDestination'
  { -- | Configuration information for delivery of dataset contents to Amazon S3.
    s3DestinationConfiguration :: Prelude.Maybe S3DestinationConfiguration,
    -- | Configuration information for delivery of dataset contents to AWS IoT
    -- Events.
    iotEventsDestinationConfiguration :: Prelude.Maybe IotEventsDestinationConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DatasetContentDeliveryDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3DestinationConfiguration', 'datasetContentDeliveryDestination_s3DestinationConfiguration' - Configuration information for delivery of dataset contents to Amazon S3.
--
-- 'iotEventsDestinationConfiguration', 'datasetContentDeliveryDestination_iotEventsDestinationConfiguration' - Configuration information for delivery of dataset contents to AWS IoT
-- Events.
newDatasetContentDeliveryDestination ::
  DatasetContentDeliveryDestination
newDatasetContentDeliveryDestination =
  DatasetContentDeliveryDestination'
    { s3DestinationConfiguration =
        Prelude.Nothing,
      iotEventsDestinationConfiguration =
        Prelude.Nothing
    }

-- | Configuration information for delivery of dataset contents to Amazon S3.
datasetContentDeliveryDestination_s3DestinationConfiguration :: Lens.Lens' DatasetContentDeliveryDestination (Prelude.Maybe S3DestinationConfiguration)
datasetContentDeliveryDestination_s3DestinationConfiguration = Lens.lens (\DatasetContentDeliveryDestination' {s3DestinationConfiguration} -> s3DestinationConfiguration) (\s@DatasetContentDeliveryDestination' {} a -> s {s3DestinationConfiguration = a} :: DatasetContentDeliveryDestination)

-- | Configuration information for delivery of dataset contents to AWS IoT
-- Events.
datasetContentDeliveryDestination_iotEventsDestinationConfiguration :: Lens.Lens' DatasetContentDeliveryDestination (Prelude.Maybe IotEventsDestinationConfiguration)
datasetContentDeliveryDestination_iotEventsDestinationConfiguration = Lens.lens (\DatasetContentDeliveryDestination' {iotEventsDestinationConfiguration} -> iotEventsDestinationConfiguration) (\s@DatasetContentDeliveryDestination' {} a -> s {iotEventsDestinationConfiguration = a} :: DatasetContentDeliveryDestination)

instance
  Prelude.FromJSON
    DatasetContentDeliveryDestination
  where
  parseJSON =
    Prelude.withObject
      "DatasetContentDeliveryDestination"
      ( \x ->
          DatasetContentDeliveryDestination'
            Prelude.<$> (x Prelude..:? "s3DestinationConfiguration")
            Prelude.<*> (x Prelude..:? "iotEventsDestinationConfiguration")
      )

instance
  Prelude.Hashable
    DatasetContentDeliveryDestination

instance
  Prelude.NFData
    DatasetContentDeliveryDestination

instance
  Prelude.ToJSON
    DatasetContentDeliveryDestination
  where
  toJSON DatasetContentDeliveryDestination' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("s3DestinationConfiguration" Prelude..=)
              Prelude.<$> s3DestinationConfiguration,
            ("iotEventsDestinationConfiguration" Prelude..=)
              Prelude.<$> iotEventsDestinationConfiguration
          ]
      )
