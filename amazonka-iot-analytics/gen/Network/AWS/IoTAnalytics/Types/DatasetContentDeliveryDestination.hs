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

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types.IotEventsDestinationConfiguration
import Network.AWS.IoTAnalytics.Types.S3DestinationConfiguration
import qualified Network.AWS.Lens as Lens

-- | The destination to which dataset contents are delivered.
--
-- /See:/ 'newDatasetContentDeliveryDestination' smart constructor.
data DatasetContentDeliveryDestination = DatasetContentDeliveryDestination'
  { -- | Configuration information for delivery of dataset contents to Amazon S3.
    s3DestinationConfiguration :: Core.Maybe S3DestinationConfiguration,
    -- | Configuration information for delivery of dataset contents to AWS IoT
    -- Events.
    iotEventsDestinationConfiguration :: Core.Maybe IotEventsDestinationConfiguration
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      iotEventsDestinationConfiguration =
        Core.Nothing
    }

-- | Configuration information for delivery of dataset contents to Amazon S3.
datasetContentDeliveryDestination_s3DestinationConfiguration :: Lens.Lens' DatasetContentDeliveryDestination (Core.Maybe S3DestinationConfiguration)
datasetContentDeliveryDestination_s3DestinationConfiguration = Lens.lens (\DatasetContentDeliveryDestination' {s3DestinationConfiguration} -> s3DestinationConfiguration) (\s@DatasetContentDeliveryDestination' {} a -> s {s3DestinationConfiguration = a} :: DatasetContentDeliveryDestination)

-- | Configuration information for delivery of dataset contents to AWS IoT
-- Events.
datasetContentDeliveryDestination_iotEventsDestinationConfiguration :: Lens.Lens' DatasetContentDeliveryDestination (Core.Maybe IotEventsDestinationConfiguration)
datasetContentDeliveryDestination_iotEventsDestinationConfiguration = Lens.lens (\DatasetContentDeliveryDestination' {iotEventsDestinationConfiguration} -> iotEventsDestinationConfiguration) (\s@DatasetContentDeliveryDestination' {} a -> s {iotEventsDestinationConfiguration = a} :: DatasetContentDeliveryDestination)

instance
  Core.FromJSON
    DatasetContentDeliveryDestination
  where
  parseJSON =
    Core.withObject
      "DatasetContentDeliveryDestination"
      ( \x ->
          DatasetContentDeliveryDestination'
            Core.<$> (x Core..:? "s3DestinationConfiguration")
            Core.<*> (x Core..:? "iotEventsDestinationConfiguration")
      )

instance
  Core.Hashable
    DatasetContentDeliveryDestination

instance
  Core.NFData
    DatasetContentDeliveryDestination

instance
  Core.ToJSON
    DatasetContentDeliveryDestination
  where
  toJSON DatasetContentDeliveryDestination' {..} =
    Core.object
      ( Core.catMaybes
          [ ("s3DestinationConfiguration" Core..=)
              Core.<$> s3DestinationConfiguration,
            ("iotEventsDestinationConfiguration" Core..=)
              Core.<$> iotEventsDestinationConfiguration
          ]
      )
