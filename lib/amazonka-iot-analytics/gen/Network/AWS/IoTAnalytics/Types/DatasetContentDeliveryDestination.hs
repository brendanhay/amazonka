{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetContentDeliveryDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetContentDeliveryDestination
  ( DatasetContentDeliveryDestination (..),

    -- * Smart constructor
    mkDatasetContentDeliveryDestination,

    -- * Lenses
    dcddS3DestinationConfiguration,
    dcddIotEventsDestinationConfiguration,
  )
where

import Network.AWS.IoTAnalytics.Types.IotEventsDestinationConfiguration
import Network.AWS.IoTAnalytics.Types.S3DestinationConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The destination to which dataset contents are delivered.
--
-- /See:/ 'mkDatasetContentDeliveryDestination' smart constructor.
data DatasetContentDeliveryDestination = DatasetContentDeliveryDestination'
  { s3DestinationConfiguration ::
      Lude.Maybe
        S3DestinationConfiguration,
    iotEventsDestinationConfiguration ::
      Lude.Maybe
        IotEventsDestinationConfiguration
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DatasetContentDeliveryDestination' with the minimum fields required to make a request.
--
-- * 'iotEventsDestinationConfiguration' - Configuration information for delivery of dataset contents to AWS IoT Events.
-- * 's3DestinationConfiguration' - Configuration information for delivery of dataset contents to Amazon S3.
mkDatasetContentDeliveryDestination ::
  DatasetContentDeliveryDestination
mkDatasetContentDeliveryDestination =
  DatasetContentDeliveryDestination'
    { s3DestinationConfiguration =
        Lude.Nothing,
      iotEventsDestinationConfiguration = Lude.Nothing
    }

-- | Configuration information for delivery of dataset contents to Amazon S3.
--
-- /Note:/ Consider using 's3DestinationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcddS3DestinationConfiguration :: Lens.Lens' DatasetContentDeliveryDestination (Lude.Maybe S3DestinationConfiguration)
dcddS3DestinationConfiguration = Lens.lens (s3DestinationConfiguration :: DatasetContentDeliveryDestination -> Lude.Maybe S3DestinationConfiguration) (\s a -> s {s3DestinationConfiguration = a} :: DatasetContentDeliveryDestination)
{-# DEPRECATED dcddS3DestinationConfiguration "Use generic-lens or generic-optics with 's3DestinationConfiguration' instead." #-}

-- | Configuration information for delivery of dataset contents to AWS IoT Events.
--
-- /Note:/ Consider using 'iotEventsDestinationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcddIotEventsDestinationConfiguration :: Lens.Lens' DatasetContentDeliveryDestination (Lude.Maybe IotEventsDestinationConfiguration)
dcddIotEventsDestinationConfiguration = Lens.lens (iotEventsDestinationConfiguration :: DatasetContentDeliveryDestination -> Lude.Maybe IotEventsDestinationConfiguration) (\s a -> s {iotEventsDestinationConfiguration = a} :: DatasetContentDeliveryDestination)
{-# DEPRECATED dcddIotEventsDestinationConfiguration "Use generic-lens or generic-optics with 'iotEventsDestinationConfiguration' instead." #-}

instance Lude.FromJSON DatasetContentDeliveryDestination where
  parseJSON =
    Lude.withObject
      "DatasetContentDeliveryDestination"
      ( \x ->
          DatasetContentDeliveryDestination'
            Lude.<$> (x Lude..:? "s3DestinationConfiguration")
            Lude.<*> (x Lude..:? "iotEventsDestinationConfiguration")
      )

instance Lude.ToJSON DatasetContentDeliveryDestination where
  toJSON DatasetContentDeliveryDestination' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("s3DestinationConfiguration" Lude..=)
              Lude.<$> s3DestinationConfiguration,
            ("iotEventsDestinationConfiguration" Lude..=)
              Lude.<$> iotEventsDestinationConfiguration
          ]
      )
