{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.KinesisDataStreamDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.KinesisDataStreamDestination
  ( KinesisDataStreamDestination (..),

    -- * Smart constructor
    mkKinesisDataStreamDestination,

    -- * Lenses
    kdsdDestinationStatus,
    kdsdStreamARN,
    kdsdDestinationStatusDescription,
  )
where

import Network.AWS.DynamoDB.Types.DestinationStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a Kinesis data stream destination.
--
-- /See:/ 'mkKinesisDataStreamDestination' smart constructor.
data KinesisDataStreamDestination = KinesisDataStreamDestination'
  { -- | The current status of replication.
    destinationStatus :: Lude.Maybe DestinationStatus,
    -- | The ARN for a specific Kinesis data stream.
    streamARN :: Lude.Maybe Lude.Text,
    -- | The human-readable string that corresponds to the replica status.
    destinationStatusDescription :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KinesisDataStreamDestination' with the minimum fields required to make a request.
--
-- * 'destinationStatus' - The current status of replication.
-- * 'streamARN' - The ARN for a specific Kinesis data stream.
-- * 'destinationStatusDescription' - The human-readable string that corresponds to the replica status.
mkKinesisDataStreamDestination ::
  KinesisDataStreamDestination
mkKinesisDataStreamDestination =
  KinesisDataStreamDestination'
    { destinationStatus = Lude.Nothing,
      streamARN = Lude.Nothing,
      destinationStatusDescription = Lude.Nothing
    }

-- | The current status of replication.
--
-- /Note:/ Consider using 'destinationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kdsdDestinationStatus :: Lens.Lens' KinesisDataStreamDestination (Lude.Maybe DestinationStatus)
kdsdDestinationStatus = Lens.lens (destinationStatus :: KinesisDataStreamDestination -> Lude.Maybe DestinationStatus) (\s a -> s {destinationStatus = a} :: KinesisDataStreamDestination)
{-# DEPRECATED kdsdDestinationStatus "Use generic-lens or generic-optics with 'destinationStatus' instead." #-}

-- | The ARN for a specific Kinesis data stream.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kdsdStreamARN :: Lens.Lens' KinesisDataStreamDestination (Lude.Maybe Lude.Text)
kdsdStreamARN = Lens.lens (streamARN :: KinesisDataStreamDestination -> Lude.Maybe Lude.Text) (\s a -> s {streamARN = a} :: KinesisDataStreamDestination)
{-# DEPRECATED kdsdStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | The human-readable string that corresponds to the replica status.
--
-- /Note:/ Consider using 'destinationStatusDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kdsdDestinationStatusDescription :: Lens.Lens' KinesisDataStreamDestination (Lude.Maybe Lude.Text)
kdsdDestinationStatusDescription = Lens.lens (destinationStatusDescription :: KinesisDataStreamDestination -> Lude.Maybe Lude.Text) (\s a -> s {destinationStatusDescription = a} :: KinesisDataStreamDestination)
{-# DEPRECATED kdsdDestinationStatusDescription "Use generic-lens or generic-optics with 'destinationStatusDescription' instead." #-}

instance Lude.FromJSON KinesisDataStreamDestination where
  parseJSON =
    Lude.withObject
      "KinesisDataStreamDestination"
      ( \x ->
          KinesisDataStreamDestination'
            Lude.<$> (x Lude..:? "DestinationStatus")
            Lude.<*> (x Lude..:? "StreamArn")
            Lude.<*> (x Lude..:? "DestinationStatusDescription")
      )
