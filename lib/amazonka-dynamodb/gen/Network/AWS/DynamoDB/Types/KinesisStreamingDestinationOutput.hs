{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.KinesisStreamingDestinationOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.KinesisStreamingDestinationOutput
  ( KinesisStreamingDestinationOutput (..),

    -- * Smart constructor
    mkKinesisStreamingDestinationOutput,

    -- * Lenses
    ksdoDestinationStatus,
    ksdoStreamARN,
    ksdoTableName,
  )
where

import Network.AWS.DynamoDB.Types.DestinationStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | /See:/ 'mkKinesisStreamingDestinationOutput' smart constructor.
data KinesisStreamingDestinationOutput = KinesisStreamingDestinationOutput'
  { destinationStatus ::
      Lude.Maybe
        DestinationStatus,
    streamARN ::
      Lude.Maybe Lude.Text,
    tableName ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KinesisStreamingDestinationOutput' with the minimum fields required to make a request.
--
-- * 'destinationStatus' - The current status of the replication.
-- * 'streamARN' - The ARN for the specific Kinesis data stream.
-- * 'tableName' - The name of the table being modified.
mkKinesisStreamingDestinationOutput ::
  KinesisStreamingDestinationOutput
mkKinesisStreamingDestinationOutput =
  KinesisStreamingDestinationOutput'
    { destinationStatus =
        Lude.Nothing,
      streamARN = Lude.Nothing,
      tableName = Lude.Nothing
    }

-- | The current status of the replication.
--
-- /Note:/ Consider using 'destinationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksdoDestinationStatus :: Lens.Lens' KinesisStreamingDestinationOutput (Lude.Maybe DestinationStatus)
ksdoDestinationStatus = Lens.lens (destinationStatus :: KinesisStreamingDestinationOutput -> Lude.Maybe DestinationStatus) (\s a -> s {destinationStatus = a} :: KinesisStreamingDestinationOutput)
{-# DEPRECATED ksdoDestinationStatus "Use generic-lens or generic-optics with 'destinationStatus' instead." #-}

-- | The ARN for the specific Kinesis data stream.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksdoStreamARN :: Lens.Lens' KinesisStreamingDestinationOutput (Lude.Maybe Lude.Text)
ksdoStreamARN = Lens.lens (streamARN :: KinesisStreamingDestinationOutput -> Lude.Maybe Lude.Text) (\s a -> s {streamARN = a} :: KinesisStreamingDestinationOutput)
{-# DEPRECATED ksdoStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

-- | The name of the table being modified.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksdoTableName :: Lens.Lens' KinesisStreamingDestinationOutput (Lude.Maybe Lude.Text)
ksdoTableName = Lens.lens (tableName :: KinesisStreamingDestinationOutput -> Lude.Maybe Lude.Text) (\s a -> s {tableName = a} :: KinesisStreamingDestinationOutput)
{-# DEPRECATED ksdoTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Lude.FromJSON KinesisStreamingDestinationOutput where
  parseJSON =
    Lude.withObject
      "KinesisStreamingDestinationOutput"
      ( \x ->
          KinesisStreamingDestinationOutput'
            Lude.<$> (x Lude..:? "DestinationStatus")
            Lude.<*> (x Lude..:? "StreamArn")
            Lude.<*> (x Lude..:? "TableName")
      )
