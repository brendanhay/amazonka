{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.KinesisParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.KinesisParameters
  ( KinesisParameters (..),

    -- * Smart constructor
    mkKinesisParameters,

    -- * Lenses
    kpPartitionKeyPath,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This object enables you to specify a JSON path to extract from the event and use as the partition key for the Amazon Kinesis data stream, so that you can control the shard to which the event goes. If you do not include this parameter, the default is to use the @eventId@ as the partition key.
--
-- /See:/ 'mkKinesisParameters' smart constructor.
newtype KinesisParameters = KinesisParameters'
  { -- | The JSON path to be extracted from the event and used as the partition key. For more information, see <https://docs.aws.amazon.com/streams/latest/dev/key-concepts.html#partition-key Amazon Kinesis Streams Key Concepts> in the /Amazon Kinesis Streams Developer Guide/ .
    partitionKeyPath :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KinesisParameters' with the minimum fields required to make a request.
--
-- * 'partitionKeyPath' - The JSON path to be extracted from the event and used as the partition key. For more information, see <https://docs.aws.amazon.com/streams/latest/dev/key-concepts.html#partition-key Amazon Kinesis Streams Key Concepts> in the /Amazon Kinesis Streams Developer Guide/ .
mkKinesisParameters ::
  -- | 'partitionKeyPath'
  Lude.Text ->
  KinesisParameters
mkKinesisParameters pPartitionKeyPath_ =
  KinesisParameters' {partitionKeyPath = pPartitionKeyPath_}

-- | The JSON path to be extracted from the event and used as the partition key. For more information, see <https://docs.aws.amazon.com/streams/latest/dev/key-concepts.html#partition-key Amazon Kinesis Streams Key Concepts> in the /Amazon Kinesis Streams Developer Guide/ .
--
-- /Note:/ Consider using 'partitionKeyPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpPartitionKeyPath :: Lens.Lens' KinesisParameters Lude.Text
kpPartitionKeyPath = Lens.lens (partitionKeyPath :: KinesisParameters -> Lude.Text) (\s a -> s {partitionKeyPath = a} :: KinesisParameters)
{-# DEPRECATED kpPartitionKeyPath "Use generic-lens or generic-optics with 'partitionKeyPath' instead." #-}

instance Lude.FromJSON KinesisParameters where
  parseJSON =
    Lude.withObject
      "KinesisParameters"
      (\x -> KinesisParameters' Lude.<$> (x Lude..: "PartitionKeyPath"))

instance Lude.ToJSON KinesisParameters where
  toJSON KinesisParameters' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("PartitionKeyPath" Lude..= partitionKeyPath)]
      )
