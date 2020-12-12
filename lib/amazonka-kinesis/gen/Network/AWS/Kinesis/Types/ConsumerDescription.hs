{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.ConsumerDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.ConsumerDescription
  ( ConsumerDescription (..),

    -- * Smart constructor
    mkConsumerDescription,

    -- * Lenses
    cdConsumerName,
    cdConsumerARN,
    cdConsumerStatus,
    cdConsumerCreationTimestamp,
    cdStreamARN,
  )
where

import Network.AWS.Kinesis.Types.ConsumerStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object that represents the details of a registered consumer. This type of object is returned by 'DescribeStreamConsumer' .
--
-- /See:/ 'mkConsumerDescription' smart constructor.
data ConsumerDescription = ConsumerDescription'
  { consumerName ::
      Lude.Text,
    consumerARN :: Lude.Text,
    consumerStatus :: ConsumerStatus,
    consumerCreationTimestamp :: Lude.Timestamp,
    streamARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConsumerDescription' with the minimum fields required to make a request.
--
-- * 'consumerARN' - When you register a consumer, Kinesis Data Streams generates an ARN for it. You need this ARN to be able to call 'SubscribeToShard' .
--
-- If you delete a consumer and then create a new one with the same name, it won't have the same ARN. That's because consumer ARNs contain the creation timestamp. This is important to keep in mind if you have IAM policies that reference consumer ARNs.
-- * 'consumerCreationTimestamp' -
-- * 'consumerName' - The name of the consumer is something you choose when you register the consumer.
-- * 'consumerStatus' - A consumer can't read data while in the @CREATING@ or @DELETING@ states.
-- * 'streamARN' - The ARN of the stream with which you registered the consumer.
mkConsumerDescription ::
  -- | 'consumerName'
  Lude.Text ->
  -- | 'consumerARN'
  Lude.Text ->
  -- | 'consumerStatus'
  ConsumerStatus ->
  -- | 'consumerCreationTimestamp'
  Lude.Timestamp ->
  -- | 'streamARN'
  Lude.Text ->
  ConsumerDescription
mkConsumerDescription
  pConsumerName_
  pConsumerARN_
  pConsumerStatus_
  pConsumerCreationTimestamp_
  pStreamARN_ =
    ConsumerDescription'
      { consumerName = pConsumerName_,
        consumerARN = pConsumerARN_,
        consumerStatus = pConsumerStatus_,
        consumerCreationTimestamp = pConsumerCreationTimestamp_,
        streamARN = pStreamARN_
      }

-- | The name of the consumer is something you choose when you register the consumer.
--
-- /Note:/ Consider using 'consumerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdConsumerName :: Lens.Lens' ConsumerDescription Lude.Text
cdConsumerName = Lens.lens (consumerName :: ConsumerDescription -> Lude.Text) (\s a -> s {consumerName = a} :: ConsumerDescription)
{-# DEPRECATED cdConsumerName "Use generic-lens or generic-optics with 'consumerName' instead." #-}

-- | When you register a consumer, Kinesis Data Streams generates an ARN for it. You need this ARN to be able to call 'SubscribeToShard' .
--
-- If you delete a consumer and then create a new one with the same name, it won't have the same ARN. That's because consumer ARNs contain the creation timestamp. This is important to keep in mind if you have IAM policies that reference consumer ARNs.
--
-- /Note:/ Consider using 'consumerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdConsumerARN :: Lens.Lens' ConsumerDescription Lude.Text
cdConsumerARN = Lens.lens (consumerARN :: ConsumerDescription -> Lude.Text) (\s a -> s {consumerARN = a} :: ConsumerDescription)
{-# DEPRECATED cdConsumerARN "Use generic-lens or generic-optics with 'consumerARN' instead." #-}

-- | A consumer can't read data while in the @CREATING@ or @DELETING@ states.
--
-- /Note:/ Consider using 'consumerStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdConsumerStatus :: Lens.Lens' ConsumerDescription ConsumerStatus
cdConsumerStatus = Lens.lens (consumerStatus :: ConsumerDescription -> ConsumerStatus) (\s a -> s {consumerStatus = a} :: ConsumerDescription)
{-# DEPRECATED cdConsumerStatus "Use generic-lens or generic-optics with 'consumerStatus' instead." #-}

-- |
--
-- /Note:/ Consider using 'consumerCreationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdConsumerCreationTimestamp :: Lens.Lens' ConsumerDescription Lude.Timestamp
cdConsumerCreationTimestamp = Lens.lens (consumerCreationTimestamp :: ConsumerDescription -> Lude.Timestamp) (\s a -> s {consumerCreationTimestamp = a} :: ConsumerDescription)
{-# DEPRECATED cdConsumerCreationTimestamp "Use generic-lens or generic-optics with 'consumerCreationTimestamp' instead." #-}

-- | The ARN of the stream with which you registered the consumer.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdStreamARN :: Lens.Lens' ConsumerDescription Lude.Text
cdStreamARN = Lens.lens (streamARN :: ConsumerDescription -> Lude.Text) (\s a -> s {streamARN = a} :: ConsumerDescription)
{-# DEPRECATED cdStreamARN "Use generic-lens or generic-optics with 'streamARN' instead." #-}

instance Lude.FromJSON ConsumerDescription where
  parseJSON =
    Lude.withObject
      "ConsumerDescription"
      ( \x ->
          ConsumerDescription'
            Lude.<$> (x Lude..: "ConsumerName")
            Lude.<*> (x Lude..: "ConsumerARN")
            Lude.<*> (x Lude..: "ConsumerStatus")
            Lude.<*> (x Lude..: "ConsumerCreationTimestamp")
            Lude.<*> (x Lude..: "StreamARN")
      )
