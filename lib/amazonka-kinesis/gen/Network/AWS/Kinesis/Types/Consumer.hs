{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.Consumer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.Consumer
  ( Consumer (..),

    -- * Smart constructor
    mkConsumer,

    -- * Lenses
    cConsumerStatus,
    cConsumerARN,
    cConsumerName,
    cConsumerCreationTimestamp,
  )
where

import Network.AWS.Kinesis.Types.ConsumerStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object that represents the details of the consumer you registered. This type of object is returned by 'RegisterStreamConsumer' .
--
-- /See:/ 'mkConsumer' smart constructor.
data Consumer = Consumer'
  { -- | A consumer can't read data while in the @CREATING@ or @DELETING@ states.
    consumerStatus :: ConsumerStatus,
    -- | When you register a consumer, Kinesis Data Streams generates an ARN for it. You need this ARN to be able to call 'SubscribeToShard' .
    --
    -- If you delete a consumer and then create a new one with the same name, it won't have the same ARN. That's because consumer ARNs contain the creation timestamp. This is important to keep in mind if you have IAM policies that reference consumer ARNs.
    consumerARN :: Lude.Text,
    -- | The name of the consumer is something you choose when you register the consumer.
    consumerName :: Lude.Text,
    -- |
    consumerCreationTimestamp :: Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Consumer' with the minimum fields required to make a request.
--
-- * 'consumerStatus' - A consumer can't read data while in the @CREATING@ or @DELETING@ states.
-- * 'consumerARN' - When you register a consumer, Kinesis Data Streams generates an ARN for it. You need this ARN to be able to call 'SubscribeToShard' .
--
-- If you delete a consumer and then create a new one with the same name, it won't have the same ARN. That's because consumer ARNs contain the creation timestamp. This is important to keep in mind if you have IAM policies that reference consumer ARNs.
-- * 'consumerName' - The name of the consumer is something you choose when you register the consumer.
-- * 'consumerCreationTimestamp' -
mkConsumer ::
  -- | 'consumerStatus'
  ConsumerStatus ->
  -- | 'consumerARN'
  Lude.Text ->
  -- | 'consumerName'
  Lude.Text ->
  -- | 'consumerCreationTimestamp'
  Lude.Timestamp ->
  Consumer
mkConsumer
  pConsumerStatus_
  pConsumerARN_
  pConsumerName_
  pConsumerCreationTimestamp_ =
    Consumer'
      { consumerStatus = pConsumerStatus_,
        consumerARN = pConsumerARN_,
        consumerName = pConsumerName_,
        consumerCreationTimestamp = pConsumerCreationTimestamp_
      }

-- | A consumer can't read data while in the @CREATING@ or @DELETING@ states.
--
-- /Note:/ Consider using 'consumerStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cConsumerStatus :: Lens.Lens' Consumer ConsumerStatus
cConsumerStatus = Lens.lens (consumerStatus :: Consumer -> ConsumerStatus) (\s a -> s {consumerStatus = a} :: Consumer)
{-# DEPRECATED cConsumerStatus "Use generic-lens or generic-optics with 'consumerStatus' instead." #-}

-- | When you register a consumer, Kinesis Data Streams generates an ARN for it. You need this ARN to be able to call 'SubscribeToShard' .
--
-- If you delete a consumer and then create a new one with the same name, it won't have the same ARN. That's because consumer ARNs contain the creation timestamp. This is important to keep in mind if you have IAM policies that reference consumer ARNs.
--
-- /Note:/ Consider using 'consumerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cConsumerARN :: Lens.Lens' Consumer Lude.Text
cConsumerARN = Lens.lens (consumerARN :: Consumer -> Lude.Text) (\s a -> s {consumerARN = a} :: Consumer)
{-# DEPRECATED cConsumerARN "Use generic-lens or generic-optics with 'consumerARN' instead." #-}

-- | The name of the consumer is something you choose when you register the consumer.
--
-- /Note:/ Consider using 'consumerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cConsumerName :: Lens.Lens' Consumer Lude.Text
cConsumerName = Lens.lens (consumerName :: Consumer -> Lude.Text) (\s a -> s {consumerName = a} :: Consumer)
{-# DEPRECATED cConsumerName "Use generic-lens or generic-optics with 'consumerName' instead." #-}

-- |
--
-- /Note:/ Consider using 'consumerCreationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cConsumerCreationTimestamp :: Lens.Lens' Consumer Lude.Timestamp
cConsumerCreationTimestamp = Lens.lens (consumerCreationTimestamp :: Consumer -> Lude.Timestamp) (\s a -> s {consumerCreationTimestamp = a} :: Consumer)
{-# DEPRECATED cConsumerCreationTimestamp "Use generic-lens or generic-optics with 'consumerCreationTimestamp' instead." #-}

instance Lude.FromJSON Consumer where
  parseJSON =
    Lude.withObject
      "Consumer"
      ( \x ->
          Consumer'
            Lude.<$> (x Lude..: "ConsumerStatus")
            Lude.<*> (x Lude..: "ConsumerARN")
            Lude.<*> (x Lude..: "ConsumerName")
            Lude.<*> (x Lude..: "ConsumerCreationTimestamp")
      )
