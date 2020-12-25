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
    cConsumerName,
    cConsumerARN,
    cConsumerStatus,
    cConsumerCreationTimestamp,
  )
where

import qualified Network.AWS.Kinesis.Types.ConsumerARN as Types
import qualified Network.AWS.Kinesis.Types.ConsumerName as Types
import qualified Network.AWS.Kinesis.Types.ConsumerStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object that represents the details of the consumer you registered. This type of object is returned by 'RegisterStreamConsumer' .
--
-- /See:/ 'mkConsumer' smart constructor.
data Consumer = Consumer'
  { -- | The name of the consumer is something you choose when you register the consumer.
    consumerName :: Types.ConsumerName,
    -- | When you register a consumer, Kinesis Data Streams generates an ARN for it. You need this ARN to be able to call 'SubscribeToShard' .
    --
    -- If you delete a consumer and then create a new one with the same name, it won't have the same ARN. That's because consumer ARNs contain the creation timestamp. This is important to keep in mind if you have IAM policies that reference consumer ARNs.
    consumerARN :: Types.ConsumerARN,
    -- | A consumer can't read data while in the @CREATING@ or @DELETING@ states.
    consumerStatus :: Types.ConsumerStatus,
    -- |
    consumerCreationTimestamp :: Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Consumer' value with any optional fields omitted.
mkConsumer ::
  -- | 'consumerName'
  Types.ConsumerName ->
  -- | 'consumerARN'
  Types.ConsumerARN ->
  -- | 'consumerStatus'
  Types.ConsumerStatus ->
  -- | 'consumerCreationTimestamp'
  Core.NominalDiffTime ->
  Consumer
mkConsumer
  consumerName
  consumerARN
  consumerStatus
  consumerCreationTimestamp =
    Consumer'
      { consumerName,
        consumerARN,
        consumerStatus,
        consumerCreationTimestamp
      }

-- | The name of the consumer is something you choose when you register the consumer.
--
-- /Note:/ Consider using 'consumerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cConsumerName :: Lens.Lens' Consumer Types.ConsumerName
cConsumerName = Lens.field @"consumerName"
{-# DEPRECATED cConsumerName "Use generic-lens or generic-optics with 'consumerName' instead." #-}

-- | When you register a consumer, Kinesis Data Streams generates an ARN for it. You need this ARN to be able to call 'SubscribeToShard' .
--
-- If you delete a consumer and then create a new one with the same name, it won't have the same ARN. That's because consumer ARNs contain the creation timestamp. This is important to keep in mind if you have IAM policies that reference consumer ARNs.
--
-- /Note:/ Consider using 'consumerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cConsumerARN :: Lens.Lens' Consumer Types.ConsumerARN
cConsumerARN = Lens.field @"consumerARN"
{-# DEPRECATED cConsumerARN "Use generic-lens or generic-optics with 'consumerARN' instead." #-}

-- | A consumer can't read data while in the @CREATING@ or @DELETING@ states.
--
-- /Note:/ Consider using 'consumerStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cConsumerStatus :: Lens.Lens' Consumer Types.ConsumerStatus
cConsumerStatus = Lens.field @"consumerStatus"
{-# DEPRECATED cConsumerStatus "Use generic-lens or generic-optics with 'consumerStatus' instead." #-}

-- |
--
-- /Note:/ Consider using 'consumerCreationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cConsumerCreationTimestamp :: Lens.Lens' Consumer Core.NominalDiffTime
cConsumerCreationTimestamp = Lens.field @"consumerCreationTimestamp"
{-# DEPRECATED cConsumerCreationTimestamp "Use generic-lens or generic-optics with 'consumerCreationTimestamp' instead." #-}

instance Core.FromJSON Consumer where
  parseJSON =
    Core.withObject "Consumer" Core.$
      \x ->
        Consumer'
          Core.<$> (x Core..: "ConsumerName")
          Core.<*> (x Core..: "ConsumerARN")
          Core.<*> (x Core..: "ConsumerStatus")
          Core.<*> (x Core..: "ConsumerCreationTimestamp")
