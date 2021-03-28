{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.ConsumerDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Types.ConsumerDescription
  ( ConsumerDescription (..)
  -- * Smart constructor
  , mkConsumerDescription
  -- * Lenses
  , cdConsumerName
  , cdConsumerARN
  , cdConsumerStatus
  , cdConsumerCreationTimestamp
  , cdStreamARN
  ) where

import qualified Network.AWS.Kinesis.Types.ConsumerARN as Types
import qualified Network.AWS.Kinesis.Types.ConsumerName as Types
import qualified Network.AWS.Kinesis.Types.ConsumerStatus as Types
import qualified Network.AWS.Kinesis.Types.StreamARN as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object that represents the details of a registered consumer. This type of object is returned by 'DescribeStreamConsumer' .
--
-- /See:/ 'mkConsumerDescription' smart constructor.
data ConsumerDescription = ConsumerDescription'
  { consumerName :: Types.ConsumerName
    -- ^ The name of the consumer is something you choose when you register the consumer.
  , consumerARN :: Types.ConsumerARN
    -- ^ When you register a consumer, Kinesis Data Streams generates an ARN for it. You need this ARN to be able to call 'SubscribeToShard' .
--
-- If you delete a consumer and then create a new one with the same name, it won't have the same ARN. That's because consumer ARNs contain the creation timestamp. This is important to keep in mind if you have IAM policies that reference consumer ARNs.
  , consumerStatus :: Types.ConsumerStatus
    -- ^ A consumer can't read data while in the @CREATING@ or @DELETING@ states.
  , consumerCreationTimestamp :: Core.NominalDiffTime
    -- ^ 
  , streamARN :: Types.StreamARN
    -- ^ The ARN of the stream with which you registered the consumer.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ConsumerDescription' value with any optional fields omitted.
mkConsumerDescription
    :: Types.ConsumerName -- ^ 'consumerName'
    -> Types.ConsumerARN -- ^ 'consumerARN'
    -> Types.ConsumerStatus -- ^ 'consumerStatus'
    -> Core.NominalDiffTime -- ^ 'consumerCreationTimestamp'
    -> Types.StreamARN -- ^ 'streamARN'
    -> ConsumerDescription
mkConsumerDescription consumerName consumerARN consumerStatus
  consumerCreationTimestamp streamARN
  = ConsumerDescription'{consumerName, consumerARN, consumerStatus,
                         consumerCreationTimestamp, streamARN}

-- | The name of the consumer is something you choose when you register the consumer.
--
-- /Note:/ Consider using 'consumerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdConsumerName :: Lens.Lens' ConsumerDescription Types.ConsumerName
cdConsumerName = Lens.field @"consumerName"
{-# INLINEABLE cdConsumerName #-}
{-# DEPRECATED consumerName "Use generic-lens or generic-optics with 'consumerName' instead"  #-}

-- | When you register a consumer, Kinesis Data Streams generates an ARN for it. You need this ARN to be able to call 'SubscribeToShard' .
--
-- If you delete a consumer and then create a new one with the same name, it won't have the same ARN. That's because consumer ARNs contain the creation timestamp. This is important to keep in mind if you have IAM policies that reference consumer ARNs.
--
-- /Note:/ Consider using 'consumerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdConsumerARN :: Lens.Lens' ConsumerDescription Types.ConsumerARN
cdConsumerARN = Lens.field @"consumerARN"
{-# INLINEABLE cdConsumerARN #-}
{-# DEPRECATED consumerARN "Use generic-lens or generic-optics with 'consumerARN' instead"  #-}

-- | A consumer can't read data while in the @CREATING@ or @DELETING@ states.
--
-- /Note:/ Consider using 'consumerStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdConsumerStatus :: Lens.Lens' ConsumerDescription Types.ConsumerStatus
cdConsumerStatus = Lens.field @"consumerStatus"
{-# INLINEABLE cdConsumerStatus #-}
{-# DEPRECATED consumerStatus "Use generic-lens or generic-optics with 'consumerStatus' instead"  #-}

-- | 
--
-- /Note:/ Consider using 'consumerCreationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdConsumerCreationTimestamp :: Lens.Lens' ConsumerDescription Core.NominalDiffTime
cdConsumerCreationTimestamp = Lens.field @"consumerCreationTimestamp"
{-# INLINEABLE cdConsumerCreationTimestamp #-}
{-# DEPRECATED consumerCreationTimestamp "Use generic-lens or generic-optics with 'consumerCreationTimestamp' instead"  #-}

-- | The ARN of the stream with which you registered the consumer.
--
-- /Note:/ Consider using 'streamARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdStreamARN :: Lens.Lens' ConsumerDescription Types.StreamARN
cdStreamARN = Lens.field @"streamARN"
{-# INLINEABLE cdStreamARN #-}
{-# DEPRECATED streamARN "Use generic-lens or generic-optics with 'streamARN' instead"  #-}

instance Core.FromJSON ConsumerDescription where
        parseJSON
          = Core.withObject "ConsumerDescription" Core.$
              \ x ->
                ConsumerDescription' Core.<$>
                  (x Core..: "ConsumerName") Core.<*> x Core..: "ConsumerARN"
                    Core.<*> x Core..: "ConsumerStatus"
                    Core.<*> x Core..: "ConsumerCreationTimestamp"
                    Core.<*> x Core..: "StreamARN"
