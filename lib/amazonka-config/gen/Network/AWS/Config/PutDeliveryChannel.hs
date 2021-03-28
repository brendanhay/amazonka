{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.PutDeliveryChannel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a delivery channel object to deliver configuration information to an Amazon S3 bucket and Amazon SNS topic.
--
-- Before you can create a delivery channel, you must create a configuration recorder.
-- You can use this action to change the Amazon S3 bucket or an Amazon SNS topic of the existing delivery channel. To change the Amazon S3 bucket or an Amazon SNS topic, call this action and specify the changed values for the S3 bucket and the SNS topic. If you specify a different value for either the S3 bucket or the SNS topic, this action will keep the existing value for the parameter that is not changed.
module Network.AWS.Config.PutDeliveryChannel
    (
    -- * Creating a request
      PutDeliveryChannel (..)
    , mkPutDeliveryChannel
    -- ** Request lenses
    , pdcDeliveryChannel

    -- * Destructuring the response
    , PutDeliveryChannelResponse (..)
    , mkPutDeliveryChannelResponse
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the 'PutDeliveryChannel' action.
--
-- /See:/ 'mkPutDeliveryChannel' smart constructor.
newtype PutDeliveryChannel = PutDeliveryChannel'
  { deliveryChannel :: Types.DeliveryChannel
    -- ^ The configuration delivery channel object that delivers the configuration information to an Amazon S3 bucket and to an Amazon SNS topic.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutDeliveryChannel' value with any optional fields omitted.
mkPutDeliveryChannel
    :: Types.DeliveryChannel -- ^ 'deliveryChannel'
    -> PutDeliveryChannel
mkPutDeliveryChannel deliveryChannel
  = PutDeliveryChannel'{deliveryChannel}

-- | The configuration delivery channel object that delivers the configuration information to an Amazon S3 bucket and to an Amazon SNS topic.
--
-- /Note:/ Consider using 'deliveryChannel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdcDeliveryChannel :: Lens.Lens' PutDeliveryChannel Types.DeliveryChannel
pdcDeliveryChannel = Lens.field @"deliveryChannel"
{-# INLINEABLE pdcDeliveryChannel #-}
{-# DEPRECATED deliveryChannel "Use generic-lens or generic-optics with 'deliveryChannel' instead"  #-}

instance Core.ToQuery PutDeliveryChannel where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutDeliveryChannel where
        toHeaders PutDeliveryChannel{..}
          = Core.pure
              ("X-Amz-Target", "StarlingDoveService.PutDeliveryChannel")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutDeliveryChannel where
        toJSON PutDeliveryChannel{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DeliveryChannel" Core..= deliveryChannel)])

instance Core.AWSRequest PutDeliveryChannel where
        type Rs PutDeliveryChannel = PutDeliveryChannelResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull PutDeliveryChannelResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutDeliveryChannelResponse' smart constructor.
data PutDeliveryChannelResponse = PutDeliveryChannelResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutDeliveryChannelResponse' value with any optional fields omitted.
mkPutDeliveryChannelResponse
    :: PutDeliveryChannelResponse
mkPutDeliveryChannelResponse = PutDeliveryChannelResponse'
