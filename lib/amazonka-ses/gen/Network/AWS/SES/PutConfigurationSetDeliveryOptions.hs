{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.PutConfigurationSetDeliveryOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates the delivery options for a configuration set.
module Network.AWS.SES.PutConfigurationSetDeliveryOptions
    (
    -- * Creating a request
      PutConfigurationSetDeliveryOptions (..)
    , mkPutConfigurationSetDeliveryOptions
    -- ** Request lenses
    , pcsdoConfigurationSetName
    , pcsdoDeliveryOptions

    -- * Destructuring the response
    , PutConfigurationSetDeliveryOptionsResponse (..)
    , mkPutConfigurationSetDeliveryOptionsResponse
    -- ** Response lenses
    , pcsdorrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | A request to modify the delivery options for a configuration set.
--
-- /See:/ 'mkPutConfigurationSetDeliveryOptions' smart constructor.
data PutConfigurationSetDeliveryOptions = PutConfigurationSetDeliveryOptions'
  { configurationSetName :: Types.ConfigurationSetName
    -- ^ The name of the configuration set that you want to specify the delivery options for.
  , deliveryOptions :: Core.Maybe Types.DeliveryOptions
    -- ^ Specifies whether messages that use the configuration set are required to use Transport Layer Security (TLS).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutConfigurationSetDeliveryOptions' value with any optional fields omitted.
mkPutConfigurationSetDeliveryOptions
    :: Types.ConfigurationSetName -- ^ 'configurationSetName'
    -> PutConfigurationSetDeliveryOptions
mkPutConfigurationSetDeliveryOptions configurationSetName
  = PutConfigurationSetDeliveryOptions'{configurationSetName,
                                        deliveryOptions = Core.Nothing}

-- | The name of the configuration set that you want to specify the delivery options for.
--
-- /Note:/ Consider using 'configurationSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcsdoConfigurationSetName :: Lens.Lens' PutConfigurationSetDeliveryOptions Types.ConfigurationSetName
pcsdoConfigurationSetName = Lens.field @"configurationSetName"
{-# INLINEABLE pcsdoConfigurationSetName #-}
{-# DEPRECATED configurationSetName "Use generic-lens or generic-optics with 'configurationSetName' instead"  #-}

-- | Specifies whether messages that use the configuration set are required to use Transport Layer Security (TLS).
--
-- /Note:/ Consider using 'deliveryOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcsdoDeliveryOptions :: Lens.Lens' PutConfigurationSetDeliveryOptions (Core.Maybe Types.DeliveryOptions)
pcsdoDeliveryOptions = Lens.field @"deliveryOptions"
{-# INLINEABLE pcsdoDeliveryOptions #-}
{-# DEPRECATED deliveryOptions "Use generic-lens or generic-optics with 'deliveryOptions' instead"  #-}

instance Core.ToQuery PutConfigurationSetDeliveryOptions where
        toQuery PutConfigurationSetDeliveryOptions{..}
          = Core.toQueryPair "Action"
              ("PutConfigurationSetDeliveryOptions" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2010-12-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "ConfigurationSetName" configurationSetName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DeliveryOptions")
                deliveryOptions

instance Core.ToHeaders PutConfigurationSetDeliveryOptions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest PutConfigurationSetDeliveryOptions where
        type Rs PutConfigurationSetDeliveryOptions =
             PutConfigurationSetDeliveryOptionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper
              "PutConfigurationSetDeliveryOptionsResult"
              (\ s h x ->
                 PutConfigurationSetDeliveryOptionsResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | An HTTP 200 response if the request succeeds, or an error message if the request fails.
--
-- /See:/ 'mkPutConfigurationSetDeliveryOptionsResponse' smart constructor.
newtype PutConfigurationSetDeliveryOptionsResponse = PutConfigurationSetDeliveryOptionsResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutConfigurationSetDeliveryOptionsResponse' value with any optional fields omitted.
mkPutConfigurationSetDeliveryOptionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutConfigurationSetDeliveryOptionsResponse
mkPutConfigurationSetDeliveryOptionsResponse responseStatus
  = PutConfigurationSetDeliveryOptionsResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcsdorrsResponseStatus :: Lens.Lens' PutConfigurationSetDeliveryOptionsResponse Core.Int
pcsdorrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE pcsdorrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
