{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    PutConfigurationSetDeliveryOptions (..),
    mkPutConfigurationSetDeliveryOptions,

    -- ** Request lenses
    pcsdoConfigurationSetName,
    pcsdoDeliveryOptions,

    -- * Destructuring the response
    PutConfigurationSetDeliveryOptionsResponse (..),
    mkPutConfigurationSetDeliveryOptionsResponse,

    -- ** Response lenses
    pcsdorrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | A request to modify the delivery options for a configuration set.
--
-- /See:/ 'mkPutConfigurationSetDeliveryOptions' smart constructor.
data PutConfigurationSetDeliveryOptions = PutConfigurationSetDeliveryOptions'
  { -- | The name of the configuration set that you want to specify the delivery options for.
    configurationSetName :: Types.ConfigurationSetName,
    -- | Specifies whether messages that use the configuration set are required to use Transport Layer Security (TLS).
    deliveryOptions :: Core.Maybe Types.DeliveryOptions
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutConfigurationSetDeliveryOptions' value with any optional fields omitted.
mkPutConfigurationSetDeliveryOptions ::
  -- | 'configurationSetName'
  Types.ConfigurationSetName ->
  PutConfigurationSetDeliveryOptions
mkPutConfigurationSetDeliveryOptions configurationSetName =
  PutConfigurationSetDeliveryOptions'
    { configurationSetName,
      deliveryOptions = Core.Nothing
    }

-- | The name of the configuration set that you want to specify the delivery options for.
--
-- /Note:/ Consider using 'configurationSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcsdoConfigurationSetName :: Lens.Lens' PutConfigurationSetDeliveryOptions Types.ConfigurationSetName
pcsdoConfigurationSetName = Lens.field @"configurationSetName"
{-# DEPRECATED pcsdoConfigurationSetName "Use generic-lens or generic-optics with 'configurationSetName' instead." #-}

-- | Specifies whether messages that use the configuration set are required to use Transport Layer Security (TLS).
--
-- /Note:/ Consider using 'deliveryOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcsdoDeliveryOptions :: Lens.Lens' PutConfigurationSetDeliveryOptions (Core.Maybe Types.DeliveryOptions)
pcsdoDeliveryOptions = Lens.field @"deliveryOptions"
{-# DEPRECATED pcsdoDeliveryOptions "Use generic-lens or generic-optics with 'deliveryOptions' instead." #-}

instance Core.AWSRequest PutConfigurationSetDeliveryOptions where
  type
    Rs PutConfigurationSetDeliveryOptions =
      PutConfigurationSetDeliveryOptionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "PutConfigurationSetDeliveryOptions")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "ConfigurationSetName" configurationSetName)
                Core.<> (Core.toQueryValue "DeliveryOptions" Core.<$> deliveryOptions)
            )
      }
  response =
    Response.receiveXMLWrapper
      "PutConfigurationSetDeliveryOptionsResult"
      ( \s h x ->
          PutConfigurationSetDeliveryOptionsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | An HTTP 200 response if the request succeeds, or an error message if the request fails.
--
-- /See:/ 'mkPutConfigurationSetDeliveryOptionsResponse' smart constructor.
newtype PutConfigurationSetDeliveryOptionsResponse = PutConfigurationSetDeliveryOptionsResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutConfigurationSetDeliveryOptionsResponse' value with any optional fields omitted.
mkPutConfigurationSetDeliveryOptionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutConfigurationSetDeliveryOptionsResponse
mkPutConfigurationSetDeliveryOptionsResponse responseStatus =
  PutConfigurationSetDeliveryOptionsResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcsdorrsResponseStatus :: Lens.Lens' PutConfigurationSetDeliveryOptionsResponse Core.Int
pcsdorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED pcsdorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
