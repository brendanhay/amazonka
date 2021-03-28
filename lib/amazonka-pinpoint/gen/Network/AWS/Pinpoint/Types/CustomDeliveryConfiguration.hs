{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CustomDeliveryConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.CustomDeliveryConfiguration
  ( CustomDeliveryConfiguration (..)
  -- * Smart constructor
  , mkCustomDeliveryConfiguration
  -- * Lenses
  , cdcDeliveryUri
  , cdcEndpointTypes
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.EndpointTypesElement as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the delivery configuration settings for sending a campaign or campaign treatment through a custom channel. This object is required if you use the CampaignCustomMessage object to define the message to send for the campaign or campaign treatment.
--
-- /See:/ 'mkCustomDeliveryConfiguration' smart constructor.
data CustomDeliveryConfiguration = CustomDeliveryConfiguration'
  { deliveryUri :: Core.Text
    -- ^ The destination to send the campaign or treatment to. This value can be one of the following:
--
--
--     * The name or Amazon Resource Name (ARN) of an AWS Lambda function to invoke to handle delivery of the campaign or treatment.
--
--
--     * The URL for a web application or service that supports HTTPS and can receive the message. The URL has to be a full URL, including the HTTPS protocol.
--
--
  , endpointTypes :: Core.Maybe [Types.EndpointTypesElement]
    -- ^ The types of endpoints to send the campaign or treatment to. Each valid value maps to a type of channel that you can associate with an endpoint by using the ChannelType property of an endpoint.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CustomDeliveryConfiguration' value with any optional fields omitted.
mkCustomDeliveryConfiguration
    :: Core.Text -- ^ 'deliveryUri'
    -> CustomDeliveryConfiguration
mkCustomDeliveryConfiguration deliveryUri
  = CustomDeliveryConfiguration'{deliveryUri,
                                 endpointTypes = Core.Nothing}

-- | The destination to send the campaign or treatment to. This value can be one of the following:
--
--
--     * The name or Amazon Resource Name (ARN) of an AWS Lambda function to invoke to handle delivery of the campaign or treatment.
--
--
--     * The URL for a web application or service that supports HTTPS and can receive the message. The URL has to be a full URL, including the HTTPS protocol.
--
--
--
-- /Note:/ Consider using 'deliveryUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcDeliveryUri :: Lens.Lens' CustomDeliveryConfiguration Core.Text
cdcDeliveryUri = Lens.field @"deliveryUri"
{-# INLINEABLE cdcDeliveryUri #-}
{-# DEPRECATED deliveryUri "Use generic-lens or generic-optics with 'deliveryUri' instead"  #-}

-- | The types of endpoints to send the campaign or treatment to. Each valid value maps to a type of channel that you can associate with an endpoint by using the ChannelType property of an endpoint.
--
-- /Note:/ Consider using 'endpointTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcEndpointTypes :: Lens.Lens' CustomDeliveryConfiguration (Core.Maybe [Types.EndpointTypesElement])
cdcEndpointTypes = Lens.field @"endpointTypes"
{-# INLINEABLE cdcEndpointTypes #-}
{-# DEPRECATED endpointTypes "Use generic-lens or generic-optics with 'endpointTypes' instead"  #-}

instance Core.FromJSON CustomDeliveryConfiguration where
        toJSON CustomDeliveryConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DeliveryUri" Core..= deliveryUri),
                  ("EndpointTypes" Core..=) Core.<$> endpointTypes])

instance Core.FromJSON CustomDeliveryConfiguration where
        parseJSON
          = Core.withObject "CustomDeliveryConfiguration" Core.$
              \ x ->
                CustomDeliveryConfiguration' Core.<$>
                  (x Core..: "DeliveryUri") Core.<*> x Core..:? "EndpointTypes"
