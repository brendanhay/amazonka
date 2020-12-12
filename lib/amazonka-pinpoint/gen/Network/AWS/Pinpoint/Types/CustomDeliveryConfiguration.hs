{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.CustomDeliveryConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.CustomDeliveryConfiguration
  ( CustomDeliveryConfiguration (..),

    -- * Smart constructor
    mkCustomDeliveryConfiguration,

    -- * Lenses
    cdcEndpointTypes,
    cdcDeliveryURI,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.EndpointTypesElement
import qualified Network.AWS.Prelude as Lude

-- | Specifies the delivery configuration settings for sending a campaign or campaign treatment through a custom channel. This object is required if you use the CampaignCustomMessage object to define the message to send for the campaign or campaign treatment.
--
-- /See:/ 'mkCustomDeliveryConfiguration' smart constructor.
data CustomDeliveryConfiguration = CustomDeliveryConfiguration'
  { endpointTypes ::
      Lude.Maybe [EndpointTypesElement],
    deliveryURI :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CustomDeliveryConfiguration' with the minimum fields required to make a request.
--
-- * 'deliveryURI' - The destination to send the campaign or treatment to. This value can be one of the following:
--
--
--     * The name or Amazon Resource Name (ARN) of an AWS Lambda function to invoke to handle delivery of the campaign or treatment.
--
--
--     * The URL for a web application or service that supports HTTPS and can receive the message. The URL has to be a full URL, including the HTTPS protocol.
--
--
-- * 'endpointTypes' - The types of endpoints to send the campaign or treatment to. Each valid value maps to a type of channel that you can associate with an endpoint by using the ChannelType property of an endpoint.
mkCustomDeliveryConfiguration ::
  -- | 'deliveryURI'
  Lude.Text ->
  CustomDeliveryConfiguration
mkCustomDeliveryConfiguration pDeliveryURI_ =
  CustomDeliveryConfiguration'
    { endpointTypes = Lude.Nothing,
      deliveryURI = pDeliveryURI_
    }

-- | The types of endpoints to send the campaign or treatment to. Each valid value maps to a type of channel that you can associate with an endpoint by using the ChannelType property of an endpoint.
--
-- /Note:/ Consider using 'endpointTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcEndpointTypes :: Lens.Lens' CustomDeliveryConfiguration (Lude.Maybe [EndpointTypesElement])
cdcEndpointTypes = Lens.lens (endpointTypes :: CustomDeliveryConfiguration -> Lude.Maybe [EndpointTypesElement]) (\s a -> s {endpointTypes = a} :: CustomDeliveryConfiguration)
{-# DEPRECATED cdcEndpointTypes "Use generic-lens or generic-optics with 'endpointTypes' instead." #-}

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
-- /Note:/ Consider using 'deliveryURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcDeliveryURI :: Lens.Lens' CustomDeliveryConfiguration Lude.Text
cdcDeliveryURI = Lens.lens (deliveryURI :: CustomDeliveryConfiguration -> Lude.Text) (\s a -> s {deliveryURI = a} :: CustomDeliveryConfiguration)
{-# DEPRECATED cdcDeliveryURI "Use generic-lens or generic-optics with 'deliveryURI' instead." #-}

instance Lude.FromJSON CustomDeliveryConfiguration where
  parseJSON =
    Lude.withObject
      "CustomDeliveryConfiguration"
      ( \x ->
          CustomDeliveryConfiguration'
            Lude.<$> (x Lude..:? "EndpointTypes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "DeliveryUri")
      )

instance Lude.ToJSON CustomDeliveryConfiguration where
  toJSON CustomDeliveryConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EndpointTypes" Lude..=) Lude.<$> endpointTypes,
            Lude.Just ("DeliveryUri" Lude..= deliveryURI)
          ]
      )
