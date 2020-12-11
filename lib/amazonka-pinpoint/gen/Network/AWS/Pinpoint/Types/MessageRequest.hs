-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.MessageRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.MessageRequest
  ( MessageRequest (..),

    -- * Smart constructor
    mkMessageRequest,

    -- * Lenses
    mrTraceId,
    mrContext,
    mrAddresses,
    mrTemplateConfiguration,
    mrEndpoints,
    mrMessageConfiguration,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.AddressConfiguration
import Network.AWS.Pinpoint.Types.DirectMessageConfiguration
import Network.AWS.Pinpoint.Types.EndpointSendConfiguration
import Network.AWS.Pinpoint.Types.TemplateConfiguration
import qualified Network.AWS.Prelude as Lude

-- | Specifies the configuration and other settings for a message.
--
-- /See:/ 'mkMessageRequest' smart constructor.
data MessageRequest = MessageRequest'
  { traceId ::
      Lude.Maybe Lude.Text,
    context :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    addresses ::
      Lude.Maybe (Lude.HashMap Lude.Text (AddressConfiguration)),
    templateConfiguration :: Lude.Maybe TemplateConfiguration,
    endpoints ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (EndpointSendConfiguration)),
    messageConfiguration :: DirectMessageConfiguration
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MessageRequest' with the minimum fields required to make a request.
--
-- * 'addresses' - A map of key-value pairs, where each key is an address and each value is an <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-addressconfiguration AddressConfiguration> object. An address can be a push notification token, a phone number, or an email address. You can use an <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-addressconfiguration AddressConfiguration> object to tailor the message for an address by specifying settings such as content overrides and message variables.
-- * 'context' - A map of custom attributes to attach to the message. For a push notification, this payload is added to the data.pinpoint object. For an email or text message, this payload is added to email/SMS delivery receipt event attributes.
-- * 'endpoints' - A map of key-value pairs, where each key is an endpoint ID and each value is an <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration> object. You can use an <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration> object to tailor the message for an endpoint by specifying settings such as content overrides and message variables.
-- * 'messageConfiguration' - The settings and content for the default message and any default messages that you defined for specific channels.
-- * 'templateConfiguration' - The message template to use for the message.
-- * 'traceId' - The unique identifier for tracing the message. This identifier is visible to message recipients.
mkMessageRequest ::
  -- | 'messageConfiguration'
  DirectMessageConfiguration ->
  MessageRequest
mkMessageRequest pMessageConfiguration_ =
  MessageRequest'
    { traceId = Lude.Nothing,
      context = Lude.Nothing,
      addresses = Lude.Nothing,
      templateConfiguration = Lude.Nothing,
      endpoints = Lude.Nothing,
      messageConfiguration = pMessageConfiguration_
    }

-- | The unique identifier for tracing the message. This identifier is visible to message recipients.
--
-- /Note:/ Consider using 'traceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrTraceId :: Lens.Lens' MessageRequest (Lude.Maybe Lude.Text)
mrTraceId = Lens.lens (traceId :: MessageRequest -> Lude.Maybe Lude.Text) (\s a -> s {traceId = a} :: MessageRequest)
{-# DEPRECATED mrTraceId "Use generic-lens or generic-optics with 'traceId' instead." #-}

-- | A map of custom attributes to attach to the message. For a push notification, this payload is added to the data.pinpoint object. For an email or text message, this payload is added to email/SMS delivery receipt event attributes.
--
-- /Note:/ Consider using 'context' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrContext :: Lens.Lens' MessageRequest (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
mrContext = Lens.lens (context :: MessageRequest -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {context = a} :: MessageRequest)
{-# DEPRECATED mrContext "Use generic-lens or generic-optics with 'context' instead." #-}

-- | A map of key-value pairs, where each key is an address and each value is an <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-addressconfiguration AddressConfiguration> object. An address can be a push notification token, a phone number, or an email address. You can use an <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-addressconfiguration AddressConfiguration> object to tailor the message for an address by specifying settings such as content overrides and message variables.
--
-- /Note:/ Consider using 'addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrAddresses :: Lens.Lens' MessageRequest (Lude.Maybe (Lude.HashMap Lude.Text (AddressConfiguration)))
mrAddresses = Lens.lens (addresses :: MessageRequest -> Lude.Maybe (Lude.HashMap Lude.Text (AddressConfiguration))) (\s a -> s {addresses = a} :: MessageRequest)
{-# DEPRECATED mrAddresses "Use generic-lens or generic-optics with 'addresses' instead." #-}

-- | The message template to use for the message.
--
-- /Note:/ Consider using 'templateConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrTemplateConfiguration :: Lens.Lens' MessageRequest (Lude.Maybe TemplateConfiguration)
mrTemplateConfiguration = Lens.lens (templateConfiguration :: MessageRequest -> Lude.Maybe TemplateConfiguration) (\s a -> s {templateConfiguration = a} :: MessageRequest)
{-# DEPRECATED mrTemplateConfiguration "Use generic-lens or generic-optics with 'templateConfiguration' instead." #-}

-- | A map of key-value pairs, where each key is an endpoint ID and each value is an <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration> object. You can use an <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration> object to tailor the message for an endpoint by specifying settings such as content overrides and message variables.
--
-- /Note:/ Consider using 'endpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrEndpoints :: Lens.Lens' MessageRequest (Lude.Maybe (Lude.HashMap Lude.Text (EndpointSendConfiguration)))
mrEndpoints = Lens.lens (endpoints :: MessageRequest -> Lude.Maybe (Lude.HashMap Lude.Text (EndpointSendConfiguration))) (\s a -> s {endpoints = a} :: MessageRequest)
{-# DEPRECATED mrEndpoints "Use generic-lens or generic-optics with 'endpoints' instead." #-}

-- | The settings and content for the default message and any default messages that you defined for specific channels.
--
-- /Note:/ Consider using 'messageConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrMessageConfiguration :: Lens.Lens' MessageRequest DirectMessageConfiguration
mrMessageConfiguration = Lens.lens (messageConfiguration :: MessageRequest -> DirectMessageConfiguration) (\s a -> s {messageConfiguration = a} :: MessageRequest)
{-# DEPRECATED mrMessageConfiguration "Use generic-lens or generic-optics with 'messageConfiguration' instead." #-}

instance Lude.ToJSON MessageRequest where
  toJSON MessageRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TraceId" Lude..=) Lude.<$> traceId,
            ("Context" Lude..=) Lude.<$> context,
            ("Addresses" Lude..=) Lude.<$> addresses,
            ("TemplateConfiguration" Lude..=) Lude.<$> templateConfiguration,
            ("Endpoints" Lude..=) Lude.<$> endpoints,
            Lude.Just ("MessageConfiguration" Lude..= messageConfiguration)
          ]
      )
