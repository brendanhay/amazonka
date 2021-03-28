{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.MessageRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.MessageRequest
  ( MessageRequest (..)
  -- * Smart constructor
  , mkMessageRequest
  -- * Lenses
  , mrMessageConfiguration
  , mrAddresses
  , mrContext
  , mrEndpoints
  , mrTemplateConfiguration
  , mrTraceId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.AddressConfiguration as Types
import qualified Network.AWS.Pinpoint.Types.DirectMessageConfiguration as Types
import qualified Network.AWS.Pinpoint.Types.EndpointSendConfiguration as Types
import qualified Network.AWS.Pinpoint.Types.TemplateConfiguration as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the configuration and other settings for a message.
--
-- /See:/ 'mkMessageRequest' smart constructor.
data MessageRequest = MessageRequest'
  { messageConfiguration :: Types.DirectMessageConfiguration
    -- ^ The settings and content for the default message and any default messages that you defined for specific channels.
  , addresses :: Core.Maybe (Core.HashMap Core.Text Types.AddressConfiguration)
    -- ^ A map of key-value pairs, where each key is an address and each value is an <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-addressconfiguration AddressConfiguration> object. An address can be a push notification token, a phone number, or an email address. You can use an <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-addressconfiguration AddressConfiguration> object to tailor the message for an address by specifying settings such as content overrides and message variables.
  , context :: Core.Maybe (Core.HashMap Core.Text Core.Text)
    -- ^ A map of custom attributes to attach to the message. For a push notification, this payload is added to the data.pinpoint object. For an email or text message, this payload is added to email/SMS delivery receipt event attributes.
  , endpoints :: Core.Maybe (Core.HashMap Core.Text Types.EndpointSendConfiguration)
    -- ^ A map of key-value pairs, where each key is an endpoint ID and each value is an <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration> object. You can use an <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration> object to tailor the message for an endpoint by specifying settings such as content overrides and message variables.
  , templateConfiguration :: Core.Maybe Types.TemplateConfiguration
    -- ^ The message template to use for the message.
  , traceId :: Core.Maybe Core.Text
    -- ^ The unique identifier for tracing the message. This identifier is visible to message recipients.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MessageRequest' value with any optional fields omitted.
mkMessageRequest
    :: Types.DirectMessageConfiguration -- ^ 'messageConfiguration'
    -> MessageRequest
mkMessageRequest messageConfiguration
  = MessageRequest'{messageConfiguration, addresses = Core.Nothing,
                    context = Core.Nothing, endpoints = Core.Nothing,
                    templateConfiguration = Core.Nothing, traceId = Core.Nothing}

-- | The settings and content for the default message and any default messages that you defined for specific channels.
--
-- /Note:/ Consider using 'messageConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrMessageConfiguration :: Lens.Lens' MessageRequest Types.DirectMessageConfiguration
mrMessageConfiguration = Lens.field @"messageConfiguration"
{-# INLINEABLE mrMessageConfiguration #-}
{-# DEPRECATED messageConfiguration "Use generic-lens or generic-optics with 'messageConfiguration' instead"  #-}

-- | A map of key-value pairs, where each key is an address and each value is an <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-addressconfiguration AddressConfiguration> object. An address can be a push notification token, a phone number, or an email address. You can use an <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-addressconfiguration AddressConfiguration> object to tailor the message for an address by specifying settings such as content overrides and message variables.
--
-- /Note:/ Consider using 'addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrAddresses :: Lens.Lens' MessageRequest (Core.Maybe (Core.HashMap Core.Text Types.AddressConfiguration))
mrAddresses = Lens.field @"addresses"
{-# INLINEABLE mrAddresses #-}
{-# DEPRECATED addresses "Use generic-lens or generic-optics with 'addresses' instead"  #-}

-- | A map of custom attributes to attach to the message. For a push notification, this payload is added to the data.pinpoint object. For an email or text message, this payload is added to email/SMS delivery receipt event attributes.
--
-- /Note:/ Consider using 'context' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrContext :: Lens.Lens' MessageRequest (Core.Maybe (Core.HashMap Core.Text Core.Text))
mrContext = Lens.field @"context"
{-# INLINEABLE mrContext #-}
{-# DEPRECATED context "Use generic-lens or generic-optics with 'context' instead"  #-}

-- | A map of key-value pairs, where each key is an endpoint ID and each value is an <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration> object. You can use an <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration> object to tailor the message for an endpoint by specifying settings such as content overrides and message variables.
--
-- /Note:/ Consider using 'endpoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrEndpoints :: Lens.Lens' MessageRequest (Core.Maybe (Core.HashMap Core.Text Types.EndpointSendConfiguration))
mrEndpoints = Lens.field @"endpoints"
{-# INLINEABLE mrEndpoints #-}
{-# DEPRECATED endpoints "Use generic-lens or generic-optics with 'endpoints' instead"  #-}

-- | The message template to use for the message.
--
-- /Note:/ Consider using 'templateConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrTemplateConfiguration :: Lens.Lens' MessageRequest (Core.Maybe Types.TemplateConfiguration)
mrTemplateConfiguration = Lens.field @"templateConfiguration"
{-# INLINEABLE mrTemplateConfiguration #-}
{-# DEPRECATED templateConfiguration "Use generic-lens or generic-optics with 'templateConfiguration' instead"  #-}

-- | The unique identifier for tracing the message. This identifier is visible to message recipients.
--
-- /Note:/ Consider using 'traceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrTraceId :: Lens.Lens' MessageRequest (Core.Maybe Core.Text)
mrTraceId = Lens.field @"traceId"
{-# INLINEABLE mrTraceId #-}
{-# DEPRECATED traceId "Use generic-lens or generic-optics with 'traceId' instead"  #-}

instance Core.FromJSON MessageRequest where
        toJSON MessageRequest{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("MessageConfiguration" Core..= messageConfiguration),
                  ("Addresses" Core..=) Core.<$> addresses,
                  ("Context" Core..=) Core.<$> context,
                  ("Endpoints" Core..=) Core.<$> endpoints,
                  ("TemplateConfiguration" Core..=) Core.<$> templateConfiguration,
                  ("TraceId" Core..=) Core.<$> traceId])
