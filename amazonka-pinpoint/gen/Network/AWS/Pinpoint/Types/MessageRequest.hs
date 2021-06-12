{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.MessageRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.MessageRequest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.AddressConfiguration
import Network.AWS.Pinpoint.Types.DirectMessageConfiguration
import Network.AWS.Pinpoint.Types.EndpointSendConfiguration
import Network.AWS.Pinpoint.Types.TemplateConfiguration

-- | Specifies the configuration and other settings for a message.
--
-- /See:/ 'newMessageRequest' smart constructor.
data MessageRequest = MessageRequest'
  { -- | A map of key-value pairs, where each key is an endpoint ID and each
    -- value is an
    -- <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration>
    -- object. You can use an
    -- <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration>
    -- object to tailor the message for an endpoint by specifying settings such
    -- as content overrides and message variables.
    endpoints :: Core.Maybe (Core.HashMap Core.Text EndpointSendConfiguration),
    -- | A map of custom attributes to attach to the message. For a push
    -- notification, this payload is added to the data.pinpoint object. For an
    -- email or text message, this payload is added to email\/SMS delivery
    -- receipt event attributes.
    context :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The unique identifier for tracing the message. This identifier is
    -- visible to message recipients.
    traceId :: Core.Maybe Core.Text,
    -- | A map of key-value pairs, where each key is an address and each value is
    -- an
    -- <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-addressconfiguration AddressConfiguration>
    -- object. An address can be a push notification token, a phone number, or
    -- an email address. You can use an
    -- <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-addressconfiguration AddressConfiguration>
    -- object to tailor the message for an address by specifying settings such
    -- as content overrides and message variables.
    addresses :: Core.Maybe (Core.HashMap Core.Text AddressConfiguration),
    -- | The message template to use for the message.
    templateConfiguration :: Core.Maybe TemplateConfiguration,
    -- | The settings and content for the default message and any default
    -- messages that you defined for specific channels.
    messageConfiguration :: DirectMessageConfiguration
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MessageRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpoints', 'messageRequest_endpoints' - A map of key-value pairs, where each key is an endpoint ID and each
-- value is an
-- <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration>
-- object. You can use an
-- <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration>
-- object to tailor the message for an endpoint by specifying settings such
-- as content overrides and message variables.
--
-- 'context', 'messageRequest_context' - A map of custom attributes to attach to the message. For a push
-- notification, this payload is added to the data.pinpoint object. For an
-- email or text message, this payload is added to email\/SMS delivery
-- receipt event attributes.
--
-- 'traceId', 'messageRequest_traceId' - The unique identifier for tracing the message. This identifier is
-- visible to message recipients.
--
-- 'addresses', 'messageRequest_addresses' - A map of key-value pairs, where each key is an address and each value is
-- an
-- <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-addressconfiguration AddressConfiguration>
-- object. An address can be a push notification token, a phone number, or
-- an email address. You can use an
-- <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-addressconfiguration AddressConfiguration>
-- object to tailor the message for an address by specifying settings such
-- as content overrides and message variables.
--
-- 'templateConfiguration', 'messageRequest_templateConfiguration' - The message template to use for the message.
--
-- 'messageConfiguration', 'messageRequest_messageConfiguration' - The settings and content for the default message and any default
-- messages that you defined for specific channels.
newMessageRequest ::
  -- | 'messageConfiguration'
  DirectMessageConfiguration ->
  MessageRequest
newMessageRequest pMessageConfiguration_ =
  MessageRequest'
    { endpoints = Core.Nothing,
      context = Core.Nothing,
      traceId = Core.Nothing,
      addresses = Core.Nothing,
      templateConfiguration = Core.Nothing,
      messageConfiguration = pMessageConfiguration_
    }

-- | A map of key-value pairs, where each key is an endpoint ID and each
-- value is an
-- <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration>
-- object. You can use an
-- <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration>
-- object to tailor the message for an endpoint by specifying settings such
-- as content overrides and message variables.
messageRequest_endpoints :: Lens.Lens' MessageRequest (Core.Maybe (Core.HashMap Core.Text EndpointSendConfiguration))
messageRequest_endpoints = Lens.lens (\MessageRequest' {endpoints} -> endpoints) (\s@MessageRequest' {} a -> s {endpoints = a} :: MessageRequest) Core.. Lens.mapping Lens._Coerce

-- | A map of custom attributes to attach to the message. For a push
-- notification, this payload is added to the data.pinpoint object. For an
-- email or text message, this payload is added to email\/SMS delivery
-- receipt event attributes.
messageRequest_context :: Lens.Lens' MessageRequest (Core.Maybe (Core.HashMap Core.Text Core.Text))
messageRequest_context = Lens.lens (\MessageRequest' {context} -> context) (\s@MessageRequest' {} a -> s {context = a} :: MessageRequest) Core.. Lens.mapping Lens._Coerce

-- | The unique identifier for tracing the message. This identifier is
-- visible to message recipients.
messageRequest_traceId :: Lens.Lens' MessageRequest (Core.Maybe Core.Text)
messageRequest_traceId = Lens.lens (\MessageRequest' {traceId} -> traceId) (\s@MessageRequest' {} a -> s {traceId = a} :: MessageRequest)

-- | A map of key-value pairs, where each key is an address and each value is
-- an
-- <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-addressconfiguration AddressConfiguration>
-- object. An address can be a push notification token, a phone number, or
-- an email address. You can use an
-- <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-addressconfiguration AddressConfiguration>
-- object to tailor the message for an address by specifying settings such
-- as content overrides and message variables.
messageRequest_addresses :: Lens.Lens' MessageRequest (Core.Maybe (Core.HashMap Core.Text AddressConfiguration))
messageRequest_addresses = Lens.lens (\MessageRequest' {addresses} -> addresses) (\s@MessageRequest' {} a -> s {addresses = a} :: MessageRequest) Core.. Lens.mapping Lens._Coerce

-- | The message template to use for the message.
messageRequest_templateConfiguration :: Lens.Lens' MessageRequest (Core.Maybe TemplateConfiguration)
messageRequest_templateConfiguration = Lens.lens (\MessageRequest' {templateConfiguration} -> templateConfiguration) (\s@MessageRequest' {} a -> s {templateConfiguration = a} :: MessageRequest)

-- | The settings and content for the default message and any default
-- messages that you defined for specific channels.
messageRequest_messageConfiguration :: Lens.Lens' MessageRequest DirectMessageConfiguration
messageRequest_messageConfiguration = Lens.lens (\MessageRequest' {messageConfiguration} -> messageConfiguration) (\s@MessageRequest' {} a -> s {messageConfiguration = a} :: MessageRequest)

instance Core.Hashable MessageRequest

instance Core.NFData MessageRequest

instance Core.ToJSON MessageRequest where
  toJSON MessageRequest' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Endpoints" Core..=) Core.<$> endpoints,
            ("Context" Core..=) Core.<$> context,
            ("TraceId" Core..=) Core.<$> traceId,
            ("Addresses" Core..=) Core.<$> addresses,
            ("TemplateConfiguration" Core..=)
              Core.<$> templateConfiguration,
            Core.Just
              ( "MessageConfiguration"
                  Core..= messageConfiguration
              )
          ]
      )
