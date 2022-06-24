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
-- Module      : Amazonka.Pinpoint.Types.MessageRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.MessageRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types.AddressConfiguration
import Amazonka.Pinpoint.Types.DirectMessageConfiguration
import Amazonka.Pinpoint.Types.EndpointSendConfiguration
import Amazonka.Pinpoint.Types.TemplateConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Specifies the configuration and other settings for a message.
--
-- /See:/ 'newMessageRequest' smart constructor.
data MessageRequest = MessageRequest'
  { -- | A map of custom attributes to attach to the message. For a push
    -- notification, this payload is added to the data.pinpoint object. For an
    -- email or text message, this payload is added to email\/SMS delivery
    -- receipt event attributes.
    context :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A map of key-value pairs, where each key is an endpoint ID and each
    -- value is an
    -- <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration>
    -- object. You can use an
    -- <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration>
    -- object to tailor the message for an endpoint by specifying settings such
    -- as content overrides and message variables.
    endpoints :: Prelude.Maybe (Prelude.HashMap Prelude.Text EndpointSendConfiguration),
    -- | A map of key-value pairs, where each key is an address and each value is
    -- an
    -- <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-addressconfiguration AddressConfiguration>
    -- object. An address can be a push notification token, a phone number, or
    -- an email address. You can use an
    -- <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-addressconfiguration AddressConfiguration>
    -- object to tailor the message for an address by specifying settings such
    -- as content overrides and message variables.
    addresses :: Prelude.Maybe (Prelude.HashMap Prelude.Text AddressConfiguration),
    -- | The message template to use for the message.
    templateConfiguration :: Prelude.Maybe TemplateConfiguration,
    -- | The unique identifier for tracing the message. This identifier is
    -- visible to message recipients.
    traceId :: Prelude.Maybe Prelude.Text,
    -- | The settings and content for the default message and any default
    -- messages that you defined for specific channels.
    messageConfiguration :: DirectMessageConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MessageRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'context', 'messageRequest_context' - A map of custom attributes to attach to the message. For a push
-- notification, this payload is added to the data.pinpoint object. For an
-- email or text message, this payload is added to email\/SMS delivery
-- receipt event attributes.
--
-- 'endpoints', 'messageRequest_endpoints' - A map of key-value pairs, where each key is an endpoint ID and each
-- value is an
-- <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration>
-- object. You can use an
-- <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration>
-- object to tailor the message for an endpoint by specifying settings such
-- as content overrides and message variables.
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
-- 'traceId', 'messageRequest_traceId' - The unique identifier for tracing the message. This identifier is
-- visible to message recipients.
--
-- 'messageConfiguration', 'messageRequest_messageConfiguration' - The settings and content for the default message and any default
-- messages that you defined for specific channels.
newMessageRequest ::
  -- | 'messageConfiguration'
  DirectMessageConfiguration ->
  MessageRequest
newMessageRequest pMessageConfiguration_ =
  MessageRequest'
    { context = Prelude.Nothing,
      endpoints = Prelude.Nothing,
      addresses = Prelude.Nothing,
      templateConfiguration = Prelude.Nothing,
      traceId = Prelude.Nothing,
      messageConfiguration = pMessageConfiguration_
    }

-- | A map of custom attributes to attach to the message. For a push
-- notification, this payload is added to the data.pinpoint object. For an
-- email or text message, this payload is added to email\/SMS delivery
-- receipt event attributes.
messageRequest_context :: Lens.Lens' MessageRequest (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
messageRequest_context = Lens.lens (\MessageRequest' {context} -> context) (\s@MessageRequest' {} a -> s {context = a} :: MessageRequest) Prelude.. Lens.mapping Lens.coerced

-- | A map of key-value pairs, where each key is an endpoint ID and each
-- value is an
-- <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration>
-- object. You can use an
-- <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration>
-- object to tailor the message for an endpoint by specifying settings such
-- as content overrides and message variables.
messageRequest_endpoints :: Lens.Lens' MessageRequest (Prelude.Maybe (Prelude.HashMap Prelude.Text EndpointSendConfiguration))
messageRequest_endpoints = Lens.lens (\MessageRequest' {endpoints} -> endpoints) (\s@MessageRequest' {} a -> s {endpoints = a} :: MessageRequest) Prelude.. Lens.mapping Lens.coerced

-- | A map of key-value pairs, where each key is an address and each value is
-- an
-- <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-addressconfiguration AddressConfiguration>
-- object. An address can be a push notification token, a phone number, or
-- an email address. You can use an
-- <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-addressconfiguration AddressConfiguration>
-- object to tailor the message for an address by specifying settings such
-- as content overrides and message variables.
messageRequest_addresses :: Lens.Lens' MessageRequest (Prelude.Maybe (Prelude.HashMap Prelude.Text AddressConfiguration))
messageRequest_addresses = Lens.lens (\MessageRequest' {addresses} -> addresses) (\s@MessageRequest' {} a -> s {addresses = a} :: MessageRequest) Prelude.. Lens.mapping Lens.coerced

-- | The message template to use for the message.
messageRequest_templateConfiguration :: Lens.Lens' MessageRequest (Prelude.Maybe TemplateConfiguration)
messageRequest_templateConfiguration = Lens.lens (\MessageRequest' {templateConfiguration} -> templateConfiguration) (\s@MessageRequest' {} a -> s {templateConfiguration = a} :: MessageRequest)

-- | The unique identifier for tracing the message. This identifier is
-- visible to message recipients.
messageRequest_traceId :: Lens.Lens' MessageRequest (Prelude.Maybe Prelude.Text)
messageRequest_traceId = Lens.lens (\MessageRequest' {traceId} -> traceId) (\s@MessageRequest' {} a -> s {traceId = a} :: MessageRequest)

-- | The settings and content for the default message and any default
-- messages that you defined for specific channels.
messageRequest_messageConfiguration :: Lens.Lens' MessageRequest DirectMessageConfiguration
messageRequest_messageConfiguration = Lens.lens (\MessageRequest' {messageConfiguration} -> messageConfiguration) (\s@MessageRequest' {} a -> s {messageConfiguration = a} :: MessageRequest)

instance Prelude.Hashable MessageRequest where
  hashWithSalt _salt MessageRequest' {..} =
    _salt `Prelude.hashWithSalt` context
      `Prelude.hashWithSalt` endpoints
      `Prelude.hashWithSalt` addresses
      `Prelude.hashWithSalt` templateConfiguration
      `Prelude.hashWithSalt` traceId
      `Prelude.hashWithSalt` messageConfiguration

instance Prelude.NFData MessageRequest where
  rnf MessageRequest' {..} =
    Prelude.rnf context
      `Prelude.seq` Prelude.rnf endpoints
      `Prelude.seq` Prelude.rnf addresses
      `Prelude.seq` Prelude.rnf templateConfiguration
      `Prelude.seq` Prelude.rnf traceId
      `Prelude.seq` Prelude.rnf messageConfiguration

instance Core.ToJSON MessageRequest where
  toJSON MessageRequest' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Context" Core..=) Prelude.<$> context,
            ("Endpoints" Core..=) Prelude.<$> endpoints,
            ("Addresses" Core..=) Prelude.<$> addresses,
            ("TemplateConfiguration" Core..=)
              Prelude.<$> templateConfiguration,
            ("TraceId" Core..=) Prelude.<$> traceId,
            Prelude.Just
              ( "MessageConfiguration"
                  Core..= messageConfiguration
              )
          ]
      )
