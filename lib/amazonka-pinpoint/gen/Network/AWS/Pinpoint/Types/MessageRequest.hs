{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.MessageRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.MessageRequest where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.AddressConfiguration
import Network.AWS.Pinpoint.Types.DirectMessageConfiguration
import Network.AWS.Pinpoint.Types.EndpointSendConfiguration
import Network.AWS.Pinpoint.Types.TemplateConfiguration
import Network.AWS.Prelude

-- | Specifies the configuration and other settings for a message.
--
--
--
-- /See:/ 'messageRequest' smart constructor.
data MessageRequest = MessageRequest'
  { _mrTraceId :: !(Maybe Text),
    _mrContext :: !(Maybe (Map Text (Text))),
    _mrAddresses :: !(Maybe (Map Text (AddressConfiguration))),
    _mrTemplateConfiguration :: !(Maybe TemplateConfiguration),
    _mrEndpoints ::
      !(Maybe (Map Text (EndpointSendConfiguration))),
    _mrMessageConfiguration :: !DirectMessageConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MessageRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrTraceId' - The unique identifier for tracing the message. This identifier is visible to message recipients.
--
-- * 'mrContext' - A map of custom attributes to attach to the message. For a push notification, this payload is added to the data.pinpoint object. For an email or text message, this payload is added to email/SMS delivery receipt event attributes.
--
-- * 'mrAddresses' - A map of key-value pairs, where each key is an address and each value is an <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-addressconfiguration AddressConfiguration> object. An address can be a push notification token, a phone number, or an email address. You can use an <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-addressconfiguration AddressConfiguration> object to tailor the message for an address by specifying settings such as content overrides and message variables.
--
-- * 'mrTemplateConfiguration' - The message template to use for the message.
--
-- * 'mrEndpoints' - A map of key-value pairs, where each key is an endpoint ID and each value is an <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration> object. You can use an <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration> object to tailor the message for an endpoint by specifying settings such as content overrides and message variables.
--
-- * 'mrMessageConfiguration' - The settings and content for the default message and any default messages that you defined for specific channels.
messageRequest ::
  -- | 'mrMessageConfiguration'
  DirectMessageConfiguration ->
  MessageRequest
messageRequest pMessageConfiguration_ =
  MessageRequest'
    { _mrTraceId = Nothing,
      _mrContext = Nothing,
      _mrAddresses = Nothing,
      _mrTemplateConfiguration = Nothing,
      _mrEndpoints = Nothing,
      _mrMessageConfiguration = pMessageConfiguration_
    }

-- | The unique identifier for tracing the message. This identifier is visible to message recipients.
mrTraceId :: Lens' MessageRequest (Maybe Text)
mrTraceId = lens _mrTraceId (\s a -> s {_mrTraceId = a})

-- | A map of custom attributes to attach to the message. For a push notification, this payload is added to the data.pinpoint object. For an email or text message, this payload is added to email/SMS delivery receipt event attributes.
mrContext :: Lens' MessageRequest (HashMap Text (Text))
mrContext = lens _mrContext (\s a -> s {_mrContext = a}) . _Default . _Map

-- | A map of key-value pairs, where each key is an address and each value is an <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-addressconfiguration AddressConfiguration> object. An address can be a push notification token, a phone number, or an email address. You can use an <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-addressconfiguration AddressConfiguration> object to tailor the message for an address by specifying settings such as content overrides and message variables.
mrAddresses :: Lens' MessageRequest (HashMap Text (AddressConfiguration))
mrAddresses = lens _mrAddresses (\s a -> s {_mrAddresses = a}) . _Default . _Map

-- | The message template to use for the message.
mrTemplateConfiguration :: Lens' MessageRequest (Maybe TemplateConfiguration)
mrTemplateConfiguration = lens _mrTemplateConfiguration (\s a -> s {_mrTemplateConfiguration = a})

-- | A map of key-value pairs, where each key is an endpoint ID and each value is an <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration> object. You can use an <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration> object to tailor the message for an endpoint by specifying settings such as content overrides and message variables.
mrEndpoints :: Lens' MessageRequest (HashMap Text (EndpointSendConfiguration))
mrEndpoints = lens _mrEndpoints (\s a -> s {_mrEndpoints = a}) . _Default . _Map

-- | The settings and content for the default message and any default messages that you defined for specific channels.
mrMessageConfiguration :: Lens' MessageRequest DirectMessageConfiguration
mrMessageConfiguration = lens _mrMessageConfiguration (\s a -> s {_mrMessageConfiguration = a})

instance Hashable MessageRequest

instance NFData MessageRequest

instance ToJSON MessageRequest where
  toJSON MessageRequest' {..} =
    object
      ( catMaybes
          [ ("TraceId" .=) <$> _mrTraceId,
            ("Context" .=) <$> _mrContext,
            ("Addresses" .=) <$> _mrAddresses,
            ("TemplateConfiguration" .=) <$> _mrTemplateConfiguration,
            ("Endpoints" .=) <$> _mrEndpoints,
            Just ("MessageConfiguration" .= _mrMessageConfiguration)
          ]
      )
