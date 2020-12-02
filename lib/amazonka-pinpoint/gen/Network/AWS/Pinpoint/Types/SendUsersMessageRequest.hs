{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SendUsersMessageRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SendUsersMessageRequest where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.DirectMessageConfiguration
import Network.AWS.Pinpoint.Types.EndpointSendConfiguration
import Network.AWS.Pinpoint.Types.TemplateConfiguration
import Network.AWS.Prelude

-- | Specifies the configuration and other settings for a message to send to all the endpoints that are associated with a list of users.
--
--
--
-- /See:/ 'sendUsersMessageRequest' smart constructor.
data SendUsersMessageRequest = SendUsersMessageRequest'
  { _sumrTraceId ::
      !(Maybe Text),
    _sumrContext :: !(Maybe (Map Text (Text))),
    _sumrTemplateConfiguration ::
      !(Maybe TemplateConfiguration),
    _sumrMessageConfiguration ::
      !DirectMessageConfiguration,
    _sumrUsers ::
      !(Map Text (EndpointSendConfiguration))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SendUsersMessageRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sumrTraceId' - The unique identifier for tracing the message. This identifier is visible to message recipients.
--
-- * 'sumrContext' - A map of custom attribute-value pairs. For a push notification, Amazon Pinpoint adds these attributes to the data.pinpoint object in the body of the notification payload. Amazon Pinpoint also provides these attributes in the events that it generates for users-messages deliveries.
--
-- * 'sumrTemplateConfiguration' - The message template to use for the message.
--
-- * 'sumrMessageConfiguration' - The settings and content for the default message and any default messages that you defined for specific channels.
--
-- * 'sumrUsers' - A map that associates user IDs with <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration> objects. You can use an <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration> object to tailor the message for a user by specifying settings such as content overrides and message variables.
sendUsersMessageRequest ::
  -- | 'sumrMessageConfiguration'
  DirectMessageConfiguration ->
  SendUsersMessageRequest
sendUsersMessageRequest pMessageConfiguration_ =
  SendUsersMessageRequest'
    { _sumrTraceId = Nothing,
      _sumrContext = Nothing,
      _sumrTemplateConfiguration = Nothing,
      _sumrMessageConfiguration = pMessageConfiguration_,
      _sumrUsers = mempty
    }

-- | The unique identifier for tracing the message. This identifier is visible to message recipients.
sumrTraceId :: Lens' SendUsersMessageRequest (Maybe Text)
sumrTraceId = lens _sumrTraceId (\s a -> s {_sumrTraceId = a})

-- | A map of custom attribute-value pairs. For a push notification, Amazon Pinpoint adds these attributes to the data.pinpoint object in the body of the notification payload. Amazon Pinpoint also provides these attributes in the events that it generates for users-messages deliveries.
sumrContext :: Lens' SendUsersMessageRequest (HashMap Text (Text))
sumrContext = lens _sumrContext (\s a -> s {_sumrContext = a}) . _Default . _Map

-- | The message template to use for the message.
sumrTemplateConfiguration :: Lens' SendUsersMessageRequest (Maybe TemplateConfiguration)
sumrTemplateConfiguration = lens _sumrTemplateConfiguration (\s a -> s {_sumrTemplateConfiguration = a})

-- | The settings and content for the default message and any default messages that you defined for specific channels.
sumrMessageConfiguration :: Lens' SendUsersMessageRequest DirectMessageConfiguration
sumrMessageConfiguration = lens _sumrMessageConfiguration (\s a -> s {_sumrMessageConfiguration = a})

-- | A map that associates user IDs with <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration> objects. You can use an <https://docs.aws.amazon.com/pinpoint/latest/apireference/apps-application-id-messages.html#apps-application-id-messages-model-endpointsendconfiguration EndpointSendConfiguration> object to tailor the message for a user by specifying settings such as content overrides and message variables.
sumrUsers :: Lens' SendUsersMessageRequest (HashMap Text (EndpointSendConfiguration))
sumrUsers = lens _sumrUsers (\s a -> s {_sumrUsers = a}) . _Map

instance Hashable SendUsersMessageRequest

instance NFData SendUsersMessageRequest

instance ToJSON SendUsersMessageRequest where
  toJSON SendUsersMessageRequest' {..} =
    object
      ( catMaybes
          [ ("TraceId" .=) <$> _sumrTraceId,
            ("Context" .=) <$> _sumrContext,
            ("TemplateConfiguration" .=) <$> _sumrTemplateConfiguration,
            Just ("MessageConfiguration" .= _sumrMessageConfiguration),
            Just ("Users" .= _sumrUsers)
          ]
      )
