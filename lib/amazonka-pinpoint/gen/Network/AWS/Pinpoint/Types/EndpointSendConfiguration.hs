{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EndpointSendConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EndpointSendConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the content, including message variables and attributes, to use in a message that's sent directly to an endpoint.
--
--
--
-- /See:/ 'endpointSendConfiguration' smart constructor.
data EndpointSendConfiguration = EndpointSendConfiguration'
  { _escSubstitutions ::
      !(Maybe (Map Text ([Text]))),
    _escTitleOverride :: !(Maybe Text),
    _escContext ::
      !(Maybe (Map Text (Text))),
    _escRawContent :: !(Maybe Text),
    _escBodyOverride :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EndpointSendConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'escSubstitutions' - A map of the message variables to merge with the variables specified for the default message (DefaultMessage.Substitutions). The variables specified in this map take precedence over all other variables.
--
-- * 'escTitleOverride' - The title or subject line of the message. If specified, this value overrides the default message title or subject line.
--
-- * 'escContext' - A map of custom attributes to attach to the message for the address. Attribute names are case sensitive. For a push notification, this payload is added to the data.pinpoint object. For an email or text message, this payload is added to email/SMS delivery receipt event attributes.
--
-- * 'escRawContent' - The raw, JSON-formatted string to use as the payload for the message. If specified, this value overrides all other values for the message.
--
-- * 'escBodyOverride' - The body of the message. If specified, this value overrides the default message body.
endpointSendConfiguration ::
  EndpointSendConfiguration
endpointSendConfiguration =
  EndpointSendConfiguration'
    { _escSubstitutions = Nothing,
      _escTitleOverride = Nothing,
      _escContext = Nothing,
      _escRawContent = Nothing,
      _escBodyOverride = Nothing
    }

-- | A map of the message variables to merge with the variables specified for the default message (DefaultMessage.Substitutions). The variables specified in this map take precedence over all other variables.
escSubstitutions :: Lens' EndpointSendConfiguration (HashMap Text ([Text]))
escSubstitutions = lens _escSubstitutions (\s a -> s {_escSubstitutions = a}) . _Default . _Map

-- | The title or subject line of the message. If specified, this value overrides the default message title or subject line.
escTitleOverride :: Lens' EndpointSendConfiguration (Maybe Text)
escTitleOverride = lens _escTitleOverride (\s a -> s {_escTitleOverride = a})

-- | A map of custom attributes to attach to the message for the address. Attribute names are case sensitive. For a push notification, this payload is added to the data.pinpoint object. For an email or text message, this payload is added to email/SMS delivery receipt event attributes.
escContext :: Lens' EndpointSendConfiguration (HashMap Text (Text))
escContext = lens _escContext (\s a -> s {_escContext = a}) . _Default . _Map

-- | The raw, JSON-formatted string to use as the payload for the message. If specified, this value overrides all other values for the message.
escRawContent :: Lens' EndpointSendConfiguration (Maybe Text)
escRawContent = lens _escRawContent (\s a -> s {_escRawContent = a})

-- | The body of the message. If specified, this value overrides the default message body.
escBodyOverride :: Lens' EndpointSendConfiguration (Maybe Text)
escBodyOverride = lens _escBodyOverride (\s a -> s {_escBodyOverride = a})

instance Hashable EndpointSendConfiguration

instance NFData EndpointSendConfiguration

instance ToJSON EndpointSendConfiguration where
  toJSON EndpointSendConfiguration' {..} =
    object
      ( catMaybes
          [ ("Substitutions" .=) <$> _escSubstitutions,
            ("TitleOverride" .=) <$> _escTitleOverride,
            ("Context" .=) <$> _escContext,
            ("RawContent" .=) <$> _escRawContent,
            ("BodyOverride" .=) <$> _escBodyOverride
          ]
      )
