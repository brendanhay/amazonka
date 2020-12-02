{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.AddressConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.AddressConfiguration where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.ChannelType
import Network.AWS.Prelude

-- | Specifies address-based configuration settings for a message that's sent directly to an endpoint.
--
--
--
-- /See:/ 'addressConfiguration' smart constructor.
data AddressConfiguration = AddressConfiguration'
  { _acSubstitutions ::
      !(Maybe (Map Text ([Text]))),
    _acTitleOverride :: !(Maybe Text),
    _acContext :: !(Maybe (Map Text (Text))),
    _acRawContent :: !(Maybe Text),
    _acBodyOverride :: !(Maybe Text),
    _acChannelType :: !(Maybe ChannelType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AddressConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acSubstitutions' - A map of the message variables to merge with the variables specified by properties of the DefaultMessage object. The variables specified in this map take precedence over all other variables.
--
-- * 'acTitleOverride' - The message title to use instead of the default message title. This value overrides the default message title.
--
-- * 'acContext' - An object that maps custom attributes to attributes for the address and is attached to the message. Attribute names are case sensitive. For a push notification, this payload is added to the data.pinpoint object. For an email or text message, this payload is added to email/SMS delivery receipt event attributes.
--
-- * 'acRawContent' - The raw, JSON-formatted string to use as the payload for the message. If specified, this value overrides all other values for the message.
--
-- * 'acBodyOverride' - The message body to use instead of the default message body. This value overrides the default message body.
--
-- * 'acChannelType' - The channel to use when sending the message.
addressConfiguration ::
  AddressConfiguration
addressConfiguration =
  AddressConfiguration'
    { _acSubstitutions = Nothing,
      _acTitleOverride = Nothing,
      _acContext = Nothing,
      _acRawContent = Nothing,
      _acBodyOverride = Nothing,
      _acChannelType = Nothing
    }

-- | A map of the message variables to merge with the variables specified by properties of the DefaultMessage object. The variables specified in this map take precedence over all other variables.
acSubstitutions :: Lens' AddressConfiguration (HashMap Text ([Text]))
acSubstitutions = lens _acSubstitutions (\s a -> s {_acSubstitutions = a}) . _Default . _Map

-- | The message title to use instead of the default message title. This value overrides the default message title.
acTitleOverride :: Lens' AddressConfiguration (Maybe Text)
acTitleOverride = lens _acTitleOverride (\s a -> s {_acTitleOverride = a})

-- | An object that maps custom attributes to attributes for the address and is attached to the message. Attribute names are case sensitive. For a push notification, this payload is added to the data.pinpoint object. For an email or text message, this payload is added to email/SMS delivery receipt event attributes.
acContext :: Lens' AddressConfiguration (HashMap Text (Text))
acContext = lens _acContext (\s a -> s {_acContext = a}) . _Default . _Map

-- | The raw, JSON-formatted string to use as the payload for the message. If specified, this value overrides all other values for the message.
acRawContent :: Lens' AddressConfiguration (Maybe Text)
acRawContent = lens _acRawContent (\s a -> s {_acRawContent = a})

-- | The message body to use instead of the default message body. This value overrides the default message body.
acBodyOverride :: Lens' AddressConfiguration (Maybe Text)
acBodyOverride = lens _acBodyOverride (\s a -> s {_acBodyOverride = a})

-- | The channel to use when sending the message.
acChannelType :: Lens' AddressConfiguration (Maybe ChannelType)
acChannelType = lens _acChannelType (\s a -> s {_acChannelType = a})

instance Hashable AddressConfiguration

instance NFData AddressConfiguration

instance ToJSON AddressConfiguration where
  toJSON AddressConfiguration' {..} =
    object
      ( catMaybes
          [ ("Substitutions" .=) <$> _acSubstitutions,
            ("TitleOverride" .=) <$> _acTitleOverride,
            ("Context" .=) <$> _acContext,
            ("RawContent" .=) <$> _acRawContent,
            ("BodyOverride" .=) <$> _acBodyOverride,
            ("ChannelType" .=) <$> _acChannelType
          ]
      )
