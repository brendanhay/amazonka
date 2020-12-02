{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.User
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.User where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A user associated with the broker.
--
-- /See:/ 'user' smart constructor.
data User = User'
  { _uGroups :: !(Maybe [Text]),
    _uConsoleAccess :: !(Maybe Bool),
    _uUsername :: !(Maybe Text),
    _uPassword :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'User' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uGroups' - The list of groups (20 maximum) to which the ActiveMQ user belongs. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- * 'uConsoleAccess' - Enables access to the ActiveMQ Web Console for the ActiveMQ user (Does not apply to RabbitMQ brokers).
--
-- * 'uUsername' - Required. The username of the broker user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- * 'uPassword' - Required. The password of the broker user. This value must be at least 12 characters long, must contain at least 4 unique characters, and must not contain commas.
user ::
  User
user =
  User'
    { _uGroups = Nothing,
      _uConsoleAccess = Nothing,
      _uUsername = Nothing,
      _uPassword = Nothing
    }

-- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
uGroups :: Lens' User [Text]
uGroups = lens _uGroups (\s a -> s {_uGroups = a}) . _Default . _Coerce

-- | Enables access to the ActiveMQ Web Console for the ActiveMQ user (Does not apply to RabbitMQ brokers).
uConsoleAccess :: Lens' User (Maybe Bool)
uConsoleAccess = lens _uConsoleAccess (\s a -> s {_uConsoleAccess = a})

-- | Required. The username of the broker user. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
uUsername :: Lens' User (Maybe Text)
uUsername = lens _uUsername (\s a -> s {_uUsername = a})

-- | Required. The password of the broker user. This value must be at least 12 characters long, must contain at least 4 unique characters, and must not contain commas.
uPassword :: Lens' User (Maybe Text)
uPassword = lens _uPassword (\s a -> s {_uPassword = a})

instance Hashable User

instance NFData User

instance ToJSON User where
  toJSON User' {..} =
    object
      ( catMaybes
          [ ("groups" .=) <$> _uGroups,
            ("consoleAccess" .=) <$> _uConsoleAccess,
            ("username" .=) <$> _uUsername,
            ("password" .=) <$> _uPassword
          ]
      )
