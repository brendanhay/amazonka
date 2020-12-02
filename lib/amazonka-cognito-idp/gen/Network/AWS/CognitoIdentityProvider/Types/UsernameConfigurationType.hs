{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UsernameConfigurationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UsernameConfigurationType where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The username configuration type.
--
--
--
-- /See:/ 'usernameConfigurationType' smart constructor.
newtype UsernameConfigurationType = UsernameConfigurationType'
  { _uctCaseSensitive ::
      Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UsernameConfigurationType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uctCaseSensitive' - Specifies whether username case sensitivity will be applied for all users in the user pool through Cognito APIs. Valid values include:     * __@True@ __ : Enables case sensitivity for all username input. When this option is set to @True@ , users must sign in using the exact capitalization of their given username. For example, “UserName”. This is the default value.     * __@False@ __ : Enables case insensitivity for all username input. For example, when this option is set to @False@ , users will be able to sign in using either "username" or "Username". This option also enables both @preferred_username@ and @email@ alias to be case insensitive, in addition to the @username@ attribute.
usernameConfigurationType ::
  -- | 'uctCaseSensitive'
  Bool ->
  UsernameConfigurationType
usernameConfigurationType pCaseSensitive_ =
  UsernameConfigurationType' {_uctCaseSensitive = pCaseSensitive_}

-- | Specifies whether username case sensitivity will be applied for all users in the user pool through Cognito APIs. Valid values include:     * __@True@ __ : Enables case sensitivity for all username input. When this option is set to @True@ , users must sign in using the exact capitalization of their given username. For example, “UserName”. This is the default value.     * __@False@ __ : Enables case insensitivity for all username input. For example, when this option is set to @False@ , users will be able to sign in using either "username" or "Username". This option also enables both @preferred_username@ and @email@ alias to be case insensitive, in addition to the @username@ attribute.
uctCaseSensitive :: Lens' UsernameConfigurationType Bool
uctCaseSensitive = lens _uctCaseSensitive (\s a -> s {_uctCaseSensitive = a})

instance FromJSON UsernameConfigurationType where
  parseJSON =
    withObject
      "UsernameConfigurationType"
      (\x -> UsernameConfigurationType' <$> (x .: "CaseSensitive"))

instance Hashable UsernameConfigurationType

instance NFData UsernameConfigurationType

instance ToJSON UsernameConfigurationType where
  toJSON UsernameConfigurationType' {..} =
    object (catMaybes [Just ("CaseSensitive" .= _uctCaseSensitive)])
