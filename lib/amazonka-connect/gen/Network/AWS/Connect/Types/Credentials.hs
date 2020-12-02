{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.Credentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.Credentials where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains credentials to use for federation.
--
--
--
-- /See:/ 'credentials' smart constructor.
data Credentials = Credentials'
  { _cAccessTokenExpiration ::
      !(Maybe POSIX),
    _cAccessToken :: !(Maybe (Sensitive Text)),
    _cRefreshToken :: !(Maybe (Sensitive Text)),
    _cRefreshTokenExpiration :: !(Maybe POSIX)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'Credentials' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cAccessTokenExpiration' - A token generated with an expiration time for the session a user is logged in to Amazon Connect.
--
-- * 'cAccessToken' - An access token generated for a federated user to access Amazon Connect.
--
-- * 'cRefreshToken' - Renews a token generated for a user to access the Amazon Connect instance.
--
-- * 'cRefreshTokenExpiration' - Renews the expiration timer for a generated token.
credentials ::
  Credentials
credentials =
  Credentials'
    { _cAccessTokenExpiration = Nothing,
      _cAccessToken = Nothing,
      _cRefreshToken = Nothing,
      _cRefreshTokenExpiration = Nothing
    }

-- | A token generated with an expiration time for the session a user is logged in to Amazon Connect.
cAccessTokenExpiration :: Lens' Credentials (Maybe UTCTime)
cAccessTokenExpiration = lens _cAccessTokenExpiration (\s a -> s {_cAccessTokenExpiration = a}) . mapping _Time

-- | An access token generated for a federated user to access Amazon Connect.
cAccessToken :: Lens' Credentials (Maybe Text)
cAccessToken = lens _cAccessToken (\s a -> s {_cAccessToken = a}) . mapping _Sensitive

-- | Renews a token generated for a user to access the Amazon Connect instance.
cRefreshToken :: Lens' Credentials (Maybe Text)
cRefreshToken = lens _cRefreshToken (\s a -> s {_cRefreshToken = a}) . mapping _Sensitive

-- | Renews the expiration timer for a generated token.
cRefreshTokenExpiration :: Lens' Credentials (Maybe UTCTime)
cRefreshTokenExpiration = lens _cRefreshTokenExpiration (\s a -> s {_cRefreshTokenExpiration = a}) . mapping _Time

instance FromJSON Credentials where
  parseJSON =
    withObject
      "Credentials"
      ( \x ->
          Credentials'
            <$> (x .:? "AccessTokenExpiration")
            <*> (x .:? "AccessToken")
            <*> (x .:? "RefreshToken")
            <*> (x .:? "RefreshTokenExpiration")
      )

instance Hashable Credentials

instance NFData Credentials
