{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.TokenValidityUnitsType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.TokenValidityUnitsType where

import Network.AWS.CognitoIdentityProvider.Types.TimeUnitsType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The data type for TokenValidityUnits that specifics the time measurements for token validity.
--
--
--
-- /See:/ 'tokenValidityUnitsType' smart constructor.
data TokenValidityUnitsType = TokenValidityUnitsType'
  { _tvutAccessToken ::
      !(Maybe TimeUnitsType),
    _tvutRefreshToken :: !(Maybe TimeUnitsType),
    _tvutIdToken :: !(Maybe TimeUnitsType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TokenValidityUnitsType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tvutAccessToken' - A time unit in “seconds”, “minutes”, “hours” or “days” for the value in AccessTokenValidity, defaults to hours.
--
-- * 'tvutRefreshToken' - A time unit in “seconds”, “minutes”, “hours” or “days” for the value in RefreshTokenValidity, defaults to days.
--
-- * 'tvutIdToken' - A time unit in “seconds”, “minutes”, “hours” or “days” for the value in IdTokenValidity, defaults to hours.
tokenValidityUnitsType ::
  TokenValidityUnitsType
tokenValidityUnitsType =
  TokenValidityUnitsType'
    { _tvutAccessToken = Nothing,
      _tvutRefreshToken = Nothing,
      _tvutIdToken = Nothing
    }

-- | A time unit in “seconds”, “minutes”, “hours” or “days” for the value in AccessTokenValidity, defaults to hours.
tvutAccessToken :: Lens' TokenValidityUnitsType (Maybe TimeUnitsType)
tvutAccessToken = lens _tvutAccessToken (\s a -> s {_tvutAccessToken = a})

-- | A time unit in “seconds”, “minutes”, “hours” or “days” for the value in RefreshTokenValidity, defaults to days.
tvutRefreshToken :: Lens' TokenValidityUnitsType (Maybe TimeUnitsType)
tvutRefreshToken = lens _tvutRefreshToken (\s a -> s {_tvutRefreshToken = a})

-- | A time unit in “seconds”, “minutes”, “hours” or “days” for the value in IdTokenValidity, defaults to hours.
tvutIdToken :: Lens' TokenValidityUnitsType (Maybe TimeUnitsType)
tvutIdToken = lens _tvutIdToken (\s a -> s {_tvutIdToken = a})

instance FromJSON TokenValidityUnitsType where
  parseJSON =
    withObject
      "TokenValidityUnitsType"
      ( \x ->
          TokenValidityUnitsType'
            <$> (x .:? "AccessToken")
            <*> (x .:? "RefreshToken")
            <*> (x .:? "IdToken")
      )

instance Hashable TokenValidityUnitsType

instance NFData TokenValidityUnitsType

instance ToJSON TokenValidityUnitsType where
  toJSON TokenValidityUnitsType' {..} =
    object
      ( catMaybes
          [ ("AccessToken" .=) <$> _tvutAccessToken,
            ("RefreshToken" .=) <$> _tvutRefreshToken,
            ("IdToken" .=) <$> _tvutIdToken
          ]
      )
