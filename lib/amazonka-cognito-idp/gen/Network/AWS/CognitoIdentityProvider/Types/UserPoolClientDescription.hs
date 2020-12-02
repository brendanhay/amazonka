{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserPoolClientDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UserPoolClientDescription where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The description of the user pool client.
--
--
--
-- /See:/ 'userPoolClientDescription' smart constructor.
data UserPoolClientDescription = UserPoolClientDescription'
  { _upcdClientId ::
      !(Maybe (Sensitive Text)),
    _upcdUserPoolId :: !(Maybe Text),
    _upcdClientName :: !(Maybe Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'UserPoolClientDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upcdClientId' - The ID of the client associated with the user pool.
--
-- * 'upcdUserPoolId' - The user pool ID for the user pool where you want to describe the user pool client.
--
-- * 'upcdClientName' - The client name from the user pool client description.
userPoolClientDescription ::
  UserPoolClientDescription
userPoolClientDescription =
  UserPoolClientDescription'
    { _upcdClientId = Nothing,
      _upcdUserPoolId = Nothing,
      _upcdClientName = Nothing
    }

-- | The ID of the client associated with the user pool.
upcdClientId :: Lens' UserPoolClientDescription (Maybe Text)
upcdClientId = lens _upcdClientId (\s a -> s {_upcdClientId = a}) . mapping _Sensitive

-- | The user pool ID for the user pool where you want to describe the user pool client.
upcdUserPoolId :: Lens' UserPoolClientDescription (Maybe Text)
upcdUserPoolId = lens _upcdUserPoolId (\s a -> s {_upcdUserPoolId = a})

-- | The client name from the user pool client description.
upcdClientName :: Lens' UserPoolClientDescription (Maybe Text)
upcdClientName = lens _upcdClientName (\s a -> s {_upcdClientName = a})

instance FromJSON UserPoolClientDescription where
  parseJSON =
    withObject
      "UserPoolClientDescription"
      ( \x ->
          UserPoolClientDescription'
            <$> (x .:? "ClientId")
            <*> (x .:? "UserPoolId")
            <*> (x .:? "ClientName")
      )

instance Hashable UserPoolClientDescription

instance NFData UserPoolClientDescription
