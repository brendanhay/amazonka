{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserPoolPolicyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UserPoolPolicyType where

import Network.AWS.CognitoIdentityProvider.Types.PasswordPolicyType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The policy associated with a user pool.
--
--
--
-- /See:/ 'userPoolPolicyType' smart constructor.
newtype UserPoolPolicyType = UserPoolPolicyType'
  { _upptPasswordPolicy ::
      Maybe PasswordPolicyType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UserPoolPolicyType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upptPasswordPolicy' - The password policy.
userPoolPolicyType ::
  UserPoolPolicyType
userPoolPolicyType =
  UserPoolPolicyType' {_upptPasswordPolicy = Nothing}

-- | The password policy.
upptPasswordPolicy :: Lens' UserPoolPolicyType (Maybe PasswordPolicyType)
upptPasswordPolicy = lens _upptPasswordPolicy (\s a -> s {_upptPasswordPolicy = a})

instance FromJSON UserPoolPolicyType where
  parseJSON =
    withObject
      "UserPoolPolicyType"
      (\x -> UserPoolPolicyType' <$> (x .:? "PasswordPolicy"))

instance Hashable UserPoolPolicyType

instance NFData UserPoolPolicyType

instance ToJSON UserPoolPolicyType where
  toJSON UserPoolPolicyType' {..} =
    object
      (catMaybes [("PasswordPolicy" .=) <$> _upptPasswordPolicy])
