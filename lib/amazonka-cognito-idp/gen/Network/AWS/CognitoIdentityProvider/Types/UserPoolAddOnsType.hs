{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserPoolAddOnsType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UserPoolAddOnsType where

import Network.AWS.CognitoIdentityProvider.Types.AdvancedSecurityModeType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The user pool add-ons type.
--
--
--
-- /See:/ 'userPoolAddOnsType' smart constructor.
newtype UserPoolAddOnsType = UserPoolAddOnsType'
  { _upaotAdvancedSecurityMode ::
      AdvancedSecurityModeType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UserPoolAddOnsType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upaotAdvancedSecurityMode' - The advanced security mode.
userPoolAddOnsType ::
  -- | 'upaotAdvancedSecurityMode'
  AdvancedSecurityModeType ->
  UserPoolAddOnsType
userPoolAddOnsType pAdvancedSecurityMode_ =
  UserPoolAddOnsType'
    { _upaotAdvancedSecurityMode =
        pAdvancedSecurityMode_
    }

-- | The advanced security mode.
upaotAdvancedSecurityMode :: Lens' UserPoolAddOnsType AdvancedSecurityModeType
upaotAdvancedSecurityMode = lens _upaotAdvancedSecurityMode (\s a -> s {_upaotAdvancedSecurityMode = a})

instance FromJSON UserPoolAddOnsType where
  parseJSON =
    withObject
      "UserPoolAddOnsType"
      (\x -> UserPoolAddOnsType' <$> (x .: "AdvancedSecurityMode"))

instance Hashable UserPoolAddOnsType

instance NFData UserPoolAddOnsType

instance ToJSON UserPoolAddOnsType where
  toJSON UserPoolAddOnsType' {..} =
    object
      ( catMaybes
          [Just ("AdvancedSecurityMode" .= _upaotAdvancedSecurityMode)]
      )
