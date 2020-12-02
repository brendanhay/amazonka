{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AccountRecoverySettingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AccountRecoverySettingType where

import Network.AWS.CognitoIdentityProvider.Types.RecoveryOptionType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The data type for @AccountRecoverySetting@ .
--
--
--
-- /See:/ 'accountRecoverySettingType' smart constructor.
newtype AccountRecoverySettingType = AccountRecoverySettingType'
  { _arstRecoveryMechanisms ::
      Maybe (List1 RecoveryOptionType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AccountRecoverySettingType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arstRecoveryMechanisms' - The list of @RecoveryOptionTypes@ .
accountRecoverySettingType ::
  AccountRecoverySettingType
accountRecoverySettingType =
  AccountRecoverySettingType' {_arstRecoveryMechanisms = Nothing}

-- | The list of @RecoveryOptionTypes@ .
arstRecoveryMechanisms :: Lens' AccountRecoverySettingType (Maybe (NonEmpty RecoveryOptionType))
arstRecoveryMechanisms = lens _arstRecoveryMechanisms (\s a -> s {_arstRecoveryMechanisms = a}) . mapping _List1

instance FromJSON AccountRecoverySettingType where
  parseJSON =
    withObject
      "AccountRecoverySettingType"
      ( \x ->
          AccountRecoverySettingType' <$> (x .:? "RecoveryMechanisms")
      )

instance Hashable AccountRecoverySettingType

instance NFData AccountRecoverySettingType

instance ToJSON AccountRecoverySettingType where
  toJSON AccountRecoverySettingType' {..} =
    object
      (catMaybes [("RecoveryMechanisms" .=) <$> _arstRecoveryMechanisms])
