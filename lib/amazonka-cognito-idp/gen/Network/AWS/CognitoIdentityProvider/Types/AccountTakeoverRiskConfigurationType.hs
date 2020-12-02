{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverRiskConfigurationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverRiskConfigurationType where

import Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverActionsType
import Network.AWS.CognitoIdentityProvider.Types.NotifyConfigurationType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration for mitigation actions and notification for different levels of risk detected for a potential account takeover.
--
--
--
-- /See:/ 'accountTakeoverRiskConfigurationType' smart constructor.
data AccountTakeoverRiskConfigurationType = AccountTakeoverRiskConfigurationType'
  { _atrctNotifyConfiguration ::
      !( Maybe
           NotifyConfigurationType
       ),
    _atrctActions ::
      !AccountTakeoverActionsType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AccountTakeoverRiskConfigurationType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atrctNotifyConfiguration' - The notify configuration used to construct email notifications.
--
-- * 'atrctActions' - Account takeover risk configuration actions
accountTakeoverRiskConfigurationType ::
  -- | 'atrctActions'
  AccountTakeoverActionsType ->
  AccountTakeoverRiskConfigurationType
accountTakeoverRiskConfigurationType pActions_ =
  AccountTakeoverRiskConfigurationType'
    { _atrctNotifyConfiguration =
        Nothing,
      _atrctActions = pActions_
    }

-- | The notify configuration used to construct email notifications.
atrctNotifyConfiguration :: Lens' AccountTakeoverRiskConfigurationType (Maybe NotifyConfigurationType)
atrctNotifyConfiguration = lens _atrctNotifyConfiguration (\s a -> s {_atrctNotifyConfiguration = a})

-- | Account takeover risk configuration actions
atrctActions :: Lens' AccountTakeoverRiskConfigurationType AccountTakeoverActionsType
atrctActions = lens _atrctActions (\s a -> s {_atrctActions = a})

instance FromJSON AccountTakeoverRiskConfigurationType where
  parseJSON =
    withObject
      "AccountTakeoverRiskConfigurationType"
      ( \x ->
          AccountTakeoverRiskConfigurationType'
            <$> (x .:? "NotifyConfiguration") <*> (x .: "Actions")
      )

instance Hashable AccountTakeoverRiskConfigurationType

instance NFData AccountTakeoverRiskConfigurationType

instance ToJSON AccountTakeoverRiskConfigurationType where
  toJSON AccountTakeoverRiskConfigurationType' {..} =
    object
      ( catMaybes
          [ ("NotifyConfiguration" .=) <$> _atrctNotifyConfiguration,
            Just ("Actions" .= _atrctActions)
          ]
      )
