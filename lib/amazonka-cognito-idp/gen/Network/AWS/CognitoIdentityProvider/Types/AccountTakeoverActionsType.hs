{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverActionsType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverActionsType where

import Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverActionType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Account takeover actions type.
--
--
--
-- /See:/ 'accountTakeoverActionsType' smart constructor.
data AccountTakeoverActionsType = AccountTakeoverActionsType'
  { _atatLowAction ::
      !(Maybe AccountTakeoverActionType),
    _atatHighAction ::
      !(Maybe AccountTakeoverActionType),
    _atatMediumAction ::
      !(Maybe AccountTakeoverActionType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AccountTakeoverActionsType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atatLowAction' - Action to take for a low risk.
--
-- * 'atatHighAction' - Action to take for a high risk.
--
-- * 'atatMediumAction' - Action to take for a medium risk.
accountTakeoverActionsType ::
  AccountTakeoverActionsType
accountTakeoverActionsType =
  AccountTakeoverActionsType'
    { _atatLowAction = Nothing,
      _atatHighAction = Nothing,
      _atatMediumAction = Nothing
    }

-- | Action to take for a low risk.
atatLowAction :: Lens' AccountTakeoverActionsType (Maybe AccountTakeoverActionType)
atatLowAction = lens _atatLowAction (\s a -> s {_atatLowAction = a})

-- | Action to take for a high risk.
atatHighAction :: Lens' AccountTakeoverActionsType (Maybe AccountTakeoverActionType)
atatHighAction = lens _atatHighAction (\s a -> s {_atatHighAction = a})

-- | Action to take for a medium risk.
atatMediumAction :: Lens' AccountTakeoverActionsType (Maybe AccountTakeoverActionType)
atatMediumAction = lens _atatMediumAction (\s a -> s {_atatMediumAction = a})

instance FromJSON AccountTakeoverActionsType where
  parseJSON =
    withObject
      "AccountTakeoverActionsType"
      ( \x ->
          AccountTakeoverActionsType'
            <$> (x .:? "LowAction")
            <*> (x .:? "HighAction")
            <*> (x .:? "MediumAction")
      )

instance Hashable AccountTakeoverActionsType

instance NFData AccountTakeoverActionsType

instance ToJSON AccountTakeoverActionsType where
  toJSON AccountTakeoverActionsType' {..} =
    object
      ( catMaybes
          [ ("LowAction" .=) <$> _atatLowAction,
            ("HighAction" .=) <$> _atatHighAction,
            ("MediumAction" .=) <$> _atatMediumAction
          ]
      )
