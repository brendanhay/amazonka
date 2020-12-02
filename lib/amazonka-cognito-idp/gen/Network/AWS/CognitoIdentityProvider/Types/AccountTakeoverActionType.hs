{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverActionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverActionType where

import Network.AWS.CognitoIdentityProvider.Types.AccountTakeoverEventActionType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Account takeover action type.
--
--
--
-- /See:/ 'accountTakeoverActionType' smart constructor.
data AccountTakeoverActionType = AccountTakeoverActionType'
  { _atatNotify ::
      !Bool,
    _atatEventAction ::
      !AccountTakeoverEventActionType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AccountTakeoverActionType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atatNotify' - Flag specifying whether to send a notification.
--
-- * 'atatEventAction' - The event action.     * @BLOCK@ Choosing this action will block the request.     * @MFA_IF_CONFIGURED@ Throw MFA challenge if user has configured it, else allow the request.     * @MFA_REQUIRED@ Throw MFA challenge if user has configured it, else block the request.     * @NO_ACTION@ Allow the user sign-in.
accountTakeoverActionType ::
  -- | 'atatNotify'
  Bool ->
  -- | 'atatEventAction'
  AccountTakeoverEventActionType ->
  AccountTakeoverActionType
accountTakeoverActionType pNotify_ pEventAction_ =
  AccountTakeoverActionType'
    { _atatNotify = pNotify_,
      _atatEventAction = pEventAction_
    }

-- | Flag specifying whether to send a notification.
atatNotify :: Lens' AccountTakeoverActionType Bool
atatNotify = lens _atatNotify (\s a -> s {_atatNotify = a})

-- | The event action.     * @BLOCK@ Choosing this action will block the request.     * @MFA_IF_CONFIGURED@ Throw MFA challenge if user has configured it, else allow the request.     * @MFA_REQUIRED@ Throw MFA challenge if user has configured it, else block the request.     * @NO_ACTION@ Allow the user sign-in.
atatEventAction :: Lens' AccountTakeoverActionType AccountTakeoverEventActionType
atatEventAction = lens _atatEventAction (\s a -> s {_atatEventAction = a})

instance FromJSON AccountTakeoverActionType where
  parseJSON =
    withObject
      "AccountTakeoverActionType"
      ( \x ->
          AccountTakeoverActionType'
            <$> (x .: "Notify") <*> (x .: "EventAction")
      )

instance Hashable AccountTakeoverActionType

instance NFData AccountTakeoverActionType

instance ToJSON AccountTakeoverActionType where
  toJSON AccountTakeoverActionType' {..} =
    object
      ( catMaybes
          [ Just ("Notify" .= _atatNotify),
            Just ("EventAction" .= _atatEventAction)
          ]
      )
