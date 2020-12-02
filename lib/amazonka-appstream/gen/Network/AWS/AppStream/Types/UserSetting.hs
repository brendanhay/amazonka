{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.UserSetting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.UserSetting where

import Network.AWS.AppStream.Types.Action
import Network.AWS.AppStream.Types.Permission
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an action and whether the action is enabled or disabled for users during their streaming sessions.
--
--
--
-- /See:/ 'userSetting' smart constructor.
data UserSetting = UserSetting'
  { _usAction :: !Action,
    _usPermission :: !Permission
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UserSetting' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usAction' - The action that is enabled or disabled.
--
-- * 'usPermission' - Indicates whether the action is enabled or disabled.
userSetting ::
  -- | 'usAction'
  Action ->
  -- | 'usPermission'
  Permission ->
  UserSetting
userSetting pAction_ pPermission_ =
  UserSetting' {_usAction = pAction_, _usPermission = pPermission_}

-- | The action that is enabled or disabled.
usAction :: Lens' UserSetting Action
usAction = lens _usAction (\s a -> s {_usAction = a})

-- | Indicates whether the action is enabled or disabled.
usPermission :: Lens' UserSetting Permission
usPermission = lens _usPermission (\s a -> s {_usPermission = a})

instance FromJSON UserSetting where
  parseJSON =
    withObject
      "UserSetting"
      (\x -> UserSetting' <$> (x .: "Action") <*> (x .: "Permission"))

instance Hashable UserSetting

instance NFData UserSetting

instance ToJSON UserSetting where
  toJSON UserSetting' {..} =
    object
      ( catMaybes
          [ Just ("Action" .= _usAction),
            Just ("Permission" .= _usPermission)
          ]
      )
