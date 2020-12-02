{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.UserPendingChanges
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.UserPendingChanges where

import Network.AWS.Lens
import Network.AWS.MQ.Types.ChangeType
import Network.AWS.Prelude

-- | Returns information about the status of the changes pending for the ActiveMQ user.
--
-- /See:/ 'userPendingChanges' smart constructor.
data UserPendingChanges = UserPendingChanges'
  { _upcGroups ::
      !(Maybe [Text]),
    _upcConsoleAccess :: !(Maybe Bool),
    _upcPendingChange :: !(Maybe ChangeType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UserPendingChanges' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upcGroups' - The list of groups (20 maximum) to which the ActiveMQ user belongs. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
--
-- * 'upcConsoleAccess' - Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
--
-- * 'upcPendingChange' - Required. The type of change pending for the ActiveMQ user.
userPendingChanges ::
  UserPendingChanges
userPendingChanges =
  UserPendingChanges'
    { _upcGroups = Nothing,
      _upcConsoleAccess = Nothing,
      _upcPendingChange = Nothing
    }

-- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This value can contain only alphanumeric characters, dashes, periods, underscores, and tildes (- . _ ~). This value must be 2-100 characters long.
upcGroups :: Lens' UserPendingChanges [Text]
upcGroups = lens _upcGroups (\s a -> s {_upcGroups = a}) . _Default . _Coerce

-- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
upcConsoleAccess :: Lens' UserPendingChanges (Maybe Bool)
upcConsoleAccess = lens _upcConsoleAccess (\s a -> s {_upcConsoleAccess = a})

-- | Required. The type of change pending for the ActiveMQ user.
upcPendingChange :: Lens' UserPendingChanges (Maybe ChangeType)
upcPendingChange = lens _upcPendingChange (\s a -> s {_upcPendingChange = a})

instance FromJSON UserPendingChanges where
  parseJSON =
    withObject
      "UserPendingChanges"
      ( \x ->
          UserPendingChanges'
            <$> (x .:? "groups" .!= mempty)
            <*> (x .:? "consoleAccess")
            <*> (x .:? "pendingChange")
      )

instance Hashable UserPendingChanges

instance NFData UserPendingChanges
