{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.UserGroupPendingChanges
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.UserGroupPendingChanges where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returns the updates being applied to the user group.
--
--
--
-- /See:/ 'userGroupPendingChanges' smart constructor.
data UserGroupPendingChanges = UserGroupPendingChanges'
  { _ugpcUserIdsToAdd ::
      !(Maybe [Text]),
    _ugpcUserIdsToRemove :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UserGroupPendingChanges' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugpcUserIdsToAdd' - The list of user IDs to add.
--
-- * 'ugpcUserIdsToRemove' - The list of user group IDs ro remove.
userGroupPendingChanges ::
  UserGroupPendingChanges
userGroupPendingChanges =
  UserGroupPendingChanges'
    { _ugpcUserIdsToAdd = Nothing,
      _ugpcUserIdsToRemove = Nothing
    }

-- | The list of user IDs to add.
ugpcUserIdsToAdd :: Lens' UserGroupPendingChanges [Text]
ugpcUserIdsToAdd = lens _ugpcUserIdsToAdd (\s a -> s {_ugpcUserIdsToAdd = a}) . _Default . _Coerce

-- | The list of user group IDs ro remove.
ugpcUserIdsToRemove :: Lens' UserGroupPendingChanges [Text]
ugpcUserIdsToRemove = lens _ugpcUserIdsToRemove (\s a -> s {_ugpcUserIdsToRemove = a}) . _Default . _Coerce

instance FromXML UserGroupPendingChanges where
  parseXML x =
    UserGroupPendingChanges'
      <$> (x .@? "UserIdsToAdd" .!@ mempty >>= may (parseXMLList "member"))
      <*> ( x .@? "UserIdsToRemove" .!@ mempty
              >>= may (parseXMLList "member")
          )

instance Hashable UserGroupPendingChanges

instance NFData UserGroupPendingChanges
