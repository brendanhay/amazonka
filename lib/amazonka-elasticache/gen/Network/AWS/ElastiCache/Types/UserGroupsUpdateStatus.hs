{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.UserGroupsUpdateStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.UserGroupsUpdateStatus where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The status of the user group update.
--
--
--
-- /See:/ 'userGroupsUpdateStatus' smart constructor.
data UserGroupsUpdateStatus = UserGroupsUpdateStatus'
  { _ugusUserGroupIdsToAdd ::
      !(Maybe [Text]),
    _ugusUserGroupIdsToRemove :: !(Maybe [Text])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UserGroupsUpdateStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ugusUserGroupIdsToAdd' - The list of user group IDs to add.
--
-- * 'ugusUserGroupIdsToRemove' - The list of user group IDs to remove.
userGroupsUpdateStatus ::
  UserGroupsUpdateStatus
userGroupsUpdateStatus =
  UserGroupsUpdateStatus'
    { _ugusUserGroupIdsToAdd = Nothing,
      _ugusUserGroupIdsToRemove = Nothing
    }

-- | The list of user group IDs to add.
ugusUserGroupIdsToAdd :: Lens' UserGroupsUpdateStatus [Text]
ugusUserGroupIdsToAdd = lens _ugusUserGroupIdsToAdd (\s a -> s {_ugusUserGroupIdsToAdd = a}) . _Default . _Coerce

-- | The list of user group IDs to remove.
ugusUserGroupIdsToRemove :: Lens' UserGroupsUpdateStatus [Text]
ugusUserGroupIdsToRemove = lens _ugusUserGroupIdsToRemove (\s a -> s {_ugusUserGroupIdsToRemove = a}) . _Default . _Coerce

instance FromXML UserGroupsUpdateStatus where
  parseXML x =
    UserGroupsUpdateStatus'
      <$> ( x .@? "UserGroupIdsToAdd" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> ( x .@? "UserGroupIdsToRemove" .!@ mempty
              >>= may (parseXMLList "member")
          )

instance Hashable UserGroupsUpdateStatus

instance NFData UserGroupsUpdateStatus
