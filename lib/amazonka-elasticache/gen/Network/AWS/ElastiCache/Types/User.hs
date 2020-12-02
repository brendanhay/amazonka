{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.User
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.User where

import Network.AWS.ElastiCache.Types.Authentication
import Network.AWS.Lens
import Network.AWS.Prelude

-- | /See:/ 'user' smart constructor.
data User = User'
  { _uStatus :: !(Maybe Text),
    _uARN :: !(Maybe Text),
    _uUserGroupIds :: !(Maybe [Text]),
    _uAuthentication :: !(Maybe Authentication),
    _uEngine :: !(Maybe Text),
    _uUserName :: !(Maybe Text),
    _uAccessString :: !(Maybe Text),
    _uUserId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'User' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uStatus' - Indicates the user status. Can be "active", "modifying" or "deleting".
--
-- * 'uARN' - The Amazon Resource Name (ARN) of the user account.
--
-- * 'uUserGroupIds' - Returns a list of the user group IDs the user belongs to.
--
-- * 'uAuthentication' - Denotes whether the user requires a password to authenticate.
--
-- * 'uEngine' - Must be Redis.
--
-- * 'uUserName' - The username of the user.
--
-- * 'uAccessString' - Access permissions string used for this user account.
--
-- * 'uUserId' - The ID of the user.
user ::
  User
user =
  User'
    { _uStatus = Nothing,
      _uARN = Nothing,
      _uUserGroupIds = Nothing,
      _uAuthentication = Nothing,
      _uEngine = Nothing,
      _uUserName = Nothing,
      _uAccessString = Nothing,
      _uUserId = Nothing
    }

-- | Indicates the user status. Can be "active", "modifying" or "deleting".
uStatus :: Lens' User (Maybe Text)
uStatus = lens _uStatus (\s a -> s {_uStatus = a})

-- | The Amazon Resource Name (ARN) of the user account.
uARN :: Lens' User (Maybe Text)
uARN = lens _uARN (\s a -> s {_uARN = a})

-- | Returns a list of the user group IDs the user belongs to.
uUserGroupIds :: Lens' User [Text]
uUserGroupIds = lens _uUserGroupIds (\s a -> s {_uUserGroupIds = a}) . _Default . _Coerce

-- | Denotes whether the user requires a password to authenticate.
uAuthentication :: Lens' User (Maybe Authentication)
uAuthentication = lens _uAuthentication (\s a -> s {_uAuthentication = a})

-- | Must be Redis.
uEngine :: Lens' User (Maybe Text)
uEngine = lens _uEngine (\s a -> s {_uEngine = a})

-- | The username of the user.
uUserName :: Lens' User (Maybe Text)
uUserName = lens _uUserName (\s a -> s {_uUserName = a})

-- | Access permissions string used for this user account.
uAccessString :: Lens' User (Maybe Text)
uAccessString = lens _uAccessString (\s a -> s {_uAccessString = a})

-- | The ID of the user.
uUserId :: Lens' User (Maybe Text)
uUserId = lens _uUserId (\s a -> s {_uUserId = a})

instance FromXML User where
  parseXML x =
    User'
      <$> (x .@? "Status")
      <*> (x .@? "ARN")
      <*> (x .@? "UserGroupIds" .!@ mempty >>= may (parseXMLList "member"))
      <*> (x .@? "Authentication")
      <*> (x .@? "Engine")
      <*> (x .@? "UserName")
      <*> (x .@? "AccessString")
      <*> (x .@? "UserId")

instance Hashable User

instance NFData User
