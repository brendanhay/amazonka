{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.CreateUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For Redis engine version 6.x onwards: Creates a Redis user. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.RBAC.html Using Role Based Access Control (RBAC)> .
module Network.AWS.ElastiCache.CreateUser
  ( -- * Creating a Request
    createUser,
    CreateUser,

    -- * Request Lenses
    cuPasswords,
    cuNoPasswordRequired,
    cuUserId,
    cuUserName,
    cuEngine,
    cuAccessString,

    -- * Destructuring the Response
    user,
    User,

    -- * Response Lenses
    uStatus,
    uARN,
    uUserGroupIds,
    uAuthentication,
    uEngine,
    uUserName,
    uAccessString,
    uUserId,
  )
where

import Network.AWS.ElastiCache.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createUser' smart constructor.
data CreateUser = CreateUser'
  { _cuPasswords ::
      !(Maybe (List1 Text)),
    _cuNoPasswordRequired :: !(Maybe Bool),
    _cuUserId :: !Text,
    _cuUserName :: !Text,
    _cuEngine :: !Text,
    _cuAccessString :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cuPasswords' - Passwords used for this user account. You can create up to two passwords for each user.
--
-- * 'cuNoPasswordRequired' - Indicates a password is not required for this user account.
--
-- * 'cuUserId' - The ID of the user.
--
-- * 'cuUserName' - The username of the user.
--
-- * 'cuEngine' - Must be Redis.
--
-- * 'cuAccessString' - Access permissions string used for this user account.
createUser ::
  -- | 'cuUserId'
  Text ->
  -- | 'cuUserName'
  Text ->
  -- | 'cuEngine'
  Text ->
  -- | 'cuAccessString'
  Text ->
  CreateUser
createUser pUserId_ pUserName_ pEngine_ pAccessString_ =
  CreateUser'
    { _cuPasswords = Nothing,
      _cuNoPasswordRequired = Nothing,
      _cuUserId = pUserId_,
      _cuUserName = pUserName_,
      _cuEngine = pEngine_,
      _cuAccessString = pAccessString_
    }

-- | Passwords used for this user account. You can create up to two passwords for each user.
cuPasswords :: Lens' CreateUser (Maybe (NonEmpty Text))
cuPasswords = lens _cuPasswords (\s a -> s {_cuPasswords = a}) . mapping _List1

-- | Indicates a password is not required for this user account.
cuNoPasswordRequired :: Lens' CreateUser (Maybe Bool)
cuNoPasswordRequired = lens _cuNoPasswordRequired (\s a -> s {_cuNoPasswordRequired = a})

-- | The ID of the user.
cuUserId :: Lens' CreateUser Text
cuUserId = lens _cuUserId (\s a -> s {_cuUserId = a})

-- | The username of the user.
cuUserName :: Lens' CreateUser Text
cuUserName = lens _cuUserName (\s a -> s {_cuUserName = a})

-- | Must be Redis.
cuEngine :: Lens' CreateUser Text
cuEngine = lens _cuEngine (\s a -> s {_cuEngine = a})

-- | Access permissions string used for this user account.
cuAccessString :: Lens' CreateUser Text
cuAccessString = lens _cuAccessString (\s a -> s {_cuAccessString = a})

instance AWSRequest CreateUser where
  type Rs CreateUser = User
  request = postQuery elastiCache
  response =
    receiveXMLWrapper "CreateUserResult" (\s h x -> parseXML x)

instance Hashable CreateUser

instance NFData CreateUser

instance ToHeaders CreateUser where
  toHeaders = const mempty

instance ToPath CreateUser where
  toPath = const "/"

instance ToQuery CreateUser where
  toQuery CreateUser' {..} =
    mconcat
      [ "Action" =: ("CreateUser" :: ByteString),
        "Version" =: ("2015-02-02" :: ByteString),
        "Passwords" =: toQuery (toQueryList "member" <$> _cuPasswords),
        "NoPasswordRequired" =: _cuNoPasswordRequired,
        "UserId" =: _cuUserId,
        "UserName" =: _cuUserName,
        "Engine" =: _cuEngine,
        "AccessString" =: _cuAccessString
      ]
