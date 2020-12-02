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
-- Module      : Network.AWS.ElastiCache.ModifyUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes user password(s) and/or access string.
module Network.AWS.ElastiCache.ModifyUser
  ( -- * Creating a Request
    modifyUser,
    ModifyUser,

    -- * Request Lenses
    muAppendAccessString,
    muAccessString,
    muPasswords,
    muNoPasswordRequired,
    muUserId,

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

-- | /See:/ 'modifyUser' smart constructor.
data ModifyUser = ModifyUser'
  { _muAppendAccessString ::
      !(Maybe Text),
    _muAccessString :: !(Maybe Text),
    _muPasswords :: !(Maybe (List1 Text)),
    _muNoPasswordRequired :: !(Maybe Bool),
    _muUserId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'muAppendAccessString' - Adds additional user permissions to the access string.
--
-- * 'muAccessString' - Access permissions string used for this user account.
--
-- * 'muPasswords' - The passwords belonging to the user account. You are allowed up to two.
--
-- * 'muNoPasswordRequired' - Indicates no password is required for the user account.
--
-- * 'muUserId' - The ID of the user.
modifyUser ::
  -- | 'muUserId'
  Text ->
  ModifyUser
modifyUser pUserId_ =
  ModifyUser'
    { _muAppendAccessString = Nothing,
      _muAccessString = Nothing,
      _muPasswords = Nothing,
      _muNoPasswordRequired = Nothing,
      _muUserId = pUserId_
    }

-- | Adds additional user permissions to the access string.
muAppendAccessString :: Lens' ModifyUser (Maybe Text)
muAppendAccessString = lens _muAppendAccessString (\s a -> s {_muAppendAccessString = a})

-- | Access permissions string used for this user account.
muAccessString :: Lens' ModifyUser (Maybe Text)
muAccessString = lens _muAccessString (\s a -> s {_muAccessString = a})

-- | The passwords belonging to the user account. You are allowed up to two.
muPasswords :: Lens' ModifyUser (Maybe (NonEmpty Text))
muPasswords = lens _muPasswords (\s a -> s {_muPasswords = a}) . mapping _List1

-- | Indicates no password is required for the user account.
muNoPasswordRequired :: Lens' ModifyUser (Maybe Bool)
muNoPasswordRequired = lens _muNoPasswordRequired (\s a -> s {_muNoPasswordRequired = a})

-- | The ID of the user.
muUserId :: Lens' ModifyUser Text
muUserId = lens _muUserId (\s a -> s {_muUserId = a})

instance AWSRequest ModifyUser where
  type Rs ModifyUser = User
  request = postQuery elastiCache
  response =
    receiveXMLWrapper "ModifyUserResult" (\s h x -> parseXML x)

instance Hashable ModifyUser

instance NFData ModifyUser

instance ToHeaders ModifyUser where
  toHeaders = const mempty

instance ToPath ModifyUser where
  toPath = const "/"

instance ToQuery ModifyUser where
  toQuery ModifyUser' {..} =
    mconcat
      [ "Action" =: ("ModifyUser" :: ByteString),
        "Version" =: ("2015-02-02" :: ByteString),
        "AppendAccessString" =: _muAppendAccessString,
        "AccessString" =: _muAccessString,
        "Passwords" =: toQuery (toQueryList "member" <$> _muPasswords),
        "NoPasswordRequired" =: _muNoPasswordRequired,
        "UserId" =: _muUserId
      ]
