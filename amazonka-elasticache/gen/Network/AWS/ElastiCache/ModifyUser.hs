{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.ModifyUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes user password(s) and\/or access string.
module Network.AWS.ElastiCache.ModifyUser
  ( -- * Creating a Request
    ModifyUser (..),
    newModifyUser,

    -- * Request Lenses
    modifyUser_appendAccessString,
    modifyUser_passwords,
    modifyUser_accessString,
    modifyUser_noPasswordRequired,
    modifyUser_userId,

    -- * Destructuring the Response
    User (..),
    newUser,

    -- * Response Lenses
    user_status,
    user_accessString,
    user_userGroupIds,
    user_authentication,
    user_arn,
    user_userId,
    user_engine,
    user_userName,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyUser' smart constructor.
data ModifyUser = ModifyUser'
  { -- | Adds additional user permissions to the access string.
    appendAccessString :: Prelude.Maybe Prelude.Text,
    -- | The passwords belonging to the user. You are allowed up to two.
    passwords :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Access permissions string used for this user.
    accessString :: Prelude.Maybe Prelude.Text,
    -- | Indicates no password is required for the user.
    noPasswordRequired :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the user.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appendAccessString', 'modifyUser_appendAccessString' - Adds additional user permissions to the access string.
--
-- 'passwords', 'modifyUser_passwords' - The passwords belonging to the user. You are allowed up to two.
--
-- 'accessString', 'modifyUser_accessString' - Access permissions string used for this user.
--
-- 'noPasswordRequired', 'modifyUser_noPasswordRequired' - Indicates no password is required for the user.
--
-- 'userId', 'modifyUser_userId' - The ID of the user.
newModifyUser ::
  -- | 'userId'
  Prelude.Text ->
  ModifyUser
newModifyUser pUserId_ =
  ModifyUser'
    { appendAccessString = Prelude.Nothing,
      passwords = Prelude.Nothing,
      accessString = Prelude.Nothing,
      noPasswordRequired = Prelude.Nothing,
      userId = pUserId_
    }

-- | Adds additional user permissions to the access string.
modifyUser_appendAccessString :: Lens.Lens' ModifyUser (Prelude.Maybe Prelude.Text)
modifyUser_appendAccessString = Lens.lens (\ModifyUser' {appendAccessString} -> appendAccessString) (\s@ModifyUser' {} a -> s {appendAccessString = a} :: ModifyUser)

-- | The passwords belonging to the user. You are allowed up to two.
modifyUser_passwords :: Lens.Lens' ModifyUser (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
modifyUser_passwords = Lens.lens (\ModifyUser' {passwords} -> passwords) (\s@ModifyUser' {} a -> s {passwords = a} :: ModifyUser) Prelude.. Lens.mapping Prelude._Coerce

-- | Access permissions string used for this user.
modifyUser_accessString :: Lens.Lens' ModifyUser (Prelude.Maybe Prelude.Text)
modifyUser_accessString = Lens.lens (\ModifyUser' {accessString} -> accessString) (\s@ModifyUser' {} a -> s {accessString = a} :: ModifyUser)

-- | Indicates no password is required for the user.
modifyUser_noPasswordRequired :: Lens.Lens' ModifyUser (Prelude.Maybe Prelude.Bool)
modifyUser_noPasswordRequired = Lens.lens (\ModifyUser' {noPasswordRequired} -> noPasswordRequired) (\s@ModifyUser' {} a -> s {noPasswordRequired = a} :: ModifyUser)

-- | The ID of the user.
modifyUser_userId :: Lens.Lens' ModifyUser Prelude.Text
modifyUser_userId = Lens.lens (\ModifyUser' {userId} -> userId) (\s@ModifyUser' {} a -> s {userId = a} :: ModifyUser)

instance Prelude.AWSRequest ModifyUser where
  type Rs ModifyUser = User
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyUserResult"
      (\s h x -> Prelude.parseXML x)

instance Prelude.Hashable ModifyUser

instance Prelude.NFData ModifyUser

instance Prelude.ToHeaders ModifyUser where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ModifyUser where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ModifyUser where
  toQuery ModifyUser' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ModifyUser" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2015-02-02" :: Prelude.ByteString),
        "AppendAccessString" Prelude.=: appendAccessString,
        "Passwords"
          Prelude.=: Prelude.toQuery
            (Prelude.toQueryList "member" Prelude.<$> passwords),
        "AccessString" Prelude.=: accessString,
        "NoPasswordRequired" Prelude.=: noPasswordRequired,
        "UserId" Prelude.=: userId
      ]
