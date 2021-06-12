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

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyUser' smart constructor.
data ModifyUser = ModifyUser'
  { -- | Adds additional user permissions to the access string.
    appendAccessString :: Core.Maybe Core.Text,
    -- | The passwords belonging to the user. You are allowed up to two.
    passwords :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | Access permissions string used for this user.
    accessString :: Core.Maybe Core.Text,
    -- | Indicates no password is required for the user.
    noPasswordRequired :: Core.Maybe Core.Bool,
    -- | The ID of the user.
    userId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  ModifyUser
newModifyUser pUserId_ =
  ModifyUser'
    { appendAccessString = Core.Nothing,
      passwords = Core.Nothing,
      accessString = Core.Nothing,
      noPasswordRequired = Core.Nothing,
      userId = pUserId_
    }

-- | Adds additional user permissions to the access string.
modifyUser_appendAccessString :: Lens.Lens' ModifyUser (Core.Maybe Core.Text)
modifyUser_appendAccessString = Lens.lens (\ModifyUser' {appendAccessString} -> appendAccessString) (\s@ModifyUser' {} a -> s {appendAccessString = a} :: ModifyUser)

-- | The passwords belonging to the user. You are allowed up to two.
modifyUser_passwords :: Lens.Lens' ModifyUser (Core.Maybe (Core.NonEmpty Core.Text))
modifyUser_passwords = Lens.lens (\ModifyUser' {passwords} -> passwords) (\s@ModifyUser' {} a -> s {passwords = a} :: ModifyUser) Core.. Lens.mapping Lens._Coerce

-- | Access permissions string used for this user.
modifyUser_accessString :: Lens.Lens' ModifyUser (Core.Maybe Core.Text)
modifyUser_accessString = Lens.lens (\ModifyUser' {accessString} -> accessString) (\s@ModifyUser' {} a -> s {accessString = a} :: ModifyUser)

-- | Indicates no password is required for the user.
modifyUser_noPasswordRequired :: Lens.Lens' ModifyUser (Core.Maybe Core.Bool)
modifyUser_noPasswordRequired = Lens.lens (\ModifyUser' {noPasswordRequired} -> noPasswordRequired) (\s@ModifyUser' {} a -> s {noPasswordRequired = a} :: ModifyUser)

-- | The ID of the user.
modifyUser_userId :: Lens.Lens' ModifyUser Core.Text
modifyUser_userId = Lens.lens (\ModifyUser' {userId} -> userId) (\s@ModifyUser' {} a -> s {userId = a} :: ModifyUser)

instance Core.AWSRequest ModifyUser where
  type AWSResponse ModifyUser = User
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyUserResult"
      (\s h x -> Core.parseXML x)

instance Core.Hashable ModifyUser

instance Core.NFData ModifyUser

instance Core.ToHeaders ModifyUser where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifyUser where
  toPath = Core.const "/"

instance Core.ToQuery ModifyUser where
  toQuery ModifyUser' {..} =
    Core.mconcat
      [ "Action" Core.=: ("ModifyUser" :: Core.ByteString),
        "Version" Core.=: ("2015-02-02" :: Core.ByteString),
        "AppendAccessString" Core.=: appendAccessString,
        "Passwords"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> passwords),
        "AccessString" Core.=: accessString,
        "NoPasswordRequired" Core.=: noPasswordRequired,
        "UserId" Core.=: userId
      ]
