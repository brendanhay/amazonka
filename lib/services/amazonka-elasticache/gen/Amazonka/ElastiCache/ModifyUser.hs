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
-- Module      : Amazonka.ElastiCache.ModifyUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes user password(s) and\/or access string.
module Amazonka.ElastiCache.ModifyUser
  ( -- * Creating a Request
    ModifyUser (..),
    newModifyUser,

    -- * Request Lenses
    modifyUser_appendAccessString,
    modifyUser_accessString,
    modifyUser_passwords,
    modifyUser_noPasswordRequired,
    modifyUser_userId,

    -- * Destructuring the Response
    User (..),
    newUser,

    -- * Response Lenses
    user_status,
    user_arn,
    user_userGroupIds,
    user_authentication,
    user_engine,
    user_userName,
    user_accessString,
    user_userId,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.ElastiCache.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyUser' smart constructor.
data ModifyUser = ModifyUser'
  { -- | Adds additional user permissions to the access string.
    appendAccessString :: Prelude.Maybe Prelude.Text,
    -- | Access permissions string used for this user.
    accessString :: Prelude.Maybe Prelude.Text,
    -- | The passwords belonging to the user. You are allowed up to two.
    passwords :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Indicates no password is required for the user.
    noPasswordRequired :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the user.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'accessString', 'modifyUser_accessString' - Access permissions string used for this user.
--
-- 'passwords', 'modifyUser_passwords' - The passwords belonging to the user. You are allowed up to two.
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
      accessString = Prelude.Nothing,
      passwords = Prelude.Nothing,
      noPasswordRequired = Prelude.Nothing,
      userId = pUserId_
    }

-- | Adds additional user permissions to the access string.
modifyUser_appendAccessString :: Lens.Lens' ModifyUser (Prelude.Maybe Prelude.Text)
modifyUser_appendAccessString = Lens.lens (\ModifyUser' {appendAccessString} -> appendAccessString) (\s@ModifyUser' {} a -> s {appendAccessString = a} :: ModifyUser)

-- | Access permissions string used for this user.
modifyUser_accessString :: Lens.Lens' ModifyUser (Prelude.Maybe Prelude.Text)
modifyUser_accessString = Lens.lens (\ModifyUser' {accessString} -> accessString) (\s@ModifyUser' {} a -> s {accessString = a} :: ModifyUser)

-- | The passwords belonging to the user. You are allowed up to two.
modifyUser_passwords :: Lens.Lens' ModifyUser (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
modifyUser_passwords = Lens.lens (\ModifyUser' {passwords} -> passwords) (\s@ModifyUser' {} a -> s {passwords = a} :: ModifyUser) Prelude.. Lens.mapping Lens.coerced

-- | Indicates no password is required for the user.
modifyUser_noPasswordRequired :: Lens.Lens' ModifyUser (Prelude.Maybe Prelude.Bool)
modifyUser_noPasswordRequired = Lens.lens (\ModifyUser' {noPasswordRequired} -> noPasswordRequired) (\s@ModifyUser' {} a -> s {noPasswordRequired = a} :: ModifyUser)

-- | The ID of the user.
modifyUser_userId :: Lens.Lens' ModifyUser Prelude.Text
modifyUser_userId = Lens.lens (\ModifyUser' {userId} -> userId) (\s@ModifyUser' {} a -> s {userId = a} :: ModifyUser)

instance Core.AWSRequest ModifyUser where
  type AWSResponse ModifyUser = User
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyUserResult"
      (\s h x -> Core.parseXML x)

instance Prelude.Hashable ModifyUser where
  hashWithSalt _salt ModifyUser' {..} =
    _salt `Prelude.hashWithSalt` appendAccessString
      `Prelude.hashWithSalt` accessString
      `Prelude.hashWithSalt` passwords
      `Prelude.hashWithSalt` noPasswordRequired
      `Prelude.hashWithSalt` userId

instance Prelude.NFData ModifyUser where
  rnf ModifyUser' {..} =
    Prelude.rnf appendAccessString
      `Prelude.seq` Prelude.rnf accessString
      `Prelude.seq` Prelude.rnf passwords
      `Prelude.seq` Prelude.rnf noPasswordRequired
      `Prelude.seq` Prelude.rnf userId

instance Core.ToHeaders ModifyUser where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifyUser where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifyUser where
  toQuery ModifyUser' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ModifyUser" :: Prelude.ByteString),
        "Version"
          Core.=: ("2015-02-02" :: Prelude.ByteString),
        "AppendAccessString" Core.=: appendAccessString,
        "AccessString" Core.=: accessString,
        "Passwords"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> passwords),
        "NoPasswordRequired" Core.=: noPasswordRequired,
        "UserId" Core.=: userId
      ]
