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
-- Module      : Amazonka.IAM.RemoveUserFromGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified user from the specified group.
module Amazonka.IAM.RemoveUserFromGroup
  ( -- * Creating a Request
    RemoveUserFromGroup (..),
    newRemoveUserFromGroup,

    -- * Request Lenses
    removeUserFromGroup_groupName,
    removeUserFromGroup_userName,

    -- * Destructuring the Response
    RemoveUserFromGroupResponse (..),
    newRemoveUserFromGroupResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRemoveUserFromGroup' smart constructor.
data RemoveUserFromGroup = RemoveUserFromGroup'
  { -- | The name of the group to update.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    groupName :: Prelude.Text,
    -- | The name of the user to remove.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveUserFromGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'removeUserFromGroup_groupName' - The name of the group to update.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'userName', 'removeUserFromGroup_userName' - The name of the user to remove.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newRemoveUserFromGroup ::
  -- | 'groupName'
  Prelude.Text ->
  -- | 'userName'
  Prelude.Text ->
  RemoveUserFromGroup
newRemoveUserFromGroup pGroupName_ pUserName_ =
  RemoveUserFromGroup'
    { groupName = pGroupName_,
      userName = pUserName_
    }

-- | The name of the group to update.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
removeUserFromGroup_groupName :: Lens.Lens' RemoveUserFromGroup Prelude.Text
removeUserFromGroup_groupName = Lens.lens (\RemoveUserFromGroup' {groupName} -> groupName) (\s@RemoveUserFromGroup' {} a -> s {groupName = a} :: RemoveUserFromGroup)

-- | The name of the user to remove.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
removeUserFromGroup_userName :: Lens.Lens' RemoveUserFromGroup Prelude.Text
removeUserFromGroup_userName = Lens.lens (\RemoveUserFromGroup' {userName} -> userName) (\s@RemoveUserFromGroup' {} a -> s {userName = a} :: RemoveUserFromGroup)

instance Core.AWSRequest RemoveUserFromGroup where
  type
    AWSResponse RemoveUserFromGroup =
      RemoveUserFromGroupResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull RemoveUserFromGroupResponse'

instance Prelude.Hashable RemoveUserFromGroup where
  hashWithSalt _salt RemoveUserFromGroup' {..} =
    _salt `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` userName

instance Prelude.NFData RemoveUserFromGroup where
  rnf RemoveUserFromGroup' {..} =
    Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf userName

instance Data.ToHeaders RemoveUserFromGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RemoveUserFromGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery RemoveUserFromGroup where
  toQuery RemoveUserFromGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("RemoveUserFromGroup" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "GroupName" Data.=: groupName,
        "UserName" Data.=: userName
      ]

-- | /See:/ 'newRemoveUserFromGroupResponse' smart constructor.
data RemoveUserFromGroupResponse = RemoveUserFromGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoveUserFromGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRemoveUserFromGroupResponse ::
  RemoveUserFromGroupResponse
newRemoveUserFromGroupResponse =
  RemoveUserFromGroupResponse'

instance Prelude.NFData RemoveUserFromGroupResponse where
  rnf _ = ()
