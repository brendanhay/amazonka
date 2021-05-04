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
-- Module      : Network.AWS.IAM.AddUserToGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified user to the specified group.
module Network.AWS.IAM.AddUserToGroup
  ( -- * Creating a Request
    AddUserToGroup (..),
    newAddUserToGroup,

    -- * Request Lenses
    addUserToGroup_groupName,
    addUserToGroup_userName,

    -- * Destructuring the Response
    AddUserToGroupResponse (..),
    newAddUserToGroupResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAddUserToGroup' smart constructor.
data AddUserToGroup = AddUserToGroup'
  { -- | The name of the group to update.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    groupName :: Prelude.Text,
    -- | The name of the user to add.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AddUserToGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'addUserToGroup_groupName' - The name of the group to update.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'userName', 'addUserToGroup_userName' - The name of the user to add.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newAddUserToGroup ::
  -- | 'groupName'
  Prelude.Text ->
  -- | 'userName'
  Prelude.Text ->
  AddUserToGroup
newAddUserToGroup pGroupName_ pUserName_ =
  AddUserToGroup'
    { groupName = pGroupName_,
      userName = pUserName_
    }

-- | The name of the group to update.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
addUserToGroup_groupName :: Lens.Lens' AddUserToGroup Prelude.Text
addUserToGroup_groupName = Lens.lens (\AddUserToGroup' {groupName} -> groupName) (\s@AddUserToGroup' {} a -> s {groupName = a} :: AddUserToGroup)

-- | The name of the user to add.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
addUserToGroup_userName :: Lens.Lens' AddUserToGroup Prelude.Text
addUserToGroup_userName = Lens.lens (\AddUserToGroup' {userName} -> userName) (\s@AddUserToGroup' {} a -> s {userName = a} :: AddUserToGroup)

instance Prelude.AWSRequest AddUserToGroup where
  type Rs AddUserToGroup = AddUserToGroupResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull AddUserToGroupResponse'

instance Prelude.Hashable AddUserToGroup

instance Prelude.NFData AddUserToGroup

instance Prelude.ToHeaders AddUserToGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath AddUserToGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AddUserToGroup where
  toQuery AddUserToGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("AddUserToGroup" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "GroupName" Prelude.=: groupName,
        "UserName" Prelude.=: userName
      ]

-- | /See:/ 'newAddUserToGroupResponse' smart constructor.
data AddUserToGroupResponse = AddUserToGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AddUserToGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAddUserToGroupResponse ::
  AddUserToGroupResponse
newAddUserToGroupResponse = AddUserToGroupResponse'

instance Prelude.NFData AddUserToGroupResponse
