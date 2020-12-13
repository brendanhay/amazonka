{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.AddUserToGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified user to the specified group.
module Network.AWS.IAM.AddUserToGroup
  ( -- * Creating a request
    AddUserToGroup (..),
    mkAddUserToGroup,

    -- ** Request lenses
    autgUserName,
    autgGroupName,

    -- * Destructuring the response
    AddUserToGroupResponse (..),
    mkAddUserToGroupResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAddUserToGroup' smart constructor.
data AddUserToGroup = AddUserToGroup'
  { -- | The name of the user to add.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Lude.Text,
    -- | The name of the group to update.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    groupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddUserToGroup' with the minimum fields required to make a request.
--
-- * 'userName' - The name of the user to add.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
-- * 'groupName' - The name of the group to update.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkAddUserToGroup ::
  -- | 'userName'
  Lude.Text ->
  -- | 'groupName'
  Lude.Text ->
  AddUserToGroup
mkAddUserToGroup pUserName_ pGroupName_ =
  AddUserToGroup' {userName = pUserName_, groupName = pGroupName_}

-- | The name of the user to add.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
autgUserName :: Lens.Lens' AddUserToGroup Lude.Text
autgUserName = Lens.lens (userName :: AddUserToGroup -> Lude.Text) (\s a -> s {userName = a} :: AddUserToGroup)
{-# DEPRECATED autgUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The name of the group to update.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
autgGroupName :: Lens.Lens' AddUserToGroup Lude.Text
autgGroupName = Lens.lens (groupName :: AddUserToGroup -> Lude.Text) (\s a -> s {groupName = a} :: AddUserToGroup)
{-# DEPRECATED autgGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Lude.AWSRequest AddUserToGroup where
  type Rs AddUserToGroup = AddUserToGroupResponse
  request = Req.postQuery iamService
  response = Res.receiveNull AddUserToGroupResponse'

instance Lude.ToHeaders AddUserToGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AddUserToGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery AddUserToGroup where
  toQuery AddUserToGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AddUserToGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "UserName" Lude.=: userName,
        "GroupName" Lude.=: groupName
      ]

-- | /See:/ 'mkAddUserToGroupResponse' smart constructor.
data AddUserToGroupResponse = AddUserToGroupResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddUserToGroupResponse' with the minimum fields required to make a request.
mkAddUserToGroupResponse ::
  AddUserToGroupResponse
mkAddUserToGroupResponse = AddUserToGroupResponse'
