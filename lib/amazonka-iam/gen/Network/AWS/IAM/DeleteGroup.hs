{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified IAM group. The group must not contain any users or have any attached policies.
module Network.AWS.IAM.DeleteGroup
  ( -- * Creating a request
    DeleteGroup (..),
    mkDeleteGroup,

    -- ** Request lenses
    dgGroupName,

    -- * Destructuring the response
    DeleteGroupResponse (..),
    mkDeleteGroupResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteGroup' smart constructor.
newtype DeleteGroup = DeleteGroup'
  { -- | The name of the IAM group to delete.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    groupName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteGroup' with the minimum fields required to make a request.
--
-- * 'groupName' - The name of the IAM group to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkDeleteGroup ::
  -- | 'groupName'
  Lude.Text ->
  DeleteGroup
mkDeleteGroup pGroupName_ = DeleteGroup' {groupName = pGroupName_}

-- | The name of the IAM group to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgGroupName :: Lens.Lens' DeleteGroup Lude.Text
dgGroupName = Lens.lens (groupName :: DeleteGroup -> Lude.Text) (\s a -> s {groupName = a} :: DeleteGroup)
{-# DEPRECATED dgGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Lude.AWSRequest DeleteGroup where
  type Rs DeleteGroup = DeleteGroupResponse
  request = Req.postQuery iamService
  response = Res.receiveNull DeleteGroupResponse'

instance Lude.ToHeaders DeleteGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteGroup where
  toQuery DeleteGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "GroupName" Lude.=: groupName
      ]

-- | /See:/ 'mkDeleteGroupResponse' smart constructor.
data DeleteGroupResponse = DeleteGroupResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteGroupResponse' with the minimum fields required to make a request.
mkDeleteGroupResponse ::
  DeleteGroupResponse
mkDeleteGroupResponse = DeleteGroupResponse'
