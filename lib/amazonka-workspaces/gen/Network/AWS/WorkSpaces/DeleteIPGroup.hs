{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DeleteIPGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified IP access control group.
--
-- You cannot delete an IP access control group that is associated with a directory.
module Network.AWS.WorkSpaces.DeleteIPGroup
  ( -- * Creating a request
    DeleteIPGroup (..),
    mkDeleteIPGroup,

    -- ** Request lenses
    digGroupId,

    -- * Destructuring the response
    DeleteIPGroupResponse (..),
    mkDeleteIPGroupResponse,

    -- ** Response lenses
    digrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'mkDeleteIPGroup' smart constructor.
newtype DeleteIPGroup = DeleteIPGroup'
  { -- | The identifier of the IP access control group.
    groupId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteIPGroup' with the minimum fields required to make a request.
--
-- * 'groupId' - The identifier of the IP access control group.
mkDeleteIPGroup ::
  -- | 'groupId'
  Lude.Text ->
  DeleteIPGroup
mkDeleteIPGroup pGroupId_ = DeleteIPGroup' {groupId = pGroupId_}

-- | The identifier of the IP access control group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digGroupId :: Lens.Lens' DeleteIPGroup Lude.Text
digGroupId = Lens.lens (groupId :: DeleteIPGroup -> Lude.Text) (\s a -> s {groupId = a} :: DeleteIPGroup)
{-# DEPRECATED digGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

instance Lude.AWSRequest DeleteIPGroup where
  type Rs DeleteIPGroup = DeleteIPGroupResponse
  request = Req.postJSON workSpacesService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteIPGroupResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteIPGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("WorkspacesService.DeleteIpGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteIPGroup where
  toJSON DeleteIPGroup' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("GroupId" Lude..= groupId)])

instance Lude.ToPath DeleteIPGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteIPGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteIPGroupResponse' smart constructor.
newtype DeleteIPGroupResponse = DeleteIPGroupResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteIPGroupResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteIPGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteIPGroupResponse
mkDeleteIPGroupResponse pResponseStatus_ =
  DeleteIPGroupResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digrsResponseStatus :: Lens.Lens' DeleteIPGroupResponse Lude.Int
digrsResponseStatus = Lens.lens (responseStatus :: DeleteIPGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteIPGroupResponse)
{-# DEPRECATED digrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
