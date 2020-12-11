{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.DeleteGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a group.
module Network.AWS.Greengrass.DeleteGroup
  ( -- * Creating a request
    DeleteGroup (..),
    mkDeleteGroup,

    -- ** Request lenses
    dgGroupId,

    -- * Destructuring the response
    DeleteGroupResponse (..),
    mkDeleteGroupResponse,

    -- ** Response lenses
    dgrsResponseStatus,
  )
where

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteGroup' smart constructor.
newtype DeleteGroup = DeleteGroup' {groupId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteGroup' with the minimum fields required to make a request.
--
-- * 'groupId' - The ID of the Greengrass group.
mkDeleteGroup ::
  -- | 'groupId'
  Lude.Text ->
  DeleteGroup
mkDeleteGroup pGroupId_ = DeleteGroup' {groupId = pGroupId_}

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgGroupId :: Lens.Lens' DeleteGroup Lude.Text
dgGroupId = Lens.lens (groupId :: DeleteGroup -> Lude.Text) (\s a -> s {groupId = a} :: DeleteGroup)
{-# DEPRECATED dgGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

instance Lude.AWSRequest DeleteGroup where
  type Rs DeleteGroup = DeleteGroupResponse
  request = Req.delete greengrassService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteGroupResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteGroup where
  toPath DeleteGroup' {..} =
    Lude.mconcat ["/greengrass/groups/", Lude.toBS groupId]

instance Lude.ToQuery DeleteGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteGroupResponse' smart constructor.
newtype DeleteGroupResponse = DeleteGroupResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteGroupResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteGroupResponse
mkDeleteGroupResponse pResponseStatus_ =
  DeleteGroupResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgrsResponseStatus :: Lens.Lens' DeleteGroupResponse Lude.Int
dgrsResponseStatus = Lens.lens (responseStatus :: DeleteGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteGroupResponse)
{-# DEPRECATED dgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
