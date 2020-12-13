{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.DeleteGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a group resource.
module Network.AWS.XRay.DeleteGroup
  ( -- * Creating a request
    DeleteGroup (..),
    mkDeleteGroup,

    -- ** Request lenses
    dgGroupARN,
    dgGroupName,

    -- * Destructuring the response
    DeleteGroupResponse (..),
    mkDeleteGroupResponse,

    -- ** Response lenses
    dgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.XRay.Types

-- | /See:/ 'mkDeleteGroup' smart constructor.
data DeleteGroup = DeleteGroup'
  { -- | The ARN of the group that was generated on creation.
    groupARN :: Lude.Maybe Lude.Text,
    -- | The case-sensitive name of the group.
    groupName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteGroup' with the minimum fields required to make a request.
--
-- * 'groupARN' - The ARN of the group that was generated on creation.
-- * 'groupName' - The case-sensitive name of the group.
mkDeleteGroup ::
  DeleteGroup
mkDeleteGroup =
  DeleteGroup' {groupARN = Lude.Nothing, groupName = Lude.Nothing}

-- | The ARN of the group that was generated on creation.
--
-- /Note:/ Consider using 'groupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgGroupARN :: Lens.Lens' DeleteGroup (Lude.Maybe Lude.Text)
dgGroupARN = Lens.lens (groupARN :: DeleteGroup -> Lude.Maybe Lude.Text) (\s a -> s {groupARN = a} :: DeleteGroup)
{-# DEPRECATED dgGroupARN "Use generic-lens or generic-optics with 'groupARN' instead." #-}

-- | The case-sensitive name of the group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgGroupName :: Lens.Lens' DeleteGroup (Lude.Maybe Lude.Text)
dgGroupName = Lens.lens (groupName :: DeleteGroup -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: DeleteGroup)
{-# DEPRECATED dgGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Lude.AWSRequest DeleteGroup where
  type Rs DeleteGroup = DeleteGroupResponse
  request = Req.postJSON xRayService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteGroupResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON DeleteGroup where
  toJSON DeleteGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("GroupARN" Lude..=) Lude.<$> groupARN,
            ("GroupName" Lude..=) Lude.<$> groupName
          ]
      )

instance Lude.ToPath DeleteGroup where
  toPath = Lude.const "/DeleteGroup"

instance Lude.ToQuery DeleteGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteGroupResponse' smart constructor.
newtype DeleteGroupResponse = DeleteGroupResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
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
