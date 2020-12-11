{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.DeleteProtectionGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified protection group.
module Network.AWS.Shield.DeleteProtectionGroup
  ( -- * Creating a request
    DeleteProtectionGroup (..),
    mkDeleteProtectionGroup,

    -- ** Request lenses
    dpgProtectionGroupId,

    -- * Destructuring the response
    DeleteProtectionGroupResponse (..),
    mkDeleteProtectionGroupResponse,

    -- ** Response lenses
    dpgrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Shield.Types

-- | /See:/ 'mkDeleteProtectionGroup' smart constructor.
newtype DeleteProtectionGroup = DeleteProtectionGroup'
  { protectionGroupId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteProtectionGroup' with the minimum fields required to make a request.
--
-- * 'protectionGroupId' - The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it.
mkDeleteProtectionGroup ::
  -- | 'protectionGroupId'
  Lude.Text ->
  DeleteProtectionGroup
mkDeleteProtectionGroup pProtectionGroupId_ =
  DeleteProtectionGroup' {protectionGroupId = pProtectionGroupId_}

-- | The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it.
--
-- /Note:/ Consider using 'protectionGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgProtectionGroupId :: Lens.Lens' DeleteProtectionGroup Lude.Text
dpgProtectionGroupId = Lens.lens (protectionGroupId :: DeleteProtectionGroup -> Lude.Text) (\s a -> s {protectionGroupId = a} :: DeleteProtectionGroup)
{-# DEPRECATED dpgProtectionGroupId "Use generic-lens or generic-optics with 'protectionGroupId' instead." #-}

instance Lude.AWSRequest DeleteProtectionGroup where
  type Rs DeleteProtectionGroup = DeleteProtectionGroupResponse
  request = Req.postJSON shieldService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteProtectionGroupResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteProtectionGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSShield_20160616.DeleteProtectionGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteProtectionGroup where
  toJSON DeleteProtectionGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ProtectionGroupId" Lude..= protectionGroupId)]
      )

instance Lude.ToPath DeleteProtectionGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteProtectionGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteProtectionGroupResponse' smart constructor.
newtype DeleteProtectionGroupResponse = DeleteProtectionGroupResponse'
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

-- | Creates a value of 'DeleteProtectionGroupResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteProtectionGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteProtectionGroupResponse
mkDeleteProtectionGroupResponse pResponseStatus_ =
  DeleteProtectionGroupResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpgrsResponseStatus :: Lens.Lens' DeleteProtectionGroupResponse Lude.Int
dpgrsResponseStatus = Lens.lens (responseStatus :: DeleteProtectionGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteProtectionGroupResponse)
{-# DEPRECATED dpgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
