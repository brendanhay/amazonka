{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.DeleteWorkGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the workgroup with the specified name. The primary workgroup cannot be deleted.
module Network.AWS.Athena.DeleteWorkGroup
  ( -- * Creating a request
    DeleteWorkGroup (..),
    mkDeleteWorkGroup,

    -- ** Request lenses
    dwgRecursiveDeleteOption,
    dwgWorkGroup,

    -- * Destructuring the response
    DeleteWorkGroupResponse (..),
    mkDeleteWorkGroupResponse,

    -- ** Response lenses
    dwgrsResponseStatus,
  )
where

import Network.AWS.Athena.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteWorkGroup' smart constructor.
data DeleteWorkGroup = DeleteWorkGroup'
  { -- | The option to delete the workgroup and its contents even if the workgroup contains any named queries.
    recursiveDeleteOption :: Lude.Maybe Lude.Bool,
    -- | The unique name of the workgroup to delete.
    workGroup :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteWorkGroup' with the minimum fields required to make a request.
--
-- * 'recursiveDeleteOption' - The option to delete the workgroup and its contents even if the workgroup contains any named queries.
-- * 'workGroup' - The unique name of the workgroup to delete.
mkDeleteWorkGroup ::
  -- | 'workGroup'
  Lude.Text ->
  DeleteWorkGroup
mkDeleteWorkGroup pWorkGroup_ =
  DeleteWorkGroup'
    { recursiveDeleteOption = Lude.Nothing,
      workGroup = pWorkGroup_
    }

-- | The option to delete the workgroup and its contents even if the workgroup contains any named queries.
--
-- /Note:/ Consider using 'recursiveDeleteOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwgRecursiveDeleteOption :: Lens.Lens' DeleteWorkGroup (Lude.Maybe Lude.Bool)
dwgRecursiveDeleteOption = Lens.lens (recursiveDeleteOption :: DeleteWorkGroup -> Lude.Maybe Lude.Bool) (\s a -> s {recursiveDeleteOption = a} :: DeleteWorkGroup)
{-# DEPRECATED dwgRecursiveDeleteOption "Use generic-lens or generic-optics with 'recursiveDeleteOption' instead." #-}

-- | The unique name of the workgroup to delete.
--
-- /Note:/ Consider using 'workGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwgWorkGroup :: Lens.Lens' DeleteWorkGroup Lude.Text
dwgWorkGroup = Lens.lens (workGroup :: DeleteWorkGroup -> Lude.Text) (\s a -> s {workGroup = a} :: DeleteWorkGroup)
{-# DEPRECATED dwgWorkGroup "Use generic-lens or generic-optics with 'workGroup' instead." #-}

instance Lude.AWSRequest DeleteWorkGroup where
  type Rs DeleteWorkGroup = DeleteWorkGroupResponse
  request = Req.postJSON athenaService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteWorkGroupResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteWorkGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonAthena.DeleteWorkGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteWorkGroup where
  toJSON DeleteWorkGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RecursiveDeleteOption" Lude..=) Lude.<$> recursiveDeleteOption,
            Lude.Just ("WorkGroup" Lude..= workGroup)
          ]
      )

instance Lude.ToPath DeleteWorkGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteWorkGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteWorkGroupResponse' smart constructor.
newtype DeleteWorkGroupResponse = DeleteWorkGroupResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteWorkGroupResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteWorkGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteWorkGroupResponse
mkDeleteWorkGroupResponse pResponseStatus_ =
  DeleteWorkGroupResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwgrsResponseStatus :: Lens.Lens' DeleteWorkGroupResponse Lude.Int
dwgrsResponseStatus = Lens.lens (responseStatus :: DeleteWorkGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteWorkGroupResponse)
{-# DEPRECATED dwgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
