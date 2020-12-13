{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.DeleteTargetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified target group.
--
-- You can delete a target group if it is not referenced by any actions. Deleting a target group also deletes any associated health checks. Deleting a target group does not affect its registered targets. For example, any EC2 instances continue to run until you stop or terminate them.
module Network.AWS.ELBv2.DeleteTargetGroup
  ( -- * Creating a request
    DeleteTargetGroup (..),
    mkDeleteTargetGroup,

    -- ** Request lenses
    dtgTargetGroupARN,

    -- * Destructuring the response
    DeleteTargetGroupResponse (..),
    mkDeleteTargetGroupResponse,

    -- ** Response lenses
    dtgfrsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteTargetGroup' smart constructor.
newtype DeleteTargetGroup = DeleteTargetGroup'
  { -- | The Amazon Resource Name (ARN) of the target group.
    targetGroupARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTargetGroup' with the minimum fields required to make a request.
--
-- * 'targetGroupARN' - The Amazon Resource Name (ARN) of the target group.
mkDeleteTargetGroup ::
  -- | 'targetGroupARN'
  Lude.Text ->
  DeleteTargetGroup
mkDeleteTargetGroup pTargetGroupARN_ =
  DeleteTargetGroup' {targetGroupARN = pTargetGroupARN_}

-- | The Amazon Resource Name (ARN) of the target group.
--
-- /Note:/ Consider using 'targetGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgTargetGroupARN :: Lens.Lens' DeleteTargetGroup Lude.Text
dtgTargetGroupARN = Lens.lens (targetGroupARN :: DeleteTargetGroup -> Lude.Text) (\s a -> s {targetGroupARN = a} :: DeleteTargetGroup)
{-# DEPRECATED dtgTargetGroupARN "Use generic-lens or generic-optics with 'targetGroupARN' instead." #-}

instance Lude.AWSRequest DeleteTargetGroup where
  type Rs DeleteTargetGroup = DeleteTargetGroupResponse
  request = Req.postQuery eLBv2Service
  response =
    Res.receiveXMLWrapper
      "DeleteTargetGroupResult"
      ( \s h x ->
          DeleteTargetGroupResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteTargetGroup where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteTargetGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTargetGroup where
  toQuery DeleteTargetGroup' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteTargetGroup" :: Lude.ByteString),
        "Version" Lude.=: ("2015-12-01" :: Lude.ByteString),
        "TargetGroupArn" Lude.=: targetGroupARN
      ]

-- | /See:/ 'mkDeleteTargetGroupResponse' smart constructor.
newtype DeleteTargetGroupResponse = DeleteTargetGroupResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTargetGroupResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteTargetGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteTargetGroupResponse
mkDeleteTargetGroupResponse pResponseStatus_ =
  DeleteTargetGroupResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgfrsResponseStatus :: Lens.Lens' DeleteTargetGroupResponse Lude.Int
dtgfrsResponseStatus = Lens.lens (responseStatus :: DeleteTargetGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTargetGroupResponse)
{-# DEPRECATED dtgfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
