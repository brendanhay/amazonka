{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DeleteLifecycleHook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified lifecycle hook.
--
-- If there are any outstanding lifecycle actions, they are completed first (@ABANDON@ for launching instances, @CONTINUE@ for terminating instances).
module Network.AWS.AutoScaling.DeleteLifecycleHook
  ( -- * Creating a request
    DeleteLifecycleHook (..),
    mkDeleteLifecycleHook,

    -- ** Request lenses
    delLifecycleHookName,
    delAutoScalingGroupName,

    -- * Destructuring the response
    DeleteLifecycleHookResponse (..),
    mkDeleteLifecycleHookResponse,

    -- ** Response lenses
    drsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteLifecycleHook' smart constructor.
data DeleteLifecycleHook = DeleteLifecycleHook'
  { lifecycleHookName ::
      Lude.Text,
    autoScalingGroupName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteLifecycleHook' with the minimum fields required to make a request.
--
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
-- * 'lifecycleHookName' - The name of the lifecycle hook.
mkDeleteLifecycleHook ::
  -- | 'lifecycleHookName'
  Lude.Text ->
  -- | 'autoScalingGroupName'
  Lude.Text ->
  DeleteLifecycleHook
mkDeleteLifecycleHook pLifecycleHookName_ pAutoScalingGroupName_ =
  DeleteLifecycleHook'
    { lifecycleHookName = pLifecycleHookName_,
      autoScalingGroupName = pAutoScalingGroupName_
    }

-- | The name of the lifecycle hook.
--
-- /Note:/ Consider using 'lifecycleHookName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delLifecycleHookName :: Lens.Lens' DeleteLifecycleHook Lude.Text
delLifecycleHookName = Lens.lens (lifecycleHookName :: DeleteLifecycleHook -> Lude.Text) (\s a -> s {lifecycleHookName = a} :: DeleteLifecycleHook)
{-# DEPRECATED delLifecycleHookName "Use generic-lens or generic-optics with 'lifecycleHookName' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
delAutoScalingGroupName :: Lens.Lens' DeleteLifecycleHook Lude.Text
delAutoScalingGroupName = Lens.lens (autoScalingGroupName :: DeleteLifecycleHook -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: DeleteLifecycleHook)
{-# DEPRECATED delAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

instance Lude.AWSRequest DeleteLifecycleHook where
  type Rs DeleteLifecycleHook = DeleteLifecycleHookResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "DeleteLifecycleHookResult"
      ( \s h x ->
          DeleteLifecycleHookResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteLifecycleHook where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteLifecycleHook where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteLifecycleHook where
  toQuery DeleteLifecycleHook' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteLifecycleHook" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "LifecycleHookName" Lude.=: lifecycleHookName,
        "AutoScalingGroupName" Lude.=: autoScalingGroupName
      ]

-- | /See:/ 'mkDeleteLifecycleHookResponse' smart constructor.
newtype DeleteLifecycleHookResponse = DeleteLifecycleHookResponse'
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

-- | Creates a value of 'DeleteLifecycleHookResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteLifecycleHookResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteLifecycleHookResponse
mkDeleteLifecycleHookResponse pResponseStatus_ =
  DeleteLifecycleHookResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteLifecycleHookResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DeleteLifecycleHookResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteLifecycleHookResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
