{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DeregisterTaskFromMaintenanceWindow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a task from a maintenance window.
module Network.AWS.SSM.DeregisterTaskFromMaintenanceWindow
  ( -- * Creating a request
    DeregisterTaskFromMaintenanceWindow (..),
    mkDeregisterTaskFromMaintenanceWindow,

    -- ** Request lenses
    derWindowId,
    derWindowTaskId,

    -- * Destructuring the response
    DeregisterTaskFromMaintenanceWindowResponse (..),
    mkDeregisterTaskFromMaintenanceWindowResponse,

    -- ** Response lenses
    derrsWindowTaskId,
    derrsWindowId,
    derrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDeregisterTaskFromMaintenanceWindow' smart constructor.
data DeregisterTaskFromMaintenanceWindow = DeregisterTaskFromMaintenanceWindow'
  { windowId ::
      Lude.Text,
    windowTaskId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterTaskFromMaintenanceWindow' with the minimum fields required to make a request.
--
-- * 'windowId' - The ID of the maintenance window the task should be removed from.
-- * 'windowTaskId' - The ID of the task to remove from the maintenance window.
mkDeregisterTaskFromMaintenanceWindow ::
  -- | 'windowId'
  Lude.Text ->
  -- | 'windowTaskId'
  Lude.Text ->
  DeregisterTaskFromMaintenanceWindow
mkDeregisterTaskFromMaintenanceWindow pWindowId_ pWindowTaskId_ =
  DeregisterTaskFromMaintenanceWindow'
    { windowId = pWindowId_,
      windowTaskId = pWindowTaskId_
    }

-- | The ID of the maintenance window the task should be removed from.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derWindowId :: Lens.Lens' DeregisterTaskFromMaintenanceWindow Lude.Text
derWindowId = Lens.lens (windowId :: DeregisterTaskFromMaintenanceWindow -> Lude.Text) (\s a -> s {windowId = a} :: DeregisterTaskFromMaintenanceWindow)
{-# DEPRECATED derWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

-- | The ID of the task to remove from the maintenance window.
--
-- /Note:/ Consider using 'windowTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derWindowTaskId :: Lens.Lens' DeregisterTaskFromMaintenanceWindow Lude.Text
derWindowTaskId = Lens.lens (windowTaskId :: DeregisterTaskFromMaintenanceWindow -> Lude.Text) (\s a -> s {windowTaskId = a} :: DeregisterTaskFromMaintenanceWindow)
{-# DEPRECATED derWindowTaskId "Use generic-lens or generic-optics with 'windowTaskId' instead." #-}

instance Lude.AWSRequest DeregisterTaskFromMaintenanceWindow where
  type
    Rs DeregisterTaskFromMaintenanceWindow =
      DeregisterTaskFromMaintenanceWindowResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeregisterTaskFromMaintenanceWindowResponse'
            Lude.<$> (x Lude..?> "WindowTaskId")
            Lude.<*> (x Lude..?> "WindowId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeregisterTaskFromMaintenanceWindow where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonSSM.DeregisterTaskFromMaintenanceWindow" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeregisterTaskFromMaintenanceWindow where
  toJSON DeregisterTaskFromMaintenanceWindow' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("WindowId" Lude..= windowId),
            Lude.Just ("WindowTaskId" Lude..= windowTaskId)
          ]
      )

instance Lude.ToPath DeregisterTaskFromMaintenanceWindow where
  toPath = Lude.const "/"

instance Lude.ToQuery DeregisterTaskFromMaintenanceWindow where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeregisterTaskFromMaintenanceWindowResponse' smart constructor.
data DeregisterTaskFromMaintenanceWindowResponse = DeregisterTaskFromMaintenanceWindowResponse'
  { windowTaskId ::
      Lude.Maybe
        Lude.Text,
    windowId ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterTaskFromMaintenanceWindowResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'windowId' - The ID of the maintenance window the task was removed from.
-- * 'windowTaskId' - The ID of the task removed from the maintenance window.
mkDeregisterTaskFromMaintenanceWindowResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeregisterTaskFromMaintenanceWindowResponse
mkDeregisterTaskFromMaintenanceWindowResponse pResponseStatus_ =
  DeregisterTaskFromMaintenanceWindowResponse'
    { windowTaskId =
        Lude.Nothing,
      windowId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the task removed from the maintenance window.
--
-- /Note:/ Consider using 'windowTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsWindowTaskId :: Lens.Lens' DeregisterTaskFromMaintenanceWindowResponse (Lude.Maybe Lude.Text)
derrsWindowTaskId = Lens.lens (windowTaskId :: DeregisterTaskFromMaintenanceWindowResponse -> Lude.Maybe Lude.Text) (\s a -> s {windowTaskId = a} :: DeregisterTaskFromMaintenanceWindowResponse)
{-# DEPRECATED derrsWindowTaskId "Use generic-lens or generic-optics with 'windowTaskId' instead." #-}

-- | The ID of the maintenance window the task was removed from.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsWindowId :: Lens.Lens' DeregisterTaskFromMaintenanceWindowResponse (Lude.Maybe Lude.Text)
derrsWindowId = Lens.lens (windowId :: DeregisterTaskFromMaintenanceWindowResponse -> Lude.Maybe Lude.Text) (\s a -> s {windowId = a} :: DeregisterTaskFromMaintenanceWindowResponse)
{-# DEPRECATED derrsWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrsResponseStatus :: Lens.Lens' DeregisterTaskFromMaintenanceWindowResponse Lude.Int
derrsResponseStatus = Lens.lens (responseStatus :: DeregisterTaskFromMaintenanceWindowResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeregisterTaskFromMaintenanceWindowResponse)
{-# DEPRECATED derrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
