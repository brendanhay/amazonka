{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DeleteMaintenanceWindow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a maintenance window.
module Network.AWS.SSM.DeleteMaintenanceWindow
  ( -- * Creating a request
    DeleteMaintenanceWindow (..),
    mkDeleteMaintenanceWindow,

    -- ** Request lenses
    dmwWindowId,

    -- * Destructuring the response
    DeleteMaintenanceWindowResponse (..),
    mkDeleteMaintenanceWindowResponse,

    -- ** Response lenses
    dmwfrsWindowId,
    dmwfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkDeleteMaintenanceWindow' smart constructor.
newtype DeleteMaintenanceWindow = DeleteMaintenanceWindow'
  { -- | The ID of the maintenance window to delete.
    windowId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMaintenanceWindow' with the minimum fields required to make a request.
--
-- * 'windowId' - The ID of the maintenance window to delete.
mkDeleteMaintenanceWindow ::
  -- | 'windowId'
  Lude.Text ->
  DeleteMaintenanceWindow
mkDeleteMaintenanceWindow pWindowId_ =
  DeleteMaintenanceWindow' {windowId = pWindowId_}

-- | The ID of the maintenance window to delete.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwWindowId :: Lens.Lens' DeleteMaintenanceWindow Lude.Text
dmwWindowId = Lens.lens (windowId :: DeleteMaintenanceWindow -> Lude.Text) (\s a -> s {windowId = a} :: DeleteMaintenanceWindow)
{-# DEPRECATED dmwWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

instance Lude.AWSRequest DeleteMaintenanceWindow where
  type Rs DeleteMaintenanceWindow = DeleteMaintenanceWindowResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteMaintenanceWindowResponse'
            Lude.<$> (x Lude..?> "WindowId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteMaintenanceWindow where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.DeleteMaintenanceWindow" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteMaintenanceWindow where
  toJSON DeleteMaintenanceWindow' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("WindowId" Lude..= windowId)])

instance Lude.ToPath DeleteMaintenanceWindow where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteMaintenanceWindow where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteMaintenanceWindowResponse' smart constructor.
data DeleteMaintenanceWindowResponse = DeleteMaintenanceWindowResponse'
  { -- | The ID of the deleted maintenance window.
    windowId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteMaintenanceWindowResponse' with the minimum fields required to make a request.
--
-- * 'windowId' - The ID of the deleted maintenance window.
-- * 'responseStatus' - The response status code.
mkDeleteMaintenanceWindowResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteMaintenanceWindowResponse
mkDeleteMaintenanceWindowResponse pResponseStatus_ =
  DeleteMaintenanceWindowResponse'
    { windowId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the deleted maintenance window.
--
-- /Note:/ Consider using 'windowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwfrsWindowId :: Lens.Lens' DeleteMaintenanceWindowResponse (Lude.Maybe Lude.Text)
dmwfrsWindowId = Lens.lens (windowId :: DeleteMaintenanceWindowResponse -> Lude.Maybe Lude.Text) (\s a -> s {windowId = a} :: DeleteMaintenanceWindowResponse)
{-# DEPRECATED dmwfrsWindowId "Use generic-lens or generic-optics with 'windowId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwfrsResponseStatus :: Lens.Lens' DeleteMaintenanceWindowResponse Lude.Int
dmwfrsResponseStatus = Lens.lens (responseStatus :: DeleteMaintenanceWindowResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteMaintenanceWindowResponse)
{-# DEPRECATED dmwfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
