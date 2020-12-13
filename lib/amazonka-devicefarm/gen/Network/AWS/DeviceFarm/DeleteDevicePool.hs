{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.DeleteDevicePool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a device pool given the pool ARN. Does not allow deletion of curated pools owned by the system.
module Network.AWS.DeviceFarm.DeleteDevicePool
  ( -- * Creating a request
    DeleteDevicePool (..),
    mkDeleteDevicePool,

    -- ** Request lenses
    ddpArn,

    -- * Destructuring the response
    DeleteDevicePoolResponse (..),
    mkDeleteDevicePoolResponse,

    -- ** Response lenses
    ddprsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents a request to the delete device pool operation.
--
-- /See:/ 'mkDeleteDevicePool' smart constructor.
newtype DeleteDevicePool = DeleteDevicePool'
  { -- | Represents the Amazon Resource Name (ARN) of the Device Farm device pool to delete.
    arn :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDevicePool' with the minimum fields required to make a request.
--
-- * 'arn' - Represents the Amazon Resource Name (ARN) of the Device Farm device pool to delete.
mkDeleteDevicePool ::
  -- | 'arn'
  Lude.Text ->
  DeleteDevicePool
mkDeleteDevicePool pArn_ = DeleteDevicePool' {arn = pArn_}

-- | Represents the Amazon Resource Name (ARN) of the Device Farm device pool to delete.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddpArn :: Lens.Lens' DeleteDevicePool Lude.Text
ddpArn = Lens.lens (arn :: DeleteDevicePool -> Lude.Text) (\s a -> s {arn = a} :: DeleteDevicePool)
{-# DEPRECATED ddpArn "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest DeleteDevicePool where
  type Rs DeleteDevicePool = DeleteDevicePoolResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteDevicePoolResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteDevicePool where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.DeleteDevicePool" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteDevicePool where
  toJSON DeleteDevicePool' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("arn" Lude..= arn)])

instance Lude.ToPath DeleteDevicePool where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDevicePool where
  toQuery = Lude.const Lude.mempty

-- | Represents the result of a delete device pool request.
--
-- /See:/ 'mkDeleteDevicePoolResponse' smart constructor.
newtype DeleteDevicePoolResponse = DeleteDevicePoolResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDevicePoolResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteDevicePoolResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDevicePoolResponse
mkDeleteDevicePoolResponse pResponseStatus_ =
  DeleteDevicePoolResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddprsResponseStatus :: Lens.Lens' DeleteDevicePoolResponse Lude.Int
ddprsResponseStatus = Lens.lens (responseStatus :: DeleteDevicePoolResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDevicePoolResponse)
{-# DEPRECATED ddprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
