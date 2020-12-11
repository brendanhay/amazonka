{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteDisk
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified block storage disk. The disk must be in the @available@ state (not attached to a Lightsail instance).
--
-- The @delete disk@ operation supports tag-based access control via resource tags applied to the resource identified by @disk name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.DeleteDisk
  ( -- * Creating a request
    DeleteDisk (..),
    mkDeleteDisk,

    -- ** Request lenses
    dForceDeleteAddOns,
    dDiskName,

    -- * Destructuring the response
    DeleteDiskResponse (..),
    mkDeleteDiskResponse,

    -- ** Response lenses
    drsOperations,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteDisk' smart constructor.
data DeleteDisk = DeleteDisk'
  { forceDeleteAddOns ::
      Lude.Maybe Lude.Bool,
    diskName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDisk' with the minimum fields required to make a request.
--
-- * 'diskName' - The unique name of the disk you want to delete (e.g., @my-disk@ ).
-- * 'forceDeleteAddOns' - A Boolean value to indicate whether to delete the enabled add-ons for the disk.
mkDeleteDisk ::
  -- | 'diskName'
  Lude.Text ->
  DeleteDisk
mkDeleteDisk pDiskName_ =
  DeleteDisk'
    { forceDeleteAddOns = Lude.Nothing,
      diskName = pDiskName_
    }

-- | A Boolean value to indicate whether to delete the enabled add-ons for the disk.
--
-- /Note:/ Consider using 'forceDeleteAddOns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dForceDeleteAddOns :: Lens.Lens' DeleteDisk (Lude.Maybe Lude.Bool)
dForceDeleteAddOns = Lens.lens (forceDeleteAddOns :: DeleteDisk -> Lude.Maybe Lude.Bool) (\s a -> s {forceDeleteAddOns = a} :: DeleteDisk)
{-# DEPRECATED dForceDeleteAddOns "Use generic-lens or generic-optics with 'forceDeleteAddOns' instead." #-}

-- | The unique name of the disk you want to delete (e.g., @my-disk@ ).
--
-- /Note:/ Consider using 'diskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDiskName :: Lens.Lens' DeleteDisk Lude.Text
dDiskName = Lens.lens (diskName :: DeleteDisk -> Lude.Text) (\s a -> s {diskName = a} :: DeleteDisk)
{-# DEPRECATED dDiskName "Use generic-lens or generic-optics with 'diskName' instead." #-}

instance Lude.AWSRequest DeleteDisk where
  type Rs DeleteDisk = DeleteDiskResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteDiskResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteDisk where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.DeleteDisk" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteDisk where
  toJSON DeleteDisk' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("forceDeleteAddOns" Lude..=) Lude.<$> forceDeleteAddOns,
            Lude.Just ("diskName" Lude..= diskName)
          ]
      )

instance Lude.ToPath DeleteDisk where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDisk where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteDiskResponse' smart constructor.
data DeleteDiskResponse = DeleteDiskResponse'
  { operations ::
      Lude.Maybe [Operation],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDiskResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkDeleteDiskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDiskResponse
mkDeleteDiskResponse pResponseStatus_ =
  DeleteDiskResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsOperations :: Lens.Lens' DeleteDiskResponse (Lude.Maybe [Operation])
drsOperations = Lens.lens (operations :: DeleteDiskResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: DeleteDiskResponse)
{-# DEPRECATED drsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteDiskResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DeleteDiskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDiskResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
