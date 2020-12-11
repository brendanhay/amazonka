{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DetachDisk
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a stopped block storage disk from a Lightsail instance. Make sure to unmount any file systems on the device within your operating system before stopping the instance and detaching the disk.
--
-- The @detach disk@ operation supports tag-based access control via resource tags applied to the resource identified by @disk name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.DetachDisk
  ( -- * Creating a request
    DetachDisk (..),
    mkDetachDisk,

    -- ** Request lenses
    ddDiskName,

    -- * Destructuring the response
    DetachDiskResponse (..),
    mkDetachDiskResponse,

    -- ** Response lenses
    ddrsOperations,
    ddrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetachDisk' smart constructor.
newtype DetachDisk = DetachDisk' {diskName :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachDisk' with the minimum fields required to make a request.
--
-- * 'diskName' - The unique name of the disk you want to detach from your instance (e.g., @my-disk@ ).
mkDetachDisk ::
  -- | 'diskName'
  Lude.Text ->
  DetachDisk
mkDetachDisk pDiskName_ = DetachDisk' {diskName = pDiskName_}

-- | The unique name of the disk you want to detach from your instance (e.g., @my-disk@ ).
--
-- /Note:/ Consider using 'diskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDiskName :: Lens.Lens' DetachDisk Lude.Text
ddDiskName = Lens.lens (diskName :: DetachDisk -> Lude.Text) (\s a -> s {diskName = a} :: DetachDisk)
{-# DEPRECATED ddDiskName "Use generic-lens or generic-optics with 'diskName' instead." #-}

instance Lude.AWSRequest DetachDisk where
  type Rs DetachDisk = DetachDiskResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          DetachDiskResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DetachDisk where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.DetachDisk" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DetachDisk where
  toJSON DetachDisk' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("diskName" Lude..= diskName)])

instance Lude.ToPath DetachDisk where
  toPath = Lude.const "/"

instance Lude.ToQuery DetachDisk where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDetachDiskResponse' smart constructor.
data DetachDiskResponse = DetachDiskResponse'
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

-- | Creates a value of 'DetachDiskResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkDetachDiskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DetachDiskResponse
mkDetachDiskResponse pResponseStatus_ =
  DetachDiskResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsOperations :: Lens.Lens' DetachDiskResponse (Lude.Maybe [Operation])
ddrsOperations = Lens.lens (operations :: DetachDiskResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: DetachDiskResponse)
{-# DEPRECATED ddrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrsResponseStatus :: Lens.Lens' DetachDiskResponse Lude.Int
ddrsResponseStatus = Lens.lens (responseStatus :: DetachDiskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DetachDiskResponse)
{-# DEPRECATED ddrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
