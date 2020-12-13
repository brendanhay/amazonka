{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.AttachDisk
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a block storage disk to a running or stopped Lightsail instance and exposes it to the instance with the specified disk name.
--
-- The @attach disk@ operation supports tag-based access control via resource tags applied to the resource identified by @disk name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.AttachDisk
  ( -- * Creating a request
    AttachDisk (..),
    mkAttachDisk,

    -- ** Request lenses
    adDiskPath,
    adDiskName,
    adInstanceName,

    -- * Destructuring the response
    AttachDiskResponse (..),
    mkAttachDiskResponse,

    -- ** Response lenses
    adrsOperations,
    adrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAttachDisk' smart constructor.
data AttachDisk = AttachDisk'
  { -- | The disk path to expose to the instance (e.g., @/dev/xvdf@ ).
    diskPath :: Lude.Text,
    -- | The unique Lightsail disk name (e.g., @my-disk@ ).
    diskName :: Lude.Text,
    -- | The name of the Lightsail instance where you want to utilize the storage disk.
    instanceName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachDisk' with the minimum fields required to make a request.
--
-- * 'diskPath' - The disk path to expose to the instance (e.g., @/dev/xvdf@ ).
-- * 'diskName' - The unique Lightsail disk name (e.g., @my-disk@ ).
-- * 'instanceName' - The name of the Lightsail instance where you want to utilize the storage disk.
mkAttachDisk ::
  -- | 'diskPath'
  Lude.Text ->
  -- | 'diskName'
  Lude.Text ->
  -- | 'instanceName'
  Lude.Text ->
  AttachDisk
mkAttachDisk pDiskPath_ pDiskName_ pInstanceName_ =
  AttachDisk'
    { diskPath = pDiskPath_,
      diskName = pDiskName_,
      instanceName = pInstanceName_
    }

-- | The disk path to expose to the instance (e.g., @/dev/xvdf@ ).
--
-- /Note:/ Consider using 'diskPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adDiskPath :: Lens.Lens' AttachDisk Lude.Text
adDiskPath = Lens.lens (diskPath :: AttachDisk -> Lude.Text) (\s a -> s {diskPath = a} :: AttachDisk)
{-# DEPRECATED adDiskPath "Use generic-lens or generic-optics with 'diskPath' instead." #-}

-- | The unique Lightsail disk name (e.g., @my-disk@ ).
--
-- /Note:/ Consider using 'diskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adDiskName :: Lens.Lens' AttachDisk Lude.Text
adDiskName = Lens.lens (diskName :: AttachDisk -> Lude.Text) (\s a -> s {diskName = a} :: AttachDisk)
{-# DEPRECATED adDiskName "Use generic-lens or generic-optics with 'diskName' instead." #-}

-- | The name of the Lightsail instance where you want to utilize the storage disk.
--
-- /Note:/ Consider using 'instanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adInstanceName :: Lens.Lens' AttachDisk Lude.Text
adInstanceName = Lens.lens (instanceName :: AttachDisk -> Lude.Text) (\s a -> s {instanceName = a} :: AttachDisk)
{-# DEPRECATED adInstanceName "Use generic-lens or generic-optics with 'instanceName' instead." #-}

instance Lude.AWSRequest AttachDisk where
  type Rs AttachDisk = AttachDiskResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          AttachDiskResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AttachDisk where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.AttachDisk" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AttachDisk where
  toJSON AttachDisk' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("diskPath" Lude..= diskPath),
            Lude.Just ("diskName" Lude..= diskName),
            Lude.Just ("instanceName" Lude..= instanceName)
          ]
      )

instance Lude.ToPath AttachDisk where
  toPath = Lude.const "/"

instance Lude.ToQuery AttachDisk where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAttachDiskResponse' smart constructor.
data AttachDiskResponse = AttachDiskResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Lude.Maybe [Operation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachDiskResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkAttachDiskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AttachDiskResponse
mkAttachDiskResponse pResponseStatus_ =
  AttachDiskResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adrsOperations :: Lens.Lens' AttachDiskResponse (Lude.Maybe [Operation])
adrsOperations = Lens.lens (operations :: AttachDiskResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: AttachDiskResponse)
{-# DEPRECATED adrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adrsResponseStatus :: Lens.Lens' AttachDiskResponse Lude.Int
adrsResponseStatus = Lens.lens (responseStatus :: AttachDiskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AttachDiskResponse)
{-# DEPRECATED adrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
