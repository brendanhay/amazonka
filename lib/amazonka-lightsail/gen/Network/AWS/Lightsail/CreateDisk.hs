{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateDisk
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a block storage disk that can be attached to an Amazon Lightsail instance in the same Availability Zone (e.g., @us-east-2a@ ).
--
-- The @create disk@ operation supports tag-based access control via request tags. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.CreateDisk
  ( -- * Creating a request
    CreateDisk (..),
    mkCreateDisk,

    -- ** Request lenses
    cdfAddOns,
    cdfAvailabilityZone,
    cdfSizeInGb,
    cdfDiskName,
    cdfTags,

    -- * Destructuring the response
    CreateDiskResponse (..),
    mkCreateDiskResponse,

    -- ** Response lenses
    cdfrsOperations,
    cdfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateDisk' smart constructor.
data CreateDisk = CreateDisk'
  { -- | An array of objects that represent the add-ons to enable for the new disk.
    addOns :: Lude.Maybe [AddOnRequest],
    -- | The Availability Zone where you want to create the disk (e.g., @us-east-2a@ ). Use the same Availability Zone as the Lightsail instance to which you want to attach the disk.
    --
    -- Use the @get regions@ operation to list the Availability Zones where Lightsail is currently available.
    availabilityZone :: Lude.Text,
    -- | The size of the disk in GB (e.g., @32@ ).
    sizeInGb :: Lude.Int,
    -- | The unique Lightsail disk name (e.g., @my-disk@ ).
    diskName :: Lude.Text,
    -- | The tag keys and optional values to add to the resource during create.
    --
    -- Use the @TagResource@ action to tag a resource after it's created.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDisk' with the minimum fields required to make a request.
--
-- * 'addOns' - An array of objects that represent the add-ons to enable for the new disk.
-- * 'availabilityZone' - The Availability Zone where you want to create the disk (e.g., @us-east-2a@ ). Use the same Availability Zone as the Lightsail instance to which you want to attach the disk.
--
-- Use the @get regions@ operation to list the Availability Zones where Lightsail is currently available.
-- * 'sizeInGb' - The size of the disk in GB (e.g., @32@ ).
-- * 'diskName' - The unique Lightsail disk name (e.g., @my-disk@ ).
-- * 'tags' - The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
mkCreateDisk ::
  -- | 'availabilityZone'
  Lude.Text ->
  -- | 'sizeInGb'
  Lude.Int ->
  -- | 'diskName'
  Lude.Text ->
  CreateDisk
mkCreateDisk pAvailabilityZone_ pSizeInGb_ pDiskName_ =
  CreateDisk'
    { addOns = Lude.Nothing,
      availabilityZone = pAvailabilityZone_,
      sizeInGb = pSizeInGb_,
      diskName = pDiskName_,
      tags = Lude.Nothing
    }

-- | An array of objects that represent the add-ons to enable for the new disk.
--
-- /Note:/ Consider using 'addOns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfAddOns :: Lens.Lens' CreateDisk (Lude.Maybe [AddOnRequest])
cdfAddOns = Lens.lens (addOns :: CreateDisk -> Lude.Maybe [AddOnRequest]) (\s a -> s {addOns = a} :: CreateDisk)
{-# DEPRECATED cdfAddOns "Use generic-lens or generic-optics with 'addOns' instead." #-}

-- | The Availability Zone where you want to create the disk (e.g., @us-east-2a@ ). Use the same Availability Zone as the Lightsail instance to which you want to attach the disk.
--
-- Use the @get regions@ operation to list the Availability Zones where Lightsail is currently available.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfAvailabilityZone :: Lens.Lens' CreateDisk Lude.Text
cdfAvailabilityZone = Lens.lens (availabilityZone :: CreateDisk -> Lude.Text) (\s a -> s {availabilityZone = a} :: CreateDisk)
{-# DEPRECATED cdfAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The size of the disk in GB (e.g., @32@ ).
--
-- /Note:/ Consider using 'sizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfSizeInGb :: Lens.Lens' CreateDisk Lude.Int
cdfSizeInGb = Lens.lens (sizeInGb :: CreateDisk -> Lude.Int) (\s a -> s {sizeInGb = a} :: CreateDisk)
{-# DEPRECATED cdfSizeInGb "Use generic-lens or generic-optics with 'sizeInGb' instead." #-}

-- | The unique Lightsail disk name (e.g., @my-disk@ ).
--
-- /Note:/ Consider using 'diskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfDiskName :: Lens.Lens' CreateDisk Lude.Text
cdfDiskName = Lens.lens (diskName :: CreateDisk -> Lude.Text) (\s a -> s {diskName = a} :: CreateDisk)
{-# DEPRECATED cdfDiskName "Use generic-lens or generic-optics with 'diskName' instead." #-}

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfTags :: Lens.Lens' CreateDisk (Lude.Maybe [Tag])
cdfTags = Lens.lens (tags :: CreateDisk -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateDisk)
{-# DEPRECATED cdfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateDisk where
  type Rs CreateDisk = CreateDiskResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateDiskResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDisk where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.CreateDisk" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateDisk where
  toJSON CreateDisk' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("addOns" Lude..=) Lude.<$> addOns,
            Lude.Just ("availabilityZone" Lude..= availabilityZone),
            Lude.Just ("sizeInGb" Lude..= sizeInGb),
            Lude.Just ("diskName" Lude..= diskName),
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateDisk where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDisk where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateDiskResponse' smart constructor.
data CreateDiskResponse = CreateDiskResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Lude.Maybe [Operation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDiskResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkCreateDiskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDiskResponse
mkCreateDiskResponse pResponseStatus_ =
  CreateDiskResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfrsOperations :: Lens.Lens' CreateDiskResponse (Lude.Maybe [Operation])
cdfrsOperations = Lens.lens (operations :: CreateDiskResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: CreateDiskResponse)
{-# DEPRECATED cdfrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfrsResponseStatus :: Lens.Lens' CreateDiskResponse Lude.Int
cdfrsResponseStatus = Lens.lens (responseStatus :: CreateDiskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDiskResponse)
{-# DEPRECATED cdfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
