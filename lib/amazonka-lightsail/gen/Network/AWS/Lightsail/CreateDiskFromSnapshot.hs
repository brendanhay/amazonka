{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateDiskFromSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a block storage disk from a manual or automatic snapshot of a disk. The resulting disk can be attached to an Amazon Lightsail instance in the same Availability Zone (e.g., @us-east-2a@ ).
--
-- The @create disk from snapshot@ operation supports tag-based access control via request tags and resource tags applied to the resource identified by @disk snapshot name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.CreateDiskFromSnapshot
  ( -- * Creating a request
    CreateDiskFromSnapshot (..),
    mkCreateDiskFromSnapshot,

    -- ** Request lenses
    cdfsUseLatestRestorableAutoSnapshot,
    cdfsSourceDiskName,
    cdfsAddOns,
    cdfsDiskSnapshotName,
    cdfsRestoreDate,
    cdfsTags,
    cdfsDiskName,
    cdfsAvailabilityZone,
    cdfsSizeInGb,

    -- * Destructuring the response
    CreateDiskFromSnapshotResponse (..),
    mkCreateDiskFromSnapshotResponse,

    -- ** Response lenses
    cdfsrsOperations,
    cdfsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateDiskFromSnapshot' smart constructor.
data CreateDiskFromSnapshot = CreateDiskFromSnapshot'
  { useLatestRestorableAutoSnapshot ::
      Lude.Maybe Lude.Bool,
    sourceDiskName :: Lude.Maybe Lude.Text,
    addOns :: Lude.Maybe [AddOnRequest],
    diskSnapshotName :: Lude.Maybe Lude.Text,
    restoreDate :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    diskName :: Lude.Text,
    availabilityZone :: Lude.Text,
    sizeInGb :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDiskFromSnapshot' with the minimum fields required to make a request.
--
-- * 'addOns' - An array of objects that represent the add-ons to enable for the new disk.
-- * 'availabilityZone' - The Availability Zone where you want to create the disk (e.g., @us-east-2a@ ). Choose the same Availability Zone as the Lightsail instance where you want to create the disk.
--
-- Use the GetRegions operation to list the Availability Zones where Lightsail is currently available.
-- * 'diskName' - The unique Lightsail disk name (e.g., @my-disk@ ).
-- * 'diskSnapshotName' - The name of the disk snapshot (e.g., @my-snapshot@ ) from which to create the new storage disk.
--
-- Constraint:
--
--     * This parameter cannot be defined together with the @source disk name@ parameter. The @disk snapshot name@ and @source disk name@ parameters are mutually exclusive.
--
--
-- * 'restoreDate' - The date of the automatic snapshot to use for the new disk. Use the @get auto snapshots@ operation to identify the dates of the available automatic snapshots.
--
-- Constraints:
--
--     * Must be specified in @YYYY-MM-DD@ format.
--
--
--     * This parameter cannot be defined together with the @use latest restorable auto snapshot@ parameter. The @restore date@ and @use latest restorable auto snapshot@ parameters are mutually exclusive.
--
--
--     * Define this parameter only when creating a new disk from an automatic snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
--
--
-- * 'sizeInGb' - The size of the disk in GB (e.g., @32@ ).
-- * 'sourceDiskName' - The name of the source disk from which the source automatic snapshot was created.
--
-- Constraints:
--
--     * This parameter cannot be defined together with the @disk snapshot name@ parameter. The @source disk name@ and @disk snapshot name@ parameters are mutually exclusive.
--
--
--     * Define this parameter only when creating a new disk from an automatic snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
--
--
-- * 'tags' - The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
-- * 'useLatestRestorableAutoSnapshot' - A Boolean value to indicate whether to use the latest available automatic snapshot.
--
-- Constraints:
--
--     * This parameter cannot be defined together with the @restore date@ parameter. The @use latest restorable auto snapshot@ and @restore date@ parameters are mutually exclusive.
--
--
--     * Define this parameter only when creating a new disk from an automatic snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
mkCreateDiskFromSnapshot ::
  -- | 'diskName'
  Lude.Text ->
  -- | 'availabilityZone'
  Lude.Text ->
  -- | 'sizeInGb'
  Lude.Int ->
  CreateDiskFromSnapshot
mkCreateDiskFromSnapshot pDiskName_ pAvailabilityZone_ pSizeInGb_ =
  CreateDiskFromSnapshot'
    { useLatestRestorableAutoSnapshot =
        Lude.Nothing,
      sourceDiskName = Lude.Nothing,
      addOns = Lude.Nothing,
      diskSnapshotName = Lude.Nothing,
      restoreDate = Lude.Nothing,
      tags = Lude.Nothing,
      diskName = pDiskName_,
      availabilityZone = pAvailabilityZone_,
      sizeInGb = pSizeInGb_
    }

-- | A Boolean value to indicate whether to use the latest available automatic snapshot.
--
-- Constraints:
--
--     * This parameter cannot be defined together with the @restore date@ parameter. The @use latest restorable auto snapshot@ and @restore date@ parameters are mutually exclusive.
--
--
--     * Define this parameter only when creating a new disk from an automatic snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
--
--
--
-- /Note:/ Consider using 'useLatestRestorableAutoSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfsUseLatestRestorableAutoSnapshot :: Lens.Lens' CreateDiskFromSnapshot (Lude.Maybe Lude.Bool)
cdfsUseLatestRestorableAutoSnapshot = Lens.lens (useLatestRestorableAutoSnapshot :: CreateDiskFromSnapshot -> Lude.Maybe Lude.Bool) (\s a -> s {useLatestRestorableAutoSnapshot = a} :: CreateDiskFromSnapshot)
{-# DEPRECATED cdfsUseLatestRestorableAutoSnapshot "Use generic-lens or generic-optics with 'useLatestRestorableAutoSnapshot' instead." #-}

-- | The name of the source disk from which the source automatic snapshot was created.
--
-- Constraints:
--
--     * This parameter cannot be defined together with the @disk snapshot name@ parameter. The @source disk name@ and @disk snapshot name@ parameters are mutually exclusive.
--
--
--     * Define this parameter only when creating a new disk from an automatic snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
--
--
--
-- /Note:/ Consider using 'sourceDiskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfsSourceDiskName :: Lens.Lens' CreateDiskFromSnapshot (Lude.Maybe Lude.Text)
cdfsSourceDiskName = Lens.lens (sourceDiskName :: CreateDiskFromSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {sourceDiskName = a} :: CreateDiskFromSnapshot)
{-# DEPRECATED cdfsSourceDiskName "Use generic-lens or generic-optics with 'sourceDiskName' instead." #-}

-- | An array of objects that represent the add-ons to enable for the new disk.
--
-- /Note:/ Consider using 'addOns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfsAddOns :: Lens.Lens' CreateDiskFromSnapshot (Lude.Maybe [AddOnRequest])
cdfsAddOns = Lens.lens (addOns :: CreateDiskFromSnapshot -> Lude.Maybe [AddOnRequest]) (\s a -> s {addOns = a} :: CreateDiskFromSnapshot)
{-# DEPRECATED cdfsAddOns "Use generic-lens or generic-optics with 'addOns' instead." #-}

-- | The name of the disk snapshot (e.g., @my-snapshot@ ) from which to create the new storage disk.
--
-- Constraint:
--
--     * This parameter cannot be defined together with the @source disk name@ parameter. The @disk snapshot name@ and @source disk name@ parameters are mutually exclusive.
--
--
--
-- /Note:/ Consider using 'diskSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfsDiskSnapshotName :: Lens.Lens' CreateDiskFromSnapshot (Lude.Maybe Lude.Text)
cdfsDiskSnapshotName = Lens.lens (diskSnapshotName :: CreateDiskFromSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {diskSnapshotName = a} :: CreateDiskFromSnapshot)
{-# DEPRECATED cdfsDiskSnapshotName "Use generic-lens or generic-optics with 'diskSnapshotName' instead." #-}

-- | The date of the automatic snapshot to use for the new disk. Use the @get auto snapshots@ operation to identify the dates of the available automatic snapshots.
--
-- Constraints:
--
--     * Must be specified in @YYYY-MM-DD@ format.
--
--
--     * This parameter cannot be defined together with the @use latest restorable auto snapshot@ parameter. The @restore date@ and @use latest restorable auto snapshot@ parameters are mutually exclusive.
--
--
--     * Define this parameter only when creating a new disk from an automatic snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
--
--
--
-- /Note:/ Consider using 'restoreDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfsRestoreDate :: Lens.Lens' CreateDiskFromSnapshot (Lude.Maybe Lude.Text)
cdfsRestoreDate = Lens.lens (restoreDate :: CreateDiskFromSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {restoreDate = a} :: CreateDiskFromSnapshot)
{-# DEPRECATED cdfsRestoreDate "Use generic-lens or generic-optics with 'restoreDate' instead." #-}

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfsTags :: Lens.Lens' CreateDiskFromSnapshot (Lude.Maybe [Tag])
cdfsTags = Lens.lens (tags :: CreateDiskFromSnapshot -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateDiskFromSnapshot)
{-# DEPRECATED cdfsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The unique Lightsail disk name (e.g., @my-disk@ ).
--
-- /Note:/ Consider using 'diskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfsDiskName :: Lens.Lens' CreateDiskFromSnapshot Lude.Text
cdfsDiskName = Lens.lens (diskName :: CreateDiskFromSnapshot -> Lude.Text) (\s a -> s {diskName = a} :: CreateDiskFromSnapshot)
{-# DEPRECATED cdfsDiskName "Use generic-lens or generic-optics with 'diskName' instead." #-}

-- | The Availability Zone where you want to create the disk (e.g., @us-east-2a@ ). Choose the same Availability Zone as the Lightsail instance where you want to create the disk.
--
-- Use the GetRegions operation to list the Availability Zones where Lightsail is currently available.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfsAvailabilityZone :: Lens.Lens' CreateDiskFromSnapshot Lude.Text
cdfsAvailabilityZone = Lens.lens (availabilityZone :: CreateDiskFromSnapshot -> Lude.Text) (\s a -> s {availabilityZone = a} :: CreateDiskFromSnapshot)
{-# DEPRECATED cdfsAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The size of the disk in GB (e.g., @32@ ).
--
-- /Note:/ Consider using 'sizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfsSizeInGb :: Lens.Lens' CreateDiskFromSnapshot Lude.Int
cdfsSizeInGb = Lens.lens (sizeInGb :: CreateDiskFromSnapshot -> Lude.Int) (\s a -> s {sizeInGb = a} :: CreateDiskFromSnapshot)
{-# DEPRECATED cdfsSizeInGb "Use generic-lens or generic-optics with 'sizeInGb' instead." #-}

instance Lude.AWSRequest CreateDiskFromSnapshot where
  type Rs CreateDiskFromSnapshot = CreateDiskFromSnapshotResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateDiskFromSnapshotResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDiskFromSnapshot where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.CreateDiskFromSnapshot" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateDiskFromSnapshot where
  toJSON CreateDiskFromSnapshot' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("useLatestRestorableAutoSnapshot" Lude..=)
              Lude.<$> useLatestRestorableAutoSnapshot,
            ("sourceDiskName" Lude..=) Lude.<$> sourceDiskName,
            ("addOns" Lude..=) Lude.<$> addOns,
            ("diskSnapshotName" Lude..=) Lude.<$> diskSnapshotName,
            ("restoreDate" Lude..=) Lude.<$> restoreDate,
            ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("diskName" Lude..= diskName),
            Lude.Just ("availabilityZone" Lude..= availabilityZone),
            Lude.Just ("sizeInGb" Lude..= sizeInGb)
          ]
      )

instance Lude.ToPath CreateDiskFromSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDiskFromSnapshot where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateDiskFromSnapshotResponse' smart constructor.
data CreateDiskFromSnapshotResponse = CreateDiskFromSnapshotResponse'
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

-- | Creates a value of 'CreateDiskFromSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkCreateDiskFromSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDiskFromSnapshotResponse
mkCreateDiskFromSnapshotResponse pResponseStatus_ =
  CreateDiskFromSnapshotResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfsrsOperations :: Lens.Lens' CreateDiskFromSnapshotResponse (Lude.Maybe [Operation])
cdfsrsOperations = Lens.lens (operations :: CreateDiskFromSnapshotResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: CreateDiskFromSnapshotResponse)
{-# DEPRECATED cdfsrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdfsrsResponseStatus :: Lens.Lens' CreateDiskFromSnapshotResponse Lude.Int
cdfsrsResponseStatus = Lens.lens (responseStatus :: CreateDiskFromSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDiskFromSnapshotResponse)
{-# DEPRECATED cdfsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
