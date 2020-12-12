{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Disk
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.Disk
  ( Disk (..),

    -- * Smart constructor
    mkDisk,

    -- * Lenses
    dState,
    dResourceType,
    dArn,
    dPath,
    dCreatedAt,
    dLocation,
    dIops,
    dIsAttached,
    dAddOns,
    dAttachmentState,
    dName,
    dSizeInGb,
    dSupportCode,
    dIsSystemDisk,
    dAttachedTo,
    dGbInUse,
    dTags,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.AddOn
import Network.AWS.Lightsail.Types.DiskState
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.Tag
import qualified Network.AWS.Prelude as Lude

-- | Describes a system disk or a block storage disk.
--
-- /See:/ 'mkDisk' smart constructor.
data Disk = Disk'
  { state :: Lude.Maybe DiskState,
    resourceType :: Lude.Maybe ResourceType,
    arn :: Lude.Maybe Lude.Text,
    path :: Lude.Maybe Lude.Text,
    createdAt :: Lude.Maybe Lude.Timestamp,
    location :: Lude.Maybe ResourceLocation,
    iops :: Lude.Maybe Lude.Int,
    isAttached :: Lude.Maybe Lude.Bool,
    addOns :: Lude.Maybe [AddOn],
    attachmentState :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    sizeInGb :: Lude.Maybe Lude.Int,
    supportCode :: Lude.Maybe Lude.Text,
    isSystemDisk :: Lude.Maybe Lude.Bool,
    attachedTo :: Lude.Maybe Lude.Text,
    gbInUse :: Lude.Maybe Lude.Int,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Disk' with the minimum fields required to make a request.
--
-- * 'addOns' - An array of objects representing the add-ons enabled on the disk.
-- * 'arn' - The Amazon Resource Name (ARN) of the disk.
-- * 'attachedTo' - The resources to which the disk is attached.
-- * 'attachmentState' - (Deprecated) The attachment state of the disk.
-- * 'createdAt' - The date when the disk was created.
-- * 'gbInUse' - (Deprecated) The number of GB in use by the disk.
-- * 'iops' - The input/output operations per second (IOPS) of the disk.
-- * 'isAttached' - A Boolean value indicating whether the disk is attached.
-- * 'isSystemDisk' - A Boolean value indicating whether this disk is a system disk (has an operating system loaded on it).
-- * 'location' - The AWS Region and Availability Zone where the disk is located.
-- * 'name' - The unique name of the disk.
-- * 'path' - The disk path.
-- * 'resourceType' - The Lightsail resource type (e.g., @Disk@ ).
-- * 'sizeInGb' - The size of the disk in GB.
-- * 'state' - Describes the status of the disk.
-- * 'supportCode' - The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
-- * 'tags' - The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
mkDisk ::
  Disk
mkDisk =
  Disk'
    { state = Lude.Nothing,
      resourceType = Lude.Nothing,
      arn = Lude.Nothing,
      path = Lude.Nothing,
      createdAt = Lude.Nothing,
      location = Lude.Nothing,
      iops = Lude.Nothing,
      isAttached = Lude.Nothing,
      addOns = Lude.Nothing,
      attachmentState = Lude.Nothing,
      name = Lude.Nothing,
      sizeInGb = Lude.Nothing,
      supportCode = Lude.Nothing,
      isSystemDisk = Lude.Nothing,
      attachedTo = Lude.Nothing,
      gbInUse = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | Describes the status of the disk.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dState :: Lens.Lens' Disk (Lude.Maybe DiskState)
dState = Lens.lens (state :: Disk -> Lude.Maybe DiskState) (\s a -> s {state = a} :: Disk)
{-# DEPRECATED dState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The Lightsail resource type (e.g., @Disk@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dResourceType :: Lens.Lens' Disk (Lude.Maybe ResourceType)
dResourceType = Lens.lens (resourceType :: Disk -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: Disk)
{-# DEPRECATED dResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The Amazon Resource Name (ARN) of the disk.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dArn :: Lens.Lens' Disk (Lude.Maybe Lude.Text)
dArn = Lens.lens (arn :: Disk -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Disk)
{-# DEPRECATED dArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The disk path.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dPath :: Lens.Lens' Disk (Lude.Maybe Lude.Text)
dPath = Lens.lens (path :: Disk -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: Disk)
{-# DEPRECATED dPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The date when the disk was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCreatedAt :: Lens.Lens' Disk (Lude.Maybe Lude.Timestamp)
dCreatedAt = Lens.lens (createdAt :: Disk -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: Disk)
{-# DEPRECATED dCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The AWS Region and Availability Zone where the disk is located.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLocation :: Lens.Lens' Disk (Lude.Maybe ResourceLocation)
dLocation = Lens.lens (location :: Disk -> Lude.Maybe ResourceLocation) (\s a -> s {location = a} :: Disk)
{-# DEPRECATED dLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The input/output operations per second (IOPS) of the disk.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dIops :: Lens.Lens' Disk (Lude.Maybe Lude.Int)
dIops = Lens.lens (iops :: Disk -> Lude.Maybe Lude.Int) (\s a -> s {iops = a} :: Disk)
{-# DEPRECATED dIops "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | A Boolean value indicating whether the disk is attached.
--
-- /Note:/ Consider using 'isAttached' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dIsAttached :: Lens.Lens' Disk (Lude.Maybe Lude.Bool)
dIsAttached = Lens.lens (isAttached :: Disk -> Lude.Maybe Lude.Bool) (\s a -> s {isAttached = a} :: Disk)
{-# DEPRECATED dIsAttached "Use generic-lens or generic-optics with 'isAttached' instead." #-}

-- | An array of objects representing the add-ons enabled on the disk.
--
-- /Note:/ Consider using 'addOns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAddOns :: Lens.Lens' Disk (Lude.Maybe [AddOn])
dAddOns = Lens.lens (addOns :: Disk -> Lude.Maybe [AddOn]) (\s a -> s {addOns = a} :: Disk)
{-# DEPRECATED dAddOns "Use generic-lens or generic-optics with 'addOns' instead." #-}

-- | (Deprecated) The attachment state of the disk.
--
-- /Note:/ Consider using 'attachmentState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAttachmentState :: Lens.Lens' Disk (Lude.Maybe Lude.Text)
dAttachmentState = Lens.lens (attachmentState :: Disk -> Lude.Maybe Lude.Text) (\s a -> s {attachmentState = a} :: Disk)
{-# DEPRECATED dAttachmentState "Use generic-lens or generic-optics with 'attachmentState' instead." #-}

-- | The unique name of the disk.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dName :: Lens.Lens' Disk (Lude.Maybe Lude.Text)
dName = Lens.lens (name :: Disk -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Disk)
{-# DEPRECATED dName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The size of the disk in GB.
--
-- /Note:/ Consider using 'sizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSizeInGb :: Lens.Lens' Disk (Lude.Maybe Lude.Int)
dSizeInGb = Lens.lens (sizeInGb :: Disk -> Lude.Maybe Lude.Int) (\s a -> s {sizeInGb = a} :: Disk)
{-# DEPRECATED dSizeInGb "Use generic-lens or generic-optics with 'sizeInGb' instead." #-}

-- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
--
-- /Note:/ Consider using 'supportCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSupportCode :: Lens.Lens' Disk (Lude.Maybe Lude.Text)
dSupportCode = Lens.lens (supportCode :: Disk -> Lude.Maybe Lude.Text) (\s a -> s {supportCode = a} :: Disk)
{-# DEPRECATED dSupportCode "Use generic-lens or generic-optics with 'supportCode' instead." #-}

-- | A Boolean value indicating whether this disk is a system disk (has an operating system loaded on it).
--
-- /Note:/ Consider using 'isSystemDisk' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dIsSystemDisk :: Lens.Lens' Disk (Lude.Maybe Lude.Bool)
dIsSystemDisk = Lens.lens (isSystemDisk :: Disk -> Lude.Maybe Lude.Bool) (\s a -> s {isSystemDisk = a} :: Disk)
{-# DEPRECATED dIsSystemDisk "Use generic-lens or generic-optics with 'isSystemDisk' instead." #-}

-- | The resources to which the disk is attached.
--
-- /Note:/ Consider using 'attachedTo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAttachedTo :: Lens.Lens' Disk (Lude.Maybe Lude.Text)
dAttachedTo = Lens.lens (attachedTo :: Disk -> Lude.Maybe Lude.Text) (\s a -> s {attachedTo = a} :: Disk)
{-# DEPRECATED dAttachedTo "Use generic-lens or generic-optics with 'attachedTo' instead." #-}

-- | (Deprecated) The number of GB in use by the disk.
--
-- /Note:/ Consider using 'gbInUse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dGbInUse :: Lens.Lens' Disk (Lude.Maybe Lude.Int)
dGbInUse = Lens.lens (gbInUse :: Disk -> Lude.Maybe Lude.Int) (\s a -> s {gbInUse = a} :: Disk)
{-# DEPRECATED dGbInUse "Use generic-lens or generic-optics with 'gbInUse' instead." #-}

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTags :: Lens.Lens' Disk (Lude.Maybe [Tag])
dTags = Lens.lens (tags :: Disk -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: Disk)
{-# DEPRECATED dTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON Disk where
  parseJSON =
    Lude.withObject
      "Disk"
      ( \x ->
          Disk'
            Lude.<$> (x Lude..:? "state")
            Lude.<*> (x Lude..:? "resourceType")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "path")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "location")
            Lude.<*> (x Lude..:? "iops")
            Lude.<*> (x Lude..:? "isAttached")
            Lude.<*> (x Lude..:? "addOns" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "attachmentState")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "sizeInGb")
            Lude.<*> (x Lude..:? "supportCode")
            Lude.<*> (x Lude..:? "isSystemDisk")
            Lude.<*> (x Lude..:? "attachedTo")
            Lude.<*> (x Lude..:? "gbInUse")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
