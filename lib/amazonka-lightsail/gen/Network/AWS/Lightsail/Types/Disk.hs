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
    dfState,
    dfResourceType,
    dfArn,
    dfPath,
    dfCreatedAt,
    dfLocation,
    dfIops,
    dfIsAttached,
    dfAddOns,
    dfAttachmentState,
    dfName,
    dfSizeInGb,
    dfSupportCode,
    dfIsSystemDisk,
    dfAttachedTo,
    dfGbInUse,
    dfTags,
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
  { -- | Describes the status of the disk.
    state :: Lude.Maybe DiskState,
    -- | The Lightsail resource type (e.g., @Disk@ ).
    resourceType :: Lude.Maybe ResourceType,
    -- | The Amazon Resource Name (ARN) of the disk.
    arn :: Lude.Maybe Lude.Text,
    -- | The disk path.
    path :: Lude.Maybe Lude.Text,
    -- | The date when the disk was created.
    createdAt :: Lude.Maybe Lude.Timestamp,
    -- | The AWS Region and Availability Zone where the disk is located.
    location :: Lude.Maybe ResourceLocation,
    -- | The input/output operations per second (IOPS) of the disk.
    iops :: Lude.Maybe Lude.Int,
    -- | A Boolean value indicating whether the disk is attached.
    isAttached :: Lude.Maybe Lude.Bool,
    -- | An array of objects representing the add-ons enabled on the disk.
    addOns :: Lude.Maybe [AddOn],
    -- | (Deprecated) The attachment state of the disk.
    attachmentState :: Lude.Maybe Lude.Text,
    -- | The unique name of the disk.
    name :: Lude.Maybe Lude.Text,
    -- | The size of the disk in GB.
    sizeInGb :: Lude.Maybe Lude.Int,
    -- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
    supportCode :: Lude.Maybe Lude.Text,
    -- | A Boolean value indicating whether this disk is a system disk (has an operating system loaded on it).
    isSystemDisk :: Lude.Maybe Lude.Bool,
    -- | The resources to which the disk is attached.
    attachedTo :: Lude.Maybe Lude.Text,
    -- | (Deprecated) The number of GB in use by the disk.
    gbInUse :: Lude.Maybe Lude.Int,
    -- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Disk' with the minimum fields required to make a request.
--
-- * 'state' - Describes the status of the disk.
-- * 'resourceType' - The Lightsail resource type (e.g., @Disk@ ).
-- * 'arn' - The Amazon Resource Name (ARN) of the disk.
-- * 'path' - The disk path.
-- * 'createdAt' - The date when the disk was created.
-- * 'location' - The AWS Region and Availability Zone where the disk is located.
-- * 'iops' - The input/output operations per second (IOPS) of the disk.
-- * 'isAttached' - A Boolean value indicating whether the disk is attached.
-- * 'addOns' - An array of objects representing the add-ons enabled on the disk.
-- * 'attachmentState' - (Deprecated) The attachment state of the disk.
-- * 'name' - The unique name of the disk.
-- * 'sizeInGb' - The size of the disk in GB.
-- * 'supportCode' - The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
-- * 'isSystemDisk' - A Boolean value indicating whether this disk is a system disk (has an operating system loaded on it).
-- * 'attachedTo' - The resources to which the disk is attached.
-- * 'gbInUse' - (Deprecated) The number of GB in use by the disk.
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
dfState :: Lens.Lens' Disk (Lude.Maybe DiskState)
dfState = Lens.lens (state :: Disk -> Lude.Maybe DiskState) (\s a -> s {state = a} :: Disk)
{-# DEPRECATED dfState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The Lightsail resource type (e.g., @Disk@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfResourceType :: Lens.Lens' Disk (Lude.Maybe ResourceType)
dfResourceType = Lens.lens (resourceType :: Disk -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: Disk)
{-# DEPRECATED dfResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The Amazon Resource Name (ARN) of the disk.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfArn :: Lens.Lens' Disk (Lude.Maybe Lude.Text)
dfArn = Lens.lens (arn :: Disk -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Disk)
{-# DEPRECATED dfArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The disk path.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfPath :: Lens.Lens' Disk (Lude.Maybe Lude.Text)
dfPath = Lens.lens (path :: Disk -> Lude.Maybe Lude.Text) (\s a -> s {path = a} :: Disk)
{-# DEPRECATED dfPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The date when the disk was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfCreatedAt :: Lens.Lens' Disk (Lude.Maybe Lude.Timestamp)
dfCreatedAt = Lens.lens (createdAt :: Disk -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: Disk)
{-# DEPRECATED dfCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The AWS Region and Availability Zone where the disk is located.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfLocation :: Lens.Lens' Disk (Lude.Maybe ResourceLocation)
dfLocation = Lens.lens (location :: Disk -> Lude.Maybe ResourceLocation) (\s a -> s {location = a} :: Disk)
{-# DEPRECATED dfLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The input/output operations per second (IOPS) of the disk.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfIops :: Lens.Lens' Disk (Lude.Maybe Lude.Int)
dfIops = Lens.lens (iops :: Disk -> Lude.Maybe Lude.Int) (\s a -> s {iops = a} :: Disk)
{-# DEPRECATED dfIops "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | A Boolean value indicating whether the disk is attached.
--
-- /Note:/ Consider using 'isAttached' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfIsAttached :: Lens.Lens' Disk (Lude.Maybe Lude.Bool)
dfIsAttached = Lens.lens (isAttached :: Disk -> Lude.Maybe Lude.Bool) (\s a -> s {isAttached = a} :: Disk)
{-# DEPRECATED dfIsAttached "Use generic-lens or generic-optics with 'isAttached' instead." #-}

-- | An array of objects representing the add-ons enabled on the disk.
--
-- /Note:/ Consider using 'addOns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfAddOns :: Lens.Lens' Disk (Lude.Maybe [AddOn])
dfAddOns = Lens.lens (addOns :: Disk -> Lude.Maybe [AddOn]) (\s a -> s {addOns = a} :: Disk)
{-# DEPRECATED dfAddOns "Use generic-lens or generic-optics with 'addOns' instead." #-}

-- | (Deprecated) The attachment state of the disk.
--
-- /Note:/ Consider using 'attachmentState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfAttachmentState :: Lens.Lens' Disk (Lude.Maybe Lude.Text)
dfAttachmentState = Lens.lens (attachmentState :: Disk -> Lude.Maybe Lude.Text) (\s a -> s {attachmentState = a} :: Disk)
{-# DEPRECATED dfAttachmentState "Use generic-lens or generic-optics with 'attachmentState' instead." #-}

-- | The unique name of the disk.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfName :: Lens.Lens' Disk (Lude.Maybe Lude.Text)
dfName = Lens.lens (name :: Disk -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Disk)
{-# DEPRECATED dfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The size of the disk in GB.
--
-- /Note:/ Consider using 'sizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfSizeInGb :: Lens.Lens' Disk (Lude.Maybe Lude.Int)
dfSizeInGb = Lens.lens (sizeInGb :: Disk -> Lude.Maybe Lude.Int) (\s a -> s {sizeInGb = a} :: Disk)
{-# DEPRECATED dfSizeInGb "Use generic-lens or generic-optics with 'sizeInGb' instead." #-}

-- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
--
-- /Note:/ Consider using 'supportCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfSupportCode :: Lens.Lens' Disk (Lude.Maybe Lude.Text)
dfSupportCode = Lens.lens (supportCode :: Disk -> Lude.Maybe Lude.Text) (\s a -> s {supportCode = a} :: Disk)
{-# DEPRECATED dfSupportCode "Use generic-lens or generic-optics with 'supportCode' instead." #-}

-- | A Boolean value indicating whether this disk is a system disk (has an operating system loaded on it).
--
-- /Note:/ Consider using 'isSystemDisk' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfIsSystemDisk :: Lens.Lens' Disk (Lude.Maybe Lude.Bool)
dfIsSystemDisk = Lens.lens (isSystemDisk :: Disk -> Lude.Maybe Lude.Bool) (\s a -> s {isSystemDisk = a} :: Disk)
{-# DEPRECATED dfIsSystemDisk "Use generic-lens or generic-optics with 'isSystemDisk' instead." #-}

-- | The resources to which the disk is attached.
--
-- /Note:/ Consider using 'attachedTo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfAttachedTo :: Lens.Lens' Disk (Lude.Maybe Lude.Text)
dfAttachedTo = Lens.lens (attachedTo :: Disk -> Lude.Maybe Lude.Text) (\s a -> s {attachedTo = a} :: Disk)
{-# DEPRECATED dfAttachedTo "Use generic-lens or generic-optics with 'attachedTo' instead." #-}

-- | (Deprecated) The number of GB in use by the disk.
--
-- /Note:/ Consider using 'gbInUse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfGbInUse :: Lens.Lens' Disk (Lude.Maybe Lude.Int)
dfGbInUse = Lens.lens (gbInUse :: Disk -> Lude.Maybe Lude.Int) (\s a -> s {gbInUse = a} :: Disk)
{-# DEPRECATED dfGbInUse "Use generic-lens or generic-optics with 'gbInUse' instead." #-}

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfTags :: Lens.Lens' Disk (Lude.Maybe [Tag])
dfTags = Lens.lens (tags :: Disk -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: Disk)
{-# DEPRECATED dfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

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
