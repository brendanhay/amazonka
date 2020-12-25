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
    dAddOns,
    dArn,
    dAttachedTo,
    dAttachmentState,
    dCreatedAt,
    dGbInUse,
    dIops,
    dIsAttached,
    dIsSystemDisk,
    dLocation,
    dName,
    dPath,
    dResourceType,
    dSizeInGb,
    dState,
    dSupportCode,
    dTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.AddOn as Types
import qualified Network.AWS.Lightsail.Types.Arn as Types
import qualified Network.AWS.Lightsail.Types.AttachedTo as Types
import qualified Network.AWS.Lightsail.Types.AttachmentState as Types
import qualified Network.AWS.Lightsail.Types.DiskState as Types
import qualified Network.AWS.Lightsail.Types.Name as Types
import qualified Network.AWS.Lightsail.Types.Path as Types
import qualified Network.AWS.Lightsail.Types.ResourceLocation as Types
import qualified Network.AWS.Lightsail.Types.ResourceType as Types
import qualified Network.AWS.Lightsail.Types.SupportCode as Types
import qualified Network.AWS.Lightsail.Types.Tag as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a system disk or a block storage disk.
--
-- /See:/ 'mkDisk' smart constructor.
data Disk = Disk'
  { -- | An array of objects representing the add-ons enabled on the disk.
    addOns :: Core.Maybe [Types.AddOn],
    -- | The Amazon Resource Name (ARN) of the disk.
    arn :: Core.Maybe Types.Arn,
    -- | The resources to which the disk is attached.
    attachedTo :: Core.Maybe Types.AttachedTo,
    -- | (Deprecated) The attachment state of the disk.
    attachmentState :: Core.Maybe Types.AttachmentState,
    -- | The date when the disk was created.
    createdAt :: Core.Maybe Core.NominalDiffTime,
    -- | (Deprecated) The number of GB in use by the disk.
    gbInUse :: Core.Maybe Core.Int,
    -- | The input/output operations per second (IOPS) of the disk.
    iops :: Core.Maybe Core.Int,
    -- | A Boolean value indicating whether the disk is attached.
    isAttached :: Core.Maybe Core.Bool,
    -- | A Boolean value indicating whether this disk is a system disk (has an operating system loaded on it).
    isSystemDisk :: Core.Maybe Core.Bool,
    -- | The AWS Region and Availability Zone where the disk is located.
    location :: Core.Maybe Types.ResourceLocation,
    -- | The unique name of the disk.
    name :: Core.Maybe Types.Name,
    -- | The disk path.
    path :: Core.Maybe Types.Path,
    -- | The Lightsail resource type (e.g., @Disk@ ).
    resourceType :: Core.Maybe Types.ResourceType,
    -- | The size of the disk in GB.
    sizeInGb :: Core.Maybe Core.Int,
    -- | Describes the status of the disk.
    state :: Core.Maybe Types.DiskState,
    -- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
    supportCode :: Core.Maybe Types.SupportCode,
    -- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Disk' value with any optional fields omitted.
mkDisk ::
  Disk
mkDisk =
  Disk'
    { addOns = Core.Nothing,
      arn = Core.Nothing,
      attachedTo = Core.Nothing,
      attachmentState = Core.Nothing,
      createdAt = Core.Nothing,
      gbInUse = Core.Nothing,
      iops = Core.Nothing,
      isAttached = Core.Nothing,
      isSystemDisk = Core.Nothing,
      location = Core.Nothing,
      name = Core.Nothing,
      path = Core.Nothing,
      resourceType = Core.Nothing,
      sizeInGb = Core.Nothing,
      state = Core.Nothing,
      supportCode = Core.Nothing,
      tags = Core.Nothing
    }

-- | An array of objects representing the add-ons enabled on the disk.
--
-- /Note:/ Consider using 'addOns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAddOns :: Lens.Lens' Disk (Core.Maybe [Types.AddOn])
dAddOns = Lens.field @"addOns"
{-# DEPRECATED dAddOns "Use generic-lens or generic-optics with 'addOns' instead." #-}

-- | The Amazon Resource Name (ARN) of the disk.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dArn :: Lens.Lens' Disk (Core.Maybe Types.Arn)
dArn = Lens.field @"arn"
{-# DEPRECATED dArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The resources to which the disk is attached.
--
-- /Note:/ Consider using 'attachedTo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAttachedTo :: Lens.Lens' Disk (Core.Maybe Types.AttachedTo)
dAttachedTo = Lens.field @"attachedTo"
{-# DEPRECATED dAttachedTo "Use generic-lens or generic-optics with 'attachedTo' instead." #-}

-- | (Deprecated) The attachment state of the disk.
--
-- /Note:/ Consider using 'attachmentState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAttachmentState :: Lens.Lens' Disk (Core.Maybe Types.AttachmentState)
dAttachmentState = Lens.field @"attachmentState"
{-# DEPRECATED dAttachmentState "Use generic-lens or generic-optics with 'attachmentState' instead." #-}

-- | The date when the disk was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCreatedAt :: Lens.Lens' Disk (Core.Maybe Core.NominalDiffTime)
dCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED dCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | (Deprecated) The number of GB in use by the disk.
--
-- /Note:/ Consider using 'gbInUse' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dGbInUse :: Lens.Lens' Disk (Core.Maybe Core.Int)
dGbInUse = Lens.field @"gbInUse"
{-# DEPRECATED dGbInUse "Use generic-lens or generic-optics with 'gbInUse' instead." #-}

-- | The input/output operations per second (IOPS) of the disk.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dIops :: Lens.Lens' Disk (Core.Maybe Core.Int)
dIops = Lens.field @"iops"
{-# DEPRECATED dIops "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | A Boolean value indicating whether the disk is attached.
--
-- /Note:/ Consider using 'isAttached' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dIsAttached :: Lens.Lens' Disk (Core.Maybe Core.Bool)
dIsAttached = Lens.field @"isAttached"
{-# DEPRECATED dIsAttached "Use generic-lens or generic-optics with 'isAttached' instead." #-}

-- | A Boolean value indicating whether this disk is a system disk (has an operating system loaded on it).
--
-- /Note:/ Consider using 'isSystemDisk' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dIsSystemDisk :: Lens.Lens' Disk (Core.Maybe Core.Bool)
dIsSystemDisk = Lens.field @"isSystemDisk"
{-# DEPRECATED dIsSystemDisk "Use generic-lens or generic-optics with 'isSystemDisk' instead." #-}

-- | The AWS Region and Availability Zone where the disk is located.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dLocation :: Lens.Lens' Disk (Core.Maybe Types.ResourceLocation)
dLocation = Lens.field @"location"
{-# DEPRECATED dLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The unique name of the disk.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dName :: Lens.Lens' Disk (Core.Maybe Types.Name)
dName = Lens.field @"name"
{-# DEPRECATED dName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The disk path.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dPath :: Lens.Lens' Disk (Core.Maybe Types.Path)
dPath = Lens.field @"path"
{-# DEPRECATED dPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The Lightsail resource type (e.g., @Disk@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dResourceType :: Lens.Lens' Disk (Core.Maybe Types.ResourceType)
dResourceType = Lens.field @"resourceType"
{-# DEPRECATED dResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The size of the disk in GB.
--
-- /Note:/ Consider using 'sizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSizeInGb :: Lens.Lens' Disk (Core.Maybe Core.Int)
dSizeInGb = Lens.field @"sizeInGb"
{-# DEPRECATED dSizeInGb "Use generic-lens or generic-optics with 'sizeInGb' instead." #-}

-- | Describes the status of the disk.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dState :: Lens.Lens' Disk (Core.Maybe Types.DiskState)
dState = Lens.field @"state"
{-# DEPRECATED dState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
--
-- /Note:/ Consider using 'supportCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSupportCode :: Lens.Lens' Disk (Core.Maybe Types.SupportCode)
dSupportCode = Lens.field @"supportCode"
{-# DEPRECATED dSupportCode "Use generic-lens or generic-optics with 'supportCode' instead." #-}

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTags :: Lens.Lens' Disk (Core.Maybe [Types.Tag])
dTags = Lens.field @"tags"
{-# DEPRECATED dTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON Disk where
  parseJSON =
    Core.withObject "Disk" Core.$
      \x ->
        Disk'
          Core.<$> (x Core..:? "addOns")
          Core.<*> (x Core..:? "arn")
          Core.<*> (x Core..:? "attachedTo")
          Core.<*> (x Core..:? "attachmentState")
          Core.<*> (x Core..:? "createdAt")
          Core.<*> (x Core..:? "gbInUse")
          Core.<*> (x Core..:? "iops")
          Core.<*> (x Core..:? "isAttached")
          Core.<*> (x Core..:? "isSystemDisk")
          Core.<*> (x Core..:? "location")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "path")
          Core.<*> (x Core..:? "resourceType")
          Core.<*> (x Core..:? "sizeInGb")
          Core.<*> (x Core..:? "state")
          Core.<*> (x Core..:? "supportCode")
          Core.<*> (x Core..:? "tags")
