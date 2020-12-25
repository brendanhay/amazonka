{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.DiskSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.DiskSnapshot
  ( DiskSnapshot (..),

    -- * Smart constructor
    mkDiskSnapshot,

    -- * Lenses
    dsArn,
    dsCreatedAt,
    dsFromDiskArn,
    dsFromDiskName,
    dsFromInstanceArn,
    dsFromInstanceName,
    dsIsFromAutoSnapshot,
    dsLocation,
    dsName,
    dsProgress,
    dsResourceType,
    dsSizeInGb,
    dsState,
    dsSupportCode,
    dsTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.Arn as Types
import qualified Network.AWS.Lightsail.Types.DiskSnapshotState as Types
import qualified Network.AWS.Lightsail.Types.FromDiskArn as Types
import qualified Network.AWS.Lightsail.Types.FromInstanceArn as Types
import qualified Network.AWS.Lightsail.Types.ResourceLocation as Types
import qualified Network.AWS.Lightsail.Types.ResourceName as Types
import qualified Network.AWS.Lightsail.Types.ResourceType as Types
import qualified Network.AWS.Lightsail.Types.String as Types
import qualified Network.AWS.Lightsail.Types.Tag as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a block storage disk snapshot.
--
-- /See:/ 'mkDiskSnapshot' smart constructor.
data DiskSnapshot = DiskSnapshot'
  { -- | The Amazon Resource Name (ARN) of the disk snapshot.
    arn :: Core.Maybe Types.Arn,
    -- | The date when the disk snapshot was created.
    createdAt :: Core.Maybe Core.NominalDiffTime,
    -- | The Amazon Resource Name (ARN) of the source disk from which the disk snapshot was created.
    fromDiskArn :: Core.Maybe Types.FromDiskArn,
    -- | The unique name of the source disk from which the disk snapshot was created.
    fromDiskName :: Core.Maybe Types.ResourceName,
    -- | The Amazon Resource Name (ARN) of the source instance from which the disk (system volume) snapshot was created.
    fromInstanceArn :: Core.Maybe Types.FromInstanceArn,
    -- | The unique name of the source instance from which the disk (system volume) snapshot was created.
    fromInstanceName :: Core.Maybe Types.ResourceName,
    -- | A Boolean value indicating whether the snapshot was created from an automatic snapshot.
    isFromAutoSnapshot :: Core.Maybe Core.Bool,
    -- | The AWS Region and Availability Zone where the disk snapshot was created.
    location :: Core.Maybe Types.ResourceLocation,
    -- | The name of the disk snapshot (e.g., @my-disk-snapshot@ ).
    name :: Core.Maybe Types.ResourceName,
    -- | The progress of the disk snapshot operation.
    progress :: Core.Maybe Types.String,
    -- | The Lightsail resource type (e.g., @DiskSnapshot@ ).
    resourceType :: Core.Maybe Types.ResourceType,
    -- | The size of the disk in GB.
    sizeInGb :: Core.Maybe Core.Int,
    -- | The status of the disk snapshot operation.
    state :: Core.Maybe Types.DiskSnapshotState,
    -- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
    supportCode :: Core.Maybe Types.String,
    -- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DiskSnapshot' value with any optional fields omitted.
mkDiskSnapshot ::
  DiskSnapshot
mkDiskSnapshot =
  DiskSnapshot'
    { arn = Core.Nothing,
      createdAt = Core.Nothing,
      fromDiskArn = Core.Nothing,
      fromDiskName = Core.Nothing,
      fromInstanceArn = Core.Nothing,
      fromInstanceName = Core.Nothing,
      isFromAutoSnapshot = Core.Nothing,
      location = Core.Nothing,
      name = Core.Nothing,
      progress = Core.Nothing,
      resourceType = Core.Nothing,
      sizeInGb = Core.Nothing,
      state = Core.Nothing,
      supportCode = Core.Nothing,
      tags = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the disk snapshot.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsArn :: Lens.Lens' DiskSnapshot (Core.Maybe Types.Arn)
dsArn = Lens.field @"arn"
{-# DEPRECATED dsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The date when the disk snapshot was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsCreatedAt :: Lens.Lens' DiskSnapshot (Core.Maybe Core.NominalDiffTime)
dsCreatedAt = Lens.field @"createdAt"
{-# DEPRECATED dsCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The Amazon Resource Name (ARN) of the source disk from which the disk snapshot was created.
--
-- /Note:/ Consider using 'fromDiskArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsFromDiskArn :: Lens.Lens' DiskSnapshot (Core.Maybe Types.FromDiskArn)
dsFromDiskArn = Lens.field @"fromDiskArn"
{-# DEPRECATED dsFromDiskArn "Use generic-lens or generic-optics with 'fromDiskArn' instead." #-}

-- | The unique name of the source disk from which the disk snapshot was created.
--
-- /Note:/ Consider using 'fromDiskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsFromDiskName :: Lens.Lens' DiskSnapshot (Core.Maybe Types.ResourceName)
dsFromDiskName = Lens.field @"fromDiskName"
{-# DEPRECATED dsFromDiskName "Use generic-lens or generic-optics with 'fromDiskName' instead." #-}

-- | The Amazon Resource Name (ARN) of the source instance from which the disk (system volume) snapshot was created.
--
-- /Note:/ Consider using 'fromInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsFromInstanceArn :: Lens.Lens' DiskSnapshot (Core.Maybe Types.FromInstanceArn)
dsFromInstanceArn = Lens.field @"fromInstanceArn"
{-# DEPRECATED dsFromInstanceArn "Use generic-lens or generic-optics with 'fromInstanceArn' instead." #-}

-- | The unique name of the source instance from which the disk (system volume) snapshot was created.
--
-- /Note:/ Consider using 'fromInstanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsFromInstanceName :: Lens.Lens' DiskSnapshot (Core.Maybe Types.ResourceName)
dsFromInstanceName = Lens.field @"fromInstanceName"
{-# DEPRECATED dsFromInstanceName "Use generic-lens or generic-optics with 'fromInstanceName' instead." #-}

-- | A Boolean value indicating whether the snapshot was created from an automatic snapshot.
--
-- /Note:/ Consider using 'isFromAutoSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsIsFromAutoSnapshot :: Lens.Lens' DiskSnapshot (Core.Maybe Core.Bool)
dsIsFromAutoSnapshot = Lens.field @"isFromAutoSnapshot"
{-# DEPRECATED dsIsFromAutoSnapshot "Use generic-lens or generic-optics with 'isFromAutoSnapshot' instead." #-}

-- | The AWS Region and Availability Zone where the disk snapshot was created.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsLocation :: Lens.Lens' DiskSnapshot (Core.Maybe Types.ResourceLocation)
dsLocation = Lens.field @"location"
{-# DEPRECATED dsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The name of the disk snapshot (e.g., @my-disk-snapshot@ ).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsName :: Lens.Lens' DiskSnapshot (Core.Maybe Types.ResourceName)
dsName = Lens.field @"name"
{-# DEPRECATED dsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The progress of the disk snapshot operation.
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsProgress :: Lens.Lens' DiskSnapshot (Core.Maybe Types.String)
dsProgress = Lens.field @"progress"
{-# DEPRECATED dsProgress "Use generic-lens or generic-optics with 'progress' instead." #-}

-- | The Lightsail resource type (e.g., @DiskSnapshot@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsResourceType :: Lens.Lens' DiskSnapshot (Core.Maybe Types.ResourceType)
dsResourceType = Lens.field @"resourceType"
{-# DEPRECATED dsResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The size of the disk in GB.
--
-- /Note:/ Consider using 'sizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSizeInGb :: Lens.Lens' DiskSnapshot (Core.Maybe Core.Int)
dsSizeInGb = Lens.field @"sizeInGb"
{-# DEPRECATED dsSizeInGb "Use generic-lens or generic-optics with 'sizeInGb' instead." #-}

-- | The status of the disk snapshot operation.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsState :: Lens.Lens' DiskSnapshot (Core.Maybe Types.DiskSnapshotState)
dsState = Lens.field @"state"
{-# DEPRECATED dsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
--
-- /Note:/ Consider using 'supportCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSupportCode :: Lens.Lens' DiskSnapshot (Core.Maybe Types.String)
dsSupportCode = Lens.field @"supportCode"
{-# DEPRECATED dsSupportCode "Use generic-lens or generic-optics with 'supportCode' instead." #-}

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsTags :: Lens.Lens' DiskSnapshot (Core.Maybe [Types.Tag])
dsTags = Lens.field @"tags"
{-# DEPRECATED dsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON DiskSnapshot where
  parseJSON =
    Core.withObject "DiskSnapshot" Core.$
      \x ->
        DiskSnapshot'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "createdAt")
          Core.<*> (x Core..:? "fromDiskArn")
          Core.<*> (x Core..:? "fromDiskName")
          Core.<*> (x Core..:? "fromInstanceArn")
          Core.<*> (x Core..:? "fromInstanceName")
          Core.<*> (x Core..:? "isFromAutoSnapshot")
          Core.<*> (x Core..:? "location")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "progress")
          Core.<*> (x Core..:? "resourceType")
          Core.<*> (x Core..:? "sizeInGb")
          Core.<*> (x Core..:? "state")
          Core.<*> (x Core..:? "supportCode")
          Core.<*> (x Core..:? "tags")
