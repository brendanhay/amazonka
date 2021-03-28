{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstanceSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.InstanceSnapshot
  ( InstanceSnapshot (..)
  -- * Smart constructor
  , mkInstanceSnapshot
  -- * Lenses
  , isArn
  , isCreatedAt
  , isFromAttachedDisks
  , isFromBlueprintId
  , isFromBundleId
  , isFromInstanceArn
  , isFromInstanceName
  , isIsFromAutoSnapshot
  , isLocation
  , isName
  , isProgress
  , isResourceType
  , isSizeInGb
  , isState
  , isSupportCode
  , isTags
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.Disk as Types
import qualified Network.AWS.Lightsail.Types.InstanceSnapshotState as Types
import qualified Network.AWS.Lightsail.Types.NonEmptyString as Types
import qualified Network.AWS.Lightsail.Types.ResourceLocation as Types
import qualified Network.AWS.Lightsail.Types.ResourceName as Types
import qualified Network.AWS.Lightsail.Types.ResourceType as Types
import qualified Network.AWS.Lightsail.Types.Tag as Types
import qualified Network.AWS.Prelude as Core

-- | Describes an instance snapshot.
--
-- /See:/ 'mkInstanceSnapshot' smart constructor.
data InstanceSnapshot = InstanceSnapshot'
  { arn :: Core.Maybe Types.NonEmptyString
    -- ^ The Amazon Resource Name (ARN) of the snapshot (e.g., @arn:aws:lightsail:us-east-2:123456789101:InstanceSnapshot/d23b5706-3322-4d83-81e5-12345EXAMPLE@ ).
  , createdAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp when the snapshot was created (e.g., @1479907467.024@ ).
  , fromAttachedDisks :: Core.Maybe [Types.Disk]
    -- ^ An array of disk objects containing information about all block storage disks.
  , fromBlueprintId :: Core.Maybe Core.Text
    -- ^ The blueprint ID from which you created the snapshot (e.g., @os_debian_8_3@ ). A blueprint is a virtual private server (or /instance/ ) image used to create instances quickly.
  , fromBundleId :: Core.Maybe Core.Text
    -- ^ The bundle ID from which you created the snapshot (e.g., @micro_1_0@ ).
  , fromInstanceArn :: Core.Maybe Types.NonEmptyString
    -- ^ The Amazon Resource Name (ARN) of the instance from which the snapshot was created (e.g., @arn:aws:lightsail:us-east-2:123456789101:Instance/64b8404c-ccb1-430b-8daf-12345EXAMPLE@ ).
  , fromInstanceName :: Core.Maybe Types.ResourceName
    -- ^ The instance from which the snapshot was created.
  , isFromAutoSnapshot :: Core.Maybe Core.Bool
    -- ^ A Boolean value indicating whether the snapshot was created from an automatic snapshot.
  , location :: Core.Maybe Types.ResourceLocation
    -- ^ The region name and Availability Zone where you created the snapshot.
  , name :: Core.Maybe Types.ResourceName
    -- ^ The name of the snapshot.
  , progress :: Core.Maybe Core.Text
    -- ^ The progress of the snapshot.
  , resourceType :: Core.Maybe Types.ResourceType
    -- ^ The type of resource (usually @InstanceSnapshot@ ).
  , sizeInGb :: Core.Maybe Core.Int
    -- ^ The size in GB of the SSD.
  , state :: Core.Maybe Types.InstanceSnapshotState
    -- ^ The state the snapshot is in.
  , supportCode :: Core.Maybe Core.Text
    -- ^ The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'InstanceSnapshot' value with any optional fields omitted.
mkInstanceSnapshot
    :: InstanceSnapshot
mkInstanceSnapshot
  = InstanceSnapshot'{arn = Core.Nothing, createdAt = Core.Nothing,
                      fromAttachedDisks = Core.Nothing, fromBlueprintId = Core.Nothing,
                      fromBundleId = Core.Nothing, fromInstanceArn = Core.Nothing,
                      fromInstanceName = Core.Nothing, isFromAutoSnapshot = Core.Nothing,
                      location = Core.Nothing, name = Core.Nothing,
                      progress = Core.Nothing, resourceType = Core.Nothing,
                      sizeInGb = Core.Nothing, state = Core.Nothing,
                      supportCode = Core.Nothing, tags = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the snapshot (e.g., @arn:aws:lightsail:us-east-2:123456789101:InstanceSnapshot/d23b5706-3322-4d83-81e5-12345EXAMPLE@ ).
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isArn :: Lens.Lens' InstanceSnapshot (Core.Maybe Types.NonEmptyString)
isArn = Lens.field @"arn"
{-# INLINEABLE isArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The timestamp when the snapshot was created (e.g., @1479907467.024@ ).
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isCreatedAt :: Lens.Lens' InstanceSnapshot (Core.Maybe Core.NominalDiffTime)
isCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE isCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | An array of disk objects containing information about all block storage disks.
--
-- /Note:/ Consider using 'fromAttachedDisks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isFromAttachedDisks :: Lens.Lens' InstanceSnapshot (Core.Maybe [Types.Disk])
isFromAttachedDisks = Lens.field @"fromAttachedDisks"
{-# INLINEABLE isFromAttachedDisks #-}
{-# DEPRECATED fromAttachedDisks "Use generic-lens or generic-optics with 'fromAttachedDisks' instead"  #-}

-- | The blueprint ID from which you created the snapshot (e.g., @os_debian_8_3@ ). A blueprint is a virtual private server (or /instance/ ) image used to create instances quickly.
--
-- /Note:/ Consider using 'fromBlueprintId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isFromBlueprintId :: Lens.Lens' InstanceSnapshot (Core.Maybe Core.Text)
isFromBlueprintId = Lens.field @"fromBlueprintId"
{-# INLINEABLE isFromBlueprintId #-}
{-# DEPRECATED fromBlueprintId "Use generic-lens or generic-optics with 'fromBlueprintId' instead"  #-}

-- | The bundle ID from which you created the snapshot (e.g., @micro_1_0@ ).
--
-- /Note:/ Consider using 'fromBundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isFromBundleId :: Lens.Lens' InstanceSnapshot (Core.Maybe Core.Text)
isFromBundleId = Lens.field @"fromBundleId"
{-# INLINEABLE isFromBundleId #-}
{-# DEPRECATED fromBundleId "Use generic-lens or generic-optics with 'fromBundleId' instead"  #-}

-- | The Amazon Resource Name (ARN) of the instance from which the snapshot was created (e.g., @arn:aws:lightsail:us-east-2:123456789101:Instance/64b8404c-ccb1-430b-8daf-12345EXAMPLE@ ).
--
-- /Note:/ Consider using 'fromInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isFromInstanceArn :: Lens.Lens' InstanceSnapshot (Core.Maybe Types.NonEmptyString)
isFromInstanceArn = Lens.field @"fromInstanceArn"
{-# INLINEABLE isFromInstanceArn #-}
{-# DEPRECATED fromInstanceArn "Use generic-lens or generic-optics with 'fromInstanceArn' instead"  #-}

-- | The instance from which the snapshot was created.
--
-- /Note:/ Consider using 'fromInstanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isFromInstanceName :: Lens.Lens' InstanceSnapshot (Core.Maybe Types.ResourceName)
isFromInstanceName = Lens.field @"fromInstanceName"
{-# INLINEABLE isFromInstanceName #-}
{-# DEPRECATED fromInstanceName "Use generic-lens or generic-optics with 'fromInstanceName' instead"  #-}

-- | A Boolean value indicating whether the snapshot was created from an automatic snapshot.
--
-- /Note:/ Consider using 'isFromAutoSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isIsFromAutoSnapshot :: Lens.Lens' InstanceSnapshot (Core.Maybe Core.Bool)
isIsFromAutoSnapshot = Lens.field @"isFromAutoSnapshot"
{-# INLINEABLE isIsFromAutoSnapshot #-}
{-# DEPRECATED isFromAutoSnapshot "Use generic-lens or generic-optics with 'isFromAutoSnapshot' instead"  #-}

-- | The region name and Availability Zone where you created the snapshot.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isLocation :: Lens.Lens' InstanceSnapshot (Core.Maybe Types.ResourceLocation)
isLocation = Lens.field @"location"
{-# INLINEABLE isLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The name of the snapshot.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isName :: Lens.Lens' InstanceSnapshot (Core.Maybe Types.ResourceName)
isName = Lens.field @"name"
{-# INLINEABLE isName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The progress of the snapshot.
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isProgress :: Lens.Lens' InstanceSnapshot (Core.Maybe Core.Text)
isProgress = Lens.field @"progress"
{-# INLINEABLE isProgress #-}
{-# DEPRECATED progress "Use generic-lens or generic-optics with 'progress' instead"  #-}

-- | The type of resource (usually @InstanceSnapshot@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isResourceType :: Lens.Lens' InstanceSnapshot (Core.Maybe Types.ResourceType)
isResourceType = Lens.field @"resourceType"
{-# INLINEABLE isResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The size in GB of the SSD.
--
-- /Note:/ Consider using 'sizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isSizeInGb :: Lens.Lens' InstanceSnapshot (Core.Maybe Core.Int)
isSizeInGb = Lens.field @"sizeInGb"
{-# INLINEABLE isSizeInGb #-}
{-# DEPRECATED sizeInGb "Use generic-lens or generic-optics with 'sizeInGb' instead"  #-}

-- | The state the snapshot is in.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isState :: Lens.Lens' InstanceSnapshot (Core.Maybe Types.InstanceSnapshotState)
isState = Lens.field @"state"
{-# INLINEABLE isState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
--
-- /Note:/ Consider using 'supportCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isSupportCode :: Lens.Lens' InstanceSnapshot (Core.Maybe Core.Text)
isSupportCode = Lens.field @"supportCode"
{-# INLINEABLE isSupportCode #-}
{-# DEPRECATED supportCode "Use generic-lens or generic-optics with 'supportCode' instead"  #-}

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isTags :: Lens.Lens' InstanceSnapshot (Core.Maybe [Types.Tag])
isTags = Lens.field @"tags"
{-# INLINEABLE isTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON InstanceSnapshot where
        parseJSON
          = Core.withObject "InstanceSnapshot" Core.$
              \ x ->
                InstanceSnapshot' Core.<$>
                  (x Core..:? "arn") Core.<*> x Core..:? "createdAt" Core.<*>
                    x Core..:? "fromAttachedDisks"
                    Core.<*> x Core..:? "fromBlueprintId"
                    Core.<*> x Core..:? "fromBundleId"
                    Core.<*> x Core..:? "fromInstanceArn"
                    Core.<*> x Core..:? "fromInstanceName"
                    Core.<*> x Core..:? "isFromAutoSnapshot"
                    Core.<*> x Core..:? "location"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "progress"
                    Core.<*> x Core..:? "resourceType"
                    Core.<*> x Core..:? "sizeInGb"
                    Core.<*> x Core..:? "state"
                    Core.<*> x Core..:? "supportCode"
                    Core.<*> x Core..:? "tags"
