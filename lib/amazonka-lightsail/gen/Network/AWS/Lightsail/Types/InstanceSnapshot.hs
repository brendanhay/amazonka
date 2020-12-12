{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.InstanceSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.InstanceSnapshot
  ( InstanceSnapshot (..),

    -- * Smart constructor
    mkInstanceSnapshot,

    -- * Lenses
    insFromBlueprintId,
    insIsFromAutoSnapshot,
    insState,
    insResourceType,
    insFromAttachedDisks,
    insArn,
    insCreatedAt,
    insLocation,
    insProgress,
    insName,
    insFromBundleId,
    insSizeInGb,
    insSupportCode,
    insFromInstanceARN,
    insFromInstanceName,
    insTags,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.Disk
import Network.AWS.Lightsail.Types.InstanceSnapshotState
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.Tag
import qualified Network.AWS.Prelude as Lude

-- | Describes an instance snapshot.
--
-- /See:/ 'mkInstanceSnapshot' smart constructor.
data InstanceSnapshot = InstanceSnapshot'
  { fromBlueprintId ::
      Lude.Maybe Lude.Text,
    isFromAutoSnapshot :: Lude.Maybe Lude.Bool,
    state :: Lude.Maybe InstanceSnapshotState,
    resourceType :: Lude.Maybe ResourceType,
    fromAttachedDisks :: Lude.Maybe [Disk],
    arn :: Lude.Maybe Lude.Text,
    createdAt :: Lude.Maybe Lude.Timestamp,
    location :: Lude.Maybe ResourceLocation,
    progress :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    fromBundleId :: Lude.Maybe Lude.Text,
    sizeInGb :: Lude.Maybe Lude.Int,
    supportCode :: Lude.Maybe Lude.Text,
    fromInstanceARN :: Lude.Maybe Lude.Text,
    fromInstanceName :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'InstanceSnapshot' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the snapshot (e.g., @arn:aws:lightsail:us-east-2:123456789101:InstanceSnapshot/d23b5706-3322-4d83-81e5-12345EXAMPLE@ ).
-- * 'createdAt' - The timestamp when the snapshot was created (e.g., @1479907467.024@ ).
-- * 'fromAttachedDisks' - An array of disk objects containing information about all block storage disks.
-- * 'fromBlueprintId' - The blueprint ID from which you created the snapshot (e.g., @os_debian_8_3@ ). A blueprint is a virtual private server (or /instance/ ) image used to create instances quickly.
-- * 'fromBundleId' - The bundle ID from which you created the snapshot (e.g., @micro_1_0@ ).
-- * 'fromInstanceARN' - The Amazon Resource Name (ARN) of the instance from which the snapshot was created (e.g., @arn:aws:lightsail:us-east-2:123456789101:Instance/64b8404c-ccb1-430b-8daf-12345EXAMPLE@ ).
-- * 'fromInstanceName' - The instance from which the snapshot was created.
-- * 'isFromAutoSnapshot' - A Boolean value indicating whether the snapshot was created from an automatic snapshot.
-- * 'location' - The region name and Availability Zone where you created the snapshot.
-- * 'name' - The name of the snapshot.
-- * 'progress' - The progress of the snapshot.
-- * 'resourceType' - The type of resource (usually @InstanceSnapshot@ ).
-- * 'sizeInGb' - The size in GB of the SSD.
-- * 'state' - The state the snapshot is in.
-- * 'supportCode' - The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
-- * 'tags' - The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
mkInstanceSnapshot ::
  InstanceSnapshot
mkInstanceSnapshot =
  InstanceSnapshot'
    { fromBlueprintId = Lude.Nothing,
      isFromAutoSnapshot = Lude.Nothing,
      state = Lude.Nothing,
      resourceType = Lude.Nothing,
      fromAttachedDisks = Lude.Nothing,
      arn = Lude.Nothing,
      createdAt = Lude.Nothing,
      location = Lude.Nothing,
      progress = Lude.Nothing,
      name = Lude.Nothing,
      fromBundleId = Lude.Nothing,
      sizeInGb = Lude.Nothing,
      supportCode = Lude.Nothing,
      fromInstanceARN = Lude.Nothing,
      fromInstanceName = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The blueprint ID from which you created the snapshot (e.g., @os_debian_8_3@ ). A blueprint is a virtual private server (or /instance/ ) image used to create instances quickly.
--
-- /Note:/ Consider using 'fromBlueprintId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insFromBlueprintId :: Lens.Lens' InstanceSnapshot (Lude.Maybe Lude.Text)
insFromBlueprintId = Lens.lens (fromBlueprintId :: InstanceSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {fromBlueprintId = a} :: InstanceSnapshot)
{-# DEPRECATED insFromBlueprintId "Use generic-lens or generic-optics with 'fromBlueprintId' instead." #-}

-- | A Boolean value indicating whether the snapshot was created from an automatic snapshot.
--
-- /Note:/ Consider using 'isFromAutoSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insIsFromAutoSnapshot :: Lens.Lens' InstanceSnapshot (Lude.Maybe Lude.Bool)
insIsFromAutoSnapshot = Lens.lens (isFromAutoSnapshot :: InstanceSnapshot -> Lude.Maybe Lude.Bool) (\s a -> s {isFromAutoSnapshot = a} :: InstanceSnapshot)
{-# DEPRECATED insIsFromAutoSnapshot "Use generic-lens or generic-optics with 'isFromAutoSnapshot' instead." #-}

-- | The state the snapshot is in.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insState :: Lens.Lens' InstanceSnapshot (Lude.Maybe InstanceSnapshotState)
insState = Lens.lens (state :: InstanceSnapshot -> Lude.Maybe InstanceSnapshotState) (\s a -> s {state = a} :: InstanceSnapshot)
{-# DEPRECATED insState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The type of resource (usually @InstanceSnapshot@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insResourceType :: Lens.Lens' InstanceSnapshot (Lude.Maybe ResourceType)
insResourceType = Lens.lens (resourceType :: InstanceSnapshot -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: InstanceSnapshot)
{-# DEPRECATED insResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | An array of disk objects containing information about all block storage disks.
--
-- /Note:/ Consider using 'fromAttachedDisks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insFromAttachedDisks :: Lens.Lens' InstanceSnapshot (Lude.Maybe [Disk])
insFromAttachedDisks = Lens.lens (fromAttachedDisks :: InstanceSnapshot -> Lude.Maybe [Disk]) (\s a -> s {fromAttachedDisks = a} :: InstanceSnapshot)
{-# DEPRECATED insFromAttachedDisks "Use generic-lens or generic-optics with 'fromAttachedDisks' instead." #-}

-- | The Amazon Resource Name (ARN) of the snapshot (e.g., @arn:aws:lightsail:us-east-2:123456789101:InstanceSnapshot/d23b5706-3322-4d83-81e5-12345EXAMPLE@ ).
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insArn :: Lens.Lens' InstanceSnapshot (Lude.Maybe Lude.Text)
insArn = Lens.lens (arn :: InstanceSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: InstanceSnapshot)
{-# DEPRECATED insArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The timestamp when the snapshot was created (e.g., @1479907467.024@ ).
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insCreatedAt :: Lens.Lens' InstanceSnapshot (Lude.Maybe Lude.Timestamp)
insCreatedAt = Lens.lens (createdAt :: InstanceSnapshot -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: InstanceSnapshot)
{-# DEPRECATED insCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The region name and Availability Zone where you created the snapshot.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insLocation :: Lens.Lens' InstanceSnapshot (Lude.Maybe ResourceLocation)
insLocation = Lens.lens (location :: InstanceSnapshot -> Lude.Maybe ResourceLocation) (\s a -> s {location = a} :: InstanceSnapshot)
{-# DEPRECATED insLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The progress of the snapshot.
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insProgress :: Lens.Lens' InstanceSnapshot (Lude.Maybe Lude.Text)
insProgress = Lens.lens (progress :: InstanceSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {progress = a} :: InstanceSnapshot)
{-# DEPRECATED insProgress "Use generic-lens or generic-optics with 'progress' instead." #-}

-- | The name of the snapshot.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insName :: Lens.Lens' InstanceSnapshot (Lude.Maybe Lude.Text)
insName = Lens.lens (name :: InstanceSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: InstanceSnapshot)
{-# DEPRECATED insName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The bundle ID from which you created the snapshot (e.g., @micro_1_0@ ).
--
-- /Note:/ Consider using 'fromBundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insFromBundleId :: Lens.Lens' InstanceSnapshot (Lude.Maybe Lude.Text)
insFromBundleId = Lens.lens (fromBundleId :: InstanceSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {fromBundleId = a} :: InstanceSnapshot)
{-# DEPRECATED insFromBundleId "Use generic-lens or generic-optics with 'fromBundleId' instead." #-}

-- | The size in GB of the SSD.
--
-- /Note:/ Consider using 'sizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insSizeInGb :: Lens.Lens' InstanceSnapshot (Lude.Maybe Lude.Int)
insSizeInGb = Lens.lens (sizeInGb :: InstanceSnapshot -> Lude.Maybe Lude.Int) (\s a -> s {sizeInGb = a} :: InstanceSnapshot)
{-# DEPRECATED insSizeInGb "Use generic-lens or generic-optics with 'sizeInGb' instead." #-}

-- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
--
-- /Note:/ Consider using 'supportCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insSupportCode :: Lens.Lens' InstanceSnapshot (Lude.Maybe Lude.Text)
insSupportCode = Lens.lens (supportCode :: InstanceSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {supportCode = a} :: InstanceSnapshot)
{-# DEPRECATED insSupportCode "Use generic-lens or generic-optics with 'supportCode' instead." #-}

-- | The Amazon Resource Name (ARN) of the instance from which the snapshot was created (e.g., @arn:aws:lightsail:us-east-2:123456789101:Instance/64b8404c-ccb1-430b-8daf-12345EXAMPLE@ ).
--
-- /Note:/ Consider using 'fromInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insFromInstanceARN :: Lens.Lens' InstanceSnapshot (Lude.Maybe Lude.Text)
insFromInstanceARN = Lens.lens (fromInstanceARN :: InstanceSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {fromInstanceARN = a} :: InstanceSnapshot)
{-# DEPRECATED insFromInstanceARN "Use generic-lens or generic-optics with 'fromInstanceARN' instead." #-}

-- | The instance from which the snapshot was created.
--
-- /Note:/ Consider using 'fromInstanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insFromInstanceName :: Lens.Lens' InstanceSnapshot (Lude.Maybe Lude.Text)
insFromInstanceName = Lens.lens (fromInstanceName :: InstanceSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {fromInstanceName = a} :: InstanceSnapshot)
{-# DEPRECATED insFromInstanceName "Use generic-lens or generic-optics with 'fromInstanceName' instead." #-}

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
insTags :: Lens.Lens' InstanceSnapshot (Lude.Maybe [Tag])
insTags = Lens.lens (tags :: InstanceSnapshot -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: InstanceSnapshot)
{-# DEPRECATED insTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON InstanceSnapshot where
  parseJSON =
    Lude.withObject
      "InstanceSnapshot"
      ( \x ->
          InstanceSnapshot'
            Lude.<$> (x Lude..:? "fromBlueprintId")
            Lude.<*> (x Lude..:? "isFromAutoSnapshot")
            Lude.<*> (x Lude..:? "state")
            Lude.<*> (x Lude..:? "resourceType")
            Lude.<*> (x Lude..:? "fromAttachedDisks" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "location")
            Lude.<*> (x Lude..:? "progress")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "fromBundleId")
            Lude.<*> (x Lude..:? "sizeInGb")
            Lude.<*> (x Lude..:? "supportCode")
            Lude.<*> (x Lude..:? "fromInstanceArn")
            Lude.<*> (x Lude..:? "fromInstanceName")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
