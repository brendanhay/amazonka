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
    isFromBlueprintId,
    isIsFromAutoSnapshot,
    isState,
    isResourceType,
    isFromAttachedDisks,
    isArn,
    isCreatedAt,
    isLocation,
    isProgress,
    isName,
    isFromBundleId,
    isSizeInGb,
    isSupportCode,
    isFromInstanceARN,
    isFromInstanceName,
    isTags,
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
  { -- | The blueprint ID from which you created the snapshot (e.g., @os_debian_8_3@ ). A blueprint is a virtual private server (or /instance/ ) image used to create instances quickly.
    fromBlueprintId :: Lude.Maybe Lude.Text,
    -- | A Boolean value indicating whether the snapshot was created from an automatic snapshot.
    isFromAutoSnapshot :: Lude.Maybe Lude.Bool,
    -- | The state the snapshot is in.
    state :: Lude.Maybe InstanceSnapshotState,
    -- | The type of resource (usually @InstanceSnapshot@ ).
    resourceType :: Lude.Maybe ResourceType,
    -- | An array of disk objects containing information about all block storage disks.
    fromAttachedDisks :: Lude.Maybe [Disk],
    -- | The Amazon Resource Name (ARN) of the snapshot (e.g., @arn:aws:lightsail:us-east-2:123456789101:InstanceSnapshot/d23b5706-3322-4d83-81e5-12345EXAMPLE@ ).
    arn :: Lude.Maybe Lude.Text,
    -- | The timestamp when the snapshot was created (e.g., @1479907467.024@ ).
    createdAt :: Lude.Maybe Lude.Timestamp,
    -- | The region name and Availability Zone where you created the snapshot.
    location :: Lude.Maybe ResourceLocation,
    -- | The progress of the snapshot.
    progress :: Lude.Maybe Lude.Text,
    -- | The name of the snapshot.
    name :: Lude.Maybe Lude.Text,
    -- | The bundle ID from which you created the snapshot (e.g., @micro_1_0@ ).
    fromBundleId :: Lude.Maybe Lude.Text,
    -- | The size in GB of the SSD.
    sizeInGb :: Lude.Maybe Lude.Int,
    -- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
    supportCode :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the instance from which the snapshot was created (e.g., @arn:aws:lightsail:us-east-2:123456789101:Instance/64b8404c-ccb1-430b-8daf-12345EXAMPLE@ ).
    fromInstanceARN :: Lude.Maybe Lude.Text,
    -- | The instance from which the snapshot was created.
    fromInstanceName :: Lude.Maybe Lude.Text,
    -- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceSnapshot' with the minimum fields required to make a request.
--
-- * 'fromBlueprintId' - The blueprint ID from which you created the snapshot (e.g., @os_debian_8_3@ ). A blueprint is a virtual private server (or /instance/ ) image used to create instances quickly.
-- * 'isFromAutoSnapshot' - A Boolean value indicating whether the snapshot was created from an automatic snapshot.
-- * 'state' - The state the snapshot is in.
-- * 'resourceType' - The type of resource (usually @InstanceSnapshot@ ).
-- * 'fromAttachedDisks' - An array of disk objects containing information about all block storage disks.
-- * 'arn' - The Amazon Resource Name (ARN) of the snapshot (e.g., @arn:aws:lightsail:us-east-2:123456789101:InstanceSnapshot/d23b5706-3322-4d83-81e5-12345EXAMPLE@ ).
-- * 'createdAt' - The timestamp when the snapshot was created (e.g., @1479907467.024@ ).
-- * 'location' - The region name and Availability Zone where you created the snapshot.
-- * 'progress' - The progress of the snapshot.
-- * 'name' - The name of the snapshot.
-- * 'fromBundleId' - The bundle ID from which you created the snapshot (e.g., @micro_1_0@ ).
-- * 'sizeInGb' - The size in GB of the SSD.
-- * 'supportCode' - The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
-- * 'fromInstanceARN' - The Amazon Resource Name (ARN) of the instance from which the snapshot was created (e.g., @arn:aws:lightsail:us-east-2:123456789101:Instance/64b8404c-ccb1-430b-8daf-12345EXAMPLE@ ).
-- * 'fromInstanceName' - The instance from which the snapshot was created.
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
isFromBlueprintId :: Lens.Lens' InstanceSnapshot (Lude.Maybe Lude.Text)
isFromBlueprintId = Lens.lens (fromBlueprintId :: InstanceSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {fromBlueprintId = a} :: InstanceSnapshot)
{-# DEPRECATED isFromBlueprintId "Use generic-lens or generic-optics with 'fromBlueprintId' instead." #-}

-- | A Boolean value indicating whether the snapshot was created from an automatic snapshot.
--
-- /Note:/ Consider using 'isFromAutoSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isIsFromAutoSnapshot :: Lens.Lens' InstanceSnapshot (Lude.Maybe Lude.Bool)
isIsFromAutoSnapshot = Lens.lens (isFromAutoSnapshot :: InstanceSnapshot -> Lude.Maybe Lude.Bool) (\s a -> s {isFromAutoSnapshot = a} :: InstanceSnapshot)
{-# DEPRECATED isIsFromAutoSnapshot "Use generic-lens or generic-optics with 'isFromAutoSnapshot' instead." #-}

-- | The state the snapshot is in.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isState :: Lens.Lens' InstanceSnapshot (Lude.Maybe InstanceSnapshotState)
isState = Lens.lens (state :: InstanceSnapshot -> Lude.Maybe InstanceSnapshotState) (\s a -> s {state = a} :: InstanceSnapshot)
{-# DEPRECATED isState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The type of resource (usually @InstanceSnapshot@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isResourceType :: Lens.Lens' InstanceSnapshot (Lude.Maybe ResourceType)
isResourceType = Lens.lens (resourceType :: InstanceSnapshot -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: InstanceSnapshot)
{-# DEPRECATED isResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | An array of disk objects containing information about all block storage disks.
--
-- /Note:/ Consider using 'fromAttachedDisks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isFromAttachedDisks :: Lens.Lens' InstanceSnapshot (Lude.Maybe [Disk])
isFromAttachedDisks = Lens.lens (fromAttachedDisks :: InstanceSnapshot -> Lude.Maybe [Disk]) (\s a -> s {fromAttachedDisks = a} :: InstanceSnapshot)
{-# DEPRECATED isFromAttachedDisks "Use generic-lens or generic-optics with 'fromAttachedDisks' instead." #-}

-- | The Amazon Resource Name (ARN) of the snapshot (e.g., @arn:aws:lightsail:us-east-2:123456789101:InstanceSnapshot/d23b5706-3322-4d83-81e5-12345EXAMPLE@ ).
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isArn :: Lens.Lens' InstanceSnapshot (Lude.Maybe Lude.Text)
isArn = Lens.lens (arn :: InstanceSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: InstanceSnapshot)
{-# DEPRECATED isArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The timestamp when the snapshot was created (e.g., @1479907467.024@ ).
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isCreatedAt :: Lens.Lens' InstanceSnapshot (Lude.Maybe Lude.Timestamp)
isCreatedAt = Lens.lens (createdAt :: InstanceSnapshot -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: InstanceSnapshot)
{-# DEPRECATED isCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The region name and Availability Zone where you created the snapshot.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isLocation :: Lens.Lens' InstanceSnapshot (Lude.Maybe ResourceLocation)
isLocation = Lens.lens (location :: InstanceSnapshot -> Lude.Maybe ResourceLocation) (\s a -> s {location = a} :: InstanceSnapshot)
{-# DEPRECATED isLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The progress of the snapshot.
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isProgress :: Lens.Lens' InstanceSnapshot (Lude.Maybe Lude.Text)
isProgress = Lens.lens (progress :: InstanceSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {progress = a} :: InstanceSnapshot)
{-# DEPRECATED isProgress "Use generic-lens or generic-optics with 'progress' instead." #-}

-- | The name of the snapshot.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isName :: Lens.Lens' InstanceSnapshot (Lude.Maybe Lude.Text)
isName = Lens.lens (name :: InstanceSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: InstanceSnapshot)
{-# DEPRECATED isName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The bundle ID from which you created the snapshot (e.g., @micro_1_0@ ).
--
-- /Note:/ Consider using 'fromBundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isFromBundleId :: Lens.Lens' InstanceSnapshot (Lude.Maybe Lude.Text)
isFromBundleId = Lens.lens (fromBundleId :: InstanceSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {fromBundleId = a} :: InstanceSnapshot)
{-# DEPRECATED isFromBundleId "Use generic-lens or generic-optics with 'fromBundleId' instead." #-}

-- | The size in GB of the SSD.
--
-- /Note:/ Consider using 'sizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isSizeInGb :: Lens.Lens' InstanceSnapshot (Lude.Maybe Lude.Int)
isSizeInGb = Lens.lens (sizeInGb :: InstanceSnapshot -> Lude.Maybe Lude.Int) (\s a -> s {sizeInGb = a} :: InstanceSnapshot)
{-# DEPRECATED isSizeInGb "Use generic-lens or generic-optics with 'sizeInGb' instead." #-}

-- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
--
-- /Note:/ Consider using 'supportCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isSupportCode :: Lens.Lens' InstanceSnapshot (Lude.Maybe Lude.Text)
isSupportCode = Lens.lens (supportCode :: InstanceSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {supportCode = a} :: InstanceSnapshot)
{-# DEPRECATED isSupportCode "Use generic-lens or generic-optics with 'supportCode' instead." #-}

-- | The Amazon Resource Name (ARN) of the instance from which the snapshot was created (e.g., @arn:aws:lightsail:us-east-2:123456789101:Instance/64b8404c-ccb1-430b-8daf-12345EXAMPLE@ ).
--
-- /Note:/ Consider using 'fromInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isFromInstanceARN :: Lens.Lens' InstanceSnapshot (Lude.Maybe Lude.Text)
isFromInstanceARN = Lens.lens (fromInstanceARN :: InstanceSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {fromInstanceARN = a} :: InstanceSnapshot)
{-# DEPRECATED isFromInstanceARN "Use generic-lens or generic-optics with 'fromInstanceARN' instead." #-}

-- | The instance from which the snapshot was created.
--
-- /Note:/ Consider using 'fromInstanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isFromInstanceName :: Lens.Lens' InstanceSnapshot (Lude.Maybe Lude.Text)
isFromInstanceName = Lens.lens (fromInstanceName :: InstanceSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {fromInstanceName = a} :: InstanceSnapshot)
{-# DEPRECATED isFromInstanceName "Use generic-lens or generic-optics with 'fromInstanceName' instead." #-}

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isTags :: Lens.Lens' InstanceSnapshot (Lude.Maybe [Tag])
isTags = Lens.lens (tags :: InstanceSnapshot -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: InstanceSnapshot)
{-# DEPRECATED isTags "Use generic-lens or generic-optics with 'tags' instead." #-}

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
