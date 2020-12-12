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
    dsFromDiskName,
    dsIsFromAutoSnapshot,
    dsState,
    dsResourceType,
    dsArn,
    dsCreatedAt,
    dsLocation,
    dsProgress,
    dsName,
    dsSizeInGb,
    dsSupportCode,
    dsFromInstanceARN,
    dsFromInstanceName,
    dsFromDiskARN,
    dsTags,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.DiskSnapshotState
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.Tag
import qualified Network.AWS.Prelude as Lude

-- | Describes a block storage disk snapshot.
--
-- /See:/ 'mkDiskSnapshot' smart constructor.
data DiskSnapshot = DiskSnapshot'
  { fromDiskName ::
      Lude.Maybe Lude.Text,
    isFromAutoSnapshot :: Lude.Maybe Lude.Bool,
    state :: Lude.Maybe DiskSnapshotState,
    resourceType :: Lude.Maybe ResourceType,
    arn :: Lude.Maybe Lude.Text,
    createdAt :: Lude.Maybe Lude.Timestamp,
    location :: Lude.Maybe ResourceLocation,
    progress :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    sizeInGb :: Lude.Maybe Lude.Int,
    supportCode :: Lude.Maybe Lude.Text,
    fromInstanceARN :: Lude.Maybe Lude.Text,
    fromInstanceName :: Lude.Maybe Lude.Text,
    fromDiskARN :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DiskSnapshot' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the disk snapshot.
-- * 'createdAt' - The date when the disk snapshot was created.
-- * 'fromDiskARN' - The Amazon Resource Name (ARN) of the source disk from which the disk snapshot was created.
-- * 'fromDiskName' - The unique name of the source disk from which the disk snapshot was created.
-- * 'fromInstanceARN' - The Amazon Resource Name (ARN) of the source instance from which the disk (system volume) snapshot was created.
-- * 'fromInstanceName' - The unique name of the source instance from which the disk (system volume) snapshot was created.
-- * 'isFromAutoSnapshot' - A Boolean value indicating whether the snapshot was created from an automatic snapshot.
-- * 'location' - The AWS Region and Availability Zone where the disk snapshot was created.
-- * 'name' - The name of the disk snapshot (e.g., @my-disk-snapshot@ ).
-- * 'progress' - The progress of the disk snapshot operation.
-- * 'resourceType' - The Lightsail resource type (e.g., @DiskSnapshot@ ).
-- * 'sizeInGb' - The size of the disk in GB.
-- * 'state' - The status of the disk snapshot operation.
-- * 'supportCode' - The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
-- * 'tags' - The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
mkDiskSnapshot ::
  DiskSnapshot
mkDiskSnapshot =
  DiskSnapshot'
    { fromDiskName = Lude.Nothing,
      isFromAutoSnapshot = Lude.Nothing,
      state = Lude.Nothing,
      resourceType = Lude.Nothing,
      arn = Lude.Nothing,
      createdAt = Lude.Nothing,
      location = Lude.Nothing,
      progress = Lude.Nothing,
      name = Lude.Nothing,
      sizeInGb = Lude.Nothing,
      supportCode = Lude.Nothing,
      fromInstanceARN = Lude.Nothing,
      fromInstanceName = Lude.Nothing,
      fromDiskARN = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The unique name of the source disk from which the disk snapshot was created.
--
-- /Note:/ Consider using 'fromDiskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsFromDiskName :: Lens.Lens' DiskSnapshot (Lude.Maybe Lude.Text)
dsFromDiskName = Lens.lens (fromDiskName :: DiskSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {fromDiskName = a} :: DiskSnapshot)
{-# DEPRECATED dsFromDiskName "Use generic-lens or generic-optics with 'fromDiskName' instead." #-}

-- | A Boolean value indicating whether the snapshot was created from an automatic snapshot.
--
-- /Note:/ Consider using 'isFromAutoSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsIsFromAutoSnapshot :: Lens.Lens' DiskSnapshot (Lude.Maybe Lude.Bool)
dsIsFromAutoSnapshot = Lens.lens (isFromAutoSnapshot :: DiskSnapshot -> Lude.Maybe Lude.Bool) (\s a -> s {isFromAutoSnapshot = a} :: DiskSnapshot)
{-# DEPRECATED dsIsFromAutoSnapshot "Use generic-lens or generic-optics with 'isFromAutoSnapshot' instead." #-}

-- | The status of the disk snapshot operation.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsState :: Lens.Lens' DiskSnapshot (Lude.Maybe DiskSnapshotState)
dsState = Lens.lens (state :: DiskSnapshot -> Lude.Maybe DiskSnapshotState) (\s a -> s {state = a} :: DiskSnapshot)
{-# DEPRECATED dsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The Lightsail resource type (e.g., @DiskSnapshot@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsResourceType :: Lens.Lens' DiskSnapshot (Lude.Maybe ResourceType)
dsResourceType = Lens.lens (resourceType :: DiskSnapshot -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: DiskSnapshot)
{-# DEPRECATED dsResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The Amazon Resource Name (ARN) of the disk snapshot.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsArn :: Lens.Lens' DiskSnapshot (Lude.Maybe Lude.Text)
dsArn = Lens.lens (arn :: DiskSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DiskSnapshot)
{-# DEPRECATED dsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The date when the disk snapshot was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsCreatedAt :: Lens.Lens' DiskSnapshot (Lude.Maybe Lude.Timestamp)
dsCreatedAt = Lens.lens (createdAt :: DiskSnapshot -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: DiskSnapshot)
{-# DEPRECATED dsCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The AWS Region and Availability Zone where the disk snapshot was created.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsLocation :: Lens.Lens' DiskSnapshot (Lude.Maybe ResourceLocation)
dsLocation = Lens.lens (location :: DiskSnapshot -> Lude.Maybe ResourceLocation) (\s a -> s {location = a} :: DiskSnapshot)
{-# DEPRECATED dsLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The progress of the disk snapshot operation.
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsProgress :: Lens.Lens' DiskSnapshot (Lude.Maybe Lude.Text)
dsProgress = Lens.lens (progress :: DiskSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {progress = a} :: DiskSnapshot)
{-# DEPRECATED dsProgress "Use generic-lens or generic-optics with 'progress' instead." #-}

-- | The name of the disk snapshot (e.g., @my-disk-snapshot@ ).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsName :: Lens.Lens' DiskSnapshot (Lude.Maybe Lude.Text)
dsName = Lens.lens (name :: DiskSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DiskSnapshot)
{-# DEPRECATED dsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The size of the disk in GB.
--
-- /Note:/ Consider using 'sizeInGb' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSizeInGb :: Lens.Lens' DiskSnapshot (Lude.Maybe Lude.Int)
dsSizeInGb = Lens.lens (sizeInGb :: DiskSnapshot -> Lude.Maybe Lude.Int) (\s a -> s {sizeInGb = a} :: DiskSnapshot)
{-# DEPRECATED dsSizeInGb "Use generic-lens or generic-optics with 'sizeInGb' instead." #-}

-- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
--
-- /Note:/ Consider using 'supportCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSupportCode :: Lens.Lens' DiskSnapshot (Lude.Maybe Lude.Text)
dsSupportCode = Lens.lens (supportCode :: DiskSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {supportCode = a} :: DiskSnapshot)
{-# DEPRECATED dsSupportCode "Use generic-lens or generic-optics with 'supportCode' instead." #-}

-- | The Amazon Resource Name (ARN) of the source instance from which the disk (system volume) snapshot was created.
--
-- /Note:/ Consider using 'fromInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsFromInstanceARN :: Lens.Lens' DiskSnapshot (Lude.Maybe Lude.Text)
dsFromInstanceARN = Lens.lens (fromInstanceARN :: DiskSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {fromInstanceARN = a} :: DiskSnapshot)
{-# DEPRECATED dsFromInstanceARN "Use generic-lens or generic-optics with 'fromInstanceARN' instead." #-}

-- | The unique name of the source instance from which the disk (system volume) snapshot was created.
--
-- /Note:/ Consider using 'fromInstanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsFromInstanceName :: Lens.Lens' DiskSnapshot (Lude.Maybe Lude.Text)
dsFromInstanceName = Lens.lens (fromInstanceName :: DiskSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {fromInstanceName = a} :: DiskSnapshot)
{-# DEPRECATED dsFromInstanceName "Use generic-lens or generic-optics with 'fromInstanceName' instead." #-}

-- | The Amazon Resource Name (ARN) of the source disk from which the disk snapshot was created.
--
-- /Note:/ Consider using 'fromDiskARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsFromDiskARN :: Lens.Lens' DiskSnapshot (Lude.Maybe Lude.Text)
dsFromDiskARN = Lens.lens (fromDiskARN :: DiskSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {fromDiskARN = a} :: DiskSnapshot)
{-# DEPRECATED dsFromDiskARN "Use generic-lens or generic-optics with 'fromDiskARN' instead." #-}

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsTags :: Lens.Lens' DiskSnapshot (Lude.Maybe [Tag])
dsTags = Lens.lens (tags :: DiskSnapshot -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: DiskSnapshot)
{-# DEPRECATED dsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON DiskSnapshot where
  parseJSON =
    Lude.withObject
      "DiskSnapshot"
      ( \x ->
          DiskSnapshot'
            Lude.<$> (x Lude..:? "fromDiskName")
            Lude.<*> (x Lude..:? "isFromAutoSnapshot")
            Lude.<*> (x Lude..:? "state")
            Lude.<*> (x Lude..:? "resourceType")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "location")
            Lude.<*> (x Lude..:? "progress")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "sizeInGb")
            Lude.<*> (x Lude..:? "supportCode")
            Lude.<*> (x Lude..:? "fromInstanceArn")
            Lude.<*> (x Lude..:? "fromInstanceName")
            Lude.<*> (x Lude..:? "fromDiskArn")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
